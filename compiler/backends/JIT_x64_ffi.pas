unit JIT_x64_ffi;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  JIT_x64_ffi unit attempts to bridge the gap between our JIT and the native
  libraries by creating a thin trampoline to JIT compiled function bodies.

  In otherwords we need need to handle system ABI so that it's compatible with our VM.
  Specilizing towards our environment allows much faster callbacks than what libffi can provide.

  Early tests show 5x speedup over FFI, 10x speedup over FFI that is not aware
  that whole function bodies are JIT compiled (when it dispatches to the VM).

  Note that this is a limited trampoline for basetypes.
  - Floats
  - Ordinals
  - Pointers (anything that moves like a pointer)

  Anything complex should will fall back to libffi.
}
{$I header.inc}
{$hints off}
{$Q-}

interface

uses
  SysUtils, Math, xpr.Types, xpr.Bytecode, xpr.Interpreter, xpr.ffi, JIT_x64;

function XprCreateJITClosure(
  var   MainInterp:   TInterpreter;
  var   BC:           TBytecode;
        FuncEntry:    Int32;
        TypeInfo:     PXprCallbackTypeInfo;
        JitBodyPtr:   Pointer;
        CaptureRefs:  PPointerArray;
        CaptureCount: Int32): PXprClosureData;

procedure XprFreeJITClosure(Closure: PXprClosureData);

implementation

const
  r8  = EReg(8);
  r9  = EReg(9);
  r10 = EReg(10);
  r11 = EReg(11);

  TRAMPOLINE_ALLOC_SIZE = 512;

// ---------------------------------------------------------------------------
// Low-level emit helpers
// ---------------------------------------------------------------------------

procedure EmitCallImm64(var E: TJitEmitter; Target: Pointer);
var Imm: Int64;
begin
  Imm := Int64(Target);
  E.WriteBytes([$49, $BA]);      // mov r10, imm64
  E.WriteBytes(@Imm, 8);
  E.WriteBytes([$41, $FF, $D2]); // call r10
end;

procedure EmitStoreReg(var E: TJitEmitter; SrcReg: EReg; Offset, Bytes: Int32);
begin
  case Bytes of
    1: E.MOV_Mem_Reg_i8 (rax, Offset, SrcReg);
    2: E.MOV_Mem_Reg_i16(rax, Offset, SrcReg);
    4: E.MOV_Mem_Reg_i32(rax, Offset, SrcReg);
  else E.MOV_Mem_Reg_i64(rax, Offset, SrcReg);
  end;
end;

procedure EmitStoreR8(var E: TJitEmitter; Offset, Bytes: Int32);
begin
  case Bytes of
    1: begin E.WriteBytes([$44, $88, $80]); E.WriteBytes(@Offset, 4); end;
    2: begin E.WriteBytes([$66, $44, $89, $80]); E.WriteBytes(@Offset, 4); end;
    4: begin E.WriteBytes([$44, $89, $80]); E.WriteBytes(@Offset, 4); end;
  else begin E.WriteBytes([$4C, $89, $80]); E.WriteBytes(@Offset, 4); end;
  end;
end;

procedure EmitStoreR9(var E: TJitEmitter; Offset, Bytes: Int32);
begin
  case Bytes of
    1: begin E.WriteBytes([$44, $88, $88]); E.WriteBytes(@Offset, 4); end;
    2: begin E.WriteBytes([$66, $44, $89, $88]); E.WriteBytes(@Offset, 4); end;
    4: begin E.WriteBytes([$44, $89, $88]); E.WriteBytes(@Offset, 4); end;
  else begin E.WriteBytes([$4C, $89, $88]); E.WriteBytes(@Offset, 4); end;
  end;
end;

procedure EmitStoreStack(var E: TJitEmitter; StackOff, DestOff, Bytes: Int32);
begin
  E.WriteBytes([$4C, $8B, $9C, $24]);   // mov r11, [rsp + StackOff]
  E.WriteBytes(@StackOff, 4);

  case Bytes of
    1: begin E.WriteBytes([$44, $88, $98]); E.WriteBytes(@DestOff, 4); end;
    2: begin E.WriteBytes([$66, $44, $89, $98]); E.WriteBytes(@DestOff, 4); end;
    4: begin E.WriteBytes([$44, $89, $98]); E.WriteBytes(@DestOff, 4); end;
  else begin E.WriteBytes([$4C, $89, $98]); E.WriteBytes(@DestOff, 4); end;
  end;
end;


// ===========================================================================
// XprCreateJITClosure
// ===========================================================================

function XprCreateJITClosure(
  var   MainInterp:   TInterpreter;
  var   BC:           TBytecode;
        FuncEntry:    Int32;
        TypeInfo:     PXprCallbackTypeInfo;
        JitBodyPtr:   Pointer;
        CaptureRefs:  PPointerArray;
        CaptureCount: Int32): PXprClosureData;
var
  Data:       PXprClosureData;
  Emitter:    TJitEmitter;
  ExecMem:    PByte;
  BasePtrMem: PByte;
  i, k, offset:  Int32;
  IsFloat:    Boolean;
  Imm64:      Int64;
  StoreSize: Int32;
  {$IFDEF WINDOWS}
  Win64Pos: Int32;
  {$ELSE}
  SysVInt, SysVFloat, SysVStack: Int32;
  {$ENDIF}

  // NOT USED!
  // Wrote as an experiment to see where the costs are.
  // Can be around 15% faster to avoid the JIT's overly safe preamble.
  // Note: Experimental only for none REX, GPR (no float recovery).
  procedure EmitMinimalJITBody(p: PByte);
  var start, stop: Int32;
  begin
    start := 0;
    while not((PByte((p+start)+0)^ = $48) and
              (PByte((p+start)+1)^ = $89) and
              (PByte((p+start)+2)^ = $CB)) do Inc(start);

    // unsafe -> Just for quick experiments sake.
    stop := 0;
    while PByte(p+stop)^ <> $C3 do Inc(stop);

    while not((PByte((p+stop)-2)^ = $48) and
              (PByte((p+stop)-1)^ = $31) and
              (PByte((p+stop)-0)^ = $c0)) do Dec(stop);

    Emitter.PreserveGPR();
    Move(PByte(JitBodyPtr+start)^, Emitter.p^, stop-start+1);
    Inc(Emitter.p, stop-start+1);
    Emitter.RecoverGPR();
  end;

begin
  Result := nil;

  Data := AllocMem(SizeOf(TXprClosureData));
  Data^.Interp := AllocMem(SizeOf(TInterpreter));
  Data^.Interp^ := TInterpreter.NewForThread(MainInterp, FuncEntry, BC.Code.Size);
  Data^.BC        := @BC;
  Data^.FuncEntry := FuncEntry;

  Data^.ArgCount  := TypeInfo^.ArgCount;
  Data^.HasReturn := TypeInfo^.HasReturn;
  Data^.RetType   := TypeInfo^.RetType;
  Data^.RetSize   := TypeInfo^.RetSize;
  for i := 0 to TypeInfo^.ArgCount - 1 do
  begin
    Data^.ArgTypes[i]    := TypeInfo^.ArgTypes[i];
    Data^.ArgSizes[i]    := TypeInfo^.ArgSizes[i];
    Data^.ArgIsRef[i]    := False;
    Data^.FFIArgTypes[i] := XprTypeToFFIType(TypeInfo^.ArgTypes[i]);
  end;
  Data^.CaptureCount := CaptureCount;
  for i := 0 to CaptureCount - 1 do
    Data^.CaptureRefs[i] := CaptureRefs^[i];

  {$IFNDEF xpr_NoFFIJIT}
  Data^.IsJITDirect := True;
  Data^.JitBodyPtr  := JitBodyPtr;
  {$ENDIF}

  ScanPrologue(Data, BC);

  // 1. Allocate Frame Memory
  BasePtrMem := @Data^.Interp^.Data[0];
  FillChar(BasePtrMem^, Data^.FrameSize + SizeOf(Pointer), 0);
  Data^.Interp^.BasePtr := BasePtrMem;

  // 2. Write Capture values to the frame once
  for i := 0 to CaptureCount - 1 do
    PPointer(BasePtrMem + Data^.CaptureOffsets[i])^ := CaptureRefs^[i];

  // 3. Set the Return Pointer to an unused slot at the END of the frame
  // The JIT will dereference Frame[RetOffset] and write its answer to (BasePtrMem + FrameSize).
  if TypeInfo^.HasReturn then
    PPointer(BasePtrMem + Data^.RetOffset)^ := BasePtrMem + Data^.FrameSize;

  ExecMem := AllocateExecutableMemory(TRAMPOLINE_ALLOC_SIZE);
  Emitter.Init(ExecMem, nil);

  Emitter.MOV_Reg_Imm64(rax, Int64(BasePtrMem));

  {$IFDEF WINDOWS}
  Win64Pos := 0;
  for i := 0 to TypeInfo^.ArgCount - 1 do
  begin
    // A ref arg arrives as a pointer in an GPR reg - always.
    IsFloat := (TypeInfo^.ArgTypes[i] in [xtSingle, xtDouble]) and (not Data^.ArgIsRef[i]);
    Offset  := Data^.ArgOffsets[i];

    // ref params are pointers.
    StoreSize := IfThen(Data^.ArgIsRef[i], SizeOf(Pointer), TypeInfo^.ArgSizes[i]);

    if Win64Pos < 4 then
    begin
      if IsFloat then
      begin
        if TypeInfo^.ArgTypes[i] = xtSingle then
          Emitter.MOVSS_Mem_XMM(rax, Offset, EXMMReg(Win64Pos))
        else
          Emitter.MOVSD_Mem_XMM(rax, Offset, EXMMReg(Win64Pos));
      end
      else
      begin
        case Win64Pos of
          0: EmitStoreReg(Emitter, rcx, Offset, StoreSize);
          1: EmitStoreReg(Emitter, rdx, Offset, StoreSize);
          2: EmitStoreR8 (Emitter,      Offset, StoreSize);
          3: EmitStoreR9 (Emitter,      Offset, StoreSize);
        end;
      end;
    end
    else
    begin
      EmitStoreStack(Emitter, 40 + (Win64Pos - 4) * 8, Offset, StoreSize);
    end;
    Inc(Win64Pos);
  end;

  {$ELSE}
  SysVInt   := 0;
  SysVFloat := 0;
  SysVStack := 0;

  for i := 0 to TypeInfo^.ArgCount - 1 do
  begin
    IsFloat := (TypeInfo^.ArgTypes[i] in [xtSingle, xtDouble]) and not Data^.ArgIsRef[i];
    Offset  := Data^.ArgOffsets[i];
    StoreSize := IfThen(Data^.ArgIsRef[i], SizeOf(Pointer), TypeInfo^.ArgSizes[i]);

    if IsFloat then
    begin
      if SysVFloat < 8 then
      begin
        if TypeInfo^.ArgTypes[i] = xtSingle then
          Emitter.MOVSS_Mem_XMM(rax, Offset, EXMMReg(SysVFloat))
        else
          Emitter.MOVSD_Mem_XMM(rax, Offset, EXMMReg(SysVFloat));
        Inc(SysVFloat);
      end
      else
      begin
        EmitStoreStack(Emitter, 8 + SysVStack * 8, Offset, StoreSize);
        Inc(SysVStack);
      end;
    end
    else
    begin
      if SysVInt < 6 then
      begin
        case SysVInt of
          0: EmitStoreReg(Emitter, rdi, Offset, StoreSize);
          1: EmitStoreReg(Emitter, rsi, Offset, StoreSize);
          2: EmitStoreReg(Emitter, rdx, Offset, StoreSize);
          3: EmitStoreReg(Emitter, rcx, Offset, StoreSize);
          4: EmitStoreR8 (Emitter,      Offset, StoreSize);
          5: EmitStoreR9 (Emitter,      Offset, StoreSize);
        end;
        Inc(SysVInt);
      end
      else
      begin
        EmitStoreStack(Emitter, 8 + SysVStack * 8, Offset, StoreSize);
        Inc(SysVStack);
      end;
    end;
  end;
  {$ENDIF}


  // --- Call the JIT Body -----------------------------------------------------
  {$IFDEF WINDOWS}
  Emitter.MOV_Reg_Reg(rcx, rax);
  Emitter.WriteBytes([$48, $83, $EC, $28]);   // sub rsp, 40 (16-byte align)
  {$ELSE}
  Emitter.MOV_Reg_Reg(rdi, rax);
  Emitter.WriteBytes([$48, $83, $EC, $08]);   // sub rsp, 8
  {$ENDIF}

  EmitCallImm64(Emitter, JitBodyPtr);

  {$IFDEF WINDOWS}
  Emitter.WriteBytes([$48, $83, $C4, $28]);   // add rsp, 40
  {$ELSE}
  Emitter.WriteBytes([$48, $83, $C4, $08]);   // add rsp, 8
  {$ENDIF}


  // --- Read Return Value straight from the stack -----------------------------
  if TypeInfo^.HasReturn then
  begin
    Emitter.MOV_Reg_Imm64(rax, Int64(BasePtrMem));
    Offset := Data^.FrameSize; // This is the slot where the JIT wrote the result!

    case BaseJITType(TypeInfo^.RetType) of
      xtSingle:                    Emitter.MOVSS_XMM_Mem(xmm0, rax, Offset);
      xtDouble:                    Emitter.MOVSD_XMM_Mem(xmm0, rax, Offset);
      xtInt8:                      Emitter.MOVSX_Reg_Mem_i8(rax, rax, Offset);
      xtUInt8, xtBool, xtAnsiChar: Emitter.MOVZX_Reg_Mem_i8(rax, rax, Offset);
      xtInt16:                     Emitter.MOVSX_Reg_Mem_i16(rax, rax, Offset);
      xtUInt16, xtUnicodeChar:     Emitter.MOVZX_Reg_Mem_i16(rax, rax, Offset);
      xtInt32:                     Emitter.MOVSXD_Reg_Mem_i32(rax, rax, Offset);
      xtUInt32:                    Emitter.MOV_Reg_Mem_i32(rax, rax, Offset); // Zero-extends to rax
    else
      Emitter.MOV_Reg_Mem_i64(rax, rax, Offset);
    end;
  end;

  Emitter.RET;

  {$IFDEF verbose}
  WriteLn('=== FFI Trampoline ==================');
  for k:=0 to TRAMPOLINE_ALLOC_SIZE-1 do
  begin
    Write(IntToHex(PByte(execMem+k)^));
    if PByte(execMem+k)^ = $C3 then
      break;
  end;
  WriteLn();
  {$endif}

  SetMemoryExecutable(ExecMem, TRAMPOLINE_ALLOC_SIZE);
  Data^.FFIFuncPtr := ExecMem;
  Result := Data;
end;

procedure XprFreeJITClosure(Closure: PXprClosureData);
begin
  if Closure = nil then Exit;

  if Assigned(Closure^.FFIFuncPtr) then
    FreeExecutableMemory(Closure^.FFIFuncPtr, TRAMPOLINE_ALLOC_SIZE);

  if Assigned(Closure^.Interp) then
  begin
    SetLength(Closure^.Interp^.Data, 0);
    FreeMem(Closure^.Interp);
  end;

  FreeMem(Closure);
end;

end.
