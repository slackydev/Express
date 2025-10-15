unit JIT_x64;

interface 

uses
  SysUtils,
  xpr.Types,
  xpr.Bytecode,
  xpr.Intermediate,
  xpr.BytecodeEmitter,
  xpr.Errors;
                

// In the 'type' section of xpr.Interpreter.pas
type
  EReg = (
    rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi // 0-7
  );

  EXMMReg = (
    xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7 // 0-7
  );

  // The new emitter record
  TJitEmitter = record
  private
    p: PByte; // The current write position
    procedure WriteBytes(const Bytes: array of Byte);
  public
    procedure Init(StartPtr: PByte);

    // --- Integer Emitters ---
    procedure MOV_Reg_Imm64(Reg: EReg; Value: Int64);
    procedure MOVZX_Reg_Mem_i8(Reg: EReg; BaseReg: EReg; Offset: Int64);
    procedure MOVZX_Reg_Mem_i16(Reg: EReg; BaseReg: EReg; Offset: Int64);
    procedure MOV_Reg_Mem_i32(Reg: EReg; BaseReg: EReg; Offset: Int64);
    procedure MOV_Reg_Mem_i64(Reg: EReg; BaseReg: EReg; Offset: Int64);
    procedure MOV_Mem_Reg_i8(BaseReg: EReg; Offset: Int64; Reg: EReg);
    procedure MOV_Mem_Reg_i16(BaseReg: EReg; Offset: Int64; Reg: EReg);
    procedure MOV_Mem_Reg_i32(BaseReg: EReg; Offset: Int64; Reg: EReg);
    procedure MOV_Mem_Reg_i64(BaseReg: EReg; Offset: Int64; Reg: EReg);
    procedure ADD_Reg_Reg(DestReg, SourceReg: EReg);
    procedure IMUL_Reg_Reg(DestReg, SourceReg: EReg);
    procedure SUB_Reg_Reg(DestReg, SourceReg: EReg);
    procedure IDIV_Reg(SourceReg: EReg);
    procedure AND_Reg_Reg(DestReg, SourceReg: EReg);
    procedure OR_Reg_Reg(DestReg, SourceReg: EReg);
    procedure XOR_Reg_Reg(DestReg, SourceReg: EReg);
    procedure SHL_Reg_CL(DestReg: EReg);
    procedure SHR_Reg_CL(DestReg: EReg);
    procedure SAR_Reg_CL(DestReg: EReg);

    // --- Floating-Point Emitters ---
    procedure MOVSD_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
    procedure MOVSS_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
    procedure MOVSD_Mem_XMM(BaseReg: EReg; Offset: Int64; SourceReg: EXMMReg);
    procedure MOVSS_Mem_XMM(BaseReg: EReg; Offset: Int64; SourceReg: EXMMReg);
    procedure ADDSD_XMM_XMM(DestReg, SourceReg: EXMMReg);
    procedure MULSD_XMM_XMM(DestReg, SourceReg: EXMMReg);
    procedure SUBSD_XMM_XMM(DestReg, SourceReg: EXMMReg);
    procedure DIVSD_XMM_XMM(DestReg, SourceReg: EXMMReg);
    procedure XORPS_XMM_XMM(DestReg, SourceReg: EXMMReg);
    procedure UCOMISD_XMM_XMM(Reg1, Reg2: EXMMReg);

    // --- Control ---
    procedure RET;
    
    // --- High-Level Helpers ---
    procedure Load_Int_Operand(const arg: TOperand; Reg: EReg);
    procedure Store_Int_Result(const arg: TOperand; Reg: EReg);
    procedure Load_Float_Operand(const arg: TOperand; Reg: EXMMReg);
    procedure Store_Float_Result(const arg: TOperand; Reg: EXMMReg);
  end;
  
  
const
  PROT_READ  = $1;
  PROT_WRITE = $2;
  PROT_EXEC  = $4;

  MAP_PRIVATE   = $0002;
  MAP_ANONYMOUS = $1000;
  MAP_FAILED    = Pointer(-1);
  
function AllocateExecutableMemory(Size: SizeInt): Pointer;  
function SetMemoryExecutable(Address: Pointer; Size: SizeInt): Boolean;  
procedure FreeExecutableMemory(Address: Pointer; Size: SizeInt);

implementation

uses
  Math,
  xpr.Utils, JIT_x64,
  {$IFDEF xpr_UseSuperInstructions},
  {$IFDEF WINDOWS}Windows{$ENDIF}
  {$IFDEF UNIX}SysCall, BaseUnix, Unix{$ENDIF}
  {$ENDIF};     
  
function AllocateExecutableMemory(Size: SizeInt): Pointer;
begin
  Result := nil;
  if Size = 0 then Exit;

  {$IFDEF WINDOWS}
  Result := VirtualAlloc(nil, Size, MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE);
  {$ELSE}
  Result := fpmmap(nil, Size, PROT_READ or PROT_WRITE, MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);
  if Result = MAP_FAILED then
    Result := nil;
  {$ENDIF}
end;

function SetMemoryExecutable(Address: Pointer; Size: SizeInt): Boolean;
{$IFDEF WINDOWS}
var
  OldProtect: DWORD;
{$ENDIF}
begin
  Result := False;
  if (Address = nil) or (Size = 0) then Exit;

  {$IFDEF WINDOWS}
  Result := VirtualProtect(Address, Size, PAGE_EXECUTE_READ, @OldProtect);
  {$ELSE}
  Result := (fpmprotect(Address, Size, PROT_READ or PROT_EXEC) = 0);
  {$ENDIF}
end;

procedure FreeExecutableMemory(Address: Pointer; Size: SizeInt);
begin
  if Address = nil then Exit;

  {$IFDEF WINDOWS}
  // On Windows, MEM_RELEASE requires the size to be 0 to free the entire
  // region that was allocated by VirtualAlloc.
  VirtualFree(Address, 0, MEM_RELEASE);
  {$ELSE}
  // On Unix-like systems, you must provide the original size of the mapping to free it.
  fpmunmap(Address, Size);
  {$ENDIF}
end;

end.
             
  
{ TJitEmitter }

procedure TJitEmitter.Init(StartPtr: PByte);
begin
  p := StartPtr;
end;

procedure TJitEmitter.WriteBytes(const Bytes: array of Byte);
var
  len: Integer;
begin
  len := Length(Bytes);
  if len > 0 then
  begin
    System.Move(Bytes[0], p^, len);
    Inc(p, len);
  end;
end;

procedure TJitEmitter.RET;
begin
  WriteBytes([$C3]);
end;

// --- Integer Emitters: Immediate and Ops ---

procedure TJitEmitter.MOV_Reg_Imm64(Reg: EReg; Value: Int64);
begin
  WriteBytes([$48, $B8+Ord(Reg)]);
  PInt64(p)^ := Value; Inc(p, 8);
end;

procedure TJitEmitter.ADD_Reg_Reg(DestReg, SourceReg: EReg);
begin
  WriteBytes([$48, $01, $C0 + (Ord(SourceReg) * 8) + Ord(DestReg)]);
end;

procedure TJitEmitter.IMUL_Reg_Reg(DestReg, SourceReg: EReg);
begin
  WriteBytes([$48, $0F, $AF, $C0 + (Ord(SourceReg) * 8) + Ord(DestReg)]);
end;

// Emits: sub dest_reg, src_reg
// Example for SUB RAX, RCX: 48 29 C8
procedure TJitEmitter.SUB_Reg_Reg(DestReg, SourceReg: EReg);
begin
  WriteBytes([$48, $29, $C0 + (Ord(SourceReg) * 8) + Ord(DestReg)]);
end;

// Emits: idiv src_reg (Signed Division)
// Performs RDX:RAX / src_reg. Result in RAX, Remainder in RDX.
// IMPORTANT: RDX must be sign-extended from RAX before this call.
procedure TJitEmitter.IDIV_Reg(SourceReg: EReg);
begin
  // Before calling this, you MUST emit CQO to sign-extend RAX into RDX.
  // CQO (Convert Quadword to Octoword): 48 99
  WriteBytes([$48, $99]);

  // IDIV r/m64 opcode
  WriteBytes([$48, $F7, $F8 + Ord(SourceReg)]);
end;

// Emits: and dest_reg, src_reg
procedure TJitEmitter.AND_Reg_Reg(DestReg, SourceReg: EReg);
begin
  WriteBytes([$48, $21, $C0 + (Ord(SourceReg) * 8) + Ord(DestReg)]);
end;

// Emits: or dest_reg, src_reg
procedure TJitEmitter.OR_Reg_Reg(DestReg, SourceReg: EReg);
begin
  WriteBytes([$48, $09, $C0 + (Ord(SourceReg) * 8) + Ord(DestReg)]);
end;

// Emits: xor dest_reg, src_reg
procedure TJitEmitter.XOR_Reg_Reg(DestReg, SourceReg: EReg);
begin
  WriteBytes([$48, $31, $C0 + (Ord(SourceReg) * 8) + Ord(DestReg)]);
end;

// Emits: shl dest_reg, cl (Shift Left by RCX)
// NOTE: x86 shift instructions use the CL register (the low 8 bits of RCX) for the shift count.
procedure TJitEmitter.SHL_Reg_CL(DestReg: EReg);
begin
  WriteBytes([$48, $D3, $E0 + Ord(DestReg)]);
end;

// Emits: shr dest_reg, cl (Logical Shift Right by RCX)
procedure TJitEmitter.SHR_Reg_CL(DestReg: EReg);
begin
  WriteBytes([$48, $D3, $E8 + Ord(DestReg)]);
end;

// Emits: sar dest_reg, cl (Arithmetic Shift Right by RCX)
procedure TJitEmitter.SAR_Reg_CL(DestReg: EReg);
begin
  WriteBytes([$48, $D3, $F8 + Ord(DestReg)]);
end;

// --- Integer Emitters: Memory Loads ---

procedure TJitEmitter.MOVZX_Reg_Mem_i8(Reg: EReg; BaseReg: EReg; Offset: Int64);
begin
  p^ := $48; Inc(p); p^ := $0F; Inc(p); p^ := $B6; Inc(p);
  p^ := $80 + (Ord(Reg) * 8) + Ord(BaseReg); Inc(p);
  PInt32(p)^ := Int32(Offset); Inc(p, 4);
end;

procedure TJitEmitter.MOVZX_Reg_Mem_i16(Reg: EReg; BaseReg: EReg; Offset: Int64);
begin
  p^ := $48; Inc(p); p^ := $0F; Inc(p); p^ := $B7; Inc(p);
  p^ := $80 + (Ord(Reg) * 8) + Ord(BaseReg); Inc(p);
  PInt32(p)^ := Int32(Offset); Inc(p, 4);
end;

procedure TJitEmitter.MOV_Reg_Mem_i32(Reg: EReg; BaseReg: EReg; Offset: Int64);
begin
  p^ := $8B; Inc(p);
  p^ := $80 + (Ord(Reg) * 8) + Ord(BaseReg); Inc(p);
  PInt32(p)^ := Int32(Offset); Inc(p, 4);
end;

procedure TJitEmitter.MOV_Reg_Mem_i64(Reg: EReg; BaseReg: EReg; Offset: Int64);
begin
  p^ := $48; Inc(p); p^ := $8B; Inc(p);
  p^ := $80 + (Ord(Reg) * 8) + Ord(BaseReg); Inc(p);
  PInt32(p)^ := Int32(Offset); Inc(p, 4);
end;

// --- Integer Emitters: Memory Stores ---

procedure TJitEmitter.MOV_Mem_Reg_i8(BaseReg: EReg; Offset: Int64; Reg: EReg);
begin
  p^ := $88; Inc(p);
  p^ := $80 + (Ord(Reg) * 8) + Ord(BaseReg); Inc(p);
  PInt32(p)^ := Int32(Offset); Inc(p, 4);
end;

procedure TJitEmitter.MOV_Mem_Reg_i16(BaseReg: EReg; Offset: Int64; Reg: EReg);
begin
  p^ := $66; Inc(p); p^ := $89; Inc(p);
  p^ := $80 + (Ord(Reg) * 8) + Ord(BaseReg); Inc(p);
  PInt32(p)^ := Int32(Offset); Inc(p, 4);
end;

procedure TJitEmitter.MOV_Mem_Reg_i32(BaseReg: EReg; Offset: Int64; Reg: EReg);
begin
  p^ := $89; Inc(p);
  p^ := $80 + (Ord(Reg) * 8) + Ord(BaseReg); Inc(p);
  PInt32(p)^ := Int32(Offset); Inc(p, 4);
end;

procedure TJitEmitter.MOV_Mem_Reg_i64(BaseReg: EReg; Offset: Int64; Reg: EReg);
begin
  p^ := $48; Inc(p); p^ := $89; Inc(p);
  p^ := $80 + (Ord(Reg) * 8) + Ord(BaseReg); Inc(p);
  PInt32(p)^ := Int32(Offset); Inc(p, 4);
end;

// --- Floating-Point Emitters ---

procedure TJitEmitter.MOVSD_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
begin
  p^ := $F2; Inc(p); p^ := $0F; Inc(p); p^ := $10; Inc(p); // MOVSD xmm, m64
  p^ := $80 + (Ord(DestReg) * 8) + Ord(BaseReg); Inc(p);
  PInt32(p)^ := Int32(Offset); Inc(p, 4);
end;

procedure TJitEmitter.MOVSS_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
begin
  p^ := $F3; Inc(p); p^ := $0F; Inc(p); p^ := $10; Inc(p); // MOVSS xmm, m32
  p^ := $80 + (Ord(DestReg) * 8) + Ord(BaseReg); Inc(p);
  PInt32(p)^ := Int32(Offset); Inc(p, 4);
end;

procedure TJitEmitter.MOVSD_Mem_XMM(BaseReg: EReg; Offset: Int64; SourceReg: EXMMReg);
begin
  p^ := $F2; Inc(p); p^ := $0F; Inc(p); p^ := $11; Inc(p); // MOVSD m64, xmm
  p^ := $80 + (Ord(SourceReg) * 8) + Ord(BaseReg); Inc(p);
  PInt32(p)^ := Int32(Offset); Inc(p, 4);
end;

procedure TJitEmitter.MOVSS_Mem_XMM(BaseReg: EReg; Offset: Int64; SourceReg: EXMMReg);
begin
  p^ := $F3; Inc(p); p^ := $0F; Inc(p); p^ := $11; Inc(p); // MOVSS m32, xmm
  p^ := $80 + (Ord(SourceReg) * 8) + Ord(BaseReg); Inc(p);
  PInt32(p)^ := Int32(Offset); Inc(p, 4);
end;

procedure TJitEmitter.ADDSD_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin
  p^ := $F2; Inc(p); p^ := $0F; Inc(p); p^ := $58; Inc(p); // ADDSD xmm, xmm
  p^ := $C0 + (Ord(SourceReg) * 8) + Ord(DestReg); Inc(p);
end;

procedure TJitEmitter.MULSD_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin
  p^ := $F2; Inc(p); p^ := $0F; Inc(p); p^ := $59; Inc(p); // MULSD xmm, xmm
  p^ := $C0 + (Ord(SourceReg) * 8) + Ord(DestReg); Inc(p);
end;

// Emits: subsd dest_xmm, src_xmm
procedure TJitEmitter.SUBSD_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin
  WriteBytes([$F2, $0F, $5C, $C0 + (Ord(SourceReg) * 8) + Ord(DestReg)]);
end;

// Emits: divsd dest_xmm, src_xmm
procedure TJitEmitter.DIVSD_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin
  WriteBytes([$F2, $0F, $5E, $C0 + (Ord(SourceReg) * 8) + Ord(DestReg)]);
end;

// Emits: xorps dest_xmm, src_xmm (Used to zero out a register, e.g., XORPS XMM0, XMM0)
// This is the fastest way to set an XMM register to zero.
procedure TJitEmitter.XORPS_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin
  // No F2/F3 prefix is needed for this instruction.
  WriteBytes([$0F, $57, $C0 + (Ord(SourceReg) * 8) + Ord(DestReg)]);
end;

// Emits: ucomisd dest_xmm, src_xmm (Compare two doubles)
// Sets the CPU's flags (ZF, PF, CF) based on the comparison.
// These flags are then used by conditional jump instructions (je, jb, etc.).
procedure TJitEmitter.UCOMISD_XMM_XMM(Reg1, Reg2: EXMMReg);
begin
  WriteBytes([$66, $0F, $2E, $C0 + (Ord(Reg2) * 8) + Ord(Reg1)]);
end;

// --- High-Level Operand Helpers ---

procedure TJitEmitter.Load_Int_Operand(const arg: TBytecodeInstructionData; Reg: EReg);
begin
  if arg.Pos = mpLocal then
    case BaseJITType(arg.BaseType) of
      xtInt8:  MOVZX_Reg_Mem_i8(Reg, rbx, arg.Data.Addr);
      xtInt16: MOVZX_Reg_Mem_i16(Reg, rbx, arg.Data.Addr);
      xtInt32: MOV_Reg_Mem_i32(Reg, rbx, arg.Data.Addr);
      xtInt64: MOV_Reg_Mem_i64(Reg, rbx, arg.Data.Addr);
    end
  else
    MOV_Reg_Imm64(Reg, arg.Data.arg);
end;

procedure TJitEmitter.Store_Int_Result(const arg: TBytecodeInstructionData; Reg: EReg);
begin
  case BaseJITType(arg.BaseType) of
    xtInt8:  MOV_Mem_Reg_i8(rbx, arg.Data.Addr, Reg);
    xtInt16: MOV_Mem_Reg_i16(rbx, arg.Data.Addr, Reg);
    xtInt32: MOV_Mem_Reg_i32(rbx, arg.Data.Addr, Reg);
    xtInt64: MOV_Mem_Reg_i64(rbx, arg.Data.Addr, Reg);
  end;
end;

procedure TJitEmitter.Load_Float_Operand(const arg: TBytecodeInstructionData; Reg: EXMMReg);
begin
  if arg.Pos = mpLocal then
    case BaseJITType(arg.BaseType) of
      xtSingle: MOVSS_XMM_Mem(Reg, rbx, arg.Data.Addr);
      xtDouble: MOVSD_XMM_Mem(Reg, rbx, arg.Data.Addr);
    end
  else
  begin
    // mov xmm, imm64 is tricky. Load to GPR then move to XMM.
    MOV_Reg_Imm64(rax, arg.Data.arg);
    // movq xmm, rax
    p^ := $66; Inc(p); p^ := $48; Inc(p); p^ := $0F; Inc(p); p^ := $6E; Inc(p);
    p^ := $C0 + (Ord(Reg) * 8) + Ord(rax); Inc(p);
  end;
end;

procedure TJitEmitter.Store_Float_Result(const arg: TBytecodeInstructionData; Reg: EXMMReg);
begin
  case BaseJITType(arg.BaseType) of
    xtSingle: MOVSS_Mem_XMM(rbx, arg.Data.Addr, Reg);
    xtDouble: MOVSD_Mem_XMM(rbx, arg.Data.Addr, Reg);
  end;
end;



end.

