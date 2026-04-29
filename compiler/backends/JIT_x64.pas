unit JIT_x64;
{
  Copyright 2026 Jarl K. Holta

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}
{
  JIT_x64 is a core unit for the Express x64 JIT backend to simply build
  x64 assembler on the go. If there are minimalist FPC units out there that
  already covers all these basics but for more backends then i am interested.

  Doing 1 backend isnt too much work, doing 2 or more is timeconsuming and tricky
}

{$I header.inc}
{$hints off}

interface

uses
  SysUtils,
  xpr.Types,
  xpr.Bytecode;

type
  EJccCondition = (
    jo, jno, jb, jae, je, jne, jbe, ja, js, jns, jp, jnp, jl, jge, jle, jg
  );

  ESetccCondition = (
    seto, setno, setb, setae, sete, setne, setbe, seta,
    sets, setns, setp, setnp, setl, setge, setle, setg
  );

  EReg = (
    rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi {,r8..r15}
  );

  EXMMReg = (
    xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7
  );
  TXMMRegSet = set of EXMMReg;

  TJitEmitter = record
    p: PByte;

    // We should track actual size, and never allow direct Inc(p)
    // We can even store the size as a field in the pointer itself, effectively
    // Make it a TByteArray that is build BEFORE we allocate it as executable.
    //
    // Even after making it exec we actually keep the length stored in mem as
    // part of the exe, but share address offset by SizeOf(SizeInt). Nice for
    // Any moves, and mergers that may come later.

    ConstBase: PByte; // Cache the constants table pointer for compile-time baking
    procedure WriteBytes(const Bytes: array of Byte);
    procedure WriteBytes(AValue: Pointer; ASize: NativeInt); overload;
    procedure Init(StartPtr: PByte; AConstBase: PByte);
    function  CurrentPosition: PByte;

    // Control
    procedure Preamble;
    procedure Epilogue;
    procedure PreserveGPR;
    procedure RecoverGPR;
    procedure EmitRex(Reg, RM: EReg; W, Is8Bit: Boolean);
    procedure EmitModRM(Reg, RM: EReg; Disp32: Boolean);
    procedure EmitMem(Reg, Base: EReg; Offset: Int32);

    procedure RET;
    procedure NOP;

    // Integer moves
    procedure MOV_Reg_Imm64(Reg: EReg; Value: Int64);
    procedure MOV_Reg_Imm32(Reg: EReg; Value: Int32);
    procedure MOV_Reg_Reg(DestReg, SourceReg: EReg);
    procedure MOV_Reg_Mem_i8(Reg, BaseReg: EReg; Offset: Int64);
    procedure MOVZX_Reg_Mem_i8(Reg, BaseReg: EReg; Offset: Int64);
    procedure MOVZX_Reg_Mem_i16(Reg, BaseReg: EReg; Offset: Int64);
    procedure MOVSX_Reg_Mem_i8(Reg, BaseReg: EReg; Offset: Int64);
    procedure MOVSX_Reg_Mem_i16(Reg, BaseReg: EReg; Offset: Int64);
    procedure MOVSXD_Reg_Mem_i32(Reg, BaseReg: EReg; Offset: Int64);
    procedure MOVZX_Reg_Reg_u8(DestReg: EReg);
    procedure MOV_Reg_Mem_i32(Reg, BaseReg: EReg; Offset: Int64);
    procedure MOV_Reg_Mem_i64(Reg, BaseReg: EReg; Offset: Int64);
    procedure MOV_Mem_Reg_i8(BaseReg: EReg; Offset: Int64; Reg: EReg);
    procedure MOV_Mem_Reg_i16(BaseReg: EReg; Offset: Int64; Reg: EReg);
    procedure MOV_Mem_Reg_i32(BaseReg: EReg; Offset: Int64; Reg: EReg);
    procedure MOV_Mem_Reg_i64(BaseReg: EReg; Offset: Int64; Reg: EReg);
    procedure MOV_RegAddr_Reg_i8(AddrReg, SourceReg: EReg);
    procedure MOV_RegAddr_Reg_i16(AddrReg, SourceReg: EReg);
    procedure MOV_RegAddr_Reg_i32(AddrReg, SourceReg: EReg);
    procedure MOV_RegAddr_Reg_i64(AddrReg, SourceReg: EReg);

    // Integer arithmetic
    procedure CQO;
    procedure INC_Mem8(BaseReg: EReg; Offset: Int64);
    procedure INC_Mem16(BaseReg: EReg; Offset: Int64);
    procedure INC_Mem32(BaseReg: EReg; Offset: Int64);
    procedure INC_Mem64(BaseReg: EReg; Offset: Int64);
    procedure INC_Reg(Reg: EReg);
    procedure ADD_Reg_Reg(DestReg, SourceReg: EReg);
    procedure ADD_Reg_Imm32(Reg: EReg; Value: Int32);
    procedure IMUL_Reg_Reg(DestReg, SourceReg: EReg);
    procedure IMUL_Reg_Reg_Imm32(DestReg, SourceReg: EReg; Value: Int32);
    procedure SUB_Reg_Reg(DestReg, SourceReg: EReg);
    procedure IDIV_Reg(SourceReg: EReg);
    procedure DIV_Reg(SourceReg: EReg);
    procedure AND_Reg_Reg(DestReg, SourceReg: EReg);
    procedure OR_Reg_Reg(DestReg, SourceReg: EReg);
    procedure XOR_Reg_Reg(DestReg, SourceReg: EReg);
    procedure SHL_Reg_CL(DestReg: EReg);
    procedure SHR_Reg_CL(DestReg: EReg);
    procedure SAR_Reg_CL(DestReg: EReg);
    procedure LEA_Reg_Mem(DestReg, BaseReg, IndexReg: EReg; Scale: Byte);

    // Float moves
    procedure MOVSD_XMM_XMM(DestReg, SourceReg: EXMMReg);
    procedure MOVSS_XMM_XMM(DestReg, SourceReg: EXMMReg);
    procedure MOVSD_RegAddr_XMM(AddrReg: EReg; SourceReg: EXMMReg);
    procedure MOVSS_RegAddr_XMM(AddrReg: EReg; SourceReg: EXMMReg);
    procedure MOVSD_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
    procedure MOVSS_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
    procedure MOVSD_Mem_XMM(BaseReg: EReg; Offset: Int64; SourceReg: EXMMReg);
    procedure MOVSS_Mem_XMM(BaseReg: EReg; Offset: Int64; SourceReg: EXMMReg);
    procedure CVTSS2SD_XMM_XMM(DestReg, SourceReg: EXMMReg);
    procedure CVTSD2SS_XMM_XMM(DestReg, SourceReg: EXMMReg);

    // Float arithmetic
    procedure ADDSD_XMM_XMM(D,S: EXMMReg);
    procedure MULSD_XMM_XMM(D,S: EXMMReg);
    procedure SUBSD_XMM_XMM(D,S: EXMMReg);
    procedure DIVSD_XMM_XMM(D,S: EXMMReg);
    procedure ADDSD_XMM_Mem(D: EXMMReg; B: EReg; O: Int64);
    procedure MULSD_XMM_Mem(D: EXMMReg; B: EReg; O: Int64);
    procedure SUBSD_XMM_Mem(D: EXMMReg; B: EReg; O: Int64);
    procedure DIVSD_XMM_Mem(D: EXMMReg; B: EReg; O: Int64);

    procedure ADDSS_XMM_XMM(D, S: EXMMReg);
    procedure MULSS_XMM_XMM(D, S: EXMMReg);
    procedure SUBSS_XMM_XMM(D, S: EXMMReg);
    procedure DIVSS_XMM_XMM(D, S: EXMMReg);
    procedure ADDSS_XMM_Mem(D: EXMMReg; B: EReg; O: Int64);
    procedure MULSS_XMM_Mem(D: EXMMReg; B: EReg; O: Int64);
    procedure SUBSS_XMM_Mem(D: EXMMReg; B: EReg; O: Int64);
    procedure DIVSS_XMM_Mem(D: EXMMReg; B: EReg; O: Int64);

    procedure ADDSX_XMM_XMM(D, S: EXMMReg; T: EExpressBaseType);
    procedure MULSX_XMM_XMM(D, S: EXMMReg; T: EExpressBaseType);
    procedure SUBSX_XMM_XMM(D, S: EXMMReg; T: EExpressBaseType);
    procedure DIVSX_XMM_XMM(D, S: EXMMReg; T: EExpressBaseType);
    procedure ADDSX_XMM_Mem(D: EXMMReg; B: EReg; O: Int64; T: EExpressBaseType);
    procedure MULSX_XMM_Mem(D: EXMMReg; B: EReg; O: Int64; T: EExpressBaseType);
    procedure SUBSX_XMM_Mem(D: EXMMReg; B: EReg; O: Int64; T: EExpressBaseType);
    procedure DIVSX_XMM_Mem(D: EXMMReg; B: EReg; O: Int64; T: EExpressBaseType);

    procedure XORPS_XMM_XMM(D, S: EXMMReg);
    procedure UCOMISD_XMM_XMM(Reg1, Reg2: EXMMReg);
    procedure UCOMISS_XMM_XMM(Reg1, Reg2: EXMMReg);
    procedure CVTSI2SD_XMM_Reg(DestReg: EXMMReg; SourceReg: EReg);
    procedure CVTSI2SS_XMM_Reg32(DestReg: EXMMReg; SourceReg: EReg);
    procedure CVTSI2SS_XMM_Reg64(DestReg: EXMMReg; SourceReg: EReg);

    // Control flow
    procedure CMP_Reg_Imm32(Reg: EReg; Value: Int32);
    procedure CMP_Reg_Reg(Reg1, Reg2: EReg);
    procedure TEST_Reg_Reg(Reg1, Reg2: EReg);
    procedure TEST_Reg8_Reg8(Reg: EReg);
    function  Jcc_Rel32(Condition: EJccCondition): PInt32;
    function  JMP_Rel32: PInt32;
    procedure SETcc(Condition: ESetccCondition; DestReg: EReg);

    // High-level helpers
    procedure Load_Int_Operand(const arg: TOperand; Reg: EReg);
    procedure Store_Int_Result(const arg: TOperand; Reg: EReg);
    procedure Load_Float_Operand(const arg: TOperand; Reg: EXMMReg; TmpReg: EReg = rbp);
    procedure Store_Float_Result(const arg: TOperand; Reg: EXMMReg);
    procedure Store_Int_Result_To_Addr(const arg: TOperand; ValueReg, AddrReg: EReg);
    procedure Store_Float_Result_To_Addr(const arg: TOperand; ValueReg: EXMMReg; AddrReg: EReg);
  end;
  PJitEmitter = ^TJitEmitter;

  TXMMRegState = record
    IsDirty: Boolean;
    VarArg:  TOperand;
  end;

  TVarFrequency = record
    Count:   Integer;
    VarInfo: TOperand;
  end;
  TVarFrequencyArray = array of TVarFrequency;

  TVIPMapEntry = record
    VarInfo: TOperand;
    Reg:     EXMMReg;
  end;
  TVIPMap = array of TVIPMapEntry;

  TXMMRegisterAllocator = record
  private
    Regs:         array[EXMMReg] of TXMMRegState;
    VIPMap:       TVIPMap;
    Emitter:      PJitEmitter;
    SpillCounter: Integer;

    procedure Unset(Reg: EXMMReg);
    function  Isset(Reg: EXMMReg): Boolean;
    function  isEqual(Reg: EXMMReg; Operand: TOperand): Boolean;
    procedure Spill(Reg: EXMMReg);
    function  FindVIP(Operand: TOperand; out Reg: EXMMReg): Boolean;
    function  FindFree: EXMMReg;
    function  Evict(Exclude: TXMMRegSet = []): EXMMReg;
  public
    procedure Init(AEmitter: PJitEmitter);
    procedure AnalyzeTrace(CodeList: PBytecodeInstruction; Count: Integer; Settings: PCompilerSettings);
    function  FindReg(Offset: PtrInt; out Reg: EXMMReg): Boolean;
    function  GetFreeScratch(MarkAsUsed: Boolean = False; Exclude: TXMMRegSet = []): EXMMReg;
    function  GetReg(const arg: TOperand; Exclude: TXMMRegSet = []): EXMMReg;
    function  Allocate(): EXMMReg;
    procedure SetResult(Reg: EXMMReg; const dest_arg: TOperand);
    procedure SpillAllDirty;
    procedure ClearAllConst;
    procedure InvalidateAll;
    procedure InvalidateAllIncludingVIP;
  end;

  TGPRSlot = record
    IsSet  : Boolean;
    IsDirty: Boolean;
    Offset : Int64;
    Pos    : EMemPos;
    BType  : EExpressBaseType;
  end;

  TGPRRegAllocator = record
  private
    Slots     : array[0..4] of TGPRSlot;
    VIPCount  : Integer;
    VIPOffsets: array[0..1] of Int64;
    VIPPos    : array[0..1] of EMemPos;
    VIPBTypes : array[0..1] of EExpressBaseType;
    Emitter   : PJitEmitter;

    function  IdxOf(Reg: EReg): Integer; inline;
    function  FindCached(Offset: Int64; APos: EMemPos): Integer;
    function  VIPSlotOf(Offset: Int64; APos: EMemPos): Integer;
    procedure SpillSlot(Idx: Integer);
    procedure ClearSlotAt(Idx: Integer);
    function  AllocSlot(ExclMask: Byte): Integer;
  public
    procedure Init(AEmitter: PJitEmitter);
    procedure AnalyzeTrace(CodeList: PBytecodeInstruction; Count: Integer; Settings: PCompilerSettings);
    function  GetReg(const arg: TOperand; ExclMask: Byte = 0): EReg;
    function  GetResultReg(const dest: TOperand; ExclMask: Byte = 0): EReg;
    function  GetScratch(ExclMask: Byte = 0): EReg;
    procedure SetResult(Reg: EReg; const dest: TOperand);
    procedure SpillAll;
    procedure InvalidateAll;
    procedure InvalidateAllIncludingVIP;
    procedure ClearSlot(Reg: EReg);
    procedure ForceEvict(Reg: EReg);
    function  MaskOf(Reg: EReg): Byte; inline;
    function  IsVIPReg(Reg: EReg): Boolean;
    function  IsSlotDirty(Reg: EReg): Boolean;
  end;

const
  PROT_READ  = $1;
  PROT_WRITE = $2;
  PROT_EXEC  = $4;
  MAP_PRIVATE   = $0002;
  MAP_ANONYMOUS = $1000;
  MAP_FAILED    = Pointer(-1);

function BaseJITType(BaseType: EExpressBaseType): EExpressBaseType;
function AllocateExecutableMemory(Size: SizeInt): Pointer;
function SetMemoryExecutable(Address: Pointer; Size: SizeInt): Boolean;
procedure FreeExecutableMemory(Address: Pointer; Size: SizeInt);

implementation

uses
  xpr.Utils, Math, ctypes
  {$IFDEF WINDOWS}, Windows {$ENDIF}
  {$IFDEF UNIX}, BaseUnix, SysCall {$ENDIF};

const
  GPR_POOL: array[0..4] of EReg = (rax, rcx, rdx, rsi, rdi);
  GPR_VIP_IDX_START = 3;

// ---------------------------------------------------------------------------

function BaseJITType(BaseType: EExpressBaseType): EExpressBaseType;
begin
  case BaseType of
    xtSingle, xtDouble: Result := BaseType;
    xtInt8..xtInt64:    Result := BaseType;
    xtUInt8..xtUInt64:  Result := BaseType;
    xtAnsiChar:         Result := xtInt8;
    xtUnicodeChar:      Result := xtInt16;
    xtBool:             Result := xtUInt8;
    xtPointer:          Result := xtInt64;
    xtArray:            Result := xtInt64;
    xtAnsiString:       Result := xtInt64;
    xtUnicodeString:    Result := xtInt64;
    xtClass, xtMethod:  Result := xtInt64;
  else
    Result := xtUnknown;
  end;
end;

function AllocateExecutableMemory(Size: SizeInt): Pointer;
const PAGE_SIZE = 4096;
begin
  Result := nil;
  if Size = 0 then Exit;
  {$IFDEF WINDOWS}
  Result := VirtualAlloc(nil, Size, MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE);
  {$ELSE}
  Size   := ((Size + PAGE_SIZE - 1) div PAGE_SIZE) * PAGE_SIZE;
  Result := fpmmap(nil, Size, PROT_READ or PROT_WRITE, MAP_PRIVATE+MAP_ANON, -1, 0);
  if Result = MAP_FAILED then begin WriteLn('JIT mmap failed: ', fpgeterrno); Result := nil; end;
  {$ENDIF}
end;

function SetMemoryExecutable(Address: Pointer; Size: SizeInt): Boolean;
{$IFDEF WINDOWS} var OldProtect: DWORD; {$ENDIF}
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
  {$IFDEF WINDOWS} VirtualFree(Address, 0, MEM_RELEASE);
  {$ELSE}          fpmunmap(Address, Size);
  {$ENDIF}
end;

// ===========================================================================
// TJitEmitter
// ===========================================================================

procedure TJitEmitter.Init(StartPtr: PByte; AConstBase: PByte); 
begin 
  p := StartPtr; 
  ConstBase := AConstBase; 
end;
function  TJitEmitter.CurrentPosition: PByte; begin Result := p; end;

procedure TJitEmitter.WriteBytes(const Bytes: array of Byte);
var len: Integer;
begin
  len := Length(Bytes);
  if len > 0 then begin System.Move(Bytes[0], p^, len); Inc(p, len); end;
end;

procedure TJitEmitter.WriteBytes(AValue: Pointer; ASize: NativeInt);
begin
  if ASize > 0 then begin System.Move(AValue^, p^, ASize); Inc(p, ASize); end;
end;

// Helper for minimal GPR presevation (Windows, over preserves on Linux, which still ofc works)
procedure TJitEmitter.PreserveGPR;
begin
  WriteBytes([$53,$55,$56,$57]);  // 4 non REX must be preserved.
end;

procedure TJitEmitter.RecoverGPR;
begin
  WriteBytes([$5F,$5E,$5D,$5B]);  // 4 non REX must be recovered.
end;

// General preamble for the JIT, highly conservative, not really needed to be
// this conservative..
procedure TJitEmitter.Preamble;
begin
  WriteBytes([$50,$51,$52,$53,$55,$56,$57]);  // Push 7 regs (56 bytes) rax, rcx, rdx, rbx, rbp, rsi, rdi
  {$IFDEF WINDOWS}
  WriteBytes([$48, $83, $EC, $40]);           // sub rsp, 64 (32 for XMM + 32 shadow)
  WriteBytes([$0F, $29, $74, $24, $20]);      // movaps [rsp+32], xmm6
  WriteBytes([$0F, $29, $7C, $24, $30]);      // movaps [rsp+48], xmm7
  WriteBytes([$48, $89, $CB]);                // mov rbx, rcx
  {$ELSE}
  WriteBytes([$48, $89, $FB]);                // mov rbx, rdi
  {$ENDIF}
end;

// This mirrors the current Preamble
procedure TJitEmitter.Epilogue;
begin
  {$IFDEF WINDOWS}
  WriteBytes([$0F, $28, $74, $24, $20]);   // movaps xmm6, [rsp+32]
  WriteBytes([$0F, $28, $7C, $24, $30]);   // movaps xmm7, [rsp+48]
  WriteBytes([$48, $83, $C4, $40]);        // add rsp, 64
  {$ENDIF}
  WriteBytes([$5F,$5E,$5D,$5B,$5A,$59]);   // pop rdi,rsi,rbp,rbx,rdx,rcx  (no pop rax)
  WriteBytes([$48,$83,$C4,$08]);           // add rsp, 8  (skip the pushed rax slot)
  WriteBytes([$C3]);                       // ret
end;


(*
  The JIT currently does not really support REX, it's on the todo - but I have
  neglected it as using REX registers are usually more costly - requires
  smarter handling, and a whole set of tests once more.
*)
procedure TJitEmitter.EmitRex(Reg, RM: EReg; W: Boolean; Is8Bit: Boolean);
var rex: Byte; need: Boolean;
begin
  rex := $40;
  if W then rex := rex or $08;
  if Ord(Reg) >= 8 then rex := rex or $04;
  if Ord(RM)  >= 8 then rex := rex or $01;
  need := (rex <> $40) or W or (Is8Bit and ((Ord(Reg) >= 4) or (Ord(RM) >= 4)));
  if need then WriteBytes([rex]);
end;

procedure TJitEmitter.EmitModRM(Reg, RM: EReg; Disp32: Boolean);
var modrm: Byte; rmLo: Byte;
begin
  rmLo := Ord(RM) and 7;
  if Disp32 then modrm := $80 else modrm := $00;
  modrm := modrm or ((Ord(Reg) and 7) shl 3) or rmLo;
  WriteBytes([modrm]);
  if rmLo = 4 then WriteBytes([$24]); // SIB
end;

procedure TJitEmitter.EmitMem(Reg, Base: EReg; Offset: Int32);
begin
  EmitModRM(Reg, Base, True);
  WriteBytes(@Offset, 4);
end;

procedure TJitEmitter.RET; begin WriteBytes([$C3]); end;
procedure TJitEmitter.NOP; begin WriteBytes([$90]); end;

procedure TJitEmitter.MOV_Reg_Imm64(Reg: EReg; Value: Int64);
begin WriteBytes([$48, $B8+Ord(Reg)]); WriteBytes(@Value, 8); end;

procedure TJitEmitter.MOV_Reg_Imm32(Reg: EReg; Value: Int32);
begin WriteBytes([$B8+Ord(Reg)]); WriteBytes(@Value, 4); end;

procedure TJitEmitter.MOV_Reg_Reg(DestReg, SourceReg: EReg);
begin WriteBytes([$48,$89, $C0+(Ord(SourceReg) shl 3)+Ord(DestReg)]); end;

procedure TJitEmitter.MOV_Reg_Mem_i8(Reg, BaseReg: EReg; Offset: Int64);
begin
  EmitRex(Reg, BaseReg, False, True);
  WriteBytes([$8A]);
  EmitMem(Reg, BaseReg, Offset);
end;

procedure TJitEmitter.MOVZX_Reg_Mem_i8(Reg, BaseReg: EReg; Offset: Int64);
begin WriteBytes([$48,$0F,$B6, $80+(Ord(Reg) shl 3)+Ord(BaseReg)]); WriteBytes(@Offset,4); end;

procedure TJitEmitter.MOVZX_Reg_Mem_i16(Reg, BaseReg: EReg; Offset: Int64);
begin WriteBytes([$48,$0F,$B7, $80+(Ord(Reg) shl 3)+Ord(BaseReg)]); WriteBytes(@Offset,4); end;

procedure TJitEmitter.MOVSX_Reg_Mem_i8(Reg, BaseReg: EReg; Offset: Int64);
begin WriteBytes([$48,$0F,$BE, $80+(Ord(Reg) shl 3)+Ord(BaseReg)]); WriteBytes(@Offset,4); end;

procedure TJitEmitter.MOVSX_Reg_Mem_i16(Reg, BaseReg: EReg; Offset: Int64);
begin WriteBytes([$48,$0F,$BF, $80+(Ord(Reg) shl 3)+Ord(BaseReg)]); WriteBytes(@Offset,4); end;

procedure TJitEmitter.MOVZX_Reg_Reg_u8(DestReg: EReg);
begin WriteBytes([$48,$0F,$B6, $C0+(Ord(DestReg) shl 3)+Ord(DestReg)]); end;

procedure TJitEmitter.MOV_Reg_Mem_i32(Reg, BaseReg: EReg; Offset: Int64);
begin WriteBytes([$8B, $80+(Ord(Reg) shl 3)+Ord(BaseReg)]); WriteBytes(@Offset,4); end;

procedure TJitEmitter.MOV_Reg_Mem_i64(Reg, BaseReg: EReg; Offset: Int64);
begin WriteBytes([$48,$8B, $80+(Ord(Reg) shl 3)+Ord(BaseReg)]); WriteBytes(@Offset,4); end;

procedure TJitEmitter.MOVSXD_Reg_Mem_i32(Reg, BaseReg: EReg; Offset: Int64);
begin WriteBytes([$48,$63, $80+(Ord(Reg) shl 3)+Ord(BaseReg)]); WriteBytes(@Offset,4); end;

procedure TJitEmitter.MOV_Mem_Reg_i8(BaseReg: EReg; Offset: Int64; Reg: EReg);
begin
  EmitRex(Reg, BaseReg, False, True);
  WriteBytes([$88]);
  EmitMem(Reg, BaseReg, Offset);
end;

procedure TJitEmitter.MOV_Mem_Reg_i16(BaseReg: EReg; Offset: Int64; Reg: EReg);
var rex: Byte;
begin
  rex := $40;
  if Ord(Reg) >= 8 then rex := rex or $04;
  if Ord(BaseReg) >= 8 then rex := rex or $01;
  if rex <> $40 then WriteBytes([rex]);
  WriteBytes([$66, $89, $80 or ((Ord(Reg) and 7) shl 3) or (Ord(BaseReg) and 7)]);
  WriteBytes(@Offset, 4);
end;

procedure TJitEmitter.MOV_Mem_Reg_i32(BaseReg: EReg; Offset: Int64; Reg: EReg);
var rex: Byte;
begin
  rex := $40;
  if Ord(Reg) >= 8 then rex := rex or $04;
  if Ord(BaseReg) >= 8 then rex := rex or $01;
  if rex <> $40 then WriteBytes([rex]);
  WriteBytes([$89, $80 or ((Ord(Reg) and 7) shl 3) or (Ord(BaseReg) and 7)]);
  WriteBytes(@Offset, 4);
end;

procedure TJitEmitter.MOV_Mem_Reg_i64(BaseReg: EReg; Offset: Int64; Reg: EReg);
var rex: Byte;
begin
  rex := $48; // W=1
  if Ord(Reg) >= 8 then rex := rex or $04;
  if Ord(BaseReg) >= 8 then rex := rex or $01;
  WriteBytes([rex]);
  WriteBytes([$89, $80+(Ord(Reg) shl 3)+Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

procedure TJitEmitter.MOV_RegAddr_Reg_i8(AddrReg, SourceReg: EReg);
begin WriteBytes([$40, $88, $00+(Ord(SourceReg) shl 3)+Ord(AddrReg)]); end;

procedure TJitEmitter.MOV_RegAddr_Reg_i16(AddrReg, SourceReg: EReg);
begin WriteBytes([$66,$89, $00+(Ord(SourceReg) shl 3)+Ord(AddrReg)]); end;

procedure TJitEmitter.MOV_RegAddr_Reg_i32(AddrReg, SourceReg: EReg);
begin WriteBytes([$89, $00+(Ord(SourceReg) shl 3)+Ord(AddrReg)]); end;

procedure TJitEmitter.MOV_RegAddr_Reg_i64(AddrReg, SourceReg: EReg);
begin WriteBytes([$48,$89, $00+(Ord(SourceReg) shl 3)+Ord(AddrReg)]); end;

procedure TJitEmitter.CQO; begin WriteBytes([$48,$99]); end;

procedure TJitEmitter.INC_Mem8(BaseReg: EReg; Offset: Int64);
begin WriteBytes([$FE, $80+Ord(BaseReg)]); WriteBytes(@Offset,4); end;

procedure TJitEmitter.INC_Mem16(BaseReg: EReg; Offset: Int64);
begin WriteBytes([$66,$FF, $80+Ord(BaseReg)]); WriteBytes(@Offset,4); end;

procedure TJitEmitter.INC_Mem32(BaseReg: EReg; Offset: Int64);
begin WriteBytes([$FF, $80+Ord(BaseReg)]); WriteBytes(@Offset,4); end;

procedure TJitEmitter.INC_Mem64(BaseReg: EReg; Offset: Int64);
begin WriteBytes([$48,$FF, $80+Ord(BaseReg)]); WriteBytes(@Offset,4); end;

procedure TJitEmitter.INC_Reg(Reg: EReg);
begin WriteBytes([$48,$FF, $C0+Ord(Reg)]); end;

procedure TJitEmitter.ADD_Reg_Reg(DestReg, SourceReg: EReg);
begin WriteBytes([$48,$01, $C0+(Ord(SourceReg) shl 3)+Ord(DestReg)]); end;

procedure TJitEmitter.ADD_Reg_Imm32(Reg: EReg; Value: Int32);
begin
  if (Value >= -128) and (Value <= 127) then
    WriteBytes([$48,$83, $C0+Ord(Reg), Byte(Int8(Value))])
  else if Reg = rax then
    begin WriteBytes([$48,$05]); WriteBytes(@Value,4); end
  else
    begin WriteBytes([$48,$81, $C0+Ord(Reg)]); WriteBytes(@Value,4); end;
end;

procedure TJitEmitter.IMUL_Reg_Reg(DestReg, SourceReg: EReg);
begin WriteBytes([$48,$0F,$AF, $C0+(Ord(DestReg) shl 3)+Ord(SourceReg)]); end;

procedure TJitEmitter.IMUL_Reg_Reg_Imm32(DestReg, SourceReg: EReg; Value: Int32);
begin WriteBytes([$48,$69, $C0+(Ord(DestReg) shl 3)+Ord(SourceReg)]); WriteBytes(@Value,4); end;

procedure TJitEmitter.SUB_Reg_Reg(DestReg, SourceReg: EReg);
begin WriteBytes([$48,$29, $C0+(Ord(SourceReg) shl 3)+Ord(DestReg)]); end;

procedure TJitEmitter.IDIV_Reg(SourceReg: EReg);
begin WriteBytes([$48,$F7, $F8+Ord(SourceReg)]); end;

procedure TJitEmitter.DIV_Reg(SourceReg: EReg);
begin WriteBytes([$48,$F7, $F0+Ord(SourceReg)]); end;

procedure TJitEmitter.AND_Reg_Reg(DestReg, SourceReg: EReg);
begin WriteBytes([$48,$21, $C0+(Ord(SourceReg) shl 3)+Ord(DestReg)]); end;

procedure TJitEmitter.OR_Reg_Reg(DestReg, SourceReg: EReg);
begin WriteBytes([$48,$09, $C0+(Ord(SourceReg) shl 3)+Ord(DestReg)]); end;

procedure TJitEmitter.XOR_Reg_Reg(DestReg, SourceReg: EReg);
begin WriteBytes([$48,$31, $C0+(Ord(SourceReg) shl 3)+Ord(DestReg)]); end;

procedure TJitEmitter.SHL_Reg_CL(DestReg: EReg); begin WriteBytes([$48,$D3,$E0+Ord(DestReg)]); end;
procedure TJitEmitter.SHR_Reg_CL(DestReg: EReg); begin WriteBytes([$48,$D3,$E8+Ord(DestReg)]); end;
procedure TJitEmitter.SAR_Reg_CL(DestReg: EReg); begin WriteBytes([$48,$D3,$F8+Ord(DestReg)]); end;

procedure TJitEmitter.LEA_Reg_Mem(DestReg, BaseReg, IndexReg: EReg; Scale: Byte);
var sb: Byte;
begin
  case Scale of 1:sb:=0; 2:sb:=1; 4:sb:=2; 8:sb:=3;
  else raise Exception.Create('Invalid LEA scale');
  end;
  WriteBytes([$48,$8D, $04+(Ord(DestReg) shl 3),
              (sb shl 6)+(Ord(IndexReg) shl 3)+Ord(BaseReg)]);
end;

procedure TJitEmitter.MOVSD_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin WriteBytes([$F2,$0F,$10, $C0+(Ord(DestReg) shl 3)+Ord(SourceReg)]); end;

procedure TJitEmitter.MOVSS_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin WriteBytes([$F3,$0F,$10, $C0+(Ord(DestReg) shl 3)+Ord(SourceReg)]); end;

procedure TJitEmitter.MOVSD_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
begin WriteBytes([$F2,$0F,$10, $80+(Ord(DestReg) shl 3)+Ord(BaseReg)]); WriteBytes(@Offset,4); end;

procedure TJitEmitter.MOVSS_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
begin WriteBytes([$F3,$0F,$10, $80+(Ord(DestReg) shl 3)+Ord(BaseReg)]); WriteBytes(@Offset,4); end;

procedure TJitEmitter.MOVSD_Mem_XMM(BaseReg: EReg; Offset: Int64; SourceReg: EXMMReg);
begin WriteBytes([$F2,$0F,$11, $80+(Ord(SourceReg) shl 3)+Ord(BaseReg)]); WriteBytes(@Offset,4); end;

procedure TJitEmitter.MOVSS_Mem_XMM(BaseReg: EReg; Offset: Int64; SourceReg: EXMMReg);
begin WriteBytes([$F3,$0F,$11, $80+(Ord(SourceReg) shl 3)+Ord(BaseReg)]); WriteBytes(@Offset,4); end;

procedure TJitEmitter.MOVSD_RegAddr_XMM(AddrReg: EReg; SourceReg: EXMMReg);
begin WriteBytes([$F2,$0F,$11, $00+(Ord(SourceReg) shl 3)+Ord(AddrReg)]); end;

procedure TJitEmitter.MOVSS_RegAddr_XMM(AddrReg: EReg; SourceReg: EXMMReg);
begin WriteBytes([$F3,$0F,$11, $00+(Ord(SourceReg) shl 3)+Ord(AddrReg)]); end;

procedure TJitEmitter.CVTSS2SD_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin WriteBytes([$F3,$0F,$5A, $C0+(Ord(DestReg) shl 3)+Ord(SourceReg)]); end;

procedure TJitEmitter.CVTSD2SS_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin WriteBytes([$F2,$0F,$5A, $C0+(Ord(DestReg) shl 3)+Ord(SourceReg)]); end;

procedure TJitEmitter.ADDSD_XMM_XMM(D,S: EXMMReg); begin WriteBytes([$F2,$0F,$58,$C0+(Ord(D) shl 3)+Ord(S)]); end;
procedure TJitEmitter.MULSD_XMM_XMM(D,S: EXMMReg); begin WriteBytes([$F2,$0F,$59,$C0+(Ord(D) shl 3)+Ord(S)]); end;
procedure TJitEmitter.SUBSD_XMM_XMM(D,S: EXMMReg); begin WriteBytes([$F2,$0F,$5C,$C0+(Ord(D) shl 3)+Ord(S)]); end;
procedure TJitEmitter.DIVSD_XMM_XMM(D,S: EXMMReg); begin WriteBytes([$F2,$0F,$5E,$C0+(Ord(D) shl 3)+Ord(S)]); end;

procedure TJitEmitter.ADDSD_XMM_Mem(D: EXMMReg; B: EReg; O: Int64);
begin WriteBytes([$F2,$0F,$58,$80+(Ord(D) shl 3)+Ord(B)]); WriteBytes(@O,4); end;
procedure TJitEmitter.MULSD_XMM_Mem(D: EXMMReg; B: EReg; O: Int64);
begin WriteBytes([$F2,$0F,$59,$80+(Ord(D) shl 3)+Ord(B)]); WriteBytes(@O,4); end;
procedure TJitEmitter.SUBSD_XMM_Mem(D: EXMMReg; B: EReg; O: Int64);
begin WriteBytes([$F2,$0F,$5C,$80+(Ord(D) shl 3)+Ord(B)]); WriteBytes(@O,4); end;
procedure TJitEmitter.DIVSD_XMM_Mem(D: EXMMReg; B: EReg; O: Int64);
begin WriteBytes([$F2,$0F,$5E,$80+(Ord(D) shl 3)+Ord(B)]); WriteBytes(@O,4); end;

procedure TJitEmitter.ADDSS_XMM_XMM(D,S: EXMMReg); begin WriteBytes([$F3,$0F,$58,$C0+(Ord(D) shl 3)+Ord(S)]); end;
procedure TJitEmitter.MULSS_XMM_XMM(D,S: EXMMReg); begin WriteBytes([$F3,$0F,$59,$C0+(Ord(D) shl 3)+Ord(S)]); end;
procedure TJitEmitter.SUBSS_XMM_XMM(D,S: EXMMReg); begin WriteBytes([$F3,$0F,$5C,$C0+(Ord(D) shl 3)+Ord(S)]); end;
procedure TJitEmitter.DIVSS_XMM_XMM(D,S: EXMMReg); begin WriteBytes([$F3,$0F,$5E,$C0+(Ord(D) shl 3)+Ord(S)]); end;

procedure TJitEmitter.ADDSS_XMM_Mem(D: EXMMReg; B: EReg; O: Int64);
begin WriteBytes([$F3,$0F,$58,$80+(Ord(D) shl 3)+Ord(B)]); WriteBytes(@O,4); end;
procedure TJitEmitter.MULSS_XMM_Mem(D: EXMMReg; B: EReg; O: Int64);
begin WriteBytes([$F3,$0F,$59,$80+(Ord(D) shl 3)+Ord(B)]); WriteBytes(@O,4); end;
procedure TJitEmitter.SUBSS_XMM_Mem(D: EXMMReg; B: EReg; O: Int64);
begin WriteBytes([$F3,$0F,$5C,$80+(Ord(D) shl 3)+Ord(B)]); WriteBytes(@O,4); end;
procedure TJitEmitter.DIVSS_XMM_Mem(D: EXMMReg; B: EReg; O: Int64);
begin WriteBytes([$F3,$0F,$5E,$80+(Ord(D) shl 3)+Ord(B)]); WriteBytes(@O,4); end;

procedure TJitEmitter.ADDSX_XMM_XMM(D,S: EXMMReg; T: EExpressBaseType);
begin if T=xtSingle then ADDSS_XMM_XMM(D,S) else ADDSD_XMM_XMM(D,S); end;
procedure TJitEmitter.MULSX_XMM_XMM(D,S: EXMMReg; T: EExpressBaseType);
begin if T=xtSingle then MULSS_XMM_XMM(D,S) else MULSD_XMM_XMM(D,S); end;
procedure TJitEmitter.SUBSX_XMM_XMM(D,S: EXMMReg; T: EExpressBaseType);
begin if T=xtSingle then SUBSS_XMM_XMM(D,S) else SUBSD_XMM_XMM(D,S); end;
procedure TJitEmitter.DIVSX_XMM_XMM(D,S: EXMMReg; T: EExpressBaseType);
begin if T=xtSingle then DIVSS_XMM_XMM(D,S) else DIVSD_XMM_XMM(D,S); end;

procedure TJitEmitter.ADDSX_XMM_Mem(D: EXMMReg; B: EReg; O: Int64; T: EExpressBaseType);
begin if T=xtSingle then ADDSS_XMM_Mem(D,B,O) else ADDSD_XMM_Mem(D,B,O); end;
procedure TJitEmitter.MULSX_XMM_Mem(D: EXMMReg; B: EReg; O: Int64; T: EExpressBaseType);
begin if T=xtSingle then MULSS_XMM_Mem(D,B,O) else MULSD_XMM_Mem(D,B,O); end;
procedure TJitEmitter.SUBSX_XMM_Mem(D: EXMMReg; B: EReg; O: Int64; T: EExpressBaseType);
begin if T=xtSingle then SUBSS_XMM_Mem(D,B,O) else SUBSD_XMM_Mem(D,B,O); end;
procedure TJitEmitter.DIVSX_XMM_Mem(D: EXMMReg; B: EReg; O: Int64; T: EExpressBaseType);
begin if T=xtSingle then DIVSS_XMM_Mem(D,B,O) else DIVSD_XMM_Mem(D,B,O); end;

procedure TJitEmitter.XORPS_XMM_XMM(D,S: EXMMReg);
begin WriteBytes([$0F,$57,$C0+(Ord(D) shl 3)+Ord(S)]); end;

procedure TJitEmitter.UCOMISD_XMM_XMM(Reg1, Reg2: EXMMReg);
begin WriteBytes([$66,$0F,$2E,$C0+(Ord(Reg1) shl 3)+Ord(Reg2)]); end;

procedure TJitEmitter.UCOMISS_XMM_XMM(Reg1, Reg2: EXMMReg);
begin WriteBytes([$0F,$2E,$C0+(Ord(Reg1) shl 3)+Ord(Reg2)]); end;

procedure TJitEmitter.CVTSI2SD_XMM_Reg(DestReg: EXMMReg; SourceReg: EReg);
begin WriteBytes([$F2,$48,$0F,$2A,$C0+(Ord(DestReg) shl 3)+Ord(SourceReg)]); end;

procedure TJitEmitter.CVTSI2SS_XMM_Reg32(DestReg: EXMMReg; SourceReg: EReg);
begin WriteBytes([$F3,$0F,$2A,$C0+(Ord(DestReg) shl 3)+Ord(SourceReg)]); end;

procedure TJitEmitter.CVTSI2SS_XMM_Reg64(DestReg: EXMMReg; SourceReg: EReg);
begin WriteBytes([$F3,$48,$0F,$2A,$C0+(Ord(DestReg) shl 3)+Ord(SourceReg)]); end;

procedure TJitEmitter.TEST_Reg_Reg(Reg1, Reg2: EReg);
begin WriteBytes([$48,$85,$C0+(Ord(Reg2) shl 3)+Ord(Reg1)]); end;

procedure TJitEmitter.TEST_Reg8_Reg8(Reg: EReg);
begin
  // REX prefix to access sil/dil/spl/bpl as 8-bit
  if Ord(Reg) >= 4 then WriteBytes([$40]);
  WriteBytes([$84, $C0 + (Ord(Reg) and 7) * 9]);
end;

function TJitEmitter.Jcc_Rel32(Condition: EJccCondition): PInt32;
begin
  WriteBytes([$0F,$80+Ord(Condition)]);
  Result := PInt32(p); WriteBytes([$00,$00,$00,$00]);
end;

function TJitEmitter.JMP_Rel32: PInt32;
begin
  WriteBytes([$E9]);
  Result := PInt32(p); WriteBytes([$00,$00,$00,$00]);
end;

procedure TJitEmitter.CMP_Reg_Imm32(Reg: EReg; Value: Int32);
begin
  if Reg = rax then WriteBytes([$48,$3D]) else WriteBytes([$48,$81,$F8+Ord(Reg)]);
  WriteBytes(@Value,4);
end;

procedure TJitEmitter.CMP_Reg_Reg(Reg1, Reg2: EReg);
begin WriteBytes([$48,$39,$C0+(Ord(Reg2) shl 3)+Ord(Reg1)]); end;

procedure TJitEmitter.SETcc(Condition: ESetccCondition; DestReg: EReg);
begin WriteBytes([$40, $0F,$90+Ord(Condition),$C0+Ord(DestReg)]); end;

procedure TJitEmitter.Load_Int_Operand(const arg: TOperand; Reg: EReg);
var ConstVal: Int64;
begin
  if arg.Pos = mpConst then begin
    ConstVal := PInt64(ConstBase + arg.Data.Addr)^;
    MOV_Reg_Imm64(Reg, ConstVal);
  end else if arg.Pos = mpLocal then begin
    case BaseJITType(arg.BaseType) of
      xtInt8:   MOVSX_Reg_Mem_i8  (Reg, rbx, arg.Data.Addr);
      xtInt16:  MOVSX_Reg_Mem_i16 (Reg, rbx, arg.Data.Addr);
      xtInt32:  MOVSXD_Reg_Mem_i32(Reg, rbx, arg.Data.Addr);
      xtInt64:  MOV_Reg_Mem_i64   (Reg, rbx, arg.Data.Addr);
      xtUInt8:  MOVZX_Reg_Mem_i8  (Reg, rbx, arg.Data.Addr);
      xtUInt16: MOVZX_Reg_Mem_i16 (Reg, rbx, arg.Data.Addr);
      xtUInt32: MOV_Reg_Mem_i32   (Reg, rbx, arg.Data.Addr);
      xtUInt64: MOV_Reg_Mem_i64   (Reg, rbx, arg.Data.Addr);
      else      MOV_Reg_Mem_i64   (Reg, rbx, arg.Data.Addr);
    end
  end else
    MOV_Reg_Imm64(Reg, arg.Data.arg);
end;

procedure TJitEmitter.Store_Int_Result(const arg: TOperand; Reg: EReg);
begin
  case BaseJITType(arg.BaseType) of
    xtInt8,  xtUInt8:  MOV_Mem_Reg_i8 (rbx, arg.Data.Addr, Reg);
    xtInt16, xtUInt16: MOV_Mem_Reg_i16(rbx, arg.Data.Addr, Reg);
    xtInt32, xtUInt32: MOV_Mem_Reg_i32(rbx, arg.Data.Addr, Reg);
    else               MOV_Mem_Reg_i64(rbx, arg.Data.Addr, Reg);
  end;
end;

procedure TJitEmitter.Load_Float_Operand(const arg: TOperand; Reg: EXMMReg; TmpReg: EReg = rbp);
var ConstVal: Int64; ConstVal32: Int32;
begin
  if arg.Pos = mpLocal then begin
    case BaseJITType(arg.BaseType) of
      xtSingle: MOVSS_XMM_Mem(Reg, rbx, arg.Data.Addr);
      xtDouble: MOVSD_XMM_Mem(Reg, rbx, arg.Data.Addr);
    end;
  end else if arg.Pos = mpConst then begin
    if BaseJITType(arg.BaseType) = xtSingle then begin
      ConstVal32 := PInt32(ConstBase + arg.Data.Addr)^;
      MOV_Reg_Imm32(TmpReg, ConstVal32);
      WriteBytes([$66,$0F,$6E,$C0+(Ord(Reg) shl 3)+Ord(TmpReg)]);
    end else begin
      ConstVal := PInt64(ConstBase + arg.Data.Addr)^;
      MOV_Reg_Imm64(TmpReg, ConstVal);
      WriteBytes([$66,$48,$0F,$6E,$C0+(Ord(Reg) shl 3)+Ord(TmpReg)]);
    end;
  end else if arg.Pos = mpImm then begin
    if BaseJITType(arg.BaseType) = xtSingle then begin
      MOV_Reg_Imm32(TmpReg, arg.Data.i32);
      WriteBytes([$66,$0F,$6E,$C0+(Ord(Reg) shl 3)+Ord(TmpReg)]);
    end else begin
      MOV_Reg_Imm64(TmpReg, arg.Data.arg);
      WriteBytes([$66,$48,$0F,$6E,$C0+(Ord(Reg) shl 3)+Ord(TmpReg)]);
    end;
  end;
end;

procedure TJitEmitter.Store_Float_Result(const arg: TOperand; Reg: EXMMReg);
begin
  case BaseJITType(arg.BaseType) of
    xtSingle: MOVSS_Mem_XMM(rbx, arg.Data.Addr, Reg);
    xtDouble: MOVSD_Mem_XMM(rbx, arg.Data.Addr, Reg);
  end;
end;

procedure TJitEmitter.Store_Int_Result_To_Addr(const arg: TOperand; ValueReg, AddrReg: EReg);
begin
  case BaseJITType(arg.BaseType) of
    xtInt8,  xtUInt8:  MOV_RegAddr_Reg_i8 (AddrReg, ValueReg);
    xtInt16, xtUInt16: MOV_RegAddr_Reg_i16(AddrReg, ValueReg);
    xtInt32, xtUInt32: MOV_RegAddr_Reg_i32(AddrReg, ValueReg);
    else               MOV_RegAddr_Reg_i64(AddrReg, ValueReg);
  end;
end;

procedure TJitEmitter.Store_Float_Result_To_Addr(const arg: TOperand; ValueReg: EXMMReg; AddrReg: EReg);
begin
  case BaseJITType(arg.BaseType) of
    xtSingle: MOVSS_RegAddr_XMM(AddrReg, ValueReg);
    xtDouble: MOVSD_RegAddr_XMM(AddrReg, ValueReg);
    else raise Exception.Create('Store_Float_Result_To_Addr: unexpected type');
  end;
end;


// ===========================================================================
// TXMMRegisterAllocator
// ===========================================================================

const RESERVED_SCRATCH_REGS = 3;

procedure TXMMRegisterAllocator.Unset(Reg: EXMMReg);
begin
  Regs[Reg].VarArg.Data.Addr := -1;
  Regs[Reg].VarArg.BaseType  := xtUnknown;
  Regs[Reg].VarArg.Pos       := mpUnknown;
end;

function TXMMRegisterAllocator.Isset(Reg: EXMMReg): Boolean;
begin
  with Regs[Reg].VarArg do
    Result := not((BaseType = xtUnknown) and (Pos = mpUnknown) and (Data.Addr = -1));
end;

function TXMMRegisterAllocator.isEqual(Reg: EXMMReg; Operand: TOperand): Boolean;
begin
  with Regs[Reg].VarArg do
    Result := (BaseType = Operand.BaseType) and (Pos = Operand.Pos)
          and (Data.Addr = Operand.Data.Addr);
end;

procedure TXMMRegisterAllocator.Init(AEmitter: PJitEmitter);
var i: EXMMReg;
begin
  Self.Emitter := AEmitter; SetLength(Self.VIPMap, 0); Self.SpillCounter := 0;
  for i := Low(EXMMReg) to High(EXMMReg) do begin Unset(i); Regs[i].IsDirty := False; end;
end;

procedure TXMMRegisterAllocator.Spill(Reg: EXMMReg);
begin
  if Regs[Reg].IsDirty and Isset(Reg) and (Regs[Reg].VarArg.Pos = mpLocal) and (Regs[Reg].VarArg.Data.Addr >= 0) then begin
    Emitter^.Store_Float_Result(Regs[Reg].VarArg, Reg);
    Regs[Reg].IsDirty := False;
  end;
end;

function TXMMRegisterAllocator.FindVIP(Operand: TOperand; out Reg: EXMMReg): Boolean;
var i: Integer;
begin
  for i := 0 to High(VIPMap) do
    if (VIPMap[i].VarInfo.Data.Addr = Operand.Data.Addr) and
       (VIPMap[i].VarInfo.BaseType  = Operand.BaseType)  and
       (VIPMap[i].VarInfo.Pos       = Operand.Pos) then
    begin Reg := VIPMap[i].Reg; Result := True; Exit; end;
  Result := False;
end;

function TXMMRegisterAllocator.FindFree: EXMMReg;
var i: EXMMReg;
begin
  for i := EXMMReg(Ord(Low(EXMMReg))+Length(Self.VIPMap)) to High(EXMMReg) do
    if not Isset(i) then Exit(i);
  Result := EXMMReg(255);
end;
(*
function TXMMRegisterAllocator.Evict(Exclude: TXMMRegSet): EXMMReg;
var i: EXMMReg;
begin
  for i := EXMMReg(Ord(Low(EXMMReg))+Length(Self.VIPMap)) to High(EXMMReg) do
    if not (i in Exclude) then begin Spill(i); Unset(i); Exit(i); end;
  for i := Low(EXMMReg) to High(EXMMReg) do
    if not (i in Exclude) then begin Spill(i); Unset(i); Exit(i); end;
  Result := xmm7;
end;
*)

function TXMMRegisterAllocator.Evict(Exclude: TXMMRegSet): EXMMReg;
var i: EXMMReg;
begin
  // Only ever evict from the scratch pool (above the VIPs)
  for i := EXMMReg(Ord(Low(EXMMReg))+Length(Self.VIPMap)) to High(EXMMReg) do
    if not (i in Exclude) then
    begin
      Spill(i);
      Unset(i);
      Exit(i);
    end;

  // If we get here, it means EVERY scratch register is actively locked in the Exclude mask!
  // This means the math expression is too complex for the available scratch pool.
  WriteLn('XMM REGISTER ALLOCATOR IS OVERBOOKED - NEED MORE SCRATCH');
  raise Exception.Create('XMM REGISTER ALLOCATOR IS OVERBOOKED');
end;


procedure TXMMRegisterAllocator.AnalyzeTrace(CodeList: PBytecodeInstruction; Count: Integer; Settings: PCompilerSettings);
type TFreqEntry = record Addr: PtrInt; Pos: EMemPos; Freq: Integer; VarInfo: TOperand; end;
const
  MAX_F = 64;
  LOOP_WEIGHT: array[0..6] of Integer = (1, 10, 100, 1000, 10000, 100000, 1000000);
var
  FM: array[0..MAX_F-1] of TFreqEntry; FC,i,j,k,ri,MaxV: Int32;
  bc: TBytecodeInstruction; arg: TOperand; found: Boolean;
  FL: TVarFrequencyArray; tmp: TVarFrequency;
  CurrentWeight, Depth: Int32;
  WeightDrops: array of Int32;
begin
  FC := 0; FillChar(FM, SizeOf(FM), 0);
  CurrentWeight := 1;

  for i := 0 to Count-1 do
  begin
    Depth := (Settings+i)^.LoopDepth;
    if Depth > High(LOOP_WEIGHT) then Depth := High(LOOP_WEIGHT);
    if Depth < 0 then Depth := 0;

    CurrentWeight := LOOP_WEIGHT[Depth];
    if CurrentWeight < 1 then CurrentWeight := 1;

    bc := (CodeList+i)^;
    for j := 0 to bc.nArgs-1 do
    begin
      arg := bc.Args[j];
      if not (arg.Pos in [mpLocal{, mpConst}]) then Continue;
      if not(BaseJITType(arg.BaseType) in [xtSingle,xtDouble]) or (arg.Data.Addr < 0) then Continue;
         
      found := False;
      for k := 0 to FC-1 do 
        if (FM[k].Addr = arg.Data.Addr) and (FM[k].Pos = arg.Pos) then begin 
          Inc(FM[k].Freq, CurrentWeight * LOOP_WEIGHT[Depth]); 
          found:=True; 
          Break; 
        end;
        
      if not found and (FC < MAX_F) then begin
        FM[FC].Addr   := arg.Data.Addr; 
        FM[FC].Pos    := arg.Pos;
        FM[FC].Freq   := CurrentWeight * LOOP_WEIGHT[Depth]; 
        FM[FC].VarInfo:= arg; 
        Inc(FC);
      end;
    end;
  end;
  
  SetLength(FL, FC);
  for k := 0 to FC-1 do begin
    FL[k].Count := FM[k].Freq;
    FL[k].VarInfo := FM[k].VarInfo;
  end;
  for i := 0 to High(FL)-1 do
    for j := i+1 to High(FL) do
      if FL[j].Count > FL[i].Count then
      begin
        tmp:=FL[i];
        FL[i]:=FL[j];
        FL[j]:=tmp;
      end;
      
  for ri := Ord(Low(EXMMReg)) to Ord(High(EXMMReg)) do
  begin
    Unset(EXMMReg(ri));
    Regs[EXMMReg(ri)].IsDirty:=False;
  end;
  MaxV := Ord(High(EXMMReg))-Ord(Low(EXMMReg))+1-RESERVED_SCRATCH_REGS;
  if MaxV < 0 then MaxV := 0;
  
  SetLength(VIPMap, Min(Length(FL), MaxV));
  for i := 0 to High(VIPMap) do begin
    VIPMap[i].VarInfo := FL[i].VarInfo;
    VIPMap[i].Reg     := EXMMReg(i);
    Regs[EXMMReg(i)].VarArg  := FL[i].VarInfo;
    Regs[EXMMReg(i)].IsDirty := False;
    Emitter^.Load_Float_Operand(FL[i].VarInfo, EXMMReg(i));
  end;
end;

function TXMMRegisterAllocator.FindReg(Offset: PtrInt; out Reg: EXMMReg): Boolean;
var i: EXMMReg;
begin
  for i := Low(EXMMReg) to High(EXMMReg) do
    if Regs[i].VarArg.Data.Addr = Offset then begin Result:=True; Reg:=i; Exit; end;
  Result := False;
end;

function TXMMRegisterAllocator.GetFreeScratch(MarkAsUsed: Boolean; Exclude: TXMMRegSet): EXMMReg;
var i: EXMMReg;
begin
  for i := EXMMReg(Ord(Low(EXMMReg))+Length(Self.VIPMap)) to High(EXMMReg) do begin
    if (i in Exclude) or Isset(i) then Continue;
    Result := i;
    if MarkAsUsed then begin Regs[Result].VarArg.Data.Addr:=-2; Regs[Result].VarArg.Pos:=mpLocal; end;
    Exit;
  end;
  Result := Self.Evict(Exclude);
  if MarkAsUsed then begin Regs[Result].VarArg.Data.Addr:=-2; Regs[Result].VarArg.Pos:=mpLocal; end;
end;

function TXMMRegisterAllocator.GetReg(const arg: TOperand; Exclude: TXMMRegSet): EXMMReg;
var i: EXMMReg;
begin
  if arg.Pos in [mpImm, mpConst] then begin
    Result := GetFreeScratch(False, Exclude);
    Emitter^.Load_Float_Operand(arg, Result);
    Unset(Result); Regs[Result].VarArg.Data.Addr:=-2; Regs[Result].IsDirty:=False; Exit;
  end;
  if FindVIP(arg, Result) then Exit;
  for i := Low(EXMMReg) to High(EXMMReg) do
    if IsEqual(i, arg) then begin Result:=i; Exit; end;
  Result := GetFreeScratch(False, Exclude);
  Emitter^.Load_Float_Operand(arg, Result);
  Regs[Result].VarArg := arg; Regs[Result].IsDirty := False;
end;

function TXMMRegisterAllocator.Allocate(): EXMMReg;
begin
  Result := Self.FindFree;
  if Ord(Result) = 255 then Result := Self.Evict();
end;

procedure TXMMRegisterAllocator.SetResult(Reg: EXMMReg; const dest_arg: TOperand);
var vip_reg, final_reg: EXMMReg; is_reg_vip: Boolean; i: Integer;
begin
  is_reg_vip := False;
  for i := 0 to High(VIPMap) do if VIPMap[i].Reg = Reg then begin is_reg_vip:=True; Break; end;
  if FindVIP(dest_arg, vip_reg) then begin
    final_reg := vip_reg;
    if vip_reg <> Reg then begin
      if BaseJITType(dest_arg.BaseType) = xtSingle then Emitter^.MOVSS_XMM_XMM(vip_reg, Reg)
      else Emitter^.MOVSD_XMM_XMM(vip_reg, Reg);
    end;
  end else begin
    if is_reg_vip then begin
      final_reg := GetFreeScratch(False, [Reg]);
      if BaseJITType(dest_arg.BaseType) = xtSingle then Emitter^.MOVSS_XMM_XMM(final_reg, Reg)
      else Emitter^.MOVSD_XMM_XMM(final_reg, Reg);
    end else begin
      if Isset(Reg) and not IsEqual(Reg, dest_arg) then Spill(Reg);
      final_reg := Reg;
    end;
    Regs[final_reg].VarArg := dest_arg;
  end;
  Regs[final_reg].IsDirty := True;
end;

procedure TXMMRegisterAllocator.SpillAllDirty;
var i: EXMMReg;
begin for i := Low(EXMMReg) to High(EXMMReg) do Spill(i); end;

procedure TXMMRegisterAllocator.ClearAllConst;
var i: EXMMReg;
begin
  for i := Low(EXMMReg) to High(EXMMReg) do
    if Regs[i].VarArg.Data.Addr = -2 then Unset(i);
end;

procedure TXMMRegisterAllocator.InvalidateAll;
var i: EXMMReg; dummy: EXMMReg;
begin
  SpillAllDirty;
  for i := Low(EXMMReg) to High(EXMMReg) do
    if not FindVIP(Regs[i].VarArg, dummy) then Unset(i);
end;

procedure TXMMRegisterAllocator.InvalidateAllIncludingVIP;
var
  i: EXMMReg;
  v: Integer;
  dummy: EXMMReg;
begin
  for i := Low(EXMMReg) to High(EXMMReg) do
    if not FindVIP(Regs[i].VarArg, dummy) then
    begin
      Spill(i);
      Unset(i);
    end;

  for v := 0 to High(VIPMap) do
  begin
    i := VIPMap[v].Reg;
    Regs[i].IsDirty := False;
    Emitter^.Load_Float_Operand(VIPMap[v].VarInfo, i);
  end;
end;

{$I JIT_x64_gpr.inc}

end.
