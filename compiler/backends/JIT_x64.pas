unit JIT_x64;

{$I header.inc}
{$hints off}

interface

uses
  SysUtils,
  xpr.Types,
  xpr.Bytecode; // Only need basic types here

type
  EJccCondition = (
    jo,    // 0: Opcode 80h
    jno,   // 1: Opcode 81h
    jb,    // 2: Opcode 82h (jc, jnae)
    jae,   // 3: Opcode 83h (jnb, jnc)
    je,    // 4: Opcode 84h (jz)
    jne,   // 5: Opcode 85h (jnz)
    jbe,   // 6: Opcode 86h (jna)
    ja,    // 7: Opcode 87h (jnbe)
    js,    // 8: Opcode 88h
    jns,   // 9: Opcode 89h
    jp,    // 10: Opcode 8Ah (jpe)
    jnp,   // 11: Opcode 8Bh (jpo)
    jl,    // 12: Opcode 8Ch (jnge)
    jge,   // 13: Opcode 8Dh (jnl)
    jle,   // 14: Opcode 8Eh (jng)
    jg     // 15: Opcode 8Fh (jnle)
  );

  // Enumeration for Conditional Set (SETcc) opcodes (0F 9x)
  ESetccCondition = (
    seto,  // 0: Opcode 90h
    setno, // 1: Opcode 91h
    setb,  // 2: Opcode 92h (setc, setnae)
    setae, // 3: Opcode 93h (setnb, setnc)
    sete,  // 4: Opcode 94h (setz)
    setne, // 5: Opcode 95h (setnz)
    setbe, // 6: Opcode 96h (setna)
    seta,  // 7: Opcode 97h (setnbe)
    sets,  // 8: Opcode 98h
    setns, // 9: Opcode 99h
    setp,  // 10: Opcode 9Ah (setpe)
    setnp, // 11: Opcode 9Bh (setpo)
    setl,  // 12: Opcode 9Ch (setnge)
    setge, // 13: Opcode 9Dh (setnl)
    setle, // 14: Opcode 9Eh (setng)
    setg   // 15: Opcode 9Fh (setnle)
  );

  EReg = (
    rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi // 0-7
  );

  EXMMReg = (
    xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7 // 0-7
  );
  TXMMRegSet = set of EXMMReg;

  TJitEmitter = record
    p: PByte;
    procedure WriteBytes(const Bytes: array of Byte);
    procedure WriteBytes(AValue: Pointer; ASize: NativeInt); overload;

    procedure Init(StartPtr: PByte);
    function CurrentPosition: PByte;

    // --- Control ---
    procedure Preamble;
    procedure Epilogue;
    procedure RET;
    procedure NOP;


    // Integer Emitters
    procedure MOV_Reg_Imm64(Reg: EReg; Value: Int64);
    procedure MOV_Reg_Imm32(Reg: EReg; Value: Int32);
    procedure MOV_Reg_Reg(DestReg, SourceReg: EReg);
    procedure MOVZX_Reg_Mem_i8(Reg: EReg; BaseReg: EReg; Offset: Int64);
    procedure MOVZX_Reg_Mem_i16(Reg: EReg; BaseReg: EReg; Offset: Int64);
    procedure MOV_Reg_Mem_i32(Reg: EReg; BaseReg: EReg; Offset: Int64);
    procedure MOV_Reg_Mem_i64(Reg: EReg; BaseReg: EReg; Offset: Int64);
    procedure MOVSXD_Reg_Mem_i32(Reg: EReg; BaseReg: EReg; Offset: Int64);
    procedure MOV_Mem_Reg_i8(BaseReg: EReg; Offset: Int64; Reg: EReg);
    procedure MOV_Mem_Reg_i16(BaseReg: EReg; Offset: Int64; Reg: EReg);
    procedure MOV_Mem_Reg_i32(BaseReg: EReg; Offset: Int64; Reg: EReg);
    procedure MOV_Mem_Reg_i64(BaseReg: EReg; Offset: Int64; Reg: EReg);
    procedure MOV_RegAddr_Reg_i8(AddrReg, SourceReg: EReg);
    procedure MOV_RegAddr_Reg_i16(AddrReg, SourceReg: EReg);
    procedure MOV_RegAddr_Reg_i32(AddrReg, SourceReg: EReg);
    procedure MOV_RegAddr_Reg_i64(AddrReg, SourceReg: EReg);

    procedure CQO;
    procedure INC_Mem8(BaseReg: EReg; Offset: Int64);
    procedure INC_Mem16(BaseReg: EReg; Offset: Int64);
    procedure INC_Mem32(BaseReg: EReg; Offset: Int64);
    procedure INC_Mem64(BaseReg: EReg; Offset: Int64);
    procedure ADD_Reg_Reg(DestReg, SourceReg: EReg);
    procedure IMUL_Reg_Reg(DestReg, SourceReg: EReg);
    procedure IMUL_Reg_Reg_Imm32(DestReg, SourceReg: EReg; Value: Int32);
    procedure SUB_Reg_Reg(DestReg, SourceReg: EReg);
    procedure IDIV_Reg(SourceReg: EReg);
    procedure AND_Reg_Reg(DestReg, SourceReg: EReg);
    procedure OR_Reg_Reg(DestReg, SourceReg: EReg);
    procedure XOR_Reg_Reg(DestReg, SourceReg: EReg);
    procedure SHL_Reg_CL(DestReg: EReg);
    procedure SHR_Reg_CL(DestReg: EReg);
    procedure SAR_Reg_CL(DestReg: EReg);
    procedure LEA_Reg_Mem(DestReg: EReg; BaseReg: EReg; IndexReg: EReg; Scale: Byte);

    // Floating-Point Emitters
    // XMM_XMM
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

    // double
    procedure ADDSD_XMM_XMM(DestReg, SourceReg: EXMMReg);
    procedure MULSD_XMM_XMM(DestReg, SourceReg: EXMMReg);
    procedure SUBSD_XMM_XMM(DestReg, SourceReg: EXMMReg);
    procedure DIVSD_XMM_XMM(DestReg, SourceReg: EXMMReg);
    procedure ADDSD_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
    procedure MULSD_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
    procedure SUBSD_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
    procedure DIVSD_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);


    // single
    procedure ADDSS_XMM_XMM(DestReg, SourceReg: EXMMReg);
    procedure MULSS_XMM_XMM(DestReg, SourceReg: EXMMReg);
    procedure SUBSS_XMM_XMM(DestReg, SourceReg: EXMMReg);
    procedure DIVSS_XMM_XMM(DestReg, SourceReg: EXMMReg);
    procedure ADDSS_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
    procedure MULSS_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
    procedure SUBSS_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
    procedure DIVSS_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);

    // auto single / double
    procedure ADDSX_XMM_XMM(DestReg, SourceReg: EXMMReg; AType: EExpressBaseType);
    procedure MULSX_XMM_XMM(DestReg, SourceReg: EXMMReg; AType: EExpressBaseType);
    procedure SUBSX_XMM_XMM(DestReg, SourceReg: EXMMReg; AType: EExpressBaseType);
    procedure DIVSX_XMM_XMM(DestReg, SourceReg: EXMMReg; AType: EExpressBaseType);
    procedure ADDSX_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64; AType: EExpressBaseType);
    procedure MULSX_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64; AType: EExpressBaseType);
    procedure SUBSX_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64; AType: EExpressBaseType);
    procedure DIVSX_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64; AType: EExpressBaseType);

    //
    procedure XORPS_XMM_XMM(DestReg, SourceReg: EXMMReg);
    procedure UCOMISD_XMM_XMM(Reg1, Reg2: EXMMReg);
    procedure UCOMISS_XMM_XMM(Reg1, Reg2: EXMMReg);
    procedure CVTSI2SD_XMM_Reg(DestReg: EXMMReg; SourceReg: EReg);
    procedure CVTSI2SS_XMM_Reg32(DestReg: EXMMReg; SourceReg: EReg);
    procedure CVTSI2SS_XMM_Reg64(DestReg: EXMMReg; SourceReg: EReg);
    procedure VCVTSI2SD_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
    procedure VCVTSI2SS_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);


    // controw flow
    procedure CMP_Reg_Imm32(Reg: EReg; Value: Int32);
    procedure CMP_Reg_Reg(Reg1, Reg2: EReg);
    procedure TEST_Reg_Reg(Reg1, Reg2: EReg); // For checking if a register is zero
    function Jcc_Rel32(Condition: EJccCondition): PInt32; // Conditional Jump, returns patch location
    function JMP_Rel32: PInt32; // Unconditional Jump, returns patch location

    // Sets a byte-sized register (like AL) to 1 or 0 based on the last comparison.
    procedure SETcc(Condition: ESetccCondition; DestReg: EReg);

    // Friendly Helpers
    procedure Load_Int_Operand(const arg: TOperand; Reg: EReg);
    procedure Store_Int_Result(const arg: TOperand; Reg: EReg);
    procedure Load_Float_Operand(const arg: TOperand; Reg: EXMMReg; TmpReg:EReg=RCX);
    procedure Store_Float_Result(const arg: TOperand; Reg: EXMMReg);
    procedure Store_Int_Result_To_Addr(const arg: TOperand; ValueReg: EReg; AddrReg: EReg);
    procedure Store_Float_Result_To_Addr(const arg: TOperand; ValueReg: EXMMReg; AddrReg: EReg);
  end;
  PJitEmitter = ^TJitEmitter;


  // Holds the state of a single physical XMM register
  TXMMRegState = record
    IsDirty: Boolean;
    VarArg: TOperand; // A copy of the argument for the var it holds
  end;

  // Used for frequency analysis
  TVarFrequency = record
    Count: Integer;
    VarInfo: TOperand;
  end;
  TVarFrequencyArray = array of TVarFrequency;

  // A simple record to hold a VIP variable's mapping
  TVIPMapEntry = record
    VarInfo: TOperand;
    Reg: EXMMReg;
  end;
  TVIPMap = array of TVIPMapEntry;

  // The Linear Scan Register Allocator
  TXMMRegisterAllocator = record
  private
    Regs: array[EXMMReg] of TXMMRegState;
    VIPMap: TVIPMap; // A simple array, lookup is linear search
    Emitter: PJitEmitter;
    SpillCounter: Integer; // For simple round-robin eviction

    procedure Unset(Reg: EXMMReg);
    function Isset(Reg: EXMMReg): Boolean;
    function isEqual(Reg: EXMMReg; Operand: TOperand): Boolean;
    procedure Spill(Reg: EXMMReg);
    function FindVIP(Operand: TOperand; out Reg: EXMMReg): Boolean;
    function FindFree: EXMMReg;
    function Evict(Exclude: TXMMRegSet = []): EXMMReg;
  public
    procedure Init(AEmitter: PJitEmitter);

    // --- The Main Public Methods ---
    procedure AnalyzeTrace(CodeList: PBytecodeInstruction; Count: Integer);
    function FindReg(Offset: PtrInt; out Reg: EXMMReg): Boolean;
    function GetFreeScratch(MarkAsUsed: Boolean = False;
                        Exclude: TXMMRegSet = []): EXMMReg;
    function GetReg(const arg: TOperand; Exclude: TXMMRegSet =[]): EXMMReg;
    function Allocate(): EXMMReg;
    procedure SetResult(Reg: EXMMReg; const dest_arg: TOperand);
    procedure SpillAllDirty;
    procedure ClearAllConst;
    procedure InvalidateAll; // For use at merge points
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

function BaseJITType(BaseType: EExpressBaseType): EExpressBaseType;
begin
  case BaseType of
    xtSingle, xtDouble:
                   Result := BaseType;
    xtInt8..xtInt64, xtUInt8..xtUInt64:
                   Result := BaseType;
    xtAnsiChar:    Result := xtInt8;
    xtUnicodeChar: Result := xtInt16;
    xtBool:        Result := xtInt8;
    xtPointer, xtArray, xtAnsiString, xtUnicodeString, xtClass, xtMethod: Result := xtInt;
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
  Size := ((Size + PAGE_SIZE - 1) div PAGE_SIZE) * PAGE_SIZE;
  Result := fpmmap(nil, Size, PROT_READ or PROT_WRITE, MAP_PRIVATE+MAP_ANON, -1, 0);
  if Result = MAP_FAILED then
  begin
    Writeln('Allocate JIT mem failed with: ', fpgeterrno);
    Result := nil;
  end;
  {$ENDIF}
end;

function SetMemoryExecutable(Address: Pointer; Size: SizeInt): Boolean;
{$IFDEF WINDOWS}
var OldProtect: DWORD;
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
  VirtualFree(Address, 0, MEM_RELEASE);
  {$ELSE}
  fpmunmap(Address, Size);
  {$ENDIF}
end;

{ TJitEmitter }

procedure TJitEmitter.Init(StartPtr: PByte);
begin
  p := StartPtr;
end;

function TJitEmitter.CurrentPosition: PByte;
begin
  Result := p;
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

procedure TJitEmitter.WriteBytes(AValue: Pointer; ASize: NativeInt);
begin
  if ASize > 0 then
  begin
    System.Move(AValue^, p^, ASize);
    Inc(p, ASize);
  end;
end;

procedure TJitEmitter.Preamble;
begin
  // Save all registers we intend to use.
  // GPRs are pushed in a specific order for convenience.
  WriteBytes([
    $50, // push rax (Caller-saved, but we save it to be hyper-defensive)
    $51, // push rcx (Caller-saved / 1st Arg Win64)
    $52, // push rdx (Caller-saved / 2nd Arg Win64)
    $53, // push rbx (Callee-saved, MUST be saved)
    $57  // push rdi (Callee-saved / 1st Arg SysV)
  ]);

  // Now, safely establish RBX as our BasePtr for the rest of the function.
  // This code runs AFTER the original values of RCX/RDI have been saved.
  {$IFDEF WINDOWS}
    // On Windows, the first argument (BasePtr) is in RCX.
    // mov rbx, rcx
    WriteBytes([$48, $89, $CB]);
  {$ELSE}
    // On Unix-like systems, the first argument (BasePtr) is in RDI.
    // mov rbx, rdi
    WriteBytes([$48, $89, $FB]);
  {$ENDIF}
end;

procedure TJitEmitter.Epilogue;
begin
  // Restore ALL saved registers in the EXACT REVERSE ORDER of the push.
  WriteBytes([
    $5F, // pop rdi
    $5B, // pop rbx
    $5A, // pop rdx
    $59, // pop rcx
    $58  // pop rax
  ]);

  // Return to the interpreter.
  WriteBytes([$C3]); // ret
end;

procedure TJitEmitter.RET;
begin
  WriteBytes([$C3]);
end;

procedure TJitEmitter.NOP;
begin
  WriteBytes([$90]);
end;

procedure TJitEmitter.MOV_Reg_Imm64(Reg: EReg; Value: Int64);
begin
  WriteBytes([$48, $B8 + Ord(Reg)]);
  WriteBytes(@Value, SizeOf(Int64));
end;

procedure TJitEmitter.MOV_Reg_Imm32(Reg: EReg; Value: Int32);
begin
  WriteBytes([$B8 + Ord(Reg)]);
  WriteBytes(@Value, SizeOf(Int32));
end;

procedure TJitEmitter.MOV_Reg_Reg(DestReg, SourceReg: EReg);
begin
  WriteBytes([$48, $89, $C0 + (Ord(SourceReg) * 8) + Ord(DestReg)]);
end;

procedure TJitEmitter.CQO;
begin
  WriteBytes([$48, $99]);
end;

// Emits: inc byte ptr [BaseReg + disp32]
procedure TJitEmitter.INC_Mem8(BaseReg: EReg; Offset: Int64);
begin
  WriteBytes([$FE, $80 + Ord(BaseReg)]); // Opcode, ModR/M for [BaseReg+disp32]
  WriteBytes(@Offset, 4);                // 32-bit displacement
end;

// Emits: inc word ptr [BaseReg + disp32]
procedure TJitEmitter.INC_Mem16(BaseReg: EReg; Offset: Int64);
begin
  WriteBytes([$66]); // Operand-size override prefix
  WriteBytes([$FF, $80 + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

// Emits: inc dword ptr [BaseReg + disp32]
procedure TJitEmitter.INC_Mem32(BaseReg: EReg; Offset: Int64);
begin
  WriteBytes([$FF, $80 + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

// Emits: inc qword ptr [BaseReg + disp32]
procedure TJitEmitter.INC_Mem64(BaseReg: EReg; Offset: Int64);
begin
  WriteBytes([$48, $FF, $80 + Ord(BaseReg)]); // REX.W prefix for 64-bit
  WriteBytes(@Offset, 4);
end;

procedure TJitEmitter.ADD_Reg_Reg(DestReg, SourceReg: EReg);
begin
  WriteBytes([$48, $01, $C0 + (Ord(SourceReg) * 8) + Ord(DestReg)]);
end;

procedure TJitEmitter.IMUL_Reg_Reg(DestReg, SourceReg: EReg);
begin
  WriteBytes([$48, $0F, $AF, $C0 + (Ord(DestReg) * 8) + Ord(SourceReg)]);
end;

procedure TJitEmitter.IMUL_Reg_Reg_Imm32(DestReg, SourceReg: EReg; Value: Int32);
begin
  WriteBytes([$48, $69, $C0 + (Ord(DestReg) * 8) + Ord(SourceReg)]);
  WriteBytes(@Value, SizeOf(Int32));
end;

procedure TJitEmitter.SUB_Reg_Reg(DestReg, SourceReg: EReg);
begin
  WriteBytes([$48, $29, $C0 + (Ord(SourceReg) * 8) + Ord(DestReg)]);
end;

procedure TJitEmitter.IDIV_Reg(SourceReg: EReg);
begin
  WriteBytes([$48, $F7, $F8 + Ord(SourceReg)]);
end;

procedure TJitEmitter.AND_Reg_Reg(DestReg, SourceReg: EReg);
begin
  WriteBytes([$48, $21, $C0 + (Ord(SourceReg) * 8) + Ord(DestReg)]);
end;

procedure TJitEmitter.OR_Reg_Reg(DestReg, SourceReg: EReg);
begin
  WriteBytes([$48, $09, $C0 + (Ord(SourceReg) * 8) + Ord(DestReg)]);
end;

procedure TJitEmitter.XOR_Reg_Reg(DestReg, SourceReg: EReg);
begin
  WriteBytes([$48, $31, $C0 + (Ord(SourceReg) * 8) + Ord(DestReg)]);
end;

procedure TJitEmitter.SHL_Reg_CL(DestReg: EReg);
begin
  WriteBytes([$48, $D3, $E0 + Ord(DestReg)]);
end;

procedure TJitEmitter.SHR_Reg_CL(DestReg: EReg);
begin
  WriteBytes([$48, $D3, $E8 + Ord(DestReg)]);
end;

procedure TJitEmitter.SAR_Reg_CL(DestReg: EReg);
begin
  WriteBytes([$48, $D3, $F8 + Ord(DestReg)]);
end;

procedure TJitEmitter.LEA_Reg_Mem(DestReg: EReg; BaseReg: EReg; IndexReg: EReg; Scale: Byte);
var scale_bits: Byte;
begin
  case Scale of 1: scale_bits := 0; 2: scale_bits := 1; 4: scale_bits := 2; 8: scale_bits := 3; else raise Exception.Create('Invalid LEA scale factor'); end;
  WriteBytes([$48, $8D, $04 + (Ord(DestReg) * 8), (scale_bits shl 6) + (Ord(IndexReg) * 8) + Ord(BaseReg)]);
end;

procedure TJitEmitter.MOVZX_Reg_Mem_i8(Reg: EReg; BaseReg: EReg; Offset: Int64);
begin
  WriteBytes([$48, $0F, $B6, $80 + (Ord(Reg) * 8) + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

procedure TJitEmitter.MOVZX_Reg_Mem_i16(Reg: EReg; BaseReg: EReg; Offset: Int64);
begin
  WriteBytes([$48, $0F, $B7, $80 + (Ord(Reg) * 8) + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

procedure TJitEmitter.MOV_Reg_Mem_i32(Reg: EReg; BaseReg: EReg; Offset: Int64);
begin
  WriteBytes([$8B, $80 + (Ord(Reg) * 8) + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

procedure TJitEmitter.MOV_Reg_Mem_i64(Reg: EReg; BaseReg: EReg; Offset: Int64);
begin
  WriteBytes([$48, $8B, $80 + (Ord(Reg) * 8) + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

procedure TJitEmitter.MOVSXD_Reg_Mem_i32(Reg: EReg; BaseReg: EReg; Offset: Int64);
begin
  // MOVSXD r64, r/m32 — opcode: 48 63 /r (REX.W + 63)
  WriteBytes([$48, $63, $80 + (Ord(Reg) * 8) + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

procedure TJitEmitter.MOV_Mem_Reg_i8(BaseReg: EReg; Offset: Int64; Reg: EReg);
begin
  WriteBytes([$88, $80 + (Ord(Reg) * 8) + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

procedure TJitEmitter.MOV_Mem_Reg_i16(BaseReg: EReg; Offset: Int64; Reg: EReg);
begin
  WriteBytes([$66, $89, $80 + (Ord(Reg) * 8) + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

procedure TJitEmitter.MOV_Mem_Reg_i32(BaseReg: EReg; Offset: Int64; Reg: EReg);
begin
  WriteBytes([$89, $80 + (Ord(Reg) * 8) + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

procedure TJitEmitter.MOV_Mem_Reg_i64(BaseReg: EReg; Offset: Int64; Reg: EReg);
begin
  WriteBytes([$48, $89, $80 + (Ord(Reg) * 8) + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

procedure TJitEmitter.MOV_RegAddr_Reg_i8(AddrReg, SourceReg: EReg);
begin
  // Emits: mov [AddrReg], SourceReg_low_8_bits (e.g., mov [rax], cl)
  WriteBytes([$88, $00 + (Ord(SourceReg) * 8) + Ord(AddrReg)]);
end;

procedure TJitEmitter.MOV_RegAddr_Reg_i16(AddrReg, SourceReg: EReg);
begin
  // Emits: mov [AddrReg], SourceReg_16_bits (e.g., mov [rax], cx)
  WriteBytes([$66, $89, $00 + (Ord(SourceReg) * 8) + Ord(AddrReg)]);
end;

procedure TJitEmitter.MOV_RegAddr_Reg_i32(AddrReg, SourceReg: EReg);
begin
  // Emits: mov [AddrReg], SourceReg_32_bits (e.g., mov [rax], ecx)
  WriteBytes([$89, $00 + (Ord(SourceReg) * 8) + Ord(AddrReg)]);
end;

procedure TJitEmitter.MOV_RegAddr_Reg_i64(AddrReg, SourceReg: EReg);
begin
  // Emits: mov [AddrReg], SourceReg_64_bits (e.g., mov [rax], rcx)
  WriteBytes([$48, $89, $00 + (Ord(SourceReg) * 8) + Ord(AddrReg)]);
end;


// ---------------------
// Floats

// Emits: movsd DestReg, SourceReg (Move Scalar Double-Precision)
procedure TJitEmitter.MOVSD_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin
  WriteBytes([
    $F2,       // SSE prefix for double-precision
    $0F, $10,  // Opcode for MOVSD (reg, reg/mem)
    $C0 + (Ord(DestReg) * 8) + Ord(SourceReg) // ModR/M for register-to-register
  ]);
end;

// Emits: movss DestReg, SourceReg (Move Scalar Single-Precision)
procedure TJitEmitter.MOVSS_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin
  WriteBytes([
    $F3,       // SSE prefix for single-precision
    $0F, $10,  // Opcode for MOVSS (reg, reg/mem)
    $C0 + (Ord(DestReg) * 8) + Ord(SourceReg) // ModR/M for register-to-register
  ]);
end;

procedure TJitEmitter.MOVSD_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
begin
  WriteBytes([$F2, $0F, $10, $80 + (Ord(DestReg) * 8) + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

procedure TJitEmitter.MOVSS_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
begin
  WriteBytes([$F3, $0F, $10, $80 + (Ord(DestReg) * 8) + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

procedure TJitEmitter.MOVSD_Mem_XMM(BaseReg: EReg; Offset: Int64; SourceReg: EXMMReg);
begin
  WriteBytes([$F2, $0F, $11, $80 + (Ord(SourceReg) * 8) + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

procedure TJitEmitter.MOVSS_Mem_XMM(BaseReg: EReg; Offset: Int64; SourceReg: EXMMReg);
begin
  WriteBytes([$F3, $0F, $11, $80 + (Ord(SourceReg) * 8) + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

procedure TJitEmitter.MOVSD_RegAddr_XMM(AddrReg: EReg; SourceReg: EXMMReg);
begin
  // Emits: movsd [AddrReg], SourceReg_xmm (e.g., movsd [rax], xmm1)
  WriteBytes([$F2, $0F, $11, $00 + (Ord(SourceReg) * 8) + Ord(AddrReg)]);
end;

procedure TJitEmitter.MOVSS_RegAddr_XMM(AddrReg: EReg; SourceReg: EXMMReg);
begin
  // Emits: movss [AddrReg], SourceReg_xmm (e.g., movss [rax], xmm1)
  WriteBytes([$F3, $0F, $11, $00 + (Ord(SourceReg) * 8) + Ord(AddrReg)]);
end;


// --- SINGLE TO DOUBLE, DOUBLE TO SINGLE
// conversion for mixed mode arithmetics
procedure TJitEmitter.CVTSS2SD_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin
  // Takes the 32-bit single in SourceReg, converts it to a 64-bit double,
  // and places the result in DestReg.
  WriteBytes([$F3, $0F, $5A, $C0 + (Ord(DestReg) * 8) + Ord(SourceReg)]);
end;

procedure TJitEmitter.CVTSD2SS_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin
  // Takes the 64-bit double in SourceReg, converts it to a 32-bit single,
  // and places the result in the lower 32 bits of DestReg.
  WriteBytes([$F2, $0F, $5A, $C0 + (Ord(DestReg) * 8) + Ord(SourceReg)]);
end;


// ---- DOUBLE -----
procedure TJitEmitter.MULSD_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin
  WriteBytes([$F2, $0F, $59, $C0 + (Ord(DestReg) * 8) + Ord(SourceReg)]);
end;

procedure TJitEmitter.ADDSD_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin
  WriteBytes([$F2, $0F, $58, $C0 + (Ord(DestReg) * 8) + Ord(SourceReg)]);
end;

procedure TJitEmitter.SUBSD_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin
  WriteBytes([$F2, $0F, $5C, $C0 + (Ord(DestReg) * 8) + Ord(SourceReg)]);
end;

procedure TJitEmitter.DIVSD_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin
  WriteBytes([$F2, $0F, $5E, $C0 + (Ord(DestReg) * 8) + Ord(SourceReg)]);
end;

procedure TJitEmitter.ADDSD_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
begin
  // Emits: addsd DestReg, [BaseReg + disp32]
  WriteBytes([$F2, $0F, $58, $80 + (Ord(DestReg) * 8) + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

procedure TJitEmitter.MULSD_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
begin
  // Emits: mulsd DestReg, [BaseReg + disp32]
  WriteBytes([$F2, $0F, $59, $80 + (Ord(DestReg) * 8) + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

// Emits: subsd DestReg, [BaseReg + disp32]
// Operation: DestReg = DestReg - [memory]
procedure TJitEmitter.SUBSD_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
begin
  // Opcode: F2 0F 5C /r
  WriteBytes([$F2, $0F, $5C, $80 + (Ord(DestReg) * 8) + Ord(BaseReg)]);
  // 32-bit displacement (the offset)
  WriteBytes(@Offset, 4);
end;

// Emits: divsd DestReg, [BaseReg + disp32]
// Operation: DestReg = DestReg / [memory]
procedure TJitEmitter.DIVSD_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
begin
  // Opcode: F2 0F 5E /r
  WriteBytes([$F2, $0F, $5E, $80 + (Ord(DestReg) * 8) + Ord(BaseReg)]);
  // 32-bit displacement (the offset)
  WriteBytes(@Offset, 4);
end;


// ---- SINGLE -----
procedure TJitEmitter.ADDSS_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin
  WriteBytes([$F3, $0F, $58, $C0 + (Ord(DestReg) * 8) + Ord(SourceReg)]);
end;

// Emits: mulss dest_xmm, src_xmm
// Opcode: F3 0F 59 /r
procedure TJitEmitter.MULSS_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin
  WriteBytes([$F3, $0F, $59, $C0 + (Ord(DestReg) * 8) + Ord(SourceReg)]);
end;

// Emits: subss dest_xmm, src_xmm
// Opcode: F3 0F 5C /r
procedure TJitEmitter.SUBSS_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin
  WriteBytes([$F3, $0F, $5C, $C0 + (Ord(DestReg) * 8) + Ord(SourceReg)]);
end;

// Emits: divss dest_xmm, src_xmm
// Opcode: F3 0F 5E /r
procedure TJitEmitter.DIVSS_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin
  WriteBytes([$F3, $0F, $5E, $C0 + (Ord(DestReg) * 8) + Ord(SourceReg)]);
end;

// --- Register-Memory Variants ---

// Emits: addss DestReg, [BaseReg + disp32]
procedure TJitEmitter.ADDSS_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
begin
  WriteBytes([$F3, $0F, $58, $80 + (Ord(DestReg) * 8) + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

// Emits: mulss DestReg, [BaseReg + disp32]
procedure TJitEmitter.MULSS_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
begin
  WriteBytes([$F3, $0F, $59, $80 + (Ord(DestReg) * 8) + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

// Emits: subss DestReg, [BaseReg + disp32]
procedure TJitEmitter.SUBSS_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
begin
  WriteBytes([$F3, $0F, $5C, $80 + (Ord(DestReg) * 8) + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;

// Emits: divss DestReg, [BaseReg + disp32]
procedure TJitEmitter.DIVSS_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
begin
  WriteBytes([$F3, $0F, $5E, $80 + (Ord(DestReg) * 8) + Ord(BaseReg)]);
  WriteBytes(@Offset, 4);
end;


// ---- single / double auto -------------

procedure TJitEmitter.ADDSX_XMM_XMM(DestReg, SourceReg: EXMMReg; AType: EExpressBaseType);
begin
  if AType = xtSingle then
    ADDSS_XMM_XMM(DestReg, SourceReg)
  else
    ADDSD_XMM_XMM(DestReg, SourceReg);
end;

procedure TJitEmitter.MULSX_XMM_XMM(DestReg, SourceReg: EXMMReg; AType: EExpressBaseType);
begin
  if AType = xtSingle then
    MULSS_XMM_XMM(DestReg, SourceReg)
  else
    MULSD_XMM_XMM(DestReg, SourceReg);
end;

procedure TJitEmitter.SUBSX_XMM_XMM(DestReg, SourceReg: EXMMReg; AType: EExpressBaseType);
begin
  if AType = xtSingle then
    SUBSS_XMM_XMM(DestReg, SourceReg)
  else
    SUBSD_XMM_XMM(DestReg, SourceReg);
end;

procedure TJitEmitter.DIVSX_XMM_XMM(DestReg, SourceReg: EXMMReg; AType: EExpressBaseType);
begin
  if AType = xtSingle then
    DIVSS_XMM_XMM(DestReg, SourceReg)
  else
    DIVSD_XMM_XMM(DestReg, SourceReg);
end;

// --- Register-Memory Variants ---

procedure TJitEmitter.ADDSX_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64; AType: EExpressBaseType);
begin
  if AType = xtSingle then
    ADDSS_XMM_Mem(DestReg, BaseReg, Offset)
  else
    ADDSD_XMM_Mem(DestReg, BaseReg, Offset);
end;

procedure TJitEmitter.MULSX_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64; AType: EExpressBaseType);
begin
  if AType = xtSingle then
    MULSS_XMM_Mem(DestReg, BaseReg, Offset)
  else
    MULSD_XMM_Mem(DestReg, BaseReg, Offset);
end;

procedure TJitEmitter.SUBSX_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64; AType: EExpressBaseType);
begin
  if AType = xtSingle then
    SUBSS_XMM_Mem(DestReg, BaseReg, Offset)
  else
    SUBSD_XMM_Mem(DestReg, BaseReg, Offset);
end;

procedure TJitEmitter.DIVSX_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64; AType: EExpressBaseType);
begin
  if AType = xtSingle then
    DIVSS_XMM_Mem(DestReg, BaseReg, Offset)
  else
    DIVSD_XMM_Mem(DestReg, BaseReg, Offset);
end;

// -------------------
procedure TJitEmitter.XORPS_XMM_XMM(DestReg, SourceReg: EXMMReg);
begin
  WriteBytes([$0F, $57, $C0 + (Ord(DestReg) * 8) + Ord(SourceReg)]);
end;

procedure TJitEmitter.UCOMISD_XMM_XMM(Reg1, Reg2: EXMMReg);
begin
  WriteBytes([$66, $0F, $2E, $C0 + (Ord(Reg2) * 8) + Ord(Reg1)]);
end;

procedure TJitEmitter.UCOMISS_XMM_XMM(Reg1, Reg2: EXMMReg);
begin
  WriteBytes([$0F, $2E, $C0 + (Ord(Reg2) * 8) + Ord(Reg1)]);
end;

procedure TJitEmitter.CVTSI2SD_XMM_Reg(DestReg: EXMMReg; SourceReg: EReg);
begin
  WriteBytes([
    $F2,       // SSE prefix for double-precision scalar operations.
    $48,       // REX.W prefix indicating the GPR source is 64-bit.
    $0F, $2A,  // The two-byte opcode for CVTSI2SD.
    $C0 + (Ord(DestReg) * 8) + Ord(SourceReg) // ModR/M byte for register-to-register.
  ]);
end;

procedure TJitEmitter.CVTSI2SS_XMM_Reg32(DestReg: EXMMReg; SourceReg: EReg);
begin
  WriteBytes([
    $F3,       // SSE prefix for single-precision scalar operations.
    $0F, $2A,  // The two-byte opcode for CVTSI2SS.
    $C0 + (Ord(DestReg) * 8) + Ord(SourceReg) // ModR/M for register-to-register.
  ]);
end;

// Emits: cvtsi2ss xmm, r64 (e.g., cvtsi2ss xmm0, rax)
procedure TJitEmitter.CVTSI2SS_XMM_Reg64(DestReg: EXMMReg; SourceReg: EReg);
begin
  WriteBytes([
    $F3,       // SSE prefix.
    $48,       // REX.W prefix indicating the GPR source is 64-bit.
    $0F, $2A,  // The two-byte opcode for CVTSI2SS.
    $C0 + (Ord(DestReg) * 8) + Ord(SourceReg) // ModR/M byte for register-to-register.
  ]);
end;

procedure TJitEmitter.VCVTSI2SD_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
begin
  // Emits: vcvtsi2sd DestReg, DestReg, [BaseReg + disp32]
  // This uses the VEX encoding for AVX instructions.
  WriteBytes([
    $C4,                                        // VEX 3-byte prefix
    $E1,                                        // REX.R=1, REX.X=1, REX.B=0
    $7B,                                        // W=1, vvvv=xmm0(1111), L=0, pp=F2
    $2A,                                        // Opcode
    $80 + (Ord(DestReg) * 8) + Ord(BaseReg)     // ModR/M for [BaseReg + disp32]
  ]);
  PInt32(p)^ := Int32(Offset); Inc(p, 4);       // 32-bit displacement
end;

procedure TJitEmitter.VCVTSI2SS_XMM_Mem(DestReg: EXMMReg; BaseReg: EReg; Offset: Int64);
begin
  // ERR: WRONG
  raise Exception.Create('VCVTSI2SS_XMM_Mem: Not implemented');;
  // VEX(NDS, 128, F3, 0F, W0)
  WriteBytes([
    $C4,                                       // 3-byte VEX prefix
    $E1,                                       // R=1, X=1, B=1, m-mmmm=00001 (0F map)
    $F9,                                       // W=0, vvvv=~xmm0, L=0, pp=11 (F3)
    $2A,                                       // Opcode for CVTSI2SS
    $80 + (Ord(DestReg) * 8) + Ord(BaseReg)     // ModR/M for [BaseReg + disp32]
  ]);
  WriteBytes(@Offset, 4);                        // 32-bit displacement
end;


// Control flow

procedure TJitEmitter.TEST_Reg_Reg(Reg1, Reg2: EReg);
begin
  // TEST performs a bitwise AND but only sets flags, doesn't store the result.
  // TEST RAX, RAX is the fastest way to check if RAX is zero.
  WriteBytes([$48, $85, $C0 + (Ord(Reg2) * 8) + Ord(Reg1)]);
end;

function TJitEmitter.Jcc_Rel32(Condition: EJccCondition): PInt32;
begin
  // Emits a 2-byte opcode (0F 8x) and a 4-byte placeholder for the offset.
  // The specific '8x' byte depends on the condition.
  WriteBytes([$0F, $80 + Ord(Condition)]);
  Result := PInt32(p); // Return the address of the placeholder
  WriteBytes([$00,$00,$00,$00]); // Write the 4-byte zero placeholder
end;

function TJitEmitter.JMP_Rel32: PInt32;
begin
  // Emits a 1-byte opcode (E9) and a 4-byte placeholder.
  WriteBytes([$E9]);
  Result := PInt32(p); // Return the address of the placeholder
  WriteBytes([$00,$00,$00,$00]);
end;

procedure TJitEmitter.CMP_Reg_Imm32(Reg: EReg; Value: Int32);
begin
  // Emits: cmp r64, imm32
  if Reg = rax then
    WriteBytes([$48, $3D]) // Special short encoding for CMP RAX, imm32
  else
    WriteBytes([$48, $83, $F8 + Ord(Reg)]); // CMP r/m64, imm8
  WriteBytes(@Value, 4);
end;

procedure TJitEmitter.CMP_Reg_Reg(Reg1, Reg2: EReg);
begin
  // Emits: cmp r64, r64 (e.g., cmp rax, rcx)
  WriteBytes([$48, $39, $C0 + (Ord(Reg2) * 8) + Ord(Reg1)]);
end;

procedure TJitEmitter.SETcc(Condition: ESetccCondition; DestReg: EReg);
begin
  // Emits a SETcc instruction. This sets the low 8 bits of DestReg (e.g., AL)
  // to 1 if the condition is true, and 0 otherwise.
  // The upper 56 bits of the register are not modified.
  WriteBytes([$0F, $90 + Ord(Condition), $C0 + Ord(DestReg)]);
end;



// High level helpers

procedure TJitEmitter.Load_Int_Operand(const arg: TOperand; Reg: EReg);
begin
  if arg.Pos = mpLocal then
    case BaseJITType(arg.BaseType) of
      xtInt8:  MOVZX_Reg_Mem_i8(Reg, rbx, arg.Data.Addr);
      xtInt16: MOVZX_Reg_Mem_i16(Reg, rbx, arg.Data.Addr);
      xtInt32: MOVSXD_Reg_Mem_i32(Reg, rbx, arg.Data.Addr);
      xtInt64: MOV_Reg_Mem_i64(Reg, rbx, arg.Data.Addr);
    end
  else
    MOV_Reg_Imm64(Reg, arg.Data.arg);
end;

procedure TJitEmitter.Store_Int_Result(const arg: TOperand; Reg: EReg);
begin
  case BaseJITType(arg.BaseType) of
    xtInt8:  MOV_Mem_Reg_i8(rbx, arg.Data.Addr, Reg);
    xtInt16: MOV_Mem_Reg_i16(rbx, arg.Data.Addr, Reg);
    xtInt32: MOV_Mem_Reg_i32(rbx, arg.Data.Addr, Reg);
    xtInt64: MOV_Mem_Reg_i64(rbx, arg.Data.Addr, Reg);
  end;
end;

procedure TJitEmitter.Load_Float_Operand(const arg: TOperand; Reg: EXMMReg; TmpReg:EReg=RCX);
begin
  if arg.Pos = mpLocal then
    case BaseJITType(arg.BaseType) of
      xtSingle: MOVSS_XMM_Mem(Reg, rbx, arg.Data.Addr);
      xtDouble: MOVSD_XMM_Mem(Reg, rbx, arg.Data.Addr);
    end
  else
  begin
    case BaseJITType(arg.BaseType) of
      xtSingle:
        begin
          MOV_Reg_Imm32(TmpReg, arg.Data.i32);
          WriteBytes([$66, $0F, $6E, $C0 + (Ord(Reg) * 8) + Ord(TmpReg)]);
        end;
      xtDouble:
        begin
          MOV_Reg_Imm64(TmpReg, arg.Data.arg);
          WriteBytes([$66, $48, $0F, $6E, $C0 + (Ord(Reg) * 8) + Ord(TmpReg)]);
        end;
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

procedure TJitEmitter.Store_Int_Result_To_Addr(const arg: TOperand; ValueReg: EReg; AddrReg: EReg);
begin
  case BaseJITType(arg.BaseType) of
    xtInt8:  MOV_RegAddr_Reg_i8(AddrReg, ValueReg);
    xtInt16: MOV_RegAddr_Reg_i16(AddrReg, ValueReg);
    xtInt32: MOV_RegAddr_Reg_i32(AddrReg, ValueReg);
    xtInt64: MOV_RegAddr_Reg_i64(AddrReg, ValueReg);
  end;
end;

procedure TJitEmitter.Store_Float_Result_To_Addr(const arg: TOperand; ValueReg: EXMMReg; AddrReg: EReg);
begin
  case BaseJITType(arg.BaseType) of
    xtSingle: MOVSS_RegAddr_XMM(AddrReg, ValueReg);
    xtDouble: MOVSD_RegAddr_XMM(AddrReg, ValueReg);
    else
      Raise Exception.Create('What!?');
  end;
end;




// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// TXMMRegisterAllocator
// -----------------------------------------------------------------------------

procedure TXMMRegisterAllocator.Unset(Reg: EXMMReg);
begin
  {use typeinfo, position, and data to for equality}
  Regs[Reg].VarArg.Data.Addr := -1;
  Regs[Reg].VarArg.BaseType := xtUnknown;
  Regs[Reg].VarArg.Pos      := mpUnknown;
end;

function TXMMRegisterAllocator.Isset(Reg: EXMMReg): Boolean;
begin
  with Regs[Reg].VarArg do
    Result := not((BaseType = xtUnknown) and (Pos = mpUnknown) and (Data.Addr = -1));
end;

function TXMMRegisterAllocator.isEqual(Reg: EXMMReg; Operand: TOperand): Boolean;
begin
  with Regs[Reg].VarArg do
    Result := (BaseType = Operand.BaseType) and (Pos = Operand.Pos) and (Data.Addr = Operand.Data.Addr);
end;

procedure TXMMRegisterAllocator.Init(AEmitter: PJitEmitter);
var
  i: EXMMReg;
begin
  Self.Emitter := AEmitter;
  SetLength(Self.VIPMap, 0);
  Self.SpillCounter := 0;
  for i := Low(EXMMReg) to High(EXMMReg) do
  begin
    // Mark as empty
    Unset(i);
    Regs[i].IsDirty := False;
  end;
end;

procedure TXMMRegisterAllocator.Spill(Reg: EXMMReg);
var
  arg_to_spill: TOperand;
begin
  if Regs[Reg].IsDirty and (Isset(Reg)) then
  begin
    arg_to_spill := Regs[Reg].VarArg;
    Emitter^.Store_Float_Result(arg_to_spill, Reg);
  end;

  Regs[Reg].IsDirty := False;
  if Ord(Reg) >= Length(VIPMap) then
    Unset(Reg);
end;

function TXMMRegisterAllocator.FindVIP(Operand: TOperand; out Reg: EXMMReg): Boolean;
var
  i: Integer;
begin
  for i := 0 to High(VIPMap) do
  begin
    if isEqual(EXMMReg(i), operand) then
    begin
      Reg := VIPMap[i].Reg;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TXMMRegisterAllocator.FindFree: EXMMReg;
var
  i: EXMMReg;
begin
  for i := Low(EXMMReg) to High(EXMMReg) do
  begin
    if (not Isset(i)) {and (Ord(i) > High(VIPMap))} then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := EXMMReg(255);
end;

function TXMMRegisterAllocator.Evict(Exclude: TXMMRegSet = []): EXMMReg;
var
  i, num_vips: Integer;
  is_vip: Boolean;
begin
  num_vips := Length(VIPMap);
  while True do
  begin
    Result := EXMMReg(Self.SpillCounter);
    Self.SpillCounter := (Self.SpillCounter + 1) mod (Ord(High(EXMMReg)) + 1);

    // CRITICAL: Never evict an in-flight excluded register!
    if Result in Exclude then Continue;

    is_vip := False;
    for i := 0 to num_vips - 1 do
      if VIPMap[i].Reg = Result then
      begin
        is_vip := True;
        break;
      end;

    if not is_vip then
    begin
      if Regs[Result].IsDirty then
        Spill(Result);
      Exit;
    end;
  end;
end;

procedure TXMMRegisterAllocator.AnalyzeTrace(CodeList: PBytecodeInstruction; Count: Integer);
const
  // Reserve at least one register for scratch/temporaries.
  // Make this 2 for operations that need two scratch registers.
  RESERVED_SCRATCH_REGS = 3;
var
  FreqMap: array[0..$FFFF] of record Freq: Integer; VarInfo: TOperand; end;
  FreqList: TVarFrequencyArray;
  i, j, nUnique: Integer;
  bc_instr: TBytecodeInstruction;
  arg: TOperand;
  temp: TVarFrequency;
  reg_idx: EXMMReg;
  is_vip_reg: array[EXMMReg] of Boolean;

  MaxVIPs: Integer;
begin
  FillChar(FreqMap, SizeOf(FreqMap), 0);
  nUnique := 0;
  for i := 0 to Count - 1 do
  begin
    bc_instr := (CodeList + i)^;
    for j := 0 to bc_instr.nArgs - 1 do
    begin
      arg := bc_instr.Args[j];
      if (arg.Pos = mpLocal) and (BaseJITType(arg.BaseType) in [xtSingle, xtDouble]) then
      begin
        if (j=0) and InRange(Ord(bc_instr.Code), Ord(bcMOVH_i8_i8_ll), Ord(bcMOVH_f64_f64_li)) then
          continue;

        if arg.Data.Addr > High(FreqMap) then continue;
        if FreqMap[arg.Data.Addr].Freq = 0 then
        begin
          FreqMap[arg.Data.Addr].VarInfo := arg;
          Inc(nUnique);
        end;
        Inc(FreqMap[arg.Data.Addr].Freq);
      end;
    end;
  end;

  SetLength(FreqList, nUnique);
  i := 0;
  for j := 0 to High(FreqMap) do
  begin
    if FreqMap[j].Freq > 0 then
    begin
      if i <= High(FreqList) then
      begin
        FreqList[i].Count   := FreqMap[j].Freq;
        FreqList[i].VarInfo := FreqMap[j].VarInfo;
        Inc(i);
      end;
    end;
  end;

  for i := 0 to High(FreqList) - 1 do
    for j := i + 1 to High(FreqList) do
      if FreqList[j].Count > FreqList[i].Count then
      begin
        temp := FreqList[i];
        FreqList[i] := FreqList[j];
        FreqList[j] := temp;
      end;

  for reg_idx:=Low(EXMMReg) to High(EXMMReg) do
  begin
    Unset(reg_idx);
    Regs[reg_idx].IsDirty := False;
  end;

  MaxVIPs := Ord(High(EXMMReg)) - Ord(Low(EXMMReg)) + 1 - RESERVED_SCRATCH_REGS;
  if MaxVIPs < 0 then MaxVIPs := 0;

  SetLength(VIPMap, Min(Length(FreqList), MaxVIPs));
  for i := 0 to High(VIPMap) do
  begin
    reg_idx := EXMMReg(i);
    VIPMap[i].VarInfo := FreqList[i].VarInfo;
    VIPMap[i].Reg     := reg_idx;
    Regs[reg_idx].VarArg  := FreqList[i].VarInfo;
    Regs[reg_idx].IsDirty := False;
    Emitter^.Load_Float_Operand(FreqList[i].VarInfo, reg_idx);
  end;
end;

function TXMMRegisterAllocator.FindReg(Offset: PtrInt; out Reg: EXMMReg): Boolean;
var
  i: EXMMReg;
begin
  for i := Low(EXMMReg) to High(EXMMReg) do
  begin
    if Regs[i].VarArg.Data.Addr = Offset then
    begin
      Result := True;
      Reg := i;
      Exit;
    end;
  end;
  Result := False;
end;

function TXMMRegisterAllocator.GetFreeScratch(MarkAsUsed: Boolean; Exclude: TXMMRegSet=[]): EXMMReg;
var i: EXMMReg;
begin
  for i := EXMMReg(Ord(Low(EXMMReg)) + Length(Self.VIPMap)) to High(EXMMReg) do
  begin
    if (i in Exclude) then Continue;
    if not Isset(i) then
    begin
      Result := i;
      if MarkAsUsed then
      begin
        Regs[Result].VarArg.Data.Addr := -2;
        Regs[Result].VarArg.Pos := mpLocal;
      end;
      Exit;
    end;
  end;

  // Pass the Exclude list down to Evict!
  Result := Self.Evict(Exclude);

  if MarkAsUsed then
  begin
    Regs[Result].VarArg.Data.Addr := -2;
    Regs[Result].VarArg.Pos := mpLocal;
  end;
end;

function TXMMRegisterAllocator.GetReg(const arg: TOperand; Exclude: TXMMRegSet =[]): EXMMReg;
var i: EXMMReg;
begin
  if arg.Pos = mpImm then
  begin
    Result := GetFreeScratch(False, Exclude);
    Emitter^.Load_Float_Operand(arg, Result);
    Unset(Result);
    Regs[Result].VarArg.Data.Addr := -2;
    Regs[Result].IsDirty := False;
    Exit;
  end;

  if FindVIP(arg, Result) then Exit;

  for i := Low(EXMMReg) to High(EXMMReg) do
  begin
    if IsEqual(i, arg) then
    begin
      Result := i;
      Exit;
    end;
  end;

  Result := GetFreeScratch(False, Exclude);
  Emitter^.Load_Float_Operand(arg, Result);
  Regs[Result].VarArg  := arg;
  Regs[Result].IsDirty := False;
end;

(*
procedure TXMMRegisterAllocator.LoadImmediateToScratch(const arg: TOperand);
begin
  // This function is simple. It doesn't allocate. It just uses a known scratch reg.
  // It must still spill anything that might be in it from a previous op.
  Spill(xmm7);
  Emitter^.Load_Float_Operand(arg, xmm7);
  // Mark it as holding a temporary value so it can be evicted.
  Regs[xmm7].VarArg.Data.Addr := -2; // Magic number for "compiler temporary"
  Regs[xmm7].IsDirty := False;
end;
*)

// Make a throw-away temporary
function TXMMRegisterAllocator.Allocate(): EXMMReg;
begin
  Result := Self.FindFree();

  // simple round robin
  if Ord(Result) = 255 then
    Result := Self.Evict();
end;

procedure TXMMRegisterAllocator.SetResult(Reg: EXMMReg; const dest_arg: TOperand);
var
  vip_reg, final_reg: EXMMReg;
  is_reg_vip: Boolean;
  i: Integer;
begin
  // Check if the physical register 'Reg' is a dedicated VIP register
  is_reg_vip := False;
  for i := 0 to High(VIPMap) do
    if VIPMap[i].Reg = Reg then
    begin
      is_reg_vip := True;
      break;
    end;

  if FindVIP(dest_arg, vip_reg) then
  begin
    // Destination is a VIP
    final_reg := vip_reg;
    if vip_reg <> Reg then
    begin
      if BaseJITType(dest_arg.BaseType) = xtSingle then Emitter^.MOVSS_XMM_XMM(vip_reg, Reg)
      else Emitter^.MOVSD_XMM_XMM(vip_reg, Reg);
    end;
  end
  else
  begin
    // Destination is NON-VIP
    if is_reg_vip then
    begin
      // *** CRITICAL GUARD *** The source is a VIP register, we CANNOT hijack it!
      // Allocate a fresh scratch register and safely copy the value.
      final_reg := GetFreeScratch(False, [Reg]);
      if BaseJITType(dest_arg.BaseType) = xtSingle then Emitter^.MOVSS_XMM_XMM(final_reg, Reg)
      else Emitter^.MOVSD_XMM_XMM(final_reg, Reg);
    end
    else
    begin
      // It's a safe scratch register.
      if Isset(Reg) and (not IsEqual(Reg, dest_arg)) then
        Spill(Reg);
      final_reg := Reg;
    end;

    Regs[final_reg].VarArg := dest_arg;
  end;

  Regs[final_reg].IsDirty := True;
end;

procedure TXMMRegisterAllocator.SpillAllDirty;
var
  i: EXMMReg;
begin
  for i := Low(EXMMReg) to High(EXMMReg) do
    Spill(i);
end;

procedure TXMMRegisterAllocator.ClearAllConst;
var
  i: EXMMReg;
begin
  for i := Low(EXMMReg) to High(EXMMReg) do
    if Regs[i].VarArg.Data.Addr = -2 then Unset(i);
end;

procedure TXMMRegisterAllocator.InvalidateAll;
var
  i: EXMMReg;
  j: Int32;
  dummy: EXMMReg;
begin
  SpillAllDirty;

  // Clear non-VIP cached values
  for i := Low(EXMMReg) to High(EXMMReg) do
    if not FindVIP(Regs[i].VarArg, dummy) then
      Unset(i);

  // DO NOT RELOAD VIPS.
  // They live in registers for the whole trace!! WRONG BELLOW - KEEP REMINDER

  // Reload VIPs from memory - at a merge point, their register
  // values reflect the fall-through path only. Memory is the
  // only source of truth that both paths agree on.
  //for j:=0 to High(VIPMap) do
  //begin
  //  Emitter^.Load_Float_Operand(VIPMap[j].VarInfo, VIPMap[j].Reg);
  //  Regs[VIPMap[j].Reg].IsDirty := False;
  //end;
end;

end.
