unit xpr.Interpreter;
{
  Author: Jarl K. Holta  
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
{$I header.inc}
{$hints off}
{$R-}

interface

uses
  SysUtils,
  xpr.Types,
  xpr.Bytecode,
  xpr.Intermediate,
  xpr.BytecodeEmitter,
  xpr.Errors,
  Windows;

const
  STACK_SIZE = 4 * 1024 * 1024;  // 4MB static stack
  MAX_RECURSION_DEPTH = 10000;   // Recursion depth limit
  STACK_FRAME_SIZE = 64 * 1024;  // 64KB per stack frame (adjust as needed)
	
type
  TByteArray = array of Byte;
  PByteArray = ^TByteArray;

  TTranslateArray = array[EBytecode] of PtrUInt;

  TArgStack = record
    Data: array [0..$FFFF] of Pointer;
    Count: SizeInt;

    procedure Push(ref: Pointer); inline;
    function Pop(): Pointer; inline;
  end;

  TCallFrame = record
    ReturnAddress: PtrUInt;
    StackPtr: PByte;
    FrameSize: UInt16;
  end;

  TCallStack = record
    Frames: array[0..MAX_RECURSION_DEPTH-1] of TCallFrame; // Static array
    Top: Int32;
    procedure Init;
    procedure Push(ReturnAddr: PtrUInt; StackPtr: PByte); inline;
    function Pop: TCallFrame; inline;
    function Peek: TCallFrame; inline;
  end;

  TInterpreter = record
    ArgStack: TArgStack;
    CallStack: TCallStack;
    TryStack: TCallStack;

    RecursionDepth: Int32;
    ProgramStart: PtrUInt;

    // Tracking
    ProgramCounter: Int32;
    HasBuiltSuper: Boolean;

    // the stack
    Data: TByteArray;      // static stack is a tad faster, but limited to 2-4MB max
    StackPtr: PByte;       // Pointer to current stack position
    BasePtr: PByte;        // Base pointer for stack
    GlobalTop: SizeInt;

    procedure StackInit(Stack: TStackArray; StackPos: SizeInt);
    constructor New(Emitter: TBytecodeEmitter; StartPos: PtrUInt; Opt:EOptimizerFlags);

    function GetTop(size: SizeInt): Pointer; inline;
    function Global(offset: PtrUInt): Pointer; inline;

    function AsString(): string;

    function EmitCodeBlock(CodeList: PBytecodeInstruction; Translation: TTranslateArray; Count: Integer; var TotalSize: SizeInt): Pointer;
    procedure GenerateSuperInstructions(var BC: TBytecode; Translation: TTranslateArray);
    procedure RunSafe(var BC: TBytecode);
    procedure Run(var BC: TBytecode);

    procedure CallExternal(FuncPtr: Pointer; ArgCount: UInt16; hasReturn: Boolean);
    procedure HandleASGN(Instr: TBytecodeInstruction; HeapLeft: Boolean);
    procedure ArrayRefcount(Left, Right: Pointer);
    procedure IncRef(Left: Pointer);
    procedure DecRef(Left: Pointer);
  end;


implementation

uses
  Math, xpr.Utils;


{$I interpreter.functions.inc}


procedure PrintInt(v:Pointer; size:Byte);
begin
  case size of
    1: WriteLn('>>> ', Int8(v^), ' @ ', PtrInt(v));
    2: WriteLn('>>> ', Int16(v^), ' @ ', PtrInt(v));
    4: WriteLn('>>> ', Int32(v^), ' @ ', PtrInt(v));
    8: WriteLn('>>> ', Int64(v^), ' @ ', PtrInt(v));
  end;
end;

procedure PrintReal(v:Pointer; size:Byte);
begin
  case size of
    4: WriteLn(Format('%.4f', [Single(v^)]));
    8: WriteLn(Format('%.8f', [Double(v^)]));
  end;
end;



procedure TArgStack.Push(ref: Pointer);
begin
  Self.Data[Self.Count] := ref;
  Inc(Self.Count);
end;

function TArgStack.Pop(): Pointer;
begin
  Dec(Self.Count);
  Result := Self.Data[Self.Count];
end;


// TStack implementation
procedure TInterpreter.StackInit(Stack: TStackArray; StackPos: SizeInt);
begin
  SetLength(Data, STACK_SIZE);
  Move(stack[0], data[0], Min(STACK_SIZE, Length(Stack)));
  GlobalTop := StackPos;

  BasePtr  := @Data[0];
  StackPtr := @Data[0] + StackPos;
end;


function TInterpreter.GetTop(size: SizeInt): Pointer;
begin
  Result := StackPtr - size;
end;


// used by load_global, and invoke
function TInterpreter.Global(offset: PtrUInt): Pointer;
begin
  Result := (BasePtr + GlobalTop - offset);
end;

function TInterpreter.AsString(): string;
var i,i32: Int32; i64: Int64; p: PtrInt;
begin
  Result := '';
  i := 0;
  for p:=PtrInt(Self.BasePtr) to PtrInt(Self.StackPtr)-1 do
  begin

    if i mod 4 = 0 then begin
      Result += '>>> BasePtr+'+IntToStr(i) + ' ('+IntToStr(p)+'): ';
      Move(Pointer(p)^, i32, 4);
      Result += ' (i32 = '+IntTostr(i32) +')';
    end;


    if i mod 8 = 0 then begin
      Move(Pointer(p)^, i64, 8);
      Result += ' (i64 = '+IntTostr(i64) +')';
    end;

    if i mod 4 = 0 then Result += LineEnding;
    Inc(i);
  end;
end;



// TCallStack implementation
procedure TCallStack.Init;
begin
  Top := -1;
end;

procedure TCallStack.Push(ReturnAddr: PtrUInt; StackPtr: PByte);
begin
  Assert(Top < MAX_RECURSION_DEPTH-1, 'Call stack overflow (recursion too deep)');
  Inc(Top);
  Frames[Top].ReturnAddress := ReturnAddr;
  Frames[Top].StackPtr := StackPtr;
end;

function TCallStack.Pop: TCallFrame;
begin
  if Top < 0 then
    raise RuntimeError.Create('Call stack underflow');
  Result := Frames[Top];
  Dec(Top);
end;

function TCallStack.Peek: TCallFrame;
begin
  if Top < 0 then
    raise RuntimeError.Create('Call stack underflow');
  Result := Frames[Top];
end;

// TInterpreter implementation
constructor TInterpreter.New(Emitter: TBytecodeEmitter; StartPos: PtrUInt; Opt:EOptimizerFlags);
var
  i,j: Int32;
begin
  StackInit(Emitter.Stack, Emitter.UsedStackSize); //stackptr = after global allocations
  CallStack.Init();
  RecursionDepth := 0;
  ProgramStart   := StartPos;
  ProgramCounter := ProgramStart;
  ArgStack.Count := 0;

  for i:=0 to High(Emitter.Bytecode.FunctionTable) do
  begin
    PtrInt(Pointer(StackPtr - Emitter.Bytecode.FunctionTable[i].DataLocation)^) := Emitter.Bytecode.FunctionTable[i].CodeLocation;
  end;
end;



//----------------------------------------------------------------------------
// Copies exactly Count opcode blocks (including any inlined JZ blocks),
// *but* does not touch the RELJMP.
// totalSize RETURNS the number of bytes copied + 1 for the RET.
//----------------------------------------------------------------------------
function TInterpreter.EmitCodeBlock(CodeList: PBytecodeInstruction;
                                    Translation: TTranslateArray;
                                    Count: Integer;
                                var TotalSize: SizeInt): Pointer;
var
  offset: NativeUInt;
  dataPtrs: array of record start_, stop_: Pointer; end;
  oldProt: DWORD;
  j,k: Integer;
  code: EBytecode;
  ExecMem: Pointer;
begin
  // 1) Build temp table of (start,stop) pointers
  SetLength(dataPtrs, Count);
  TotalSize := 1;

  for j := 0 to Count - 1 do
  begin
    code := (CodeList + j)^.Code;

    dataPtrs[j].start_ := Pointer(Translation[code]);
    dataPtrs[j].stop_  := Pointer(Translation[EBytecode(Ord(code) + 1)]);
    Inc(TotalSize, NativeUInt(dataPtrs[j].stop_) - NativeUInt(dataPtrs[j].start_));
  end;

  // 2) Allocate RWX memory    {a bit extra}
  ExecMem := VirtualAlloc(nil, TotalSize, MEM_COMMIT or MEM_RESERVE,
                          PAGE_EXECUTE_READWRITE);

  // 3) Copy each snippet in order
  offset := 0;
  for j := 0 to Count - 1 do
  begin
    Move(dataPtrs[j].start_^, Pointer(NativeUInt(ExecMem) + offset)^,
         NativeUInt(dataPtrs[j].stop_) - NativeUInt(dataPtrs[j].start_));

    Inc(offset, NativeUInt(dataPtrs[j].stop_) - NativeUInt(dataPtrs[j].start_));
  end;

  PByte(NativeUInt(ExecMem) + offset)^ := $C3;
  Inc(offset, 1);

  // 5) Make it executable
  VirtualProtect(ExecMem, offset, PAGE_EXECUTE_READWRITE, @oldProt);

  Result := ExecMem;
end;

procedure TInterpreter.GenerateSuperInstructions(var BC: TBytecode; Translation: TTranslateArray);
var
  i, n: Integer;
  total: SizeInt;
  execMem: Pointer;

  function isLoop(): Boolean; inline;
  begin
    Result := (n-2 >= 0)
        and InRange(Ord(BC.Code.Data[n-2].Code), Ord(bcEQ_lll_i32), Ord(bcLTE_iil_f64))
        and (BC.Code.Data[n-1].Code = bcJZ)
        and (BC.Code.Data[i].Code   = bcRELJMP)
        and (BC.Code.Data[i].Args[0].Data.i32 = n-i-3);
  end;
begin
  i := 0;
  while i <= High(BC.Code.Data) do
  begin
    // Check if current opcode is eligible for fusion
    if ((InRange(Ord(BC.Code.Data[i].Code), Ord(bcADD_lll_i32), Ord(bcBOR_iil_u64))) or
       (InRange(Ord(BC.Code.Data[i].Code), Ord(bcMOV_i8_i8_ll), Ord(bcMOV_f64_f64_li))) or
       (InRange(Ord(BC.Code.Data[i].Code), Ord(bcMOVH_i8_i8_ll), Ord(bcMOVH_f64_f64_li))) or
       (InRange(Ord(BC.Code.Data[i].Code), Ord(bcINC_i32), Ord(bcFMAD_d32_32)))) and
       (BC.Code.Data[i].Code <> bcDREF)  then
    begin
      n := i;

      // Find how many eligible opcodes follow
      while (i <= High(BC.Code.Data)) and
            ((InRange(Ord(BC.Code.Data[i].Code), Ord(bcADD_lll_i32), Ord(bcBOR_iil_u64))) or
             (InRange(Ord(BC.Code.Data[i].Code), Ord(bcMOV_i8_i8_ll), Ord(bcMOV_f64_f64_li))) or
             (InRange(Ord(BC.Code.Data[i].Code), Ord(bcMOVH_i8_i8_ll), Ord(bcMOVH_f64_f64_li))) or
             (InRange(Ord(BC.Code.Data[i].Code), Ord(bcINC_i32), Ord(bcFMAD_d32_32)))) and
            (BC.Code.Data[i].Code <> bcDREF) do
      begin
        Inc(i);
      end;

      // control flow, so what comes before is likely cmp operation
      // we dont touch that as that might be a hotloop found in next iteration
      //if (i - n >= 2) and (BC.Code.Data[i].Code = bcJZ) then
      //  Dec(i);

      // Only generate a super-instruction if 2 or more instructions can be merged
      if (i - n >= 2) or isLoop() then
      begin
        if isLoop() then
        begin
          execMem := EmitCodeBlock(@BC.Code.Data[n-2], Translation, 1, total);
          BC.Code.Data[n-2].Code := bcHOTLOOP; // body in n+2
          BC.Code.Data[n-2].Args[4].Data.Arg := NativeInt(execMem);
        end;

        execMem := EmitCodeBlock(@BC.Code.Data[n], Translation, i - n, total);
        BC.Code.Data[n].Code := bcSUPER;
        BC.Code.Data[n].Args[4].Data.Arg := NativeInt(execMem); // Store ptr to code block
        BC.Code.Data[n].nArgs := 4;
      end;
    end
    else
      Inc(i);
  end;
end;


(*
  Interpreter doesnt truely support heap operations, all operations act on stack (local), and immediate, and global 
  
  MemPos explanation:
  * local means the variable is stored in `stack[args[x]+stackpos]`
  * global means var is stored in heap, but ptr to it is a stack var (like arrays)
  * imm [cant be assigned to, only right], where imm can contain anything up to 64 bits, a move should suffice. Even floats can be in imm, just typecast.


 Note: before run rewrite the BC into a packed format for faster dispactch?
       We can ignore things like type
       We can store opcode size and have variable size opcodes(???) - tricky for jumps, would need to rewrite jumps as well!!!

*)

procedure TInterpreter.RunSafe(var BC: TBytecode);
var
  TryFrame: TCallFrame;
begin
  Self.HasBuiltSuper := False;

  repeat
    try
      Self.Run(BC);
    except
      on E: Exception do
        WriteLn(E.ToString +' at line: ', BC.Docpos.Data[ProgramCounter].Line,
                            ', instr: ', BC.Code.Data[ProgramCounter].Code,
                            ', pc: ', ProgramCounter);
    end;

    TryFrame := TryStack.Pop();
    StackPtr := TryFrame.StackPtr;
    ProgramCounter := TryFrame.ReturnAddress;

  until TryStack.Top < 0;
end;


procedure TInterpreter.Run(var BC: TBytecode);
type
  TSuperMethod = procedure();
var
  nullpc,pc: ^TBytecodeInstruction;
  frame: TCallFrame;
  e: String;
  left, right: Pointer;
  JumpTable: TTranslateArray;
  hot_condeition: TSuperMethod;
label
  {$i interpreter.super.labels.inc}
begin
  (* should be allowed to disable easily in case not portable - and for debugging *)
  if not Self.HasBuiltSuper then
  begin
    {$i interpreter.super.bc2lb.inc}
    Self.GenerateSuperInstructions(BC, JumpTable);
    Self.HasBuiltSuper := True;
  end;

  nullpc := @BC.Code.Data[0];
  pc := @BC.Code.Data[ProgramCounter];

  while True do
  begin
    begin
      //WriteLn('*** ', ProgramCounter, ' && ', pc^.Code);
      case pc^.Code of
        bcNOOP: (* nothing *);

        bcHOTLOOP:
          begin
            left := Pointer(StackPtr - pc^.Args[2].Data.Addr);
            hot_condeition := TSuperMethod(pc^.Args[4].Data.Arg);
            while True do
            begin
              hot_condeition();                         //EQ, LT, GT etc..
              if not PBoolean(left)^ then               //JZ
              begin
                Inc(pc, pc^.Args[1].Data.i32);
                Break;
              end;
              Inc(pc);
              TSuperMethod(pc^.Args[4].Data.Arg)();     //body
              Inc(pc, pc^.Args[0].Data.i32+1);          //reljmp
            end;
          end;

        bcSUPER:
          begin
            TSuperMethod(pc^.Args[4].Data.Arg)();
            Dec(pc, 1);
          end;

        bcJMP: pc := @BC.Code.Data[pc^.Args[0].Data.i32];

        bcRELJMP: Inc(pc, pc^.Args[0].Data.i32);

        bcJZ: if not PBoolean(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ then Inc(pc, pc^.Args[1].Data.i32);
        bcJNZ: if PByte(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ <> 0  then Inc(pc, pc^.Args[1].Data.i32);

        bcJZ_i:  if pc^.Args[0].Data.Arg = 0  then Inc(pc, pc^.Args[1].Data.i32);
        bcJNZ_i: if pc^.Args[0].Data.Arg <> 0 then Inc(pc, pc^.Args[1].Data.i32);

        bcFILL:
          FillByte(Pointer(StackPtr - pc^.Args[0].Data.Addr)^, pc^.Args[1].Data.Addr, pc^.Args[2].Data.u8);

        // try except
        bcIncTry:
          TryStack.Push(pc^.args[0].Data.Addr, StackPtr);

        bcDecTry:
          TryStack.Pop();

        // array managment;
        bcINCLOCK:
          Self.IncRef(Pointer(Pointer(StackPtr - pc^.Args[0].Data.Addr)^));
        bcDECLOCK:
          Self.DecRef(Pointer(Pointer(StackPtr - pc^.Args[0].Data.Addr)^));
        bcREFCNT:
          Self.ArrayRefcount(Pointer(Pointer(StackPtr - pc^.Args[0].Data.Addr)^), Pointer(Pointer(StackPtr - pc^.Args[1].Data.Addr)^));

        bcREFCNT_imm:
          ArrayRefcount(Pointer(Pointer(StackPtr - pc^.Args[0].Data.Addr)^), Pointer(pc^.Args[1].Data.Addr));

        bcASGN_bs:
          PAnsiString(StackPtr - pc^.Args[0].Data.Addr)^ := BC.StringTable[pc^.Args[1].Data.Addr];

        bcADD_bs_ll:
          PAnsiString(StackPtr - pc^.Args[2].Data.Addr)^ := PAnsiString(StackPtr - pc^.Args[0].Data.Addr)^ + PAnsiString(StackPtr - pc^.Args[1].Data.Addr)^;

        bcADD_bs_li:
          PAnsiString(StackPtr - pc^.Args[2].Data.Addr)^ := PAnsiString(StackPtr - pc^.Args[0].Data.Addr)^ + BC.StringTable[pc^.Args[1].Data.Addr];

        bcADD_bs_il:
          PAnsiString(StackPtr - pc^.Args[2].Data.Addr)^ := BC.StringTable[pc^.Args[0].Data.Addr] + PAnsiString(StackPtr - pc^.Args[1].Data.Addr)^;

        bcADD_bs_ii:
          PAnsiString(StackPtr - pc^.Args[2].Data.Addr)^ := BC.StringTable[pc^.Args[0].Data.Addr] + BC.StringTable[pc^.Args[1].Data.Addr];


        {$I interpreter.super.binary_code.inc}
        {$I interpreter.super.asgn_code.inc}

        bcINC_i32: Inc( PInt32(Pointer(StackPtr - pc^.Args[0].Data.Addr))^);
        bcINC_u32: Inc(PUInt32(Pointer(StackPtr - pc^.Args[0].Data.Addr))^);
        bcINC_i64: Inc( PInt64(Pointer(StackPtr - pc^.Args[0].Data.Addr))^);
        bcINC_u64: Inc(PUInt64(Pointer(StackPtr - pc^.Args[0].Data.Addr))^);

        bcFMA_i8:  PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PInt8(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr;
        bcFMA_u8:  PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PUInt8(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr;
        bcFMA_i16: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PInt16(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr;
        bcFMA_u16: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PUInt16(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr;
        bcFMA_i32: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PInt32(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr;
        bcFMA_u32: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PUInt32(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr;
        bcFMA_i64: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PInt64(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr;
        bcFMA_u64: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PUInt64(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr;

        bcFMA_imm_i8:  PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + Int8(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr;
        bcFMA_imm_u8:  PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + UInt8(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr;
        bcFMA_imm_i16: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + Int16(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr;
        bcFMA_imm_u16: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + UInt16(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr;
        bcFMA_imm_i32: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + Int32(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr;
        bcFMA_imm_u32: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + UInt32(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr;
        bcFMA_imm_i64: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + Int64(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr;
        bcFMA_imm_u64: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + UInt64(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr;


        bcDREF: Move(Pointer(Pointer(StackPtr - pc^.Args[1].Data.Addr)^)^, Pointer(StackPtr - pc^.Args[0].Data.Addr)^, pc^.Args[2].Data.Addr);
        bcDREF_32: PUInt32(StackPtr - pc^.Args[0].Data.Addr)^ := PUInt32(Pointer(StackPtr - pc^.Args[1].Data.Addr)^)^;
        bcDREF_64: PUInt64(StackPtr - pc^.Args[0].Data.Addr)^ := PUInt64(Pointer(StackPtr - pc^.Args[1].Data.Addr)^)^;

        bcFMAD_d64_64:
          PInt64(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PInt64(PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PInt64(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr)^;

        bcFMAD_d64_32:
          PInt64(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PInt64(PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PInt32(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr)^;

        bcFMAD_d32_64:
          PInt32(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PInt64(PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PInt64(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr)^;

        bcFMAD_d32_32:
          PInt32(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PInt64(PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PInt32(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr)^;

        // MOV for other stuff
        bcMOV, bcMOVH:
          HandleASGN(pc^, pc^.Code=bcMOVH);

        // push the address of the variable  / value (a reference)
        //
        bcPUSH:
          if pc^.Args[0].Pos = mpLocal then
            ArgStack.Push(Pointer(StackPtr - pc^.Args[0].Data.Addr))
          else
            ArgStack.Push(@pc^.Args[0].Data.Arg);

        bcPUSHREF:
          ArgStack.Push(Pointer(Pointer(StackPtr - pc^.Args[0].Data.Addr)^));

        // pop [and derefence] - write pop to stack
        // function arguments are references, write the value (a copy)
        bcPOP:
          Move(ArgStack.Pop()^, Pointer(StackPtr - pc^.Args[1].Data.Addr)^, pc^.Args[0].Data.Addr);

        // pop [and derefence] - write ptr to pop
        // if argstack contains a pointer we can write a local value to
        bcRPOP:
          Move(Pointer(StackPtr - pc^.Args[0].Data.Addr)^, ArgStack.Pop()^,  pc^.Args[1].Data.Addr);

        // pop [as refence] - write pop to stack
        // function arguments are references, write the address to the var
        bcPOPH:
          Pointer(Pointer(StackPtr - pc^.Args[0].Data.Addr)^) := ArgStack.Pop();

        // using a global in local scope, assign it's reference
        bcLOAD_GLOBAL:
          Pointer(Pointer(StackPtr - pc^.Args[0].Data.Addr)^) := Global(pc^.Args[1].Data.Addr);

        bcCOPY_GLOBAL:
          Pointer(Pointer(StackPtr - pc^.Args[0].Data.Addr)^) := Pointer(Global(pc^.Args[1].Data.Addr)^);

        bcNEWFRAME:
          begin            {stackptr contains = pc}
            CallStack.Push(PPtrInt(StackPtr)^, StackPtr);
            PPtrInt(StackPtr)^ := 0;
            // This might save us from a lot of bullshit:
            // FillByte(StackPtr^, pc^.Args[0].Data.Addr+SizeOf(Pointer), 0);
            StackPtr += pc^.Args[0].Data.Addr; //inc by frame
          end;

        bcINVOKE:
          begin
            Inc(Self.RecursionDepth);
            Pointer(Pointer(StackPtr)^) := Pointer(pc);
            pc := @BC.Code.Data[PtrInt(Global(pc^.Args[0].Data.Addr)^)];
          end;

        bcINVOKEX:
          CallExternal(Pointer(pc^.Args[0].Data.Addr), pc^.Args[1].Data.Arg, pc^.Args[2].Data.Arg <> 0);

        bcRET:
          begin
            if CallStack.Top > -1 then
            begin
              // return value
              if pc^.nArgs = 2 then
                Move(Pointer(StackPtr - pc^.Args[0].Data.Addr)^, ArgStack.Pop()^, pc^.Args[1].Data.Addr);

              frame := CallStack.Pop;
              StackPtr := Frame.StackPtr;
              Dec(RecursionDepth);
              pc := Pointer(frame.ReturnAddress);
            end else
              Break;
          end;

        bcPRTi:
          case pc^.Args[0].Pos of
            mpImm:    PrintInt(@pc^.Args[0].Data.Arg, 8);
            mpLocal:  PrintInt(Pointer(StackPtr - pc^.Args[0].Data.Addr), XprTypeSize[pc^.Args[0].BaseType]);
          end;
        bcPRTf:
          case pc^.Args[0].Pos of
            mpLocal: PrintReal(Pointer(StackPtr - pc^.Args[0].Data.Addr), XprTypeSize[pc^.Args[0].BaseType]);
            mpImm:   PrintReal(@pc^.Args[0].Data.Raw, XprTypeSize[pc^.Args[0].BaseType]);
          end;
        bcPRTb:
          case pc^.Args[0].Pos of
            mpLocal:  WriteLn(Boolean(Pointer(StackPtr - pc^.Args[0].Data.Addr)^));
            mpImm:    WriteLn(Boolean(@pc^.Args[0].Data.Arg));
          end;
        bcPRT:
          case pc^.Args[0].Pos of
            mpLocal:  WriteLn(PAnsiString(StackPtr - pc^.Args[0].Data.Addr)^);
            mpImm:    WriteLn(BC.StringTable[pc^.Args[0].Data.Addr]);
          end;



        else
          begin
            WriteStr(e, pc^.code);
            raise RuntimeError.Create('Not implemented @ - ' + e);
          end;
      end;
    end;

    Inc(pc);
    Self.ProgramCounter := (PtrUInt(PC)-PtrUInt(nullpc)) div SizeOf(TBytecodeInstruction);
  end;

  Exit;


  (* labeled opcodes for selective inlining *)
  {$i interpreter.super.fmad.inc}
  {$i interpreter.super.binary.inc}
  {$i interpreter.super.asgn.inc}
end;


procedure TInterpreter.ArrayRefcount(Left, Right: Pointer);
type
  TArrayRec = record Refcount, High: SizeInt; Data: Pointer; end;
begin
  if (Left = nil) and (Right = nil) then Exit;

  if (Right = nil) then
  begin
    Dec(TArrayRec((Left-SizeOf(SizeInt)*2)^).Refcount);
  end else if Left = nil then
  begin
    Inc(TArrayRec((Right-SizeOf(SizeInt)*2)^).Refcount);
  end else if PtrInt(Left^) <> PtrInt(Right^) then
  begin
    Dec(TArrayRec((Left-SizeOf(SizeInt)*2)^).Refcount);
    Inc(TArrayRec((Right-SizeOf(SizeInt)*2)^).Refcount);
  end;

  // 1) after this comes some opcode(s), usually assign
  // 2) then followed by that we emit a call to Collect(Left)
  //  - Collect will be if left.refcount = 0 then SetLength(left, 0)
  //  - Left may or may not be collected now.
end;

procedure TInterpreter.IncRef(Left: Pointer);
type TArrayRec = record Refcount, High: SizeInt; Data: Pointer; end;
begin
  if (left <> nil) then Inc(TArrayRec((Left-SizeOf(SizeInt)*2)^).Refcount);
end;

procedure TInterpreter.DecRef(Left: Pointer);
type TArrayRec = record Refcount, High: SizeInt; Data: Pointer; end;
begin
  if left <> nil then Dec(TArrayRec((Left-SizeOf(SizeInt)*2)^).Refcount);
end;

(*
  No base types should be handled here, this is assignment between equal datasizes
  Again: left and right must be same size, or left larger than right,
  third argument is the datasize.
*)
procedure TInterpreter.HandleASGN(Instr: TBytecodeInstruction; HeapLeft: Boolean);
begin
  if not HeapLeft then
  begin
    if Instr.Args[1].Pos = mpImm then
      Move(
        Pointer(Instr.Args[1].Data.Arg)^,
        Pointer(Pointer(StackPtr - Instr.Args[0].Data.Addr))^,
        Instr.Args[2].Data.i32)
    else
      Move(
        Pointer(Pointer(StackPtr - Instr.Args[1].Data.Addr))^,
        Pointer(Pointer(StackPtr - Instr.Args[0].Data.Addr))^,
        Instr.Args[2].Data.i32);
  end else
  begin
    if Instr.Args[1].Pos = mpImm then
      Move(
        Pointer(Instr.Args[1].Data.Arg)^,
        Pointer(Pointer(Pointer(StackPtr - Instr.Args[0].Data.Addr))^)^,
        Instr.Args[2].Data.i32)
    else
      Move(
        Pointer(Pointer(StackPtr - Instr.Args[1].Data.Addr))^,
        Pointer(Pointer(Pointer(StackPtr - Instr.Args[0].Data.Addr))^)^,
        Instr.Args[2].Data.i32);
  end;
end;

procedure TInterpreter.CallExternal(FuncPtr: Pointer; ArgCount: UInt16; hasReturn: Boolean);
begin
  if (ArgCount > 0) then
  begin

    if hasReturn then
      TExternalFunc(FuncPtr)(@ArgStack.Data[1 + (ArgStack.Count - ArgCount)], ArgStack.Data[ArgStack.Count-ArgCount])
    else
      TExternalProc(FuncPtr)(@ArgStack.Data[ArgStack.Count - ArgCount]);

    ArgStack.Count -= ArgCount;
  end
  else
    TExternalProc(FuncPtr)(nil);
end;

end.
