unit xpr.Interpreter_dt;
{
  Author: Jarl K. Holta  
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
{$I header.inc}
{$hints off}
{$R-}
interface

uses
  SysUtils, xpr.Types, xpr.Bytecode, xpr.BytecodeEmitter, xpr.Errors;

const
  STACK_SIZE = 16 * 1024 * 1024;          // 16MB static stack
  MAX_RECURSION_DEPTH = 1000;    // Recursion depth limit
  STACK_FRAME_SIZE = 64 * 1024;  // 64KB per stack frame (adjust as needed)
	
type
  TByteArray = array of Byte;
  PByteArray = ^TByteArray;
  
  TPointerArray = array[EBytecode] of PtrUInt;

  TRuntimeCode = record
    Address: Pointer;
    Args: array[0..4] of TOperand;
    nArgs: Byte;
  end;
  PRuntimeCode = ^TRuntimeCode;

  TArgStack = record
    Data: array [0..$FF] of Pointer;
    Count: SizeInt;

    procedure Push(ref: Pointer); inline;
    function Pop(): Pointer; inline;
  end;

  TStack = packed record
    Data: TByteArray;      // static stack is a tad faster, but limited to 2-4MB max
    StackPtr: PByte;       // Pointer to current stack position
    BasePtr: PByte;        // Base pointer for stack
    GlobalTop: SizeInt;

    procedure Init(Stack: TStackArray; StackPos: SizeInt);

    function GetTop(size: SizeInt): Pointer; inline;
    function Local(offset: PtrUInt): Pointer; inline;
    function Global(offset: PtrUInt): Pointer; inline;

    function AsString(): string;
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
    Stack: TStack;

    ArgStack: TArgStack;
    CallStack: TCallStack;
    TryStack: TCallStack;

    RecursionDepth: Int32;
    ProgramStart: PtrUInt;
    ProgramCounter: Int32;

    constructor New(Emitter: TBytecodeEmitter; StartPos: PtrUInt; Opt:EOptimizerFlags);

    procedure RunSafe(var BC: TBytecode);
    procedure Run(var BC: TBytecode);

    procedure CallExternal(FuncPtr: Pointer; ArgCount: UInt16; hasReturn: Boolean);
    procedure HandleASGN(Instr: TBytecodeInstruction);
    procedure ArrayRefcount(Left, Right: Pointer);
  end;


implementation

uses
  Math, Windows;


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
procedure TStack.Init(Stack: TStackArray; StackPos: SizeInt);
begin
  SetLength(Data, STACK_SIZE);
  Move(stack[0], data[0], Min(STACK_SIZE, Length(Stack)));
  GlobalTop := StackPos;

  BasePtr  := @Data[0];
  StackPtr := @Data[0] + StackPos;
end;


function TStack.GetTop(size: SizeInt): Pointer;
begin
  Result := StackPtr - size;
end;

// stackptr is always ahead, so negative indexing
function TStack.Local(offset: PtrUInt): Pointer;
begin
  Result := StackPtr - offset;
end;


// used by load_global, and invoke
function TStack.Global(offset: PtrUInt): Pointer;
begin
  Result := (BasePtr + GlobalTop - offset);
end;

function TStack.AsString(): string;
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
  if Top >= MAX_RECURSION_DEPTH-1 then
    raise RuntimeError.Create('Call stack overflow (recursion too deep)');
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
  i: Int32;
begin
  Stack.Init(Emitter.Stack, Emitter.UsedStackSize); //stackptr = after global allocations
  CallStack.Init();
  RecursionDepth := 0;
  ProgramStart   := StartPos;
  ProgramCounter := ProgramStart;
  ArgStack.Count := 0;

  for i:=0 to High(Emitter.Bytecode.FunctionTable) do
  begin
    PtrInt(Stack.Local(Emitter.Bytecode.FunctionTable[i].DataLocation)^) := Emitter.Bytecode.FunctionTable[i].CodeLocation;
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
  //repeat
    //try
      Self.Run(BC);
    (*except
      on E: Exception do
        WriteLn(E.ToString +' at line: ', BC.Docpos.Data[ProgramCounter].Line,
                            ', instr: ', BC.Code.Data[ProgramCounter].Code,
                            ', pc: ', ProgramCounter);
    end;  *)

    TryFrame := TryStack.Pop();
    Stack.StackPtr := TryFrame.StackPtr;
    ProgramCounter := TryFrame.ReturnAddress;

  //until TryStack.Top < 0;
end;

(*
  Slow because regvars are not really used, preferably pc, and some other vars
  should live in register during the whole run, they are loaded and removed instead.

  Dispatch:
    C:\express\rev7\compiler\xprInterpreterDirect.pas:305  Inc(pc);
    00000001000598D4 488B55F0                 mov rdx,[rbp-$10]
    00000001000598D8 8B45EC                   mov eax,[rbp-$14]
    00000001000598DB 8982C8C30000             mov [rdx+$0000C3C8],eax
    C:\express\rev7\compiler\xprInterpreterDirect.pas:307
    00000001000598E1 488B45B0                 mov rax,[rbp-$50]
    00000001000598E5 486355EC                 movsxd rdx,dword ptr [rbp-$14]
    00000001000598E9 488B04D0                 mov rax,[rdx*8+rax]
    00000001000598ED 488985E8E6FFFF           mov [rbp-$00001918],rax
    C:\express\rev7\compiler\xprInterpreterDirect.pas:308  loc := DirectCode[pc];
    00000001000598F4 FFA5E8E6FFFF             jmp qword ptr [rbp-$00001918]

 So we are gonna use what we created instead to make SUPERINSTRUCTIONS,
 and live with the fact that FPC cant handle computed goto / direct threaded interpreters
*)
procedure DispatchCall(var Data: PRuntimeCode); nostackframe; assembler;
asm
  mov rcx, Data
  jmp [rcx];
end;

procedure TInterpreter.Run(var BC: TBytecode);
type
  TTranslateArray = array[EBytecode] of PtrUInt;
  TSuperMethod = procedure();
var
  i: Int32;
  frame: TCallFrame;
  left, right: Pointer;
  
  Data: PRuntimeCode;
  DirectCode: array of TRuntimeCode;

  JumpTable: TTranslateArray;

// include all labels
// bcADD_... is named lbADD_... etc.. so prefix of "bc" just changed to "lb"
label
  {$i xpri_directthreaded_labels.inc}
begin
  // creates the Data array
  // it's just like currect instruction, except .Code -> .Address now
  {$i xpri_directthreaded_bc2lb.inc}

  // Copy the program
  SetLength(DirectCode, BC.Code.High+1);
  for i:=0 to BC.Code.High do
  begin
    DirectCode[i].Address := Pointer(JumpTable[BC.Code.Data[i].Code]);
    DirectCode[i].Args    := BC.Code.Data[i].Args;
    DirectCode[i].nArgs   := BC.Code.Data[i].nArgs;
  end;

  Data := @DirectCode[ProgramCounter];

  {$asmmode intel}

  // assembler as FPC doesnt truely support label as value
  {$DEFINE DISPATCH :=
    // actual dispatch
    Inc(Data);

    asm
      mov rcx, Data
      jmp [rcx];
    end;

    //DispatchCall(Data);
  }


  asm
    mov rcx, Data
    jmp [rcx];
  end;

  //DispatchCall(Data);
  
  lbNOOP:
    DISPATCH;

  lbJMP:
    Data := @DirectCode[Data^.Args[0].Data.i32]; // Target is an absolute instruction index
    DISPATCH;

  lbRELJMP:
    Inc(Data, Data^.Args[0].Data.i32); // Target is a relative instruction index
    DISPATCH;

  lbJZ:
    if not PBoolean(Stack.Local(Data^.Args[0].Data.Addr))^ then
      Inc(Data, Data^.Args[1].Data.i32); // Jump if condition is true
    DISPATCH; // Continue to next or new jumped instruction

  lbJNZ:
    if PByte(Stack.Local(Data^.Args[0].Data.Addr))^ <> 0 then
      Inc(Data, Data^.Args[1].Data.i32); // Jump if condition is true
    DISPATCH; // Continue to next or new jumped instruction

  lbJZ_i:
    if Data^.Args[0].Data.Arg = 0 then
      Inc(Data, Data^.Args[1].Data.i32);
    DISPATCH;

  lbJNZ_i:
    if Data^.Args[0].Data.Arg <> 0 then
      Inc(Data, Data^.Args[1].Data.i32);
    DISPATCH;

  lbFILL:
    FillByte(Stack.Local(Data^.Args[0].Data.Addr)^, Data^.Args[1].Data.Addr, Data^.Args[2].Data.u8);
    DISPATCH;

  lbIncTry:
    TryStack.Push(Data^.Args[0].Data.Addr, Stack.StackPtr);
    DISPATCH;

  lbDecTry:
    TryStack.Pop();
    DISPATCH;

  lbREFCNT:
    ArrayRefcount(Pointer(Stack.Local(Data^.Args[0].Data.Addr)^), Pointer(Stack.Local(Data^.Args[1].Data.Addr)^));
    DISPATCH;

  lbREFCNT_imm:
    ArrayRefcount(Pointer(Stack.Local(Data^.Args[0].Data.Addr)^), Pointer(Data^.Args[1].Data.Addr));
    DISPATCH;

  lbBCHK:
    DISPATCH;

  {$I interpreter.binary_code_direct_threaded.inc}
  {$I interpreter.asgn_code_direct_threaded.inc}

  // --- Specialized FMA operations ---
  lbFMA_i8:  PPtrInt(Stack.Local(Data^.Args[3].Data.Addr))^ := PPtrInt(Stack.Local(Data^.Args[2].Data.Addr))^ + PInt8(Stack.Local(Data^.Args[0].Data.Addr))^ * Data^.Args[1].Data.Addr; DISPATCH;
  lbFMA_u8:  PPtrInt(Stack.Local(Data^.Args[3].Data.Addr))^ := PPtrInt(Stack.Local(Data^.Args[2].Data.Addr))^ + PUInt8(Stack.Local(Data^.Args[0].Data.Addr))^ * Data^.Args[1].Data.Addr; DISPATCH;
  lbFMA_i16: PPtrInt(Stack.Local(Data^.Args[3].Data.Addr))^ := PPtrInt(Stack.Local(Data^.Args[2].Data.Addr))^ + PInt16(Stack.Local(Data^.Args[0].Data.Addr))^ * Data^.Args[1].Data.Addr; DISPATCH;
  lbFMA_u16: PPtrInt(Stack.Local(Data^.Args[3].Data.Addr))^ := PPtrInt(Stack.Local(Data^.Args[2].Data.Addr))^ + PUInt16(Stack.Local(Data^.Args[0].Data.Addr))^ * Data^.Args[1].Data.Addr; DISPATCH;
  lbFMA_i32: PPtrInt(Stack.Local(Data^.Args[3].Data.Addr))^ := PPtrInt(Stack.Local(Data^.Args[2].Data.Addr))^ + PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ * Data^.Args[1].Data.Addr; DISPATCH;
  lbFMA_u32: PPtrInt(Stack.Local(Data^.Args[3].Data.Addr))^ := PPtrInt(Stack.Local(Data^.Args[2].Data.Addr))^ + PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ * Data^.Args[1].Data.Addr; DISPATCH;
  lbFMA_i64: PPtrInt(Stack.Local(Data^.Args[3].Data.Addr))^ := PPtrInt(Stack.Local(Data^.Args[2].Data.Addr))^ + PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ * Data^.Args[1].Data.Addr; DISPATCH;
  lbFMA_u64: PPtrInt(Stack.Local(Data^.Args[3].Data.Addr))^ := PPtrInt(Stack.Local(Data^.Args[2].Data.Addr))^ + PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ * Data^.Args[1].Data.Addr; DISPATCH;

  lbFMA_imm_i8:  PPtrInt(Stack.Local(Data^.Args[3].Data.Addr))^ := PPtrInt(Stack.Local(Data^.Args[2].Data.Addr))^ + Int8(Data^.Args[0].Data.Addr) * Data^.Args[1].Data.Addr; DISPATCH;
  lbFMA_imm_u8:  PPtrInt(Stack.Local(Data^.Args[3].Data.Addr))^ := PPtrInt(Stack.Local(Data^.Args[2].Data.Addr))^ + UInt8(Data^.Args[0].Data.Addr) * Data^.Args[1].Data.Addr; DISPATCH;
  lbFMA_imm_i16: PPtrInt(Stack.Local(Data^.Args[3].Data.Addr))^ := PPtrInt(Stack.Local(Data^.Args[2].Data.Addr))^ + Int16(Data^.Args[0].Data.Addr) * Data^.Args[1].Data.Addr; DISPATCH;
  lbFMA_imm_u16: PPtrInt(Stack.Local(Data^.Args[3].Data.Addr))^ := PPtrInt(Stack.Local(Data^.Args[2].Data.Addr))^ + UInt16(Data^.Args[0].Data.Addr) * Data^.Args[1].Data.Addr; DISPATCH;
  lbFMA_imm_i32: PPtrInt(Stack.Local(Data^.Args[3].Data.Addr))^ := PPtrInt(Stack.Local(Data^.Args[2].Data.Addr))^ + Int32(Data^.Args[0].Data.Addr) * Data^.Args[1].Data.Addr; DISPATCH;
  lbFMA_imm_u32: PPtrInt(Stack.Local(Data^.Args[3].Data.Addr))^ := PPtrInt(Stack.Local(Data^.Args[2].Data.Addr))^ + UInt32(Data^.Args[0].Data.Addr) * Data^.Args[1].Data.Addr; DISPATCH;
  lbFMA_imm_i64: PPtrInt(Stack.Local(Data^.Args[3].Data.Addr))^ := PPtrInt(Stack.Local(Data^.Args[2].Data.Addr))^ + Int64(Data^.Args[0].Data.Addr) * Data^.Args[1].Data.Addr; DISPATCH;
  lbFMA_imm_u64: PPtrInt(Stack.Local(Data^.Args[3].Data.Addr))^ := PPtrInt(Stack.Local(Data^.Args[2].Data.Addr))^ + UInt64(Data^.Args[0].Data.Addr) * Data^.Args[1].Data.Addr; DISPATCH;

  // --- DREF operations ---
  lbDREF: Move(Pointer(Stack.Local(Data^.Args[1].Data.Arg)^)^, Stack.Local(Data^.Args[0].Data.Arg)^, Data^.Args[2].Data.Arg); DISPATCH;
  lbDREF_32: PUInt32(Stack.Local(Data^.Args[0].Data.Arg))^ := PUInt32(Stack.Local(Data^.Args[1].Data.Arg)^)^; DISPATCH;
  lbDREF_64: PUInt64(Stack.Local(Data^.Args[0].Data.Arg))^ := PUInt64(Stack.Local(Data^.Args[1].Data.Arg)^)^; DISPATCH;

  // --- FMAD operations ---
  lbFMAD_d64_64: PInt64(Stack.Local(Data^.Args[3].Data.Addr))^ := PInt64(PPtrInt(Stack.Local(Data^.Args[2].Data.Addr))^ + PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ * Data^.Args[1].Data.Addr)^; DISPATCH;
  lbFMAD_d64_32: PInt64(Stack.Local(Data^.Args[3].Data.Addr))^ := PInt64(PPtrInt(Stack.Local(Data^.Args[2].Data.Addr))^ + PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ * Data^.Args[1].Data.Addr)^; DISPATCH;
  lbFMAD_d32_64: PInt32(Stack.Local(Data^.Args[3].Data.Addr))^ := PInt64(PPtrInt(Stack.Local(Data^.Args[2].Data.Addr))^ + PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ * Data^.Args[1].Data.Addr)^; DISPATCH;
  lbFMAD_d32_32: PInt32(Stack.Local(Data^.Args[3].Data.Addr))^ := PInt64(PPtrInt(Stack.Local(Data^.Args[2].Data.Addr))^ + PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ * Data^.Args[1].Data.Addr)^; DISPATCH;

  lbMOV:
    //HandleASGN(Data^); // Pass the current instruction
    DISPATCH;

  lbMOVH:
    {Nothing}
    DISPATCH;

  lbPUSH:
    if Data^.Args[0].Pos = mpLocal then
      ArgStack.Push(Stack.Local(Data^.Args[0].Data.Addr))
    else
      ArgStack.Push(@Data^.Args[0].Data.Arg); // Pass address of immediate value
    DISPATCH;

  lbPUSHREF:
    ArgStack.Push(Pointer(Stack.Local(Data^.Args[0].Data.Addr)^));
    DISPATCH;

  lbPOP:
    Move(ArgStack.Pop()^, Stack.Local(Data^.Args[1].Data.Addr)^, Data^.Args[0].Data.Addr);
    DISPATCH;

  lbRPOP:
    Move(Stack.Local(Data^.Args[0].Data.Addr)^, ArgStack.Pop()^, Data^.Args[1].Data.Addr);
    DISPATCH;

  lbPOPH:
    Pointer(Stack.Local(Data^.Args[0].Data.Addr)^) := ArgStack.Pop();
    DISPATCH;

  lbLOAD_GLOBAL:
    Pointer(Stack.Local(Data^.Args[0].Data.Addr)^) := Stack.Global(Data^.Args[1].Data.Addr);
    DISPATCH;

  lbCOPY_GLOBAL:
    Pointer(Stack.Local(Data^.Args[0].Data.Addr)^) := Pointer(Stack.Global(Data^.Args[1].Data.Addr)^);
    DISPATCH;

  lbNEWFRAME:
    CallStack.Push(PPtrInt(Stack.StackPtr)^, Stack.StackPtr);
    PPtrInt(Stack.StackPtr)^ := 0;
    Stack.StackPtr += Data^.Args[0].Data.Addr; //inc by frame
    DISPATCH;

  lbINVOKE:
    // Check recursion depth
    if RecursionDepth >= MAX_RECURSION_DEPTH then
      raise RuntimeError.Create('Recursion depth limit exceeded');

    Inc(RecursionDepth);
    PPointer(Stack.StackPtr)^ := Pointer(Data);
    Data := @DirectCode[PtrInt(Stack.Global(Data^.Args[0].Data.Addr)^)];
    //Data := Pointer();
    DISPATCH; // Jump to the function's starting instruction

  lbINVOKEX:
    CallExternal(Pointer(Data^.Args[0].Data.Addr), Data^.Args[1].Data.Arg, Data^.Args[2].Data.Arg <> 0);
    DISPATCH;

  lbRET:
    begin
      if CallStack.Top > -1 then
      begin
        // return value
        if Data^.nArgs = 2 then
          case Data^.Args[0].Pos of
            mpLocal: Move(Stack.Local(Data^.Args[0].Data.Addr)^, ArgStack.Pop()^, Data^.Args[1].Data.Addr);
            mpImm:   Move(Data^.Args[0].Data.Arg, ArgStack.Pop()^, Data^.Args[1].Data.Addr);
          end;

        frame := CallStack.Pop;
        Stack.StackPtr := Frame.StackPtr;
        Dec(RecursionDepth);
        Data := Pointer(frame.ReturnAddress);
      end else
        Exit;

      DISPATCH
    end;

  lbPRTi:
    case Data^.Args[0].Pos of
      mpImm:   PrintInt(@Data^.Args[0].Data.Arg, 8); // Immediate value as PtrInt (8 bytes for UInt64)
      mpLocal: PrintInt(Stack.Local(Data^.Args[0].Data.Addr), XprTypeSize[Data^.Args[0].BaseType]);
    end;
    DISPATCH;

  lbPRTf:
    case Data^.Args[0].Pos of
      mpLocal: PrintReal(Stack.Local(Data^.Args[0].Data.Addr), XprTypeSize[Data^.Args[0].BaseType]);
    end;
    DISPATCH;

  lbPRTb:
    case Data^.Args[0].Pos of
      mpLocal: WriteLn(Boolean(Stack.Local(Data^.Args[0].Data.Addr)^));
    end;
    DISPATCH;

  lbERROR:
    (* nothing *);
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

(*
  No base types should be handled here, this is assignment between equal datasizes
  Again: left and right must be same size, or left larger than right,
  third argument is the datasize.
*)
procedure TInterpreter.HandleASGN(Instr: TBytecodeInstruction);
begin
  case Instr.Args[1].Pos of
    mpImm:
      Move(
        Pointer(Instr.Args[1].Data.Arg)^,
        Pointer(Stack.Local(Instr.Args[0].Data.Arg))^,
        Instr.Args[2].Data.i32
      );

    mpLocal:
      Move(
        Pointer(Stack.Local(Instr.Args[1].Data.Arg))^,
        Pointer(Stack.Local(Instr.Args[0].Data.Arg))^,
        Instr.Args[2].Data.i32
      );
  end;
end;

procedure TInterpreter.CallExternal(FuncPtr: Pointer; ArgCount: UInt16; hasReturn: Boolean);
begin
  if ArgCount > 0 then
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
