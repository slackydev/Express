unit xprInterpreter;
{
  Author: Jarl K. Holta  
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
{$I header.inc}
{$hints off}
{$R-}
interface

uses
  SysUtils, xprTypes, xprBytecode, xprBytecodeEmitter, xprErrors;

const
  STACK_SIZE = 16 * 1024 * 1024;          // 16MB static stack
  MAX_RECURSION_DEPTH = 1000;    // Recursion depth limit
  STACK_FRAME_SIZE = 64 * 1024;  // 64KB per stack frame (adjust as needed)
	
type
  TByteArray = array of Byte;
  PByteArray = ^TByteArray;

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

    procedure CallExternal(FuncPtr: Pointer; ArgCount: UInt16);
    procedure HandleASGN(Instr: TBytecodeInstruction);
  end;


implementation

uses
  Math;


{$I xprInc_InstrFunctions.inc}


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
begin
  Stack.Init(Emitter.Stack, Emitter.UsedStackSize); //stackptr = after global allocations
  CallStack.Init();
  RecursionDepth := 0;
  ProgramStart   := StartPos;
  ProgramCounter := ProgramStart;
  ArgStack.Count := 0;
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
  repeat
    try
      Self.Run(BC);
    except
      on E: Exception do
        WriteLn(E.ToString);
    end;

    TryFrame := TryStack.Pop();
    Stack.StackPtr := TryFrame.StackPtr;
    ProgramCounter := TryFrame.ReturnAddress;

  until TryStack.Top < 0;
end;

procedure TInterpreter.Run(var BC: TBytecode);
var
  pc: Int32;
  frame: TCallFrame;
  e: String;
  left, right: Pointer;

  Data: array of TBytecodeInstruction;
begin
  pc := ProgramCounter;
  Data := BC.Code.Data;

  while True do
  begin
    with Data[pc], Stack do
    begin
      //WriteLn('*** ', pc, ' && ', Code);
      case Code of
        bcNOOP: (* nothing *);

        bcJMP: pc := Args[0].i32;

        bcRELJMP: pc := pc + Args[0].i32;

        bcJZ:  if not PBoolean(Local(Args[0].Arg))^ then pc := pc + Args[1].i32;
        bcJNZ: if PByte(Stack.Local(Args[0].Arg))^ <> 0   then pc := pc + Args[1].i32;

        bcJZ_i:  if Args[0].Arg = 0  then pc := pc + Args[1].i32;
        bcJNZ_i: if Args[0].Arg <> 0 then pc := pc + Args[1].i32;

        // try except
        bcIncTry:
          TryStack.Push(args[0].Addr, Stack.StackPtr);

        bcDecTry:
          TryStack.Pop();


        {$I interpreter.binary_code.inc}
        {$I interpreter.asgn_code.inc}


        bcFMA_i8:  PPtrInt(Local(Args[3].Addr))^ := PPtrInt(Local(Args[2].Addr))^ + PInt8(Local(Args[0].Addr))^ * Args[1].Addr;
        bcFMA_u8:  PPtrInt(Local(Args[3].Addr))^ := PPtrInt(Local(Args[2].Addr))^ + PUInt8(Local(Args[0].Addr))^ * Args[1].Addr;
        bcFMA_i16: PPtrInt(Local(Args[3].Addr))^ := PPtrInt(Local(Args[2].Addr))^ + PInt16(Local(Args[0].Addr))^ * Args[1].Addr;
        bcFMA_u16: PPtrInt(Local(Args[3].Addr))^ := PPtrInt(Local(Args[2].Addr))^ + PUInt16(Local(Args[0].Addr))^ * Args[1].Addr;
        bcFMA_i32: PPtrInt(Local(Args[3].Addr))^ := PPtrInt(Local(Args[2].Addr))^ + PInt32(Local(Args[0].Addr))^ * Args[1].Addr;
        bcFMA_u32: PPtrInt(Local(Args[3].Addr))^ := PPtrInt(Local(Args[2].Addr))^ + PUInt32(Local(Args[0].Addr))^ * Args[1].Addr;
        bcFMA_i64: PPtrInt(Local(Args[3].Addr))^ := PPtrInt(Local(Args[2].Addr))^ + PInt64(Local(Args[0].Addr))^ * Args[1].Addr;
        bcFMA_u64: PPtrInt(Local(Args[3].Addr))^ := PPtrInt(Local(Args[2].Addr))^ + PUInt64(Local(Args[0].Addr))^ * Args[1].Addr;

        bcDREF: Move(Pointer(Local(Args[1].Arg)^)^, Stack.Local(Args[0].Arg)^, Args[2].Arg);
        bcDREF_32: PUInt32(Local(Args[0].Arg))^ := PUInt32(Local(Args[1].Arg)^)^;
        bcDREF_64: PUInt64(Local(Args[0].Arg))^ := PUInt64(Local(Args[1].Arg)^)^;


        // should be renamed to reflect that it's purpose of upcasting (it's main usage), UPASGN? MOVUPC?
        bcMOV:
          HandleASGN(BC.Code.Data[pc]);




        // push the address of the variable  / value (a reference)
        //
        bcPUSH:
          if Args[0].Pos = mpLocal then
            ArgStack.Push(Local(Args[0].Addr))
          else
            ArgStack.Push(@Args[0].Arg);

        // pop [and derefence] - write pop to stack
        // function arguments are references, write the value (a copy)
        bcPOP:
          Move(ArgStack.Pop()^, Stack.Local(Args[1].Addr)^, Args[0].Addr);

        // pop [and derefence] - write ptr to pop
        // if argstack contains a pointer we can write a local value to
        bcRPOP:
          Move(Local(Args[0].Addr)^, ArgStack.Pop()^,  Args[1].Addr);

        // pop [as refence] - write pop to stack
        // function arguments are references, write the address to the var
        bcPOPH:
          Pointer(Local(Args[0].Addr)^) := ArgStack.Pop();

        // using a global in local scope, assign it's reference
        bcLOAD_GLOBAL:
          Move(Global(Args[1].Addr), Stack.Local(Args[0].Addr)^, SizeOf(Pointer));



        bcNEWFRAME:
          begin            {stackptr contains = pc}
            CallStack.Push(PPtrInt(StackPtr)^, Stack.StackPtr);
            PPtrInt(StackPtr)^ := 0;
            Stack.StackPtr += Args[0].Addr; //inc by frame
          end;

        bcINVOKE:
          begin
            // Check recursion depth
            if RecursionDepth >= MAX_RECURSION_DEPTH then
              raise RuntimeError.Create('Recursion depth limit exceeded');

            Inc(RecursionDepth);
            PPtrInt(StackPtr)^ := pc;
            pc := PtrInt(Global(Args[0].Addr)^);
          end;

        bcINVOKEX:
          CallExternal(Pointer(Args[0].Addr), Args[1].Arg);

        bcRET:
          begin
            if CallStack.Top > -1 then
            begin
              if nArgs = 2 then
                Move(Local(Args[0].Addr)^, ArgStack.Pop()^,  Args[1].Addr);

              frame := CallStack.Pop;
              Stack.StackPtr := Frame.StackPtr;
              Dec(RecursionDepth);
              pc := frame.ReturnAddress;
            end else
              Break;
          end;

        bcPRTi:
          case Args[0].Pos of
            mpImm:    PrintInt(@Args[0].Arg, 8);
            mpLocal:  PrintInt(Local(Args[0].Arg), XprTypeSize[Args[0].Typ]);
            //mpGlobal: PrintInt(Global(Args[0].Arg), XprTypeSize[Args[0].Typ]);
          end;
        bcPRTf:
          case Args[0].Pos of
            mpLocal: PrintReal(Local(Args[0].Arg), XprTypeSize[Args[0].Typ]);
            //mpGlobal:PrintReal(Global(Args[0].Arg), XprTypeSize[Args[0].Typ]);
          end;
        bcPRTb:
          case Args[0].Pos of
            mpLocal:  WriteLn(Boolean(Local(Args[0].Arg)^));
            //mpGlobal: WriteLn(Boolean(Global(Args[0].Arg)^));
          end;


        else
          begin
            WriteStr(e, code);
            raise RuntimeError.Create('Not implemented @ '+ IntToStr(pc-1) + ' - ' + e);
          end;
      end;
    end;

    Inc(pc);
    Self.ProgramCounter := PC;
  end;

  //WriteLn(AsString());
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
        Pointer(Instr.Args[1].Arg)^,
        Pointer(Stack.Local(Instr.Args[0].Arg))^,
        Instr.Args[2].i32
      );

    mpLocal:
      Move(
        Pointer(Stack.Local(Instr.Args[1].Arg))^,
        Pointer(Stack.Local(Instr.Args[0].Arg))^,
        Instr.Args[2].i32
      );
  end;
end;

procedure TInterpreter.CallExternal(FuncPtr: Pointer; ArgCount: UInt16);
var
  args: array of Pointer;
  i: Integer;
begin
  if ArgCount > 0 then
  begin
    SetLength(args, ArgCount);

    for i := 0 to ArgCount-1 do
      args[i] := ArgStack.Pop();

    TExternalProc(FuncPtr)(@args[0]);
  end
  else
    TExternalProc(FuncPtr)(nil);
end;

end.
