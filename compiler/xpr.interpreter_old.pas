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
  SysUtils, xpr.Types, xpr.Bytecode, xpr.BytecodeEmitter, xpr.Errors;

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

    procedure CallExternal(FuncPtr: Pointer; ArgCount: UInt16; hasReturn: Boolean);
    procedure HandleASGN(Instr: TBytecodeInstruction);
    procedure ArrayRefcount(Left, Right: Pointer);
  end;


implementation

uses
  Math;


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

        bcJMP: pc := Args[0].Data.i32;

        bcRELJMP: pc := pc + Args[0].Data.i32;

        bcJZ:  if not PBoolean(Local(Args[0].Data.Arg))^ then pc := pc + Args[1].Data.i32;
        bcJNZ: if PByte(Stack.Local(Args[0].Data.Arg))^ <> 0   then pc := pc + Args[1].Data.i32;

        bcJZ_i:  if Args[0].Data.Arg = 0  then pc := pc + Args[1].Data.i32;
        bcJNZ_i: if Args[0].Data.Arg <> 0 then pc := pc + Args[1].Data.i32;

        bcFILL:
          FillByte(Local(Args[0].Data.Addr)^, Args[1].Data.Addr, Args[2].Data.u8);

        // try except
        bcIncTry:
          TryStack.Push(args[0].Data.Addr, Stack.StackPtr);

        bcDecTry:
          TryStack.Pop();

        // array managment;
        bcREFCNT:
          ArrayRefcount(Pointer(Local(Args[0].Data.Addr)^), Pointer(Local(Args[1].Data.Addr)^));

        bcREFCNT_imm:
          ArrayRefcount(Pointer(Local(Args[0].Data.Addr)^), Pointer(Args[1].Data.Addr));

        {$I interpreter.binary_code.inc}
        {$I interpreter.asgn_code.inc}


        bcFMA_i8:  PPtrInt(Local(Args[3].Data.Addr))^ := PPtrInt(Local(Args[2].Data.Addr))^ + PInt8(Local(Args[0].Data.Addr))^ * Args[1].Data.Addr;
        bcFMA_u8:  PPtrInt(Local(Args[3].Data.Addr))^ := PPtrInt(Local(Args[2].Data.Addr))^ + PUInt8(Local(Args[0].Data.Addr))^ * Args[1].Data.Addr;
        bcFMA_i16: PPtrInt(Local(Args[3].Data.Addr))^ := PPtrInt(Local(Args[2].Data.Addr))^ + PInt16(Local(Args[0].Data.Addr))^ * Args[1].Data.Addr;
        bcFMA_u16: PPtrInt(Local(Args[3].Data.Addr))^ := PPtrInt(Local(Args[2].Data.Addr))^ + PUInt16(Local(Args[0].Data.Addr))^ * Args[1].Data.Addr;
        bcFMA_i32: PPtrInt(Local(Args[3].Data.Addr))^ := PPtrInt(Local(Args[2].Data.Addr))^ + PInt32(Local(Args[0].Data.Addr))^ * Args[1].Data.Addr;
        bcFMA_u32: PPtrInt(Local(Args[3].Data.Addr))^ := PPtrInt(Local(Args[2].Data.Addr))^ + PUInt32(Local(Args[0].Data.Addr))^ * Args[1].Data.Addr;
        bcFMA_i64: PPtrInt(Local(Args[3].Data.Addr))^ := PPtrInt(Local(Args[2].Data.Addr))^ + PInt64(Local(Args[0].Data.Addr))^ * Args[1].Data.Addr;
        bcFMA_u64: PPtrInt(Local(Args[3].Data.Addr))^ := PPtrInt(Local(Args[2].Data.Addr))^ + PUInt64(Local(Args[0].Data.Addr))^ * Args[1].Data.Addr;

        bcFMA_imm_i8:  PPtrInt(Local(Args[3].Data.Addr))^ := PPtrInt(Local(Args[2].Data.Addr))^ + Int8(Args[0].Data.Addr) * Args[1].Data.Addr;
        bcFMA_imm_u8:  PPtrInt(Local(Args[3].Data.Addr))^ := PPtrInt(Local(Args[2].Data.Addr))^ + UInt8(Args[0].Data.Addr) * Args[1].Data.Addr;
        bcFMA_imm_i16: PPtrInt(Local(Args[3].Data.Addr))^ := PPtrInt(Local(Args[2].Data.Addr))^ + Int16(Args[0].Data.Addr) * Args[1].Data.Addr;
        bcFMA_imm_u16: PPtrInt(Local(Args[3].Data.Addr))^ := PPtrInt(Local(Args[2].Data.Addr))^ + UInt16(Args[0].Data.Addr) * Args[1].Data.Addr;
        bcFMA_imm_i32: PPtrInt(Local(Args[3].Data.Addr))^ := PPtrInt(Local(Args[2].Data.Addr))^ + Int32(Args[0].Data.Addr) * Args[1].Data.Addr;
        bcFMA_imm_u32: PPtrInt(Local(Args[3].Data.Addr))^ := PPtrInt(Local(Args[2].Data.Addr))^ + UInt32(Args[0].Data.Addr) * Args[1].Data.Addr;
        bcFMA_imm_i64: PPtrInt(Local(Args[3].Data.Addr))^ := PPtrInt(Local(Args[2].Data.Addr))^ + Int64(Args[0].Data.Addr) * Args[1].Data.Addr;
        bcFMA_imm_u64: PPtrInt(Local(Args[3].Data.Addr))^ := PPtrInt(Local(Args[2].Data.Addr))^ + UInt64(Args[0].Data.Addr) * Args[1].Data.Addr;


        bcDREF: Move(Pointer(Local(Args[1].Data.Arg)^)^, Stack.Local(Args[0].Data.Arg)^, Args[2].Data.Arg);
        bcDREF_32: PUInt32(Local(Args[0].Data.Arg))^ := PUInt32(Local(Args[1].Data.Arg)^)^;
        bcDREF_64: PUInt64(Local(Args[0].Data.Arg))^ := PUInt64(Local(Args[1].Data.Arg)^)^;

        bcFMAD_d64_64:
          PInt64(Local(Args[3].Data.Addr))^ := PInt64(PPtrInt(Local(Args[2].Data.Addr))^ + PInt64(Local(Args[0].Data.Addr))^ * Args[1].Data.Addr)^;

        bcFMAD_d64_32:
          PInt64(Local(Args[3].Data.Addr))^ := PInt64(PPtrInt(Local(Args[2].Data.Addr))^ + PInt32(Local(Args[0].Data.Addr))^ * Args[1].Data.Addr)^;

        bcFMAD_d32_64:
          PInt32(Local(Args[3].Data.Addr))^ := PInt64(PPtrInt(Local(Args[2].Data.Addr))^ + PInt64(Local(Args[0].Data.Addr))^ * Args[1].Data.Addr)^;

        bcFMAD_d32_32:
          PInt32(Local(Args[3].Data.Addr))^ := PInt64(PPtrInt(Local(Args[2].Data.Addr))^ + PInt32(Local(Args[0].Data.Addr))^ * Args[1].Data.Addr)^;


        // should be renamed to reflect that it's purpose of upcasting (it's main usage), UPASGN? MOVUPC?
        bcMOV:
          HandleASGN(BC.Code.Data[pc]);




        // push the address of the variable  / value (a reference)
        //
        bcPUSH:
          if Args[0].Pos = mpLocal then
            ArgStack.Push(Local(Args[0].Data.Addr))
          else
            ArgStack.Push(@Args[0].Data.Arg);

        bcPUSHREF:
          ArgStack.Push(Pointer(Local(Args[0].Data.Addr)^));

        // pop [and derefence] - write pop to stack
        // function arguments are references, write the value (a copy)
        bcPOP:
          Move(ArgStack.Pop()^, Stack.Local(Args[1].Data.Addr)^, Args[0].Data.Addr);

        // pop [and derefence] - write ptr to pop
        // if argstack contains a pointer we can write a local value to
        bcRPOP:
          Move(Local(Args[0].Data.Addr)^, ArgStack.Pop()^,  Args[1].Data.Addr);

        // pop [as refence] - write pop to stack
        // function arguments are references, write the address to the var
        bcPOPH:
          Pointer(Local(Args[0].Data.Addr)^) := ArgStack.Pop();

        // using a global in local scope, assign it's reference
        bcLOAD_GLOBAL:
          Pointer(Stack.Local(Args[0].Data.Addr)^) := Global(Args[1].Data.Addr);

        bcCOPY_GLOBAL:
          Pointer(Stack.Local(Args[0].Data.Addr)^) := Pointer(Global(Args[1].Data.Addr)^);

        bcNEWFRAME:
          begin            {stackptr contains = pc}
            CallStack.Push(PPtrInt(StackPtr)^, Stack.StackPtr);
            PPtrInt(StackPtr)^ := 0;
            Stack.StackPtr += Args[0].Data.Addr; //inc by frame
          end;

        bcINVOKE:
          begin
            // Check recursion depth
            if RecursionDepth >= MAX_RECURSION_DEPTH then
              raise RuntimeError.Create('Recursion depth limit exceeded');

            Inc(RecursionDepth);
            PPtrInt(StackPtr)^ := pc;
            pc := PtrInt(Global(Args[0].Data.Addr)^);
          end;

        bcINVOKEX:
          CallExternal(Pointer(Args[0].Data.Addr), Args[1].Data.Arg, Args[2].Data.Arg <> 0);

        bcRET:
          begin
            if CallStack.Top > -1 then
            begin
              // return value
              if nArgs = 2 then
                case Args[0].Pos of
                  mpLocal: Move(Local(Args[0].Data.Addr)^, ArgStack.Pop()^, Args[1].Data.Addr);
                  mpImm:   Move(Args[0].Data.Arg, ArgStack.Pop()^, Args[1].Data.Addr);
                end;

              frame := CallStack.Pop;
              Stack.StackPtr := Frame.StackPtr;
              Dec(RecursionDepth);
              pc := frame.ReturnAddress;
            end else
              Break;
          end;

        bcPRTi:
          case Args[0].Pos of
            mpImm:    PrintInt(@Args[0].Data.Arg, 8);
            mpLocal:  PrintInt(Local(Args[0].Data.Arg), XprTypeSize[Args[0].BaseType]);
            //mpGlobal: PrintInt(Global(Args[0].Arg), XprTypeSize[Args[0].Typ]);
          end;
        bcPRTf:
          case Args[0].Pos of
            mpLocal: PrintReal(Local(Args[0].Data.Arg), XprTypeSize[Args[0].BaseType]);
            //mpGlobal:PrintReal(Global(Args[0].Arg), XprTypeSize[Args[0].Typ]);
          end;
        bcPRTb:
          case Args[0].Pos of
            mpLocal:  WriteLn(Boolean(Local(Args[0].Data.Arg)^));
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
