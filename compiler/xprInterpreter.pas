unit xprInterpreter;
{
  Author: Jarl K. Holta  
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
{$I header.inc}
{$hints off}

interface

uses
  SysUtils, xprTypes, xprBytecode, xprBytecodeEmitter, xprErrors;

const
  STACK_SIZE = 16 * 1024 * 1024; // 16MB static stack
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

  TStack = record
    Data: TByteArray;
    StackPtr: PByte;       // Pointer to current stack position
    BasePtr: PByte;        // Base pointer for stack
    procedure Init(Stack: TStackArray; StackPos: SizeInt);

    function GetTop(size: SizeInt): Pointer; inline;
    function Local(offset: SizeInt): Pointer; inline;
    function Global(offset: SizeInt): Pointer; inline;

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
    procedure Push(ReturnAddr: PtrUInt; StackPtr: PByte);
    function Pop: TCallFrame;
    function Peek: TCallFrame;
  end;

  TInterpreter = record
    Stack: TStack;
    ArgStack: TArgStack;
    CallStack: TCallStack;
    RecursionDepth: Int32;
    ProgramStart: PtrUInt;

    constructor New(Emitter: TBytecodeEmitter; StartPos: PtrUInt; Opt:EOptimizerFlags);
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
  Move(stack[0], data[0], Length(Stack));

  BasePtr  := @Data[0];
  StackPtr := @Data[0] + StackPos;
end;


function TStack.GetTop(size: SizeInt): Pointer;
begin
  Assert( (StackPtr - size) >= BasePtr, 'Stack underflow');
  Result := StackPtr - size;
end;

// stackptr is always ahead, so negative indexing
function TStack.Local(offset: SizeInt): Pointer;
begin
  Result := StackPtr - offset - SizeOf(Pointer);
end;

function TStack.Global(offset: SizeInt): Pointer;
begin
  Result := Pointer(Pointer(BasePtr + offset)^);
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
  ProgramStart := StartPos;
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
procedure TInterpreter.Run(var BC: TBytecode);
var
  pc, CodeSize: Int32;
  frame: TCallFrame;
  e: String;

  left_i64, right_i64: Int64;
  left_f64, right_f64: Double;

  Data: array of TBytecodeInstruction;
begin
  pc := ProgramStart;
  CodeSize := BC.Code.Size;
  Data := BC.Code.Data;

  while pc < CodeSize do
  begin
    with Data[pc] do
    begin
      //WriteLn('*** ', pc, ' && ', Code);
      case Code of
        bcPass: (* nothing *);

        bcJMP: pc := Args[0].i32;

        bcRELJMP: pc := pc + Args[0].i32;

        bcJZ:  if PByte(Stack.Local(Args[0].Arg))^ = 0 then pc := pc + Args[1].i32;
        bcJNZ: if PByte(Stack.Local(Args[0].Arg))^<> 0 then pc := pc + Args[1].i32;

        bcJZ_g:  if PByte(Stack.Global(Args[0].Arg))^ = 0 then pc := pc + Args[1].i32;
        bcJNZ_g: if PByte(Stack.Global(Args[0].Arg))^ <> 0 then pc := pc + Args[1].i32;

        bcJZ_i:  if Args[0].Arg = 0  then pc := pc + Args[1].i32;
        bcJNZ_i: if Args[0].Arg <> 0 then pc := pc + Args[1].i32;


        // push the address of the variable  / value (a reference)
        //
        bcPUSH:
        begin
          case Args[0].Pos of
            mpLocal: ArgStack.Push(Pointer(Stack.Local(Args[0].Addr)));
            mpGlobal:ArgStack.Push(Pointer(Stack.Global(Args[0].Addr)));
            mpImm:   ArgStack.Push(@Args[0].Arg);
          end;
        end;

        // pop [and derefence] - write pop to stack
        // function arguments are references, write the value (a copy)
        bcPOP:
          Move(ArgStack.Pop()^, Stack.Local(Args[1].Addr)^, Args[0].Addr);

        // pop - write pop to stack
        // function arguments are references, write the value (a copy)
        bcPOPPtr:
          Move(PtrInt(ArgStack.Pop()), Stack.Local(Args[0].Addr)^, SizeOf(Pointer));

        // pop [and derefence] - write ptr to pop
        // if argstack contains a pointer we can write a local value to
        bcRPOP:
          Move(Stack.Local(Args[0].Addr)^, ArgStack.Pop()^,  Args[1].Addr);

        // pop [as refence] - write pop to stack
        // function arguments are references, write the address to the var
        bcPOPH:
          Move(PtrInt(ArgStack.Pop()), Stack.Local(Args[0].Addr)^, SizeOf(Pointer));

        //bcFMA
        //bcFMA, bcDREF:
        //  RaiseException('Upspecialized opcode!');



        // should be renamed to reflect that it's purpose of upcasting (it's main usage), UPASGN? MOVUPC?
        bcMOV:
          HandleASGN(BC.Code.Data[pc]);

        {$I interpreter.fma_code.inc}
        {$I interpreter.dref_code.inc}
        {$I interpreter.asgn_code.inc}
        {$I interpreter.binary_code.inc}


        // --- Arithmetic Operations ---

        // ADD opcodes
        bcADD_lll_i8: PInt8(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ + PInt8(Stack.Local(Args[1].Addr))^;
        bcADD_lll_u8: PUInt8(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ + PUInt8(Stack.Local(Args[1].Addr))^;
        bcADD_lll_i16: PInt16(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ + PInt16(Stack.Local(Args[1].Addr))^;
        bcADD_lll_u16: PUInt16(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ + PUInt16(Stack.Local(Args[1].Addr))^;
        bcADD_lll_i32: PInt32(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ + PInt32(Stack.Local(Args[1].Addr))^;
        bcADD_lll_u32: PUInt32(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ + PUInt32(Stack.Local(Args[1].Addr))^;
        bcADD_lll_i64: PInt64(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ + PInt64(Stack.Local(Args[1].Addr))^;
        bcADD_lll_u64: PUInt64(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ + PUInt64(Stack.Local(Args[1].Addr))^;
        bcADD_lll_f32: PFloat32(Stack.Local(Args[2].Addr))^ := PFloat32(Stack.Local(Args[0].Addr))^ + PFloat32(Stack.Local(Args[1].Addr))^;
        bcADD_lll_f64: PFloat64(Stack.Local(Args[2].Addr))^ := PFloat64(Stack.Local(Args[0].Addr))^ + PFloat64(Stack.Local(Args[1].Addr))^;

        bcADD_lil_i8: PInt8(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ + Int8(Args[1].Arg);
        bcADD_lil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ + UInt8(Args[1].Arg);
        bcADD_lil_i16: PInt16(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ + Int16(Args[1].Arg);
        bcADD_lil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ + UInt16(Args[1].Arg);
        bcADD_lil_i32: PInt32(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ + Int32(Args[1].Arg);
        bcADD_lil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ + UInt32(Args[1].Arg);
        bcADD_lil_i64: PInt64(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ + Int64(Args[1].Arg);
        bcADD_lil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ + UInt64(Args[1].Arg);
        bcADD_lil_f32: PFloat32(Stack.Local(Args[2].Addr))^ := PFloat32(Stack.Local(Args[0].Addr))^ + Float32(Args[1].Arg);
        bcADD_lil_f64: PFloat64(Stack.Local(Args[2].Addr))^ := PFloat64(Stack.Local(Args[0].Addr))^ + Float64(Args[1].Arg);

        bcADD_ill_i8: PInt8(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) + PInt8(Stack.Local(Args[1].Addr))^;
        bcADD_ill_u8: PUInt8(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) + PUInt8(Stack.Local(Args[1].Addr))^;
        bcADD_ill_i16: PInt16(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) + PInt16(Stack.Local(Args[1].Addr))^;
        bcADD_ill_u16: PUInt16(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) + PUInt16(Stack.Local(Args[1].Addr))^;
        bcADD_ill_i32: PInt32(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) + PInt32(Stack.Local(Args[1].Addr))^;
        bcADD_ill_u32: PUInt32(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) + PUInt32(Stack.Local(Args[1].Addr))^;
        bcADD_ill_i64: PInt64(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) + PInt64(Stack.Local(Args[1].Addr))^;
        bcADD_ill_u64: PUInt64(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) + PUInt64(Stack.Local(Args[1].Addr))^;
        bcADD_ill_f32: PFloat32(Stack.Local(Args[2].Addr))^ := Float32(Args[0].Arg) + PFloat32(Stack.Local(Args[1].Addr))^;
        bcADD_ill_f64: PFloat64(Stack.Local(Args[2].Addr))^ := Float64(Args[0].Arg) + PFloat64(Stack.Local(Args[1].Addr))^;

        bcADD_iil_i8: PInt8(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) + Int8(Args[1].Arg);
        bcADD_iil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) + UInt8(Args[1].Arg);
        bcADD_iil_i16: PInt16(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) + Int16(Args[1].Arg);
        bcADD_iil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) + UInt16(Args[1].Arg);
        bcADD_iil_i32: PInt32(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) + Int32(Args[1].Arg);
        bcADD_iil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) + UInt32(Args[1].Arg);
        bcADD_iil_i64: PInt64(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) + Int64(Args[1].Arg);
        bcADD_iil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) + UInt64(Args[1].Arg);
        bcADD_iil_f32: PFloat32(Stack.Local(Args[2].Addr))^ := Float32(Args[0].Arg) + Float32(Args[1].Arg);
        bcADD_iil_f64: PFloat64(Stack.Local(Args[2].Addr))^ := Float64(Args[0].Arg) + Float64(Args[1].Arg);

        // SUB opcodes
        bcSUB_lll_i8: PInt8(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ - PInt8(Stack.Local(Args[1].Addr))^;
        bcSUB_lll_u8: PUInt8(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ - PUInt8(Stack.Local(Args[1].Addr))^;
        bcSUB_lll_i16: PInt16(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ - PInt16(Stack.Local(Args[1].Addr))^;
        bcSUB_lll_u16: PUInt16(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ - PUInt16(Stack.Local(Args[1].Addr))^;
        bcSUB_lll_i32: PInt32(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ - PInt32(Stack.Local(Args[1].Addr))^;
        bcSUB_lll_u32: PUInt32(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ - PUInt32(Stack.Local(Args[1].Addr))^;
        bcSUB_lll_i64: PInt64(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ - PInt64(Stack.Local(Args[1].Addr))^;
        bcSUB_lll_u64: PUInt64(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ - PUInt64(Stack.Local(Args[1].Addr))^;
        bcSUB_lll_f32: PFloat32(Stack.Local(Args[2].Addr))^ := PFloat32(Stack.Local(Args[0].Addr))^ - PFloat32(Stack.Local(Args[1].Addr))^;
        bcSUB_lll_f64: PFloat64(Stack.Local(Args[2].Addr))^ := PFloat64(Stack.Local(Args[0].Addr))^ - PFloat64(Stack.Local(Args[1].Addr))^;

        bcSUB_lil_i8: PInt8(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ - Int8(Args[1].Arg);
        bcSUB_lil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ - UInt8(Args[1].Arg);
        bcSUB_lil_i16: PInt16(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ - Int16(Args[1].Arg);
        bcSUB_lil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ - UInt16(Args[1].Arg);
        bcSUB_lil_i32: PInt32(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ - Int32(Args[1].Arg);
        bcSUB_lil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ - UInt32(Args[1].Arg);
        bcSUB_lil_i64: PInt64(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ - Int64(Args[1].Arg);
        bcSUB_lil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ - UInt64(Args[1].Arg);
        bcSUB_lil_f32: PFloat32(Stack.Local(Args[2].Addr))^ := PFloat32(Stack.Local(Args[0].Addr))^ - Float32(Args[1].Arg);
        bcSUB_lil_f64: PFloat64(Stack.Local(Args[2].Addr))^ := PFloat64(Stack.Local(Args[0].Addr))^ - Float64(Args[1].Arg);

        bcSUB_ill_i8: PInt8(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) - PInt8(Stack.Local(Args[1].Addr))^;
        bcSUB_ill_u8: PUInt8(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) - PUInt8(Stack.Local(Args[1].Addr))^;
        bcSUB_ill_i16: PInt16(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) - PInt16(Stack.Local(Args[1].Addr))^;
        bcSUB_ill_u16: PUInt16(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) - PUInt16(Stack.Local(Args[1].Addr))^;
        bcSUB_ill_i32: PInt32(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) - PInt32(Stack.Local(Args[1].Addr))^;
        bcSUB_ill_u32: PUInt32(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) - PUInt32(Stack.Local(Args[1].Addr))^;
        bcSUB_ill_i64: PInt64(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) - PInt64(Stack.Local(Args[1].Addr))^;
        bcSUB_ill_u64: PUInt64(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) - PUInt64(Stack.Local(Args[1].Addr))^;
        bcSUB_ill_f32: PFloat32(Stack.Local(Args[2].Addr))^ := Float32(Args[0].Arg) - PFloat32(Stack.Local(Args[1].Addr))^;
        bcSUB_ill_f64: PFloat64(Stack.Local(Args[2].Addr))^ := Float64(Args[0].Arg) - PFloat64(Stack.Local(Args[1].Addr))^;

        bcSUB_iil_i8: PInt8(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) - Int8(Args[1].Arg);
        bcSUB_iil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) - UInt8(Args[1].Arg);
        bcSUB_iil_i16: PInt16(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) - Int16(Args[1].Arg);
        bcSUB_iil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) - UInt16(Args[1].Arg);
        bcSUB_iil_i32: PInt32(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) - Int32(Args[1].Arg);
        bcSUB_iil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) - UInt32(Args[1].Arg);
        bcSUB_iil_i64: PInt64(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) - Int64(Args[1].Arg);
        bcSUB_iil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) - UInt64(Args[1].Arg);
        bcSUB_iil_f32: PFloat32(Stack.Local(Args[2].Addr))^ := Float32(Args[0].Arg) - Float32(Args[1].Arg);
        bcSUB_iil_f64: PFloat64(Stack.Local(Args[2].Addr))^ := Float64(Args[0].Arg) - Float64(Args[1].Arg);

        // MUL opcodes
        bcMUL_lll_i8: PInt8(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ * PInt8(Stack.Local(Args[1].Addr))^;
        bcMUL_lll_u8: PUInt8(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ * PUInt8(Stack.Local(Args[1].Addr))^;
        bcMUL_lll_i16: PInt16(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ * PInt16(Stack.Local(Args[1].Addr))^;
        bcMUL_lll_u16: PUInt16(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ * PUInt16(Stack.Local(Args[1].Addr))^;
        bcMUL_lll_i32: PInt32(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ * PInt32(Stack.Local(Args[1].Addr))^;
        bcMUL_lll_u32: PUInt32(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ * PUInt32(Stack.Local(Args[1].Addr))^;
        bcMUL_lll_i64: PInt64(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ * PInt64(Stack.Local(Args[1].Addr))^;
        bcMUL_lll_u64: PUInt64(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ * PUInt64(Stack.Local(Args[1].Addr))^;
        bcMUL_lll_f32: PFloat32(Stack.Local(Args[2].Addr))^ := PFloat32(Stack.Local(Args[0].Addr))^ * PFloat32(Stack.Local(Args[1].Addr))^;
        bcMUL_lll_f64: PFloat64(Stack.Local(Args[2].Addr))^ := PFloat64(Stack.Local(Args[0].Addr))^ * PFloat64(Stack.Local(Args[1].Addr))^;

        bcMUL_lil_i8: PInt8(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ * Int8(Args[1].Arg);
        bcMUL_lil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ * UInt8(Args[1].Arg);
        bcMUL_lil_i16: PInt16(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ * Int16(Args[1].Arg);
        bcMUL_lil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ * UInt16(Args[1].Arg);
        bcMUL_lil_i32: PInt32(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ * Int32(Args[1].Arg);
        bcMUL_lil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ * UInt32(Args[1].Arg);
        bcMUL_lil_i64: PInt64(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ * Int64(Args[1].Arg);
        bcMUL_lil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ * UInt64(Args[1].Arg);
        bcMUL_lil_f32: PFloat32(Stack.Local(Args[2].Addr))^ := PFloat32(Stack.Local(Args[0].Addr))^ * Float32(Args[1].Arg);
        bcMUL_lil_f64: PFloat64(Stack.Local(Args[2].Addr))^ := PFloat64(Stack.Local(Args[0].Addr))^ * Float64(Args[1].Arg);

        bcMUL_ill_i8: PInt8(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) * PInt8(Stack.Local(Args[1].Addr))^;
        bcMUL_ill_u8: PUInt8(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) * PUInt8(Stack.Local(Args[1].Addr))^;
        bcMUL_ill_i16: PInt16(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) * PInt16(Stack.Local(Args[1].Addr))^;
        bcMUL_ill_u16: PUInt16(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) * PUInt16(Stack.Local(Args[1].Addr))^;
        bcMUL_ill_i32: PInt32(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) * PInt32(Stack.Local(Args[1].Addr))^;
        bcMUL_ill_u32: PUInt32(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) * PUInt32(Stack.Local(Args[1].Addr))^;
        bcMUL_ill_i64: PInt64(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) * PInt64(Stack.Local(Args[1].Addr))^;
        bcMUL_ill_u64: PUInt64(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) * PUInt64(Stack.Local(Args[1].Addr))^;
        bcMUL_ill_f32: PFloat32(Stack.Local(Args[2].Addr))^ := Float32(Args[0].Arg) * PFloat32(Stack.Local(Args[1].Addr))^;
        bcMUL_ill_f64: PFloat64(Stack.Local(Args[2].Addr))^ := Float64(Args[0].Arg) * PFloat64(Stack.Local(Args[1].Addr))^;

        bcMUL_iil_i8: PInt8(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) * Int8(Args[1].Arg);
        bcMUL_iil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) * UInt8(Args[1].Arg);
        bcMUL_iil_i16: PInt16(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) * Int16(Args[1].Arg);
        bcMUL_iil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) * UInt16(Args[1].Arg);
        bcMUL_iil_i32: PInt32(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) * Int32(Args[1].Arg);
        bcMUL_iil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) * UInt32(Args[1].Arg);
        bcMUL_iil_i64: PInt64(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) * Int64(Args[1].Arg);
        bcMUL_iil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) * UInt64(Args[1].Arg);
        bcMUL_iil_f32: PFloat32(Stack.Local(Args[2].Addr))^ := Float32(Args[0].Arg) * Float32(Args[1].Arg);
        bcMUL_iil_f64: PFloat64(Stack.Local(Args[2].Addr))^ := Float64(Args[0].Arg) * Float64(Args[1].Arg);

        // DIV opcodes
        bcDIV_lll_i8: PInt8(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ div PInt8(Stack.Local(Args[1].Addr))^;
        bcDIV_lll_u8: PUInt8(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ div PUInt8(Stack.Local(Args[1].Addr))^;
        bcDIV_lll_i16: PInt16(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ div PInt16(Stack.Local(Args[1].Addr))^;
        bcDIV_lll_u16: PUInt16(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ div PUInt16(Stack.Local(Args[1].Addr))^;
        bcDIV_lll_i32: PInt32(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ div PInt32(Stack.Local(Args[1].Addr))^;
        bcDIV_lll_u32: PUInt32(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ div PUInt32(Stack.Local(Args[1].Addr))^;
        bcDIV_lll_i64: PInt64(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ div PInt64(Stack.Local(Args[1].Addr))^;
        bcDIV_lll_u64: PUInt64(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ div PUInt64(Stack.Local(Args[1].Addr))^;
        bcDIV_lll_f32: PFloat32(Stack.Local(Args[2].Addr))^ := PFloat32(Stack.Local(Args[0].Addr))^ / PFloat32(Stack.Local(Args[1].Addr))^;
        bcDIV_lll_f64: PFloat64(Stack.Local(Args[2].Addr))^ := PFloat64(Stack.Local(Args[0].Addr))^ / PFloat64(Stack.Local(Args[1].Addr))^;

        bcDIV_lil_i8: PInt8(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ div Int8(Args[1].Arg);
        bcDIV_lil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ div UInt8(Args[1].Arg);
        bcDIV_lil_i16: PInt16(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ div Int16(Args[1].Arg);
        bcDIV_lil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ div UInt16(Args[1].Arg);
        bcDIV_lil_i32: PInt32(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ div Int32(Args[1].Arg);
        bcDIV_lil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ div UInt32(Args[1].Arg);
        bcDIV_lil_i64: PInt64(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ div Int64(Args[1].Arg);
        bcDIV_lil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ div UInt64(Args[1].Arg);
        bcDIV_lil_f32: PFloat32(Stack.Local(Args[2].Addr))^ := PFloat32(Stack.Local(Args[0].Addr))^ / Float32(Args[1].Arg);
        bcDIV_lil_f64: PFloat64(Stack.Local(Args[2].Addr))^ := PFloat64(Stack.Local(Args[0].Addr))^ / Float64(Args[1].Arg);

        bcDIV_ill_i8: PInt8(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) div PInt8(Stack.Local(Args[1].Addr))^;
        bcDIV_ill_u8: PUInt8(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) div PUInt8(Stack.Local(Args[1].Addr))^;
        bcDIV_ill_i16: PInt16(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) div PInt16(Stack.Local(Args[1].Addr))^;
        bcDIV_ill_u16: PUInt16(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) div PUInt16(Stack.Local(Args[1].Addr))^;
        bcDIV_ill_i32: PInt32(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) div PInt32(Stack.Local(Args[1].Addr))^;
        bcDIV_ill_u32: PUInt32(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) div PUInt32(Stack.Local(Args[1].Addr))^;
        bcDIV_ill_i64: PInt64(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) div PInt64(Stack.Local(Args[1].Addr))^;
        bcDIV_ill_u64: PUInt64(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) div PUInt64(Stack.Local(Args[1].Addr))^;
        bcDIV_ill_f32: PFloat32(Stack.Local(Args[2].Addr))^ := Float32(Args[0].Arg) / PFloat32(Stack.Local(Args[1].Addr))^;
        bcDIV_ill_f64: PFloat64(Stack.Local(Args[2].Addr))^ := Float64(Args[0].Arg) / PFloat64(Stack.Local(Args[1].Addr))^;

        bcDIV_iil_i8: PInt8(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) div Int8(Args[1].Arg);
        bcDIV_iil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) div UInt8(Args[1].Arg);
        bcDIV_iil_i16: PInt16(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) div Int16(Args[1].Arg);
        bcDIV_iil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) div UInt16(Args[1].Arg);
        bcDIV_iil_i32: PInt32(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) div Int32(Args[1].Arg);
        bcDIV_iil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) div UInt32(Args[1].Arg);
        bcDIV_iil_i64: PInt64(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) div Int64(Args[1].Arg);
        bcDIV_iil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) div UInt64(Args[1].Arg);
        bcDIV_iil_f32: PFloat32(Stack.Local(Args[2].Addr))^ := Float32(Args[0].Arg) / Float32(Args[1].Arg);
        bcDIV_iil_f64: PFloat64(Stack.Local(Args[2].Addr))^ := Float64(Args[0].Arg) / Float64(Args[1].Arg);

        // --- MOD opcodes (Modulo) ---
        // lll (local, local, local)
        bcMOD_lll_i8: PInt8(Stack.Local(Args[2].Addr))^ := Modulo(PInt8(Stack.Local(Args[0].Addr))^, PInt8(Stack.Local(Args[1].Addr))^);
        bcMOD_lll_u8: PUInt8(Stack.Local(Args[2].Addr))^ := Modulo(PUInt8(Stack.Local(Args[0].Addr))^, PUInt8(Stack.Local(Args[1].Addr))^);
        bcMOD_lll_i16: PInt16(Stack.Local(Args[2].Addr))^ := Modulo(PInt16(Stack.Local(Args[0].Addr))^, PInt16(Stack.Local(Args[1].Addr))^);
        bcMOD_lll_u16: PUInt16(Stack.Local(Args[2].Addr))^ := Modulo(PUInt16(Stack.Local(Args[0].Addr))^, PUInt16(Stack.Local(Args[1].Addr))^);
        bcMOD_lll_i32: PInt32(Stack.Local(Args[2].Addr))^ := Modulo(PInt32(Stack.Local(Args[0].Addr))^, PInt32(Stack.Local(Args[1].Addr))^);
        bcMOD_lll_u32: PUInt32(Stack.Local(Args[2].Addr))^ := Modulo(PUInt32(Stack.Local(Args[0].Addr))^, PUInt32(Stack.Local(Args[1].Addr))^);
        bcMOD_lll_i64: PInt64(Stack.Local(Args[2].Addr))^ := Modulo(PInt64(Stack.Local(Args[0].Addr))^, PInt64(Stack.Local(Args[1].Addr))^);
        bcMOD_lll_u64: PUInt64(Stack.Local(Args[2].Addr))^ := Modulo(PUInt64(Stack.Local(Args[0].Addr))^, PUInt64(Stack.Local(Args[1].Addr))^);
        bcMOD_lll_f32: PFloat32(Stack.Local(Args[2].Addr))^ := Modulo(PFloat32(Stack.Local(Args[0].Addr))^, PFloat32(Stack.Local(Args[1].Addr))^);
        bcMOD_lll_f64: PFloat64(Stack.Local(Args[2].Addr))^ := Modulo(PFloat64(Stack.Local(Args[0].Addr))^, PFloat64(Stack.Local(Args[1].Addr))^);

        // lil (local, immediate, local)
        bcMOD_lil_i8: PInt8(Stack.Local(Args[2].Addr))^ := Modulo(PInt8(Stack.Local(Args[0].Addr))^, Int8(Args[1].Arg));
        bcMOD_lil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := Modulo(PUInt8(Stack.Local(Args[0].Addr))^, UInt8(Args[1].Arg));
        bcMOD_lil_i16: PInt16(Stack.Local(Args[2].Addr))^ := Modulo(PInt16(Stack.Local(Args[0].Addr))^, Int16(Args[1].Arg));
        bcMOD_lil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := Modulo(PUInt16(Stack.Local(Args[0].Addr))^, UInt16(Args[1].Arg));
        bcMOD_lil_i32: PInt32(Stack.Local(Args[2].Addr))^ := Modulo(PInt32(Stack.Local(Args[0].Addr))^, Int32(Args[1].Arg));
        bcMOD_lil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := Modulo(PUInt32(Stack.Local(Args[0].Addr))^, UInt32(Args[1].Arg));
        bcMOD_lil_i64: PInt64(Stack.Local(Args[2].Addr))^ := Modulo(PInt64(Stack.Local(Args[0].Addr))^, Int64(Args[1].Arg));
        bcMOD_lil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := Modulo(PUInt64(Stack.Local(Args[0].Addr))^, UInt64(Args[1].Arg));
        bcMOD_lil_f32: PFloat32(Stack.Local(Args[2].Addr))^ := Modulo(PFloat32(Stack.Local(Args[0].Addr))^, Float32(Args[1].Arg));
        bcMOD_lil_f64: PFloat64(Stack.Local(Args[2].Addr))^ := Modulo(PFloat64(Stack.Local(Args[0].Addr))^, Float64(Args[1].Arg));

        // ill (immediate, local, local)
        bcMOD_ill_i8: PInt8(Stack.Local(Args[2].Addr))^ := Modulo(Int8(Args[0].Arg), PInt8(Stack.Local(Args[1].Addr))^);
        bcMOD_ill_u8: PUInt8(Stack.Local(Args[2].Addr))^ := Modulo(UInt8(Args[0].Arg), PUInt8(Stack.Local(Args[1].Addr))^);
        bcMOD_ill_i16: PInt16(Stack.Local(Args[2].Addr))^ := Modulo(Int16(Args[0].Arg), PInt16(Stack.Local(Args[1].Addr))^);
        bcMOD_ill_u16: PUInt16(Stack.Local(Args[2].Addr))^ := Modulo(UInt16(Args[0].Arg), PUInt16(Stack.Local(Args[1].Addr))^);
        bcMOD_ill_i32: PInt32(Stack.Local(Args[2].Addr))^ := Modulo(Int32(Args[0].Arg), PInt32(Stack.Local(Args[1].Addr))^);
        bcMOD_ill_u32: PUInt32(Stack.Local(Args[2].Addr))^ := Modulo(UInt32(Args[0].Arg), PUInt32(Stack.Local(Args[1].Addr))^);
        bcMOD_ill_i64: PInt64(Stack.Local(Args[2].Addr))^ := Modulo(Int64(Args[0].Arg), PInt64(Stack.Local(Args[1].Addr))^);
        bcMOD_ill_u64: PUInt64(Stack.Local(Args[2].Addr))^ := Modulo(UInt64(Args[0].Arg), PUInt64(Stack.Local(Args[1].Addr))^);
        bcMOD_ill_f32: PFloat32(Stack.Local(Args[2].Addr))^ := Modulo(Float32(Args[0].Arg), PFloat32(Stack.Local(Args[1].Addr))^);
        bcMOD_ill_f64: PFloat64(Stack.Local(Args[2].Addr))^ := Modulo(Float64(Args[0].Arg), PFloat64(Stack.Local(Args[1].Addr))^);

        // iil (immediate, immediate, local)
        bcMOD_iil_i8: PInt8(Stack.Local(Args[2].Addr))^ := Modulo(Int8(Args[0].Arg), Int8(Args[1].Arg));
        bcMOD_iil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := Modulo(UInt8(Args[0].Arg), UInt8(Args[1].Arg));
        bcMOD_iil_i16: PInt16(Stack.Local(Args[2].Addr))^ := Modulo(Int16(Args[0].Arg), Int16(Args[1].Arg));
        bcMOD_iil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := Modulo(UInt16(Args[0].Arg), UInt16(Args[1].Arg));
        bcMOD_iil_i32: PInt32(Stack.Local(Args[2].Addr))^ := Modulo(Int32(Args[0].Arg), Int32(Args[1].Arg));
        bcMOD_iil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := Modulo(UInt32(Args[0].Arg), UInt32(Args[1].Arg));
        bcMOD_iil_i64: PInt64(Stack.Local(Args[2].Addr))^ := Modulo(Int64(Args[0].Arg), Int64(Args[1].Arg));
        bcMOD_iil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := Modulo(UInt64(Args[0].Arg), UInt64(Args[1].Arg));
        bcMOD_iil_f32: PFloat32(Stack.Local(Args[2].Addr))^ := Modulo(Float32(Args[0].Arg), Float32(Args[1].Arg));
        bcMOD_iil_f64: PFloat64(Stack.Local(Args[2].Addr))^ := Modulo(Float64(Args[0].Arg), Float64(Args[1].Arg));

        // POW opcodes (Assuming a Power function is available)
        bcPOW_lll_i8: PInt8(Stack.Local(Args[2].Addr))^ := Power(PInt8(Stack.Local(Args[0].Addr))^, PInt8(Stack.Local(Args[1].Addr))^);
        bcPOW_lll_u8: PUInt8(Stack.Local(Args[2].Addr))^ := Power(PUInt8(Stack.Local(Args[0].Addr))^, PUInt8(Stack.Local(Args[1].Addr))^);
        bcPOW_lll_i16: PInt16(Stack.Local(Args[2].Addr))^ := Power(PInt16(Stack.Local(Args[0].Addr))^, PInt16(Stack.Local(Args[1].Addr))^);
        bcPOW_lll_u16: PUInt16(Stack.Local(Args[2].Addr))^ := Power(PUInt16(Stack.Local(Args[0].Addr))^, PUInt16(Stack.Local(Args[1].Addr))^);
        bcPOW_lll_i32: PInt32(Stack.Local(Args[2].Addr))^ := Power(PInt32(Stack.Local(Args[0].Addr))^, PInt32(Stack.Local(Args[1].Addr))^);
        bcPOW_lll_u32: PUInt32(Stack.Local(Args[2].Addr))^ := Power(PUInt32(Stack.Local(Args[0].Addr))^, PUInt32(Stack.Local(Args[1].Addr))^);
        bcPOW_lll_i64: PInt64(Stack.Local(Args[2].Addr))^ := Power(PInt64(Stack.Local(Args[0].Addr))^, PInt64(Stack.Local(Args[1].Addr))^);
        bcPOW_lll_u64: PUInt64(Stack.Local(Args[2].Addr))^ := Power(PUInt64(Stack.Local(Args[0].Addr))^, PUInt64(Stack.Local(Args[1].Addr))^);
        bcPOW_lll_f32: PFloat32(Stack.Local(Args[2].Addr))^ := Power(PFloat32(Stack.Local(Args[0].Addr))^, PFloat32(Stack.Local(Args[1].Addr))^);
        bcPOW_lll_f64: PFloat64(Stack.Local(Args[2].Addr))^ := Power(PFloat64(Stack.Local(Args[0].Addr))^, PFloat64(Stack.Local(Args[1].Addr))^);

        bcPOW_lil_i8: PInt8(Stack.Local(Args[2].Addr))^ := Power(PInt8(Stack.Local(Args[0].Addr))^, Int8(Args[1].Arg));
        bcPOW_lil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := Power(PUInt8(Stack.Local(Args[0].Addr))^, UInt8(Args[1].Arg));
        bcPOW_lil_i16: PInt16(Stack.Local(Args[2].Addr))^ := Power(PInt16(Stack.Local(Args[0].Addr))^, Int16(Args[1].Arg));
        bcPOW_lil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := Power(PUInt16(Stack.Local(Args[0].Addr))^, UInt16(Args[1].Arg));
        bcPOW_lil_i32: PInt32(Stack.Local(Args[2].Addr))^ := Power(PInt32(Stack.Local(Args[0].Addr))^, Int32(Args[1].Arg));
        bcPOW_lil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := Power(PUInt32(Stack.Local(Args[0].Addr))^, UInt32(Args[1].Arg));
        bcPOW_lil_i64: PInt64(Stack.Local(Args[2].Addr))^ := Power(PInt64(Stack.Local(Args[0].Addr))^, Int64(Args[1].Arg));
        bcPOW_lil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := Power(PUInt64(Stack.Local(Args[0].Addr))^, UInt64(Args[1].Arg));
        bcPOW_lil_f32: PFloat32(Stack.Local(Args[2].Addr))^ := Power(PFloat32(Stack.Local(Args[0].Addr))^, Float32(Args[1].Arg));
        bcPOW_lil_f64: PFloat64(Stack.Local(Args[2].Addr))^ := Power(PFloat64(Stack.Local(Args[0].Addr))^, Float64(Args[1].Arg));

        bcPOW_ill_i8: PInt8(Stack.Local(Args[2].Addr))^ := Power(Int8(Args[0].Arg), PInt8(Stack.Local(Args[1].Addr))^);
        bcPOW_ill_u8: PUInt8(Stack.Local(Args[2].Addr))^ := Power(UInt8(Args[0].Arg), PUInt8(Stack.Local(Args[1].Addr))^);
        bcPOW_ill_i16: PInt16(Stack.Local(Args[2].Addr))^ := Power(Int16(Args[0].Arg), PInt16(Stack.Local(Args[1].Addr))^);
        bcPOW_ill_u16: PUInt16(Stack.Local(Args[2].Addr))^ := Power(UInt16(Args[0].Arg), PUInt16(Stack.Local(Args[1].Addr))^);
        bcPOW_ill_i32: PInt32(Stack.Local(Args[2].Addr))^ := Power(Int32(Args[0].Arg), PInt32(Stack.Local(Args[1].Addr))^);
        bcPOW_ill_u32: PUInt32(Stack.Local(Args[2].Addr))^ := Power(UInt32(Args[0].Arg), PUInt32(Stack.Local(Args[1].Addr))^);
        bcPOW_ill_i64: PInt64(Stack.Local(Args[2].Addr))^ := Power(Int64(Args[0].Arg), PInt64(Stack.Local(Args[1].Addr))^);
        bcPOW_ill_u64: PUInt64(Stack.Local(Args[2].Addr))^ := Power(UInt64(Args[0].Arg), PUInt64(Stack.Local(Args[1].Addr))^);
        bcPOW_ill_f32: PFloat32(Stack.Local(Args[2].Addr))^ := Power(Float32(Args[0].Arg), PFloat32(Stack.Local(Args[1].Addr))^);
        bcPOW_ill_f64: PFloat64(Stack.Local(Args[2].Addr))^ := Power(Float64(Args[0].Arg), PFloat64(Stack.Local(Args[1].Addr))^);

        bcPOW_iil_i8: PInt8(Stack.Local(Args[2].Addr))^ := Power(Int8(Args[0].Arg), Int8(Args[1].Arg));
        bcPOW_iil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := Power(UInt8(Args[0].Arg), UInt8(Args[1].Arg));
        bcPOW_iil_i16: PInt16(Stack.Local(Args[2].Addr))^ := Power(Int16(Args[0].Arg), Int16(Args[1].Arg));
        bcPOW_iil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := Power(UInt16(Args[0].Arg), UInt16(Args[1].Arg));
        bcPOW_iil_i32: PInt32(Stack.Local(Args[2].Addr))^ := Power(Int32(Args[0].Arg), Int32(Args[1].Arg));
        bcPOW_iil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := Power(UInt32(Args[0].Arg), UInt32(Args[1].Arg));
        bcPOW_iil_i64: PInt64(Stack.Local(Args[2].Addr))^ := Power(Int64(Args[0].Arg), Int64(Args[1].Arg));
        bcPOW_iil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := Power(UInt64(Args[0].Arg), UInt64(Args[1].Arg));
        bcPOW_iil_f32: PFloat32(Stack.Local(Args[2].Addr))^ := Power(Float32(Args[0].Arg), Float32(Args[1].Arg));
        bcPOW_iil_f64: PFloat64(Stack.Local(Args[2].Addr))^ := Power(Float64(Args[0].Arg), Float64(Args[1].Arg));

        // EQ opcodes
        bcEQ_lll_i8: PBoolean(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ = PInt8(Stack.Local(Args[1].Addr))^;
        bcEQ_lll_u8: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ = PUInt8(Stack.Local(Args[1].Addr))^;
        bcEQ_lll_i16: PBoolean(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ = PInt16(Stack.Local(Args[1].Addr))^;
        bcEQ_lll_u16: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ = PUInt16(Stack.Local(Args[1].Addr))^;
        bcEQ_lll_i32: PBoolean(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ = PInt32(Stack.Local(Args[1].Addr))^;
        bcEQ_lll_u32: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ = PUInt32(Stack.Local(Args[1].Addr))^;
        bcEQ_lll_i64: PBoolean(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ = PInt64(Stack.Local(Args[1].Addr))^;
        bcEQ_lll_u64: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ = PUInt64(Stack.Local(Args[1].Addr))^;
        bcEQ_lll_f32: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat32(Stack.Local(Args[0].Addr))^ = PFloat32(Stack.Local(Args[1].Addr))^;
        bcEQ_lll_f64: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat64(Stack.Local(Args[0].Addr))^ = PFloat64(Stack.Local(Args[1].Addr))^;

        bcEQ_lil_i8: PBoolean(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ = Int8(Args[1].Arg);
        bcEQ_lil_u8: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ = UInt8(Args[1].Arg);
        bcEQ_lil_i16: PBoolean(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ = Int16(Args[1].Arg);
        bcEQ_lil_u16: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ = UInt16(Args[1].Arg);
        bcEQ_lil_i32: PBoolean(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ = Int32(Args[1].Arg);
        bcEQ_lil_u32: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ = UInt32(Args[1].Arg);
        bcEQ_lil_i64: PBoolean(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ = Int64(Args[1].Arg);
        bcEQ_lil_u64: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ = UInt64(Args[1].Arg);
        bcEQ_lil_f32: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat32(Stack.Local(Args[0].Addr))^ = Float32(Args[1].Arg);
        bcEQ_lil_f64: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat64(Stack.Local(Args[0].Addr))^ = Float64(Args[1].Arg);

        bcEQ_ill_i8: PBoolean(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) = PInt8(Stack.Local(Args[1].Addr))^;
        bcEQ_ill_u8: PBoolean(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) = PUInt8(Stack.Local(Args[1].Addr))^;
        bcEQ_ill_i16: PBoolean(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) = PInt16(Stack.Local(Args[1].Addr))^;
        bcEQ_ill_u16: PBoolean(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) = PUInt16(Stack.Local(Args[1].Addr))^;
        bcEQ_ill_i32: PBoolean(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) = PInt32(Stack.Local(Args[1].Addr))^;
        bcEQ_ill_u32: PBoolean(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) = PUInt32(Stack.Local(Args[1].Addr))^;
        bcEQ_ill_i64: PBoolean(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) = PInt64(Stack.Local(Args[1].Addr))^;
        bcEQ_ill_u64: PBoolean(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) = PUInt64(Stack.Local(Args[1].Addr))^;
        bcEQ_ill_f32: PBoolean(Stack.Local(Args[2].Addr))^ := Float32(Args[0].Arg) = PFloat32(Stack.Local(Args[1].Addr))^;
        bcEQ_ill_f64: PBoolean(Stack.Local(Args[2].Addr))^ := Float64(Args[0].Arg) = PFloat64(Stack.Local(Args[1].Addr))^;

        bcEQ_iil_i8: PBoolean(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) = Int8(Args[1].Arg);
        bcEQ_iil_u8: PBoolean(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) = UInt8(Args[1].Arg);
        bcEQ_iil_i16: PBoolean(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) = Int16(Args[1].Arg);
        bcEQ_iil_u16: PBoolean(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) = UInt16(Args[1].Arg);
        bcEQ_iil_i32: PBoolean(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) = Int32(Args[1].Arg);
        bcEQ_iil_u32: PBoolean(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) = UInt32(Args[1].Arg);
        bcEQ_iil_i64: PBoolean(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) = Int64(Args[1].Arg);
        bcEQ_iil_u64: PBoolean(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) = UInt64(Args[1].Arg);
        bcEQ_iil_f32: PBoolean(Stack.Local(Args[2].Addr))^ := Float32(Args[0].Arg) = Float32(Args[1].Arg);
        bcEQ_iil_f64: PBoolean(Stack.Local(Args[2].Addr))^ := Float64(Args[0].Arg) = Float64(Args[1].Arg);

        // NE opcodes
        bcNE_lll_i8: PBoolean(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ <> PInt8(Stack.Local(Args[1].Addr))^;
        bcNE_lll_u8: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ <> PUInt8(Stack.Local(Args[1].Addr))^;
        bcNE_lll_i16: PBoolean(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ <> PInt16(Stack.Local(Args[1].Addr))^;
        bcNE_lll_u16: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ <> PUInt16(Stack.Local(Args[1].Addr))^;
        bcNE_lll_i32: PBoolean(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ <> PInt32(Stack.Local(Args[1].Addr))^;
        bcNE_lll_u32: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ <> PUInt32(Stack.Local(Args[1].Addr))^;
        bcNE_lll_i64: PBoolean(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ <> PInt64(Stack.Local(Args[1].Addr))^;
        bcNE_lll_u64: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ <> PUInt64(Stack.Local(Args[1].Addr))^;
        bcNE_lll_f32: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat32(Stack.Local(Args[0].Addr))^ <> PFloat32(Stack.Local(Args[1].Addr))^;
        bcNE_lll_f64: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat64(Stack.Local(Args[0].Addr))^ <> PFloat64(Stack.Local(Args[1].Addr))^;

        bcNE_lil_i8: PBoolean(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ <> Int8(Args[1].Arg);
        bcNE_lil_u8: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ <> UInt8(Args[1].Arg);
        bcNE_lil_i16: PBoolean(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ <> Int16(Args[1].Arg);
        bcNE_lil_u16: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ <> UInt16(Args[1].Arg);
        bcNE_lil_i32: PBoolean(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ <> Int32(Args[1].Arg);
        bcNE_lil_u32: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ <> UInt32(Args[1].Arg);
        bcNE_lil_i64: PBoolean(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ <> Int64(Args[1].Arg);
        bcNE_lil_u64: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ <> UInt64(Args[1].Arg);
        bcNE_lil_f32: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat32(Stack.Local(Args[0].Addr))^ <> Float32(Args[1].Arg);
        bcNE_lil_f64: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat64(Stack.Local(Args[0].Addr))^ <> Float64(Args[1].Arg);

        bcNE_ill_i8: PBoolean(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) <> PInt8(Stack.Local(Args[1].Addr))^;
        bcNE_ill_u8: PBoolean(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) <> PUInt8(Stack.Local(Args[1].Addr))^;
        bcNE_ill_i16: PBoolean(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) <> PInt16(Stack.Local(Args[1].Addr))^;
        bcNE_ill_u16: PBoolean(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) <> PUInt16(Stack.Local(Args[1].Addr))^;
        bcNE_ill_i32: PBoolean(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) <> PInt32(Stack.Local(Args[1].Addr))^;
        bcNE_ill_u32: PBoolean(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) <> PUInt32(Stack.Local(Args[1].Addr))^;
        bcNE_ill_i64: PBoolean(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) <> PInt64(Stack.Local(Args[1].Addr))^;
        bcNE_ill_u64: PBoolean(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) <> PUInt64(Stack.Local(Args[1].Addr))^;
        bcNE_ill_f32: PBoolean(Stack.Local(Args[2].Addr))^ := Float32(Args[0].Arg) <> PFloat32(Stack.Local(Args[1].Addr))^;
        bcNE_ill_f64: PBoolean(Stack.Local(Args[2].Addr))^ := Float64(Args[0].Arg) <> PFloat64(Stack.Local(Args[1].Addr))^;

        bcNE_iil_i8: PBoolean(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) <> Int8(Args[1].Arg);
        bcNE_iil_u8: PBoolean(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) <> UInt8(Args[1].Arg);
        bcNE_iil_i16: PBoolean(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) <> Int16(Args[1].Arg);
        bcNE_iil_u16: PBoolean(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) <> UInt16(Args[1].Arg);
        bcNE_iil_i32: PBoolean(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) <> Int32(Args[1].Arg);
        bcNE_iil_u32: PBoolean(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) <> UInt32(Args[1].Arg);
        bcNE_iil_i64: PBoolean(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) <> Int64(Args[1].Arg);
        bcNE_iil_u64: PBoolean(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) <> UInt64(Args[1].Arg);
        bcNE_iil_f32: PBoolean(Stack.Local(Args[2].Addr))^ := Float32(Args[0].Arg) <> Float32(Args[1].Arg);
        bcNE_iil_f64: PBoolean(Stack.Local(Args[2].Addr))^ := Float64(Args[0].Arg) <> Float64(Args[1].Arg);

        // LT opcodes
        bcLT_lll_i8: PBoolean(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ < PInt8(Stack.Local(Args[1].Addr))^;
        bcLT_lll_u8: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ < PUInt8(Stack.Local(Args[1].Addr))^;
        bcLT_lll_i16: PBoolean(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ < PInt16(Stack.Local(Args[1].Addr))^;
        bcLT_lll_u16: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ < PUInt16(Stack.Local(Args[1].Addr))^;
        bcLT_lll_i32: PBoolean(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ < PInt32(Stack.Local(Args[1].Addr))^;
        bcLT_lll_u32: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ < PUInt32(Stack.Local(Args[1].Addr))^;
        bcLT_lll_i64: PBoolean(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ < PInt64(Stack.Local(Args[1].Addr))^;
        bcLT_lll_u64: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ < PUInt64(Stack.Local(Args[1].Addr))^;
        bcLT_lll_f32: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat32(Stack.Local(Args[0].Addr))^ < PFloat32(Stack.Local(Args[1].Addr))^;
        bcLT_lll_f64: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat64(Stack.Local(Args[0].Addr))^ < PFloat64(Stack.Local(Args[1].Addr))^;

        bcLT_lil_i8: PBoolean(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ < Int8(Args[1].Arg);
        bcLT_lil_u8: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ < UInt8(Args[1].Arg);
        bcLT_lil_i16: PBoolean(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ < Int16(Args[1].Arg);
        bcLT_lil_u16: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ < UInt16(Args[1].Arg);
        bcLT_lil_i32: PBoolean(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ < Int32(Args[1].Arg);
        bcLT_lil_u32: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ < UInt32(Args[1].Arg);
        bcLT_lil_i64: PBoolean(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ < Int64(Args[1].Arg);
        bcLT_lil_u64: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ < UInt64(Args[1].Arg);
        bcLT_lil_f32: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat32(Stack.Local(Args[0].Addr))^ < Float32(Args[1].Arg);
        bcLT_lil_f64: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat64(Stack.Local(Args[0].Addr))^ < Float64(Args[1].Arg);

        bcLT_ill_i8: PBoolean(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) < PInt8(Stack.Local(Args[1].Addr))^;
        bcLT_ill_u8: PBoolean(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) < PUInt8(Stack.Local(Args[1].Addr))^;
        bcLT_ill_i16: PBoolean(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) < PInt16(Stack.Local(Args[1].Addr))^;
        bcLT_ill_u16: PBoolean(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) < PUInt16(Stack.Local(Args[1].Addr))^;
        bcLT_ill_i32: PBoolean(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) < PInt32(Stack.Local(Args[1].Addr))^;
        bcLT_ill_u32: PBoolean(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) < PUInt32(Stack.Local(Args[1].Addr))^;
        bcLT_ill_i64: PBoolean(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) < PInt64(Stack.Local(Args[1].Addr))^;
        bcLT_ill_u64: PBoolean(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) < PUInt64(Stack.Local(Args[1].Addr))^;
        bcLT_ill_f32: PBoolean(Stack.Local(Args[2].Addr))^ := Float32(Args[0].Arg) < PFloat32(Stack.Local(Args[1].Addr))^;
        bcLT_ill_f64: PBoolean(Stack.Local(Args[2].Addr))^ := Float64(Args[0].Arg) < PFloat64(Stack.Local(Args[1].Addr))^;

        bcLT_iil_i8: PBoolean(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) < Int8(Args[1].Arg);
        bcLT_iil_u8: PBoolean(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) < UInt8(Args[1].Arg);
        bcLT_iil_i16: PBoolean(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) < Int16(Args[1].Arg);
        bcLT_iil_u16: PBoolean(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) < UInt16(Args[1].Arg);
        bcLT_iil_i32: PBoolean(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) < Int32(Args[1].Arg);
        bcLT_iil_u32: PBoolean(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) < UInt32(Args[1].Arg);
        bcLT_iil_i64: PBoolean(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) < Int64(Args[1].Arg);
        bcLT_iil_u64: PBoolean(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) < UInt64(Args[1].Arg);
        bcLT_iil_f32: PBoolean(Stack.Local(Args[2].Addr))^ := Float32(Args[0].Arg) < Float32(Args[1].Arg);
        bcLT_iil_f64: PBoolean(Stack.Local(Args[2].Addr))^ := Float64(Args[0].Arg) < Float64(Args[1].Arg);

        // GT opcodes
        bcGT_lll_i8: PBoolean(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ > PInt8(Stack.Local(Args[1].Addr))^;
        bcGT_lll_u8: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ > PUInt8(Stack.Local(Args[1].Addr))^;
        bcGT_lll_i16: PBoolean(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ > PInt16(Stack.Local(Args[1].Addr))^;
        bcGT_lll_u16: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ > PUInt16(Stack.Local(Args[1].Addr))^;
        bcGT_lll_i32: PBoolean(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ > PInt32(Stack.Local(Args[1].Addr))^;
        bcGT_lll_u32: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ > PUInt32(Stack.Local(Args[1].Addr))^;
        bcGT_lll_i64: PBoolean(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ > PInt64(Stack.Local(Args[1].Addr))^;
        bcGT_lll_u64: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ > PUInt64(Stack.Local(Args[1].Addr))^;
        bcGT_lll_f32: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat32(Stack.Local(Args[0].Addr))^ > PFloat32(Stack.Local(Args[1].Addr))^;
        bcGT_lll_f64: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat64(Stack.Local(Args[0].Addr))^ > PFloat64(Stack.Local(Args[1].Addr))^;

        bcGT_lil_i8: PBoolean(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ > Int8(Args[1].Arg);
        bcGT_lil_u8: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ > UInt8(Args[1].Arg);
        bcGT_lil_i16: PBoolean(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ > Int16(Args[1].Arg);
        bcGT_lil_u16: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ > UInt16(Args[1].Arg);
        bcGT_lil_i32: PBoolean(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ > Int32(Args[1].Arg);
        bcGT_lil_u32: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ > UInt32(Args[1].Arg);
        bcGT_lil_i64: PBoolean(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ > Int64(Args[1].Arg);
        bcGT_lil_u64: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ > UInt64(Args[1].Arg);
        bcGT_lil_f32: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat32(Stack.Local(Args[0].Addr))^ > Float32(Args[1].Arg);
        bcGT_lil_f64: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat64(Stack.Local(Args[0].Addr))^ > Float64(Args[1].Arg);

        bcGT_ill_i8: PBoolean(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) > PInt8(Stack.Local(Args[1].Addr))^;
        bcGT_ill_u8: PBoolean(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) > PUInt8(Stack.Local(Args[1].Addr))^;
        bcGT_ill_i16: PBoolean(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) > PInt16(Stack.Local(Args[1].Addr))^;
        bcGT_ill_u16: PBoolean(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) > PUInt16(Stack.Local(Args[1].Addr))^;
        bcGT_ill_i32: PBoolean(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) > PInt32(Stack.Local(Args[1].Addr))^;
        bcGT_ill_u32: PBoolean(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) > PUInt32(Stack.Local(Args[1].Addr))^;
        bcGT_ill_i64: PBoolean(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) > PInt64(Stack.Local(Args[1].Addr))^;
        bcGT_ill_u64: PBoolean(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) > PUInt64(Stack.Local(Args[1].Addr))^;
        bcGT_ill_f32: PBoolean(Stack.Local(Args[2].Addr))^ := Float32(Args[0].Arg) > PFloat32(Stack.Local(Args[1].Addr))^;
        bcGT_ill_f64: PBoolean(Stack.Local(Args[2].Addr))^ := Float64(Args[0].Arg) > PFloat64(Stack.Local(Args[1].Addr))^;

        bcGT_iil_i8: PBoolean(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) > Int8(Args[1].Arg);
        bcGT_iil_u8: PBoolean(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) > UInt8(Args[1].Arg);
        bcGT_iil_i16: PBoolean(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) > Int16(Args[1].Arg);
        bcGT_iil_u16: PBoolean(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) > UInt16(Args[1].Arg);
        bcGT_iil_i32: PBoolean(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) > Int32(Args[1].Arg);
        bcGT_iil_u32: PBoolean(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) > UInt32(Args[1].Arg);
        bcGT_iil_i64: PBoolean(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) > Int64(Args[1].Arg);
        bcGT_iil_u64: PBoolean(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) > UInt64(Args[1].Arg);
        bcGT_iil_f32: PBoolean(Stack.Local(Args[2].Addr))^ := Float32(Args[0].Arg) > Float32(Args[1].Arg);
        bcGT_iil_f64: PBoolean(Stack.Local(Args[2].Addr))^ := Float64(Args[0].Arg) > Float64(Args[1].Arg);

        // GTE opcodes
        bcGTE_lll_i8: PBoolean(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ >= PInt8(Stack.Local(Args[1].Addr))^;
        bcGTE_lll_u8: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ >= PUInt8(Stack.Local(Args[1].Addr))^;
        bcGTE_lll_i16: PBoolean(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ >= PInt16(Stack.Local(Args[1].Addr))^;
        bcGTE_lll_u16: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ >= PUInt16(Stack.Local(Args[1].Addr))^;
        bcGTE_lll_i32: PBoolean(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ >= PInt32(Stack.Local(Args[1].Addr))^;
        bcGTE_lll_u32: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ >= PUInt32(Stack.Local(Args[1].Addr))^;
        bcGTE_lll_i64: PBoolean(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ >= PInt64(Stack.Local(Args[1].Addr))^;
        bcGTE_lll_u64: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ >= PUInt64(Stack.Local(Args[1].Addr))^;
        bcGTE_lll_f32: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat32(Stack.Local(Args[0].Addr))^ >= PFloat32(Stack.Local(Args[1].Addr))^;
        bcGTE_lll_f64: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat64(Stack.Local(Args[0].Addr))^ >= PFloat64(Stack.Local(Args[1].Addr))^;

        bcGTE_lil_i8: PBoolean(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ >= Int8(Args[1].Arg);
        bcGTE_lil_u8: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ >= UInt8(Args[1].Arg);
        bcGTE_lil_i16: PBoolean(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ >= Int16(Args[1].Arg);
        bcGTE_lil_u16: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ >= UInt16(Args[1].Arg);
        bcGTE_lil_i32: PBoolean(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ >= Int32(Args[1].Arg);
        bcGTE_lil_u32: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ >= UInt32(Args[1].Arg);
        bcGTE_lil_i64: PBoolean(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ >= Int64(Args[1].Arg);
        bcGTE_lil_u64: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ >= UInt64(Args[1].Arg);
        bcGTE_lil_f32: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat32(Stack.Local(Args[0].Addr))^ >= Float32(Args[1].Arg);
        bcGTE_lil_f64: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat64(Stack.Local(Args[0].Addr))^ >= Float64(Args[1].Arg);

        bcGTE_ill_i8: PBoolean(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) >= PInt8(Stack.Local(Args[1].Addr))^;
        bcGTE_ill_u8: PBoolean(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) >= PUInt8(Stack.Local(Args[1].Addr))^;
        bcGTE_ill_i16: PBoolean(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) >= PInt16(Stack.Local(Args[1].Addr))^;
        bcGTE_ill_u16: PBoolean(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) >= PUInt16(Stack.Local(Args[1].Addr))^;
        bcGTE_ill_i32: PBoolean(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) >= PInt32(Stack.Local(Args[1].Addr))^;
        bcGTE_ill_u32: PBoolean(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) >= PUInt32(Stack.Local(Args[1].Addr))^;
        bcGTE_ill_i64: PBoolean(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) >= PInt64(Stack.Local(Args[1].Addr))^;
        bcGTE_ill_u64: PBoolean(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) >= PUInt64(Stack.Local(Args[1].Addr))^;
        bcGTE_ill_f32: PBoolean(Stack.Local(Args[2].Addr))^ := Float32(Args[0].Arg) >= PFloat32(Stack.Local(Args[1].Addr))^;
        bcGTE_ill_f64: PBoolean(Stack.Local(Args[2].Addr))^ := Float64(Args[0].Arg) >= PFloat64(Stack.Local(Args[1].Addr))^;

        bcGTE_iil_i8: PBoolean(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) >= Int8(Args[1].Arg);
        bcGTE_iil_u8: PBoolean(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) >= UInt8(Args[1].Arg);
        bcGTE_iil_i16: PBoolean(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) >= Int16(Args[1].Arg);
        bcGTE_iil_u16: PBoolean(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) >= UInt16(Args[1].Arg);
        bcGTE_iil_i32: PBoolean(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) >= Int32(Args[1].Arg);
        bcGTE_iil_u32: PBoolean(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) >= UInt32(Args[1].Arg);
        bcGTE_iil_i64: PBoolean(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) >= Int64(Args[1].Arg);
        bcGTE_iil_u64: PBoolean(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) >= UInt64(Args[1].Arg);
        bcGTE_iil_f32: PBoolean(Stack.Local(Args[2].Addr))^ := Float32(Args[0].Arg) >= Float32(Args[1].Arg);
        bcGTE_iil_f64: PBoolean(Stack.Local(Args[2].Addr))^ := Float64(Args[0].Arg) >= Float64(Args[1].Arg);

        // LTE opcodes
        bcLTE_lll_i8: PBoolean(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ <= PInt8(Stack.Local(Args[1].Addr))^;
        bcLTE_lll_u8: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ <= PUInt8(Stack.Local(Args[1].Addr))^;
        bcLTE_lll_i16: PBoolean(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ <= PInt16(Stack.Local(Args[1].Addr))^;
        bcLTE_lll_u16: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ <= PUInt16(Stack.Local(Args[1].Addr))^;
        bcLTE_lll_i32: PBoolean(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ <= PInt32(Stack.Local(Args[1].Addr))^;
        bcLTE_lll_u32: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ <= PUInt32(Stack.Local(Args[1].Addr))^;
        bcLTE_lll_i64: PBoolean(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ <= PInt64(Stack.Local(Args[1].Addr))^;
        bcLTE_lll_u64: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ <= PUInt64(Stack.Local(Args[1].Addr))^;
        bcLTE_lll_f32: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat32(Stack.Local(Args[0].Addr))^ <= PFloat32(Stack.Local(Args[1].Addr))^;
        bcLTE_lll_f64: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat64(Stack.Local(Args[0].Addr))^ <= PFloat64(Stack.Local(Args[1].Addr))^;

        bcLTE_lil_i8: PBoolean(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ <= Int8(Args[1].Arg);
        bcLTE_lil_u8: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ <= UInt8(Args[1].Arg);
        bcLTE_lil_i16: PBoolean(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ <= Int16(Args[1].Arg);
        bcLTE_lil_u16: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ <= UInt16(Args[1].Arg);
        bcLTE_lil_i32: PBoolean(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ <= Int32(Args[1].Arg);
        bcLTE_lil_u32: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ <= UInt32(Args[1].Arg);
        bcLTE_lil_i64: PBoolean(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ <= Int64(Args[1].Arg);
        bcLTE_lil_u64: PBoolean(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ <= UInt64(Args[1].Arg);
        bcLTE_lil_f32: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat32(Stack.Local(Args[0].Addr))^ <= Float32(Args[1].Arg);
        bcLTE_lil_f64: PBoolean(Stack.Local(Args[2].Addr))^ := PFloat64(Stack.Local(Args[0].Addr))^ <= Float64(Args[1].Arg);

        bcLTE_ill_i8: PBoolean(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) <= PInt8(Stack.Local(Args[1].Addr))^;
        bcLTE_ill_u8: PBoolean(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) <= PUInt8(Stack.Local(Args[1].Addr))^;
        bcLTE_ill_i16: PBoolean(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) <= PInt16(Stack.Local(Args[1].Addr))^;
        bcLTE_ill_u16: PBoolean(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) <= PUInt16(Stack.Local(Args[1].Addr))^;
        bcLTE_ill_i32: PBoolean(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) <= PInt32(Stack.Local(Args[1].Addr))^;
        bcLTE_ill_u32: PBoolean(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) <= PUInt32(Stack.Local(Args[1].Addr))^;
        bcLTE_ill_i64: PBoolean(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) <= PInt64(Stack.Local(Args[1].Addr))^;
        bcLTE_ill_u64: PBoolean(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) <= PUInt64(Stack.Local(Args[1].Addr))^;
        bcLTE_ill_f32: PBoolean(Stack.Local(Args[2].Addr))^ := Float32(Args[0].Arg) <= PFloat32(Stack.Local(Args[1].Addr))^;
        bcLTE_ill_f64: PBoolean(Stack.Local(Args[2].Addr))^ := Float64(Args[0].Arg) <= PFloat64(Stack.Local(Args[1].Addr))^;

        bcLTE_iil_i8: PBoolean(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) <= Int8(Args[1].Arg);
        bcLTE_iil_u8: PBoolean(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) <= UInt8(Args[1].Arg);
        bcLTE_iil_i16: PBoolean(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) <= Int16(Args[1].Arg);
        bcLTE_iil_u16: PBoolean(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) <= UInt16(Args[1].Arg);
        bcLTE_iil_i32: PBoolean(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) <= Int32(Args[1].Arg);
        bcLTE_iil_u32: PBoolean(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) <= UInt32(Args[1].Arg);
        bcLTE_iil_i64: PBoolean(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) <= Int64(Args[1].Arg);
        bcLTE_iil_u64: PBoolean(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) <= UInt64(Args[1].Arg);
        bcLTE_iil_f32: PBoolean(Stack.Local(Args[2].Addr))^ := Float32(Args[0].Arg) <= Float32(Args[1].Arg);
        bcLTE_iil_f64: PBoolean(Stack.Local(Args[2].Addr))^ := Float64(Args[0].Arg) <= Float64(Args[1].Arg);

        // BND opcodes (Bitwise AND)
        bcBND_lll_i8: PInt8(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ and PInt8(Stack.Local(Args[1].Addr))^;
        bcBND_lll_u8: PUInt8(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ and PUInt8(Stack.Local(Args[1].Addr))^;
        bcBND_lll_i16: PInt16(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ and PInt16(Stack.Local(Args[1].Addr))^;
        bcBND_lll_u16: PUInt16(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ and PUInt16(Stack.Local(Args[1].Addr))^;
        bcBND_lll_i32: PInt32(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ and PInt32(Stack.Local(Args[1].Addr))^;
        bcBND_lll_u32: PUInt32(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ and PUInt32(Stack.Local(Args[1].Addr))^;
        bcBND_lll_i64: PInt64(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ and PInt64(Stack.Local(Args[1].Addr))^;
        bcBND_lll_u64: PUInt64(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ and PUInt64(Stack.Local(Args[1].Addr))^;

        bcBND_lil_i8: PInt8(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ and Int8(Args[1].Arg);
        bcBND_lil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ and UInt8(Args[1].Arg);
        bcBND_lil_i16: PInt16(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ and Int16(Args[1].Arg);
        bcBND_lil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ and UInt16(Args[1].Arg);
        bcBND_lil_i32: PInt32(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ and Int32(Args[1].Arg);
        bcBND_lil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ and UInt32(Args[1].Arg);
        bcBND_lil_i64: PInt64(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ and Int64(Args[1].Arg);
        bcBND_lil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ and UInt64(Args[1].Arg);

        bcBND_ill_i8: PInt8(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) and PInt8(Stack.Local(Args[1].Addr))^;
        bcBND_ill_u8: PUInt8(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) and PUInt8(Stack.Local(Args[1].Addr))^;
        bcBND_ill_i16: PInt16(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) and PInt16(Stack.Local(Args[1].Addr))^;
        bcBND_ill_u16: PUInt16(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) and PUInt16(Stack.Local(Args[1].Addr))^;
        bcBND_ill_i32: PInt32(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) and PInt32(Stack.Local(Args[1].Addr))^;
        bcBND_ill_u32: PUInt32(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) and PUInt32(Stack.Local(Args[1].Addr))^;
        bcBND_ill_i64: PInt64(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) and PInt64(Stack.Local(Args[1].Addr))^;
        bcBND_ill_u64: PUInt64(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) and PUInt64(Stack.Local(Args[1].Addr))^;

        bcBND_iil_i8: PInt8(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) and Int8(Args[1].Arg);
        bcBND_iil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) and UInt8(Args[1].Arg);
        bcBND_iil_i16: PInt16(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) and Int16(Args[1].Arg);
        bcBND_iil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) and UInt16(Args[1].Arg);
        bcBND_iil_i32: PInt32(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) and Int32(Args[1].Arg);
        bcBND_iil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) and UInt32(Args[1].Arg);
        bcBND_iil_i64: PInt64(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) and Int64(Args[1].Arg);
        bcBND_iil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) and UInt64(Args[1].Arg);

        // SHL opcodes (Shift Left)
        bcSHL_lll_i8: PInt8(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ shl PInt8(Stack.Local(Args[1].Addr))^;
        bcSHL_lll_u8: PUInt8(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ shl PUInt8(Stack.Local(Args[1].Addr))^;
        bcSHL_lll_i16: PInt16(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ shl PInt16(Stack.Local(Args[1].Addr))^;
        bcSHL_lll_u16: PUInt16(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ shl PUInt16(Stack.Local(Args[1].Addr))^;
        bcSHL_lll_i32: PInt32(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ shl PInt32(Stack.Local(Args[1].Addr))^;
        bcSHL_lll_u32: PUInt32(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ shl PUInt32(Stack.Local(Args[1].Addr))^;
        bcSHL_lll_i64: PInt64(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ shl PInt64(Stack.Local(Args[1].Addr))^;
        bcSHL_lll_u64: PUInt64(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ shl PUInt64(Stack.Local(Args[1].Addr))^;

        bcSHL_lil_i8: PInt8(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ shl Int8(Args[1].Arg);
        bcSHL_lil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ shl UInt8(Args[1].Arg);
        bcSHL_lil_i16: PInt16(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ shl Int16(Args[1].Arg);
        bcSHL_lil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ shl UInt16(Args[1].Arg);
        bcSHL_lil_i32: PInt32(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ shl Int32(Args[1].Arg);
        bcSHL_lil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ shl UInt32(Args[1].Arg);
        bcSHL_lil_i64: PInt64(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ shl Int64(Args[1].Arg);
        bcSHL_lil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ shl UInt64(Args[1].Arg);

        bcSHL_ill_i8: PInt8(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) shl PInt8(Stack.Local(Args[1].Addr))^;
        bcSHL_ill_u8: PUInt8(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) shl PUInt8(Stack.Local(Args[1].Addr))^;
        bcSHL_ill_i16: PInt16(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) shl PInt16(Stack.Local(Args[1].Addr))^;
        bcSHL_ill_u16: PUInt16(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) shl PUInt16(Stack.Local(Args[1].Addr))^;
        bcSHL_ill_i32: PInt32(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) shl PInt32(Stack.Local(Args[1].Addr))^;
        bcSHL_ill_u32: PUInt32(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) shl PUInt32(Stack.Local(Args[1].Addr))^;
        bcSHL_ill_i64: PInt64(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) shl PInt64(Stack.Local(Args[1].Addr))^;
        bcSHL_ill_u64: PUInt64(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) shl PUInt64(Stack.Local(Args[1].Addr))^;

        bcSHL_iil_i8: PInt8(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) shl Int8(Args[1].Arg);
        bcSHL_iil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) shl UInt8(Args[1].Arg);
        bcSHL_iil_i16: PInt16(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) shl Int16(Args[1].Arg);
        bcSHL_iil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) shl UInt16(Args[1].Arg);
        bcSHL_iil_i32: PInt32(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) shl Int32(Args[1].Arg);
        bcSHL_iil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) shl UInt32(Args[1].Arg);
        bcSHL_iil_i64: PInt64(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) shl Int64(Args[1].Arg);
        bcSHL_iil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) shl UInt64(Args[1].Arg);

        // SHR opcodes (Shift Right)
        bcSHR_lll_i8: PInt8(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ shr PInt8(Stack.Local(Args[1].Addr))^;
        bcSHR_lll_u8: PUInt8(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ shr PUInt8(Stack.Local(Args[1].Addr))^;
        bcSHR_lll_i16: PInt16(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ shr PInt16(Stack.Local(Args[1].Addr))^;
        bcSHR_lll_u16: PUInt16(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ shr PUInt16(Stack.Local(Args[1].Addr))^;
        bcSHR_lll_i32: PInt32(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ shr PInt32(Stack.Local(Args[1].Addr))^;
        bcSHR_lll_u32: PUInt32(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ shr PUInt32(Stack.Local(Args[1].Addr))^;
        bcSHR_lll_i64: PInt64(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ shr PInt64(Stack.Local(Args[1].Addr))^;
        bcSHR_lll_u64: PUInt64(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ shr PUInt64(Stack.Local(Args[1].Addr))^;

        bcSHR_lil_i8: PInt8(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ shr Int8(Args[1].Arg);
        bcSHR_lil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ shr UInt8(Args[1].Arg);
        bcSHR_lil_i16: PInt16(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ shr Int16(Args[1].Arg);
        bcSHR_lil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ shr UInt16(Args[1].Arg);
        bcSHR_lil_i32: PInt32(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ shr Int32(Args[1].Arg);
        bcSHR_lil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ shr UInt32(Args[1].Arg);
        bcSHR_lil_i64: PInt64(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ shr Int64(Args[1].Arg);
        bcSHR_lil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ shr UInt64(Args[1].Arg);

        bcSHR_ill_i8: PInt8(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) shr PInt8(Stack.Local(Args[1].Addr))^;
        bcSHR_ill_u8: PUInt8(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) shr PUInt8(Stack.Local(Args[1].Addr))^;
        bcSHR_ill_i16: PInt16(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) shr PInt16(Stack.Local(Args[1].Addr))^;
        bcSHR_ill_u16: PUInt16(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) shr PUInt16(Stack.Local(Args[1].Addr))^;
        bcSHR_ill_i32: PInt32(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) shr PInt32(Stack.Local(Args[1].Addr))^;
        bcSHR_ill_u32: PUInt32(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) shr PUInt32(Stack.Local(Args[1].Addr))^;
        bcSHR_ill_i64: PInt64(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) shr PInt64(Stack.Local(Args[1].Addr))^;
        bcSHR_ill_u64: PUInt64(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) shr PUInt64(Stack.Local(Args[1].Addr))^;

        bcSHR_iil_i8: PInt8(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) shr Int8(Args[1].Arg);
        bcSHR_iil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) shr UInt8(Args[1].Arg);
        bcSHR_iil_i16: PInt16(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) shr Int16(Args[1].Arg);
        bcSHR_iil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) shr UInt16(Args[1].Arg);
        bcSHR_iil_i32: PInt32(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) shr Int32(Args[1].Arg);
        bcSHR_iil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) shr UInt32(Args[1].Arg);
        bcSHR_iil_i64: PInt64(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) shr Int64(Args[1].Arg);
        bcSHR_iil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) shr UInt64(Args[1].Arg);

        // XOR opcodes (Bitwise XOR)
        bcXOR_lll_i8: PInt8(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ xor PInt8(Stack.Local(Args[1].Addr))^;
        bcXOR_lll_u8: PUInt8(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ xor PUInt8(Stack.Local(Args[1].Addr))^;
        bcXOR_lll_i16: PInt16(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ xor PInt16(Stack.Local(Args[1].Addr))^;
        bcXOR_lll_u16: PUInt16(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ xor PUInt16(Stack.Local(Args[1].Addr))^;
        bcXOR_lll_i32: PInt32(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ xor PInt32(Stack.Local(Args[1].Addr))^;
        bcXOR_lll_u32: PUInt32(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ xor PUInt32(Stack.Local(Args[1].Addr))^;
        bcXOR_lll_i64: PInt64(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ xor PInt64(Stack.Local(Args[1].Addr))^;
        bcXOR_lll_u64: PUInt64(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ xor PUInt64(Stack.Local(Args[1].Addr))^;

        bcXOR_lil_i8: PInt8(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ xor Int8(Args[1].Arg);
        bcXOR_lil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ xor UInt8(Args[1].Arg);
        bcXOR_lil_i16: PInt16(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ xor Int16(Args[1].Arg);
        bcXOR_lil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ xor UInt16(Args[1].Arg);
        bcXOR_lil_i32: PInt32(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ xor Int32(Args[1].Arg);
        bcXOR_lil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ xor UInt32(Args[1].Arg);
        bcXOR_lil_i64: PInt64(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ xor Int64(Args[1].Arg);
        bcXOR_lil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ xor UInt64(Args[1].Arg);

        bcXOR_ill_i8: PInt8(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) xor PInt8(Stack.Local(Args[1].Addr))^;
        bcXOR_ill_u8: PUInt8(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) xor PUInt8(Stack.Local(Args[1].Addr))^;
        bcXOR_ill_i16: PInt16(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) xor PInt16(Stack.Local(Args[1].Addr))^;
        bcXOR_ill_u16: PUInt16(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) xor PUInt16(Stack.Local(Args[1].Addr))^;
        bcXOR_ill_i32: PInt32(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) xor PInt32(Stack.Local(Args[1].Addr))^;
        bcXOR_ill_u32: PUInt32(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) xor PUInt32(Stack.Local(Args[1].Addr))^;
        bcXOR_ill_i64: PInt64(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) xor PInt64(Stack.Local(Args[1].Addr))^;
        bcXOR_ill_u64: PUInt64(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) xor PUInt64(Stack.Local(Args[1].Addr))^;

        bcXOR_iil_i8: PInt8(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) xor Int8(Args[1].Arg);
        bcXOR_iil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) xor UInt8(Args[1].Arg);
        bcXOR_iil_i16: PInt16(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) xor Int16(Args[1].Arg);
        bcXOR_iil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) xor UInt16(Args[1].Arg);
        bcXOR_iil_i32: PInt32(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) xor Int32(Args[1].Arg);
        bcXOR_iil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) xor UInt32(Args[1].Arg);
        bcXOR_iil_i64: PInt64(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) xor Int64(Args[1].Arg);
        bcXOR_iil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) xor UInt64(Args[1].Arg);

        // BOR opcodes (Bitwise OR)
        bcBOR_lll_i8: PInt8(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ or PInt8(Stack.Local(Args[1].Addr))^;
        bcBOR_lll_u8: PUInt8(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ or PUInt8(Stack.Local(Args[1].Addr))^;
        bcBOR_lll_i16: PInt16(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ or PInt16(Stack.Local(Args[1].Addr))^;
        bcBOR_lll_u16: PUInt16(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ or PUInt16(Stack.Local(Args[1].Addr))^;
        bcBOR_lll_i32: PInt32(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ or PInt32(Stack.Local(Args[1].Addr))^;
        bcBOR_lll_u32: PUInt32(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ or PUInt32(Stack.Local(Args[1].Addr))^;
        bcBOR_lll_i64: PInt64(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ or PInt64(Stack.Local(Args[1].Addr))^;
        bcBOR_lll_u64: PUInt64(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ or PUInt64(Stack.Local(Args[1].Addr))^;

        bcBOR_lil_i8: PInt8(Stack.Local(Args[2].Addr))^ := PInt8(Stack.Local(Args[0].Addr))^ or Int8(Args[1].Arg);
        bcBOR_lil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := PUInt8(Stack.Local(Args[0].Addr))^ or UInt8(Args[1].Arg);
        bcBOR_lil_i16: PInt16(Stack.Local(Args[2].Addr))^ := PInt16(Stack.Local(Args[0].Addr))^ or Int16(Args[1].Arg);
        bcBOR_lil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := PUInt16(Stack.Local(Args[0].Addr))^ or UInt16(Args[1].Arg);
        bcBOR_lil_i32: PInt32(Stack.Local(Args[2].Addr))^ := PInt32(Stack.Local(Args[0].Addr))^ or Int32(Args[1].Arg);
        bcBOR_lil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := PUInt32(Stack.Local(Args[0].Addr))^ or UInt32(Args[1].Arg);
        bcBOR_lil_i64: PInt64(Stack.Local(Args[2].Addr))^ := PInt64(Stack.Local(Args[0].Addr))^ or Int64(Args[1].Arg);
        bcBOR_lil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := PUInt64(Stack.Local(Args[0].Addr))^ or UInt64(Args[1].Arg);

        bcBOR_ill_i8: PInt8(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) or PInt8(Stack.Local(Args[1].Addr))^;
        bcBOR_ill_u8: PUInt8(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) or PUInt8(Stack.Local(Args[1].Addr))^;
        bcBOR_ill_i16: PInt16(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) or PInt16(Stack.Local(Args[1].Addr))^;
        bcBOR_ill_u16: PUInt16(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) or PUInt16(Stack.Local(Args[1].Addr))^;
        bcBOR_ill_i32: PInt32(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) or PInt32(Stack.Local(Args[1].Addr))^;
        bcBOR_ill_u32: PUInt32(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) or PUInt32(Stack.Local(Args[1].Addr))^;
        bcBOR_ill_i64: PInt64(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) or PInt64(Stack.Local(Args[1].Addr))^;
        bcBOR_ill_u64: PUInt64(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) or PUInt64(Stack.Local(Args[1].Addr))^;

        bcBOR_iil_i8: PInt8(Stack.Local(Args[2].Addr))^ := Int8(Args[0].Arg) or Int8(Args[1].Arg);
        bcBOR_iil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := UInt8(Args[0].Arg) or UInt8(Args[1].Arg);
        bcBOR_iil_i16: PInt16(Stack.Local(Args[2].Addr))^ := Int16(Args[0].Arg) or Int16(Args[1].Arg);
        bcBOR_iil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := UInt16(Args[0].Arg) or UInt16(Args[1].Arg);
        bcBOR_iil_i32: PInt32(Stack.Local(Args[2].Addr))^ := Int32(Args[0].Arg) or Int32(Args[1].Arg);
        bcBOR_iil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := UInt32(Args[0].Arg) or UInt32(Args[1].Arg);
        bcBOR_iil_i64: PInt64(Stack.Local(Args[2].Addr))^ := Int64(Args[0].Arg) or Int64(Args[1].Arg);
        bcBOR_iil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := UInt64(Args[0].Arg) or UInt64(Args[1].Arg);

        // --- SAR opcodes (Shift Arithmetic Right) ---
        // lll (local, local, local)
        bcSAR_lll_i8: PInt8(Stack.Local(Args[2].Addr))^ := Sar(PInt8(Stack.Local(Args[0].Addr))^, PInt8(Stack.Local(Args[1].Addr))^);
        bcSAR_lll_u8: PUInt8(Stack.Local(Args[2].Addr))^ := Sar(PUInt8(Stack.Local(Args[0].Addr))^, PUInt8(Stack.Local(Args[1].Addr))^);
        bcSAR_lll_i16: PInt16(Stack.Local(Args[2].Addr))^ := Sar(PInt16(Stack.Local(Args[0].Addr))^, PInt16(Stack.Local(Args[1].Addr))^);
        bcSAR_lll_u16: PUInt16(Stack.Local(Args[2].Addr))^ := Sar(PUInt16(Stack.Local(Args[0].Addr))^, PUInt16(Stack.Local(Args[1].Addr))^);
        bcSAR_lll_i32: PInt32(Stack.Local(Args[2].Addr))^ := Sar(PInt32(Stack.Local(Args[0].Addr))^, PInt32(Stack.Local(Args[1].Addr))^);
        bcSAR_lll_u32: PUInt32(Stack.Local(Args[2].Addr))^ := Sar(PUInt32(Stack.Local(Args[0].Addr))^, PUInt32(Stack.Local(Args[1].Addr))^);
        bcSAR_lll_i64: PInt64(Stack.Local(Args[2].Addr))^ := Sar(PInt64(Stack.Local(Args[0].Addr))^, PInt64(Stack.Local(Args[1].Addr))^);
        bcSAR_lll_u64: PUInt64(Stack.Local(Args[2].Addr))^ := Sar(PUInt64(Stack.Local(Args[0].Addr))^, PUInt64(Stack.Local(Args[1].Addr))^);

        // lil (local, immediate, local)
        bcSAR_lil_i8: PInt8(Stack.Local(Args[2].Addr))^ := Sar(PInt8(Stack.Local(Args[0].Addr))^, Int8(Args[1].Arg));
        bcSAR_lil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := Sar(PUInt8(Stack.Local(Args[0].Addr))^, UInt8(Args[1].Arg));
        bcSAR_lil_i16: PInt16(Stack.Local(Args[2].Addr))^ := Sar(PInt16(Stack.Local(Args[0].Addr))^, Int16(Args[1].Arg));
        bcSAR_lil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := Sar(PUInt16(Stack.Local(Args[0].Addr))^, UInt16(Args[1].Arg));
        bcSAR_lil_i32: PInt32(Stack.Local(Args[2].Addr))^ := Sar(PInt32(Stack.Local(Args[0].Addr))^, Int32(Args[1].Arg));
        bcSAR_lil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := Sar(PUInt32(Stack.Local(Args[0].Addr))^, UInt32(Args[1].Arg));
        bcSAR_lil_i64: PInt64(Stack.Local(Args[2].Addr))^ := Sar(PInt64(Stack.Local(Args[0].Addr))^, Int64(Args[1].Arg));
        bcSAR_lil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := Sar(PUInt64(Stack.Local(Args[0].Addr))^, UInt64(Args[1].Arg));

        // ill (immediate, local, local)
        bcSAR_ill_i8: PInt8(Stack.Local(Args[2].Addr))^ := Sar(Int8(Args[0].Arg), PInt8(Stack.Local(Args[1].Addr))^);
        bcSAR_ill_u8: PUInt8(Stack.Local(Args[2].Addr))^ := Sar(UInt8(Args[0].Arg), PUInt8(Stack.Local(Args[1].Addr))^);
        bcSAR_ill_i16: PInt16(Stack.Local(Args[2].Addr))^ := Sar(Int16(Args[0].Arg), PInt16(Stack.Local(Args[1].Addr))^);
        bcSAR_ill_u16: PUInt16(Stack.Local(Args[2].Addr))^ := Sar(UInt16(Args[0].Arg), PUInt16(Stack.Local(Args[1].Addr))^);
        bcSAR_ill_i32: PInt32(Stack.Local(Args[2].Addr))^ := Sar(Int32(Args[0].Arg), PInt32(Stack.Local(Args[1].Addr))^);
        bcSAR_ill_u32: PUInt32(Stack.Local(Args[2].Addr))^ := Sar(UInt32(Args[0].Arg), PUInt32(Stack.Local(Args[1].Addr))^);
        bcSAR_ill_i64: PInt64(Stack.Local(Args[2].Addr))^ := Sar(Int64(Args[0].Arg), PInt64(Stack.Local(Args[1].Addr))^);
        bcSAR_ill_u64: PUInt64(Stack.Local(Args[2].Addr))^ := Sar(UInt64(Args[0].Arg), PUInt64(Stack.Local(Args[1].Addr))^);

        // iil (immediate, immediate, local)
        bcSAR_iil_i8: PInt8(Stack.Local(Args[2].Addr))^ := Sar(Int8(Args[0].Arg), Int8(Args[1].Arg));
        bcSAR_iil_u8: PUInt8(Stack.Local(Args[2].Addr))^ := Sar(UInt8(Args[0].Arg), UInt8(Args[1].Arg));
        bcSAR_iil_i16: PInt16(Stack.Local(Args[2].Addr))^ := Sar(Int16(Args[0].Arg), Int16(Args[1].Arg));
        bcSAR_iil_u16: PUInt16(Stack.Local(Args[2].Addr))^ := Sar(UInt16(Args[0].Arg), UInt16(Args[1].Arg));
        bcSAR_iil_i32: PInt32(Stack.Local(Args[2].Addr))^ := Sar(Int32(Args[0].Arg), Int32(Args[1].Arg));
        bcSAR_iil_u32: PUInt32(Stack.Local(Args[2].Addr))^ := Sar(UInt32(Args[0].Arg), UInt32(Args[1].Arg));
        bcSAR_iil_i64: PInt64(Stack.Local(Args[2].Addr))^ := Sar(Int64(Args[0].Arg), Int64(Args[1].Arg));
        bcSAR_iil_u64: PUInt64(Stack.Local(Args[2].Addr))^ := Sar(UInt64(Args[0].Arg), UInt64(Args[1].Arg));

        bcNEWFRAME:
          begin
            CallStack.Push(PPtrInt(Stack.StackPtr)^, Stack.StackPtr);
            PPtrInt(Stack.StackPtr)^ := 0;
            Stack.StackPtr += Args[0].Addr;
          end;

        bcINVOKE:
          begin
            // Check recursion depth
            if RecursionDepth >= MAX_RECURSION_DEPTH then
              raise RuntimeError.Create('Recursion depth limit exceeded');

            Inc(RecursionDepth);
            PPtrInt(Stack.StackPtr)^ := pc;
            pc := PtrInt(Stack.Global(Args[0].Addr)^);
          end;

        bcINVOKEX:
          CallExternal(Pointer(Args[0].Addr), Args[1].Arg);

        bcRET:
          begin
            if CallStack.Top > -1 then
            begin
              if nArgs = 2 then
                Move(Stack.Local(Args[0].Addr)^, ArgStack.Pop()^,  Args[1].Addr);

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
            mpLocal:  PrintInt(Stack.Local(Args[0].Arg), XprTypeSize[Args[0].Typ]);
            mpGlobal: PrintInt(Stack.Global(Args[0].Arg), XprTypeSize[Args[0].Typ]);
          end;
        bcPRTf:
          case Args[0].Pos of
            mpLocal: PrintReal(Stack.Local(Args[0].Arg), XprTypeSize[Args[0].Typ]);
            mpGlobal:PrintReal(Stack.Global(Args[0].Arg), XprTypeSize[Args[0].Typ]);
          end;
        bcPRTb:
          case Args[0].Pos of
            mpLocal:  WriteLn(Boolean(Stack.Local(Args[0].Arg)^));
            mpGlobal: WriteLn(Boolean(Stack.Global(Args[0].Arg)^));
          end;


        else
          begin
            WriteStr(e, code);
            raise RuntimeError.Create('Not implemented @ '+ IntToStr(pc-1) + ' - ' + e);
          end;
      end;
    end;

    Inc(pc);
  end;

  //WriteLn(Stack.AsString());
end;



(*
  procedure for typecasting, it's used to ensure same type left and right before binary operations..

  i32 + i64

  should produce a typecast, then binary op

  i64 := i32;
  i64 + i64;

  So type mixing add a bit of a cost, but the result is a simpler interpreter.

  Todo: Does not handle it all correctly, we need to handle for combinations of MemPos:
  * local [left and right], where local means the variable is stored in `stack[args[x]+stackpos]`
  * global [left and right], where global means var is stored in heap, where ptr is immediate `PType(args[x]+stackpos)^`
  * imm [cant be assigned to, only right], where imm can contain anything up to 64 bits, a move should suffice. Even floats can be in imm.
    any and all immediate will not have type defined, but we know based on the other (left) what type right is, except that right is always 64 bits
*)
procedure TInterpreter.HandleASGN(Instr: TBytecodeInstruction);
var
  srcPtr, destPtr: Pointer;
  srcType, destType: EExpressBaseType;
  srcSize, destSize: Integer;
  tempInt: Int64;
  tempFloat: Double;
begin
  // Get source and destination information
  srcType := Instr.Args[0].Typ;
  destType := Instr.Args[1].Typ;
  srcSize := XprTypeSize[srcType];
  destSize := XprTypeSize[destType];

  if destSize > srcSize then
    case Instr.Args[0].Pos of
      mpImm:   ;
      mpLocal:  FillByte(Stack.Local(Instr.Args[0].Arg)^, destSize, 0);
      mpGlobal: FillByte(Stack.Global(Instr.Args[0].Arg)^, destSize, 0);
    end;

  with Instr do
    case Int8(Args[0].Pos) * 10 + Int8(Args[1].Pos) of
      Int8(mpLocal)*10+Int8(mpLocal):
        Move(
          Pointer(Stack.Local(Args[1].Arg))^,
          Pointer(Stack.Local(Args[0].Arg))^,
          XprTypeSize[Args[0].Typ]
        );
      Int8(mpLocal)*10+Int8(mpGlobal):
        Move(
          Pointer(Stack.Global(Args[1].Arg))^,
          Pointer(Stack.Local(Args[0].Arg))^,
          XprTypeSize[Args[0].Typ]
        );
      Int8(mpGlobal)*10+Int8(mpLocal):
        Move(
          Pointer(Stack.Local(Args[1].Arg))^,
          Pointer(Stack.Global(Args[0].Arg))^,
          XprTypeSize[Args[0].Typ]
        );
      Int8(mpGlobal)*10+Int8(mpGlobal):
        Move(
          Pointer(Stack.Global(Args[1].Arg))^,
          Pointer(Stack.Global(Args[0].Arg))^,
          XprTypeSize[Args[0].Typ]
        );
      Int8(mpGlobal)*10+Int8(mpImm):
        Move(
          Args[1].Arg,
          Pointer(Stack.Global(Args[0].Arg))^,
          XprTypeSize[Args[0].Typ]
        );
      Int8(mpLocal)*10+Int8(mpImm):
        Move(
          Args[1].Arg,
          Pointer(Stack.Local(Args[0].Arg))^,
          XprTypeSize[Args[0].Typ]
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
