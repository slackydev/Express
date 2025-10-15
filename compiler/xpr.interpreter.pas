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
  xpr.Errors;

const
  STACK_SIZE = 4 * 1024 * 1024;  // 4MB static stack
  MAX_RECURSION_DEPTH = 100;   // Recursion depth limit
  STACK_FRAME_SIZE = 64 * 1024;  // 64KB per stack frame (adjust as needed)

type
  TByteArray = array of Byte;
  PByteArray = ^TByteArray;

  TTranslateArray = array[EBytecode] of PtrUInt;
  TArrayRec = record Refcount, High: SizeInt; end;

  TArgStack = record
    Data: array [0..$FFFF] of Pointer;
    Count: SizeInt;

    procedure Push(ref: Pointer); inline;
    function Pop(): Pointer; inline;
  end;

  TCallFrame = record
    ReturnAddress: Pointer;
    FrameBase: PByte;
    StackPtr: PByte;
    FunctionHeaderPC: PtrUInt;
  end;

  TCallStack = record
    Frames: array[0..MAX_RECURSION_DEPTH-1] of TCallFrame; // Static array
    Top: Int32;
    procedure Init;
    procedure Push(ReturnAddr: Pointer; StackPtr, FrameBase: PByte; FuncHeaderPC: PtrUInt); inline;
    function Pop: TCallFrame; inline;
    function Peek: TCallFrame; inline;
  end;

  TInterpreter = record
    ArgStack: TArgStack;
    CallStack: TCallStack;
    TryStack: TCallStack;

    NativeException, CurrentException: Pointer;

    RecursionDepth: Int32;
    ProgramStart: PtrUInt;

    // Tracking
    ProgramRawLocation, ProgramBase: Pointer;
    HasCreatedJIT: Boolean;

    // the stack
    Data: TByteArray;      // static stack is a tad faster, but limited to 2-4MB max
    BasePtr: PByte;        // Base pointer for stack
    StackPtr: PByte;       // Pointer to current stack position

    procedure StackInit(Stack: TStackArray; StackPos: SizeInt);
    function GetProgramCounter(): Int32;
    procedure SetProgramCounter(pc: Int32);

    constructor New(Emitter: TBytecodeEmitter; StartPos: PtrUInt; Opt:EOptimizerFlags);
    procedure Free(var BC: TBytecode);

    function Global(offset: PtrUInt): Pointer; inline;
    function AsString(): string;

    {$IFDEF xpr_UseSuperInstructions}
    procedure FreeCodeBlock(CodePtr: Pointer; TotalSize: SizeInt);
    procedure FreeJIT(var BC: TBytecode);
    function EmitCodeBlock(CodeList: PBytecodeInstruction; Translation: TTranslateArray; Count: Int32; var TotalSize: SizeInt): Pointer;
    function EmitJITBlock(CodeList: PBytecodeInstruction; Count: Int32; var TotalSize: SizeInt; CanJMP:Boolean=False): Pointer;
    procedure GenerateSuperInstructions(var BC: TBytecode; Translation: TTranslateArray);
    procedure x86_64_Compile(var BC: TBytecode; AllowJumps: Boolean);
    {$ENDIF}

    procedure RunSafe(var BC: TBytecode);
    function Run(var BC: TBytecode): Int32;

    // error handling
    function BuildStackTraceString(const BC: TBytecode): string;
    procedure TranslateNativeException(const FpcException: Exception; ToExceptionClass: Pointer);
    function GetCurrentExceptionString(): string;
    procedure WriteExceptionStr(ToExceptionClass: Pointer; Message: string);

    // runtime
    procedure CallExternal(FuncPtr: Pointer; ArgCount: UInt16; hasReturn: Boolean); inline;
    procedure HandleASGN(Instr: TBytecodeInstruction; HeapLeft: Boolean);
    function IsA(ClassVMTs: TVMTList; CurrentID, TargetID: Int32): Boolean; inline;
    procedure DynCast(ClassVMTs: TVMTList; const Instruction: PBytecodeInstruction);
    function GetVirtualMethod(ClassVMTs: TVMTList; SelfPtr: Pointer; MethodIndex: Int32): PtrInt;
    procedure ArrayRefcount(Left, Right: Pointer);
    procedure IncRef(Left: Pointer);
    procedure DecRef(Left: Pointer);
    procedure PushClosure(ClosureRec: Pointer);

    property ProgramCounter: Int32 read GetProgramCounter write SetProgramCounter;
  end;


implementation

uses
  Math,
  xpr.Utils
  {$IFDEF xpr_UseSuperInstructions},
    {$IFDEF WINDOWS}Windows,JIT_x64{$ENDIF}
    {$IFDEF UNIX}SysCall, BaseUnix, Unix{$ENDIF}

  {$ENDIF};


{$IFDEF xpr_UseSuperInstructions}
const
  PROT_READ  = $1;
  PROT_WRITE = $2;
  PROT_EXEC  = $4;

  MAP_PRIVATE   = $0002;
  MAP_ANONYMOUS = $1000;
  MAP_FAILED    = Pointer(-1);
{$ENDIF}


{$I interpreter.functions.inc}

procedure PrintInt(v:Pointer; size:Byte);
begin
  case size of
    1: WriteLn(Int8(v^));
    2: WriteLn(Int16(v^));
    4: WriteLn(Int32(v^));
    8: WriteLn(Int64(v^));
  end;
end;

procedure PrintReal(v:Pointer; size:Byte);
begin
  case size of
    4: WriteLn(Format('%.8f', [Single(v^)]));
    8: WriteLn(Format('%.13f', [Double(v^)]));
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

  BasePtr  := @Data[0];
  StackPtr := @Data[0] + StackPos;
end;

function TInterpreter.GetProgramCounter(): Int32;
begin
  Result := (PtrUInt(Self.ProgramRawLocation)-PtrUInt(Self.ProgramBase)) div SizeOf(TBytecodeInstruction);
end;

procedure TInterpreter.SetProgramCounter(pc: Int32);
begin
  Self.ProgramRawLocation := Pointer(PtrUInt(Self.ProgramBase) + pc * SizeOf(TBytecodeInstruction));
end;

// used by load_global, and invoke
function TInterpreter.Global(offset: PtrUInt): Pointer;
begin
  Result := @Self.Data[offset];
end;

function TInterpreter.AsString(): string;
var i,i32: Int32; i64: Int64; p: PtrInt;
begin
  Result := '';
  (*
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
  *)
end;



// TCallStack implementation
procedure TCallStack.Init;
begin
  Top := -1;
end;

procedure TCallStack.Push(ReturnAddr: Pointer; StackPtr, FrameBase: PByte; FuncHeaderPC: PtrUInt);
begin
  Assert(Top < MAX_RECURSION_DEPTH-1, 'Call stack overflow (recursion too deep)');
  Inc(Top);
  Frames[Top].ReturnAddress := ReturnAddr;
  Frames[Top].StackPtr  := StackPtr;
  Frames[Top].FrameBase := FrameBase;
  Frames[Top].FunctionHeaderPC := FuncHeaderPC;
end;

function TCallStack.Pop: TCallFrame;
begin
  Assert(Top >= 0, 'Call stack underflow');
  Result := Frames[Top];
  Dec(Top);
end;

function TCallStack.Peek: TCallFrame;
begin
  Assert(Top >= 0, 'Call stack underflow');
  Result := Frames[Top];
end;

// TInterpreter implementation
constructor TInterpreter.New(Emitter: TBytecodeEmitter; StartPos: PtrUInt; Opt:EOptimizerFlags);
var
  i,j: Int32;
  PMethods: array [0..511] of PtrInt;
begin
  StackInit(Emitter.Stack, Emitter.UsedStackSize); //stackptr = after global allocations
  CallStack.Init();
  RecursionDepth := 0;
  ProgramStart   := StartPos;
  ArgStack.Count := 0;

  // populate methods variables and VMT
  with Emitter.Bytecode do
  begin
    for i:=0 to High(Emitter.Bytecode.FunctionTable) do
    begin
      PtrInt(Pointer(BasePtr + FunctionTable[i].DataLocation)^) := FunctionTable[i].CodeLocation;
      if FunctionTable[i].ClassID <> -1 then
        ClassVMTs.Data[FunctionTable[i].ClassID].Methods[FunctionTable[i].VMTIndex] := FunctionTable[i].CodeLocation;
    end;

    // inheritance
    // populate parents VMT into children:
    for i:=ClassVMTs.High downto 1 do
    begin
      if ClassVMTs.Data[i].ParentID < 0 then
        continue;

      PMethods := ClassVMTs.Data[ClassVMTs.Data[i].ParentID].Methods;
      for j:=0 to High(PMethods) do
        if (ClassVMTs.Data[i].Methods[j] = -1) and (PMethods[j] <> -1) then
          ClassVMTs.Data[i].Methods[j] := PMethods[j];
    end;
  end;
end;


procedure TInterpreter.Free(var BC: TBytecode);
begin
  {$IFDEF xpr_UseSuperInstructions}
  Self.FreeJIT(BC);
  {$ENDIF}
end;

{$IFDEF xpr_UseSuperInstructions}
{$I interpreter.jitcode.inc}
{$ENDIF}


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
  IsNativeException: Boolean;
  Code: Int32;

  function HandleException(): Boolean;
  begin
    Result := False;
    // this is an normal runtime exception, triggering early exit
    if TryStack.Top < 0 then
    begin
      WriteLn('Fatal: ', Self.GetCurrentExceptionString());
      Writeln('RuntimeError: ', BC.Docpos.Data[ProgramCounter].ToString() + ' - Code:', BC.Code.Data[ProgramCounter].Code, ', pc: ', ProgramCounter);
      Writeln();
      WriteLn(Self.BuildStackTraceString(BC));
      Exit(True);
    end;

    TryFrame := TryStack.Pop();
    StackPtr := TryFrame.StackPtr;
    BasePtr  := TryFrame.FrameBase;
    ProgramCounter := PtrUInt(TryFrame.ReturnAddress);
  end;
var
  OldMask: TFPUExceptionMask;
begin
  //as per IEEE 754
  OldMask := GetExceptionMask;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

  Self.ProgramBase := @BC.Code.Data[0];
  Self.ProgramCounter := 0;
  Self.TryStack.Init();
  Self.CurrentException := nil;

  // ...
  repeat
    try
      Code := Self.Run(BC);
      if Code = 0 then
        Break
      else if (code = 1) and HandleException() then
        Break;
    except
      on E: Exception do // Catches BOTH native FPC errors and our VM raise
      begin
        // Exception is native
        // RuntimeError is special VM exception
        IsNativeException := (Self.CurrentException = nil);

        // It was a native error (like 1/0). Translate it now.
        if IsNativeException then
          TranslateNativeException(E, Self.NativeException);

        if HandleException() then
          Break;
      end;
    end;
  until False; // This loop is now only exited by a Break.

  SetExceptionMask(oldMask);
end;


function TInterpreter.Run(var BC: TBytecode): Int32;
type
  TSuperMethod = procedure();
  TJitMethod   = procedure(BasePtr: Pointer);
var
  pc: ^TBytecodeInstruction;
  frame: TCallFrame;
  e: String;
  left, right: Pointer;
  {$IFDEF xpr_UseSuperInstructions}
  JumpTable: TTranslateArray;
  hot_condition: TSuperMethod;
  {$ENDIF}
label
  {$i interpreter.super.labels.inc}
begin
  {$IFDEF xpr_UseSuperInstructions}
  (* should be allowed to disable easily in case not portable - and for debugging *)
  if (not Self.HasCreatedJIT) then
  begin
    {$i interpreter.super.bc2lb.inc}
    {$IFDEF WIN64}
    // JIT LEVEL = 2
    x86_64_Compile(BC, True);  // capture loops
    x86_64_Compile(BC, False); // capture linear
    {$ENDIF}
    // JIT LEVEL = 1
    Self.GenerateSuperInstructions(BC, JumpTable);
    Self.HasCreatedJIT := True;
    WriteLn('JIT Success!');
  end;
  {$ENDIF}

  Result := 0;
  pc := @BC.Code.Data[ProgramCounter];

  while True do
  begin
    begin
      //WriteLn('*** ', ProgramCounter, ' && ', pc^.Code);
      case pc^.Code of
        bcNOOP: (* nothing *);

        {$IFDEF xpr_UseSuperInstructions}
        bcHOTLOOP:
          begin
            left := Pointer(BasePtr + pc^.Args[2].Data.Addr);
            hot_condition := TSuperMethod(pc^.Args[4].Data.Addr);
            while True do
            begin
              hot_condition();                         //EQ, LT, GT etc..
              if not PBoolean(left)^ then              //JZ
              begin
                Inc(pc, pc^.Args[1].Data.i32);
                Break;
              end;
              Inc(pc);
              TSuperMethod(pc^.Args[4].Data.Addr)();    //body
              Inc(pc, pc^.Args[0].Data.i32+1);          //reljmp
            end;
          end;

        bcHOTLOOP_JIT:
          begin
            left := Pointer(BasePtr + pc^.Args[2].Data.Addr);
            hot_condition := TSuperMethod(pc^.Args[4].Data.Addr);
            while True do
            begin
              hot_condition();                      //EQ, LT, GT etc..
              if PBoolean(left)^ then               //JZ
              begin
                Inc(pc);
                TJITMethod(pc^.Args[4].Data.Addr)(BasePtr);  //body
                Inc(pc, pc^.nArgs);
                Inc(pc, pc^.Args[0].Data.i32+1);   //reljmp
              end else begin
                Inc(pc, pc^.Args[1].Data.i32);
                Break;
              end;
            end;
          end;

        bcSUPER:
          begin
            TSuperMethod(pc^.Args[4].Data.Addr)();
            Dec(pc);
            //continue; // pc already increased
          end;

        bcJIT:
          begin
            TJITMethod(pc^.Args[4].Data.Addr)(BasePtr);
            Inc(pc, pc^.nArgs-1);
            //continue;
          end;
        {$ENDIF}

        bcJMP: pc := @BC.Code.Data[pc^.Args[0].Data.i32];

        bcRELJMP: Inc(pc, pc^.Args[0].Data.i32);

        bcJZ:  if not PBoolean(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ then Inc(pc, pc^.Args[1].Data.i32);
        bcJNZ: if PByte(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <> 0  then Inc(pc, pc^.Args[1].Data.i32);

        bcJZ_i:  if pc^.Args[0].Data.Arg = 0  then Inc(pc, pc^.Args[1].Data.i32);
        bcJNZ_i: if pc^.Args[0].Data.Arg <> 0 then Inc(pc, pc^.Args[1].Data.i32);

        bcBCHK:
          begin
            Left := PPointer(BasePtr + pc^.Args[0].Data.Addr)^;
            if PtrUInt(Left) = 0 then
            begin
              Self.CurrentException := PPointer(BasePtr + pc^.Args[2].Data.Addr)^;
              Self.WriteExceptionStr(Self.CurrentException, 'Out of range, array is empty!');
              Exit(1);
            end;

            if pc^.Args[1].Pos = mpLocal then
              case pc^.Args[1].BaseType of
                xtInt8:  PtrInt(Right) := PInt8(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
                xtInt16: PtrInt(Right) := PInt16(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
                xtInt32: PtrInt(Right) := PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
                xtInt64: PtrInt(Right) := PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
                xtUInt8:  PtrInt(Right) := PUInt8(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
                xtUInt16: PtrInt(Right) := PUInt16(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
                xtUInt32: PtrInt(Right) := PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
                xtUInt64: PtrInt(Right) := PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
                xtAnsiChar: PtrInt(Right) := PUInt8(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
                xtUnicodeChar: PtrInt(Right) := PUInt16(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
                xtBoolean: PtrInt(Right) := PUInt8(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
                else
                  WriteLn('THIS IS IMPOSSIBLE');
              end
            else
              PtrInt(Right) := pc^.Args[1].Data.Arg;

            if PtrInt(Right) > TArrayRec((Left-SizeOf(SizeInt)*2)^).High then
            begin
              Self.CurrentException := PPointer(BasePtr + pc^.Args[2].Data.Addr)^;
              Self.WriteExceptionStr(Self.CurrentException, Format('Out of range: Index=%d for Array[0..%d]', [PtrInt(Right), TArrayRec((Left-SizeOf(SizeInt)*2)^).High]));
              Exit(1);
            end;
          end;

        {$I interpreter.super.asgn_code.inc}
        {$I interpreter.super.binary_code.inc}

        bcFILL:
          FillByte(Pointer(BasePtr + pc^.Args[0].Data.Addr)^, pc^.Args[1].Data.Addr, pc^.Args[2].Data.u8);

        bcSET_ERRHANDLER:
          Self.NativeException := PPointer(BasePtr + pc^.Args[0].Data.Addr)^;

        // Arg[0] = The exception object instance to throw
        bcRAISE:
          begin
            Self.CurrentException := PPointer(BasePtr + pc^.Args[0].Data.Addr)^;
            Exit(1);
          end;

        bcGET_EXCEPTION:
          begin
            PPointer(BasePtr + pc^.Args[0].Data.Addr)^ := Self.CurrentException;
            IncRef(Self.CurrentException);
          end;

        bcUNSET_EXCEPTION:
          Self.CurrentException := nil;

        bcNEW:
          begin
            left := AllocMem(pc^.Args[2].Data.i32);

            // VMT
            SizeInt(Pointer(left)^) := pc^.Args[1].Data.i32;
            Inc(left, SizeOf(SizeInt));

            // Refcount - nothing references this yet
            SizeInt(Pointer(left)^) := 0;
            Inc(left, SizeOf(SizeInt));

            // Size
            SizeInt(Pointer(left)^) := pc^.Args[2].Data.i32;
            Inc(left, SizeOf(SizeInt));

            PPointer(BasePtr + pc^.Args[0].Data.Addr)^ := left;
          end;

        bcRELEASE:
          begin
            left := PPointer(BasePtr + pc^.Args[0].Data.Addr)^;
            FreeMem(left - SizeOf(Pointer)*3);
            PPointer(BasePtr + pc^.Args[0].Data.Addr)^ := nil;
          end;

        bcDYNCAST:
          Self.DynCast(BC.ClassVMTs,pc);

        bcIS:
          begin
            left := PPointer(BasePtr + pc^.Args[1].Data.Addr)^;
            if left = nil then
              PBoolean(BasePtr + pc^.Args[0].Data.Addr)^ := False
            else
              PBoolean(BasePtr + pc^.Args[0].Data.Addr)^ := Self.IsA(
                BC.ClassVMTs,
                BC.ClassVMTs.Data[PPtrInt(Pointer(left) - SizeOf(Pointer) * 3)^].SelfID,
                pc^.Args[2].Data.i32
              );
          end;

        // try except
        bcIncTry:
          TryStack.Push(Pointer(pc^.args[0].Data.Addr), StackPtr, BasePtr, 0);

        bcDecTry:
          TryStack.Pop();

        // array managment;
        bcINCLOCK:
          Self.IncRef(PPointer(BasePtr + pc^.Args[0].Data.Addr)^);
        bcDECLOCK:
          Self.DecRef(PPointer(BasePtr + pc^.Args[0].Data.Addr)^);

        bcREFCNT:
          Self.ArrayRefcount(Pointer(Pointer(BasePtr + pc^.Args[0].Data.Addr)^), Pointer(Pointer(BasePtr + pc^.Args[1].Data.Addr)^));

        bcREFCNT_imm:
          Self.ArrayRefcount(Pointer(Pointer(BasePtr + pc^.Args[0].Data.Addr)^), Pointer(pc^.Args[1].Data.Addr));

        // string operators
        bcLOAD_STR:
        begin
          PPointer(BasePtr + pc^.Args[0].Data.Addr)^ := Pointer(BC.StringTable[pc^.Args[1].Data.Addr]);
          //no owner yet:
        end;

        bcADD_STR:
          begin
            PAnsiString(BasePtr + pc^.Args[2].Data.Addr)^ := PAnsiString(BasePtr + pc^.Args[0].Data.Addr)^ + PAnsiString(BasePtr + pc^.Args[1].Data.Addr)^;
            //no owner yet:
            PSizeInt(PPointer(BasePtr + pc^.Args[2].Data.Addr)^-SizeOf(SizeInt)*2)^ := 0;
          end;

        bcCh2Str:
          begin
            PPointer(BasePtr + pc^.Args[0].Data.Addr)^ := nil;
            case pc^.Args[1].Pos of
              mpImm:   PAnsiString(BasePtr + pc^.Args[0].Data.Addr)^ := AnsiChar(pc^.Args[1].Data.u8);
              mpLocal: PAnsiString(BasePtr + pc^.Args[0].Data.Addr)^ := PAnsiChar(BasePtr + pc^.Args[1].Data.Addr)^;
            end;
            //no owner yet!
            PSizeInt(PPointer(BasePtr + pc^.Args[0].Data.Addr)^-SizeOf(SizeInt)*2)^ := 0;
          end;

        bcADDR:
          PPointer(BasePtr + pc^.Args[0].Data.Addr)^ := (BasePtr + pc^.Args[1].Data.Addr);


        bcINC_i32: Inc( PInt32(Pointer(BasePtr + pc^.Args[0].Data.i32))^);
        bcINC_u32: Inc(PUInt32(Pointer(BasePtr + pc^.Args[0].Data.u32))^);
        bcINC_i64: Inc( PInt64(Pointer(BasePtr + pc^.Args[0].Data.Arg))^);
        bcINC_u64: Inc(PUInt64(Pointer(BasePtr + pc^.Args[0].Data.u64))^);

        bcFMA_i8:  PPtrInt(Pointer(BasePtr + pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ + PInt8(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr;
        bcFMA_u8:  PPtrInt(Pointer(BasePtr + pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ + PUInt8(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr;
        bcFMA_i16: PPtrInt(Pointer(BasePtr + pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ + PInt16(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr;
        bcFMA_u16: PPtrInt(Pointer(BasePtr + pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ + PUInt16(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr;
        bcFMA_i32: PPtrInt(Pointer(BasePtr + pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ + PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr;
        bcFMA_u32: PPtrInt(Pointer(BasePtr + pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ + PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr;
        bcFMA_i64: PPtrInt(Pointer(BasePtr + pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ + PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr;
        bcFMA_u64: PPtrInt(Pointer(BasePtr + pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ + PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr;

        bcFMA_imm_i8:  PPtrInt(Pointer(BasePtr + pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ + Int8(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr;
        bcFMA_imm_u8:  PPtrInt(Pointer(BasePtr + pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ + UInt8(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr;
        bcFMA_imm_i16: PPtrInt(Pointer(BasePtr + pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ + Int16(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr;
        bcFMA_imm_u16: PPtrInt(Pointer(BasePtr + pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ + UInt16(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr;
        bcFMA_imm_i32: PPtrInt(Pointer(BasePtr + pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ + Int32(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr;
        bcFMA_imm_u32: PPtrInt(Pointer(BasePtr + pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ + UInt32(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr;
        bcFMA_imm_i64: PPtrInt(Pointer(BasePtr + pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ + Int64(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr;
        bcFMA_imm_u64: PPtrInt(Pointer(BasePtr + pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ + UInt64(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr;


        bcDREF: Move(Pointer(Pointer(BasePtr + pc^.Args[1].Data.Addr)^)^, Pointer(BasePtr + pc^.Args[0].Data.Addr)^, pc^.Args[2].Data.Addr);
        bcDREF_32: PUInt32(BasePtr + pc^.Args[0].Data.Addr)^ := PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr)^)^;
        bcDREF_64: PUInt64(BasePtr + pc^.Args[0].Data.Addr)^ := PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr)^)^;

        // fast addressing operations | addr + element * itemsize
        bcFMAD_d64_64:
          PInt64(Pointer(BasePtr + pc^.Args[3].Data.Addr))^ := PUInt64(PPtrInt(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ + PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr)^;

        bcFMAD_d64_32:
          PInt64(Pointer(BasePtr + pc^.Args[3].Data.Addr))^ := PUInt64(PPtrInt(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ + PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr)^;

        bcFMAD_d32_64:
          PInt32(Pointer(BasePtr + pc^.Args[3].Data.Addr))^ := PUInt32(PPtrInt(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ + PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr)^;

        bcFMAD_d32_32:
          PInt32(Pointer(BasePtr + pc^.Args[3].Data.Addr))^ := PUInt32(PPtrInt(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ + PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr)^;

        // MOV for other stuff
        bcMOV, bcMOVH:
          HandleASGN(pc^, pc^.Code=bcMOVH);

        // push the address of the variable (a reference)
        //
        bcPUSH:
          if pc^.Args[0].Pos = mpLocal then
            ArgStack.Push(Pointer(BasePtr + pc^.Args[0].Data.Addr))
          else
            ArgStack.Push(@pc^.Args[0].Data.Raw);

        // dereferences and pushes
        bcPUSHREF:
          ArgStack.Push(Pointer(Pointer(BasePtr + pc^.Args[0].Data.Addr)^));

        bcPUSH_FP:
          ArgStack.Push(BasePtr);

        bcPUSH_CLOSURE:
          Self.PushClosure(Pointer(BasePtr + pc^.Args[0].Data.Addr));


        // pop [and dereference] - write pop to stack
        // function arguments are references, write the value (a copy)
        bcPOP:
          Move(ArgStack.Pop()^, Pointer(BasePtr + pc^.Args[1].Data.Addr)^, pc^.Args[0].Data.Addr);

        // pop [and dereference] - write ptr to pop
        // if argstack contains a pointer we can write a local value to
        bcRPOP:
          Move(Pointer(BasePtr + pc^.Args[0].Data.Addr)^, ArgStack.Pop()^,  pc^.Args[1].Data.Addr);

        // pop [as reference] - write pop to stack
        // function arguments are references, write the address to the var
        bcPOPH:
          Pointer(Pointer(BasePtr + pc^.Args[0].Data.Addr)^) := ArgStack.Pop();

        // using a global in local scope, assign it's reference
        //bcLOAD_EXTERN:
        //  Pointer(Pointer(BasePtr + pc^.Args[0].Data.Addr)^) := Pointer(BasePtr + pc^.Args[1].Data.Addr)^;

        // using a global in local scope, assign it's reference
        bcLOAD_GLOBAL:
          Pointer(Pointer(BasePtr + pc^.Args[0].Data.Addr)^) := Global(pc^.Args[1].Data.Addr);

        bcCOPY_GLOBAL:
          Pointer(Pointer(BasePtr + pc^.Args[0].Data.Addr)^) := Pointer(Global(pc^.Args[1].Data.Addr)^);

        bcNEWFRAME:
          begin
            // This might save us from a lot of bullshit:
            FillByte(StackPtr^, pc^.Args[0].Data.Addr+SizeOf(Pointer), 0);
            BasePtr  := StackPtr;              // where old frame ends, is where the new one starts
            StackPtr += pc^.Args[0].Data.Addr; // inc by frame
          end;

        bcINVOKE:
          begin
            Inc(Self.RecursionDepth);

            // 1. Determine the target PC and store it in the 'left' scratch variable.
            if pc^.Args[0].Pos = mpGlobal then // is global var
              PtrUInt(left) := PtrInt(Global(pc^.Args[0].Data.Addr)^)
            else
              PtrUInt(left) := PPtrInt(BasePtr + pc^.Args[0].Data.Addr)^;

            // 2. Push the current pc as return address and the target pc (from 'left') as the header.
            CallStack.Push(pc, StackPtr, BasePtr, PtrUInt(left));

            // 3. Jump to the target.
            pc := @BC.Code.Data[PtrUInt(left)];
          end;


        bcINVOKEX:
          CallExternal(Pointer(pc^.Args[0].Data.Addr), pc^.Args[1].Data.u16, pc^.Args[2].Data.i8 <> 0);

        bcINVOKE_VIRTUAL:
          begin
            Inc(Self.RecursionDepth);

            // 1. Get the target PC from the VMT and store it in the 'left' scratch variable.
            PtrUInt(left) := Self.GetVirtualMethod(
              BC.ClassVMTs,
              ArgStack.Data[(ArgStack.Count - pc^.Args[1].Data.i32) + pc^.Args[2].Data.i32],
              pc^.Args[0].Data.i32
            );

            // 2. Push the current pc and the target pc (from 'left').
            CallStack.Push(pc, StackPtr, BasePtr, PtrUInt(left));

            // 3. Jump to the target.
            pc := @BC.Code.Data[PtrUInt(left)];
          end;

        bcRET:
          begin
            if CallStack.Top > -1 then
            begin
              frame := CallStack.Pop;
              StackPtr := Frame.StackPtr;
              BasePtr  := Frame.FrameBase;
              Dec(RecursionDepth);
              pc := Pointer(frame.ReturnAddress);
            end else
              Break;
          end;

        bcPRTi:
          case pc^.Args[0].Pos of
            mpImm:    PrintInt(@pc^.Args[0].Data.Arg, 8);
            mpLocal:  PrintInt(Pointer(BasePtr + pc^.Args[0].Data.Addr), XprTypeSize[pc^.Args[0].BaseType]);
          end;
        bcPRTf:
          case pc^.Args[0].Pos of
            mpLocal: PrintReal(Pointer(BasePtr + pc^.Args[0].Data.Addr), XprTypeSize[pc^.Args[0].BaseType]);
            mpImm:   PrintReal(@pc^.Args[0].Data.Raw, XprTypeSize[pc^.Args[0].BaseType]);
          end;
        bcPRTb:
          case pc^.Args[0].Pos of
            mpLocal:  WriteLn(Boolean(Pointer(BasePtr + pc^.Args[0].Data.Addr)^));
            mpImm:    WriteLn(Boolean(pc^.Args[0].Data.u8));
          end;
        bcPRT:
          case pc^.Args[0].Pos of
            mpLocal:  WriteLn(PAnsiString(BasePtr + pc^.Args[0].Data.Addr)^);
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
    // on 32bit this is a HUGE cost.
    Self.ProgramRawLocation := pc;
    //Self.ProgramCounter := (PtrUInt(PC)-PtrUInt(nullpc)) div SizeOf(TBytecodeInstruction);
  end;

  Exit;


  (* labeled opcodes for selective inlining *)
  {$i interpreter.super.fmad.inc}
  {$i interpreter.super.binary.inc}
  {$i interpreter.super.asgn.inc}
end;


function TInterpreter.IsA(ClassVMTs: TVMTList; CurrentID, TargetID: Int32): Boolean;
begin
  // This loop walks up the inheritance chain using the ParentID from the VMT.
  while CurrentID <> -1 do // -1 indicates no parent (base class)
  begin
    if CurrentID = TargetID then Exit(True);
    CurrentID := ClassVMTs.Data[CurrentID].ParentID;
  end;
  Result := False;
end;

procedure TInterpreter.DynCast(ClassVMTs: TVMTList; const Instruction: PBytecodeInstruction);
var
  DestAddr: PtrInt;
  SourcePtr: Pointer;
  ActualClassID, TargetClassID, VMTIndex: Integer;
begin
  // Args from Instruction^: [DestVar], [SourceVar], [TargetClassID]
  DestAddr      := PtrInt(BasePtr + Instruction^.Args[0].Data.Addr);
  SourcePtr     := PPointer(BasePtr + Instruction^.Args[1].Data.Addr)^;
  TargetClassID := Instruction^.Args[2].Data.i32;

  if SourcePtr = nil then
  begin
    PPointer(DestAddr)^ := nil;
    Exit;
  end;

  VMTIndex      := PPtrInt(Pointer(SourcePtr) - SizeOf(Pointer) * 3)^;
  ActualClassID := ClassVMTs.Data[VMTIndex].SelfID;

  if IsA(ClassVMTs, ActualClassID, TargetClassID) then
  begin
    PPointer(DestAddr)^ := SourcePtr;
  end else
  begin
    // Failure: The cast is invalid. Raise a runtime error.
    raise RuntimeError.Create(
      Format('Invalid class cast: Cannot cast an object of type ID %d to type ID %d.', [ActualClassID, TargetClassID])
    );
  end;
end;

function TInterpreter.GetVirtualMethod(ClassVMTs: TVMTList; SelfPtr: Pointer; MethodIndex: Int32): PtrInt;
var
  VMTIndex: Int32;
begin
  // Safety check: calling a method on a nil object.
  if Pointer(SelfPtr^) = nil then
    raise RuntimeError.Create('Access violation: method call on a nil object');

  VMTIndex := SizeInt(((Pointer(SelfPtr^)-SizeOf(Pointer)*3))^);
  if (VMTIndex < 0) or (VMTIndex > ClassVMTs.High()) then
    raise RuntimeError.Create('Invalid Class ID found in object: '+IntTostr(VMTIndex));

  Result := ClassVMTs.Data[VMTIndex].Methods[MethodIndex];
  if Result = -1 then
    raise RuntimeError.Create('Abstract method called or invalid VMT');
end;

procedure TInterpreter.ArrayRefcount(Left, Right: Pointer);
begin
  if (Left = nil) and (Right = nil) then Exit;

  if (Right = nil) then
  begin
    Self.DecRef(Left);
  end else if Left = nil then
  begin
    Self.IncRef(Right);
  end else if PtrInt(Left^) <> PtrInt(Right^) then
  begin
    Self.DecRef(Left);
    Self.IncRef(Right);
  end;
end;


procedure TInterpreter.IncRef(Left: Pointer);
begin
  if (left <> nil) then
  begin
    Inc(TArrayRec((Left-SizeOf(SizeInt)*2)^).Refcount);
  end;
end;

procedure TInterpreter.DecRef(Left: Pointer);
begin
  if (left <> nil) then
  begin
    Dec(TArrayRec((Left-SizeOf(SizeInt)*2)^).Refcount);
  end;
end;


procedure TInterpreter.PushClosure(ClosureRec: Pointer);
type TClosureRec = packed record Func: Pointer; Size: SizeInt; Refs: array of Pointer; end;
var
  i: Int32;
begin
  for i:=0 to High(TClosureRec(ClosureRec^).Refs) do
    Self.ArgStack.Push(TClosureRec(ClosureRec^).Refs[i]);
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
        Pointer(Instr.Args[1].Data.Addr)^,
        Pointer(Pointer(BasePtr + Instr.Args[0].Data.Addr))^,
        Instr.Args[2].Data.i32)
    else
      Move(
        Pointer(Pointer(BasePtr + Instr.Args[1].Data.Addr))^,
        Pointer(Pointer(BasePtr + Instr.Args[0].Data.Addr))^,
        Instr.Args[2].Data.i32);
  end else
  begin
    if Instr.Args[1].Pos = mpImm then
      Move(
        Pointer(Instr.Args[1].Data.Addr)^,
        Pointer(Pointer(Pointer(BasePtr + Instr.Args[0].Data.Addr))^)^,
        Instr.Args[2].Data.i32)
    else
      Move(
        Pointer(Pointer(BasePtr + Instr.Args[1].Data.Addr))^,
        Pointer(Pointer(Pointer(BasePtr + Instr.Args[0].Data.Addr))^)^,
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


function TInterpreter.BuildStackTraceString(const BC: TBytecode): string;
var
  i: Integer;
  Frame: TCallFrame;
  CurrentFuncHeaderPC, CallSitePC: PtrUInt;
  FuncName, LineInfo: string;
begin
  Result := 'Stack Trace:' + LineEnding;

  // --- Step 1: The current, failing function ---
  if CallStack.Top < 0 then
    FuncName := ''
  else
  begin
    CurrentFuncHeaderPC := CallStack.Peek.FunctionHeaderPC;

    // Get the name from the header and the line info from the actual error location.
    if BC.Code.Data[CurrentFuncHeaderPC].Code = bcNOOP then
      FuncName := BC.StringTable[BC.Code.Data[CurrentFuncHeaderPC].Args[0].Data.Addr]
    else
      FuncName := '<Unknown Function>';
  end;

  LineInfo := BC.Docpos.Data[Self.ProgramCounter].ToString();
  Result += Format('  at %s (%s) [pc=%d]', [FuncName, LineInfo, Self.ProgramCounter]) + LineEnding;

  // --- Step 2: Walk the rest of the call stack for the callers ---
  for i := CallStack.Top downto 1 do
  begin
    Frame := CallStack.Frames[i];

    // The function name is directly available from the stored header PC.
    FuncName := BC.StringTable[BC.Code.Data[CallStack.Frames[i-1].FunctionHeaderPC].Args[0].Data.Addr];

    // The location of the call is the instruction *before* the return address.
    CallSitePC := (PtrUInt(Frame.ReturnAddress) - PtrUInt(@BC.Code.Data[0])) div SizeOf(TBytecodeInstruction);
    LineInfo := BC.Docpos.Data[CallSitePC].ToString();

    Result += Format('  from %s (%s) [pc=%d]', [FuncName, LineInfo, CallSitePC]) + LineEnding;
  end;

  Frame := CallStack.Frames[0];
  FuncName := '';
  CallSitePC := (PtrUInt(Frame.ReturnAddress) - PtrUInt(@BC.Code.Data[0])) div SizeOf(TBytecodeInstruction);
  LineInfo := BC.Docpos.Data[CallSitePC].ToString();

  Result += Format('  from (%s) [pc=%d]', [LineInfo, CallSitePC]) + LineEnding;
end;

procedure TInterpreter.TranslateNativeException(const FpcException: Exception; ToExceptionClass: Pointer);
begin
  // 1. Safety check: If the singleton was never registered, we can't do anything.
  if ToExceptionClass = nil then
    Exit;

  // classes are like records, first field **must** contain the Message.
  // Our strings are compatible with FPC.
  PAnsiString(ToExceptionClass)^ := FpcException.Message;

  Self.CurrentException := ToExceptionClass;

  // Probably will cause leak, we should probably NOT do this.. i'll check it out later on.
  IncRef(ToExceptionClass);
end;

function TInterpreter.GetCurrentExceptionString(): string;
begin
  if Self.CurrentException <> nil then
    Result := PAnsiString(Self.CurrentException)^
  else
    Result := 'Interrupted with no exception handling (82526289)';
end;

procedure TInterpreter.WriteExceptionStr(ToExceptionClass: Pointer; Message: string);
begin
  if ToExceptionClass <> nil then
    PAnsiString(ToExceptionClass)^ := Message;
end;

end.
