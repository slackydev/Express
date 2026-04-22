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
  MAX_RECURSION_DEPTH = 10000;   // Recursion depth limit

// VM Execution States (RunCode)
const
  VM_RUNNING   = 0;   // Normal execution state
  VM_EXCEPTION = 1;   // An exception was raised; currently unwinding to find a catch block
  VM_SOFT_STOP = 2;   // External termination requested; unwinding memory cleanups only
  VM_HALTED    = 255; // Execution has completely finished, or fatally crashed

type
  TByteArray = array of Byte;
  PByteArray = ^TByteArray;

  TTranslateArray = array[EBytecode] of PtrUInt;
  TArrayRec = record Refcount, High: SizeInt; end;

  // ArgStack exists purely for function-calls
  // All call arguments are pushed onto this stack
  TArgStack = record
    Data:     array of Pointer;
    Count:    SizeInt;
    Capacity: SizeInt;

    procedure Init(ACapacity: SizeInt = 256);
    procedure Push(ref: Pointer); inline;
    function Pop(): Pointer; inline;
  end;

  // Callframe is maintains frame data, this is used for calls but also for
  // exception handling frames (try-except).
  TCallFrame = packed record
    ReturnAddress: Pointer;
    FrameBase: PByte;
    StackPtr: PByte;
    FunctionHeaderPC: PtrUInt;
  end;

  // Maintain a stack of all frames, used for calls and for try
  //
  TCallStack = record
    Frames: array of TCallFrame;
    Top: Int32;
    Capacity: Int32;

    procedure Init(ACapacity: Int32 = 64);
    procedure Push(ReturnAddr: Pointer; StackPtr, FrameBase: PByte; FuncHeaderPC: PtrUInt); inline;
    function Pop: TCallFrame; inline;
    function Peek: TCallFrame; inline;
  end;

  // Superinstructions, and the x64 jit.
  TSuperMethod = procedure();
  TJitMethod   = function(BasePtr: Pointer): Int32;

  TInterpreter = record
    MemBases: array[EMemPos] of PByte;
    ArgStack: TArgStack;
    CallStack: TCallStack;
    TryStack: TCallStack;

    NativeException, CurrentException: Pointer;

    RunCode: Byte;
    RecursionDepth: Int32;
    ProgramStart: PtrUInt;

    // Tracking
    ProgramRawLocation, ProgramBase: Pointer;
    HasCreatedJIT: Boolean;
    IsThread: Boolean;

    // the stack
    Data: TByteArray;      // static stack is a tad faster, but limited to 2-4MB max
    BasePtr: PByte;        // Base pointer for stack
    StackPtr: PByte;       // Pointer to current stack position
    GlobalBase: PByte;

    // JIT and superinstrutions
    JumpTable: TTranslateArray;
    hot_condition: TSuperMethod;

    // Error

    procedure StackInit(Stack: TStackArray; StackPos: SizeInt);
    function GetProgramCounter(): Int32;
    procedure SetProgramCounter(pc: Int32);

    constructor New(Emitter: TBytecodeEmitter; StartPos: PtrUInt; Opt:EOptimizerFlags);
    constructor NewForThread(MainInterp: TInterpreter; EntryPC: Int32; BCSize: Int32; ThreadStackSize: SizeInt = 64 * 1024);

    procedure Free(var BC: TBytecode);

    function Global(offset: PtrUInt): Pointer; inline;
    function AsString(): string;

    {$IFNDEF xpr_DisableJIT}
    procedure FreeCodeBlock(CodePtr: Pointer; TotalSize: SizeInt);
    procedure FreeJIT(var BC: TBytecode);
    function EmitCodeBlock(CodeList: PBytecodeInstruction; Translation: TTranslateArray; Count: Int32; var TotalSize: SizeInt): Pointer;
    function EmitJITBlock(CodeList: PBytecodeInstruction; Count: Int32; var TotalSize: SizeInt; Settings:PCompilerSettings; CanJMP:Boolean=False): Pointer;
    procedure GenerateSuperInstructions(var BC: TBytecode; Translation: TTranslateArray);
    procedure x86_64_Compile(var BC: TBytecode; AllowJumps: Boolean);
    {$ENDIF}

    function CallFunction(BC: TBytecode; Location: PtrUInt; Args: array of Pointer): Int32;
    procedure RunSafe(var BC: TBytecode; ResetExceptions:Boolean=True);
    procedure Run(var BC: TBytecode);

    // error handling
    function BuildStackTraceString(const BC: TBytecode): string;
    procedure TranslateNativeException(const FpcException: Exception; ToExceptionClass: Pointer);
    function GetCurrentExceptionString(): string;
    procedure WriteExceptionStr(ToExceptionClass: Pointer; Message: string);

    // runtime
    procedure CallExternal(FuncPtr: Pointer; ArgCount: UInt16; hasReturn: Boolean); inline;
    function BoundsCheck(pc: PBytecodeInstruction): Int32;
    procedure HandleMOV(pc: PBytecodeInstruction);
    procedure HandleSTORE(pc: PBytecodeInstruction);
    procedure HandleBinary(pc: PBytecodeInstruction);
    function IsA(ClassVMTs: TVMTList; CurrentID, TargetID: Int32): Boolean; inline;
    procedure DynCastOpcode(ClassVMTs: TVMTList; const pc: PBytecodeInstruction);
    function GetVirtualMethod(ClassVMTs: TVMTList; SelfPtr: Pointer; MethodIndex: Int32): PtrInt;

    { Increment the reference count for a managed value.
      Unified implementation via FPC's dynarray function, which works
      for strings, arrays, and classes alike since all share the same
      rc field offset (ptr - 2*SizeOf(SizeInt)). }
    procedure IncRef(Left: Pointer; BaseType: EExpressBaseType);

    { Decrement the reference count and free the allocation when rc reaches 0.
      Zeroes the caller's pointer after freeing. Dispatches by BaseType:
      - Arrays/strings → FPC RTL (handles freeing + nil)
      - Classes        → manual dec + FreeMem if zero (no virtual destructor;
                         the Collect path handles destructor dispatch) }
    procedure DecRef(var Left: Pointer; BaseType: EExpressBaseType);

    { Manage refcounts for a heap-slot overwrite (e.g. a[i] := newVal).
      Increments the new value's rc, decrements the old value's rc.
      Uses pointer identity — not value comparison — to detect self-assignment. }
    procedure ArrayRefcount(var Left: Pointer; Right: Pointer; BaseType: EExpressBaseType);

    procedure PushClosure(ClosureRec: Pointer);
    procedure RunThreadOpcode(pc: PBytecodeInstruction; var bc: TBytecode);
    procedure NewClassOpcode(pc: PBytecodeInstruction; var bc: TBytecode);
    procedure TransferArgsFromInterp(var Src: TInterpreter; ArgCount: Int32);
    function CreateFFICallback(pc: PBytecodeInstruction; Lambda, CallbackType: Pointer): Pointer;
    property ProgramCounter: Int32 read GetProgramCounter write SetProgramCounter;
  end;


implementation

uses
  Math,
  xpr.Utils,
  xpr.ffi,
  TypInfo
  {$IFNDEF xpr_DisableJIT},
    JIT_x64
    {$IFDEF WINDOWS},
    Windows
    {$ENDIF}
    {$IFDEF UNIX},
    BaseUnix, Unix
    {$ENDIF}
  {$ENDIF};

{$IFNDEF xpr_DisableJIT}
const
  PROT_READ  = $1;
  PROT_WRITE = $2;
  PROT_EXEC  = $4;

  MAP_PRIVATE   = $0002;
  MAP_ANONYMOUS = $1000;
  MAP_FAILED    = Pointer(-1);
{$ENDIF}


{$DEFINE BASEPTR_0 := (BasePtr + pc^.Args[0].Data.Addr)}
{$DEFINE BASEPTR_1 := (BasePtr + pc^.Args[1].Data.Addr)}
{$DEFINE BASEPTR_2 := (BasePtr + pc^.Args[2].Data.Addr)}
{$DEFINE BASEPTR_3 := (BasePtr + pc^.Args[3].Data.Addr)}
{$DEFINE MEMBASE_0 := (MemBases[pc^.Args[0].Pos] + pc^.Args[0].Data.Addr)}
{$DEFINE MEMBASE_1 := (MemBases[pc^.Args[1].Pos] + pc^.Args[1].Data.Addr)}
{$DEFINE MEMBASE_2 := (MemBases[pc^.Args[2].Pos] + pc^.Args[2].Data.Addr)}
{$DEFINE MEMBASE_3 := (MemBases[pc^.Args[3].Pos] + pc^.Args[3].Data.Addr)}
{$DEFINE MEMBASE_4 := (MemBases[pc^.Args[4].Pos] + pc^.Args[4].Data.Addr)}


// =============================================================================
// Internal FPC memory manager hooks
// We piggyback heavily on FPCs native refcounting for strings and arrays.
// It saves us writing a lot of garbage collection logic from scratch.
// =============================================================================

procedure fpc_dynarray_incr_ref(p: Pointer);
  [external name 'FPC_DYNARRAY_INCR_REF'];

procedure fpc_dynarray_decr_ref(var p: Pointer; typeinfo: Pointer);
  [external name 'FPC_DYNARRAY_DECR_REF'];

procedure fpc_ansistr_incr_ref(s: Pointer);
  [external name 'FPC_ANSISTR_INCR_REF'];

procedure fpc_ansistr_decr_ref(var s: Pointer);
  [external name 'FPC_ANSISTR_DECR_REF'];

procedure fpc_unicodestr_incr_ref(s: Pointer);
  [external name 'FPC_UNICODESTR_INCR_REF'];

procedure fpc_unicodestr_decr_ref(var s: Pointer);
  [external name 'FPC_UNICODESTR_DECR_REF'];

{$I interpreter.functions.inc}


// =============================================================================
// Threading Support
// Handles spinning up a new OS thread running its own interpreter instance
// =============================================================================

type
  TThreadData = record
    Interp: ^TInterpreter;
    BC:     ^TBytecode;
  end;
  PThreadData = ^TThreadData;

function XprThreadEntry(Data: Pointer): PtrInt;
var
  TD: PThreadData;
begin
  TD := PThreadData(Data);
  TD^.Interp^.RunSafe(TD^.BC^, False);
  SetLength(TD^.Interp^.Data,             0);  // thread stack
  SetLength(TD^.Interp^.CallStack.Frames, 0);  // call stack
  SetLength(TD^.Interp^.TryStack.Frames,  0);  // try stack
  SetLength(TD^.Interp^.ArgStack.Data,    0);  // arg stack
  FreeMem(TD^.Interp);
  FreeMem(TD);
  Result := 0;
  EndThread;
end;


// ============================================================================
// Stack Implementations
// Basic fast array wrappers because using classes here would be too slow
// ============================================================================

procedure TArgStack.Init(ACapacity: SizeInt = 256);
begin
  SetLength(Data, ACapacity);
  Capacity := ACapacity;
  Count := 0;
end;

procedure TArgStack.Push(ref: Pointer);
begin
  if Count >= Capacity then
  begin
    Capacity := Max(Capacity * 2, 16);
    SetLength(Data, Capacity);
  end;
  Data[Count] := ref;
  Inc(Count);
end;

function TArgStack.Pop(): Pointer;
begin
  Dec(Count);
  Result := Data[Count];
end;

procedure TCallStack.Init(ACapacity: Int32 = 64);
begin
  SetLength(Frames, ACapacity);
  Capacity := ACapacity;
  Top := -1;
end;

procedure TCallStack.Push(ReturnAddr: Pointer; StackPtr, FrameBase: PByte; FuncHeaderPC: PtrUInt);
begin
  Inc(Top);
  if Top >= Capacity then
  begin
    Capacity := Capacity * 2;
    SetLength(Frames, Capacity);
  end;
  Frames[Top].ReturnAddress    := ReturnAddr;
  Frames[Top].StackPtr         := StackPtr;
  Frames[Top].FrameBase        := FrameBase;
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


// ============================================================================
// Interpreter Initialization and Teardown
// Setting up the virtual machine state
// ============================================================================

constructor TInterpreter.New(Emitter: TBytecodeEmitter; StartPos: PtrUInt; Opt:EOptimizerFlags);
var
  i,j: Int32;
  PMethods: array [0..511] of PtrInt;
begin
  //stackptr = after global allocations
  //baseptr  = Data[0]
  StackInit(Emitter.Stack, Emitter.UsedStackSize);

  CallStack.Init(MAX_RECURSION_DEPTH);
  ArgStack.Init(4096);
  TryStack.Init();

  RecursionDepth := 0;
  ProgramStart   := StartPos;

  IsThread := False;

  // XXX: This should probably happen as emitter's final step.
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
    // iterate FORWARD so each parent is fully resolved before its children
    for i:=1 to ClassVMTs.High do
    begin
      if ClassVMTs.Data[i].ParentID < 0 then
        continue;

      PMethods := ClassVMTs.Data[ClassVMTs.Data[i].ParentID].Methods;
      for j:=0 to High(PMethods) do
        if (ClassVMTs.Data[i].Methods[j] = -1) and (PMethods[j] <> -1) then
          ClassVMTs.Data[i].Methods[j] := PMethods[j];
    end;
  end;

  GlobalBase := @Data[0];
end;

constructor TInterpreter.NewForThread(MainInterp: TInterpreter; EntryPC: Int32; BCSize: Int32; ThreadStackSize: SizeInt = 64 * 1024);
begin
  // Fresh small stack
  SetLength(Data, ThreadStackSize);
  //FillByte(Data[0], ThreadStackSize, 0);

  // BasePtr points into the MAIN thread's stack for global access
  // StackPtr starts at zero offset — no globals owned here
  BasePtr  := MainInterp.BasePtr;      // locals live here in main thread
  StackPtr := @Data[0];                // thread-local stack starts fresh
  GlobalBase := MainInterp.GlobalBase; // Global variables

  CallStack.Init(32);
  TryStack.Init(32);
  ArgStack.Init(32);

  RecursionDepth := 0;
  RunCode := VM_EXCEPTION;

  ProgramStart := EntryPC;
  ProgramBase  := MainInterp.ProgramBase;
  ProgramCounter := EntryPC;

  JumpTable      := MainInterp.JumpTable;   // read-only, safe to share
  hot_condition  := MainInterp.hot_condition;
  HasCreatedJIT  := MainInterp.HasCreatedJIT;
  CurrentException := MainInterp.CurrentException;
  NativeException  := MainInterp.NativeException;

  IsThread       := True;

  // Push a sentinel call frame
  CallStack.Push(nil, StackPtr, BasePtr, ProgramStart);
end;

procedure TInterpreter.Free(var BC: TBytecode);
begin
  if Self.CurrentException <> nil then
  begin
    Self.DecRef(Self.CurrentException, xtClass);
    Self.CurrentException := nil;
  end;

  {$IFNDEF xpr_DisableJIT}
  Self.FreeJIT(BC);
  {$ENDIF}
end;

{$IFNDEF xpr_DisableJIT}
{$I interpreter.jitcode.inc}
{$ENDIF}

procedure TInterpreter.StackInit(Stack: TStackArray; StackPos: SizeInt);
begin
  SetLength(Data, STACK_SIZE);
  Move(stack[0], data[0], Min(STACK_SIZE, Length(Stack)));

  BasePtr  := @Data[0];
  StackPtr := @Data[0] + StackPos;
end;


// ============================================================================
// State tracking and basic memory access
// ============================================================================

function TInterpreter.GetProgramCounter(): Int32;
begin
  Result := (PtrUInt(Self.ProgramRawLocation)-PtrUInt(Self.ProgramBase)) div SizeOf(TBytecodeInstruction);
end;

procedure TInterpreter.SetProgramCounter(pc: Int32);
begin
  Self.ProgramRawLocation := Pointer(PtrUInt(Self.ProgramBase) + pc * SizeOf(TBytecodeInstruction));
end;

function TInterpreter.Global(offset: PtrUInt): Pointer;
begin
  Result := GlobalBase + offset;
end;

function TInterpreter.AsString(): string;
var i,i32: Int32; i64: Int64; p: PtrInt;
begin
  Result := '';
  // Debug dumping code, mostly disabled these days unless things get real bad
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


// =============================================================================
// Memory Management & Refcounting
// We need to make sure arrays and strings are handled carefully here so they
// don't leak or blow up. FPC's built-in tools help a lot.
// =============================================================================

procedure TInterpreter.IncRef(Left: Pointer; BaseType: EExpressBaseType);
var rc: SizeInt;
begin
  (*
    Note: strings in FPC are VERY different from dynarray, so special care must
    be taken at all times.
  *)
  if Left = nil then Exit;

  case BaseType of
    xtAnsiString:
      fpc_ansistr_incr_ref(Left);

    xtUnicodeString:
      fpc_unicodestr_incr_ref(Left);

    xtArray:
      fpc_dynarray_incr_ref(Left);

    xtClass:
      begin
        {$IFDEF CPU64}
        rc := InterlockedIncrement64(PInt64(Left - 2*SizeOf(SizeInt))^);
        {$ELSE}
        rc := InterlockedIncrement(PLongInt(Left - 2*SizeOf(SizeInt))^);
        {$ENDIF}
        if rc <= 0 then
        begin
          FreeMem(Pointer(PtrUInt(Left) - 3*SizeOf(SizeInt)));
          Left := nil;
        end;
      end;
  end;
end;

procedure TInterpreter.DecRef(var Left: Pointer; BaseType: EExpressBaseType);
var rc: SizeInt;
begin
  if Left = nil then Exit;
  case BaseType of
    xtAnsiString:
      fpc_ansistr_decr_ref(Left);

    xtUnicodeString:
      fpc_unicodestr_decr_ref(Left);

    xtArray:
      fpc_dynarray_decr_ref(Left, nil);

    xtClass:
      begin
        {$IFDEF CPU64}
        rc := InterlockedDecrement64(PInt64(Left - 2*SizeOf(SizeInt))^);
        {$ELSE}
        rc := InterlockedDecrement(PLongInt(Left - 2*SizeOf(SizeInt))^);
        {$ENDIF}
        if rc <= 0 then
        begin
          FreeMem(Pointer(PtrUInt(Left) - 3*SizeOf(SizeInt)));
          Left := nil;
        end;
      end;
  end;
end;

procedure TInterpreter.ArrayRefcount(var Left: Pointer; Right: Pointer; BaseType: EExpressBaseType);
begin
  { Pointer identity check: if Left and Right already point to the same
    allocation, nothing changes. This covers both the self-assignment case
    (a[i] := a[i]) and nil := nil. }
  if Left = Right then Exit;

  { IncRef the incoming value first. This order matters: if an exception
    occurs between IncRef and DecRef (theoretically), the new value is
    protected. More importantly, for the case Right=nil, we skip IncRef
    and only DecRef the outgoing value. }
  if Right <> nil then
    IncRef(Right, BaseType);

  if Left <> nil then
    DecRef(Left, BaseType);

  { DecRef zeroes Left if it freed the allocation. For the non-freed case
    (rc > 1 after decrement), Left still holds the old value — but that's
    fine because the bcMOV/store that follows ArrayRefcount will overwrite
    the slot with Right immediately. }
end;


// =============================================================================
// OOP, Classes, Casting and VMTs
// Dynamic class casting and VMT lookups for method resolution
// =============================================================================

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

procedure TInterpreter.DynCastOpcode(ClassVMTs: TVMTList; const pc: PBytecodeInstruction);
var
  DestAddr: PtrInt;
  SourcePtr: Pointer;
  ActualClassID, TargetClassID, VMTIndex: Integer;
begin
  // Args from Instruction^: [DestVar], [SourceVar], [TargetClassID]
  DestAddr      := PtrInt(MEMBASE_0);
  SourcePtr     := PPointer(MEMBASE_1)^;
  TargetClassID := pc^.Args[2].Data.i32;

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
    IncRef(SourcePtr, xtClass); // produces a new reference
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
  SelfPtr := Pointer(SelfPtr^);

  // Safety check: calling a method on a nil object.
  if SelfPtr = nil then
    raise RuntimeError.Create('Access violation: method call on a nil object');

  VMTIndex := SizeInt(((SelfPtr-SizeOf(Pointer)*3))^);
  if (VMTIndex < 0) or (VMTIndex > ClassVMTs.High()) then
    raise RuntimeError.Create('Invalid Class ID found in object: '+IntTostr(VMTIndex));

  Result := ClassVMTs.Data[VMTIndex].Methods[MethodIndex];
  if Result = -1 then
    raise RuntimeError.Create('Abstract method called or invalid VMT');
end;


// =============================================================================
// External calls, FFI and Closures
// Bridging the gap between our VM and the outside world
// =============================================================================

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

procedure TInterpreter.PushClosure(ClosureRec: Pointer);
type TClosureRec = packed record Func: Pointer; Size: SizeInt; Refs: array of Pointer; end;
var
  i: Int32;
begin
  for i:=0 to High(TClosureRec(ClosureRec^).Refs) do
    Self.ArgStack.Push(TClosureRec(ClosureRec^).Refs[i]);
end;

procedure TInterpreter.TransferArgsFromInterp(var Src: TInterpreter; ArgCount: Int32);
var
  i: Int32;
  SrcPtr: Pointer;
begin
  for i := 0 to ArgCount - 1 do
  begin
    SrcPtr := Src.ArgStack.Data[Src.ArgStack.Count - ArgCount + i];
    Move(SrcPtr^, StackPtr^, 8);
    ArgStack.Data[i] := StackPtr;
    StackPtr += 8;
  end;
  ArgStack.Count := ArgCount;
  Src.ArgStack.Count -= ArgCount;
end;

function TInterpreter.CreateFFICallback(pc: PBytecodeInstruction; Lambda, CallbackType: Pointer): Pointer;
var
  left, right: Pointer;
begin
  Left  := Pointer(MEMBASE_0);  // pointer TO closure record
  Right := PXprCallbackTypeInfo(pc^.Args[1].Data.Addr);

  // Pass the Lambda record 'Left' directly!
  Right := XprCreateClosureFromTypeInfo(Left, Right);

  if Right <> nil then
  begin
    XprRegisterClosure(Right);
    Result := PXprClosureData(Right)^.FFIFuncPtr;
  end else
    Result := nil;
end;


// =============================================================================
// Exceptions & Stack Traces
// Handlers for when things go terribly wrong
// =============================================================================

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
  Result += Format('  at  %s (%s) [pc=%d]', [FuncName, LineInfo, Self.ProgramCounter]) + LineEnding;

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
  if Frame.ReturnAddress <> nil then
  begin
    FuncName := '';
    CallSitePC := (PtrUInt(Frame.ReturnAddress) - PtrUInt(@BC.Code.Data[0])) div SizeOf(TBytecodeInstruction);
    LineInfo := BC.Docpos.Data[CallSitePC].ToString();

    Result += Format('  from (%s) [pc=%d]', [LineInfo, CallSitePC]) + LineEnding;
  end;
end;

procedure TInterpreter.TranslateNativeException(const FpcException: Exception; ToExceptionClass: Pointer);
begin
  if ToExceptionClass = nil then Exit;

  PAnsiString(ToExceptionClass)^ := FpcException.Message;
  if Self.CurrentException <> ToExceptionClass then
  begin
    if Self.CurrentException <> nil then
      Self.DecRef(Self.CurrentException, xtClass);

    Self.CurrentException := ToExceptionClass;
    Self.IncRef(Self.CurrentException, xtClass);
  end;
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


// =============================================================================
// Opcode Helpers
// Minor handlers pulled out to keep the main run loop slightly cleaner
// =============================================================================

//XXX: strings store len, delphi everything is len, keep in mind
//     FPC stores .High for dynarray, .Len for strings
//     Pascal world is a mess of compatibility layers, no uniform agreement.
function TInterpreter.BoundsCheck(pc: PBytecodeInstruction): Int32;
var
  Arr: Pointer;
  Index: PtrInt;
begin
  Result := 0;
  arr := PPointer(MEMBASE_0)^;

  if arr = nil then
  begin
    Self.CurrentException := PPointer(MEMBASE_2)^;
    Self.WriteExceptionStr(Self.CurrentException, 'Out of range, array is empty!');
    Exit(1);
  end;

  case BaseIntType(pc^.Args[1].BaseType) of
    xtInt8:   Index := PInt8(Pointer(MEMBASE_1))^;
    xtInt16:  Index := PInt16(Pointer(MEMBASE_1))^;
    xtInt32:  Index := PInt32(Pointer(MEMBASE_1))^;
    xtInt64:  Index := PInt64(Pointer(MEMBASE_1))^;
    xtUInt8:  Index := PUInt8(Pointer(MEMBASE_1))^;
    xtUInt16: Index := PUInt16(Pointer(MEMBASE_1))^;
    xtUInt32: Index := PUInt32(Pointer(MEMBASE_1))^;
    xtUInt64: Index := PUInt64(Pointer(MEMBASE_1))^;
    else
      raise Exception.Create('Impossible illegal index-operand');
  end;

  if Index > TArrayRec((arr-SizeOf(SizeInt)*2)^).High then
  begin
    Self.CurrentException := PPointer(MEMBASE_2)^;
    Self.WriteExceptionStr(Self.CurrentException, Format('Out of range: Index=%d for Array[0..%d]', [Index, TArrayRec((Arr-SizeOf(SizeInt)*2)^).High]));
    Exit(1);
  end;
end;

procedure TInterpreter.HandleMOV(pc: PBytecodeInstruction);
begin
  // No base types should be handled here, this is assignment between equal datasizes
  // Again: MEMBASE_0 and MEMBASE_1 must be same size, or MEMBASE_0 larger than MEMBASE_1.
  // Third argument is the datasize.
  case pc^.Args[0].BaseType of
    xtDouble:
      case pc^.Args[1].BaseType of
         xtInt8:   PFloat64(MEMBASE_0)^ := PInt8(MEMBASE_1)^;
         xtUInt8:  PFloat64(MEMBASE_0)^ := PUInt8(MEMBASE_1)^;
         xtInt16:  PFloat64(MEMBASE_0)^ := PInt16(MEMBASE_1)^;
         xtUInt16: PFloat64(MEMBASE_0)^ := PUInt16(MEMBASE_1)^;
       end;
    xtSingle:
       case pc^.Args[1].BaseType of
         xtInt8:   PFloat32(MEMBASE_0)^ := PInt8(MEMBASE_1)^;
         xtUInt8:  PFloat32(MEMBASE_0)^ := PUInt8(MEMBASE_1)^;
         xtInt16:  PFloat32(MEMBASE_0)^ := PInt16(MEMBASE_1)^;
         xtUInt16: PFloat32(MEMBASE_0)^ := PUInt16(MEMBASE_1)^;
       end;
    else
      Move(MEMBASE_1^, MEMBASE_0^, pc^.Args[2].Data.i32);
  end;
end;

procedure TInterpreter.HandleSTORE(pc: PBytecodeInstruction);
begin
  // No base types should be handled here, this is assignment between equal datasizes
  // Again: MEMBASE_0 and MEMBASE_1 must be same size, or MEMBASE_0 larger than MEMBASE_1.
  // Third argument is the datasize.
  case pc^.Args[0].BaseType of
    xtDouble:
      case pc^.Args[1].BaseType of
         xtInt8:   PFloat64(Pointer(MEMBASE_0)^)^ := PInt8(MEMBASE_1)^;
         xtUInt8:  PFloat64(Pointer(MEMBASE_0)^)^ := PUInt8(MEMBASE_1)^;
         xtInt16:  PFloat64(Pointer(MEMBASE_0)^)^ := PInt16(MEMBASE_1)^;
         xtUInt16: PFloat64(Pointer(MEMBASE_0)^)^ := PUInt16(MEMBASE_1)^;
       end;
    xtSingle:
       case pc^.Args[1].BaseType of
         xtInt8:   PFloat32(Pointer(MEMBASE_0)^)^ := PInt8(MEMBASE_1)^;
         xtUInt8:  PFloat32(Pointer(MEMBASE_0)^)^ := PUInt8(MEMBASE_1)^;
         xtInt16:  PFloat32(Pointer(MEMBASE_0)^)^ := PInt16(MEMBASE_1)^;
         xtUInt16: PFloat32(Pointer(MEMBASE_0)^)^ := PUInt16(MEMBASE_1)^;
       end;
    else
      Move(MEMBASE_1^, Pointer(MEMBASE_0^)^, pc^.Args[2].Data.i32);
  end;
end;

procedure TInterpreter.HandleBinary(pc: PBytecodeInstruction);
begin
  // Type dispatch
  case BaseIntType(pc^.Args[0].BaseType) of
    xtInt8:
      case pc^.Code of
        bcADD: PInt8(MEMBASE_2)^ := PInt8(MEMBASE_0)^ + PInt8(MEMBASE_1)^;
        bcSUB: PInt8(MEMBASE_2)^ := PInt8(MEMBASE_0)^ - PInt8(MEMBASE_1)^;
        bcMUL: PInt8(MEMBASE_2)^ := PInt8(MEMBASE_0)^ * PInt8(MEMBASE_1)^;
        bcDIV: PInt8(MEMBASE_2)^ := PInt8(MEMBASE_0)^ div PInt8(MEMBASE_1)^;
        bcMOD: PInt8(MEMBASE_2)^ := PInt8(MEMBASE_0)^ mod PInt8(MEMBASE_1)^;
        bcPOW: PInt8(MEMBASE_2)^ := Power(PInt8(MEMBASE_0)^, PInt8(MEMBASE_1)^);

        bcEQ:  PBoolean(MEMBASE_2)^ := PInt8(MEMBASE_0)^ = PInt8(MEMBASE_1)^;
        bcNE:  PBoolean(MEMBASE_2)^ := PInt8(MEMBASE_0)^ <> PInt8(MEMBASE_1)^;
        bcGT:  PBoolean(MEMBASE_2)^ := PInt8(MEMBASE_0)^ > PInt8(MEMBASE_1)^;
        bcLT:  PBoolean(MEMBASE_2)^ := PInt8(MEMBASE_0)^ < PInt8(MEMBASE_1)^;
        bcGTE: PBoolean(MEMBASE_2)^ := PInt8(MEMBASE_0)^ >= PInt8(MEMBASE_1)^;
        bcLTE: PBoolean(MEMBASE_2)^ := PInt8(MEMBASE_0)^ <= PInt8(MEMBASE_1)^;

        bcBND: PInt8(MEMBASE_2)^ := PInt8(MEMBASE_0)^ and PInt8(MEMBASE_1)^;
        bcBOR: PInt8(MEMBASE_2)^ := PInt8(MEMBASE_0)^ or  PInt8(MEMBASE_1)^;
        bcXOR: PInt8(MEMBASE_2)^ := PInt8(MEMBASE_0)^ xor PInt8(MEMBASE_1)^;
        bcSHR: PInt8(MEMBASE_2)^ := PInt8(MEMBASE_0)^ shr PInt8(MEMBASE_1)^;
        bcSHL: PInt8(MEMBASE_2)^ := PInt8(MEMBASE_0)^ shl PInt8(MEMBASE_1)^;
        bcSAR: PInt8(MEMBASE_2)^ := Sar(PInt8(MEMBASE_0)^, PInt8(MEMBASE_1)^);
      end;

    xtUInt8:
      case pc^.Code of
        bcADD: PUInt8(MEMBASE_2)^ := PUInt8(MEMBASE_0)^ + PUInt8(MEMBASE_1)^;
        bcSUB: PUInt8(MEMBASE_2)^ := PUInt8(MEMBASE_0)^ - PUInt8(MEMBASE_1)^;
        bcMUL: PUInt8(MEMBASE_2)^ := PUInt8(MEMBASE_0)^ * PUInt8(MEMBASE_1)^;
        bcDIV: PUInt8(MEMBASE_2)^ := PUInt8(MEMBASE_0)^ div PUInt8(MEMBASE_1)^;
        bcMOD: PUInt8(MEMBASE_2)^ := PUInt8(MEMBASE_0)^ mod PUInt8(MEMBASE_1)^;
        bcPOW: PUInt8(MEMBASE_2)^ := Power(PUInt8(MEMBASE_0)^, PUInt8(MEMBASE_1)^);
        bcEQ:  PBoolean(MEMBASE_2)^ := PUInt8(MEMBASE_0)^ = PUInt8(MEMBASE_1)^;
        bcNE:  PBoolean(MEMBASE_2)^ := PUInt8(MEMBASE_0)^ <> PUInt8(MEMBASE_1)^;
        bcGT:  PBoolean(MEMBASE_2)^ := PUInt8(MEMBASE_0)^ > PUInt8(MEMBASE_1)^;
        bcLT:  PBoolean(MEMBASE_2)^ := PUInt8(MEMBASE_0)^ < PUInt8(MEMBASE_1)^;
        bcGTE: PBoolean(MEMBASE_2)^ := PUInt8(MEMBASE_0)^ >= PUInt8(MEMBASE_1)^;
        bcLTE: PBoolean(MEMBASE_2)^ := PUInt8(MEMBASE_0)^ <= PUInt8(MEMBASE_1)^;
        bcBND: PUInt8(MEMBASE_2)^ := PUInt8(MEMBASE_0)^ and PUInt8(MEMBASE_1)^;
        bcBOR: PUInt8(MEMBASE_2)^ := PUInt8(MEMBASE_0)^ or  PUInt8(MEMBASE_1)^;
        bcXOR: PUInt8(MEMBASE_2)^ := PUInt8(MEMBASE_0)^ xor PUInt8(MEMBASE_1)^;
        bcSHR: PUInt8(MEMBASE_2)^ := PUInt8(MEMBASE_0)^ shr PUInt8(MEMBASE_1)^;
        bcSHL: PUInt8(MEMBASE_2)^ := PUInt8(MEMBASE_0)^ shl PUInt8(MEMBASE_1)^;
        bcSAR: PUInt8(MEMBASE_2)^ := Sar(PUInt8(MEMBASE_0)^, PUInt8(MEMBASE_1)^);
      end;

    xtInt16:
      case pc^.Code of
        bcADD: PInt16(MEMBASE_2)^ := PInt16(MEMBASE_0)^ + PInt16(MEMBASE_1)^;
        bcSUB: PInt16(MEMBASE_2)^ := PInt16(MEMBASE_0)^ - PInt16(MEMBASE_1)^;
        bcMUL: PInt16(MEMBASE_2)^ := PInt16(MEMBASE_0)^ * PInt16(MEMBASE_1)^;
        bcDIV: PInt16(MEMBASE_2)^ := PInt16(MEMBASE_0)^ div PInt16(MEMBASE_1)^;
        bcMOD: PInt16(MEMBASE_2)^ := PInt16(MEMBASE_0)^ mod PInt16(MEMBASE_1)^;
        bcPOW: PInt16(MEMBASE_2)^ := Power(PInt16(MEMBASE_0)^, PInt16(MEMBASE_1)^);
        bcEQ:  PBoolean(MEMBASE_2)^ := PInt16(MEMBASE_0)^ = PInt16(MEMBASE_1)^;
        bcNE:  PBoolean(MEMBASE_2)^ := PInt16(MEMBASE_0)^ <> PInt16(MEMBASE_1)^;
        bcGT:  PBoolean(MEMBASE_2)^ := PInt16(MEMBASE_0)^ > PInt16(MEMBASE_1)^;
        bcLT:  PBoolean(MEMBASE_2)^ := PInt16(MEMBASE_0)^ < PInt16(MEMBASE_1)^;
        bcGTE: PBoolean(MEMBASE_2)^ := PInt16(MEMBASE_0)^ >= PInt16(MEMBASE_1)^;
        bcLTE: PBoolean(MEMBASE_2)^ := PInt16(MEMBASE_0)^ <= PInt16(MEMBASE_1)^;
        bcBND: PInt16(MEMBASE_2)^ := PInt16(MEMBASE_0)^ and PInt16(MEMBASE_1)^;
        bcBOR: PInt16(MEMBASE_2)^ := PInt16(MEMBASE_0)^ or  PInt16(MEMBASE_1)^;
        bcXOR: PInt16(MEMBASE_2)^ := PInt16(MEMBASE_0)^ xor PInt16(MEMBASE_1)^;
        bcSHR: PInt16(MEMBASE_2)^ := PInt16(MEMBASE_0)^ shr PInt16(MEMBASE_1)^;
        bcSHL: PInt16(MEMBASE_2)^ := PInt16(MEMBASE_0)^ shl PInt16(MEMBASE_1)^;
        bcSAR: PInt16(MEMBASE_2)^ := Sar(PInt16(MEMBASE_0)^, PInt16(MEMBASE_1)^);
      end;

    xtUInt16:
      case pc^.Code of
        bcADD: PUInt16(MEMBASE_2)^ := PUInt16(MEMBASE_0)^ + PUInt16(MEMBASE_1)^;
        bcSUB: PUInt16(MEMBASE_2)^ := PUInt16(MEMBASE_0)^ - PUInt16(MEMBASE_1)^;
        bcMUL: PUInt16(MEMBASE_2)^ := PUInt16(MEMBASE_0)^ * PUInt16(MEMBASE_1)^;
        bcDIV: PUInt16(MEMBASE_2)^ := PUInt16(MEMBASE_0)^ div PUInt16(MEMBASE_1)^;
        bcMOD: PUInt16(MEMBASE_2)^ := PUInt16(MEMBASE_0)^ mod PUInt16(MEMBASE_1)^;
        bcPOW: PUInt16(MEMBASE_2)^ := Power(PUInt16(MEMBASE_0)^, PUInt16(MEMBASE_1)^);
        bcEQ:  PBoolean(MEMBASE_2)^ := PUInt16(MEMBASE_0)^ = PUInt16(MEMBASE_1)^;
        bcNE:  PBoolean(MEMBASE_2)^ := PUInt16(MEMBASE_0)^ <> PUInt16(MEMBASE_1)^;
        bcGT:  PBoolean(MEMBASE_2)^ := PUInt16(MEMBASE_0)^ > PUInt16(MEMBASE_1)^;
        bcLT:  PBoolean(MEMBASE_2)^ := PUInt16(MEMBASE_0)^ < PUInt16(MEMBASE_1)^;
        bcGTE: PBoolean(MEMBASE_2)^ := PUInt16(MEMBASE_0)^ >= PUInt16(MEMBASE_1)^;
        bcLTE: PBoolean(MEMBASE_2)^ := PUInt16(MEMBASE_0)^ <= PUInt16(MEMBASE_1)^;
        bcBND: PUInt16(MEMBASE_2)^ := PUInt16(MEMBASE_0)^ and PUInt16(MEMBASE_1)^;
        bcBOR: PUInt16(MEMBASE_2)^ := PUInt16(MEMBASE_0)^ or  PUInt16(MEMBASE_1)^;
        bcXOR: PUInt16(MEMBASE_2)^ := PUInt16(MEMBASE_0)^ xor PUInt16(MEMBASE_1)^;
        bcSHR: PUInt16(MEMBASE_2)^ := PUInt16(MEMBASE_0)^ shr PUInt16(MEMBASE_1)^;
        bcSHL: PUInt16(MEMBASE_2)^ := PUInt16(MEMBASE_0)^ shl PUInt16(MEMBASE_1)^;
        bcSAR: PUInt16(MEMBASE_2)^ := Sar(PUInt16(MEMBASE_0)^, PUInt16(MEMBASE_1)^);
      end;
  end;
end;

procedure TInterpreter.RunThreadOpcode(pc: PBytecodeInstruction; var bc: TBytecode);
  procedure RunFromLambda();
  var
    Lambda, argsArray: Pointer;
    captureCount: SizeInt;
    TD: ^TThreadData;
    Handle: TThreadID;
    ci: Int32;
  begin
    Lambda := Pointer(MEMBASE_0);

    TD         := AllocMem(SizeOf(TThreadData));
    TD^.Interp := AllocMem(SizeOf(TInterpreter));
    TD^.Interp^ := TInterpreter.NewForThread(Self, PtrInt(Lambda^), BC.Code.Size);
    TD^.BC     := @BC;

    // Push closure captures onto main ArgStack so TransferArgsFromInterp
    // can move them to the thread stack.
    // Closure record layout: [method: PtrInt][size: Int64][args: dynarray ptr]
    captureCount := PSizeInt(PByte(Lambda) + SizeOf(PtrInt))^;
    argsArray    := PPointer(PByte(Lambda) + SizeOf(PtrInt) + SizeOf(SizeInt))^;
    if (captureCount > 0) and (argsArray <> nil) then
      for ci := 0 to captureCount - 1 do
        ArgStack.Push(PPointerArray(argsArray)^[ci]);

    // Transfer explicit args + captures, total must match lambda's full param count
    TD^.Interp^.TransferArgsFromInterp(Self, pc^.Args[2].Data.u16 + captureCount);

    Handle := BeginThread(@XprThreadEntry, TD);
    PtrInt(Pointer(MEMBASE_1)^) := PtrInt(Handle);
  end;

  procedure RunFromFunc();
  var
    Method: Pointer;
    TD: ^TThreadData;
    Handle: TThreadID;
  begin
    Method := Pointer(MEMBASE_0);

    TD         := AllocMem(SizeOf(TThreadData));
    TD^.Interp := AllocMem(SizeOf(TInterpreter));
    TD^.Interp^ := TInterpreter.NewForThread(Self, PtrInt(Method^), BC.Code.Size);
    TD^.BC     := @BC;

    // Transfer explicit args + captures, total must match method's full param count
    TD^.Interp^.TransferArgsFromInterp(Self, pc^.Args[2].Data.u16);

    Handle := BeginThread(@XprThreadEntry, TD);
    PtrInt(Pointer(MEMBASE_1)^) := PtrInt(Handle);
  end;
begin
  if pc^.Args[0].BaseType = xtMethod then
    RunFromFunc()
  else
    RunFromLambda();
end;

procedure TInterpreter.NewClassOpcode(pc: PBytecodeInstruction; var bc: TBytecode);
var left: Pointer;
begin
  left := AllocMem(pc^.Args[2].Data.i32);

  // VMT index
  SizeInt(Pointer(left)^) := pc^.Args[1].Data.i32;
  Inc(left, SizeOf(SizeInt));

  // Refcount: starts at 1. The result slot returned to the caller
  // is the initial owner. ManageMemory will IncRef for named var
  // assignment; statement-boundary cleanup will Collect the temp
  // if the result is never assigned. Either path is correct with rc=1.
  SizeInt(Pointer(left)^) := 1;
  Inc(left, SizeOf(SizeInt));

  // Object size
  SizeInt(Pointer(left)^) := pc^.Args[2].Data.i32;
  Inc(left, SizeOf(SizeInt));

  PPointer(MEMBASE_0)^ := left;
end;


// =============================================================================
// Core Execution Loop
// The engine room. We try to keep things super lean here.
// =============================================================================

function TInterpreter.CallFunction(BC: TBytecode; Location: PtrUInt; Args: array of Pointer): Int32;
var
  i: Int32;
  OldMask: TFPUExceptionMask;
begin
  OldMask := GetExceptionMask;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

  Self.ProgramStart  := Location + 1;
  Self.ProgramCounter:= Self.ProgramStart;
//Self.IsThread      := True; // <-- If we use RunSafe
  Self.HasCreatedJIT := True; // XXX No JIT for now. Setup cost and mutates BC

  // set args
  for i:=0 to High(Args) do Self.ArgStack.Push(Args[i]);

  // run unsafe
  Self.Run(BC);
  Result := Self.RunCode;
  SetExceptionMask(OldMask);
end;

procedure TInterpreter.RunSafe(var BC: TBytecode; ResetExceptions:Boolean=True);
var
  TryFrame: TCallFrame;
  IsNativeException, IsFatal, WasSoftStop: Boolean;
  FinalExceptionPC: Int32;
  ExceptionStack: specialize TArrayList<Int32>;

  function UnhandledException(): Boolean;
  begin
    Result := False;
    if TryStack.Top < 0 then
    begin
      if Self.RunCode = VM_SOFT_STOP then
      begin
        Self.RunCode := VM_HALTED; // Clean stop achieved
        Exit(True);
      end;

      WriteLn('Fatal: ', Self.GetCurrentExceptionString());
      Writeln('RuntimeError: ', BC.Docpos.Data[ProgramCounter].ToString() + ' - Code:', BC.Code.Data[ProgramCounter].Code, ', pc: ', ProgramCounter);
      Writeln();
      WriteLn(Self.BuildStackTraceString(BC));

      // release object
      DecRef(Self.CurrentException, xtClass);
      Self.CurrentException := nil;

      Self.RunCode := VM_HALTED; // Hard stop the VM cleanly
      Exit(True);
    end;

    TryFrame := TryStack.Pop();
    ExceptionStack.Insert(ProgramCounter, 0);
    if ExceptionStack.Size > 12 then
      ExceptionStack.Pop();

    if TryStack.Top = -1 then
      IsFatal := True;

    // Unwind the callstack, pop all frames one by one, stop the script cleanly.
    if Self.RunCode = VM_SOFT_STOP then
    begin
      WasSoftStop := True;
      // Skip user-defined try..except blocks, keep popping until we find a function cleanup frame
      while (BC.Code.Data[PtrUInt(TryFrame.ReturnAddress)].Code = bcGET_EXCEPTION) do
      begin
        if TryStack.Top < 0 then
        begin
          Self.RunCode := VM_HALTED;
          Exit(True);
        end;
        TryFrame := TryStack.Pop();
      end;
    end;

    StackPtr := TryFrame.StackPtr;
    BasePtr  := TryFrame.FrameBase;
    ProgramCounter := PtrUInt(TryFrame.ReturnAddress);
  end;
var
  OldMask: TFPUExceptionMask;
  ei: Int32;
const
  E_AT: string   = '  at ';
  E_FROM: string = 'from ';
begin
  XprSetCurrentContext(Self, BC);
  ExceptionStack.Init([]);
  IsFatal := False;
  WasSoftStop := False;

  //as per IEEE 754
  OldMask := GetExceptionMask;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

  Self.ProgramBase := @BC.Code.Data[0];
  if IsThread then
    Self.ProgramCounter := Self.ProgramStart
  else
    Self.ProgramCounter := 0;

  if ResetExceptions then
  begin
    Self.CurrentException := nil;
    Self.NativeException  := nil;
  end;

  // Run loop with exception catch wrappers
  repeat
    try
      Self.Run(BC);

      if Self.RunCode in [VM_RUNNING, VM_HALTED] then
        Break
      else if (Self.RunCode in [VM_EXCEPTION, VM_SOFT_STOP]) and UnhandledException() then
        Break;
    except
      on E: Exception do // Catches BOTH native FPC errors and our VM raise
      begin
        if Self.RunCode = VM_RUNNING then Self.RunCode := VM_EXCEPTION; // Native crash

        IsNativeException := (Self.CurrentException = nil);
        if IsNativeException then
          TranslateNativeException(E, Self.NativeException);

        if UnhandledException() then
        begin
          if Self.RunCode = VM_EXCEPTION then
          begin
            WriteLn('=== RunSafe Fatal Crashlog =================');
            Writeln('>> ', E.ToString);
            DumpExceptionBacktrace(Output);
            WriteLn('============================================');
          end;
          Break;
        end;
      end;
    end;
  until False;

  // All try-stacks are exhausted, shows unhandled failure
  if (not WasSoftStop) and (IsFatal and (ExceptionStack.Size <> 0)) then
  begin
    Writeln('=== Fatal RuntimeError ====================');
    WriteLn(Self.GetCurrentExceptionString());
    ei := 0;
    while(ExceptionStack.Size > 0) do
    begin
      FinalExceptionPC := ExceptionStack.Pop();
      if ei > 0 then Write(E_AT) else Write(E_FROM);
      Writeln('(',BC.Docpos.Data[FinalExceptionPC].ToString() +') Code: ', BC.Code.Data[FinalExceptionPC].Code, ' [pc=',FinalExceptionPC,']');
      Inc(ei);
    end;
    Writeln();
    Writeln('=== Stacktrace ============================');
    WriteLn(Self.BuildStackTraceString(BC));

    // -------------------------------------------------
    // Any leftover exception? release
    // This feels a little bit risky
    if (Self.NativeException <> nil) then
      Self.DecRef(Self.NativeException, xtClass);
    Self.NativeException := nil;

    if (Self.CurrentException <> nil) then
      Self.DecRef(Self.CurrentException, xtClass);
    Self.CurrentException := nil;
  end;

  SetExceptionMask(oldMask);
end;


(* =============================================================================
   Binary and complex instruction actually special path into
   > if dest = mpLocal then BasePtr + addr...
   > else                   MemBase[...] + addr

   Where-as the very small instructions always use the MemBases indirection,
   this is per design through experimentation, small instructions may fit
   better into a single instruction cacheline, that's my hypothesis.
   We cant beat that.

   While for large cases dest = mpLocal yields near fallthrough performance
   under decent branch predictors, no indirection, solving the dest reliably
   first so a location to write to exists. I suppose.
   ========================================================================== *)
procedure TInterpreter.Run(var BC: TBytecode);
var
  {WARNING: DONT ADD MORE GPR LOCAL VARIABLES}
  {         FPC'S OPTIMIZER STRUGGLES}
  {         NOT EVEN A RETURN VALUE}
  pc: PBytecodeInstruction;
  left: Pointer;
  frame: TCallFrame;

{$IFNDEF xpr_DisableJIT}
{$i interpreter.super.labels.inc}
{$ENDIF}
label WHILE_CASE_ENTER, WHILE_CASE_EXIT;
begin
  // Initialize MemBases shared across this TInterpreter instance
  // JIT needs this, solve before JIT builds

  // Constants cannot be empty - Will always have some data ARRAYLIST_MIN = 32
  MemBases[mpConst]  := PByte(@BC.Constants.Data[0]);
  MemBases[mpGlobal] := GlobalBase;
  MemBases[mpLocal]  := BasePtr;
  MemBases[mpHeap]   := nil;

  {$IFNDEF xpr_DisableJIT}
  if (not Self.HasCreatedJIT) then
  begin
    {$i interpreter.super.bc2lb.inc}
    {$IFDEF CPUX86_64}
    x86_64_Compile(BC, True);  // capture loops
    x86_64_Compile(BC, False); // capture linear
    {$ENDIF}
    Self.GenerateSuperInstructions(BC, JumpTable);
    Self.HasCreatedJIT := True;
    {$IFDEF VERBOSE}
    WriteLn('JIT compilation successful');
    {$ENDIF}
  end;
  {$ENDIF}

  // Set up the PC and start blasting
  pc := @BC.Code.Data[ProgramCounter];

  // reset as VM_RUNNING from whatever state was.
  Self.RunCode := VM_RUNNING;

  WHILE_CASE_ENTER:
  while pc <> nil do
  begin
    case pc^.Code of
      // all jumpcodes need to check if we can actually do continue
      // Backward absolute jump = potential infinite loop's.
      bcJMP:
        begin
          pc := @BC.Code.Data[pc^.Args[0].Data.i32];
          if (Self.RunCode <> VM_RUNNING) and (PtrUInt(pc) < PtrUInt(Self.ProgramRawLocation)) then
          begin
            pc := nil;
            continue;
          end;
        end;

      bcRELJMP:
          if (pc^.Args[0].Data.i32 < 0) and (Self.RunCode <> VM_RUNNING) then
          begin
            pc := nil;
            continue;
          end else
            Inc(pc, pc^.Args[0].Data.i32);

      bcJZ:
        if (not PBoolean(MEMBASE_0)^) then
          if (pc^.Args[1].Data.i32 < 0) and (Self.RunCode <> VM_RUNNING) then
          begin
            pc := nil;
            continue;
          end else
            Inc(pc, pc^.Args[1].Data.i32);

      bcJNZ:
        if PBoolean(MEMBASE_0)^ then
          if (pc^.Args[1].Data.i32 < 0) and (Self.RunCode <> VM_RUNNING) then
          begin
            pc := nil;
            continue;
          end else
            Inc(pc, pc^.Args[1].Data.i32);

      bcINC_i32: Inc( PInt32(BASEPTR_0)^); // can only be local
      bcINC_u32: Inc(PUInt32(BASEPTR_0)^);
      bcINC_i64: Inc( PInt64(BASEPTR_0)^);
      bcINC_u64: Inc(PUInt64(BASEPTR_0)^);

      // --- FAST ADDRESSING OPERATIONS (FMA) ---
      // Dest = BaseAddr + ElementIndex * ItemSize
      bcFMAD64_64:
        if pc^.Args[3].Pos = mpLocal then PInt64(BASEPTR_3)^ := PUInt64(PPtrInt(MEMBASE_2)^ + PInt64(MEMBASE_0)^ * pc^.Args[1].Data.Addr)^
        else                              PInt64(MEMBASE_3)^ := PUInt64(PPtrInt(MEMBASE_2)^ + PInt64(MEMBASE_0)^ * pc^.Args[1].Data.Addr)^;
      bcFMAD64_32:
        if pc^.Args[3].Pos = mpLocal then PInt64(BASEPTR_3)^ := PUInt64(PPtrInt(MEMBASE_2)^ + PInt32(MEMBASE_0)^ * pc^.Args[1].Data.Addr)^
        else                              PInt64(MEMBASE_3)^ := PUInt64(PPtrInt(MEMBASE_2)^ + PInt32(MEMBASE_0)^ * pc^.Args[1].Data.Addr)^;
      bcFMAD32_64:
        if pc^.Args[3].Pos = mpLocal then PInt32(BASEPTR_3)^ := PUInt32(PPtrInt(MEMBASE_2)^ + PInt64(MEMBASE_0)^ * pc^.Args[1].Data.Addr)^
        else                              PInt32(MEMBASE_3)^ := PUInt32(PPtrInt(MEMBASE_2)^ + PInt64(MEMBASE_0)^ * pc^.Args[1].Data.Addr)^;
      bcFMAD32_32:
        if pc^.Args[3].Pos = mpLocal then PInt32(BASEPTR_3)^ := PUInt32(PPtrInt(MEMBASE_2)^ + PInt32(MEMBASE_0)^ * pc^.Args[1].Data.Addr)^
        else                              PInt32(MEMBASE_3)^ := PUInt32(PPtrInt(MEMBASE_2)^ + PInt32(MEMBASE_0)^ * pc^.Args[1].Data.Addr)^;

      // --- FMA (Fused Multiply-Add) ---
      bcFMA_i8:
        if pc^.Args[3].Pos = mpLocal then PPtrInt(BASEPTR_3)^ := PPtrInt(MEMBASE_2)^ + PInt8(MEMBASE_0)^ * pc^.Args[1].Data.Addr
        else                              PPtrInt(MEMBASE_3)^ := PPtrInt(MEMBASE_2)^ + PInt8(MEMBASE_0)^ * pc^.Args[1].Data.Addr;
      bcFMA_u8:
        if pc^.Args[3].Pos = mpLocal then PPtrInt(BASEPTR_3)^ := PPtrInt(MEMBASE_2)^ + PUInt8(MEMBASE_0)^ * pc^.Args[1].Data.Addr
        else                              PPtrInt(MEMBASE_3)^ := PPtrInt(MEMBASE_2)^ + PUInt8(MEMBASE_0)^ * pc^.Args[1].Data.Addr;
      bcFMA_i16:
        if pc^.Args[3].Pos = mpLocal then PPtrInt(BASEPTR_3)^ := PPtrInt(MEMBASE_2)^ + PInt16(MEMBASE_0)^ * pc^.Args[1].Data.Addr
        else                              PPtrInt(MEMBASE_3)^ := PPtrInt(MEMBASE_2)^ + PInt16(MEMBASE_0)^ * pc^.Args[1].Data.Addr;
      bcFMA_u16:
        if pc^.Args[3].Pos = mpLocal then PPtrInt(BASEPTR_3)^ := PPtrInt(MEMBASE_2)^ + PUInt16(MEMBASE_0)^ * pc^.Args[1].Data.Addr
        else                              PPtrInt(MEMBASE_3)^ := PPtrInt(MEMBASE_2)^ + PUInt16(MEMBASE_0)^ * pc^.Args[1].Data.Addr;
      bcFMA_i32:
        if pc^.Args[3].Pos = mpLocal then PPtrInt(BASEPTR_3)^ := PPtrInt(MEMBASE_2)^ + PInt32(MEMBASE_0)^ * pc^.Args[1].Data.Addr
        else                              PPtrInt(MEMBASE_3)^ := PPtrInt(MEMBASE_2)^ + PInt32(MEMBASE_0)^ * pc^.Args[1].Data.Addr;
      bcFMA_u32:
        if pc^.Args[3].Pos = mpLocal then PPtrInt(BASEPTR_3)^ := PPtrInt(MEMBASE_2)^ + PUInt32(MEMBASE_0)^ * pc^.Args[1].Data.Addr
        else                              PPtrInt(MEMBASE_3)^ := PPtrInt(MEMBASE_2)^ + PUInt32(MEMBASE_0)^ * pc^.Args[1].Data.Addr;
      bcFMA_i64:
        if pc^.Args[3].Pos = mpLocal then PPtrInt(BASEPTR_3)^ := PPtrInt(MEMBASE_2)^ + PInt64(MEMBASE_0)^ * pc^.Args[1].Data.Addr
        else                              PPtrInt(MEMBASE_3)^ := PPtrInt(MEMBASE_2)^ + PInt64(MEMBASE_0)^ * pc^.Args[1].Data.Addr;
      bcFMA_u64:
        if pc^.Args[3].Pos = mpLocal then PPtrInt(BASEPTR_3)^ := PPtrInt(MEMBASE_2)^ + PUInt64(MEMBASE_0)^ * pc^.Args[1].Data.Addr
        else                              PPtrInt(MEMBASE_3)^ := PPtrInt(MEMBASE_2)^ + PUInt64(MEMBASE_0)^ * pc^.Args[1].Data.Addr;

      // --- DEREFERENCES AND POINTERS ---
      bcADDR:   PPointer(MEMBASE_0)^ := MEMBASE_1;
      bcDREF:   Move(Pointer(PPointer(MEMBASE_1)^)^, Pointer(MEMBASE_0)^, pc^.Args[2].Data.Addr);
      bcDREF32: PUInt32(MEMBASE_0)^ := PUInt32(PPointer(MEMBASE_1)^)^;
      bcDREF64: PUInt64(MEMBASE_0)^ := PUInt64(PPointer(MEMBASE_1)^)^;

      // -----------------------------------------------------------------------
      // --- SPECIALIZED BINARY MATH (Dest is Local, Sources are Branchless) ---
      // -----------------------------------------------------------------------

      // --- ADD (+) ---
      bcADD_i32:
        if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := PInt32  (MEMBASE_0)^ + PInt32  (MEMBASE_1)^
        else                              PInt32  (MEMBASE_2)^ := PInt32  (MEMBASE_0)^ + PInt32  (MEMBASE_1)^;
      bcADD_u32:
        if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ + PUInt32 (MEMBASE_1)^
        else                              PUInt32 (MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ + PUInt32 (MEMBASE_1)^;
      bcADD_i64:
        if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := PInt64  (MEMBASE_0)^ + PInt64  (MEMBASE_1)^
        else                              PInt64  (MEMBASE_2)^ := PInt64  (MEMBASE_0)^ + PInt64  (MEMBASE_1)^;
      bcADD_u64:
        if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ + PUInt64 (MEMBASE_1)^
        else                              PUInt64 (MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ + PUInt64 (MEMBASE_1)^;
      bcADD_f32:
        if pc^.Args[2].Pos = mpLocal then PFloat32(BASEPTR_2)^ := PFloat32(MEMBASE_0)^ + PFloat32(MEMBASE_1)^
        else                              PFloat32(MEMBASE_2)^ := PFloat32(MEMBASE_0)^ + PFloat32(MEMBASE_1)^;
      bcADD_f64:
        if pc^.Args[2].Pos = mpLocal then PFloat64(BASEPTR_2)^ := PFloat64(MEMBASE_0)^ + PFloat64(MEMBASE_1)^
        else                              PFloat64(MEMBASE_2)^ := PFloat64(MEMBASE_0)^ + PFloat64(MEMBASE_1)^;

      // --- SUB (-) ---
      bcSUB_i32:
        if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := PInt32  (MEMBASE_0)^ - PInt32  (MEMBASE_1)^
        else                              PInt32  (MEMBASE_2)^ := PInt32  (MEMBASE_0)^ - PInt32  (MEMBASE_1)^;
      bcSUB_u32:
        if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ - PUInt32 (MEMBASE_1)^
        else                              PUInt32 (MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ - PUInt32 (MEMBASE_1)^;
      bcSUB_i64:
        if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := PInt64  (MEMBASE_0)^ - PInt64  (MEMBASE_1)^
        else                              PInt64  (MEMBASE_2)^ := PInt64  (MEMBASE_0)^ - PInt64  (MEMBASE_1)^;
      bcSUB_u64:
        if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ - PUInt64 (MEMBASE_1)^
        else                              PUInt64 (MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ - PUInt64 (MEMBASE_1)^;
      bcSUB_f32:
        if pc^.Args[2].Pos = mpLocal then PFloat32(BASEPTR_2)^ := PFloat32(MEMBASE_0)^ - PFloat32(MEMBASE_1)^
        else                              PFloat32(MEMBASE_2)^ := PFloat32(MEMBASE_0)^ - PFloat32(MEMBASE_1)^;
      bcSUB_f64:
        if pc^.Args[2].Pos = mpLocal then PFloat64(BASEPTR_2)^ := PFloat64(MEMBASE_0)^ - PFloat64(MEMBASE_1)^
        else                              PFloat64(MEMBASE_2)^ := PFloat64(MEMBASE_0)^ - PFloat64(MEMBASE_1)^;

      // --- MUL (*) ---
      bcMUL_i32:
        if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := PInt32  (MEMBASE_0)^ * PInt32  (MEMBASE_1)^
        else                              PInt32  (MEMBASE_2)^ := PInt32  (MEMBASE_0)^ * PInt32  (MEMBASE_1)^;
      bcMUL_u32:
        if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ * PUInt32 (MEMBASE_1)^
        else                              PUInt32 (MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ * PUInt32 (MEMBASE_1)^;
      bcMUL_i64:
        if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := PInt64  (MEMBASE_0)^ * PInt64  (MEMBASE_1)^
        else                              PInt64  (MEMBASE_2)^ := PInt64  (MEMBASE_0)^ * PInt64  (MEMBASE_1)^;
      bcMUL_u64:
        if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ * PUInt64 (MEMBASE_1)^
        else                              PUInt64 (MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ * PUInt64 (MEMBASE_1)^;
      bcMUL_f32:
        if pc^.Args[2].Pos = mpLocal then PFloat32(BASEPTR_2)^ := PFloat32(MEMBASE_0)^ * PFloat32(MEMBASE_1)^
        else                              PFloat32(MEMBASE_2)^ := PFloat32(MEMBASE_0)^ * PFloat32(MEMBASE_1)^;
      bcMUL_f64:
        if pc^.Args[2].Pos = mpLocal then PFloat64(BASEPTR_2)^ := PFloat64(MEMBASE_0)^ * PFloat64(MEMBASE_1)^
        else                              PFloat64(MEMBASE_2)^ := PFloat64(MEMBASE_0)^ * PFloat64(MEMBASE_1)^;

      // --- DIV (div / /) ---
      bcDIV_i32:
        if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := PInt32  (MEMBASE_0)^ div PInt32  (MEMBASE_1)^
        else                              PInt32  (MEMBASE_2)^ := PInt32  (MEMBASE_0)^ div PInt32  (MEMBASE_1)^;
      bcDIV_u32:
        if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ div PUInt32 (MEMBASE_1)^
        else                              PUInt32 (MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ div PUInt32 (MEMBASE_1)^;
      bcDIV_i64:
        if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := PInt64  (MEMBASE_0)^ div PInt64  (MEMBASE_1)^
        else                              PInt64  (MEMBASE_2)^ := PInt64  (MEMBASE_0)^ div PInt64  (MEMBASE_1)^;
      bcDIV_u64:
        if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ div PUInt64 (MEMBASE_1)^
        else                              PUInt64 (MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ div PUInt64 (MEMBASE_1)^;
      bcDIV_f32:
        if pc^.Args[2].Pos = mpLocal then PFloat32(BASEPTR_2)^ := PFloat32(MEMBASE_0)^  /  PFloat32(MEMBASE_1)^
        else                              PFloat32(MEMBASE_2)^ := PFloat32(MEMBASE_0)^  /  PFloat32(MEMBASE_1)^;
      bcDIV_f64:
        if pc^.Args[2].Pos = mpLocal then PFloat64(BASEPTR_2)^ := PFloat64(MEMBASE_0)^  /  PFloat64(MEMBASE_1)^
        else                              PFloat64(MEMBASE_2)^ := PFloat64(MEMBASE_0)^  /  PFloat64(MEMBASE_1)^;

      // --- MOD (mod) ---
      bcMOD_i32:
        if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := PInt32  (MEMBASE_0)^ mod PInt32  (MEMBASE_1)^
        else                              PInt32  (MEMBASE_2)^ := PInt32  (MEMBASE_0)^ mod PInt32  (MEMBASE_1)^;
      bcMOD_u32:
        if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ mod PUInt32 (MEMBASE_1)^
        else                              PUInt32 (MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ mod PUInt32 (MEMBASE_1)^;
      bcMOD_i64:
        if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := PInt64  (MEMBASE_0)^ mod PInt64  (MEMBASE_1)^
        else                              PInt64  (MEMBASE_2)^ := PInt64  (MEMBASE_0)^ mod PInt64  (MEMBASE_1)^;
      bcMOD_u64:
        if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ mod PUInt64 (MEMBASE_1)^
        else                              PUInt64 (MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ mod PUInt64 (MEMBASE_1)^;
      bcMOD_f32:
        if pc^.Args[2].Pos = mpLocal then PFloat32(BASEPTR_2)^ := PFloat32(MEMBASE_0)^ mod PFloat32(MEMBASE_1)^
        else                              PFloat32(MEMBASE_2)^ := PFloat32(MEMBASE_0)^ mod PFloat32(MEMBASE_1)^;
      bcMOD_f64:
        if pc^.Args[2].Pos = mpLocal then PFloat64(BASEPTR_2)^ := PFloat64(MEMBASE_0)^ mod PFloat64(MEMBASE_1)^
        else                              PFloat64(MEMBASE_2)^ := PFloat64(MEMBASE_0)^ mod PFloat64(MEMBASE_1)^;

      // --- POW (Power) ---
      bcPOW_i32:
        if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := Power(PInt32  (MEMBASE_0)^, PInt32  (MEMBASE_1)^)
        else                              PInt32  (MEMBASE_2)^ := Power(PInt32  (MEMBASE_0)^, PInt32  (MEMBASE_1)^);
      bcPOW_u32:
        if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := Power(PUInt32 (MEMBASE_0)^, PUInt32 (MEMBASE_1)^)
        else                              PUInt32 (MEMBASE_2)^ := Power(PUInt32 (MEMBASE_0)^, PUInt32 (MEMBASE_1)^);
      bcPOW_i64:
        if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := Power(PInt64  (MEMBASE_0)^, PInt64  (MEMBASE_1)^)
        else                              PInt64  (MEMBASE_2)^ := Power(PInt64  (MEMBASE_0)^, PInt64  (MEMBASE_1)^);
      bcPOW_u64:
        if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := Power(PUInt64 (MEMBASE_0)^, PUInt64 (MEMBASE_1)^)
        else                              PUInt64 (MEMBASE_2)^ := Power(PUInt64 (MEMBASE_0)^, PUInt64 (MEMBASE_1)^);
      bcPOW_f32:
        if pc^.Args[2].Pos = mpLocal then PFloat32(BASEPTR_2)^ := Power(PFloat32(MEMBASE_0)^, PFloat32(MEMBASE_1)^)
        else                              PFloat32(MEMBASE_2)^ := Power(PFloat32(MEMBASE_0)^, PFloat32(MEMBASE_1)^);
      bcPOW_f64:
        if pc^.Args[2].Pos = mpLocal then PFloat64(BASEPTR_2)^ := Power(PFloat64(MEMBASE_0)^, PFloat64(MEMBASE_1)^)
        else                              PFloat64(MEMBASE_2)^ := Power(PFloat64(MEMBASE_0)^, PFloat64(MEMBASE_1)^);

      // -----------------------------------------------------------------------
      // --- COMPARISON OPERATORS (Returns Boolean)                          ---
      // -----------------------------------------------------------------------

      // --- EQ (=) ---
      bcEQ_i32:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt32  (MEMBASE_0)^ = PInt32  (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PInt32  (MEMBASE_0)^ = PInt32  (MEMBASE_1)^;
      bcEQ_u32:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ = PUInt32 (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ = PUInt32 (MEMBASE_1)^;
      bcEQ_i64:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt64  (MEMBASE_0)^ = PInt64  (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PInt64  (MEMBASE_0)^ = PInt64  (MEMBASE_1)^;
      bcEQ_u64:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ = PUInt64 (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ = PUInt64 (MEMBASE_1)^;
      bcEQ_f32:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat32(MEMBASE_0)^ = PFloat32(MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PFloat32(MEMBASE_0)^ = PFloat32(MEMBASE_1)^;
      bcEQ_f64:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat64(MEMBASE_0)^ = PFloat64(MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PFloat64(MEMBASE_0)^ = PFloat64(MEMBASE_1)^;

      // --- NE (<>) ---
      bcNE_i32:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt32  (MEMBASE_0)^ <> PInt32  (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PInt32  (MEMBASE_0)^ <> PInt32  (MEMBASE_1)^;
      bcNE_u32:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ <> PUInt32 (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ <> PUInt32 (MEMBASE_1)^;
      bcNE_i64:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt64(MEMBASE_0)^ <> PInt64(MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PInt64(MEMBASE_0)^ <> PInt64(MEMBASE_1)^;
      bcNE_u64:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ <> PUInt64 (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ <> PUInt64 (MEMBASE_1)^;
      bcNE_f32:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat32(MEMBASE_0)^ <> PFloat32(MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PFloat32(MEMBASE_0)^ <> PFloat32(MEMBASE_1)^;
      bcNE_f64:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat64(MEMBASE_0)^ <> PFloat64(MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PFloat64(MEMBASE_0)^ <> PFloat64(MEMBASE_1)^;

      // --- LT (<) ---
      bcLT_i32:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt32  (MEMBASE_0)^ < PInt32  (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PInt32  (MEMBASE_0)^ < PInt32  (MEMBASE_1)^;
      bcLT_u32:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ < PUInt32 (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ < PUInt32 (MEMBASE_1)^;
      bcLT_i64:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt64  (MEMBASE_0)^ < PInt64  (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PInt64  (MEMBASE_0)^ < PInt64  (MEMBASE_1)^;
      bcLT_u64:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ < PUInt64 (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ < PUInt64 (MEMBASE_1)^;
      bcLT_f32:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat32(MEMBASE_0)^ < PFloat32(MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PFloat32(MEMBASE_0)^ < PFloat32(MEMBASE_1)^;
      bcLT_f64:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat64(MEMBASE_0)^ < PFloat64(MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PFloat64(MEMBASE_0)^ < PFloat64(MEMBASE_1)^;

      // --- GT (>) ---
      bcGT_i32:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt32  (MEMBASE_0)^ > PInt32  (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PInt32  (MEMBASE_0)^ > PInt32  (MEMBASE_1)^;
      bcGT_u32:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ > PUInt32 (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ > PUInt32 (MEMBASE_1)^;
      bcGT_i64:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt64  (MEMBASE_0)^ > PInt64  (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PInt64  (MEMBASE_0)^ > PInt64  (MEMBASE_1)^;
      bcGT_u64:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ > PUInt64 (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ > PUInt64 (MEMBASE_1)^;
      bcGT_f32:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat32(MEMBASE_0)^ > PFloat32(MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PFloat32(MEMBASE_0)^ > PFloat32(MEMBASE_1)^;
      bcGT_f64:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat64(MEMBASE_0)^ > PFloat64(MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PFloat64(MEMBASE_0)^ > PFloat64(MEMBASE_1)^;

      // --- GTE (>=) ---
      bcGTE_i32:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt32  (MEMBASE_0)^ >= PInt32  (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PInt32  (MEMBASE_0)^ >= PInt32  (MEMBASE_1)^;
      bcGTE_u32:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ >= PUInt32 (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ >= PUInt32 (MEMBASE_1)^;
      bcGTE_i64:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt64  (MEMBASE_0)^ >= PInt64  (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PInt64  (MEMBASE_0)^ >= PInt64  (MEMBASE_1)^;
      bcGTE_u64:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ >= PUInt64 (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ >= PUInt64 (MEMBASE_1)^;
      bcGTE_f32:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat32(MEMBASE_0)^ >= PFloat32(MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PFloat32(MEMBASE_0)^ >= PFloat32(MEMBASE_1)^;
      bcGTE_f64:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat64(MEMBASE_0)^ >= PFloat64(MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PFloat64(MEMBASE_0)^ >= PFloat64(MEMBASE_1)^;

      // --- LTE (<=) ---
      bcLTE_i32:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt32  (MEMBASE_0)^ <= PInt32  (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PInt32  (MEMBASE_0)^ <= PInt32  (MEMBASE_1)^;
      bcLTE_u32:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ <= PUInt32 (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ <= PUInt32 (MEMBASE_1)^;
      bcLTE_i64:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt64  (MEMBASE_0)^ <= PInt64  (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PInt64  (MEMBASE_0)^ <= PInt64  (MEMBASE_1)^;
      bcLTE_u64:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ <= PUInt64 (MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ <= PUInt64 (MEMBASE_1)^;
      bcLTE_f32:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat32(MEMBASE_0)^ <= PFloat32(MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PFloat32(MEMBASE_0)^ <= PFloat32(MEMBASE_1)^;
      bcLTE_f64:
        if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat64(MEMBASE_0)^ <= PFloat64(MEMBASE_1)^
        else                              PBoolean(MEMBASE_2)^ := PFloat64(MEMBASE_0)^ <= PFloat64(MEMBASE_1)^;

      // -----------------------------------------------------------------------
      // --- BITWISE OPERATORS (Integers Only)                               ---
      // -----------------------------------------------------------------------

      // --- BND (and) ---
      bcBND_i32:
        if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := PInt32  (MEMBASE_0)^ and PInt32  (MEMBASE_1)^
        else                              PInt32  (MEMBASE_2)^ := PInt32  (MEMBASE_0)^ and PInt32  (MEMBASE_1)^;
      bcBND_u32:
        if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ and PUInt32 (MEMBASE_1)^
        else                              PUInt32 (MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ and PUInt32 (MEMBASE_1)^;
      bcBND_i64:
        if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := PInt64  (MEMBASE_0)^ and PInt64  (MEMBASE_1)^
        else                              PInt64  (MEMBASE_2)^ := PInt64  (MEMBASE_0)^ and PInt64  (MEMBASE_1)^;
      bcBND_u64:
        if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ and PUInt64 (MEMBASE_1)^
        else                              PUInt64 (MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ and PUInt64 (MEMBASE_1)^;

      // --- SHL (shl) ---
      bcSHL_i32:
        if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := PInt32  (MEMBASE_0)^ shl PInt32  (MEMBASE_1)^
        else                              PInt32  (MEMBASE_2)^ := PInt32  (MEMBASE_0)^ shl PInt32  (MEMBASE_1)^;
      bcSHL_u32:
        if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ shl PUInt32 (MEMBASE_1)^
        else                              PUInt32 (MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ shl PUInt32 (MEMBASE_1)^;
      bcSHL_i64:
        if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := PInt64  (MEMBASE_0)^ shl PInt64  (MEMBASE_1)^
        else                              PInt64  (MEMBASE_2)^ := PInt64  (MEMBASE_0)^ shl PInt64  (MEMBASE_1)^;
      bcSHL_u64:
        if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ shl PUInt64 (MEMBASE_1)^
        else                              PUInt64 (MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ shl PUInt64 (MEMBASE_1)^;

      // --- SHR (shr) ---
      bcSHR_i32:
        if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := PInt32  (MEMBASE_0)^ shr PInt32  (MEMBASE_1)^
        else                              PInt32  (MEMBASE_2)^ := PInt32  (MEMBASE_0)^ shr PInt32  (MEMBASE_1)^;
      bcSHR_u32:
        if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ shr PUInt32 (MEMBASE_1)^
        else                              PUInt32 (MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ shr PUInt32 (MEMBASE_1)^;
      bcSHR_i64:
        if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := PInt64  (MEMBASE_0)^ shr PInt64  (MEMBASE_1)^
        else                              PInt64  (MEMBASE_2)^ := PInt64  (MEMBASE_0)^ shr PInt64  (MEMBASE_1)^;
      bcSHR_u64:
        if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ shr PUInt64 (MEMBASE_1)^
        else                              PUInt64 (MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ shr PUInt64 (MEMBASE_1)^;

      // --- XOR (xor) ---
      bcXOR_i32:
        if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := PInt32  (MEMBASE_0)^ xor PInt32  (MEMBASE_1)^
        else                              PInt32  (MEMBASE_2)^ := PInt32  (MEMBASE_0)^ xor PInt32  (MEMBASE_1)^;
      bcXOR_u32:
        if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ xor PUInt32 (MEMBASE_1)^
        else                              PUInt32 (MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ xor PUInt32 (MEMBASE_1)^;
      bcXOR_i64:
        if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := PInt64  (MEMBASE_0)^ xor PInt64  (MEMBASE_1)^
        else                              PInt64  (MEMBASE_2)^ := PInt64  (MEMBASE_0)^ xor PInt64  (MEMBASE_1)^;
      bcXOR_u64:
        if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ xor PUInt64 (MEMBASE_1)^
        else                              PUInt64 (MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ xor PUInt64 (MEMBASE_1)^;

      // --- BOR (or) ---
      bcBOR_i32:
        if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := PInt32  (MEMBASE_0)^ or PInt32  (MEMBASE_1)^
        else                              PInt32  (MEMBASE_2)^ := PInt32  (MEMBASE_0)^ or PInt32  (MEMBASE_1)^;
      bcBOR_u32:
        if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ or PUInt32 (MEMBASE_1)^
        else                              PUInt32 (MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ or PUInt32 (MEMBASE_1)^;
      bcBOR_i64:
        if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := PInt64  (MEMBASE_0)^ or PInt64  (MEMBASE_1)^
        else                              PInt64  (MEMBASE_2)^ := PInt64  (MEMBASE_0)^ or PInt64  (MEMBASE_1)^;
      bcBOR_u64:
        if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ or PUInt64 (MEMBASE_1)^
        else                              PUInt64 (MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ or PUInt64 (MEMBASE_1)^;

      // --- SAR (Arithmetic Shift Right) ---
      bcSAR_i32:
        if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := Sar(PInt32  (MEMBASE_0)^, PInt32  (MEMBASE_1)^)
        else                              PInt32  (MEMBASE_2)^ := Sar(PInt32  (MEMBASE_0)^, PInt32  (MEMBASE_1)^);
      bcSAR_u32:
        if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := Sar(PUInt32 (MEMBASE_0)^, PUInt32 (MEMBASE_1)^)
        else                              PUInt32 (MEMBASE_2)^ := Sar(PUInt32 (MEMBASE_0)^, PUInt32 (MEMBASE_1)^);
      bcSAR_i64:
        if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := Sar(PInt64  (MEMBASE_0)^, PInt64  (MEMBASE_1)^)
        else                              PInt64  (MEMBASE_2)^ := Sar(PInt64  (MEMBASE_0)^, PInt64  (MEMBASE_1)^);
      bcSAR_u64:
        if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := Sar(PUInt64 (MEMBASE_0)^, PUInt64 (MEMBASE_1)^)
        else                              PUInt64 (MEMBASE_2)^ := Sar(PUInt64 (MEMBASE_0)^, PUInt64 (MEMBASE_1)^);


      // -------------------------------------------------------------
      // --- MOV OPERATIONS (Dest is Local, Src is Branchless)     ---
      // -------------------------------------------------------------

      // --- Same-Size or Truncating ---
      bcMOV8:  PInt8 (MEMBASE_0)^ := PInt8 (MEMBASE_1)^;
      bcMOV16: PInt16(MEMBASE_0)^ := PInt16(MEMBASE_1)^;
      bcMOV32: PInt32(MEMBASE_0)^ := PInt32(MEMBASE_1)^;
      bcMOV64: PInt64(MEMBASE_0)^ := PInt64(MEMBASE_1)^;

      // --- Widening Signed (Sign Extension) ---
      bcMOVSX16_8:  PInt16(MEMBASE_0)^ := Int16(PInt8(MEMBASE_1)^);
      bcMOVSX32_8:  PInt32(MEMBASE_0)^ := Int32(PInt8(MEMBASE_1)^);
      bcMOVSX32_16: PInt32(MEMBASE_0)^ := Int32(PInt16(MEMBASE_1)^);
      bcMOVSX64_8:  PInt64(MEMBASE_0)^ := Int64(PInt8(MEMBASE_1)^);
      bcMOVSX64_16: PInt64(MEMBASE_0)^ := Int64(PInt16(MEMBASE_1)^);
      bcMOVSX64_32: PInt64(MEMBASE_0)^ := Int64(PInt32(MEMBASE_1)^);

      // --- Widening Unsigned (Zero Extension) ---
      bcMOVZX16_8:  PUInt16(MEMBASE_0)^ := UInt16(PUInt8(MEMBASE_1)^);
      bcMOVZX32_8:  PUInt32(MEMBASE_0)^ := UInt32(PUInt8(MEMBASE_1)^);
      bcMOVZX32_16: PUInt32(MEMBASE_0)^ := UInt32(PUInt16(MEMBASE_1)^);
      bcMOVZX64_8:  PUInt64(MEMBASE_0)^ := UInt64(PUInt8(MEMBASE_1)^);
      bcMOVZX64_16: PUInt64(MEMBASE_0)^ := UInt64(PUInt16(MEMBASE_1)^);
      bcMOVZX64_32: PUInt64(MEMBASE_0)^ := UInt64(PUInt32(MEMBASE_1)^);

      // --- Float Conversion ---
      bcMOVF32_i32: PFloat32(MEMBASE_0)^ := Float32(PInt32(MEMBASE_1)^);
      bcMOVF32_u32: PFloat32(MEMBASE_0)^ := Float32(PUInt32(MEMBASE_1)^);
      bcMOVF32_i64: PFloat32(MEMBASE_0)^ := Float32(PInt64(MEMBASE_1)^);
      bcMOVF32_u64: PFloat32(MEMBASE_0)^ := Float32(PUInt64(MEMBASE_1)^);
      bcMOVF32_f32: PFloat32(MEMBASE_0)^ := PFloat32(MEMBASE_1)^;
      bcMOVF32_f64: PFloat32(MEMBASE_0)^ := Float32(PFloat64(MEMBASE_1)^);

      bcMOVF64_i32: PFloat64(MEMBASE_0)^ := Float64(PInt32(MEMBASE_1)^);
      bcMOVF64_u32: PFloat64(MEMBASE_0)^ := Float64(PUInt32(MEMBASE_1)^);
      bcMOVF64_i64: PFloat64(MEMBASE_0)^ := Float64(PInt64(MEMBASE_1)^);
      bcMOVF64_u64: PFloat64(MEMBASE_0)^ := Float64(PUInt64(MEMBASE_1)^);
      bcMOVF64_f32: PFloat64(MEMBASE_0)^ := Float64(PFloat32(MEMBASE_1)^);
      bcMOVF64_f64: PFloat64(MEMBASE_0)^ := PFloat64(MEMBASE_1)^;


      // -------------------------------------------------------------
      // --- STORE OPERATIONS (Dest is Reference, Src is Branchless) -
      // -------------------------------------------------------------

      // --- Same-Size or Truncating ---
      bcSTORE8:  PInt8 (PPointer(MEMBASE_0)^)^ := PInt8 (MEMBASE_1)^;
      bcSTORE16: PInt16(PPointer(MEMBASE_0)^)^ := PInt16(MEMBASE_1)^;
      bcSTORE32: PInt32(PPointer(MEMBASE_0)^)^ := PInt32(MEMBASE_1)^;
      bcSTORE64: PInt64(PPointer(MEMBASE_0)^)^ := PInt64(MEMBASE_1)^;

      // --- Widening Signed (Sign Extension) ---
      bcSTORESX16_8:  PInt16(PPointer(MEMBASE_0)^)^ := Int16(PInt8(MEMBASE_1)^);
      bcSTORESX32_8:  PInt32(PPointer(MEMBASE_0)^)^ := Int32(PInt8(MEMBASE_1)^);
      bcSTORESX32_16: PInt32(PPointer(MEMBASE_0)^)^ := Int32(PInt16(MEMBASE_1)^);
      bcSTORESX64_8:  PInt64(PPointer(MEMBASE_0)^)^ := Int64(PInt8(MEMBASE_1)^);
      bcSTORESX64_16: PInt64(PPointer(MEMBASE_0)^)^ := Int64(PInt16(MEMBASE_1)^);
      bcSTORESX64_32: PInt64(PPointer(MEMBASE_0)^)^ := Int64(PInt32(MEMBASE_1)^);

      // --- Widening Unsigned (Zero Extension) ---
      bcSTOREZX16_8:  PUInt16(PPointer(MEMBASE_0)^)^ := UInt16(PUInt8(MEMBASE_1)^);
      bcSTOREZX32_8:  PUInt32(PPointer(MEMBASE_0)^)^ := UInt32(PUInt8(MEMBASE_1)^);
      bcSTOREZX32_16: PUInt32(PPointer(MEMBASE_0)^)^ := UInt32(PUInt16(MEMBASE_1)^);
      bcSTOREZX64_8:  PUInt64(PPointer(MEMBASE_0)^)^ := UInt64(PUInt8(MEMBASE_1)^);
      bcSTOREZX64_16: PUInt64(PPointer(MEMBASE_0)^)^ := UInt64(PUInt16(MEMBASE_1)^);
      bcSTOREZX64_32: PUInt64(PPointer(MEMBASE_0)^)^ := UInt64(PUInt32(MEMBASE_1)^);

      // --- Float Conversion ---
      bcSTOREF32_i32: PFloat32(PPointer(MEMBASE_0)^)^ := Float32(PInt32(MEMBASE_1)^);
      bcSTOREF32_u32: PFloat32(PPointer(MEMBASE_0)^)^ := Float32(PUInt32(MEMBASE_1)^);
      bcSTOREF32_i64: PFloat32(PPointer(MEMBASE_0)^)^ := Float32(PInt64(MEMBASE_1)^);
      bcSTOREF32_u64: PFloat32(PPointer(MEMBASE_0)^)^ := Float32(PUInt64(MEMBASE_1)^);
      bcSTOREF32_f32: PFloat32(PPointer(MEMBASE_0)^)^ := PFloat32(MEMBASE_1)^;
      bcSTOREF32_f64: PFloat32(PPointer(MEMBASE_0)^)^ := Float32(PFloat64(MEMBASE_1)^);

      bcSTOREF64_i32: PFloat64(PPointer(MEMBASE_0)^)^ := Float64(PInt32(MEMBASE_1)^);
      bcSTOREF64_u32: PFloat64(PPointer(MEMBASE_0)^)^ := Float64(PUInt32(MEMBASE_1)^);
      bcSTOREF64_i64: PFloat64(PPointer(MEMBASE_0)^)^ := Float64(PInt64(MEMBASE_1)^);
      bcSTOREF64_u64: PFloat64(PPointer(MEMBASE_0)^)^ := Float64(PUInt64(MEMBASE_1)^);
      bcSTOREF64_f32: PFloat64(PPointer(MEMBASE_0)^)^ := Float64(PFloat32(MEMBASE_1)^);
      bcSTOREF64_f64: PFloat64(PPointer(MEMBASE_0)^)^ := PFloat64(MEMBASE_1)^;

      // =======================================================================
      // JIT AND SUPERINSTRUCTION FASTPATHS
      // =======================================================================
      bcJIT:
        begin
          PtrInt(Left) := TJITMethod(pc^.Args[4].Data.Addr)(BasePtr);
          if PtrInt(Left) = 0 then
            Inc(pc, pc^.nArgs-1)
          else
          begin
            Inc(pc, PtrInt(Left) - 1); // Increment towards failure point
            continue;
          end;
        end;

      bcHOTLOOP:
        begin
          left := Pointer(MEMBASE_2);
          hot_condition := TSuperMethod(pc^.Args[4].Data.Addr);
          while True do
          begin
            hot_condition();                         //EQ, LT, GT etc..
            if not PBoolean(left)^ then              //JZ
            begin
              Inc(pc, pc^.Args[1].Data.i32);
              Break;
            end;  // This should respect soft-stop as well
            Inc(pc);
            TSuperMethod(pc^.Args[4].Data.Addr)();    //body
            Inc(pc, pc^.Args[0].Data.i32+1);          //reljmp
          end;
        end;

      bcSUPER:
        begin
          TSuperMethod(pc^.Args[4].Data.Addr)();
          Dec(pc);
        end;

      // =======================================================================
      // GENERALIZED SLOWPATHS
      // =======================================================================
      bcADD..bcSAR:
        HandleBinary(pc);

      bcMOV:
        HandleMOV(pc);

      bcSTORE:
        HandleSTORE(pc);

      // =======================================================================
      // OTHER SYSTEM AND API CALLS
      // =======================================================================

      bcLOAD_GLOBAL: PPointer(MEMBASE_0)^ := Global(pc^.Args[1].Data.Addr);
      bcCOPY_GLOBAL: PPointer(MEMBASE_0)^ := PPointer(Global(pc^.Args[1].Data.Addr))^;

      bcFILL:
        FillByte(Pointer(MEMBASE_0)^, pc^.Args[1].Data.Addr, pc^.Args[2].Data.u8);

      // --- THIS SHOULD ONLY HAPPEN AT GLOBAL SCOPE - Leaving it as MEMBASE_0 just in case.
      bcSET_ERRHANDLER: Self.NativeException := PPointer(MEMBASE_0)^;

      bcBCHK:
        if Self.BoundsCheck(pc) = 1 then
        begin
          Self.RunCode := VM_EXCEPTION;
          pc := nil;
          continue;
        end;

      bcRAISE:
        begin
          if PPointer(MEMBASE_0)^ <> Self.CurrentException then
          begin
            // Transfer ownership: The objects reference (either the constructor is rc=1,
            // or the compiler-emitted IncRef for existing vars) moves to CurrentException.
            // !! No incref.

            // However we might need to DecRef CurrentException since we are overwriting it.
            if (Self.CurrentException <> nil) then
              Self.DecRef(Self.CurrentException, xtClass);

            Self.CurrentException := PPointer(MEMBASE_0)^;
          end;

          Self.RunCode := VM_EXCEPTION;
          pc := nil;
          continue;
        end;

      bcGET_EXCEPTION:
        // Equal objects? No entry here!
        if PPointer(MEMBASE_0)^ <> Self.CurrentException then
        begin
          // Decrement other object
          if PPointer(MEMBASE_0)^ <> nil then
            Self.DecRef(PPointer(MEMBASE_0)^, xtClass);

          PPointer(MEMBASE_0)^ := Self.CurrentException;
          if Self.CurrentException <> nil then
            Self.IncRef(Self.CurrentException, xtClass);
        end;

      bcUNSET_EXCEPTION:
        begin
          DecRef(Self.CurrentException, xtClass); // release our reference
          Self.CurrentException := nil;
        end;

      bcNEW:     Self.NewClassOpcode(pc,bc);
      bcDYNCAST: Self.DynCastOpcode(BC.ClassVMTs,pc);

      bcRELEASE:
        begin
          left := PPointer(MEMBASE_0)^;
          FreeMem(left - SizeOf(Pointer)*3);
          PPointer(MEMBASE_0)^ := nil;
        end;

      bcIS:
        begin
          left := PPointer(MEMBASE_1)^;
          if left = nil then
            PBoolean(MEMBASE_0)^ := False
          else
            PBoolean(MEMBASE_0)^ := Self.IsA(
              BC.ClassVMTs,
              BC.ClassVMTs.Data[PPtrInt(Pointer(left) - SizeOf(Pointer) * 3)^].SelfID,
              pc^.Args[2].Data.i32
            );
        end;

      // try-except looses track of exactly what BC triggered exception
      // TODO XXX
      bcIncTry: TryStack.Push(Pointer(pc^.args[0].Data.Addr), StackPtr, BasePtr, 0);
      bcDecTry: TryStack.Pop();

      bcINCLOCK: Self.IncRef(PPointer(MEMBASE_0)^, pc^.Args[0].BaseType);
      bcDECLOCK: Self.DecRef(PPointer(MEMBASE_0)^, pc^.Args[0].BaseType);

      bcREFCNT:
        Self.ArrayRefcount(
          PPointer(Pointer(MEMBASE_0)^)^,
          PPointer(MEMBASE_1)^,
          pc^.Args[0].BaseType
        );

      // --- String operations
      bcLOAD_STR:
        PAnsiString(MEMBASE_0)^    := BC.StringTable[pc^.Args[1].Data.Addr];

      bcLOAD_USTR:
        PUnicodeString(MEMBASE_0)^ := UTF8Decode(BC.StringTable[pc^.Args[1].Data.Addr]);

      bcADD_STR:
        PAnsiString(MEMBASE_2)^ := PAnsiString(MEMBASE_0)^ + PAnsiString(MEMBASE_1)^;

      bcADD_USTR:
        PUnicodeString(MEMBASE_2)^ := PUnicodeString(MEMBASE_0)^ + PUnicodeString(MEMBASE_1)^;

      bcCh2Str:
        PAnsiString(MEMBASE_0)^ := PAnsiChar(MEMBASE_1)^;

      bcCh2UStr:
        PUnicodeString(MEMBASE_0)^ := PUnicodeChar(MEMBASE_1)^;

      bcPUSH:         ArgStack.Push(MEMBASE_0);
      bcPUSHREF:      ArgStack.Push(Pointer(PPointer(MEMBASE_0)^));
      bcPUSH_FP:      ArgStack.Push(BasePtr);
      bcPUSH_CLOSURE: Self.PushClosure(Pointer(MEMBASE_0));

      bcPOP:  Move(ArgStack.Pop()^, Pointer(MEMBASE_1)^, pc^.Args[0].Data.Addr);
      bcRPOP: Move(Pointer(MEMBASE_0)^, ArgStack.Pop()^,  pc^.Args[1].Data.Addr);
      bcPOPH: PPointer(MEMBASE_0)^ := ArgStack.Pop();

      bcNEWFRAME:
        begin
          FillByte(StackPtr^, pc^.Args[0].Data.Addr+SizeOf(Pointer), 0);
          BasePtr  := StackPtr;
          StackPtr += pc^.Args[0].Data.Addr;
          MemBases[mpLocal] := BasePtr; // Update the memory cache!
        end;

      bcINVOKE:
        begin
          Inc(Self.RecursionDepth);
          PtrUInt(left) := PPtrInt(MEMBASE_0)^;
          CallStack.Push(pc, StackPtr, BasePtr, PtrUInt(left));
          pc := @BC.Code.Data[PtrUInt(left)];
        end;

      bcINVOKEX:
        CallExternal(Pointer(pc^.Args[0].Data.Addr), pc^.Args[1].Data.u16, pc^.Args[2].Data.i8 <> 0);

      bcINVOKE_VIRTUAL:
        begin
          Inc(Self.RecursionDepth);
          PtrUInt(left) := Self.GetVirtualMethod(
            BC.ClassVMTs,
            ArgStack.Data[(ArgStack.Count - pc^.Args[1].Data.i32) + pc^.Args[2].Data.i32],
            pc^.Args[0].Data.i32
          );
          CallStack.Push(pc, StackPtr, BasePtr, PtrUInt(left));
          pc := @BC.Code.Data[PtrUInt(left)];
        end;

      bcRET:
        begin
          if CallStack.Top > -1 then
          begin
            frame := CallStack.Pop;
            Dec(RecursionDepth);
            if frame.ReturnAddress = nil then
            begin
              Self.RunCode := VM_HALTED;
              Exit;
            end;
            StackPtr := Frame.StackPtr;
            BasePtr  := Frame.FrameBase;
            MemBases[mpLocal] := BasePtr; // Update the memory cache!
            pc := Pointer(frame.ReturnAddress);
          end else
          begin
            Self.RunCode := VM_HALTED;
            Exit;
          end;
        end;

      bcRET_RAISE:
        begin
          if CallStack.Top > -1 then
          begin
            frame := CallStack.Pop;
            StackPtr := Frame.StackPtr;
            BasePtr  := Frame.FrameBase;
            MemBases[mpLocal] := BasePtr; // Update the memory cache!
            Dec(RecursionDepth);

            // sentinel: execution with no caller
            if frame.ReturnAddress = nil then
            begin
              Self.RunCode := VM_HALTED;
              Exit;
            end;

            if Self.RunCode <> VM_SOFT_STOP then
              Self.RunCode := VM_EXCEPTION;

            pc := nil;
            Continue;
          end else
          begin
            Self.RunCode := VM_HALTED;
            Exit;
          end;
        end;

      // threading
      bcSPAWN:
        Self.RunThreadOpcode(pc,bc);

      // ffi
      bcFFICALL:
        XprCallImport(Self, PXprNativeImport(pc^.Args[0].Data.Addr)^);
      bcFFICALL_DYN:
        XprCallDynamicImport(Self, PXprNativeImport(pc^.Args[0].Data.Addr)^);
      bcCREATE_CALLBACK:
        PPointer(MEMBASE_2)^ := Self.CreateFFICallback(pc, Pointer(MEMBASE_0), Pointer(pc^.Args[1].Data.Addr));

      //
      bcPRINT:
        if pc^.Args[1].Data.Addr <> 0 then
          WriteLn(PAnsiString(MEMBASE_0)^)
        else
          WriteLn;


      // --- THE END
      bcNOOP:
        (* nothing *);

      else
        raise RuntimeError.Create('Not implemented @ - ' + GetEnumName(TypeInfo(EBytecode), Ord(pc^.code)));
    end;

    Inc(pc);
    Self.ProgramRawLocation := pc;
  end;
  WHILE_CASE_EXIT:

  {$IFNDEF xpr_DisableJIT}
  Exit;

  {$I interpreter.super.fmad.inc}
  {$I interpreter.super.binary.inc}
  {$I interpreter.super.asgn.inc}
  {$ENDIF}
end;

end.
