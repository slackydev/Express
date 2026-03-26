unit xprffi;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL

  Foreign Function Interface for Express.

  Two directions:
    Import  - call a native C function from script via @native annotation
    Export  - give a script lambda to native code as a callback pointer

  Callback design:
    Each closure gets a dedicated TInterpreter (via NewForThread) plus a
    pre-built FFI CIF. When native code calls the function pointer,
    ExpressCallbackBinder marshals the C args into the interpreter's arg
    stack and calls Run. Identical to bcSPAWN but without the thread —
    each closure is an isolated execution environment sharing only the
    read-only bytecode and globals with the main interpreter.

  Thread safety:
    A single closure must not be called concurrently from multiple threads.
    That is the caller's responsibility (same rule as in C).
    Multiple distinct closures are fine — each has independent state.

  Context:
    Call XprSetCurrentContext before running any script that uses callbacks.
    This gives the FFI layer access to the live interpreter and bytecode.
}
{$I header.inc}
{$hints off}

interface

uses
  SysUtils,
  {$IFDEF WINDOWS}Windows,{$ELSE}dynlibs,{$ENDIF}
  xpr.Types,
  xpr.Bytecode,
  xpr.Interpreter,
  xpr.Vartypes,
  ffi;

// -- Context -------------------------------------------------------------------

// Must be called before running any script that creates callbacks.
// Single-threaded use only - for multi-script concurrency make this a threadvar.
procedure XprSetCurrentContext(var Interp: TInterpreter; var BC: TBytecode);

// -- Type mapping --------------------------------------------------------------

// Map an Express base type to the corresponding libffi primitive.
// Managed types (strings, arrays, classes) and records pass as pointers -
// they are always heap-allocated and Express passes them by reference.
function XprTypeToFFIType(T: EExpressBaseType): PFFIType;

// -- Import: calling native functions from script ------------------------------

type
  // Prepared call descriptor for one native import.
  // Built once at compile time; reused on every call.
  TXprNativeImport = record
    Func:      Pointer;
    Cif:       TFFICif;
    ArgCount:  Int32;
    ArgTypes:  array[0..63] of PFFIType;
    RetType:   PFFIType;
    HasReturn: Boolean;
  end;
  PXprNativeImport = ^TXprNativeImport;

// Resolve a symbol from a DLL and build a TXprNativeImport.
// Called at compile time when the @native annotation is processed.
// The library handle is intentionally leaked - it must remain loaded
// for the lifetime of the process.
function XprResolveImport(
  out   Import:   TXprNativeImport;
  const Lib:      string;
  const Sym:      string;
        FuncType: XType_Method;
        ABI:      TFFIABI = FFI_DEFAULT_ABI): Boolean;

// Execute one native call from inside the interpreter dispatch loop.
// Args are already on Interp.ArgStack (pushed by icPOP/icPOPH in the stub).
// On return, the result (if any) is pushed back onto ArgStack for icRET
// to deliver to the caller.
procedure XprCallImport(
  var Interp: TInterpreter;
  var Import: TXprNativeImport);

// -- Export: script lambdas as native callbacks --------------------------------

type
  PXprClosureData = ^TXprClosureData;
  TXprClosureData = record
    Interp:       ^TInterpreter;
    BC:           ^TBytecode;
    FuncEntry:    Int32;
    Cif:          TFFICif;
    ArgCount:     Int32;
    ArgSizes:     array[0..63] of Int32;
    ArgTypes:     array[0..63] of EExpressBaseType;
    RetSize:      Int32;
    RetType:      EExpressBaseType;
    HasReturn:    Boolean;
    FFIClosure:   PFFIClosure;
    FFIArgTypes:  array[0..63] of PFFIType;
    // Captured outer variable refs - pointers into the outer frame
    CaptureCount: Int32;
    CaptureRefs:  array[0..63] of Pointer;
  end;


  TXprCallbackTypeInfo = packed record
    ArgCount:  Int32;
    RetSize:   Int32;
    HasReturn: Boolean;
    RetType:   EExpressBaseType;
    ArgSizes:  array[0..63] of Int32;
    ArgTypes:  array[0..63] of EExpressBaseType;
    ABI:       TFFIABI;
  end;
  PXprCallbackTypeInfo = ^TXprCallbackTypeInfo;

// And a runtime creation function that uses the pre-built descriptor
// instead of XType_Method - called from bcCREATE_CALLBACK handler
function XprCreateClosureFromTypeInfo(
  FuncEntry: PtrInt;
  TypeInfo:  PXprCallbackTypeInfo): PXprClosureData;

// Create a closure from a script function entry point.
// Allocates a dedicated TInterpreter (same pattern as bcSPAWN).
// Returns nil on failure. Caller must call XprFreeClosure when done.
function XprCreateClosure(
  var   MainInterp: TInterpreter;
  var   BC:         TBytecode;
        FuncEntry:  Int32;
        FuncType:   XType_Method;
        ABI:        TFFIABI = FFI_DEFAULT_ABI): PXprClosureData;

// Convenience wrapper used by the _CreateCallback runtime intrinsic.
// Uses GCurrentInterpreter/GCurrentBC set by XprSetCurrentContext.
function XprCreateClosureFromRaw(
  FuncEntry: PtrInt;
  FuncType:  XType_Method;
  ABI:       TFFIABI = FFI_DEFAULT_ABI): PXprClosureData;

// Free all resources owned by a closure.
procedure XprFreeClosure(var Closure: PXprClosureData);

// Global closure registry - used by _FreeCallback to find a closure
// given only the raw function pointer that was handed to native code.
procedure XprRegisterClosure(Closure: PXprClosureData);
procedure XprUnregisterAndFreeClosure(FuncPtr: Pointer);

procedure ExpressCallbackBinder(
  var Cif:      TFFICif;
      Ret:      Pointer;
      Args:     PPointerArray;
      UserData: Pointer); cdecl;

implementation

uses
  Math;

// -- Context -------------------------------------------------------------------

var
  GCurrentInterpreter: ^TInterpreter = nil;
  GCurrentBC:          ^TBytecode    = nil;


function XprCreateClosureFromTypeInfo(
  FuncEntry: PtrInt;
  TypeInfo:  PXprCallbackTypeInfo): PXprClosureData;
var
  Data:    PXprClosureData;
  FuncPtr: Pointer;
  i:       Int32;
  ArgPtrs: PFFITypeArray;
  RetPtr:  PFFIType;
begin
  Result := nil;
  if not FFILoaded() then Exit;
  Assert(GCurrentInterpreter <> nil,
    'XprSetCurrentContext must be called before creating callbacks');

  Data := AllocMem(SizeOf(TXprClosureData));
  try
    Data^.Interp := AllocMem(SizeOf(TInterpreter));
    Data^.Interp^ := TInterpreter.NewForThread(
      GCurrentInterpreter^, FuncEntry, GCurrentBC^.Code.Size);
    Data^.BC        := GCurrentBC;
    Data^.FuncEntry := FuncEntry;
    Data^.ArgCount  := TypeInfo^.ArgCount;
    Data^.HasReturn := TypeInfo^.HasReturn;
    Data^.RetSize   := TypeInfo^.RetSize;
    Data^.RetType   := TypeInfo^.RetType;

    for i := 0 to Data^.ArgCount - 1 do
    begin
      Data^.ArgTypes[i]    := TypeInfo^.ArgTypes[i];
      Data^.ArgSizes[i]    := TypeInfo^.ArgSizes[i];
      Data^.FFIArgTypes[i] := XprTypeToFFIType(TypeInfo^.ArgTypes[i]);
    end;

    if Data^.HasReturn then
      RetPtr := XprTypeToFFIType(Data^.RetType)
    else
      RetPtr := @ffi_type_void;

    if Data^.ArgCount > 0 then
      ArgPtrs := PFFITypeArray(@Data^.FFIArgTypes[0])
    else
      ArgPtrs := nil;

    if ffi_prep_cif(Data^.Cif, TypeInfo^.ABI,
                    Data^.ArgCount, RetPtr, ArgPtrs) <> FFI_OK then
    begin
      FreeMem(Data^.Interp); FreeMem(Data); Exit;
    end;

    Data^.FFIClosure := ffi_closure_alloc(SizeOf(TFFIClosure), FuncPtr);
    if Data^.FFIClosure = nil then
    begin
      FreeMem(Data^.Interp); FreeMem(Data); Exit;
    end;

    if ffi_prep_closure_loc(Data^.FFIClosure^, Data^.Cif,
         @ExpressCallbackBinder, Data, FuncPtr) <> FFI_OK then
    begin
      ffi_closure_free(Data^.FFIClosure);
      FreeMem(Data^.Interp); FreeMem(Data); Exit;
    end;

    Result := Data;
  except
    if Assigned(Data) then
    begin
      if Assigned(Data^.Interp) then FreeMem(Data^.Interp);
      FreeMem(Data);
    end;
    raise;
  end;
end;

procedure XprSetCurrentContext(var Interp: TInterpreter; var BC: TBytecode);
begin
  GCurrentInterpreter := @Interp;
  GCurrentBC          := @BC;
end;

// -- Closure registry ----------------------------------------------------------

var
  GClosures:     array of PXprClosureData;
  GClosureCount: Int32 = 0;

procedure XprRegisterClosure(Closure: PXprClosureData);
begin
  if GClosureCount >= Length(GClosures) then
    SetLength(GClosures, Max(8, Length(GClosures) * 2));
  GClosures[GClosureCount] := Closure;
  Inc(GClosureCount);
end;

procedure XprUnregisterAndFreeClosure(FuncPtr: Pointer);
var i: Int32;
begin
  for i := 0 to GClosureCount - 1 do
    if GClosures[i]^.FFIClosure = FuncPtr then
    begin
      XprFreeClosure(GClosures[i]);
      // Swap-remove
      GClosures[i] := GClosures[GClosureCount - 1];
      Dec(GClosureCount);
      Exit;
    end;
end;

// -- Type mapping --------------------------------------------------------------

function XprTypeToFFIType(T: EExpressBaseType): PFFIType;
begin
  case T of
    xtInt8:           Result := @ffi_type_sint8;
    xtUInt8:          Result := @ffi_type_uint8;
    xtInt16:          Result := @ffi_type_sint16;
    xtUInt16:         Result := @ffi_type_uint16;
    xtInt32:          Result := @ffi_type_sint32;
    xtUInt32:         Result := @ffi_type_uint32;
    xtInt64:          Result := @ffi_type_sint64;
    xtUInt64:         Result := @ffi_type_uint64;
    xtSingle:         Result := @ffi_type_float;
    xtDouble:         Result := @ffi_type_double;
    xtBool:           Result := @ffi_type_uint8;
    xtAnsiChar:       Result := @ffi_type_uint8;
    xtUnicodeChar:    Result := @ffi_type_uint16;
    // All managed / reference types pass as an opaque pointer
    xtAnsiString,
    xtUnicodeString,
    xtArray,
    xtClass,
    xtRecord,
    xtPointer,
    xtMethod,
    xtExternalMethod: Result := @ffi_type_pointer;
  else
    Result := @ffi_type_void;
  end;
end;

// -- Import --------------------------------------------------------------------

// Internal: build the import record from an already-resolved function pointer.
function XprBuildImport(
  out   Import:   TXprNativeImport;
        Func:     Pointer;
        FuncType: XType_Method;
        ABI:      TFFIABI): Boolean;
var
  i, paramStart: Int32;
  ArgPtrs:       PFFITypeArray;
begin
  FillChar(Import, SizeOf(Import), 0);
  Import.Func     := Func;
  // RealParamcount excludes the implicit 'self' for type methods.
  Import.ArgCount := FuncType.RealParamcount;
  Import.HasReturn := (FuncType.ReturnType <> nil) and
                      (FuncType.ReturnType.BaseType <> xtUnknown);

  // Params[0] may be 'self' for type methods; real params start after that.
  paramStart := Length(FuncType.Params) - Import.ArgCount;
  for i := 0 to Import.ArgCount - 1 do
    Import.ArgTypes[i] := XprTypeToFFIType(
      FuncType.Params[paramStart + i].BaseType);

  if Import.HasReturn then
    Import.RetType := XprTypeToFFIType(FuncType.ReturnType.BaseType)
  else
    Import.RetType := @ffi_type_void;

  if Import.ArgCount > 0 then
    ArgPtrs := PFFITypeArray(@Import.ArgTypes[0])
  else
    ArgPtrs := nil;

  Result := ffi_prep_cif(
    Import.Cif, ABI,
    Import.ArgCount,
    Import.RetType,
    ArgPtrs) = FFI_OK;
end;

function XprResolveImport(
  out   Import:   TXprNativeImport;
  const Lib:      string;
  const Sym:      string;
        FuncType: XType_Method;
        ABI:      TFFIABI): Boolean;
var
  Handle: TLibHandle;
  Func:   Pointer;
begin
  Result := False;
  if not FFILoaded() then Exit;

  Handle := system.LoadLibrary(Lib);
  if Handle = NilHandle then Exit;

  Func := system.GetProcAddress(Handle, Sym);
  if Func = nil then
  begin
    FreeLibrary(Handle);
    Exit;
  end;

  // Intentionally do not FreeLibrary - the lib must stay resident
  // for the entire lifetime of any function pointer derived from it.
  Result := XprBuildImport(Import, Func, FuncType, ABI);
end;

procedure XprCallImport(var Interp: TInterpreter; var Import: TXprNativeImport);
var
  i, base: Int32;
  ArgPtrs: array[0..63] of Pointer;
  RetPtr:  Pointer;
begin
  base := Interp.ArgStack.Count - Import.ArgCount;

  for i := 0 to Import.ArgCount - 1 do
    ArgPtrs[i] := Interp.ArgStack.Data[base + i];

  if Import.HasReturn then
    RetPtr := Interp.ArgStack.Data[base - 1]  // pointer to caller's result slot
  else
    RetPtr := nil;

  Dec(Interp.ArgStack.Count, Import.ArgCount + Ord(Import.HasReturn));

  if Import.ArgCount > 0 then
    ffi_call(Import.Cif, Import.Func, RetPtr, PPointerArray(@ArgPtrs[0]))
  else
    ffi_call(Import.Cif, Import.Func, RetPtr, nil);
  // ffi_call writes directly to RetPtr - no push needed
end;

// -- Export (callbacks) --------------------------------------------------------

procedure ExpressCallbackBinder(
  var Cif:      TFFICif;
      Ret:      Pointer;
      Args:     PPointerArray;
      UserData: Pointer); cdecl;
var
  Data:  PXprClosureData;
  i:     Int32;
  Stack: PByte;
begin
  Data := PXprClosureData(UserData);

  Data^.Interp^.ArgStack.Count := 0;
  Data^.Interp^.CallStack.Init();
  Data^.Interp^.TryStack.Init();
  Data^.Interp^.RecursionDepth := 0;
  Data^.Interp^.RunCode := 1;

  Data^.Interp^.CallStack.Push(
    nil,
    Data^.Interp^.StackPtr,
    Data^.Interp^.BasePtr,
    0);

  Stack := Data^.Interp^.StackPtr;

  // push Ret directly - icPOPH stores it as the result ref,
  // so the script writes directly into the libffi return buffer.
  // DO NOT copy Ret into thread stack memory.
  if Data^.HasReturn then
  begin
    Data^.Interp^.ArgStack.Data[Data^.Interp^.ArgStack.Count] := Ret;
    Inc(Data^.Interp^.ArgStack.Count);
  end;

  // Declared value args - copy C arg values into thread stack, push pointers
  for i := 0 to Data^.ArgCount - 1 do
  begin
    Move(Args^[i]^, Stack^, Data^.ArgSizes[i]);
    Data^.Interp^.ArgStack.Data[Data^.Interp^.ArgStack.Count] := Stack;
    Inc(Data^.Interp^.ArgStack.Count);
    Inc(Stack, Data^.ArgSizes[i]);
    Stack := PByte(PtrUInt(Stack + 7) and not 7);
  end;

  // push captured outer variable refs.
  // DelayedCompile pops these via icPOPH (pbRef), so push raw pointers directly.
  // Push order is 0..CaptureCount-1 so they are popped CaptureCount-1..0,
  // matching the High downto 0 loop in DelayedCompile.
  for i := 0 to Data^.CaptureCount - 1 do
  begin
    Data^.Interp^.ArgStack.Data[Data^.Interp^.ArgStack.Count] := Data^.CaptureRefs[i];
    Inc(Data^.Interp^.ArgStack.Count);
  end;

  Data^.Interp^.StackPtr := Stack;

  Data^.Interp^.ProgramCounter := Data^.FuncEntry;
  Data^.Interp^.Run(Data^.BC^);
end;

function XprCreateClosure(
  var   MainInterp: TInterpreter;
  var   BC:         TBytecode;
        FuncEntry:  Int32;
        FuncType:   XType_Method;
        ABI:        TFFIABI): PXprClosureData;
var
  Data:       PXprClosureData;
  FuncPtr:    Pointer;
  i:          Int32;
  paramStart: Int32;
  ArgPtrs:    PFFITypeArray;
  RetPtr:     PFFIType;
begin
  Result := nil;
  if not FFILoaded() then Exit;

  Data := AllocMem(SizeOf(TXprClosureData));
  try
    Data^.Interp := AllocMem(SizeOf(TInterpreter));
    Data^.Interp^ := TInterpreter.NewForThread(
      MainInterp, FuncEntry, BC.Code.Size);
    Data^.BC        := @BC;
    Data^.FuncEntry := FuncEntry;
    Data^.ArgCount  := FuncType.RealParamcount;
    Data^.HasReturn := (FuncType.ReturnType <> nil) and
                       (FuncType.ReturnType.BaseType <> xtUnknown);

    paramStart := Length(FuncType.Params) - Data^.ArgCount;
    for i := 0 to Data^.ArgCount - 1 do
    begin
      Data^.ArgTypes[i]    := FuncType.Params[paramStart + i].BaseType;
      Data^.ArgSizes[i]    := XprTypeSize[Data^.ArgTypes[i]];
      Data^.FFIArgTypes[i] := XprTypeToFFIType(Data^.ArgTypes[i]);
    end;

    if Data^.HasReturn then
    begin
      Data^.RetType := FuncType.ReturnType.BaseType;
      Data^.RetSize := XprTypeSize[Data^.RetType];
      RetPtr        := XprTypeToFFIType(Data^.RetType);
    end else
    begin
      Data^.RetSize := 0;
      RetPtr        := @ffi_type_void;
    end;

    if Data^.ArgCount > 0 then
      ArgPtrs := PFFITypeArray(@Data^.FFIArgTypes[0])
    else
      ArgPtrs := nil;

    if ffi_prep_cif(Data^.Cif, ABI, Data^.ArgCount,
                    RetPtr, ArgPtrs) <> FFI_OK then
    begin
      FreeMem(Data^.Interp);
      FreeMem(Data);
      Exit;
    end;

    Data^.FFIClosure := ffi_closure_alloc(SizeOf(TFFIClosure), FuncPtr);
    if Data^.FFIClosure = nil then
    begin
      FreeMem(Data^.Interp);
      FreeMem(Data);
      Exit;
    end;

    if ffi_prep_closure_loc(
         Data^.FFIClosure^,
         Data^.Cif,
         @ExpressCallbackBinder,
         Data,
         FuncPtr) <> FFI_OK then
    begin
      ffi_closure_free(Data^.FFIClosure);
      FreeMem(Data^.Interp);
      FreeMem(Data);
      Exit;
    end;

    Result := Data;
  except
    if Assigned(Data) then
    begin
      if Assigned(Data^.Interp) then FreeMem(Data^.Interp);
      FreeMem(Data);
    end;
    raise;
  end;
end;

function XprCreateClosureFromRaw(
  FuncEntry: PtrInt;
  FuncType:  XType_Method;
  ABI:       TFFIABI): PXprClosureData;
begin
  Assert(GCurrentInterpreter <> nil,
    'XprSetCurrentContext must be called before creating callbacks');
  Result := XprCreateClosure(
    GCurrentInterpreter^, GCurrentBC^, FuncEntry, FuncType, ABI);
end;

procedure XprFreeClosure(var Closure: PXprClosureData);
begin
  if Closure = nil then Exit;
  if Assigned(Closure^.FFIClosure) then
    ffi_closure_free(Closure^.FFIClosure);
  if Assigned(Closure^.Interp) then
    FreeMem(Closure^.Interp);
  FreeMem(Closure);
  Closure := nil;
end;

finalization
  // Release any closures the script forgot to free.
  while GClosureCount > 0 do
  begin
    Dec(GClosureCount);
    XprFreeClosure(GClosures[GClosureCount]);
  end;
  GClosures := nil;

end.
