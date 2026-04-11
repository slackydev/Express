unit xpr.ffi;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL

  Foreign Function Interface for Express.

  Two directions:
    Import  - call a native C function from script via @native annotation
    Export  - give a script lambda to native code as a callback pointer

  Callback design:
    Each closure gets a dedicated TInterpreter (via NewForThread) plus a
    pre-built FFI CIF. ScanPrologue reads the bytecode prologue once at
    creation time and records the exact frame slot for each argument,
    result pointer, and captured variable. ExpressCallbackBinder then writes
    directly into those slots and jumps past the prologue, skipping all
    icNEWFRAME / icPOP / icPOPH dispatch overhead on every call.

  Thread safety:
    A single closure must not be called concurrently from multiple threads.
    Multiple distinct closures are safe - each has independent state.

  Context:
    Call XprSetCurrentContext before running any script that creates callbacks.
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

procedure XprSetCurrentContext(var Interp: TInterpreter; var BC: TBytecode);

// -- Type mapping --------------------------------------------------------------

function XprTypeToFFIType(T: EExpressBaseType): PFFIType;

// -- Import --------------------------------------------------------------------

type
  TXprNativeImport = record
    Func:            Pointer;
    Cif:             TFFICif;
    // ArgCount = total CIF arg count (= ExpressArgCount + 1 when FPCResult).
    // ExpressArgCount = number of args actually on the Express ArgStack.
    ArgCount:        Int32;
    ExpressArgCount: Int32;
    // ArgTypes[0..ArgCount-1].  Sized to 65: slot 0 may be FPCResult hidden ptr.
    ArgTypes:        array[0..64] of PFFIType;
    RetType:         PFFIType;
    HasReturn:       Boolean;
    // When True the return type is a FPC managed type (AnsiString / UnicodeString /
    // dynamic array).  FPC exports those with a hidden first parameter (a pointer to
    // the caller's result slot) and a void C-level return, rather than returning via
    // a register.  XprCallImport prepends that pointer automatically.
    FPCResult:       Boolean;
    // IsRef[i] - true when Express param i is pbRef.
    // The CIF entry is ffi_type_pointer; XprCallImport supplies an extra indirection
    // layer so ffi_call passes &(address-of-original) rather than (address-of-original).
    IsRef:           array[0..63] of Boolean;
  end;
  PXprNativeImport = ^TXprNativeImport;

function XprResolveImport(
  out   Import:   TXprNativeImport;
  const Lib:      string;
  const Sym:      string;
        FuncType: XType_Method;
        ABI:      TFFIABI = FFI_DEFAULT_ABI): Boolean;

procedure XprCallImport(
  var Interp: TInterpreter;
  var Import: TXprNativeImport);

procedure XprCallDynamicImport(
  var Interp: TInterpreter;
  var Import: TXprNativeImport);

// -- Export --------------------------------------------------------------------

type
  PXprClosureData = ^TXprClosureData;
  TXprClosureData = record
    Interp:         ^TInterpreter;
    BC:             ^TBytecode;
    FuncEntry:      Int32;
    Cif:            TFFICif;
    ArgCount:       Int32;
    ArgSizes:       array[0..63] of Int32;
    ArgTypes:       array[0..63] of EExpressBaseType;
    ArgIsRef:       array[0..63] of Boolean;  // true = pbRef param -> pointer-sized slot
    RetSize:        Int32;
    RetType:        EExpressBaseType;
    HasReturn:      Boolean;
    FFIClosure:     PFFIClosure;
    FFIFuncPtr:     Pointer;
    FFIArgTypes:    array[0..63] of PFFIType;
    // Captured outer variable refs
    CaptureCount:   Int32;
    CaptureRefs:    array[0..63] of Pointer;
    // Precomputed frame layout (filled by ScanPrologue)
    FrameSize:      Int32;
    BodyEntry:      Int32;              // first instruction after prologue
    ArgOffsets:     array[0..63] of Int32;
    RetOffset:      Int32;
    CaptureOffsets: array[0..63] of Int32;
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

function XprCreateClosureFromTypeInfo(
  FuncEntry: PtrInt;
  TypeInfo:  PXprCallbackTypeInfo): PXprClosureData;

function XprCreateClosure(
  var   MainInterp: TInterpreter;
  var   BC:         TBytecode;
        FuncEntry:  Int32;
        FuncType:   XType_Method;
        ABI:        TFFIABI = FFI_DEFAULT_ABI): PXprClosureData;

function XprCreateClosureFromRaw(
  FuncEntry: PtrInt;
  FuncType:  XType_Method;
  ABI:       TFFIABI = FFI_DEFAULT_ABI): PXprClosureData;

procedure XprFreeClosure(var Closure: PXprClosureData);

procedure XprRegisterClosure(Closure: PXprClosureData);
procedure XprUnregisterAndFreeClosure(FuncPtr: Pointer);

procedure ExpressCallbackBinder(
  var Cif:      TFFICif;
      Ret:      Pointer;
      Args:     PPointerArray;
      UserData: Pointer); cdecl;

function XprBuildImport(
  out   Import:   TXprNativeImport;
        Func:     Pointer;
        FuncType: XType_Method;
        ABI:      TFFIABI): Boolean;

function XprBuildImportCIF(
  out   Import:   TXprNativeImport;
        FuncType: XType_Method;
        ABI:      TFFIABI): Boolean;

function XprCCToABI(const CC: string): TFFIABI;

implementation

uses
  Math, xpr.Utils;

// -- Context -------------------------------------------------------------------

var
  GCurrentInterpreter: ^TInterpreter = nil;
  GCurrentBC:          ^TBytecode    = nil;

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
    if GClosures[i]^.FFIFuncPtr = FuncPtr then
    begin
      XprFreeClosure(GClosures[i]);
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

// -- Import -------------------------------------------------------------------

function XprBuildImport(
  out   Import:   TXprNativeImport;
        Func:     Pointer;
        FuncType: XType_Method;
        ABI:      TFFIABI): Boolean;
var
  i, paramStart, cifI: Int32;
  ArgPtrs:             PFFITypeArray;
begin
  FillChar(Import, SizeOf(Import), 0);
  Import.Func            := Func;
  Import.ExpressArgCount := FuncType.RealParamcount;
  Import.HasReturn       := (FuncType.ReturnType <> nil) and
                            (FuncType.ReturnType.BaseType <> xtUnknown);

  // -- Detect FPC managed return types ----------------------------------------
  // FPC does weird stuff here:
  // strings / dyn arrays aren't returned normally, it passes a hidden pointer
  // as first arg instead (result goes there, function returns void)
  //
  // also structs:
  // - win64: >8 bytes => hidden pointer
  // - sysv:  >16 bytes => same thing
  //
  // small structs go in registers but libffi can't deal with that properly
  // unless you define ffi_type_struct (not doing that here)
  //
  // workaround: just pass them by ref instead
  Import.FPCResult := Import.HasReturn and
    ((FuncType.ReturnType.BaseType in [xtAnsiString, xtUnicodeString, xtArray]) or
     ((FuncType.ReturnType.BaseType = xtRecord) and
      (FuncType.ReturnType.Size() > {$IFDEF MSWINDOWS}8{$ELSE}16{$ENDIF})));

  // -- Build CIF arg type list ------------------------------------------------
  cifI       := 0;
  paramStart := Length(FuncType.Params) - Import.ExpressArgCount;

  if Import.FPCResult then
  begin
    // Slot 0: hidden result pointer passed before all declared params
    Import.ArgTypes[0] := @ffi_type_pointer;
    cifI := 1;
  end;

  for i := 0 to Import.ExpressArgCount - 1 do
  begin
    // pbRef: C receives a pointer to the value.  ffi_type_pointer + RefStore.
    //
    // pbCopy xtRecord: on x86_64 (both Win64 and System V) structs larger than
    // 8/16 bytes are passed via an implicit pointer anyway, the caller provides
    // the address, the callee reads through it. bcPUSH already leaves &record_slot
    // on the ArgStack, so using the same RefStore indirection as pbRef gives
    // ffi_call the right address to pass as the pointer argument.
    // small records that fit in registers need an ffi_type_struct descriptor
    if (FuncType.Passing[paramStart + i] = pbRef) or
       (FuncType.Params[paramStart + i].BaseType = xtRecord) then
    begin
      Import.IsRef[i]       := True;
      Import.ArgTypes[cifI] := @ffi_type_pointer;
    end else
    begin
      Import.IsRef[i]       := False;
      Import.ArgTypes[cifI] := XprTypeToFFIType(FuncType.Params[paramStart + i].BaseType);
    end;
    Inc(cifI);
  end;

  Import.ArgCount := cifI;  // total CIF args (hidden + declared)

  // -- Return type -------------------------------------------------------------
  if Import.FPCResult then
    Import.RetType := @ffi_type_void   // hidden-param convention, void C return
  else if Import.HasReturn then
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

function XprBuildImportCIF(
  out   Import:   TXprNativeImport;
        FuncType: XType_Method;
        ABI:      TFFIABI): Boolean;
begin
  Result := XprBuildImport(Import, nil, FuncType, ABI);
end;

function XprCCToABI(const CC: string): TFFIABI;
begin
  case LowerCase(CC) of
    {$IFDEF CPU86}
    'stdcall':  Result := FFI_STDCALL;
    'cdecl':    Result := FFI_CDECL;
    'fastcall': Result := FFI_FASTCALL;
    'thiscall': Result := FFI_THISCALL;
    'pascal':   Result := FFI_PASCAL;
    {$ENDIF}
    {$IFDEF CPUX86_64}
      {$IFDEF UNIX}
      'unix64': Result := FFI_UNIX64;
      {$ENDIF}
      {$IFDEF MSWINDOWS}
      'win64':  Result := FFI_WIN64;
      {$ENDIF}
    {$ENDIF}
    'ffi': Result := FFI_DEFAULT_ABI;
  else
    Result := FFI_DEFAULT_ABI;
  end;
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

  Result := XprBuildImport(Import, Func, FuncType, ABI);
end;

procedure XprCallImport(var Interp: TInterpreter; var Import: TXprNativeImport);
var
  i, base, cifI: Int32;
  // +1 for possible FPCResult hidden arg, +64 for declared args
  ArgPtrs:       array[0..64] of Pointer;

  // need an extra level of indirection for ref params
  // ArgStack already has &value, but ffi_call expects a pointer to that
  // so we stash it in RefStore and pass &RefStore[i]
  RefStore:      array[0..63] of Pointer;
  RetPtr:        Pointer;
  HiddenArg:     Pointer;
begin
  // base = index of first Express arg in ArgStack
  base := Interp.ArgStack.Count - Import.ExpressArgCount;

  // Result slot pushed before args (one below base)
  if Import.HasReturn then
    RetPtr := Interp.ArgStack.Data[base - 1]
  else
    RetPtr := nil;

  cifI := 0;

  if Import.FPCResult then
  begin
    // FPC hidden out-param: pass the address of the result slot as the first
    // CIF argument.  ffi_call will read *ArgPtrs[0] = HiddenArg = RetPtr and
    // pass that pointer to the callee, which writes the result value there.
    HiddenArg   := RetPtr;
    ArgPtrs[0]  := @HiddenArg;
    cifI        := 1;
  end;

  for i := 0 to Import.ExpressArgCount - 1 do
  begin
    if Import.IsRef[i] then
    begin
      // The ArgStack slot already holds the address-of-original (pushed by
      // either bcPUSH on a value var or bcPUSHREF on a ref var - both result
      // in "address of the actual value" sitting in ArgStack.Data[base+i]).
      // For a pointer-typed CIF entry ffi_call reads *avalue[i], so we need
      // avalue[i] = &(address-of-original).  RefStore provides that level.
      RefStore[i]  := Interp.ArgStack.Data[base + i];
      ArgPtrs[cifI] := @RefStore[i];
    end else
      // Value param: ArgStack.Data[base+i] = address of value slot.
      // ffi reads *avalue[i] = the value itself.  Already correct.
      ArgPtrs[cifI] := Interp.ArgStack.Data[base + i];

    Inc(cifI);
  end;

  Dec(Interp.ArgStack.Count, Import.ExpressArgCount + Ord(Import.HasReturn));

  if Import.FPCResult then
    // Void C-level return; result delivered via hidden first arg
    ffi_call(Import.Cif, Import.Func, nil, PPointerArray(@ArgPtrs[0]))
  else if Import.ArgCount > 0 then
    ffi_call(Import.Cif, Import.Func, RetPtr, PPointerArray(@ArgPtrs[0]))
  else
    ffi_call(Import.Cif, Import.Func, RetPtr, nil);
end;

procedure XprCallDynamicImport(
  var Interp: TInterpreter;
  var Import: TXprNativeImport);
var
  FuncPtr: Pointer;
begin
  // func_ptr is on top of ArgStack; ArgStack.Data[i] is a pointer
  // to the variable's storage slot, so one dereference gets the value.
  FuncPtr := PPointer(Interp.ArgStack.Data[Interp.ArgStack.Count - 1])^;
  Dec(Interp.ArgStack.Count, 1);

  Import.Func := FuncPtr;
  XprCallImport(Interp, Import);
  Import.Func := nil;   // leave shared struct clean
end;

// -- Prologue scanner ----------------------------------------------------------
//
// Reads the bytecode prologue once at closure-creation time.
// Records frame-relative offsets for all args, result ptr, and captures,
//
// Prologue emitted by DelayedCompile (High downto 0 arg order):
//   bcNOOP / bcPASS
//   bcNEWFRAME [framesize]
//   bcPOPH / bcPOP × (CaptureCount + ArgCount + Ord(HasReturn))
//     captures first (implied args, highest index first)
//     then declared args (highest index first)
//     then result ptr last

procedure ScanPrologue(Data: PXprClosureData; var BC: TBytecode);
var
  pc:         Int32;
  instr:      ^TBytecodeInstruction;
  scanIdx:    Int32;
  totalPops:  Int32;
  allOffsets: array[0..129] of Int32;
  isRef:      array[0..129] of Boolean;
  capBase, argBase, resultScan, i: Int32;
begin
  pc := Data^.FuncEntry;
  Inc(pc); // skip bcNOOP (was icPASS)

  // bcNEWFRAME
  Data^.FrameSize := BC.Code.Data[pc].Args[0].Data.Addr;
  Inc(pc);

  // Collect all POP/POPH frame offsets in scan order
  scanIdx := 0;
  while (pc < BC.Code.Size) and
        (BC.Code.Data[pc].Code in [bcPOP, bcPOPH]) do
  begin
    instr := @BC.Code.Data[pc];
    if instr^.Code = bcPOP then
    begin
      allOffsets[scanIdx] := instr^.Args[1].Data.Addr;
      isRef[scanIdx]      := False;
    end else
    begin
      allOffsets[scanIdx] := instr^.Args[0].Data.Addr;
      isRef[scanIdx]      := True;
    end;
    Inc(scanIdx);
    Inc(pc);
  end;

  totalPops := scanIdx;

  // Derive CaptureCount from scan - TypeInfo doesn't carry it
  // total = CaptureCount + ArgCount + Ord(HasReturn)
  Data^.CaptureCount := totalPops - Data^.ArgCount - Ord(Data^.HasReturn);
  if Data^.CaptureCount < 0 then Data^.CaptureCount := 0;

  // Scan order: captures[High..0], args[High..0], result
  capBase    := 0;
  argBase    := Data^.CaptureCount;
  resultScan := Data^.CaptureCount + Data^.ArgCount;

  for i := 0 to Data^.CaptureCount - 1 do
    Data^.CaptureOffsets[Data^.CaptureCount - 1 - i] := allOffsets[capBase + i];

  for i := 0 to Data^.ArgCount - 1 do
    Data^.ArgOffsets[Data^.ArgCount - 1 - i] := allOffsets[argBase + i];

  if Data^.HasReturn and (resultScan < totalPops) then
    Data^.RetOffset := allOffsets[resultScan];

  Data^.BodyEntry := pc;
end;

// -- Callback binder -----------------------------------------------------------

procedure ExpressCallbackBinder(
  var Cif:      TFFICif;
      Ret:      Pointer;
      Args:     PPointerArray;
      UserData: Pointer); cdecl;
var
  Data:   PXprClosureData;
  Interp: ^TInterpreter;
  Frame:  PByte;
  i:      Int32;
begin
  Data   := PXprClosureData(UserData);
  Interp := Data^.Interp;

  // Build the frame directly - replaces bcNEWFRAME dispatch
  Frame            := @Interp^.Data[0];
  FillByte(Frame^, Data^.FrameSize + SizeOf(Pointer), 0);
  Interp^.BasePtr  := Frame;
  Interp^.StackPtr := Frame + Data^.FrameSize;

  // Write args directly into their precomputed frame slots - replaces bcPOP
  for i := 0 to Data^.ArgCount - 1 do
    if Data^.ArgIsRef[i] then
      // Ref param: ffi gives us avalue[i] = pointer to a Pointer (the ref value).
      // The Express ref frame slot must hold the pointer itself (address of original).
      // ArgSizes[i] = SizeOf(Pointer) for ref params (set in XprCreateClosure).
      PPointer(Frame + Data^.ArgOffsets[i])^ := PPointer(Args^[i])^
    else
      Move(Args^[i]^, (Frame + Data^.ArgOffsets[i])^, Data^.ArgSizes[i]);

  // Write result pointer into its slot - replaces bcPOPH
  if Data^.HasReturn then
    PPointer(Frame + Data^.RetOffset)^ := Ret;

  // Write captured refs into their slots - replaces bcPOPH for captures
  for i := 0 to Data^.CaptureCount - 1 do
    PPointer(Frame + Data^.CaptureOffsets[i])^ := Data^.CaptureRefs[i];

  // Reset minimal execution state
  Interp^.ArgStack.Count := 0;
  Interp^.CallStack.Top  := -1;
  Interp^.TryStack.Top   := -1;
  Interp^.RecursionDepth := 0;
  Interp^.RunCode        := 1;

  // Sentinel frame - direct write, no Push() call overhead
  Interp^.CallStack.Top                        := 0;
  Interp^.CallStack.Frames[0].ReturnAddress    := nil;
  Interp^.CallStack.Frames[0].StackPtr         := Interp^.StackPtr;
  Interp^.CallStack.Frames[0].FrameBase        := Frame;
  Interp^.CallStack.Frames[0].FunctionHeaderPC := 0;

  // Jump past prologue - straight to first real instruction
  Interp^.ProgramCounter := Data^.BodyEntry;
  Interp^.Run(Data^.BC^);
end;

// -- Closure creation ----------------------------------------------------------

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
      Data^.ArgTypes[i]  := TypeInfo^.ArgTypes[i];
      Data^.ArgIsRef[i]  := False;  // TypeInfo has no passing info; treat all as value
      Data^.ArgSizes[i]  := TypeInfo^.ArgSizes[i];
      Data^.FFIArgTypes[i] := XprTypeToFFIType(TypeInfo^.ArgTypes[i]);
    end;

    // Scan prologue - derives CaptureCount, FrameSize, BodyEntry, ExitEntry,
    // ArgOffsets, RetOffset, CaptureOffsets
    ScanPrologue(Data, GCurrentBC^);

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

    Data^.FFIFuncPtr := FuncPtr;
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
    Data^.Interp^ := TInterpreter.NewForThread(MainInterp, FuncEntry, BC.Code.Size);
    Data^.BC        := @BC;
    Data^.FuncEntry := FuncEntry;
    Data^.ArgCount  := FuncType.RealParamcount;
    Data^.HasReturn := (FuncType.ReturnType <> nil) and
                       (FuncType.ReturnType.BaseType <> xtUnknown);

    paramStart := Length(FuncType.Params) - Data^.ArgCount;
    for i := 0 to Data^.ArgCount - 1 do
    begin
      Data^.ArgTypes[i]  := FuncType.Params[paramStart + i].BaseType;
      Data^.ArgIsRef[i]  := (FuncType.Passing[paramStart + i] = pbRef);
      if Data^.ArgIsRef[i] then
      begin
        // Ref param: C caller passes a pointer.  CIF type = pointer;
        // frame slot must receive the pointer value (not the pointed-to value).
        Data^.ArgSizes[i]    := SizeOf(Pointer);
        Data^.FFIArgTypes[i] := @ffi_type_pointer;
      end else
      begin
        Data^.ArgSizes[i]    := XprTypeSize[Data^.ArgTypes[i]];
        Data^.FFIArgTypes[i] := XprTypeToFFIType(Data^.ArgTypes[i]);
      end;
    end;

    ScanPrologue(Data, BC);

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
      FreeMem(Data^.Interp); FreeMem(Data); Exit;
    end;

    Data^.FFIClosure := ffi_closure_alloc(SizeOf(TFFIClosure), FuncPtr);
    if Data^.FFIClosure = nil then
    begin
      FreeMem(Data^.Interp); FreeMem(Data); Exit;
    end;

    if ffi_prep_closure_loc(
         Data^.FFIClosure^, Data^.Cif,
         @ExpressCallbackBinder, Data, FuncPtr) <> FFI_OK then
    begin
      ffi_closure_free(Data^.FFIClosure);
      FreeMem(Data^.Interp); FreeMem(Data); Exit;
    end;

    Data^.FFIFuncPtr := FuncPtr;
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

// -- Closure free --------------------------------------------------------------

procedure XprFreeClosure(var Closure: PXprClosureData);
begin
  if Closure = nil then Exit;
  if Assigned(Closure^.FFIClosure) then
    ffi_closure_free(Closure^.FFIClosure);
  if Assigned(Closure^.Interp) then
  begin
    SetLength(Closure^.Interp^.Data,              0);  // thread stack
    SetLength(Closure^.Interp^.CallStack.Frames,  0);  // call stack
    SetLength(Closure^.Interp^.TryStack.Frames,   0);  // try stack
    SetLength(Closure^.Interp^.ArgStack.Data,     0);  // arg stack
    FreeMem(Closure^.Interp);
  end;
  FreeMem(Closure);
  Closure := nil;
end;

finalization
  while GClosureCount > 0 do
  begin
    Dec(GClosureCount);
    XprFreeClosure(GClosures[GClosureCount]);
  end;
  GClosures := nil;
end.
