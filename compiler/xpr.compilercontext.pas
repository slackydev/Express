unit xpr.CompilerContext;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
{$I header.inc}
{.$hints OFF}
interface

uses
  SysUtils,
  xpr.Dictionary,
  xpr.Types,
  xpr.Tokenizer,
  xpr.Intermediate,
  xpr.Errors;

const
  STACK_ITEM_ALIGN = 8; //each element in stack takes up 8 bytes no matter - no option.

type
  EInstructionArg = (ia1, ia2, ia3, ia4, ia5, ia6);

  TIntrinsics = class(TObject)
  end;

  TCompilerContext = class;

  XType = class(TObject)
    BaseType: EExpressBaseType;
    Name: string;
    TypeOfExpr: Pointer; {XTree_Node}

    constructor Create(ABaseType: EExpressBaseType=xtUnknown);
    destructor Destroy; override;
    function Size(): SizeInt; virtual;
    function Hash(): string; virtual;
    function EvalCode(OP: EOperator; Other: XType): EIntermediate; virtual;
    function CanAssign(Other: XType): Boolean; virtual;
    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; virtual;
    function Equals(Other: XType): Boolean;  virtual; reintroduce;
    function ToString(): string; override;
    function IsManagedType(ctx: TCompilerContext): Boolean; inline;
  end;
  XTypeArray = array of XType;

  TXprVar = record // might be best to make this a class, records are clumpsy here
    VarType: XType;
    Addr: Int64;
    MemPos: EMemPos;

    Reference: Boolean;    // ref arguments for example
    IsGlobal: Boolean;     // scope = zero

    IsTemporary: Boolean;   // Difference is that temp vars are **unmanaged**
    NestingLevel: Integer;  // scope = nonlocal
    NonWriteable: Boolean;  // constant
    IsBorrowedRef: Boolean; // temp holds a borrowed pointer, does not own the allocation

    constructor Create(AType: XType; AAddr: PtrInt=0; AMemPos: EMemPos=mpLocal);
    function IfRefDeref(ctx: TCompilerContext): TXprVar;
    function DerefToTemp(ctx: TCompilerContext): TXprVar;
    function Deref(ctx: TCompilerContext; Dest: TXprVar): TXprVar;
    function IsManaged(ctx: TCompilerContext): Boolean;
    function InCurrentScope(ctx: TCompilerContext): Boolean;
  end;

  TNamedVar = record
    Name: string;
    XprVar: TXprVar;
  end;

  TNameScopeTrack = record
    VarScope: Int32;
    Name: string;
  end;
  TNameScopeTrackList = specialize TArrayList<TNameScopeTrack>;

  TXprVarList = specialize TArrayList<TXprVar>;
  TVarList    = specialize TArrayList<TNamedVar>;
  TStringToObject = specialize TDictionary<string, XType>;
  TXprTypeList = specialize TArrayList<XType>;

  { XTree_Node }
  XTree_Node = class(TObject)
    FDocPos:  TDocPos;
    FContext: TCompilerContext;
    FResType: XType;
    FResTypeHint: XType;
    FSettings: TCompilerSettings;

    constructor Create(ACTX: TCompilerContext; DocPos: TDocPos); virtual;
    destructor Destroy; override;
    function Emit(instr: EIntermediate; args: array of TXprVar; Doc: TDocPos): SizeInt;
    function Emit(instr: EIntermediate; Doc: TDocPos): SizeInt; overload;
    function Emit(Opcode: TInstruction; Doc: TDocPos): SizeInt;

    function ToString(offset:string=''): string; virtual; reintroduce;

    function ResType(): XType; virtual;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; virtual;
    function CompileLValue(Dest: TXprVar): TXprVar; virtual;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; virtual;
    procedure SetResTypeHint(AHintType: XType); virtual;
    function Copy(): XTree_Node; virtual;

    property ctx: TCompilerContext read FContext write FContext;
  end;
  XNodeList  = specialize TArrayList<XTree_Node>;
  XNodeArray = array of XTree_Node;
  XTypeList  = specialize TArrayList<XType>;

  TGenericMethods = specialize TDictionary<string, XTree_Node>;
  TNodeMemManagement = specialize TDictionary<PtrUInt, XTree_Node>;
  TTypeMemManagement = specialize TDictionary<PtrUInt, XType>;

  // === Compiler context ===
  TCompiledFile = specialize TDictionary<string, XTree_Node>;

  TScopedVars  = array of TVarDeclDictionary;
  TScopedTypes = array of TStringToObject;

  TMiniContext = class(TObject)
  public
    Vars:  TScopedVars;
    Types: TScopedTypes;
    Stack: array of SizeInt;
    NameScopeDepth: Int32;

    CompilingStack: XStringList;
    NamespaceStack: XStringList;

    destructor Destroy; override;
  end;

  TExceptionHandler = record
    VarName: string;
    ExceptionType: XType;
    Body: XTree_Node;
  end;
  TExceptionHandlerArray = array of TExceptionHandler;


  TCaseBranch = record
    Labels: specialize TArrayList<XTree_Node>; // Using a list to support `1, 2, 3: ...`
    Body: XTree_Node;
  end;
  TCaseBranchArray = array of TCaseBranch;

  TCompilerContext = class(TObject)
  public
    MainFileContents: string;

    FSettings: TCompilerSettings;
    FCompilingStack: XStringList;
    FNamespaceStack: XStringList;
    FCurrentMethodStack: XTypeList;
    FUnitASTCache: TCompiledFile;
    FSettingOverride: specialize TArrayList<TCompilerSettings>;
    FReturnPatchList: array of PtrInt;

    LibrarySearchPaths: XStringList;
    Intermediate: TIntermediateCode;

    Variables: TXprVarList;
    Constants: TXprVarList;

    StringConstMap: TStringToIntDict;

    Scope: SizeInt;
    NameScopeDepth: Int32;
    NameScopeStack: array of TNameScopeTrackList;

    // these relate to current scope
    VarDecl:  TScopedVars;
    TypeDecl: TScopedTypes;
    StackPosArr: array of SizeInt;

    // needed for break/continue finalization
    LoopScopeStack: specialize TArrayList<SizeInt>;

    //
    GenericMap: TGenericMethods;
    //
    TypeIntrinsics: TIntrinsics;
    DelayedNodes: XNodeArray;

    PatchPositions: specialize TArrayList<PtrInt>;

    // auto managed memory
    ManagedTypes: TTypeMemManagement;
    ManagedNodes: TNodeMemManagement;

  {methods}
    constructor Create(FileContents: string='');
    destructor Destroy; override;

    procedure PushNameScope();
    procedure PopNameScope();
    procedure TrackNameScope(AVarScope: Int32; const AName: string);

    procedure PushLoopScope();
    procedure PopLoopScope();

    procedure ImportUnit(UnitPath, UnitAlias: string; DocPos: TDocPos);
    procedure DelayedImportUnit(UnitPath, UnitAlias: string; DocPos: TDocPos);

    function GetMiniContext(): TMiniContext;
    procedure SetMiniContext(MCTX: TMiniContext);


    // stack
    function FrameSize(): SizeInt;
    procedure IncScope();
    procedure DecScope(RestoreStack: Boolean = False);

    procedure SyncStackPosMax(Into, From: Int32);
    function IsInsideFunction(AltScope: Int32 = -1): Boolean;
    function FunctionNestingDepth(): Int32;
    function FunctionScopeLevel(): Int32;

    function  GetStackPos(): SizeInt;    {$ifdef xinline}inline;{$endif}
    procedure IncStackPos(Size:Int32=STACK_ITEM_ALIGN); {$ifdef xinline}inline;{$endif}
    procedure DecStackPos(Size:Int32=STACK_ITEM_ALIGN); {$ifdef xinline}inline;{$endif}

    // namespace
    function GetCurrentNamespace: string;

    procedure PushCurrentMethod(Method: XType);
    function PopCurrentMethod(): XType;
    function GetCurrentMethod(): XType;

    // ir code generation
    function CodeSize(): SizeInt;     {$ifdef xinline}inline;{$endif}

    function  Emit(Opcode: TInstruction; Pos: TDocPos; Setting: TCompilerSettings): PtrInt;
    procedure PatchArg(Pos: SizeInt; ArgID:EInstructionArg; NewArg: PtrInt);
    procedure PatchJump(Addr: PtrInt; NewAddr: PtrInt=0);
    function  RelAddr(Addr: PtrInt): TXprVar;
    function PushFunction(VarAddr: PtrInt): Int32;
    function PushVirtualMethod(VarAddr: PtrInt; ClassID, VMTIndex: Int32): Int32;

    procedure PreparePatch;
    procedure PopPatch;
    procedure RunPatch(PlaceholderOp: EIntermediate; TargetAddr: PtrInt);

    procedure EmitReturnJump(DocPos: TDocPos);
    procedure PatchReturnJumps();
    procedure EmitFinalizeScope(Flags: TCompilerFlags);
    procedure EmitScopeCleanupTo(StopAtScope: SizeInt);

    { Returns the current count of allocated compiler variables.
      Call this immediately before compiling each statement to establish
      a high-water mark. }
    function TempHighWater(): Int32;

    { Emits Collect calls for every temporary managed variable allocated
      since the given high-water mark that has not been claimed by an
      assignment (i.e. IsTemporary is still True).
      Call this immediately after compiling each statement. }
    procedure EmitAbandonedTempCleanup(HighWater: Int32);

    // docpos
    function CurrentDocPos(): TDocPos;

    // memory management
    procedure AddManagedType(Typ: XType);
    procedure AddManagedNode(Node: XTree_Node);

    // ------------------------------------------------------
    function TryGetLocalVar(Name: string): TXprVar;
    function TryGetGlobalVar(Name: string): TXprVar;

    function GetVar(Name: string; Pos:TDocPos): TXprVar;
    function TryGetVar(Name: string): TXprVar;

    function GetVarList(Name: string): TXprVarList;
    function GetTempVar(Typ: XType): TXprVar;

    // ------------------------------------------------------
    function GetType(Name: string; Pos: TDocPos): XType;
    function GetType(BaseType: EExpressBaseType; Pos: TDocPos): XType;
    function GetType(Name: string): XType;
    function GetType(BaseType: EExpressBaseType): XType;

    // ------------------------------------------------------

    function AddClass(Name: string; Typ: XType): TVirtualMethodTable;
    procedure AddType(Name: string; Typ:XType; Manage:Boolean=False);

    function RegConst(Value: TXprVar): Int32; overload;
    function RegConst(constref Value: TConstant): TXprVar;
    function RegConst(Value: Boolean):  TXprVar; overload;
    function RegConst(Value: Int64):  TXprVar; overload;
    function RegConst(Value: Double): TXprVar; overload;
    function RegConst(Value: Pointer): TXprVar; overload;
    function RegConst(const Value: string): TXprVar;

    function ManageTempVar(var Value: TXprVar; DocPos: TDocPos): Int32;
    function RegVar(Name: string; var Value: TXprVar; DocPos: TDocPos; GlobalInLocal: Boolean = False): Int32; overload;
    function RegVar(Name: string; VarType: XType; DocPos: TDocPos; out Index: Int32): TXprVar; overload;
    function RegVar(Name: string; VarType: XType; DocPos: TDocPos): TXprVar; overload;

    function RegGlobalVar(Name: string; var Value: TXprVar; DocPos: TDocPos): Int32;
    function RegMethod(Name: string; var Value: TXprVar; DocPos: TDocPos): Int32;

    // value addition
    function AddVar(Value: Variant; Name: string; VarType:XType): TXprVar;
    function AddExternalVar(Addr: Pointer; Name: string; VarType:XType): TXprVar;
    function AddExternalFunc(Addr: TExternalProc; Name: string; Params: array of XType; PassBy: array of EPassBy; ResType: XType): TXprVar;
    function AddExternalFunc(Addr: TExternalFunc; Name: string; Params: array of XType; PassBy: array of EPassBy; ResType: XType): TXprVar; overload;
    function AddExternalMethod(Addr: TExternalProc; Name: string; SelfType: XType; Params: array of XType; PassBy: array of EPassBy; ResType: XType): TXprVar;
    function AddExternalMethod(Addr: TExternalFunc; Name: string; SelfType: XType; Params: array of XType; PassBy: array of EPassBy; ResType: XType): TXprVar;

    // helper
    function IsManagedRecord(ARec: XType): Boolean;
    procedure EmitFinalizeVar(VarToFinalize: TXprVar; ForceGlobal:Boolean=False);
    procedure EmitCollect(VarToFinalize: TXprVar);
    procedure EmitDecref(VarToDecref: TXprVar);
    procedure EmitIncref(VarToIncref: TXprVar);
    function EmitUpcastIfNeeded(VarToCast: TXprVar; TargetType: XType; DerefIfUpcast:Boolean): TXprVar;
    procedure VarToDefault(TargetVar: TXprVar);
    procedure EmitRangeCheck(ArrVar, IndexVar, ExceptionVar: TXprVar; DocPos: TDocPos);

    function GetManagedDeclarations(): TXprVarList;
    function GetGlobalManagedDeclarations(): TXprVarList;
    function GetClosureVariables(): TVarList;

    function GenerateIntrinsics(Name: string; Arguments: array of XType; SelfType: XType; CompileAs: string = ''): XTree_Node;
    function ResolveMethod(Name: string; Arguments: XTypeArray; SelfType, RetType: XType; DocPos:TDocPos): TXprVar;
    procedure ResolveToFinalType(var PseudoType: XType);

    // Extending exceptions
    function GetLineString(DocPos: TDocPos): string;
    procedure RaiseException(Msg:string);
    procedure RaiseException(Msg:string; DocPos: TDocPos);
    procedure RaiseExceptionFmt(Msg:string; Args: array of const; DocPos: TDocPos);
    procedure RaiseException(Typ:EExceptionType; Msg:string; DocPos: TDocPos);
    procedure RaiseExceptionFmt(Typ:EExceptionType; Msg:string; Args: array of const; DocPos: TDocPos);

    // ------------------------------------------------------
    procedure RegisterInternals;
    procedure ProcessDirective(const Directive: string);
    procedure PushSettingOverride(setting: TCompilerSettings);
    function PopSettingOverride(): TCompilerSettings;
    function CurrentSetting(Default: TCompilerSettings): TCompilerSettings;

    // ------------------------------------------------------
    property StackPos: SizeInt read GetStackPos;
    property CurrentNamespace: string read GetCurrentNamespace;
  end;


const
  NullVar:    TXprVar = (VarType:nil; Addr:0; MemPos:mpImm; Reference:False; IsGlobal:False; IsTemporary: False; NestingLevel: 0; NonWriteable:False);
  NullResVar: TXprVar = (VarType:nil; Addr:0; MemPos:mpImm; Reference:False; IsGlobal:False; IsTemporary: False; NestingLevel: 0; NonWriteable:False);

  GLOBAL_SCOPE = 0;

function GetInstr(OP: EIntermediate; args: array of TXprVar): TInstruction;
function GetInstr(OP: EIntermediate): TInstruction;
function STORE_FAST(Left, Right: TXprVar; Heap: Boolean): TInstruction; {$ifdef xinline}inline;{$endif}

function External(v: PtrInt; Typ: XType = nil): TXprVar;
function Immediate(v: PtrInt; Typ: XType = nil): TXprVar;
function OpAddr(v: PtrInt; loc:EMemPos=mpHeap): TXprVar;

operator =  (L,R: TXprVar): Boolean;
operator <> (L,R: TXprVar): Boolean;
operator + (left: XNodeArray; Right: XNodeArray): XNodeArray;
operator + (left: XTypeArray; Right: XType): XTypeArray;

implementation

uses
  Math,LazFileUtils,
  xpr.Vartypes,
  xpr.Langdef,
  xpr.Tree,
  xpr.TypeIntrinsics,
  xpr.Utils,
  xpr.Parser,
  xpr.MagicIntrinsics;

type
  TEncodedVar = record
    Depth: Int32;
    Index: Int32;
  end;

function EncodeVarIndex(Depth: Int32; Index: Int32): Int64; inline;
begin
  Result := (Int64(Depth) shl 32) or UInt32(Index);
end;

function DecodeVarIndex(Encoded: Int64): TEncodedVar; inline;
begin
  Result.Depth := Int32(Encoded shr 32);
  Result.Index := Int32(Encoded and $FFFFFFFF);
end;

destructor TMiniContext.Destroy;
var i: Int32;
begin
  // Scope 0 (global) is shared with the context — never free it here
  for i := 1 to High(Vars) do
    Vars[i].Free;
  for i := 1 to High(Types) do
    Types[i].Free;
  inherited;
end;

(*
  Tries to find a unit file on disk using a prioritized search path.
  Returns the canonical, absolute path if found, or an empty string otherwise.
*)
function ResolveUnitPath(const UnitPath: string; const ImportingFileDir: string;
                         const LibraryPaths: XStringList): string;
var
  TestPath: string;
  i: Integer;
begin
  // Check if the path is already absolute.
  if FilenameIsAbsolute(UnitPath) then
  begin
    if FileExists(UnitPath) then
      Exit(ExpandFileName(UnitPath));
    Exit('');
  end;

  // Check relative to the directory of the file that is importing it.
  TestPath := CreateAbsoluteSearchPath(UnitPath, ImportingFileDir);
  if FileExists(TestPath) then
    Exit(ExpandFileName(TestPath));

  // Check in each of the registered library search paths.
  for i := 0 to LibraryPaths.High do
  begin
    TestPath := CreateAbsoluteSearchPath(UnitPath, LibraryPaths.Data[i]);
    if FileExists(TestPath) then
      Exit(ExpandFileName(TestPath));
  end;

  // Check relative to the current working directory.
  TestPath := CreateAbsoluteSearchPath(UnitPath, GetCurrentDir());
  if FileExists(TestPath) then
    Exit(ExpandFileName(TestPath));

  // Err Not found
  Result := '';
end;


(*~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~*)

constructor TCompilerContext.Create(FileContents: string='');
begin
  Self.MainFileContents := FileContents;

  Intermediate.Init();
  Constants.Init([]);
  Variables.Init([]);
  PatchPositions.Init([]);
  LibrarySearchPaths.Init([]);
  FSettingOverride.Init([]);
  LoopScopeStack.Init([]);
  DelayedNodes :=[];

  ManagedNodes := TNodeMemManagement.Create(@HashPointer);
  ManagedTypes := TTypeMemManagement.Create(@HashPointer);

  StringConstMap := TStringToIntDict.Create(@HashStr); // Create the map
  SetLength(Intermediate.StringTable, 0); // Initialize the array

  Self.TypeIntrinsics := TTypeIntrinsics.Create(Self, NoDocPos);
  Self.GenericMap := TGenericMethods.Create(@HashStr);

  FNamespaceStack.Init([]);
  FCompilingStack.Init([]);
  FCurrentMethodStack.Init([]);

  FUnitASTCache   := TCompiledFile.Create(@HashStr);

  NameScopeDepth := 0;
  Scope := -1;
  IncScope();
  Self.RegisterInternals;
end;

destructor TCompilerContext.Destroy;
var
  i, j: Int32;
  node: XTree_Node;
begin
  DecScope();
  GenericMap.Free;
  TypeIntrinsics.Free;
  StringConstMap.Free;
  FUnitASTCache.Free;

  for i := 0 to Intermediate.ClassVMTs.High do
    Intermediate.ClassVMTs.Data[i].Free;

  for i := 0 to ManagedTypes.RealSize - 1 do
    for j := 0 to High(ManagedTypes.Items[i]) do
    begin
      ManagedTypes.Items[i][j].val.Free;
    end;
  ManagedTypes.Free;

  DelayedNodes :=[];

  for i := 0 to ManagedNodes.RealSize - 1 do
    for j := 0 to High(ManagedNodes.Items[i]) do
    begin
      node := ManagedNodes.Items[i][j].val;
      node.FContext := nil;  // prevents Remove() call in Destroy, which would corrupt iteration
      node.Free;
    end;
  ManagedNodes.Free;
end;

procedure TCompilerContext.PushNameScope();
begin
  Inc(NameScopeDepth);
  if NameScopeDepth >= Length(NameScopeStack) then
    SetLength(NameScopeStack, NameScopeDepth + 16);
  NameScopeStack[NameScopeDepth].Init([]);
end;

procedure TCompilerContext.PopNameScope();
var
  i, j: Int32;
  Track: TNameScopeTrack;
  declList: XIntList;
  Decoded: TEncodedVar;
begin
  if NameScopeDepth <= 0 then Exit;

  for i := 0 to NameScopeStack[NameScopeDepth].High do
  begin
    Track := NameScopeStack[NameScopeDepth].Data[i];
    if VarDecl[Track.VarScope].Get(Track.Name, declList) then
    begin
      for j := 0 to declList.High do
      begin
        Decoded := DecodeVarIndex(declList.Data[j]);
        if Decoded.Depth = NameScopeDepth then
        begin
          declList.Data[j] := EncodeVarIndex(MaxInt, Decoded.Index);
        end;
      end;
      VarDecl[Track.VarScope][Track.Name] := declList;
    end;
  end;

  NameScopeStack[NameScopeDepth].Init([]);
  Dec(NameScopeDepth);
end;

procedure TCompilerContext.TrackNameScope(AVarScope: Int32; const AName: string);
var
  Track: TNameScopeTrack;
begin
  if NameScopeDepth > 0 then
  begin
    Track.VarScope := AVarScope;
    Track.Name := AName;
    NameScopeStack[NameScopeDepth].Add(Track);
  end;
end;

procedure TCompilerContext.PushLoopScope();
begin
  LoopScopeStack.Add(NameScopeDepth);
  Inc(FSettings.LoopDepth);
end;

procedure TCompilerContext.PopLoopScope();
begin
  LoopScopeStack.Pop();
  Dec(FSettings.LoopDepth);
end;

function TCompilerContext.GetCurrentNamespace: string;
begin
  if FNamespaceStack.High >= 0 then
    Result := FNamespaceStack.Data[FNamespaceStack.High]
  else
    Result := '';
end;

procedure TCompilerContext.PushCurrentMethod(Method: XType);
begin
  Self.FCurrentMethodStack.Add(Method);
end;

function TCompilerContext.PopCurrentMethod(): XType;
begin
  Result := Self.FCurrentMethodStack.Pop();
end;

function TCompilerContext.GetCurrentMethod: XType;
begin
  if Self.FCurrentMethodStack.High >= 0 then
    Result := Self.FCurrentMethodStack.Data[Self.FCurrentMethodStack.High]
  else
    Result := nil;
end;


// This is the new, simplified import logic.
procedure TCompilerContext.ImportUnit(UnitPath, UnitAlias: string; DocPos: TDocPos);
var
  UnitAST: XTree_Node;
  UnitCode, ResolvedPath, ImportingFileDir: string;
  UnitTokenizer: TTokenizer;
  i: Int32;
begin
  UnitAlias := Xprcase(UnitAlias);

  if FCompilingStack.High >= 0 then
    ImportingFileDir := ExtractFileDir(FCompilingStack.Data[FCompilingStack.High])
  else
    ImportingFileDir := GetCurrentDir;

  ResolvedPath := ResolveUnitPath(UnitPath, ImportingFileDir, Self.LibrarySearchPaths);

  if ResolvedPath = '' then
    RaiseExceptionFmt('Cannot find unit file: `%s`', [UnitPath], DocPos);

  for i := 0 to FCompilingStack.High do
    if FCompilingStack.Data[i] = ResolvedPath then
      RaiseExceptionFmt('Circular import detected: `%s` is already being compiled.',[ResolvedPath], DocPos);

  // 1. Check the AST cache. If not found, parse the unit and cache the result.
  // XXX: Broken
  if (not FUnitASTCache.Get(ResolvedPath+':'+CurrentNamespace+UnitAlias, UnitAST)) then
  begin
    UnitCode := LoadFileContents(ResolvedPath);
    if UnitCode = '' then
      RaiseExceptionFmt('Cannot find or read unit file: %s', [ResolvedPath], DocPos);

    UnitTokenizer := Tokenize(ResolvedPath, UnitCode);
    try
      UnitAST := Parse(UnitTokenizer, Self);
    finally
      UnitTokenizer := Default(TTokenizer);  // force finalize strings/arrays now
    end;

    FUnitASTCache.Add(UnitPath+':'+CurrentNamespace+UnitAlias, UnitAST)
  end else
  begin

    Exit;
  end;

  FCompilingStack.Add(ResolvedPath);
  if UnitAlias <> '' then
    FNamespaceStack.Add({CurrentNamespace + }UnitAlias + '::');

  try
    // The AST's compile methods will now automatically use the new prefix.
    UnitAST.Compile(NullResVar, [cfRootBody]);
  finally
    // 3. Pop the namespace prefix off the stack when done.
    if UnitAlias <> '' then FNamespaceStack.Pop();
  end;
  FCompilingStack.Pop();
end;

// In TCompilerContext implementation
procedure TCompilerContext.DelayedImportUnit(UnitPath, UnitAlias: string; DocPos: TDocPos);
var
  UnitAST: XTree_Node;
begin
  UnitAlias := Xprcase(UnitAlias);

  if (not FUnitASTCache.Get(UnitPath+':'+CurrentNamespace+UnitAlias, UnitAST)) then
    RaiseExceptionFmt('Internal compiler error: AST for unit `%s` not found during delayed compile.',[UnitPath+':'+CurrentNamespace+UnitAlias], DocPos);

  if UnitAlias <> '' then FNamespaceStack.Add({CurrentNamespace + }UnitAlias + '::');
  try
    UnitAST.DelayedCompile(NullResVar, [cfRootBody]);
  finally
    if UnitAlias <> '' then FNamespaceStack.Pop();
  end;
end;


// not really good enough..
// we probably want a fully CTX state copy
// or some better design choices...
function TCompilerContext.GetMiniContext(): TMiniContext;
var
  i: Int32;
begin
  Result := TMiniContext.Create();

  SetLength(Result.vars,  Scope+1);
  SetLength(Result.types, Scope+1);
  SetLength(Result.stack, Scope+1);
  Result.NameScopeDepth := Self.NameScopeDepth;

  Result.CompilingStack.Init(Self.FCompilingStack.RawOfManaged());
  Result.NamespaceStack.Init(Self.FNamespaceStack.RawOfManaged());

  Result.vars[GLOBAL_SCOPE]  := VarDecl[GLOBAL_SCOPE];
  Result.types[GLOBAL_SCOPE] := TypeDecl[GLOBAL_SCOPE];
  Result.stack[GLOBAL_SCOPE] := StackPosArr[GLOBAL_SCOPE];

  for i:=1 to Scope do
  begin
    Result.vars[i]   := VarDecl[i].Copy();
    Result.types[i]  := TypeDecl[i].Copy();
    Result.stack[i]  := StackPosArr[i];
  end;
end;

procedure TCompilerContext.SetMiniContext(MCTX: TMiniContext);
var
  i: Int32;
begin
  Self.Scope := High(MCTX.vars);
  Self.NameScopeDepth := MCTX.NameScopeDepth;

  Self.FCompilingStack.Init(MCTX.CompilingStack.RawOfManaged());
  SElf.FNamespaceStack.Init(MCTX.NamespaceStack.RawOfManaged());

  SetLength(Self.VarDecl, Scope+1);
  SetLength(Self.TypeDecl, Scope+1);
  SetLength(Self.StackPosArr, Scope+1);

  for i:=1 to Scope do
  begin
    Self.VarDecl[i].Free;
    Self.TypeDecl[i].Free;
    Self.VarDecl[i]           := MCTX.vars[i].Copy();
    Self.TypeDecl[i]          := MCTX.types[i].Copy();
    Self.StackPosArr[i]       := MCTX.Stack[i];
  end;
end;


// ----------------------------------------------------------------------------
//
function TCompilerContext.FrameSize(): SizeInt;
begin
  result := StackPosArr[Scope];
end;

procedure TCompilerContext.IncScope();
begin
  Inc(Scope);
  SetLength(StackPosArr, Scope+1);
  SetLength(VarDecl,     Scope+1);
  SetLength(TypeDecl,    Scope+1);

  if Scope = GLOBAL_SCOPE then
  begin
    VarDecl[Scope]  := TVarDeclDictionary.Create(@HashStr);
    TypeDecl[Scope] := TStringToObject.Create(@HashStr);
  end else
  begin
    VarDecl[Scope]  := TVarDeclDictionary.Create(@HashStr);
    TypeDecl[Scope] := TypeDecl[Scope-1].Copy();
  end;
end;

procedure TCompilerContext.DecScope(RestoreStack: Boolean = False);
var savedPos: SizeInt;
begin
  if(Scope <> GLOBAL_SCOPE) then
    savedPos := StackPosArr[Scope-1]; // save parents stack pos before teardown
  VarDecl[scope].Free();
  TypeDecl[scope].Free();
  SetLength(StackPosArr, Scope);
  SetLength(VarDecl,  Scope);
  SetLength(TypeDecl, Scope);
  Dec(Scope);
  if (Scope <> GLOBAL_SCOPE) and RestoreStack then
    StackPosArr[Scope] := savedPos;
end;

procedure TCompilerContext.SyncStackPosMax(Into, From: Int32);
begin
  Self.StackPosArr[Into] := Max(
    Self.StackPosArr[Into],
    Self.StackPosArr[From]
  );
end;

function TCompilerContext.IsInsideFunction(AltScope:Int32=-1): Boolean;
begin
  if AltScope = -1 then
    AltScope := Scope;
  Result := AltScope > GLOBAL_SCOPE;
end;

function TCompilerContext.FunctionNestingDepth(): Int32;
begin
  Result := Scope;
end;

function TCompilerContext.FunctionScopeLevel(): Int32;
begin
  Result := Scope;
end;

function TCompilerContext.GetStackPos(): SizeInt;
begin
  Result := StackPosArr[Scope];
end;

procedure TCompilerContext.IncStackPos(Size:Int32=STACK_ITEM_ALIGN);
begin
  if (Size mod STACK_ITEM_ALIGN <> 0) or (Size = 0) then
    Size := STACK_ITEM_ALIGN * (Size div STACK_ITEM_ALIGN + 1);
  Inc(StackPosArr[Scope], Size);
end;

procedure TCompilerContext.DecStackPos(Size:Int32=STACK_ITEM_ALIGN);
begin
  if (Size mod STACK_ITEM_ALIGN <> 0) or (Size = 0) then
    Size := STACK_ITEM_ALIGN * (Size div STACK_ITEM_ALIGN + 1);
  Dec(StackPosArr[Scope], Size);
end;

// ----------------------------------------------------------------------------
//
function TCompilerContext.CodeSize(): SizeInt;
begin
  Result := Intermediate.Code.Size;
end;

function TCompilerContext.Emit(Opcode: TInstruction; Pos: TDocPos; Setting: TCompilerSettings): PtrInt;
begin
  if Opcode.Code = icNOOP then RaiseException('Tried to emit `NO_OPCODE`', Pos);

  Self.FSettings := Setting;
  Result := Intermediate.AddInstruction(Opcode, Pos, Setting);
end;

procedure TCompilerContext.PatchArg(Pos: SizeInt; ArgID:EInstructionArg; NewArg: PtrInt);
begin
  case ArgID of
    ia1: Intermediate.Code.Data[Pos].args[0].arg := NewArg;
    ia2: Intermediate.Code.Data[Pos].args[1].arg := NewArg;
    ia3: Intermediate.Code.Data[Pos].args[2].arg := NewArg;
    ia4: Intermediate.Code.Data[Pos].args[3].arg := NewArg;
    ia5: Intermediate.Code.Data[Pos].args[4].arg := NewArg;
  //ia6: Intermediate.Code.Data[Pos].args[5].arg := NewArg;
  end;
end;

procedure TCompilerContext.PatchJump(Addr: PtrInt; NewAddr: PtrInt=0);
var tmpErr: string;
begin
  if NewAddr = 0 then
    NewAddr := Self.CodeSize();

  case Intermediate.Code.Data[Addr].Code of
    icJMP:
      Intermediate.Code.Data[Addr].Args[0].arg := NewAddr;
    icRELJMP, icJFUNC, icJCONT, icJBREAK:
      Intermediate.Code.Data[Addr].Args[0].arg := NewAddr-Addr-1;
    icJZ, icJNZ:
      Intermediate.Code.Data[Addr].Args[1].arg := NewAddr-Addr-1;
    else
    begin
      WriteStr(tmpErr, Intermediate.Code.Data[Addr].Code);
      try
        RaiseException('Tried to patch none-jump instruction: '+tmpErr, Intermediate.DocPos.Data[Addr]);
      except
        on E:Exception do raise e at get_caller_addr(get_frame);
      end;
    end;
  end;
end;

function TCompilerContext.RelAddr(Addr: PtrInt): TXprVar;
begin
  Result := TXprVar.Create(nil, Addr - Self.CodeSize() - 1, mpImm);
end;


function TCompilerContext.PushFunction(VarAddr: PtrInt): Int32;
begin
  Result := Length(Intermediate.FunctionTable);
  SetLength(Intermediate.FunctionTable, Result+1);
  Intermediate.FunctionTable[Result].CodeLocation := CodeSize();
  Intermediate.FunctionTable[Result].DataLocation := VarAddr;
  Intermediate.FunctionTable[Result].ClassID      := -1;
  Intermediate.FunctionTable[Result].VMTIndex     := -1;
end;

function TCompilerContext.PushVirtualMethod(VarAddr: PtrInt; ClassID, VMTIndex: Int32): Int32;
begin
  Result := Length(Intermediate.FunctionTable);
  SetLength(Intermediate.FunctionTable, Result+1);
  Intermediate.FunctionTable[Result].CodeLocation := CodeSize();
  Intermediate.FunctionTable[Result].DataLocation := VarAddr;
  Intermediate.FunctionTable[Result].ClassID      := ClassID;
  Intermediate.FunctionTable[Result].VMTIndex     := VMTIndex;
end;

// ----------------------------------------------------------------------------
// All getters prefer local with namespace before it looks outside namespace

function TCompilerContext.TryGetLocalVar(Name: string): TXprVar;
var
  idx: XIntList;
  k: Int32;
  Decoded: TEncodedVar;
begin
  Result := NullResVar;

  if CurrentNamespace <> '' then
  begin
    idx := Self.VarDecl[scope].GetDef(XprCase(CurrentNamespace + Name), NULL_INT_LIST);
    if (idx.Data <> nil) then
    begin
      for k := 0 to idx.High do
      begin
        Decoded := DecodeVarIndex(idx.Data[k]);
        if Decoded.Depth <= NameScopeDepth then
          Exit(Self.Variables.Data[Decoded.Index]);
      end;
    end;
  end;

  idx := Self.VarDecl[scope].GetDef(XprCase(Name), NULL_INT_LIST);
  if (idx.Data <> nil) then
  begin
    for k := 0 to idx.High do
    begin
      Decoded := DecodeVarIndex(idx.Data[k]);
      if Decoded.Depth <= NameScopeDepth then
        Exit(Self.Variables.Data[Decoded.Index]);
    end;
  end;
end;

function TCompilerContext.TryGetGlobalVar(Name: string): TXprVar;
var
  idx: XIntList;
  k: Int32;
  Decoded: TEncodedVar;
begin
  Result := NullResVar;

  if CurrentNamespace <> '' then
  begin
    idx := Self.VarDecl[GLOBAL_SCOPE].GetDef(XprCase(CurrentNamespace + Name), NULL_INT_LIST);
    if (idx.Data <> nil) then
    begin
      for k := 0 to idx.High do
      begin
        Decoded := DecodeVarIndex(idx.Data[k]);
        if Decoded.Depth <= NameScopeDepth then
          Exit(Self.Variables.Data[Decoded.Index]);
      end;
    end;
  end;

  idx := Self.VarDecl[GLOBAL_SCOPE].GetDef(XprCase(Name), NULL_INT_LIST);
  if (idx.Data <> nil) then
  begin
    for k := 0 to idx.High do
    begin
      Decoded := DecodeVarIndex(idx.Data[k]);
      if Decoded.Depth <= NameScopeDepth then
        Exit(Self.Variables.Data[Decoded.Index]);
    end;
  end;
end;



function TCompilerContext.GetVar(Name: string; Pos:TDocPos): TXprVar;
var
  idx: XIntList;
  i, k: Int32;
  PrefixedName: string;
  Decoded: TEncodedVar;
begin
  Result := NullResVar;
  PrefixedName := CurrentNamespace + Name;

  // Search scopes from the inside out (current scope -> parent -> ... -> global)
  for i := Self.Scope downto GLOBAL_SCOPE do
  begin
    // prefer local w/ namespace
    if CurrentNamespace <> '' then
    begin
      idx := Self.VarDecl[i].GetDef(XprCase(PrefixedName), NULL_INT_LIST);
      if (idx.Data <> nil) then
      begin
        for k := 0 to idx.High do
        begin
          Decoded := DecodeVarIndex(idx.Data[k]);
          if Decoded.Depth <= NameScopeDepth then
          begin
            Result := Self.Variables.Data[Decoded.Index];
            Result.NestingLevel := Self.Scope - i;
            if not IsInsideFunction(i) then Result.IsGlobal := True;
            Exit;
          end;
        end;
      end;
    end;

    idx := Self.VarDecl[i].GetDef(XprCase(Name), NULL_INT_LIST);
    if (idx.Data <> nil)  then
    begin
      for k := 0 to idx.High do
      begin
        Decoded := DecodeVarIndex(idx.Data[k]);
        if Decoded.Depth <= NameScopeDepth then
        begin
          Result := Self.Variables.Data[Decoded.Index];
          Result.NestingLevel := Self.Scope - i;
          if not IsInsideFunction(i) then Result.IsGlobal := True;
          Exit;
        end;
      end;
    end;
  end;

  // NoDocPos = No raise, we have no where to raise this..
  // so this seems like a logical natural way to avoid error
  if (Pos.Column <> -1) then
    RaiseExceptionFmt(eUndefinedIdentifier,[Name], Pos);
end;

function TCompilerContext.TryGetVar(Name: string): TXprVar;
begin
  Result := Self.GetVar(Name, NoDocPos);
  if Result.IsGlobal then // is this correct?
    Result.MemPos := mpGlobal;
end;

// Generates a priority list from every scope.
// XXX: Functions are not truely scoped, keep in mind for now
function TCompilerContext.GetVarList(Name: string): TXprVarList;
var
  IndexList: XIntList;
  i: Integer;
  PrefixedName: string;

  procedure AddVarsFromIndexList(const IndexList: XIntList; IsGlobal: Boolean; CostDistance: Int32);
  var
    j: Int32;
    temp: TXprVar;
    Decoded: TEncodedVar;
  begin
    if IndexList.Data <> nil then
    begin
      for j := IndexList.High downto 0 do
      begin
        Decoded := DecodeVarIndex(IndexList.Data[j]);
        if Decoded.Depth <= NameScopeDepth then
        begin
          temp := Self.Variables.Data[Decoded.Index];
          temp.NestingLevel := CostDistance;
          if IsGlobal then temp.IsGlobal := True;
          Result.Add(temp);
        end;
      end;
    end;
  end;

begin
  Result.Init([]);
  PrefixedName := CurrentNamespace + Name;

  // Search scopes from the inside out (current scope -> parent -> ... -> global)
  for i := Self.Scope downto GLOBAL_SCOPE do
  begin
    if CurrentNamespace <> '' then
    begin
      IndexList := Self.VarDecl[i].GetDef(XprCase(PrefixedName), NULL_INT_LIST);
      AddVarsFromIndexList(IndexList, not IsInsideFunction(i), (Self.Scope - i));

      IndexList := Self.VarDecl[i].GetDef(XprCase(Name), NULL_INT_LIST);
      AddVarsFromIndexList(IndexList, not IsInsideFunction(i), 1+(Self.Scope - i));
    end else
    begin
      IndexList := Self.VarDecl[i].GetDef(XprCase(Name), NULL_INT_LIST);
      AddVarsFromIndexList(IndexList, not IsInsideFunction(i), (Self.Scope - i));
    end;
  end;
end;

function TCompilerContext.GetTempVar(Typ: XType): TXprVar;
begin
  Assert(Typ <> nil);
  Result := TXprVar.Create(Typ);
  Result.Addr := StackPos;
  if not IsInsideFunction() then
    Result.IsGlobal := True;

  Result.IsTemporary := True;

  IncStackPos(Result.VarType.Size);
  Variables.Add(Result);
end;


// ----------------------------------------------------------------------------
//

function TCompilerContext.GetType(Name: string): XType;
var
  i: Int32;
  PrefixedName: string;
begin
  if (scope < 0) or (scope >= Length(TypeDecl)) then
    Exit(nil);

  PrefixedName := CurrentNamespace + Name;

  for i := Scope downto GLOBAL_SCOPE do
  begin
    if TypeDecl[i] = nil then Continue;

    if CurrentNamespace <> '' then
    begin
      Result := TypeDecl[i].GetDef(XprCase(PrefixedName), nil);
      if Result <> nil then Exit;
    end;

    Result := TypeDecl[i].GetDef(XprCase(Name), nil);
    if Result <> nil then Exit;
  end;

  Result := nil;
end;

function TCompilerContext.GetType(Name: string; Pos:TDocPos): XType;
begin
  Result := Self.GetType(XprCase(Name));
  if Result = nil then
    RaiseExceptionFmt(eUndefinedType,[Name], Pos);
end;

function TCompilerContext.GetType(BaseType: EExpressBaseType): XType;
var i: Int32;
begin
  for i := Scope downto GLOBAL_SCOPE do
  begin
    if TypeDecl[i] = nil then Continue;
    Result := TypeDecl[i].GetDef(XprCase(BT2S(BaseType)), nil);
    if Result <> nil then Exit;
  end;
  Result := nil;
end;

function TCompilerContext.GetType(BaseType: EExpressBaseType; Pos: TDocPos): XType;
begin
  Result := GetType(BaseType);
  if Result = nil then
    RaiseExceptionFmt(eUndefinedType,[BT2S(BaseType)], Pos);
end;


// ----------------------------------------------------------------------------
//
(*
  Registers a new class type with the compiler. This is the central function
  for creating the runtime Virtual Method Table (VMT).
*)
function TCompilerContext.AddClass(Name: string; Typ: XType): TVirtualMethodTable;
var
  VMT, ParentVMT: TVirtualMethodTable;
  ParentID: Int32;
  ClassTyp: XType_Class;
begin
  if not(Typ Is XType_Class) then
    RaiseException('AddClass got a none-class type entry');

  ClassTyp := Typ as XType_Class;
  if ClassTyp = nil then
    RaiseException('Cannot add a nil class type.');

  ParentID := -1;
  if ClassTyp.Parent <> nil then
  begin
    // The parent class must have already been compiled and registered,
    // so its ClassID will be valid.
    ParentID := ClassTyp.Parent.ClassID;
  end;

  ClassTyp.ClassID := Self.Intermediate.ClassVMTs.Size();

  Result := TVirtualMethodTable.Create(ClassTyp.ClassID, ParentID);

  // not needed
  if ClassTyp.Parent <> nil then
  begin
    ParentVMT := Self.Intermediate.ClassVMTs.Data[ParentID];
    System.Move(ParentVMT.Methods[0], Result.Methods[0], Length(Result.Methods) * SizeOf(Pointer));
  end;

  Self.Intermediate.ClassVMTs.Add(Result);
  Self.AddType(Name, ClassTyp, True);
end;

procedure TCompilerContext.AddType(Name: string; Typ: XType; Manage: Boolean = False);
var
  targetScope: Int32;
begin
  if not IsInsideFunction() then
  begin
    Name := CurrentNamespace + Name;
    targetScope := GLOBAL_SCOPE;  // always register globals at scope 0
  end else
    targetScope := Scope;

  TypeDecl[targetScope][XprCase(Name)] := Typ;

  if Manage then
    Self.AddManagedType(Typ);
end;

procedure TCompilerContext.AddManagedType(Typ: XType);
begin
  if Typ <> nil then
    ManagedTypes.Add(PtrUInt(Typ), Typ);
end;

procedure TCompilerContext.AddManagedNode(Node: XTree_Node);
begin
  ManagedNodes.Add(PtrUInt(Node), Node);
end;



// ----------------------------------------------------------------------------
//

(*
  Heap allocate constants later on before runtime
  We will update the bytecode to contain the address of the constant

  Some constants can go into Immediate (fits 64 bits) for higher performance
*)
function TCompilerContext.RegConst(Value: TXprVar): Int32;
begin
  Value.MemPos := mpConst;
  Result := Self.Constants.Add(Value);
end;

function TCompilerContext.RegConst(constref Value: TConstant): TXprVar;
var i, j: Int32;
begin
  Result := TXprVar.Create(GetType(Value.Typ));
  Result.MemPos := mpConst;
  Result.Addr   := Intermediate.Constants.Add(Value);
  Result.NonWriteable := True;
  RegConst(Result);
end;

function TCompilerContext.RegConst(Value: Boolean): TXprVar; begin Result := RegConst(Constant(Value, xtBool)); end;
function TCompilerContext.RegConst(Value: Int64):   TXprVar; begin Result := RegConst(Constant(Value, xtInt64)); end;
function TCompilerContext.RegConst(Value: Double):  TXprVar; begin Result := RegConst(Constant(Value, xtDouble)); end;
function TCompilerContext.RegConst(Value: Pointer): TXprVar;
var
  c: TConstant;
begin
  c.val_p := Value;
  c.typ := xtPointer;
  Result := RegConst(c);
end;

function TCompilerContext.RegConst(const Value: string): TXprVar;
var
  Index: SizeInt;
begin
  if not StringConstMap.Get(Value, Index) then
  begin
    Index := Length(Intermediate.StringTable);
    SetLength(Intermediate.StringTable, Index + 1);
    Intermediate.StringTable[Index] := Value;
    StringConstMap[Value] := Index;
  end;

  Result := TXprVar.Create(GetType(xtAnsiString), Index, mpConst);
  Result.NonWriteable := True;
end;

// ----------------------------------------------------------------------------
//

function TCompilerContext.ManageTempVar(var Value: TXprVar; DocPos: TDocPos): Int32;
var
  declList: XIntList = (FTop:0; Data:nil);
  exists: Boolean;
  Name: string;
begin
  Name := '%'+Value.VarType.Hash()+'['+Variables.Size.ToString()+']';
  Value.IsTemporary := False;

  Result := Self.Variables.Add(Value);

  exists := self.VarDecl[scope].Get(XprCase(Name), declList);
  if exists then
    declList.Insert(EncodeVarIndex(NameScopeDepth, Result), 0)
  else
    declList.Init([EncodeVarIndex(NameScopeDepth, Result)]);

  Self.VarDecl[scope][XprCase(Name)] := declList;
  Self.TrackNameScope(scope, XprCase(Name));
end;

function TCompilerContext.RegVar(Name: string; var Value: TXprVar; DocPos: TDocPos; GlobalInLocal: Boolean = False): Int32;
var
  declList: XIntList = (FTop:0; Data:nil);
  exists, TypeExtension: Boolean;
  PrefixedName: string;
  varScope, i, varDepth: Int32;
begin
  varScope := scope;
  varDepth := NameScopeDepth;
  if GlobalInLocal then
  begin
    if not IsInsideFunction() then
      SyncStackPosMax(GLOBAL_SCOPE, varScope);
    scope := GLOBAL_SCOPE;
    varDepth := 0; // Global vars have NameScopeDepth 0
  end;

  Value.IsTemporary := False;

  // Apply the current namespace prefix to the name being registered in GLOBAL SCOPE.

  // Type extensions will pass through without namespace prefix.
  // XXX: Shadowing of currently existing global namespace in the case of .TypeMethod
  //      Solve? Keep namespace if exists? Err, no will caus weirdness
  TypeExtension := (Value.VarType is XType_Method) and (
     XType_Method(Value.VarType).TypeMethod or XType_Method(Value.VarType).ClassMethod
  );

  if ((scope = GLOBAL_SCOPE) or (not IsInsideFunction())) and (not TypeExtension) then
    PrefixedName := CurrentNamespace + Name
  else
    PrefixedName := Name;

  Value.Addr := StackPos;
  if not IsInsideFunction() then
    Value.IsGlobal := True;

  Result := Self.Variables.Add(Value);

  if GlobalInLocal then
  begin
    exists := self.VarDecl[GLOBAL_SCOPE].Get(XprCase(PrefixedName), declList);
    if exists then
    begin
      if (Value.VarType.BaseType in [xtMethod, xtExternalMethod]) then
        declList.Add(EncodeVarIndex(varDepth, Result))
      else
        declList.Insert(EncodeVarIndex(varDepth, Result), 0);
    end else
      declList.Init([EncodeVarIndex(varDepth, Result)]);
    Self.VarDecl[GLOBAL_SCOPE][XprCase(PrefixedName)] := declList;
    Self.TrackNameScope(GLOBAL_SCOPE, XprCase(PrefixedName));
  end else
  begin
    exists := self.VarDecl[varScope].Get(XprCase(PrefixedName), declList);
    if exists then
    begin
      if (Value.VarType.BaseType in[xtMethod, xtExternalMethod]) then
        declList.Add(EncodeVarIndex(varDepth, Result))
      else
        declList.Insert(EncodeVarIndex(varDepth, Result), 0);
    end else
      declList.Init([EncodeVarIndex(varDepth, Result)]);
    Self.VarDecl[varScope][XprCase(PrefixedName)] := declList;
    Self.TrackNameScope(varScope, XprCase(PrefixedName));
  end;

  // this acts on whatever is `self.scope`
  IncStackPos(Value.VarType.Size);

  scope := varScope;

  if GlobalInLocal and (not IsInsideFunction()) then
  begin
    for i := 1 to scope do
      SyncStackPosMax(i, GLOBAL_SCOPE);
  end;
end;

function TCompilerContext.RegVar(Name: string; VarType: XType; DocPos: TDocPos; out Index: Int32): TXprVar;
begin
  Assert(VarType <> nil);
  Result := TXprVar.Create(VarType);
  Result.Reference := False;
  Index := Self.RegVar(XprCase(Name), Result, DocPos);
end;

function TCompilerContext.RegVar(Name: string; VarType: XType; DocPos: TDocPos): TXprVar;
var _: Int32;
begin
  Result := Self.RegVar(XprCase(Name), VarType, DocPos, _);
end;


function TCompilerContext.RegGlobalVar(Name: string; var Value: TXprVar; DocPos: TDocPos): Int32;
var oldScope, i: Int32;
begin
  oldScope := Scope;
  if not IsInsideFunction() then
    SyncStackPosMax(GLOBAL_SCOPE, oldScope);

  scope    := GLOBAL_SCOPE;
  Result   := Self.RegVar(XprCase(Name), Value, DocPos);
  scope    := oldScope;

  if not IsInsideFunction() then
  begin
    for i := 1 to scope do
      SyncStackPosMax(i, GLOBAL_SCOPE);
  end;
end;

function TCompilerContext.RegMethod(Name: string; var Value: TXprVar; DocPos: TDocPos): Int32;
begin
  Result := Self.RegVar(XprCase(Name), Value, DocPos, True);
end;


function TCompilerContext.AddVar(Value: Variant; Name: string; VarType:XType): TXprVar;
var
  aConst: TXprVar;
begin
  aConst := RegConst(Constant(Value, VarType.BaseType));
  Result := RegVar(Name, VarType, CurrentDocPos());
  Self.Emit(GetInstr(icMOV,[Result, aConst]), CurrentDocPos(), Self.FSettings);
end;

// ----------------------------------------------------------------------------
//
function TCompilerContext.AddExternalVar(Addr: Pointer; Name: string; VarType:XType): TXprVar;
var
  ptrType: XType;
  ptrVar: TXprVar;
  Index:Int32;
begin
  Result := Self.RegVar(Name, Self.GetType(xtPointer), CurrentDocPos(), Index);
  Self.Variables.Data[Index].Reference := True;
  Self.Variables.Data[Index].VarType   := VarType;
  Self.Variables.Data[Index].IsTemporary := False;
  Result := Self.Variables.Data[Index];
  Self.Emit(GetInstr(icMOV, [Result, External(PtrUInt(Addr), Self.GetType(xtPointer))]), CurrentDocPos(), Self.FSettings);

  (*
  // 1. Create the variable pointing directly to the absolute address via mpHeap
  Result := TXprVar.Create(VarType, PtrInt(Addr), mpHeap);
  Result.IsTemporary  := False;
  Result.NonWriteable := False;
  Result.Reference    := False;

  if not IsInsideFunction() then
    Result.IsGlobal := True;

  // 2. Add it to the global tracking list to get an Index
  varDepth := NameScopeDepth;

  // 3. Register it directly in the scope dictionary
  PrefixedName := CurrentNamespace + Name;
  exists := self.VarDecl[scope].Get(XprCase(PrefixedName), declList);

  if exists then
    declList.Insert(EncodeVarIndex(varDepth, Self.Variables.Add(Result)), 0)
  else
    declList.Init([EncodeVarIndex(varDepth, Self.Variables.Add(Result))]);

  Self.VarDecl[scope][XprCase(PrefixedName)] := declList;
  Self.TrackNameScope(scope, XprCase(PrefixedName));
  *)
end;

function TCompilerContext.AddExternalFunc(Addr: TExternalProc; Name: string; Params: array of XType; PassBy: array of EPassBy; ResType: XType): TXprVar;
var
  i: Int32;
  argtypes: XTypeArray;
  passing: TPassArgsBy;
  exists: Boolean;
  declList: XIntList = (FTop:0; Data:nil);
begin
  //if self.VarDecl[scope].Contains(Xprcase(Name)) then
  //  RaiseExceptionFmt(eSyntaxError, eIdentifierExists, [Name], CurrentDocPos);

  if Length(Params) <> Length(PassBy) then
    RaiseException('AddExternalFunc: Lengths must be the same for: '+ Name);

  SetLength(argtypes, Length(Params));
  SetLength(passing, Length(PassBy));
  for i:=0 to High(params) do
  begin
    passing[i]  := PassBy[i];
    argtypes[i] := Params[i];
  end;

  Result := TXprVar.Create(XType_Method.Create(Name, argtypes, passing, ResType, False), PtrInt(Addr), mpHeap);
  Self.AddManagedType(Result.VarType);

  exists := self.VarDecl[scope].Get(Xprcase(Name), declList);
  if exists then
    declList.Add(EncodeVarIndex(NameScopeDepth, Self.Variables.Add(Result)))
  else
    declList.Init([EncodeVarIndex(NameScopeDepth, Self.Variables.Add(Result))]);

  Self.VarDecl[scope][Xprcase(Name)] := declList;
  Self.TrackNameScope(scope, Xprcase(Name));
end;

function TCompilerContext.AddExternalFunc(Addr: TExternalFunc; Name: string; Params: array of XType; PassBy: array of EPassBy; ResType: XType): TXprVar;
begin
  Result := Self.AddExternalFunc(TExternalProc(Addr), Name, Params, PassBy, ResType);
end;

function TCompilerContext.AddExternalMethod(Addr: TExternalProc;
  Name: string; SelfType: XType; Params: array of XType;
  PassBy: array of EPassBy; ResType: XType): TXprVar;
var
  i: Int32;
  argtypes: XTypeArray;
  passing: TPassArgsBy;
  exists: Boolean;
  declList: XIntList = (FTop:0; Data:nil);
  method: XType_Method;
begin
  // Prepend SelfType as first param
  SetLength(argtypes, Length(Params) + 1);
  SetLength(passing,  Length(Params) + 1);
  argtypes[0] := SelfType;
  passing[0]  := pbRef;
  for i := 0 to High(Params) do
  begin
    argtypes[i+1] := Params[i];
    passing[i+1]  := PassBy[i];
  end;

  method := XType_Method.Create(Name, argtypes, passing, ResType, True); // True = TypeMethod
  method.RealParamcount := Length(Params) + 1;
  Self.AddManagedType(method);

  Result := TXprVar.Create(method, PtrInt(Addr), mpHeap);

  exists := self.VarDecl[scope].Get(XprCase(Name), declList);
  if exists then
    declList.Add(EncodeVarIndex(NameScopeDepth, Self.Variables.Add(Result)))
  else
    declList.Init([EncodeVarIndex(NameScopeDepth, Self.Variables.Add(Result))]);

  Self.VarDecl[scope][XprCase(Name)] := declList;
  Self.TrackNameScope(scope, XprCase(Name));
end;

function TCompilerContext.AddExternalMethod(Addr: TExternalFunc; Name: string; SelfType: XType; Params: array of XType; PassBy: array of EPassBy; ResType: XType): TXprVar;
begin
  Result := Self.AddExternalMethod(TExternalProc(Addr), Name, SelfType, Params, PassBy, ResType);
end;


(*
  Marks the beginning of a code section that may contain patchable loop jumps.
  Pushes the current code size onto a stack, defining the start of the search range
  for RunPatch.
*)
procedure TCompilerContext.PreparePatch;
begin
  PatchPositions.Add(Self.CodeSize());
end;

(*
  Marks the end of a patching scope. Pops the last start position off the stack.
  This is crucial for correctly handling nested loops.
*)
procedure TCompilerContext.PopPatch;
begin
  if PatchPositions.Size = 0 then
    RaiseException('PopPatch called without a corresponding PreparePatch', CurrentDocPos);

  PatchPositions.PopFast(0);
end;

(*
  Scans the most recent code block (since the last PreparePatch) for a specific
  placeholder opcode (e.g., icJBREAK) and replaces it with a valid jump to the
  specified TargetAddr.
*)
procedure TCompilerContext.RunPatch(PlaceholderOp: EIntermediate; TargetAddr: PtrInt);
var
  i, patchStart: PtrInt;
begin
  if PatchPositions.Size = 0 then
    RaiseException('RunPatch called outside of a PreparePatch/PopPatch scope', CurrentDocPos);

  patchStart := PatchPositions.Data[PatchPositions.High];

  for i := patchStart to Intermediate.Code.High do
  begin
    if Intermediate.Code.Data[i].Code = PlaceholderOp then
    begin
      Intermediate.Code.Data[i].Code := icRELJMP;
      PatchJump(i, TargetAddr);
    end;
  end;
end;

procedure TCompilerContext.EmitReturnJump(DocPos: TDocPos);
begin
  SetLength(FReturnPatchList, Length(FReturnPatchList)+1);
  FReturnPatchList[High(FReturnPatchList)] := Self.Emit(GetInstr(icJMP,[NullVar]), DocPos, FSettings);
end;

procedure TCompilerContext.PatchReturnJumps();
var i: Int32;
begin
  for i := 0 to High(FReturnPatchList) do
    PatchJump(FReturnPatchList[i]);
  FReturnPatchList :=[];
end;

procedure TCompilerContext.EmitFinalizeScope(Flags: TCompilerFlags);
var
  managed: TXprVarList;
  i: Int32;
begin
  if cfNoCollect in Flags then Exit;
  managed := GetManagedDeclarations();
  for i := managed.High downto 0 do
    EmitFinalizeVar(managed.Data[i]);
end;

procedure TCompilerContext.EmitScopeCleanupTo(StopAtScope: SizeInt);
var
  s, i, j, k: Int32;
  xprVar: TXprVar;
  Decoded: TEncodedVar;
begin
  for s := Scope downto StopAtScope do
  begin
    with VarDecl[s] do
      for i := 0 to RealSize - 1 do
        for j := 0 to High(Items[i]) do
          for k := 0 to Items[i][j].val.High() do
          begin
            Decoded := DecodeVarIndex(Items[i][j].val.data[k]);
            xprVar := Variables.Data[Decoded.Index];
            if xprVar.IsTemporary then Continue;
            if xprVar.Reference  then Continue;
            if not xprVar.VarType.IsManagedType(Self) then Continue;
            EmitFinalizeVar(xprVar);
          end;
  end;
end;


function TCompilerContext.TempHighWater(): Int32;
begin
  Result := Self.Variables.Size;
end;

procedure TCompilerContext.EmitAbandonedTempCleanup(HighWater: Int32);
var
  i: Int32;
  V: TXprVar;
begin
  { Walk every variable allocated during this statement. }
  for i := HighWater to Self.Variables.High do
  begin
    V := Self.Variables.Data[i];

    { Skip named variables — they are registered in VarDecl and collected
      at scope exit by GetManagedDeclarations + EmitFinalizeVar. }
    if not V.IsTemporary then Continue;

    { Skip reference variables — they do not own their data. }
    if V.Reference then Continue;

    if V.IsBorrowedRef then Continue;

    { Only managed types carry heap allocations that need releasing. }
    if not V.VarType.IsManagedType(Self) then Continue;

    Self.EmitCollect(V);

    { Mark as borrowed so parent statements don't collect it again }
    Self.Variables.Data[i].IsBorrowedRef := True;
  end;
end;



(*
  Extracts the latest docpos
*)
function TCompilerContext.CurrentDocPos(): TDocPos;
begin
  Result := NoDocPos;
  if Self.Intermediate.DocPos.Size > 0 then
    Result :=  Self.Intermediate.DocPos.Data[Self.Intermediate.DocPos.High];
end;

function TCompilerContext.IsManagedRecord(ARec: XType): Boolean;
var
  i: Int32;
  Rec: XType_Record;
begin
  if not(ARec is XType_Record) then
    RaiseException('This is illegal', CurrentDocPos);

  Result := False;
  Rec := ARec as XType_Record;
  if Rec = nil then Exit; // Safety check
  for i:=0 to Rec.FieldTypes.High do
  begin
    if Rec.FieldTypes.Data[i] = nil then Continue; // Skip nil fields

    if Rec.FieldTypes.Data[i].BaseType in XprRefcountedTypes then
      Result := True
    else if Rec.FieldTypes.Data[i] is XType_Record then
      Result := Self.IsManagedRecord(Rec.FieldTypes.Data[i]);

    if Result then Exit;
  end;
end;

procedure TCompilerContext.EmitFinalizeVar(VarToFinalize: TXprVar; ForceGlobal:Boolean=False);
var
  doJmpVar: TXprVar;
  noCollect: PtrInt;
  hasNullCheck: Boolean;
begin
  // Only managed types need finalization, and ref arguments dont need finalization
  // references also includes result var. [XXX: GLOBAL, NONLOCAL]
  if (not VarToFinalize.VarType.IsManagedType(Self)) or (VarToFinalize.Reference) or
     (VarToFinalize.IsGlobal and not ForceGlobal) then
    Exit;

  //XXX {this triggers leaking}
  //if not VarToFinalize.InCurrentScope(Self) then
  //  Exit;

  // NOT NEEDED, JUST A SHORTCUT
  //hasNullCheck := VarToFinalize.VarType is XType_Pointer;

  // can we avoid the call?
  // This handles all simple cases, but not record of array(s)
  // complex records will take the slower calling route
  //if hasNullCheck then
  //begin
  //  doJmpVar := Self.GetTempVar(Self.GetType(xtBool));
  //  Self.Emit(GetInstr(icNEQ,[VarToFinalize.IfRefDeref(Self), Self.RegConst(0), doJmpVar]), Self.CurrentDocPos(), Self.FSettings);
  //  noCollect := Self.Emit(GetInstr(icJZ, [doJmpVar, NullVar]), Self.CurrentDocPos(), Self.FSettings);
  //end;

  // XXX, special.. maybe not.. few percent faster
  if VarToFinalize.VarType.BaseType = xtArray then
  begin
    with XTree_Invoke.Create(XTree_Identifier.Create('SetLen', Self, CurrentDocPos),[XTree_Int.Create('0', Self, CurrentDocPos)], Self, CurrentDocPos) do
    try
      SelfExpr := XTree_VarStub.Create(VarToFinalize.IfRefDeref(Self), Self, CurrentDocPos);
      Compile(NullResVar,[]);
    finally
      Free();
    end;
  end else
    with XTree_Invoke.Create(XTree_Identifier.Create('Collect', Self, CurrentDocPos),[], Self, CurrentDocPos) do
    try
      SelfExpr := XTree_VarStub.Create(VarToFinalize.IfRefDeref(Self), Self, CurrentDocPos);
      Compile(NullResVar,[]);
    finally
      Free();
    end;

  if VarToFinalize.VarType.BaseType = xtClass then
      Self.Emit(
        GetInstr(icFILL,[VarToFinalize,
                          Immediate(VarToFinalize.VarType.Size()),
                          Immediate(0)]),
        CurrentDocPos(), FSettings);

  // jump to here
  //if hasNullCheck then
  //  Self.PatchJump(noCollect);
end;

procedure TCompilerContext.EmitCollect(VarToFinalize: TXprVar);
var
  doJmpVar: TXprVar;
  noCollect: PtrInt;
  hasNullCheck: Boolean;
begin
  // Only managed types need finalization, and references dont touch refcounting system.
  if (not VarToFinalize.VarType.IsManagedType(Self)) then
    Exit;

  // NOT NEEDED, JUST A SHORTCUT
  //hasNullCheck := VarToFinalize.VarType is XType_Pointer;

  // can we avoid the call?
  // This handles all simple cases, but not record of array(s)
  // complex records will take the slower calling route
  //if hasNullCheck then
  //begin
  //  doJmpVar := Self.GetTempVar(Self.GetType(xtBool));
  //  Self.Emit(GetInstr(icNEQ,[VarToFinalize.IfRefDeref(Self), Self.RegConst(0), doJmpVar]), Self.CurrentDocPos(), Self.FSettings);
  //  noCollect := Self.Emit(GetInstr(icJZ, [doJmpVar, NullVar]), Self.CurrentDocPos(), Self.FSettings);
  //end;

  if VarToFinalize.VarType.BaseType = xtArray then
  begin
    with XTree_Invoke.Create(XTree_Identifier.Create('SetLen', Self, CurrentDocPos),[XTree_Int.Create('0', Self, CurrentDocPos)], Self, CurrentDocPos) do
    try
      SelfExpr := XTree_VarStub.Create(VarToFinalize.IfRefDeref(Self), Self, CurrentDocPos);
      Compile(NullResVar,[]);
    finally
      Free();
    end;
  end else
    with XTree_Invoke.Create(XTree_Identifier.Create('Collect', Self, CurrentDocPos),[], Self, CurrentDocPos) do
    try
      SelfExpr := XTree_VarStub.Create(VarToFinalize.IfRefDeref(Self), Self, CurrentDocPos);
      Compile(NullResVar,[]);
    finally
      Free();
    end;

  // jump to here
  //if hasNullCheck then
  //  Self.PatchJump(noCollect);
end;

procedure TCompilerContext.EmitDecref(VarToDecref: TXprVar);
var
  i: integer;
  RecType: XType_Record;
  FieldVar: TXprVar;
  FieldNode, VarNode: XTree_Node;
begin
  // --- Pre-condition Checks ---
  // If the variable's type isn't managed, there's nothing to do.
  if not VarToDecref.VarType.IsManagedType(Self) then
    Exit;

  case VarToDecref.VarType.BaseType of
    xtArray, xtAnsiString, xtUnicodeString, xtClass:
    begin
      Self.Emit(GetInstr(icDECLOCK, [VarToDecref.IfRefDeref(Self)]), Self.CurrentDocPos, Self.FSettings);
    end;

    xtRecord:
    begin
      // recursive to find arrays
      RecType := VarToDecref.VarType as XType_Record;
      VarNode := XTree_VarStub.Create(VarToDecref, Self, CurrentDocPos);
      try
        for i := 0 to RecType.FieldTypes.High do
        begin
          if RecType.FieldTypes.Data[i].IsManagedType(Self) then
          begin
            FieldNode := XTree_Field.Create(VarNode,
              XTree_Identifier.Create(RecType.FieldNames.Data[i], Self, CurrentDocPos),
              Self, CurrentDocPos);
            try
              FieldVar := FieldNode.Compile(NullResVar,[]);
              Self.EmitDecref(FieldVar); // Recursive call
            finally
              FieldNode.Free;
            end;
          end;
        end;
      finally
        VarNode.Free;
      end;
    end;
  end;
end;

procedure TCompilerContext.EmitIncref(VarToIncref: TXprVar);
var
  i: integer;
  RecType: XType_Record;
  FieldVar: TXprVar;
  FieldNode, VarNode: XTree_Node;
begin
  if not VarToIncref.VarType.IsManagedType(Self) then
    Exit;

  case VarToIncref.VarType.BaseType of
    xtArray, xtAnsiString, xtUnicodeString, xtClass:
    begin
      Self.Emit(GetInstr(icINCLOCK, [VarToIncref.IfRefDeref(Self)]), Self.CurrentDocPos, Self.FSettings);
    end;

    xtRecord:
    begin
      RecType := VarToIncref.VarType as XType_Record;
      VarNode := XTree_VarStub.Create(VarToIncref, Self, CurrentDocPos);
      try
        for i := 0 to RecType.FieldTypes.High do
        begin
          if RecType.FieldTypes.Data[i].IsManagedType(Self) then
          begin
            FieldNode := XTree_Field.Create(VarNode,
              XTree_Identifier.Create(RecType.FieldNames.Data[i], Self, CurrentDocPos),
              Self, CurrentDocPos);
            try
              FieldVar := FieldNode.Compile(NullResVar,[]);
              Self.EmitIncref(FieldVar); // Safely recursive call
            finally
              FieldNode.Free;
            end;
          end;
        end;
      finally
        VarNode.Free;
      end;
    end;
  end;
end;


// In the implementation section:
function TCompilerContext.EmitUpcastIfNeeded(VarToCast: TXprVar; TargetType: XType; DerefIfUpcast: Boolean): TXprVar;
var
  InstrCast: EIntermediate;
  TempVar: TXprVar;
begin
  Assert(TargetType <> nil, 'Unknown target type');
  Result := VarToCast;

  if DerefIfUpcast and VarToCast.Reference then
    VarToCast := VarToCast.DerefToTemp(Self);

  Result := VarToCast;

  if (VarToCast.VarType.BaseType = TargetType.BaseType) and
     (VarToCast.VarType.BaseType <> xtPointer) then
    Exit(VarToCast);

  // reinterpret?
  if (TargetType is XType_Ordinal) and (BaseIntType(VarToCast.VarType.BaseType) = BaseIntType(TargetType.BaseType)) then
  begin
    Result.VarType := TargetType;
    Exit;
  end;

  if (VarToCast.VarType.BaseType = xtPointer) and (TargetType.BaseType = xtPointer) then
  begin
    if (XType_Pointer(TargetType).ItemType = nil) and (XType_Pointer(VarToCast.VarType).ItemType = nil) then
      Exit;

    if (XType_Pointer(TargetType).ItemType <> nil) and (XType_Pointer(VarToCast.VarType).ItemType <> nil) then
      if XType_Pointer(TargetType).ItemType.BaseType = XType_Pointer(VarToCast.VarType).ItemType.BaseType then
        Exit;
  end;

  // Maybe upcast
  if VarToCast.VarType.BaseType <> TargetType.BaseType then
  begin
    // true dynamic cast
    if DerefIfUpcast and VarToCast.Reference then
      VarToCast := VarToCast.DerefToTemp(Self);

    TempVar := Self.GetTempVar(TargetType);

    InstrCast := TargetType.EvalCode(op_Asgn, VarToCast.VarType);
    if InstrCast = icNOOP then
      RaiseExceptionFmt(eNotCompatible3+' in upcasting',[OperatorToStr(op_Asgn), BT2S(TargetType.BaseType), BT2S(VarToCast.VarType.BaseType)], CurrentDocPos);

    Self.Emit(GetInstr(InstrCast,  [TempVar, VarToCast]), CurrentDocPos, Self.FSettings);
    Exit(TempVar);
  end;

  Result := VarToCast;
end;

procedure TCompilerContext.VarToDefault(TargetVar: TXprVar);
var
  MagicNode: XTree_Node;
  DefaultIntrinsic: XTree_Invoke;
  VarStub: XTree_VarStub;
begin
  // Cannot default a nil or non-local variable. (also block references?)
  if (TargetVar = NullResVar) or (TargetVar.MemPos <> mpLocal) then
    Exit;

  // 1. Retrieve the prototype 'default' intrinsic node from the global map.
  if not MagicMethods.Get('default', MagicNode) then
    RaiseException('Internal compiler error: "default" magic intrinsic not registered.', NoDocPos);

  VarStub := XTree_VarStub.Create(TargetVar, Self, Self.CurrentDocPos);
  try
    DefaultIntrinsic := XTree_Invoke(MagicNode);
    DefaultIntrinsic.FContext := Self;
    DefaultIntrinsic.FDocPos  := Self.CurrentDocPos;
    DefaultIntrinsic.Method   := nil; // Not needed, the type itself is the dispatcher
    DefaultIntrinsic.SelfExpr := nil;
    DefaultIntrinsic.FResType := nil;
    DefaultIntrinsic.FResTypeHint := nil;

    SetLength(DefaultIntrinsic.Args, 1);
    DefaultIntrinsic.Args[0] := VarStub;
    DefaultIntrinsic.Compile(NullResVar,[]);
  finally
    // The VarStub is owned by the intrinsic's Args array, but since the
    // intrinsic node is a reused prototype, we must free the stub manually.
    VarStub.Free;
    DefaultIntrinsic.Args :=[];
  end;
end;

procedure TCompilerContext.EmitRangeCheck(ArrVar, IndexVar, ExceptionVar: TXprVar; DocPos: TDocPos);
begin
  Self.Emit(GetInstr(icBCHK,[ArrVar, IndexVar, ExceptionVar]), DocPos, Self.FSettings);
end;

function TCompilerContext.GetManagedDeclarations(): TXprVarList;
var
  i,j,k: Int32;
  xprVar: TXprVar;
  Decoded: TEncodedVar;
begin
  Result.Init([]);
  // looks crazy, but it's not that bad

  with Self.VarDecl[scope] do
    for i:=0 to RealSize-1 do
      for j:=0 to High(Items[i]) do
        for k:=0 to Items[i][j].val.High() do
        begin
          Decoded := DecodeVarIndex(Items[i][j].val.data[k]);
          xprVar := Self.Variables.Data[Decoded.Index];

          if (xprVar.VarType.IsManagedType(Self)) then
            Result.Add(xprVar);
        end;
end;

function TCompilerContext.GetGlobalManagedDeclarations(): TXprVarList;
var i,j,k,s: Int32; xprVar: TXprVar; Decoded: TEncodedVar;
begin
  Result.Init([]);
  for s:=GLOBAL_SCOPE to High(Self.VarDecl) do
  begin
    if Self.IsInsideFunction(s) then
      break;

    with Self.VarDecl[s] do
      for i := 0 to RealSize-1 do
        for j := 0 to High(Items[i]) do
          for k := 0 to Items[i][j].val.High() do
          begin
            Decoded := DecodeVarIndex(Items[i][j].val.data[k]);
            xprVar := Self.Variables.Data[Decoded.Index];
            if xprVar.VarType.IsManagedType(Self) then
              Result.Add(xprVar);
          end;
  end;
end;

function TCompilerContext.GetClosureVariables(): TVarList;
var
  i,j,k,s: Int32;
  xprVar: TNamedVar;
  Decoded: TEncodedVar;
begin
  Result.Init([]);

  for s := Scope downto 1 do
  begin
    with Self.VarDecl[s] do
      for i := 0 to RealSize-1 do
        for j := 0 to High(Items[i]) do
        begin
          xprVar.XprVar := NullVar;
          for k := 0 to Items[i][j].val.High do
          begin
            Decoded := DecodeVarIndex(Items[i][j].val.data[k]);
            if Decoded.Depth <= NameScopeDepth then
            begin
              xprVar.XprVar := Self.Variables.Data[Decoded.Index];
              break;
            end;
          end;
          if xprVar.XprVar = NullVar then continue;
          xprVar.Name := Items[i][j].key;
          Result.Add(xprVar);
        end;

    break; // Only capture from the immediate parent scope level
  end;
end;



function GetConversionCost(FromType, ToType: XType): Integer;
var FromMethod, ToMethod: XType_Method;
begin
  if FromType.Equals(ToType) then
    Exit(0);

  (*
  // -- Callable -> callable conversion
  if ((FromType is XType_Method) or (FromType is XType_Lambda)) and
     ((ToType is XType_Method) or (ToType is XType_Lambda)) then
  begin
    if FromType is XType_Lambda then
      FromMethod := XType_Lambda(FromType).FieldTypes.Data[0] as XType_Method
    else
      FromMethod := XType_Method(FromType);

    if ToType is XType_Lambda then
      ToMethod := XType_Lambda(ToType).FieldTypes.Data[0] as XType_Method
    else
      ToMethod := XType_Method(ToType);

    // If the argument has implied captured parameters that the
    // parameter type doesn't account for, signatures are incompatible.
    // A capturing nested func cannot satisfy a clean callable parameter.
    if Length(FromMethod.Params) > FromMethod.RealParamcount then
      Exit(-1);

    // Real parameter counts and types must match
    if FromMethod.RealParamcount <> Length(ToMethod.Params) then
      Exit(-1);

    Exit(0);  // compatible callable
  end;
  *)

  // stricter array/string handling
  // foo(nil)
  if (FromType is XType_String) or (FromType is XType_String) then
  begin
    if (ToType.BaseType = xtPointer) and (XType_Pointer(ToType).ItemType = nil) then
      Exit(200); // "wild" pointer, less ideal, but still legal

    Exit(-1);
  end;

  // --- Pointer logic ---
  if (FromType is XType_Pointer) and (ToType is XType_Pointer) then
  begin
    // Both are pointers -> recurse on what they point to.
    if (XType_Pointer(FromType).ItemType = nil) then
      Exit(0); // nil is assignable to any pointer

    if (XType_Pointer(ToType).ItemType = nil) then
      Exit(200); // "wild" pointer, less ideal, but still legal

    // Delegate comparison to the underlying types
    Result := GetConversionCost(XType_Pointer(FromType).ItemType,
                                XType_Pointer(ToType).ItemType);
    Exit;
  end;

  // --- Class conversions ---
  if (FromType is XType_Class) and (ToType is XType_Class) and
     (ToType.CanAssign(FromType)) then
    Exit(5);

  // --- Integer widening ---
  if (FromType.BaseType in XprIntTypes) and (ToType.BaseType in XprIntTypes) and
     (FromType.Size < ToType.Size) then
    Exit(10);

  // --- Float widening ---
  if (FromType.BaseType in XprFloatTypes) and (ToType.BaseType in XprFloatTypes) and
     (FromType.Size < ToType.Size) then
    Exit(10);

  // --- Int → Float ---
  if (FromType.BaseType in XprIntTypes) and (ToType.BaseType in XprFloatTypes) then
    Exit(100);

  // --- Fallback ---
  if ToType.CanAssign(FromType) then
    Exit(200);

  Result := -1;
end;

function TCompilerContext.GenerateIntrinsics(Name: string; Arguments: array of XType; SelfType: XType; CompileAs: string = ''): XTree_Node;
var
  CURRENT_SCOPE, CleanScope, i: Int32;
  SavedMiniCtx: TMiniContext;
begin
  Result := nil;
  TTypeIntrinsics(TypeIntrinsics).FContext := Self;
  case Lowercase(Name) of
    // Core
    '_refcnt'    : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateRefcount(SelfType, Arguments);
    'high'       : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateHigh(SelfType, Arguments);
    'len'        : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateLen(SelfType, Arguments);
    'setlen'     : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateSetLen(SelfType, Arguments);
    'collect'    : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateCollect(SelfType, Arguments);
    'tostr'      : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateToStr(SelfType, Arguments);
    'default'    : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateDefault(SelfType, Arguments);
    '__passign__'  : Result := (TypeIntrinsics as TTypeIntrinsics).GeneratePtrAssign(SelfType, Arguments, CompileAs);
    '__pdispose__' : Result := (TypeIntrinsics as TTypeIntrinsics).GeneratePtrDispose(SelfType, Arguments, CompileAs);
    '__eq__'       : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateEq(SelfType, Arguments);
    '__neq__'      : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateNeq(SelfType, Arguments);

    // Tier 1
    'push'       : Result := (TypeIntrinsics as TTypeIntrinsics).GeneratePush(SelfType, Arguments);
    'pop'        : Result := (TypeIntrinsics as TTypeIntrinsics).GeneratePop(SelfType, Arguments);
    'slice'      : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateSlice(SelfType, Arguments);
    'copy'       : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateCopy(SelfType, Arguments);
    'contains'   : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateContains(SelfType, Arguments);
    'indexof'    : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateIndexOf(SelfType, Arguments);
    'delete'     : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateDelete(SelfType, Arguments);
    'insert'     : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateInsert(SelfType, Arguments);
    'remove'     : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateRemove(SelfType, Arguments);
    // Tier 2
    'reverse'    : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateReverse(SelfType, Arguments);
    'reversed'   : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateReversed(SelfType, Arguments);

    'sort':
      if Length(Arguments) = 0 then
        Result := (TypeIntrinsics as TTypeIntrinsics).GenerateSort(SelfType, Arguments)
      else
        Result := (TypeIntrinsics as TTypeIntrinsics).GenerateSortOneArg(SelfType, Arguments);

    'sorted':
      if Length(Arguments) = 0 then
        Result := (TypeIntrinsics as TTypeIntrinsics).GenerateSorted(SelfType, Arguments)
      else if (Length(Arguments) = 1) and
              ((Arguments[0] is XType_Lambda) or (Arguments[0] is XType_Method)) then
        Result := (TypeIntrinsics as TTypeIntrinsics).GenerateSortedCmp(SelfType, Arguments)
      else
        Result := (TypeIntrinsics as TTypeIntrinsics).GenerateSortedWeighted(SelfType, Arguments);

    'concat'     : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateConcat(SelfType, Arguments);
    // numeric
    'sum'        : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateSum(SelfType, Arguments);
    'min'        : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateMin(SelfType, Arguments);
    'max'        : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateMax(SelfType, Arguments);
    'mean'       : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateMean(SelfType, Arguments);
    'variance'   : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateVariance(SelfType, Arguments);
    'stddev'     : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateStdDev(SelfType, Arguments);
    'median'     : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateMedian(SelfType, Arguments);
  end;

  if (Result <> nil) then
  begin
    CURRENT_SCOPE := Result.FContext.Scope;

    // Push the current local block's watermark up to Global before compiling the method
    if not Result.FContext.IsInsideFunction(CURRENT_SCOPE) then
      Result.FContext.SyncStackPosMax(GLOBAL_SCOPE, CURRENT_SCOPE);

    Result.FContext.Scope := GLOBAL_SCOPE;
    Result.Compile(NullResVar,[]);
    XTree_Function(Result).PreCompiled := True;

    Result.FContext.Scope := CURRENT_SCOPE;

    if not Result.FContext.IsInsideFunction(CURRENT_SCOPE) then
    begin
      for i := 1 to CURRENT_SCOPE do
        Result.FContext.SyncStackPosMax(i, GLOBAL_SCOPE);
    end;
  end;
end;

function TCompilerContext.ResolveMethod(Name: string; Arguments: XTypeArray; SelfType, RetType: XType; DocPos: TDocPos): TXprVar;
var
  intrinsic, generics: XTree_Node;
const
  COST_IMPOSSIBLE  = -1;
  SCOPE_PENALTY    =  1000;

  function IsCompatible(ArgType, ParamType: XType): Int32;
  begin
    Result := -1;
    if ArgType.Equals(ParamType) then
      Exit(0);

    if (ArgType is XType_Class) and (ParamType is XType_Class) then
    begin
      if ArgType.CanAssign(ParamType) then
        Result := 10;
      if ParamType.CanAssign(ArgType) then
        Result := 200;
    end;
  end;

  function Resolve(CandidateList: TXprVarList): TXprVar;
  var
    i, j: Int32;
    BestScore, CurrentScore, TotalScore: Int32;
    FType: XType_Method;
    EffectiveArgs: XTypeArray;
    BestCandidate, CandidateVar: TXprVar;
    Ambiguous: Boolean;
  begin
    EffectiveArgs :=[];
    BestCandidate := NullVar;
    BestScore := MaxInt;

    if (SelfType <> nil) then
    begin
      SetLength(EffectiveArgs, Length(Arguments) + 1);
      EffectiveArgs[0] := SelfType;
      if Length(Arguments) > 0 then
        Move(Arguments[0], EffectiveArgs[1], SizeOf(XType)*Length(Arguments));
    end else
    begin
      EffectiveArgs := Arguments;
    end;

    Ambiguous := False;
    for i := 0 to CandidateList.High do
    begin
      CandidateVar := CandidateList.Data[i];
      if (not((CandidateVar.VarType is XType_Method) or (CandidateVar.VarType is XType_Lambda))) then
        Continue;

      if CandidateVar.VarType is XType_Lambda then
        FType := XType_Lambda(CandidateVar.VarType).FieldTypes.Data[0] as XType_Method
      else
        FType := XType_Method(CandidateVar.VarType);

      if (SelfType <> nil) and not FType.TypeMethod then Continue;
      if FType.RealParamcount <> Length(EffectiveArgs) then Continue;
      TotalScore := (CandidateVar.NestingLevel * SCOPE_PENALTY);


      // Score the argument list
      for j := 0 to High(EffectiveArgs) do
      begin
        if (FType.Passing[j] = pbRef) then
        begin
          CurrentScore := IsCompatible(EffectiveArgs[j], FType.Params[j]);
          if (CurrentScore = -1) then
          begin
            TotalScore := -1;
            break;
          end;
        end else // pbCopy
        begin
          CurrentScore := GetConversionCost(EffectiveArgs[j], FType.Params[j]);
        end;

        if CurrentScore = COST_IMPOSSIBLE then
        begin
          TotalScore := -1; // Invalidate this candidate
          break;
        end;

        TotalScore := TotalScore + CurrentScore;
      end;

      if TotalScore = -1 then Continue;

      // Logic for comparing scores and handling ambiguity
      if TotalScore < BestScore then
      begin
        BestScore := TotalScore;
        BestCandidate := CandidateVar;
      end
      else if TotalScore = BestScore then
      begin
        if (BestCandidate <> NullVar) and (SelfType is XType_Class) then
        begin
          if ((BestCandidate.VarType is XType_Lambda) and (XType_Method(XType_Lambda(BestCandidate.VarType).FieldTypes.Data[0]).Params[0].CanAssign(FType.Params[0]) and
             not FType.Params[0].Equals(XType_Method(XType_Lambda(BestCandidate.VarType).FieldTypes.Data[0]).Params[0]))) or

            ((BestCandidate.VarType is XType_Method) and (XType_Method(BestCandidate.VarType).Params[0].CanAssign(FType.Params[0]) and
             not FType.Params[0].Equals(XType_Method(BestCandidate.VarType).Params[0]))) then
          begin
            BestCandidate := CandidateVar;
          end
          else
          begin
            Ambiguous := True;
            //BestCandidate := NullVar;
          end;
        end
        else begin
          Ambiguous := True;
          //BestCandidate := NullVar;
        end;
      end;
    end;

    if Ambiguous and (BestScore < MaxInt) then
    begin
      // this is allowed, for late entries VMT, but currently also to support
      // the fact that I have made a design blunder .. all methods are global scope
      // even in compiletime - which is maybe bad - ruins scope priority as a
      // weight to factor in.
      WriteLn(Format(DocPos.ToString + ' Hint: Ambiguous call to `%s`.', [Name]));
    end;

    Result := BestCandidate;
  end;

var
  Func: XTree_Function;
  CURRENT_SCOPE, i: Int32;
begin
  Result := Resolve(Self.GetVarList(Name));

  if (Result = NullVar) then
  begin
    intrinsic := Self.GenerateIntrinsics(name, arguments, selftype);
    if intrinsic <> nil then
      Result := XTree_Function(intrinsic).MethodVar
    else if Self.GenericMap.Get(XprCase(Name), generics) then
    begin
      Func := XTree_GenericFunction(generics).CopyMethod(Arguments, SelfType, RetType, DocPos);

      CURRENT_SCOPE :=  Func.FContext.Scope;
      if not Func.FContext.IsInsideFunction(CURRENT_SCOPE) then
        Func.FContext.SyncStackPosMax(GLOBAL_SCOPE, CURRENT_SCOPE);

      Func.FContext.Scope := GLOBAL_SCOPE;
      Result := Func.Compile(NullResVar,[]);
      Self.DelayedNodes += Func;

      Func.FContext.Scope := CURRENT_SCOPE;
      if not Func.FContext.IsInsideFunction(CURRENT_SCOPE) then
      begin
        for i := 1 to CURRENT_SCOPE do
          Func.FContext.SyncStackPosMax(i, GLOBAL_SCOPE);
      end;
    end;
  end;
end;


procedure TCompilerContext.ResolveToFinalType(var PseudoType: XType);
var
  i: Int32;
begin
  if PseudoType = nil then
    Exit;

  case PseudoType.BaseType of
    xtRecord:
      for i:=0 to (PseudoType as XType_Record).FieldTypes.High do
        Self.ResolveToFinalType((PseudoType as XType_Record).FieldTypes.Data[i]);

    xtArray, xtAnsiString, xtUnicodeString, xtPointer:
      if (PseudoType as XType_Pointer).ItemType <> nil then
        Self.ResolveToFinalType((PseudoType as XType_Pointer).ItemType);

    xtMethod:
      begin
        Self.ResolveToFinalType((PseudoType as XType_Method).ReturnType);
        for i:=0 to High((PseudoType as XType_Method).Params) do
          Self.ResolveToFinalType((PseudoType as XType_Method).Params[i]);
      end;

    xtClass:
      begin
        Self.ResolveToFinalType(XType((PseudoType as XType_Class).Parent));
        for i:=0 to (PseudoType as XType_Class).FieldTypes.High do
          Self.ResolveToFinalType((PseudoType as XType_Class).FieldTypes.Data[i]);
      end;

    xtUnknown:
      PseudoType := Self.GetType(PseudoType.Name);

    // else it's already resolved
  end;
end;


// ----------------------------------------------------------------------------
//
function TCompilerContext.GetLineString(DocPos: TDocPos): string;
var
  ErrorFile: string;
  lines: TStringArray;
  i: Int32;
begin
  ErrorFile := '';

  case DocPos.Document of
    '__main__':        ErrorFile := Self.MainFileContents;
    '__internal__':    ErrorFile := Self.MainFileContents;
    '__system__':      ErrorFile := Self.MainFileContents;
    else
      try
        ErrorFile := LoadFileContents(DocPos.Document);
      except
        ErrorFile := '';
      end;
  end;

  lines := StrExplode(ErrorFile, LineEnding);

  if (DocPos.Line-1 < Length(lines)) and (DocPos.Line >= 1) then
  begin
    Result := '|  '+ lines[DocPos.Line-1] + LineEnding;
    Result += '|  ';
    for i:=0 to DocPos.Column-1 do
      Result += ' ';
    Result += '^^^';
  end;
end;

procedure TCompilerContext.RaiseException(Msg:string);
var
  E: ExpressError;
begin
    E := ExpressError.Create(AtPos(CurrentDocPos()) + Msg+LineEnding+GetLineString(CurrentDocPos()), CurrentDocPos());
    raise e at get_caller_addr(get_frame);
end;

procedure TCompilerContext.RaiseException(Msg:string; DocPos: TDocPos);
var
  E: ExpressError;
begin
    E := ExpressError.Create(AtPos(DocPos) + Msg+LineEnding+GetLineString(DocPos), DocPos);
    raise e at get_caller_addr(get_frame);
end;

procedure TCompilerContext.RaiseExceptionFmt(Msg:string; Args: array of const; DocPos: TDocPos);
begin
  xpr.Errors.RaiseExceptionFmt(Msg+LineEnding+GetLineString(DocPos), Args, DocPos);
end;

procedure TCompilerContext.RaiseException(Typ:EExceptionType; Msg:string; DocPos: TDocPos);
begin
  xpr.Errors.RaiseException(Typ, Msg+LineEnding+GetLineString(DocPos), DocPos);
end;

procedure TCompilerContext.RaiseExceptionFmt(Typ:EExceptionType; Msg:string; Args: array of const; DocPos: TDocPos);
begin
  xpr.Errors.RaiseExceptionFmt(Typ, Msg+LineEnding+GetLineString(DocPos), Args, DocPos);
end;

// ----------------------------------------------------------------------------
//
procedure TCompilerContext.RegisterInternals;
begin
  AddType('Unknown', XType.Create(xtUnknown), True);

  AddType(BT2S(xtBool),  XType_Bool.Create(xtBool), True);
  AddType(BT2S(xtAnsiChar), XType_Char.Create(xtAnsiChar), True);
  AddType(BT2S(xtUnicodeChar), XType_Char.Create(xtUnicodeChar), True);

  AddType(BT2S(xtInt8),     XType_Integer.Create(xtInt8), True);
  AddType(BT2S(xtInt16),    XType_Integer.Create(xtInt16), True);
  AddType(BT2S(xtInt32),    XType_Integer.Create(xtInt32), True);
  AddType(BT2S(xtInt64),    XType_Integer.Create(xtInt64), True);
  AddType(BT2S(xtUInt8),    XType_Integer.Create(xtUInt8), True);
  AddType(BT2S(xtUInt16),   XType_Integer.Create(xtUInt16), True);
  AddType(BT2S(xtUInt32),   XType_Integer.Create(xtUInt32), True);
  AddType(BT2S(xtUInt64),   XType_Integer.Create(xtUInt64), True);

  AddType(BT2S(xtSingle),   XType_Float.Create(xtSingle), True);
  AddType(BT2S(xtDouble),   XType_Float.Create(xtDouble), True);

  AddType(BT2S(xtPointer),  XType_Pointer.Create(nil), True);

  AddType(BT2S(xtAnsiString), XType_String.Create(self.GetType(xtAnsiChar)), True);
  AddType(BT2S(xtUnicodeString), XType_String.Create(self.GetType(xtUnicodeChar)), True);

  (* alias and system specific *)
  AddType('Int',    self.GetType(xtInt));
  AddType('Float',  self.GetType(xtDouble));
  AddType('Char',   self.GetType(xtChar));
  AddType('String', self.GetType(xtAnsiString));

  AddType('NativeInt', self.GetType(xtInt));
  AddType('PtrInt',    self.GetType(xtInt));

  (* complex internals *)
  AddType('!ClosureArray',   XType_Array.Create(self.GetType(xtPointer)), True);

  // < 64 bytes
  RegConst(0); RegConst(1); RegConst(2); RegConst(10);
  RegConst(1.0); RegConst(0.0);
end;


procedure TCompilerContext.ProcessDirective(const Directive: string);
var
  S: TStringArray;
  DirectiveName, DirectiveValue: string;
begin
  // Simple directive parser
  S := Directive.Split([' ']);
  if Length(S) = 0 then Exit;

  DirectiveName := XprCase(S[0]);
  if Length(S) > 1 then
    DirectiveValue := XprCase(S[1])
  else
    DirectiveValue := '';

  case DirectiveName of
    'rangechecks':
      case DirectiveValue of
        'on' : FSettings.RangeChecks := True;
        'off': FSettings.RangeChecks := False;
      else
        RaiseException('Unknown rangecheck setting');
      end;
    'inline':
      case DirectiveValue of
        'on' : FSettings.CanInline := True;
        'off': FSettings.CanInline := False;
      else
        RaiseException('Unknown inline setting');
      end;
    'jit':
      case DirectiveValue of
        'on':  FSettings.JIT := 1;
        'off': FSettings.JIT := 0;
        'low': FSettings.JIT := 1;
        'max': FSettings.JIT := 2;
        'full':FSettings.JIT := 3;
      else
        RaiseException('Unknown JIT mode');
      end;
    'regcost':
      FSettings.JITPenalty := StrToInt(DirectiveValue);

    else
      RaiseException('Unknown JIT mode');
    end;
end;

procedure TCompilerContext.PushSettingOverride(setting: TCompilerSettings);
begin
  Self.FSettingOverride.Add(setting);
end;

function TCompilerContext.PopSettingOverride(): TCompilerSettings;
begin
  Result := Self.FSettingOverride.Pop();
end;

function TCompilerContext.CurrentSetting(Default: TCompilerSettings): TCompilerSettings;
begin
  if Self.FSettingOverride.Size = 0 then
  begin
    Result := Default
  end
  else
  begin
    Result := Self.FSettingOverride.Data[Self.FSettingOverride.High()];
  end;
  Result.LoopDepth := FSettings.LoopDepth;
end;


// ============================================================================
// Basenode
//
var
  __TOTAL_NODES: Int32 = 0;

constructor XTree_Node.Create(ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Inc(__TOTAL_NODES);
  Self.FDocPos   := DocPos;
  Self.FContext  := ACTX;
  Self.FResType  := nil;
  if ACTX <> nil then
  begin
    ACTX.AddManagedNode(Self);
    Self.FSettings := ACTX.FSettings;
  end
  (* else
    WriteLn('Will leak: ', Self.InstanceSize, 'b');
  *)
end;


destructor XTree_Node.Destroy;
begin
  if FContext <> nil then
    FContext.ManagedNodes.Remove(PtrUInt(Self));
  inherited;
end;

function XTree_Node.Emit(instr: EIntermediate; args: array of TXprVar; Doc: TDocPos): SizeInt;
begin
  Result := ctx.Emit(GetInstr(instr, args), FDocPos, FContext.CurrentSetting(Self.FSettings));
end;

function XTree_Node.Emit(instr: EIntermediate; Doc: TDocPos): SizeInt;
begin
  Result := ctx.Emit(GetInstr(instr), FDocPos, FContext.CurrentSetting(Self.FSettings));
end;

function XTree_Node.Emit(Opcode: TInstruction; Doc: TDocPos): SizeInt;
begin
  Result := ctx.Emit(OpCode, FDocPos, FContext.CurrentSetting(Self.FSettings));
end;


function XTree_Node.ToString(offset:string=''): string;
begin
  Result := Offset + System.Copy(Self.ClassName(), 7, Length(Self.ClassName())-6)+'(...)';
end;

function XTree_Node.ResType(): XType;
begin
  Result := FResType;
end;

function XTree_Node.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  result := NullResVar;
end;

function XTree_Node.CompileLValue(Dest: TXprVar): TXprVar;
begin
  Result := NullResVar;
  RaiseException('Internal error: Basenode can not be written to', FDocPos);
end;

function XTree_Node.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
begin
  result := NullResVar;
end;

procedure XTree_Node.SetResTypeHint(AHintType: XType);
begin
  Self.FResTypeHint := AHintType;
end;

function XTree_Node.Copy(): XTree_Node;
begin
  RaiseException('Internal error: Basenode is virtual', FDocPos);
  Result := nil;
end;


(*~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~*)

constructor XType.Create(ABaseType: EExpressBaseType = xtUnknown);
begin
  TypeOfExpr := nil;
  BaseType := ABaseType;
end;

destructor XType.Destroy;
begin
  TypeOfExpr := nil;
  inherited;
end;

function XType.Size(): SizeInt;
begin
  Result := XprTypeSize[BaseType];
end;

function XType.Hash(): string;
begin
  Result := BT2S(Self.BaseType);
end;

function XType.ToString(): string;
begin
  if Self.Name <> '' then
    Result := Self.Name
  else
    Result := Self.ClassName;
end;

function XType.EvalCode(OP: EOperator; Other: XType): EIntermediate;
begin
  if other = nil then
    RaiseException('nil type passed to EvalCode');

  Result := OP2IC(OP);
end;

function XType.CanAssign(Other: XType): Boolean;
begin
  Result := True;
end;

function XType.ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType;
begin
  Result := ctx.GetType(GetEvalRes(OP, Self.BaseType, Other.BaseType));
end;

function XType.Equals(Other: XType): Boolean;
begin
  Result := Self.BaseType = Other.BaseType;
end;

function XType.IsManagedType(ctx: TCompilerContext): Boolean;
begin
  Assert(Self <> nil, 'It should be impossible');
  Result := ((Self.BaseType in XprRefcountedTypes)) or
            ((Self is XType_Record) and ctx.IsManagedRecord(Self));
end;

(*~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~*)

constructor TXprVar.Create(AType: XType; AAddr:PtrInt=0; AMemPos: EMemPos=mpLocal);
begin
  Self.VarType   := AType;
  Self.Addr      := AAddr;
  Self.MemPos    := AMemPos;
  Self.Reference := False;
  Self.IsGlobal  := False;
  Self.NestingLevel := 0;
  Self.NonWriteable := False;
  Self.IsTemporary := False;
  Self.IsBorrowedRef := False;
end;

function TXprVar.IfRefDeref(ctx: TCompilerContext): TXprVar;
begin
  if Self.Reference then
  begin
    Result := Self.Deref(ctx, NullResVar);
    //Result.IsTemporary := Self.IsTemporary;
  end
  else
    Result := Self;
end;

function TXprVar.DerefToTemp(ctx: TCompilerContext): TXprVar;
begin
  Result := Self.Deref(ctx, NullResVar);
end;

function TXprVar.Deref(ctx: TCompilerContext; Dest: TXprVar): TXprVar;
var
  instr: EIntermediate;
begin
  Assert(Self.VarType <> nil);

  if Dest = NullResVar then
  begin
    Result := ctx.GetTempVar(Self.VarType);
    Result.IsBorrowedRef := True;
    ctx.Variables.Data[ctx.Variables.High] := Result;  // write flag back to canonical store
  end
  else
    Result := Dest;


  ctx.Emit(
    GetInstr(icDREF,[Result, Self, Immediate(Self.VarType.Size)]),
    ctx.Intermediate.DocPos.Data[ctx.Intermediate.Code.High],
    ctx.FSettings
  );
end;

function TXprVar.IsManaged(ctx: TCompilerContext): Boolean;
begin
  if Self.IsTemporary then Exit(False);

  Result := ((Self.VarType.BaseType in XprRefcountedTypes)) or
            ((Self.VarType is XType_Record) and ctx.IsManagedRecord(Self.VarType));
end;

function TXprVar.InCurrentScope(ctx: TCompilerContext): Boolean;
begin
  Result := Self.NestingLevel = ctx.Scope;
end;

(*~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~*)

operator = (L,R: TXprVar): Boolean;
begin
  Result := (L.VarType = R.VarType) and (L.Addr = R.Addr) and (L.Reference = R.Reference) and (L.IsGlobal = R.IsGlobal);
end;

operator <> (L,R: TXprVar): Boolean;
begin
  Result := (L.VarType <> R.VarType) or (L.Addr <> R.Addr) or (L.Reference <> R.Reference) or (L.IsGlobal <> R.IsGlobal);
end;


// ----------------------------------------------------------------------------
//
function GetInstr(OP: EIntermediate; args: array of TXprVar): TInstruction;
var
  i: Int32;
begin
  FillChar(Result, SizeOf(TInstruction), 0);
  Result.Code  := OP;
  Result.nArgs := Length(args);
  for i:=0 to Min(High(Result.Args), Result.nArgs)-1 do
  begin
    Result.Args[i].Arg := args[i].Addr;
    if(args[i].VarType <> nil) then
      Result.Args[i].BaseType := args[i].VarType.BaseType
    else
      Result.Args[i].BaseType := xtUnknown;
    Result.Args[i].Pos := args[i].MemPos;

    // might be useful
    Result.Args[i].NestingLevel := args[i].NestingLevel;
    Result.Args[i].IsTemporary  := args[i].IsTemporary;
    Result.Args[i].Reference    := args[i].Reference;
  end;
end;

function GetInstr(OP: EIntermediate): TInstruction;
begin
  FillChar(Result, SizeOf(TInstruction), 0);
  Result.Code  := OP;
  Result.nArgs := 0;
end;

function STORE_FAST(Left, Right: TXprVar; Heap: Boolean): TInstruction;
begin
  if Heap then
    Result := GetInstr(icMOVH,[Left, Right, Immediate(Right.VarType.Size)])
  else
    Result := GetInstr(icMOV,  [Left, Right, Immediate(Right.VarType.Size)]);
end;

function Immediate(v: PtrInt; Typ: XType = nil): TXprVar;
begin
  Result := TXprVar.Create(Typ, v, mpImm);
end;

function External(v: PtrInt; Typ: XType = nil): TXprVar;
begin
  Result := TXprVar.Create(Typ, v, mpHeap);
end;

function OpAddr(v: PtrInt; loc:EMemPos=mpHeap): TXprVar;
begin
  Result := TXprVar.Create(nil, v, loc);
end;


operator + (left: XNodeArray; Right: XNodeArray): XNodeArray;
begin
  SetLength(Result, Length(left)+Length(right));

  if Length(left) > 0 then
    Move(left[0], Result[0], Length(left)*SizeOf(XTree_Node));

  if Length(right) > 0 then
    Move(right[0], Result[Length(left)], Length(right)*SizeOf(XTree_Node));
end;

operator + (left: XTypeArray; Right: XType): XTypeArray;
begin
  Result := left;
  SetLength(Result, Length(Result)+1);
  Result[High(Result)] := Right;
end;

end.
