unit xpr.CompilerContext;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
{$I header.inc}
{.$hints OFF}
interface

uses
  SysUtils, xpr.Dictionary, xpr.Types, xpr.Tokenizer, xpr.Intermediate;

const
  STACK_ITEM_ALIGN = 8; //each element in stack takes up 8 bytes no matter - no option.

type
  EInstructionArg = (ia1, ia2, ia3, ia4, ia5, ia6);

  TIntrinsics = class(TObject)
  end;

  TCompilerContext = class;
  XType = class(TObject)
    BaseType: EExpressBaseType;

    constructor Create(ABaseType: EExpressBaseType=xtUnknown);
    function Size: SizeInt; virtual;
    function EvalCode(OP: EOperator; Other: XType): EIntermediate; virtual;
    function CanAssign(Other: XType): Boolean; virtual;
    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; virtual;
    function Equals(Other: XType): Boolean;  virtual; reintroduce;
    function ToString(): string; override;
    function IsManaged(ctx: TCompilerContext): Boolean; inline;
  end;
  XTypeArray = array of XType;

  TXprVar = record // might be best to make this a class, records are clumpsy here
    VarType: XType;
    Addr: PtrInt;
    MemPos: EMemPos;
    Reference: Boolean;
    IsGlobal: Boolean;

    constructor Create(AType: XType; AAddr: PtrInt=0; AMemPos: EMemPos=mpLocal);
    function IfRefDeref(ctx: TCompilerContext): TXprVar;
    function DerefToTemp(ctx: TCompilerContext): TXprVar;
    function Deref(ctx: TCompilerContext; Dest: TXprVar): TXprVar;
    function IsManaged(ctx: TCompilerContext): Boolean;
  end;

  TXprVarList = specialize TArrayList<TXprVar>;
  TStringToObject = specialize TDictionary<string, XType>;


  { XTree_Node }
  XTree_Node = class(TObject)
    FDocPos:  TDocPos;
    FContext: TCompilerContext;
    FResType: XType;

    constructor Create(ACTX: TCompilerContext; DocPos: TDocPos); virtual;
    function ToString(offset:string=''): string; virtual; reintroduce;

    function ResType(): XType; virtual;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; virtual;
    function CompileLValue(Dest: TXprVar): TXprVar; virtual;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; virtual;

    property ctx: TCompilerContext read FContext write FContext;
  end;
  XNodeArray = array of XTree_Node;

  TCompiledFile = specialize TDictionary<string, XTree_Node>;

  TCompilerContext = class(TObject)
  public
    FCompilingStack: XStringList;
    FNamespaceStack: XStringList;
    FUnitASTCache: TCompiledFile;
  public
    LibrarySearchPaths: XStringList;
    Intermediate: TIntermediateCode;

    Variables: TXprVarList;
    Constants: TXprVarList;

    StringConstMap: TStringToIntDict;

    Scope: SizeInt;

    //ImportedUnits: TDictionary<string, TUnitExports>;
    //UnitAliases:   TDictionary<string, string>;

    // all these relate to current scope
    VarDecl:  array of TVarDeclDictionary;
    TypeDecl: array of TStringToObject;
    StackPosArr: array of SizeInt;

    TypeIntrinsics: TIntrinsics;
    DelayedNodes: XNodeArray;

    PatchPositions: specialize TArrayList<PtrInt>;

 {methods}
    constructor Create();
    procedure ImportUnit(UnitPath, UnitAlias: string; DocPos: TDocPos);
    procedure DelayedImportUnit(UnitPath, UnitAlias: string; DocPos: TDocPos);

    // stack
    function FrameSize(): SizeInt;
    procedure IncScope();                {$ifdef xinline}inline;{$endif}
    procedure DecScope();                {$ifdef xinline}inline;{$endif}
    function  GetStackPos(): SizeInt;    {$ifdef xinline}inline;{$endif}
    procedure IncStackPos(Size:Int32=STACK_ITEM_ALIGN); {$ifdef xinline}inline;{$endif}
    procedure DecStackPos(Size:Int32=STACK_ITEM_ALIGN); {$ifdef xinline}inline;{$endif}

    // namespace
    function GetCurrentNamespace: string;

    // ir code generation
    function CodeSize(): SizeInt;     {$ifdef xinline}inline;{$endif}

    function  Emit(Opcode: TInstruction; Pos: TDocPos): PtrInt;
    procedure PatchArg(Pos: SizeInt; ArgID:EInstructionArg; NewArg: PtrInt);
    procedure PatchJump(Addr: PtrInt; NewAddr: PtrInt=0);
    function  RelAddr(Addr: PtrInt): TXprVar;
    function PushFunction(VarAddr: PtrInt): Int32;

    procedure PreparePatch;
    procedure PopPatch;
    procedure RunPatch(PlaceholderOp: EIntermediate; TargetAddr: PtrInt);

    // ------------------------------------------------------
    function TryGetLocalVar(Name: string): TXprVar;
    function TryGetGlobalVar(Name: string): TXprVar;

    function GetVar(Name: string; Pos:TDocPos): TXprVar;
    function GetVarList(Name: string): TXprVarList;
    function GetTempVar(Typ: XType): TXprVar;

    // ------------------------------------------------------
    function GetType(Name: string; Pos: TDocPos): XType;
    function GetType(BaseType: EExpressBaseType; Pos: TDocPos): XType;
    function GetType(Name: string): XType;
    function GetType(BaseType: EExpressBaseType): XType;

    // ------------------------------------------------------
    procedure AddType(Name: string; Typ:XType);

    function RegConst(Value: TXprVar): Int32; overload;
    function RegConst(constref Value: TConstant): TXprVar;
    function RegConst(Value: Boolean):  TXprVar; overload;
    function RegConst(Value: Int64):  TXprVar; overload;
    function RegConst(Value: Double): TXprVar; overload;
    function RegConst(const Value: string): TXprVar;

    function RegVar(Name: string; var Value: TXprVar; DocPos: TDocPos): Int32; overload;
    function RegVar(Name: string; VarType: XType; DocPos: TDocPos; out Index: Int32): TXprVar; overload;
    function RegVar(Name: string; VarType: XType; DocPos: TDocPos): TXprVar; overload;

    function RegGlobalVar(Name: string; var Value: TXprVar; DocPos: TDocPos): Int32;

    function AddVar(constref Value; Name: string; BaseType: EExpressBaseType; DocPos: TDocPos): TXprVar; overload;
    function AddVar(Value: Boolean; Name: string; DocPos: TDocPos): TXprVar; overload;
    function AddVar(Value: Int64;   Name: string; DocPos: TDocPos): TXprVar; overload;
    function AddVar(Value: Double;  Name: string; DocPos: TDocPos): TXprVar; overload;

    function AddExternalFunc(Addr: TExternalProc; Name: string; Params: array of XType; PassBy: array of EPassBy; ResType: XType): TXprVar;
    function AddExternalFunc(Addr: TExternalFunc; Name: string; Params: array of XType; PassBy: array of EPassBy; ResType: XType): TXprVar; overload;

    // helper
    function IsManagedRecord(ARec: XType): Boolean;
    procedure EmitFinalizeVar(VarToFinalize: TXprVar; IsReturnValue: Boolean = False);
    function EmitUpcastIfNeeded(VarToCast: TXprVar; TargetType: XType; DerefIfUpcast:Boolean): TXprVar;
    function GetManagedDeclarations(): TXprVarList;
    function ResolveMethod(Name: string; Arguments: array of XType; SelfType: XType = nil): TXprVar;

    // ------------------------------------------------------
    procedure RegisterInternals;

    // ------------------------------------------------------
    property StackPos: SizeInt read GetStackPos;
    property CurrentNamespace: string read GetCurrentNamespace;
  end;


const
  NullVar:    TXprVar = (VarType:nil; Addr:0; MemPos:mpImm; Reference:False; IsGlobal:False);
  NullResVar: TXprVar = (VarType:nil; Addr:0; MemPos:mpImm; Reference:False; IsGlobal:False);

  GLOBAL_SCOPE = 0;

function GetInstr(OP: EIntermediate; args: array of TXprVar): TInstruction;
function GetInstr(OP: EIntermediate): TInstruction;
function STORE_FAST(Left, Right: TXprVar; Heap: Boolean): TInstruction; {$ifdef xinline}inline;{$endif}

function Immediate(v: PtrInt; Typ: XType = nil): TXprVar;
function OpAddr(v: PtrInt; loc:EMemPos=mpHeap): TXprVar;

operator =  (L,R: TXprVar): Boolean;
operator <> (L,R: TXprVar): Boolean;

implementation

uses
  Math,LazFileUtils,
  xpr.Errors,
  xpr.Vartypes,
  xpr.Langdef,
  xpr.Tree,
  xpr.TypeIntrinsics,
  xpr.Utils,
  xpr.Parser;


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
    TestPath := CreateAbsoluteSearchPath(LibraryPaths.Data[i], UnitPath);
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

constructor TCompilerContext.Create();
begin
  Intermediate.Init();
  Constants.Init([]);
  Variables.Init([]);
  PatchPositions.Init([]);
  LibrarySearchPaths.Init([]);

  StringConstMap := TStringToIntDict.Create(@HashStr); // Create the map
  SetLength(Intermediate.StringTable, 0); // Initialize the array

  Self.TypeIntrinsics := TTypeIntrinsics.Create(Self, NoDocPos);

  FNamespaceStack.Init([]);
  FCompilingStack.Init([]);

  FUnitASTCache   := TCompiledFile.Create(@HashStr);

  Scope := -1;
  IncScope();
  Self.RegisterInternals;
end;


function TCompilerContext.GetCurrentNamespace: string;
begin
  if FNamespaceStack.High >= 0 then
    Result := FNamespaceStack.Data[FNamespaceStack.High]
  else
    Result := '';
end;

// This is the new, simplified import logic.
procedure TCompilerContext.ImportUnit(UnitPath, UnitAlias: string; DocPos: TDocPos);
var
  UnitAST: XTree_Node;
  UnitCode, ResolvedPath, ImportingFileDir: string;
  UnitTokenizer: TTokenizer;
  i: Int32;
begin
  if FCompilingStack.High >= 0 then
    ImportingFileDir := ExtractFileDir(FCompilingStack.Data[FCompilingStack.High])
  else
    ImportingFileDir := GetCurrentDir;

  ResolvedPath := ResolveUnitPath(UnitPath, ImportingFileDir, Self.LibrarySearchPaths);

  if ResolvedPath = '' then
    RaiseExceptionFmt('Cannot find unit file: `%s`', [UnitPath], DocPos);

  for i := 0 to FCompilingStack.High do
    if FCompilingStack.Data[i] = ResolvedPath then
      RaiseExceptionFmt('Circular import detected: `%s` is already being compiled.', [ResolvedPath], DocPos);

  // 1. Check the AST cache. If not found, parse the unit and cache the result.
  if (not FUnitASTCache.Get(ResolvedPath+':'+CurrentNamespace+UnitAlias, UnitAST)) then
  begin
    UnitCode := LoadFileContents(ResolvedPath);
    if UnitCode = '' then
      RaiseExceptionFmt('Cannot find or read unit file: %s', [ResolvedPath], DocPos);

    UnitTokenizer := Tokenize(UnitCode);
    UnitAST := Parse(UnitTokenizer, Self); //Safe, parser does not modify context, so use current ctx

    FUnitASTCache.Add(UnitPath+':'+CurrentNamespace+UnitAlias, UnitAST)
  end;

  FCompilingStack.Add(ResolvedPath);
  if UnitAlias <> '' then
    FNamespaceStack.Add({CurrentNamespace + }UnitAlias + '.');

  try
    // The AST's compile methods will now automatically use the new prefix.
    UnitAST.Compile(NullResVar, []);
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
  if (not FUnitASTCache.Get(UnitPath+':'+CurrentNamespace+UnitAlias, UnitAST)) then
    RaiseExceptionFmt('Internal compiler error: AST for unit `%s` not found during delayed compile.', [UnitPath+':'+CurrentNamespace+UnitAlias], DocPos);

  if UnitAlias <> '' then FNamespaceStack.Add({CurrentNamespace + }UnitAlias + '.');
  try
    UnitAST.DelayedCompile(NullResVar, []);
  finally
    if UnitAlias <> '' then FNamespaceStack.Pop();
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
    VarDecl[Scope]  := TVarDeclDictionary.Create(@HashStr);//VarDecl[Scope-1].Copy(); // stacks every level
    TypeDecl[Scope] := TypeDecl[Scope-1].Copy();
  end;
end;

procedure TCompilerContext.DecScope();
begin
  if Scope = GLOBAL_SCOPE then
    Exit;

  VarDecl[scope].Free();
  TypeDecl[scope].Free();

  SetLength(StackPosArr, Scope);
  SetLength(VarDecl,  Scope);
  SetLength(TypeDecl, Scope);
  Dec(Scope);
end;

function TCompilerContext.GetStackPos(): SizeInt;
begin
  Result := StackPosArr[Scope]+SizeOf(Pointer);
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

function TCompilerContext.Emit(Opcode: TInstruction; Pos: TDocPos): PtrInt;
begin
  if Opcode.Code = icNOOP then RaiseException('Tried to emit `NO_OPCODE`', Pos);
  Result := Intermediate.AddInstruction(Opcode, Pos);
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
      RaiseException('Tried to patch none-jump instruction', Intermediate.DocPos.Data[Addr]);
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
end;

// ----------------------------------------------------------------------------
//

function TCompilerContext.TryGetLocalVar(Name: string): TXprVar;
var
  idx: XIntList;
begin
  Result := NullResVar;

  idx := Self.VarDecl[scope].GetDef(XprCase(Name), NULL_INT_LIST);
  if (idx.Data <> nil) then
    Result := Self.Variables.Data[idx.Data[0]];
end;

function TCompilerContext.TryGetGlobalVar(Name: string): TXprVar;
var
  idx: XIntList;
begin
  Result := NullResVar;
  idx := Self.VarDecl[GLOBAL_SCOPE].GetDef(XprCase(Name), NULL_INT_LIST);
  if (idx.Data <> nil) then
    Result := Self.Variables.Data[idx.Data[0]];
end;

function TCompilerContext.GetVar(Name: string; Pos:TDocPos): TXprVar;
var
  idx: XIntList;
  PrefixedName: string;
begin
  Result := NullResVar;

  // 1. First, try a direct lookup. This handles fully-qualified names like "Math.PI".
  idx := Self.VarDecl[scope].GetDef(XprCase(Name), NULL_INT_LIST);
  if (idx.Data = nil) then idx := Self.VarDecl[0].GetDef(XprCase(Name), NULL_INT_LIST);

  // 2. If not found, and we are in a namespace, try prepending the current namespace.
  // This handles unqualified names like "PI" when inside the "Math" unit.
  if (idx.Data = nil) and (CurrentNamespace <> '') then
  begin
    PrefixedName := CurrentNamespace + Name;
    idx := Self.VarDecl[scope].GetDef(XprCase(PrefixedName), NULL_INT_LIST);
    if (idx.Data = nil) then idx := Self.VarDecl[0].GetDef(XprCase(PrefixedName), NULL_INT_LIST);
  end;

  if (idx.Data = nil) then
    RaiseExceptionFmt('[GetVar] '+eUndefinedIdentifier, [Name], Pos)
  else
    Result := Self.Variables.Data[idx.Data[0]];
end;

function TCompilerContext.GetVarList(Name: string): TXprVarList;
var
  LocalList, GlobalList, PrefixedGlobalList: XIntList;
  i: Int32;
  temp: TXprVar;
  PrefixedName: string;

  // Helper procedure to avoid duplicating the code that adds variables from an index list.
  procedure AddVarsFromIndexList(const IndexList: XIntList; IsGlobal: Boolean);
  var i: Integer; temp: TXprVar;
  begin
    if IndexList.Data <> nil then
    begin
      for i := 0 to IndexList.High do
      begin
        temp := Self.Variables.Data[IndexList.data[i]];
        if IsGlobal then temp.IsGlobal := True;
        Result.Add(temp);
      end;
    end;
  end;

begin
  Result.Init([]);

  LocalList := Self.VarDecl[scope].GetDef(XprCase(Name), NULL_INT_LIST);
  AddVarsFromIndexList(LocalList, False);

  GlobalList := Self.VarDecl[GLOBAL_SCOPE].GetDef(XprCase(Name), NULL_INT_LIST);
  AddVarsFromIndexList(GlobalList, True);

  if CurrentNamespace <> '' then
  begin
    WriteLn('Resolving with namespace: ', CurrentNamespace+Name);
    PrefixedName := CurrentNamespace + Name;
    PrefixedGlobalList := Self.VarDecl[GLOBAL_SCOPE].GetDef(XprCase(PrefixedName), NULL_INT_LIST);
    WriteLn('Found: ', PrefixedGlobalList.Size);
    AddVarsFromIndexList(PrefixedGlobalList, True);
  end;
end;

function TCompilerContext.GetTempVar(Typ: XType): TXprVar;
begin
  Result := TXprVar.Create(Typ);
  Result.Addr := StackPos;
  if Scope = GLOBAL_SCOPE then
    Result.IsGlobal := True;


  IncStackPos(Result.VarType.Size);
  Variables.Add(Result);
end;


// ----------------------------------------------------------------------------
//

function TCompilerContext.GetType(BaseType: EExpressBaseType; Pos:TDocPos): XType;
begin
  Result := Self.TypeDecl[scope].GetDef(XprCase(BT2S(BaseType)), nil);
  if Result = nil then
    RaiseExceptionFmt(eUndefinedIdentifier, [BT2S(BaseType)], Pos);
end;

function TCompilerContext.GetType(Name: string): XType;
var
  PrefixedName: string;
begin
  Result := Self.TypeDecl[scope].GetDef(XprCase(Name), nil);

  if (Result = nil) and (CurrentNamespace <> '') then
  begin
    PrefixedName := CurrentNamespace + Name;
    Result := Self.TypeDecl[scope].GetDef(XprCase(PrefixedName), nil);
  end;
end;

function TCompilerContext.GetType(Name: string; Pos:TDocPos): XType;
begin
  Result := Self.GetType(XprCase(Name));
  if Result = nil then
    RaiseExceptionFmt(eUndefinedIdentifier, [Name], Pos);
end;

function TCompilerContext.GetType(BaseType: EExpressBaseType): XType;
begin
  Result := Self.TypeDecl[scope].GetDef(XprCase(BT2S(BaseType)), nil);
end;


// ----------------------------------------------------------------------------
//

procedure TCompilerContext.AddType(Name: string; Typ: XType);
begin
  self.TypeDecl[scope][XprCase(CurrentNamespace + Name)] := Typ;
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
begin
  Result := TXprVar.Create(GetType(Value.Typ));
  Result.MemPos := mpConst;
  Result.Addr   := Intermediate.Constants.Add(Value) ;//GetMem(Result.FType.Size);
  RegConst(Result);
end;

function TCompilerContext.RegConst(Value: Boolean): TXprVar; begin Result := RegConst(Constant(Value, xtBoolean)); end;
function TCompilerContext.RegConst(Value: Int64):   TXprVar; begin Result := RegConst(Constant(Value, xtInt64)); end;
function TCompilerContext.RegConst(Value: Double):  TXprVar; begin Result := RegConst(Constant(Value, xtDouble)); end;

function TCompilerContext.RegConst(const Value: string): TXprVar;
var
  Index: SizeInt;
begin
  // Check if this exact string has already been registered.
  if not StringConstMap.Get(Value, Index) then
  begin
    Index := Length(Intermediate.StringTable);
    SetLength(Intermediate.StringTable, Index + 1);
    Intermediate.StringTable[Index] := Value;
    StringConstMap[Value] := Index;
  end;

  // Return a special TXprVar that represents this constant string.
  // The 'Addr' field holds the INDEX into the string table.
  Result := TXprVar.Create(GetType(xtAnsiString), Index, mpConst);
end;

// ----------------------------------------------------------------------------
//

function TCompilerContext.RegVar(Name: string; var Value: TXprVar; DocPos: TDocPos): Int32;
var
  declList: XIntList = (FTop:0; Data:nil);
  exists: Boolean;
  PrefixedName: string;
begin
  // Apply the current namespace prefix to the name being registered.
  PrefixedName := CurrentNamespace + Name;

  Value.Addr := StackPos;
  if Scope = GLOBAL_SCOPE then
    Value.IsGlobal := True;

  Result := Self.Variables.Add(Value);

  exists := self.VarDecl[scope].Get(XprCase(PrefixedName), declList);
  if exists then
  begin
    if (Value.VarType.BaseType in [xtMethod, xtExternalMethod]) then
      declList.Add(Result)
    else
      declList.Insert(Result, 0);
  end else
    declList.Init([Result]);

  Self.VarDecl[scope][XprCase(PrefixedName)] := declList;
  IncStackPos(Value.VarType.Size);
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
var _: Int32; oldScope: Int32;
begin
  oldScope := Scope;
  scope    := GLOBAL_SCOPE;
  Result   := Self.RegVar(XprCase(Name), Value, DocPos);
  scope    := oldScope;
end;




function TCompilerContext.AddVar(constref Value; Name: string; BaseType: EExpressBaseType; DocPos: TDocPos): TXprVar; overload;
begin
  (*
  Result := RegVar(Name, GetType(BaseType), DocPos);
  case BaseType of
    xtBoolean:  Boolean(Result.FPtr^)  := UInt8(Value) <> 0;
    xtAnsiChar: AnsiChar(Result.FPtr^) := AnsiChar(Value);
    xtWideChar: WideChar(Result.FPtr^) := WideChar(Value);
    xtInt8:    Int8(Result.FPtr^)   := Int8(Value);
    xtInt16:   Int16(Result.FPtr^)  := Int16(Value);
    xtInt32:   Int32(Result.FPtr^)  := Int32(Value);
    xtInt64:   Int64(Result.FPtr^)  := Int64(Value);
    xtUInt8:   UInt8(Result.FPtr^)  := UInt8(Value);
    xtUInt16:  UInt16(Result.FPtr^) := UInt16(Value);
    xtUInt32:  UInt32(Result.FPtr^) := UInt32(Value);
    xtUInt64:  UInt64(Result.FPtr^) := UInt64(Value);
    xtSingle:  Single(Result.FPtr^) := Single(Value);
    xtDouble:  Double(Result.FPtr^) := Double(Value);
    xtAnsiString: AnsiString(Result.FPtr^) := AnsiString(Value);
    xtWideString: WideString(Result.FPtr^) := WideString(Value);
  end;
  *)
end;

function TCompilerContext.AddVar(Value: Int64; Name: string; DocPos: TDocPos): TXprVar;
begin
  //Result := RegVar(Name, GetType(xtInt64), DocPos);
  //Move(Value, Result.FAddr^, Result.FType.Size);
end;

function TCompilerContext.AddVar(Value: Double;  Name: string; DocPos: TDocPos): TXprVar;
begin
  //Result := RegVar(Name, GetType(xtDouble), DocPos);
  //Move(Value, Result.FAddr^, Result.FType.Size);
end;

function TCompilerContext.AddVar(Value: Boolean; Name: string; DocPos: TDocPos): TXprVar;
begin
  //Result := RegVar(Name, GetType(xtBoolean), DocPos);
  //Move(Value, Result.FAddr^, Result.FType.Size);
end;

// ----------------------------------------------------------------------------
//
function TCompilerContext.AddExternalFunc(Addr: TExternalProc; Name: string; Params: array of XType; PassBy: array of EPassBy; ResType: XType): TXprVar;
var
  i: Int32;
  argtypes: XTypeArray;
  passing: TPassArgsBy;
  exists: Boolean;
  declList: XIntList = (FTop:0; Data:nil);
begin
  //if self.VarDecl[scope].Contains(Xprcase(Name)) then
  //  RaiseExceptionFmt(eSyntaxError, eIdentifierExists, [Name], NoDocPos);

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

  exists := self.VarDecl[scope].Get(Xprcase(Name), declList);
  if exists then
    declList.Add(Self.Variables.Add(Result))
  else
    declList.Init([Self.Variables.Add(Result)]);

  Self.VarDecl[scope][Xprcase(Name)] := declList;
end;

function TCompilerContext.AddExternalFunc(Addr: TExternalFunc; Name: string; Params: array of XType; PassBy: array of EPassBy; ResType: XType): TXprVar;
begin
  Result := Self.AddExternalFunc(TExternalProc(Addr), Name, Params, PassBy, ResType);
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
    RaiseException('PopPatch called without a corresponding PreparePatch', NoDocPos);

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
    RaiseException('RunPatch called outside of a PreparePatch/PopPatch scope', NoDocPos);

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


function TCompilerContext.IsManagedRecord(ARec: XType): Boolean;
var
  i: Int32;
  Rec: XType_Record;
begin
  if not(ARec is XType_Record) then
    RaiseException('This is illegal', NoDocPos);

  Result := False;
  Rec := ARec as XType_Record;
  if Rec = nil then Exit; // Safety check
  for i:=0 to Rec.FieldTypes.High do
  begin
    if Rec.FieldTypes.Data[i] = nil then Continue; // Skip nil fields

    if Rec.FieldTypes.Data[i] is XType_Array then
      Result := True
    else if Rec.FieldTypes.Data[i] is XType_Record then
      Result := IsManagedRecord(Rec.FieldTypes.Data[i] as XType_Record);

    if Result then Exit;
  end;
end;

procedure TCompilerContext.EmitFinalizeVar(VarToFinalize: TXprVar; IsReturnValue: Boolean = False);
var
  selfVar: TXprVar;
begin
  // Only managed types need finalization.
  if not VarToFinalize.IsManaged(Self) then
    Exit;

  // Don't finalize the 'Self' parameter of a method. It's a reference managed by the caller.
  selfVar := Self.TryGetLocalVar('Self');
  if (selfVar <> NullResVar) and (selfVar.Addr = VarToFinalize.Addr) and selfVar.Reference then
    Exit;

  // If the variable is NOT being returned (i.e., its lifetime is ending here),
  // and it's a local stack variable holding a managed type, decrement its refcount.
  if (not IsReturnValue) and (not VarToFinalize.Reference) and ((not VarToFinalize.IsGlobal) or (Scope = GLOBAL_SCOPE)) then
    Emit(GetInstr(icDECLOCK, [VarToFinalize]), NoDocPos);

  with XTree_Invoke.Create(XTree_Identifier.Create('Collect', Self, NoDocPos), [], Self, NoDocPos) do
  try
    SelfExpr := XTree_VarStub.Create(VarToFinalize.IfRefDeref(Self), Self, NoDocPos);
    Compile(NullResVar, []);
  finally
    Free();
  end;
end;

// In the implementation section:
function TCompilerContext.EmitUpcastIfNeeded(VarToCast: TXprVar; TargetType: XType; DerefIfUpcast: Boolean): TXprVar;
var
  InstrCast: EIntermediate;
  TempVar: TXprVar;
begin
  if VarToCast.VarType.BaseType = TargetType.BaseType then
    Exit(VarToCast);

  // Ensure operands are in stack
  //if VarToCast.Reference then VarToCast := VarToCast.DerefToTemp(Self);

  // Maybe upcast
  if VarToCast.VarType.BaseType <> TargetType.BaseType then
  begin
    if DerefIfUpcast and VarToCast.Reference then
      VarToCast := VarToCast.DerefToTemp(Self);

    TempVar := Self.GetTempVar(TargetType);

    InstrCast := TargetType.EvalCode(op_Asgn, VarToCast.VarType);
    if InstrCast = icNOOP then
      RaiseExceptionFmt(eNotCompatible3, [OperatorToStr(op_Asgn), BT2S(VarToCast.VarType.BaseType), BT2S(TargetType.BaseType)], NoDocPos);

    Self.Emit(GetInstr(InstrCast,  [TempVar, VarToCast]), NoDocPos);
    Exit(TempVar);
  end;

  Result := VarToCast;
end;

function TCompilerContext.GetManagedDeclarations(): TXprVarList;
var
  i,j,k: Int32;
  xprVar: TXprVar;
begin
  Result.Init([]);
  // looks crazy, but it's not that bad

  with Self.VarDecl[scope] do
    for i:=0 to RealSize-1 do
      for j:=0 to High(Items[i]) do
        for k:=0 to Items[i][j].val.High() do
        begin
          xprVar := Self.Variables.Data[Items[i][j].val.data[k]];
          if (xprVar.VarType is XType_Array) or
            ((xprVar.VarType is XType_Record) and Self.IsManagedRecord(xprvar.VarType)) then
            Result.Add(xprVar)
        end;
end;

function TCompilerContext.ResolveMethod(Name: string; Arguments: array of XType; SelfType: XType = nil): TXprVar;
var
  FuncType: XType;

  function MatchingParams(): Boolean;
  var
    i, impliedArgs:Int32;
    FType: XType_Method;
  begin
    impliedArgs := 0;
    FType := XType_Method(FuncType);

    if SelfType <> nil then
    begin
      impliedArgs := 1;
      if (not FType.TypeMethod) or (Length(FType.Params) = 0) then Exit(False);

      if(not(FType.Params[0].Equals(SelfType))) or (not(FType.Params[0].CanAssign(SelfType))) then
        Exit(False);
    end;

    if Length(FType.Params) <> Length(Arguments)+impliedArgs then
      Exit(False);

    for i:=impliedArgs to High(FType.Params) do
      if (not((FType.Passing[i] = pbRef)  and (FType.Params[i].Equals(Arguments[i-impliedArgs])))) and
         (not((FType.Passing[i] = pbCopy) and (FType.Params[i].CanAssign(Arguments[i-impliedArgs])))) then
        Exit(False);

    FuncType := FType;
    Result := True;
  end;


  procedure GenerateTypeIntrinsics();
  var
    func: XTree_Function;
  begin
    if SelfType = nil then Exit;

    // hardcoding this for now.
    func := nil;
    (TypeIntrinsics as TTypeIntrinsics).FContext := Self;
    case Lowercase(Name) of
      'high':      func := (TypeIntrinsics as TTypeIntrinsics).GenerateHigh(SelfType, Arguments);
      'len':       func := (TypeIntrinsics as TTypeIntrinsics).GenerateLen(SelfType, Arguments);
      'setlen':    func := (TypeIntrinsics as TTypeIntrinsics).GenerateSetLen(SelfType, Arguments);
      'collect':   func := (TypeIntrinsics as TTypeIntrinsics).GenerateCollect(SelfType, Arguments);
      'tostr':     func := (TypeIntrinsics as TTypeIntrinsics).GenerateToStr(SelfType, Arguments);
    end;

    if func <> nil then
    begin
      Self.DelayedNodes += func;
      func.Compile(NullVar);
      func.PreCompiled := True;
    end;
  end;


  function Resolve(): TXprVar;
  var
    i: Int32;
    list: TXprVarList;
  begin
    Result := NullVar;
    list := Self.GetVarList(Name);

    for i:=0 to list.High do
      if list.Data[i].VarType is XType_Method then
      begin
        Result := list.Data[i];
        FuncType := XType_Method(list.Data[i].VarType);
        if MatchingParams() then
          Exit()
        else
          Result := NullVar;
      end;
  end;

begin
  WriteLn('Resolving: ', CurrentNamespace+Name);
  Result := Resolve();

  if Result = NullVar then
  begin
    GenerateTypeIntrinsics();
    Result := Resolve();
  end;
end;

// ----------------------------------------------------------------------------
//
procedure TCompilerContext.RegisterInternals;
begin
  AddType(BT2S(xtBoolean),  XType_Bool.Create(xtBoolean));
  AddType(BT2S(xtAnsiChar), XType_Char.Create(xtAnsiChar));
  AddType(BT2S(xtWideChar), XType_Char.Create(xtWideChar));

  AddType(BT2S(xtInt8),     XType_Integer.Create(xtInt8));
  AddType(BT2S(xtInt16),    XType_Integer.Create(xtInt16));
  AddType(BT2S(xtInt32),    XType_Integer.Create(xtInt32));
  AddType(BT2S(xtInt64),    XType_Integer.Create(xtInt64));
  AddType(BT2S(xtUInt8),    XType_Integer.Create(xtUInt8));
  AddType(BT2S(xtUInt16),   XType_Integer.Create(xtUInt16));
  AddType(BT2S(xtUInt32),   XType_Integer.Create(xtUInt32));
  AddType(BT2S(xtUInt64),   XType_Integer.Create(xtUInt64));

  AddType(BT2S(xtSingle),   XType_Float.Create(xtSingle));
  AddType(BT2S(xtDouble),   XType_Float.Create(xtDouble));

  AddType(BT2S(xtPointer),  XType_Pointer.Create(xtPointer));

  AddType(BT2S(xtAnsiString), XType_String.Create(self.GetType(xtAnsiChar)));
  AddType(BT2S(xtUnicodeString), XType_String.Create(self.GetType(xtWideChar)));

  (* alias and system specific *)
  AddType('Int',    self.GetType(xtInt));
  AddType('Float',  self.GetType(xtDouble));
  AddType('Char',   self.GetType(xtChar));
  AddType('String', self.GetType(xtAnsiString));

  AddType('NativeInt', self.GetType(xtInt));
  AddType('PtrInt',    self.GetType(xtInt));
end;



// ============================================================================
// Basenode
//
constructor XTree_Node.Create(ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FDocPos   := DocPos;
  Self.FContext  := ACTX;
  Self.FResType  := nil;
end;

function XTree_Node.ToString(offset:string=''): string;
begin
  Result := Offset + Copy(Self.ClassName(), 7, Length(Self.ClassName())-6)+'(...)';
end;

function XTree_Node.ResType(): XType;
begin
  Result := FResType;
end;

function XTree_Node.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
begin
  result := NullResVar;
end;

function XTree_Node.CompileLValue(Dest: TXprVar): TXprVar;
begin
  Result := NullResVar;
  RaiseException('Can not be written to', FDocPos);
end;

function XTree_Node.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
begin
  result := NullResVar;
end;



(*~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~*)

constructor XType.Create(ABaseType: EExpressBaseType = xtUnknown);
begin
  BaseType := ABaseType;
end;

function XType.Size: SizeInt;
begin
  Result := XprTypeSize[BaseType];
end;

function XType.EvalCode(OP: EOperator; Other: XType): EIntermediate;
begin
  if other = nil then
  begin
    RaiseException('nil type passed to EvalCode');
    Exit(OP2IC(OP));
  end;
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
  Result := (Self = Other) and (Self.BaseType = Other.BaseType);
end;

function XType.ToString(): string;
begin
  Result := Self.ClassName;
end;

function XType.IsManaged(ctx: TCompilerContext): Boolean;
begin
  Result := (Self is XType_Array) or ((Self is XType_Record) and ctx.IsManagedRecord(Self));
end;

(*~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~*)

constructor TXprVar.Create(AType: XType; AAddr:PtrInt=0; AMemPos: EMemPos=mpLocal);
begin
  Self.VarType   := AType;
  Self.Addr   := AAddr;
  Self.MemPos := AMemPos;
  Self.Reference := False;
  Self.IsGlobal := False;
end;

function TXprVar.IfRefDeref(ctx: TCompilerContext): TXprVar;
begin
  if Self.Reference then
    Result := Self.Deref(ctx, NullResVar)
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
  Result := Dest;
  if Result = NullResVar then
    Result := ctx.GetTempVar(Self.VarType);

  ctx.Emit(GetInstr(icDREF, [Result, Self, Immediate(Self.VarType.Size)]), ctx.Intermediate.DocPos.Data[ctx.Intermediate.Code.High]);
end;

function TXprVar.IsManaged(ctx: TCompilerContext): Boolean;
begin
  Result := (Self.VarType is XType_Array) or ((Self.VarType is XType_Record) and ctx.IsManagedRecord(Self.VarType));
end;

(*~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~*)

operator = (L,R: TXprVar): Boolean;
begin
  Result := (L.VarType = R.VarType) and (L.Addr = R.Addr)
end;

operator <> (L,R: TXprVar): Boolean;
begin
  Result := (L.VarType <> R.VarType) or (L.Addr <> R.Addr);
end;


// ----------------------------------------------------------------------------
//
function GetInstr(OP: EIntermediate; args: array of TXprVar): TInstruction;
var
  i: Int32;
begin
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
  end;
end;

function GetInstr(OP: EIntermediate): TInstruction;
begin
  Result.Code  := OP;
  Result.nArgs := 0;
end;

function STORE_FAST(Left, Right: TXprVar; Heap: Boolean): TInstruction;
begin
  if Heap then
    Result := GetInstr(icMOVH, [Left, Right, Immediate(Right.VarType.Size)])
  else
    Result := GetInstr(icMOV,  [Left, Right, Immediate(Right.VarType.Size)]);
end;

function Immediate(v: PtrInt; Typ: XType = nil): TXprVar;
begin
  Result := TXprVar.Create(Typ, v, mpImm);
end;

function OpAddr(v: PtrInt; loc:EMemPos=mpHeap): TXprVar;
begin
  Result := TXprVar.Create(nil, v, loc);
end;

end.

