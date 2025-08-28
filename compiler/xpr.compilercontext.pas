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
    constructor Create(ABaseType: EExpressBaseType=xtUnknown);
    function Size(): SizeInt; virtual;
    function Hash(): string; virtual;
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
    NestingLevel: Integer; // nonlocal

    constructor Create(AType: XType; AAddr: PtrInt=0; AMemPos: EMemPos=mpLocal);
    function IfRefDeref(ctx: TCompilerContext): TXprVar;
    function DerefToTemp(ctx: TCompilerContext): TXprVar;
    function Deref(ctx: TCompilerContext; Dest: TXprVar): TXprVar;
    function IsManaged(ctx: TCompilerContext): Boolean;
  end;

  TXprVarList = specialize TArrayList<TXprVar>;
  TStringToObject = specialize TDictionary<string, XType>;
  TXprTypeList = specialize TArrayList<XType>;

  { XTree_Node }
  XTree_Node = class(TObject)
    FDocPos:  TDocPos;
    FContext: TCompilerContext;
    FResType: XType;

    constructor Create(ACTX: TCompilerContext; DocPos: TDocPos); virtual;
    function ToString(offset:string=''): string; virtual; reintroduce;

    function ResType(): XType; virtual;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; virtual;
    function CompileLValue(Dest: TXprVar): TXprVar; virtual;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; virtual;

    property ctx: TCompilerContext read FContext write FContext;
  end;
  XNodeArray = array of XTree_Node;

  TCompiledFile = specialize TDictionary<string, XTree_Node>;

  TScopedVars  = array of TVarDeclDictionary;
  TScopedTypes = array of TStringToObject;

  TMiniContext = class(TObject)
  public
    Vars:  TScopedVars;
    Types: TScopedTypes;
    Stack: array of SizeInt;

    CompilingStack: XStringList;
    NamespaceStack: XStringList;
  end;

  TCompilerContext = class(TObject)
  public
    MainFileContents: string;

    FCompilingStack: XStringList;
    FNamespaceStack: XStringList;
    FUnitASTCache: TCompiledFile;


    LibrarySearchPaths: XStringList;
    Intermediate: TIntermediateCode;

    Variables: TXprVarList;
    Constants: TXprVarList;
    ManagedTypes: TXprTypeList;

    StringConstMap: TStringToIntDict;

    Scope: SizeInt;

    // these relate to current scope
    VarDecl:  TScopedVars;
    TypeDecl: TScopedTypes;
    StackPosArr: array of SizeInt;

    //
    TypeIntrinsics: TIntrinsics;
    DelayedNodes: XNodeArray;

    PatchPositions: specialize TArrayList<PtrInt>;

 {methods}
    constructor Create(FileContents: string='');
    procedure ImportUnit(UnitPath, UnitAlias: string; DocPos: TDocPos);
    procedure DelayedImportUnit(UnitPath, UnitAlias: string; DocPos: TDocPos);

    function GetMiniContext(): TMiniContext;
    procedure SetMiniContext(MCTX: TMiniContext);


    // stack
    function FrameSize(): SizeInt;
    procedure IncScope();
    procedure DecScope();
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
    function PushVirtualMethod(VarAddr: PtrInt; ClassID, VMTIndex: Int32): Int32;

    procedure PreparePatch;
    procedure PopPatch;
    procedure RunPatch(PlaceholderOp: EIntermediate; TargetAddr: PtrInt);

    // docpos
    function CurrentDocPos(): TDocPos;

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

    function AddClass(Name: string; Typ: XType): TVirtualMethodTable;
    procedure AddType(Name: string; Typ:XType);
    procedure AddManagedType(Typ: XType);

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
    procedure EmitFinalizeVar(VarToFinalize: TXprVar);
    procedure EmitCollect(VarToFinalize: TXprVar);
    function EmitUpcastIfNeeded(VarToCast: TXprVar; TargetType: XType; DerefIfUpcast:Boolean): TXprVar;
    procedure VarToDefault(TargetVar: TXprVar);
    function GetManagedDeclarations(): TXprVarList;
    function GenerateIntrinsics(Name: string; Arguments: array of XType; SelfType: XType; CompileAs: string = ''): XTree_Node;
    function ResolveMethod(Name: string; Arguments: array of XType; SelfType: XType = nil): TXprVar;

    // Extending exceptions
    function GetLineString(DocPos: TDocPos): string;
    procedure RaiseException(Msg:string);
    procedure RaiseException(Msg:string; DocPos: TDocPos);
    procedure RaiseExceptionFmt(Msg:string; Args: array of const; DocPos: TDocPos);
    procedure RaiseException(Typ:EExceptionType; Msg:string; DocPos: TDocPos);
    procedure RaiseExceptionFmt(Typ:EExceptionType; Msg:string; Args: array of const; DocPos: TDocPos);

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
  xpr.Vartypes,
  xpr.Langdef,
  xpr.Tree,
  xpr.TypeIntrinsics,
  xpr.Utils,
  xpr.Parser,
  xpr.MagicIntrinsics;


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

constructor TCompilerContext.Create(FileContents: string='');
begin
  Self.MainFileContents := FileContents;

  Intermediate.Init();
  Constants.Init([]);
  Variables.Init([]);
  PatchPositions.Init([]);
  LibrarySearchPaths.Init([]);
  ManagedTypes.Init([]);
  DelayedNodes := [];

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
      RaiseExceptionFmt('Circular import detected: `%s` is already being compiled.', [ResolvedPath], DocPos);

  // 1. Check the AST cache. If not found, parse the unit and cache the result.
  if (not FUnitASTCache.Get(ResolvedPath+':'+CurrentNamespace+UnitAlias, UnitAST)) then
  begin
    UnitCode := LoadFileContents(ResolvedPath);
    if UnitCode = '' then
      RaiseExceptionFmt('Cannot find or read unit file: %s', [ResolvedPath], DocPos);

    UnitTokenizer := Tokenize(ResolvedPath, UnitCode);
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
  UnitAlias := Xprcase(UnitAlias);

  if (not FUnitASTCache.Get(UnitPath+':'+CurrentNamespace+UnitAlias, UnitAST)) then
    RaiseExceptionFmt('Internal compiler error: AST for unit `%s` not found during delayed compile.', [UnitPath+':'+CurrentNamespace+UnitAlias], DocPos);

  if UnitAlias <> '' then FNamespaceStack.Add({CurrentNamespace + }UnitAlias + '.');
  try
    UnitAST.DelayedCompile(NullResVar, []);
  finally
    if UnitAlias <> '' then FNamespaceStack.Pop();
  end;
end;



function TCompilerContext.GetMiniContext(): TMiniContext;
var
  i: Int32;
begin
  Result := TMiniContext.Create();

  SetLength(Result.vars,  Scope+1);
  SetLength(Result.types, Scope+1);
  SetLength(Result.stack, Scope+1);
  Result.CompilingStack.Init(Self.FCompilingStack.RawOfManaged());
  Result.NamespaceStack.Init(Self.FNamespaceStack.RawOfManaged());

  Result.vars[GLOBAL_SCOPE]  := VarDecl[GLOBAL_SCOPE];
  Result.types[GLOBAL_SCOPE] := TypeDecl[GLOBAL_SCOPE];
  Result.stack[GLOBAL_SCOPE] := StackPosArr[GLOBAL_SCOPE];
  for i:=1 to Scope do
  begin
    Result.vars[i]  := VarDecl[i].Copy();
    Result.types[i] := TypeDecl[i].Copy();
    Result.stack[i] := StackPosArr[i];
  end;
end;

procedure TCompilerContext.SetMiniContext(MCTX: TMiniContext);
var
  i: Int32;
begin
  Self.Scope := High(MCTX.vars);
  Self.FCompilingStack.Init(MCTX.CompilingStack.RawOfManaged());
  SElf.FNamespaceStack.Init(MCTX.NamespaceStack.RawOfManaged());

  SetLength(Self.VarDecl, Scope+1);
  SetLength(Self.TypeDecl, Scope+1);
  SetLength(Self.StackPosArr, Scope+1);
  for i:=1 to Scope do
  begin
    Self.VarDecl[i]     := MCTX.vars[i].Copy();
    Self.TypeDecl[i]    := MCTX.types[i].Copy();
    Self.StackPosArr[i] := MCTX.Stack[i];
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
begin
  Result := NullResVar;

  if CurrentNamespace <> '' then
  begin
    idx := Self.VarDecl[scope].GetDef(XprCase(CurrentNamespace + Name), NULL_INT_LIST);
    if (idx.Data <> nil) then
      Exit(Self.Variables.Data[idx.Data[0]]);
  end;

  idx := Self.VarDecl[scope].GetDef(XprCase(Name), NULL_INT_LIST);
  if (idx.Data <> nil) then
    Result := Self.Variables.Data[idx.Data[0]];
end;

function TCompilerContext.TryGetGlobalVar(Name: string): TXprVar;
var
  idx: XIntList;
begin
  Result := NullResVar;

  if CurrentNamespace <> '' then
  begin
    idx := Self.VarDecl[GLOBAL_SCOPE].GetDef(XprCase(CurrentNamespace + Name), NULL_INT_LIST);
    if (idx.Data <> nil) then
      Exit(Self.Variables.Data[idx.Data[0]]);
  end;

  idx := Self.VarDecl[GLOBAL_SCOPE].GetDef(XprCase(Name), NULL_INT_LIST);
  if (idx.Data <> nil) then
    Exit(Self.Variables.Data[idx.Data[0]]);
end;


function TCompilerContext.GetVar(Name: string; Pos:TDocPos): TXprVar;
var
  idx: XIntList;
  i: Integer;
  PrefixedName: string;
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
        Result := Self.Variables.Data[idx.Data[0]];
        Result.NestingLevel := Self.Scope - i;
        if i = GLOBAL_SCOPE then Result.IsGlobal := True;
        Exit;
      end;
    end;

    idx := Self.VarDecl[i].GetDef(XprCase(Name), NULL_INT_LIST);
    if (idx.Data <> nil)  then
    begin
      Result := Self.Variables.Data[idx.Data[0]];
      Result.NestingLevel := Self.Scope - i;
      if i = GLOBAL_SCOPE then Result.IsGlobal := True;
      Exit;
    end;
  end;

  // If the loop finishes, the identifier was not found in any scope.
  RaiseExceptionFmt(eUndefinedIdentifier, [Name], Pos);
end;

// Generates a priority list from every scope.
// XXX: Functions are not truely scoped, keep in mind for now
function TCompilerContext.GetVarList(Name: string): TXprVarList;
var
  IndexList: XIntList;
  i: Integer;
  PrefixedName: string;

  procedure AddVarsFromIndexList(const IndexList: XIntList; IsGlobal: Boolean);
  var j: Integer; temp: TXprVar;
  begin
    if IndexList.Data <> nil then
    begin
      for j := IndexList.High downto 0 do
      begin
        temp := Self.Variables.Data[IndexList.data[j]];
        if IsGlobal then temp.IsGlobal := True;
        Result.Add(temp);
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
      AddVarsFromIndexList(IndexList, i = GLOBAL_SCOPE);
    end;

    IndexList := Self.VarDecl[i].GetDef(XprCase(Name), NULL_INT_LIST);
    AddVarsFromIndexList(IndexList, i = GLOBAL_SCOPE);
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
    RaiseExceptionFmt('[GetType]'+ eUndefinedIdentifier, [Name], Pos);
end;

function TCompilerContext.GetType(BaseType: EExpressBaseType): XType;
begin
  Result := Self.TypeDecl[scope].GetDef(XprCase(BT2S(BaseType)), nil);
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
  Self.AddType(Name, ClassTyp);
end;

procedure TCompilerContext.AddType(Name: string; Typ: XType);
begin
  self.TypeDecl[scope][XprCase(CurrentNamespace + Name)] := Typ;
end;

procedure TCompilerContext.AddManagedType(Typ: XType);
begin
  self.ManagedTypes.Add(Typ);
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
  if not StringConstMap.Get(Value, Index) then
  begin
    Index := Length(Intermediate.StringTable);
    SetLength(Intermediate.StringTable, Index + 1);
    Intermediate.StringTable[Index] := Value;
    StringConstMap[Value] := Index;
  end;

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
  // Apply the current namespace prefix to the name being registered in GLOBAL SCOPE.
  if scope = GLOBAL_SCOPE then
    PrefixedName := CurrentNamespace + Name
  else
    PrefixedName := Name;

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

procedure TCompilerContext.EmitFinalizeVar(VarToFinalize: TXprVar);
begin
  // Only managed types need finalization, and ref arguments dont need finalization
  // references also includes result var. [XXX: GLOBAL, NONLOCAL]
  if (not VarToFinalize.IsManaged(Self)) or (VarToFinalize.Reference) then
    Exit;

  with XTree_Invoke.Create(XTree_Identifier.Create('Collect', Self, CurrentDocPos), [], Self, CurrentDocPos) do
  try
    SelfExpr := XTree_VarStub.Create(VarToFinalize.IfRefDeref(Self), Self, CurrentDocPos);
    Compile(NullResVar, []);
  finally
    Free();
  end;
end;

procedure TCompilerContext.EmitCollect(VarToFinalize: TXprVar);
begin
  // Only managed types need finalization, and references dont touch refcounting system.
  if (not VarToFinalize.IsManaged(Self)) then
    Exit;

  with XTree_Invoke.Create(XTree_Identifier.Create('Collect', Self, CurrentDocPos), [], Self, CurrentDocPos) do
  try
    SelfExpr := XTree_VarStub.Create(VarToFinalize.IfRefDeref(Self), Self, CurrentDocPos);
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
  Assert(TargetType <> nil, 'Well thats a pitty');

  Result := VarToCast;

  if (VarToCast.VarType.BaseType = TargetType.BaseType) and
     (VarToCast.VarType.BaseType <> xtPointer) then
    Exit();


  if (VarToCast.VarType is XType_Pointer) and (TargetType is XType_Pointer) then
  begin
    if (XType_Pointer(TargetType).PointsTo = nil) and (XType_Pointer(VarToCast.VarType).PointsTo = nil) then
      Exit;

    if (XType_Pointer(TargetType).PointsTo <> nil) and (XType_Pointer(VarToCast.VarType).PointsTo <> nil) then
      if XType_Pointer(TargetType).PointsTo.BaseType = XType_Pointer(VarToCast.VarType).PointsTo.BaseType then
        Exit;
  end;

  // Maybe upcast
  if VarToCast.VarType.BaseType <> TargetType.BaseType then
  begin
    if DerefIfUpcast and VarToCast.Reference then
      VarToCast := VarToCast.DerefToTemp(Self);

    TempVar := Self.GetTempVar(TargetType);

    InstrCast := TargetType.EvalCode(op_Asgn, VarToCast.VarType);
    if InstrCast = icNOOP then
      RaiseExceptionFmt(eNotCompatible3, [OperatorToStr(op_Asgn), BT2S(VarToCast.VarType.BaseType), BT2S(TargetType.BaseType)], CurrentDocPos);

    Self.Emit(GetInstr(InstrCast,  [TempVar, VarToCast]), CurrentDocPos);
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

    SetLength(DefaultIntrinsic.Args, 1);
    DefaultIntrinsic.Args[0] := VarStub;
    DefaultIntrinsic.Compile(NullResVar, []);
  finally
    // The VarStub is owned by the intrinsic's Args array, but since the
    // intrinsic node is a reused prototype, we must free the stub manually.
    VarStub.Free;
    DefaultIntrinsic.Args := [];
  end;
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
          if (xprVar.VarType.IsManaged(Self)) then
            Result.Add(xprVar);
        end;
end;



function GetConversionCost(FromType, ToType: XType): Integer;
begin
  if FromType.Equals(ToType) then
    Exit(0);

  // --- Pointer logic ---
  if (FromType is XType_Pointer) and (ToType is XType_Pointer) then
  begin
    // Both are pointers → recurse on what they point to.
    if (XType_Pointer(FromType).PointsTo = nil) then
      Exit(0); // nil is assignable to any pointer

    if (XType_Pointer(ToType).PointsTo = nil) then
      Exit(200); // "wild" pointer, less ideal, but still legal

    // Delegate comparison to the underlying types
    Result := GetConversionCost(XType_Pointer(FromType).PointsTo,
                                XType_Pointer(ToType).PointsTo);
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
begin
  Result := nil;
  TTypeIntrinsics(TypeIntrinsics).FContext := Self;
  case Lowercase(Name) of
    '_refcnt': Result := (TypeIntrinsics as TTypeIntrinsics).GenerateRefcount(SelfType, Arguments);
    'high'   : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateHigh(SelfType, Arguments);
    'len'    : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateLen(SelfType, Arguments);
    'setlen' : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateSetLen(SelfType, Arguments);
    'collect': Result := (TypeIntrinsics as TTypeIntrinsics).GenerateCollect(SelfType, Arguments);
    'tostr'  : Result := (TypeIntrinsics as TTypeIntrinsics).GenerateToStr(SelfType, Arguments);
    'default': Result := (TypeIntrinsics as TTypeIntrinsics).GenerateDefault(SelfType, Arguments);


    '__passign__' : Result := (TypeIntrinsics as TTypeIntrinsics).GeneratePtrAssign(SelfType, Arguments, CompileAs);
    '__pdispose__': Result := (TypeIntrinsics as TTypeIntrinsics).GeneratePtrDispose(SelfType, Arguments, CompileAs);
  end;

  if (Result <> nil) then
  begin
    Self.DelayedNodes += Result;
    Result.Compile(NullResVar, []);
    XTree_Function(Result).PreCompiled := True;
  end;
end;

function TCompilerContext.ResolveMethod(Name: string; Arguments: array of XType; SelfType: XType = nil): TXprVar;
var
  intrinsic: XTree_Node;
const
  COST_EXACT_MATCH = 0;
  COST_IMPOSSIBLE  = -1;

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
    BestCandidate := NullVar;
    BestScore := MaxInt;

    if SelfType <> nil then
    begin
      SetLength(EffectiveArgs, Length(Arguments) + 1);
      EffectiveArgs[0] := SelfType;
      for i := 0 to High(Arguments) do
        EffectiveArgs[i + 1] := Arguments[i];
    end else
    begin
      SetLength(EffectiveArgs, Length(Arguments) );
      for i:=0 to High(arguments) do
        EffectiveArgs[i] := Arguments[i];
    end;

    Ambiguous := False;
    for i := 0 to CandidateList.High do
    begin
      CandidateVar := CandidateList.Data[i];
      if not (CandidateVar.VarType is XType_Method) then Continue;
      FType := XType_Method(CandidateVar.VarType);

      if (SelfType <> nil) and not FType.TypeMethod then Continue;
      if Length(FType.Params) <> Length(EffectiveArgs) then Continue;

      TotalScore := 0;

      // --- Step 3: Score the unified argument list (with the updated ref check) ---
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

      // --- Step 4 (un-changed) ---
      // ... (The logic for comparing scores and handling ambiguity remains identical) ...
      if TotalScore < BestScore then
      begin
        BestScore := TotalScore;
        BestCandidate := CandidateVar;
      end
      else if TotalScore = BestScore then
      begin
        if (BestCandidate <> NullVar) and (SelfType is XType_Class) then
        begin
          if XType_Method(BestCandidate.VarType).Params[0].CanAssign(FType.Params[0]) and
             not FType.Params[0].Equals(XType_Method(BestCandidate.VarType).Params[0]) then
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
      WriteLn(Format(CurrentDocPos.ToString + ' Warning: Ambiguous call to `%s`.', [Name]));
    end;

    Result := BestCandidate;
  end;

begin
  // --- Main Logic (un-changed) ---
  Result := Resolve(Self.GetVarList(Name));

  if (Result = NullVar) then
  begin
    intrinsic := Self.GenerateIntrinsics(name, arguments, selftype);
    if intrinsic <> nil then
      Result := XTree_Function(intrinsic).MethodVar;
  end;

  if (Result <> NullResVar) and (SelfType <> nil) then
  begin
    //Writeln(SelfType.Hash());
    //WriteLn(Result.VarType.ToString(), '->', Result.VarType.Hash());
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
  if (DocPos.Document <> '__main__') then
  begin
    if (DocPos.Document <> '') then
    try
      ErrorFile := LoadFileContents(DocPos.Document);
    except
      ErrorFile := Self.MainFileContents;
    end;
  end else
    ErrorFile := Self.MainFileContents;

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
begin
  xpr.Errors.RaiseException(Msg+LineEnding+GetLineString(CurrentDocPos()), CurrentDocPos());
end;

procedure TCompilerContext.RaiseException(Msg:string; DocPos: TDocPos);
begin
  xpr.Errors.RaiseException(Msg+LineEnding+GetLineString(DocPos), DocPos);
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
  AddType('Unknown', XType.Create(xtUnknown));

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

  AddType(BT2S(xtPointer),  XType_Pointer.Create(nil));

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



(*~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~*)

constructor XType.Create(ABaseType: EExpressBaseType = xtUnknown);
begin
  BaseType := ABaseType;
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
  Result := (Self = Other) and (Self.BaseType = Other.BaseType);
end;

function XType.IsManaged(ctx: TCompilerContext): Boolean;
begin
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
  Assert(Self.VarType <> nil);

  Result := Dest;
  if Result = NullResVar then
    Result := ctx.GetTempVar(Self.VarType);

  ctx.Emit(GetInstr(icDREF, [Result, Self, Immediate(Self.VarType.Size)]), ctx.Intermediate.DocPos.Data[ctx.Intermediate.Code.High]);
end;

function TXprVar.IsManaged(ctx: TCompilerContext): Boolean;
begin
  Result := ((Self.VarType.BaseType in XprRefcountedTypes)) or
            ((Self.VarType is XType_Record) and ctx.IsManagedRecord(Self.VarType));
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

