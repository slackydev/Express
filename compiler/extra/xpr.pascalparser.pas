unit xpr.PascalParser;
{
  Author: Jarl K. Holta (Pascal/Lape compatibility layer)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Full Recursive-Descent Pascal/Lape Parser for the Express VM.

  Feature coverage:
  - Line-insensitive (ignores tkNEWLINE throughout)
  - Strict Pascal operator precedence via Pratt parsing
  - program / unit / library preambles with interface+implementation sections
  - uses clause → XTree_ImportUnit
  - var / const blocks (global and local)
  - type blocks:
      record, array, enum (with optional explicit values), pointer (^T),
      class / object declarations with visibility sections, field lists,
      and inline method stubs (stubs are skipped; bodies arrive later as
      type-bound routines - see SelfType on XTree_Function)
  - generic type parameters on functions and class declarations
      via tkKW_GENERIC and tkKW_SPECIALIZE
  - procedure / function / constructor / destructor
      with type-bound syntax  TFoo.Method
      and forward declarations (skipped cleanly)
  - Pascal modifiers: inline → @inline, overload → @overload,
      override → @override; virtual/abstract/stdcall/etc silently consumed
  - begin..end blocks, if/then/else, while/do,
      repeat..until, case..of, with..do,
      try..except (typed on..do + catch-all), try..finally
  - for i := lo to/downto hi do   → XTree_ForTo  (native, no desugar)
  - for x in collection do        → XTree_ForIn
  - for var i := lo to hi do      → XTree_ForTo with DeclareIdent
  - raise Expr / raise            → XTree_Raise (bare = re-raise)
  - inherited / inherited Name(args) → XTree_InheritedCall
  - goto Label / label Foo:       → XTree_Goto / XTree_Label
  - label declarations silently consumed
  - Exit(expr) → XTree_Return
  - WriteLn / Write → XTree_Print
  - Inc(x[,n]) / Dec(x[,n]) desugared to compound assignment
  - Assigned(p) desugared to p <> nil
  - array literals [a, b, c] → XTree_InitializerList
  - implicit Result variable (native Express support)
}
{$I header.inc}
{$hints off}

interface

uses
  SysUtils,
  xpr.Types,
  xpr.Tokenizer,
  xpr.Tree,
  xpr.CompilerContext,
  xpr.Langdef,
  xpr.Vartypes;

type
  TMethodStubInfo = record
    Name: string;
    Modifiers: string;
  end;

  TPascalParser = class(TObject)
  private
    FPos:       Int32;
    FTokenizer: TTokenizer;
    FContext:   TCompilerContext;
    FMethodStubs: array of TMethodStubInfo;

    // -- token navigation --------------------------------------------------
    function DocPos: TDocPos; inline;
    function Current: TToken; inline;
    function Peek(n: Int32 = 1): TToken;
    function Next(): TToken;
    function NextIf(Token: ETokenKind): Boolean;
    function NextIfIdent(const IdentVal: string): Boolean;
    procedure Consume(Token: ETokenKind);
    procedure SkipNewlines; inline;

    // -- operator precedence (Pratt) ----------------------------------------
    function GetPascalPrecedence(Token: ETokenKind): Int8;
    function GetPascalAssoc(Token: ETokenKind): Int8;
    function IsPascalUnary(Token: ETokenKind): Boolean;

    // -- helpers -----------------------------------------------------------
    function IsVisibility: Boolean;
    // Skip a method stub in a class body (header only, no begin..end)
    procedure SkipMethodStub(const ClsName: string = '');
    // Collect one modifier token name; returns '' when no modifier found
    function ConsumeModifier(out IsForward: Boolean): string;

  public
    constructor Create(T: TTokenizer; ctx: TCompilerContext);

    // -- top-level ---------------------------------------------------------
    function ParseProgram(): XTree_Node;
    function ParseDeclarations(): XNodeArray;

    // -- type declarations -------------------------------------------------
    function ParseTypeBlock(): XNodeArray;
    function ParseTypeDefinition(): XType;
    function ParseEnum(const TypeName: string): XNodeArray;
    function ParseRecord(): XType_Record;
    function ParseClassDecl(const ClsName, GenericParams: string): XTree_ClassDecl;

    // -- variable / constant declarations ----------------------------------
    function ParseVarBlock(IsConst: Boolean): XNodeArray;

    // -- routines ----------------------------------------------------------
    function ParseRoutine(IsFunction: Boolean; IsConstructor: Boolean = False;
                          ForceForward: Boolean = False;
                          ClsName: string = ''): XTree_Function;

    // -- statements --------------------------------------------------------
    function ParseBlock(): XTree_ExprList;
    function ParseStatement(): XTree_Node;

    function ParseIf(): XTree_If;
    function ParseWhile(): XTree_While;
    function ParseRepeat(): XTree_Repeat;
    function ParseFor(): XTree_Node;
    function ParseCase(): XTree_Case;
    function ParseTry(): XTree_Node;
    function ParseWith(): XTree_With;
    function ParseRaise(): XTree_Raise;
    function ParsePrint(): XTree_Print;

    // -- expressions -------------------------------------------------------
    function ParseExpression(): XTree_Node;
    function ParsePrimary(): XTree_Node;
    function RHSExpr(Left: XTree_Node; leftPrecedence: Int8 = 0): XTree_Node;
    function ParseExpressionList(): XNodeArray;
  end;

function ParsePascal(Tokenizer: TTokenizer; ctx: TCompilerContext = nil): XTree_Node;

implementation

uses xpr.Errors, xpr.Utils;

// -----------------------------------------------------------------------------
//  Public entry point
// -----------------------------------------------------------------------------

function ParsePascal(Tokenizer: TTokenizer; ctx: TCompilerContext = nil): XTree_Node;
var
  Parser: TPascalParser;
begin
  if ctx = nil then
    ctx := TCompilerContext.Create(Tokenizer.Data);

  // pascal without 100 type alias' for int's is not pascal at all
  ctx.AddType('ptrint',    ctx.GetType(xtInt));
  ctx.AddType('ptruint',   ctx.GetType(xtUInt));
  ctx.AddType('nativeint', ctx.GetType(xtInt));
  ctx.AddType('nativeuint',ctx.GetType(xtUInt));
  ctx.AddType('sizeint',   ctx.GetType(xtInt));
  ctx.AddType('sizeuint',  ctx.GetType(xtUInt));
  ctx.AddType('cardinal',  ctx.GetType(xtInt));
  ctx.AddType('integer',   ctx.GetType(xtInt32));
  ctx.AddType('longint',   ctx.GetType(xtInt32));
  ctx.AddType('smallint',  ctx.GetType(xtInt16));
  ctx.AddType('shortint',  ctx.GetType(xtInt8));
  ctx.AddType('longword',  ctx.GetType(xtUInt32));
  ctx.AddType('word',      ctx.GetType(xtUInt32));
  ctx.AddType('byte',      ctx.GetType(xtUInt8));

  Parser := TPascalParser.Create(Tokenizer, ctx);
  Result := Parser.ParseProgram();
  Parser.Free();
end;

// -----------------------------------------------------------------------------
//  Constructor / basic navigation
// -----------------------------------------------------------------------------

constructor TPascalParser.Create(T: TTokenizer; ctx: TCompilerContext);
begin
  FTokenizer := T;
  FContext   := ctx;
  FPos       := 0;
  SkipNewlines();
end;

procedure TPascalParser.SkipNewlines;
begin
  while FTokenizer.Tokens[FPos].Token = tkNEWLINE do Inc(FPos);
end;

function TPascalParser.DocPos: TDocPos;
begin
  Result := FTokenizer.Tokens[FPos].DocPos;
end;

function TPascalParser.Current: TToken;
begin
  Result := FTokenizer.Tokens[FPos];
end;

function TPascalParser.Peek(n: Int32 = 1): TToken;
var i, steps: Int32;
begin
  i := FPos; steps := 0;
  while steps < n do
  begin
    Inc(i);
    if FTokenizer.Tokens[i].Token <> tkNEWLINE then Inc(steps);
  end;
  Result := FTokenizer.Tokens[i];
end;

function TPascalParser.Next(): TToken;
begin
  repeat
    Inc(FPos);
  until FTokenizer.Tokens[FPos].Token <> tkNEWLINE;
  Result := FTokenizer.Tokens[FPos];
end;

function TPascalParser.NextIf(Token: ETokenKind): Boolean;
begin
  Result := Current.Token = Token;
  if Result then Next();
end;

function TPascalParser.NextIfIdent(const IdentVal: string): Boolean;
begin
  Result := (Current.Token = tkIDENT) and (XprCase(Current.Value) = IdentVal);
  if Result then Next();
end;

procedure TPascalParser.Consume(Token: ETokenKind);
begin
  if Current.Token <> Token then
    FContext.RaiseExceptionFmt('Expected `%s` but found `%s`',
      [TokenToString(Token), Current.ToString], DocPos);
  Next();
end;

// -----------------------------------------------------------------------------
//  Pascal operator precedence (higher number = tighter binding)
// -----------------------------------------------------------------------------

function TPascalParser.GetPascalPrecedence(Token: ETokenKind): Int8;
begin
  case Token of
    tkDOT, tkLSQUARE, tkDEREF, tkLPARENTHESES: Result := 9;
    tkAT:                          Result := 8;
    tkNOT:                         Result := 7;
    tkPOW:                         Result := 6;
    tkMUL, tkDIV, tkMOD,
    tkAND, tkSHL, tkSHR:           Result := 5;
    tkPLUS, tkMINUS,
    tkOR, tkXOR:                   Result := 4;
    tkEQ, tkNE, tkLT, tkLTE,
    tkGT, tkGTE, tkIN, tkKW_IS:   Result := 3;
    tkASGN:                        Result := 2;
  else
    Result := -1;
  end;
end;

function TPascalParser.GetPascalAssoc(Token: ETokenKind): Int8;
begin
  case Token of
    tkASGN, tkPOW, tkNOT, tkAT: Result := 0; // right-associative
  else
    Result := 1;
  end;
end;

function TPascalParser.IsPascalUnary(Token: ETokenKind): Boolean;
begin
  Result := Token in [tkNOT, tkAT, tkMINUS, tkPLUS];
end;

// -----------------------------------------------------------------------------
//  Helpers
// -----------------------------------------------------------------------------

function TPascalParser.IsVisibility: Boolean;
var S: string;
begin
  if Current.Token <> tkIDENT then Exit(False);
  S := XprCase(Current.Value);
  Result := (S = 'private') or (S = 'public') or (S = 'protected') or
            (S = 'published') or (S = 'strict');
end;

{ Consume one Pascal routine modifier and return its lowercase name.
  Sets IsForward = True when the modifier is 'forward'.
  Returns '' and does not advance when no modifier is present. }
function TPascalParser.ConsumeModifier(out IsForward: Boolean): string;
var S: string;
begin
  IsForward := False;
  Result := '';

  // Token-based modifiers
  if Current.Token = tkKW_OVERLOAD then begin Next(); NextIf(tkSEMI); Exit('overload'); end;
  if Current.Token = tkKW_OVERRIDE then begin Next(); NextIf(tkSEMI); Exit('override'); end;

  if Current.Token <> tkIDENT then Exit;
  S := XprCase(Current.Value);

  case S of
    'inline', 'jit', 'jitlow',
    'virtual', 'dynamic', 'abstract', 'reintroduce',
    'stdcall', 'cdecl', 'pascal', 'register', 'safecall',
    'static', 'deprecated', 'experimental', 'platform':
      begin Next(); NextIf(tkSEMI); Result := S; end;
    'forward':
      begin Next(); NextIf(tkSEMI); IsForward := True; Result := 'forward'; end;
    'external':
      begin
        Next();
        // optional library name string and optional 'name' clause
        if Current.Token = tkSTRING then Next();
        if NextIfIdent('name') then
          if Current.Token = tkSTRING then Next();
        NextIf(tkSEMI);
        Result := 'external';
      end;
    'message', 'dispid', 'index':
      begin
        Next();
        if not (Current.Token in [tkSEMI, tkKW_END, tkUNKNOWN]) then
          ParseExpression(); // consume and discard argument
        NextIf(tkSEMI);
        Result := S;
      end;
  end;
end;

{ Skip a method stub in a class body - header only, no begin..end block.
  On entry Current points at the procedure/function/constructor/destructor token. }
procedure TPascalParser.SkipMethodStub(const ClsName: string = '');
var
  depth: Int32; IsForward: Boolean; ModName: string;
  MName, Mods: string;
begin
  Next(); // consume the method-kind token

  MName := '';
  // Optional qualified name: ClassName.MethodName
  if Current.Token = tkIDENT then
  begin
    MName := Current.Value;
    Next(); // method name
  end;

  if (Current.Token = tkDOT) and (Peek(1).Token = tkIDENT) then
  begin
    Next(); // consume dot
    MName := Current.Value;
    Next(); // consume method name after dot
  end;

  // Generic type params: <T, U>
  if Current.Token = tkLT then
  begin
    Next(); depth := 1;
    while (depth > 0) and (Current.Token <> tkUNKNOWN) do
    begin
      if Current.Token = tkLT then Inc(depth)
      else if Current.Token = tkGT then Dec(depth);
      if depth > 0 then Next();
    end;
    if Current.Token = tkGT then Next();
  end;

  // Parameter list
  if Current.Token = tkLPARENTHESES then
  begin
    Next(); depth := 1;
    while (depth > 0) and (Current.Token <> tkUNKNOWN) do
    begin
      if Current.Token = tkLPARENTHESES then Inc(depth)
      else if Current.Token = tkRPARENTHESES then Dec(depth);
      if depth > 0 then Next();
    end;
    if Current.Token = tkRPARENTHESES then Next();
  end;

  // Return type for functions
  if Current.Token = tkCOLON then
  begin
    Next();
    ParseTypeDefinition(); // consume & discard
  end;

  NextIf(tkSEMI);

  // Modifiers
  Mods := '';
  repeat
    ModName := ConsumeModifier(IsForward);
    if ModName <> '' then Mods := Mods + ModName + ';';
  until ModName = '';

  // Store modifiers from the class stub to be applied later during implementation
  if (ClsName <> '') and (Mods <> '') and (MName <> '') then
  begin
    SetLength(FMethodStubs, Length(FMethodStubs) + 1);
    FMethodStubs[High(FMethodStubs)].Name := XprCase(ClsName + '.' + MName);
    FMethodStubs[High(FMethodStubs)].Modifiers := Mods;
  end;
end;

// -----------------------------------------------------------------------------
//  ParseProgram  –  handles program / unit / library preambles
// -----------------------------------------------------------------------------

function TPascalParser.ParseProgram(): XTree_Node;
var
  RootNodes: XNodeArray;
  DeclNodes: XNodeArray;
  i: Int32;
  UName, S: string;
begin
  SetLength(RootNodes, 0);

  // program / unit / library Name;
  if (Current.Token in [tkKW_PROGRAM, tkKW_SPECIALIZE {reused}]) or
     NextIfIdent('library') or NextIfIdent('unit') or
     (Current.Token = tkKW_PROGRAM) then
  begin
    if Current.Token in [tkKW_PROGRAM] then Next();
    if Current.Token = tkIDENT then Next(); // name
    NextIf(tkSEMI);
  end;

  // Top-level parse loop - handles both flat programs and units with
  // interface / implementation sections.
  while not (Current.Token in [tkUNKNOWN]) do
  begin
    // -- interface / implementation / initialization / finalization --
    if Current.Token = tkIDENT then
    begin
      S := XprCase(Current.Value);

      if S = 'interface' then   begin Next(); Continue; end;
      if S = 'implementation' then begin Next(); Continue; end;

      if S = 'initialization' then
      begin
        Next();
        // Treat as the main begin block
        while not (Current.Token in [tkIDENT, tkKW_BEGIN, tkUNKNOWN]) or
              (not ((Current.Token = tkIDENT) and
                    (XprCase(Current.Value) = 'finalization'))) do
        begin
          if Current.Token = tkSEMI then begin Next(); Continue; end;
          if Current.Token in [tkUNKNOWN] then Break;
          RootNodes += ParseStatement();
          NextIf(tkSEMI);
        end;
        Continue;
      end;

      if S = 'finalization' then
      begin
        // Skip finalization section
        Next();
        while not (Current.Token in [tkUNKNOWN, tkDOT]) do Next();
        Break;
      end;

      if S = 'uses' then
      begin
        Next(); // consume 'uses'
        repeat
          // Support dotted unit names: System.SysUtils
          UName := Current.Value;
          Consume(tkIDENT);
          while NextIf(tkDOT) do
          begin
            UName += '.' + Current.Value;
            Consume(tkIDENT);
          end;
          // Optional 'in <filename>'
          if NextIfIdent('in') then
          begin
            if Current.Token = tkSTRING then Next(); // skip filename
          end;
          RootNodes += XTree_ImportUnit.Create(UName, '', FContext, DocPos);
        until not NextIf(tkCOMMA);
        NextIf(tkSEMI);
        Continue;
      end;
    end;

    if Current.Token = tkKW_BEGIN then
    begin
      RootNodes += ParseBlock();
      NextIf(tkDOT); // end.
      Break;
    end;

    if Current.Token in [tkKW_TYPE, tkKW_VAR, tkKW_CONST,
                          tkKW_FUNC, tkKW_CONSTRUCTOR] then
    begin
      DeclNodes := ParseDeclarations();
      for i := 0 to High(DeclNodes) do
        RootNodes += DeclNodes[i];
      Continue;
    end;

    // Explicit identifier checks for routine implementations parsed globally
    if Current.Token = tkIDENT then
    begin
      S := XprCase(Current.Value);
      if (S = 'procedure') or (S = 'function') or (S = 'constructor') or (S = 'destructor') then
      begin
        DeclNodes := ParseDeclarations();
        for i := 0 to High(DeclNodes) do
          RootNodes += DeclNodes[i];
        Continue;
      end;
    end;

    // label declarations at program level - silently skip
    if (Current.Token = tkIDENT) and (XprCase(Current.Value) = 'label') then
    begin
      Next();
      while Current.Token = tkIDENT do
      begin
        Next();
        if not NextIf(tkCOMMA) then Break;
      end;
      NextIf(tkSEMI);
      Continue;
    end;

    // Anything else is unexpected - try to recover by skipping
    if Current.Token in [tkDOT, tkSEMI] then begin Next(); Continue; end;

    FContext.RaiseExceptionFmt(
      'Unexpected token in top-level: `%s`', [Current.ToString], DocPos);
  end;

  Result := XTree_ExprList.Create(RootNodes, FContext, DocPos);
end;

// -----------------------------------------------------------------------------
//  ParseDeclarations
// -----------------------------------------------------------------------------

function TPascalParser.ParseDeclarations(): XNodeArray;
var
  FuncNode: XTree_Function;
  S: string;
begin
  Result := nil;

  if Current.Token = tkIDENT then
  begin
    S := XprCase(Current.Value);
    if (S = 'procedure') or (S = 'function') or (S = 'constructor') or (S = 'destructor') then
    begin
      FuncNode := ParseRoutine(S = 'function', S = 'constructor');
      if FuncNode <> nil then Result += FuncNode;
      Exit;
    end;
  end;

  case Current.Token of
    tkKW_TYPE:        Result := ParseTypeBlock();
    tkKW_VAR:         Result := ParseVarBlock(False);
    tkKW_CONST:       Result := ParseVarBlock(True);
    tkKW_FUNC:
      begin
        FuncNode := ParseRoutine(XprCase(Current.Value) = 'function');
        if FuncNode <> nil then Result += FuncNode;
      end;
    tkKW_CONSTRUCTOR:
      begin
        FuncNode := ParseRoutine(False, True);
        if FuncNode <> nil then Result += FuncNode;
      end;
  else
    FContext.RaiseExceptionFmt(
      'Unexpected token in declarations: `%s`', [Current.ToString], DocPos);
  end;
end;

// -----------------------------------------------------------------------------
//  Type declarations
// -----------------------------------------------------------------------------

function TPascalParser.ParseTypeBlock(): XNodeArray;
var
  TypeName, GenericParams, SourceName, MangledName: string;
  ExplicitTypes: XTypeArray;
  IsGeneric: Boolean;
  Doc: TDocPos;
  BaseType: XType;
  PtrType: XType_Pointer;
  GenericType: XTree_TypeDecl;
begin
  Result := nil;
  Next(); // consume 'type'

  while Current.Token = tkIDENT do
  begin
    Doc := DocPos;
    TypeName := Current.Value;
    Next();

    // Generic parameter list:  TFoo<T, U> = ...
    GenericParams := '';
    IsGeneric := False;
    if Current.Token = tkLT then
    begin
      Next(); // consume '<'
      IsGeneric := True;
      while Current.Token = tkIDENT do
      begin
        if GenericParams <> '' then GenericParams += ',';
        GenericParams += Current.Value;
        Next();
        if not NextIf(tkCOMMA) then Break;
      end;
      if Current.Token = tkGT then Next(); // consume '>'
    end;

    Consume(tkEQ);

    // specialize TList<Integer>
    if Current.Token = tkKW_SPECIALIZE then
    begin
      Next(); // consume 'specialize'
      SourceName := Current.Value;
      Consume(tkIDENT);
      // Handle dotted source names
      while NextIf(tkDOT) do
      begin
        SourceName += '.' + Current.Value;
        Consume(tkIDENT);
      end;
      SetLength(ExplicitTypes, 0);
      if Current.Token = tkLT then
      begin
        Next();
        repeat
          SetLength(ExplicitTypes, Length(ExplicitTypes) + 1);
          ExplicitTypes[High(ExplicitTypes)] := ParseTypeDefinition();
        until not NextIf(tkCOMMA);
        if Current.Token = tkGT then Next();
      end;
      Result += XTree_Specialize.CreateTypeSpec(TypeName, SourceName, ExplicitTypes, FContext, Doc);
      NextIf(tkSEMI);
      Continue;
    end;

    // class / object declaration
    if Current.Token = tkKW_CLASS then
    begin
      Result += ParseClassDecl(TypeName, GenericParams);
      NextIf(tkSEMI);
      Continue;
    end;

    // enum
    if Current.Token = tkLPARENTHESES then
    begin
      Result += ParseEnum(TypeName);
      NextIf(tkSEMI);
      Continue;
    end;

    // pointer: PFoo = ^TFoo
    if Current.Token = tkDEREF then
    begin
      Next(); // consume '^'
      BaseType := ParseTypeDefinition();
      PtrType := XType_Pointer.Create(BaseType);
      FContext.AddManagedType(PtrType);
      Result += XTree_TypeDecl.Create(TypeName, PtrType, FContext, Doc);
      NextIf(tkSEMI);
      Continue;
    end;

    // Anything else: record, array, named type alias, …
    if IsGeneric then
    begin
      // We have a generic alias/type for other types.
      GenericType := XTree_TypeDecl.Create(TypeName, ParseTypeDefinition(), FContext, Doc);
      GenericType.TypeParams := GenericParams.Split(',');
      SetLength(GenericType.TypeConstraints, Length(GenericType.TypeParams));
      Result += GenericType;
    end else
      Result += XTree_TypeDecl.Create(TypeName, ParseTypeDefinition(), FContext, Doc);

    NextIf(tkSEMI);
  end;
end;

function TPascalParser.ParseTypeDefinition(): XType;
var
  TypeName, MangledName: string;
  ExplicitTypes: XTypeArray;
  i: Int32;
begin
  if Current.Token = tkKW_RECORD then
  begin
    Next();
    Result := ParseRecord();
    if not NextIf(tkKW_END) then
      FContext.RaiseException('Expected `end` to close record', DocPos);
  end
  else if Current.Token = tkKW_ARRAY then
  begin
    Next();
    // Raise on optional static bounds: array[1..10] of ...
    if Current.Token = tkLSQUARE then
      FContext.RaiseException('Static arrays are not implemented', DocPos);

    Consume(tkKW_OF);
    Result := XType_Array.Create(ParseTypeDefinition());
    FContext.AddManagedType(Result);
  end
  else if Current.Token = tkDEREF then
  begin
    // ^T  inside a larger type expression
    Next();
    Result := XType_Pointer.Create(ParseTypeDefinition());
    FContext.AddManagedType(Result);
  end
  else if Current.Token = tkKW_SPECIALIZE then
  begin
    FContext.RaiseException('Specialize var not yet implemented', DocPos);
  end
  else if Current.Token = tkIDENT then
  begin
    // Handle qualified names (System.Integer etc.) – just take the last part
    TypeName := Current.Value;
    Next();
    while NextIf(tkDOT) do
    begin
      TypeName := Current.Value;
      Next();
    end;

    // 'string[N]' - treat as plain string
    if (XprCase(TypeName) = 'string') and (Current.Token = tkLSQUARE) then
    begin
      while Current.Token <> tkRSQUARE do Next();
      Next(); // consume ']'
    end;

    Result := FContext.GetType(TypeName);
    // delayed resolution, let tree handle
    if Result = nil then
    begin
      Result      := XType.Create(xtUnknown);
      Result.Name := TypeName;
      FContext.AddManagedType(Result);
    end;
  end
  else
    FContext.RaiseException(
      'Invalid type definition at `' + Current.Value + '`', DocPos);
end;

{ Enum with optional explicit ordinal values:
    type TColor = (clRed, clGreen = 5, clBlue); }
function TPascalParser.ParseEnum(const TypeName: string): XNodeArray;
var
  EnumIdx: Int64;
  EnumName: string;
  Names:  TStringArray;
  Values: array of Int64;
  i: Int32;
  Doc: TDocPos;
  ET: XType_Enum;
begin
  SetLength(Result, 0);
  Doc := DocPos;
  Consume(tkLPARENTHESES);
  EnumIdx := 0;
  SetLength(Names, 0);
  SetLength(Values, 0);

  while Current.Token = tkIDENT do
  begin
    EnumName := Current.Value;
    Next();

    // Optional explicit value:  clGreen = 5
    if NextIf(tkEQ) then
    begin
      EnumIdx := StrToInt64(Current.Value);
      Next();
    end;

    SetLength(Names,  Length(Names)  + 1); Names[High(Names)]   := EnumName;
    SetLength(Values, Length(Values) + 1); Values[High(Values)] := EnumIdx;
    Inc(EnumIdx);

    if not NextIf(tkCOMMA) then Break;
  end;
  Consume(tkRPARENTHESES);

  // Emit one const VarDecl per member
  for i := 0 to High(Names) do
    Result += XTree_VarDecl.Create(
      Names[i],
      XTree_Int.Create(IntToStr(Values[i]), FContext, Doc),
      FContext.GetType(xtInt), True, FContext, Doc);

  // Register the enum type (aliases to Int for now)
  if TypeName <> '' then
  begin
    ET := XType_Enum.Create(Names, Values);
    FContext.AddManagedType(ET);
    FContext.AddType(TypeName, ET, False);
  end;
end;

function TPascalParser.ParseRecord(): XType_Record;
var
  Fields: XStringList; Types: XTypeList;
  Idents: TStringArray; FieldType: XType; i: Integer;
begin
  Fields.Init([]); Types.Init([]);

  // Simple record body - skip variant 'case' sections for now
  while not (Current.Token in [tkKW_END, tkUNKNOWN]) do
  begin
    // Variant record case section - skip the whole thing
    if Current.Token = tkKW_CASE then
    begin
      while not (Current.Token in [tkKW_END, tkUNKNOWN]) do Next();
      Break;
    end;

    // Visibility sections inside records (rare but FPC allows them)
    if IsVisibility then begin Next(); Continue; end;

    if Current.Token <> tkIDENT then begin Next(); Continue; end;

    SetLength(Idents, 0);
    repeat
      Idents += Current.Value; Next();
    until not NextIf(tkCOMMA);

    Consume(tkCOLON);
    FieldType := ParseTypeDefinition();

    for i := 0 to High(Idents) do
    begin
      Fields.Add(Idents[i]);
      Types.Add(FieldType);
    end;
    NextIf(tkSEMI);
  end;

  Result := XType_Record.Create(Fields, Types);
  FContext.AddManagedType(Result);
end;

{ Parse a Pascal class / object declaration.
    TFoo = class(TParent)
    private
      FField: Integer;
    public
      constructor Create(AVal: Integer);
      procedure DoWork;
      property Val: Integer read FGetVal;
    end;

  Method stubs are silently skipped - their bodies arrive later as
  type-bound procedures (procedure TFoo.DoWork; begin ... end).
  Properties are silently skipped.

  The resulting XTree_ClassDecl carries only field declarations and
  type-level declarations. Methods registered later via SelfType on
  XTree_Function are attached to the class VMT by the compiler. }
function TPascalParser.ParseClassDecl(const ClsName, GenericParams: string): XTree_ClassDecl;
var
  Param, ParentName: string;
  Fields, TypeDecls, Methods: XNodeArray;
  FuncNode: XTree_Function;
  IdentList: XIdentNodeList;
  FieldType: XType;
  Doc: TDocPos;
  TypeParams: TStringArray;
  SavePos, P, depth: Int32;
  S: string;
begin
  Doc := DocPos;
  Consume(tkKW_CLASS);

  // Optional parent: class(TParent)
  ParentName := '';
  if NextIf(tkLPARENTHESES) then
  begin
    ParentName := Current.Value;
    Consume(tkIDENT);
    while NextIf(tkDOT) do
    begin
      ParentName += '.' + Current.Value;
      Consume(tkIDENT);
    end;
    Consume(tkRPARENTHESES);
  end;

  // Forward declaration: 'TFoo = class;'
  if Current.Token = tkSEMI then
  begin
    // Emit an empty class with no fields; implementations arrive later
    Result := XTree_ClassDecl.Create(ClsName, ParentName, nil, nil, nil, FContext, Doc);
    if GenericParams <> '' then
    begin
      SetLength(TypeParams, 0);
      // parse comma-separated names from the already-captured string
      P := 1;
      Param := '';
      while P <= Length(GenericParams) do
      begin
        if GenericParams[P] = ',' then
        begin
          TypeParams += Trim(Param); Param := '';
        end else
          Param += GenericParams[P];
        Inc(P);
      end;
      if Param <> '' then TypeParams += Trim(Param);
      Result.TypeParams := TypeParams;
    end;
    Exit;
  end;

  SetLength(Fields, 0);
  SetLength(TypeDecls, 0);
  SetLength(Methods, 0);

  // -- class body ------------------------------------------------------------
  while not (Current.Token in [tkKW_END, tkUNKNOWN]) do
  begin
    // Visibility keywords (private / public / protected / published / strict)
    if IsVisibility then begin Next(); Continue; end;

    // 'class var' / 'class procedure' / 'class function'
    if Current.Token = tkKW_CLASS then
    begin
      Next(); // consume 'class'
      if Current.Token in [tkKW_FUNC, tkKW_CONSTRUCTOR] then
      begin
        FuncNode := ParseRoutine(XprCase(Current.Value) = 'function', Current.Token = tkKW_CONSTRUCTOR, True, ClsName);
        if FuncNode <> nil then Methods += FuncNode;
      end
      else if Current.Token = tkIDENT then
      begin
        S := XprCase(Current.Value);
        if (S = 'procedure') or (S = 'function') or (S = 'constructor') or (S = 'destructor') then
        begin
          FuncNode := ParseRoutine(S = 'function', S = 'constructor', True, ClsName);
          if FuncNode <> nil then Methods += FuncNode;
        end
        else
        begin
          // unknown class identifier (like 'class const'), skip to semicolon
          while not (Current.Token in [tkSEMI, tkKW_END, tkUNKNOWN]) do Next();
          NextIf(tkSEMI);
        end;
      end
      else if Current.Token = tkKW_VAR then
      begin
        Next(); // consume 'var' - fall through to field parsing below
        // don't Continue; let the field parsing handle the rest
      end
      else
      begin
        // class const, class type, or unknown - skip to next semicolon
        while not (Current.Token in [tkSEMI, tkKW_END, tkUNKNOWN]) do Next();
        NextIf(tkSEMI);
      end;
      Continue;
    end;

    // Method stubs
    if Current.Token in [tkKW_FUNC, tkKW_CONSTRUCTOR] then
    begin
      FuncNode := ParseRoutine(XprCase(Current.Value) = 'function', Current.Token = tkKW_CONSTRUCTOR, True, ClsName);
      if FuncNode <> nil then Methods += FuncNode;
      Continue;
    end;

    if Current.Token = tkIDENT then
    begin
      S := XprCase(Current.Value);
      if (S = 'procedure') or (S = 'function') or (S = 'constructor') or (S = 'destructor') then
      begin
        FuncNode := ParseRoutine(S = 'function', S = 'constructor', True, ClsName);
        if FuncNode <> nil then Methods += FuncNode;
        Continue;
      end;
    end;

    // Property declarations - skip until semicolon (handling nested parens)
    if Current.Token = tkKW_PROPERTY then
    begin
      depth := 0;
      while not (Current.Token in [tkUNKNOWN]) do
      begin
        if Current.Token = tkLPARENTHESES then Inc(depth)
        else if Current.Token = tkRPARENTHESES then Dec(depth);
        if (Current.Token = tkSEMI) and (depth = 0) then begin Next(); Break; end;
        if Current.Token = tkKW_END then Break;
        Next();
      end;
      Continue;
    end;

    // Nested type block
    if Current.Token = tkKW_TYPE then
    begin
      TypeDecls += ParseTypeBlock();
      Continue;
    end;

    // Nested const block (class constants)
    if Current.Token = tkKW_CONST then
    begin
      Fields += ParseVarBlock(True);
      Continue;
    end;

    // var keyword inside class body (explicit field section)
    if Current.Token = tkKW_VAR then
    begin
      Next(); // consume 'var'
      // Fall through to field parsing
    end;

    // Field declaration:  FField [, FField2] : Type;
    if Current.Token = tkIDENT then
    begin
      // Peek ahead to see if this is truly a field (has a colon after idents)
      // rather than a stray keyword we missed.
      SavePos := FPos;
      IdentList.Init([]);
      repeat
        IdentList.Add(XTree_Identifier.Create(Current.Value, FContext, DocPos));
        Consume(tkIDENT);
      until not NextIf(tkCOMMA);

      if Current.Token = tkCOLON then
      begin
        Next(); // consume ':'
        FieldType := ParseTypeDefinition();
        Fields += XTree_VarDecl.Create(IdentList, nil, FieldType, False, FContext, Doc);
        NextIf(tkSEMI);
      end else
      begin
        // Not a field - restore and skip to next semicolon
        FPos := SavePos;
        SkipNewlines();
        while not (Current.Token in [tkSEMI, tkKW_END, tkUNKNOWN]) do Next();
        NextIf(tkSEMI);
      end;
      Continue;
    end;

    // Semi or other stray token
    if Current.Token = tkSEMI then begin Next(); Continue; end;
    Next(); // fallback: skip unknown
  end;

  if Current.Token = tkKW_END then Next(); // consume 'end'

  // PASS Methods INSTEAD OF nil!
  Result := XTree_ClassDecl.Create(ClsName, ParentName, TypeDecls, Fields, Methods, FContext, Doc);

  // Attach generic type parameter names
  if GenericParams <> '' then
  begin
    SetLength(TypeParams, 0);
    P := 1;
    Param := '';
    while P <= Length(GenericParams) do
    begin
      if GenericParams[P] = ',' then begin TypeParams += Trim(Param); Param := ''; end
      else Param += GenericParams[P];
      Inc(P);
    end;
    if Param <> '' then TypeParams += Trim(Param);
    Result.TypeParams := TypeParams;
  end;
end;

// -----------------------------------------------------------------------------
//  Var / const blocks
// -----------------------------------------------------------------------------

function TPascalParser.ParseVarBlock(IsConst: Boolean): XNodeArray;
var
  Idents: XIdentNodeList;
  VarType: XType;
  InitExpr: XTree_Node;
begin
  Result := nil;
  Next(); // consume 'var' / 'const'

  while Current.Token = tkIDENT do
  begin
    Idents.Init([]);
    repeat
      Idents.Add(XTree_Identifier.Create(Current.Value, FContext, DocPos));
      Next();
    until not NextIf(tkCOMMA);

    VarType  := nil;
    InitExpr := nil;

    if NextIf(tkCOLON) then VarType := ParseTypeDefinition();
    if NextIf(tkEQ) or NextIf(tkASGN) then InitExpr := ParseExpression();

    Result += XTree_VarDecl.Create(Idents, InitExpr, VarType, IsConst, FContext, DocPos);
    Consume(tkSEMI);
  end;
end;

// -----------------------------------------------------------------------------
//  Routine parsing  (function / procedure / constructor / destructor)
// -----------------------------------------------------------------------------

function TPascalParser.ParseRoutine(IsFunction: Boolean; IsConstructor: Boolean = False;
                      ForceForward: Boolean = False;
                      ClsName: string = ''): XTree_Function;
var
  FuncName, TypePrefix: string;
  ArgsNames: TStringArray;
  ArgsPass:  TPassArgsBy;
  ArgsTypes: XTypeArray;
  RetType:   XType;
  Body:      XTree_ExprList;
  LocalDecls:XNodeArray;
  i:         Int32;
  IsRef:     Boolean;
  ParamIdents: TStringArray;
  ParamType:   XType;
  Annotations: XTree_ExprList;
  AnnotNode:   XTree_Annotation;
  IsForward:   Boolean;
  ModName:     string;
  TypeParams:  TStringArray;
  Doc:         TDocPos;
  IsDestructor: Boolean;

  procedure AddAnnot(const AName: string; AValue: XTree_Node = nil);
  var k: Int32; AN: XTree_Annotation;
  begin
    if Annotations = nil then
      Annotations := XTree_ExprList.Create([], FContext, DocPos);
    for k := 0 to High(Annotations.List) do
      if XprCase(XTree_Identifier(XTree_Annotation(Annotations.List[k]).Identifier).Name) = AName then Exit;
    AN := XTree_Annotation.Create(FContext, DocPos);
    AN.Identifier := XTree_Identifier.Create(AName, FContext, DocPos);
    AN.Value := AValue;
    Annotations.List += AN;
  end;
var
  FullMName, SavedMods: string;
  j: Int32;
  SplitMods: TStringArray;
begin
  Doc := DocPos;
  IsDestructor := (not IsConstructor) and
                  (XprCase(Current.Value) = 'destructor');
  Next(); // consume procedure / function / constructor / destructor

  FuncName   := Current.Value;
  Consume(tkIDENT);

  // Type-bound method:  procedure TFoo.DoWork;
  TypePrefix := '';
  if NextIf(tkDOT) then
  begin
    TypePrefix := FuncName;
    FuncName   := Current.Value;
    Consume(tkIDENT);
    // nested:  TFoo.TBar.Method  (uncommon but possible)
    while NextIf(tkDOT) do
    begin
      TypePrefix := FuncName;
      FuncName   := Current.Value;
      Consume(tkIDENT);
    end;
  end
  else if ClsName <> '' then
    TypePrefix := ClsName;


  // Generic type params on function:  procedure Sort<T>(...)
  SetLength(TypeParams, 0);
  if Current.Token = tkLT then
  begin
    Next(); // '<'
    while Current.Token = tkIDENT do
    begin
      TypeParams += Current.Value;
      Next();
      if not NextIf(tkCOMMA) then Break;
    end;
    if Current.Token = tkGT then Next();
  end;

  SetLength(ArgsNames, 0);
  SetLength(ArgsPass,  0);
  SetLength(ArgsTypes, 0);
  RetType := nil;

  // Parameter list
  if NextIf(tkLPARENTHESES) then
  begin
    while Current.Token <> tkRPARENTHESES do
    begin
      // Passing convention
      IsRef := NextIf(tkKW_VAR) or NextIf(tkKW_REF) or NextIfIdent('out');
      if NextIfIdent('const') then IsRef := False;

      SetLength(ParamIdents, 0);
      repeat
        ParamIdents += Current.Value;
        Consume(tkIDENT);
      until not NextIf(tkCOMMA);

      Consume(tkCOLON);
      ParamType := ParseTypeDefinition();

      // Optional default value  (just parse & discard for now)
      if NextIf(tkEQ) then ParseExpression();

      for i := 0 to High(ParamIdents) do
      begin
        ArgsNames += ParamIdents[i];
        if IsRef then ArgsPass += pbRef else ArgsPass += pbCopy;
        ArgsTypes += ParamType;
      end;
      if not NextIf(tkSEMI) then Break;
    end;
    Consume(tkRPARENTHESES);
  end;

  // Return type for functions (not for constructor/destructor)
  if IsFunction and (not IsConstructor) and (not IsDestructor) then
  begin
    Consume(tkCOLON);
    RetType := ParseTypeDefinition();
  end;

  Consume(tkSEMI);

  // -- Modifiers ------------------------------------------------------------
  Annotations := nil;
  IsForward   := False;

  repeat
    ModName := ConsumeModifier(IsForward);
    if ModName = '' then Break;
    if IsForward then Break; // forward decl - stop immediately

    if (ModName = 'inline') or (ModName = 'jit') or (ModName = 'jitlow') then
    begin
      if ModName = 'jit' then AddAnnot(ModName, XTree_String.Create('max', FContext, DocPos))
      else if ModName = 'jitlow' then AddAnnot('jit', XTree_String.Create('low', FContext, DocPos))
      else AddAnnot(ModName, nil);
    end
    else if (ModName = 'virtual') or (ModName = 'overload') or (ModName = 'override') then
      AddAnnot(ModName, nil);
  until False;

  if ForceForward then IsForward := True;

  // 2. Modifiers declared previously in the class stub header (e.g. `Speak; virtual;`)
  if TypePrefix <> '' then
  begin
    FullMName := XprCase(TypePrefix + '.' + FuncName);

    for i := 0 to High(FMethodStubs) do
    begin
      if FMethodStubs[i].Name = FullMName then
      begin
        SavedMods := FMethodStubs[i].Modifiers;
        SplitMods := SavedMods.Split([';']);
        for j := 0 to High(SplitMods) do
        begin
          if SplitMods[j] = '' then Continue;
          ModName := SplitMods[j];
          if (ModName = 'virtual') or (ModName = 'overload') or (ModName = 'override') then
            AddAnnot(ModName, nil);
        end;
        Break;
      end;
    end;
  end;

  if IsConstructor then
    AddAnnot('virtual', nil);

  if IsForward then
  begin
    Result := XTree_Function.Create(FuncName, ArgsNames, ArgsPass, ArgsTypes, RetType, nil, FContext, Doc);
    Result.isConstructor := IsConstructor;
    Result.TypeParams    := TypeParams;
    Result.Annotations   := Annotations;
    Result.IsForwardDecl := True;
    if TypePrefix <> '' then
    begin
      Result.SelfType := FContext.GetType(TypePrefix);
      if Result.SelfType = nil then
      begin
        Result.SelfType := XType.Create(xtUnknown);
        Result.SelfType.Name := TypePrefix;
        FContext.AddManagedType(Result.SelfType);
      end;
    end;
    Exit(Result);
  end;

  // -- Body -----------------------------------------------------------------
  Body := XTree_ExprList.Create(FContext, DocPos);

  // Local declarations (var / const / type / label)
  while Current.Token in [tkKW_VAR, tkKW_TYPE, tkKW_CONST] do
  begin
    LocalDecls := ParseDeclarations();
    for i := 0 to High(LocalDecls) do Body.List += LocalDecls[i];
  end;

  // Label declarations inside a function
  if (Current.Token = tkIDENT) and (XprCase(Current.Value) = 'label') then
  begin
    Next();
    while Current.Token = tkIDENT do
    begin
      Next();
      if not NextIf(tkCOMMA) then Break;
    end;
    NextIf(tkSEMI);
  end;

  if TypePrefix <> '' then
    Body.List += XTree_With.Create(
                   [XTree_Identifier.Create('self', FContext, Doc)],
                   ParseBlock(),
                   FContext,
                   Doc
                 )
  else
    Body.List += ParseBlock();
  Consume(tkSEMI);

  // -- Assemble -------------------------------------------------------------
  Result := XTree_Function.Create(FuncName, ArgsNames, ArgsPass, ArgsTypes,
                                   RetType, Body, FContext, Doc);
  Result.isConstructor := IsConstructor;
  Result.TypeParams    := TypeParams;
  Result.Annotations := Annotations;

  // Type-bound method
  if TypePrefix <> '' then
  begin
    Result.SelfType := FContext.GetType(TypePrefix);
    if Result.SelfType = nil then
    begin
      Result.SelfType      := XType.Create(xtUnknown);
      Result.SelfType.Name := TypePrefix;
      FContext.AddManagedType(Result.SelfType);
      FContext.AddType(TypePrefix, Result.SelfType, False); // Make sure it's known!
    end;
  end;

  // If it's a method body bound to a class (and not ForceForward), it's the implementation!
  if (TypePrefix <> '') and (not ForceForward) then
    Result.IsImplementation := True;
end;

// -----------------------------------------------------------------------------
//  Statements & blocks
// -----------------------------------------------------------------------------

function TPascalParser.ParseBlock(): XTree_ExprList;
var Nodes: XNodeArray;
begin
  SetLength(Nodes, 0);
  Consume(tkKW_BEGIN);
  while not NextIf(tkKW_END) do
  begin
    if Current.Token = tkSEMI then begin Next(); Continue; end;
    Nodes += ParseStatement();
    NextIf(tkSEMI);
  end;
  Result := XTree_ExprList.Create(Nodes, FContext, DocPos);
end;

function TPascalParser.ParseStatement(): XTree_Node;
var
  S:         string;
  LabelName: string;
  Doc:       TDocPos;
  inv:       XTree_Invoke;
  Args:      XNodeArray;
  HasParens: Boolean;
begin
  Doc := DocPos;

  if Current.Token = tkKW_BEGIN then Exit(ParseBlock());

  // -- Check for goto / label placement before the expression parser ---------
  if Current.Token = tkIDENT then
  begin
    S := XprCase(Current.Value);

    // WriteLn / Write -> ParsePrint
    if (S = 'writeln') or (S = 'write') then
    begin
      Next(); // consume WriteLn/Write
      SetLength(Args, 0);
      HasParens := NextIf(tkLPARENTHESES);
      if HasParens then
      begin
        if Current.Token <> tkRPARENTHESES then
          Args := ParseExpressionList();
        Consume(tkRPARENTHESES);
      end;
      Exit(XTree_Print.Create(Args, FContext, Doc));
    end;

    // goto LabelName
    if S = 'goto' then
    begin
      Next(); // consume 'goto'
      LabelName := Current.Value;
      Consume(tkIDENT);
      Exit(XTree_Goto.Create(LabelName, FContext, Doc));
    end;

    // LabelName:   (label placement - ident followed by bare colon, not :=)
    if (Peek(1).Token = tkCOLON) then
    begin
      LabelName := Current.Value;
      Next(); // consume ident
      Next(); // consume ':'
      Exit(XTree_Label.Create(LabelName, FContext, Doc));
    end;
  end;

  case Current.Token of
    tkKW_IF:       Result := ParseIf();
    tkKW_WHILE:    Result := ParseWhile();
    tkKW_REPEAT:   Result := ParseRepeat();
    tkKW_FOR:      Result := ParseFor();
    tkKW_CASE:     Result := ParseCase();
    tkKW_TRY:      Result := ParseTry();
    tkKW_WITH:     Result := ParseWith();
    tkKW_RAISE:    Result := ParseRaise();
    tkKW_PRINT:    Result := ParsePrint();
    tkKW_BREAK:    begin Result := XTree_Break.Create(FContext, Doc);    Next(); end;
    tkKW_CONTINUE: begin Result := XTree_Continue.Create(FContext, Doc); Next(); end;
    // 'pass' is an Express no-op - treat as empty statement
    tkKW_PASS:     begin Result := XTree_Pass.Create(FContext, Doc);     Next(); end;
  else
    begin
      Result := ParseExpression();
      // In Pascal, a standalone identifier or field access used as a statement
      // is a parameterless procedure call (e.g. `MyGreeter.SayHello;`).
      if Result is XTree_Identifier then
      begin
        Result := XTree_Invoke.Create(Result, nil, FContext, Result.FDocPos);
      end
      else if Result is XTree_Field then
      begin
        if (XTree_Field(Result).Right is XTree_Identifier) and
           (XprCase(XTree_Identifier(XTree_Field(Result).Right).Name) = 'free') then
        begin
          Result := XTree_Pass.Create(FContext, Result.FDocPos);
        end
        else
        begin
          inv := XTree_Invoke.Create(XTree_Field(Result).Right, nil, FContext, Result.FDocPos);
          inv.SelfExpr := XTree_Field(Result).Left;
          Result := inv;
        end;
      end
      else if Result is XTree_Invoke then
      begin
        inv := XTree_Invoke(Result);
        if (inv.Method is XTree_Identifier) and
           (XprCase(XTree_Identifier(inv.Method).Name) = 'free') and
           (inv.SelfExpr <> nil) then
        begin
          Result := XTree_Pass.Create(FContext, Result.FDocPos);
        end;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------
//  Individual statement parsers
// -----------------------------------------------------------------------------

function TPascalParser.ParsePrint(): XTree_Print;
var Args: XNodeArray; Doc: TDocPos; HasParens: Boolean;
begin
  Doc := DocPos;
  Consume(tkKW_PRINT);
  SetLength(Args, 0);
  HasParens := NextIf(tkLPARENTHESES);
  if HasParens then
  begin
    if Current.Token <> tkRPARENTHESES then
      Args := ParseExpressionList();
    Consume(tkRPARENTHESES);
  end else
  begin
    if not (Current.Token in [tkSEMI, tkKW_END, tkKW_ELSE, tkKW_UNTIL, tkUNKNOWN]) then
      Args := ParseExpressionList();
  end;
  Result := XTree_Print.Create(Args, FContext, Doc);
end;

function TPascalParser.ParseIf(): XTree_If;
var Cond, ThenBody: XTree_Node; ElseBody: XTree_ExprList; Doc: TDocPos;
begin
  Doc := DocPos;
  Consume(tkKW_IF);
  Cond := ParseExpression();
  Consume(tkKW_THEN);
  ThenBody := ParseStatement();
  ElseBody := nil;
  if NextIf(tkKW_ELSE) then
    ElseBody := XTree_ExprList.Create(ParseStatement(), FContext, DocPos);
  Result := XTree_If.Create([Cond], [ThenBody], ElseBody, FContext, Doc);
end;

function TPascalParser.ParseWhile(): XTree_While;
var Cond: XTree_Node; Body: XTree_ExprList; Doc: TDocPos;
begin
  Doc := DocPos;
  Consume(tkKW_WHILE);
  Cond := ParseExpression();
  Consume(tkKW_DO);
  Body := XTree_ExprList.Create(ParseStatement(), FContext, DocPos);
  Result := XTree_While.Create(Cond, Body, FContext, Doc);
end;

function TPascalParser.ParseRepeat(): XTree_Repeat;
var Nodes: XNodeArray; Doc: TDocPos;
begin
  Doc := DocPos;
  Consume(tkKW_REPEAT);
  SetLength(Nodes, 0);
  while Current.Token <> tkKW_UNTIL do
  begin
    if Current.Token = tkSEMI then begin Next(); Continue; end;
    Nodes += ParseStatement();
    NextIf(tkSEMI);
  end;
  Consume(tkKW_UNTIL);
  Result := XTree_Repeat.Create(
    ParseExpression(),
    XTree_ExprList.Create(Nodes, FContext, Doc),
    FContext, Doc);
end;

{ Handles both for..to/downto (→ XTree_ForTo) and for..in (→ XTree_ForIn).
  Also supports  for var i := lo to hi do  (DeclareIdent = True). }
function TPascalParser.ParseFor(): XTree_Node;
var
  IterIdent:   XTree_Identifier;
  StartExpr, EndExpr, Collection: XTree_Node;
  IsDownTo:    Boolean;
  DeclareIdent: Boolean;
  Body:        XTree_ExprList;
  Doc:         TDocPos;
begin
  Doc := DocPos;
  Consume(tkKW_FOR);

  // Optional 'var' before the loop variable
  DeclareIdent := NextIf(tkKW_VAR);

  IterIdent := XTree_Identifier.Create(Current.Value, FContext, DocPos);
  Consume(tkIDENT);

  // for x in collection do  →  XTree_ForIn
  if Current.Token = tkIN then
  begin
    Next(); // consume 'in'
    Collection := ParseExpression();
    Consume(tkKW_DO);
    Body := XTree_ExprList.Create(ParseStatement(), FContext, DocPos);
    // DeclareIdent=0 means use existing var; 1 means declare it
    Result := XTree_ForIn.Create(IterIdent, Collection,
                                  Byte(DeclareIdent), Body, FContext, Doc);
    Exit;
  end;

  // for i := lo to/downto hi do  →  XTree_ForTo
  Consume(tkASGN);
  StartExpr := ParseExpression();

  if NextIf(tkKW_TO) then IsDownTo := False
  else if NextIf(tkKW_DOWNTO) then IsDownTo := True
  else FContext.RaiseException('Expected "to" or "downto"', DocPos);

  EndExpr := ParseExpression();
  Consume(tkKW_DO);
  Body := XTree_ExprList.Create(ParseStatement(), FContext, DocPos);

  Result := XTree_ForTo.Create(
    IterIdent, StartExpr, EndExpr,
    nil,          // StepExpr - nil = implicit ±1
    IsDownTo,
    DeclareIdent,
    Body, FContext, Doc);
end;

function TPascalParser.ParseCase(): XTree_Case;
var
  Expr:          XTree_Node;
  Branches:      specialize TArrayList<TCaseBranch>;
  ElseBody:      XTree_Node;
  Cur:           TCaseBranch;
  Doc:           TDocPos;
  ElseNodes:     XNodeArray;
begin
  Doc := DocPos;
  Consume(tkKW_CASE);
  Expr := ParseExpression();
  Consume(tkKW_OF);
  Branches.Init([]);
  ElseBody := nil;

  while not (Current.Token in [tkKW_END, tkKW_ELSE, tkUNKNOWN]) do
  begin
    Cur.Labels.Init([]);
    repeat Cur.Labels.Add(ParseExpression()); until not NextIf(tkCOMMA);
    Consume(tkCOLON);
    Cur.Body := ParseStatement();
    Branches.Add(Cur);
    NextIf(tkSEMI);
  end;

  if NextIf(tkKW_ELSE) then
  begin
    SetLength(ElseNodes, 0);
    while not NextIf(tkKW_END) do
    begin
      if Current.Token = tkSEMI then begin Next(); Continue; end;
      ElseNodes += ParseStatement();
      NextIf(tkSEMI);
    end;
    ElseBody := XTree_ExprList.Create(ElseNodes, FContext, DocPos);
  end else
    NextIf(tkKW_END);

  Result := XTree_Case.Create(Expr, Branches.RawOfManaged(), ElseBody, FContext, Doc);
end;

{ try..except [on E: EType do / catch-all]  →  XTree_Try
  try..finally                               →  XTree_TryFinally
  Note: Pascal does not allow mixing except and finally in one try block. }
function TPascalParser.ParseTry(): XTree_Node;
var
  TryBody:     XTree_ExprList;
  FinallyBody: XTree_ExprList;
  Handlers:    specialize TArrayList<TExceptionHandler>;
  CurHandler:  TExceptionHandler;
  ElseBody:    XTree_Node;
  TryNodes, FinNodes, ElseNodes: XNodeArray;
  Doc:         TDocPos;
  ExcTypeName: string;
begin
  Doc := DocPos;
  Consume(tkKW_TRY);

  // Collect try body
  SetLength(TryNodes, 0);
  while not (Current.Token in [tkKW_EXCEPT, tkKW_FINALLY, tkUNKNOWN]) do
  begin
    if Current.Token = tkSEMI then begin Next(); Continue; end;
    TryNodes += ParseStatement();
    NextIf(tkSEMI);
  end;
  TryBody := XTree_ExprList.Create(TryNodes, FContext, DocPos);

  // -- finally ---------------------------------------------------------------
  if NextIf(tkKW_FINALLY) then
  begin
    SetLength(FinNodes, 0);
    while not (Current.Token in [tkKW_END, tkUNKNOWN]) do
    begin
      if Current.Token = tkSEMI then begin Next(); Continue; end;
      FinNodes += ParseStatement();
      NextIf(tkSEMI);
    end;
    Consume(tkKW_END);
    FinallyBody := XTree_ExprList.Create(FinNodes, FContext, DocPos);
    Exit(XTree_TryFinally.Create(TryBody, FinallyBody, FContext, Doc));
  end;

  // -- except ----------------------------------------------------------------
  Consume(tkKW_EXCEPT);
  Handlers.Init([]);
  ElseBody := nil;

  if Current.Token = tkKW_ON then
  begin
    // Typed handlers:  on E: EType do stmt
    while NextIf(tkKW_ON) do
    begin
      // Optional var name before colon: on E: EMyException do
      if (Current.Token = tkIDENT) and (Peek(1).Token = tkCOLON) then
      begin
        CurHandler.VarName := Current.Value;
        Consume(tkIDENT);
        Consume(tkCOLON);
      end else
        CurHandler.VarName := '_e';

      // Exception type: may be dotted (SysUtils.Exception)
      ExcTypeName := Current.Value;
      Consume(tkIDENT);
      while NextIf(tkDOT) do
      begin
        ExcTypeName += '.' + Current.Value;
        Consume(tkIDENT);
      end;
      CurHandler.ExceptionType := FContext.GetType(ExcTypeName);
      Consume(tkKW_DO);
      CurHandler.Body := ParseStatement();
      Handlers.Add(CurHandler);
      NextIf(tkSEMI);
    end;

    // Optional catch-all after typed handlers
    if not (Current.Token in [tkKW_END, tkUNKNOWN]) then
    begin
      SetLength(ElseNodes, 0);
      while not (Current.Token in [tkKW_END, tkUNKNOWN]) do
      begin
        if Current.Token = tkSEMI then begin Next(); Continue; end;
        ElseNodes += ParseStatement();
        NextIf(tkSEMI);
      end;
      ElseBody := XTree_ExprList.Create(ElseNodes, FContext, DocPos);
    end;
  end else
  begin
    // Pure catch-all except block (no 'on' handlers)
    SetLength(ElseNodes, 0);
    while not (Current.Token in [tkKW_END, tkUNKNOWN]) do
    begin
      if Current.Token = tkSEMI then begin Next(); Continue; end;
      ElseNodes += ParseStatement();
      NextIf(tkSEMI);
    end;
    ElseBody := XTree_ExprList.Create(ElseNodes, FContext, DocPos);
  end;

  Consume(tkKW_END);
  Result := XTree_Try.Create(TryBody, Handlers.RawOfManaged(), ElseBody, FContext, Doc);
end;

{ with obj [, obj2 ...] do stmt }
function TPascalParser.ParseWith(): XTree_With;
var Subjects: XNodeArray; Body: XTree_ExprList; Doc: TDocPos;
begin
  Doc := DocPos;
  Consume(tkKW_WITH);
  SetLength(Subjects, 0);
  repeat
    Subjects += ParseExpression();
  until not NextIf(tkCOMMA);
  Consume(tkKW_DO);
  Body := XTree_ExprList.Create(ParseStatement(), FContext, DocPos);
  Result := XTree_With.Create(Subjects, Body, FContext, Doc);
end;

{ raise Expr       - raise a new or existing exception object
  raise            - re-raise the current exception (bare raise) }
function TPascalParser.ParseRaise(): XTree_Raise;
var Doc: TDocPos; ExcExpr: XTree_Node;
begin
  Doc := DocPos;
  Consume(tkKW_RAISE);
  if Current.Token in [tkSEMI, tkKW_END, tkKW_ELSE, tkKW_EXCEPT,
                        tkKW_FINALLY, tkUNKNOWN] then
    ExcExpr := nil   // bare raise = re-raise
  else
    ExcExpr := ParseExpression();
  Result := XTree_Raise.Create(ExcExpr, FContext, Doc);
end;

// -----------------------------------------------------------------------------
//  Expression parser  (Pratt style)
// -----------------------------------------------------------------------------

function TPascalParser.ParseExpressionList(): XNodeArray;
begin
  SetLength(Result, 0);
  if Current.Token = tkRPARENTHESES then Exit;
  repeat
    Result += ParseExpression();
  until not NextIf(tkCOMMA);
end;

function TPascalParser.ParsePrimary(): XTree_Node;
var
  Doc:     TDocPos;
  op:      TToken;
  Args:    XNodeArray;
  RetExpr: XTree_Node;
  LValue, Step: XTree_Node;
  IsInc:   Boolean;
  S:       string;
  StepOp:  EOperator;
begin
  Doc := DocPos;

  // -- Identifier-keyed special forms ----------------------------------------
  if Current.Token = tkIDENT then
  begin
    S := XprCase(Current.Value);

    // Exit([expr])  →  XTree_Return
    if S = 'exit' then
    begin
      Next(); RetExpr := nil;
      if NextIf(tkLPARENTHESES) then
      begin
        if Current.Token <> tkRPARENTHESES then RetExpr := ParseExpression();
        Consume(tkRPARENTHESES);
      end;
      Exit(XTree_Return.Create(RetExpr, FContext, Doc));
    end;

    // Inc(x[, n])  →  x := x + n
    // Dec(x[, n])  →  x := x - n
    if (S = 'inc') or (S = 'dec') then
    begin
      IsInc := (S = 'inc');
      Next(); Consume(tkLPARENTHESES);
      LValue := ParseExpression();
      Step   := nil;
      if NextIf(tkCOMMA) then Step := ParseExpression()
      else Step := XTree_Int.Create('1', FContext, Doc);
      Consume(tkRPARENTHESES);
      StepOp := op_ADD;
      if not IsInc then StepOp := op_SUB;
      Exit(XTree_Assign.Create(op_Asgn, LValue,
        XTree_BinaryOp.Create(StepOp, LValue, Step, FContext, Doc),
        FContext, Doc));
    end;

    // Assigned(p)  →  p <> nil
    if S = 'assigned' then
    begin
      Next(); Consume(tkLPARENTHESES);
      LValue := ParseExpression();
      Consume(tkRPARENTHESES);
      Exit(XTree_BinaryOp.Create(op_NEQ, LValue,
             XTree_Pointer.Create('nil', FContext, Doc),
             FContext, Doc));
    end;
  end;

  // -- inherited -------------------------------------------------------------
  if Current.Token = tkKW_INHERITED then
  begin
    Next(); // consume 'inherited'
    SetLength(Args, 0);
    // Optional method name (consumed but not currently forwarded to the node)
    if (Current.Token = tkIDENT) and (Peek(1).Token = tkLPARENTHESES) then
      Next(); // skip name for now - Express InheritedCall uses current method name
    if NextIf(tkLPARENTHESES) then
    begin
      if Current.Token <> tkRPARENTHESES then Args := ParseExpressionList();
      Consume(tkRPARENTHESES);
    end;
    Exit(XTree_InheritedCall.Create(Args, FContext, Doc));
  end;

  // -- Atoms -----------------------------------------------------------------
  case Current.Token of
    tkINTEGER: begin Result := XTree_Int.Create(Current.Value, FContext, Doc);     Next(); end;
    tkFLOAT:   begin Result := XTree_Float.Create(Current.Value, FContext, Doc);   Next(); end;
    tkSTRING:  begin Result := XTree_String.Create(Current.Value, FContext, Doc);  Next(); end;
    tkCHAR:    begin Result := XTree_Char.Create(Current.Value, FContext, Doc);    Next(); end;
    tkBOOL:    begin Result := XTree_Bool.Create(Current.Value, FContext, Doc);    Next(); end;
    tkKW_NIL:  begin Result := XTree_Pointer.Create('nil', FContext, Doc);         Next(); end;
    tkIDENT:   begin Result := XTree_Identifier.Create(Current.Value, FContext, Doc); Next(); end;

    // Array literal  [a, b, c]
    tkLSQUARE:
      begin
        Next(); SetLength(Args, 0);
        if Current.Token <> tkRSQUARE then
          repeat Args += ParseExpression(); until not NextIf(tkCOMMA);
        Consume(tkRSQUARE);
        Result := XTree_InitializerList.Create(Args, FContext, Doc);
      end;

    // Grouped expression  (expr)
    tkLPARENTHESES:
      begin
        Next();
        Result := ParseExpression();
        Consume(tkRPARENTHESES);
      end;

  else
    if IsPascalUnary(Current.Token) then
    begin
      op := Current; Next();
      Result := XTree_UnaryOp.Create(
        AsOperator(op.Token),
        RHSExpr(ParsePrimary(), 8),
        FContext, Doc);
    end else
      FContext.RaiseExceptionFmt(
        'Unexpected token in expression: `%s`', [Current.ToString], Doc);
  end;
end;

function TPascalParser.RHSExpr(Left: XTree_Node;
                                 leftPrecedence: Int8 = 0): XTree_Node;
var
  precedence, nextPrecedence: Int8;
  Right: XTree_Node;
  op:    TToken;
  Doc:   TDocPos;
  clsName: string;
  cArgs: XNodeArray;
begin
  while True do
  begin
    precedence := GetPascalPrecedence(Current.Token);
    if precedence < leftPrecedence then Exit(Left);

    op := Current; Doc := DocPos; Next();

    // Pointer dereference  x^
    if AsOperator(op.Token) = op_DEREF then
    begin
      Left := XTree_UnaryOp.Create(op_DEREF, Left, FContext, Doc);
      Continue;
    end;

    // Function / method call  f(...)
    if op.Token = tkLPARENTHESES then
    begin
      if Left is XTree_Field then
      begin
        Result := XTree_Invoke.Create(
          XTree_Field(Left).Right, ParseExpressionList(), FContext, Left.FDocPos);
        XTree_Invoke(Result).SelfExpr := XTree_Field(Left).Left;
      end else
        Result := XTree_Invoke.Create(Left, ParseExpressionList(), FContext, Left.FDocPos);
      Consume(tkRPARENTHESES);
      Left := Result;
      Continue;
    end;

    // Array indexing  a[i] / a[i, j]
    if op.Token = tkLSQUARE then
    begin
      repeat
        Right := ParseExpression();
        Left  := XTree_Index.Create(Left, Right, FContext, Doc);
      until not NextIf(tkCOMMA);
      Consume(tkRSQUARE);
      Continue;
    end;

    // All other binary operators
    Right := ParsePrimary();
    if Right = nil then
      FContext.RaiseException('Invalid expression', DocPos);

    if AsOperator(op.Token) <> op_Dot then
    begin
      nextPrecedence := GetPascalPrecedence(Current.Token);
      if precedence < nextPrecedence then
        Right := RHSExpr(Right, precedence + 1)
      else if precedence = nextPrecedence then
        Right := RHSExpr(Right, precedence + GetPascalAssoc(op.Token));
    end;

    case AsOperator(op.Token) of
      op_Dot:
        begin
          if (Right is XTree_Identifier) and (XprCase(XTree_Identifier(Right).Name) = 'create') then
          begin
            clsName := '';
            if Left is XTree_Identifier then
              clsName := XTree_Identifier(Left).Name
            else if (Left is XTree_Field) and (XTree_Field(Left).Right is XTree_Identifier) and (XTree_Field(Left).Left is XTree_Identifier) then
              clsName := XTree_Identifier(XTree_Field(Left).Left).Name + '.' + XTree_Identifier(XTree_Field(Left).Right).Name;

            if clsName <> '' then
            begin
              SetLength(cArgs, 0);
              if Current.Token = tkLPARENTHESES then
              begin
                Next();
                if Current.Token <> tkRPARENTHESES then cArgs := ParseExpressionList();
                Consume(tkRPARENTHESES);
              end;
              Left := XTree_ClassCreate.Create(clsName, cArgs, FContext, Doc);
              Continue;
            end;
          end;
          Left := XTree_Field.Create(Left, Right, FContext, Doc);
        end;
      op_Asgn: Left := XTree_Assign.Create(op_Asgn, Left, Right, FContext, Doc);
    else
      Left := XTree_BinaryOp.Create(AsOperator(op.Token), Left, Right, FContext, Doc);
    end;
  end;
  Result := Left;
end;

function TPascalParser.ParseExpression(): XTree_Node;
begin
  Result := ParsePrimary();
  if Result <> nil then Result := RHSExpr(Result);
end;

end.
