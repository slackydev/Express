unit xpr.PascalParser;
{
  Author: Jarl K. Holta (Pascal/Lape compatibility layer)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Full Recursive-Descent Pascal/Lape Parser for the Express VM.
  - Fully line insensitive (ignores tkNEWLINE).
  - Uses strictly Pascal Operator Precedence.
  - Desugars Enums into Constants at parse-time.
  - Desugars `for..to` and `for..downto` into C-style XTree_For.
  - Desugars `Exit(expr)` into XTree_Return.
  - Desugars `WriteLn` / `Write` into XTree_Print.
  - Implicit `Result` variable is natively supported by the Express backend.
  - Supports Type-bound methods (`procedure TNode.DoSomething;`).
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
  TPascalParser = class(TObject)
  private
    FPos:       Int32;
    FTokenizer: TTokenizer;
    FContext:   TCompilerContext;

    function DocPos: TDocPos; inline;
    function Current: TToken; inline;
    function Peek(n: Int32 = 1): TToken;

    function Next(): TToken;
    function NextIf(Token: ETokenKind): Boolean;
    function NextIfIdent(const IdentVal: string): Boolean;
    procedure Consume(Token: ETokenKind);
    procedure SkipNewlines; inline;

    // Translated Lape Precedence
    function GetPascalPrecedence(Token: ETokenKind): Int8;
    function GetPascalAssoc(Token: ETokenKind): Int8;
    function IsPascalUnary(Token: ETokenKind): Boolean;

  public
    constructor Create(T: TTokenizer; ctx: TCompilerContext);

    function ParseProgram(): XTree_Node;
    function ParseDeclarations(): XNodeArray;

    function ParseTypeBlock(): XNodeArray;
    function ParseTypeDefinition(): XType;
    function ParseEnum(TypeName: string): XNodeArray;
    function ParseRecord(): XType_Record;

    function ParseVarBlock(IsConst: Boolean): XNodeArray;
    function ParseRoutine(IsFunction: Boolean): XTree_Function;

    function ParseBlock(): XTree_ExprList;
    function ParseStatement(): XTree_Node;

    function ParseIf(): XTree_If;
    function ParseWhile(): XTree_While;
    function ParseRepeat(): XTree_Repeat;
    function ParseFor(): XTree_For;
    function ParseCase(): XTree_Case;
    function ParseTry(): XTree_Try;
    function ParsePrint(): XTree_Print;

    function ParseExpression(): XTree_Node;
    function ParsePrimary(): XTree_Node;
    function RHSExpr(Left: XTree_Node; leftPrecedence: Int8 = 0): XTree_Node;
    function ParseExpressionList(): XNodeArray;
  end;

function ParsePascal(Tokenizer: TTokenizer; ctx: TCompilerContext = nil): XTree_Node;

implementation

uses xpr.Errors, xpr.Utils;

function ParsePascal(Tokenizer: TTokenizer; ctx: TCompilerContext = nil): XTree_Node;
var
  Parser: TPascalParser;
begin
  if ctx = nil then ctx := TCompilerContext.Create(Tokenizer.Data);
  Parser := TPascalParser.Create(Tokenizer, ctx);
  Result := Parser.ParseProgram();
  Parser.Free();
end;

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
    FContext.RaiseExceptionFmt('Expected `%s` but found `%s`', [TokenToString(Token), Current.ToString], DocPos);
  Next();
end;

// ─── LAPE OPERATOR PRECEDENCE TO EXPRESS CONVERSION ────────────────────────
{
  Lape uses 1 as highest precedence and 8 as lowest.
  Express uses Pratt Parsing where Higher Number = Tighter Binding.
  Formula: ExpressPrec = 10 - LapePrec
}
function TPascalParser.GetPascalPrecedence(Token: ETokenKind): Int8;
begin
  case Token of
    tkDOT, tkLSQUARE, tkDEREF, tkLPARENTHESES: Result := 9;
    tkAT:                      Result := 8;
    tkNOT:                     Result := 7;
    tkPOW:                     Result := 6;
    tkMUL, tkDIV, tkMOD,
    tkAND, tkSHL, tkSHR:       Result := 5;
    tkPLUS, tkMINUS,
    tkOR, tkXOR:               Result := 4;
    tkEQ, tkNE, tkLT, tkLTE,
    tkGT, tkGTE, tkIN, tkKW_IS:Result := 3;
    tkASGN:                    Result := 2;
  else
    Result := -1;
  end;
end;

{ Lape assocLeft -> Express 1, Lape assocRight -> Express 0 }
function TPascalParser.GetPascalAssoc(Token: ETokenKind): Int8;
begin
  case Token of
    tkASGN, tkPOW, tkNOT, tkAT: Result := 0; // Right associative
  else
    Result := 1; // Left associative
  end;
end;

function TPascalParser.IsPascalUnary(Token: ETokenKind): Boolean;
begin
  Result := Token in [tkNOT, tkAT, tkMINUS, tkPLUS];
end;

// ─── PROGRAM STRUCTURE ─────────────────────────────────────────────────────

function TPascalParser.ParseProgram(): XTree_Node;
var
  RootNodes: XNodeArray;
  DeclNodes: XNodeArray;
  i: Int32;
begin
  SetLength(RootNodes, 0);

  if NextIf(tkKW_PROGRAM) or NextIfIdent('library') or NextIfIdent('unit') then
  begin
    Consume(tkIDENT);
    Consume(tkSEMI);
  end;

  if NextIfIdent('uses') then
  begin
    repeat
      RootNodes += XTree_ImportUnit.Create(Current.Value, '', FContext, DocPos);
      Consume(tkIDENT);
    until not NextIf(tkCOMMA);
    Consume(tkSEMI);
  end;

  while not (Current.Token in [tkKW_BEGIN, tkUNKNOWN]) do
  begin
    DeclNodes := ParseDeclarations();
    for i := 0 to High(DeclNodes) do
      RootNodes += DeclNodes[i];
  end;

  if Current.Token = tkKW_BEGIN then
  begin
    RootNodes += ParseBlock();
    if Current.Token = tkDOT then Next(); // end.
  end;

  Result := XTree_ExprList.Create(RootNodes, FContext, DocPos);
end;

function TPascalParser.ParseDeclarations(): XNodeArray;
begin
  SetLength(Result, 0);

  case Current.Token of
    tkKW_TYPE: Result := ParseTypeBlock();
    tkKW_VAR:  Result := ParseVarBlock(False);
    tkKW_CONST:Result := ParseVarBlock(True);
    tkKW_FUNC:
      begin
        // Both procedure and function map to tkKW_FUNC in our PascalTokenizer
        Result += ParseRoutine(XprCase(Current.Value) = 'function');
      end;
  else
    FContext.RaiseExceptionFmt('Unexpected token in declarations: `%s`', [Current.ToString], DocPos);
  end;
end;

// ─── DECLARATIONS ──────────────────────────────────────────────────────────

function TPascalParser.ParseTypeBlock(): XNodeArray;
var
  TypeName: string;
begin
  SetLength(Result, 0);
  Next(); // consume 'type'

  while Current.Token = tkIDENT do
  begin
    TypeName := Current.Value;
    Next();
    Consume(tkEQ); // '='

    if Current.Token = tkLPARENTHESES then
      Result += ParseEnum(TypeName)
    else
      FContext.AddType(TypeName, ParseTypeDefinition(), True);

    Consume(tkSEMI);
  end;
end;

function TPascalParser.ParseTypeDefinition(): XType;
begin
  if Current.Token = tkKW_RECORD then
  begin
    Next(); // consume 'record'
    Result := ParseRecord();
    if not NextIf(tkKW_END) then
      FContext.RaiseException('Expected `end` to close record', DocPos);
  end
  else if Current.Token = tkKW_ARRAY then
  begin
    Next(); // consume 'array'
    // Ignore explicit array sizing [1..10] (treat as dynamic arrays)
    if Current.Token = tkLSQUARE then
    begin
      while Current.Token <> tkRSQUARE do Next();
      Consume(tkRSQUARE);
    end;

    if Current.Token = tkKW_OF then // Strictly matched to token now
    begin
      Next(); // consume 'of'
      Result := XType_Array.Create(ParseTypeDefinition());
      FContext.AddManagedType(Result);
    end else
      FContext.RaiseException('Expected `of` after array', DocPos);
  end
  else if Current.Token = tkIDENT then
  begin
    Result := FContext.GetType(Current.Value);
    if Result = nil then
      FContext.RaiseExceptionFmt('Unknown type `%s`', [Current.Value], DocPos);
    Next();
  end
  else
    FContext.RaiseException('Invalid type definition', DocPos);
end;

function TPascalParser.ParseEnum(TypeName: string): XNodeArray;
var
  EnumIdx: Int32;
  EnumName: string;
begin
  SetLength(Result, 0);
  Consume(tkLPARENTHESES);
  EnumIdx := 0;

  while Current.Token = tkIDENT do
  begin
    EnumName := Current.Value;
    Next();

    Result += XTree_VarDecl.Create(
      EnumName,
      XTree_Int.Create(IntToStr(EnumIdx), FContext, DocPos),
      FContext.GetType(xtInt), True, FContext, DocPos
    );
    Inc(EnumIdx);
    if not NextIf(tkCOMMA) then Break;
  end;
  Consume(tkRPARENTHESES);

  if TypeName <> '' then
    FContext.AddType(TypeName, FContext.GetType(xtInt), False);
end;

function TPascalParser.ParseRecord(): XType_Record;
var
  Fields: XStringList; Types: XTypeList; Idents: TStringArray;
  FieldType: XType; i: Integer;
begin
  Fields.Init([]); Types.Init([]);
  while Current.Token = tkIDENT do
  begin
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
    Consume(tkSEMI);
  end;
  Result := XType_Record.Create(Fields, Types);
  FContext.AddManagedType(Result);
end;

function TPascalParser.ParseVarBlock(IsConst: Boolean): XNodeArray;
var
  Idents: XIdentNodeList; VarType: XType; InitExpr: XTree_Node;
begin
  SetLength(Result, 0);
  Next(); // var/const
  while Current.Token = tkIDENT do
  begin
    Idents.Init([]);
    repeat
      Idents.Add(XTree_Identifier.Create(Current.Value, FContext, DocPos));
      Next();
    until not NextIf(tkCOMMA);

    VarType := nil; InitExpr := nil;

    if NextIf(tkCOLON) then VarType := ParseTypeDefinition();
    if NextIf(tkEQ) or NextIf(tkASGN) then InitExpr := ParseExpression();

    Result += XTree_VarDecl.Create(Idents, InitExpr, VarType, IsConst, FContext, DocPos);
    Consume(tkSEMI);
  end;
end;

function TPascalParser.ParseRoutine(IsFunction: Boolean): XTree_Function;
var
  FuncName, TypePrefix: string;
  ArgsNames: TStringArray; ArgsPass: TPassArgsBy; ArgsTypes: XTypeArray;
  RetType: XType; Body: XTree_ExprList; LocalDecls: XNodeArray;
  i: Int32; IsRef: Boolean; ParamIdents: TStringArray; ParamType: XType;
  Annotations: XTree_ExprList;
  AnnotateNode: XTree_Annotation;
  ModName: string;
begin
  Next(); // consume procedure/function

  FuncName := Current.Value;
  Consume(tkIDENT);

  // Support Type-Bound methods (procedure TMyNode.DoWork;)
  TypePrefix := '';
  if NextIf(tkDOT) then
  begin
    TypePrefix := FuncName;
    FuncName := Current.Value;
    Consume(tkIDENT);
  end;

  SetLength(ArgsNames, 0); SetLength(ArgsPass, 0); SetLength(ArgsTypes, 0);
  RetType := nil;

  if NextIf(tkLPARENTHESES) then
  begin
    while Current.Token <> tkRPARENTHESES do
    begin
      IsRef := NextIf(tkKW_VAR) or NextIf(tkKW_REF) or NextIfIdent('out');
      if NextIfIdent('const') then IsRef := False;

      SetLength(ParamIdents, 0);
      repeat
        ParamIdents += Current.Value; Consume(tkIDENT);
      until not NextIf(tkCOMMA);

      Consume(tkCOLON);
      ParamType := ParseTypeDefinition();

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

  if IsFunction then
  begin
    Consume(tkCOLON);
    RetType := ParseTypeDefinition();
  end;
  Consume(tkSEMI);

  // --- PARSE PASCAL MODIFIERS INTO EXPRESS ANNOTATIONS ---
  Annotations := nil;
  while Current.Token = tkIDENT do
  begin
    ModName := XprCase(Current.Value);
    if (ModName = 'inline') or (ModName = 'jit') or
       (ModName = 'overload') or (ModName = 'override') then
    begin
      Next(); // consume the modifier identifier

      // Translate specific modifiers to Express AST annotations
      if (ModName = 'inline') or (ModName = 'jit') then
      begin
        if Annotations = nil then
          Annotations := XTree_ExprList.Create([], FContext, DocPos);

        AnnotateNode := XTree_Annotation.Create(FContext, DocPos);
        AnnotateNode.Identifier := XTree_Identifier.Create(ModName, FContext, DocPos);

        if ModName = 'jit' then
          AnnotateNode.Value := XTree_String.Create('max', FContext, DocPos)
        else
          // A nil value in Express annotations implies True (e.g., @inline === @inline(True))
          AnnotateNode.Value := nil;

        Annotations.List += AnnotateNode;
      end;

      // In Pascal, modifiers are typically terminated with a semicolon
      if Current.Token = tkSEMI then Next();
    end
    else
      Break; // Not a recognized routine modifier, break the loop
  end;
  // -------------------------------------------------------

  Body := XTree_ExprList.Create(FContext, DocPos);

  while (Current.Token in [tkKW_VAR, tkKW_TYPE, tkKW_CONST]) do
  begin
    LocalDecls := ParseDeclarations();
    for i := 0 to High(LocalDecls) do Body.List += LocalDecls[i];
  end;

  Body.List += ParseBlock();
  Consume(tkSEMI);

  Result := XTree_Function.Create(FuncName, ArgsNames, ArgsPass, ArgsTypes, RetType, Body, FContext, DocPos);

  // Attach the annotations we built
  Result.Annotations := Annotations;

  if TypePrefix <> '' then
  begin
    Result.SelfType := FContext.GetType(TypePrefix);
    if Result.SelfType = nil then FContext.RaiseExceptionFmt('Unknown type `%s` for method binding', [TypePrefix], DocPos);
  end;
end;

// ─── STATEMENTS & BLOCKS ───────────────────────────────────────────────────

function TPascalParser.ParseBlock(): XTree_ExprList;
var
  Nodes: XNodeArray;
begin
  SetLength(Nodes, 0);
  Consume(tkKW_BEGIN);

  while not NextIf(tkKW_END) do
  begin
    if Current.Token = tkSEMI then begin Next(); Continue; end;
    Nodes += ParseStatement();
    if Current.Token = tkSEMI then Next();
  end;
  Result := XTree_ExprList.Create(Nodes, FContext, DocPos);
end;

function TPascalParser.ParseStatement(): XTree_Node;
begin
  if Current.Token = tkKW_BEGIN then Exit(ParseBlock());

  case Current.Token of
    tkKW_IF:     Result := ParseIf();
    tkKW_WHILE:  Result := ParseWhile();
    tkKW_REPEAT: Result := ParseRepeat();
    tkKW_FOR:    Result := ParseFor();
    tkKW_CASE:   Result := ParseCase();
    tkKW_TRY:    Result := ParseTry();
    tkKW_PRINT:  Result := ParsePrint();
    tkKW_BREAK:  begin Result := XTree_Break.Create(FContext, DocPos); Next(); end;
    tkKW_CONTINUE:begin Result := XTree_Continue.Create(FContext, DocPos); Next(); end;
  else
    Result := ParseExpression();
  end;
end;

// ─── WRITELN / WRITE (XTree_Print) ─────────────────────────────────────────

function TPascalParser.ParsePrint(): XTree_Print;
var
  Args: XNodeArray;
  Doc: TDocPos;
  HasParens: Boolean;
begin
  Doc := DocPos;
  Consume(tkKW_PRINT);
  SetLength(Args, 0);

  HasParens := NextIf(tkLPARENTHESES);

  if HasParens then
  begin
    if Current.Token <> tkRPARENTHESES then
    begin
      repeat
        Args += ParseExpression();
      until not NextIf(tkCOMMA);
    end;
    Consume(tkRPARENTHESES);
  end
  else
  begin
    if not (Current.Token in [tkSEMI, tkKW_END, tkKW_ELSE, tkKW_UNTIL, tkUNKNOWN]) then
    begin
      repeat
        Args += ParseExpression();
      until not NextIf(tkCOMMA);
    end;
  end;

  Result := XTree_Print.Create(Args, FContext, Doc);
end;

function TPascalParser.ParseIf(): XTree_If;
var Condition, ThenBody: XTree_Node; ElseBody: XTree_ExprList; Doc: TDocPos;
begin
  Doc := DocPos; Consume(tkKW_IF);
  Condition := ParseExpression();
  Consume(tkKW_THEN);
  ThenBody := ParseStatement();
  ElseBody := nil;
  if NextIf(tkKW_ELSE) then ElseBody := XTree_ExprList.Create(ParseStatement(), FContext, DocPos);
  Result := XTree_If.Create([Condition], [ThenBody], ElseBody, FContext, Doc);
end;

function TPascalParser.ParseWhile(): XTree_While;
var Condition: XTree_Node; Body: XTree_ExprList; Doc: TDocPos;
begin
  Doc := DocPos; Consume(tkKW_WHILE); Condition := ParseExpression(); Consume(tkKW_DO);
  Body := XTree_ExprList.Create(ParseStatement(), FContext, DocPos);
  Result := XTree_While.Create(Condition, Body, FContext, Doc);
end;

function TPascalParser.ParseRepeat(): XTree_Repeat;
var Nodes: XNodeArray; Doc: TDocPos;
begin
  Doc := DocPos; Consume(tkKW_REPEAT); SetLength(Nodes, 0);
  while Current.Token <> tkKW_UNTIL do
  begin
    if Current.Token = tkSEMI then begin Next(); Continue; end;
    Nodes += ParseStatement();
    if Current.Token = tkSEMI then Next();
  end;
  Consume(tkKW_UNTIL);
  Result := XTree_Repeat.Create(ParseExpression(), XTree_ExprList.Create(Nodes, FContext, Doc), FContext, Doc);
end;

function TPascalParser.ParseFor(): XTree_For;
var
  IteratorIdent: XTree_Identifier; StartExpr, EndExpr: XTree_Node;
  IsDownTo: Boolean; Body: XTree_ExprList; InitNode, CondNode, IncNode: XTree_Node; Doc: TDocPos;
begin
  Doc := DocPos; Consume(tkKW_FOR);
  IteratorIdent := XTree_Identifier.Create(Current.Value, FContext, DocPos);
  Consume(tkIDENT); Consume(tkASGN);
  StartExpr := ParseExpression();

  if NextIf(tkKW_TO) then IsDownTo := False
  else if NextIf(tkKW_DOWNTO) then IsDownTo := True // Note: Ensure tkKW_DOWNTO exists!
  else FContext.RaiseException('Expected "to" or "downto"', DocPos);

  EndExpr := ParseExpression(); Consume(tkKW_DO);
  Body := XTree_ExprList.Create(ParseStatement(), FContext, DocPos);

  InitNode := XTree_Assign.Create(op_Asgn, IteratorIdent, StartExpr, FContext, Doc);
  if IsDownTo then
  begin
    CondNode := XTree_BinaryOp.Create(op_GTE, IteratorIdent, EndExpr, FContext, Doc);
    IncNode  := XTree_Assign.Create(op_Asgn, IteratorIdent,
                 XTree_BinaryOp.Create(op_SUB, IteratorIdent, XTree_Int.Create('1', FContext, Doc), FContext, Doc), FContext, Doc);
  end else
  begin
    CondNode := XTree_BinaryOp.Create(op_LTE, IteratorIdent, EndExpr, FContext, Doc);
    IncNode  := XTree_Assign.Create(op_Asgn, IteratorIdent,
                 XTree_BinaryOp.Create(op_ADD, IteratorIdent, XTree_Int.Create('1', FContext, Doc), FContext, Doc), FContext, Doc);
  end;
  Result := XTree_For.Create(InitNode, CondNode, IncNode, Body, FContext, Doc);
end;

function TPascalParser.ParseCase(): XTree_Case;
var
  Expr: XTree_Node; Branches: specialize TArrayList<TCaseBranch>; ElseBody: XTree_Node;
  CurrentBranch: TCaseBranch; Doc: TDocPos; ElseNodes: XNodeArray;
begin
  Doc := DocPos; Consume(tkKW_CASE); Expr := ParseExpression(); Consume(tkKW_OF);
  Branches.Init([]); ElseBody := nil;

  while not (Current.Token in [tkKW_END, tkKW_ELSE, tkUNKNOWN]) do
  begin
    CurrentBranch.Labels.Init([]);
    repeat CurrentBranch.Labels.Add(ParseExpression()); until not NextIf(tkCOMMA);
    Consume(tkCOLON);
    CurrentBranch.Body := ParseStatement();
    Branches.Add(CurrentBranch);
    if Current.Token = tkSEMI then Next();
  end;

  if NextIf(tkKW_ELSE) then
  begin
    SetLength(ElseNodes, 0);
    while not NextIf(tkKW_END) do
    begin
      if Current.Token = tkSEMI then begin Next(); Continue; end;
      ElseNodes += ParseStatement();
      if Current.Token = tkSEMI then Next();
    end;
    ElseBody := XTree_ExprList.Create(ElseNodes, FContext, DocPos);
  end else
    NextIf(tkKW_END);

  Result := XTree_Case.Create(Expr, Branches.RawOfManaged(), ElseBody, FContext, Doc);
end;

function TPascalParser.ParseTry(): XTree_Try;
var
  TryBody: XTree_ExprList; Handlers: specialize TArrayList<TExceptionHandler>; CurHandler: TExceptionHandler;
  FinallyBody: XTree_Node; Doc: TDocPos; TryNodes, FinNodes: XNodeArray;
begin
  Doc := DocPos; Consume(tkKW_TRY);
  SetLength(TryNodes, 0);
  while not (Current.Token in [tkKW_EXCEPT, tkKW_FINALLY, tkUNKNOWN]) do
  begin
    if Current.Token = tkSEMI then begin Next(); Continue; end;
    TryNodes += ParseStatement();
    if Current.Token = tkSEMI then Next();
  end;
  TryBody := XTree_ExprList.Create(TryNodes, FContext, DocPos);

  Handlers.Init([]); FinallyBody := nil;

  if NextIf(tkKW_EXCEPT) then
  begin
    while NextIf(tkKW_ON) do
    begin
      CurHandler.VarName := Current.Value; Consume(tkIDENT);
      Consume(tkCOLON);
      CurHandler.ExceptionType := FContext.GetType(Current.Value);
      Next(); Consume(tkKW_DO);
      CurHandler.Body := ParseStatement();
      Handlers.Add(CurHandler);
      if Current.Token = tkSEMI then Next();
    end;
  end;

  if NextIf(tkKW_FINALLY) then
  begin
    SetLength(FinNodes, 0);
    while not NextIf(tkKW_END) do
    begin
      if Current.Token = tkSEMI then begin Next(); Continue; end;
      FinNodes += ParseStatement();
      if Current.Token = tkSEMI then Next();
    end;
    FinallyBody := XTree_ExprList.Create(FinNodes, FContext, DocPos);
  end else
    NextIf(tkKW_END);

  Result := XTree_Try.Create(TryBody, Handlers.RawOfManaged(), FinallyBody, FContext, Doc);
end;

// ─── EXPRESSIONS ───────────────────────────────────────────────────────────

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
  Doc: TDocPos; op: TToken; Args: XNodeArray; RetExpr: XTree_Node;
begin
  Doc := DocPos;

  // DESUGAR Exit(expr) -> XTree_Return
  if (Current.Token = tkIDENT) and (XprCase(Current.Value) = 'exit') then
  begin
    Next(); RetExpr := nil;
    if NextIf(tkLPARENTHESES) then
    begin
      if Current.Token <> tkRPARENTHESES then RetExpr := ParseExpression();
      Consume(tkRPARENTHESES);
    end;
    Exit(XTree_Return.Create(RetExpr, FContext, Doc));
  end;

  case Current.Token of
    tkINTEGER: begin Result := XTree_Int.Create(Current.Value, FContext, Doc); Next(); end;
    tkFLOAT:   begin Result := XTree_Float.Create(Current.Value, FContext, Doc); Next(); end;
    tkSTRING:  begin Result := XTree_String.Create(Current.Value, FContext, Doc); Next(); end;
    tkCHAR:    begin Result := XTree_Char.Create(Current.Value, FContext, Doc); Next(); end;
    tkBOOL:    begin Result := XTree_Bool.Create(Current.Value, FContext, Doc); Next(); end;
    tkKW_NIL:  begin Result := XTree_Pointer.Create('nil', FContext, Doc); Next(); end;
    tkIDENT:   begin Result := XTree_Identifier.Create(Current.Value, FContext, Doc); Next(); end;

    tkLSQUARE: // Lape Array Initialization [1, 2, 3]
      begin
        Next(); SetLength(Args, 0);
        if Current.Token <> tkRSQUARE then
        repeat Args += ParseExpression(); until not NextIf(tkCOMMA);
        Consume(tkRSQUARE);
        Result := XTree_InitializerList.Create(Args, FContext, Doc);
      end;

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
      // Unaries are Lape prec 2 (Addr) or 3 (Not, Minus). Mapped to Express 8 or 7.
      // 8 is passed to force strong binding for everything after the unary operator.
      Result := XTree_UnaryOp.Create(AsOperator(op.Token), RHSExpr(ParsePrimary(), 8), FContext, Doc);
    end else
      FContext.RaiseExceptionFmt('Unexpected token in expression: `%s`', [Current.ToString], Doc);
  end;
end;

function TPascalParser.RHSExpr(Left: XTree_Node; leftPrecedence: Int8 = 0): XTree_Node;
var
  precedence, nextPrecedence: Int8; Right: XTree_Node; op: TToken; Doc: TDocPos;
begin
  while True do
  begin
    precedence := GetPascalPrecedence(Current.Token);
    if precedence < leftPrecedence then Exit(Left);

    op := Current; Doc := DocPos; Next();

    if AsOperator(op.Token) = op_DEREF then
    begin
      Left := XTree_UnaryOp.Create(op_DEREF, Left, FContext, Doc);
      Continue;
    end;

    if op.Token = tkLPARENTHESES then
    begin
      if Left is XTree_Field then
      begin
        Result := XTree_Invoke.Create(XTree_Field(Left).Right, ParseExpressionList(), FContext, Left.FDocPos);
        XTree_Invoke(Result).SelfExpr := XTree_Field(Left).Left;
      end else
        Result := XTree_Invoke.Create(Left, ParseExpressionList(), FContext, Left.FDocPos);
      Consume(tkRPARENTHESES); Left := Result; Continue;
    end;

    // ─── THE CRITICAL FIX FOR ARRAY INDEXING ─────────────────────────────────
    if op.Token = tkLSQUARE then
    begin
      repeat
        Right := ParseExpression(); // Parse from 0 precedence to allow ANY operator inside!
        Left := XTree_Index.Create(Left, Right, FContext, Doc);
      until not NextIf(tkCOMMA); // Multi-dim support! Arr[x, y] -> Arr[x][y]
      Consume(tkRSQUARE);
      Continue;
    end;

    Right := ParsePrimary();
    if Right = nil then FContext.RaiseException('Invalid expression', DocPos);

    if AsOperator(op.Token) <> op_Dot then
    begin
      nextPrecedence := GetPascalPrecedence(Current.Token);
      if precedence < nextPrecedence then
        Right := RHSExpr(Right, precedence + 1)
      else if precedence = nextPrecedence then
        Right := RHSExpr(Right, precedence + GetPascalAssoc(op.Token));
    end;

    if AsOperator(op.Token) = op_Dot then
      Left := XTree_Field.Create(Left, Right, FContext, Doc)
    else if AsOperator(op.Token) = op_Asgn then
      Left := XTree_Assign.Create(op_Asgn, Left, Right, FContext, Doc)
    else
      Left := XTree_BinaryOp.Create(AsOperator(op.Token), Left, Right, FContext, Doc);
  end;
  Result := Left;
end;

function TPascalParser.ParseExpression(): XTree_Node;
begin
  Result := ParsePrimary();
  if Result <> nil then Result := RHSExpr(Result);
end;

end.
