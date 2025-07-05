unit xprParser;
{
  Author: Jarl K. Holta  
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  utilities
}
{$I header.inc}
{.$hints off}

interface

uses
  SysUtils,
  xprTypes,
  xprTokenizer,
  xprTree,
  xprCompilerContext;

const
  ATOM       = [tkINTEGER, tkFLOAT, tkCHAR, tkSTRING, tkBOOL, tkKW_NIL];
  SEPARATORS = [tkNEWLINE, tkSEMI];

type
  EIncOrder = (PostInc=0, PreInc=1);

  TParser = class(TObject)
  private
    FPos: Int32;
    FTokenizer: TTokenizer;
    FContext: TCompilerContext;
    FLooping:Int32;
  
 {flags}
    FLineInsenstive: specialize TArrayList<Boolean>;
  public
    constructor Create(T:TTokenizer; ctx:TCompilerContext);
    function Parse(): XTree_Node;

    procedure RaiseException(msg:string);
    procedure RaiseExceptionFmt(msg:string; fmt:array of const);
    
    function DocPos: TDocPos; {$ifdef xinline}inline;{$endif}
    function Current: TToken; {$ifdef xinline}inline;{$endif}
    function Peek(n:Int32=1): TToken; {$ifdef xinline}inline;{$endif}
    procedure SkipTokens(tokens:TTokenKindSet); {$ifdef xinline}inline;{$endif}
    procedure SkipNewline; {$ifdef xinline}inline;{$endif}

    procedure SetInsesitive(Value:Boolean = True);
    procedure ResetInsesitive();
    function IsInsesitive(): Boolean;

    function Next(IncOrder:EIncOrder=PostInc; Increment:Int32=1): TToken; {$ifdef xinline}inline;{$endif}
    function NextIf(Token:ETokenKind; IncOrder:EIncOrder=PostInc; Increment:Int32=1): Boolean; {$ifdef xinline}inline;{$endif}
    function NextIf(Tokens:array of ETokenKind; IncOrder:EIncOrder=PostInc; Increment:Int32=1): Boolean; {$ifdef xinline}inline;{$endif}
    procedure Expect(Token:ETokenKind); {$ifdef xinline}inline;{$endif}
    procedure ExpectAny(Tokens: array of ETokenKind); {$ifdef xinline}inline;{$endif}
    function Consume(Token:ETokenKind; IncOrder:EIncOrder=PostInc; Increment:Int32=1): TToken;
    procedure ConsumeSeparator();

    function OperatorPrecedence(): Int8; {$ifdef xinline}inline;{$endif}
    function OperatorAssoc(): Int8; {$ifdef xinline}inline;{$endif}
    
    function IS_UNARY(): Boolean; {$ifdef xinline}inline;{$endif}

    function ParseAddType(Name:String=''; SkipFinalSeparators: Boolean=True): XType;
    function ParsePrint(): XTree_Print;
    function ParseIf(): XTree_If;
    function ParseWhile(): XTree_While;
    function ParseFor(): XTree_For;
    function ParseTry(): XTree_Try;

    function ParseFunction(): XTree_Function;
    function ParseVardecl: XTree_VarDecl;
    procedure ParseTypedecl();

    function ParseReturn(): XTree_Return;
    function ParseIdentRaw(): String;
    function ParseIdentListRaw(Insensitive:Boolean): TStringArray;
    function ParseIdent(): XTree_Identifier;
    function ParseIdentList(Insensitive:Boolean): XIdentNodeList;
    function ParseAtom(): XTree_Node;
    function ParsePrimary(): XTree_Node;
    function RHSExpr(Left:XTree_Node; leftPrecedence:Int8=0): XTree_Node;
    function ParseExpression(ExpectSeparator:Boolean=True): XTree_Node;
    function ParseExpressionList(Insensitive:Boolean; AllowEmpty:Boolean=False): XNodeArray;
    function ParseStatement: XTree_Node;
    function ParseStatements(EndKeywords:array of ETokenKind; Increase:Boolean=False): XNodeArray;
  end;

  function Parse(Tokenizer:TTokenizer; ctx:TCompilerContext = nil): XTree_Node;

implementation

uses
  xprUtils, xprErrors, xprLangdef, xprVartypes;

function Parse(Tokenizer:TTokenizer; ctx:TCompilerContext = nil): XTree_Node;
var
  Parser:TParser;
begin
  if ctx = nil then
    ctx := TCompilerContext.Create();
  Parser := TParser.Create(Tokenizer, ctx);
  Result := Parser.Parse();
  Parser.Free();
end;

constructor TParser.Create(T:TTokenizer; ctx:TCompilerContext);
begin
  FTokenizer := T;
  FContext := ctx;
  FPos := 0;
  FLooping := 0;
  FLineInsenstive.Init([False]);
end;

function TParser.Parse(): XTree_Node;
begin
  Result := XTree_ExprList.Create(ParseStatements([]), FContext, DocPos);
end;


//----------------------------------------------------------------------------\\

procedure TParser.RaiseException(msg:string);
begin
  xprErrors.RaiseException(eSyntaxError, msg, DocPos);
end;

procedure TParser.RaiseExceptionFmt(msg:string; fmt:array of const);
begin
  try
    xprErrors.RaiseExceptionFmt(eSyntaxError, msg, fmt, DocPos);
  except
    on e:SyntaxError do
      raise SyntaxError.Create(e.Message) at get_caller_addr(get_frame);
  end;
end;

{==============================================================================]
  The basics
[==============================================================================}
function TParser.DocPos: TDocPos;
begin
  if FPos = 0 then
    Result := FTokenizer.Tokens[0].DocPos
  else
    Result := FTokenizer.Tokens[FPos].DocPos;
end;

function TParser.Current: TToken;
begin
  Result := FTokenizer.Tokens[FPos];
end;

function TParser.Peek(n:Int32=1): TToken;
begin
  Result := FTokenizer.Tokens[FPos+n];
end;

procedure TParser.SkipTokens(tokens:TTokenKindSet);
begin
  while(Current.Token in Tokens) do Inc(FPos);
end;

procedure TParser.SkipNewline;
begin
  while(Current.Token = tkNEWLINE) do Inc(FPos);
end;

procedure TParser.SetInsesitive(Value:Boolean = True);
begin
  FLineInsenstive.Add(Value);
end;

procedure TParser.ResetInsesitive();
begin
  FLineInsenstive.Pop();
end;

function TParser.IsInsesitive(): Boolean;
begin
  Result := FLineInsenstive.Data[FLineInsenstive.High];
end;

function TParser.Next(IncOrder:EIncOrder=PostInc; Increment:Int32=1): TToken;
begin
  if IsInsesitive then SkipNewline;
  if IncOrder = PostInc then
  begin
    Result := FTokenizer.Tokens[FPos];
    Inc(FPos, Increment);
  end else
  begin
    Inc(FPos, Increment);
    Result := FTokenizer.Tokens[FPos];
  end;
end;

function TParser.NextIf(Token:ETokenKind; IncOrder:EIncOrder=PostInc; Increment:Int32=1): Boolean;
begin
  if IsInsesitive then SkipNewline;
  Result := Peek(Ord(IncOrder)).token = Token;
  if Result then Inc(FPos, Increment);
end;

function TParser.NextIf(Tokens:array of ETokenKind; IncOrder:EIncOrder=PostInc; Increment:Int32=1): Boolean;
var i:Int32;
begin
  if IsInsesitive then SkipNewline;
  Result := False;
  for i:=0 to High(Tokens) do
  begin
    Result := Peek(Ord(IncOrder)).token = Tokens[i];
    if Result then break;
  end;
  if Result then Inc(FPos, Increment);
end;

procedure TParser.Expect(Token:ETokenKind);
begin
  if Token <> Current.Token then
    RaiseExceptionFmt(eExpectedButFound, [TokenToString(Token), Current.ToString]);
end;

procedure TParser.ExpectAny(Tokens: array of ETokenKind);
begin
  if not(Current.Token in Tokens) then
    RaiseException(eUnexpected);
end;

function TParser.Consume(Token:ETokenKind; IncOrder:EIncOrder=PostInc; Increment:Int32=1): TToken;
begin
  if IsInsesitive then SkipNewline();
  Result := Next(IncOrder, Increment);
  if Token <> Result.Token then
    RaiseExceptionFmt(eExpectedButFound, [TokenToString(Token), Result.ToString]);
end;

procedure TParser.ConsumeSeparator();
begin
  if not(FTokenizer.Tokens[FPos].Token in SEPARATORS) then
    RaiseExceptionFmt(eExpectedButFound, ['semicolon or newline', TokenToString(FTokenizer.Tokens[FPos].Token)])
  else
    Next();
end;



// ----------------------------------------------------------------------------
// Operator information

function TParser.OperatorPrecedence(): Int8;
var
  def:TOperatorPrecedence = (prec:-1; assoc:0);
begin
  Result := PrecedenceMap.GetDef(Current.Token, def).prec;
end;

function TParser.OperatorAssoc(): Int8;
var
  def:TOperatorPrecedence = (prec:0; assoc:1);
begin
  Result := PrecedenceMap.GetDef(Current.Token, def).assoc;
end;

function TParser.IS_UNARY: Boolean;
var
  def:TOperatorPrecedence = (prec:-1; assoc:0);
begin
  Result := UnaryPrecedenceMap.GetDef(Current.Token, def).prec <> -1;
end;

// ----------------------------------------------------------------------------
// Here starts the actual parsing.. :)
//

//print <exprlist>
function TParser.ParsePrint(): XTree_Print;
var
  exprs: XNodeArray;
  _pos: TDocPos;
begin
  _pos := DocPos;
  Consume(tkKW_PRINT);
  exprs := ParseExpressionList(False, False);
  Result := XTree_Print.Create(exprs, FContext, _pos);
  SkipTokens(SEPARATORS);
end;

// ----------------------------------------------------------------------------
// type, or type declaration
// - Point
// - type TPoint = record x, y: Int32 end
function TParser.ParseAddType(Name: string=''; SkipFinalSeparators: Boolean=True): XType;
var
  i:Int32;
  Idents: XIdentNodeList;
  DType:  XType;
  Fields: XStringList;
  Types:  XTypeList;
begin
  Result := nil;
  SetInsesitive();
  SkipNewline();

  case Current.Token of
    tkIDENT:
      begin
        Result := FContext.GetType(current.Value);
        if Result = nil then
          RaiseExceptionFmt(eUndefinedIdentifier, [Current.Value]);
        Next();
      end;

    tkKW_RECORD:
      begin
        Next();
        SkipNewline;
        Fields.Init([]);
        Types.Init([]);
        while (Current.Token = tkIDENT) do
        begin
          Idents := ParseIdentList(True); //Ex: a,b,c: Int32;
          Consume(tkCOLON);
          DType := ParseAddType();
          SkipTokens(SEPARATORS);
          for i:=0 to Idents.High do
          begin
            Fields.Add(XTree_Identifier(Idents.Data[i]).Name);
            Types.Add(DType);
            Idents.Data[i].Free();
          end;
        end;
        if Fields.Size = 0 then
          RaiseExceptionFmt(eExpectedButFound, ['field variables', '`end of record`']);

        Consume(tkKW_END);
        Result := XType_Record.Create(Fields, Types);
      end;

    tkKW_ARRAY:
      begin
        Next();
        Consume(tkKW_OF);
        Result := XType_Array.Create(ParseAddType());
      end;

    else
      RaiseExceptionFmt(eExpectedButFound, ['type declaration', Current.Value]);
  end;

  if (Name <> '') then
    FContext.AddType(Name, Result);

  if SkipFinalSeparators then
    SkipTokens(SEPARATORS);

  ResetInsesitive();
end;

// ----------------------------------------------------------------------------
// IF statement
// > if condition then <stmts> [end | else <stmts> end]
// > if (condition) <stmt> [else <stmt>]
//
// TODO: Add the ELIF, because our language cant stack "else if"
function TParser.ParseIf(): XTree_If;
var
  Condition: XTree_Node;
  Conditions, Bodys: specialize TArrayList<XTree_Node>;

  ElseBody: XTree_ExprList;
  Token: TToken;
begin
  Consume(tkKW_IF);
  Consume(tkLPARENTHESES);
  Condition := ParseExpression(False);
  Consume(tkRPARENTHESES);
  Bodys.Init([]);
  Conditions.Init([]);

  ElseBody := nil;
  if NextIf(tkKW_THEN) then
  begin
    Bodys.Add(XTree_ExprList.Create(ParseStatements([tkKW_END, tkKW_ELIF, tkKW_ELSE], False), FContext, DocPos));
    Conditions.Add(Condition);

    while NextIf(tkKW_ELIF) do
    begin
      Consume(tkLPARENTHESES);
      Condition := ParseExpression(False);
      Consume(tkRPARENTHESES);
      Consume(tkKW_THEN);

      Bodys.Add(XTree_ExprList.Create(ParseStatements([tkKW_END, tkKW_ELIF, tkKW_ELSE], False), FContext, DocPos));
      Conditions.Add(Condition);
    end;

    if (Next() = tkKW_ELSE) then
      ElseBody := XTree_ExprList.Create(ParseStatements([tkKW_END], True), FContext, DocPos);
  end
  else
  begin
    Bodys.Add(XTree_ExprList.Create(self.ParseStatement(), FContext, DocPos));
    Conditions.Add(Condition);

    while NextIf(tkKW_ELIF) do
    begin
      Consume(tkLPARENTHESES);
      Condition := ParseExpression(False);
      Consume(tkRPARENTHESES);

      Bodys.Add(XTree_ExprList.Create(ParseStatement(), FContext, DocPos));
      Conditions.Add(Condition);
    end;

    if NextIf(tkKW_ELSE) then
      ElseBody := XTree_ExprList.Create(self.ParseStatement(), FContext, DocPos);
  end;

  Result := XTree_If.Create(XNodeArray(Conditions.Raw()), XNodeArray(Bodys.Raw()), ElseBody, FContext, DocPos);
end;

// ----------------------------------------------------------------------------
// WHILE loop
// > while (<boolexpr>) do <stmts> end
// > while (<boolexpr>) <stmt>
function TParser.ParseWhile(): XTree_While;
var
  Condition: XTree_Node;
  Body: XTree_ExprList;
begin
  Consume(tkKW_WHILE);
  Consume(tkLPARENTHESES, PostInc);
  Condition := ParseExpression(False);
  Consume(tkRPARENTHESES);

  Inc(FLooping);
  if NextIf(tkKW_DO) then
    Body := XTree_ExprList.Create(ParseStatements([tkKW_END], True), FContext, DocPos)
  else
    Body := XTree_ExprList.Create(self.ParseStatement(), FContext, DocPos);

  Result := XTree_While.Create(Condition, body, FContext, DocPos);
  Dec(FLooping);
end;


// ----------------------------------------------------------------------------
// FOR loop
// > for (entrystmt; boolexpr; loopstmt) do <stmts> end
// > for (entrystmt; boolexpr; loopstmt) <stmt>
function TParser.ParseFor(): XTree_For;
var
  EntryStmt, Condition, LoopStmt: XTree_Node;
  Body: XTree_ExprList;
begin
  Consume(tkKW_FOR);

  Consume(tkLPARENTHESES, PostInc);
  if current.Token = tkKW_VAR then
  begin
    EntryStmt := ParseVardecl();
    Consume(tkSEMI);
  end
  else
    EntryStmt := ParseExpression(True);

  Condition := ParseExpression(True);
  LoopStmt := ParseExpression(False);
  Consume(tkRPARENTHESES);

  Inc(FLooping);
  if NextIf(tkKW_DO) then
    Body := XTree_ExprList.Create(ParseStatements([tkKW_END], True), FContext, DocPos)
  else
    Body := XTree_ExprList.Create(self.ParseStatement(), FContext, DocPos);

  Result := XTree_For.Create(EntryStmt, Condition, LoopStmt, Body, FContext, DocPos);
  Dec(FLooping);
end;


// ----------------------------------------------------------------------------
// Try-Except
function TParser.ParseTry(): XTree_Try;
var
  TryBody, ExceptBody: XTree_ExprList;
begin
  Consume(tkKW_TRY);

  TryBody := XTree_ExprList.Create(ParseStatements([tkKW_EXCEPT], True), FContext, DocPos);
  ExceptBody := XTree_ExprList.Create(ParseStatements([tkKW_END], True), FContext, DocPos);

  Result := XTree_Try.Create(TryBody, ExceptBody, FContext, DocPos);
end;



// ----------------------------------------------------------------------------
// Parses a function declaration
//
function TParser.ParseFunction(): XTree_Function;
var
  Name, TypeName, Temp: string;
  Idents, Args: TStringArray;
  DType:  XType;
  ByRef: TPassArgsBy;
  Types: XTypeArray;
  Body: XTree_ExprList;
  Ret: XType;

  procedure ParseParams();
  var i,l: Int32; isRef: Boolean;
  begin
    isRef := NextIf(tkKW_REF);

    while (Current.Token = tkIDENT) do
    begin
      SetLength(Idents, 0);
      Idents := ParseIdentListRaw(True); // a,b,c
      Consume(tkCOLON);                  // :
      DType := ParseAddType();           // Type
      SkipTokens(SEPARATORS);
      for i:=0 to High(Idents) do
      begin
        l := Length(Types);
        SetLength(Types, l+1);
        SetLength(Args,  l+1);
        SetLength(ByRef, l+1);
        Args[l]  := Idents[i];

        if isRef then ByRef[l] := pbRef
                 else ByRef[l] := pbCopy;

        Types[l] := DType;
      end;

      isRef := NextIf(tkKW_REF);
    end;
  end;

begin
  SetLength(TypeName, 0);
  SetLength(Args, 0);
  SetLength(ByRef, 0);
  SetLength(Types, 0);

  Consume(tkKW_FUNC);
  Name := Consume(tkIDENT, PostInc).Value;
  if NextIf(tkDOT) then
  begin
    Temp := Consume(tkIDENT, PostInc).Value;
    TypeName := Name;
    Name := Temp;
  end;

  Consume(tkLPARENTHESES);
  SetInsesitive();
  if Current.Token <> tkRPARENTHESES then ParseParams();
  Consume(tkRPARENTHESES);
  ResetInsesitive();
  if NextIf(tkCOLON) then
  begin
    Ret := ParseAddType();
    SkipTokens(SEPARATORS);
  end else
    Ret := nil;

  Body := XTree_ExprList.Create(ParseStatements([tkKW_END], True), FContext, DocPos);
  Result := XTree_Function.Create(Name, Args, ByRef, Types, Ret, Body, FContext, DocPos);
  if TypeName <> '' then
    Result.TypeName := TypeName;
end;


// ----------------------------------------------------------------------------
// Parses var declaration
// - var <identlist>: <type> [= <expression>]
// - var <identlist> := <expression>
function TParser.ParseVardecl: XTree_VarDecl;
var
  Right: XTree_Node;
  Left: XIdentNodeList;
  Typ: XType;
begin
  Consume(tkKW_VAR);
  Left  := ParseIdentList(True);
  Right := nil;
  if NextIf(tkCOLON) then
  begin
    Typ := ParseAddType('', False);
    if NextIf(tkEQ) then
      Right := ParseExpression(False)
    else
      ConsumeSeparator();
  end
  else begin
    Typ := nil;
    Consume(tkASGN);
    Right := ParseExpression(False);
  end;
  Result := XTree_VarDecl.Create(Left, Right, Typ, FContext, Left.Data[0].FDocPos);
end;

// ----------------------------------------------------------------------------
// Parses a type declaration
// - type <identifier> = <type definition>
procedure TParser.ParseTypedecl();
var
  Name: string;
begin
  Consume(tkKW_TYPE);
  Name := Consume(tkIDENT, PostInc).Value;
  SetInsesitive();
  Consume(tkEQ, PostInc);
  ParseAddType(Name);
  ResetInsesitive();
end;


// ----------------------------------------------------------------------------
// Parses return
// - return expr
function TParser.ParseReturn(): XTree_Return;
begin
  Consume(tkKW_RETURN);
  Result := XTree_Return.Create(ParseExpression(False), FContext, DocPos);
end;


// ----------------------------------------------------------------------------
// Identifiers

function TParser.ParseIdentRaw(): String;
begin
  Result := Consume(tkIDENT).Value;
end;

function TParser.ParseIdentListRaw(Insensitive:Boolean): TStringArray;
begin
  SetLength(Result, 0);
  SetInsesitive(Insensitive);
  Result += ParseIdentRaw();
  while NextIf(tkCOMMA) do Result += ParseIdentRaw();
  ResetInsesitive();
end;

function TParser.ParseIdent(): XTree_Identifier;
var
  docstart: TDocPos;
begin
  docstart := DocPos;
  Result   := XTree_Identifier.Create(Self.ParseIdentRaw(), FContext, docstart);
end;

function TParser.ParseIdentList(Insensitive:Boolean): XIdentNodeList;
begin
  SetInsesitive(Insensitive);
  Result.Init([]);
  Result.Add(ParseIdent());
  while NextIf(tkCOMMA) do Result.Add(ParseIdent());
  ResetInsesitive();
end;



// ----------------------------------------------------------------------------
// Simple expressions

function TParser.ParseAtom(): XTree_Node;
begin
  Result := nil;
  case Current.token of
    tkBOOL:    Result := XTree_Bool.Create(Current.value, FContext, DocPos);
    tkINTEGER: Result := XTree_Int.Create(Current.value, FContext, DocPos);
    tkFLOAT:   Result := XTree_Float.Create(Current.value, FContext, DocPos);
    tkCHAR:    Result := XTree_Char.Create(Current.value, FContext, DocPos);
  //tkSTRING:  Result := XTree_String.Create(Current.value, FContext, DocPos);
    else
      RaiseException(eUnexpected);
  end;
  Next();
end;

function TParser.ParsePrimary(): XTree_Node;
var op:TToken;
begin
  if IsInsesitive() then SkipNewline;


  if (Current.Token in ATOM) then
    Result := ParseAtom()
  else if (Current.Token = tkIDENT) then
  begin
    Result := XTree_Identifier.Create(Current.value, FContext, DocPos);
    Next();
  end
  else if IS_UNARY then
  begin
    op := Next(PostInc);
    Result := XTree_UnaryOp.Create(AsOperator(op.Token), ParsePrimary(), FContext, DocPos)
  end
  else if Current.Token = tkLPARENTHESES then
  begin
    Next();
    SetInsesitive();
    Result := ParseExpression(False);
    Consume(tkRPARENTHESES, PostInc);
    ResetInsesitive();
  end
  else
    Result := nil;
end;


// ----------------------------------------------------------------------------
// Expressions

function TParser.RHSExpr(Left:XTree_Node; leftPrecedence:Int8=0): XTree_Node;
var
  precedence, nextPrecedence:Int8;
  Right: XTree_Node;
  op: TToken;

  function Merge(OP: EOperator; Left, Right: XTree_Node): XTree_Node;
  var exprList: XNodeArray; i: Int32;
  begin
    if OP = op_Index then
    begin
      Result := XTree_Index.Create(Left, RHSExpr(Right), FContext, DocPos);
      Consume(tkRSQUARE);
    end
    else if OP = op_Dot then
      Result := XTree_Field.Create(Left, Right, FContext, DocPos)
    else if OP in AssignOps then
      Result := XTree_Assign.Create(op, Left, Right, FContext, DocPos)
    else
      Result := XTree_BinaryOp.Create(op, Left, Right, FContext, DocPos);
  end;

begin
  while True do
  begin
    precedence := OperatorPrecedence();
    if precedence < leftPrecedence then
      Exit(Left);

    op := Next(PostInc);

    // handle the case of invoking
    if AsOperator(OP.Token) = op_Invoke then
    begin
      Result := XTree_Invoke.Create(Left, ParseExpressionList(True, True), FContext, DocPos);
      Consume(tkRPARENTHESES);
      Left := Result;
      Continue;
    end;

    Right := ParsePrimary();
    if Right = nil then
    begin
      RaiseException(eInvalidExpression);
    end;

    nextPrecedence := OperatorPrecedence();
    if precedence < nextPrecedence then
      Right := RHSExpr(Right, precedence + 1)
    else if precedence = nextPrecedence then
      Right := RHSExpr(Right, precedence + OperatorAssoc());
    Left := Merge(AsOperator(op.Token), Left, Right);
  end;

  Result := Left;
end;

function TParser.ParseExpression(ExpectSeparator:Boolean=True): XTree_Node;
begin
  Result := ParsePrimary();
  if (Result <> nil) then
    Result := RHSExpr(Result);

  SetInsesitive();
  if NextIf(tkNEWLINE) then Exit;
  ResetInsesitive();

  if (ExpectSeparator) then ConsumeSeparator;///XXX Consume(tkSEMI, PostInc);
end;

function TParser.ParseExpressionList(Insensitive:Boolean; AllowEmpty:Boolean=False): XNodeArray;
var
  expr: XTree_Node;
  top: Int32;
begin
  Result := nil;
  SetInsesitive(Insensitive);
  top := 0;
  while True do
  begin
    expr := ParseExpression(False);
    if (expr = nil) then
    begin
      if (not AllowEmpty) then RaiseException(eInvalidExpression);
      break;
    end;
    SetLength(Result, top + 1);
    Result[top] := expr;
    Inc(top);
    if not NextIf(tkCOMMA) then
      break;
  end;
  ResetInsesitive();
end;

// ----------------------------------------------------------------------------
// Parses a single statement
function TParser.ParseStatement: XTree_Node;
begin
  Result := nil;
  SkipNewline;

  if (Current.token = tkKW_TYPE) then
    ParseTypedecl()
  else if (Current.token = tkKW_VAR) then
    Result := ParseVardecl()
  else if (Current.token = tkKW_FUNC) then
    Result := ParseFunction()
  else if (Current.token = tkKW_PRINT) then
    Result := ParsePrint()
  else if (Current.token = tkKW_IF) then
    Result := ParseIf()
  else if (Current.token = tkKW_WHILE) then
    Result := ParseWhile()
  else if (Current.token = tkKW_FOR) then
    Result := ParseFor()
  else if (Current.token = tkKW_RETURN) then
    Result := ParseReturn()
  else if (Current.token = tkKW_TRY) then
      Result := ParseTry()
  else
    Result := ParseExpression(False);

  SkipTokens(SEPARATORS);
end;

// Parses multiple statement
function TParser.ParseStatements(EndKeywords:array of ETokenKind; Increase:Boolean=False): XNodeArray;
var
  prim:XTree_Node;
begin
  SEtLength(Result, 0);
  while (Current.Token <> tkUNKNOWN) and (not(Current.Token in EndKeywords)) do
  begin
    prim := self.ParseStatement();
    SkipNewline;
    if prim <> nil then
    begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := prim;
    end;
  end;

  if Length(EndKeywords) <> 0 then
    ExpectAny(EndKeywords);

  if Increase then
    Next;
end;


end.
