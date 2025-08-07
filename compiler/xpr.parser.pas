unit xpr.Parser;
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
  xpr.Types,
  xpr.Tokenizer,
  xpr.Tree,
  xpr.CompilerContext;

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

    function ParseImport(): XTree_ImportUnit;
    function ParseAddType(Name:String=''; SkipFinalSeparators: Boolean=True): XType;
    function ParsePrint(): XTree_Print;
    function ParseIf(): XTree_If;
    function ParseWhile(): XTree_While;
    function ParseRepeat(): XTree_Repeat;
    function ParseFor(): XTree_For;
    function ParseTry(): XTree_Try;

    function ParseContinue(): XTree_Continue;
    function ParseBreak(): XTree_Break;

    function ParseFunction(): XTree_Function;
    function ParseVardecl: XTree_VarDecl;
    procedure ParseTypedecl();

    function ParseIfExpr(): XTree_IfExpr;
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
  xpr.Utils, xpr.Errors, xpr.Langdef, xpr.Vartypes;

function CompoundToBinaryOp(CompOp: EOperator): EOperator;
begin
  case CompOp of
    op_AsgnADD: Result := op_Add;
    op_AsgnSUB: Result := op_Sub;
    op_AsgnMUL: Result := op_Mul;
    op_AsgnDIV: Result := op_Div;
    op_AsgnMOD: Result := op_Mod;
    op_AsgnBND: Result := op_BND;
    op_AsgnBOR: Result := op_BOR;
    op_AsgnXOR: Result := op_XOR;
    op_AsgnSHL: Result := op_SHL;
    op_AsgnSHR: Result := op_SHR;
  else
    Result := op_Unknown; // Should not happen if called correctly
  end;
end;


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
  FContext.DelayedNodes := [];
  Result := XTree_ExprList.Create(ParseStatements([]), FContext, DocPos);
end;


//----------------------------------------------------------------------------\\

procedure TParser.RaiseException(msg:string);
begin
  xpr.Errors.RaiseException(eSyntaxError, msg, DocPos);
end;

procedure TParser.RaiseExceptionFmt(msg:string; fmt:array of const);
begin
  try
    xpr.Errors.RaiseExceptionFmt(eSyntaxError, msg, fmt, DocPos);
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

// Parses an import statement
// > import 'path/to/unit.xpr' as MyUnit
// Parses an import statement. Now returns a base XTree_Node because it can
// create a list of imports if the syntax is `from unit import symbol1, symbol2`.
// > import 'path/to/unit.expr'
// > import 'path/to/unit.expr' as MyUnit
// > import 'path/to/unit.expr' as *
function TParser.ParseImport(): XTree_ImportUnit;
var
  UnitPath, UnitAlias: string;
begin
  Consume(tkKW_IMPORT);

  // The unit path must be a string literal.
  if Current.Token <> tkSTRING then
    RaiseExceptionFmt(eExpectedButFound, ['a string literal for the unit path', Current.ToString]);

  UnitPath := Current.Value;
  Next();

  // Check for the optional 'as' clause.
  if NextIf(tkKW_AS) then
  begin
    if Current.Token = tkMUL then
    begin
      UnitAlias := '';
      Next();
    end else
      UnitAlias := Consume(tkIDENT).Value;
  end else
  begin // Case 3: No 'as' clause. Auto-generate the alias from the filename.
    UnitAlias := ChangeFileExt(ExtractFileName(UnitPath), '');
    if UnitAlias = '' then
      RaiseException('Could not determine an alias for the imported unit. Please provide one using "as".');
  end;

  Result := XTree_ImportUnit.Create(UnitPath, UnitAlias, FContext, DocPos);
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
        Result := XType_Array.Create(ParseAddType('',False));
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
// REPEAT-UNTIL loop
// > repeat <stmts> until (condition)
function TParser.ParseRepeat(): XTree_Repeat;
var
  Condition: XTree_Node;
  Body: XTree_ExprList;
begin
  Consume(tkKW_REPEAT);

  Inc(FLooping);
  try
    // The body is a series of statements terminated by the 'until' keyword.
    Body := XTree_ExprList.Create(ParseStatements([tkKW_UNTIL], True), FContext, DocPos);

    Consume(tkLPARENTHESES);
    Condition := ParseExpression(False);
    Consume(tkRPARENTHESES);

    Result := XTree_Repeat.Create(Condition, Body, FContext, DocPos);
  finally
    Dec(FLooping);
  end;
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
// Parses break
// > break
function TParser.ParseBreak(): XTree_Break;
begin
  Consume(tkKW_BREAK);
  if FLooping <= 0 then
    RaiseException('`break` is not allowed outside of a loop');
  Result := XTree_Break.Create(FContext, DocPos);
end;

// ----------------------------------------------------------------------------
// Parses continue
// > continue
function TParser.ParseContinue(): XTree_Continue;
begin
  Consume(tkKW_CONTINUE);
  if FLooping <= 0 then
    RaiseException('`continue` is not allowed outside of a loop');
  Result := XTree_Continue.Create(FContext, DocPos);
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
// Parses if-exressions (Ternary operator)
// - if(cond) <that> else <this>
function TParser.ParseIfExpr(): XTree_IfExpr;
var
  _pos: TDocPos;
  Cond, ThenNode, ElseNode: XTree_Node;
begin
  _pos := DocPos;
  Consume(tkKW_IF);
  Consume(tkLPARENTHESES);
  Cond := ParseExpression(False);
  Consume(tkRPARENTHESES);

  // Unlike an 'if' statement, the 'then' is implied and not a keyword here.
  // Or, you could require a 'then' keyword if you prefer that syntax.
  // Let's assume the syntax is `if (cond) expr else expr`

  ThenNode := ParseExpression(False);

  Consume(tkKW_ELSE);

  ElseNode := ParseExpression(False);

  Result := XTree_IfExpr.Create(Cond, ThenNode, ElseNode, FContext, _pos);
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
    tkKW_NIL:  Result := XTree_Pointer.Create(Current.value, FContext, DocPos);
    tkINTEGER: Result := XTree_Int.Create(Current.value, FContext, DocPos);
    tkFLOAT:   Result := XTree_Float.Create(Current.value, FContext, DocPos);
    tkCHAR:    Result := XTree_Char.Create(Current.value, FContext, DocPos);
    tkSTRING:  Result := XTree_String.Create(Current.value, FContext, DocPos);
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
  else if (Current.Token = tkKW_IF) then
  begin
    Result := ParseIfExpr();
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
  var exprList: XNodeArray; i: Int32; binOp: EOperator;
  begin
    if OP = op_Index then
    begin
      Result := XTree_Index.Create(Left, RHSExpr(Right), FContext, DocPos);
      Consume(tkRSQUARE);
    end
    else if OP = op_Dot then
      Result := XTree_Field.Create(Left, Right, FContext, DocPos)
    else if OP in CompoundOps then
    begin
      binOp := CompoundToBinaryOp(OP);
      if binOp = op_Unknown then
        RaiseExceptionFmt('Unsupported compound assignment operator: %s', [OperatorToStr(OP)]);

      Result := XTree_Assign.Create(op_Asgn, Left, XTree_BinaryOp.Create(binOp, Left, Right, FContext, DocPos), FContext, DocPos);
    end
    else if OP = op_Asgn then
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

  case Current.token of
    tkKW_IMPORT:   Result := ParseImport();
    tkKW_TYPE:     ParseTypedecl();
    tkKW_VAR:      Result := ParseVardecl();
    tkKW_FUNC:     Result := ParseFunction();
    tkKW_PRINT:    Result := ParsePrint();
    tkKW_IF:       Result := ParseIf();
    tkKW_WHILE:    Result := ParseWhile();
    tkKW_REPEAT:   Result := ParseRepeat();
    tkKW_FOR:      Result := ParseFor();
    tkKW_RETURN:   Result := ParseReturn();
    tkKW_TRY:      Result := ParseTry();
    tkKW_BREAK:    Result := ParseBreak();
    tkKW_CONTINUE: Result := ParseContinue();
  else
    Result := ParseExpression(False);
  end;

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
