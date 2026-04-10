unit xpr.Parser;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Parser v2 - Indentation-sensitive block delimiters.

  Design rules:
    - Blocks delimited by indentation, not 'end' keywords
    - Body must be strictly indented past the opening keyword's column
    - Continuation keywords (elif, else, except, until, case) sit at the
      SAME column as their opener - ParseBlock stops there and the opener
      handles them
    - 'type' alone on a line opens an indented group of type declarations;
      'type TAlias = ...' on one line is the old single-declaration form
    - record/class: inline fields if tokens follow on same line, block otherwise
    - Single-line body: content on same line as block-opener (then/do) → single stmt
    - func shorthand: 'func name(): T => expr' stays on one line, no block
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
    FPos:       Int32;
    FTokenizer: TTokenizer;
    FContext:   TCompilerContext;
    FLooping:   Int32;
    FInFunction:Int32;

    FLineInsenstive: specialize TArrayList<Boolean>;

    // -- Indentation helpers --------------------------------------------------
    // Column of the first non-newline token at or after Pos.
    function TokenIndentAt(Pos: Int32): Int32;
    // Column of the first non-newline token from current position.
    function CurrentIndent(): Int32;
    // True when the current token is on the same source line as RefLine.
    function OnSameLine(RefLine: Int32): Boolean;
    // True when we are sitting on a newline or EOF (end-of-line).
    function AtEndOfLine(): Boolean;

    function LineStartIndent(Line: Int32): Int32;
  public
    constructor Create(T: TTokenizer; ctx: TCompilerContext);
    function Parse(): XTree_Node;

    procedure RaiseException(msg: string);
    procedure RaiseExceptionFmt(msg: string; fmt: array of const);

    function DocPos: TDocPos;     {$ifdef xinline}inline;{$endif}
    function Current: TToken;     {$ifdef xinline}inline;{$endif}
    function Peek(n: Int32=1): TToken; {$ifdef xinline}inline;{$endif}
    procedure SkipTokens(tokens: TTokenKindSet); {$ifdef xinline}inline;{$endif}
    procedure SkipNewline;        {$ifdef xinline}inline;{$endif}

    procedure SetInsesitive(Value: Boolean = True);
    procedure ResetInsesitive();
    function  IsInsesitive(): Boolean;

    function  Next(IncOrder: EIncOrder=PostInc; Increment: Int32=1): TToken; {$ifdef xinline}inline;{$endif}
    function  NextIf(Token: ETokenKind; IncOrder: EIncOrder=PostInc; Increment: Int32=1): Boolean; {$ifdef xinline}inline;{$endif}
    function  NextIf(Tokens: array of ETokenKind; IncOrder: EIncOrder=PostInc; Increment: Int32=1): Boolean; {$ifdef xinline}inline;{$endif}
    procedure Expect(Token: ETokenKind);    {$ifdef xinline}inline;{$endif}
    procedure ExpectAny(Tokens: array of ETokenKind); {$ifdef xinline}inline;{$endif}
    function  Consume(Token: ETokenKind; IncOrder: EIncOrder=PostInc; Increment: Int32=1): TToken;
    procedure ConsumeSeparator();

    function OperatorPrecedence(): Int8; {$ifdef xinline}inline;{$endif}
    function OperatorAssoc(): Int8;      {$ifdef xinline}inline;{$endif}
    function IS_UNARY(): Boolean;        {$ifdef xinline}inline;{$endif}

    // -- Core indentation-based block parser ----------------------------------
    // Parses statements while indent > ParentIndent.
    // Does NOT consume the token that caused the stop.
    function ParseBlock(ParentIndent: Int32): XNodeArray;
    function ParseBlockAsExprList(ParentIndent: Int32): XTree_ExprList;

    // -- Statement parsers ----------------------------------------------------
    function ParseNSIdent(DoInc: Boolean = False): TToken;

    // Parse explicit type parameters: <T> or <T, U, V>
    // Returns the names as a string array. Leaves current token after '>'.
    function ParseTypeParams(): TStringArray;
    procedure ParseTypeParamsFull(out Params: TStringArray; out Constraints: TStringArray);

    // Parse explicit CONCRETE type params at a callsite: <Int, Double>
    // Resolves each name to an XType immediately.
    function ParseTypeParams_Concrete(): XTypeArray;

    // Returns True if we are looking at `< IDENT [, IDENT]* > (`
    // which means a generic instantiation rather than a comparison.
    function IsGenericCallAhead(): Boolean;
    function ParseImport(): XTree_ImportUnit;
    function ParseAddType(Name: string=''; SkipFinalSeparators: Boolean=True;
                          AllowUntyped: Boolean=False;
                          BlockParentIndent: Int32=-1): XType;
    function ParsePrint(): XTree_Print;
    function ParseIf(): XTree_If;
    function ParseSwitch(): XTree_Case;
    function ParseWhile(): XTree_While;
    function ParsePass(): XTree_Pass;
    function ParseRepeat(): XTree_Repeat;
    function ParseFor(): XTree_For;
    function ParseForIn(): XTree_Node;
    function ParseTry(): XTree_Try;
    function ParseContinue(): XTree_Continue;
    function ParseBreak(): XTree_Break;
    function ParseFunction(IsExpression: Boolean = False): XTree_Node;
    function ParseRefdecl(): XTree_Node;
    function ParseVardecl(): XTree_Node;
    function ParseClassDecl(ClassDeclName: string; ParentIndent: Int32;
                             ATypeParams: TStringArray = nil;
                             ATypeConstraints: TStringArray = nil): XTree_ClassDecl;
    function ParseTypeDecl(): XTree_Node;
    function ParseRaise(): XTree_Raise;
    function ParseIfExpr(): XTree_IfExpr;
    function ParseInitializerList(): XTree_InitializerList;
    function ParseDestructureList(): XTree_Destructure;
    function ParseListComp(): XTree_Node;
    function ParseReturn(): XTree_Return;
    function ParseIdentRaw(): string;
    function ParseIdentListRaw(Insensitive: Boolean): TStringArray;
    function ParseIdent(): XTree_Identifier;
    function ParseIdentList(Insensitive: Boolean): XIdentNodeList;
    function ParseAtom(): XTree_Node;
    function ParseInterpolatedString(const S: string): XTree_Node;
    function ParsePrimary(): XTree_Node;
    function RHSExpr(Left: XTree_Node; leftPrecedence: Int8=0): XTree_Node;
    function ParseExpression(ExpectSeparator: Boolean=True; PostParse: Boolean=True): XTree_Node;
    function ParseExpressionList(Insensitive: Boolean; AllowEmpty: Boolean=False): XNodeArray;
    function ParseAnnotation(): XTree_ExprList;
    function ParseStatement(): XTree_Node;
  end;

  function Parse(Tokenizer: TTokenizer; ctx: TCompilerContext = nil): XTree_Node;
  function Parse(AName: string; ctx: TCompilerContext; AScript: string): XTree_Node;

implementation

uses
  xpr.Utils, xpr.Errors, xpr.Langdef, xpr.Vartypes, ffi, xpr.ffi;

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
    Result := op_Unknown;
  end;
end;

function Parse(Tokenizer: TTokenizer; ctx: TCompilerContext = nil): XTree_Node;
var
  Parser: TParser;
begin
  if ctx = nil then
    ctx := TCompilerContext.Create(Tokenizer.Data)
  else if ctx.MainFileContents = '' then
    ctx.MainFileContents := Tokenizer.Data;

  Parser := TParser.Create(Tokenizer, ctx);
  Result := Parser.Parse();
  Parser.Free();
end;

function Parse(AName: string; ctx: TCompilerContext; AScript: string): XTree_Node;
var
  Parser:    TParser;
  Tokenizer: TTokenizer;
begin
  Tokenizer := Tokenize(AName, AScript);

  if ctx = nil then
    ctx := TCompilerContext.Create(Tokenizer.Data)
  else if ctx.MainFileContents = '' then
    ctx.MainFileContents := Tokenizer.Data;

  Parser := TParser.Create(Tokenizer, ctx);
  Result := Parser.Parse();
  Parser.Free();
end;

constructor TParser.Create(T: TTokenizer; ctx: TCompilerContext);
begin
  FTokenizer   := T;
  FContext     := ctx;
  FPos         := 0;
  FLooping     := 0;
  FInFunction  := 0;
  FLineInsenstive.Init([False]);
end;

function TParser.Parse(): XTree_Node;
begin
  Result := XTree_ExprList.Create(ParseBlock(-1), FContext, DocPos);
end;

// -- Basics -------------------------------------------------------------------

procedure TParser.RaiseException(msg: string);
begin
  FContext.RaiseException(eSyntaxError, msg, DocPos);
end;

procedure TParser.RaiseExceptionFmt(msg: string; fmt: array of const);
begin
  try
    FContext.RaiseExceptionFmt(eSyntaxError, msg, fmt, DocPos);
  except
    on e: SyntaxError do
      raise SyntaxError.Create(e.Message) at get_caller_addr(get_frame);
  end;
end;

function TParser.DocPos: TDocPos;
begin
  if FPos = 0 then Result := FTokenizer.Tokens[0].DocPos
  else             Result := FTokenizer.Tokens[FPos].DocPos;
end;

function TParser.Current: TToken;
begin
  Result := FTokenizer.Tokens[FPos];
end;

function TParser.Peek(n: Int32=1): TToken;
begin
  Result := FTokenizer.Tokens[FPos + n];
end;

procedure TParser.SkipTokens(tokens: TTokenKindSet);
begin
  while (Current.Token in Tokens) do Inc(FPos);
end;

procedure TParser.SkipNewline;
begin
  while (Current.Token = tkNEWLINE) do Inc(FPos);
end;

procedure TParser.SetInsesitive(Value: Boolean = True);
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

function TParser.Next(IncOrder: EIncOrder=PostInc; Increment: Int32=1): TToken;
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

function TParser.NextIf(Token: ETokenKind; IncOrder: EIncOrder=PostInc; Increment: Int32=1): Boolean;
begin
  if IsInsesitive then SkipNewline;
  Result := Peek(Ord(IncOrder)).Token = Token;
  if Result then Inc(FPos, Increment);
end;

function TParser.NextIf(Tokens: array of ETokenKind; IncOrder: EIncOrder=PostInc; Increment: Int32=1): Boolean;
var i: Int32;
begin
  if IsInsesitive then SkipNewline;
  Result := False;
  for i := 0 to High(Tokens) do
  begin
    Result := Peek(Ord(IncOrder)).Token = Tokens[i];
    if Result then break;
  end;
  if Result then Inc(FPos, Increment);
end;

procedure TParser.Expect(Token: ETokenKind);
begin
  if Token <> Current.Token then
    RaiseExceptionFmt(eExpectedButFound, [TokenToString(Token), Current.ToString]);
end;

procedure TParser.ExpectAny(Tokens: array of ETokenKind);
begin
  if not (Current.Token in Tokens) then
    RaiseException(eUnexpected);
end;

function TParser.Consume(Token: ETokenKind; IncOrder: EIncOrder=PostInc; Increment: Int32=1): TToken;
begin
  if IsInsesitive then SkipNewline();
  Result := Next(IncOrder, Increment);
  if Token <> Result.Token then
    RaiseExceptionFmt(eExpectedButFound, [TokenToString(Token), Result.ToString]);
end;

procedure TParser.ConsumeSeparator();
begin
  if not (FTokenizer.Tokens[FPos].Token in SEPARATORS) then
    RaiseExceptionFmt(eExpectedButFound,
      ['semicolon or newline', TokenToString(FTokenizer.Tokens[FPos].Token)])
  else
    Next();
end;

// -- Operator helpers ---------------------------------------------------------

function TParser.OperatorPrecedence(): Int8;
var def: TOperatorPrecedence = (prec: -1; assoc: 0);
begin
  Result := PrecedenceMap.GetDef(Current.Token, def).prec;
end;

function TParser.OperatorAssoc(): Int8;
var def: TOperatorPrecedence = (prec: 0; assoc: 1);
begin
  Result := PrecedenceMap.GetDef(Current.Token, def).assoc;
end;

function TParser.IS_UNARY: Boolean;
var def: TOperatorPrecedence = (prec: -1; assoc: 0);
begin
  Result := UnaryPrecedenceMap.GetDef(Current.Token, def).prec <> -1;
end;

// -- Indentation helpers -------------------------------------------------------

function TParser.TokenIndentAt(Pos: Int32): Int32;
var p: Int32;
begin
  p := Pos;
  while (p < Length(FTokenizer.Tokens)) and
        (FTokenizer.Tokens[p].Token = tkNEWLINE) do
    Inc(p);
  if p < Length(FTokenizer.Tokens) then
    Result := FTokenizer.Tokens[p].DocPos.Column
  else
    Result := -1;
end;

function TParser.CurrentIndent(): Int32;
begin
  Result := TokenIndentAt(FPos);
end;

function TParser.OnSameLine(RefLine: Int32): Boolean;
begin
  // Current token is on the same line and not a newline or EOF.
  Result := (Current.Token <> tkNEWLINE) and
            (Current.Token <> tkUNKNOWN) and
            (DocPos.Line = RefLine);
end;

function TParser.AtEndOfLine(): Boolean;
begin
  Result := (Current.Token = tkNEWLINE) or (Current.Token = tkUNKNOWN);
end;

function TParser.LineStartIndent(Line: Int32): Int32;
var p: Int32;
begin
  p := 0;
  while p < Length(FTokenizer.Tokens) do
  begin
    if (FTokenizer.Tokens[p].Token <> tkNEWLINE) and
       (FTokenizer.Tokens[p].DocPos.Line = Line) then
    begin
      Result := FTokenizer.Tokens[p].DocPos.Column;
      Exit;
    end;
    Inc(p);
  end;
  Result := 0;
end;

// -- Core block parser ---------------------------------------------------------
//
//  ParseBlock(ParentIndent):
//    Skips blank lines, determines block indent from first real token.
//    Parses statements while indent > ParentIndent.
//    Stops (without consuming) when:
//      - indent <= ParentIndent  (dedent - caller's continuation keyword or EOF)
//      - tkUNKNOWN (EOF)
//
function TParser.ParseBlock(ParentIndent: Int32): XNodeArray;
var
  blockIndent:  Int32;
  stmt:         XTree_Node;
  initialPos:   Int32;
begin
  SetLength(Result, 0);

  SkipTokens(SEPARATORS);
  if Current.Token = tkUNKNOWN then Exit;

  blockIndent := CurrentIndent();

  // An empty block is valid (e.g. a func with only a comment).
  if blockIndent <= ParentIndent then Exit;

  while True do
  begin
    SkipTokens(SEPARATORS);
    if Current.Token = tkUNKNOWN then break;

    // Dedented back to or past parent - stop, let caller handle it.
    if CurrentIndent() <= ParentIndent then break;

    // Indented inconsistently (past parent but below block level) → error.
    if CurrentIndent() < blockIndent then
      RaiseException('Inconsistent indentation: unexpected dedent inside block');

    initialPos := FPos;
    stmt       := ParseStatement();
    SkipTokens(SEPARATORS);

    if stmt <> nil then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := stmt;
    end;

    if (stmt = nil) and (FPos = initialPos) then
      RaiseException(eUnexpected + ', found: ' + Current.ToString);
  end;
end;

function TParser.ParseBlockAsExprList(ParentIndent: Int32): XTree_ExprList;
begin
  Result := XTree_ExprList.Create(ParseBlock(ParentIndent), FContext, DocPos);
end;

// -- ParseNSIdent -------------------------------------------------------------

function TParser.ParseNSIdent(DoInc: Boolean = False): TToken;
begin
  Self.SetInsesitive(False);
  Result := Consume(tkIDENT);
  while NextIf(tkCOLONCOLON) do
    Result.Value += '::' + Consume(tkIDENT).Value;
  if not DoInc then Dec(FPos);
  Self.ResetInsesitive();
end;

// -- Generic helpers -----------------------------------------------------------

// Lookahead: are we looking at `< IDENT [:IDENT] [, IDENT [:IDENT]]* > (` ?
// That pattern unambiguously means a generic instantiation, not a comparison.
// Saves and restores FPos unconditionally.
function TParser.IsGenericCallAhead(): Boolean;
var
  saved: Int32;
begin
  Result := False;
  if Current.Token <> tkLT then Exit;
  saved := FPos;
  try
    Next(); // skip <
    if Current.Token <> tkIDENT then Exit;
    Next(); // first type param name
    if Current.Token = tkCOLON then begin Next(); Next(); end; // skip :constraint
    while Current.Token = tkCOMMA do
    begin
      Next(); // skip ,
      if Current.Token <> tkIDENT then Exit;
      Next(); // next type param name
      if Current.Token = tkCOLON then begin Next(); Next(); end;
    end;
    if Current.Token <> tkGT then Exit;
    Next(); // skip >
    Result := Current.Token = tkLPARENTHESES;
  finally
    FPos := saved;
  end;
end;

// Consume `< T[:constraint] [, U[:constraint]]* >`.
// Fills Params with type-param names, Constraints with constraint name ('' = none).
// Returns Params for convenience.
function TParser.ParseTypeParams(): TStringArray;
begin
  Result := [];
  Consume(tkLT);
  repeat
    Result += Consume(tkIDENT, PostInc).Value;
    // optional :constraint
    if NextIf(tkCOLON) then
      Consume(tkIDENT, PostInc); // constraint name consumed but stored via ParseTypeParamsFull
  until not NextIf(tkCOMMA);
  Consume(tkGT);
end;

// Full version that also returns the parallel constraint array.
procedure TParser.ParseTypeParamsFull(out Params: TStringArray; out Constraints: TStringArray);
begin
  Params      := [];
  Constraints := [];
  Consume(tkLT);
  repeat
    Params += Consume(tkIDENT, PostInc).Value;
    if NextIf(tkCOLON) then
    begin
      // allow array constraint
      if Current.Token = tkKW_ARRAY then
         Constraints += Consume(tkKW_ARRAY, PostInc).Value
      else
        // other cases
        Constraints += Consume(tkIDENT, PostInc).Value;
    end
    else
      Constraints += '';
  until not NextIf(tkCOMMA);
  Consume(tkGT);
end;

// Consume `< TypeName [, TypeName]* >` and resolve each name to an XType.
// Used at callsites: Compare<Int>(x, y)
function TParser.ParseTypeParams_Concrete(): XTypeArray;
var
  name: string;
  typ:  XType;
begin
  Result := [];
  Consume(tkLT);
  repeat
    name := ParseNSIdent(True).Value;
    typ  := FContext.GetType(name);
    if typ = nil then
      RaiseExceptionFmt('Unknown type `%s` in generic instantiation', [name]);
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := typ;
  until not NextIf(tkCOMMA);
  Consume(tkGT);
end;

// -- print ---------------------------------------------------------------------

function TParser.ParsePrint(): XTree_Print;
var
  exprs: XNodeArray;
  _pos:  TDocPos;
begin
  _pos  := DocPos;
  Consume(tkKW_PRINT);
  exprs := ParseExpressionList(False, False);
  Result := XTree_Print.Create(exprs, FContext, _pos);
  SkipTokens(SEPARATORS);
end;

// -- import --------------------------------------------------------------------

function TParser.ParseImport(): XTree_ImportUnit;
var
  UnitPath, UnitAlias: string;
begin
  Consume(tkKW_IMPORT);

  if Current.Token <> tkSTRING then
    RaiseExceptionFmt(eExpectedButFound,
      ['a string literal for the unit path', Current.ToString]);

  UnitPath := Current.Value;
  Next();

  if NextIf(tkKW_AS) then
  begin
    if Current.Token = tkMUL then begin UnitAlias := ''; Next(); end
    else UnitAlias := Consume(tkIDENT).Value;
  end else
  begin
    UnitAlias := ChangeFileExt(ExtractFileName(UnitPath), '');
    if UnitAlias = '' then
      RaiseException(
        'Could not determine an alias for the imported unit. Please provide one using "as".');
  end;

  Result := XTree_ImportUnit.Create(UnitPath, UnitAlias, FContext, DocPos);
end;

// -- ParseAddType --------------------------------------------------------------
//
//  BlockParentIndent is only meaningful when parsing a record/class type that
//  might open an indented field block.  Pass the column of the enclosing
//  'type' or 'record' keyword so ParseAddType knows what indent a block would
//  start from.
//
function TParser.ParseAddType(Name: string='';
                              SkipFinalSeparators: Boolean=True;
                              AllowUntyped: Boolean=False;
                              BlockParentIndent: Int32=-1): XType;
var
  i:           Int32;
  Idents:      XIdentNodeList;
  DType:       XType;
  Fields:      XStringList;
  Types:       XTypeList;
  Args:        TStringArray;
  ArgTypes:    XTypeArray;
  ArgPass:     TPassArgsBy;
  RetType:     XType;
  wasCurlyRec, isLambda, isRef, isPacked: Boolean;
  TargetName,  callingConv: string;
  Expr:        XTree_Node;
  kwLine:      Int32;  // source line of record/class keyword
  recParentIndent: Int32;
  LFields: XStringList;
  LTypes:  XTypeList;
begin
  Result := nil;
  SetInsesitive();
  SkipNewline();

  if NextIf(tkDEREF) then
    Result := XType_Pointer.Create(ParseAddType('', False))
  else case Current.Token of

    tkKW_TYPE:
      begin
        Result      := XType.Create(xtUnknown);
        Result.Name := Current.Value;
        FContext.AddManagedType(Result);
        Next();
      end;

    tkIDENT:
      begin
        if (XprCase(Current.Value) = 'typeof') and (Peek(1).Token = tkLPARENTHESES) then
        begin
          Next();
          Consume(tkLPARENTHESES);
          Expr := ParseExpression(False, False);
          Consume(tkRPARENTHESES);
          Result          := XType.Create(xtUnknown);
          Result.Name     := '';
          Result.TypeOfExpr := Pointer(Expr);
          FContext.AddManagedType(Result);
        end else
        begin
          TargetName := ParseNSIdent(False).Value;
          Result := FContext.GetType(TargetName);
          if Result = nil then
          begin
            Result      := XType.Create(xtUnknown);
            Result.Name := TargetName;
            FContext.AddManagedType(Result);
          end;
          Next();
        end;
      end;

    // record: inline if tokens follow on same line, block otherwise
    // compact:  TPoint = record x, y: Int
    // block:    TRec = record
    //             s: string
    tkKW_PACKED, tkKW_RECORD, tkLPARENTHESES:
      begin
        isPacked := NextIf(tkKW_PACKED);
        kwLine      := DocPos.Line;
        wasCurlyRec := Next().Token = tkLPARENTHESES;

        Fields.Init([]);
        Types.Init([]);

        if wasCurlyRec or OnSameLine(kwLine) then
        begin
          if wasCurlyRec then SkipNewline;
          // Single line form `record`: only parse fields on the same line as 'record'.
          // Do NOT SkipNewline here, that would walk onto the next declaration.
          while (Current.Token = tkIDENT) and (wasCurlyRec or OnSameLine(kwLine)) do
          begin
            Idents := ParseIdentList(True);
            Consume(tkCOLON);
            // SkipFinalSeparators=False: let us control when to eat the terminator
            DType := ParseAddType('', False, AllowUntyped);
            // Eat an optional semicolon but never a newline - that belongs to the outer loop
            if Current.Token = tkSEMI then Next();
            for i := 0 to Idents.High do
            begin
              Fields.Add(XTree_Identifier(Idents.Data[i]).Name);
              Types.Add(DType);
              Idents.Data[i].Free();
            end;
            if wasCurlyRec then SkipNewline;
          end;

          if wasCurlyRec then Consume(tkRPARENTHESES);
        end else
        begin
          // -- Block form ---------------------------------------------------
          // Body is indented past BlockParentIndent (the 'record' keyword's col).
          // If no BlockParentIndent supplied, use the current indent as parent.
          if BlockParentIndent >= 0 then
            recParentIndent := BlockParentIndent
          else
            recParentIndent := DocPos.Column;

          SkipTokens(SEPARATORS);
          while CurrentIndent() > recParentIndent do
          begin
            SkipTokens(SEPARATORS);
            if Current.Token <> tkIDENT then break;
            Idents := ParseIdentList(True);
            Consume(tkCOLON);
            DType  := ParseAddType('', True, AllowUntyped);
            SkipTokens(SEPARATORS);
            for i := 0 to Idents.High do
            begin
              Fields.Add(XTree_Identifier(Idents.Data[i]).Name);
              Types.Add(DType);
              Idents.Data[i].Free();
            end;
          end;
        end;

        if Fields.Size = 0 then
          RaiseExceptionFmt(eExpectedButFound, ['field variables', '`end of record`']);

        Result := XType_Record.Create(Fields, Types);
        XType_Record(Result).Aligned := not isPacked;
        FContext.AddManagedType(Result);
      end;

    tkKW_ARRAY:
      begin
        if AllowUntyped and (Peek().Token <> tkKW_OF) then
        begin
          Result      := XType.Create(xtUnknown);
          Result.Name := Current.Value;
          FContext.AddManagedType(Result);
          Next();
        end else
        begin
          Next();
          Consume(tkKW_OF);
          Result := XType_Array.Create(ParseAddType('', False, AllowUntyped));
          FContext.AddManagedType(Result);
        end;
      end;

    tkKW_FUNC, tkKW_LAMBDA:
      begin
        isLambda := Next().Token = tkKW_LAMBDA;
        SetLength(Args, 0);
        SetLength(ArgTypes, 0);
        SetLength(ArgPass, 0);
        RetType := nil;

        if NextIf(tkLPARENTHESES) then
        begin
          while Current.Token <> tkRPARENTHESES do
          begin
            isRef  := NextIf(tkKW_REF);
            DType  := ParseAddType('', False);
            SetLength(ArgTypes, Length(ArgTypes) + 1);
            ArgTypes[High(ArgTypes)] := DType;
            SetLength(ArgPass, Length(ArgPass) + 1);
            if isRef then ArgPass[High(ArgPass)] := pbRef
            else          ArgPass[High(ArgPass)] := pbCopy;
            if not NextIf(tkCOMMA) then break;
          end;
          Consume(tkRPARENTHESES);
        end;

        if NextIf(tkCOLON) then
          RetType := ParseAddType('', False);

        Result := XType_Method.Create('', ArgTypes, ArgPass, RetType, False);

        if NextIf(tkSEMI) then
        begin
          callingconv := Current.Value;
          if NextIf(tkIDENT) then
          begin
            XType_Method(Result).CallingConvention := XprCase(callingConv);
            if XprCCToABI(callingConv) <> FFI_UNKNOWN_ABI then
              XType_Method(Result).BaseType := xtExternalMethod;
          end;
        end;

        if isLambda then
        begin
          LFields.Init(['method', 'size', 'args']);
          LTypes.Init([Result as XType,
                       FContext.GetType(xtInt),
                       FContext.GetType('!ClosureArray')]);
          Result := XType_Lambda.Create(LFields, LTypes);
        end;
        FContext.AddManagedType(Result);
      end;

  else
    RaiseExceptionFmt(eExpectedButFound, ['type declaration', Current.Value]);
  end;

  if Name <> '' then
    FContext.AddType(Name, Result);

  if SkipFinalSeparators then
    SkipTokens(SEPARATORS);

  ResetInsesitive();
end;

// -- if ------------------------------------------------------------------------
//
//  if (cond) then              ← block body (newline after 'then')
//    stmt1
//    stmt2
//  elif (cond2) then           ← continuation at same col as 'if'
//    stmt3
//  else                        ← continuation at same col as 'if'
//    stmt4
//
//  if (cond) then stmt         ← single-statement body (token on same line)
//
function TParser.ParseIf(): XTree_If;
var
  myIndent:   Int32;
  myLine:     Int32;
  Condition:  XTree_Node;
  Conditions: specialize TArrayList<XTree_Node>;
  Bodys:      specialize TArrayList<XTree_Node>;
  ElseBody:   XTree_ExprList;

  function ParseBody(): XTree_ExprList;
  begin
    if AtEndOfLine() then
      // block form
      Result := ParseBlockAsExprList(myIndent)
    else
      // single-statement form: token already on this line
      Result := XTree_ExprList.Create(ParseStatement(), FContext, DocPos);
  end;

begin
  myIndent := DocPos.Column;
  myLine   := DocPos.Line;
  Consume(tkKW_IF);
  Consume(tkLPARENTHESES);
  Condition := ParseExpression(False);
  Consume(tkRPARENTHESES);

  Bodys.Init([]);
  Conditions.Init([]);
  ElseBody := nil;

  NextIf(tkKW_THEN);  // 'then' is optional but consumed if present
  Bodys.Add(ParseBody());
  Conditions.Add(Condition);

  // Handle elif / else continuations at the same indent level
  while True do
  begin
    SkipTokens(SEPARATORS);
    if CurrentIndent() <> myIndent then break;

    if Current.Token = tkKW_ELIF then
    begin
      Next(); // consume 'elif'
      Consume(tkLPARENTHESES);
      Condition := ParseExpression(False);
      Consume(tkRPARENTHESES);
      NextIf(tkKW_THEN);
      Bodys.Add(ParseBody());
      Conditions.Add(Condition);
    end else if Current.Token = tkKW_ELSE then
    begin
      Next(); // consume 'else'
      ElseBody := ParseBody();
      break;  // else is always the last clause
    end else
      break;
  end;

  Result := XTree_If.Create(
    XNodeArray(Conditions.Raw()),
    XNodeArray(Bodys.Raw()),
    ElseBody, FContext, DocPos);
end;

// -- switch/case ---------------------------------------------------------------
//
//  switch expr of
//    case 1:
//      stmt
//    case 2, 3:
//      stmt
//    else:
//      stmt
//
function TParser.ParseSwitch(): XTree_Case;
var
  myIndent:      Int32;
  Expression:    XTree_Node;
  Branches:      specialize TArrayList<TCaseBranch>;
  CurrentBranch: TCaseBranch;
  ElseBody:      XTree_Node;
  caseIndent:    Int32;  // indent of 'case' / 'else' labels
begin
  myIndent   := DocPos.Column;
  Consume(tkKW_SWITCH);
  Expression := ParseExpression(False);
  Consume(tkKW_OF);

  Branches.Init([]);
  ElseBody := nil;

  // The case labels are indented one level past the switch keyword.
  // We discover that indent from the first label.
  SkipTokens(SEPARATORS);
  if Current.Token = tkUNKNOWN then
  begin
    Result := XTree_Case.Create(Expression, Branches.RawOfManaged(), nil, FContext, DocPos);
    Exit;
  end;

  caseIndent := CurrentIndent();
  if caseIndent <= myIndent then
  begin
    Result := XTree_Case.Create(Expression, Branches.RawOfManaged(), nil, FContext, DocPos);
    Exit;
  end;

  while True do
  begin
    SkipTokens(SEPARATORS);
    if Current.Token = tkUNKNOWN then break;
    if CurrentIndent() < caseIndent then break; // dedented past case labels

    case Current.Token of
      tkKW_CASE:
        begin
          Next(); // consume 'case'
          CurrentBranch.Labels.Init([]);

          // Parse comma-separated labels
          repeat
            SkipNewline;
            CurrentBranch.Labels.Add(ParseAtom());
          until not NextIf(tkCOMMA);

          Consume(tkCOLON);

          // Body is indented past the 'case' label's column.
          CurrentBranch.Body := ParseBlockAsExprList(caseIndent);
          Branches.Add(CurrentBranch);
        end;

      tkKW_ELSE:
        begin
          Next(); // consume 'else'
          Consume(tkCOLON);
          ElseBody := ParseBlockAsExprList(caseIndent);
          break;
        end;

      tkNEWLINE, tkSEMI:
        Next();

    else
      break;
    end;
  end;

  Result := XTree_Case.Create(
    Expression, Branches.RawOfManaged(), ElseBody, FContext, DocPos);
end;

// -- while ---------------------------------------------------------------------
//
//  while (cond) do       ← block (newline after 'do')
//    stmt1
//
//  while (cond) do stmt  ← single statement
//
function TParser.ParseWhile(): XTree_While;
var
  myIndent:  Int32;
  Condition: XTree_Node;
  Body:      XTree_ExprList;
begin
  myIndent  := DocPos.Column;
  Consume(tkKW_WHILE);
  Consume(tkLPARENTHESES);
  Condition := ParseExpression(False);
  Consume(tkRPARENTHESES);

  NextIf(tkKW_DO); // 'do' is optional but consumed if present

  Inc(FLooping);
  try
    if AtEndOfLine() then
      Body := ParseBlockAsExprList(myIndent)
    else
      Body := XTree_ExprList.Create(ParseStatement(), FContext, DocPos);
  finally
    Dec(FLooping);
  end;

  Result := XTree_While.Create(Condition, Body, FContext, DocPos);
end;

function TParser.ParsePass(): XTree_Pass;
begin
  Result := XTree_Pass.Create(FContext, DocPos);
  Consume(tkKW_PASS);
end;

// -- repeat-until --------------------------------------------------------------
//
//  repeat            ← block body
//    stmt
//  until (cond)      ← continuation at same col as 'repeat'
//
function TParser.ParseRepeat(): XTree_Repeat;
var
  myIndent:  Int32;
  Condition: XTree_Node;
  Body:      XTree_ExprList;
begin
  myIndent := DocPos.Column;
  Consume(tkKW_REPEAT);

  Inc(FLooping);
  try
    Body := ParseBlockAsExprList(myIndent);

    // 'until' must be at the same column as 'repeat'
    SkipTokens(SEPARATORS);
    if CurrentIndent() <> myIndent then
      RaiseException('`until` must be at the same indentation as `repeat`');
    Consume(tkKW_UNTIL);
    Consume(tkLPARENTHESES);
    Condition := ParseExpression(False);
    Consume(tkRPARENTHESES);

    Result := XTree_Repeat.Create(Condition, Body, FContext, DocPos);
  finally
    Dec(FLooping);
  end;
end;

// -- for (C-style) -------------------------------------------------------------

function TParser.ParseFor(): XTree_For;
var
  myIndent:              Int32;
  EntryStmt, Condition,
  LoopStmt:              XTree_Node;
  Body:                  XTree_ExprList;
begin
  myIndent := DocPos.Column;
  Consume(tkKW_FOR);
  Consume(tkLPARENTHESES);

  if Current.Token = tkKW_VAR then
  begin
    EntryStmt := ParseVardecl();
    Consume(tkSEMI);
  end else
    EntryStmt := ParseExpression(True);

  Condition := ParseExpression(True);
  LoopStmt  := ParseExpression(False);
  Consume(tkRPARENTHESES);

  NextIf(tkKW_DO);

  Inc(FLooping);
  try
    if AtEndOfLine() then
      Body := ParseBlockAsExprList(myIndent)
    else
      Body := XTree_ExprList.Create(ParseStatement(), FContext, DocPos);
  finally
    Dec(FLooping);
  end;

  Result := XTree_For.Create(EntryStmt, Condition, LoopStmt, Body, FContext, DocPos);
end;

// -- for-in --------------------------------------------------------------------

function TParser.ParseForIn(): XTree_Node;
var
  myIndent, i:   Int32;
  items:      XNodeList;
  collection: XTree_Node;
  body:       XTree_ExprList;
  _pos:       TDocPos;
  OldFPos:    Int32;
  DeclareVar: Byte;
  PatternNode: XTree_Node;
  idents: XIdentNodeList;
  nodes: XNodeArray;
begin
  _pos     := DocPos;
  myIndent := DocPos.Column;
  OldFPos  := FPos;

  Consume(tkKW_FOR);
  Consume(tkLPARENTHESES);

  DeclareVar := 0;
  if NextIf(tkKW_VAR) then DeclareVar := 1
  else if NextIf(tkKW_REF) then DeclareVar := 2;

  items.Init([]);

  // Special case: var (x, y) - destructuring pattern inside for-in
  if (DeclareVar = 1) and (Current.Token = tkLPARENTHESES) then
  begin
    Next(); // consume '('
    idents := ParseIdentList(True);
    Consume(tkRPARENTHESES);

    SetLength(nodes, idents.Size);
    for i := 0 to idents.High do
      nodes[i] := idents.Data[i];
    items.Add(XTree_Destructure.Create(nodes, FContext, _pos));
  end else
  begin
    repeat
      items.Add(ParseExpression(False, False));
    until (not NextIf(tkCOMMA)) or (DeclareVar = 2);
  end;

  if not NextIf(tkKW_IN) then
  begin
    FPos := OldFPos;
    Exit(ParseFor());
  end;

  collection := ParseExpression(False);
  Consume(tkRPARENTHESES);

  NextIf(tkKW_DO);

  Inc(FLooping);
  try
    if AtEndOfLine() then
      body := ParseBlockAsExprList(myIndent)
    else
      body := XTree_ExprList.Create(ParseStatement(), FContext, DocPos);
  finally
    Dec(FLooping);
  end;

  if items.Size = 1 then
    Result := XTree_ForIn.Create(items.Data[0], collection, DeclareVar, body, FContext, _pos)
  else
    Result := XTree_ForIn.Create(
      XTree_Destructure.Create(items.Raw(), FContext, _pos),
      collection, DeclareVar, body, FContext, _pos);
end;

// -- try/except ----------------------------------------------------------------
//
//  try
//    risky()
//  except on e: EType do      ← continuation at same col as 'try'
//    handle()
//  except                     ← catch-all, also at same col
//    fallback()
//
function TParser.ParseTry(): XTree_Try;
var
  myIndent:       Int32;
  TryBody:        XTree_Node;
  ElseBody:       XTree_Node;
  Doc:            TDocPos;
  Handlers:       specialize TArrayList<TExceptionHandler>;
  CurrentHandler: TExceptionHandler;
begin
  Doc      := Self.DocPos;
  myIndent := DocPos.Column;
  Consume(tkKW_TRY);

  TryBody := ParseBlockAsExprList(myIndent);

  Handlers.Init([]);
  ElseBody := nil;

  // 'except' continuations at same indent as 'try'
  while True do
  begin
    SkipTokens(SEPARATORS);
    if CurrentIndent() <> myIndent then break;
    if Current.Token <> tkKW_EXCEPT then break;

    Next(); // consume 'except'

    if Current.Token = tkKW_ON then
    begin
      Next(); // consume 'on'
      CurrentHandler.VarName       := ParseNSIdent(True).Value;
      Consume(tkCOLON);
      CurrentHandler.ExceptionType := ParseAddType();
      NextIf(tkKW_DO); // 'do' optional
      CurrentHandler.Body := ParseBlockAsExprList(myIndent);
      Handlers.Add(CurrentHandler);
    end else
    begin
      // catch-all 'except' block
      ElseBody := ParseBlockAsExprList(myIndent);
      break;
    end;
  end;

  Result := XTree_Try.Create(
    TryBody as XTree_ExprList,
    Handlers.RawOfManaged(),
    ElseBody, FContext, Doc);
end;

// -- break / continue ---------------------------------------------------------

function TParser.ParseBreak(): XTree_Break;
begin
  Consume(tkKW_BREAK);
  if FLooping <= 0 then RaiseException('`break` is not allowed outside of a loop');
  Result := XTree_Break.Create(FContext, DocPos);
end;

function TParser.ParseContinue(): XTree_Continue;
begin
  Consume(tkKW_CONTINUE);
  if FLooping <= 0 then RaiseException('`continue` is not allowed outside of a loop');
  Result := XTree_Continue.Create(FContext, DocPos);
end;

// -- func ---------------------------------------------------------------------
//
//  func name(params): RetType    ← block body follows on next line(s)
//    stmt
//
//  func name(params): RetType => expr   ← single-expression shorthand
//
//  lambda(params): RetType ...   ← closure
//
function TParser.ParseFunction(IsExpression: Boolean = False): XTree_Node;
var
  myIndent:   Int32;
  myLine:     Int32;
  Name:       string;
  TypeName:   string;
  Idents:     TStringArray;
  Args:       TStringArray;
  DType:      XType;
  ByRef:      TPassArgsBy;
  Types:      XTypeArray;
  Body:       XTree_ExprList;
  Ret:        XType;
  HeaderDocPos: TDocPos;
  TypeParams: TStringArray;      // <T, U, ...> from declaration
  TypeConstraints: TStringArray; // parallel constraints: 'numeric', 'array', '' etc.
  Defaults: XNodeArray;          // parallel to Args: nil = required
  isLambda:   Boolean;
  HasReturn:  Boolean;
  TypeMethod: XType;
  Expr:       XTree_Node;
  CallingConv:string;

  procedure ParseParams();
  var i, l: Int32; isRef: Boolean; defaultExpr: XTree_Node;
  begin
    isRef := NextIf(tkKW_REF);
    while Current.Token = tkIDENT do
    begin
      SetLength(Idents, 0);
      Idents := ParseIdentListRaw(True);
      Consume(tkCOLON);

      // AllowUntyped=True so param types named T/U/etc. (xtUnknown) are accepted
      DType := ParseAddType('', True, True);

      // Optional default value:  param: Type = expr
      defaultExpr := nil;
      if NextIf(tkASGN) then
        defaultExpr := ParseExpression(False, False);

      SkipTokens(SEPARATORS);
      for i := 0 to High(Idents) do
      begin
        l := Length(Types);
        SetLength(Types,    l + 1);
        SetLength(Args,     l + 1);
        SetLength(ByRef,    l + 1);
        SetLength(Defaults, l + 1);
        Args[l]     := Idents[i];
        if isRef then ByRef[l] := pbRef else ByRef[l] := pbCopy;
        Types[l]    := DType;
        Defaults[l] := defaultExpr;
      end;
      isRef := NextIf(tkKW_REF);
    end;
  end;

begin
  myIndent     := DocPos.Column;
  myLine       := DocPos.Line;
  HeaderDocPos := DocPos;

  SetLength(TypeName, 0);
  SetLength(Args,     0);
  SetLength(ByRef,    0);
  SetLength(Types,    0);
  SetLength(TypeParams, 0);
  SetLength(TypeConstraints, 0);
  SetLength(Defaults, 0);
  isLambda := False;

  case Current.Token of
    tkKW_FUNC:   Consume(tkKW_FUNC);
    tkKW_LAMBDA: begin Consume(tkKW_LAMBDA); isLambda := True; end;
  end;

  if isLambda then
    myIndent := LineStartIndent(myLine);

  if not isLambda then
  begin
    if IsExpression then RaiseException('Anonymous function expected!');
    // Parse optional type-method prefix: TypeName.MethodName
    // AllowUntyped=True so generic param names in SelfType parse as xtUnknown
    TypeMethod := ParseAddType('', True, True);

    // Type params can appear in two positions:
    //
    //  BEFORE the dot - method on a generic type:
    //    func TArray<T>.Append(value: T)
    //    func Compare<T>(x,y: T)          ← no dot, T applies to the function itself
    //
    //  AFTER the method name - method with its own type params:
    //    func array.NumericSum<T:numeric>(): T
    //
    // Parse BEFORE the dot here:
    if Current.Token = tkLT then
      ParseTypeParamsFull(TypeParams, TypeConstraints);

    if NextIf(tkDOT) then
    begin
      Name := Consume(tkIDENT, PostInc).Value;
      // Parse AFTER the method name (only if not already parsed before the dot):
      if (Length(TypeParams) = 0) and (Current.Token = tkLT) then
        ParseTypeParamsFull(TypeParams, TypeConstraints);
    end
    else
    begin
      Name       := TypeMethod.Name;
      TypeMethod := nil;
    end;
  end else
  begin
    TypeMethod := nil;
    Name       := '';
  end;

  HasReturn := False;
  Consume(tkLPARENTHESES);
  SetInsesitive();
  if Current.Token <> tkRPARENTHESES then ParseParams();
  Consume(tkRPARENTHESES);
  ResetInsesitive();

  if NextIf(tkCOLON) then
  begin
    Ret       := ParseAddType('', True, Length(TypeParams) > 0);
    HasReturn := True;
  end else
    Ret := nil;

  // allow a calling convention
  CallingConv := '';
  if (Peek(-1).Token = tkSEMI) and (Current.Token = tkIDENT) then
  begin
    CallingConv := Current.Value;
    Next();
  end;

  SkipTokens(SEPARATORS);

  // -- Shorthand: func name(): T => expr --------------------------------------
  if HasReturn and NextIf(tkEQ) and (Consume(tkGT).Token = tkGT) then
  begin
    Expr := ParseExpression(False, False);
    Body := XTree_ExprList.Create(
      [XTree_Assign.Create(op_Asgn,
         XTree_Identifier.Create('result', FContext, DocPos),
         Expr, FContext, DocPos)],
      FContext, DocPos);
  end else
  begin
    // -- Block body: indented past the func keyword --------------------------
    Body := ParseBlockAsExprList(myIndent);
  end;

  Result := XTree_Function.Create(Name, Args, ByRef, Types, Ret, Body, FContext, HeaderDocPos);
  XTree_Function(Result).CallingConvention := CallingConv;
  XTree_Function(Result).TypeParams        := TypeParams;
  XTree_Function(Result).TypeConstraints   := TypeConstraints;
  XTree_Function(Result).ArgDefaults       := Defaults;

  if TypeMethod <> nil then
    XTree_Function(Result).SelfType := TypeMethod;

  // Wrap in XTree_GenericFunction when type params are declared
  if Length(TypeParams) > 0 then
    Result := XTree_GenericFunction.Create(Result, FContext, HeaderDocPos);

  if isLambda then
    Result := XTree_ClosureFunction.Create(Result as XTree_Function, FContext, HeaderDocPos);
end;

// -- ref declaration -----------------------------------------------------------

function TParser.ParseRefdecl(): XTree_Node;
var
  Identifiers: XIdentNodeList;
begin
  Next();
  Identifiers := ParseIdentList(True);
  Result := XTree_NonLocalDecl.Create(
    Identifiers, FContext, Identifiers.Data[0].FDocPos);
end;

// -- var / const declaration ---------------------------------------------------

function TParser.ParseVardecl(): XTree_Node;
var
  Nodes:   XNodeArray;
  Right:   XTree_Node;
  Pattern: XTree_Node;
  Left:    XIdentNodeList;
  Typ:     XType;
  tok:     TToken;
  i:       Int32;
begin
  tok := Current; // remember 'var' or 'const'
  Next();

  if Current.Token = tkLPARENTHESES then
  begin
    Next();
    Left := ParseIdentList(True);
    Consume(tkRPARENTHESES);
    SetLength(Nodes, Left.Size);
    for i := 0 to High(Nodes) do Nodes[i] := Left.Data[i];
    Pattern := XTree_Destructure.Create(Nodes, FContext, DocPos);
    Consume(tkASGN);
    Right := ParseExpression(False);
    Result := XTree_DestructureDecl.Create(
      Pattern as XTree_Destructure, Right, FContext, Pattern.FDocPos);
    Exit;
  end;

  Left  := ParseIdentList(True);
  Right := nil;
  if NextIf(tkCOLON) then
  begin
    Typ := ParseAddType('', False);
    if NextIf(tkEQ) then Right := ParseExpression(False)
    else ConsumeSeparator();
  end else
  begin
    Typ := nil;
    Consume(tkASGN);
    Right := ParseExpression(False);
  end;

  Result := XTree_VarDecl.Create(
    Left, Right, Typ, tok.Token = tkKW_CONST, FContext, Left.Data[0].FDocPos);
end;

// -- class declaration ---------------------------------------------------------
//
//  type
//    TFoo = class(TBase)    ← ParentIndent is the 'type' block's parent indent
//      var x: Int           ← class body at TFoo's indent + 1
//      func Create()
//        self.x := 0
//
function TParser.ParseClassDecl(ClassDeclName: string; ParentIndent: Int32;
                                 ATypeParams: TStringArray;
                                 ATypeConstraints: TStringArray): XTree_ClassDecl;
var
  myIndent:   Int32;
  ParentName: string;
  Fields:     XNodeArray;
  Methods:    XNodeArray;
  _pos:       TDocPos;
begin
  _pos     := DocPos;
  myIndent := ParentIndent;   // column of 'class' keyword

  Consume(tkKW_CLASS);

  ParentName := '';
  if NextIf(tkLPARENTHESES) then
  begin
    ParentName := ParseNSIdent(True).Value;
    Consume(tkRPARENTHESES);
  end;

  SetLength(Fields,  0);
  SetLength(Methods, 0);

  // Body is indented past myIndent.
  SkipTokens(SEPARATORS);
  while CurrentIndent() > myIndent do
  begin
    SkipTokens(SEPARATORS);
    if Current.Token = tkUNKNOWN then break;
    if CurrentIndent() <= myIndent then break;

    case Current.Token of
      tkKW_VAR, tkKW_CONST:
        Fields  += ParseVardecl();
      tkKW_FUNC:
        Methods += ParseFunction();
      tkNEWLINE, tkSEMI:
        Next();
    else
      RaiseExceptionFmt(
        'Unexpected token in class body: `%s`. Expected `var`, `const`, or `func`.',
        [Current.Value]);
    end;
  end;

  Result := XTree_ClassDecl.Create(
    ClassDeclName, ParentName, Fields, Methods, FContext, _pos);
  Result.TypeParams      := ATypeParams;
  Result.TypeConstraints := ATypeConstraints;
end;

// -- type declaration ----------------------------------------------------------
//
//  Two forms:
//
//  1. Single (backward compat): type TAlias = array of Int
//
//  2. Block form:
//       type
//         TAlias  = array of Int
//         TPoint  = record x, y: Int
//         TRec    = record
//           s: string
//         TFoo    = class(TBase)
//           var x: Int
//           func Create() ...
//
function TParser.ParseTypeDecl(): XTree_Node;
var
  myIndent, declIndent: Int32;
  Name:        string;
  SourceName:  string;
  Typ:         XType;
  nodes:       XNodeArray;
  _pos:        TDocPos;
  TypeParams:  TStringArray;
  Constraints: TStringArray;
  TypeDecl:    XTree_TypeDecl;
  ExplicitTypes: XTypeArray;
begin
  _pos     := DocPos;
  myIndent := DocPos.Column;
  Consume(tkKW_TYPE);

  // -- Block form: 'type' alone on line --------------------------------------
  if AtEndOfLine() then
  begin
    SkipTokens(SEPARATORS);
    SetLength(nodes, 0);

    while CurrentIndent() > myIndent do
    begin
      SkipTokens(SEPARATORS);
      if Current.Token = tkUNKNOWN then break;
      if CurrentIndent() <= myIndent then break;

      declIndent := CurrentIndent();
      Name := Consume(tkIDENT, PostInc).Value;

      // Optional generic type params in block form: TMyClass<K, V> = class
      SetLength(TypeParams, 0);
      SetLength(Constraints, 0);
      if Current.Token = tkLT then
        ParseTypeParamsFull(TypeParams, Constraints);

      Consume(tkEQ, PostInc);

      if Current.Token = tkKW_SPECIALIZE then
      begin
        // type TA = specialize TSource<T1, T2>
        Next(); // consume 'specialize'
        SourceName    := ParseNSIdent(True).Value;
        ExplicitTypes := ParseTypeParams_Concrete();
        nodes += XTree_Specialize.CreateTypeSpec(Name, SourceName, ExplicitTypes, FContext, DocPos);
      end
      else if Current.Token = tkKW_CLASS then
      begin
        nodes += ParseClassDecl(Name, declIndent, TypeParams, Constraints);
      end
      else
      begin
        SetInsesitive();
        Typ := ParseAddType('', True, Length(TypeParams) > 0, declIndent);
        ResetInsesitive();
        TypeDecl := XTree_TypeDecl.Create(Name, Typ, FContext, DocPos);
        TypeDecl.TypeParams      := TypeParams;
        TypeDecl.TypeConstraints := Constraints;
        nodes += TypeDecl;
      end;
      SkipTokens(SEPARATORS);
    end;

    if Length(nodes) = 0 then
      RaiseException('Expected at least one type declaration inside `type` block');

    if Length(nodes) = 1 then
      Result := nodes[0]
    else
      Result := XTree_ExprList.Create(nodes, FContext, _pos);

    Exit;
  end;

  // -- Single form: type TAlias = ... ----------------------------------------
  declIndent := CurrentIndent();
  Name := Consume(tkIDENT, PostInc).Value;

  // Optional generic type params: type TArray<T> = array of T
  SetLength(TypeParams, 0);
  SetLength(Constraints, 0);
  if Current.Token = tkLT then
    ParseTypeParamsFull(TypeParams, Constraints);

  Consume(tkEQ, PostInc);

  if Current.Token = tkKW_SPECIALIZE then
  begin
    // type TA = specialize TSource<T1, T2>
    Next(); // consume 'specialize'
    SourceName    := ParseNSIdent(True).Value;
    ExplicitTypes := ParseTypeParams_Concrete();
    Result := XTree_Specialize.CreateTypeSpec(Name, SourceName, ExplicitTypes, FContext, DocPos);
  end
  else if Current.Token = tkKW_CLASS then
  begin
    Result := ParseClassDecl(Name, myIndent, TypeParams, Constraints);
  end
  else
  begin
    SetInsesitive();
    Typ := ParseAddType('', True, Length(TypeParams) > 0, myIndent);
    ResetInsesitive();
    TypeDecl := XTree_TypeDecl.Create(Name, Typ, FContext, DocPos);
    TypeDecl.TypeParams      := TypeParams;
    TypeDecl.TypeConstraints := Constraints;
    Result := TypeDecl;
  end;
end;

// -- raise ---------------------------------------------------------------------

function TParser.ParseRaise(): XTree_Raise;
var
  name: string;
  _pos: TDocPos;
begin
  _pos := DocPos;
  Next(); // consume 'raise'

  if (Current.Token = tkIDENT) and (Peek(1).Token = tkLPARENTHESES) then
  begin
    Name := ParseNSIdent(True).Value;
    Consume(tkLPARENTHESES);
    Result := XTree_Raise.Create(
      XTree_ClassCreate.Create(name, ParseExpressionList(True, True), FContext, _pos),
      FContext, _pos);
    Consume(tkRPARENTHESES);
  end else
    Result := XTree_Raise.Create(ParseExpression(False), FContext, _pos);
end;

// -- if-expression (ternary) ---------------------------------------------------

function TParser.ParseIfExpr(): XTree_IfExpr;
var
  _pos: TDocPos;
  Cond, ThenN, ElseN: XTree_Node;
begin
  _pos := DocPos;
  Consume(tkKW_IF);
  Consume(tkLPARENTHESES);
  Cond := ParseExpression(False);
  Consume(tkRPARENTHESES);
  ThenN := ParseExpression(False);
  Consume(tkKW_ELSE);
  ElseN := ParseExpression(False, False);
  Result := XTree_IfExpr.Create(Cond, ThenN, ElseN, FContext, _pos);
end;

// -- [1, 2, 3] initializer list -----------------------------------------------

function TParser.ParseInitializerList(): XTree_InitializerList;
var
  Items:    XNodeArray;
  Expr:     XTree_Node;
  DocStart: TDocPos;
begin
  Consume(tkLSQUARE);
  SkipTokens(SEPARATORS);
  SetInsesitive();

  SetLength(Items, 0);
  DocStart := DocPos;

  if Current.Token = tkRSQUARE then
  begin
    Next();
    Exit(XTree_InitializerList.Create(Items, FContext, DocStart));
  end;

  repeat
    Expr := ParseExpression(False, False);
    if Expr = nil then RaiseException('Expected an expression inside initializer list.');
    Items += Expr;
    SkipNewline;
  until not NextIf(tkCOMMA);

  ResetInsesitive();
  Consume(tkRSQUARE);
  Result := XTree_InitializerList.Create(Items, FContext, DocStart);
end;

// -- Destructure list ---------------------------------------------------------

function TParser.ParseDestructureList(): XTree_Destructure;
var
  IsPattern: Boolean;
  Targets:   XNodeArray;
begin
  Result := nil;
  Targets := ParseExpressionList(True, False);
  SkipNewline;

  if Current.Token <> tkRPARENTHESES then IsPattern := False
  else IsPattern := Peek(1).Token = tkASGN;

  if IsPattern then
  begin
    Consume(tkRPARENTHESES);
    Result := XTree_Destructure.Create(Targets, FContext, DocPos);
  end;
end;

// -- list comprehensions ------------------------------------------------------

function TParser.ParseListComp(): XTree_Node;
var
  _pos:       TDocPos;
  ItemVar:    XTree_Node;
  Collection: XTree_Node;
  StartExpr:  XTree_Node;
  EndExpr:    XTree_Node;
  FilterExpr: XTree_Node;
  YieldExpr:  XTree_Node;
  VarName:    string;
  IsRange:    Boolean;
  idents:     XIdentNodeList;
  nodes:      XNodeArray;
  i:          Int32;
  DeclareVar: Boolean;
begin
  _pos := DocPos;

  Consume(tkLSQUARE);
  Self.SetInsesitive(True);

  Consume(tkKW_FOR);
  DeclareVar := NextIf(tkKW_VAR);  // optional

  FilterExpr := nil;
  Collection := nil;
  StartExpr  := nil;
  EndExpr    := nil;
  ItemVar    := nil;
  IsRange    := False;


  VarName := Consume(tkIDENT, PostInc).Value;

  if Current.Token = tkASGN then
  begin
    IsRange := True;
    Consume(tkASGN);
    StartExpr := ParseExpression(False);
    Consume(tkKW_TO);
    EndExpr := ParseExpression(False);
    ItemVar := XTree_Identifier.Create(VarName, FContext, _pos);
  end
  else
  begin
    ItemVar := XTree_Identifier.Create(VarName, FContext, _pos);
    Consume(tkKW_IN);
    Collection := ParseExpression(False);
  end;


  Consume(tkKW_DO);
  SkipNewline;

  // Optional where clause
  if Current.Token = tkKW_WHERE then
  begin
    Next(); // consume 'where'
    Consume(tkLPARENTHESES);
    FilterExpr := ParseExpression(False);
    Consume(tkRPARENTHESES);
    SkipNewline;
  end;

  // Yield expression
  YieldExpr := ParseExpression(False, False);

  Self.ResetInsesitive();
  Consume(tkRSQUARE);

  Result := XTree_ListComp.Create(
    ItemVar, Collection,
    StartExpr, EndExpr,
    FilterExpr, YieldExpr,
    IsRange, DeclareVar,
    FContext, _pos);
end;

// -- return --------------------------------------------------------------------

function TParser.ParseReturn(): XTree_Return;
begin
  Consume(tkKW_RETURN);
  Result := XTree_Return.Create(ParseExpression(False), FContext, DocPos);
end;

// -- Identifiers --------------------------------------------------------------

function TParser.ParseIdentRaw(): string;
begin
  Result := Consume(tkIDENT).Value;
end;

function TParser.ParseIdentListRaw(Insensitive: Boolean): TStringArray;
begin
  SetLength(Result, 0);
  SetInsesitive(Insensitive);
  Result += ParseIdentRaw();
  while NextIf(tkCOMMA) do Result += ParseIdentRaw();
  ResetInsesitive();
end;

function TParser.ParseIdent(): XTree_Identifier;
var docstart: TDocPos;
begin
  docstart := DocPos;
  Result   := XTree_Identifier.Create(Self.ParseIdentRaw(), FContext, docstart);
end;

function TParser.ParseIdentList(Insensitive: Boolean): XIdentNodeList;
begin
  SetInsesitive(Insensitive);
  Result.Init([]);
  Result.Add(ParseIdent());
  while NextIf(tkCOMMA) do Result.Add(ParseIdent());
  ResetInsesitive();
end;

// -- Atoms ---------------------------------------------------------------------

function TParser.ParseAtom(): XTree_Node;
begin
  Result := nil;
  case Current.Token of
    tkBOOL:    Result := XTree_Bool.Create(Current.Value, FContext, DocPos);
    tkKW_NIL:  Result := XTree_Pointer.Create(Current.Value, FContext, DocPos);
    tkINTEGER: Result := XTree_Int.Create(Current.Value, FContext, DocPos);
    tkFLOAT:   Result := XTree_Float.Create(Current.Value, FContext, DocPos);
    tkCHAR:    Result := XTree_Char.Create(Current.Value, FContext, DocPos);
    tkSTRING:
    begin
      if Pos('$', Current.Value) > 0 then
        Result := ParseInterpolatedString(Current.Value)
      else
        Result := XTree_String.Create(Current.Value, FContext, DocPos);
    end;
  else
    RaiseException(eUnexpected);
  end;
  Next();
end;

// -- string interpolation ---------------------------------------------------------------------
// 'foo $x' & 'foo ${x+y}'
function TParser.ParseInterpolatedString(const S: string): XTree_Node;
var
  i, Start:   Int32;
  Parts:      XNodeArray;
  ExprStr:    string;
  ExprNode:   XTree_Node;
  InvokeNode: XTree_Invoke;
  TempTok:    TTokenizer;
  TempParser: TParser;
  _pos:       TDocPos;
begin
  _pos := DocPos;
  SetLength(Parts, 0);
  i     := 1;
  Start := 1;

  while i <= Length(S) do
  begin
    // $$ -> literal $
    if (S[i] = '$') and (i < Length(S)) and (S[i+1] = '$') then
    begin
      if i > Start then
        Parts += XTree_String.Create(Copy(S, Start, i - Start), FContext, _pos);
      Parts += XTree_String.Create('$', FContext, _pos);
      Inc(i, 2);
      Start := i;
      Continue;
    end;

    if S[i] = '$' then
    begin
      // deny none variables EG: `'$100'` so we dont have to escape regular use.
      if (i >= Length(S)) or
         not ((S[i+1] in ['a'..'z','A'..'Z','_']) or (S[i+1] = '{')) then
      begin
        Inc(i);
        continue;
      end;

      // flush literal segment before $
      if i > Start then
        Parts += XTree_String.Create(Copy(S, Start, i - Start), FContext, _pos);

      Inc(i); // skip $

      if (i <= Length(S)) and (S[i] = '{') then
      begin
        // ${expression} form
        Inc(i); // skip {
        Start := i;
        while (i <= Length(S)) and (S[i] <> '}') do Inc(i);
        ExprStr := Copy(S, Start, i - Start);
        Inc(i); // skip }
      end else
      begin
        // $ident form - grab identifier chars only
        Start := i;
        while (i <= Length(S)) and (S[i] in ['a'..'z','A'..'Z','0'..'9','_']) do Inc(i);
        ExprStr := Copy(S, Start, i - Start);
      end;

      if ExprStr = '' then
      begin
        // bare $ with nothing after - treat as literal
        Parts += XTree_String.Create('$', FContext, _pos);
      end else
      begin
        // Parse the expression string directly as an expression, not a full block
        TempTok    := Tokenize('__interp__', ExprStr);
        TempParser := TParser.Create(TempTok, FContext);
        try
          ExprNode := TempParser.ParseExpression(False, False);
        finally
          TempParser.Free;
        end;

        // wrap in .ToStr()
        InvokeNode := XTree_Invoke.Create(
          XTree_Identifier.Create('ToStr', FContext, _pos),
          [], FContext, _pos);
        InvokeNode.SelfExpr := ExprNode;
        Parts += InvokeNode;
      end;

      Start := i;
    end else
      Inc(i);
  end;

  // flush trailing literal segment
  if Start <= Length(S) then
    Parts += XTree_String.Create(Copy(S, Start, Length(S) - Start + 1), FContext, _pos);

  if Length(Parts) = 0 then
    Exit(XTree_String.Create('', FContext, _pos));

  // fold into left-associative + chain
  Result := Parts[0];
  for i := 1 to High(Parts) do
    Result := XTree_BinaryOp.Create(op_ADD, Result, Parts[i], FContext, _pos);
end;

// -- Primary expressions -------------------------------------------------------

function TParser.ParsePrimary(): XTree_Node;
var
  op:            TToken;
  name:          string;
  InitialPos:    Int32;
  prec:          Int32;
  _pos:          TDocPos;
  ExplicitTypes: XTypeArray;
begin
  if IsInsesitive() then SkipNewline;

  if Current.Token in ATOM then
    Result := ParseAtom()
  else if Current.Token = tkKW_INHERITED then
  begin
    Next();
    Consume(tkLPARENTHESES);
    Result := XTree_InheritedCall.Create(
      ParseExpressionList(True, True), FContext, DocPos);
    Consume(tkRPARENTHESES);
  end
  // specialize FuncName<ConcreteType1, ...>
  // Forces specialization of a generic and returns the method pointer.
  else if (Current.Token = tkKW_SPECIALIZE) then
  begin
    _pos := DocPos;
    Consume(tkKW_SPECIALIZE);
    name := ParseNSIdent(True).Value;
    ExplicitTypes := ParseTypeParams_Concrete();
    Result := XTree_Specialize.Create(name, ExplicitTypes, FContext, _pos);
  end
  else if Current.Token = tkIDENT then
    Result := XTree_Identifier.Create(ParseNSIdent(True).Value, FContext, DocPos)
  else if Current.Token = tkLSQUARE then
  begin
    if Peek().Token = tkKW_FOR then
      Result := ParseListComp()
    else
      Result := ParseInitializerList();
  end
  else if Current.Token = tkKW_LAMBDA then
    Result := ParseFunction(True)
  else if Current.Token = tkKW_IF then
    Result := ParseIfExpr()
  else if Current.Token = tkKW_NEW then
  begin
    _pos := DocPos;
    Next();
    Name := ParseNSIdent(True).Value;

    // Optional explicit type params: new TPair<Int, String>(...)
    SetLength(ExplicitTypes, 0);
    if IsGenericCallAhead() then
      ExplicitTypes := ParseTypeParams_Concrete();

    Consume(tkLPARENTHESES);
    Result := XTree_ClassCreate.Create(
      Name, ParseExpressionList(True, True), FContext, _pos);
    XTree_ClassCreate(Result).ExplicitTypeParams := ExplicitTypes;
    Consume(tkRPARENTHESES);
  end
  else if IS_UNARY() then
  begin
    op   := Next(PostInc);
    prec := UnaryPrecedenceMap[op.Token].Prec;
    Result := XTree_UnaryOp.Create(
      AsOperator(op.Token),
      RHSExpr(ParsePrimary(), prec),
      FContext, op.DocPos);
  end
  else if Current.Token = tkLPARENTHESES then
  begin
    Next();
    InitialPos := Self.FPos;
    Result     := Self.ParseDestructureList();
    if Result = nil then
    begin
      Self.FPos := InitialPos;
      SetInsesitive();
      Result := ParseExpression(False);
      Consume(tkRPARENTHESES);
      ResetInsesitive();
    end;
  end else
    Result := nil;
end;

// -- Operator-precedence expression parser ------------------------------------

function TParser.RHSExpr(Left: XTree_Node; leftPrecedence: Int8=0): XTree_Node;
var
  precedence, nextPrecedence: Int8;
  Right: XTree_Node;
  op:    TToken;

  function Merge(OP: EOperator; Left, Right: XTree_Node): XTree_Node;
  var
    exprList: XNodeArray;
    i:        Int32;
    binOp:    EOperator;
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
        RaiseExceptionFmt('Unsupported compound assignment operator: %s',
          [OperatorToStr(OP)]);
      Result := XTree_Assign.Create(
        op_Asgn, Left,
        XTree_BinaryOp.Create(binOp, Left, Right, FContext, DocPos),
        FContext, DocPos);
    end
    else if OP = op_Asgn then
      Result := XTree_Assign.Create(op, Left, Right, FContext, DocPos)
    else if (OP = op_AS) and (Right is XTree_Identifier) then
      Result := XTree_DynCast.Create(Left, Right, FContext, DocPos)
    else if (OP = op_IS) and (Right is XTree_Identifier) then
      Result := XTree_TypeIs.Create(Left, Right, FContext, DocPos)
    else if (OP = op_ISNOT) and (Right is XTree_Identifier) then
      Result := XTree_UnaryOp.Create(
        op_not, XTree_TypeIs.Create(Left, Right, FContext, DocPos),
        FContext, DocPos)
    else
      Result := XTree_BinaryOp.Create(op, Left, Right, FContext, DocPos);
  end;

var
  SpecializeResType: string;
  ExplicitTypeParams: XTypeArray;
  ExplicitTypeName: string;
  ExplicitType: XType;
  k: Int32;
begin
  while True do
  begin
    if IsInsesitive() then SkipNewline;

    // Detect `ident<Type>(` - explicit generic instantiation at callsite.
    // Must be checked BEFORE OperatorPrecedence consumes the '<' as op_LT.
    if (Left is XTree_Identifier) and IsGenericCallAhead() then
    begin
      ExplicitTypeParams := ParseTypeParams_Concrete();
      Consume(tkLPARENTHESES);
      Result := XTree_Invoke.Create(
        Left, ParseExpressionList(True, True), FContext, Left.FDocPos);
      Consume(tkRPARENTHESES);
      XTree_Invoke(Result).ExplicitTypeParams := ExplicitTypeParams;
      Left := Result;
      SetLength(ExplicitTypeParams, 0);
      Continue;
    end;

    // Detect `obj.Method<Type>(` - explicit generic on a field method call.
    if (Left is XTree_Field) and
       (XTree_Field(Left).Right is XTree_Identifier) and
       IsGenericCallAhead() then
    begin
      ExplicitTypeParams := ParseTypeParams_Concrete();
      Consume(tkLPARENTHESES);
      Result := XTree_Invoke.Create(
        XTree_Field(Left).Right,
        ParseExpressionList(True, True), FContext, Left.FDocPos);
      XTree_Invoke(Result).SelfExpr := XTree_Field(Left).Left;
      Consume(tkRPARENTHESES);
      XTree_Invoke(Result).ExplicitTypeParams := ExplicitTypeParams;
      Left := Result;
      SetLength(ExplicitTypeParams, 0);
      Continue;
    end;

    precedence := OperatorPrecedence();
    if precedence < leftPrecedence then Exit(Left);

    op := Next(PostInc);

    if AsOperator(op.Token) = op_DEREF then
    begin
      Left := XTree_UnaryOp.Create(op_DEREF, Left, FContext, op.DocPos);
      Continue;
    end;

    if AsOperator(op.Token) = op_Invoke then
    begin
      if Left is XTree_Field then
      begin
        // a.Method() or a.Len().ToStr() - extract SelfExpr from the field node
        // so the object side goes through PushArgsToStack's temp-spill path
        Result := XTree_Invoke.Create(
          XTree_Field(Left).Right,            // method name identifier
          ParseExpressionList(True, True), FContext, Left.FDocPos);
        XTree_Invoke(Result).SelfExpr := XTree_Field(Left).Left;  // the object
      end
      else
        Result := XTree_Invoke.Create(
          Left, ParseExpressionList(True, True), FContext, Left.FDocPos);

      Consume(tkRPARENTHESES);
      if NextIf(tkCOLON) then
      begin
        SpecializeResType := ParseNSIdent(True).Value;
        XTree_Invoke(Result).SpecializeResType := SpecializeResType;
      end;
      Left := Result;
      SpecializeResType := '';
      Continue;
    end;

    // 'expr as func(T): R' - overload selection by signature
    // Intercept before ParsePrimary since 'func' is a keyword, not an identifier.
    if (AsOperator(op.Token) = op_AS) and
       (Current.Token in [tkKW_FUNC, tkKW_LAMBDA, tkLPARENTHESES]) then
    begin
      Result := XTree_FuncSelect.Create(Left, ParseAddType('', False, False),
                                         FContext, op.DocPos);
      Left := Result;
      Continue;
    end;

    Right := ParsePrimary();
    if Right = nil then FContext.RaiseException(eInvalidExpression, DocPos);

    // Dot operator never recurses right - each segment is just one identifier.
    // The outer loop handles a.B().C() by converting Field+Invoke repeatedly.
    if AsOperator(op.Token) <> op_Dot then
    begin
      nextPrecedence := OperatorPrecedence();
      if precedence < nextPrecedence then
        Right := RHSExpr(Right, precedence + 1)
      else if precedence = nextPrecedence then
        Right := RHSExpr(Right, precedence + OperatorAssoc());
    end;

    Left := Merge(AsOperator(op.Token), Left, Right);
  end;

  Result := Left;
end;

function TParser.ParseExpression(ExpectSeparator: Boolean=True;
                                  PostParse: Boolean=True): XTree_Node;
begin
  Result := ParsePrimary();
  if Result <> nil then Result := RHSExpr(Result);

  if not PostParse then Exit;

  SetInsesitive();
  if NextIf(tkNEWLINE) then
  begin
    ResetInsesitive();
    Exit;
  end;
  ResetInsesitive();

  if ExpectSeparator then ConsumeSeparator();
end;

function TParser.ParseExpressionList(Insensitive: Boolean;
                                      AllowEmpty: Boolean=False): XNodeArray;
var
  expr:    XTree_Node;
  top:     Int32;
  argName: string;
  _pos:    TDocPos;
begin
  Result := nil;
  SetInsesitive(Insensitive);
  top := 0;

  // Empty list (e.g. empty call parens)
  if AllowEmpty and (Current.Token = tkRPARENTHESES) then
  begin
    ResetInsesitive();
    Exit;
  end;

  while True do
  begin
    _pos := DocPos;

    // Bare comma or ')' at start of slot → nil (use default for this position)
    if Current.Token = tkRPARENTHESES then break;

    if Current.Token = tkCOMMA then
    begin
      // Consecutive comma: this slot is skipped (nil = use default)
      SetLength(Result, top + 1);
      Result[top] := nil;
      Inc(top);
      Next(); // consume the comma
      Continue;
    end;

    // pass keyword → explicit default slot
    if Current.Token = tkKW_PASS then
    begin
      Next();
      SetLength(Result, top + 1);
      Result[top] := nil;  // nil = use default
      Inc(top);
    end
    // named arg: IDENT := expr   or   IDENT := pass
    else if (Current.Token = tkIDENT) and (Peek(1).Token = tkASGN) then
    begin
      argName := Current.Value;
      Next(); // consume ident
      Next(); // consume :=
      if Current.Token = tkKW_PASS then
      begin
        Next();
        expr := nil;
      end else
        expr := ParseExpression(False, False);
      SetLength(Result, top + 1);
      Result[top] := XTree_NamedArg.Create(argName, expr, FContext, _pos);
      Inc(top);
    end
    // normal expression
    else
    begin
      expr := ParseExpression(False);
      if expr = nil then
      begin
        if not AllowEmpty then RaiseException(eInvalidExpression);
        break;
      end;
      SetLength(Result, top + 1);
      Result[top] := expr;
      Inc(top);
    end;

    // After each slot: consume comma separator if present, else stop
    if not NextIf(tkCOMMA) then break;
  end;

  ResetInsesitive();
end;

// -- Annotations --------------------------------------------------------------

function TParser.ParseAnnotation(): XTree_ExprList;
var
  AnnotationNode: XTree_Annotation;
begin
  if Current.Token <> tkAT then Exit(nil);

  Result := XTree_ExprList.Create([], FContext, DocPos);
  while Current.Token = tkAT do
  begin
    Next();
    Expect(tkIDENT);
    AnnotationNode            := XTree_Annotation.Create(FContext, DocPos);
    AnnotationNode.Identifier := XTree_Identifier.Create(Current.Value, FContext, DocPos);
    Consume(tkIDENT);

    if Current.Token = tkLPARENTHESES then
    begin
      Next();
      case Current.Token of
        tkSTRING, tkINTEGER, tkFLOAT, tkBOOL:
          AnnotationNode.Value := ParseExpression(False);
      else
        RaiseException(
          'A simple literal (string, number, or boolean) is expected as an annotation argument.');
      end;
      Consume(tkRPARENTHESES);
    end;

    SetLength(Result.List, Length(Result.List) + 1);
    Result.List[High(Result.List)] := AnnotationNode;
    SkipTokens(SEPARATORS);
  end;
end;

// -- ParseStatement ------------------------------------------------------------

function TParser.ParseStatement(): XTree_Node;
var
  Annotation: XTree_ExprList;
  hadDirective: Boolean;
begin
  Result := nil;
  SkipNewline;

  hadDirective := False;
  while Current.Token = tkDIRECTIVE do
  begin
    FContext.ProcessDirective(Current.Value);
    Next();
    hadDirective := True;
  end;

  // Return nil so ParseBlock loops back, calls SkipTokens(SEPARATORS),
  // checks CurrentIndent() <= ParentIndent, and correctly stops.
  if hadDirective then
    Exit(nil);

  Annotation := ParseAnnotation();

  case Current.Token of
    tkKW_IMPORT:   Result := ParseImport();
    tkKW_TYPE:     Result := ParseTypeDecl();
    tkKW_RAISE:    Result := ParseRaise();
    tkKW_REF:      Result := ParseRefdecl();
    tkKW_VAR:      Result := ParseVardecl();
    tkKW_CONST:    Result := ParseVardecl();
    tkKW_FUNC:     Result := ParseFunction();
    tkKW_PRINT:    Result := ParsePrint();
    tkKW_IF:       Result := ParseIf();
    tkKW_SWITCH:   Result := ParseSwitch();
    tkKW_WHILE:    Result := ParseWhile();
    tkKW_PASS:     Result := ParsePass();
    tkKW_REPEAT:   Result := ParseRepeat();
    tkKW_FOR:      Result := ParseForIn();
    tkKW_RETURN:   Result := ParseReturn();
    tkKW_TRY:      Result := ParseTry();
    tkKW_BREAK:    Result := ParseBreak();
    tkKW_CONTINUE: Result := ParseContinue();
  else
    Result := ParseExpression(False);
  end;

  if (Result is XTree_Annotating) and (Annotation <> nil) then
    XTree_Annotating(Result).Annotations := Annotation
  else if (Result is XTree_GenericFunction) and (Annotation <> nil) then
    XTree_GenericFunction(Result).GenericFunction.Annotations := Annotation
  else if Annotation <> nil then
    RaiseException(Current.Value + ' does not support annotations.');

  SkipTokens(SEPARATORS);
end;

end.
