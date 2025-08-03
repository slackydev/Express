unit xpr.Tokenizer;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
  A straight forward tokenizer
}

interface
{$I header.inc}
{$hints off}

uses
  Classes, SysUtils, 
  xpr.Dictionary;

type
  ETokenKind = (
    tkUNKNOWN, tkNEWLINE,

    //keywords
    tkKW_AS,
    tkKW_AT,
    tkKW_ARRAY,
    //tkKW_BEGIN,
    tkKW_BREAK,
    tkKW_CASE,
    tkKW_CONST,
    tkKW_CONTINUE,
    tkKW_CLASS,
    tkKW_DO,
    tkKW_ELSE,
    tkKW_ELIF,
    tkKW_END,
    tkKW_EXCEPT,
    tkKW_FINALLY,
    tkKW_FOR,
    tkKW_FUNC,
    tkKW_IF,
    tkKW_IMPORT,
    tkKW_IN,
    tkKW_IS,
    tkKW_NIL,
    tkKW_OF,
    tkKW_OVERLOAD,
    tkKW_OVERRIDE,
    tkKW_PASS,
    tkKW_PRINT,
    tkKW_RAISE,
    tkKW_RECORD,
    tkKW_REF,
    tkKW_REPEAT,
    tkKW_RETURN,
    tkKW_THEN,
    tkKW_TRY,
    tkKW_TYPE,
    tkKW_UNTIL,
    tkKW_VAR,
    tkKW_WHILE,
    
    //atoms
    tkIDENT, tkCHAR, tkBOOL, tkINTEGER, tkFLOAT, tkSTRING,
    
    //operators
    tkAND, tkAT, tkBND, tkBOR, tkDEREF, tkDIV, tkEQ, tkGT, tkGTE, tkIN,
    tkINV, tkLT, tkLTE, tkMINUS, tkMUL, tkMOD, tkNE, tkNOT, tkOR, tkPLUS, tkPOW,
    tkSAR, tkSHL, tkSHR, tkXOR, tkUMINUS,

    //assignment operators
    tkASGN, tkBND_ASGN, tkBOR_ASGN, tkDIV_ASGN, tkMINUS_ASGN, tkMOD_ASGN,
    tkMUL_ASGN, tkPLUS_ASGN, tkXOR_ASGN, tkSHL_ASGN, tkSHR_ASGN,

    //incdec
    tkPLUS_PLUS, tkMINUS_MINUS,

    //symbols
    tkLPARENTHESES, tkRPARENTHESES, tkLSQUARE, tkRSQUARE, tkLCURLY, tkRCURLY,
    tkSEMI, tkDOT, tkDOTDOT, tkCOMMA, tkCOLON
  );
  TTokenKindSet = set of ETokenKind;

  TDocPos = packed record
    Line, Column: Int32;
    function ToString: string;
  end;
  TDocPosArray = array of TDocPos;

  TToken = record
    Token: ETokenKind;
    Value: string;
    DocPos: TDocPos;
    function ToString: string;
  end;
  TTokenArray = array of TToken;
  

  (*
    Tokenizer, used to turn text into an array of TTokens
  *)
  TTokenizer = record
  private
    FArrHigh: Int32;
  public
    Data: String;
    Pos, LineStart: Int32;
    Tokens: TTokenArray;
    DocPos: TDocPos;

    function Next(): Char; inline;
    function Peek(n:Int32=1): Char; inline;
    function Test(Value: String): Boolean; inline;
    function Current: Char; inline;
    function Prev: Char; inline;
    function Next_CheckNewline: Char; inline;

    procedure Append(token: ETokenKind; value:string=''); inline;
    procedure AppendInc(token: ETokenKind; value:string=''; n:Int32=1); inline;
    procedure Extend(t:TTokenizer); inline;
    procedure AddToken(cases:array of string; token: ETokenKind);
    procedure AddIdent(); inline;
    procedure AddNumber(); inline;
    procedure AddChar(); inline;
    procedure AddString(); inline;
    procedure HandleComment(); inline;
    procedure Tokenize(expr:String);
  end;
  
  (*
    Types needed to store reserved words
  *)
  TKeywordMap = specialize TDictionary<string, ETokenKind>;
  
  TReservedName = record
    Value: string;
    Token: ETokenKind;
  end;

const
    TokenAssignOps = [tkASGN..tkSHR_ASGN];

function Tokenize(script: string): TTokenizer;
function Token(AToken: ETokenKind; AValue:string=''): TToken; inline;
function TokenToString(x: ETokenKind): string; inline;

operator = (L,R:TToken): Boolean; inline;
operator = (L:TToken; R:ETokenKind): Boolean; inline;
operator in (L:ETokenKind; R:array of ETokenKind): Boolean; inline;

function isNoDocPos(Pos: TDocPos): Boolean; inline;

const
  NoDocPos:TDocPos = (Line:-1; Column:-1);
  tkINDEX = tkLSQUARE;

  ReservedWords: array [0..47] of TReservedName = (
      (Value: 'as'; Token: tkKW_AS),
      (Value: 'at'; Token: tkKW_AT),
      (Value: 'array'; Token: tkKW_ARRAY),
      (Value: 'break'; Token: tkKW_BREAK),
      (Value: 'case'; Token: tkKW_CASE),
      (Value: 'const'; Token: tkKW_CONST),
      (Value: 'continue'; Token: tkKW_CONTINUE),
      (Value: 'class'; Token: tkKW_CLASS),
      (Value: 'do'; Token: tkKW_DO),
      (Value: 'else'; Token: tkKW_ELSE),
      (Value: 'elif'; Token: tkKW_ELIF),
      (Value: 'end'; Token: tkKW_END),
      (Value: 'except'; Token: tkKW_EXCEPT),
      (Value: 'finally'; Token: tkKW_FINALLY),
      (Value: 'for'; Token: tkKW_FOR),
      (Value: 'function'; Token: tkKW_FUNC),
      (Value: 'if'; Token: tkKW_IF),
      (Value: 'import'; Token: tkKW_IMPORT),
      (Value: 'in'; Token: tkKW_IN),
      (Value: 'is'; Token: tkKW_IS),
      (Value: 'nil'; Token: tkKW_NIL),
      (Value: 'of'; Token: tkKW_OF),
      (Value: 'overload'; Token: tkKW_OVERLOAD),
      (Value: 'override'; Token: tkKW_OVERRIDE),
      (Value: 'pass'; Token: tkKW_PASS),
      (Value: 'print'; Token: tkKW_PRINT),
      (Value: 'raise'; Token: tkKW_RAISE),
      (Value: 'record'; Token: tkKW_RECORD),
      (Value: 'ref'; Token: tkKW_REF),
      (Value: 'repeat'; Token: tkKW_REPEAT),
      (Value: 'return'; Token: tkKW_RETURN),
      (Value: 'then'; Token: tkKW_THEN),
      (Value: 'try'; Token: tkKW_TRY),
      (Value: 'type'; Token: tkKW_TYPE),
      (Value: 'until'; Token: tkKW_UNTIL),
      (Value: 'var'; Token: tkKW_VAR),
      (Value: 'while'; Token: tkKW_WHILE),

      (Value: 'true';      Token: tkBOOL),
      (Value: 'false';     Token: tkBOOL),

      (Value: 'and';   Token: tkAND),
      (Value: 'or';    Token: tkOR),
      (Value: 'div';   Token: tkDIV),
      (Value: 'shl';   Token: tkSHL),
      (Value: 'shr';   Token: tkSHR),
      (Value: 'sar';   Token: tkSAR),
      (Value: 'xor';   Token: tkXOR),
      (Value: 'in';    Token: tkIN),
      (Value: 'not';   Token: tkNOT)
  );

  TokenString: array[ETokenKind] of string = (
    '', #10#13,
    //keywords
    'as',
    'at',
    'array',
    'break',
    'case',
    'const',
    'continue',
    'class',
    'do',
    'else',
    'elif',
    'end',
    'except',
    'finally',
    'for',
    'function',
    'if',
    'import',
    'in',
    'is',
    'nil',
    'of',
    'overload',
    'override',
    'pass',
    'print',
    'raise',
    'record',
    'ref',
    'repeat',
    'return',
    'then',
    'try',
    'type',
    'until',
    'var',
    'while',

    //atoms
    '<ident>', '<char>', '<bool>', '<int>', '<float>', '<string>',
    //operators
    'and', '@', '&', '|', '^', '/', '=', '>', '>=', 'in',
    '~', '<', '<=', '-', '*', '%', '!=', 'not', 'or', '+', '**',
    'sar', 'shl', 'shr', 'xor','-',
    //assignment operators
    ':=', '&=', '|=', '/=', '-=', '%=', '*=', '+=', '^=', '<<=', '>>=',
    //incdec
    '++', '--',
    //symbols
    '(', ')', '[', ']', '{', '}',
    ';', '.', '..', ',', ':'
  );
  
var
  KeywordMap: TKeywordMap;

implementation

uses
  xpr.Utils, xpr.Types;

function TDocPos.ToString(): string;
begin
  Result := Format('%d:%d', [self.line, self.column]);
end;

function isNoDocPos(Pos: TDocPos): Boolean;
begin
  Result := (Pos.Line = NoDocPos.Line) and (Pos.Column = NoDocPos.Column);
end;


function TTokenizer.Next(): Char;
begin
  Result := data[pos+1];
  Inc(pos);
end;

function TTokenizer.Peek(n:Int32=1): Char;
begin
  Result := data[pos+n];
end;

function TTokenizer.Test(Value: string): Boolean;
begin 
  Result := Copy(data,pos,Length(Value)) = Value;
end;

function TTokenizer.Current: Char;
begin
  Result := data[pos];
end;

function TTokenizer.Prev: Char;
begin
  if pos-1 >= 0 then
    Result := data[pos-1]
  else
    Result := #0;
end;

function TTokenizer.Next_CheckNewline: Char;
begin
  if (Current+Peek() = #13#10) then
  begin
    Inc(DocPos.line);
    Inc(pos);
    lineStart := pos+1;
  end else if (Current = #10) or (Current = #13) then
  begin
    Inc(DocPos.line);
    lineStart := pos+1;
  end;

  Inc(pos);
  Result := data[pos];
end;

procedure TTokenizer.Append(token: ETokenKind; value: string='');
begin
  if FArrHigh >= Length(Tokens) then
    SetLength(Tokens, 2 * Length(Tokens));
  Tokens[FArrHigh].value := value;
  Tokens[FArrHigh].token := token;
  Tokens[FArrHigh].DocPos := DocPos;
  Inc(FArrHigh);
end;

procedure TTokenizer.AppendInc(token: ETokenKind; value: string=''; n:Int32=1);
var i:Int32;
begin
  if FArrHigh >= Length(tokens) then
    SetLength(tokens, 2 * Length(tokens));
  tokens[FArrHigh].value := value;
  tokens[FArrHigh].token := token;
  tokens[FArrHigh].docpos := DocPos;
  Inc(FArrHigh);
  
  for i:=0 to n-1 do
    Next();
end;


procedure TTokenizer.Extend(t:TTokenizer);
var i:Int32;
begin
  for i:=0 to t.FArrHigh-1 do
    self.Append(t.tokens[i].token,t.tokens[i].value);
end;


procedure TTokenizer.AddToken(cases: array of string; token: ETokenKind);
var i:Int32;
begin
  for i:=0 to High(cases) do
    if Slice(data, pos, pos+Length(cases[i])-1) = cases[i] then
    begin
      self.Append(token,cases[i]);
      Inc(pos, Length(cases[i]));
      Exit;
    end;
  raise Exception.Create('Undefined symbol');
end;


procedure TTokenizer.AddIdent();
var
  i: Int32;
  tmp: String;
  tok: ETokenKind;
begin
  i := pos;
  Inc(pos);
  while Current in ['a'..'z','A'..'Z','_','0'..'9'] do Inc(pos);
  tmp := Slice(data, i, pos-1);
  tok := KeywordMap.GetDef(XprCase(tmp), tkIdent);
  self.Append(tok, tmp);
end;


procedure TTokenizer.AddNumber();
var
  i:Int32;
begin
  i := pos;
  Inc(pos);
  while (self.Current in ['0'..'9',#32]) do Inc(pos);

  if self.Current = '.' then
  begin
    Next();
    while self.Current in ['0'..'9',#32] do Inc(pos);
    self.Append(tkFLOAT, StringReplace(Slice(data,i,pos-1), #32, '', [rfReplaceAll]));
  end else
    self.Append(tkINTEGER, StringReplace(Slice(data,i,pos-1), #32, '', [rfReplaceAll]));
end;


procedure TTokenizer.AddChar();
var
  i:Int32;
begin
  i := pos;
  Inc(pos);
  while self.Current in ['0'..'9'] do Inc(pos);
  self.Append(tkCHAR, Chr(StrToInt(Slice(data,i+1,pos-1))));
end;


procedure TTokenizer.AddString();
var
  i:Int32;
  str: string;
begin
  i := pos;
  Inc(pos);
  while (Current <> data[i]) and (Current <> #0) do Next_CheckNewline;
  str := Slice(data,i+1,pos-1);
  if Length(str) <= 1 then
    Self.Append(tkCHAR, str)
  else
    Self.Append(tkSTRING, str);
  Inc(pos);
end;


procedure TTokenizer.HandleComment();
begin
  if data[pos] = '/' then
  begin
    while not(Current in [#10,#13,#0]) do 
      Inc(pos);
  end else
  begin
    Inc(pos);
    while (Current <> #0) and (Current+Peek <> '*)') do
      Next_CheckNewline();
    Inc(pos, 2);
  end;
end;

procedure TTokenizer.Tokenize(expr: String);
begin
  SetLength(tokens, 1);
  FArrHigh := 0;
  data := Expr + #0#0#0;
  pos  := 1;

  //DocPos.filename := '__main__';
  DocPos.Line := 1;
  lineStart   := 1;

  while (data[pos] <> #0) do
  begin
    DocPos.column := Pos - lineStart;
    case data[pos] of
      '\':
        begin
          // Check if the backslash is followed by a newline sequence
          // Escape expression termination!
          if (Peek(1) = #10) or (Peek(1) = #13) then
          begin
            Inc(pos);

            if (Current = #13) and (Peek(1) = #10) then
              Inc(pos, 2)
            else
              Inc(pos);

            Inc(DocPos.line);
            lineStart := pos;

            continue;
          end else
            raise Exception.Create('Invalid symbol "\" at line '+ IntToStr(DocPos.Line));
        end;

      #13, #10:
        begin
          // To handle Windows (\r\n), Unix (\n), and old Mac (\r) newlines robustly:
          if (Current = #13) and (Peek(1) = #10) then
          begin
            Inc(pos);
            continue;
          end;

          // Now we have a standalone \n or \r. Treat it as a single newline token.
          self.AppendInc(tkNewline, '', 1);
          Inc(DocPos.line);
          lineStart := pos;
        end;
      #1..#9: Next;
      #11..#12, #14..#32: Next;
      ';': 
        self.AppendInc(tkSEMI, data[pos], 1);
      '.': 
        if Test('..') then
          self.AppendInc(tkDOTDOT, data[pos]+data[pos+1], 2)
        else
          self.AppendInc(tkDOT, data[pos], 1);
      ',': 
        self.AppendInc(tkCOMMA, data[pos], 1);
      ':':
        if Test(':=') then
          self.AppendInc(tkASGN, data[pos]+data[pos+1], 2)
        else
          self.AppendInc(tkCOLON, data[pos], 1);
      
      (* compare operators *)
      '<':if Test('<=') then
            self.AppendInc(tkLTE, data[pos]+data[pos+1], 2)
          else if Test('<<=') then
            self.AppendInc(tkSHL_ASGN, data[pos]+data[pos+1]+data[pos+2], 3)
          else
            self.AppendInc(tkLT, data[pos], 1);
      '>':if Test('>=') then
            self.AppendInc(tkGTE, data[pos]+data[pos+1], 2)
          else if Test('>>=') then
            self.AppendInc(tkSHR_ASGN, data[pos]+data[pos+1]+data[pos+2], 3)
          else
            self.AppendInc(tkGT, data[pos], 1);
      '!':if Test('!=') then
            self.AppendInc(tkNE, data[pos]+data[pos+1], 2);
        //else
        //  self.AppendInc(tkINV, data[pos], 1);
      '=':
        self.AppendInc(tkEQ, data[pos], 1);

      
      (* bitwise *)
      '&':if Test('&=') then
            self.AppendInc(tkBND_ASGN, data[pos]+data[pos+1], 2)
          else
            self.AppendInc(tkBND, data[pos], 1);
      '|':if Test('|=') then
            self.AppendInc(tkBOR_ASGN, data[pos]+data[pos+1], 2)
          else
            self.AppendInc(tkBOR, data[pos], 1);
      '^':if Test('^=') then
            self.AppendInc(tkXOR_ASGN, data[pos]+data[pos+1], 2)
          else
            self.AppendInc(tkDEREF, data[pos], 1);
      '~':
          self.AppendInc(tkINV, data[pos], 1);
      
      (* sum and factorial, comment.. *)
      '+':if Test('+=') then
            self.AppendInc(tkPLUS_ASGN, data[pos]+data[pos+1], 2)
          else if Test('++') then
            self.AppendInc(tkPLUS_PLUS, data[pos]+data[pos+1], 2)
          else
            self.AppendInc(tkPLUS, data[pos], 1);
      '-':if Test('-=') then
            self.AppendInc(tkMINUS_ASGN, data[pos]+data[pos+1], 2)
          else if Test('--') then
            self.AppendInc(tkMINUS_MINUS, data[pos]+data[pos+1], 2)
          else
            self.AppendInc(tkMINUS, data[pos], 1);
      '*':if Test('*=') then
            self.AppendInc(tkMUL_ASGN, data[pos]+data[pos+1], 2)
          else if Test('**') then
            self.AppendInc(tkPOW, data[pos]+data[pos+1], 2)
          else
            self.AppendInc(tkMUL, data[pos], 1);
      '/':if Test('/=') then
            self.AppendInc(tkDIV_ASGN, data[pos]+data[pos+1], 2)
          else if Test('//') then
            self.HandleComment()
          else
            self.AppendInc(tkDIV, data[pos], 1);
      '%':if Test('%=') then
            self.AppendInc(tkMOD_ASGN, data[pos]+data[pos+1], 2)
          else
            self.AppendInc(tkMOD, data[pos], 1);
      
      (* the rest... *)
      '(':if Test('(*') then
            self.HandleComment()
          else
            self.AppendInc(tkLPARENTHESES, data[pos], 1);
      
      ')': self.AppendInc(tkRPARENTHESES, data[pos], 1);
      '[': self.AppendInc(tkLSQUARE, data[pos], 1);
      ']': self.AppendInc(tkRSQUARE, data[pos], 1);
      '{': self.AppendInc(tkLCURLY,  data[pos], 1);
      '}': self.AppendInc(tkRCURLY,  data[pos], 1);
      '@': self.AppendInc(tkAT, data[pos], 1);
      'a'..'z','A'..'Z','_':
        self.AddIdent();
      '0'..'9':
        self.AddNumber();
      '#':
        self.AddChar();
      #34,#39:
        self.AddString();
      #0:
        break;
      else
        raise Exception.Create('Invalid symbol "'+data[pos]+'" at line '+ IntToStr(DocPos.Line));
    end;
  end;
  
  Self.Append(tkUnknown,'');
  SetLength(tokens, FArrHigh);
end;


function Tokenize(script: string): TTokenizer;
begin
  Result.Pos := 0;
  Result.Tokenize(script);
end;

function TokenToString(x: ETokenKind): string;
begin
  WriteStr(Result, x);
end;

function Token(AToken: ETokenKind; AValue: string=''): TToken;
begin
  Result.Token := AToken;
  if AValue = '' then
    Result.Value := TokenString[AToken]
  else
    Result.Value := AValue;
end;


function TToken.ToString(): String;
begin
  WriteStr(Result, self.token);
  Result += '("'+self.value+'")';
end;

operator = (L, R: TToken): Boolean;
begin
  Result := (L.Value = R.Value) and (R.Token = L.Token);
end;

operator = (L: TToken; R: ETokenKind): Boolean;
begin
  Result := (L.Token = R);
end;

operator in (L:ETokenKind; R:array of ETokenKind): Boolean;
var i:Int32;
begin
  Result := False;
  for i:=0 to High(R) do
    if L = R[i] then Exit(True);
end;

var i:Int32;
initialization
begin
  KeywordMap := TKeywordMap.Create(@HashStr);
  for i:=0 to High(ReservedWords) do
    if ReservedWords[i].Value <> '' then
      KeywordMap.Add(Xprcase(ReservedWords[i].Value), ReservedWords[i].Token);
end;

end.
