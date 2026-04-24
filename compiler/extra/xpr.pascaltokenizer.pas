unit xpr.PascalTokenizer;
{
  Author: Jarl K. Holta (Pascal/Lape compatibility layer)
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
{$I header.inc}
{$hints off}

interface

uses
  Classes, SysUtils,
  xpr.Tokenizer,
  xpr.Dictionary,
  xpr.Utils,
  xpr.Types;

function TokenizePascal(filename, script: string): TTokenizer;

implementation

var
  PascalKeywordMap: TKeywordMap;

procedure InitPascalKeywords;
begin
  PascalKeywordMap := TKeywordMap.Create(@HashStr);

  // -- OPERATORS --------------------------------------------------------------
  PascalKeywordMap.Add('and',    tkAND);
  PascalKeywordMap.Add('div',    tkDIV);
  PascalKeywordMap.Add('in',     tkIN);
  PascalKeywordMap.Add('is',     tkKW_IS);
  PascalKeywordMap.Add('mod',    tkMOD);
  PascalKeywordMap.Add('not',    tkNOT);
  PascalKeywordMap.Add('or',     tkOR);
  PascalKeywordMap.Add('shl',    tkSHL);
  PascalKeywordMap.Add('shr',    tkSHR);
  PascalKeywordMap.Add('xor',    tkXOR);

  // -- STRUCTURAL KEYWORDS ----------------------------------------------------
  PascalKeywordMap.Add('array',     tkKW_ARRAY);
  PascalKeywordMap.Add('begin',     tkKW_BEGIN);
  PascalKeywordMap.Add('break',     tkKW_BREAK);
  PascalKeywordMap.Add('case',      tkKW_CASE);
  PascalKeywordMap.Add('class',     tkKW_CLASS);
  PascalKeywordMap.Add('const',     tkKW_CONST);
  PascalKeywordMap.Add('constructor', tkKW_CONSTRUCTOR);
  PascalKeywordMap.Add('continue',  tkKW_CONTINUE);
  PascalKeywordMap.Add('destructor',tkKW_FUNC);    // treated as a procedure
  PascalKeywordMap.Add('do',        tkKW_DO);
  PascalKeywordMap.Add('downto',    tkKW_DOWNTO);
  PascalKeywordMap.Add('else',      tkKW_ELSE);
  PascalKeywordMap.Add('end',       tkKW_END);
  PascalKeywordMap.Add('except',    tkKW_EXCEPT);
  PascalKeywordMap.Add('finally',   tkKW_FINALLY);
  PascalKeywordMap.Add('for',       tkKW_FOR);
  PascalKeywordMap.Add('function',  tkKW_FUNC);
  PascalKeywordMap.Add('generic',   tkKW_GENERIC);
  PascalKeywordMap.Add('if',        tkKW_IF);
  PascalKeywordMap.Add('inherited', tkKW_INHERITED);
  PascalKeywordMap.Add('object',    tkKW_CLASS);   // legacy 'object' = class
  PascalKeywordMap.Add('of',        tkKW_OF);
  PascalKeywordMap.Add('on',        tkKW_ON);
  PascalKeywordMap.Add('overload',  tkKW_OVERLOAD);
  PascalKeywordMap.Add('override',  tkKW_OVERRIDE);
  PascalKeywordMap.Add('packed',    tkKW_PACKED);
  PascalKeywordMap.Add('procedure', tkKW_FUNC);
  PascalKeywordMap.Add('program',   tkKW_PROGRAM);
  PascalKeywordMap.Add('raise',     tkKW_RAISE);
  PascalKeywordMap.Add('record',    tkKW_RECORD);
  PascalKeywordMap.Add('repeat',    tkKW_REPEAT);
  PascalKeywordMap.Add('specialize',tkKW_SPECIALIZE);
  PascalKeywordMap.Add('then',      tkKW_THEN);
  PascalKeywordMap.Add('to',        tkKW_TO);
  PascalKeywordMap.Add('try',       tkKW_TRY);
  PascalKeywordMap.Add('type',      tkKW_TYPE);
  PascalKeywordMap.Add('until',     tkKW_UNTIL);
  PascalKeywordMap.Add('var',       tkKW_VAR);
  PascalKeywordMap.Add('while',     tkKW_WHILE);
  PascalKeywordMap.Add('with',      tkKW_WITH);

  // -- MAGIC BUILT-INS --------------------------------------------------------
  PascalKeywordMap.Add('writeln',   tkKW_PRINT);
  PascalKeywordMap.Add('write',     tkKW_PRINT);

  // -- CONSTANTS --------------------------------------------------------------
  PascalKeywordMap.Add('nil',   tkKW_NIL);
  PascalKeywordMap.Add('true',  tkBOOL);
  PascalKeywordMap.Add('false', tkBOOL);
end;

function TokenizePascal(filename, script: string): TTokenizer;
var
  c: Char;
  startPos: Int32;
  identStr: string;
  tok: ETokenKind;
begin
  Result.Data     := script + #0#0#0;
  Result.Pos      := 1;
  Result.LineStart := 1;
  Result.DocPos.Document := filename;
  Result.DocPos.Line     := 1;

  SetLength(Result.Tokens, 1);
  Result.FArrHigh := 0;

  if PascalKeywordMap = nil then InitPascalKeywords();

  while Result.Data[Result.Pos] <> #0 do
  begin
    Result.DocPos.Column := Result.Pos - Result.LineStart;
    c := Result.Current;

    case c of
      // -- newlines --
      #13, #10:
        begin
          if (c = #13) and (Result.Peek(1) = #10) then Inc(Result.Pos);
          Result.AppendInc(tkNEWLINE, '', 1);
          Inc(Result.DocPos.Line);
          Result.LineStart := Result.Pos;
        end;
      #1..#9, #11..#12, #14..#32:
        Result.Next();

      // -- punctuation --
      ';': Result.AppendInc(tkSEMI,    ';', 1);
      ',': Result.AppendInc(tkCOMMA,   ',', 1);
      '^': Result.AppendInc(tkDEREF,   '^', 1);
      '@': Result.AppendInc(tkAT,      '@', 1);

      '(':
        if Result.Test('(*') then Result.HandleComment()
        else Result.AppendInc(tkLPARENTHESES, '(', 1);
      ')': Result.AppendInc(tkRPARENTHESES, ')', 1);
      '[': Result.AppendInc(tkLSQUARE, '[', 1);
      ']': Result.AppendInc(tkRSQUARE, ']', 1);

      // -- arithmetic --
      '+': Result.AppendInc(tkPLUS,  '+', 1);
      '-': Result.AppendInc(tkMINUS, '-', 1);
      '*':
        if Result.Test('**') then Result.AppendInc(tkPOW, '**', 2)
        else Result.AppendInc(tkMUL, '*', 1);
      '/':
        if Result.Test('//') then Result.HandleComment()
        else Result.AppendInc(tkDIV, '/', 1);

      // -- dots --
      '.':
        if Result.Test('..') then Result.AppendInc(tkDOTDOT, '..', 2)
        else Result.AppendInc(tkDOT, '.', 1);

      // -- colon / assignment --
      ':':
        if Result.Test(':=') then Result.AppendInc(tkASGN,  ':=', 2)
        else Result.AppendInc(tkCOLON, ':', 1);

      // -- comparisons --
      '=': Result.AppendInc(tkEQ, '=', 1);

      '<':
        if Result.Test('<=') then Result.AppendInc(tkLTE, '<=', 2)
        else if Result.Test('<>') then Result.AppendInc(tkNE, '<>', 2)
        else Result.AppendInc(tkLT, '<', 1);

      '>':
        if Result.Test('>=') then Result.AppendInc(tkGTE, '>=', 2)
        else Result.AppendInc(tkGT, '>', 1);

      // -- comments & directives --
      '{':
        if Result.Test('{$') then Result.HandleDirective()
        else Result.HandleComment();

      // -- string literals --
      '''':
        begin
          startPos := Result.Pos;
          Inc(Result.Pos);
          while (Result.Current <> #0) do
          begin
            if Result.Current = '''' then
              if Result.Peek(1) = '''' then Inc(Result.Pos) else Break;
            Result.Next_CheckNewline();
          end;
          identStr := Copy(Result.Data, startPos + 1, Result.Pos - startPos - 1);
          identStr := StringReplace(identStr, '''''', '''', [rfReplaceAll]);
          Result.Append(tkSTRING, identStr);
          Inc(Result.Pos);
        end;

      // -- numbers --
      '$': Result.AddHexNumber();
      '#': Result.AddChar();
      '0'..'9': Result.AddNumber();

      // -- identifiers & keywords --
      'a'..'z', 'A'..'Z', '_':
        begin
          startPos := Result.Pos;
          while Result.Current in ['a'..'z', 'A'..'Z', '0'..'9', '_'] do
            Inc(Result.Pos);
          identStr := Copy(Result.Data, startPos, Result.Pos - startPos);

          if PascalKeywordMap.Get(XprCase(identStr), tok) then
            Result.Append(tok, identStr)
          else
            Result.Append(tkIDENT, identStr);
        end;

      #0: Break;
    else
      raise Exception.Create(
        'Invalid symbol "' + c + '" at line ' + IntToStr(Result.DocPos.Line));
    end;
  end;

  Result.Append(tkUNKNOWN, '');
  SetLength(Result.Tokens, Result.FArrHigh);
end;

end.
