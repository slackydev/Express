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
  PascalKeywordMap.Add('operator',  tkKW_OPERATOR);
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

operator + (Left, Right: TStringArray): TStringArray;
var i: Int32;
begin
  SetLength(Result, Length(Left)+Length(Right));
  for i:=0 to High(Left) do Result[i] := Left[i];
  for i:=0 to High(Right) do Result[Length(Left)+i] := Right[i];
end;

function TokenizePascal(filename, script: string): TTokenizer;
type
  TMacroDef = record
    Name, Value: string;
  end;
var
  c: Char;
  startPos, P, P2: Int32;
  identStr, dir, arg: string;
  tok: ETokenKind;
  Defines: TStringArray;
  Macros: array of TMacroDef;
  IfdefDepth, SkipDepth: Int32;
  MacName, MacVal: string;

  function IsDefined(const Name: string): Boolean;
  var i: Int32;
  begin
    Result := False;
    for i := 0 to High(Defines) do
      if XprCase(Defines[i]) = XprCase(Name) then Exit(True);
  end;

  procedure DefineMacro(const AName, AValue: string);
  var i: Int32;
  begin
    for i := 0 to High(Macros) do
      if XprCase(Macros[i].Name) = XprCase(AName) then
      begin
        Macros[i].Value := AValue;
        Exit;
      end;
    SetLength(Macros, Length(Macros) + 1);
    Macros[High(Macros)].Name := AName;
    Macros[High(Macros)].Value := AValue;
  end;

  function GetMacro(const AName: string): string;
  var i: Int32;
  begin
    Result := '';
    for i := 0 to High(Macros) do
      if XprCase(Macros[i].Name) = XprCase(AName) then
        Exit(Macros[i].Value);
  end;

begin
  Result.Data     := script + #0#0#0;
  Result.Pos      := 1;
  Result.LineStart := 1;
  Result.DocPos.Document := filename;
  Result.DocPos.Line     := 1;

  SetLength(Result.Tokens, 1);
  Result.FArrHigh := 0;

  if PascalKeywordMap = nil then InitPascalKeywords();

  // -- Pre-processor Default Directives ---------------------------------------
  Defines := ['EXPRESS'];                                                    // We are express
  {$IFDEF AARCH32}  Defines := Defines + ['ARM32', 'AARCH32']; {$ENDIF}      // ARM-32
  {$IFDEF AARCH64}  Defines := Defines + ['ARM64', 'AARCH64']; {$ENDIF}      // ARM-64
  {$IFDEF CPU386}   Defines := Defines + ['CPU386']; {$ENDIF}                // x86
  {$IFDEF CPUX64}   Defines := Defines + ['CPUX64']; {$ENDIF}                // x86-64
  {$IFDEF CPU64}    Defines := Defines + ['CPU64'];  {$ENDIF}                // 64bit (generic)
  {$IFDEF CPU32}    Defines := Defines + ['CPU32'];  {$ENDIF}                // 32bit (generic)
  {$IFDEF MSWINDOWS}Defines := Defines + ['MSWINDOWS', 'WINDOWS']; {$ENDIF}  // OS windows
  {$IFDEF UNIX}     Defines := Defines + ['UNIX']; {$ENDIF}                  // OS generic unix
  {$IFDEF LINUX}    Defines := Defines + ['LINUX']; {$ENDIF}                 // Linux
  {$IFDEF DARWIN}   Defines := Defines + ['DARWIN', 'MACOS']; {$ENDIF}       // MACOS
  DefineMacro('SIMBACOMMIT','''f79e177''');

  IfdefDepth := 0;
  SkipDepth  := 0;

  while Result.Data[Result.Pos] <> #0 do
  begin
    Result.DocPos.Column := Result.Pos - Result.LineStart;
    c := Result.Current;

    // -- newlines --
    if (c = #13) or (c = #10) then
    begin
      if (c = #13) and (Result.Peek(1) = #10) then Inc(Result.Pos);
      if SkipDepth = 0 then Result.AppendInc(tkNEWLINE, '', 1)
      else Inc(Result.Pos);
      Inc(Result.DocPos.Line);
      Result.LineStart := Result.Pos;
      Continue;
    end;

    // -- comments & conditional preprocessor directives --
        if c = '{' then
    begin
      if Result.Test('{$') then
      begin
        startPos := Result.Pos + 2; // skip {$
        Inc(Result.Pos, 2);
        while (Result.Data[Result.Pos] <> '}') and (Result.Data[Result.Pos] <> #0) do
          Inc(Result.Pos);
        identStr := Copy(Result.Data, startPos, Result.Pos - startPos);
        if Result.Data[Result.Pos] = '}' then Inc(Result.Pos);

        dir := Trim(identStr);
        P := Pos(' ', dir);
        if P > 0 then
        begin
          arg := Trim(Copy(dir, P + 1, Length(dir)));
          dir := XprCase(Trim(Copy(dir, 1, P - 1)));
        end else
        begin
          dir := XprCase(dir);
          arg := '';
        end;

        if dir = 'ifdef' then
        begin
          Inc(IfdefDepth);
          if (SkipDepth = 0) and not IsDefined(arg) then SkipDepth := IfdefDepth;
          Continue;
        end
        else if dir = 'ifndef' then
        begin
          Inc(IfdefDepth);
          if (SkipDepth = 0) and IsDefined(arg) then SkipDepth := IfdefDepth;
          Continue;
        end
        else if dir = 'else' then
        begin
          // If we were skipping because of THIS level, start executing.
          if SkipDepth = IfdefDepth then SkipDepth := 0
          // If we were executing, we now need to skip.
          else if SkipDepth = 0 then SkipDepth := IfdefDepth;
          Continue;
        end
        else if dir = 'endif' then
        begin
          if SkipDepth = IfdefDepth then SkipDepth := 0;
          if IfdefDepth > 0 then Dec(IfdefDepth);
          Continue;
        end
        else if dir = 'define' then
        begin
          if SkipDepth = 0 then
          begin
            P2 := Pos(':=', arg);
            if P2 > 0 then
            begin
              MacName := Trim(Copy(arg, 1, P2 - 1));
              MacVal  := Trim(Copy(arg, P2 + 2, Length(arg)));
              DefineMacro(MacName, MacVal);
              if not IsDefined(MacName) then Defines := Defines + [MacName];
            end
            else
            begin
              if not IsDefined(arg) then Defines := Defines + [arg];
            end;
          end;
          Continue;
        end
        else if dir = 'undef' then
        begin
          if SkipDepth = 0 then
          begin
            for P := 0 to High(Defines) do
              if XprCase(Defines[P]) = XprCase(arg) then
              begin
                Defines[P] := Defines[High(Defines)];
                SetLength(Defines, Length(Defines) - 1);
                Break;
              end;
            for P := 0 to High(Macros) do
              if XprCase(Macros[P].Name) = XprCase(arg) then
              begin
                Macros[P] := Macros[High(Macros)];
                SetLength(Macros, Length(Macros) - 1);
                Break;
              end;
          end;
          Continue;
        end
        else if dir = 'macro' then
        begin
          if SkipDepth = 0 then
          begin
            MacVal := GetMacro(arg);
            if MacVal <> '' then
            begin
              // Instantly materialize the macro value into the parsing stream
              System.Insert(MacVal, Result.Data, Result.Pos);
            end;
          end;
          Continue;
        end;

        // Regular compiler directives (e.g. {$I file.pas} or {$rangechecks on})
        if SkipDepth = 0 then
          Result.Append(tkDIRECTIVE, identStr);
        Continue;
      end
      else
      begin
        Result.HandleComment();
        Continue;
      end;
    end;

    if (c = '(') and Result.Test('(*') then
    begin
      Result.HandleComment();
      Continue;
    end;

    if (c = '/') and Result.Test('//') then
    begin
      Result.HandleComment();
      Continue;
    end;

    // -- EXCLUDED BLOCK SKIPPER --
    if SkipDepth > 0 then
    begin
      Inc(Result.Pos);
      Continue;
    end;

    // -- standard token evaluation --
    case c of
      #1..#9, #11..#12, #14..#32:
        Result.Next();

      // -- punctuation --
      ';': Result.AppendInc(tkSEMI,    ';', 1);
      ',': Result.AppendInc(tkCOMMA,   ',', 1);
      '^': Result.AppendInc(tkDEREF,   '^', 1);
      '@': Result.AppendInc(tkAT,      '@', 1);

      '(': Result.AppendInc(tkLPARENTHESES, '(', 1);
      ')': Result.AppendInc(tkRPARENTHESES, ')', 1);
      '[': Result.AppendInc(tkLSQUARE, '[', 1);
      ']': Result.AppendInc(tkRSQUARE, ']', 1);

      // -- arithmetic --
      '+':
        if Result.Test('+=') then
          Result.AppendInc(tkPLUS_ASGN, '+=', 2)
        else
          Result.AppendInc(tkPLUS,  '+', 1);
      '-':
        if Result.Test('-=') then
          Result.AppendInc(tkMINUS_ASGN, '-=', 2)
        else
          Result.AppendInc(tkMINUS, '-', 1);
      '*':
        if Result.Test('*=') then
          Result.AppendInc(tkMUL_ASGN, '*=', 2)
        else
        if Result.Test('**') then Result.AppendInc(tkPOW, '**', 2)
        else Result.AppendInc(tkMUL, '*', 1);
      '/':
        if Result.Test('/=') then
          Result.AppendInc(tkDIV_ASGN, '/=', 2)
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
