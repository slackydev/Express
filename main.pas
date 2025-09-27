program Main;

{$IFDEF WINDOWS}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$i header.inc}

uses
  SysUtils, Variants,
  xpr.Utils,
  xpr.Types,
  xpr.Express;

procedure RunScript(const AFileName: string);
var
  Script: TExpress;
  exec_t: Double;
  Resultvar: Variant;
begin
  WriteFancy('--- Running Script: %s ---', [AFileName]);

  Script := TExpress.Create;
  try
    // --- COMPILATION ---
    WriteFancy('Compiling...');
    Script.CompileFile(AFileName);

    WriteFancy(Script.BC.ToString(True));

    // After compiling, read the stats from the properties.
    WriteFancy('Parsed source in %.3f ms', [Script.ParseTimeMs]);
    WriteFancy('Compiled AST in %.3f ms', [Script.ASTCompileTimeMs]);
    WriteFancy('Emitted Bytecode in %.3f ms', [Script.BytecodeEmitTimeMs]);
    WriteFancy('Total compile time: %.3f ms', [Script.TotalCompileTimeMs]);
    WriteFancy('Memory used for compilation: %.4f mb', [Script.CompileMemoryUsedMb]);
    WriteLn;

    // --- EXECUTION ---
    WriteLn('Executing...');
    exec_t := MarkTime();

    Script.Run();

    exec_t := MarkTime() - exec_t;
    WriteFancy('Executed in %.3f ms', [exec_t]);
    WriteFancy('Memory used for execution: %.4f mb', [Script.RunMemorySpilled]);

    // --- Example of getting a result back ---
    resultVar := Script.GetVar('Result');
    if not VarIsNull(resultVar) then
      WriteFancy('Script returned ''Result'': %s', [string(resultVar)]);

  finally
    Script.Free;
  end;
end;

var
  fileName:string;
begin
  FormatSettings.DecimalSeparator := '.';
  FormatSettings.ThousandSeparator := ',';

  WriteFancy('Express Host ' + {$I %Date%} + ' ' + {$I %Time%});
  WriteFancy('-----------------------------------');

  try
    fileName := 'test.xpr';
    if ParamCount > 0 then
      fileName := ParamStr(1);

    RunScript(fileName);

  except
    on E: EExpressError do
    begin
      WriteLn(Format('SCRIPT ERROR:'#10'  Message: %s', [E.Message]));
      if E.DocPos.Line > -1 then
        WriteLn(Format('  Location: %s (Line: %d, Col: %d)', [E.DocPos.Document, E.DocPos.Line, E.DocPos.Column]));
      if E.StackTrace <> '' then
        WriteLn(#10 + E.StackTrace);
    end;
    on E: Exception do
      WriteLn('HOST ERROR: ' + E.ClassName + #10'  Message: ' + E.Message);
  end;

  WriteFancy('');
  WriteFancy('Press enter to exit...');
  ReadLn;
end.
