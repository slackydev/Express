program Main;

{$IFDEF WINDOWS}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$i header.inc}

uses
  SysUtils, Variants,
  xpr.Utils,
  xpr.Types,
  xpr.Express,
  xpr.nativebench;

procedure RunScript(const AFileName: string);
var
  Script: TExpress;
  exec_t: Double;
  Resultvar: Variant;
  StartHeapUsed: SizeInt;
begin
  WriteFancy('--- Running Script: %s ---', [AFileName]);
  StartHeapUsed := GetFPCHeapStatus().CurrHeapUsed;
  WriteLn('Program use so far: ', (GetFPCHeapStatus().CurrHeapUsed - StartHeapUsed) div 1024,' KB');

  Script := TExpress.Create;

  // --- COMPILATION ---
  WriteFancy('Compiling...');
  Script.CompileFile(AFileName);

  WriteFancy(Script.BC.ToString(True));

  // After compiling, read the stats from the properties.
  WriteFancy('Parsed source in %.3f ms', [Script.ParseTimeMs]);
  WriteFancy('Compiled AST in %.3f ms', [Script.ASTCompileTimeMs]);
  WriteFancy('Emitted Bytecode in %.3f ms', [Script.BytecodeEmitTimeMs]);
  WriteFancy('Total compile time: %.3f ms', [Script.TotalCompileTimeMs]);
  WriteFancy('Memory used for compilation: %.4f MB', [Script.CompileMemoryUsedMb]);
  WriteLn;

  // --- EXECUTION ---
  WriteLn('Executing...');
  exec_t := MarkTime();

  Script.Run();

  exec_t := MarkTime() - exec_t;
  WriteFancy('Executed in %.3f ms', [exec_t]);
  WriteFancy('Memory spilled in execution: %d bytes', [Script.MemorySpilled]);

  // --- Example of getting a result back ---
  resultVar := Script.GetVar('a');
  if not VarIsNull(resultVar) then
    WriteFancy('Script returned ''a'': %s', [string(resultVar)]);

  resultVar := Script.GetVar('b');
  if not VarIsNull(resultVar) then
    WriteFancy('Script returned ''b'': %s', [string(resultVar)]);

  Script.Free;


  WriteLn('Program leaked: ', (GetFPCHeapStatus().CurrHeapUsed - StartHeapUsed) div 1024,' KB');

  XprNativeBenchmark.Pidigits();
end;

var
  fileName:string;
begin
  FormatSettings.DecimalSeparator := '.';
  FormatSettings.ThousandSeparator := ',';

  WriteFancy('Express Host ' + {$I %Date%} + ' ' + {$I %Time%});
  WriteFancy('-----------------------------------');

  fileName := 'tests/refcount.xpr';
  //fileName := 'tests/scimark.xpr';

  if ParamCount > 0 then
    fileName := ParamStr(1);

  RunScript(fileName);


  WriteFancy('');
  WriteFancy('Press enter to exit...');
  ReadLn;
end.
