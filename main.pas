program Main;

{$IFDEF WINDOWS}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$i header.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Variants,
  xpr.Utils,
  xpr.Types,
  xpr.Express,
  xpr.nativebench
  {$IFDEF WINDOWS}
  ,Windows  // UTF-8
  {$ENDIF};

const
  TrackMemoryAllocCount = True;

var
  MemTracking: record
    Enabled: Boolean;
    Count: Int64;
    MemManager: TMemoryManager;
    DefaultMemManager: TMemoryManager;
  end;

function MyAllocMem(Size:ptruint):Pointer;
begin
  if MemTracking.Enabled then
    Inc(MemTracking.Count);
  Result := MemTracking.DefaultMemManager.AllocMem(Size);
end;

function MyGetMem(Size:ptruint):Pointer;
begin
  if MemTracking.Enabled then
    Inc(MemTracking.Count);
  Result := MemTracking.DefaultMemManager.GetMem(Size);
end;

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

  MemTracking.Enabled := True;
  MemTracking.Count := 0;
  Script.CompileFile(AFileName);
  MemTracking.Enabled := False;

  WriteFancy(Script.BC.ToString(True));

  // After compiling, read the stats from the properties.
  WriteFancy('Parsed source in %.3f ms', [Script.ParseTimeMs]);
  WriteFancy('Compiled AST in %.3f ms', [Script.ASTCompileTimeMs]);
  WriteFancy('Emitted Bytecode in %.3f ms', [Script.BytecodeEmitTimeMs]);
  WriteFancy('Total compile time: %.3f ms', [Script.TotalCompileTimeMs]);
  WriteFancy('Memory used for compilation: %.4f MB', [Script.CompileMemoryUsedMb]);
  if TrackMemoryAllocCount then
    WriteFancy('Memory allocation calls for complilation: %d', [MemTracking.Count]);

  WriteLn;

  // --- EXECUTION ---
  WriteLn('Executing...');

  MemTracking.Count := 0;
  MemTracking.Enabled := True;

  exec_t := MarkTime();
  Script.Run();
  exec_t := MarkTime() - exec_t;

  MemTracking.Enabled := True;

  WriteFancy('Executed in %.3f ms', [exec_t]);
  WriteFancy('Memory spilled in execution: %d bytes', [Script.MemorySpilled]);
  if TrackMemoryAllocCount then
    WriteFancy('Memory allocation calls in execution: %d', [MemTracking.Count]);

  // --- Example of getting a result back ---
  //resultVar := Script.GetVar('t');
  //if not VarIsNull(resultVar) then
  //  WriteFancy('Script returned ''t'': %s ms', [string(resultVar)]);
  Script.Free;

  WriteLn('Program holds: ', (GetFPCHeapStatus().CurrHeapUsed - StartHeapUsed) div 1024,' KB [note may be FPC that retians - inaccurate]');
  WriteLn();
  WriteLn('Native: ');
  //XprNativeBenchmark.ShellSort();
  //XprNativeBenchmark.SplitTPA();
  //XprNativeBenchmark.Pidigits();
  //XprNativeBenchmark.DotProduct();
  //XprNativeBenchmark.Scimark();
end;

procedure RunPascalScript();
var
  Script: TExpress;
begin
  Script := TExpress.Create;
  try
    Script.RunFile('tests/shellsort.pas');
  finally
    Script.Free;
  end;
end;

procedure RunTests();
var
  fileName:string;
begin
  WriteFancy('Express Host ' + {$I %Date%} + ' ' + {$I %Time%});
  WriteFancy('-----------------------------------');

  fileName := 'tests/run_tests.xpr';

  if ParamCount > 0 then
    fileName := ParamStr(1);

  RunScript(fileName);

  WriteFancy('');
  WriteFancy('Press enter to exit...');
  ReadLn;
end;

begin
  if TrackMemoryAllocCount then
  begin
    MemTracking.Count := 0;
    MemTracking.Enabled := False;
    GetMemoryManager(MemTracking.DefaultMemManager);
    MemTracking.MemManager := MemTracking.DefaultMemManager;
    MemTracking.MemManager.AllocMem := @MyAllocMem;
    MemTracking.MemManager.GetMem := @MyGetMem;
    SetMemoryManager(MemTracking.MemManager);
  end;

  {$IFDEF WINDOWS}
  SetConsoleOutputCP(CP_WINUNICODE);  // UTF-8
  {$ENDIF}

  FormatSettings.DecimalSeparator := '.';
  FormatSettings.ThousandSeparator := ',';

  if (ParamCount() > 0) and (ParamStr(1) = '-p') then
    RunPascalScript()
  else
    RunTests();
end.
