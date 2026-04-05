program runtests;

{$i header.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  xpr.Utils,
  xpr.Types,
  xpr.Express;

var
  Script: TExpress;
begin
  FormatSettings.DecimalSeparator := '.';
  FormatSettings.ThousandSeparator := ',';

  Script := TExpress.Create();
  Script.Context.LibrarySearchPaths.Add('../../');
  Script.CompileFile('../run_tests.xpr');
  try
    Script.Run();
  except
    on E: Exception do
    begin
      WriteLn(E.ToString);
      DumpExceptionBacktrace(Output);
      ExitCode := 1;
    end;
  end;
  Script.Free();
end.
