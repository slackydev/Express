program test_softstop;

{$i header.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  xpr.Utils,
  xpr.Types,
  xpr.Interpreter,
  xpr.Express;


type
  TRunnerData = record
    Script:   TExpress;
    ExitCode: Byte;      
    Elapsed:  Int64;        
  end;
  PRunnerData = ^TRunnerData;

function RunnerThread(Data: Pointer): PtrInt;
var
  RD:    PRunnerData;
  T0:    QWord;
begin
  RD := PRunnerData(Data);
  T0 := GetTickCount64();
  try
    RD^.Script.Run();
  except
    on E: Exception do
    begin
      WriteLn('[runner] exception escaped Run(): ', E.Message);
      DumpExceptionBacktrace(Output);
    end;
  end;

  RD^.Elapsed  := Int64(GetTickCount64() - T0);

  RD^.ExitCode := RD^.Script.VM.RunCode;

  Result := 0;
end;

// ---------------------------------------------------------------------------
// Human-readable name for a VM state byte.
// ---------------------------------------------------------------------------
function RunCodeName(Code: Byte): string;
begin
  case Code of
    VM_RUNNING   : Result := 'VM_RUNNING';
    VM_EXCEPTION : Result := 'VM_EXCEPTION';
    VM_SOFT_STOP : Result := 'VM_SOFT_STOP';
    VM_HALTED    : Result := 'VM_HALTED';
  else
    Result := 'UNKNOWN(' + IntToStr(Code) + ')';
  end;
end;

// ---------------------------------------------------------------------------
const
  SOFT_STOP_DELAY_MS = 2000;

var
  Script:     TExpress;
  RD:         TRunnerData;
  Handle:     TThreadID;
  T0:         QWord;
  StopTarget: PByte; 
begin
  FormatSettings.DecimalSeparator  := '.';
  FormatSettings.ThousandSeparator := ',';

  WriteLn('=== SoftStop Harness ===');
  WriteLn('Script : /tests/test_softstop.xpr');
  WriteLn('Delay  : ', SOFT_STOP_DELAY_MS, ' ms');
  WriteLn;


  Script := TExpress.Create();
  try
    Script.Context.LibrarySearchPaths.Add('../../');

    Write('Compiling... ');
    try
      Script.CompileFile('../test_softstop.xpr');
    except
      on E: Exception do
      begin
        WriteLn('FAILED');
        WriteLn(E.Message);
        ExitCode := 1;
        Exit;
      end;
    end;
    WriteLn('OK');

    StopTarget := @Script.VM.RunCode;


    RD.Script   := Script;
    RD.ExitCode := 0;
    RD.Elapsed  := 0;

    WriteLn('Spawning runner thread...');
    T0 := GetTickCount64();
    Handle := BeginThread(@RunnerThread, @RD);

    if Handle = 0 then
    begin
      WriteLn('ERROR: BeginThread failed');
      ExitCode := 1;
      Exit;
    end;


    WriteLn('Main thread sleeping for ', SOFT_STOP_DELAY_MS, ' ms...');
    Sleep(SOFT_STOP_DELAY_MS);

    WriteLn('Signalling VM_SOFT_STOP...');
    StopTarget^ := VM_SOFT_STOP;  


    WriteLn('Waiting for runner to exit...');
    WaitForThreadTerminate(Handle, 10000 {ms timeout});
    CloseThread(Handle);


    WriteLn;
    WriteLn('=== Results ===');
    WriteLn('Total wall-clock ms : ', Int64(GetTickCount64() - T0));
    WriteLn('Script wall-clock ms: ', RD.Elapsed);
    WriteLn('Final RunCode       : ', RunCodeName(RD.ExitCode), ' (', RD.ExitCode, ')');
    WriteLn;

    if RD.ExitCode = VM_HALTED then
      WriteLn('PASS - VM exited cleanly after soft-stop signal.')
    else if RD.ExitCode = VM_SOFT_STOP then
    begin
      WriteLn('WARN - VM returned with RunCode still = VM_SOFT_STOP.');
      WriteLn('       RunSafe should have flipped it to VM_HALTED on exit.');
      ExitCode := 1;
    end else
    begin
      WriteLn('FAIL - Unexpected final state: ', RunCodeName(RD.ExitCode));
      ExitCode := 1;
    end;

  finally
    Script.Free();
  end;

 WriteFancy('Memory spilled in execution: %d bytes', [Script.MemorySpilled]);
 ReadLn;
end.
