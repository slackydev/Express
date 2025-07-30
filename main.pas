program Main;
{$i header.inc}

uses
  Classes, SysUtils, CustApp,
  xprTypes,
  xprTokenizer,
  xprBytecode,
  xprIntermediate,
  xprInterpreter,
  xprInterpreterSuper,
  xprTree,
  xprUtils,
  xprParser,
  xprCompilerContext,
  xprVartypes,
  xprBytecodeEmitter,
  xprTypeIntrinsics;

type
  TExpressTest = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure DoHandleException(Sender:TObject; E:Exception);
  end;


//----------------------------------------------------------------------------\\

{$i scimark}

function DotProductTest: Int64;
var
  A, B: array of Int64;
  i, N: Int64;
  dot: Int64;
  t: Double;
begin

  N := 10000000; // 10 million
  SetLength(A, N);
  SetLength(B, N);

  t := MarkTime();

  // Fill arrays as per original logic
  for i := 0 to N - 1 do
  begin
    A[i] := i mod 1000;
    B[i] := (i * 7) mod 1000;
  end;

  // Dot product
  dot := 0;
  for i := 0 to N - 1 do
    dot := dot + A[i] * B[i];

  Result := dot;
  WriteLn(Format('DotProd in %.3f ms', [MarkTime() - t])+#13#10);
end;

procedure lapeisfast;
var
  a, b, hits, i, n: Int64;
begin
  a := 1594;
  hits := 0;

  n := 10000000;
  for i:=0 to n do
  begin
    b := a;
    b := b + a * (i mod 5);
    b := b - a * (i mod 10);
    if b+i = a then
      Inc(hits);
  end;
end;

procedure ShellShortTest();
var
  arr: array of Int64;
  n, i, j, gap: Int64;
  tmp: Int64;
  before, after: Double;
begin
  n := 1000000;
  SetLength(arr, n);

  for i := 0 to n - 1 do
    arr[i] := Random(1000000);

  before := MarkTime();

  // Compute initial gap
  gap := 1;
  while gap <= (n - 1) div 3 do
    gap := gap * 3 + 1;

  while gap >= 1 do
  begin
    for i := gap to n - 1 do
    begin
      j := i;
      while (j >= gap) and (arr[j] < arr[j - gap]) do
      begin
        tmp := arr[j];
        arr[j] := arr[j - gap];
        arr[j - gap] := tmp;
        j := j - gap;
      end;
    end;
    gap := gap div 3;
  end;

  after := MarkTime();

  WriteLn(Format('ShellSort in %.3f ms', [after-before])+#13#10);
end;




procedure _Inc64(const Params: PParamArray); cdecl;
begin
  Int64(Params^[0]^) := Int64(Params^[0]^) + 1;
end;

procedure _RandInt(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int64(Result^) := Trunc(Int64(Params^[0]^) + Random()*Int64(Params^[1]^));
end;

procedure _GetTickCount(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := MarkTime();
end;

procedure _FreeMem(const Params: PParamArray); cdecl;
begin
  FreeMem(Pointer(Params^[0]^));
end;

procedure _ReallocMem(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  if PtrInt(Params^[0]^) <= 0 then
  begin
    Pointer(Result^) := AllocMem(SizeInt(Params^[1]^));
    Exit;
  end;

  if SizeInt(Params^[1]^) = 0 then
  begin
    FreeMem(Pointer(Params^[0]^));
    Pointer(Params^[0]^) := nil;
    Pointer(Result^) := nil;
  end
  else
  begin
    Pointer(Result^) := ReAllocMem(Pointer(Params^[0]^), SizeInt(Params^[1]^));
  end;
end;

procedure _AllocMem(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Pointer(Result^) := AllocMem(SizeInt(Params^[0]^));
end;


procedure _AllocArray(const Params: PParamArray; const Result: Pointer); cdecl;
const
  ARRAY_HEADER_SIZE = 2*SizeOf(SizeInt);
var
  CurrentRawPtr: Pointer; // The actual raw pointer value passed in Params^[0]
  NewTotalBytes, OldElementCount, ItemSize, OffsetToZero: SizeInt; // The total bytes requested (DataSizeNode)
  BytesToZero, NewElementCount: SizeInt; // The new array length requested (NewLength)
begin

  // Extract parameters from PParamArray.
  // Params^[0]^ contains the value of the 'Raw' pointer from the caller.
  CurrentRawPtr := Pointer(Params^[0]^);
  NewTotalBytes := SizeInt(Params^[1]^);
  NewElementCount := SizeInt(Params^[2]^);
  ItemSize := SizeInt(Params^[3]^);

  if (PtrInt(CurrentRawPtr) <= 0) and (PtrInt(CurrentRawPtr) >= -20) then
  begin
    Pointer(Result^) := AllocMem(NewTotalBytes);                  // Allocate new memory
    PSizeInt(Pointer(Result^))^ := 1;                             // Set refcount to 1
    PSizeInt(PSizeInt(Pointer(Result^))+1)^ := NewElementCount-1; // Set array.high
  end
  else if NewTotalBytes = 0 then
  begin
    FreeMem(CurrentRawPtr);  // Free the existing memory (FPC tracks this)
    Pointer(Result^) := nil;
  end else
  begin
    OldElementCount := PSizeInt(PSizeInt(CurrentRawPtr)+1)^+1;
    if NewElementCount = OldElementCount then // I think this is OK
    begin
      Pointer(Result^) := Pointer(Params^[0]^);
      Exit;
    end;
    Pointer(Result^) := ReAllocMem(CurrentRawPtr, NewTotalBytes); // Reallocate memory

    // Only fill bytes if growing
    if NewElementCount > OldElementCount then
    begin
      OffsetToZero := ARRAY_HEADER_SIZE + (OldElementCount * ItemSize); // Start of new elements
      BytesToZero := (NewElementCount - OldElementCount) * ItemSize;    // Number of bytes for new elements
      FillByte(Pointer(NativeUInt(Result^) + OffsetToZero)^, BytesToZero, 0);
    end;

    PSizeInt(PSizeInt(Pointer(Result^))+1)^ := NewElementCount-1;
  end;
end;


procedure _Sin(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := Sin(Double(Params^[0]^));
end;

procedure _Cos(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := Cos(Double(Params^[0]^));
end;

procedure _Trunc(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int64(Result^) := Trunc(Double(Params^[0]^));
end;

procedure _Round(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int64(Result^) := Round(Double(Params^[0]^));
end;

procedure _Ln(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := Ln(Double(Params^[0]^));
end;

procedure _Float(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := Int64(Params^[0]^);
end;


function Test(f:String; writeTree:Boolean=True; writeCode:Boolean=True): TIntermediateCode;
var
  tree: XTree_Node;
  ctx: TCompilerContext;
  s,tmp: string;
  ast_t, parse_t, t, print_t:Double;
  tokens: TTokenizer;
begin
  s := LoadFileContents('tests/'+f);

  ctx := TCompilerContext.Create();

  ctx.AddType('TIntArray', XType_Array.Create(ctx.GetType('Int64')));
  ctx.AddType('TInt8Array', XType_Array.Create(ctx.GetType('Int8')));

  ctx.AddExternalFunc(@_GetTickCount, 'GetTickCount', [], [], ctx.GetType('Double'));
  ctx.AddExternalFunc(@_Inc64, 'Inc', [ctx.GetType('Int64')], [pbRef], nil);
  ctx.AddExternalFunc(@_RandInt, 'RandInt', [ctx.GetType('Int64'), ctx.GetType('Int64')], [pbRef,pbRef], ctx.GetType('Int64'));
  ctx.AddExternalFunc(@_Sin, 'Sin', [ctx.GetType('Double')], [pbRef], ctx.GetType('Double'));
  ctx.AddExternalFunc(@_Cos, 'Cos', [ctx.GetType('Double')], [pbRef], ctx.GetType('Double'));
  ctx.AddExternalFunc(@_Ln, 'Ln', [ctx.GetType('Double')], [pbRef], ctx.GetType('Double'));
  ctx.AddExternalFunc(@_trunc, 'Trunc', [ctx.GetType('Double')], [pbRef], ctx.GetType('Int64'));
  ctx.AddExternalFunc(@_Round, 'Round', [ctx.GetType('Double')], [pbRef], ctx.GetType('Int64'));
  ctx.AddExternalFunc(@_Float, 'Float', [ctx.GetType('Int64')], [pbRef], ctx.GetType('Double'));


  ctx.AddExternalFunc(@_AllocArray, 'AllocArray', [ctx.GetType('Pointer'), ctx.GetType(xtInt), ctx.GetType(xtInt), ctx.GetType(xtInt)], [pbRef, pbRef, pbRef, pbRef], ctx.GetType('Pointer'));
  ctx.AddExternalFunc(@_FreeMem,    'FreeMem',    [ctx.GetType('Pointer')], [pbRef], nil);
  ctx.AddExternalFunc(@_ReallocMem, 'ReAllocMem', [ctx.GetType('Pointer'), ctx.GetType(xtInt)], [pbRef, pbRef], ctx.GetType('Pointer'));
  ctx.AddExternalFunc(@_AllocMem,   'AllocMem',   [ctx.GetType('Pointer'), ctx.GetType(xtInt)], [pbRef, pbRef], ctx.GetType('Pointer'));


  WriteLn('Compiling ...');
  t := MarkTime();
  tokens := Tokenize(s);
  tree := Parse(Tokens, ctx);
  parse_t := MarkTime() - t;
  WriteLn(Format('Parsed source in %.3f ms', [parse_t]));

  WriteLn('Compiling AST');
  ast_t := MarkTime();
  Result := CompileAST(tree, True);
  Result.StackPosArr := tree.ctx.StackPosArr; // xxx
  ast_t := MarkTime() - ast_t;
  WriteLn(Format('Compiled AST in %.3f ms', [ast_t]));

  t := MarkTime() - t;
  WriteLn(Format('Compiled in %.3f ms', [t]));
end;


procedure TExpressTest.DoRun;
var
  IR: TIntermediateCode;
  runner: TInterpreter;
  flags: EOptimizerFlags;
  Emitter: TBytecodeEmitter;
  BC: TBytecode;

  t: Double;
  i: Int32;
begin
  WriteLn('Express rev 2');
  WriteLn('-------------');

  DefaultFormatSettings.DecimalSeparator:='.';

  if ParamStr(1) = '' then
    ir := Test('lapeisfast.xpr')
  else
    ir := Test(ParamStr(1));

  flags := [optSpecializeExpr];
  if ParamStr(2).Contains('optcmp') then
    flags := flags + [optCmpFlag];

  //WriteFancy(IR.ToString(True));

  Emitter := TBytecodeEmitter.New(IR);
  Emitter.Compile();
  WriteFancy(Emitter.Bytecode.ToString(True));


  runner := TInterpreter.New(Emitter, 0, flags);

  //WriteLn('Executing...');
  t := MarkTime();
  runner.RunSafe(Emitter.Bytecode);
  WriteLn(Format('Executed in %.3f ms', [MarkTime() - t])+#13#10);

  RunScimark();

  //while True do Sleep(500);
  Terminate();
end;

procedure TExpressTest.DoHandleException(Sender:TObject; E:Exception);
begin
  WriteLn('ERROR: ', E.Message);
  while True do Sleep(5000);
end;

var
  Application: TExpressTest;
begin
  Application := TExpressTest.Create(nil);
  Application.Title := 'My Application';
  Application.OnException := @Application.DoHandleException;
  Application.Run;
  Application.Free;
end.

