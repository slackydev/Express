program Main;
{$i header.inc}

uses
  Classes, SysUtils, CustApp,
  xprTypes,
  xprTokenizer,
  xprBytecode,
  xprIntermediate,
  xprInterpreter,
  xprTree,
  xprUtils,
  xprParser,
  xprCompilerContext,
  xprVartypes,
  xprBytecodeEmitter;

type
  TExpressTest = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure DoHandleException(Sender:TObject; E:Exception);
  end;


//----------------------------------------------------------------------------\\

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

  // Fill arrays as per original logic
  for i := 0 to N - 1 do
  begin
    A[i] := i mod 1000;
    B[i] := (i * 7) mod 1000;
  end;

  t := MarkTime();

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

procedure _SetLength_Test(const Params: PParamArray); cdecl;
type
  TTestArray = array of Int64;
  PTestArray = ^TTestArray;
begin
  SetLength(PTestArray(Params^[0])^, Int64(Params^[1]^)); //this will leak as we have no mem-management
end;


procedure _Length(const Params: PParamArray; const Result: Pointer); cdecl;
type
  TTestArray = array of Int64;
  PTestArray = ^TTestArray;
begin            // result var is last var atm
  Int64(Result^) := Length(PTestArray(Params^[0])^);
end;

 procedure _ArgOrderTest(const Params: PParamArray; const Result: Pointer); cdecl;
begin            // result var is last var atm
  Int64(Result^) := 500;
  WriteLn( PInt64(Params^[0])^ );
  WriteLn( PInt64(Params^[1])^ );
  WriteLn( PInt64(Params^[2])^ );
  WriteLn( PInt64(Params^[3])^ );
  WriteLn( PInt64(Params^[4])^ );
end;



procedure Native_SetLength(const Params: PParamArray);
type
  TNativeArray = record RefCount, High: SizeInt; Data: Pointer; end;
  PNativeArray = ^TNativeArray;
var
  Arr: TNativeArray;
  NewLen: Int32;
  ItemSize: Int32;
  TotalSize: SizeInt;
begin
  //Arr := PNativeArray(Params^[0])^.Data;
  //NewLen := Int64(Params^[1]^);
  //ItemSize := Args[0].FType.ItemType.Size;

end;


function Test(f:String; writeTree:Boolean=True; writeCode:Boolean=True): TIntermediateCode;
var
  tree: XTree_Node;
  ctx: TCompilerContext;
  s,tmp: string;
  ast_t, parse_t, t, print_t:Double;
begin
  s := LoadFileContents('tests/'+f);

  ctx := TCompilerContext.Create();

  ctx.AddType('TIntArray', XType_Array.Create(ctx.GetType('Int64')));
  ctx.AddExternalFunc(@_GetTickCount, 'GetTickCount', [], [], ctx.GetType('Double'));
  ctx.AddExternalFunc(@_Inc64, 'Inc', [ctx.GetType('Int64')], [pbRef], nil);
  ctx.AddExternalFunc(@_SetLength_Test, 'SetLength', [ctx.GetType('TIntArray'), ctx.GetType('Int64')], [pbRef, pbRef], nil);
  ctx.AddExternalFunc(@_Length, 'Length', [ctx.GetType('TIntArray')], [pbRef], ctx.GetType('Int64'));
  ctx.AddExternalFunc(@_RandInt, 'RandInt', [ctx.GetType('Int64'), ctx.GetType('Int64')], [pbRef,pbRef], ctx.GetType('Int64'));
  ctx.AddExternalFunc(@_ArgOrderTest, 'ArgOrderTest', [ctx.GetType('Int64'), ctx.GetType('Int64'), ctx.GetType('Int64'), ctx.GetType('Int64'), ctx.GetType('Int64')], [pbRef,pbRef,pbRef,pbRef,pbRef], ctx.GetType('Int64'));


  WriteLn('Compiling ...');
  t := MarkTime();
  tree := Parse(Tokenize(s), ctx);
  parse_t := MarkTime() - t;
  WriteLn(Format('Parsed source in %.3f ms', [parse_t]));

  if writeTree then
  begin
    print_t := MarkTime();
    WriteLn('----| TREE STRUCTURE |--------------------------------------------');
    tmp := tree.ToString();
    WriteFancy(tmp);
    WriteLn('------------------------------------------------------------------'+#13#10);
    t += MarkTime() - print_t;
  end;

  ast_t := MarkTime();
  Result := CompileAST(tree);
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

  WriteFancy(IR.ToString(True));

  Emitter := TBytecodeEmitter.New(IR);
  Emitter.Compile();
  WriteFancy(Emitter.Bytecode.ToString(True));


  runner := TInterpreter.New(Emitter, 0, flags);

  //WriteLn('Executing...');
  t := MarkTime();
  runner.RunSafe(Emitter.Bytecode);
  WriteLn(Format('Executed in %.3f ms', [MarkTime() - t])+#13#10);

  ShellShortTest();

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

