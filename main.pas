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
  t := MarkTime();

  N := 10000000; // 10 million
  SetLength(A, N);
  SetLength(B, N);

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




procedure _Inc64(const Params: PParamArray); cdecl;
begin
  Int64(Params^[0]^) := Int64(Params^[0]^) + 1;
end;

procedure _GetTickCount(const Params: PParamArray); cdecl;
begin
  Double(Params^[0]^) := MarkTime();
end;

procedure _SetLength_Test(const Params: PParamArray); cdecl;
type
  TTestArray = array of Int64;
  PTestArray = ^TTestArray;
begin
  SetLength(PTestArray(Params^[0])^, Int64(Params^[1]^)*4); //this will leak as we have no mem-management
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
  ctx.AddExternalFunc(@_GetTickCount, 'GetTickCount', [ctx.GetType('Double')], [pbRef], nil);
  ctx.AddExternalFunc(@_Inc64, 'Inc', [ctx.GetType('Int64')], [pbRef], nil);
  ctx.AddExternalFunc(@_SetLength_Test, 'SetLength', [ctx.GetType('TIntArray'), ctx.GetType('Int64')], [pbRef, pbRef], nil);

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
  runner.Run(Emitter.Bytecode);
  WriteLn(Format('Executed in %.3f ms', [MarkTime() - t])+#13#10);

  DotProductTest();

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

