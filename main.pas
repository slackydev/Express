program Main;

{$IFDEF WINDOWS}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$i header.inc}

uses
  Classes, SysUtils,
  xpr.Types,
  xpr.Langdef,
  xpr.Tokenizer,
  xpr.Bytecode,
  xpr.Intermediate,
  xpr.Interpreter,
  xpr.Tree,
  xpr.Utils,
  xpr.Errors,
  xpr.Parser,
  xpr.CompilerContext,
  xpr.Vartypes,
  xpr.BytecodeEmitter,
  xpr.TypeIntrinsics,
  xpr.NativeBench,
  xpr.import.system;

//----------------------------------------------------------------------------\\



var
  StartHeapUsed: PtrUInt;

function Test(f:String; writeTree:Boolean=True; writeCode:Boolean=True): TIntermediateCode;
var
  tree: XTree_Node;
  ctx: TCompilerContext;
  s: string;
  ast_t, parse_t, t:Double;
  tokens: TTokenizer;
begin
  StartHeapUsed := GetFPCHeapStatus().CurrHeapUsed;

  s := LoadFileContents('./tests/' + f);

  ctx := TCompilerContext.Create();

  // important!
  ImportExternalMethods(ctx);
  ImportSystemModules(ctx);

  // ...
  WriteFancy('Compiling ...');
  t := MarkTime();
  tokens := Tokenize('__main__', s);
  tree := Parse(Tokens, ctx);
  parse_t := MarkTime() - t;
  WriteFancy('Parsed source in %.3f ms', [parse_t]);

  //WriteFancy(tree.ToString()); {direct tree, no internal}

  WriteFancy('Compiling AST');
  ast_t := MarkTime();
  Result := CompileAST(tree, True);
  Result.StackPosArr := tree.ctx.StackPosArr; // xxx
  ast_t := MarkTime() - ast_t;
  WriteFancy('Compiled AST in %.3f ms', [ast_t]);
  t := MarkTime() - t;
  WriteFancy('Compiled in %.3f ms', [t]);
  WriteFancy('Memory used: %f mb', [(GetFPCHeapStatus().CurrHeapUsed - StartHeapUsed) / (1024*1024)]);
end;


procedure Run(FileName: String);
var
  IR: TIntermediateCode;
  runner: TInterpreter;
  flags: EOptimizerFlags;
  Emitter: TBytecodeEmitter;

  t: Double;
begin
  WriteFancy('Running %s', [FileName]);
  ir := Test(FileName);
  flags := [optSpecializeExpr];
  if ParamStr(2).Contains('optcmp') then
    flags := flags + [optCmpFlag];

  //WriteFancy(IR.ToString(True));

  Emitter := TBytecodeEmitter.New(IR);
  Emitter.Compile();
  WriteFancy(Emitter.Bytecode.ToString(True));

  runner := TInterpreter.New(Emitter, 0, flags);

  StartHeapUsed := GetFPCHeapStatus().CurrHeapUsed;
  WriteLn('Executing...');
  t := MarkTime();
  runner.RunSafe(Emitter.Bytecode);
  WriteFancy('Executed in %.3f ms', [MarkTime() - t]);
  WriteFancy('Memory used: %f mb', [(GetFPCHeapStatus().CurrHeapUsed - StartHeapUsed) / (1024*1024)]);
end;

begin
  FormatSettings.DecimalSeparator := '.';
  FormatSettings.ThousandSeparator := ',';
  FormatSettings.DateSeparator := '-';
  FormatSettings.TimeSeparator := ':';

  WriteFancy('Express ' + {$I %Date%} + ' ' + {$I %Time%});
  WriteFancy('---------------------------');

  //XprNativeBenchmark.DotProduct;
  //XprNativeBenchmark.LapeIsFast;
  //XprNativeBenchmark.ShellShort;
  //XprNativeBenchmark.Scimark;

  try
    if (ParamStr(1) = '') then
      Run('scimark.xpr')
    else
      Run(ParamStr(1));
  except
    on E: Exception do
      WriteLn(E.Message);
  end;

  WriteFancy('');
  WriteFancy('Press enter to exit...');
  ReadLn;
end.

