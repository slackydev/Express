program Main;

{$APPTYPE CONSOLE}
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
  xpr.Parser,
  xpr.CompilerContext,
  xpr.Vartypes,
  xpr.BytecodeEmitter,
  xpr.TypeIntrinsics,
  xpr.NativeBench;

//----------------------------------------------------------------------------\\

procedure _IntToStr(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  AnsiString(Result^) := IntToStr(Int64(Params^[0]^));
end;

procedure _FloatToStr(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  AnsiString(Result^) := FloatToStrDot(Double(Params^[0]^));
end;

procedure _PtrToStr(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  AnsiString(Result^) := IntToHex(PtrUInt(Params^[0]^));
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
  s: string;
  ast_t, parse_t, t:Double;
  tokens: TTokenizer;
begin
  s := LoadFileContents('tests/'+f);

  ctx := TCompilerContext.Create();

  ctx.AddType('TIntArray', XType_Array.Create(ctx.GetType('Int64')));
  ctx.AddType('TInt8Array', XType_Array.Create(ctx.GetType('Int8')));

  ctx.AddExternalFunc(@_GetTickCount, 'GetTickCount', [], [], ctx.GetType('Double'));
  ctx.AddExternalFunc(@_Inc64, 'Inc', [ctx.GetType('Int64')], [pbRef], nil);
  ctx.AddExternalFunc(@_RandInt, 'RandInt', [ctx.GetType('Int64'), ctx.GetType('Int64')], [pbCopy,pbCopy], ctx.GetType('Int64'));
  ctx.AddExternalFunc(@_Sin, 'Sin', [ctx.GetType('Double')], [pbCopy], ctx.GetType('Double'));
  ctx.AddExternalFunc(@_Cos, 'Cos', [ctx.GetType('Double')], [pbCopy], ctx.GetType('Double'));
  ctx.AddExternalFunc(@_Ln, 'Ln', [ctx.GetType('Double')], [pbCopy], ctx.GetType('Double'));
  ctx.AddExternalFunc(@_trunc, 'Trunc', [ctx.GetType('Double')], [pbCopy], ctx.GetType('Int64'));
  ctx.AddExternalFunc(@_Round, 'Round', [ctx.GetType('Double')], [pbCopy], ctx.GetType('Int64'));
  ctx.AddExternalFunc(@_Float, 'Float', [ctx.GetType('Int64')],  [pbCopy], ctx.GetType('Double'));


  ctx.AddExternalFunc(@_AllocArray, 'AllocArray', [ctx.GetType('Pointer'), ctx.GetType(xtInt), ctx.GetType(xtInt), ctx.GetType(xtInt)], [pbRef, pbRef, pbRef, pbRef], ctx.GetType('Pointer'));
  ctx.AddExternalFunc(@_FreeMem,    'FreeMem',    [ctx.GetType('Pointer')], [pbRef], nil);
  ctx.AddExternalFunc(@_ReallocMem, 'ReAllocMem', [ctx.GetType('Pointer'), ctx.GetType(xtInt)], [pbRef, pbRef], ctx.GetType('Pointer'));
  ctx.AddExternalFunc(@_AllocMem,   'AllocMem',   [ctx.GetType('Pointer'), ctx.GetType(xtInt)], [pbRef, pbRef], ctx.GetType('Pointer'));

  ctx.AddExternalFunc(@_IntToStr,   'IntToStr',   [ctx.GetType('Int64')],   [pbCopy], ctx.GetType(xtAnsiString));
  ctx.AddExternalFunc(@_FloatToStr, 'FloatToStr', [ctx.GetType('Double')],  [pbCopy], ctx.GetType(xtAnsiString));
  ctx.AddExternalFunc(@_PtrToStr,   'PtrToStr',   [ctx.GetType('Pointer')], [pbCopy], ctx.GetType(xtAnsiString));

  WriteFancy('Compiling ...');
  t := MarkTime();
  tokens := Tokenize(s);
  tree := Parse(Tokens, ctx);
  parse_t := MarkTime() - t;
  WriteFancy('Parsed source in %.3f ms', [parse_t]);

  WriteFancy('Compiling AST');
  ast_t := MarkTime();
  Result := CompileAST(tree, True);
  Result.StackPosArr := tree.ctx.StackPosArr; // xxx
  ast_t := MarkTime() - ast_t;
  WriteFancy('Compiled AST in %.3f ms', [ast_t]);

  t := MarkTime() - t;
  WriteFancy('Compiled in %.3f ms', [t]);
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

  WriteLn('Executing...');
  t := MarkTime();
  runner.RunSafe(Emitter.Bytecode);
  WriteFancy('Executed in %.3f ms', [MarkTime() - t]);
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
      Run('lapeisfast.xpr')
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

