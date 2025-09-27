unit xpr.Express;

{$I header.inc}

interface

uses
  SysUtils, Classes, Variants,
  xpr.Types,
  xpr.Errors,
  xpr.Tokenizer,
  xpr.CompilerContext,
  xpr.Vartypes,
  xpr.BytecodeEmitter,
  xpr.Interpreter,
  xpr.Intermediate,
  xpr.Bytecode;

type
  EExpressError = class(Exception)
  public
    ErrorType: EExceptionType;
    DocPos: TDocPos;
    StackTrace: string;
  end;

  TExpress = class;

  TExpressBinder = class(TObject)
  private
    FExpress: TExpress;
    constructor Create(AExpress: TExpress);
  public
    function AddVar(const AName: string; const AValue: Variant): TExpressBinder;
    function AddVar(const AName: string; APointer: Pointer; AType: XType): TExpressBinder; overload; // External var

    function AddFunction(const AName: string; AFunc: Pointer;
                         const AParams: XTypeArray; AReturnType: XType = nil;
                         const APassBy: TPassArgsBy = nil): TExpressBinder;
  end;

  TExpress = class(TObject)
  private
    FContext: TCompilerContext;
    FEmitter: TBytecodeEmitter;
    FInterpreter: TInterpreter;
    FIsCompiled: Boolean;
    FBinder: TExpressBinder;
    FSystemImported: Boolean;
    FIntermediate: TIntermediateCode;
    FBytecode: TBytecode;
    FTree: XTree_Node;

    // --- Statistics ---
    FParseTimeMs: Double;
    FASTCompileTimeMs: Double;
    FBytecodeEmitTimeMs: Double;
    FCompileMemUsed: Double;
    FRunMemorySpilled: Double;
    
    procedure EnsureSystemImported;
    function GetVarInfo(const AName: string; out Ptr: Pointer; out Typ: EExpressBaseType): Boolean;
    function XprTypeFromVariant(const AValue: Variant): XType;
    procedure ClearCompilation;

  public
    constructor Create;
    destructor Destroy; override;

    property Context: TCompilerContext read FContext;
    property Bind: TExpressBinder read FBinder;

    procedure Compile(const ACode: string; const ASourceName: string = '__main__');
    procedure CompileFile(const AFileName: string);
    function CompileTime: Double;
    procedure Run(FromPosition: PtrUInt = 0);

    function RunCode(const ACode: string; const ASourceName: string = '__main__'): Variant;
    function RunFile(const AFileName: string): Variant;

    function GetVar(const AName: string): Variant;

    // --- Diagnostic Properties ---
    property ParseTimeMs: Double read FParseTimeMs;
    property ASTCompileTimeMs: Double read FASTCompileTimeMs;
    property BytecodeEmitTimeMs: Double read FBytecodeEmitTimeMs;
    property TotalCompileTimeMs: Double read CompileTime;
    property CompileMemoryUsedMB: Double read FCompileMemUsed;
    property RunMemorySpilled: Double read FRunMemorySpilled;
    property AST: XTree_Node read FTree;
    property IR: TIntermediateCode read FIntermediate;
    property BC: TBytecode read FEmitter.Bytecode;
  end;

implementation

uses
  xpr.Parser,
  xpr.Import.System,
  xpr.Utils,
  xpr.Tree;

// --- TExpressBinder Implementation ---

constructor TExpressBinder.Create(AExpress: TExpress);
begin
  FExpress := AExpress;
end;

function TExpressBinder.AddVar(const AName: string; const AValue: Variant): TExpressBinder;
var
  xprType: XType;
begin
  xprType := FExpress.XprTypeFromVariant(AValue);
  if xprType = nil then
    raise EExpressError.CreateFmt('Unsupported variant type for variable "%s"', [AName]);
  FExpress.Context.AddVar(AValue, AName, xprType);
  Result := Self;
end;

function TExpressBinder.AddVar(const AName: string; APointer: Pointer; AType: XType): TExpressBinder;
begin
  FExpress.Context.AddExternalVar(APointer, AName, AType);
  Result := Self;
end;

function TExpressBinder.AddFunction(const AName: string; AFunc: Pointer;
                                     const AParams: XTypeArray; AReturnType: XType;
                                     const APassBy: TPassArgsBy): TExpressBinder;
var
  i: Int32;
  passByCopy: TPassArgsBy;
begin
  if Length(APassBy) = 0 then
  begin
    SetLength(passByCopy, Length(AParams));
    for i := 0 to High(passByCopy) do
      passByCopy[i] := pbCopy;
  end else
    passByCopy := APassBy;

  if AReturnType = nil then
    FExpress.Context.AddExternalFunc(TExternalProc(AFunc), AName, AParams, passByCopy, nil)
  else
    FExpress.Context.AddExternalFunc(TExternalFunc(AFunc), AName, AParams, passByCopy, AReturnType);
  Result := Self;
end;

// --- TExpress Implementation ---

constructor TExpress.Create;
begin
  inherited;
  FContext := TCompilerContext.Create();
  FBinder := TExpressBinder.Create(Self);
  FIsCompiled := False;
  FSystemImported := False;
end;

destructor TExpress.Destroy;
begin
  FBinder.Free;
  FContext.Free;
  inherited;
end;

procedure TExpress.ClearCompilation;
begin
  FContext.Intermediate.Init();
  FContext.DelayedNodes := [];
  FEmitter := Default(TBytecodeEmitter);
  FInterpreter := Default(TInterpreter);
  FIsCompiled := False;
end;

procedure TExpress.EnsureSystemImported;
begin
  if FSystemImported then Exit;
  ImportExternalMethods(FContext);
  ImportSystemModules(FContext);
  FSystemImported := True;
end;

procedure TExpress.Compile(const ACode: string; const ASourceName: string);
var
  tokens: TTokenizer;
  t, ast_t, emit_t: Double;
  StartHeapUsed: SizeInt;
  err: EExpressError;
begin
  ClearCompilation;
  EnsureSystemImported;

  StartHeapUsed := GetFPCHeapStatus().CurrHeapUsed;

  try
    t := MarkTime();
    tokens := Tokenize(ASourceName, ACode);
    FTree := Parse(tokens, FContext);
    FParseTimeMs := MarkTime() - t;

    ast_t := MarkTime();
    FIntermediate := CompileAST(FTree, False);
    FIntermediate.StackPosArr := FTree.ctx.StackPosArr;
    FASTCompileTimeMs := MarkTime() - ast_t;

    emit_t := MarkTime();
    FEmitter := TBytecodeEmitter.New(FIntermediate);
    FEmitter.Compile();
    FBytecodeEmitTimeMs := MarkTime() - emit_t;

    FIsCompiled := True;
  except
    on E: SyntaxError do
    begin
      err := EExpressError.CreateFmt('%s', [E.Message]);
      err.DocPos := E.DocPos;
      raise err at get_caller_addr(get_frame);
    end;
  end;
  FCompileMemUsed := (GetFPCHeapStatus().CurrHeapUsed - StartHeapUsed) / (1024*1024);
end;

procedure TExpress.CompileFile(const AFileName: string);
begin
  Compile(LoadFileContents(AFileName), AFileName);
end;

function TExpress.CompileTime(): Double;
begin
  Result := Self.FASTCompileTimeMs + Self.FParseTimeMs + Self.FBytecodeEmitTimeMs;
end;

procedure TExpress.Run(FromPosition: PtrUInt = 0);
var
  StartHeapUsed: SizeInt;
  err: EExpressError;
begin
  if not FIsCompiled then
    raise EExpressError.Create('No script has been compiled for execution.');


  FInterpreter := TInterpreter.New(FEmitter, FromPosition, []);
  FInterpreter.HasCreatedJIT := False;

  StartHeapUsed := GetFPCHeapStatus().CurrHeapUsed;
  try
    FInterpreter.RunSafe(FEmitter.Bytecode);
  except
    on E: RuntimeError do
    begin
      err := EExpressError.CreateFmt('Runtime Error: %s', [E.Message]);
      err.StackTrace := FInterpreter.BuildStackTraceString(FEmitter.Bytecode);
      raise err;
    end;
    on E: Exception do
    begin
      err := EExpressError.CreateFmt('Native Host Error during script execution: %s', [E.Message]);
      err.StackTrace := FInterpreter.BuildStackTraceString(FEmitter.Bytecode);
      raise err;
    end;
  end;
  FRunMemorySpilled := (GetFPCHeapStatus().CurrHeapUsed - StartHeapUsed) / (1024*1024);
end;

function TExpress.RunCode(const ACode: string; const ASourceName: string): Variant;
begin
  Compile(ACode, ASourceName);
  Run;
  Result := GetVar('Result');
end;

function TExpress.RunFile(const AFileName: string): Variant;
begin
  CompileFile(AFileName);
  Run;
  Result := GetVar('Result');
end;

function TExpress.GetVarInfo(const AName: string; out Ptr: Pointer; out Typ: EExpressBaseType): Boolean;
var
  xprVar: TXprVar;
begin
  Result := False;
  if not FIsCompiled then Exit;
  xprVar := FContext.TryGetGlobalVar(AName);
  if xprVar <> NullVar then
  begin
    Ptr := FInterpreter.Global(xprVar.Addr);
    Typ := xprVar.VarType.BaseType;
    Result := True;
  end;
end;

function TExpress.GetVar(const AName: string): Variant;
var
  ptr: Pointer;
  typ: EExpressBaseType;
begin
  Result := Null;
  if GetVarInfo(AName, ptr, typ) then
  begin
    case typ of
      xtInt8:    Result := PInt8(ptr)^;
      xtUInt8:   Result := PUInt8(ptr)^;
      xtInt16:   Result := PInt16(ptr)^;
      xtUInt16:  Result := PUInt16(ptr)^;
      xtInt32:   Result := PInt32(ptr)^;
      xtUInt32:  Result := PUInt32(ptr)^;
      xtInt64:   Result := PInt64(ptr)^;
      xtUInt64:  Result := PUInt64(ptr)^;
      xtSingle:  Result := PSingle(ptr)^;
      xtDouble:  Result := PDouble(ptr)^;
      xtBoolean: Result := PBoolean(ptr)^;
      xtAnsiChar: Result := PAnsiChar(ptr)^;
      xtAnsiString: Result := PAnsiString(ptr)^;
    else
      Result := PtrUInt(ptr);
    end;
  end;
end;

function TExpress.XprTypeFromVariant(const AValue: Variant): XType;
begin
  case VarType(AValue) of
    varByte:     Result := Context.GetType(xtUInt8);
    varWord:     Result := Context.GetType(xtUInt16);
    varLongWord: Result := Context.GetType(xtUInt32);
    varUInt64:   Result := Context.GetType(xtUInt64);
    varSmallint: Result := Context.GetType(xtInt16);
    varInteger:  Result := Context.GetType(xtInt32);
    varInt64:    Result := Context.GetType(xtInt64);
    varSingle:   Result := Context.GetType(xtSingle);
    varDouble:   Result := Context.GetType(xtDouble);
    varBoolean:  Result := Context.GetType(xtBoolean);
    varOleStr, varString:
      begin
        if Length(AValue) = 1 then
          Result := Context.GetType(xtAnsiChar)
        else
          Result := Context.GetType(xtAnsiString);
      end
  else
    Result := nil;
  end;
end;

end.
