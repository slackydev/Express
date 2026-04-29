unit xpr.Express;
{
  Copyright 2026 Jarl K. Holta

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}
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
  EExpressLanguage = (xlExpress, xlPascal);

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
    FRunMemorySpilled: SizeInt;
    
    procedure EnsureSystemImported;
    function GetVarInfo(const AName: string; out Ptr: Pointer; out Typ: EExpressBaseType): Boolean;
    function XprTypeFromVariant(const AValue: Variant): XType;
    procedure ClearCompilation;

  public
    constructor Create;
    destructor Destroy; override;

    property Context: TCompilerContext read FContext;
    property Bind: TExpressBinder read FBinder;

    procedure Compile(const ACode: string; const ASourceName: string = '__main__'; ALanguage: EExpressLanguage = xlExpress);
    procedure CompileFile(const AFileName: string);
    function CompileTime: Double;
    procedure Run(FromPosition: PtrUInt = 0);

    function RunCode(const ACode: string; const ASourceName: string = '__main__'; ALanguage: EExpressLanguage = xlExpress): Variant;
    function RunFile(const AFileName: string): Variant;
    procedure SetupVM();

    function GetType(const ABaseType: EExpressBaseType): XType;
    function GetType(const AName: string): XType;
    function GetVar(const AName: string): Variant;
    function GetMethod(const AName: string): PtrInt;

    function Call(FunctionName: string; Args: array of Pointer): Boolean;

    // --- Diagnostic Properties ---
    property ParseTimeMs: Double read FParseTimeMs;
    property ASTCompileTimeMs: Double read FASTCompileTimeMs;
    property BytecodeEmitTimeMs: Double read FBytecodeEmitTimeMs;
    property TotalCompileTimeMs: Double read CompileTime;
    property CompileMemoryUsedMB: Double read FCompileMemUsed;
    property MemorySpilled: SizeInt read FRunMemorySpilled;
    property AST: XTree_Node read FTree;
    property IR: TIntermediateCode read FIntermediate;
    property BC: TBytecode read FEmitter.Bytecode;
    property VM: TInterpreter read FInterpreter;
  end;

var
  xBool: XType;
  xNativeInt: XType;
  xInt8, xInt16, xInt32, xInt64: XType;
  xUInt8, xUInt16, xUInt32, xUInt64: XType;
  xSingle, xDouble: XType;
  xString, xUString: XType;
  xChar, xUChar: XType;
  xPointer: XType;

implementation

uses
  xpr.Parser,
  xpr.PascalTokenizer, 
  xpr.PascalParser,    
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

  // cache simple types
  xBool := FContext.GetType(xtBool);

  xNativeInt := FContext.GetType(xtInt);
  xInt8   := FContext.GetType(xtInt8);
  xInt16  := FContext.GetType(xtInt16);
  xInt32  := FContext.GetType(xtInt32);
  xInt64  := FContext.GetType(xtInt64);
  xUInt8  := FContext.GetType(xtUInt8);
  xUInt16 := FContext.GetType(xtUInt16);
  xUInt32 := FContext.GetType(xtUInt32);
  xUInt64 := FContext.GetType(xtUInt64);

  xSingle := FContext.GetType(xtSingle);
  xDouble := FContext.GetType(xtDouble);

  xString  := FContext.GetType(xtString);
  xUString := FContext.GetType(xtUnicodeString);
  xChar    := FContext.GetType(xtAnsiChar);
  xUChar   := FContext.GetType(xtUnicodeChar);
  xPointer := FContext.GetType(xtPointer);
end;

destructor TExpress.Destroy;
begin
  FBinder.Free;
  FContext.Free;

  // no data, no free - inidactes compile failure
  if Length(FInterpreter.Data) <> 0 then
    FInterpreter.Free(FEmitter.Bytecode);

  FEmitter := Default(TBytecodeEmitter);  // force-finalize internal arrays
  FTree := nil;  // owned by context, already freed
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

procedure TExpress.Compile(const ACode: string; const ASourceName: string; ALanguage: EExpressLanguage);
var
  tokens: TTokenizer;
  t, ast_t, emit_t: Double;
  StartHeapUsed: SizeInt;
  err: EExpressError;
begin
  ClearCompilation;
  EnsureSystemImported;

  StartHeapUsed := GetFPCHeapStatus().CurrHeapUsed;

  t := MarkTime();

  // Route to the correct pipeline based on requested language
  if ALanguage = xlPascal then
  begin
    ImportPascalCompatModules(FContext);
    tokens := TokenizePascal(ASourceName, ACode);
    FTree := ParsePascal(tokens, FContext);
  end
  else
  begin
    tokens := Tokenize(ASourceName, ACode);
    FTree := Parse(tokens, FContext);
  end;

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

  FCompileMemUsed := (GetFPCHeapStatus().CurrHeapUsed - StartHeapUsed) / (1024*1024);
end;

procedure TExpress.CompileFile(const AFileName: string);
var
  Ext: string;
  Lang: EExpressLanguage;
begin
  // Auto-detect language from file extension
  Ext := LowerCase(ExtractFileExt(AFileName));
  if (Ext = '.pas') or (Ext = '.simba') or (Ext = '.pp') or (Ext = '.inc') then
    Lang := xlPascal
  else
    Lang := xlExpress;

  Compile(LoadFileContents(AFileName), AFileName, Lang);
end;

function TExpress.CompileTime(): Double;
begin
  Result := Self.FASTCompileTimeMs + Self.FParseTimeMs + Self.FBytecodeEmitTimeMs;
end;

procedure TExpress.Run(FromPosition: PtrUInt = 0);
var
  StartHeapUsed: SizeInt;
begin
  if not FIsCompiled then
    raise EExpressError.Create('No script has been compiled for execution.');

  FInterpreter := TInterpreter.New(FEmitter, FromPosition, []);
  FInterpreter.HasCreatedJIT := False;

  StartHeapUsed := GetFPCHeapStatus().CurrHeapUsed;

  try
    FInterpreter.RunSafe(FEmitter.Bytecode);
  finally
    //FInterpreter.Free(FEmitter.Bytecode);
    FRunMemorySpilled := GetFPCHeapStatus().CurrHeapUsed - StartHeapUsed;
  end;
end;

procedure TExpress.SetupVM();
begin
  if not FIsCompiled then
    raise EExpressError.Create('No script has been compiled for execution.');

  FInterpreter := TInterpreter.New(FEmitter, 0, []);
  FInterpreter.HasCreatedJIT := False;
end;

function TExpress.RunCode(const ACode: string; const ASourceName: string; ALanguage: EExpressLanguage): Variant;
begin
  Compile(ACode, ASourceName, ALanguage);
  Run;
  Result := GetVar('Result');
end;

function TExpress.RunFile(const AFileName: string): Variant;
begin
  CompileFile(AFileName);
  Run;
  Result := GetVar('Result');
end;


function TExpress.GetType(const ABaseType: EExpressBaseType): XType;
begin
  Result := FContext.GetType(ABaseType);
end;

function TExpress.GetType(const AName: string): XType;
begin
  Result := FContext.GetType(AName);
end;

function TExpress.GetVarInfo(const AName: string; out Ptr: Pointer; out Typ: EExpressBaseType): Boolean;
var
  xprVar: TXprVar;
begin
  Result := False;
  if not FIsCompiled then Exit;
  xprVar := FContext.TryGetGlobalVar(AName);
  if (xprVar <> NullVar) and (xprVar.MemPos in [mpLocal, mpGlobal]) then
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
      xtBool:    Result := PBoolean(ptr)^;
      xtAnsiChar: Result := PAnsiChar(ptr)^;
      xtAnsiString: Result := PAnsiString(ptr)^;
      xtUnicodeChar: Result := PUnicodeChar(ptr)^;
      xtUnicodeString: Result := PUnicodeString(ptr)^;
    else
      Result := PtrUInt(ptr);
    end;
  end;
end;

function TExpress.GetMethod(const AName: string): PtrInt;
var
  ptr: Pointer;
  typ: EExpressBaseType;
begin
  Result := -1;
  if GetVarInfo(AName, ptr, typ) and (typ = xtMethod) then
    Result := PPtrInt(ptr)^;
end;

function TExpress.Call(FunctionName: string; Args: array of Pointer): Boolean;
var
  Method: PtrInt;
begin
  Result := False;
  Method := Self.GetMethod(FunctionName);
  if Method <> -1 then
  begin
    Self.FInterpreter.CallFunction(Self.FEmitter.Bytecode, Method, Args);
    Result := True;
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
    varBoolean:  Result := Context.GetType(xtBool);
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
