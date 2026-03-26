unit xpr.MagicIntrinsics;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This unit provides functions to generate Abstract Syntax Tree (AST) branches
  for intrinsic magic "functions", these methods are not true functions, but
  emits branch rewrites from XTree_Invoke, so they act as if its a call.

  Warning! These nodes are all supposed to be reuse nodes!
  Todo: We can create some sort of cached copy if it's seen before
}
{$I header.inc}
{$hints off}

interface

uses
  SysUtils,
  xpr.Types,
  xpr.Tokenizer,
  xpr.Tree,
  xpr.Vartypes,
  xpr.Errors,
  xpr.CompilerContext,
  xpr.Dictionary;

var
  MagicMethods: TGenericMethods;


type
  (* Represents the default(x) intrinsic *)
  XTree_Default = class(XTree_Invoke)
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  (* Represents the addr(x) intrinsic *)
  XTree_Addr = class(XTree_Invoke)
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  (* Represents the SizeOf(var/type) intrinsic *)
  XTree_SizeOf = class(XTree_Invoke)
    function ResType(): XType; override;   
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  XTree_ThreadSpawn = class(XTree_Invoke)
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
  end;


implementation

uses
  xpr.typeintrinsics,
  xpr.Intermediate;


function XTree_Default.ResType(): XType;
begin
  Result := nil;
end;

function XTree_Default.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var
  TargetArgNode: XTree_Node;
  TargetVar, ClassPtrVar, BoolTemp: TXprVar;
  TargetType: XType;
  DefaultValueNode: XTree_Node;
  i: Integer;
  RecType: XType_Record;
  ThisClass: XType_Class;
  FieldNode: XTree_Field;
  RecursiveDefaultCall: XTree_Invoke;
  FieldIdent: XTree_Identifier;
  skipFields: PtrInt;
begin
  if Length(Args) <> 1 then
    ctx.RaiseException('The "default" intrinsic expects exactly one argument (a variable)', FDocPos);

  TargetArgNode := Args[0];
  TargetType := TargetArgNode.ResType();

  DefaultValueNode := nil;

  case TargetType.BaseType of
    xtInt8..xtUInt64, xtBool, xtAnsiChar, xtUnicodeChar:
      DefaultValueNode := XTree_Int.Create('0', ctx, FDocPos);

    xtSingle, xtDouble:
      DefaultValueNode := XTree_Float.Create('0.0', ctx, FDocPos);

    xtPointer, xtArray, xtAnsiString, xtUnicodeString, xtMethod:
      DefaultValueNode := XTree_Pointer.Create('nil', ctx, FDocPos);

    xtRecord:
      begin
        // For records, generate a sequence of recursive calls
        RecType := TargetType as XType_Record;
        for i := 0 to RecType.FieldNames.High do
        begin
          // 1. Create a node representing the field we need to initialize.
          FieldIdent := XTree_Identifier.Create(RecType.FieldNames.Data[i], ctx, FDocPos);
          FieldNode := XTree_Field.Create(TargetArgNode, FieldIdent, ctx, FDocPos);

          // 2. Create the recursive 'default' call with the field as its argument.
          RecursiveDefaultCall := XTree_Default.Create(
            nil, [FieldNode], ctx, FDocPos
          );

          // 3. Compile the recursive call immediately.
          try
            RecursiveDefaultCall.Compile(NullResVar, Flags);
          finally
            // The invoke node owns its children, so freeing it cleans up everything.
            RecursiveDefaultCall.Free;
          end;
        end;

        Exit(NullResVar);
      end;

    xtClass:
    begin
      ThisClass := TargetType as XType_Class;

      // Guard: if the class pointer is nil, skip field-defaulting.
      // This case arises when default() is called from a record's Default method
      // on a class-typed field that was never assigned (stays nil).
      // Without the guard, the field loop emits code that dereferences nil.
      if ThisClass.FieldNames.Size > 0 then
      begin
        // Compile the target once to read the class pointer value
        ClassPtrVar := TargetArgNode.Compile(NullResVar, Flags);
        ClassPtrVar := ClassPtrVar.IfRefDeref(ctx);

        // nil check: if class pointer = 0, jump past field loop
        BoolTemp   := ctx.GetTempVar(ctx.GetType(xtBool));
        ctx.Emit(GetInstr(icNEQ, [ClassPtrVar, Immediate(0), BoolTemp]),
                 ctx.CurrentDocPos(), ctx.FSettings);
        skipFields := ctx.Emit(GetInstr(icJZ, [BoolTemp, NullVar]),
                                   ctx.CurrentDocPos(), ctx.FSettings);

        for i := 0 to ThisClass.FieldNames.High do
        begin
          FieldIdent := XTree_Identifier.Create(ThisClass.FieldNames.Data[i], ctx, FDocPos);
          FieldNode  := XTree_Field.Create(TargetArgNode, FieldIdent, ctx, FDocPos);
          RecursiveDefaultCall := XTree_Invoke.Create(
            XTree_Identifier.Create('default', ctx, FDocPos), [FieldNode], ctx, FDocPos
          );
          try
            RecursiveDefaultCall.Compile(NullResVar, Flags);
          finally
            RecursiveDefaultCall.Free;
          end;
        end;

        ctx.PatchJump(skipFields);
      end;

      // Write nil to Self WITHOUT calling Collect
      DefaultValueNode := XTree_Pointer.Create('nil', ctx, FDocPos);
      with XTree_Assign.Create(op_Asgn, TargetArgNode, DefaultValueNode, ctx, FDocPos) do
      try
        Compile(NullResVar, Flags + [cfNoCollect]);
      finally
        Free;
      end;

      Exit(NullResVar);
    end;
  else
    ctx.RaiseExceptionFmt('Cannot determine a default value for type `%s`.', [TargetType.ToString()], FDocPos);
  end;

  // For all simple (non-record) types, we now have a DefaultValueNode.
  // We emit a direct assignment to the target variable.
  with XTree_Assign.Create(op_Asgn, TargetArgNode, DefaultValueNode, ctx, FDocPos) do
  try
    Compile(NullResVar, Flags);
  finally
    Free;
  end;

  // The intrinsic itself returns no value.
  Result := NullResVar;
end;




function XTree_Addr.ResType(): XType;
begin
  if FResType = nil then
  begin
    if Length(Args) <> 1 then ctx.RaiseException('The "address-of" intrinsic expects exactly one argument.', FDocPos);
    FResType := XType_Pointer.Create(Args[0].ResType());
    ctx.AddManagedType(FResType);
  end;
  Result := FResType;
end;

function XTree_Addr.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var
  LeftVar: TXprVar;
begin
  if Length(Args) <> 1 then
    ctx.RaiseException('The "address-of" intrinsic expects exactly one argument.', FDocPos);

  LeftVar := Args[0].CompileLValue(NullResVar);

  if LeftVar = NullResVar then
    ctx.RaiseException('Left operand for address-of intrinsic compiled to NullResVar', Args[0].FDocPos);

  if not LeftVar.Reference then
  begin
    Result := Dest;
    if Result = NullResVar then Result := ctx.GetTempVar(ResType());
    Self.Emit(icADDR, [Result, LeftVar], FDocPos);
  end
  else
  begin
    Result := LeftVar;
    Result.VarType := Self.ResType();
  end;

  Result.Reference := False;
end;



//
function XTree_SizeOf.ResType(): XType;
begin
  if FResType = nil then
    FResType := ctx.GetType(xtInt);
  Result := inherited;
end; 
  
function XTree_SizeOf.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; 
begin
  if (Length(Args) <> 1) then
    ctx.RaiseException('The "sizeof" intrinsic expects exactly one argument.', FDocPos);

  Result := NullResVar;

  if (Args[0].ResType() <> nil) then
  begin
    if Dest = NullResVar then
      Dest := ctx.GetTempVar(ctx.GetType(xtInt));

    with XTree_Assign.Create(op_asgn, nil, nil, FContext, FDocPos) do
    try
      Left  := XTree_VarStub.Create(dest, FContext, FDocPos);
      Right := XTree_Int.Create(IntToStr(Args[0].ResType().Size()), FContext, FDocPos);
      Result := Compile(Dest, flags);
    finally
      Free();
    end;
    Result := Dest;
  end else
    ctx.RaiseException(eUnexpected, FDocPos);
end;


function XTree_ThreadSpawn.ResType(): XType;
begin
  Result := ctx.GetType(xtInt64); // returns thread handle
end;

function XTree_ThreadSpawn.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  LambdaVar, HandleVar, ArgVar: TXprVar;
  i, ArgCount: Int32;
begin
  if Length(Args) < 1 then
    ctx.RaiseException('thread_spawn expects at least one lambda argument', FDocPos);

  ArgCount := Length(Args) - 1;

  // Push extra arguments onto ArgStack — they get transferred to thread stack at spawn
  for i := 1 to High(Args) do
  begin
    ArgVar := Args[i].Compile(NullResVar, Flags).IfRefDeref(ctx);
    Self.Emit(GetInstr(icPUSH, [ArgVar]), FDocPos);
  end;

  LambdaVar := Args[0].Compile(NullResVar, Flags).IfRefDeref(ctx);

  HandleVar := Dest;
  if HandleVar = NullResVar then
    HandleVar := ctx.GetTempVar(ctx.GetType(xtInt64));

  Self.Emit(GetInstr(icSPAWN, [LambdaVar, HandleVar, Immediate(ArgCount)]), FDocPos);
  Result := HandleVar;
end;



initialization
  MagicMethods := TGenericMethods.Create(@HashStr);
  MagicMethods['sizeof']   := XTree_Node(XTree_SizeOf.Create(nil,[],nil,NoDocPos));
  MagicMethods['addr']     := XTree_Node(XTree_Addr.Create(nil,[],nil,NoDocPos));
  MagicMethods['default']  := XTree_Node(XTree_Default.Create(nil,[],nil,NoDocPos));
  MagicMethods['thread_spawn'] := XTree_Node(XTree_ThreadSpawn.Create(nil, [], nil, NoDocPos));

finalization
  MagicMethods['sizeof'].FContext := nil;
  MagicMethods['addr'].FContext := nil;
  MagicMethods['default'].FContext := nil;
  MagicMethods['thread_spawn'].FContext := nil;

  MagicMethods['sizeof'].Free();
  MagicMethods['addr'].Free();
  MagicMethods['default'].Free();
  MagicMethods['thread_spawn'].Free();
  MagicMethods.Free();


end.
