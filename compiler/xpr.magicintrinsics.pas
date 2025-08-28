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

type 
  TMagicMethod = specialize TDictionary<string, XTree_Node>;

var
  MagicMethods: TMagicMethod;


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
  TargetVar: TXprVar;
  TargetType: XType;
  DefaultValueNode: XTree_Node;
  i: Integer;
  RecType: XType_Record;
  ThisClass: XType_Class;
  FieldNode: XTree_Field;
  RecursiveDefaultCall: XTree_Invoke;
  FieldIdent: XTree_Identifier;
begin
  if Length(Args) <> 1 then
    ctx.RaiseException('The "default" intrinsic expects exactly one argument (a variable)', FDocPos);

  TargetArgNode := Args[0];
  TargetType := TargetArgNode.ResType();

  DefaultValueNode := nil;

  case TargetType.BaseType of
    xtInt8..xtUInt64, xtBoolean, xtAnsiChar, xtWideChar:
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
          RecursiveDefaultCall := XTree_Invoke.Create(
            XTree_Identifier.Create('default', ctx, FDocPos), [FieldNode], ctx, FDocPos
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
        // For a class instance, we recursively default all of its fields,
        // walking up the inheritance chain to include parent fields.
        ThisClass := TargetType as XType_Class;

        // For each field declared at the current level of the hierarchy...
        for i := 0 to ThisClass.FieldNames.High do
        begin
          // 1. Create a node representing the field (e.g., 'myCat.name').
          FieldIdent := XTree_Identifier.Create(ThisClass.FieldNames.Data[i], ctx, FDocPos);
          FieldNode := XTree_Field.Create(TargetArgNode, FieldIdent, ctx, FDocPos);

          // 2. Create the recursive 'default' call with the field as its argument.
          RecursiveDefaultCall := XTree_Invoke.Create(
            XTree_Identifier.Create('default', ctx, FDocPos), [FieldNode], ctx, FDocPos
          );

          // 3. Compile the recursive call immediately.
          try
            RecursiveDefaultCall.Compile(NullResVar, Flags);
          finally
            // The invoke node owns its children, so freeing it cleans up everything.
            RecursiveDefaultCall.Free;
          end;
        end;

        DefaultValueNode := XTree_Pointer.Create('nil', ctx, FDocPos);
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
    ctx.Emit(GetInstr(icADDR, [Result, LeftVar]), FDocPos)
  end
  else
    Result := LeftVar;

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




begin
  MagicMethods := TMagicMethod.Create(@HashStr);
  MagicMethods['sizeof']   := XTree_Node(XTree_SizeOf.Create(nil,[],nil,NoDocPos));
  MagicMethods['addr']     := XTree_Node(XTree_Addr.Create(nil,[],nil,NoDocPos));
  MagicMethods['default']  := XTree_Node(XTree_Default.Create(nil,[],nil,NoDocPos));

end.
