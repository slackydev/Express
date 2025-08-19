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
  MagicMethods['sizeof'] := XTree_Node(XTree_SizeOf.Create(nil,[],nil,NoDocPos));
  MagicMethods['addr']   := XTree_Node(XTree_Addr.Create(nil,[],nil,NoDocPos));

end.
