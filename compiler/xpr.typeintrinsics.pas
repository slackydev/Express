unit xpr.TypeIntrinsics;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This unit provides functions to generate Abstract Syntax Tree (AST) branches
  for intrinsic array operations like Length and SetLength.
  These operations are implemented by synthesizing calls to low-level
  memory management functions and direct manipulation of the array's
  internal [refcount, length, dataptr] structure.
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
  xpr.CompilerContext;

type
  TTypeIntrinsics = class(TIntrinsics)
  public
    {** AST Node Factory Helpers **}
    function IntLiteral(Value: Int64): XTree_Int;
    function StringLiteral(const Value: string): XTree_String;
    function NilPointer: XTree_Int;
    function Id(const Name: string): XTree_Identifier;
    function SelfId: XTree_Identifier;
    function SelfAsPtr: XTree_Identifier;
    function AddrOf(ANode: XTree_Node): XTree_UnaryOp;
    function Deref(APointerExpr: XTree_Node; AType: XType=nil): XTree_UnaryOp;
    function BinOp(Op: EOperator; Left, Right: XTree_Node): XTree_BinaryOp;
    function Assign(LHS, RHS: XTree_Node): XTree_Assign;
    function Call(FuncName: string; Args: XNodeArray): XTree_Invoke;
    function MethodCall(Target: XTree_Node; FuncName: string; Args: XNodeArray): XTree_Invoke;
    function ReturnStmt(Expr: XTree_Node = nil): XTree_Return;
    function ExprList(Nodes: XNodeArray = nil): XTree_ExprList;
    function VarDecl(Names: TStringArray; VarType: XType; AExpr: XTree_Node=nil): XTree_VarDecl;
    function IfStmt(ACond: XTree_Node; AThenBody, AElseBody: XTree_Node): XTree_If;
    function ForLoop(Init, Cond, Inc: XTree_Node; Body: XTree_ExprList): XTree_For;

    {** Common Action Helpers **}
    function ArrayHeaderField(ArrayExpr: XTree_Node; Index: Integer): XTree_Index;
    function ArrayRefcount(ArrayExpr: XTree_Node): XTree_Index;
    function ArrayHighIndex(ArrayExpr: XTree_Node): XTree_Index;
    function ArrayLength(ArrayExpr: XTree_Node): XTree_BinaryOp;
    function ReturnIfNil(PtrExpr: XTree_Node): XTree_If;
    function FunctionDef(FuncName: string; ArgNames: TStringArray; ByRef: TPassArgsBy; ArgTypes: XTypeArray; ReturnType: XType; Body: XTree_ExprList): XTree_Function;

  public
    FContext: TCompilerContext;
    FDocPos: TDocPos;

    constructor Create(AContext: TCompilerContext; ADocPos: TDocPos);


    function GenerateRefcount(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateHigh(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateLen(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateToStr(SelfType: XType; Args: array of XType): XTree_Function;

    function GeneratePtrAssign(SelfType: XType; Args: array of XType; FuncName: string = ''): XTree_Function;
    function GeneratePtrDispose(SelfType: XType; Args: array of XType; FuncName: string = ''): XTree_Function;

    function GenerateCollect(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateDefault(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateSetLen(SelfType: XType; Args: array of XType): XTree_Function;
  end;

implementation

uses
  xpr.Parser,
  xpr.MagicIntrinsics;

const
  ARRAY_HEADER_SIZE: Int32 = 2 * SizeOf(SizeInt);

{ TTypeIntrinsics - Constructor }

constructor TTypeIntrinsics.Create(AContext: TCompilerContext; ADocPos: TDocPos);
begin
  inherited Create;
  FContext := AContext;
  FDocPos := ADocPos;
end;

{ TTypeIntrinsics - AST Node Factory Helpers }

function TTypeIntrinsics.IntLiteral(Value: Int64): XTree_Int;
begin
  Result := XTree_Int.Create(IntToStr(Value), FContext, FDocPos);
  Result.FResType := FContext.GetType(xtInt);
  Result.Value := Value;
end;

function TTypeIntrinsics.StringLiteral(const Value: string): XTree_String;
begin
  Result := XTree_String.Create(Value, FContext, FDocPos);
end;

function TTypeIntrinsics.NilPointer: XTree_Int;
begin
  Result := XTree_Int.Create('0', FContext, FDocPos);
  Result.FResType := FContext.GetType(xtPointer);
  Result.Value := 0;
end;

function TTypeIntrinsics.Id(const Name: string): XTree_Identifier;
begin
  Result := XTree_Identifier.Create(Name, FContext, FDocPos);
end;

function TTypeIntrinsics.SelfId: XTree_Identifier;
begin
  Result := Id('Self');
end;

function TTypeIntrinsics.SelfAsPtr: XTree_Identifier;
begin
    Result := Id('Self');
    Result.FResType := FContext.GetType(xtPointer);
end;

function TTypeIntrinsics.AddrOf(ANode: XTree_Node): XTree_UnaryOp;
begin
  Result := XTree_UnaryOp.Create(op_Addr, ANode, FContext, FDocPos);
  Result.FResType := FContext.GetType(xtPointer);
end;

function TTypeIntrinsics.Deref(APointerExpr: XTree_Node; AType: XType=nil): XTree_UnaryOp;
begin
  Result := XTree_UnaryOp.Create(op_Deref, APointerExpr, FContext, FDocPos);
  if AType <> nil then
    Result.FResType := AType;
end;


function TTypeIntrinsics.BinOp(Op: EOperator; Left, Right: XTree_Node): XTree_BinaryOp;
begin
  Result := XTree_BinaryOp.Create(Op, Left, Right, FContext, FDocPos);
end;

function TTypeIntrinsics.Assign(LHS, RHS: XTree_Node): XTree_Assign;
begin
  Result := XTree_Assign.Create(op_Asgn, LHS, RHS, FContext, FDocPos);
end;

function TTypeIntrinsics.Call(FuncName: string; Args: XNodeArray): XTree_Invoke;
begin
  Result := XTree_Invoke.Create(Id(FuncName), Args, FContext, FDocPos);
end;

function TTypeIntrinsics.MethodCall(Target: XTree_Node; FuncName: string; Args: XNodeArray): XTree_Invoke;
var
  InvokeNode: XTree_Invoke;
begin
  InvokeNode := Call(FuncName, Args);
  InvokeNode.SelfExpr := Target;
  Result := InvokeNode;
end;

function TTypeIntrinsics.ReturnStmt(Expr: XTree_Node = nil): XTree_Return;
begin
  Result := XTree_Return.Create(Expr, FContext, FDocPos);
end;

function TTypeIntrinsics.ExprList(Nodes: XNodeArray = nil): XTree_ExprList;
begin
  if Nodes = nil then
    Result := XTree_ExprList.Create(FContext, FDocPos)
  else
    Result := XTree_ExprList.Create(Nodes, FContext, FDocPos);
end;

function TTypeIntrinsics.VarDecl(Names: TStringArray; VarType: XType; AExpr: XTree_Node=nil): XTree_VarDecl;
var
  IdentList: XIdentNodeList;
  Name: string;
begin
  IdentList.Init([]);
  for Name in Names do
    IdentList.Add(Id(Name));
  Result := XTree_VarDecl.Create(IdentList, AExpr, VarType, FContext, FDocPos);
end;

function TTypeIntrinsics.IfStmt(ACond: XTree_Node; AThenBody, AElseBody: XTree_Node): XTree_If;
var
  Conds, Bodys: XNodeArray;
  ThenList, ElseList: XTree_ExprList;
begin
  SetLength(Conds, 1);
  Conds[0] := ACond;

  if (AThenBody <> nil) and (AThenBody is XTree_ExprList) then
     ThenList := XTree_ExprList(AThenBody)
  else
     ThenList := ExprList([AThenBody]);

  SetLength(Bodys, 1);
  Bodys[0] := ThenList;

  if AElseBody <> nil then
  begin
    if AElseBody is XTree_ExprList then
      ElseList := XTree_ExprList(AElseBody)
    else
      ElseList := ExprList([AElseBody]);
  end else
    ElseList := nil;

  Result := XTree_If.Create(Conds, Bodys, ElseList, FContext, FDocPos);
end;

function TTypeIntrinsics.ForLoop(Init, Cond, Inc: XTree_Node; Body: XTree_ExprList): XTree_For;
begin
  Result := XTree_For.Create(Init, Cond, Inc, Body, FContext, FDocPos);
end;



{ TTypeIntrinsics - Common Action Helpers }

function TTypeIntrinsics.ArrayHeaderField(ArrayExpr: XTree_Node; Index: Integer): XTree_Index;
begin
  Result := XTree_Index.Create(ArrayExpr, IntLiteral(Index), FContext, FDocPos);
  Result.FResType := FContext.GetType(xtInt);
  Result.ForceTypeSize := SizeOf(SizeInt);
end;

function TTypeIntrinsics.ArrayRefcount(ArrayExpr: XTree_Node): XTree_Index;
begin
  Result := ArrayHeaderField(ArrayExpr, -2);
end;

function TTypeIntrinsics.ArrayHighIndex(ArrayExpr: XTree_Node): XTree_Index;
begin
  Result := ArrayHeaderField(ArrayExpr, -1);
end;

function TTypeIntrinsics.ArrayLength(ArrayExpr: XTree_Node): XTree_BinaryOp;
begin
  Result := BinOp(op_ADD, ArrayHighIndex(ArrayExpr), IntLiteral(1));
  Result.FResType := FContext.GetType(xtInt);
end;

function TTypeIntrinsics.ReturnIfNil(PtrExpr: XTree_Node): XTree_If;
begin
  Result := IfStmt(BinOp(op_EQ, PtrExpr, NilPointer), ReturnStmt(), nil);
end;

// internal methods default to no collecting / finalizing var (no free)
function TTypeIntrinsics.FunctionDef(FuncName: string; ArgNames: TStringArray; ByRef: TPassArgsBy; ArgTypes: XTypeArray; ReturnType: XType; Body: XTree_ExprList): XTree_Function;
begin
  Result := XTree_Function.Create(FuncName, ArgNames, ByRef, ArgTypes, ReturnType, Body, FContext, FDocPos);
  Result.InternalFlags += [cfNoCollect];
end;



{ TTypeIntrinsics - Intrinsic Function Generators }

function TTypeIntrinsics.GenerateRefcount(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
  ReturnValueNode: XTree_Node;
begin
  if SelfType = nil then
    Exit(nil);

  if (Length(Args) > 0) or (not (SelfType.BaseType in XprRefcountedTypes)) then
    Exit(nil);

  ReturnValueNode := Self.ArrayRefcount(SelfId);

  Body := ExprList([
    IfStmt(
      BinOp(op_NEQ, SelfId, NilPointer),
      ReturnStmt(ReturnValueNode),
      ReturnStmt(IntLiteral(-1))
    )
  ]);

  Result := FunctionDef('_refcnt', [], nil, [], FContext.GetType(xtInt), Body);
  Result.SelfType := SelfType;
end;

function TTypeIntrinsics.GenerateHigh(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
  ReturnValueNode: XTree_Node;
begin
  if SelfType = nil then
    Exit(nil);

  if (Length(Args) > 0) or not ((SelfType is XType_Array) or (SelfType is XType_String)) then
    Exit(nil);

  if (SelfType is XType_String) then
    ReturnValueNode := BinOp(op_Sub, ArrayHighIndex(SelfId), IntLiteral(1))
  else
    ReturnValueNode := ArrayHighIndex(SelfId);

  Body := ExprList([
    IfStmt(
      BinOp(op_NEQ, SelfId, NilPointer),
      ReturnStmt(ReturnValueNode),
      ReturnStmt(IntLiteral(-1))
    )
  ]);

  Result := FunctionDef('High', [], nil, [], FContext.GetType(xtInt), Body);
  Result.SelfType := SelfType;
end;

function TTypeIntrinsics.GenerateLen(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
  ResultVar: XTree_Identifier;
  LengthValueNode: XTree_Node;
begin
  if SelfType = nil then
    Exit(nil);

  if (Length(Args) > 0) or not ((SelfType is XType_Array) or (SelfType is XType_String)) then
    Exit(nil);

  ResultVar := Id('Result');

  if (SelfType is XType_String) then
    LengthValueNode := ArrayHighIndex(SelfId)
  else
    LengthValueNode := ArrayLength(SelfId);

  Body := ExprList([
    IfStmt(
      BinOp(op_NEQ, SelfId, NilPointer),
      Assign(ResultVar, LengthValueNode),
      Assign(ResultVar, IntLiteral(0))
    ),
    ReturnStmt(ResultVar)
  ]);

  Result := FunctionDef('Len', [], nil, [], FContext.GetType(xtInt), Body);
  Result.SelfType := SelfType;
end;


function TTypeIntrinsics.GenerateToStr(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
  ReturnNode: XTree_Node;
  StringType: XType;
begin
  if SelfType = nil then
    Exit(nil);

  if Length(Args) > 0 then
    Exit(nil);

  Body := ExprList();
  StringType := FContext.GetType(xtAnsiString); // The function will always return a string.

  // The 'self' variable holds the value we need to convert.
  case SelfType.BaseType of
    xtAnsiString, xtUnicodeString:
      ReturnNode := SelfId();

    xtInt8..xtUInt64:
      ReturnNode := Call('IntToStr', [SelfId()]);

    xtSingle, xtDouble:
      ReturnNode := Call('FloatToStr', [SelfId()]);

    xtBoolean:
      // Use a ternary expression: if(self) "true" else "false"
      ReturnNode := XTree_IfExpr.Create(
        SelfId(),
        StringLiteral('True'),
        StringLiteral('False'),
        FContext, FDocPos
      );

    xtPointer:
        ReturnNode := Call('PtrToStr', [SelfId()]);

    xtArray:
      // For now, arrays and records are represented by their address.
      // This creates the string: "(TypeName @ " + PtrToStr(Self) + ")"
      begin
        ReturnNode := BinOp(op_Add, StringLiteral('(' + SelfType.ToString() + ' @ '),
          BinOp(op_Add, Call('PtrToStr', [SelfId()]), StringLiteral(')'))
        );
      end;

  else
    // Default for unknown types
    ReturnNode := StringLiteral('<Undefined>');
  end;

  // The function body is just a single return statement.
  Body.List += ReturnStmt(ReturnNode);

  Result := FunctionDef('ToStr', [], nil, [], StringType, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags:=[];
end;


function TTypeIntrinsics.GeneratePtrAssign(SelfType: XType; Args: array of XType; FuncName: string = ''): XTree_Function;
var
  Body: XTree_ExprList;
  PType: XType_Pointer;
begin
  // This intrinsic is the universal entry point for finalizing any managed type.
  if (SelfType <> nil) or (Length(Args) <> 2) then
    Exit(nil);

  PType := Args[0] as XType_Pointer;


  Body := ExprList();
  Body.List += Parse('__main__', FContext,
  'if(right^ != nil)then'    + LineEnding +
    'left^ := right^;'       + LineEnding +
  'end;'
  );

  if FuncName = '' then
    Result := FunctionDef('__passign__', ['left','right'], [pbRef,pbRef], [PType,PType], nil, Body)
  else
    Result := FunctionDef(FuncName, ['left','right'], [pbRef,pbRef], [PType,PType], nil, Body);
end;

function TTypeIntrinsics.GeneratePtrDispose(SelfType: XType; Args: array of XType; FuncName:string=''): XTree_Function;
var
  Body: XTree_ExprList;
  PType: XType_Pointer;
begin
  // This intrinsic is the universal entry point for finalizing any managed type.
  if (SelfType <> nil) or (Length(Args) <> 1) then
    Exit(nil);

  PType := Args[0] as XType_Pointer;
  Body := ExprList();

  case XType_Pointer(Args[0]).PointsTo.BaseType of
    xtPointer:
      Body.List += Call('freemem', [Deref(Id('ptr'))]);
    else
      // Default will set everything to nil, casuing a reaction in XTree_Assign
      // XTree_Assign will trigger collect. Collection machanism depends on type.

      // xtArray: An array will trigger SetLen(0) which means full release of data, or reduction in refcount.

      // xtClass: Setting to nil will simply trigger it's built in .Free, which currently
      //          directly releases it's data, as refcounting is ToDo.
      Body.List += XTree_Default.Create(nil,[Deref(Id('ptr'))],FContext,FDocPos);
  end;

  if FuncName = '' then
    Result := FunctionDef('__pdispose__', ['ptr'], [pbCopy], [PType], nil, Body)
  else
    Result := FunctionDef(FuncName, ['ptr'], [pbCopy], [PType], nil, Body);
  Result.InternalFlags -= [cfNoCollect];
end;


function TTypeIntrinsics.GenerateCollect(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
  SelfIdent: XTree_Node;
begin
  if SelfType = nil then
    Exit(nil);

  // This intrinsic is the universal entry point for finalizing any managed type.
  //if not SelfType.IsManaged(FContext) then
  //  Exit(nil);

  Body := ExprList();
  SelfIdent := SelfId();

  // The Collect method acts as a dispatcher, calling the correct
  // finalization intrinsic based on the object's top-level type.
  case SelfType.BaseType of
    xtRecord:
    begin
      // --- For a managed record ---
      // Its 'Collect' simply nils it out, and lets assign handle it.
      Body.List += MethodCall(SelfIdent, 'Default', []);
    end;

    xtArray, xtAnsiString, xtUnicodeString: // This also correctly handles XType_String
    begin
      // --- For an array or string ---
      // 'Collect' is called when the variable's lifetime ends.
      // This means we must decrement its refcount. SetLen(0) will handle
      // the rest (checking if the count is zero and finalizing children).
      // Generated code: Self.SetLen(0)
      Body.List += MethodCall(SelfIdent, 'SetLen', [IntLiteral(0)]);
    end;

    xtClass:
    begin
      // --- For a class instance ---
      // The equivalent of finalization is calling its destructor and
      // deallocating its memory. This is exactly what 'Free' does.
      // Generated code: Self.Free()
      Body.List += ReturnIfNil(SelfIdent);
      Body.List += VarDecl(['HeaderSize'], FContext.GetType(xtInt), IntLiteral(2 * SizeOf(SizeInt)));
      Body.List += VarDecl(['raw'], FContext.GetType(xtPointer), BinOp(op_sub, SelfAsPtr, Id('HeaderSize')));
      Body.List += IfStmt(
        BinOp(op_EQ, Deref(Id('raw'), FContext.GetType(xtInt)), IntLiteral(1)),
        MethodCall(SelfIdent, 'Free', []),
        Assign(
          Deref(Id('raw'), FContext.GetType(xtInt)),
          BinOp(op_sub,
            Deref(Id('raw'), FContext.GetType(xtInt)),
            IntLiteral(1)
          )
        )
      );
    end;
  end;

  Result := FunctionDef('Collect', [], nil, [], nil, Body);
  Result.SelfType := SelfType;
end;

function TTypeIntrinsics.GenerateDefault(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
begin
  if SelfType = nil then
    Exit(nil);

  Body := ExprList([XTree_Default.Create(nil,[SelfId()],FContext,FDocPos)]);

  Result := FunctionDef('Default', [], nil, [], nil, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags:=[]; //allow full free
end;

function TTypeIntrinsics.GenerateSetLen(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
  ItemType: XType;
  InternalUnitImport: XTree_ImportUnit;
  PItemType: XType;
  TDisposalProto: XType_Method;
  TCopyProto: XType_method;
begin
  if SelfType = nil then
    Exit(nil);

  if (Length(Args) <> 1) or not (SelfType is XType_Array) then
    Exit(nil);

  Body := ExprList();

  ItemType := (SelfType as XType_Array).ItemType;
  PItemType := XType_Pointer.Create(ItemType);
  FContext.AddManagedType(PItemType);

  // --- Step 1: Ensure the internal runtime library is imported ---
  // This generates: import 'Internals.xpr' as __internal
  // The compiler will handle caching, so this only happens once per compilation under this namespace.
  InternalUnitImport := XTree_ImportUnit.Create(
    'system/internals.xpr', '__internal', FContext, FDocPos
  );
  Body.List += InternalUnitImport;


  TDisposalProto := XType_Method.Create('_TDisposalMethod', [PItemType], [pbCopy], nil, False);
  FContext.AddManagedType(TDisposalProto);

  TCopyProto := XType_Method.Create('_TCopyMethod', [PItemType,PItemType], [pbRef, pbRef], nil, False);
  FContext.AddManagedType(TCopyProto);

  Body.List += VarDecl(['dispose'], TDisposalProto);
  Body.List += VarDecl(['copy'], TCopyProto);

  // --- Step 2: Determine the correct Dispose/Copy intrinsic functions ---
  if ItemType.IsManaged(FContext) then
  begin
    Body.List += FContext.GenerateIntrinsics('__pdispose__', [PItemType],           nil, '__pdispose__'+TDisposalProto.Hash());
    Body.List += FContext.GenerateIntrinsics('__passign__', [PItemType, PItemType], nil, '__passign__' +TCopyProto.Hash());

    Body.List += Assign(Id('dispose'), Id('__pdispose__'+TDisposalProto.Hash()));
    Body.List += Assign(Id('copy'),    Id('__passign__' +TCopyProto.Hash()));
  end;

  Body.List += VarDecl(['raw'], FContext.GetType(xtPointer), SelfAsPtr());

  Body.List += Assign(SelfId(), XTree_Invoke.Create(
    XTree_Field.Create(Id('__internal'), Id('_ArraySetLength'), FContext, FDocPos),
    [ // The arguments list:
      Id('raw'),
      Id('NewLength'),
      IntLiteral(ItemType.Size),
      Id('dispose'),
      Id('copy')
    ],
    FContext, FDocPos
  ));

  Result := FunctionDef('SetLen', ['NewLength'], [pbCopy], [FContext.GetType(xtInt)], nil, Body);
  Result.SelfType := SelfType;
end;


end.
