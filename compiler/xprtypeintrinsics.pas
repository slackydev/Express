unit xprTypeIntrinsics;
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
  xprTypes,
  xprTokenizer,
  xprTree,
  xprVartypes,
  xprErrors,
  xprCompilerContext;

type
  TTypeIntrinsics = class(TIntrinsics)
  private
    {** AST Node Factory Helpers **}
    function IntLiteral(Value: Int64): XTree_Int;
    function NilPointer: XTree_Int;
    function Id(const Name: string): XTree_Identifier;
    function SelfId: XTree_Identifier;
    function SelfAsPtr: XTree_Identifier;
    function AddrOf(ANode: XTree_Node): XTree_UnaryOp;
    function Deref(APointerExpr: XTree_Node; AType: XType): XTree_UnaryOp;
    function BinOp(Op: EOperator; Left, Right: XTree_Node): XTree_BinaryOp;
    function Assign(LHS, RHS: XTree_Node): XTree_Assign;
    function Call(FuncName: string; Args: XNodeArray): XTree_Invoke;
    function MethodCall(Target: XTree_Node; FuncName: string; Args: XNodeArray): XTree_Invoke;
    function ReturnStmt(Expr: XTree_Node = nil): XTree_Return;
    function ExprList(Nodes: XNodeArray = nil): XTree_ExprList;
    function VarDecl(Names: TStringArray; VarType: XType): XTree_VarDecl;
    function IfStmt(ACond: XTree_Node; AThenBody, AElseBody: XTree_Node): XTree_If;
    function ForLoop(Init, Cond, Inc: XTree_Node; Body: XTree_ExprList): XTree_For;

    {** Common Action Helpers **}
    function ArrayHeaderField(ArrayExpr: XTree_Node; Index: Integer): XTree_Index;
    function ArrayRefcount(ArrayExpr: XTree_Node): XTree_Index;
    function ArrayHighIndex(ArrayExpr: XTree_Node): XTree_Index;
    function ArrayLength(ArrayExpr: XTree_Node): XTree_BinaryOp;
    function ReturnIfNil(ArrayExpr: XTree_Node): XTree_If;
    function FunctionDef(FuncName: string; ArgNames: TStringArray; ByRef: TPassArgsBy; ArgTypes: XTypeArray; ReturnType: XType; Body: XTree_ExprList): XTree_Function;

  public
    FContext: TCompilerContext;
    FDocPos: TDocPos;

    constructor Create(AContext: TCompilerContext; ADocPos: TDocPos);

    function GenerateHigh(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateLen(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateCollect(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateSetLen(SelfType: XType; Args: array of XType): XTree_Function;
  end;

implementation

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

function TTypeIntrinsics.Deref(APointerExpr: XTree_Node; AType: XType): XTree_UnaryOp;
begin
  Result := XTree_UnaryOp.Create(op_Deref, APointerExpr, FContext, FDocPos);
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

function TTypeIntrinsics.VarDecl(Names: TStringArray; VarType: XType): XTree_VarDecl;
var
  IdentList: XIdentNodeList;
  Name: string;
begin
  IdentList.Init([]);
  for Name in Names do
    IdentList.Add(Id(Name));
  Result := XTree_VarDecl.Create(IdentList, nil, VarType, FContext, FDocPos);
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

function TTypeIntrinsics.ReturnIfNil(ArrayExpr: XTree_Node): XTree_If;
begin
  Result := IfStmt(BinOp(op_EQ, ArrayExpr, NilPointer), ReturnStmt(), nil);
end;

function TTypeIntrinsics.FunctionDef(FuncName: string; ArgNames: TStringArray; ByRef: TPassArgsBy; ArgTypes: XTypeArray; ReturnType: XType; Body: XTree_ExprList): XTree_Function;
begin
  Result := XTree_Function.Create(FuncName, ArgNames, ByRef, ArgTypes, ReturnType, Body, FContext, FDocPos);
end;



{ TTypeIntrinsics - Intrinsic Function Generators }

function TTypeIntrinsics.GenerateHigh(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
begin
  if (Length(Args) > 0) or (SelfType.BaseType <> xtArray) then Exit(nil);

  Body := ExprList([
    IfStmt(
      BinOp(op_NEQ, SelfId, NilPointer),
      ReturnStmt(ArrayHighIndex(SelfId)),
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
begin
  if (Length(Args) > 0) or (SelfType.BaseType <> xtArray) then Exit(nil);

  ResultVar := Id('Result');

  Body := ExprList([
    VarDecl(['Result'], FContext.GetType(xtInt)),
    IfStmt(
      BinOp(op_NEQ, SelfId, NilPointer),
      Assign(ResultVar, ArrayLength(SelfId)),
      Assign(ResultVar, IntLiteral(0))
    ),
    ReturnStmt(ResultVar)
  ]);

  Result := FunctionDef('Len', [], nil, [], FContext.GetType(xtInt), Body);
  Result.SelfType := SelfType;
end;

function TTypeIntrinsics.GenerateCollect(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body, CleanupBody, LoopBody, InnerIfBody: XTree_ExprList;
  SelfIdent, FieldPtr, ElementNode: XTree_Node;
  i: Integer;
  FieldType: XType;
begin
  SelfIdent := SelfId;
  Body := ExprList();

  case SelfType.BaseType of
    xtArray:
    begin
      Body.List += ReturnIfNil(SelfIdent);

      // This logic block executes only if the refcount of the array is 0.
      CleanupBody := ExprList();

      // First, collect all child elements if they are managed.
      if XType_Array(SelfType).ItemType.IsManaged(FContext) then
      begin
        Body.List += VarDecl(['!i', '!r'], FContext.GetType(xtInt));
        ElementNode := XTree_Index.Create(SelfIdent, Id('!i'), FContext, FDocPos);
        ElementNode.FResType := XType_Array(SelfType).ItemType;

        // if element refcount becomes 0, call SetLen(0) on it
        InnerIfBody := ExprList([ MethodCall(ElementNode, 'SetLen', [IntLiteral(0)]) ]);

        // Emulates: !r := element[-2] - 1; element[-2] := !r; if !r = 0 then ...
        LoopBody := ExprList([
            Assign(Id('!r'), BinOp(op_Sub, ArrayRefcount(ElementNode), IntLiteral(1))),
            Assign(ArrayRefcount(ElementNode), Id('!r')),
            IfStmt(BinOp(op_EQ, Id('!r'), IntLiteral(0)), InnerIfBody, nil)
        ]);

        CleanupBody.List += ForLoop(
          Assign(Id('!i'), IntLiteral(0)),
          BinOp(op_LTE, Id('!i'), MethodCall(SelfId, 'High', [])),
          Assign(Id('!i'), BinOp(op_ADD, Id('!i'), IntLiteral(1))),
          LoopBody
        );
      end;

      // After children are handled, free the array itself by calling SetLen(0).
      CleanupBody.List += MethodCall(SelfIdent, 'SetLen', [IntLiteral(0)]);

      // The entire cleanup logic is wrapped in an 'if refcount = 0' check.
      Body.List += IfStmt(
        BinOp(op_EQ, ArrayRefcount(SelfIdent), IntLiteral(0)),
        CleanupBody,
        nil
      );
    end;
    xtRecord:
    begin
      for i := 0 to XType_Record(SelfType).FieldTypes.High do
      begin
        FieldType := XType_Record(SelfType).FieldTypes.Data[i];
        if FieldType.IsManaged(FContext) then
        begin
          FieldPtr := XTree_Field.Create(SelfIdent, Id(XType_Record(SelfType).FieldNames.Data[i]), FContext, FDocPos);
          FieldPtr.FResType := FieldType;

          case FieldType.BaseType of
            xtArray: Body.List += MethodCall(FieldPtr, 'SetLen', [IntLiteral(0)]);
            xtRecord: Body.List += MethodCall(FieldPtr, 'Collect', []);
            // Other managed types like String would be handled here if necessary.
          end;
        end;
      end;
    end;
  end;

  Result := FunctionDef('Collect', [], nil, [], nil, Body);
  Result.SelfType := SelfType;
end;

function TTypeIntrinsics.GenerateSetLen(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body, ShrinkBody, LoopBody: XTree_ExprList;
  NewSize, RawPtr: XTree_Node;
begin
  if (Length(Args) <> 1) or (SelfType.BaseType <> xtArray) then Exit(nil);

  Body := ExprList([
    VarDecl(['!high', '!newsize', '!i'], FContext.GetType(xtInt)),
    VarDecl(['!raw'], FContext.GetType(xtPointer))
  ]);

  // !high := High(Self)
  Body.List += Assign(Id('!high'), MethodCall(SelfId, 'High', []));

  // If shrinking a managed array, collect abandoned elements first.
  if XType_Array(SelfType).ItemType.IsManaged(FContext) then
  begin
    LoopBody := ExprList([
      MethodCall(XTree_Index.Create(SelfId, Id('!i'), FContext, FDocPos), 'Collect', [])
    ]);
    ShrinkBody := ExprList([
      ForLoop(
        Assign(Id('!i'), Id('NewLength')),
        BinOp(op_LTE, Id('!i'), Id('!high')),
        Assign(Id('!i'), BinOp(op_ADD, Id('!i'), IntLiteral(1))),
        LoopBody
      )
    ]);
    Body.List += IfStmt(BinOp(op_LT, Id('NewLength'), Id('!high')), ShrinkBody, nil);
  end;

  // Calculate required memory size. !newsize := (NewLength * ItemSize) + HeaderSize
  NewSize := BinOp(op_ADD,
    BinOp(op_MUL, Id('NewLength'), IntLiteral(XType_Array(SelfType).ItemType.Size)),
    IntLiteral(ARRAY_HEADER_SIZE)
  );

  // If NewLength > 0, use calculated size, otherwise size is 0.
  Body.List += IfStmt(
    BinOp(op_NEQ, Id('NewLength'), IntLiteral(0)),
    Assign(Id('!newsize'), NewSize),
    Assign(Id('!newsize'), IntLiteral(0))
  );

  // Get raw pointer (points before header) and reallocate.
  RawPtr := BinOp(op_SUB, SelfId, IntLiteral(ARRAY_HEADER_SIZE));
  Body.List += Assign(Id('!raw'), RawPtr);
  Body.List += Assign(Id('!raw'),
    Call('AllocArray', [Id('!raw'), Id('!newsize'), Id('NewLength'), IntLiteral(XType_Array(SelfType).ItemType.Size)])
  );

  // Update Self to point to the data portion of the new block, or nil if allocation failed/size is 0.
  Body.List += IfStmt(
    BinOp(op_NEQ, Id('!raw'), NilPointer),
    Assign(SelfAsPtr, BinOp(op_ADD, Id('!raw'), IntLiteral(ARRAY_HEADER_SIZE))),
    Assign(SelfAsPtr, NilPointer)
  );

  Result := FunctionDef('SetLen', ['NewLength'], [pbCopy], [FContext.GetType(xtInt)], nil, Body);
  Result.SelfType := SelfType;
end;

end.
