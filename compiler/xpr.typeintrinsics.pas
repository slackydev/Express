unit xpr.TypeIntrinsics;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Generates AST branches for intrinsic type methods.

  Generators that need compile-time type information (sizes, field names,
  disposal function hashes) use the AST builder helpers. Generators whose
  bodies are pure Express logic use Parse()

  Note: `collect` is generally disabled here, you have to manage your own heap
  allocated vars and temporaries. This means manual invoke .setlen and .free
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

const
  JIT_RC_STATE = '@opt(''jit:full; rangechecks:off'')';

const
  // Pure Express source strings for Parse-based generators.
  // Variable names must match the FunctionDef arg names below.

  SRC_CONTAINS =
    'var h := self.High()'                           + LineEnding +
    JIT_RC_STATE                                     + LineEnding +
    'for(var i := 0; i <= h; i += 1) do'             + LineEnding +
    '  if(self[i] = Value)then'                      + LineEnding +
    '    return True'                                + LineEnding +
    'return False'                                   + LineEnding;

  SRC_INDEXOF =
    'var h := self.High()'                           + LineEnding +
    JIT_RC_STATE                                     + LineEnding +
    'for(var i := 0; i <= h; i += 1) do'             + LineEnding +
    '  if(self[i] = Value)then'                      + LineEnding +
    '    return i'                                   + LineEnding +
    'return -1'                                      + LineEnding;

  SRC_DELETE =
    'if(self = nil) then return'                        + LineEnding +
    'var h := self.High()'                              + LineEnding +
    JIT_RC_STATE                                        + LineEnding +
    'for(var j := Index; j < h; j += 1) do'             + LineEnding +
    '  self[j] := self[j + 1]'                          + LineEnding +
    'self.SetLen(self.Len() - 1)'                       + LineEnding;

  SRC_INSERT =
    'if(self = nil) then return'                        + LineEnding +
    'self.SetLen(self.Len() + 1)'                       + LineEnding +
    'var h := self.High()'                              + LineEnding +
    JIT_RC_STATE                                        + LineEnding +
    'for(var j := h; j > Index; j -= 1) do'             + LineEnding +
    '  self[j] := self[j - 1]'                          + LineEnding +
    'self[Index] := Value'                              + LineEnding;

  SRC_REMOVE =
    'var i := self.IndexOf(Value)'                      + LineEnding +
    'if(i >= 0) then'                                   + LineEnding +
    '  self.Delete(i)'                                  + LineEnding;

  SRC_REVERSE =
    'if(self = nil) then return'                        + LineEnding +
    'var l := self.Len()'                               + LineEnding +
    JIT_RC_STATE                                        + LineEnding +
    'for(var lo := 0; lo < l / 2; lo += 1) do'          + LineEnding +
    '  var hi := (l-1) - lo'                            + LineEnding +
    '  var tmp := self[lo]'                             + LineEnding +
    '  self[lo] := self[hi]'                            + LineEnding +
    '  self[hi] := tmp'                                 + LineEnding;

  SRC_SORT =
    'if(self = nil) then return'                        + LineEnding +
    'var local := self;'                                + LineEnding +
    'var l := local.Len()'                              + LineEnding +
    'var gaps: array of Int'                            + LineEnding +
    'gaps := [3735498,1769455,835387,392925,184011,85764,39744,18298,8359,3785,1695,749,326,138,57,23,9,4,1]' + LineEnding +
    JIT_RC_STATE                                        + LineEnding +
    'for(var gi := 0; gi <= gaps.High(); gi += 1) do'   + LineEnding +
    '  var gap := gaps[gi]'                             + LineEnding +
    '  if(gap >= l) then continue'                      + LineEnding +
    '  for(var i := gap; i < l; i += 1) do'             + LineEnding +
    '    var key := local[i]'                           + LineEnding +
    '    var j := i - gap'                              + LineEnding +
    '    while(j >= 0 and local[j] > key) do'           + LineEnding +
    '      local[j + gap] := local[j]'                  + LineEnding +
    '      j -= gap'                                    + LineEnding +
    '    local[j + gap] := key'                         + LineEnding +
    // free locals, collect is disabled for internals.
    'gaps.SetLen(0)'                                    + LineEnding +
    'local.SetLen(0)'                                   + LineEnding;

  // minimal value in JIT because inner functioncall.
  // leaving super instructions ON as it may give SOME value.
  SRC_SORT_CMP =
    'if(self = nil) then return'                        + LineEnding +
    'var gaps: array of Int'                            + LineEnding +
    'gaps := [3735498,1769455,835387,392925,184011,85764,39744,18298,8359,3785,1695,749,326,138,57,23,9,4,1]' + LineEnding +
    JIT_RC_STATE                                        + LineEnding +
    'for(var gi := 0; gi <= gaps.High(); gi += 1) do'   + LineEnding +
    '  var gap := gaps[gi]'                             + LineEnding +
    '  if(gap >= self.Len()) then continue'             + LineEnding +
    '  var h := self.High()'                            + LineEnding +
    '  for(var i := gap; i <= h; i += 1) do'            + LineEnding +
    '    var key := self[i]'                            + LineEnding +
    '    var j := i - gap'                              + LineEnding +
    '    while(j >= 0 and Cmp(self[j], key) > 0) do'    + LineEnding +
    '      self[j + gap] := self[j]'                    + LineEnding +
    '      j -= gap'                                    + LineEnding +
    '    self[j + gap] := key'                          + LineEnding +
    'gaps.SetLen(0)'                                    + LineEnding;

  SRC_SORT_WEIGHTED =
    'if(self = nil) then return'                               + LineEnding +
    'var local := self;'                                       + LineEnding +
    'var l := local.Len()'                                     + LineEnding +
    'var gaps: array of Int'                                   + LineEnding +
    'gaps := [3735498,1769455,835387,392925,184011,85764,39744,18298,8359,3785,1695,749,326,138,57,23,9,4,1]' + LineEnding +
    JIT_RC_STATE                                               + LineEnding +
    'for(var gi := 0; gi <= gaps.High(); gi += 1) do'          + LineEnding +
    '  var gap := gaps[gi]'                                    + LineEnding +
    '  if(gap >= l) then continue'                             + LineEnding +
    '  for(var i := gap; i < l; i += 1) do'                    + LineEnding +
    '    var key  := local[i]'                                 + LineEnding +
    '    var keyW := weights[i]'                               + LineEnding +
    '    var j    := i - gap'                                  + LineEnding +
    '    while(j >= 0 and Weights[j] > keyW) do'               + LineEnding +
    '      local[j + gap]   := local[j]'                       + LineEnding +
    '      weights[j + gap] := weights[j]'                     + LineEnding +
    '      j -= gap'                                           + LineEnding +
    '    local[j + gap]    := key'                             + LineEnding +
    '    weights[j + gap] := keyW'                             + LineEnding +
    'gaps.SetLen(0)'                                    + LineEnding +
    'local.SetLen(0)'                                   + LineEnding;


  SRC_CONCAT =
    'if(self = nil) then return Other.Copy()'                      + LineEnding +
    'if(Other = nil) then return self.Copy()'                      + LineEnding +
    'var lenA := self.Len()'                                       + LineEnding +
    'var lenB := Other.Len()'                                      + LineEnding +
    'result.SetLen(lenA + lenB)'                                   + LineEnding +
    'move(addr(self[0]), addr(result[0]), SizeOf(self[0])*lenA)'   + LineEnding +
    'move(addr(other[0]), addr(result[lenA]), SizeOf(other[0])*lenB)' + LineEnding;

  SRC_SUM =
    'if(self = nil) then return 0'           + LineEnding +
    'var s := self[0]'                       + LineEnding +
    'var h := self.High()'                   + LineEnding +
    JIT_RC_STATE                             + LineEnding +
    'for(var i := 1; i <= h; i += 1) do'     + LineEnding +
    '  s += self[i]'                         + LineEnding +
    'return s'                               + LineEnding;

  SRC_MIN =
    'if(self = nil) then return 0'           + LineEnding +
    'Result := self[0]'                      + LineEnding +
    'var h := self.High()'                   + LineEnding +
    JIT_RC_STATE                             + LineEnding +
    'for(var i := 1; i <= h; i += 1) do'     + LineEnding +
    '  if(self[i] < Result) then'            + LineEnding +
    '    Result := self[i]'                  + LineEnding +
    'return Result'                          + LineEnding;

  SRC_MAX =
    'if(self = nil) then return 0'           + LineEnding +
    'Result := self[0]'                      + LineEnding +
    'var h := self.High()'                   + LineEnding +
    JIT_RC_STATE                             + LineEnding +
    'for(var i := 1; i <= h; i += 1) do'     + LineEnding +
    '  if(self[i] > Result) then'            + LineEnding +
    '    Result := self[i]'                  + LineEnding +
    'return Result'                          + LineEnding;

  SRC_MEAN =
    'if(self = nil) then return 0.0'                            + LineEnding +
    'if(self.Len() = 0) then return 0.0'                        + LineEnding +
    'return Double(self.Sum()) / Double(self.Len())'            + LineEnding;

  SRC_VARIANCE =
    'if(self = nil) then return 0.0'                + LineEnding +
    'if(self.Len() = 0) then return 0.0'            + LineEnding +
    'var mean := self.Mean()'                       + LineEnding +
    'var sum := 0.0'                                + LineEnding +
    'var h := self.High()'                          + LineEnding +
    JIT_RC_STATE                                    + LineEnding +
    'for(var i := 0; i <= h; i += 1) do'            + LineEnding +
    '  var d := Double(self[i]) - mean'             + LineEnding +
    '  sum += d * d'                                + LineEnding +
    'return sum / Double(self.Len())'               + LineEnding;

  SRC_STDDEV =
    'return Sqrt(self.Variance())' + LineEnding;

  SRC_MEDIAN =
    'if(self = nil) then return 0.0'                               + LineEnding +
    'if(self.Len() = 0) then return 0.0'                           + LineEnding +
    'var tmp := self.Copy()'                                       + LineEnding +
    'tmp.Sort()'                                                   + LineEnding +
    'var mid := tmp.Len() / 2'                                     + LineEnding +
    'if(tmp.Len() % 2 = 0) then'                                   + LineEnding +
    '  return (Double(tmp[mid - 1]) + Double(tmp[mid])) / 2.0'     + LineEnding +
    'return Double(tmp[mid])'                                      + LineEnding;

  SRC_SORTED =
    'Result := self.Copy()'  + LineEnding +
    'Result.Sort()'          + LineEnding;

  SRC_SORTED_CMP =
    'Result := self.Copy()'  + LineEnding +
    'Result.Sort(Cmp)'       + LineEnding;

  SRC_SORTED_WEIGHTED =
    'var w := Weights.Copy()' + LineEnding +
    'Result := self.Copy()'   + LineEnding +
    'Result.Sort(w)'          + LineEnding +
    'return Result'           + LineEnding;

  SRC_REVERSED =
    'Result := self.Copy()'  + LineEnding +
    'Result.Reverse()'       + LineEnding;

  SRC_CONCAT_ITEM =
    'if(self = nil) then'                                                    + LineEnding +
    '  result.SetLen(1)'                                                     + LineEnding +
    '  result[0] := Item'                                                    + LineEnding +
    '  return result'                                                        + LineEnding +
    'var l := self.Len()'                                                    + LineEnding +
    'result.SetLen(l + 1)'                                                   + LineEnding +
    'move(addr(self[0]), addr(result[0]), SizeOf(self[0]) * l)'              + LineEnding +
    'result[l] := Item'                                                      + LineEnding;

type
  TTypeIntrinsics = class(TIntrinsics)
  public
    {** AST Node Factory Helpers **}
    function IntLiteral(Value: Int64): XTree_Int;
    function FloatLiteral(Value: Double): XTree_Float;
    function StringLiteral(const Value: string): XTree_String;
    function NilPointer: XTree_Int;
    function Id(const Name: string): XTree_Identifier;
    function SelfId: XTree_Identifier;
    function SelfAsPtr: XTree_Identifier;
    function TypeCast(NewType: XType; Expr: XTree_Node): XTree_TypeCast;
    function AddrOf(ANode: XTree_Node): XTree_UnaryOp;
    function Deref(APointerExpr: XTree_Node; AType: XType=nil): XTree_UnaryOp;
    function BinOp(Op: EOperator; Left, Right: XTree_Node): XTree_BinaryOp;
    function Assign(LHS, RHS: XTree_Node): XTree_Assign;
    function Call(FuncName: string; Args: XNodeArray): XTree_Invoke;
    function MethodCall(Target: XTree_Node; FuncName: string; Args: XNodeArray): XTree_Invoke;
    function Index(ArrExpr, IdxExpr: XTree_Node): XTree_Index;
    function ReturnStmt(Expr: XTree_Node = nil): XTree_Return;
    function ExprList(Nodes: XNodeArray = nil): XTree_ExprList;
    function VarDecl(Names: TStringArray; VarType: XType; AExpr: XTree_Node=nil): XTree_VarDecl;
    function IfStmt(ACond: XTree_Node; AThenBody, AElseBody: XTree_Node): XTree_If;
    function ForLoop(Init, Cond, Inc: XTree_Node; Body: XTree_ExprList): XTree_For;
    function WhileLoop(Cond: XTree_Node; Body: XTree_ExprList): XTree_While;

    {** Common Action Helpers **}
    function ArrayHeaderField(ArrayExpr: XTree_Node; Idx: Integer): XTree_Index;
    function ArrayRefcount(ArrayExpr: XTree_Node; BaseType:EExpressBaseType): XTree_Node;
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

    // ordinal helpers Int.Next(), Enum.Prev()
    function GenerateNext(SelfType: XType; Args: array of XType): XTree_Function;
    function GeneratePrev(SelfType: XType; Args: array of XType): XTree_Function;

    function GenerateSetLen1D(SelfType: XType; ArgName: string): XTree_Function;
    function GenerateSetLen(SelfType: XType; Args: array of XType): XTree_Function;

    function GenerateAppend(SelfType: XType; Args: array of XType): XTree_Function;
    function GeneratePop(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateSlice(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateCopy(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateEq(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateNeq(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateContains(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateIndexOf(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateDelete(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateInsert(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateRemove(SelfType: XType; Args: array of XType): XTree_Function;

    function GenerateReverse(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateReversed(SelfType: XType; Args: array of XType): XTree_Function;

    function GenerateSort(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateSortOneArg(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateSortWeighted(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateSorted(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateSortedCmp(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateSortedWeighted(SelfType: XType; Args: array of XType): XTree_Function;

    function GenerateConcat(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateConcatItem(SelfType: XType; Args: array of XType): XTree_Function;

    function GenerateSum(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateMin(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateMax(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateMean(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateVariance(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateStdDev(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateMedian(SelfType: XType; Args: array of XType): XTree_Function;
  end;

implementation

uses
  xpr.Parser,
  xpr.MagicIntrinsics;

const
  ARRAY_HEADER_SIZE: Int32 = 2 * SizeOf(SizeInt);

constructor TTypeIntrinsics.Create(AContext: TCompilerContext; ADocPos: TDocPos);
begin
  inherited Create;
  FContext := AContext;
  FDocPos  := ADocPos;
end;

{ ============================================================ }
{ AST Node Factory Helpers                                      }
{ ============================================================ }

function TTypeIntrinsics.IntLiteral(Value: Int64): XTree_Int;
begin
  Result := XTree_Int.Create(IntToStr(Value), FContext, FDocPos);
  Result.FResType := FContext.GetType(xtInt);
  Result.Value := Value;
end;

function TTypeIntrinsics.FloatLiteral(Value: Double): XTree_Float;
begin
  Result := XTree_Float.Create(FloatToStr(Value), FContext, FDocPos);
  Result.FResType := FContext.GetType(xtDouble);
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

function TTypeIntrinsics.TypeCast(NewType: XType; Expr: XTree_Node): XTree_TypeCast;
begin
  Result := XTree_TypeCast.Create(NewType, Expr, FContext, FDocPos)
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

function TTypeIntrinsics.Index(ArrExpr, IdxExpr: XTree_Node): XTree_Index;
begin
  Result := XTree_Index.Create(ArrExpr, IdxExpr, FContext, FDocPos);
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
  Result := XTree_VarDecl.Create(IdentList, AExpr, VarType, False, FContext, FDocPos);
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

function TTypeIntrinsics.WhileLoop(Cond: XTree_Node; Body: XTree_ExprList): XTree_While;
begin
  Result := XTree_While.Create(Cond, Body, FContext, FDocPos);
end;

{ ============================================================ }
{ Common Action Helpers                                         }
{ ============================================================ }
function TTypeIntrinsics.ArrayHeaderField(ArrayExpr: XTree_Node; Idx: Integer): XTree_Index;
begin
  Result := XTree_Index.Create(ArrayExpr, IntLiteral(Idx), FContext, FDocPos);
  Result.FResType := FContext.GetType(xtInt);
  Result.ForceTypeSize := SizeOf(SizeInt);
end;

function TTypeIntrinsics.ArrayRefcount(ArrayExpr: XTree_Node; BaseType:EExpressBaseType): XTree_Node;
begin
  if BaseType = xtAnsiString then
    Result := Call('__astring_refcount', [ArrayExpr])
  else if BaseType = xtUnicodeString then
    Result := Call('__ustring_refcount', [ArrayExpr])
  else
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

function TTypeIntrinsics.FunctionDef(FuncName: string; ArgNames: TStringArray; ByRef: TPassArgsBy;
  ArgTypes: XTypeArray; ReturnType: XType; Body: XTree_ExprList): XTree_Function;
begin
  Result := XTree_Function.Create(FuncName, ArgNames, ByRef, ArgTypes, ReturnType, Body, FContext, FDocPos);
  Result.InternalFlags += [cfNoCollect];
end;

{ ============================================================ }
{ AST-built generators                                          }
{ ============================================================ }

function TTypeIntrinsics.GenerateRefcount(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
begin
  if SelfType = nil then Exit(nil);
  if (Length(Args) > 0) or not (SelfType.BaseType in XprRefcountedTypes) then Exit(nil);

  Body := ExprList([
    IfStmt(
      BinOp(op_NEQ, SelfId, NilPointer),
      ReturnStmt(ArrayRefcount(SelfId, SelfType.BaseType)),
      ReturnStmt(IntLiteral(0)))
  ]);

  Result := FunctionDef('_refcnt', [], nil, [], FContext.GetType(xtInt), Body);
  Result.SelfType := SelfType;
end;

function TTypeIntrinsics.GenerateHigh(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
  ReturnValueNode: XTree_Node;
begin
  if SelfType = nil then Exit(nil);
  if (Length(Args) > 0) or not ((SelfType is XType_Array) or (SelfType is XType_String)) then Exit(nil);

  if SelfType is XType_String then
    ReturnValueNode := BinOp(op_Sub, ArrayHighIndex(SelfId), IntLiteral(1))
  else
    ReturnValueNode := ArrayHighIndex(SelfId);

  Body := ExprList([
    IfStmt(
      BinOp(op_NEQ, SelfId, NilPointer),
      ReturnStmt(ReturnValueNode),
      ReturnStmt(IntLiteral(-1)))
  ]);

  Result := FunctionDef('High', [], nil, [], FContext.GetType(xtInt), Body);
  Result.SelfType := SelfType;
end;

function TTypeIntrinsics.GenerateLen(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
  LengthValueNode: XTree_Node;
begin
  if SelfType = nil then Exit(nil);
  if (Length(Args) > 0) or not ((SelfType is XType_Array) or (SelfType is XType_String)) then Exit(nil);

  if SelfType is XType_String then
    LengthValueNode := ArrayHighIndex(SelfId)
  else
    LengthValueNode := ArrayLength(SelfId);

  Body := ExprList([
    IfStmt(
      BinOp(op_NEQ, SelfId, NilPointer),
      Assign(Id('Result'), LengthValueNode),
      Assign(Id('Result'), IntLiteral(0))),
    ReturnStmt(Id('Result'))
  ]);

  Result := FunctionDef('Len', [], nil, [], FContext.GetType(xtInt), Body);
  Result.SelfType := SelfType;
end;

function TTypeIntrinsics.GenerateToStr(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
  ReturnNode, ConcatNode: XTree_Node;
  StringType, IntType: XType;
  RecType: XType_Record;
  ClassT: XType_Class;
  i: Int32;
  EnumT: XType_Enum;
  Branches: TCaseBranchArray;
begin
  if SelfType = nil then Exit(nil);
  if Length(Args) > 0 then Exit(nil);

  Body := ExprList();
  StringType := FContext.GetType(xtAnsiString);
  IntType    := FContext.GetType(xtInt);

  case SelfType.BaseType of
    xtAnsiString, xtUnicodeString:
      ReturnNode := BinOp(op_Add, StringLiteral(''''),
                     BinOp(op_Add, SelfId(), StringLiteral('''')));

    xtAnsiChar, xtUnicodeChar:
      ReturnNode := BinOp(op_Add, StringLiteral(''''),
                     BinOp(op_Add, SelfId(), StringLiteral('''')));

    xtInt8..xtUInt64:
    begin
      if SelfType is XType_Enum then
      begin
        EnumT := XType_Enum(SelfType);
        SetLength(Branches, Length(EnumT.MemberNames));

        for i := 0 to High(EnumT.MemberNames) do
        begin
          Branches[i].Labels.Init([IntLiteral(EnumT.MemberValues[i])]);
          Branches[i].Body := ReturnStmt(StringLiteral(EnumT.MemberNames[i]));
        end;

        Body.List += XTree_Case.Create(
          SelfId(),
          // options
          Branches,
          // fallback: out-of-range
          ExprList([
            ReturnStmt(
              BinOp(op_Add,
                    BinOp(op_Add,StringLiteral('Invalid('),Call('IntToStr', [SelfId()])),
                    StringLiteral(')')
              ))]),
          FContext, FDocPos
        );

        Result := FunctionDef('ToStr', [], nil, [], StringType, Body);
        Result.SelfType := SelfType;
        Result.InternalFlags := [];
        Exit;
      end
      else
        ReturnNode := Call('IntToStr', [SelfId()]);
    end;

    xtSingle, xtDouble:
      ReturnNode := Call('FloatToStr', [SelfId()]);

    xtBool:
      ReturnNode := XTree_IfExpr.Create(
        SelfId(), StringLiteral('True'), StringLiteral('False'),
        FContext, FDocPos);

    xtPointer:
      ReturnNode := Call('PtrToStr', [SelfId()]);

    xtArray:
    begin
      Body.List += IfStmt(BinOp(op_EQ, SelfId(), NilPointer()),
        ReturnStmt(StringLiteral('[]')), nil);
      Body.List += VarDecl(['!r'], StringType, StringLiteral('['));
      Body.List += ForLoop(
        VarDecl(['!i'], IntType, IntLiteral(0)),
        BinOp(op_LTE, Id('!i'), MethodCall(SelfId(), 'High', [])),
        Assign(Id('!i'), BinOp(op_ADD, Id('!i'), IntLiteral(1))),
        ExprList([
          IfStmt(BinOp(op_GT, Id('!i'), IntLiteral(0)),
            Assign(Id('!r'), BinOp(op_ADD, Id('!r'), StringLiteral(', '))),
            nil),
          Assign(Id('!r'), BinOp(op_ADD, Id('!r'),
            MethodCall(Index(SelfId(), Id('!i')), 'ToStr', [])))
        ])
      );
      Body.List += Assign(Id('!r'), BinOp(op_ADD, Id('!r'), StringLiteral(']')));
      Body.List += ReturnStmt(Id('!r'));
      Result := FunctionDef('ToStr', [], nil, [], StringType, Body);
      Result.SelfType := SelfType;
      Result.InternalFlags := [];
      Exit;
    end;

    xtRecord:
    begin
      RecType := SelfType as XType_Record;
      if RecType.FieldNames.Size = 0 then
        ReturnNode := StringLiteral('[]')
      else
      begin
        ConcatNode := StringLiteral('(');
        for i := 0 to RecType.FieldNames.High do
        begin
          if i > 0 then
            ConcatNode := BinOp(op_ADD, ConcatNode, StringLiteral(', '));
          ConcatNode := BinOp(op_ADD, ConcatNode, MethodCall(
              XTree_Field.Create(SelfId(), Id(RecType.FieldNames.Data[i]), FContext, FDocPos),
              'ToStr', []
            )
          );
        end;
        ReturnNode := BinOp(op_ADD, ConcatNode, StringLiteral(')'));
      end;
    end;

    xtClass:
    begin
      ClassT := SelfType as XType_Class;
      Body.List += IfStmt(BinOp(op_EQ, SelfId(), NilPointer()),
        ReturnStmt(StringLiteral(ClassT.Name + '[nil]')), nil);
      if ClassT.FieldNames.Size = 0 then
        ReturnNode := StringLiteral(ClassT.Name + '[]')
      else
      begin
        ConcatNode := StringLiteral(ClassT.Name + '[');
        for i := 0 to ClassT.FieldNames.High do
        begin
          if i > 0 then
            ConcatNode := BinOp(op_ADD, ConcatNode, StringLiteral(', '));
          ConcatNode := BinOp(op_ADD, ConcatNode,
            StringLiteral(ClassT.FieldNames.Data[i] + ': '));
          ConcatNode := BinOp(op_ADD, ConcatNode,
            MethodCall(
              XTree_Field.Create(SelfId(), Id(ClassT.FieldNames.Data[i]), FContext, FDocPos),
              'ToStr', []));
        end;
        ReturnNode := BinOp(op_ADD, ConcatNode, StringLiteral(']'));
      end;
    end;

  else
    ReturnNode := StringLiteral('<' + SelfType.ToString() + '>');
  end;

  Body.List += ReturnStmt(ReturnNode);
  Result := FunctionDef('ToStr', [], nil, [], StringType, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GeneratePtrAssign(SelfType: XType; Args: array of XType; FuncName: string = ''): XTree_Function;
var
  Body: XTree_ExprList;
  PType: XType_Pointer;
begin
  if (SelfType <> nil) or (Length(Args) <> 2) then Exit(nil);
  PType := Args[0] as XType_Pointer;

  Body := ExprList();
  Body.List += Parse('__main__', FContext,
    'if(right^ != nil)then'  + LineEnding +
    '  left^ := right^;'     + LineEnding
  );

  if FuncName = '' then
    Result := FunctionDef('__passign__', ['left','right'], [pbRef,pbRef], [PType,PType], nil, Body)
  else
    Result := FunctionDef(FuncName, ['left','right'], [pbRef,pbRef], [PType,PType], nil, Body);
end;

function TTypeIntrinsics.GeneratePtrDispose(SelfType: XType; Args: array of XType; FuncName: string = ''): XTree_Function;
var
  Body: XTree_ExprList;
  PType: XType_Pointer;
begin
  if (SelfType <> nil) or (Length(Args) <> 1) then Exit(nil);
  PType := Args[0] as XType_Pointer;
  Body := ExprList();

  case XType_Pointer(Args[0]).ItemType.BaseType of
    xtPointer:
      Body.List += Call('freemem', [Deref(Id('ptr'))]);
    else
      Body.List += XTree_Default.Create(nil, [Deref(Id('ptr'))], FContext, FDocPos);
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
  RecType: XType_Record;
  i: Int32;
begin
  if SelfType = nil then Exit(nil);

  Body := ExprList();
  SelfIdent := SelfId();

  case SelfType.BaseType of
    xtRecord:
    begin
      RecType := SelfType as XType_Record;
      for i := 0 to RecType.FieldTypes.High do
        if RecType.FieldTypes.Data[i].IsManagedType(FContext) then
          Body.List += MethodCall(
            XTree_Field.Create(SelfId(), Id(RecType.FieldNames.Data[i]), FContext, FDocPos),
            'Collect', []);
    end;

    xtArray:
      Body.List += MethodCall(SelfIdent, 'SetLen', [IntLiteral(0)]);

    xtAnsiString, xtUnicodeString:
      Body.List += MethodCall(SelfIdent, 'SetLen', [IntLiteral(0)]);

    xtClass:
    begin
      Body.List += ReturnIfNil(SelfIdent);
      Body.List += VarDecl(['HeaderSize'], FContext.GetType(xtInt), IntLiteral(2 * SizeOf(SizeInt)));
      Body.List += VarDecl(['raw'], FContext.GetType(xtPointer),
        BinOp(op_sub, SelfAsPtr, Id('HeaderSize')));

      Body.List += IfStmt(
        BinOp(op_EQ, Deref(Id('raw'), FContext.GetType(xtInt)), IntLiteral(0)),
        ReturnStmt(), nil
      );

      Body.List += IfStmt(
        BinOp(op_EQ, Deref(Id('raw'), FContext.GetType(xtInt)), IntLiteral(1)),
        ExprList([
          Call('__atomic_dec_ref', [SelfIdent]),
          MethodCall(SelfIdent, 'Free', []),
          MethodCall(SelfIdent, 'Default', []),
          VarDecl(['block_start'], FContext.GetType(xtPointer),
            BinOp(op_sub, Id('raw'), IntLiteral(SizeOf(SizeInt)))),
          Call('freemem', [Id('block_start')])
        ]),
        // use atomic decref:
        Call('__atomic_dec_ref', [SelfIdent])
        // OLD PATH:
        //Assign(
        //  Deref(Id('raw'), FContext.GetType(xtInt)),
        //  BinOp(op_sub, Deref(Id('raw'), FContext.GetType(xtInt)), IntLiteral(1))
        //)
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
  if SelfType = nil then Exit(nil);
  Body := ExprList([XTree_Default.Create(nil, [SelfId()], FContext, FDocPos)]);
  Result := FunctionDef('Default', [], nil, [], nil, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateSetLen1D(SelfType: XType; ArgName: string): XTree_Function;
var
  Body: XTree_ExprList;
  ItemType, PItemType: XType;
  TDisposalProto, TCopyProto: XType_Method;
begin
  if SelfType = nil then Exit(nil);
  if not (SelfType is XType_Array) then Exit(nil);

  Body := ExprList();

  if SelfType is XType_String then
  begin
    if SelfType.BaseType = xtAnsiString then
      Body.List += Call('__astring_setlength', [Id('self'), Id(ArgName)])
    else if SelfType.BaseType = xtUnicodeString then
      Body.List += Call('__ustring_setlength', [Id('self'), Id(ArgName)]);
  end else
  begin
    ItemType  := (SelfType as XType_Array).ItemType;
    PItemType := XType_Pointer.Create(ItemType);
    FContext.AddManagedType(PItemType);

    TDisposalProto := XType_Method.Create('_TDisposalMethod', [PItemType], [pbCopy], nil, False);
    FContext.AddManagedType(TDisposalProto);

    TCopyProto := XType_Method.Create('_TCopyMethod', [PItemType, PItemType], [pbRef, pbRef], nil, False);
    FContext.AddManagedType(TCopyProto);

    Body.List += VarDecl(['dispose'], TDisposalProto);
    Body.List += VarDecl(['copy'], TCopyProto);

    if ItemType.IsManagedType(FContext) then
    begin
      FContext.GenerateIntrinsics('__pdispose__', [PItemType], nil, '__pdispose__' + TDisposalProto.Hash());
      FContext.GenerateIntrinsics('__passign__', [PItemType, PItemType], nil, '__passign__' + TCopyProto.Hash());
      Body.List += Assign(Id('dispose'), Id('__pdispose__' + TDisposalProto.Hash()));
      Body.List += Assign(Id('copy'),    Id('__passign__'  + TCopyProto.Hash()));
    end;

    Body.List += VarDecl(['raw'], FContext.GetType(xtPointer), SelfAsPtr());
    Body.List += Assign(
      XTree_TypeCast.Create(
        FContext.GetType(xtPointer),
        SelfId(),
        FContext,
        FDocPos
      ),
      XTree_Invoke.Create(
        Id('__internal::__array_setlength'),
        [Id('raw'), Id(ArgName), IntLiteral(ItemType.Size), Id('dispose'), Id('copy')],
        FContext, FDocPos)
      );
  end;

  Result := FunctionDef('SetLen', [ArgName], [pbCopy], [FContext.GetType(xtInt)], nil, Body);
  Result.SelfType := SelfType;
end;

function TTypeIntrinsics.GenerateSetLen(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
  IntType: XType;
  NumDims, i: Int32;
  ArgNames: TStringArray;
  ArgPass: TPassArgsBy;
  ArgTypes: XTypeArray;
  InnerCallArgs: XNodeArray;
  LoopBody: XTree_ExprList;
begin
  if SelfType = nil then Exit(nil);
  if not (SelfType is XType_Array) then Exit(nil);

  NumDims := Length(Args);
  if NumDims = 0 then Exit(nil);
  for i := 0 to NumDims - 1 do
    if not (Args[i].BaseType in XprIntTypes) then Exit(nil);

  if NumDims = 1 then
    Exit(GenerateSetLen1D(SelfType, 'NewLength'));

  IntType := FContext.GetType(xtInt);
  SetLength(ArgNames, NumDims);
  SetLength(ArgPass,  NumDims);
  SetLength(ArgTypes, NumDims);
  for i := 0 to NumDims - 1 do
  begin
    ArgNames[i] := 'Dim' + IntToStr(i);
    ArgPass[i]  := pbCopy;
    ArgTypes[i] := IntType;
  end;

  Body := ExprList();
  Body.List += MethodCall(SelfId(), 'SetLen', [Id('Dim0')]);

  SetLength(InnerCallArgs, NumDims - 1);
  for i := 1 to NumDims - 1 do
    InnerCallArgs[i - 1] := Id('Dim' + IntToStr(i));

  LoopBody := ExprList([
    MethodCall(Index(SelfId(), Id('!sli')), 'SetLen', InnerCallArgs)
  ]);

  Body.List += ForLoop(
    VarDecl(['!sli'], IntType, IntLiteral(0)),
    BinOp(op_LTE, Id('!sli'), MethodCall(SelfId(), 'High', [])),
    Assign(Id('!sli'), BinOp(op_ADD, Id('!sli'), IntLiteral(1))),
    LoopBody
  );

  Result := FunctionDef('SetLen', ArgNames, ArgPass, ArgTypes, nil, Body);
  Result.SelfType := SelfType;
end;

// Append/Pop/Slice/Copy: signature needs ItemType from Pascal but body uses Parse

function TTypeIntrinsics.GenerateAppend(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
  ItemType: XType;
begin
  if SelfType = nil then Exit(nil);
  if not (SelfType is XType_Array) or (SelfType is XType_String) then Exit(nil);
  if Length(Args) <> 1 then Exit(nil);

  ItemType := (SelfType as XType_Array).ItemType;
  Body := ExprList();
  Body.List += Parse('__internal__', FContext,
    'self.SetLen(self.Len() + 1)' + LineEnding +
    'self[self.High()] := Value'  + LineEnding);

  Result := FunctionDef('Append', ['Value'], [pbCopy], [ItemType], nil, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GeneratePop(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
  ItemType: XType;
begin
  if SelfType = nil then Exit(nil);
  if not (SelfType is XType_Array) or (SelfType is XType_String) then Exit(nil);
  if Length(Args) <> 0 then Exit(nil);

  ItemType := (SelfType as XType_Array).ItemType;
  Body := ExprList();
  Body.List += Parse('__internal__', FContext,
    'Result := self[self.High()]'  + LineEnding +
    'self.SetLen(self.Len() - 1)'  + LineEnding +
    'return Result'                + LineEnding);

  Result := FunctionDef('Pop', [], nil, [], ItemType, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateSlice(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
  IntType: XType;
  ArgNames: TStringArray;
  ArgPass: TPassArgsBy;
  ArgTypes: XTypeArray;
  ToLine: string;
  a: XType;
begin
  if SelfType = nil then Exit(nil);
  if not (SelfType is XType_Array) or (SelfType is XType_String) then Exit(nil);
  if not (Length(Args) in [1, 2]) then Exit(nil);
  for a in Args do
    if not (a.BaseType in XprIntTypes) then Exit(nil);

  IntType := FContext.GetType(xtInt);

  if Length(Args) = 1 then
  begin
    ArgNames := ['Start'];
    ArgPass  := [pbCopy];
    ArgTypes := [IntType];
    ToLine   := 'var len := self.Len() - Start';
  end else
  begin
    ArgNames := ['Start', 'Stop'];
    ArgPass  := [pbCopy, pbCopy];
    ArgTypes := [IntType, IntType];
    ToLine   := 'var len := Stop - Start';
  end;

  Body := ExprList();
  Body.List += Parse('__internal__', FContext,
    'if(self = nil) then return nil'                  + LineEnding +
    ToLine                                            + LineEnding +
    'result.SetLen(len)'                              + LineEnding +
    'move(addr(self[Start]), addr(result[0]), SizeOf(self[0])*len)'+ LineEnding);

  Result := FunctionDef('Slice', ArgNames, ArgPass, ArgTypes, SelfType, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [cfNoCollect];
end;

function TTypeIntrinsics.GenerateCopy(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
begin
  if SelfType = nil then Exit(nil);
  if not (SelfType is XType_Array) or (SelfType is XType_String) then Exit(nil);
  if Length(Args) <> 0 then Exit(nil);

  Body := ExprList();
  Body.List += Parse('__internal__', FContext,
    'if(self = nil) then return nil'                          + LineEnding +
    'result.SetLen(self.Len())'                               + LineEnding +
    'var l := self.Len()'                                     + LineEnding +
    'move(addr(self[0]), addr(result[0]), SizeOf(self[0])*l)' + LineEnding
  );

  Result := FunctionDef('Copy', [], nil, [], SelfType, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [cfNoCollect];
end;

{ ============================================================ }
{ Parse-based generators - pure Express logic                   }
{ ============================================================ }

function IsNumericArray(SelfType: XType): Boolean;
begin
  Result := (SelfType is XType_Array) and
            not (SelfType is XType_String) and
            ((SelfType as XType_Array).ItemType.BaseType in XprNumericTypes);
end;

function IsPlainArray(SelfType: XType; const Args: array of XType; ArgCount: Int32): Boolean;
begin
  Result := (SelfType <> nil) and
            (SelfType is XType_Array) and
            not (SelfType is XType_String) and
            (Length(Args) = ArgCount);
end;

function TTypeIntrinsics.GenerateEq(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList;
begin
  if SelfType = nil then Exit(nil);
  if Length(Args) <> 1 then Exit(nil);
  if not (SelfType is XType_Array) then Exit(nil);

  Body := ExprList();
  Body.List += Parse('__internal__', FContext,
    'if(self.Len() != Other.Len()) then return False'  + LineEnding +
    'var h := self.High()'                             + LineEnding +
    JIT_RC_STATE                                       + LineEnding +
    'for(var i:=0; i<=h; i += 1) do'                   + LineEnding +
    '  if(self[i] != Other[i]) then'                   + LineEnding +
    '    return False'                                 + LineEnding +
    'return True'                                      + LineEnding);

  Result := FunctionDef('__eq__', ['Other'], [pbCopy], [SelfType],
    FContext.GetType(xtBool), Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateNeq(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList;
begin
  if SelfType = nil then Exit(nil);
  if Length(Args) <> 1 then Exit(nil);
  if not (SelfType is XType_Array) then Exit(nil);

  Body := ExprList();
  Body.List += Parse('__internal__', FContext,
    'if(self.Len() != Other.Len()) then return False'  + LineEnding +
    'var h := self.High()'                             + LineEnding +
    JIT_RC_STATE                                       + LineEnding +
    'for(var i:=0; i<=h; i += 1) do'                   + LineEnding +
    '  if(self[i] != Other[i]) then'                   + LineEnding +
    '    return True'                                  + LineEnding +
    'return False'                                     + LineEnding);

  Result := FunctionDef('__neq__', ['Other'], [pbCopy], [SelfType],
    FContext.GetType(xtBool), Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateContains(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList; ItemType: XType;
begin
  if not IsPlainArray(SelfType, Args, 1) then Exit(nil);
  ItemType := (SelfType as XType_Array).ItemType;
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_CONTAINS);
  Result := FunctionDef('Contains', ['Value'], [pbCopy], [ItemType],
    FContext.GetType(xtBool), Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateIndexOf(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList; ItemType: XType;
begin
  if not IsPlainArray(SelfType, Args, 1) then Exit(nil);
  ItemType := (SelfType as XType_Array).ItemType;
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_INDEXOF);
  Result := FunctionDef('IndexOf', ['Value'], [pbCopy], [ItemType],
    FContext.GetType(xtInt), Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateDelete(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList;
begin
  if not IsPlainArray(SelfType, Args, 1) then Exit(nil);
  if not (Args[0].BaseType in XprIntTypes) then Exit(nil);
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_DELETE);
  Result := FunctionDef('Delete', ['Index'], [pbCopy], [FContext.GetType(xtInt)], nil, Body);
  Result.SelfType := SelfType;
end;

function TTypeIntrinsics.GenerateInsert(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList; ItemType: XType;
begin
  if not IsPlainArray(SelfType, Args, 2) then Exit(nil);
  if not (Args[0].BaseType in XprIntTypes) then Exit(nil);
  ItemType := (SelfType as XType_Array).ItemType;
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_INSERT);
  Result := FunctionDef('Insert', ['Index', 'Value'], [pbCopy, pbCopy],
    [FContext.GetType(xtInt), ItemType], nil, Body);
  Result.SelfType := SelfType;
end;

function TTypeIntrinsics.GenerateRemove(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList; ItemType: XType;
begin
  if not IsPlainArray(SelfType, Args, 1) then Exit(nil);
  ItemType := (SelfType as XType_Array).ItemType;
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_REMOVE);
  Result := FunctionDef('Remove', ['Value'], [pbCopy], [ItemType], nil, Body);
  Result.SelfType := SelfType;
end;

function TTypeIntrinsics.GenerateReverse(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList;
begin
  if not IsPlainArray(SelfType, Args, 0) then Exit(nil);
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_REVERSE);
  Result := FunctionDef('Reverse', [], nil, [], nil, Body);
  Result.SelfType := SelfType;
end;

function TTypeIntrinsics.GenerateReversed(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList;
begin
  if not IsPlainArray(SelfType, Args, 0) then Exit(nil);
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_REVERSED);
  Result := FunctionDef('Reversed', [], nil, [], SelfType, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateSort(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList;
begin
  if not IsPlainArray(SelfType, Args, 0) then Exit(nil);
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_SORT);
  Result := FunctionDef('Sort', [], nil, [], nil, Body);
  Result.SelfType := SelfType;
end;

function TTypeIntrinsics.GenerateSortOneArg(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
  CmpMethod: XType_Method;
  CmpType: XType;
begin
  Result := nil;

  // Must be a plain array with exactly one argument
  if not IsPlainArray(SelfType, Args, 1) then Exit;

  CmpType  := Args[0];

  // weighted sort?
  if not((CmpType is XType_Lambda) or (CmpType is XType_Method)) then
    Exit(Self.GenerateSortWeighted(SelfType, Args));

  // Unwrap lambda or method
  if CmpType is XType_Lambda then
    CmpMethod := XType_Lambda(CmpType).FieldTypes.Data[0] as XType_Method
  else if CmpType is XType_Method then
    CmpMethod := XType_Method(CmpType)
  else
    Exit;

  // Reject nested functions with captured implied parameters.
  // They carry implied args that the intrinsic cannot supply from global scope.
  // User must use a lambda or a global-scope non-capturing function.
  // This only aplies to XType_Method, not lambda!
  if (Length(CmpMethod.Params) > CmpMethod.RealParamcount) and
     (CmpType is XType_Method) then
    Exit;

  if CmpMethod.RealParamcount <> 2 then Exit;

  // Validate: Cmp(<dont care>, <dont care>): numeric
  if CmpMethod.RealParamcount <> 2 then Exit;
  if (CmpMethod.ReturnType = nil) or
     not (CmpMethod.ReturnType.BaseType in XprSignedInts) then Exit;

  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_SORT_CMP);

  Result := FunctionDef('sort', ['Cmp'], [pbCopy], [CmpType], nil, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateSortWeighted(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
  WeightType: XType;
  WeightItemType: XType;
begin
  Result := nil;

  // self must be a plain array with exactly one argument
  if not IsPlainArray(SelfType, Args, 1) then Exit;

  WeightType := Args[0];

  // Weights must be an array (not string)
  if not (WeightType is XType_Array) then Exit;
  if WeightType is XType_String then Exit;

  WeightItemType := (WeightType as XType_Array).ItemType;

  // Weight element must be a comparable numeric or ordinal type
  if not (WeightItemType.BaseType in XprNumericTypes + XprOrdinalTypes) then Exit;

  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_SORT_WEIGHTED);

  Result := FunctionDef('sort', ['Weights'], [pbRef], [WeightType], nil, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateSorted(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList;
begin
  if not IsPlainArray(SelfType, Args, 0) then Exit(nil);
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_SORTED);
  Result := FunctionDef('Sorted', [], nil, [], SelfType, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateSortedCmp(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList; CmpType: XType;
begin
  if not IsPlainArray(SelfType, Args, 1) then Exit(nil);
  CmpType := Args[0];
  if not ((CmpType is XType_Lambda) or (CmpType is XType_Method)) then Exit(nil);
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_SORTED_CMP);
  Result := FunctionDef('Sorted', ['Cmp'], [pbCopy], [CmpType], SelfType, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateSortedWeighted(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList; WeightType: XType;
begin
  if not IsPlainArray(SelfType, Args, 1) then Exit(nil);
  WeightType := Args[0];
  if not (WeightType is XType_Array) or (WeightType is XType_String) then Exit(nil);
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_SORTED_WEIGHTED);
  Result := FunctionDef('Sorted', ['Weights'], [pbCopy], [WeightType], SelfType, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateConcatItem(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
  ItemType: XType;
begin
  Result := nil;
  if SelfType = nil then Exit;
  if not (SelfType is XType_Array) or (SelfType is XType_String) then Exit;
  if Length(Args) <> 1 then Exit;

  ItemType := (SelfType as XType_Array).ItemType;
  if not ItemType.Equals(Args[0]) then Exit;

  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_CONCAT_ITEM);

  Result := FunctionDef('Concat', ['Item'], [pbCopy], [ItemType], SelfType, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateConcat(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
  ItemType: XType;
begin
  Result := nil;
  if SelfType = nil then Exit;
  if not (SelfType is XType_Array) or (SelfType is XType_String) then Exit;
  if Length(Args) <> 1 then Exit;

  ItemType := (SelfType as XType_Array).ItemType;

  // Single-item overload: arr.Concat(scalar) — no intermediate array allocation
  if ItemType.Equals(Args[0]) then
  begin
    Result := GenerateConcatItem(SelfType, Args);
    Exit;
  end;

  // Array overload: arr.Concat(other_arr)
  if not (Args[0] is XType_Array) then Exit;
  if not SelfType.Equals(Args[0]) then Exit;

  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_CONCAT);
  Result := FunctionDef('Concat', ['Other'], [pbRef], [SelfType], SelfType, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateSum(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList; ItemType: XType;
begin
  if not IsNumericArray(SelfType) then Exit(nil);
  if Length(Args) <> 0 then Exit(nil);
  ItemType := (SelfType as XType_Array).ItemType;
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_SUM);
  Result := FunctionDef('Sum', [], nil, [], ItemType, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateMin(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList; ItemType: XType;
begin
  if not IsNumericArray(SelfType) then Exit(nil);
  if Length(Args) <> 0 then Exit(nil);
  ItemType := (SelfType as XType_Array).ItemType;
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_MIN);
  Result := FunctionDef('Min', [], nil, [], ItemType, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateMax(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList; ItemType: XType;
begin
  if not IsNumericArray(SelfType) then Exit(nil);
  if Length(Args) <> 0 then Exit(nil);
  ItemType := (SelfType as XType_Array).ItemType;
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_MAX);
  Result := FunctionDef('Max', [], nil, [], ItemType, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateMean(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList;
begin
  if not IsNumericArray(SelfType) then Exit(nil);
  if Length(Args) <> 0 then Exit(nil);
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_MEAN);
  Result := FunctionDef('Mean', [], nil, [], FContext.GetType(xtDouble), Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateVariance(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList;
begin
  if not IsNumericArray(SelfType) then Exit(nil);
  if Length(Args) <> 0 then Exit(nil);
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_VARIANCE);
  Result := FunctionDef('Variance', [], nil, [], FContext.GetType(xtDouble), Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateStdDev(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList;
begin
  if not IsNumericArray(SelfType) then Exit(nil);
  if Length(Args) <> 0 then Exit(nil);
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_STDDEV);
  Result := FunctionDef('StdDev', [], nil, [], FContext.GetType(xtDouble), Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateMedian(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList;
begin
  if not IsNumericArray(SelfType) then Exit(nil);
  if Length(Args) <> 0 then Exit(nil);
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_MEDIAN);
  Result := FunctionDef('Median', [], nil, [], FContext.GetType(xtDouble), Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GenerateNext(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body:     XTree_ExprList;
  EnumT:    XType_Enum;
  Branches: TCaseBranchArray;
  i:        Int32;
begin
  Result := nil;
  if SelfType = nil then Exit;
  if Length(Args) > 0 then Exit;        // Next takes no arguments
  if not (SelfType.BaseType in XprOrdinalTypes) then Exit;

  Body := ExprList();

  if SelfType is XType_Enum then
  begin
    EnumT := XType_Enum(SelfType);
    SetLength(Branches, Length(EnumT.MemberNames)-1);
    for i := 0 to High(EnumT.MemberNames) - 1 do
    begin
      Branches[i].Labels.Init([IntLiteral(EnumT.MemberValues[i])]);
      Branches[i].Body := ExprList([ReturnStmt(TypeCast(EnumT, IntLiteral(EnumT.MemberValues[i + 1])))]);
    end;

    Body.List += XTree_Case.Create(
      SelfId(),
      Branches,
      ExprList([ReturnStmt(TypeCast(EnumT, BinOp(op_ADD, SelfId(),IntLiteral(1))))]),  // out-of-range fallback
      FContext, FDocPos);
  end
  else
    // All other ordinals: self + 1
    Body.List += ReturnStmt(BinOp(op_ADD, SelfId(), IntLiteral(1)));

  Result := FunctionDef('Next', [], nil, [], SelfType, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

function TTypeIntrinsics.GeneratePrev(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body:     XTree_ExprList;
  EnumT:    XType_Enum;
  Branches: TCaseBranchArray;
  i:        Int32;
begin
  Result := nil;
  if SelfType = nil then Exit;
  if Length(Args) > 0 then Exit;        // Next takes no arguments
  if not (SelfType.BaseType in XprOrdinalTypes) then Exit;

  Body := ExprList();

  if SelfType is XType_Enum then
  begin
    EnumT := XType_Enum(SelfType);
    SetLength(Branches, Length(EnumT.MemberNames)-1);

    for i := 1 to High(EnumT.MemberNames) do
    begin
      Branches[i-1].Labels.Init([IntLiteral(EnumT.MemberValues[i])]);
      Branches[i-1].Body := ExprList([ReturnStmt(TypeCast(EnumT, IntLiteral(EnumT.MemberValues[i - 1])))]);
    end;

    Body.List += XTree_Case.Create(
      SelfId(),
      Branches,
      ExprList([ReturnStmt(TypeCast(EnumT, BinOp(op_SUB, SelfId(),IntLiteral(1))))]),  // out-of-range fallback
      FContext, FDocPos);
  end
  else
    // All other ordinals: self - 1
    Body.List += ReturnStmt(BinOp(op_SUB, SelfId(), IntLiteral(1)));

  Result := FunctionDef('Next', [], nil, [], SelfType, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

end.
