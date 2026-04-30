unit xpr.TypeIntrinsics;
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
{
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

// This is for functions that are safe by default
// Where it's impossible to go out of range unless corrupted.
// jit:full means it can use both jit tiers (super and x64) where x64 is preffered.
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

  SRC_INSERTION_SORT =
    JIT_RC_STATE                                                  + LineEnding +
    'for (var i := left + 1; i <= right; i += 1) do             ' + LineEnding +
    '  var key := self[i]                                       ' + LineEnding +
    '  var j := i - 1                                           ' + LineEnding +
    '  while((j >= left) and (self[j] > key))do                 ' + LineEnding +
    '    self[j+1] := self[j]                                   ' + LineEnding +
    '    j -= 1                                                 ' + LineEnding +
    '  self[j+1] := key                                         ' + LineEnding;

  SRC_SORT =
    'const RUN := 24                                           ' + LineEnding +
    'var loc := self;                                          ' + LineEnding +
    'var buf: TypeOf(self)                                     ' + LineEnding +
    JIT_RC_STATE                                                 + LineEnding +
    'func Merge(L, M, R: Int)                                  ' + LineEnding +
    '  var I := L;                                             ' + LineEnding +
    '  var J := M + 1;                                         ' + LineEnding +
    '  var K := L;                                             ' + LineEnding +
    '  while (I <= M) and (J <= R) do                          ' + LineEnding +
    '    if loc[I] <= loc[J] then                              ' + LineEnding +
    '      buf[K] := loc[I]                                    ' + LineEnding +
    '      i+=1                                                ' + LineEnding +
    '    else                                                  ' + LineEnding +
    '      buf[K] := loc[J]                                    ' + LineEnding +
    '      j+=1                                                ' + LineEnding +
    '    k+=1                                                  ' + LineEnding +
    '                                                          ' + LineEnding +
    '  if I <= M then Move(loc[I], buf[K], (M - I + 1) * SizeOf(loc[0]))' + LineEnding +
    '  if J <= R then Move(loc[J], buf[K], (R - J + 1) * SizeOf(loc[0]))' + LineEnding +
    '  Move(buf[L], loc[L], (R - L + 1) * SizeOf(loc[0]))      ' + LineEnding +
    '                                                          ' + LineEnding +
    'var N := loc.len();                                       ' + LineEnding +
    'if N <= 1 then return;                                    ' + LineEnding +
    'var L := 0;                                               ' + LineEnding +
    'while L < N do                                            ' + LineEnding +
    '  Self.InsertionSort(L, Min(L + RUN - 1, N - 1))          ' + LineEnding +
    '  L += RUN                                                ' + LineEnding +
    '                                                          ' + LineEnding +
    'buf.SetLen(N)                                             ' + LineEnding +
    'var width := RUN                                          ' + LineEnding +
    JIT_RC_STATE                                                 + LineEnding +
    'while width < N do                                        ' + LineEnding +
    '  L := 0                                                  ' + LineEnding +
    '  while L < N do                                          ' + LineEnding +
    '    var M := L + width - 1                                ' + LineEnding +
    '    if M >= N - 1 then break                              ' + LineEnding +
    '    var R := L + 2 * width - 1                            ' + LineEnding +
    '    if R >= N then R := N - 1                             ' + LineEnding +
    '    if loc[M] > loc[M + 1] then                           ' + LineEnding +
    '      Merge(L, M, R)                                      ' + LineEnding +
    '    L += 2 * width                                        ' + LineEnding +
    '  width *= 2;                                             ' + LineEnding +
    'buf.setlen(0);loc.setlen(0);                              ' + LineEnding;

  SRC_SORT_WEIGHTED =
    'if(self = nil) then return'                               + LineEnding +
    'var loc := self;'                                         + LineEnding +
    'var N := loc.len();'                                      + LineEnding +
    'if N <= 1 then return;'                                   + LineEnding +
    'const RUN := 24'                                          + LineEnding +
    'var bufL: TypeOf(self)'                                   + LineEnding +
    'var bufW: TypeOf(weights)'                                + LineEnding +
    JIT_RC_STATE                                               + LineEnding +
    'func Merge(L, M, R: Int)'                                 + LineEnding +
    '  var I := L;'                                            + LineEnding +
    '  var J := M + 1;'                                        + LineEnding +
    '  var K := L;'                                            + LineEnding +
    '  while (I <= M) and (J <= R) do'                         + LineEnding +
    '    if weights[I] <= weights[J] then'                     + LineEnding +
    '      bufL[K] := loc[I]'                                  + LineEnding +
    '      bufW[K] := weights[I]'                              + LineEnding +
    '      I += 1'                                             + LineEnding +
    '    else'                                                 + LineEnding +
    '      bufL[K] := loc[J]'                                  + LineEnding +
    '      bufW[K] := weights[J]'                              + LineEnding +
    '      J += 1'                                             + LineEnding +
    '    K += 1'                                               + LineEnding +

    '  while I <= M do'                                        + LineEnding +
    '    bufL[K] := loc[I]'                                    + LineEnding +
    '    bufW[K] := weights[I]'                                + LineEnding +
    '    I += 1; K += 1'                                       + LineEnding +

    '  while J <= R do'                                        + LineEnding +
    '    bufL[K] := loc[J]'                                    + LineEnding +
    '    bufW[K] := weights[J]'                                + LineEnding +
    '    J += 1; K += 1'                                       + LineEnding +

    '  Move(bufL[L], loc[L], (R - L + 1) * SizeOf(loc[0]))'    + LineEnding +
    '  Move(bufW[L], weights[L], (R - L + 1) * SizeOf(weights[0]))' + LineEnding +
    // insertion pre-pass
    'var L := 0;'                                              + LineEnding +
    JIT_RC_STATE                                               + LineEnding +
    'while L < N do'                                           + LineEnding +
    '  var R := Min(L + RUN - 1, N - 1)'                       + LineEnding +
    '  var i := L + 1'                                         + LineEnding +
    '  while i <= R do'                                        + LineEnding +
    '    var key  := loc[i]'                                   + LineEnding +
    '    var keyW := weights[i]'                               + LineEnding +
    '    var j := i - 1'                                       + LineEnding +
    '    while (j >= L) and (weights[j] > keyW) do'            + LineEnding +
    '      loc[j + 1] := loc[j]'                               + LineEnding +
    '      weights[j + 1]   := weights[j]'                     + LineEnding +
    '      j -= 1'                                             + LineEnding +
    '    loc[j + 1] := key'                                    + LineEnding +
    '    weights[j + 1]   := keyW'                             + LineEnding +
    '    i += 1'                                               + LineEnding +
    '  L += RUN'                                               + LineEnding +

    'bufL.SetLen(N)'                                           + LineEnding +
    'bufW.SetLen(N)'                                           + LineEnding +
    'var width := RUN'                                         + LineEnding +
    JIT_RC_STATE                                               + LineEnding +
    'while width < N do'                                       + LineEnding +
    '  L := 0'                                                 + LineEnding +
    '  while L < N do'                                         + LineEnding +
    '    var M := L + width - 1'                               + LineEnding +
    '    if M >= N - 1 then break'                             + LineEnding +
    '    var R := L + 2 * width - 1'                           + LineEnding +
    '    if R >= N then R := N - 1'                            + LineEnding +
    '    if weights[M] > weights[M + 1] then'                  + LineEnding +
    '      Merge(L, M, R)'                                     + LineEnding +
    '    L += 2 * width'                                       + LineEnding +
    '  width *= 2'                                             + LineEnding +

    'bufL.SetLen(0)'                                           + LineEnding +
    'bufW.SetLen(0)'                                           + LineEnding +
    'loc.SetLen(0)'                                            + LineEnding;

  SRC_CONCAT =
    'if(self = nil) then return Other.Copy()'                      + LineEnding +
    'if(Other = nil) then return self.Copy()'                      + LineEnding +
    'var lenA := self.Len()'                                       + LineEnding +
    'var lenB := Other.Len()'                                      + LineEnding +
    'result.SetLen(lenA + lenB)'                                   + LineEnding +
    'memmove(addr(self[0]), addr(result[0]), SizeOf(self[0])*lenA)'   + LineEnding +
    'memmove(addr(other[0]), addr(result[lenA]), SizeOf(other[0])*lenB)' + LineEnding;

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
    'memmove(addr(self[0]), addr(result[0]), SizeOf(self[0]) * l)'           + LineEnding +
    'result[l] := Item'                                                      + LineEnding;

type
  TTypeIntrinsics = class(TIntrinsics)
  public
    {** AST Node Factory Helpers **}
    function IntLiteral(Value: Int64): XTree_Int;
    function BoolLiteral(Value: Boolean): XTree_Bool;
    function FloatLiteral(Value: Double): XTree_Float;
    function StringLiteral(const Value: string): XTree_String;
    function NilPointer: XTree_Pointer;
    function Id(const Name: string): XTree_Identifier;
    function Annotate(name:string; value: Int32): XTree_Annotation;
    function Annotate(name: string; value: Boolean): XTree_Annotation; overload;
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

    function GenerateInsertionSort(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateSort(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateSortOneArg(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateSortWeighted(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateSorted(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateSortedCmp(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateSortedWeighted(SelfType: XType; Args: array of XType): XTree_Function;

    function GenerateConcat(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateConcatItem(SelfType: XType; Args: array of XType): XTree_Function;

    function GenerateFormat(SelfType: XType; Args: array of XType): XTree_Function;

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

function TTypeIntrinsics.BoolLiteral(Value: Boolean): XTree_Bool;
begin
  Result := XTree_Bool.Create(BoolToStr(Value), FContext, FDocPos);
  Result.FResType := FContext.GetType(xtBool);
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

function TTypeIntrinsics.NilPointer: XTree_Pointer;
begin
  Result := XTree_Pointer.Create('nil', FContext, FDocPos);
  Result.FResType := FContext.GetType(xtPointer);
  Result.Value := 0;
end;

function TTypeIntrinsics.Id(const Name: string): XTree_Identifier;
begin
  Result := XTree_Identifier.Create(Name, FContext, FDocPos);
end;

function TTypeIntrinsics.Annotate(name: string; value: Int32): XTree_Annotation;
begin
  Result := XTree_Annotation.Create(FContext, FDocPos);
  Result.Identifier := Id(name);
  Result.Value := IntLiteral(value);
end;

function TTypeIntrinsics.Annotate(name: string; value: Boolean): XTree_Annotation;
begin
  Result := XTree_Annotation.Create(FContext, FDocPos);
  Result.Identifier := Id(name);
  Result.Value := BoolLiteral(value);
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
  Result.Annotations := ExprList([Annotate('jit', 0)]);
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
  Result.Annotations := ExprList([Annotate('jit', 0)]);
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
  Result.Annotations := ExprList([Annotate('jit', 0)]);
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
        Result.Annotations := ExprList([Annotate('jit', 0)]);
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
      Result.Annotations := ExprList([Annotate('jit', 0)]);
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
  Result.Annotations := ExprList([Annotate('jit', 0)]);
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
  Result.Annotations := ExprList([Annotate('jit', 0)]);
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
  Result.Annotations := ExprList([Annotate('jit', 0)]);
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
  Result.Annotations := ExprList([Annotate('jit', 0)]);
  Result.SelfType := SelfType;
end;

function TTypeIntrinsics.GenerateDefault(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
begin
  if SelfType = nil then Exit(nil);
  Body := ExprList([XTree_Default.Create(nil, [SelfId()], FContext, FDocPos)]);
  Result := FunctionDef('Default', [], nil, [], nil, Body);
  Result.Annotations := ExprList([Annotate('jit', 0)]);
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
  Result.Annotations := ExprList([Annotate('jit', 0)]);
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
  Result.Annotations := ExprList([Annotate('jit', 0)]);
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

// array and string
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
  if not (SelfType is XType_Array) then Exit(nil);
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
    ToLine   := 'if(Stop = -1) then stop := self.Len()'           + LineEnding +
                'var len := Stop - Start';
  end;

  Body := ExprList();
  Body.List += Parse('__internal__', FContext,
    'if(self = nil) then return nil'                  + LineEnding +
    ToLine                                            + LineEnding +
    'result.SetLen(len)'                              + LineEnding +
    'memmove(addr(self[Start]), addr(result[0]), SizeOf(self[0])*len)'+ LineEnding);

  Result := FunctionDef('Slice', ArgNames, ArgPass, ArgTypes, SelfType, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [cfNoCollect];
end;

// array and string
function TTypeIntrinsics.GenerateCopy(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
begin
  if SelfType = nil then Exit(nil);
  if not (SelfType is XType_Array) then Exit(nil);
  if Length(Args) <> 0 then Exit(nil);

  Body := ExprList();
  Body.List += Parse('__internal__', FContext,
    'if(self = nil) then return nil'                          + LineEnding +
    'result.SetLen(self.Len())'                               + LineEnding +
    'var l := self.Len()'                                     + LineEnding +
    'memmove(addr(self[0]), addr(result[0]), SizeOf(self[0])*l)' + LineEnding
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

// array and string
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

// array and string
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

// array and string
function TTypeIntrinsics.GenerateContains(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList; ItemType: XType;
begin
  if Length(Args) <> 1 then Exit(nil);
  if not (SelfType is XType_Array) then Exit(nil);
  ItemType := (SelfType as XType_Array).ItemType;
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_CONTAINS);
  Result := FunctionDef('Contains', ['Value'], [pbCopy], [ItemType],
    FContext.GetType(xtBool), Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

// array and string
function TTypeIntrinsics.GenerateIndexOf(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList; ItemType: XType;
begin
  if Length(Args) <> 1 then Exit(nil);
  if not (SelfType is XType_Array) then Exit(nil);
  ItemType := (SelfType as XType_Array).ItemType;
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_INDEXOF);
  Result := FunctionDef('IndexOf', ['Value'], [pbCopy], [ItemType],
    FContext.GetType(xtInt), Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

// array only
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

// array only
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

// array only
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

// array only
function TTypeIntrinsics.GenerateReverse(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList;
begin
  if not IsPlainArray(SelfType, Args, 0) then Exit(nil);
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_REVERSE);
  Result := FunctionDef('Reverse', [], nil, [], nil, Body);
  Result.SelfType := SelfType;
end;

// array only
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

function TTypeIntrinsics.GenerateInsertionSort(SelfType: XType; Args: array of XType): XTree_Function;
var Body: XTree_ExprList;
begin
  if not IsPlainArray(SelfType, Args, 2) then Exit(nil);
  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_INSERTION_SORT);
  Result := FunctionDef('InsertionSort', ['left','right'], [pbCopy,pbCopy], [FContext.GetType(xtInt), FContext.GetType(xtInt)], nil, Body);
  Result.SelfType := SelfType;
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
  if CmpMethod.Passing[0] <> pbRef then Exit;
  if CmpMethod.Passing[1] <> pbRef then Exit;

  // Validate: Cmp(ref <dont care>, ref <dont care>): Int
  if (CmpMethod.ReturnType = nil) or
     not (CmpMethod.ReturnType.BaseType = xtInt) then Exit;

  Body := ExprList();
  //Body.List += Parse('__internal__', FContext, SRC_SORT_CMP);

  Body.List += Parse('__internal__', FContext,
    'if self.len() > 0 then'                                                + LineEnding +
    '  var nativeFunc := create_callback(Cmp)'                              + LineEnding +
    '  __MergeSort(addr(self[0]), self.len(), SizeOf(self[0]), nativeFunc)' + LineEnding +
    '  free_callback(nativeFunc)'                                           + LineEnding
  );

  Result := FunctionDef('sort', ['cmp'], [pbCopy], [CmpType], nil, Body);
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

// array and string
function TTypeIntrinsics.GenerateConcatItem(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
  ItemType: XType;
begin
  Result := nil;
  if SelfType = nil then Exit;
  if not (SelfType is XType_Array) then Exit;
  if Length(Args) <> 1 then Exit;

  ItemType := (SelfType as XType_Array).ItemType;
  if not ItemType.Equals(Args[0]) then Exit;

  Body := ExprList();
  Body.List += Parse('__internal__', FContext, SRC_CONCAT_ITEM);

  Result := FunctionDef('Concat', ['Item'], [pbCopy], [ItemType], SelfType, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;

// array and string
function TTypeIntrinsics.GenerateConcat(SelfType: XType; Args: array of XType): XTree_Function;
var
  Body: XTree_ExprList;
  ItemType: XType;
begin
  Result := nil;
  if SelfType = nil then Exit;
  if not (SelfType is XType_Array) then Exit;
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
  Result := FunctionDef('Concat', ['Other'], [pbCopy], [SelfType], SelfType, Body);
  Result.SelfType := SelfType;
  Result.InternalFlags := [];
end;



function TTypeIntrinsics.GenerateFormat(SelfType: XType; Args: array of XType): XTree_Function;
const
  Q = #39; // single quote
var
  Body: XTree_ExprList;
  StringType: XType;
  ArgNames: TStringArray;
  ArgPass: TPassArgsBy;
  ArgTypes: XTypeArray;
  N, i: Int32;
  FuncCall, Src: string;
begin
  if SelfType = nil then Exit(nil);
  if not (SelfType.BaseType in [xtAnsiString, xtUnicodeString]) then Exit(nil);
  N := Length(Args);
  if N = 0 then Exit(nil);

  StringType := FContext.GetType(xtAnsiString);

  SetLength(ArgNames, N);
  SetLength(ArgPass,  N);
  SetLength(ArgTypes, N);
  for i := 0 to N - 1 do
  begin
    ArgNames[i] := 'arg' + IntToStr(i);
    ArgPass[i]  := pbCopy;
    ArgTypes[i] := Args[i];
  end;

  Src :=
    'var res := ' + Q+Q                                  + LineEnding +
    'var i := 0'                                         + LineEnding +
    'var h := self.High()'                               + LineEnding +
    'var argIdx := 0'                                    + LineEnding +
    'var spec := ' + Q+Q                                 + LineEnding +
    'while i <= h do'                                    + LineEnding +
    '  if self[i] = #37 then'                            + LineEnding +  // '%'
    '    i += 1'                                         + LineEnding +
    '    if self[i] = #37 then'                          + LineEnding +  // '%%'
    '      res += ' + Q+'%'+Q                            + LineEnding +
    '      i += 1'                                       + LineEnding +
    '    else'                                           + LineEnding +
    '      spec := ' + Q+'%'+Q                           + LineEnding +
    '      while (i <= h) and (self[i] < #65) do'        + LineEnding +  // < 'A'
    '        spec += self[i]'                            + LineEnding +
    '        i += 1'                                     + LineEnding +
    '      spec += self[i]'                              + LineEnding +
    '      i += 1'                                       + LineEnding;

  // Compile-time dispatch: one branch per arg, types known statically
  for i := 0 to N - 1 do
  begin
    case Args[i].BaseType of
      xtInt8..xtUInt64:   FuncCall := 'FormatInt(spec, Int(arg'    + IntToStr(i) + '))';
      xtSingle, xtDouble: FuncCall := 'FormatFloat(spec, Double(arg' + IntToStr(i) + '))';
      xtString: FuncCall := 'FormatString(spec, arg' + IntToStr(i) + ')';
    else
      FuncCall := 'FormatString(spec, arg' + IntToStr(i) + '.ToStr())';
    end;

    if i = 0 then
      Src += '      if argIdx = 0 then res += ' + FuncCall + LineEnding
    else
      Src += '      elif argIdx = ' + IntToStr(i) + ' then res += ' + FuncCall + LineEnding;
  end;

  Src +=
    '      argIdx += 1'  + LineEnding +
    '  else'             + LineEnding +
    '    res += self[i]' + LineEnding +
    '    i += 1'         + LineEnding +
    'return res'         + LineEnding;

  Body := ExprList();
  Body.List += Parse('__internal__', FContext, Src);

  Result := FunctionDef('Format', ArgNames, ArgPass, ArgTypes, StringType, Body);
  Result.SelfType      := SelfType;
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
