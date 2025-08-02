unit xpr.Tree;
// Author: Jarl K. Holta
// License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
{$I header.inc}

interface

uses
  SysUtils, xpr.Types, xpr.Tokenizer, xpr.CompilerContext, xpr.Intermediate;

type
  (* 
    A list of statements or expressions
  *)
  XTree_ExprList = class(XTree_Node)
    List: XNodeArray;
    DelayedList: XNodeArray;

    constructor Create(AList: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    constructor Create(AStmt: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); overload;
    function ToString(offset:string=''): string; override;

    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  (* basic stub helper *)
  XTree_VarStub = class(XTree_Node)
    VarDecl: TXprVar;
    constructor Create(AVar: TXprVar; ACTX: TCompilerContext; DocPos: TDocPos); overload;
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
    function CompileLValue(Dest: TXprVar): TXprVar; override;
  end;

  (*
    A stub for all constants
  *)
  XTree_Const = class(XTree_Node)
    StrValue: string;
    Expected: EExpressBaseType;

    function ToString(offset:string=''): string; override;

    function ResType(): XType; override;
    function SetExpectedType(ExpectedType: EExpressBaseType): Boolean; virtual;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  XTree_Bool  = class(XTree_Const)
    Value: Boolean;
    constructor Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  XTree_Pointer  = class(XTree_Const)
    Value: PtrInt;
    constructor Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  XTree_Char  = class(XTree_Const)
    Value: WideChar;
    constructor Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  XTree_Int  = class(XTree_Const)
    Value: Int64;
    constructor Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function SetExpectedType(ExpectedType: EExpressBaseType): Boolean; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  XTree_Float = class(XTree_Const)
    Value: Double;
    constructor Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function SetExpectedType(ExpectedType: EExpressBaseType): Boolean; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  XTree_String = class(XTree_Const)
    constructor Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  (* 
    A variable
  *)
  XTree_Identifier = class(XTree_Node)
    Name: string;
    constructor Create(AName:String; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset:string=''): string; override;

    function ResType(): XType; override;

    function CompileLValue(Dest: TXprVar): TXprVar; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;
  XIdentNodeList = specialize TArrayList<XTree_Identifier>;

  (*
    Declaring variables
  *)
  XTree_VarDecl = class(XTree_Node)
    Variables: XIdentNodeList;
    VarType: XType;
    Expr: XTree_Node;
    constructor Create(AVariables: XIdentNodeList; AExpr: XTree_Node; AType: XType; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset:string=''): string; override;

    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  (* A conditional (ternary) expression that returns a value *)
  XTree_IfExpr = class(XTree_Node)
    Condition: XTree_Node;
    ThenExpr: XTree_Node;
    ElseExpr: XTree_Node;

    constructor Create(ACond, AThen, AElse: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function ToString(offset: string = ''): string; override;

    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  (* return from function *)
  XTree_Return = class(XTree_Node)
    Expr: XTree_Node;
    constructor Create(AExpr: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  (* Exit the current loop immediately *)
  XTree_Break = class(XTree_Node)
    constructor Create(ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function ToString(offset: string = ''): string; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  (* Skip to the next iteration of the current loop *)
  XTree_Continue = class(XTree_Node)
    constructor Create(ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function ToString(offset: string = ''): string; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  (* Declaring function *)
  XTree_Function = class(XTree_Node)
    Name:     string;
    ArgNames: TStringArray;
    ArgPass:  TPassArgsBy;
    ArgTypes: XTypeArray;
    RetType:  XType;
    PorgramBlock: XTree_ExprList;
    FrameSize: SizeInt;

     // Two ways to achieve the same
    SelfType: XType;
    TypeName: String;

    // populated after .Compile for .DelayedCompile
    PreCompiled: Boolean;
    FullyCompiled: Boolean;
    MethodVar: TXprVar;

    //constructor Create(AName: string; AArgNames: TStringArray; AArgTypes: XTypeArray; AProg: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    constructor Create(AName: string; AArgNames: TStringArray; ByRef: TPassArgsBy; AArgTypes: XTypeArray; ARet:XType; AProg: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;

    function ToString(Offset:string=''): string; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  (* A field lookup *)
  XTree_Field = class(XTree_Node)
    Left:  XTree_Node;
    Right: XTree_Node;

    constructor Create(ALeft, ARight: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(Offset:string=''): string; override;
    function ResType(): XType; override;

    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
    function CompileLValue(Dest: TXprVar): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  (* Call a function *)
  XTree_Invoke = class(XTree_Node)
    Method: XTree_Node;
    Args: XNodeArray;
    SelfExpr: XTree_Node;

    constructor Create(AFunc: XTree_Node; ArgList: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(Offset:string=''): string; override;
    function ResolveMethod(out Func: TXprVar; out FuncType: XType): Boolean;
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  (* An array lookup *)
  XTree_Index = class(XTree_Node)
    Expr, Index: XTree_Node;
    ForceTypeSize: Int32; // useful for length and refcount

    constructor Create(AExpr, AIndex: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(Offset:string=''): string; override;
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
    function CompileLValue(Dest: TXprVar): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  
  (* if statement *)
  XTree_If = class(XTree_Node)
    Conditions: XNodeArray;
    Bodys: XNodeArray;
    ElseBody: XTree_ExprList;
    constructor Create(AConds, ABodys: XNodeArray; AElseBody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset:string=''): string; override;
    
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  (* while loop *)
  XTree_While = class(XTree_Node)
    Condition: XTree_Node;
    Body: XTree_ExprList;
    constructor Create(ACond: XTree_Node; ABody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset:string=''): string; override;
    
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  XTree_Try = class(XTree_Node)
    TryBody, ExceptBody: XTree_ExprList;
    constructor Create(ATryBody, AExceptBody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset:string=''): string; override;

    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  (* for loop *)
  XTree_For = class(XTree_Node)
    EntryStmt, Condition, LoopStmt: XTree_Node;
    Body: XTree_ExprList;
    constructor Create(AEntryStmt, ACondition, ALoopStmt: XTree_Node; ABody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset:string=''): string; override;

    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  (* Pascal-style repeat-until loop *)
  XTree_Repeat = class(XTree_Node)
    Condition: XTree_Node;
    Body: XTree_ExprList;
    constructor Create(ACond: XTree_Node; ABody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset: string = ''): string; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags = []): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags = []): TXprVar; override;
  end;

  (*  operator types *)
  XTree_UnaryOp = class(XTree_Node)
    Left: XTree_Node;
    OP: EOperator;

    constructor Create(Operation:EOperator; ALeft: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset:string=''): string; override;

    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  XTree_BinaryOp = class(XTree_Node)
    Left, Right: XTree_Node;
    OP: EOperator;
    
    constructor Create(Operation:EOperator; ALeft, ARight: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset:string=''): string; override;

    function RedefineConstant(A,B: XTree_Node): Boolean;
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  XTree_Assign = class(XTree_BinaryOp)
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

  (*  print 'print' *)
  XTree_Print = class(XTree_Node)
    Args: XNodeArray;
    constructor Create(ArgList: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset:string=''): string; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
  end;

function CompileAST(astnode:XTree_Node; writeTree: Boolean=False; doFree:Boolean = True): TIntermediateCode;

operator + (left: XNodeArray; Right: XTree_Node): XNodeArray;
function NodeArray(Arr: array of XTree_Node): XNodeArray;

implementation

uses
  Math, xpr.Utils, xpr.Vartypes, xpr.Errors, xpr.Langdef;


// HELPERS

operator + (left: XNodeArray; Right: XTree_Node): XNodeArray;
begin
  Result := Left;
  SetLength(Result, Length(Result)+1);
  Result[High(Result)] := Right;
end;

function NodeArray(Arr: array of XTree_Node): XNodeArray;
begin
  SetLength(Result, Length(Arr));
  Move(Arr[0], Result[0], SizeOf(XTree_Node));
end;


function CompileAST(astnode:XTree_Node; writeTree: Boolean = False; doFree:Boolean = True): TIntermediateCode;
begin
  astnode.Compile(NullResVar);
  astnode.ctx.Emit(GetInstr(icRET), NoDocPos);
  astnode.DelayedCompile(NullResVar);

  // DelayedCompile can add new delayed nodes
  while Length(astnode.ctx.DelayedNodes) > 0 do
  begin
    XTree_ExprList(astnode).DelayedList := astnode.ctx.DelayedNodes;
    astnode.ctx.DelayedNodes := [];
    astnode.DelayedCompile(NullResVar);
  end;

  if writeTree then
  begin
    WriteLn('----| TREE STRUCTURE |--------------------------------------------');
    WriteFancy(astnode.ToString());
    WriteLn('------------------------------------------------------------------'+#13#10);
  end;

  Result := astnode.ctx.Intermediate;
  //if doFree then
  //  astNode.Free;
end;


// ============================================================================
// List of expressions and statements
//
constructor XTree_ExprList.Create(AList: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FDocPos  := DocPos;
  Self.FContext := ACTX;

  Self.List := AList;
  Self.DelayedList := [];
end;

constructor XTree_ExprList.Create(AStmt: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FDocPos  := DocPos;
  Self.FContext := ACTX;

  SetLength(Self.List, 1);
  Self.List[0] := AStmt;
end;

function XTree_ExprList.ToString(Offset:string=''): string;
var i:Int32;
begin
  Result := Offset + _AQUA_+'ExprList'+_WHITE_+'('+ LineEnding;
  for i:=0 to High(Self.List) do
  begin
    Result += Self.List[i].ToString(Offset + '  ');
    if i <> High(Self.List) then Result += ', ';
    Result += LineEnding;
  end;
  Result += Offset + ')';

  if Length(DelayedList) > 0 then
  begin
    Result += LineEnding + Offset + _AQUA_+'ExprList[Delayed]'+_WHITE_+'('+ LineEnding;
    for i:=0 to High(Self.DelayedList) do
    begin
      Result += Self.DelayedList[i].ToString(Offset + '  ');
      if i <> High(Self.DelayedList) then Result += ', ';
      Result += LineEnding;
    end;
    Result += Offset + ')';
  end;
end;

function XTree_ExprList.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var i:Int32;
begin
  i := 0;
  while i <= High(Self.List) do
  begin
    Self.List[i].Compile(NullResVar);
    Inc(i);
  end;

  Result := NullResVar;
end;

function XTree_ExprList.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var i:Int32;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName(), ' - Delayed nodes: ', Length(DelayedList));{$ENDIF}
  for i:=0 to High(Self.List) do
    Self.List[i].DelayedCompile(NullResVar);

  for i:=0 to High(Self.DelayedList) do
    Self.DelayedList[i].Compile(NullResVar);

  for i:=0 to High(Self.DelayedList) do
  begin
    {$IFDEF DEBUGGING_TREE}WriteLn('Is delayed node function: ', Self.DelayedList[i] is XTree_Function);{$ENDIF}
    Self.DelayedList[i].DelayedCompile(NullResVar);
  end;

  Result := NullResVar;
end;



constructor XTree_VarStub.Create(AVar: TXprVar; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FDocPos  := DocPos;
  Self.FContext := ACTX;

  Self.VarDecl  := AVar;
end;

function XTree_VarStub.ResType(): XType;
begin
  Result := VarDecl.VarType;
end;

function XTree_VarStub.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
begin
  Result := VarDecl;
end;

function XTree_VarStub.CompileLValue(Dest: TXprVar): TXprVar;
begin
  Result := VarDecl;
end;



// ============================================================================
// Script constants
//
function XTree_Const.ToString(Offset:string=''): string;
begin
  Result := Offset + Self.ClassName+'('+_PURPLE_+Self.StrValue+_WHITE_+')';
end;

function XTree_Const.ResType(): XType;
begin
  if (FResType = nil) then
    FResType := ctx.GetType(Self.Expected);
  Result := inherited;
end;

function XTree_Const.SetExpectedType(ExpectedType: EExpressBaseType): Boolean;
begin
  Result := False;
end;

function XTree_Const.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
begin
  Result := NullResVar;
  RaiseException(eUnexpected, FDocPos);
end;

constructor XTree_Bool.Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  Self.StrValue := AValue;
  Self.Value    := AValue.ToBoolean();
  Self.Expected := xtBoolean;
end;

function XTree_Bool.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
begin
  Result := ctx.RegConst(Constant(Value, Expected));
end;

constructor XTree_Pointer.Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  Self.StrValue := AValue;
  Self.Value    := 0;
  Self.Expected := xtPointer;
end;

function XTree_Pointer.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
begin
  Result := ctx.RegConst(Constant(Value, Expected));
end;


constructor XTree_Char.Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  if Length(AValue) <> 1 then RaiseException(eUnexpected, DocPos);
  Self.StrValue := AValue[1];
  Self.Value    := AValue[1];
  Self.Expected := xtAnsiChar;
end;

function XTree_Char.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
begin
  Result := ctx.RegConst(Constant(Value, Expected));
end;


constructor XTree_Int.Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;
  Self.StrValue := AValue;
  Self.Value    := AValue.ToInt64();
  Self.Expected := SmallestIntSize(Value, xtInt);
end;

function XTree_Int.SetExpectedType(ExpectedType: EExpressBaseType): Boolean;
begin
  if not(ExpectedType in XprBoolTypes+XprIntTypes) then
    Exit(False);

  if(Expected in XprIntTypes) and (SmallestIntSize(Value) > ExpectedType) then
    Exit(False);

  Expected := ExpectedType;
  FResType := ctx.GetType(Self.Expected);
  Result := True;
end;

function XTree_Int.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
begin
  Result := ctx.RegConst(Constant(Value, Expected));
end;


constructor XTree_Float.Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;
  Self.StrValue := AValue;
  Self.Value    := AValue.ToExtended();
  Self.Expected := xtDouble;
end;

function XTree_Float.SetExpectedType(ExpectedType: EExpressBaseType): Boolean;
begin
  if not(ExpectedType in XprFloatTypes) then Exit(False);
  Expected := ExpectedType;
  FResType := ctx.GetType(Self.Expected);
  Result := True;
end;

function XTree_Float.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
begin
  Result := ctx.RegConst(Constant(Value, Expected));
end;

constructor XTree_String.Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;
  Self.StrValue := AValue;
  Self.Expected := xtAnsiString; // This is an AnsiString literal
end;

function XTree_String.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
begin
  Result := ctx.RegConst(Self.StrValue);
end;


// ============================================================================
// Holding an identifier, it has no other purpose than to be used as a lookup
//
constructor XTree_Identifier.Create(AName:String; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  Self.Name     := AName;
end;

function XTree_Identifier.ToString(Offset:string=''): string;
begin
  Result := Offset + _YELLOW_+Self.Name+_WHITE_;
end;

function XTree_Identifier.ResType(): XType;
var
  foundVar: TXprVar;
begin
  Assert(Self.Name <> '');
  if (Self.FResType = nil) then
  begin
    foundVar := FContext.GetVar(Self.Name, FDocPos);
    if foundVar = NullResVar then
      RaiseExceptionFmt('Identifier `%` not declared', [Self.Name], FDocPos);
    Self.FResType := foundVar.VarType;
    if Self.FResType = nil then
      RaiseExceptionFmt('Variable `%` has no defined type', [Self.Name], FDocPos);
  end;
  Result := inherited;
end;

function XTree_Identifier.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var
  localVar: TXprVar;
begin
  Result := Self.FContext.GetVar(Self.Name, FDocPos);
  if Result = NullResVar then
    RaiseExceptionFmt('Identifier `%` not declared', [Self.Name], FDocPos);

  if Result.IsGlobal and (ctx.Scope <> GLOBAL_SCOPE) then
  begin
    localVar := TXprVar.Create(Result.VarType);
    localVar.Reference := True;//not(Result.VarType.BaseType in XprPointerTypes);
    ctx.RegVar(Self.Name, localVar, FDocPos);

    if localVar.Reference then
      ctx.Emit(GetInstr(icLOAD_GLOBAL, [localVar, Result]), FDocPos)
    else
      ctx.Emit(GetInstr(icCOPY_GLOBAL, [localVar, Result]), FDocPos);

    Result := localVar;
  end;
end;

function XTree_Identifier.CompileLValue(Dest: TXprVar): TXprVar;
begin
  Result := Self.Compile(Dest,[]);
end;


// ============================================================================
// Variable declaration
//
constructor XTree_VarDecl.Create(AVariables: XIdentNodeList; AExpr: XTree_Node; AType: XType; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  Self.Variables := AVariables;
  Self.VarType   := AType;
  Self.Expr      := AExpr;
end;

function XTree_VarDecl.ToString(Offset:string=''): string;
var i:Int32;
begin
  Result := Offset + _AQUA_+'VarDecl'+_WHITE_+'(';
  for i:=0 to Self.Variables.High do
  begin
    Result += Self.Variables.Data[i].ToString();
    if i <> Self.Variables.High then Result += ', ';
  end;
  if(Expr <> nil) then Result += _GRAY_ + ' <- '+ _WHITE_ + Self.Expr.ToString();
  Result += ')';
end;

function XTree_VarDecl.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var
  i:Int32;
begin
  if VarType = nil then
    begin
      if Self.Expr = nil then
        RaiseException('Variable declaration requires an explicit type or an initial assignment', FDocPos);

      Self.VarType := Self.Expr.ResType();
      if Self.VarType = nil then
        RaiseExceptionFmt('Could not infer type for variable `%s`', [Self.Variables.Data[0].Name], FDocPos);
    end;

  for i:=0 to Self.Variables.High do
  begin
    Self.Variables.Data[i].FResType := self.VarType;

    ctx.RegVar(Self.Variables.Data[i].Name, self.VarType, Self.FDocPos);
  end;

  if Self.Expr <> nil then
  begin
    for i:=0 to Self.Variables.High do
      with XTree_Assign.Create(op_Asgn, Self.Variables.Data[i], Self.Expr, ctx, FDocPos) do
      try
        Compile(NullResVar, Flags);
      finally
        Free();
      end;
  end else
  begin
    {emit assign with default zero}
    for i:=0 to Self.Variables.High do
    begin
      ctx.Emit(GetInstr(icFill, [Self.Variables.Data[i].Compile(NullResVar), Immediate(Self.Variables.Data[i].ResType().Size), Immediate(0)]), FDocPos);
    end;
  end;
  Result := NullResVar;
end;

function XTree_VarDecl.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var i: Int32;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName());{$ENDIF}
  if Self.Expr <> nil then
    Self.Expr.DelayedCompile(Dest, Flags);
  Result := NullResVar;
end;


// ============================================================================
// IF Expression
//
constructor XTree_IfExpr.Create(ACond, AThen, AElse: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.Condition := ACond;
  Self.ThenExpr  := AThen;
  Self.ElseExpr  := AElse;
end;

function XTree_IfExpr.ToString(offset: string): string;
begin
  Result := offset + _AQUA_+'IfExpr'+_WHITE_+'(' + LineEnding;
  Result += Condition.ToString(offset+'  ') + ', ' + LineEnding;
  Result += ThenExpr.ToString(offset+'  ') + ', ' + LineEnding;
  Result += ElseExpr.ToString(offset+'  ') + LineEnding;
  Result += offset + ')';
end;

(*
  The type of an 'if' expression is determined by its branches.
  The 'then' and 'else' branches MUST have compatible types.
*)
function XTree_IfExpr.ResType(): XType;
var
  thenType, elseType: XType;
  commonBaseType: EExpressBaseType;
begin
  if FResType = nil then
  begin
    thenType := ThenExpr.ResType();
    elseType := ElseExpr.ResType();

    if (thenType = nil) or (elseType = nil) then
      RaiseException('Both branches of an if-expression must return a value', FDocPos);

    // Use the same logic as binary operations to find the common type
    commonBaseType := CommonArithmeticCast(thenType.BaseType, elseType.BaseType);
    WriteLn(commonBaseType);
    if commonBaseType = xtUnknown then
       RaiseExceptionFmt(
        'Incompatible types in branches of if-expression: `%s` and `%s` have no common type',
        [thenType.ToString(), elseType.ToString()], FDocPos);

    FResType := FContext.GetType(commonBaseType);
  end;
  Result := FResType;
end;

function XTree_IfExpr.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var
  boolVar, thenResult, elseResult: TXprVar;
  elseJump, endJump: PtrInt;
  finalType: XType;
begin
  finalType := Self.ResType();

  if Dest = NullResVar then
    Result := ctx.GetTempVar(finalType)
  else
    Result := Dest;

  boolVar := Condition.Compile(NullResVar, Flags);
  if not (boolVar.VarType.BaseType = xtBoolean) then
    RaiseExceptionFmt('If expression condition must be a boolean, got `%s`', [boolVar.VarType.ToString], Condition.FDocPos);

  elseJump := ctx.Emit(GetInstr(icJZ, [boolVar, NullVar]), Condition.FDocPos);

  // Compile THEN branch to a temporary
  thenResult := ThenExpr.Compile(NullResVar, Flags);
  // Upcast the result if needed using our new shared helper
  thenResult := ctx.EmitUpcastIfNeeded(thenResult.IfRefDeref(ctx), finalType, False);
  // Move the final, correctly-typed result into the destination
  ctx.Emit(STORE_FAST(Result, thenResult, False), ThenExpr.FDocPos);

  endJump := ctx.Emit(GetInstr(icRELJMP, [NullVar]), ThenExpr.FDocPos);
  ctx.PatchJump(elseJump);

  // Compile ELSE branch to a temporary
  elseResult := ElseExpr.Compile(NullResVar, Flags);
  // Upcast the result if needed
  elseResult := ctx.EmitUpcastIfNeeded(elseResult.IfRefDeref(ctx), finalType, False);
  // Move the final, correctly-typed result into the destination
  ctx.Emit(STORE_FAST(Result, elseResult, False), ElseExpr.FDocPos);

  ctx.PatchJump(endJump);
end;

function XTree_IfExpr.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
begin
  Condition.DelayedCompile(Dest, Flags);
  ThenExpr.DelayedCompile(Dest, Flags);
  ElseExpr.DelayedCompile(Dest, Flags);
  Result := NullResVar;
end;


// ============================================================================
// return statement
//
constructor XTree_Return.Create(AExpr: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  Self.Expr     := AExpr;
end;

function XTree_Return.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var
  resVar, newResVar: TXprVar;
  managed: TXprVarList;
  i: Int32;

begin
  {handle managed declarations}
  managed := ctx.GetManagedDeclarations();

  resVar := NullResVar;
  if Self.Expr <> nil then
  begin
    resVar := Self.Expr.Compile(Dest, Flags);
    if resVar.MemPos = mpConst then
    begin
      newResVar := ctx.GetTempVar(resVar.VarType);
      ctx.Emit(GetInstr(icMOV, [newResVar, resVar]), FDocPos);
      resVar := newResVar;
    end;
  end;

  for i:=0 to managed.High() do
    ctx.EmitFinalizeVar(managed.Data[i], managed.Data[i].Addr = resVar.Addr);

  {return to sender}
  if Self.Expr <> nil then
  begin
    if resVar <> NullResVar then
      ctx.Emit(GetInstr(icRET, [resVar, Immediate(resVar.VarType.Size, ctx.GetType(xtInt32))]), FDocPos)
    else
      ctx.Emit(GetInstr(icRET, [resVar]), FDocPos);
  end else
     ctx.Emit(GetInstr(icRET, []), FDocPos);

  Result := NullResVar;
end;

function XTree_Return.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName());{$ENDIF}
  Self.Expr.DelayedCompile(Dest, Flags);
  Result := NullResVar;
end;


// ============================================================================
// break statement
//
constructor XTree_Break.Create(ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
end;

function XTree_Break.ToString(offset: string): string;
begin
  Result := offset + _AQUA_ + 'Break' + _WHITE_;
end;

function XTree_Break.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
begin
  // Emit the placeholder opcode. The parent loop's RunPatch will find and replace it.
  ctx.Emit(GetInstr(icJBREAK, [NullVar]), FDocPos);
  Result := NullResVar;
end;


// ============================================================================
// continue statement
//
constructor XTree_Continue.Create(ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
end;

function XTree_Continue.ToString(offset: string): string;
begin
  Result := offset + _AQUA_ + 'Continue' + _WHITE_;
end;

function XTree_Continue.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
begin
  // Emit the placeholder opcode. The parent loop's RunPatch will find and replace it.
  ctx.Emit(GetInstr(icJCONT, [NullVar]), FDocPos);
  Result := NullResVar;
end;

// ============================================================================
// Function declaration
//
// Todo: Handle differnet types of passing parameters
//
constructor XTree_Function.Create(AName: string; AArgNames: TStringArray; ByRef: TPassArgsBy; AArgTypes: XTypeArray; ARet:XType; AProg: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  Name     := AName;
  ArgNames := AArgNames;
  ArgPass  := ByRef;
  ArgTypes := AArgTypes;
  RetType  := ARet;
  PorgramBlock := AProg;

  SetLength(TypeName, 0);

  PreCompiled := False;
  FullyCompiled := False;
  FrameSize := 0;
end;

function XTree_Function.ToString(offset:string=''): string;
var i: Int32;
var selfstr: string;
begin
  selfstr := '';
  if SelfType <> nil then selfstr := SelfType.ToString+'.';
  Result := Offset + _AQUA_ +'Function->'+_WHITE_+selfstr+Self.Name+'(';
  for i:=0 to High(Self.ArgNames) do
  begin
    Result += ArgNames[i];
    if i <> High(Self.ArgNames) then Result += ', '
  end;
  Result += LineEnding;
  Result += Self.PorgramBlock.ToString(Offset + '  ') + LineEnding;
  Result += Offset + ')';
end;

(*
  Approach to ref arg:
  Keeps "Reference" compile-time only

  We store a typed Pointer tagged with Reference for the codegen stage:
    At compile-time, if an argument is by-ref, allocate a local variable as usual,
    But instead of storing the value directly, store a pointer (address) into it,

  Then (when codegen sees Reference it will emit):
    On any use except ASGN: emit a DEREF to Temp, use the Temp for the operation
    On store: emit a specialized store operation that dereferences


  XXX: The conditional for XprPointerTypes might be wrong, but it works for simple cases.
  Reference flag might not really do justice, we need double deref, triple even possibly.
  Case where it can fail are change of basepointer, ie setlength
*)
function XTree_Function.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var
  method: XType_Method;
  arg, ptrVar: TXprVar;
  afterFunc: PtrInt;
  i,funcIdx,ptrIdx,allocFrame: Int32;

  procedure AddSelf();
  var
    i: Int32;
  begin
    if SelfType = nil then
      SelfType := ctx.GetType(TypeName);

    SetLength(ArgTypes, Length(ArgTypes)+1);
    SetLength(ArgPass,  Length(ArgPass)+1);
    SetLength(ArgNames, Length(ArgPass)+1);

    for i:=High(ArgTypes)-1 downto 0 do
    begin
      ArgTypes[i+1] := ArgTypes[i];
      ArgPass[i+1]  := ArgPass[i];
      ArgNames[i+1] := ArgNames[i];
    end;

    ArgNames[0] := 'self';
    ArgPass[0]  := pbRef;
    ArgTypes[0] := SelfType;
  end;

begin
  if PreCompiled then Exit(NullResVar);

  if (TypeName <> '') or (SelfType <> nil) then AddSelf();
  method := XType_Method.Create(Name, ArgTypes, ArgPass, RetType, False);
  method.TypeMethod := SelfType <> nil;

  methodVar := TXprVar.Create(method, ctx.CodeSize());
  funcIdx   := ctx.RegGlobalVar(Name, methodVar, FDocPos);

  Result := NullResVar;
  PreCompiled := True;
end;

function XTree_Function.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var
  arg, ptrVar: TXprVar;
  i,ptrIdx,allocFrame: Int32;
  CreationScope: SizeInt;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName(), ', Name: ', name);{$ENDIF}
  if FullyCompiled then Exit(NullResVar);

  // store it in functiontable for late population
  ctx.PushFunction(MethodVar.Addr);

  // functions are not delayed code by default - no entry here!
  ctx.Emit(GetInstr(icPASS, [NullVar]), FDocPos);

  CreationScope := ctx.Scope;
  ctx.Scope := GLOBAL_SCOPE;
  ctx.IncScope();

    allocFrame := ctx.Emit(GetInstr(icNEWFRAME, [NullVar]), FDocPos);

    for i:=High(ArgTypes) downto 0 do
    begin
      if (ArgPass[i] = pbRef) then
      begin
        // store as xtPointer to ensure enough stackspace
        ptrVar := ctx.RegVar(ArgNames[i], ctx.GetType(xtPointer), Self.FDocPos, ptrIdx);
        ctx.Variables.Data[ptrIdx].Reference := True;        // mark it for derefToTemp
        ctx.Variables.Data[ptrIdx].VarType   := ArgTypes[i]; // change type

        ptrVar := ctx.Variables.Data[ptrIdx];
        ctx.Emit(GetInstr(icPOPH, [ptrVar]), FDocPos);
      end else
      begin
        arg := ctx.RegVar(ArgNames[i], ArgTypes[i], Self.FDocPos);
        ctx.Emit(GetInstr(icPOP, [Immediate(arg.VarType.Size, ctx.GetType(xtInt32)), arg]), FDocPos);
      end;
    end;

    PorgramBlock.Compile(NullResVar, Flags);

    //ctx.Emit(GetInstr(icRET), Self.FDocPos);
    with XTree_Return.Create(nil, FContext, FDocPos) do
    try
      Compile(NullResVar);
    finally
      Free();
    end;


    // too late for recursive calls!!!
    ctx.PatchArg(allocFrame, ia1, ctx.FrameSize());

  ctx.DecScope();
  ctx.Scope := CreationScope;

  Result := NullResVar;

  FullyCompiled := True;
end;

// ============================================================================
// Resolve (record) field-lookups
//
constructor XTree_Field.Create(ALeft, ARight: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  Self.Left  := ALeft;
  Self.Right := ARight;
end;

function XTree_Field.ToString(Offset:string=''): string;
begin
  Result := Offset + Self.Left.ToString() +'.'+ Right.ToString();
end;


function XTree_Field.ResType(): XType;
var
  invoke: XTree_Invoke;
begin
  if (Self.FResType <> nil) then
    Exit(inherited);

  if Self.Right is XTree_Identifier then
  begin
    if not (Self.Left.ResType() is XType_Record) then
      RaiseExceptionFmt('Cannot access fields on non-record type `%s`', [Self.Left.ResType().ToString], Self.Left.FDocPos);

    if (Self.FResType = nil) then
      FResType := XType_Record(Self.Left.ResType()).FieldType(XTree_Identifier(Self.Right).Name);
  end
  else if Self.Right is XTree_Invoke then
  begin
    Invoke := XTree_Invoke(Self.Right);
    Invoke.SelfExpr := Left;
    FResType := XTree_Invoke(Self.Right).ResType();
  end
  else
    RaiseException('Unsupported right side in XTree_Field', FDocPos);

  Result := inherited;
end;


function XTree_Field.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var
  Offset: PtrInt;
  leftVar: TXprVar;
  Field: XTree_Identifier;
  invoke: XTree_Invoke;
begin
  Result := NullResVar;
  if Self.Right is XTree_Identifier then
  begin
    if not (Self.Left.ResType() is XType_Record) then
      RaiseExceptionFmt('Cannot access fields on non-record type `%s`', [Self.Left.ResType().ToString], Self.Left.FDocPos);

    Field   := Right as XTree_Identifier;
    Offset  := XType_Record(Self.Left.ResType()).FieldOffset(Field.Name);

    if Offset = -1 then
      RaiseExceptionFmt(eSyntaxError, 'Unrecognized fieldname `%`', [Field.Name], Field.FDocPos);

    leftVar := Self.Left.CompileLValue(NullResVar);
    if (LeftVar.Reference) then
    begin
      LeftVar.VarType := ctx.GetType(xtInt);
      ctx.Emit(GetInstr(icADD, [LeftVar, Immediate(Offset, ctx.GetType(xtInt)), LeftVar]), Self.FDocPos);
      Result := LeftVar;
      Result.VarType := Self.ResType();
    end else
    begin
      Result := LeftVar;
      Result.Addr += Offset;
      Result.VarType := Self.ResType();
    end;
  end else if Right is XTree_Invoke then
  begin
    invoke := XTree_Invoke(Right);
    Invoke.SelfExpr := Self.Left;
    Result := Invoke.Compile(Dest, Flags);
  end else
    RaiseException('Unsupported right side in XTree_Field', FDocPos);
end;

function XTree_Field.CompileLValue(Dest: TXprVar): TXprVar;
var
  Offset: PtrInt;
  Field: XTree_Identifier;
  leftVar: TXprVar;
  invoke: XTree_Invoke;
begin
  if Self.Right is XTree_Identifier then
  begin
    if not (Self.Left.ResType() is XType_Record) then
      RaiseExceptionFmt('Cannot access fields on non-record type `%s`', [Self.Left.ResType().ToString], Self.Left.FDocPos);

    Field   := Right as XTree_Identifier;
    Offset  := XType_Record(Self.Left.ResType()).FieldOffset(Field.Name);
    if Offset = -1 then
      RaiseExceptionFmt(eSyntaxError, 'Unrecognized fieldname `%`', [Field.Name], Field.FDocPos);

    LeftVar := Self.Left.CompileLValue(Dest);

    if (LeftVar.Reference) then
    begin
      LeftVar.VarType := ctx.GetType(xtInt);
      ctx.Emit(GetInstr(icADD, [LeftVar, Immediate(Offset, ctx.GetType(xtInt)), LeftVar]), Self.FDocPos);
      Result := LeftVar;
      Result.VarType := Self.ResType();
    end else
    begin
      Result := LeftVar;
      Result.Addr += Offset;
      Result.VarType := Self.ResType();
    end;
  end else
    Result := Inherited;
end;

function XTree_Field.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName);{$ENDIF}
  Self.Left.DelayedCompile(Dest, Flags);
  Self.Right.DelayedCompile(Dest, Flags);
  Result := NullResVar;
end;


// ============================================================================
// Invoke a method
//
constructor XTree_Invoke.Create(AFunc: XTree_Node; ArgList: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  Method := AFunc;
  Args   := ArgList;
  SelfExpr := nil;
end;

function XTree_Invoke.ResolveMethod(out Func: TXprVar; out FuncType: XType): Boolean;
var
  i: Int32;
  arguments: Array of XType;
begin
  Result := False;
  SetLength(arguments, Length(Self.Args));
  for i:=0 to High(Args) do
    begin
      if Args[i] = nil then
        RaiseExceptionFmt('Argument at index %d is nil', [i], FDocPos);
      Arguments[i] := Self.Args[i].ResType();
    end;

  if not(Self.Method is XTree_Identifier) then
    RaiseException('Cannot resolve method for non-identifier method node', Self.Method.FDocPos);

  if SelfExpr <> nil then
  begin
    if SelfExpr.ResType = nil then
      RaiseException('Self expression has no type', SelfExpr.FDocPos);

    Func := ctx.ResolveMethod(XTree_Identifier(Method).Name, Arguments, SelfExpr.ResType())
  end
  else
    Func := ctx.ResolveMethod(XTree_Identifier(Method).Name, Arguments, nil);

  FuncType := Func.VarType;
  Result := FuncType <> nil;
end;

function XTree_Invoke.ToString(offset:string=''): string;
var i: Int32;
begin

  if Method is XTree_Identifier then
  begin
    Result := Offset + XTree_Identifier(Method).Name+'(';
    for i:=0 to High(Args) do Result += Args[i].ToString() +', ';
    Result +=')';

  end else
  begin
    Result := Offset + Copy(Self.ClassName(), 7, Length(Self.ClassName())-6)+'(';
    for i:=0 to High(Args) do Result += Args[i].ToString() +', ';
    Result +=')';

  end;
end;

function XTree_Invoke.ResType(): XType;
var
  funcType: XType;
  func: TXprVar;
  ErrorArgs: string; ArgCount: Int32;
begin
  if FResType = nil then
  begin
    if Self.ResolveMethod(Func, funcType) then
      FResType := XType_Method(funcType).ReturnType
    else
    begin
      ErrorArgs := '';
      for ArgCount:=0 to High(Args) do
        ErrorArgs += Args[ArgCount].ResType().ToString + ',';

      if Method is XTree_Identifier then
        RaiseException('Cant resolve function: '+XTree_Identifier(Method).Name +' -> '+ErrorArgs, FDocPos)
      else
        RaiseException('Cant resolve function: ??', FDocPos);
    end;
  end;
  Result := inherited;
end;

function XTree_Invoke.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var
  Func: TXprVar;
  FuncType: XType_Method;

  procedure PushArgsToStack();
  var
    i, paramIndex, impliedArgs: Int32;
    initialArg, finalArg: TXprVar;
    expectedType: XType;
  begin
    impliedArgs := 0;
    // Handle the implicit 'self' argument first.
    if SelfExpr <> nil then
    begin
      impliedArgs := 1;
      initialArg := SelfExpr.CompileLValue(NullVar);
      if initialArg = NullResVar then
        RaiseException('Self expression compiled to NullResVar', SelfExpr.FDocPos);

      if initialArg.Reference then ctx.Emit(GetInstr(icPUSHREF, [initialArg]), FDocPos)
      else                         ctx.Emit(GetInstr(icPUSH,    [initialArg]), FDocPos);
    end;

    // Loop through the explicit arguments.
    for i := 0 to High(Args) do
    begin
      paramIndex := i + impliedArgs;
      if Args[i] = nil then
        RaiseExceptionFmt('Argument at index %d is nil', [i], FDocPos);

      initialArg := Args[i].Compile(NullVar);
      if initialArg = NullResVar then
        RaiseExceptionFmt('Argument at index %d compiled to NullResVar', [i], FDocPos);

      finalArg := initialArg;

      if finalArg.IsManaged(ctx) and (FuncType.Passing[paramIndex] <> pbRef) then
        ctx.Emit(GetInstr(icINCLOCK, [finalArg.IfRefDeref(ctx)]), FDocPos);

      expectedType := FuncType.Params[paramIndex];
      if (FuncType.Passing[paramIndex] = pbCopy) and (expectedType.BaseType <> initialArg.VarType.BaseType) then
      begin
        finalArg := ctx.EmitUpcastIfNeeded(initialArg, expectedType, True);
      end;

      if (finalArg.Reference) then
        ctx.Emit(GetInstr(icPUSHREF, [finalArg]), FDocPos)
      else
        ctx.Emit(GetInstr(icPUSH,    [finalArg]), FDocPos);
    end;
  end;

  procedure VerifyParams();
  var i, impliedArgs, paramIndex:Int32;
  begin
    impliedArgs := 0;
    if SelfExpr <> nil then
    begin
      impliedArgs := 1;
      if not FuncType.TypeMethod then RaiseException('Cannot call a non-method with a Self expression', FDocPos);
      if not FuncType.Params[0].CanAssign(SelfExpr.ResType()) then
        RaiseExceptionFmt('Incompatible type for Self expression: expected `%s`, got `%s`', [FuncType.Params[0].ToString, SelfExpr.ResType().ToString], SelfExpr.FDocPos);
    end;

    if Length(FuncType.Params) <> Length(Args)+impliedArgs then
      RaiseExceptionFmt('Expected %d arguments, got %d', [Length(FuncType.Params), Length(Args)], FDocPos);

    for i:=0 to High(Args) do
    begin
      paramIndex := i + impliedArgs;
      if (FuncType.Passing[paramIndex] = pbRef) then
      begin
         if not FuncType.Params[paramIndex].Equals(Args[i].ResType()) then
            RaiseExceptionFmt('Incompatible argument %d for "ref" parameter: expected `%s`, got `%s`', [i, FuncType.Params[paramIndex].ToString(), Args[i].ResType().ToString()], Args[i].FDocPos);
      end
      else // pbCopy
      begin
        if not FuncType.Params[paramIndex].CanAssign(Args[i].ResType()) then
           RaiseExceptionFmt('Incompatible argument %d: Cannot assign `%s` to parameter of type `%s`', [i, Args[i].ResType().ToString(), FuncType.Params[paramIndex].ToString()], Args[i].FDocPos);
      end;
    end;
  end;

var
  totalSlots: UInt16;
begin
  Result := NullResVar;
  Func   := NullResVar;

  if Method is XTree_Identifier then
    self.ResolveMethod(Func, XType(FuncType))
  else
  begin
    Func     := Method.Compile(NullVar);
    FuncType := XType_Method(func.VarType);
  end;

  if Func = NullResVar then
    RaiseException('[Invoke] Function not matched', FDocPos);
  if not(func.VarType is XType_Method) then
    RaiseException('[Invoke] Cannot invoke an identifier that is not a function', FDocPos);

  VerifyParams();

  if (FuncType.ReturnType <> nil) then
  begin
    if (Dest = NullResVar) then
      Dest := ctx.GetTempVar(FuncType.ReturnType);

    if Dest.IsManaged(ctx) then
      ctx.Emit(GetInstr(icFILL, [Dest, Immediate(Dest.VarType.Size), Immediate(0)]), FDocPos);

    ctx.Emit(GetInstr(icPUSH, [Dest]), FDocPos);
    Result := Dest;
  end;

  PushArgsToStack();

  totalSlots := Length(Args);
  if (FuncType.ReturnType <> nil) then
    Inc(totalSlots);
  if SelfExpr <> nil then
    Inc(totalSlots);

  if Func.MemPos = mpHeap then
    ctx.Emit(GetInstr(icINVOKEX, [Func, Immediate(totalSlots), Immediate(Ord(FuncType.ReturnType <> nil))]), FDocPos)
  else
    ctx.Emit(GetInstr(icINVOKE, [Func, Immediate(totalSlots)]), FDocPos);
end;


function XTree_Invoke.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var i:Int32;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName);{$ENDIF}

  for i:=0 to High(Self.Args) do
    Self.Args[i].DelayedCompile(Dest, Flags);

  Self.Method.DelayedCompile(Dest, Flags);
  if SelfExpr <> nil then
    Self.SelfExpr.DelayedCompile(Dest, Flags);

  Result := NullResVar;
end;


// ============================================================================
// Simple index operation
//
constructor XTree_Index.Create(AExpr, AIndex: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  Self.Expr  := AExpr;
  Self.Index := AIndex;

  Self.ForceTypeSize := 0;
end;

function XTree_Index.ToString(Offset:string=''): string;
begin
  Result := Offset + 'Index('+Self.Expr.ToString() +', '+ Self.Index.ToString()+')';
end;

function XTree_Index.ResType(): XType;
var
  exprType: XType;
  arrayType: XType_Array;
begin
  if (Self.FResType = nil) then
  begin
    exprType := Self.Expr.ResType();
    if not (exprType is XType_Array) then
      RaiseExceptionFmt('Cannot index into non-array type `%s`', [exprType.ToString], Self.Expr.FDocPos);

    arrayType := XType_Array(exprType);
    FResType := arrayType.ItemType;
    if FResType = nil then
      RaiseExceptionFmt('Array item type is nil for array of type `%s`', [arrayType.ToString], Self.Expr.FDocPos);
  end;
  Result := inherited;
end;

function XTree_Index.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var
  ArrVar, IndexVar, AddressVar: TXprVar;
  ItemSize: Integer;
begin
  Assert(Self.Expr.ResType is XType_Array, 'Index target must be an array @ '+FDocPos.ToString);

  if not (Self.Expr.ResType() is XType_Array) then
    RaiseExceptionFmt('Index target must be an array, got `%s`', [Self.Expr.ResType().ToString], FDocPos);

  ArrVar := Expr.Compile(NullResVar, Flags);
  if ArrVar = NullResVar then
    RaiseException('Array expression compiled to NullResVar', Expr.FDocPos);

  IndexVar := Index.Compile(NullResVar, Flags);
  if IndexVar = NullResVar then
    RaiseException('Index expression compiled to NullResVar', Index.FDocPos);

  // Ensure vars are on stack! We need a way to deal with this centrally
  if ArrVar.Reference   then ArrVar   := ArrVar.DerefToTemp(ctx);
  if IndexVar.Reference then IndexVar := IndexVar.DerefToTemp(ctx);

  // Calculate address: arr + index * item_size
  if ForceTypeSize = 0 then
    ItemSize := XType_Array(Expr.ResType()).ItemType.Size
  else
    ItemSize := ForceTypeSize;

  AddressVar := ctx.GetTempVar(ctx.GetType(EExpressBaseType.xtPointer));

  ctx.Emit(GetInstr(icFMA, [IndexVar, Immediate(ItemSize), ArrVar, AddressVar]), FDocPos);

  AddressVar.Reference := False;
  AddressVar.VarType   := ResType();
  Result := AddressVar.Deref(ctx, Dest);  // Dereference for read
end;

function XTree_Index.CompileLValue(Dest: TXprVar): TXprVar;
var
  ArrVar, IndexVar, AddressVar: TXprVar;
  ItemSize: Integer;
begin
  Assert(Self.Expr.ResType is XType_Array, 'Index target must be an array @ '+FDocPos.ToString);

  // Compile array base and index
  ArrVar   := Expr.CompileLValue(NullResVar);
  IndexVar := Index.Compile(NullResVar);

  // Ensure vars are on stack! We need a way to deal with this centrally
  if ArrVar.Reference   then ArrVar   := ArrVar.DerefToTemp(ctx);
  if IndexVar.Reference then IndexVar := IndexVar.DerefToTemp(ctx);

  // Calculate address: arr + index * item_size
  if ForceTypeSize = 0 then
    ItemSize := XType_Array(Expr.ResType()).ItemType.Size
  else
    ItemSize := ForceTypeSize;

  AddressVar := ctx.GetTempVar(ctx.GetType(EExpressBaseType.xtPointer));

  ctx.Emit(GetInstr(icFMA, [IndexVar, Immediate(ItemSize), ArrVar, AddressVar]), FDocPos);

  AddressVar.Reference := False;
  AddressVar.VarType   := ResType();

  Result := AddressVar;
  Result.Reference := True;
end;

function XTree_Index.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName);{$ENDIF}

  Self.Expr.DelayedCompile(Dest, Flags);
  Self.Index.DelayedCompile(Dest, Flags);
  Result := NullResVar;
end;

// ============================================================================
// IF statement
//    if (condition) then <stmts> end
//    if (condition) then <stmts> else <stmts> end
//    if (condition) <stmt>
//    if (condition) <stmt> else <stmt>
//
// NOT ALLOWED: if (condition) <stmt> else <stmts> end

(*
  Creates an IF statement node.
  AConds: Array of condition expressions.
  ABodys: Array of statement lists corresponding to each condition.
  AElseBody: Optional list of statements for the ELSE branch.
*)
constructor XTree_If.Create(AConds, ABodys: XNodeArray; AElseBody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  Self.Conditions := AConds;
  Self.Bodys      := ABodys;
  Self.ElseBody  := AElseBody;
end;

(*
  Returns a string representation of the IF statement for debugging or logging.
*)
function XTree_If.ToString(Offset:string=''): string;
begin
  Result := Offset + _AQUA_+'If'+_WHITE_+'(' + LineEnding;
  if (Length(Self.Conditions) > 0) and (Self.Conditions[0] <> nil) then
    Result += Self.Conditions[0].ToString(Offset+'  ')+ ', ' + LineEnding;
  if (Length(Self.Bodys) > 0) and (Self.Bodys[0] <> nil) then
    Result += Self.Bodys[0].ToString(Offset+'  ')+ ', ' + LineEnding;
  if Self.ElseBody <> nil then
    Result += Self.ElseBody.ToString(Offset+'  ') + LineEnding;
  Result += Offset + ')';
end;

(*
  Compiles the IF statement.
  Generates intermediate code for evaluating each condition and
  jumping to the corresponding body or skipping to the next condition/else branch.
*)
function XTree_If.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var
  i: Int32;
  nextCondJumps: array of PtrInt;
  boolVar: TXprVar;
  skipRestJump: PtrInt;
begin
  if (Self.Conditions = nil) or (Length(Self.Conditions) = 0) then
    RaiseException(eSyntaxError, 'If statement must have at least one condition', FDocPos);
  if (Self.Bodys = nil) or (Length(Self.Bodys) = 0) then
    RaiseException(eSyntaxError, 'If statement must have at least one body', FDocPos);
  if Length(Self.Conditions) <> Length(Self.Bodys) then
    RaiseException('Mismatched number of conditions and bodies in If statement', FDocPos);

  SetLength(nextCondJumps, Length(Self.Conditions));

  // Process each condition in turn
  for i := 0 to High(Self.Conditions) do
  begin
    if Self.Conditions[i] = nil then
      RaiseExceptionFmt('Condition at index %d is nil in If statement', [i], FDocPos);

    // Compile the condition
    boolVar := Self.Conditions[i].Compile(NullResVar, Flags);
    if boolVar = NullResVar then
      RaiseExceptionFmt('Condition at index %d did not compile to a valid result', [i], Self.Conditions[i].FDocPos);
    if not (boolVar.VarType.BaseType = xtBoolean) then
      RaiseExceptionFmt('If condition must be a boolean, got `%s`', [boolVar.VarType.ToString], Self.Conditions[i].FDocPos);

    // Emit jump if false → skip to next condition check
    nextCondJumps[i] := ctx.Emit(
      GetInstr(icJZ, [boolVar, NullVar]),
      Self.Conditions[i].FDocPos
    );

    if Self.Bodys[i] = nil then
      RaiseExceptionFmt('Body at index %d is nil in If statement', [i], FDocPos);

    // Compile the corresponding body
    Self.Bodys[i].Compile(NullResVar, Flags);

    // After executing this body, skip the rest of the if-chain
    if (i < High(Self.Conditions)) or (Self.ElseBody <> nil) then
    begin
      skipRestJump := ctx.Emit(
        GetInstr(icRELJMP, [NullVar]),
        Self.Bodys[i].FDocPos
      );
      ctx.PatchJump(nextCondJumps[i]);
      nextCondJumps[i] := skipRestJump;
    end else
      ctx.PatchJump(nextCondJumps[i]);
  end;


  if Self.ElseBody <> nil then
    Self.ElseBody.Compile(NullResVar, Flags);

  for i:=0 to High(nextCondJumps) do
    if nextCondJumps[i] <> 0 then
      ctx.PatchJump(nextCondJumps[i]);

  Result := NullResVar;
end;

(*
  Performs delayed compilation for all child nodes (conditions, bodies, else body)
  within the IF statement.
*)
function XTree_If.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var i: Int32;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName);{$ENDIF}

  for i:=0 to High(Self.Conditions) do
    if Self.Conditions[i] <> nil then
      Self.Conditions[i].DelayedCompile(Dest, Flags);

  for i:=0 to High(Self.Bodys) do
    if Self.Bodys[i] <> nil then
      Self.Bodys[i].DelayedCompile(Dest, Flags);

  if Self.ElseBody <> nil then
    Self.ElseBody.DelayedCompile(Dest, Flags);

  Result := NullResVar;
end;


// ============================================================================
// WHILE loop
//    while (condition) do <stmts> end
//    while (condition) <stmt>
//
(*
  Creates a WHILE loop node.
  ACond: The condition expression for the loop.
  ABody: The statement list to be executed as long as the condition is true.
*)
constructor XTree_While.Create(ACond: XTree_Node; ABody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  Self.Condition := ACond;
  Self.Body      := ABody;
end;

(*
  Returns a string representation of the WHILE loop for debugging or logging.
*)
function XTree_While.ToString(Offset:string=''): string;
begin
  Result := Offset + _AQUA_+'While'+_WHITE_+'(' + LineEnding;
  if Self.Condition <> nil then
    Result += Self.Condition.ToString(Offset + '  ') + ', ' + LineEnding;
  if Self.Body <> nil then
    Result += Self.Body.ToString(Offset + '  ') + LineEnding;
  Result += Offset + ')';
end;

(*
  Compiles the WHILE loop.
  Generates intermediate code for evaluating the condition,
  looping back to the condition, and exiting the loop when the condition is false.
*)
function XTree_While.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var
  loopStart, loopEnd: PtrInt;
  boolVar: TXprVar;
begin
  if Self.Condition = nil then
    RaiseException(eSyntaxError, 'While loop condition cannot be empty', FDocPos);
  if Self.Body = nil then
    RaiseException(eSyntaxError, 'While loop body cannot be empty', FDocPos);

  // Mark the start of the patching scope for this loop.
  ctx.PreparePatch();

  // The 'continue' target is the beginning of the condition check.
  loopStart := ctx.CodeSize();
  boolVar := Condition.Compile(NullResVar, Flags);
  if boolVar = NullResVar then
    RaiseException('While loop condition failed to compile', Condition.FDocPos);
  if not (boolVar.VarType.BaseType = xtBoolean) then
    RaiseExceptionFmt('While loop condition must be a boolean, got `%s`', [boolVar.VarType.ToString], Condition.FDocPos);

  // Emit the jump that will exit the loop.
  loopEnd := ctx.Emit(GetInstr(icJZ, [boolVar, NullVar]), Condition.FDocPos);

  // Compile the loop body. Any 'break' or 'continue' nodes inside
  // will emit their respective placeholder opcodes.
  Body.Compile(NullVar, Flags);

  // Emit the jump that brings execution back to the top of the loop.
  ctx.Emit(GetInstr(icRELJMP, [ctx.RelAddr(loopStart)]), FDocPos);

  // Patch the main exit jump to point to the instruction after the loop.
  ctx.PatchJump(loopEnd);

  // Now, run the patcher for all placeholders generated within our scope.
  // - 'continue' jumps back to the condition check at the top.
  // - 'break' jumps to the instruction immediately after the loop.
  ctx.RunPatch(icJCONT, loopStart);
  ctx.RunPatch(icJBREAK, ctx.CodeSize());

  // Clean up the patching scope.
  ctx.PopPatch();

  Result := NullResVar;
end;

(*
  Performs delayed compilation for the condition and body of the WHILE loop.
*)
function XTree_While.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var i: Int32;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName);{$ENDIF}

  if Self.Condition <> nil then
    Self.Condition.DelayedCompile(Dest, Flags);
  if Self.Body <> nil then
    Self.Body.DelayedCompile(Dest, Flags);

  Result := NullResVar;
end;


// ============================================================================
// TRY-EXCEPT
//    try <stmts> except <stmts> end
//
(*
  Creates a TRY-EXCEPT block node.
  ATryBody: The list of statements to execute within the try block.
  AExceptBody: The list of statements to execute if an exception occurs in the try block.
*)
constructor XTree_Try.Create(ATryBody, AExceptBody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  Self.TryBody    := ATryBody;
  Self.ExceptBody := AExceptBody;
end;

(*
  Returns a string representation of the TRY-EXCEPT block for debugging or logging.
*)
function XTree_Try.ToString(Offset:string=''): string;
begin
  Result := Offset + _AQUA_+'Try'+_WHITE_+'(' + LineEnding;
  if Self.TryBody <> nil then
    Result += Self.TryBody.ToString(Offset + '  ') + LineEnding;
  Result += Offset + ')'+ _AQUA_+' Try'+_WHITE_+'(' + LineEnding;
  if Self.ExceptBody <> nil then
    Result += Self.ExceptBody.ToString(Offset + '  ') + LineEnding;
  Result += Offset + ')' + LineEnding;
end;

(*
  Compiles the TRY-EXCEPT block.
  Generates intermediate code to mark the beginning of a try block,
  compile the try body, and set up jump targets for the exception handler and normal exit.
*)
function XTree_Try.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var
  catch, noExcept: PtrInt;
begin
  if Self.TryBody = nil then
    RaiseException(eSyntaxError, 'Try block body cannot be empty', FDocPos);
  if Self.ExceptBody = nil then
    RaiseException(eSyntaxError, 'Except block body cannot be empty', FDocPos);

  //try -->
  catch := ctx.Emit(GetInstr(icIncTry, [NullVar]), TryBody.FDocPos);
  TryBody.Compile(NullVar, Flags);
  ctx.Emit(GetInstr(icDecTry), TryBody.FDocPos);
  noExcept := ctx.Emit(GetInstr(icRELJMP, [NullVar]), ExceptBody.FDocPos);
  //except -->
  ctx.PatchArg(catch, ia1, ctx.CodeSize());
  ExceptBody.Compile(NullVar, Flags);
  //all good -->
  ctx.PatchJump(noExcept, ctx.CodeSize());

  Result := NullResVar;
end;

(*
  Performs delayed compilation for the try and except bodies.
*)
function XTree_Try.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var i: Int32;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName);{$ENDIF}

  if Self.TryBody <> nil then
    Self.TryBody.DelayedCompile(Dest, Flags);
  if Self.ExceptBody <> nil then
    Self.ExceptBody.DelayedCompile(Dest, Flags);

  Result := NullResVar;
end;


// ============================================================================
// FOR loop
//    for (pre_expr; condition; post_expr) do <stmts> end
//    for (pre_expr; condition; post_expr) <stmt>
//
// PS: Expressions and statements can be empty - for example to achieve inf loop
(*
  Creates a FOR loop node, supporting C-style for loops.
  AEntryStmt: Optional initial statement executed once before the loop begins.
  ACondition: Optional condition expression; loop continues as long as true.
  ALoopStmt: Optional statement executed after each iteration of the loop body.
  ABody: The statement list to be executed in each loop iteration.
*)
constructor XTree_For.Create(AEntryStmt, ACondition, ALoopStmt: XTree_Node; ABody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  Self.EntryStmt := AEntryStmt;
  Self.Condition := ACondition;
  Self.LoopStmt  := ALoopStmt;
  Self.Body      := ABody;
end;

(*
  Returns a string representation of the FOR loop for debugging or logging.
*)
function XTree_For.ToString(Offset:string=''): string;
begin
  Result := Offset + _AQUA_+'For'+_WHITE_+'(' + LineEnding;
  if Self.EntryStmt <> nil then
    Result += Self.EntryStmt.ToString(Offset + '  ') + ', ' + LineEnding;
  if Self.Condition <> nil then
    Result += Self.Condition.ToString(Offset + '  ') + ', ' + LineEnding;
  if Self.LoopStmt <> nil then
    Result += Self.LoopStmt.ToString(Offset + '  ') + ', ' + LineEnding;
  if Self.Body <> nil then
    Result += Self.Body.ToString(Offset + '  ') + LineEnding;
  Result += Offset + ')';
end;

(*
  Compiles the FOR loop.
  Generates intermediate code for the entry statement, condition check,
  body execution, loop statement execution, and the jump back to the condition.
*)
function XTree_For.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var
  loopStart, loopEnd, continueTarget: PtrInt;
  boolVar: TXprVar;
begin
  if Self.Body = nil then
    RaiseException(eSyntaxError, 'For loop body cannot be empty', FDocPos);

  // Mark the start of the patching scope for this loop.
  ctx.PreparePatch();

  // Compile the initializer statement, if it exists.
  if EntryStmt <> nil then
    EntryStmt.Compile(NullVar, Flags);

  // Mark the top of the loop for the final jump and condition check.
  loopStart := ctx.CodeSize();
  loopEnd := 0; // Initialize to 0, indicating no jump yet.

  // Compile the condition, if it exists.
  if Condition <> nil then
  begin
    boolVar := Condition.Compile(NullResVar, Flags);
    if boolVar = NullResVar then
      RaiseException('For loop condition failed to compile', Condition.FDocPos);
    if not (boolVar.VarType.BaseType = xtBoolean) then
      RaiseExceptionFmt('For loop condition must be a boolean, got `%s`', [boolVar.VarType.ToString], Condition.FDocPos);

    // Emit the jump that will exit the loop.
    loopEnd := ctx.Emit(GetInstr(icJZ, [boolVar, NullVar]), Condition.FDocPos);
  end;

  // Compile the loop body.
  Body.Compile(NullVar, Flags);

  // Mark the position of the increment statement. This is the 'continue' target.
  continueTarget := ctx.CodeSize();
  if LoopStmt <> nil then
    LoopStmt.Compile(NullVar, Flags);

  // Emit the jump that brings execution back to the top of the loop.
  ctx.Emit(GetInstr(icRELJMP, [ctx.RelAddr(loopStart)]), FDocPos);

  // If there was a condition, patch its exit jump.
  if loopEnd <> 0 then
    ctx.PatchJump(loopEnd);

  // Run the patcher for all placeholders generated within our scope.
  // - 'continue' jumps to the increment statement.
  // - 'break' jumps to the instruction immediately after the loop.
  ctx.RunPatch(icJCONT, continueTarget);
  ctx.RunPatch(icJBREAK, ctx.CodeSize());

  // Clean up the patching scope.
  ctx.PopPatch();

  Result := NullResVar;
end;


(*
  Performs delayed compilation for the entry statement, condition, body, and loop statement
  of the FOR loop.
*)
function XTree_For.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var i: Int32;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName);{$ENDIF}

  if EntryStmt <> nil then Self.EntryStmt.DelayedCompile(Dest, Flags);
  if Condition <> nil then Self.Condition.DelayedCompile(Dest, Flags);

  Self.Body.DelayedCompile(Dest, Flags);

  if LoopStmt <> nil then Self.LoopStmt.DelayedCompile(Dest, Flags);

  Result := NullResVar;
end;




// ============================================================================
// REPEAT-UNTIL loop
//
constructor XTree_Repeat.Create(ACond: XTree_Node; ABody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;
  Self.Condition := ACond;
  Self.Body      := ABody;
end;

function XTree_Repeat.ToString(offset: string): string;
begin
  Result := Offset + _AQUA_+'Repeat'+_WHITE_+'(' + LineEnding;
  if Self.Body <> nil then
    Result += Self.Body.ToString(Offset + '  ') + ', ' + LineEnding;
  if Self.Condition <> nil then
    Result += Self.Condition.ToString(Offset + '  ') + LineEnding;
  Result += Offset + ')';
end;

function XTree_Repeat.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var
  loopStart, continueTarget: PtrInt;
  boolVar: TXprVar;
begin
  if Self.Condition = nil then
    RaiseException(eSyntaxError, 'Repeat..Until loop condition cannot be empty', FDocPos);
  if Self.Body = nil then
    RaiseException(eSyntaxError, 'Repeat..Until loop body cannot be empty', FDocPos);

  // Mark the start of the patching scope for this loop.
  ctx.PreparePatch();

  // Mark the top of the loop. This is where we will jump back to.
  loopStart := ctx.CodeSize();

  // Compile the loop body.
  Body.Compile(NullVar, Flags);

  // The 'continue' target is the address of the condition check.
  continueTarget := ctx.CodeSize();
  boolVar := Condition.Compile(NullResVar, Flags);
  if boolVar = NullResVar then
    RaiseException('Repeat..Until condition failed to compile', Condition.FDocPos);
  if not (boolVar.VarType.BaseType = xtBoolean) then
    RaiseExceptionFmt('Repeat..Until condition must be a boolean, got `%s`', [boolVar.VarType.ToString], Condition.FDocPos);

  // Emit the conditional jump. The loop continues if the condition is FALSE (zero).
  ctx.Emit(GetInstr(icJZ, [boolVar, ctx.RelAddr(loopStart)]), Condition.FDocPos);

  // Now that the entire loop is emitted, run the patcher.
  ctx.RunPatch(icJCONT, continueTarget);
  ctx.RunPatch(icJBREAK, ctx.CodeSize());

  // Clean up the patching scope.
  ctx.PopPatch();

  Result := NullResVar;
end;

function XTree_Repeat.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName);{$ENDIF}
  if Self.Body <> nil then
    Self.Body.DelayedCompile(Dest, Flags);
  if Self.Condition <> nil then
    Self.Condition.DelayedCompile(Dest, Flags);
  Result := NullResVar;
end;



// ============================================================================
// Operators

(*
  Creates a unary operation node.
  Operation: The unary operator (e.g., op_Add, op_Sub, op_Addr, op_DEREF).
  ALeft: The single operand for the unary operation.
*)
constructor XTree_UnaryOp.Create(Operation: EOperator; ALeft: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FDocPos  := DocPos;
  Self.FContext := ACTX;

  Self.Left := ALeft;
  Self.OP   := Operation;
end;

(*
  Returns a string representation of the unary operation for debugging or logging.
  Note: The modification of OP for op_Sub should not happen in ToString.
*)
function XTree_UnaryOp.ToString(Offset:string=''): string;
begin
  Result := Offset + _LBLUE_+'UnaryOp'+_WHITE_+'(';
  REsult += _GREEN_+OperatorToStr(OP)+_WHITE_ +', ';
  if Self.Left <> nil then
    Result += Self.Left.ToString()
  else
    Result += '<nil>';
  Result += ')';
  // if OP = op_Sub then OP := op_USUB; // This line causes a side effect and should be removed or moved.
end;

(*
  Determines the resulting type of the unary operation.
  Handles pointer address-of, dereferencing, and unary plus/minus.
*)
function XTree_UnaryOp.ResType(): XType;
var
  leftType: XType;
begin
  if FResType = nil then
  begin
    if Self.Left = nil then
      RaiseException('Left operand of unary operator cannot be nil', FDocPos);

    leftType := Self.Left.ResType();
    if leftType = nil then
      RaiseExceptionFmt('Left operand of unary operator has no resolved type', [OperatorToStr(OP)], Self.Left.FDocPos);

    case op of
      op_Addr:
        FResType := ctx.GetType(xtPointer);
      op_DEREF:
        begin
          if not (leftType is XType_Pointer) then
            RaiseExceptionFmt('Cannot dereference non-pointer type `%s`', [leftType.ToString], Self.Left.FDocPos);
          FResType := XType_Pointer(leftType);
          if FResType = nil then
            RaiseExceptionFmt('Dereferenced pointer has no base type', [leftType.ToString], Self.Left.FDocPos);
        end;
      op_Add, op_Sub: // Unary plus/minus
        begin
          if not ((leftType is XType_Ordinal) or (leftType is XType_Float)) then
            RaiseExceptionFmt('Unary plus/minus only applicable to numeric types, got `%s`', [leftType.ToString], Self.Left.FDocPos);
          FResType := leftType;
        end;
      else
        RaiseExceptionFmt('Unary operator `%s` not supported for type `%s`', [OperatorToStr(OP), leftType.ToString], FDocPos);
    end;
  end;
  Result := inherited; // This should be FResType instead of inherited
end;

(*
  Compiles the unary operation, generating intermediate code.
  Handles address-of, dereferencing, and converts unary minus into a binary subtraction.
*)
function XTree_UnaryOp.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var
  LeftVar: TXprVar;
  NewLeft: XTree_Node; // NewRight is already 'Left'
  Instr: EIntermediate;
  leftType: XType;
begin
  if Self.Left = nil then
    RaiseException('Left operand of unary operator cannot be nil during compilation', FDocPos);

  Result := Dest;
  if Result = NullResVar then Result := ctx.GetTempVar(ResType()); // ResType() will perform type checking

  leftType := Self.Left.ResType(); // This should already be resolved and checked by ResType()

  case op of
    op_Add:
      Exit(Left.Compile(Dest, Flags)); // Unary plus is a no-op at bytecode level
    op_Addr:
      begin
        LeftVar := Left.Compile(NullResVar, Flags);
        if LeftVar = NullResVar then
          RaiseException('Left operand for address-of operator compiled to NullResVar', Left.FDocPos);
        ctx.Emit(GetInstr(OP2IC(OP), [LeftVar, Result]), FDocPos);
        Result.Reference := True;
      end;
    op_DEREF:
      begin
        LeftVar := Left.Compile(NullResVar, Flags);
        if LeftVar = NullResVar then
          RaiseException('Left operand for dereference operator compiled to NullResVar', Left.FDocPos);
        if not (LeftVar.VarType is XType_Pointer) then
          RaiseExceptionFmt('Cannot dereference non-pointer variable `%s`', [LeftVar.VarType.ToString], Left.FDocPos);
        ctx.Emit(GetInstr(icDREF, [LeftVar, Result]), FDocPos);
        Result.Reference := False;
      end;
    op_Sub: // Unary minus: represented as 0 - operand
      begin
        if (leftType is XType_Ordinal) then
          NewLeft := XTree_Int.Create('0', ctx, FDocPos)
        else if (leftType is XType_Float) then
          NewLeft := XTree_Float.Create('0', ctx, FDocPos)
        else
          RaiseExceptionFmt('Unary minus not supported for type `%s`', [leftType.ToString], Left.FDocPos);

        // Create a temporary BinaryOp to compile the subtraction
        with XTree_BinaryOp.Create(op_SUB, NewLeft, Left, ctx, FDocPos) do
        begin
          Result := Compile(Dest);
          if Result = NullResVar then
            RaiseException('Unary minus operation failed to compile', FDocPos);
        end;
      end;
    else
      RaiseExceptionFmt('Compilation for unary operator `%s` not implemented', [OperatorToStr(OP)], FDocPos);
  end;
end;

(*
  Performs delayed compilation for the left operand of the unary operation.
*)
function XTree_UnaryOp.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var i: Int32;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName);{$ENDIF}

  if Self.Left <> nil then
    Self.Left.DelayedCompile(Dest, Flags);

  Result := NullResVar;
end;



(*
  Creates a binary operation node.
  Operation: The binary operator (e.g., op_Add, op_Sub, op_EQ, op_AND).
  ALeft: The left operand of the binary operation.
  ARight: The right operand of the binary operation.
*)
constructor XTree_BinaryOp.Create(Operation:EOperator; ALeft, ARight: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FDocPos  := DocPos;
  Self.FContext := ACTX;

  Self.Left := ALeft;
  Self.Right:= ARight;
  Self.OP   := Operation;
end;

(*
  Returns a string representation of the binary operation for debugging or logging.
*)
function XTree_BinaryOp.ToString(Offset:string=''): string;
begin
  Result := Offset + _LBLUE_+'Operator'+_WHITE_+'(';
  if Self.Left <> nil then
    Result += Self.Left.ToString()  + ', '
  else
    Result += '<nil>, ';
  if Self.Right <> nil then
    Result += Self.Right.ToString() + ', '
  else
    Result += '<nil>, ';
  REsult += _GREEN_+OperatorToStr(OP)+_WHITE_;
  Result += ')';
end;

function XTree_BinaryOp.RedefineConstant(A,B: XTree_Node): Boolean;
begin
  Result := False; // Default to false
  if (A <> nil) and (B <> nil) and (A is XTree_Const) and
     (B.ResType() <> nil) and (A.ResType() <> nil) and (B.ResType() <> A.ResType()) then
  begin
    Result := XTree_Const(A).SetExpectedType(B.ResType.BaseType);
  end;
end;

(*
  Determines the resulting type of the binary operation based on the operands' types and the operator.
*)
function XTree_BinaryOp.ResType(): XType;
var
  leftType, rightType: XType;
begin
  if Self.Left = nil then
    RaiseException('Left operand of binary operator cannot be nil', FDocPos);
  if Self.Right = nil then
    RaiseException('Right operand of binary operator cannot be nil', FDocPos);

  if (FResType = nil) then
  begin
    RedefineConstant(Left, Right);
    RedefineConstant(Right, Left);

    leftType  := Left.ResType();
    rightType := Right.ResType();

    if leftType = nil then
      RaiseExceptionFmt('Left operand type could not be resolved for operator `%s`', [OperatorToStr(OP)], FDocPos);

    if rightType = nil then
      RaiseExceptionFmt('Right operand type could not be resolved for operator `%s`', [OperatorToStr(OP)], FDocPos);


    FResType := leftType.ResType(OP, rightType, FContext);
    if FResType = nil then
      RaiseExceptionFmt(eNotCompatible3, [OperatorToStr(OP), BT2S(leftType.BaseType), BT2S(rightType.BaseType)], FDocPos);
  end;
  Result := FResType; // Should return FResType not inherited
end;


(*
  Compiles the binary operation, generating intermediate code.
  Handles type promotion for arithmetic operations and short-circuiting for logical AND/OR.
*)
(*
  Compiles the binary operation, generating intermediate code.
  Handles type promotion for arithmetic operations and short-circuiting for logical AND/OR.
*)
function XTree_BinaryOp.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var
  LeftVar, RightVar, TmpBool: TXprVar;
  Instr: EIntermediate;
  CommonTypeVar: XType;

  function DoShortCircuitOp(): TXprVar;
  var
    Instr: EIntermediate;
    PatchPos: PtrInt;
  begin
    TmpBool := Left.Compile(NullResVar, Flags);
    if TmpBool = NullResVar then
      RaiseException('Left operand of short-circuit operation compiled to NullResVar', Left.FDocPos);
    if not (TmpBool.VarType.BaseType = xtBoolean) then
      RaiseExceptionFmt('Short-circuit operator requires boolean operand, got `%s`', [TmpBool.VarType.ToString], Left.FDocPos);

    Instr := TmpBool.VarType.EvalCode(OP, TmpBool.VarType);
    PatchPos := ctx.Emit(GetInstr(Instr, [TmpBool, NullVar]), FDocPos);

    RightVar := Right.Compile(TmpBool, Flags); // Right compiles to TmpBool if possible
    if RightVar = NullResVar then
      RaiseException('Right operand of short-circuit operation compiled to NullResVar', Right.FDocPos);
    if not (RightVar.VarType.BaseType = xtBoolean) then
      RaiseExceptionFmt('Short-circuit operator requires boolean operand, got `%s`', [RightVar.VarType.ToString], Right.FDocPos);

    ctx.PatchJump(PatchPos);
    Result := TmpBool;
  end;

begin
  Assert(not(OP in AssignOps), 'Assignment does not belong here, dont come again!');

  if Self.Left = nil then
    RaiseException('Left operand of binary operator cannot be nil during compilation', FDocPos);
  if Self.Right = nil then
    RaiseException('Right operand of binary operator cannot be nil during compilation', FDocPos);

  RedefineConstant(Left, Right);
  RedefineConstant(Right, Left);

  if OP in [op_AND, op_OR] then
    Exit(DoShortCircuitOp());

  // Determine the result variable. This logic remains the same.
  Result := Dest;
  if Dest = NullResVar then
  begin
    Result := ctx.GetTempVar(Self.ResType()); // Self.ResType() will handle type errors

    // dont really like this, but it solves some unexpected problems
    if Result.IsManaged(ctx) then
      ctx.Emit(GetInstr(icFILL, [Result, Immediate(Result.VarType.Size), Immediate(0)]), FDocPos);
  end;

  if Left.ResType() = nil then
    RaiseException('Cannot infer type from Left operand', FDocPos);
  if Right.ResType() = nil then
    RaiseException('Cannot infer type from Right operand', FDocPos);

  // Handle arithmetic operations with type promotion
  if OP in ArithOps+LogicalOps then
  begin
     // Determine common arithmetic type
    CommonTypeVar := ctx.GetType(CommonArithmeticCast(Left.ResType().BaseType, Right.ResType().BaseType));

    // Compile left operand and cast if needed
    LeftVar := Left.Compile(NullResVar, Flags);
    if LeftVar = NullResVar then
      RaiseException('Left operand failed to compile for arithmetic operation', Left.FDocPos);

    LeftVar := ctx.EmitUpcastIfNeeded(LeftVar.IfRefDeref(ctx), CommonTypeVar, False);

    // Compile right operand and cast if needed
    RightVar := Right.Compile(NullResVar, Flags);
    if RightVar = NullResVar then
      RaiseException('Right operand failed to compile for arithmetic operation', Right.FDocPos);

    RightVar := ctx.EmitUpcastIfNeeded(RightVar.IfRefDeref(ctx), CommonTypeVar, False);
  end
  else
  begin
    // Non-arithmetic operations.
    LeftVar := Left.Compile(NullResVar, Flags);
    if LeftVar = NullResVar then
      RaiseException('Left operand failed to compile for binary operation', Left.FDocPos);

    RightVar := Right.Compile(NullResVar, Flags);
    if RightVar = NullResVar then
      RaiseException('Right operand failed to compile for binary operation', Right.FDocPos);

    // Ensure operands are values on the stack, not references.
    if LeftVar.Reference  then LeftVar  := LeftVar.DerefToTemp(ctx);
    if RightVar.Reference then RightVar := RightVar.DerefToTemp(ctx);
  end;

  // Emit the binary operation. This logic remains the same.
  Instr := LeftVar.VarType.EvalCode(OP, RightVar.VarType);
  if Instr <> icNOOP then
  begin
    ctx.Emit(GetInstr(Instr, [LeftVar, RightVar, Result]), FDocPos);
  end
  else
    RaiseExceptionFmt(eNotCompatible3, [OperatorToStr(OP), BT2S(Left.ResType.BaseType), BT2S(Right.ResType.BaseType)], Left.FDocPos);
end;


(*
  Performs delayed compilation for the left and right operands of the binary operation.
*)
function XTree_BinaryOp.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var i: Int32;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName);{$ENDIF}

  if Self.Left <> nil then
    Self.Left.DelayedCompile(Dest, Flags);
  if Self.Right <> nil then
    Self.Right.DelayedCompile(Dest, Flags);

  Result := NullResVar;
end;


// ---------------------------------------------
// Assignment
(*
  Determines the resulting type of an assignment operation.
  Assignment typically does not produce a value, so it returns nil.
*)
function XTree_Assign.ResType(): XType;
begin
  Result := nil;
end;

(*
  Compiles an assignment operation.
  Handles simple assignments, compound assignments (where supported),
  record assignments (block moves), and managed type reference counting.
*)
function XTree_Assign.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var
  LeftVar, RightVar: TXprVar;
  Instr: EIntermediate;

  procedure AssignToRecord();
  var
    i: Int32;
    RecType: XType_Record;
    IdentNode: XTree_Identifier;
    FieldDest, FieldSource: XTree_Field;
    FieldAssign: XTree_Assign;
    LeftStub, RightStub: XTree_VarStub;
  begin
    RecType := Left.ResType as XType_Record;

    // 1) Evaluate the LHS and RHS address variables
    LeftVar  := Left.CompileLValue(NullResVar);
    RightVar := Right.CompileLValue(NullResVar);

    // 2) Create "stub" AST nodes to represent the already-computed locations.
    LeftStub  := XTree_VarStub.Create(LeftVar, ctx, Left.FDocPos);
    RightStub := XTree_VarStub.Create(RightVar, ctx, Right.FDocPos);

    try
      // 3) Loop through the fields and perform recursive assignment using the stubs.
      for i := 0 to RecType.FieldTypes.High do
      begin
        IdentNode   := XTree_Identifier.Create(RecType.FieldNames.Data[i], ctx, FDocPos);
        FieldDest   := XTree_Field.Create(LeftStub, IdentNode, ctx, FDocPos);
        FieldSource := XTree_Field.Create(RightStub,IdentNode, ctx, FDocPos);

        with XTree_Assign.Create(op_Asgn, FieldDest, FieldSource, ctx, FDocPos) do
        try
          Compile(NullResVar, Flags);
        finally
          Free;
        end;
      end;
    finally
      LeftStub.Free;
      RightStub.Free;
    end;

  end;

  procedure CheckRefcount(LeftVar, RightVar: TXprVar);
  begin
    if (LeftVar = NullResVar) or (RightVar = NullResVar) then
      Exit; // Cannot check refcount on NullResVar

    // records with managed types should be elementwise assigned to active refcount
    if (Left.ResType() is XType_Array) then // Assuming ResType() for Left is not nil
    begin
      if (LeftVar.Reference) and (RightVar <> LeftVar) then
        ctx.Emit(GetInstr(icINCLOCK, [RightVar]), FDocPos)
      else if (not LeftVar.Reference) then
                                    {dec,     inc}
        ctx.Emit(GetInstr(icREFCNT, [LeftVar, RightVar]), FDocPos)
    end;
  end;
begin
  Result := NullResVar;

  if Left = nil then
    RaiseException(eSyntaxError, 'Left hand side of assignment cannot be nil', FDocPos);
  if Right = nil then
    RaiseException(eSyntaxError, 'Right hand side of assignment cannot be nil', FDocPos);
  if Left is XTree_Const then // just fuck off
    RaiseException(eSyntaxError, eExpectedVar, Left.FDocPos);

  // try to make the constant the same type as the value we are assigning to.
  if (Right is XTree_Const) and (Left.ResType() <> nil) and (Right.ResType() <> nil) and (Left.ResType() <> Right.ResType()) then
    XTree_Const(Right).SetExpectedType(Left.ResType.BaseType);

  if (Left.ResType() is XType_Record) then
  begin
    AssignToRecord();
    Exit(NullResVar);
  end;

  LeftVar := Left.CompileLValue(NullResVar);
  if LeftVar = NullResVar then
    RaiseException('Left hand side of assignment did not compile to a valid LValue', Left.FDocPos);


  // Compile index assignment (for dereferenced pointers or array elements)
  if (LeftVar.Reference) then
  begin
    RightVar := Right.Compile(NullResVar, []);
    if RightVar = NullResVar then
      RaiseException('Right hand side of assignment to a reference failed to compile', Right.FDocPos);

    // Ensure right are in stack (very short route)
    if (RightVar.Reference) then RightVar := RightVar.DerefToTemp(ctx);

    // Maybe refcount & collect
    CheckRefcount(LeftVar, RightVar);
    ctx.EmitFinalizeVar(LeftVar, True);

    //  write: `a^ := value`
    ctx.Emit(STORE_FAST(LeftVar, RightVar, True), FDocPos);

    Exit;
  end;

  // Compile RHS for direct assignment
  // If LeftVar is a local and types match, try to compile Right directly into LeftVar
  if (LeftVar.VarType <> nil) and (Right.ResType() <> nil) and (LeftVar.VarType.BaseType = Right.ResType().BaseType) and (LeftVar.MemPos = mpLocal) then
  begin
    RightVar := Right.Compile(LeftVar, Flags);
  end
  else
  begin
    RightVar := Right.Compile(NullResVar, Flags);
  end;

  if RightVar = NullResVar then
    RaiseException('Right hand side of assignment failed to compile', Right.FDocPos);


  // Ensure right are in stack
  if RightVar.Reference then RightVar := RightVar.DerefToTemp(ctx);

  // Maybe refcount & collect
  CheckRefcount(LeftVar, RightVar);
  ctx.EmitFinalizeVar(LeftVar, True);

  // XXX: drop assign to self to self (not sure we can trust this conditional)
  if (LeftVar.MemPos = RightVar.MemPos) and (LeftVar.Addr = RightVar.Addr) and (OP = op_Asgn) then
      Exit; // Optimization: A := A is a no-op for direct assignment

  // Handle different assignment types
  if (LeftVar.VarType <> nil) and (RightVar.VarType <> nil)
  and(LeftVar.VarType.EvalCode(OP, RightVar.VarType) <> icNOOP) then
  begin
    // Simple assignment: `x := value` or simple compound assignments
    Instr := LeftVar.VarType.EvalCode(OP, RightVar.VarType);
    if (Instr = icMOV) and (LeftVar.MemPos = mpLocal) then
      ctx.Emit(STORE_FAST(LeftVar, RightVar, False), FDocPos)
    else
      ctx.Emit(GetInstr(Instr, [LeftVar, RightVar]), FDocPos);
  end
  else
    RaiseExceptionFmt(eNotCompatible3, [OperatorToStr(OP), BT2S(Left.ResType.BaseType), BT2S(Right.ResType.BaseType)], Left.FDocPos);
end;

(*
  Performs delayed compilation for the left and right operands of the assignment.
*)
function XTree_Assign.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var i: Int32;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName);{$ENDIF}

  if Self.Left <> nil then
    Self.Left.DelayedCompile(Dest, Flags);
  if Self.Right <> nil then
    Self.Right.DelayedCompile(Dest, Flags);

  Result := NullResVar;
end;

// ============================================================================
// print keyword... meh
//
(*
  Creates a print statement node.
  ArgList: List of expressions to be printed.
*)
constructor XTree_Print.Create(ArgList: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FDocPos  := DocPos;
  Self.FContext := ACTX;

  Self.Args := ArgList;
end;

(*
  Returns a string representation of the print statement for debugging or logging.
*)
function XTree_Print.ToString(Offset:string=''): string;
var i:Int32;
begin
  Result := Offset + _AQUA_+'Print'+_WHITE_+'(';
  if Self.Args <> nil then
  begin
    for i:=0 to High(Self.Args) do
    begin
      if Self.Args[i] <> nil then
        Result += Self.Args[i].ToString('')
      else
        Result += '<nil>';
      if i <> High(Self.Args) then Result += ', ';
    end;
  end;
  Result += ')';
end;

(*
  Compiles the print statement, generating intermediate code to print the first argument.
  Note: This implementation only prints the first argument.
*)
function XTree_Print.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var arg: TXprVar;
begin
  if (Self.Args = nil) or (Length(Self.Args) = 0) then
    RaiseException(eSyntaxError, 'Print statement requires at least one argument', FDocPos);
  if Self.Args[0] = nil then
    RaiseException('First argument of print statement is nil', FDocPos);

  arg := Self.Args[0].Compile(NullResVar, Flags);
  if arg = NullResVar then
    RaiseException('Argument for print statement failed to compile', Self.Args[0].FDocPos);

  if arg.Reference then arg := arg.DerefToTemp(ctx);

  if arg.VarType = nil then
    RaiseException('Argument for print statement has no resolved type', Self.Args[0].FDocPos);

  ctx.Emit(GetInstr(icPRINT, [arg, Immediate(arg.VarType.Size)]), FDocPos);

  Result := NullVar;
end;

(*
  Performs delayed compilation for all arguments of the print statement.
*)
function XTree_Print.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var i: Int32;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName);{$ENDIF}

  if Self.Args <> nil then
  begin
    for i:=0 to High(Self.Args) do
      if Self.Args[i] <> nil then
        Self.Args[i].DelayedCompile(Dest, Flags);
  end;

  Result := NullResVar;
end;


end.
