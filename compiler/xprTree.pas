unit xprTree;
// Author: Jarl K. Holta
// License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
{$I header.inc}

interface

uses
  SysUtils, xprTypes, xprTokenizer, xprCompilerContext, xprIntermediate;

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

  (* return from function *)
  XTree_Return = class(XTree_Node)
    Expr: XTree_Node;
    constructor Create(AExpr: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar; override;
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
  Math, xprUtils, xprVartypes, xprErrors, xprLangdef;


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
  resVar: TXprVar;
  managed: TXprVarList;
  i: Int32;

  procedure CollectManaged(Value: TXprVar);
  var selfVar: TXprVar;
  begin
    //Exit;
    selfVar := ctx.TryGetLocalVar('Self');
    if (selfVar = Value) and Selfvar.Reference then Exit;

    if Value.IsManaged(ctx) then
    begin
      // This is a dynamic call, needs to be handled carefully
      // Assuming XTree_Invoke and XTree_Identifier constructor are robust
      with XTree_Invoke.Create(XTree_Identifier.Create('Collect', FContext, FDocPos), [], FContext, FDocPos) do
      try
        SelfExpr := XTree_VarStub.Create(Value.IfRefDeref(ctx), FContext, NoDocPos);
        Compile(NullResVar, []); // This compile call should ideally have error handling as well if it can fail silently.
      finally
        Free();
      end;
    end;
  end;

begin
  {handle managed declarations}
  managed := ctx.GetManagedDeclarations();

  resVar := NullResVar;
  if Self.Expr <> nil then
    resVar := Self.Expr.Compile(Dest, Flags);

  for i:=0 to managed.High() do
  begin
    if (managed.Data[i].Addr <> resVar.Addr) then
    begin
      if(not managed.Data[i].Reference) and (not managed.Data[i].IsGlobal) then
      begin
        ctx.Emit(GetInstr(icDECLOCK, [managed.Data[i]]), FDocPos);
      end;

      CollectManaged(managed.Data[i]);
    end;

  end;

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

  (*
    Pass all arugments by reference at first, aka, we pass the address
    To the functionbody we should add handling for copying (when not passed by ref).
  *)
  procedure PushArgsToStack();
  var i:Int32;
  arg: TXprVar;
  begin
    if SelfExpr <> nil then
    begin
      arg := SelfExpr.CompileLValue(NullVar);
      if arg = NullResVar then
        RaiseException('Self expression compiled to NullResVar', SelfExpr.FDocPos);

      // no refcount, self is reference!

      if arg.Reference then ctx.Emit(GetInstr(icPUSHREF, [arg]), FDocPos)
      else                  ctx.Emit(GetInstr(icPUSH, [arg]), FDocPos);
    end;

    i := 0;
    while i <= High(Args) do
    begin
      if Args[i] = nil then
        RaiseExceptionFmt('Argument at index %d is nil', [i], FDocPos);

      arg := Args[i].Compile(NullVar);
      if arg = NullResVar then
        RaiseExceptionFmt('Argument at index %d compiled to NullResVar', [i], FDocPos);

      if arg.IsManaged(ctx) and (FuncType.Passing[i] <> pbRef) then
        ctx.Emit(GetInstr(icINCLOCK, [arg.IfRefDeref(ctx)]), FDocPos);

      if arg.Reference then ctx.Emit(GetInstr(icPUSHREF, [arg]), FDocPos)
      else                  ctx.Emit(GetInstr(icPUSH, [arg]), FDocPos);
      Inc(i);
    end;
  end;

  procedure VerifyParams();
  var i, impliedArgs:Int32;
  begin
    impliedArgs := 0;
    if SelfExpr <> nil then
    begin
      impliedArgs := 1;
      if not FuncType.TypeMethod then RaiseException('Type method error thing', FDocPos);

      if(not(FuncType.Params[0].Equals(SelfExpr.ResType()))) or (not(FuncType.Params[0].CanAssign(SelfExpr.ResType()))) then
        RaiseException('Type method error thing 452696826021', FDocPos);
    end;

    if Length(FuncType.Params) <> Length(Args)+impliedArgs then
      RaiseExceptionFmt('Expected %d arguments, got %d', [Length(FuncType.Params), Length(Args)], FDocPos);

    for i:=impliedArgs to High(FuncType.Params) do
      if (not((FuncType.Passing[i] = pbRef)  and (FuncType.Params[i].Equals(Args[i-impliedArgs].ResType())))) and
         (not((FuncType.Passing[i] = pbCopy) and (FuncType.Params[i].CanAssign(Args[i-impliedArgs].ResType())))) then
        RaiseExceptionFmt('Incompatible argument [%d]', [i], Args[i].FDocPos);
  end;

var
  totalSlots: UInt16;
  i: Int32;
begin
  Result := NullResVar;
  Func   := NullResVar;

  if Method is XTree_Identifier then
  begin
    self.ResolveMethod(Func, XType(FuncType));
    if Func = NullResVar then
      RaiseException('[Invoke] Function `'+XTree_Identifier(Method).Name+'` not matched', FDocPos);
  end else
  begin
    Func     := Method.Compile(NullVar);
    FuncType := XType_Method(func.VarType);
  end;

  if Func = NullResVar then
    RaiseException('[Invoke] Function not matched', FDocPos);

  if not(func.VarType is XType_Method) then
    RaiseException('[Invoke] Cannot invoke identifer', FDocPos);

  VerifyParams();

  if (FuncType.ReturnType <> nil) then
  begin
    if (Dest = NullResVar) then
      Dest := ctx.GetTempVar(FuncType.ReturnType);
    ctx.Emit(GetInstr(icPUSH, [Dest]), FDocPos);
    Result := Dest;
  end;

  PushArgsToStack();

  // inc stackptr by current frame size to pass by it local must now operate on a negative basis
  // this can be done in invoke
  totalSlots := Length(Args);
  if (FuncType.ReturnType <> nil) then
    totalSlots := totalSlots + 1;  // Account for return slot

  if SelfExpr <> nil then
    totalSlots += 1;

  if Func.MemPos = mpHeap then
  begin
    ctx.Emit(GetInstr(icINVOKEX, [Func, Immediate(totalSlots), Immediate(Ord(FuncType.ReturnType <> nil))]), FDocPos)
  end else
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
  before, after: PtrInt;
  boolVar: TXprVar;
begin
  if Self.Condition = nil then
    RaiseException(eSyntaxError, 'While loop condition cannot be empty', FDocPos);
  if Self.Body = nil then
    RaiseException(eSyntaxError, 'While loop body cannot be empty', FDocPos);

  //while -->
  before := ctx.CodeSize();
  boolVar := Condition.Compile(NullResVar, Flags); // Use NullResVar to ensure a result variable
  if boolVar = NullResVar then
    RaiseException('While loop condition failed to compile', Condition.FDocPos);

  if not (boolVar.VarType.BaseType = xtBoolean) then
    RaiseExceptionFmt('While loop condition must be a boolean, got `%s`', [boolVar.VarType.ToString], Condition.FDocPos);

  after := ctx.Emit(GetInstr(icJZ, [boolVar, NullVar]), Condition.FDocPos);
  //-- do -->
  Body.Compile(NullVar, Flags);
  ctx.Emit(GetInstr(icRELJMP, [ctx.RelAddr(before)]), Condition.FDocPos);
  ctx.PatchJump(after);
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
  before, after: PtrInt;
  boolVar: TXprVar;
begin
  if Self.Body = nil then
    RaiseException(eSyntaxError, 'For loop body cannot be empty', FDocPos);

  after := 0;
  if EntryStmt <> nil then
    EntryStmt.Compile(NullVar, Flags);

  //for -->
  before := ctx.CodeSize();
  if Condition <> nil then
  begin
    boolVar := Condition.Compile(NullResVar, Flags);
    if boolVar = NullResVar then
      RaiseException('For loop condition failed to compile', Condition.FDocPos);
    if not (boolVar.VarType.BaseType = xtBoolean) then
      RaiseExceptionFmt('For loop condition must be a boolean, got `%s`', [boolVar.VarType.ToString], Condition.FDocPos);

    after := ctx.Emit(GetInstr(icJZ, [boolVar, NullVar]), Condition.FDocPos);
  end;

  //-- do -->
  Body.Compile(NullVar, Flags);
  if LoopStmt <> nil then
    LoopStmt.Compile(NullVar, Flags);

  ctx.Emit(GetInstr(icRELJMP, [ctx.RelAddr(before)]), FDocPos); // Using FDocPos of the For node itself

  if after <> 0 then // Only patch if there was a condition that created a jump
    ctx.PatchJump(after);

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
function XTree_BinaryOp.Compile(Dest: TXprVar; Flags: TCompilerFlags=[]): TXprVar;
var
  LeftVar, RightVar, TmpBool: TXprVar;
  Instr, InstrCast: EIntermediate;
  CommonType: EExpressBaseType;
  CommonTypeVar: XType;
  TmpLeft, TmpRight: TXprVar;

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

  Result := Dest;
  if Dest = NullResVar then
    Result := ctx.GetTempVar(Self.ResType()); // Self.ResType() will handle type errors


  if Left.ResType() = nil then
    RaiseException('Cannot infer type from Left operand', FDocPos);

  if Right.ResType() = nil then
  begin
    WriteFancy(Right.ToString());
    RaiseException('Cannot infer type from Right operand', FDocPos);
  end;


  // Handle arithmetic operations with type promotion
  if OP in ArithOps+LogicalOps then
  begin
    // Determine common arithmetic type
    CommonType := CommonArithmeticCast(Left.ResType().BaseType, Right.ResType().BaseType);
    CommonTypeVar := ctx.GetType(CommonType);

    // Compile left operand and cast if needed
    LeftVar := Left.Compile(NullResVar, Flags);
    if LeftVar = NullResVar then
      RaiseException('Left operand failed to compile for arithmetic operation', Left.FDocPos);

    // Ensure operands are in stack
    if LeftVar.Reference then LeftVar := LeftVar.DerefToTemp(ctx);

    if LeftVar.VarType.BaseType <> CommonType then
    begin
      TmpLeft := ctx.GetTempVar(CommonTypeVar);
      InstrCast := CommonTypeVar.EvalCode(op_Asgn, LeftVar.VarType);
      if InstrCast = icNOOP then
        RaiseExceptionFmt(eNotCompatible3, [OperatorToStr(op_Asgn), BT2S(LeftVar.VarType.BaseType), BT2S(CommonType)], FDocPos);
      ctx.Emit(GetInstr(InstrCast, [TmpLeft, LeftVar]), FDocPos);
      LeftVar := TmpLeft;
    end;

    // Compile right operand and cast if needed
    RightVar := Right.Compile(NullResVar, Flags);
    if RightVar = NullResVar then
      RaiseException('Right operand failed to compile for arithmetic operation', Right.FDocPos);

    // Ensure operands are in stack
    if RightVar.Reference then RightVar := RightVar.DerefToTemp(ctx);

    if RightVar.VarType.BaseType <> CommonType then
    begin
      TmpRight := ctx.GetTempVar(CommonTypeVar);
      InstrCast := CommonTypeVar.EvalCode(op_Asgn, RightVar.VarType);
      if InstrCast = icNOOP then
        RaiseExceptionFmt(eNotCompatible3, [OperatorToStr(op_Asgn), BT2S(RightVar.VarType.BaseType), BT2S(CommonType)], FDocPos);
      ctx.Emit(GetInstr(InstrCast, [TmpRight, RightVar]), FDocPos);
      RightVar := TmpRight;
    end;
  end
  else
  begin
    // Non-arithmetic operations
    LeftVar := Left.Compile(NullResVar, Flags);
    if LeftVar = NullResVar then
      RaiseException('Left operand failed to compile for binary operation', Left.FDocPos);

    RightVar := Right.Compile(NullResVar, Flags);
    if RightVar = NullResVar then
      RaiseException('Right operand failed to compile for binary operation', Right.FDocPos);

    // Ensure operands are in stack
    if LeftVar.Reference  then LeftVar  := LeftVar.DerefToTemp(ctx);
    if RightVar.Reference then RightVar := RightVar.DerefToTemp(ctx);
  end;

  // Emit the binary operation
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

  function GetBinop(): EOperator;
  begin
    case OP of
      op_AsgnBND: Result := op_BND;
      op_AsgnBOR: Result := op_BOR;
      op_AsgnDiv: Result := op_Div;
      op_AsgnSub: Result := op_Sub;
      op_AsgnMod: Result := op_Mod;
      op_AsgnMul: Result := op_Mul;
      op_AsgnAdd: Result := op_Add;
      op_AsgnXOR: Result := op_XOR;
      else Result := op_Unknown;
    end;
  end;

  procedure AssignToSimple();
  var
    BinOp: EOperator;
    TempVar: TXprVar;
  begin
    if OP in CompoundOps then
    begin
      // For compound assignments like +=, -= etc.
      // This part previously had a RaiseException, so it's a place for future implementation.
      BinOp := GetBinop();
      if BinOp = op_Unknown then
        RaiseExceptionFmt('Compound assignment operator `%s` not recognized', [OperatorToStr(OP)], FDocPos);

      // Perform the binary operation first
      // Result of binary op will be stored in a temp var, then assigned to LeftVar
      // LeftVar.CompileLValue gives us the address, now we need its value
      LeftVar := Left.Compile(NullResVar, Flags); // Compile Left as RValue for binary operation
      if LeftVar = NullResVar then
        RaiseException('Left operand for compound assignment failed to compile', Left.FDocPos);

      // Create a temporary binary operation node
      with XTree_BinaryOp.Create(BinOp, Left, Right, ctx, FDocPos) do
      begin
        TempVar := Compile(NullResVar, Flags); // Compile the binary operation
        if TempVar = NullResVar then
          RaiseException('Compound assignment binary operation failed to compile', FDocPos);
      end;

      // Now assign the result back to the LValue
      LeftVar := Left.CompileLValue(NullResVar); // Get LValue again
      if LeftVar = NullResVar then
        RaiseException('Left operand for compound assignment LValue failed to compile', Left.FDocPos);
      if LeftVar.Reference then LeftVar := LeftVar.DerefToTemp(ctx); // Dereference if it's a pointer to an LValue

      ctx.Emit(GetInstr(LeftVar.VarType.EvalCode(op_Asgn, TempVar.VarType), [LeftVar, TempVar]), FDocPos);
    end
    else
      ctx.Emit(GetInstr(OP2IC(OP), [LeftVar, RightVar]), FDocPos);
  end;

  procedure AssignToRecord();
  begin
    if OP <> op_Asgn then
      RaiseException('Compound assignment not supported for records', FDocPos);

    if (LeftVar = NullResVar) or (RightVar = NullResVar) then
      RaiseException('Cannot assign records with NullResVar operands', FDocPos);

    if LeftVar.MemPos = mpHeap then
    begin
      // Emit single block move operation for heap allocated records
      ctx.Emit(GetInstr(icMOVH, [
        LeftVar,              // Destination (stack offset or address for heap)
        RightVar,             // Source (stack offset or address for heap)
        Immediate(LeftVar.VarType.Size)   // Size in bytes
      ]), FDocPos);
    end else
    begin
      // Emit single block move operation for stack/static allocated records
      ctx.Emit(GetInstr(icMOV, [
        LeftVar,              // Destination (stack offset)
        RightVar,             // Source (stack offset)
        Immediate(LeftVar.VarType.Size)   // Size in bytes
      ]), FDocPos);
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

  // NOTE: duplicated in _Return.Compile()
  procedure CollectManaged(Value: TXprVar);
  var selfVar: TXprVar;
  begin
    //Exit;
    selfVar := ctx.TryGetLocalVar('Self');
    if (selfVar = Value) and Selfvar.Reference then Exit;

    if Value.IsManaged(ctx) then
    begin
      // This is a dynamic call, needs to be handled carefully
      // Assuming XTree_Invoke and XTree_Identifier constructor are robust
      with XTree_Invoke.Create(XTree_Identifier.Create('Collect', FContext, FDocPos), [], FContext, FDocPos) do
      try
        SelfExpr := XTree_VarStub.Create(Value, FContext, NoDocPos);
        Compile(NullResVar, []); // This compile call should ideally have error handling as well if it can fail silently.
      finally
        Free();
      end;
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
    CollectManaged(LeftVar);

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
  CollectManaged(LeftVar);

  // XXX: drop assign to self to self (not sure we can trust this conditional)
  if (LeftVar.MemPos = RightVar.MemPos) and (LeftVar.Addr = RightVar.Addr) and (OP = op_Asgn) then
      Exit; // Optimization: A := A is a no-op for direct assignment

  // Handle different assignment types
  if (LeftVar.VarType <> nil) and (RightVar.VarType <> nil) and (LeftVar.VarType.EvalCode(OP, RightVar.VarType) <> icNOOP) then
  begin
    // Simple assignment: `x := value` or simple compound assignments
    Instr := LeftVar.VarType.EvalCode(OP, RightVar.VarType);
    if (Instr = icMOV) and (LeftVar.MemPos = mpLocal) then
      ctx.Emit(STORE_FAST(LeftVar, RightVar, False), FDocPos)
    else
      ctx.Emit(GetInstr(Instr, [LeftVar, RightVar]), FDocPos);
  end
  else if (LeftVar.VarType <> nil) and (LeftVar.VarType.BaseType = xtRecord) then
    AssignToRecord()
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
