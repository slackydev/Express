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

    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;

    function Copy(): XTree_Node; override;
  end;

  (* basic stub helper *)
  XTree_VarStub = class(XTree_Node)
    VarDecl: TXprVar;
    constructor Create(AVar: TXprVar; ACTX: TCompilerContext; DocPos: TDocPos); overload;
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function CompileLValue(Dest: TXprVar): TXprVar; override;
    function Copy(): XTree_Node; override;
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
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  XTree_Bool  = class(XTree_Const)
    Value: Boolean;
    constructor Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  XTree_Pointer  = class(XTree_Const)
    Value: PtrInt;
    constructor Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  XTree_Char  = class(XTree_Const)
    Value: WideChar;
    constructor Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  XTree_Int  = class(XTree_Const)
    Value: Int64;
    constructor Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function SetExpectedType(ExpectedType: EExpressBaseType): Boolean; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  XTree_Float = class(XTree_Const)
    Value: Double;
    constructor Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function SetExpectedType(ExpectedType: EExpressBaseType): Boolean; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  XTree_String = class(XTree_Const)
    constructor Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  XTree_ImportUnit = class(XTree_Node)
    UnitPath: string;
    UnitAlias: string;
    constructor Create(APath, AAlias: string; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function ToString(offset: string = ''): string; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (* 
    A variable
  *)
  XTree_Identifier = class(XTree_Node)
    Name: string;
    constructor Create(AName:String; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset:string=''): string; override;

    function ResType(): XType; override;

    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function CompileLValue(Dest: TXprVar): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;
  XIdentNodeList = specialize TArrayList<XTree_Identifier>;

  (*
    Much like a stub node it needs no compilation
  *)
  XTree_Destructure = class(XTree_Node)
    Targets: XNodeArray;

    constructor Create(ATargets: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function CompileLValue(Dest: TXprVar): TXprVar; override;
    function ToString(offset: string = ''): string; override;
    function Copy(): XTree_Node; override;
  end;

  (*
    Declaring variables
    ref a,b,c
  *)
  XTree_NonLocalDecl = class(XTree_Node)
    Variables: XIdentNodeList;

    constructor Create(AVariables: XIdentNodeList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (*
    Declaring variables
    var a,b,c: type
    var a,b,c: expr
    var a,b,c: type = expr
  *)
  XTree_VarDecl = class(XTree_Node)
    Variables: XIdentNodeList;
    VarType: XType;
    Expr: XTree_Node;
    VarTypeName: string;
    IsConst, IsTypeOf: Boolean;

    constructor Create(AVariables: XIdentNodeList; AExpr: XTree_Node; AType: XType; Constant:Boolean; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    constructor Create(AVariable: string; AExpr: XTree_Node; AType: XType; Constant:Boolean; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;

    function ToString(offset:string=''): string; override;

    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (*
    Represents a destructuring declaration, which is a variant of vardecl
    var (a, b) := record
  *)
  XTree_DestructureDecl = class(XTree_Node)
    Pattern: XTree_Destructure; // The (a, b) part
    Expression: XTree_Node;            // The foo() part

    constructor Create(APattern: XTree_Destructure; AExpression: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (*
    Represents an initializer list literal., [1, 2, 3] or [10, 20]
  *)
  XTree_InitializerList = class(XTree_Node)
    Items: XNodeArray; // The list of expressions inside the brackets

    constructor Create(AItems: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function ToString(offset: string = ''): string; override;
    function Copy(): XTree_Node; override;
  end;

  XTree_TypeCast = class(XTree_Node)
    TargetType: XType;
    Expression: XTree_Node;
    constructor Create(ATargetType: XType; AExpr: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function CompileLValue(Dest: TXprVar): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (*
    Represents a full 'class ... end' declaration.
    Its job is not to emit executable code, but to build and register
    the class's metadata (its XType_Class) in the compiler context.
  *)
  XTree_ClassDecl = class(XTree_Node)
    ClassDeclName: string;
    ParentName: string;
    Fields: XNodeArray;
    Methods: XNodeArray;
    ClassDeclType: XType;

    constructor Create(AName, AParentName: string; AFields, AMethods: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function ToString(offset:string=''): string; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  //
  XTree_ClassCreate = class(XTree_Node)
    ClassTyp: XType;
    ClassIdent: string;
    Args: XNodeArray;

    constructor Create(AClassTyp: XType; AArgs: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    constructor Create(AClassIdent: String; AArgs: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  //
  XTree_DynCast = class(XTree_Node)
    Expression: XTree_Node;   // The node for 'myObject'
    TargetTypeNode: XTree_Identifier; // The node for 'TChildClass'

    constructor Create(AExpr: XTree_Node; ATargetType: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function CompileLValue(Dest: TXprVar): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  XTree_TypeIs = class(XTree_Node)
    Expression: XTree_Node;   // The node for 'myObject'
    TargetTypeNode: XTree_Identifier; // The node for 'TChildClass'

    constructor Create(AExpr: XTree_Node; ATargetType: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (* A conditional (ternary) expression that returns a value *)
  XTree_IfExpr = class(XTree_Node)
    Condition: XTree_Node;
    ThenExpr: XTree_Node;
    ElseExpr: XTree_Node;

    constructor Create(ACond, AThen, AElse: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function ToString(offset: string = ''): string; override;

    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (* return from function *)
  XTree_Return = class(XTree_Node)
    Expr: XTree_Node;
    constructor Create(AExpr: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (* Exit the current loop immediately *)
  XTree_Break = class(XTree_Node)
    constructor Create(ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function ToString(offset: string = ''): string; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (* Skip to the next iteration of the current loop *)
  XTree_Continue = class(XTree_Node)
    constructor Create(ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function ToString(offset: string = ''): string; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (* Declaring function *)
  XTree_Function = class(XTree_Node)
    Name:     string;
    ArgNames: TStringArray;
    ArgPass:  TPassArgsBy;
    ArgTypes: XTypeArray;
    RetType:  XType;
    PorgramBlock: XTree_ExprList;
    IsNested: Boolean;
    MiniCTX: TMiniContext;
    SingleExpression: Boolean;

    // Currently only used for VMT info
    Extra: SizeInt;

    // Two ways to achieve the same (This is an afterthought)
    SelfType: XType;
    TypeName: String;

    // populated after .Compile for .DelayedCompile
    PreCompiled: Boolean;
    FullyCompiled: Boolean;
    MethodVar: TXprVar;

    //internal function properties
    InternalFlags: TCompilerFlags;

    //constructor Create(AName: string; AArgNames: TStringArray; AArgTypes: XTypeArray; AProg: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    constructor Create(AName: string; AArgNames: TStringArray; ByRef: TPassArgsBy; AArgTypes: XTypeArray; ARet:XType; AProg: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ResType(): XType; override;
    function ToString(Offset:string=''): string; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  XTree_GenericFunction = class(XTree_Node)
    GenericFunction: XTree_Function;

    constructor Create(AMethod: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function CopyMethod(ArgTypes: XTypeArray; ASelfType, ARetType: XType; DocPos: TDocPos): XTree_Function;
    function Copy(): XTree_Node; override;
  end;

  (* A field lookup *)
  XTree_Field = class(XTree_Node)
    Left:  XTree_Node;
    Right: XTree_Node;

    constructor Create(ALeft, ARight: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(Offset:string=''): string; override;
    function ResType(): XType; override;

    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function CompileLValue(Dest: TXprVar): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (* Call a function *)
  XTree_Invoke = class(XTree_Node)
    Method: XTree_Node;
    Args: XNodeArray;
    SelfExpr: XTree_Node;
    SpecializeResType: string;

    constructor Create(AFunc: XTree_Node; ArgList: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(Offset:string=''): string; override;
    function ResolveMethod(out Func: TXprVar; out FuncType: XType): Boolean;
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;

    // type casts need this
    function CompileLValue(Dest: TXprVar): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  XTree_InheritedCall = class(XTree_Node)
    Args: XNodeArray;
    SelfExpr: XTree_Node;
    ResolvedParentMethod: TXprVar;

    constructor Create(AArgs: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (* An array lookup *)
  XTree_Index = class(XTree_Node)
    Expr, Index: XTree_Node;
    ForceTypeSize: Int32; // useful for length and refcount

    constructor Create(AExpr, AIndex: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(Offset:string=''): string; override;
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function CompileLValue(Dest: TXprVar): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  
  (* if statement *)
  XTree_If = class(XTree_Node)
    Conditions: XNodeArray;
    Bodys: XNodeArray;
    ElseBody: XTree_ExprList;
    constructor Create(AConds, ABodys: XNodeArray; AElseBody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset:string=''): string; override;
    
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (* case...of...end *)
  XTree_Case = class(XTree_Node)
    Expression: XTree_Node;
    Branches: TCaseBranchArray;
    ElseBody: XTree_Node;

    constructor Create(AExpression: XTree_Node; ABranches: TCaseBranchArray; AElseBody: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function ToString(offset: string = ''): string; override;
    function Copy(): XTree_Node; override;
  end;

  (* while loop *)
  XTree_While = class(XTree_Node)
    Condition: XTree_Node;
    Body: XTree_ExprList;
    constructor Create(ACond: XTree_Node; ABody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset:string=''): string; override;
    
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (* exeptions *)
  XTree_Raise = class(XTree_Node)
    ExceptionObject: XTree_Node;
    constructor Create(AExceptionObject: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  XTree_Try = class(XTree_Node)
      TryBody: XTree_ExprList;
      Handlers: TExceptionHandlerArray;
      ElseBody: XTree_Node; // For the final optional 'except' (catch-all)

      constructor Create(ATryBody: XTree_ExprList; AHandlers: TExceptionHandlerArray; AElseBody: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
      function ToString(offset: string = ''): string; override;
      function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
      function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
      function Copy(): XTree_Node; override;
    end;

  (* for loop *)
  XTree_For = class(XTree_Node)
    EntryStmt, Condition, LoopStmt: XTree_Node;
    Body: XTree_ExprList;
    constructor Create(AEntryStmt, ACondition, ALoopStmt: XTree_Node; ABody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset:string=''): string; override;

    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (* Pascal-style repeat-until loop *)
  XTree_Repeat = class(XTree_Node)
    Condition: XTree_Node;
    Body: XTree_ExprList;
    constructor Create(ACond: XTree_Node; ABody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset: string = ''): string; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags = []): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (*  operator types *)
  XTree_UnaryOp = class(XTree_Node)
    Left: XTree_Node;
    OP: EOperator;

    constructor Create(Operation:EOperator; ALeft: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset:string=''): string; override;

    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function CompileLValue(Dest: TXprVar): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  XTree_BinaryOp = class(XTree_Node)
    Left, Right: XTree_Node;
    OP: EOperator;
    
    constructor Create(Operation:EOperator; ALeft, ARight: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset:string=''): string; override;

    function RedefineConstant(A,B: XTree_Node): Boolean;
    procedure NodeTypeHint(A,B: XTree_Node);
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function CompileLValue(Dest: TXprVar): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  XTree_Assign = class(XTree_BinaryOp)
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (*  print 'print' *)
  XTree_Print = class(XTree_Node)
    Args: XNodeArray;
    constructor Create(ArgList: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset:string=''): string; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

function CompileAST(astnode:XTree_Node; writeTree: Boolean=False; doFree:Boolean = True): TIntermediateCode;

operator + (left: XNodeArray; Right: XTree_Node): XNodeArray;
function NodeArray(Arr: array of XTree_Node): XNodeArray;

implementation

uses
  xpr.Utils,
  xpr.Vartypes,
  xpr.Errors,
  xpr.Langdef,
  xpr.MagicIntrinsics,
  Math;


var
  __TIMETHING, __TIMETHING_TMP: Double;


// HELPERS

operator + (left: XNodeArray; Right: XTree_Node): XNodeArray;
begin
  Result := Copy(Left);
  SetLength(Result, Length(Result)+1);
  Result[High(Result)] := Right;
end;

function SameData(x: TInstructionData; y: TXprVar): Boolean;
begin
  Result := (x.Pos = y.MemPos) and (x.BaseType = y.VarType.BaseType) and
            (x.NestingLevel = y.NestingLevel) and (x.Reference = y.Reference) and
            (x.IsTemporary = y.IsTemporary);
end;

function NodeArray(Arr: array of XTree_Node): XNodeArray;
begin
  SetLength(Result, Length(Arr));
  Move(Arr[0], Result[0], SizeOf(XTree_Node));
end;


function CopyNodeArray(Src: XNodeArray): XNodeArray;
var
  i: Integer;
begin
  if Src = nil then Exit(nil);
  SetLength(Result, Length(Src));
  for i := 0 to High(Src) do
  begin
    if Src[i] <> nil then
      Result[i] := Src[i].Copy()
    else
      Result[i] := nil;
  end;
end;

function CopyIdentNodeList(Src: XIdentNodeList): XIdentNodeList;
var
  i: Integer;
begin
  Result.Init([]);
  if Src.Data = nil then Exit(Result);
  for i := 0 to Src.High() do
  begin
    if Src.Data[i] <> nil then
      Result.Add(Src.Data[i].Copy() as XTree_Identifier)
    else
      Result.Add(nil);
  end;
end;





function CompileAST(astnode:XTree_Node; writeTree: Boolean = False; doFree:Boolean = True): TIntermediateCode;
begin
  astnode.Compile(NullResVar, []);
  with XTree_Return.Create(nil, astnode.ctx, astnode.ctx.CurrentDocPos()) do
  try
    Compile(NullResVar, [])
  finally
    Free();
  end;

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

function IsSelf(Node: XTree_Node): Boolean;
begin
  Result := (Node is XTree_Identifier) and (XprCase(XTree_Identifier(Node).Name) = 'self');
end;

function IsConstructor(Node: XTree_Node): Boolean;
begin
  Result := (Node is XTree_Identifier) and (XprCase(XTree_Identifier(Node).Name) = 'create');
end;

function IsConstructor(Typ: XType): Boolean;
begin
  Result := (XprCase(Typ.Name) = 'create') and (Typ is XType_method) and XType_method(Typ).ClassMethod;
end;

function IsDestructor(Node: XTree_Node): Boolean;
begin
  Result := (Node is XTree_Identifier) and (XprCase(XTree_Identifier(Node).Name) = 'free');
end;

function IsDestructor(Typ: XType): Boolean;
begin
  Result := (XprCase(Typ.Name) = 'free') and (Typ is XType_method) and XType_method(Typ).ClassMethod;
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
    if Self.List[i] <> nil then
    begin
      Result += Self.List[i].ToString(Offset + '  ');
      if i <> High(Self.List) then Result += ', ';
      Result += LineEnding;
    end;
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

function XTree_ExprList.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var i:Int32;
begin
  i := 0;
  while i <= High(Self.List) do
  begin
    // Allow empty expressions for simplicty
    if Self.List[i] <> nil then
    begin
      Self.List[i].Compile(NullResVar, Flags);
    end;
    Inc(i);
  end;

  Result := NullResVar;
end;

function XTree_ExprList.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var i:Int32;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName(), ' - Delayed nodes: ', Length(DelayedList));{$ENDIF}
  for i:=0 to High(Self.List) do
    Self.List[i].DelayedCompile(NullResVar);

  for i:=0 to High(Self.DelayedList) do
    Self.DelayedList[i].Compile(NullResVar, Flags);

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

function XTree_VarStub.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
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

function XTree_Const.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  Result := NullResVar;
  ctx.RaiseException(eUnexpected, FDocPos);
end;

constructor XTree_Bool.Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  Self.StrValue := AValue;
  Self.Value    := AValue.ToBoolean();
  Self.Expected := xtBoolean;
end;

function XTree_Bool.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
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

function XTree_Pointer.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  Result := ctx.RegConst(Constant(Value, Expected));
end;


constructor XTree_Char.Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  if Length(AValue) <> 1 then ctx.RaiseException(eUnexpected, DocPos);
  Self.StrValue := AValue[1];
  Self.Value    := AValue[1];
  Self.Expected := xtAnsiChar;
end;

function XTree_Char.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
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

function XTree_Int.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
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

function XTree_Float.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
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

function XTree_String.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  constString: TXprVar;
begin
  constString := ctx.RegConst(Self.StrValue);

  // Reduce complication, and simplify instructionset by treating this as a
  // LOAD instruction. Same should be done for any other Table constants.

  // It's getting assigned to somewhere, no need
  if Dest <> NullResVar then
  begin
    ctx.Emit(GetInstr(icMOV, [dest, constString]), FDocPos);
    Result := dest;
  end else
  begin
    Result := ctx.GetTempVar(ctx.GetType(xtAnsiString));
    ctx.Emit(GetInstr(icMOV, [Result, constString]), FDocPos);
  end;

  with XTree_UnaryOp.Create(op_INCREF, nil, FContext, FDocPos) do
  try
    Left  := XTree_VarStub.Create(Result, FContext, FDocPos);
  finally
    Free();
  end;
end;


constructor XTree_ImportUnit.Create(APath, AAlias: string; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.UnitPath := APath;
  Self.UnitAlias := AAlias;
end;

function XTree_ImportUnit.ToString(offset: string): string;
begin
  Result := offset + _AQUA_+'Import "'+_PURPLE_+UnitPath+_AQUA_+'" as '+_YELLOW_+UnitAlias+_WHITE_;
end;

function XTree_ImportUnit.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  // The compile step simply delegates the work to the compiler context.
  ctx.ImportUnit(Self.UnitPath, Self.UnitAlias, FDocPos);
  Result := NullResVar;
end;

function XTree_ImportUnit.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  // The compile step simply delegates the work to the compiler context.
  ctx.DelayedImportUnit(Self.UnitPath, Self.UnitAlias, FDocPos);
  Result := NullResVar;
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
    if ctx.GetType(Self.Name) <> nil then
    begin
      Self.FResType := ctx.GetType(Self.Name);
    end
    else begin
      foundVar := ctx.GetVar(Self.Name, FDocPos);
      if foundVar = NullResVar then
        ctx.RaiseExceptionFmt('Identifier `%` not declared', [Self.Name], FDocPos);

      Self.FResType := foundVar.VarType;
      if Self.FResType = nil then
        ctx.RaiseExceptionFmt('Variable `%` has no defined type', [Self.Name], FDocPos);

      // XXX: a reference variable is passed as pointer, but automatic dereferenced
      // into the type it points to upon use.
      //if (Self.FResType.BaseType = xtPointer) and (foundVar.Reference) and (XType_Pointer(Self.FResType).PointsTo <> nil) then
      //  Self.FResType := XType_Pointer(Self.FResType).PointsTo;
    end;
  end;
  Result := inherited;
end;


function XTree_Identifier.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  foundVar, localVar, static_link: TXprVar;
  refType: XType_Pointer;
begin
  foundVar := ctx.GetVar(Self.Name, FDocPos);

  if (foundVar.NestingLevel > 0) or (foundVar.IsGlobal and (ctx.Scope <> GLOBAL_SCOPE)) then
  begin
    WriteLn('[Warning] Use `ref '+Self.Name+'` to declare intent to use nonlocal variables at '+Self.FDocPos.ToString);

    // create a local reference of global
    localVar := TXprVar.Create(foundVar.VarType);
    localVar.Reference := True;
    // dont register it, force new load every time - this handles uses without explicit ref declaration

    if foundVar.IsGlobal then begin
      ctx.Emit(GetInstr(icLOAD_GLOBAL, [localVar, foundVar]), FDocPos);
    end else
    begin
      static_link := ctx.TryGetLocalVar('!static_link');
      //if static_link = NullResVar then
        ctx.RaiseException('NOT IMPLEMENTED', FDocPos);

      // It's a non-local from a parent function. Use the new instruction.
      ctx.Emit(GetInstr(icLOAD_NONLOCAL,
        [ localVar,
          foundVar,
          static_link,
          Immediate(foundVar.NestingLevel)]),
        FDocPos
      );
    end;
    Result := localVar;
  end
  else
  begin
    // It's a simple local variable (NestingLevel = 0).
    // No special instruction is needed; just return the variable itself.
    Result := foundVar;
  end;

  if result = NullResVar then
    ctx.RaiseException('This is bad!');
end;

function XTree_Identifier.CompileLValue(Dest: TXprVar): TXprVar;
begin
  Result := Self.Compile(Dest,[]);
  Result.IsTemporary := False;
end;


constructor XTree_NonLocalDecl.Create(AVariables: XIdentNodeList; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  Self.Variables := AVariables;
end;

function XTree_NonLocalDecl.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  foundVar, localVar, static_link: TXprVar;
  refType: XType_Pointer;
  i: Int32;
begin
  for i:=0 to Self.Variables.High() do
  begin
    Result := NullResVar;
    foundVar := ctx.GetVar(Self.Variables.Data[i].Name, FDocPos);

    if (foundVar.NestingLevel > 0) or (foundVar.IsGlobal and (ctx.Scope <> GLOBAL_SCOPE)) then
    begin
      // create a local reference of global
      localVar := TXprVar.Create(foundVar.VarType);
      localVar.Reference := True; // this should be enough
      ctx.RegVar(Self.Variables.Data[i].Name, localVar, FDocPos);

      if foundVar.IsGlobal then
      begin
        ctx.Emit(GetInstr(icLOAD_GLOBAL, [localVar, foundVar]), FDocPos);
      end
      else  begin
        static_link := ctx.TryGetLocalVar('!static_link');
        //if static_link = NullResVar then
          ctx.RaiseException('NOT IMPLEMENTED', FDocPos);

        // It's a non-local from a parent function. Use the new instruction.
        ctx.Emit(GetInstr(icLOAD_NONLOCAL,
          [ localVar,
            foundVar,
            static_link,
            Immediate(foundVar.NestingLevel)]),
          FDocPos
        );
      end;
    end else if foundVar <> NullResVar then
      ctx.RaiseException('Cannot load local var as nonlocal!', FDocPos)
    else
      ctx.RaiseException(eUndefinedIdentifier, FDocPos);
  end;
end;

function XTree_NonLocalDecl.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  inherited;
end;

// ============================================================================
// Variable declaration
//
constructor XTree_VarDecl.Create(AVariables: XIdentNodeList; AExpr: XTree_Node; AType: XType; Constant:Boolean; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  Self.Variables := AVariables;
  Self.VarType   := AType;     // this is resolved in parsing. It's too early though!
  Self.Expr      := AExpr;
  Self.IsConst   := Constant;
end;

constructor XTree_VarDecl.Create(AVariable: string; AExpr: XTree_Node; AType: XType; Constant:Boolean; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  Self.Variables.Init([]);
  Self.Variables.Add(XTree_Identifier.Create(AVariable, FContext, FDocPos));

  Self.VarType   := AType;
  Self.Expr      := AExpr;
  Self.IsConst   := Constant;
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

  if VarType <> nil then
    Result += ': '+ Self.VarType.ToString()
  else
    Result += ': <expr>'; // might not be resolved yet

  if(Expr <> nil) then Result += _GRAY_ + ' <- '+ _WHITE_ + Self.Expr.ToString();
  Result += ')';
end;

function XTree_VarDecl.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  i, varidx:Int32;
  tmpRes: TXprVar;
begin
  if (VarType <> nil) and ((Self.VarType.BaseType = xtUnknown) or (IsTypeOf)) then
  begin
    if VarType.TypeOfExpr <> nil then
    begin
      Self.VarType := XTree_Node(VarType.TypeOfExpr).ResType();
    end
    else
      Self.VarType := ctx.GetType(Self.VarType.Name, Self.FDocPos);
  end;

  if VarType = nil then
    begin
      if Self.Expr = nil then
        ctx.RaiseException('Variable declaration requires an explicit type or an initial assignment', FDocPos);

      Self.VarType := Self.Expr.ResType();
      if Self.VarType = nil then
        ctx.RaiseExceptionFmt('Could not infer type for variable `%s`', [Self.Variables.Data[0].Name], FDocPos);
    end;

  for i:=0 to Self.Variables.High do
  begin
    Self.Variables.Data[i].FResType := self.VarType;

    ctx.RegVar(Self.Variables.Data[i].Name, self.VarType, Self.FDocPos, varidx);
  end;

  if Self.Expr <> nil then
  begin
    for i:=0 to Self.Variables.High do
    begin
      with XTree_Assign.Create(op_Asgn, Self.Variables.Data[i], Self.Expr, ctx, FDocPos) do
      try
        Compile(NullResVar, Flags);
      finally
        Free();
      end;

      case Variables.Data[i].Name of
        '__G_NativeExceptionTemplate':
          ctx.Emit(GetInstr(icSET_ERRHANDLER, [Self.Variables.Data[i].Compile(NullResVar, Flags)]), FDocPos);
      end;
    end;
  end else
  begin
    {fill with 0}
    for i:=0 to Self.Variables.High do
    begin
      tmpRes := Self.Variables.Data[i].Compile(NullResVar, Flags);
      ctx.Emit(GetInstr(icFILL, [TmpRes, Immediate(TmpRes.VarType.Size()), Immediate(0)]), FDocPos);
      // dont use var to default, zero fill instead.
    end;
  end;

  ctx.Variables.Data[varidx].NonWriteable:=Self.IsConst;
  Result := NullResVar;
end;

function XTree_VarDecl.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName());{$ENDIF}
  if Self.Expr <> nil then
    Self.Expr.DelayedCompile(Dest, Flags);
  Result := NullResVar;
end;

(*
  special case of vardecl
*)
constructor XTree_DestructureDecl.Create(APattern: XTree_Destructure; AExpression: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.Pattern := APattern;
  Self.Expression := AExpression;
end;

function XTree_DestructureDecl.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  i: Integer;
  SourceType: XType;
  RecType: XType_Record;
  TargetIdent: XTree_Identifier;
begin
  SourceType := Expression.ResType();
  if not (SourceType is XType_Record) then
    ctx.RaiseException('The right-hand side of a destructuring declaration must be a record type.', Expression.FDocPos);

  RecType := SourceType as XType_Record;
  if RecType.FieldNames.Size <> Length(Pattern.Targets) then
    ctx.RaiseExceptionFmt('The number of variables to declare (%d) does not match the number of fields in the source record (%d).',
      [Length(Pattern.Targets), RecType.FieldNames.Size], Pattern.FDocPos);

  // declare each new variable as per the record field types.
  for i := 0 to High(Pattern.Targets) do
  begin
    TargetIdent := Pattern.Targets[i] as XTree_Identifier;
    ctx.RegVar(TargetIdent.Name, RecType.FieldTypes.Data[i], TargetIdent.FDocPos);
  end;

  // Let assign handle it as per usual
  with XTree_Assign.Create(op_Asgn, Pattern, Expression, ctx, FDocPos) do
  try
    Compile(NullResVar, Flags);
  finally
    Free;
  end;

  Result := NullResVar;
end;

function XTree_DestructureDecl.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  Expression.DelayedCompile(Dest, Flags);
  Result := NullResVar;
end;


// ============================================================================
// x := [1,2,3,4,5,6]
//
//
constructor XTree_InitializerList.Create(AItems: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.Items := AItems;
  // An initializer list itself doesn't have a type; it gets its meaning
  // from the variable it's being assigned to.
  Self.FResType := nil;
end;

function XTree_InitializerList.ToString(offset: string): string;
var
  i: Integer;
begin
  Result := offset + _AQUA_ + 'InitializerList' + _WHITE_ + '[';
  for i := 0 to High(Items) do
  begin
    Result += Items[i].ToString('');
    if i < High(Items) then
      Result += ', ';
  end;
  Result += ']';
end;

function XTree_InitializerList.ResType(): XType;
var
  i: Integer;
  ItemType, CommonItemType: XType;
  CommonBaseType: EExpressBaseType;
begin
  // This method is now ONLY for INFERENCE MODE.
  // It should only be called when the list is a standalone expression.
  if FResType <> nil then Exit(FResType);

  if Length(Items) = 0 then
    ctx.RaiseException('Cannot infer type of an empty initializer list "[]".', FDocPos);

  CommonItemType := Items[0].ResType();
  for i := 1 to High(Items) do
  begin
    ItemType := Items[i].ResType();
    CommonBaseType := CommonArithmeticCast(CommonItemType.BaseType, ItemType.BaseType);
    if CommonBaseType = xtUnknown then
      ctx.RaiseExceptionFmt('Incompatible types in initializer list: Cannot find a common type between `%s` and `%s`.',
        [CommonItemType.ToString(), ItemType.ToString()], FDocPos);
    CommonItemType := ctx.GetType(CommonBaseType);
  end;

  FResType := XType_Array.Create(CommonItemType);
  ctx.AddManagedType(FResType);
  Result := FResType;
end;

function XTree_InitializerList.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  ItemCount,j: Int32;
  ItemResult, TargetVar: TXprVar;
  TargetType: XType;
  ArrayType: XType_Array;
  RecType: XType_Record;
  SetLenCall: XTree_Invoke;
  IndexNode: XTree_Index;
  AssignNode: XTree_Assign;
  TargetStub: XTree_VarStub;
  FieldNode: XTree_Field;
begin
  ItemCount := Length(Self.Items);

  // Check if a destination was provided
  if Dest <> NullResVar then
  begin
    // assignment "x := [1, 2]"
    // The type is determined by the LHS.
    TargetVar := Dest;
    TargetType := Dest.VarType;
  end
  else
  begin
    // inference "[1, 2]"
    // The type must be inferred from the list's content. This call will also cache the type.
    TargetType := Self.ResType();
    if TargetType = nil then Exit(NullResVar); // ResType already raised an error.

    // Create a temporary variable to hold the newly created array.
    TargetVar := ctx.GetTempVar(TargetType);
  end;

  TargetStub := XTree_VarStub.Create(TargetVar, ctx, FDocPos);
  try
    case TargetType.BaseType of
      xtArray, xtAnsiString, xtUnicodeString:
      begin
        ArrayType := TargetType as XType_Array;

        // Call SetLen.
        SetLenCall := XTree_Invoke.Create(
          XTree_Identifier.Create('SetLen', ctx, FDocPos),
          [XTree_Int.Create(IntToStr(ItemCount), ctx, FDocPos)], // The arguments
          ctx, FDocPos
        );
        SetLenCall.SelfExpr := TargetStub;

        try
          SetLenCall.Compile(NullResVar, Flags);
        finally
          SetLenCall.Free;
        end;

        // itemwise assignment, this can be optimized a bit
        for j := 0 to ItemCount - 1 do
        begin
          ItemResult := Items[j].Compile(NullResVar, Flags);
          ItemResult := ctx.EmitUpcastIfNeeded(ItemResult.IfRefDeref(ctx), ArrayType.ItemType, False);
          ItemResult.IsTemporary := True;

          IndexNode := XTree_Index.Create(TargetStub, XTree_Int.Create(IntToStr(j), ctx, FDocPos), ctx, FDocPos);
          AssignNode := XTree_Assign.Create(op_Asgn, IndexNode, XTree_VarStub.Create(ItemResult, ctx, FDocPos), ctx, FDocPos);
          try
            AssignNode.Compile(NullResVar, Flags);
          finally
            AssignNode.Free;
          end;
        end;
      end;

      xtRecord:
      begin
        RecType := TargetType as XType_Record;
        if ItemCount <> RecType.FieldNames.Size then
          ctx.RaiseExceptionFmt('Incorrect number of initializers for record `%s`: expected %d, got %d.', [RecType.Name, RecType.FieldNames.Size, ItemCount], FDocPos);

        for j := 0 to ItemCount - 1 do
        begin
          FieldNode := XTree_Field.Create(TargetStub, XTree_Identifier.Create(RecType.FieldNames.Data[j], ctx, FDocPos), ctx, FDocPos);
          ItemResult := FieldNode.Compile(NullResVar, Flags);
          ItemResult.IsTemporary := True;
          AssignNode := XTree_Assign.Create(op_Asgn, XTree_VarStub.Create(ItemResult, ctx, FDocPos), Items[j], ctx, FDocPos);
          try
            AssignNode.Compile(NullResVar, Flags);
          finally
            AssignNode.Free;
          end;
        end;
      end;

    else
      ctx.RaiseExceptionFmt('Type `%s` cannot be initialized with an initializer list.', [TargetType.ToString()], FDocPos);
    end;
  finally
    TargetStub.Free;
  end;

  Result := TargetVar;
end;

function XTree_InitializerList.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  i: Integer;
begin
  // An initializer list itself has no delayed logic, but its item expressions might.
  for i := 0 to High(Items) do
    if Items[i] <> nil then
      Items[i].DelayedCompile(Dest, Flags);
  Result := NullResVar;
end;



// ============================================================================
// (a,b,c) := <record>
//
constructor XTree_Destructure.Create(ATargets: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.Targets := ATargets;
end;

function XTree_Destructure.ToString(offset: string): string;
var
  i: Integer;
begin
  Result := offset + _AQUA_ + 'Destructure' + _WHITE_ + '(';
  for i := 0 to High(Self.Targets) do
  begin
    Result += Self.Targets[i].ToString('');
    if i < High(Self.Targets) then
      Result += ', ';
  end;
  Result += ')';
end;

function XTree_Destructure.ResType(): XType;
begin
  ctx.RaiseException('A destructuring pattern (a, b) cannot be used as a value.', FDocPos);
  Result := nil;
end;

function XTree_Destructure.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  Result := NullResVar;
end;

function XTree_Destructure.CompileLValue(Dest: TXprVar): TXprVar;
begin
  result := inherited;
end;


// ============================================================================
// Type casting
// > Double(myInt)
//
constructor XTree_TypeCast.Create(ATargetType: XType; AExpr: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.TargetType := ATargetType;
  Self.Expression := AExpr;
end;

function XTree_TypeCast.ResType(): XType;
begin
  // The result of a cast is always the target type.
  Result := Self.TargetType;
end;

function XTree_TypeCast.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  SourceVar: TXprVar;
  InstrCast: EIntermediate;
  IsReinterpretation: Boolean;
begin
  // always compile the source expression to a stable temporary first!
  SourceVar := Expression.Compile(NullResVar, Flags).IfRefDeref(ctx);


  // It's a reinterpretation if types are compatible pointers, or if they are
  // non-pointer ordinals of the exact same size.
  IsReinterpretation :=
    ( (Self.TargetType is XType_Pointer) and (SourceVar.VarType is XType_Pointer) ) or
    (
      (Self.TargetType.BaseType in XprOrdinalTypes) and
      (SourceVar.VarType.BaseType in XprOrdinalTypes) and
      (Self.TargetType.Size = SourceVar.VarType.Size)
    );

  // --- Step 3: Execute the chosen strategy. ---
  if IsReinterpretation then
  begin
    // PATH A: reinterpretation cast
    Result := SourceVar;
    Result.VarType := Self.TargetType;
  end
  else
  begin
    // PATH B: Dynamic conversion cast
    // Determine the destination for the converted value.
    Result := Dest;
    if Result = NullResVar then
      Result := ctx.GetTempVar(Self.TargetType);

    InstrCast := Self.TargetType.EvalCode(op_Asgn, SourceVar.VarType);
    if InstrCast = icNOOP then
      ctx.RaiseExceptionFmt('Invalid cast: Cannot convert type `%s` to `%s`.',
        [SourceVar.VarType.ToString(), Self.TargetType.ToString()], FDocPos);

    ctx.Emit(GetInstr(InstrCast, [Result, SourceVar]), FDocPos);
  end;
end;

function XTree_TypeCast.CompileLValue(Dest: TXprVar): TXprVar;
var
  SourceVar: TXprVar;
begin
  // A cast can only produce a valid L-Value if it's a simple
  // reinterpretation of one pointer type to another.
  // e.g., PInt32(myPointerToBytes)

  if not ((Self.TargetType is XType_Pointer) and (Self.Expression.ResType() is XType_Pointer)) then
  begin
    ctx.RaiseExceptionFmt('Invalid assignment: The result of a `%s` cast is not a variable that can be assigned to.',
      [Self.TargetType.ToString()], FDocPos);
    Exit(NullResVar);
  end;

  // Compile the underlying pointer expression.
  SourceVar := Expression.Compile(NullResVar, []);

  Result := SourceVar;
  Result.VarType := Self.TargetType;
end;


// ============================================================================
// Class Declaration
//
constructor XTree_ClassDecl.Create(AName, AParentName: string; AFields, AMethods: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  Self.ClassDeclName  := AName;
  Self.ParentName := AParentName;
  Self.Fields     := AFields;
  Self.Methods    := AMethods;
  SElf.ClassDeclType  := nil;
end;

function XTree_ClassDecl.ToString(Offset:string=''): string;
var i:Int32;
begin
  Result := Offset + _AQUA_+Self.ClassDeclName+_WHITE_+'(';
  for i:=0 to High(Self.Fields) do
  begin
    Result += LineEnding+Self.Fields[i].ToString(Offset + '  ');
  end;

  for i:=0 to High(Self.Methods) do
  begin
    Result += LineEnding+Self.Methods[i].ToString(Offset + '  ');
  end;
  Result += LineEnding+Offset + ')';
end;

(*
  Phase 1 Compilation: Builds the class metadata (XType_Class).
  This involves resolving the parent, building the field lists, creating the
  Virtual Method Table (VMT), and registering the new type.
*)
function XTree_ClassDecl.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  ParentType: XType_Class;
  NewClassType: XType_Class;
  FieldNames: XStringList;
  FieldTypes: XTypeList;
  FieldInfo: XFieldInfoList;
  Info: TFieldInfo;

  i, j: Integer;
  FieldDecl: XTree_VarDecl;
  MethodNode: XTree_Function;
  typ: XType;
  items: TVMList;
begin
  // --- Step 1 & 2: Resolve Parent and Build Field Lists ---
  ParentType := nil;
  if ParentName <> '' then
  begin
    typ := ctx.GetType(ParentName);
    if not (typ is XType_Class) then
      ctx.RaiseExceptionFmt('Parent `%s` is not a class type.', [ParentName], FDocPos);
    ParentType := typ as XType_Class;
  end;

  FieldNames.Init([]);
  FieldTypes.Init([]);
  FieldInfo.Init([]);

  for i := 0 to High(Fields) do
  begin
    FieldDecl := Fields[i] as XTree_VarDecl;
    for j:=0 to FieldDecl.Variables.High do
    begin
      FieldNames.Add(FieldDecl.Variables.Data[j].Name);
      Info.IsPrivate  := False;
      Info.IsWritable := not FieldDecl.IsConst;
      FieldInfo.Add(Info);

      if FieldDecl.VarType <> nil then
        FieldTypes.Add(FieldDecl.VarType)
      else if FieldDecl.Expr <> nil then
        FieldTypes.Add(FieldDecl.Expr.ResType())
      else
        ctx.RaiseExceptionFmt('Field `%s` must have an explicit type or an initializer.', [FieldDecl.Variables.Data[j].Name], FieldDecl.FDocPos);
    end;
  end;

  // --- Step 3 & 4: Create Types and Runtime VMT Shell ---
  NewClassType := XType_Class.Create(ParentType, FieldNames, FieldTypes, FieldInfo);
  NewClassType.Name := ClassDeclName;
  ctx.AddClass(ClassDeclName, NewClassType);

  // --- Step 5: Build the VMTs ---

  // A. Inherit from the parent with deep copy
  if ParentType <> nil then
    for i:=0 to High(ParentType.VMT.Items) do
      for j:=0 to High(ParentType.VMT.Items[i]) do
      begin
        items := ParentType.VMT.Items[i][j].val;
        items.Data := System.Copy(Items.data);
        NewClassType.VMT.Add(ParentType.VMT.Items[i][j].key, items);
      end;

  // B. Add/Override with this class's methods.
  for i := 0 to High(Methods) do
  begin
    MethodNode := XTree_Function(Methods[i]);
    MethodNode.SelfType := NewClassType;
    MethodNode.Compile(NullResVar, Flags+[cfClassMethod]);
  end;

  Result := NullResVar;
end;



constructor XTree_ClassCreate.Create(AClassTyp: XType; AArgs: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.ClassTyp := AClassTyp;
  Self.Args := AArgs;
end;

constructor XTree_ClassCreate.Create(AClassIdent: String; AArgs: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.ClassTyp := nil;
  Self.ClassIdent := AClassIdent;
  Self.Args := AArgs;
end;

function XTree_ClassCreate.ResType(): XType;
begin
  if ClassTyp = nil then
    Result :=  ctx.GetType(Self.ClassIdent)
  else
    Result := ClassTyp;

  FResType := Result;

  if Result = nil then
    RaiseException('Undefined class type', FDocPos);
end;

function XTree_ClassCreate.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  initInvoke: XTree_Invoke;
begin
  if Self.ClassTyp = nil then
    Self.ClassTyp := ctx.GetType(Self.ClassIdent);

  // 1. Determine the destination variable for the new object pointer.
  if Dest = NullResVar then
    Result := ctx.GetTempVar(ClassTyp)
  else
    Result := Dest;

  // 2. Emit the NEW instruction with the ClassID and InstanceSize.
  ctx.Emit(GetInstr(icNEW, [Result, Immediate(XType_Class(ClassTyp).ClassID), Immediate(XType_Class(ClassTyp).GetInstanceSize())]), FDocPos);

  // 3. Find the 'init' method in the class's compile-time VMT.
  if XType_Class(ClassTyp).VMT.Contains('create') then
  begin
    // 4. Build an XTree_Invoke node on the fly to call the init method.
    initInvoke := XTree_Invoke.Create(
      XTree_Identifier.Create('Create', ctx, FDocPos), // Method to call
      Self.Args,                                       // Arguments to pass
      ctx,
      FDocPos
    );
    // The 'self' for the init call is the new object we just created.
    initInvoke.SelfExpr := XTree_VarStub.Create(Result, ctx, FDocPos);

    // 5. Compile the invoke call. This pushes args and calls the method.
    try
      initInvoke.Compile(NullResVar, Flags); // init has no return value
    finally
      initInvoke.Free;
    end;
  end else
  begin
    // Class has no 'init' method. This is only an error if arguments were provided.
    if Length(Args) > 0 then
      ctx.RaiseExceptionFmt('Class `%s` has no "init" method that accepts arguments.', [ClassTyp.Name], FDocPos);
  end;
end;



//
// ============================================================================
// Dynamic Cast ('as' operator)
//
constructor XTree_DynCast.Create(AExpr: XTree_Node; ATargetType: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.Expression := AExpr;
  // We know from the parser that this will be an identifier.
  Self.TargetTypeNode := ATargetType as XTree_Identifier;
end;

function XTree_DynCast.ResType(): XType;
begin
  if FResType = nil then
  begin
    // The result type of the expression is the target type of the cast.
    FResType := ctx.GetType(Self.TargetTypeNode.Name, Self.TargetTypeNode.FDocPos);
    if FResType = nil then
      ctx.RaiseExceptionFmt('Unknown type name `%s` in cast expression.', [Self.TargetTypeNode.Name], Self.TargetTypeNode.FDocPos);
  end;
  Result := FResType;
end;

function XTree_DynCast.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  TargetType: XType_Class;
  SourceVar: TXprVar;
begin
  // Determine the target type from our ResType method.
  if not (ResType() is XType_Class) then
    ctx.RaiseException('Dynamic cast target must be a class type.', TargetTypeNode.FDocPos);
  TargetType := ResType() as XType_Class;

  // 1. Compile the expression being cast.
  SourceVar := Expression.Compile(NullResVar, Flags);
  if not (SourceVar.VarType is XType_Class) and not (SourceVar.VarType.BaseType = xtPointer) then
    ctx.RaiseException('Only class types can be dynamically cast.', Expression.FDocPos);

  // 2. Determine the destination variable for the result of the cast.
  if Dest = NullResVar then
    Result := ctx.GetTempVar(TargetType)
  else
    Result := Dest;

  // 3. Emit the DYN_CAST instruction.
  // Args: [DestVar], [SourceVar], [TargetClassID]
  ctx.Emit(GetInstr(icDYNCAST, [Result, SourceVar, Immediate(TargetType.ClassID)]), FDocPos);
end;

function XTree_DynCast.CompileLValue(Dest: TXprVar): TXprVar;
begin
  Result := Compile(Dest, []);
end;

function XTree_DynCast.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  // A cast expression has its own logic but must also process its child expression.
  Expression.DelayedCompile(Dest, Flags);
  Result := NullResVar;
end;


// ============================================================================
// Dynamic Type Check ('is' operator)
//
constructor XTree_TypeIs.Create(AExpr: XTree_Node; ATargetType: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.Expression := AExpr;
  // We know from the parser that the RHS will be an identifier.
  Self.TargetTypeNode := ATargetType as XTree_Identifier;
end;

function XTree_TypeIs.ResType(): XType;
begin
  // The result of an 'is' operator is always a boolean.
  if FResType = nil then
  begin
    FResType := ctx.GetType(xtBoolean);
  end;
  Result := FResType;
end;

function XTree_TypeIs.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  TargetType: XType;
  SourceVar: TXprVar;
begin
  TargetType := ctx.GetType(TargetTypeNode.Name, TargetTypeNode.FDocPos);

  if TargetType = nil then
    ctx.RaiseExceptionFmt('Unknown type name `%s` in "is" expression.', [TargetTypeNode.Name], TargetTypeNode.FDocPos);

  SourceVar := Expression.Compile(NullResVar, Flags);

  // 2. Determine the destination variable for the boolean result.
  if Dest = NullResVar then
    Result := ctx.GetTempVar(ctx.GetType(xtBoolean))
  else
    Result := Dest;

  case TargetType.BaseType of
    xtClass:
      begin
        if not (SourceVar.VarType is XType_Class) and not (SourceVar.VarType.BaseType = xtPointer) then
          ctx.RaiseException('Only class types can be checked with the "is" operator against another class.', Expression.FDocPos);

        ctx.Emit(GetInstr(icIS, [Result, SourceVar, Immediate(XType_Class(TargetType).ClassID)]), FDocPos);
      end;

    // Future extension that we can do with some type info if we wanna get fancy:
    // xtArray:
    //   begin
    //     // Emit a different instruction, e.g., icIS_ARRAY
    //     ctx.Emit(GetInstr(icIS_ARRAY, [Result, SourceVar]), FDocPos);
    //   end;

  else
    ctx.RaiseExceptionFmt('The "is" operator is not yet supported for type `%s`.', [TargetType.ToString()], TargetTypeNode.FDocPos);
  end;
end;

function XTree_TypeIs.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  Expression.DelayedCompile(Dest, Flags);
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
      ctx.RaiseException('Both branches of an if-expression must return a value', FDocPos);

    // Use the same logic as binary operations to find the common type
    commonBaseType := CommonArithmeticCast(thenType.BaseType, elseType.BaseType);

    if commonBaseType = xtUnknown then
       ctx.RaiseExceptionFmt(
        'Incompatible types in branches of if-expression: `%s` and `%s` have no common type',
        [thenType.ToString(), elseType.ToString()], FDocPos);

    FResType := FContext.GetType(commonBaseType);
  end;
  Result := FResType;
end;

function XTree_IfExpr.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
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
    ctx.RaiseExceptionFmt('If expression condition must be a boolean, got `%s`', [boolVar.VarType.ToString], Condition.FDocPos);

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

function XTree_IfExpr.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
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

function XTree_Return.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  managed: TXprVarList;
  i: Int32;
  this: XType_method;
begin
  Result := NullResVar;

  if Self.Expr <> nil then
  begin
    with XTree_Assign.Create(op_Asgn, nil, nil, ctx, FDocPos)  do
    try
      Left  := XTree_Identifier.Create('result', ctx, FDocPos);
      Right := Self.Expr;
      Compile(NullResVar, Flags);
    finally
      Free();
    end;
  end;

  {handle managed declarations, but does not touch result/references}
  if not(cfNoCollect in Flags) then
  begin
    managed := ctx.GetManagedDeclarations();
    for i:=0 to managed.High() do
      ctx.EmitFinalizeVar(managed.Data[i]);
  end;

  {decrease reference of result before leaving}
  {this will be increased once another variable takes responsibility}
  this := XType_Method(ctx.GetCurrentMethod());
  if (this <> nil) and (this.ReturnType <> nil) and (this.ReturnType.IsManagedType(ctx)) then
  begin
    with XTree_Identifier.Create('result', ctx, FDocPos) do
    try
      ctx.EmitDecref(Compile(NullResVar, Flags));
    finally
      Free();
    end;
  end;

  {return to sender}
  ctx.Emit(GetInstr(icRET, []), FDocPos);
end;

function XTree_Return.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
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

function XTree_Break.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
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

function XTree_Continue.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
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
  IsNested := (ACTX.Scope <> GLOBAL_SCOPE);
  SetLength(TypeName, 0);

  PreCompiled := False;
  FullyCompiled := False;
  Extra := 0;

  InternalFlags := [];

  Self.MiniCTX := nil;
end;


function XTree_Function.ResType(): XType;
var i: Int32; tempctx: TMiniContext;
begin
  if (FResType = nil) and (SingleExpression) and (Self.RetType = nil) then
  begin
    tempctx := ctx.GetMiniContext(); //temporary state
    for i:=0 to High(Self.ArgNames) do
      ctx.RegVar(ArgNames[i], ArgTypes[i], Self.FDocPos);

    Self.RetType := Self.PorgramBlock.List[0].ResType();
    ctx.SetMiniContext(tempctx);  //recover
  end;

  FResType := Self.RetType;
  Result := inherited;
end;

function XTree_Function.ToString(offset:string=''): string;
var i: Int32;
var selfstr: string;
begin
  selfstr := '';
  if SelfType <> nil then selfstr := SelfType.ToString+'.';
  Result := Offset + _AQUA_ +'Func->'+_WHITE_+selfstr+Self.Name+'(';
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
function XTree_Function.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  method: XType_Method;

  procedure AddSelf();
  var
    i: Int32;
  begin
    if SelfType = nil then
      SelfType := ctx.GetType(TypeName);

    // Now, add 'self' to the formal parameter list.
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

  procedure ExtendClassVMT();
  var
    ClassT: XType_Class;
    RuntimeVMT: TVirtualMethodTable;
    EntryName: string;
    VMTEntries: TVMList;
    NewEntry, UpdatedEntry: TVMItem;
    i: Int32;
    VMTIndex: Integer;
    FoundOverride: Boolean;
  begin
    ClassT := SelfType as XType_Class;
    RuntimeVMT := ctx.Intermediate.ClassVMTs.Data[ClassT.ClassID];
    EntryName := XprCase(Self.Name);

    FoundOverride := False;
    if ClassT.VMT.Get(EntryName, VMTEntries) then
    begin
      // A method or overload group with this name exists. Check for a true override.
      for i := 0 to VMTEntries.High do
      begin
        if VMTEntries.Data[i].MethodDef.Equals(method) then
        begin
          // --- OVERRIDE LOGIC ---
          VMTIndex := VMTEntries.Data[i].Index;
          Self.Extra := VMTIndex; // Store VMT index for DelayedCompile

          RuntimeVMT.Methods[VMTIndex] := -1; // Placeholder for now

          // Update the compile-time VMT entry with our new (but equal) signature.
          UpdatedEntry := VMTEntries.Data[i];
          UpdatedEntry.MethodDef := method;
          VMTEntries.Data[i] := UpdatedEntry;

          FoundOverride := True;
          break;
        end;
      end;
    end;

    if not FoundOverride then
    begin
      NewEntry.MethodDef := Method;
      NewEntry.Index     := ClassT.VMT.Size;

      // Update the RUNTIME VMT at the new index.
      if NewEntry.Index >= Length(RuntimeVMT.Methods) then
         ctx.RaiseExceptionFmt('Maximum number of virtual methods exceeded for class `%s`', [ClassName], FDocPos);

      RuntimeVMT.Methods[NewEntry.Index] := -1;
      Self.Extra := NewEntry.Index;

      // Add it to the COMPILE-TIME VMT list for this name.
      if not ClassT.VMT.Contains(EntryName) then
      begin
        VMTEntries.Init([NewEntry]);
        ClassT.VMT.Add(EntryName, VMTEntries);
      end else
      begin
        VMTEntries := ClassT.VMT[EntryName];
        VMTEntries.Add(NewEntry);
        ClassT.VMT.AddOrModify(EntryName, VMTEntries);
      end;
    end;
  end;
var
  i: Int32;
begin
  if PreCompiled then
    Exit(methodVar);

  if (TypeName <> '') or (SelfType <> nil) then
    AddSelf();

  method := XType_Method.Create(Name, ArgTypes, ArgPass, ResType(), SelfType <> nil);
  method.IsNested     := CTX.Scope <> GLOBAL_SCOPE;
  method.NestingLevel := CTX.Scope;
  method.ClassMethod := cfClassMethod in Flags;

  // Address is resolved after compilation as part of linking
  // this variable will be pushed to a list for late resolution.
  methodVar := TXprVar.Create(method, 0);
  //ctx.RegGlobalVar(Name, methodVar, FDocPos);
  ctx.RegMethod(Name, methodVar, FDocPos);

  // If this was a class method, we need to update the compile-time VMT entry
  // with the final, complete method type definition.
  if method.ClassMethod and (SelfType <> nil) and (SelfType is XType_Class) then
  begin
    ExtendClassVMT();
  end;

  Result := methodVar;

  Self.MiniCTX  := ctx.GetMiniContext();
  PreCompiled := True;

  ctx.DelayedNodes += Self;
end;

function XTree_Function.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  arg, ptrVar, staticLinkVar: TXprVar;
  i, ptrIdx, allocFrame: Int32;
  CreationCTX: TMiniContext;
  SelfClass: XType_Class;
  ResPtrType: XType_Pointer;
  tmpVar: TXprVar;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName(), ', Name: ', name);{$ENDIF}
  if FullyCompiled then Exit(NullResVar);

  Flags += InternalFlags;

  if XType_Method(Self.MethodVar.VarType).ClassMethod then
  begin
    SelfClass := XType_Method(Self.MethodVar.VarType).GetClass() as XType_Class;
    ctx.PushVirtualMethod(MethodVar.Addr, SelfClass.ClassID, Self.Extra);
  end
  else
    ctx.PushFunction(MethodVar.Addr);

  ctx.Emit(GetInstr(icPASS, [ctx.RegConst(Name)]), Self.FDocPos);

  ctx.PushCurrentMethod(Self.MethodVar.VarType);
  CreationCTX := ctx.GetMiniContext();
  ctx.SetMiniContext(MiniCTX);
  try
    ctx.IncScope();

    allocFrame    := ctx.Emit(GetInstr(icNEWFRAME, [NullVar]), Self.FDocPos);
    staticLinkVar := ctx.RegVar('!static_link', ctx.GetType(xtPointer), Self.FDocPos);

    for i:=High(ArgTypes) downto 0 do
    begin
      if (ArgPass[i] = pbRef) then
      begin
        // XXX
        ptrVar := ctx.RegVar(ArgNames[i], ctx.GetType(xtPointer), Self.FDocPos, ptrIdx);
        ctx.Variables.Data[ptrIdx].Reference := True; // XXX: might need to be levels of nesting
        ctx.Variables.Data[ptrIdx].VarType   := ArgTypes[i];
        ptrVar := ctx.Variables.Data[ptrIdx];
        ctx.Emit(GetInstr(icPOPH, [ptrVar]), Self.FDocPos);
      end else
      begin
        // XXX: IncRef here by using a temp, then assigning to var
        //      Instead of caller handled.
        arg := ctx.RegVar(ArgNames[i], ArgTypes[i], Self.FDocPos);
        if arg.IsManaged(ctx) then
        begin
          tmpVar := ctx.GetTempVar(arg.VarType);
          ctx.Emit(GetInstr(icPOP, [Immediate(arg.VarType.Size), tmpVar]), FDocPos);

          // Assign triggers refcounting and cleanup, we disable cleanup though
          with XTree_Assign.Create(op_Asgn, nil, nil, ctx, FDocPos) do
          try
            Left  := XTree_VarStub.Create(Arg, ctx, fdocpos);
            Right := XTree_VarStub.Create(tmpVar, ctx, fdocpos);
            Compile(NullResVar, [cfNoCollect]); // we dont need to collect
          finally;
            Free();
          end;
        end else
        begin
          ctx.Emit(GetInstr(icPOP, [Immediate(arg.VarType.Size), arg]), FDocPos);
        end;
      end;
    end;

    // result variable [reference]
    if Self.ResType() <> nil then
    begin
      ptrVar := ctx.RegVar('result', ctx.GetType(xtPointer), Self.FDocPos, ptrIdx);
      ctx.Variables.Data[ptrIdx].Reference := True;
      ctx.Variables.Data[ptrIdx].VarType   := Self.ResType();
      ptrVar := ctx.Variables.Data[ptrIdx];
      ctx.Emit(GetInstr(icPOPH, [ptrVar]), Self.FDocPos);
    end;

    (*
    // handle static link
    if IsNested then
    begin
      ctx.Emit(GetInstr(icPOPH, [staticLinkVar]), FDocPos);
    end;
    *)

    if SingleExpression then
    begin
      with XTree_Assign.Create(op_Asgn, nil, nil, FContext, FDocPos) do
      try
        Left := XTree_Identifier.Create('result', FContext, FDocPos);
        Right:= PorgramBlock.List[0];
        Compile(NullResVar, Flags);
      finally
        Free();
      end;
    end else
      PorgramBlock.Compile(NullResVar, Flags);

    with XTree_Return.Create(nil, FContext, ctx.CurrentDocPos()) do
    try
      Compile(NullResVar, Flags);
    finally
      Free();
    end;

    ctx.PatchArg(allocFrame, ia1, ctx.FrameSize());
  finally
    ctx.DecScope();
    ctx.SetMiniContext(CreationCTX);
    CreationCTX.Free();
    ctx.PopCurrentMethod();
  end;

  Result := NullResVar;
  FullyCompiled := True;
end;


// ============================================================================
// Generic function magic
//
constructor XTree_GenericFunction.Create(AMethod: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;
  Self.GenericFunction := AMethod as XTree_Function;
end;

function XTree_GenericFunction.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  // add this generic method with given name
  ctx.GenericMap[Self.GenericFunction.Name] := Self;
  Result := NullResVar;
end;

function XTree_GenericFunction.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  Result := NullResVar; (* nothing *)
end;

// var x:=Function<ReturnType>(arg)
// Function<ReturnType>(arg)
function XTree_GenericFunction.CopyMethod(ArgTypes: XTypeArray; ASelfType, ARetType: XType; Docpos: TDocPos): XTree_Function;
var
  i: Integer;

  // --- RECURSIVE HELPER FUNCTION ---
  // This is the core of the new logic. It can resolve any generic type,
  // including nested ones like 'array of array of T'.
  function _IsGenericType(Template: XType; ArgType: XType): Boolean;
  begin
    Result := False;
    // Check if blueprint is a simple generic placeholder <type, array, class, etc..>
    if (Template.BaseType = xtUnknown) then
    begin
      case XprCase(Template.Name) of
        'type':
          begin
            Exit(True); // 'type' accepts anything.
          end;
        'array':
          begin
            if (ArgType <> nil) and (not (ArgType is XType_Array)) then
              ctx.RaiseException('Generic method expects an array argument, but received ' + ArgType.ToString(), DocPos);
            Exit(True);
          end;
        'class':
          begin
            if (ArgType <> nil) and (not (ArgType is XType_Class)) then
              ctx.RaiseException('Generic method expects a class argument, but received ' + ArgType.ToString(), DocPos);
            Exit(True);
          end;
        'numeric':
          begin
            if (ArgType <> nil) and (not (ArgType is XType_Numeric)) then
              ctx.RaiseException('Generic method expects a numeric argument, but received ' + ArgType.ToString(), DocPos);

            if (ArgType <> nil) and (ArgType is XType_Pointer) then
              ctx.RaiseException('Generic method expects a numeric argument, but received ' + ArgType.ToString(), DocPos);

            Exit(True);
          end;
        else
          ctx.RaiseException('Generic unexpected argument, received ' + ArgType.ToString(), DocPos);
      end;
    end;

    // --- Recursive Case: The blueprint is a nested generic, like 'array of T' ---
    // We need to resolve the inner type.
    if (Template is XType_Array) then
    begin
      // First, ensure the concrete type is also an array.
      if (ArgType <> nil) and (not (ArgType is XType_Array)) then
        ctx.RaiseException('Generic method expects a nested array, but received ' + ArgType.ToString(), DocPos);

      if (Template as XType_Array).ItemType.BaseType <> xtUnknown then
        Exit(False);

      // Recursively resolve the element types.
      if ArgType <> nil then
        Exit(_IsGenericType((Template as XType_Array).ItemType, (ArgType as XType_Array).ItemType))
      else
        Exit(_IsGenericType((Template as XType_Array).ItemType, nil))
    end;

    // If we get here, it's not a generic type!
    Result := False;
  end;

  function ResolveGenericType(Template: XType; ArgType: XType): XType;
  begin
    if _IsGenericType(Template, ArgType) then
      Result := ArgType
    else
      Result := Template;
  end;

var
  initial_i: int32;
begin
  // 1. Basic argument count check.
  if Length(Self.GenericFunction.ArgTypes) <> Length(ArgTypes) then
    ctx.RaiseException('Unmatched generic function: Incorrect number of arguments.', DocPos);

  // 2. Create a clean clone of the function blueprint.
  Result := XTree_Function.Create(
    Self.GenericFunction.Name,
    Self.GenericFunction.ArgNames,
    Self.GenericFunction.ArgPass,
    [],
    Self.GenericFunction.RetType,
    Self.GenericFunction.PorgramBlock.Copy() as XTree_ExprList,
    FContext,
    FDocPos
  );
  Result.SingleExpression := Self.GenericFunction.SingleExpression;

  // 3. Resolve the 'self' type for generic extension methods.
  Result.SelfType := nil;
  initial_i := 0;
  if Self.GenericFunction.SelfType <> nil then
  begin
    Result.SelfType := ResolveGenericType(Self.GenericFunction.SelfType, ASelfType);
    initial_i := 1;
  end;

  // 4. Resolve all argument types.
  SetLength(Result.ArgTypes, Length(Self.GenericFunction.ArgTypes));
  for i := initial_i to High(ArgTypes) do
  begin
    Result.ArgTypes[i] := ResolveGenericType(
      Self.GenericFunction.ArgTypes[i-initial_i],
      ArgTypes[i]
    );
  end;

  if Self.GenericFunction.RetType <> nil then
  begin
    Result.RetType := ResolveGenericType(Self.GenericFunction.RetType, ARetType);

    if Result.RetType = nil then
      ctx.RaiseException('Could not resolve returntype for generic method `'+self.GenericFunction.Name+'`', DocPos);
  end;
  // 5. return type
  // ...
end;

// ============================================================================
// Resolve (record, class or namespace) field-lookups
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
  fullName: string;
  leftIdent: XTree_Identifier;
  rightIdent: XTree_Identifier;
  importedVar: TXprVar;
begin
  if (Self.FResType <> nil) then
    Exit(inherited);

  // --- PATH 1: Attempt to resolve as a static namespace access ---
  // This handles cases like 'Math.PI' or 'Math.Abs(...)'.
  // We can only do this if both sides are simple identifiers at parse time.
  if (Left is XTree_Identifier) and ((Right is XTree_Identifier) or (Right is XTree_Invoke)) then
  begin
    leftIdent := Left as XTree_Identifier;

    if Right is XTree_Identifier then
      rightIdent := Right as XTree_Identifier
    else // Right is XTree_Invoke
      rightIdent := XTree_Invoke(Right).Method as XTree_Identifier;

    fullName := leftIdent.Name + '.' + rightIdent.Name;

    // Try to find a symbol with the fully qualified name. - TryGetGlobalVar
    importedVar := ctx.TryGetVar(fullName);

    if importedVar <> NullResVar then
    begin
      // SUCCESS: It's a static symbol from a unit.
      if Right is XTree_Invoke then
      begin
        // For a function call, the result type is the function's return type.
        FResType := XType_Method(importedVar.VarType).ReturnType;
      end else
      begin
        // For a variable, the result type is the variable's type.
        FResType := importedVar.VarType;
      end;
      Exit(inherited);
    end;
  end;

  // --- PATH 2: Fallback to resolving as a dynamic record field access ---
  // This handles 'myRecord.field' or 'myRecord.Method()'.
  if Self.Right is XTree_Identifier then
  begin
    if (Self.Left.ResType() is XType_Record) then
      FResType := XType_Record(Self.Left.ResType()).FieldType(XTree_Identifier(Self.Right).Name)
    else if (Self.Left.ResType() is XType_Class) then
      FResType := XType_Class(Self.Left.ResType()).FieldType(XTree_Identifier(Self.Right).Name)
    else
      ctx.RaiseExceptionFmt('Cannot access fields on type `%s`', [Self.Left.ResType().ToString], Self.Left.FDocPos);
  end
  else if Self.Right is XTree_Invoke then
  begin
    Invoke := XTree_Invoke(Self.Right);
    Invoke.SelfExpr := Left;
    FResType := Invoke.ResType();
  end
  else
  begin
    ctx.RaiseException('Unsupported right side in field access expression', FDocPos);
  end;

  Result := inherited;
end;


function XTree_Field.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  Offset: PtrInt;
  leftVar, importedVar, objectPtr: TXprVar;
  Field: XTree_Identifier;
  invoke: XTree_Invoke;
  fullName: string;
  leftIdent, rightIdent: XTree_Identifier;
  ptrType: XType;
begin
  Result := NullResVar;

  Right.SetResTypeHint(Self.FResTypeHint); // pass on to whatever is right

  // --- PATH 1: Attempt to compile as a static namespace access ---
  if (Left is XTree_Identifier) and ((Right is XTree_Identifier) or (Right is XTree_Invoke)) then
  begin
    leftIdent := Left as XTree_Identifier;
    if Right is XTree_Identifier then
      rightIdent := Right as XTree_Identifier
    else
      rightIdent := XTree_Invoke(Right).Method as XTree_Identifier;

    fullName := leftIdent.Name + '.' + rightIdent.Name;
    importedVar := ctx.TryGetVar(fullName);

    if importedVar <> NullResVar then
    begin
      // SUCCESS: It's a static symbol.
      if Right is XTree_Identifier then
      begin
        // It's a global variable from a unit (e.g., Math.PI).
        Result := importedVar;
        Exit;
      end
      else // It's an XTree_Invoke
      begin
        // It's a global function call from a unit (e.g., Math.Abs(x)).
        invoke := Right as XTree_Invoke;
        // Replace the method name 'Abs' with a direct stub to the resolved function.
        invoke.Method := XTree_VarStub.Create(importedVar, ctx, FDocPos);
        Result := invoke.Compile(Dest, Flags);
        Exit;
      end;
    end;
  end;

  // --- PATH 2: Fallback to compiling as a dynamic record/class member access ---
  if (Self.Right is XTree_Identifier) then
  begin
    // --- SUB-PATH 2A: CLASS FIELD ACCESS ---
    if (Self.Left.ResType() is XType_Class) then
    begin
      Field   := Right as XTree_Identifier;
      Offset  := XType_Class(Self.Left.ResType()).FieldOffset(Field.Name);
      if Offset = -1 then
        ctx.RaiseExceptionFmt(eSyntaxError, 'Unrecognized fieldname `%s` in class `%s`', [Field.Name, Self.Left.ResType().ToString()], Field.FDocPos);

      // 1. Compile the 'Left' side to get the variable holding the object pointer.
      leftVar := Self.Left.Compile(NullResVar, Flags);
      leftVar.VarType := ctx.GetType(xtPointer);
      LeftVar := leftVar.IfRefDeref(ctx);

      // 2. DEREFERENCE the object pointer to get the actual address of the object on the heap.
      // We need a temporary variable to hold this heap address.
      objectPtr := ctx.GetTempVar(ctx.GetType(xtPointer));
      ctx.Emit(GetInstr(icADD, [LeftVar, Immediate(Offset), objectPtr]), Self.FDocPos);
      Result := ctx.GetTempVar(Self.ResType());
      ctx.Emit(GetInstr(icDREF, [Result, objectPtr, Immediate(Result.VarType.Size)]), FDocPos);
    end
    // --- SUB-PATH 2B: RECORD FIELD ACCESS ---
    else if (Self.Left.ResType() is XType_Record) then
    begin
      Field   := Right as XTree_Identifier;
      Offset  := XType_Record(Self.Left.ResType()).FieldOffset(Field.Name);
      if Offset = -1 then
        ctx.RaiseExceptionFmt(eSyntaxError, 'Unrecognized fieldname `%s`', [Field.Name], Field.FDocPos);

      leftVar := Self.Left.CompileLValue(NullResVar);
      if (LeftVar.Reference) then
      begin
        leftVar.VarType := ctx.GetType(xtPointer);
        objectPtr := ctx.GetTempVar(ctx.GetType(xtPointer));
        objectPtr.Reference := True;
        ctx.Emit(GetInstr(icADD, [LeftVar, Immediate(Offset, ctx.GetType(xtInt)), objectPtr]), Self.FDocPos);
        Result := objectPtr;
        Result.VarType := Self.ResType();
      end else
      begin
        Result := LeftVar;
        Result.Addr += Offset;
        Result.VarType := Self.ResType();
      end;
    end
    else
      ctx.RaiseExceptionFmt('Cannot access fields on non-record/class type `%s`', [Self.Left.ResType().ToString], Self.Left.FDocPos);

  end else if Right is XTree_Invoke then
  begin
    invoke := XTree_Invoke(Right);
    Invoke.SelfExpr := Self.Left;
    Result := Invoke.Compile(Dest, Flags);
  end else
  begin
    ctx.RaiseException('Unsupported right side in field access expression', FDocPos);
  end;
end;

function XTree_Field.CompileLValue(Dest: TXprVar): TXprVar;
var
  Offset: PtrInt;
  Field: XTree_Identifier;
  LocalVar, LeftVar, objectPtr: TXprVar;
  fullName: string;
begin
  // First, check if it's a namespace lookup. If so, it's an error.
  if (Left is XTree_Identifier) and (Right is XTree_Identifier) then
  begin
    fullName := XTree_Identifier(Left).Name + '.' + XTree_Identifier(Right).Name;
    if ctx.TryGetVar(fullName) <> NullResVar then
      ctx.RaiseException('Cannot assign to an imported symbol. Symbols from units are read-only.', FDocPos);
  end;

  // If it wasn't a namespace lookup, proceed with the record L-Value logic.
  if Self.Right is XTree_Identifier then
  begin
    // --- SUB-PATH 2A: CLASS FIELD ACCESS ---
    if (Self.Left.ResType() is XType_Class) then
    begin
      Field   := Right as XTree_Identifier;
      Offset  := XType_Class(Self.Left.ResType()).FieldOffset(Field.Name);
      if Offset = -1 then
        ctx.RaiseExceptionFmt('Unrecognized fieldname `%s` in class `%s`', [Field.Name, Self.Left.ResType().ToString()], Field.FDocPos);

      if (not XType_Class(Self.Left.ResType()).IsWritable(Field.Name)) then
      begin
        if(IsSelf(Left) and IsConstructor(ctx.GetCurrentMethod())) or
          ((XprCase(ctx.GetCurrentMethod().Name) = 'default') {XXX and verify internal code}) then
          // nothing
        else
          ctx.RaiseExceptionFmt('Cannot write to a class constant `%s`', [Field.Name], ctx.CurrentDocPos);
      end;

      // 1. Compile the 'Left' side to get the variable holding the object pointer.
      leftVar := Self.Left.CompileLValue(NullResVar);
      leftVar.VarType := ctx.GetType(xtPointer);
      LocalVar := leftVar.IfRefDeref(ctx);

      // 2. DEREFERENCE the object pointer to get the actual address of the object on the heap.
      // We need a temporary variable to hold this heap address.
      objectPtr := ctx.GetTempVar(ctx.GetType(xtPointer));
      ctx.Emit(GetInstr(icADD, [LocalVar, Immediate(Offset), objectPtr]), Self.FDocPos);

      Result := objectPtr;
      Result.VarType   := Self.ResType();
      Result.Reference := True;
      Result.IsTemporary:=LeftVar.IsTemporary;
    end
    // --- SUB-PATH 2B: RECORD FIELD ACCESS ---
    else if (Self.Left.ResType() is XType_Record) then
    begin
      Field   := Right as XTree_Identifier;
      Offset := XType_Record(Self.Left.ResType()).FieldOffset(Field.Name);

      if Offset = -1 then
        ctx.RaiseExceptionFmt(eSyntaxError, 'Unrecognized fieldname `%`', [Field.Name], Field.FDocPos);

      LeftVar := Self.Left.CompileLValue(Dest);

      if (LeftVar.Reference) then
      begin
        leftVar.VarType := ctx.GetType(xtPointer);
        objectPtr := ctx.GetTempVar(ctx.GetType(xtPointer));
        objectPtr.Reference := True;
        ctx.Emit(GetInstr(icADD, [LeftVar, Immediate(Offset, ctx.GetType(xtInt)), objectPtr]), Self.FDocPos);
        Result := objectPtr;
        Result.VarType := Self.ResType();
      end else
      begin
        Result := LeftVar;
        Result.Addr += Offset;
        Result.VarType := Self.ResType();
      end;

      Result.IsTemporary:=LeftVar.IsTemporary;
    end
    else
      ctx.RaiseExceptionFmt('Cannot access fields on non-record/class type `%s`', [Self.Left.ResType().ToString], Self.Left.FDocPos);
  end else
    Result := Inherited; // Will raise "Cannot be written to"
end;

function XTree_Field.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName);{$ENDIF}
  Self.Left.DelayedCompile(Dest, Flags);
  Self.Right.DelayedCompile(Dest, Flags);
  Result := NullResVar;
end;


// ============================================================================
// Invoke a method
//
// Most tasks, if not all are CALLEE handled, or should eventually be.
// This way callee can be simplified without dealing with forced bullshit from caller.
//
// All passing is technically by reference, but `copy` might cause a temp.
//
constructor XTree_Invoke.Create(AFunc: XTree_Node; ArgList: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;

  Method := AFunc;
  Args   := ArgList;
  SelfExpr := nil;
  SpecializeResType := '';
end;

function XTree_Invoke.ResolveMethod(out Func: TXprVar; out FuncType: XType): Boolean;
var
  i: Int32;
  arguments: XTypeArray;
  rettype: XType;
begin
  Result := False;
  arguments := [];
  SetLength(arguments, Length(Self.Args));

  rettype := nil;
  if SpecializeResType <> '' then
    rettype := ctx.GetType(SpecializeResType);

  if retType = nil then
    retType := Self.FResTypeHint;

  for i:=0 to High(Args) do
    begin
      if Args[i] = nil then
        ctx.RaiseExceptionFmt('Argument at index %d is nil', [i], FDocPos);
      Arguments[i] := Self.Args[i].ResType();
    end;

  if not(Self.Method is XTree_Identifier) then
    ctx.RaiseException('Cannot resolve method for non-identifier method node', Self.Method.FDocPos);

  if SelfExpr <> nil then
  begin
    if SelfExpr.ResType = nil then
      ctx.RaiseException('Self expression has no type', SelfExpr.FDocPos);

    Func := ctx.ResolveMethod(XTree_Identifier(Method).Name, Arguments, SelfExpr.ResType(), rettype, SelfExpr.FDocPos);
  end
  else
    Func := ctx.ResolveMethod(XTree_Identifier(Method).Name, Arguments, nil, rettype, Self.FDocPos);


  FuncType := Func.VarType;
  Result   := FuncType <> nil;
end;

function XTree_Invoke.ToString(offset:string=''): string;
var i: Int32;
begin

  if Method is XTree_Identifier then
  begin
    Result := Offset + System.Copy(Self.ClassName(), 7, Length(Self.ClassName())-6)+'('+XTree_Identifier(Method).Name+'(';
    for i:=0 to High(Args) do Result += Args[i].ToString() +', ';
    Result +='))';

  end else
  begin
    Result := Offset + System.Copy(Self.ClassName(), 7, Length(Self.ClassName())-6)+'(';
    for i:=0 to High(Args) do Result += Args[i].ToString() +', ';
    Result +=')';

  end;
end;

function XTree_Invoke.ResType(): XType;
var
  funcType: XType;
  func: TXprVar;
  ErrorArgs: string; ArgCount: Int32;
  MagicNode: XTree_Node;
begin

  if FResType = nil then
  begin
    if(Method is XTree_Identifier) and (Length(Args) = 1) and (ctx.GetType(XTree_Identifier(Method).Name) <> nil) then
    begin
      FResType := ctx.GetType(XTree_Identifier(Method).Name);
      Exit(FResType);
    end;

    if Self.ResolveMethod(Func, funcType) then
      FResType := XType_Method(funcType).ReturnType
    else
    begin
      // magic method
      if (Method is XTree_Identifier) and (SelfExpr = nil) and
         (MagicMethods.Get(XprCase(XTree_Identifier(Method).Name), MagicNode)) then
      begin
        XTree_Invoke(MagicNode).Args     := Self.Args;
        XTree_Invoke(MagicNode).Method   := Self.Method;
        XTree_Invoke(MagicNode).SelfExpr := Self.SelfExpr;
        XTree_Invoke(MagicNode).FContext := Self.FContext;
        XTree_Invoke(MagicNode).FDocPos  := Self.FDocPos;

        FResType := MagicNode.ResType();
        Exit(FResType);
      end;

      // Failed
      ErrorArgs := '';
      for ArgCount:=0 to High(Args) do
        ErrorArgs += Args[ArgCount].ResType().ToString + ',';

      if Method is XTree_Identifier then
        ctx.RaiseException('Cant resolve function: '+XTree_Identifier(Method).Name +' -> '+ErrorArgs, FDocPos)
      else
        ctx.RaiseException('Cant resolve function: ??', FDocPos);
    end;
  end;
  Result := inherited;
end;


function XTree_Invoke.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  Func: TXprVar;
  FuncType: XType_Method;
  SelfVar: TXprVar;

  procedure PushArgsToStack();
  var
    i, paramIndex, impliedArgs: Int32;
    initialArg, finalArg: TXprVar;
    expectedType: XType;
  begin
    // XXX: If nested then self arg is illegal!
    //if FuncType.IsNested then
    //  ctx.Emit(GetInstr(icPUSH_FP), FDocPos);

    SelfVar := NullVar;
    impliedArgs := 0;
    // Handle the implicit 'self' argument first.
    if SelfExpr <> nil then
    begin
      impliedArgs := 1;
      SelfVar := SelfExpr.CompileLValue(NullVar);
      if SelfVar = NullResVar then
        ctx.RaiseException('Self expression compiled to NullResVar', SelfExpr.FDocPos);

      if SelfVar.Reference then ctx.Emit(GetInstr(icPUSHREF, [SelfVar]), FDocPos)
      else                      ctx.Emit(GetInstr(icPUSH,    [SelfVar]), FDocPos);
    end;

    // Loop through the explicit arguments.
    for i := 0 to High(Args) do
    begin
      paramIndex := i + impliedArgs;
      if Args[i] = nil then
        ctx.RaiseExceptionFmt('Argument at index %d is nil', [i], FDocPos);

      initialArg := Args[i].Compile(NullVar, Flags);
      if initialArg = NullResVar then
        ctx.RaiseExceptionFmt('Argument at index %d compiled to NullResVar', [i], FDocPos);

      finalArg := initialArg;

      expectedType := FuncType.Params[paramIndex];
      if (FuncType.Passing[paramIndex] = pbCopy) then
      begin
        finalArg := ctx.EmitUpcastIfNeeded(initialArg, expectedType, True);
      end;

      if (finalArg.Reference) then
        ctx.Emit(GetInstr(icPUSHREF, [finalArg]), FDocPos)
      else
        ctx.Emit(GetInstr(icPUSH,    [finalArg]), FDocPos);
    end;
  end;

  function TryTypeCast(IntrinsicName: string): TXprVar;
  var vType: XType;
  begin
    Result := NullResVar;
    vType := ctx.GetType(IntrinsicName);
    if vType <> nil then
    begin
      if Length(Args) <> 1 then
        ctx.RaiseExceptionFmt('Typecast expects exactly one argument, but got %d.', [Length(Args)], FDocPos);

      with XTree_TypeCast.Create(vType, Self.Args[0], FContext, FDocPos) do
      try
        Result := Compile(Dest, Flags);
      finally
        Free;
      end;
      Exit; // We are done.
    end;
  end;

var
  totalSlots: UInt16;
  FreeingInstance: Boolean;
  MagicNode: XTree_Node;
  TmpRes: TXprVar;
  debug_name_id: TXprVar;
begin
  Result := NullResVar;
  Func   := NullResVar;
  FreeingInstance := False;
  SelfVar := NullVar;

  // --- Might be a type cast or a magic intrinsic
  // --- These must take priority
  if (Length(Args) = 1) and (SelfExpr = nil) and (Method is XTree_Identifier) then
  begin
    Result := TryTypeCast(XTree_Identifier(Method).Name);
    if Result <> NullResVar then Exit;
  end;

  debug_name_id := NullVar;
  // --- Regular functions
  if (Method is XTree_Identifier) then
  begin
    debug_name_id := ctx.RegConst(XTree_Identifier(Self.Method).Name);

    self.ResolveMethod(Func, XType(FuncType));

    // class destructor handling
    // handled here as just another call (which it usually is)
    FreeingInstance := (SelfExpr <> nil) and (SelfExpr.ResType() is XType_Class) and
                       IsDestructor(Method);

    if (Func = NullResVar) and FreeingInstance then
    begin
      SelfVar := SelfExpr.CompileLValue(NullVar);
      ctx.Emit(GetInstr(icRELEASE, [SelfVar.IfRefDeref(ctx)]), FDocPos);
      Exit;
    end;
  end else
  begin
    Func     := Method.Compile(NullVar, Flags);
    FuncType := XType_Method(func.VarType);
  end;

  // --- No regular function matched, check magic intrinsics
  if (Func = NullResVar) and (Method is XTree_Identifier) and (SelfExpr = nil) then
  begin
    if MagicMethods.Get(XprCase(XTree_Identifier(Method).Name), MagicNode) then
    begin
      XTree_Invoke(MagicNode).Args     := Self.Args;
      XTree_Invoke(MagicNode).Method   := Self.Method;
      XTree_Invoke(MagicNode).SelfExpr := Self.SelfExpr;
      XTree_Invoke(MagicNode).FContext := Self.FContext;
      XTree_Invoke(MagicNode).FDocPos  := Self.FDocPos;

      Result := XTree_Invoke(MagicNode).Compile(Dest, Flags);
      Exit;
    end;
  end;

  if Func = NullResVar then
    ctx.RaiseExceptionFMT('Function not matched `%s`', [XTree_Identifier(Method).name], FDocPos);
  if not(func.VarType is XType_Method) then
    ctx.RaiseException('Cannot invoke an identifier that is not a function', FDocPos);


  // res, stackptr, args
  if (FuncType.ReturnType <> nil) then
  begin
    Result := Dest;
    if Dest = NullResVar then
      Result := ctx.GetTempVar(FuncType.ReturnType);

    ctx.Emit(GetInstr(icFILL, [Result, Immediate(Result.VarType.Size()), Immediate(0)]), FDocPos);
    ctx.Emit(GetInstr(icPUSH, [Result]), FDocPos);
  end;

  PushArgsToStack();

  totalSlots := Length(Args);
  if (FuncType.ReturnType <> nil) then
    Inc(totalSlots);
  if SelfExpr <> nil then
    Inc(totalSlots);
  //if FuncType.IsNested then
  //  Inc(totalSlots);

  if FuncType.ClassMethod then
  begin
    ctx.Emit(GetInstr(icINVOKE_VIRTUAL, [Immediate(FuncType.GetVMTIndex()), Immediate(totalSlots), Immediate(Ord(FuncType.ReturnType <> nil))]), FDocPos);
  end else if Func.MemPos = mpHeap then
    ctx.Emit(GetInstr(icINVOKEX, [Func, Immediate(totalSlots), Immediate(Ord(FuncType.ReturnType <> nil))]), FDocPos)
  else
    ctx.Emit(GetInstr(icINVOKE, [Func, Immediate(Ord(Func.IsGlobal)), debug_name_id]), FDocPos);

  //--
  if FreeingInstance and (SelfVar <> NullVar) then
  begin
    ctx.Emit(GetInstr(icRELEASE, [SelfVar.IfRefDeref(ctx)]), FDocPos);
  end;
end;


function XTree_Invoke.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
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


function XTree_Invoke.CompileLValue(Dest: TXprVar): TXprVar;
var vType: XType;
begin
  if (Length(args) <> 1) then
    ctx.RaiseExceptionFmt('Typecast expects exactly one argument, but got %d.', [Length(Args)], FDocPos);

  if (not(args[0] is XTree_Identifier)) or (not(Method is XTree_Identifier)) then
    ctx.RaiseException('Functions can not be written to', FDocPos);

  vType := ctx.GetType(XTree_Identifier(Self.Method).Name);
  if vType <> nil then
  begin
    if Length(Args) <> 1 then
      ctx.RaiseExceptionFmt('Typecast expects exactly one argument, but got %d.', [Length(Args)], FDocPos);

    with XTree_TypeCast.Create(vType, Self.Args[0], FContext, FDocPos) do
    try
      Result := CompileLValue(Dest);
    finally
      Free;
    end;
  end else
    ctx.RaiseException(eUnexpected, FDocPos);
end;


//-------
constructor XTree_InheritedCall.Create(AArgs: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.Args := AArgs;
  Self.ResolvedParentMethod := NullVar; // This will still be useful for ResType
end;

function XTree_InheritedCall.ResType(): XType;
var
  CurrentMethod, ParentMethodDef: XType_Method;
  CurrentClass, ParentClass: XType_Class;
  ArgList: XTypeArray;
  i: Int32;
begin
  if FResType <> nil then
  begin
    Result := FResType;
    Exit;
  end;

  CurrentMethod := ctx.GetCurrentMethod() as XType_Method;
  if CurrentMethod = nil then
    ctx.RaiseException('Internal error [43534517]', FDocPos);

  // If the method hasn't been resolved yet, do it now.
  if (ResolvedParentMethod = NullVar) then
  begin
    if CurrentMethod.ClassMethod then
    begin
      CurrentClass := CurrentMethod.GetClass() as XType_Class;
      ParentClass := CurrentClass.Parent;
      if ParentClass = nil then
        ctx.RaiseException('`inherited` called in a base class that has no parent.', FDocPos);

      SetLength(ArgList, Length(Self.Args));
      for i:=0 to High(ArgList) do
        ArgList[i] := Self.Args[i].ResType();

      Self.ResolvedParentMethod := ctx.ResolveMethod(CurrentMethod.Name, ArgList, CurrentClass.Parent,nil,FDocPos);

      if Self.ResolvedParentMethod = NullVar then
        ctx.RaiseExceptionFmt('No parent method with a matching signature for `%s` was found.', [CurrentMethod.Name], FDocPos);
    end else
    begin
      ctx.RaiseException('`inherited` can only be used inside a class method.', FDocPos);
    end;
  end;

  ParentMethodDef := ResolvedParentMethod.VarType as XType_Method;
  FResType := ParentMethodDef.ReturnType;
  Result := FResType;
end;

function XTree_InheritedCall.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  InvokeNode: XTree_Invoke;
  MethodStub: XTree_VarStub;
  wasClassMethod: Boolean;
begin
  // Resolve the parent method to call. ResType does all the heavy lifting and caching.
  Self.ResType();

  MethodStub := XTree_VarStub.Create(Self.ResolvedParentMethod, ctx, FDocPos);
  InvokeNode := XTree_Invoke.Create(MethodStub, Self.Args, ctx, FDocPos);
  InvokeNode.SelfExpr := XTree_Identifier.Create('self', FContext, FDocPos);
  try
    wasClassMethod := XType_method(MethodStub.VarDecl.VarType).ClassMethod;
    XType_method(MethodStub.VarDecl.VarType).ClassMethod := False;
    Result := InvokeNode.Compile(Dest, Flags);
    XType_method(MethodStub.VarDecl.VarType).ClassMethod := wasClassMethod;
  finally
    InvokeNode.Free;
  end;
end;

function XTree_InheritedCall.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var i: Integer;
begin
  // An inherited call itself has no delayed logic, but its arguments might.
  for i := 0 to High(Args) do
    Args[i].DelayedCompile(Dest, Flags);
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
begin
  if (Self.FResType = nil) then
  begin
    exprType := Self.Expr.ResType();
    if (not (exprType is XType_Array)) and (not (exprType.BaseType = xtPointer)) then
      ctx.RaiseExceptionFmt('Cannot index into non-array type `%s`', [exprType.ToString], Self.Expr.FDocPos);

    case exprType.BaseType of
      xtArray, xtAnsiString, xtUnicodeString:
        FResType := XType_Array(exprType).ItemType;
      xtPointer:
      begin
        FResType := XType_Pointer(exprType).ItemType;
        if FResType = nil then FResType := ctx.GetType(xtUnknown);
      end;
    end;

    if FResType = nil then
      ctx.RaiseExceptionFmt('Item type is nil for indexable type `%s`', [exprType.ToString], Self.Expr.FDocPos);
  end;
  Result := inherited;
end;

function XTree_Index.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  ArrVar, IndexVar, AddressVar: TXprVar;
  ItemSize: Integer;
begin
  if (Self.ResType() = nil) then
    ctx.RaiseExceptionFmt('Index target must be indexable, got `%s`', [Self.Expr.ResType().ToString], FDocPos);

  ArrVar := Expr.Compile(NullResVar, Flags);
  if ArrVar = NullResVar then
    ctx.RaiseException('Left expression compiled to NullResVar', Expr.FDocPos);

  IndexVar := Index.Compile(NullResVar, Flags);
  if IndexVar = NullResVar then
    ctx.RaiseException('Index expression compiled to NullResVar', Index.FDocPos);

  // Ensure vars are on stack! We need a way to deal with this centrally
  ArrVar   := ArrVar.IfRefDeref(ctx);
  IndexVar := IndexVar.IfRefDeref(ctx);

  // Calculate address: arr + index * item_size
  if ForceTypeSize = 0 then
  begin
    case Expr.ResType().BaseType of
      xtArray, xtAnsiString, xtUnicodeString:
        ItemSize := XType_Array(Expr.ResType()).ItemType.Size();
      xtPointer:
        begin
          if XType_Pointer <> nil then
            ItemSize := XType_Pointer(Expr.ResType()).ItemType.Size()
          else
            ItemSize := 1;

        end;
    end;
  end else
    ItemSize := ForceTypeSize;

  AddressVar := ctx.GetTempVar(ctx.GetType(EExpressBaseType.xtPointer));

  ctx.Emit(GetInstr(icFMA, [IndexVar, Immediate(ItemSize), ArrVar, AddressVar]), FDocPos);

  AddressVar.Reference := False;
  AddressVar.VarType   := ResType();
  Result := AddressVar.Deref(ctx, Dest);  // Dereference for read
end;

function XTree_Index.CompileLValue(Dest: TXprVar): TXprVar;
var
  ArrVar, LocalVar, IndexVar, AddressVar: TXprVar;
  ItemSize: Integer;
begin
  // Compile array base and index
  ArrVar   := Expr.CompileLValue(NullResVar);
  IndexVar := Index.Compile(NullResVar, []);

  // Ensure vars are on stack! We need a way to deal with this centrally
  LocalVar := ArrVar.IfRefDeref(ctx);
  IndexVar := IndexVar.IfRefDeref(ctx);

  // Calculate address: arr + index * item_size
  if ForceTypeSize = 0 then
  begin
    case Expr.ResType().BaseType of
      xtArray, xtAnsiString, xtUnicodeString:
        ItemSize := XType_Array(Expr.ResType()).ItemType.Size();
      xtPointer:
        ItemSize := XType_Pointer(Expr.ResType()).ItemType.Size();
      else
        ItemSize := 1; // XXX cant happen
    end;
  end else
    ItemSize := ForceTypeSize;

  AddressVar := ctx.GetTempVar(ctx.GetType(EExpressBaseType.xtPointer));

  ctx.Emit(GetInstr(icFMA, [IndexVar, Immediate(ItemSize), LocalVar, AddressVar]), FDocPos);

  AddressVar.Reference := False;
  AddressVar.VarType   := ResType();

  Result := AddressVar;
  Result.Reference := True;
  Result.IsTemporary := ArrVar.IsTemporary;
end;

function XTree_Index.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName);{$ENDIF}

  Self.Expr.DelayedCompile(Dest, Flags);
  Self.Index.DelayedCompile(Dest, Flags);
  Result := NullResVar;
end;

// ============================================================================
// IF statement
//    if (condition) then <stmts> end
//    if (condition) then <stmts> [elif (condition) ..] else <stmts> end
//    if (condition) <stmt>
//    if (condition) <stmt> [elif (condition) ..] else <stmt>

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
function XTree_If.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  i: Int32;
  nextCondJumps: array of PtrInt;
  boolVar: TXprVar;
  skipRestJump: PtrInt;
begin
  if (Self.Conditions = nil) or (Length(Self.Conditions) = 0) then
    ctx.RaiseException(eSyntaxError, 'If statement must have at least one condition', FDocPos);
  if (Self.Bodys = nil) or (Length(Self.Bodys) = 0) then
    ctx.RaiseException(eSyntaxError, 'If statement must have at least one body', FDocPos);
  if Length(Self.Conditions) <> Length(Self.Bodys) then
    ctx.RaiseException('Mismatched number of conditions and bodies in If statement', FDocPos);

  SetLength(nextCondJumps, Length(Self.Conditions));

  // Process each condition in turn
  for i := 0 to High(Self.Conditions) do
  begin
    if Self.Conditions[i] = nil then
      ctx.RaiseExceptionFmt('Condition at index %d is nil in If statement', [i], FDocPos);

    // Compile the condition
    boolVar := Self.Conditions[i].Compile(NullResVar, Flags);
    if boolVar = NullResVar then
      ctx.RaiseExceptionFmt('Condition at index %d did not compile to a valid result', [i], Self.Conditions[i].FDocPos);
    if not (boolVar.VarType.BaseType = xtBoolean) then
      ctx.RaiseExceptionFmt('If condition must be a boolean, got `%s`', [boolVar.VarType.ToString], Self.Conditions[i].FDocPos);

    // Emit jump if false → skip to next condition check
    nextCondJumps[i] := ctx.Emit(
      GetInstr(icJZ, [boolVar, NullVar]),
      Self.Conditions[i].FDocPos
    );

    if Self.Bodys[i] = nil then
      ctx.RaiseExceptionFmt('Body at index %d is nil in If statement', [i], FDocPos);

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
function XTree_If.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
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
// case statement
//    switch case-condition of
//      case 1:
//        ..
//      case 2,3,4:
//        ..
//      else:
//        ..
//    end;

constructor XTree_Case.Create(AExpression: XTree_Node; ABranches: TCaseBranchArray; AElseBody: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.Expression := AExpression;
  Self.Branches := ABranches;
  Self.ElseBody := AElseBody;
end;

function XTree_Case.ToString(offset: string): string;
begin
  // A simple representation for debugging.
  Result := offset + _AQUA_ + 'Case' + _WHITE_ + ' of (' + Expression.ToString() + ')';
end;

function XTree_Case.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
const
  // Heuristics to decide when to use a jump table.
  JUMP_TABLE_DENSITY_THRESHOLD = 0.5; // Use table if over 50% dense.
  JUMP_TABLE_MIN_CASES = 4;           // Don't bother with a table for just a few cases.
  JUMP_TABLE_MAX_CASES = 10000;       // huge tables is stupid
var
  SwitchVar: TXprVar;
  IsOrdinal: Boolean;

  procedure CompileAsIf;
  var
    Conditions: XNodeArray;
    Bodys: XNodeArray;
    IfChainAST: XTree_If;
    i, j: Integer;
    Branch: TCaseBranch;
    Condition, SubCondition: XTree_Node;
  begin
    SetLength(Conditions, Length(Branches));
    SetLength(Bodys, Length(Branches));

    for i := 0 to High(Branches) do
    begin
      Branch := Branches[i];
      Condition := nil;

      // Build the condition
      for j := 0 to Branch.Labels.High do
      begin
        SubCondition := XTree_BinaryOp.Create(op_EQ,
          XTree_VarStub.Create(SwitchVar, ctx, FDocPos),
          Branch.Labels.Data[j],
          ctx, FDocPos
        );
        if Condition = nil then
          Condition := SubCondition
        else
          Condition := XTree_BinaryOp.Create(op_OR, Condition, SubCondition, ctx, FDocPos);
      end;

      Conditions[i] := Condition;
      Bodys[i] := Branch.Body;
    end;

    // simple with chained if..
    IfChainAST := XTree_If.Create(
      Conditions,
      Bodys,
      Self.ElseBody as XTree_ExprList,
      ctx, FDocPos
    );

    try
      IfChainAST.Compile(NullResVar, Flags);
    finally
      IfChainAST.Free;
    end;
  end;

  procedure CompileAsTable;
  begin

  end;
begin
  // Comple the expression only ONCE!
  SwitchVar := Expression.Compile(NullResVar, Flags);
  IsOrdinal := SwitchVar.VarType.BaseType in XprOrdinalTypes;

  CompileAsIf();

  (*
  // Decide which strategy to use.
  if not IsOrdinal or (Length(Branches) < JUMP_TABLE_MIN_CASES) then
  begin
    CompileAsIf;
  end
  else
  begin
    // Check density to see if a jump table is worthwhile.
    // This means we have to compute all cases and their range
    // we should also have a limit on it's span, say 10K for example.
    var MinV, MaxV: Int64;
    MinV := High(Int64); MaxV := Low(Int64);
    LabelCount := 0;
    for Branch in Branches do
    begin
      Inc(LabelCount, Branch.Labels.Size);
      for i:=0 to Branch.Labels.Data.High do
      begin
        MinV := Min(MinV, (Branch.Labels.Data[i] as XTree_Int).Value);
        MaxV := Max(MaxV, (Branch.Labels.Data[i] as XTree_Int).Value);
      end;
    end;

    Density := LabelCount / (MaxV - MinV + 1);
    if (Density >= JUMP_TABLE_DENSITY_THRESHOLD) and (MaxV-MinV <= JUMP_TABLE_MAX_CASES) then
      CompileAsTable
    else
      CompileAsIf;
  end;
  *)
  Result := NullResVar;
end;

function XTree_Case.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var i: Integer;
begin
  Expression.DelayedCompile(Dest, Flags);
  for i := 0 to High(Branches) do
    Branches[i].Body.DelayedCompile(Dest, Flags);
  if ElseBody <> nil then
    ElseBody.DelayedCompile(Dest, Flags);
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
function XTree_While.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  loopStart, loopEnd: PtrInt;
  boolVar: TXprVar;
begin
  if Self.Condition = nil then
    ctx.RaiseException(eSyntaxError, 'While loop condition cannot be empty', FDocPos);
  if Self.Body = nil then
    ctx.RaiseException(eSyntaxError, 'While loop body cannot be empty', FDocPos);

  // Mark the start of the patching scope for this loop.
  ctx.PreparePatch();

  // The 'continue' target is the beginning of the condition check.
  loopStart := ctx.CodeSize();
  boolVar := Condition.Compile(NullResVar, Flags);
  if boolVar = NullResVar then
    ctx.RaiseException('While loop condition failed to compile', Condition.FDocPos);
  if not (boolVar.VarType.BaseType = xtBoolean) then
    ctx.RaiseExceptionFmt('While loop condition must be a boolean, got `%s`', [boolVar.VarType.ToString], Condition.FDocPos);

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
  ctx.RunPatch(icJCONT, loopStart);
  ctx.RunPatch(icJBREAK, ctx.CodeSize());

  // Clean up the patching scope.
  ctx.PopPatch();

  Result := NullResVar;
end;

(*
  Performs delayed compilation for the condition and body of the WHILE loop.
*)
function XTree_While.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
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
//    raise Exception(args);
//
constructor XTree_Raise.Create(AExceptionObject: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.ExceptionObject := AExceptionObject;
end;

function XTree_Raise.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  ExceptionVar: TXprVar;
  BaseExceptionType: XType;
begin
  // 1. Compile the expression to get the exception object instance.
  ExceptionVar := ExceptionObject.Compile(NullResVar, Flags);

  // 2. Perform a type-safety check.
  (*
  BaseExceptionType := ctx.GetType('Exception', False); // False = don't raise error if not found
  if (BaseExceptionType = nil) then
     ctx.RaiseException('The base "Exception" class is not defined. Cannot raise exceptions.', FDocPos)
  else if not (ExceptionVar.VarType is XType_Class) or
          not BaseExceptionType.CanAssign(ExceptionVar.VarType) then
     ctx.RaiseException('Can only raise objects that inherit from the base Exception class.', FDocPos);
  *)

  // 3. Emit the RAISE instruction. This will put the object in CurrentException
  //    and trigger the native raise to unwind to RunSafe.
  ctx.Emit(GetInstr(icRAISE, [ExceptionVar.IfRefDeref(ctx)]), FDocPos);

  Result := NullResVar;
end;

function XTree_Raise.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  ExceptionObject.DelayedCompile(Dest, Flags);
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
constructor XTree_Try.Create(ATryBody: XTree_ExprList; AHandlers: TExceptionHandlerArray; AElseBody: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.TryBody   := ATryBody;
  Self.Handlers  := AHandlers;
  Self.ElseBody  := AElseBody;
end;

function XTree_Try.ToString(offset: string): string;
begin
  // You can expand this later to show all the handlers for better debugging.
  Result := offset + _AQUA_ + 'Try' + _WHITE_ + '(...)';
end;

function XTree_Try.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  HandlerLogicStart, EndOfTryBlockJump: PtrInt;
  ExceptionTempVar: TXprVar;

  // These will hold the lists for the XTree_If constructor
  Conditions: XNodeArray;
  Bodys: XNodeArray;

  i,j: Int32;
  ConditionNode: XTree_TypeIs;
  ThenNode: XTree_ExprList;
  AssignNode: XTree_Assign;
  HandlerVar: TXprVar;
  HandlerStub: XTree_VarStub;
  HandlerChainAST: XTree_If;
  ErrorIdent: XTree_Identifier;
  ExceptionBase: XType;
  Ident: XIdentNodeList;
begin
  //try -->
  HandlerLogicStart := ctx.Emit(GetInstr(icIncTry, [NullVar]), TryBody.FDocPos);
  TryBody.Compile(NullVar, Flags);
  ctx.Emit(GetInstr(icDecTry), TryBody.FDocPos);
  EndOfTryBlockJump := ctx.Emit(GetInstr(icRELJMP, [NullVar]), FDocPos);
  ctx.PatchArg(HandlerLogicStart, ia1, ctx.CodeSize());

  //except -->
  // Get the current exception object into a temporary variable.
  ExceptionBase := ctx.GetType('Exception');
  if ExceptionBase = nil then
    ctx.RaiseException('No exception handlers are implemented, declare `type Exception = class`');

  ErrorIdent := XTree_Identifier.Create('!E', FContext, FDocPos);
  ctx.RegVar('!E', ExceptionBase, FDocPos);

  ExceptionTempVar := ErrorIdent.Compile(NullResVar, Flags);
  ctx.Emit(GetInstr(icGET_EXCEPTION, [ExceptionTempVar]), FDocPos);

  Conditions := [];
  Bodys := [];

  // 6. Build the flat lists of conditions and bodies.
  SetLength(Conditions, Max(1, Length(Self.Handlers)));
  SetLength(Bodys, Max(1, Length(Self.Handlers)));

  for i := 0 to High(Handlers) do
  begin
    // --- Late resolve ---
    if Handlers[i].ExceptionType.BaseType = xtUnknown then
       Handlers[i].ExceptionType := ctx.GetType(Handlers[i].ExceptionType.Name);

    // if "E is THandlerType"
    Conditions[i] := XTree_TypeIs.Create(
      ErrorIdent,
      XTree_Identifier.Create(Handlers[i].ExceptionType.Name, ctx, FDocPos),
      ctx, FDocPos
    );

    // --- Create the Body for this condition ---
    ThenNode := XTree_ExprList.Create(ctx, FDocPos);

    //"var E := `!E as THandlerType`"
    ThenNode.List += XTree_VarDecl.Create(
      Handlers[i].VarName,
      XTree_DynCast.Create(
        ErrorIdent,
        XTree_Identifier.Create(Handlers[i].ExceptionType.Name, ctx, FDocPos),
        ctx,
        FDocPos
      ),
      Handlers[i].ExceptionType,
      False,
      ctx, fdocpos
    );

    // Extend with handler body.
    for j:=0 to High(XTree_ExprList(Handlers[i].Body).List) do
      ThenNode.List += XTree_ExprList(Handlers[i].Body).List[j];

    Bodys[i] := ThenNode;
  end;

  if Length(Handlers) = 0 then
  begin
    Conditions[0] := XTree_Bool.Create('false', ctx, fdocpos); //if false
    Bodys[0]      := nil;
  end;

  if Self.ElseBody = nil then
  begin
    Self.ElseBody := XTree_ExprList.Create([
      XTree_Print.create([XTree_Int.Create('12345', FContext, FDocPos)], FContext, FDocPos),
      XTree_Raise.Create(XTree_VarStub.Create(ExceptionTempVar,ctx, FDocPos), ctx, Fdocpos)
    ], ctx, Fdocpos);
  end;

  // 7. Assemble the entire handler logic into a single XTree_If node.
  HandlerChainAST := XTree_If.Create(
    Conditions,
    Bodys,
    Self.ElseBody as XTree_ExprList, // The final 'except' becomes the 'else'
    ctx,
    FDocPos
  );

  // 8. Compile the generated AST.
  if HandlerChainAST <> nil then
  begin
    try
      // This single call now generates the entire, efficient if-elif-else chain.
      HandlerChainAST.Compile(NullResVar, Flags);
    finally
      HandlerChainAST.Free;
    end;
  end;

  // 9. We are now at the end. Patch the initial jump to get here.
  ctx.PatchJump(EndOfTryBlockJump);

  Result := NullResVar;
end;

function XTree_Try.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var i: Integer;
begin
  TryBody.DelayedCompile(Dest, Flags);
  for i := 0 to High(Handlers) do
    Handlers[i].Body.DelayedCompile(Dest, Flags);
  if ElseBody <> nil then
    ElseBody.DelayedCompile(Dest, Flags);
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
function XTree_For.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  loopStart, loopEnd, continueTarget: PtrInt;
  boolVar: TXprVar;
begin
  if Self.Body = nil then
    ctx.RaiseException(eSyntaxError, 'For loop body cannot be empty', FDocPos);

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
      ctx.RaiseException('For loop condition failed to compile', Condition.FDocPos);
    if not (boolVar.VarType.BaseType = xtBoolean) then
      ctx.RaiseExceptionFmt('For loop condition must be a boolean, got `%s`', [boolVar.VarType.ToString], Condition.FDocPos);

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
function XTree_For.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
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

function XTree_Repeat.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  loopStart, continueTarget: PtrInt;
  boolVar: TXprVar;
begin
  if Self.Condition = nil then
    ctx.RaiseException(eSyntaxError, 'Repeat..Until loop condition cannot be empty', FDocPos);
  if Self.Body = nil then
    ctx.RaiseException(eSyntaxError, 'Repeat..Until loop body cannot be empty', FDocPos);

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
    ctx.RaiseException('Repeat..Until condition failed to compile', Condition.FDocPos);
  if not (boolVar.VarType.BaseType = xtBoolean) then
    ctx.RaiseExceptionFmt('Repeat..Until condition must be a boolean, got `%s`', [boolVar.VarType.ToString], Condition.FDocPos);

  // Emit the conditional jump. The loop continues if the condition is FALSE (zero).
  ctx.Emit(GetInstr(icJZ, [boolVar, ctx.RelAddr(loopStart)]), Condition.FDocPos);

  // Now that the entire loop is emitted, run the patcher.
  ctx.RunPatch(icJCONT, continueTarget);
  ctx.RunPatch(icJBREAK, ctx.CodeSize());

  // Clean up the patching scope.
  ctx.PopPatch();

  Result := NullResVar;
end;

function XTree_Repeat.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
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
      ctx.RaiseException('Left operand of unary operator cannot be nil', FDocPos);

    leftType := Self.Left.ResType();
    if leftType = nil then
      ctx.RaiseExceptionFmt('Left operand of unary operator has no resolved type', [OperatorToStr(OP)], Self.Left.FDocPos);

    case op of
      op_Addr:
        begin
          FResType := XType_Pointer.Create(leftType);
        end;

      op_DEREF:
        begin
          if not (leftType is XType_Pointer) then
            ctx.RaiseExceptionFmt('Cannot dereference non-pointer type `%s`', [leftType.ToString], Self.Left.FDocPos);

          FResType := (leftType as XType_Pointer).ItemType;

          if FResType = nil then
            ctx.RaiseExceptionFmt('Cannot dereference an untyped pointer. Use a cast first.', [leftType.ToString], Self.Left.FDocPos);
        end;

      op_Add, op_Sub: // Unary plus/minus
        begin
          if not ((leftType is XType_Ordinal) or (leftType is XType_Float)) then
            ctx.RaiseExceptionFmt('Unary plus/minus only applicable to numeric types, got `%s`', [leftType.ToString], Self.Left.FDocPos);
          FResType := leftType;
        end;
      op_INCREF, op_DECREF:
          FResType := Left.ResType();
      else
        ctx.RaiseExceptionFmt('Unary operator `%s` not supported for type `%s`', [OperatorToStr(OP), leftType.ToString], FDocPos);
    end;
  end;
  Result := inherited; // This should be FResType instead of inherited
end;

(*
  Compiles the unary operation, generating intermediate code.
  Handles address-of, dereferencing, and converts unary minus into a binary subtraction.
*)
function XTree_UnaryOp.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  LeftVar: TXprVar;
  NewLeft: XTree_Node; // NewRight is already 'Left'
  leftType: XType;
begin
  if Self.Left = nil then
    ctx.RaiseException('Left operand of unary operator cannot be nil during compilation', FDocPos);

  Result := Dest;
  if Result = NullResVar then Result := ctx.GetTempVar(ResType()); // ResType() will perform type checking

  leftType := Self.Left.ResType(); // This should already be resolved and checked by ResType()

  case op of
    op_Add:
      Exit(Left.Compile(Dest, Flags)); // Unary plus is a no-op at bytecode level

    op_Addr:
      begin
        LeftVar := Left.CompileLValue(NullResVar);

        if LeftVar = NullResVar then
          ctx.RaiseException('Left operand for address-of operator compiled to NullResVar', Left.FDocPos);

        if not LeftVar.Reference then
          ctx.Emit(GetInstr(OP2IC(OP), [Result, LeftVar]), FDocPos)
        else
          Result := LeftVar;

        Result.Reference := False;
      end;

    op_DEREF:
      begin
        LeftVar := Left.Compile(NullResVar, Flags).IfRefDeref(ctx);

        if LeftVar = NullResVar then
          ctx.RaiseException('Left operand for dereference operator compiled to NullResVar', Left.FDocPos);

        if not (LeftVar.VarType is XType_Pointer) then
          ctx.RaiseExceptionFmt('Cannot dereference non-pointer variable `%s`', [LeftVar.VarType.ToString], Left.FDocPos);

        ctx.Emit(GetInstr(icDREF, [Result, LeftVar, Immediate(Result.VarType.Size)]), FDocPos);
        Result.Reference := False;
      end;

    op_Sub: // Unary minus: represented as 0 - operand
      begin
        if (leftType is XType_Ordinal) then
          NewLeft := XTree_Int.Create('0', ctx, FDocPos)
        else if (leftType is XType_Float) then
          NewLeft := XTree_Float.Create('0', ctx, FDocPos)
        else
          ctx.RaiseExceptionFmt('Unary minus not supported for type `%s`', [leftType.ToString], Left.FDocPos);

        // Create a temporary BinaryOp to compile the subtraction
        with XTree_BinaryOp.Create(op_SUB, NewLeft, Left, ctx, FDocPos) do
        begin
          Result := Compile(Dest, Flags);
          if Result = NullResVar then
            ctx.RaiseException('Unary minus operation failed to compile', FDocPos);
        end;
      end;

      op_INCREF:
        begin
          Result := Left.Compile(NullResVar, Flags);
          ctx.Emit(GetInstr(icINCLOCK, [Result]), FDocPos);
        end;

      op_DECREF:
        ctx.Emit(GetInstr(icDECLOCK, [Left.Compile(NullResVar, Flags)]), FDocPos);
    else
      ctx.RaiseExceptionFmt('Compilation for unary operator `%s` not implemented', [OperatorToStr(OP)], FDocPos);
  end;
end;

(*
  Performs delayed compilation for the left operand of the unary operation.
*)
function XTree_UnaryOp.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName);{$ENDIF}

  if Self.Left <> nil then
    Self.Left.DelayedCompile(Dest, Flags);

  Result := NullResVar;
end;

function XTree_UnaryOp.CompileLValue(Dest: TXprVar): TXprVar;
var
  LeftVar: TXprVar;
begin
  if Self.Left = nil then
    ctx.RaiseException('Left operand of unary operator cannot be nil during compilation', FDocPos);

  Result := Dest;
  if Result = NullResVar then Result := ctx.GetTempVar(ResType());

  if op = op_DEREF then
  begin
    LeftVar := Left.CompileLValue(NullResVar).IfRefDeref(ctx);

    if not (LeftVar.VarType is XType_Pointer) then
      ctx.RaiseExceptionFmt('Cannot dereference non-pointer variable `%s`', [LeftVar.VarType.ToString], Left.FDocPos);

    Result := LeftVar;
    Result.Reference := True;
    Result.VarType := ResType();
  end else
    Result := inherited;
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

procedure XTree_BinaryOp.NodeTypeHint(A,B: XTree_Node);
begin
  if (A.ResType() <> nil) then
    B.SetResTypeHint(A.ResType());

  if (B.ResType() <> nil) then
    A.SetResTypeHint(B.ResType());
end;

(*
  Determines the resulting type of the binary operation based on the operands' types and the operator.
*)
function XTree_BinaryOp.ResType(): XType;
var
  leftType, rightType: XType;
begin
  if Self.Left = nil then
    ctx.RaiseException('Left operand of binary operator cannot be nil', FDocPos);
  if Self.Right = nil then
    ctx.RaiseException('Right operand of binary operator cannot be nil', FDocPos);

  if (FResType = nil) then
  begin
    NodeTypeHint(Left, Right);
    RedefineConstant(Left, Right);
    RedefineConstant(Right, Left);

    leftType  := Left.ResType();
    rightType := Right.ResType();

    if leftType = nil then
      ctx.RaiseExceptionFmt('Left operand type could not be resolved for operator `%s`', [OperatorToStr(OP)], FDocPos);

    if rightType = nil then
      ctx.RaiseExceptionFmt('Right operand type could not be resolved for operator `%s`', [OperatorToStr(OP)], FDocPos);

    FResType := leftType.ResType(OP, rightType, FContext);
    if FResType = nil then
      ctx.RaiseExceptionFmt(eNotCompatible3, [OperatorToStr(OP), BT2S(leftType.BaseType), BT2S(rightType.BaseType)], FDocPos);
  end;
  Result := FResType; // Should return FResType not inherited
end;


(*
  Compiles the binary operation, generating intermediate code.
  Handles type promotion for arithmetic operations and short-circuiting for logical AND/OR.
*)
function XTree_BinaryOp.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  LeftVar, RightVar, TempVar, TmpBool: TXprVar;
  Instr: EIntermediate;
  CommonTypeVar: XType;

  function DoShortCircuitOp(): TXprVar;
  var
    Instr: EIntermediate;
    PatchPos: PtrInt;
  begin
    TmpBool := Left.Compile(NullResVar, Flags);
    if TmpBool = NullResVar then
      ctx.RaiseException('Left operand of short-circuit operation compiled to NullResVar', Left.FDocPos);
    if not (TmpBool.VarType.BaseType = xtBoolean) then
      ctx.RaiseExceptionFmt('Short-circuit operator requires boolean operand, got `%s`', [TmpBool.VarType.ToString], Left.FDocPos);

    Instr := TmpBool.VarType.EvalCode(OP, TmpBool.VarType);
    PatchPos := ctx.Emit(GetInstr(Instr, [TmpBool, NullVar]), FDocPos);

    RightVar := Right.Compile(TmpBool, Flags); // Right compiles to TmpBool if possible
    if RightVar = NullResVar then
      ctx.RaiseException('Right operand of short-circuit operation compiled to NullResVar', Right.FDocPos);
    if not (RightVar.VarType.BaseType = xtBoolean) then
      ctx.RaiseExceptionFmt('Short-circuit operator requires boolean operand, got `%s`', [RightVar.VarType.ToString], Right.FDocPos);

    ctx.PatchJump(PatchPos);
    Result := TmpBool;
  end;

begin
  Assert(not(OP in AssignOps), 'Assignment does not belong here, dont come again!');

  if Self.Left = nil then
    ctx.RaiseException('Left operand of binary operator cannot be nil during compilation', FDocPos);
  if Self.Right = nil then
    ctx.RaiseException('Right operand of binary operator cannot be nil during compilation', FDocPos);

  RedefineConstant(Left, Right);
  RedefineConstant(Right, Left);

  NodeTypeHint(Left, Right);

  if OP in [op_AND, op_OR] then
    Exit(DoShortCircuitOp());

  // Determine the result variable. This logic remains the same.
  Result := Dest;
  if Dest = NullResVar then
  begin
    Result := ctx.GetTempVar(Self.ResType()); // Self.ResType() will handle type errors

    // dont really like this, but it solves some unexpected problems
    // we check on vartype as we always want to zero fill anything complex
    if Result.VarType.IsManagedType(ctx) then
      ctx.Emit(GetInstr(icFILL, [Result, Immediate(Result.VarType.Size), Immediate(0)]), FDocPos);
  end;

  if Left.ResType() = nil then
    ctx.RaiseException('Cannot infer type from Left operand', FDocPos);
  if Right.ResType() = nil then
    ctx.RaiseException('Cannot infer type from Right operand', FDocPos);

  // Handle arithmetic operations with type promotion
  if OP in ArithOps+LogicalOps then
  begin
     // Determine common arithmetic type
    CommonTypeVar := ctx.GetType(CommonArithmeticCast(Left.ResType().BaseType, Right.ResType().BaseType));

    // Compile left operand and cast if needed
    LeftVar := Left.Compile(NullResVar, Flags);
    if LeftVar = NullResVar then
      ctx.RaiseException('Left operand failed to compile for arithmetic operation', Left.FDocPos);

    // Compile right operand and cast if needed
    RightVar := Right.Compile(NullResVar, Flags);
    if RightVar = NullResVar then
      ctx.RaiseException('Right operand failed to compile for arithmetic operation', Right.FDocPos);

    // do after compile both sides
    LeftVar  := ctx.EmitUpcastIfNeeded(LeftVar.IfRefDeref(ctx), CommonTypeVar, False);
    RightVar := ctx.EmitUpcastIfNeeded(RightVar.IfRefDeref(ctx), CommonTypeVar, False);
  end
  else
  begin
    // Non-arithmetic operations.
    LeftVar := Left.Compile(NullResVar, Flags);
    if LeftVar = NullResVar then
      ctx.RaiseException('Left operand failed to compile for binary operation', Left.FDocPos);

    RightVar := Right.Compile(NullResVar, Flags);
    if RightVar = NullResVar then
      ctx.RaiseException('Right operand failed to compile for binary operation', Right.FDocPos);

    // Ensure operands are values on the stack, not references.
    LeftVar  := LeftVar.IfRefDeref(ctx);
    RightVar := RightVar.IfRefDeref(ctx);
  end;

  Assert(LeftVar.VarType  <> nil);
  Assert(RightVar.VarType <> nil);

  // Emit the binary operation. This logic remains the same.
  Instr := LeftVar.VarType.EvalCode(OP, RightVar.VarType);
  if Instr <> icNOOP then
  begin
    ctx.Emit(GetInstr(Instr, [LeftVar, RightVar, Result]), FDocPos);
  end
  else
    ctx.RaiseExceptionFmt(eNotCompatible3, [OperatorToStr(OP), BT2S(Left.ResType.BaseType), BT2S(Right.ResType.BaseType)], Left.FDocPos);
end;


function XTree_BinaryOp.CompileLValue(Dest: TXprVar): TXprVar;
begin
  // not sure if this makes sense.. Maybe make Dest NullResVar and we are good
  // XXX: Leaving for now
  Result := Self.Compile(Dest, []);
end;


(*
  Performs delayed compilation for the left and right operands of the binary operation.
*)
function XTree_BinaryOp.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
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
function XTree_Assign.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  LeftVar, RightVar: TXprVar;
  OldLeftValueVar: TXprVar;
  Instr: EIntermediate;

  function TryNoMOV(): Boolean;
  var
    last_instr_ptr: ^TInstruction;
  begin
    Result := False;
    if (not LeftVar.Reference) and (RightVar.IsTemporary) and (RightVar.VarType.Equals(LeftVar.VarType)) then
    begin
      last_instr_ptr := @ctx.Intermediate.Code.Data[ctx.Intermediate.Code.High];

      case last_instr_ptr^.Code of
        icADD..icSAR:
        begin
          if SameData(last_instr_ptr^.Args[2], RightVar) then
          begin
            last_instr_ptr^.Args[2].Addr := LeftVar.Addr;
            Exit(True);
          end;
        end;

        icDREF:
        begin
          if SameData(last_instr_ptr^.Args[0], RightVar) then
          begin
            last_instr_ptr^.Args[0].Addr := LeftVar.Addr;
            Exit(True);
          end;
        end;
      end;
    end;
  end;

  procedure AssignToRecord();
  var
    i: Int32;
    RecType: XType_Record;
    IdentNode: XTree_Identifier;
    FieldDest, FieldSource: XTree_Field;
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

  procedure AssignByDestructuring(PatternNode: XTree_Destructure; RHS_Node: XTree_Node);
  var
    SourceVar: TXprVar;
    SourceType: XType;
    RecType: XType_Record;
    i: Int32;
    TargetNode: XTree_Node;
    SourceFieldNode: XTree_Field;
    AssignNode: XTree_Assign;
    TempFieldNames: XStringList;
    TempTypeList: XTypeList;
  begin
    // Compile RHS expression once to a temp!

    // special case of (a,b) := [b,a]
    if RHS_Node is XTree_InitializerList then
    begin
      TempFieldNames.Init([]);
      TempTypeList.Init([]);
      for i:=0 to High(PatternNode.Targets) do
      begin
        TempFieldNames.Add('!'+i.ToString());
        TempTypeList.Add(PatternNode.Targets[i].ResType());
      end;

      RecType := XType_Record.Create(TempFieldNames, TempTypeList);
      ctx.AddManagedType(RecType);
      RHS_Node.FResType := RecType;
    end;


    SourceVar := RHS_Node.Compile(NullResVar, Flags);
    SourceType := SourceVar.VarType;

    // validate that the source is a type we can destructure (currently only records).
    // future maybe tuple..
    if not (SourceType is XType_Record) then
    begin
      ctx.RaiseException('The right-hand side of a destructuring assignment must be a record.', RHS_Node.FDocPos);
      Exit;
    end;

    RecType := SourceType as XType_Record;

    // match count of LHS and RHS
    if RecType.FieldNames.Size <> Length(PatternNode.Targets) then
    begin
      ctx.RaiseExceptionFmt('The number of variables in the pattern (%d) does not match the number of fields in the record type `%s` (%d).',
        [Length(PatternNode.Targets), RecType.Name, RecType.FieldNames.Size], PatternNode.FDocPos);
      Exit;
    end;

    // sequentually assign itemwise
    for i := 0 to High(PatternNode.Targets) do
    begin
      TargetNode := PatternNode.Targets[i];

      SourceFieldNode := XTree_Field.Create(
        XTree_VarStub.Create(SourceVar, ctx, RHS_Node.FDocPos),
        XTree_Identifier.Create(RecType.FieldNames.Data[i], ctx, RHS_Node.FDocPos),
        ctx, RHS_Node.FDocPos
      );

      with XTree_Assign.Create(op_Asgn, TargetNode, SourceFieldNode, ctx, FDocPos) do
      try
        Compile(NullResVar, Flags);
      finally
        Free;
      end;
    end;
  end;

  procedure ManageMemory(LeftVar, RightVar: TXprVar; IsEqual: Boolean);
  begin
    // records with managed types should be elementwise assigned to active refcount
    if (LeftVar.VarType.BaseType in XprRefcountedTypes) and (not IsEqual) and
       (RightVar.VarType.BaseType in XprRefcountedTypes) then
    begin
      ctx.Emit(GetInstr(icINCLOCK, [RightVar]), FDocPos);
      // any decrement will be handled by collect
    end;

    if (LeftVar.VarType.BaseType in XprRefcountedTypes) and (not(cfNoCollect in Flags)) then
      ctx.EmitCollect(LeftVar);
  end;

  procedure ManagedAssign();
  begin
    // Get the OLD value from the LHS before we overwrite it.
    if LeftVar.Reference then
    begin
      // If LHS is a pointer (e.g., a[j].str), DEREF it to get the old string pointer.
      OldLeftValueVar := ctx.GetTempVar(LeftVar.VarType);
      OldLeftValueVar.IsTemporary := True; // this menas no collect, hmm.
      ctx.Emit(GetInstr(icDREF, [OldLeftValueVar, LeftVar, Immediate(LeftVar.VarType.Size)]), FDocPos);
    end
    else
    begin
      // If LHS is a simple local variable, its current value is the old value.
      // We don't need to copy it, we will just use LeftVar itself before the MOV.
      OldLeftValueVar := LeftVar;
    end;

    // IncLock right, incase left := right, where left already = right
    // Retain Right! We own this!
    if RightVar.VarType.IsManagedType(ctx) and (not (cfNoRefcount in Flags)) then
      ctx.Emit(GetInstr(icINCLOCK, [RightVar]), FDocPos);

    // Release the OLD value of Left
    if (not (cfNoCollect in Flags)) then
      ctx.EmitCollect(OldLeftValueVar);

    // Perform the actual assignment (pointer copy)
    if LeftVar.Reference then
      ctx.Emit(STORE_FAST(LeftVar, RightVar, True), FDocPos)
    else
      ctx.Emit(GetInstr(icMOV, [LeftVar, RightVar]), FDocPos);
  end;

begin
  Result := NullResVar;

  if Left = nil then
    ctx.RaiseException(eSyntaxError, 'Left hand side of assignment cannot be nil', FDocPos);
  if Right = nil then
    ctx.RaiseException(eSyntaxError, 'Right hand side of assignment cannot be nil', FDocPos);
  if Left is XTree_Const then // just fuck off
    ctx.RaiseException(eSyntaxError, eExpectedVar, Left.FDocPos);


  // hint at whatever to let it know we have a type for resolution
  Right.SetResTypeHint(Left.ResType());

  // try to make the constant the same type as the value we are assigning to.
  if (Right is XTree_Const) and (Left.ResType() <> nil) and (Right.ResType() <> nil) and (Left.ResType() <> Right.ResType()) then
    XTree_Const(Right).SetExpectedType(Left.ResType.BaseType);

  // --- Solve cases that can exit early ---
  // ---------------------------------------


  // 0) Destructure List Assignment
  if Left is XTree_Destructure then
  begin
    AssignByDestructuring(Left as XTree_Destructure, Right);
    Exit(NullResVar);
  end;

  // 1) Initializer List Assignment
  if Right is XTree_InitializerList then
  begin
    LeftVar := Left.CompileLValue(NullResVar);
    if LeftVar = NullResVar then
      ctx.RaiseException('Left hand side of assignment did not compile to a valid LValue', Left.FDocPos);

    Right.Compile(LeftVar, Flags);
    Exit(NullResVar);
  end;

  // 3) record := record, element wise assign
  if (Left.ResType() is XType_Record) then
  begin
    AssignToRecord();
    Exit(NullResVar);
  end;


  // --- Paths that generalize -------------------------------------------------
  // ---------------------------------------------------------------------------
  RightVar := Right.Compile(NullResVar, Flags).IfRefDeref(ctx);
  if RightVar = NullResVar then
    ctx.RaiseException('Right-hand side of assignment failed to compile.', Right.FDocPos);

  LeftVar := Left.CompileLValue(NullResVar);
  if LeftVar = NullResVar then
    ctx.RaiseException('Left-hand side of assignment is not a valid destination.', Left.FDocPos);

  // Check if we need to perform reference counting
  if LeftVar.IsManaged(ctx) then
  begin
    ManagedAssign();
    Exit(NullResVar);
  end;

  // optimize by rewriting the bytecode a tad
  if TryNoMOV() then
    Exit;

  // --- Must be simple assign path --------------------------------------------
  // ---------------------------------------------------------------------------
  if LeftVar.Reference then
    ctx.Emit(STORE_FAST(LeftVar, RightVar, True), FDocPos)
  else
    ctx.Emit(GetInstr(icMOV, [LeftVar, RightVar]), FDocPos);
end;

(*
  Performs delayed compilation for the left and right operands of the assignment.
*)
function XTree_Assign.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
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
function XTree_Print.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var managedVar,arg: TXprVar;
begin
  if (Self.Args = nil) or (Length(Self.Args) = 0) then
    ctx.RaiseException(eSyntaxError, 'Print statement requires at least one argument', FDocPos);
  if Self.Args[0] = nil then
    ctx.RaiseException('First argument of print statement is nil', FDocPos);

  arg := Self.Args[0].Compile(NullResVar, Flags);

  // We have to managed this var, this increases refcount
  // and ensures it follows normal rules, cost of magic expression I suppose
  managedVar := ctx.RegVar(arg.VarType.Hash()+'['+ctx.Variables.Size.ToString()+']', arg.VarType, FDocPos);
  with XTree_Assign.Create(op_Asgn, nil, nil, FContext, FDocPos) do
  try
    Left  := XTree_VarStub.Create(managedVar, ctx, fdocpos);
    Right := XTree_VarStub.Create(arg, ctx, fdocpos);
    Compile(NullResVar, Flags);
  finally
    Free();
  end;

  if managedVar = NullResVar then
    ctx.RaiseException('Argument for print statement failed to compile', Self.Args[0].FDocPos);

  if managedVar.Reference then managedVar := managedVar.DerefToTemp(ctx);

  if managedVar.VarType = nil then
    ctx.RaiseException('Argument for print statement has no resolved type', Self.Args[0].FDocPos);


  ctx.Emit(GetInstr(icPRINT, [managedVar, Immediate(managedVar.VarType.Size)]), FDocPos);

  Result := NullVar;
end;

(*
  Performs delayed compilation for all arguments of the print statement.
*)
function XTree_Print.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
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


// -----------------------------------------------------------------------------
// --- Copy methods ------------------------------------------------------------

{ XTree_ExprList }
function XTree_ExprList.Copy(): XTree_Node;
begin
  Result := XTree_ExprList.Create(CopyNodeArray(Self.List), FContext, FDocPos);
  (Result as XTree_ExprList).DelayedList := CopyNodeArray(Self.DelayedList);
end;

{ XTree_VarStub }
function XTree_VarStub.Copy(): XTree_Node;
begin
  Result := XTree_VarStub.Create(Self.VarDecl, FContext, FDocPos);
end;

{ XTree_Const }
function XTree_Const.Copy(): XTree_Node;
begin
  // This base method should not be called directly.
  // Descendants will implement the specific Create call.
  Result := inherited Copy();
end;

{ XTree_Bool }
function XTree_Bool.Copy(): XTree_Node;
begin
  Result := XTree_Bool.Create(Self.StrValue, FContext, FDocPos);
end;

{ XTree_Pointer }
function XTree_Pointer.Copy(): XTree_Node;
begin
  Result := XTree_Pointer.Create(Self.StrValue, FContext, FDocPos);
end;

{ XTree_Char }
function XTree_Char.Copy(): XTree_Node;
begin
  Result := XTree_Char.Create(Self.StrValue, FContext, FDocPos);
end;

{ XTree_Int }
function XTree_Int.Copy(): XTree_Node;
begin
  Result := XTree_Int.Create(Self.StrValue, FContext, FDocPos);
end;

{ XTree_Float }
function XTree_Float.Copy(): XTree_Node;
begin
  Result := XTree_Float.Create(Self.StrValue, FContext, FDocPos);
end;

{ XTree_String }
function XTree_String.Copy(): XTree_Node;
begin
  Result := XTree_String.Create(Self.StrValue, FContext, FDocPos);
end;

{ XTree_ImportUnit }
function XTree_ImportUnit.Copy(): XTree_Node;
begin
  Result := XTree_ImportUnit.Create(Self.UnitPath, Self.UnitAlias, FContext, FDocPos);
end;

{ XTree_Identifier }
function XTree_Identifier.Copy(): XTree_Node;
begin
  Result := XTree_Identifier.Create(Self.Name, FContext, FDocPos);
end;

{ XTree_Destructure }
function XTree_Destructure.Copy(): XTree_Node;
begin
  Result := XTree_Destructure.Create(CopyNodeArray(Self.Targets), FContext, FDocPos);
end;

{ XTree_NonLocalDecl }
function XTree_NonLocalDecl.Copy(): XTree_Node;
begin
  Result := XTree_NonLocalDecl.Create(CopyIdentNodeList(Self.Variables), FContext, FDocPos);
end;

{ XTree_VarDecl }
function XTree_VarDecl.Copy(): XTree_Node;
var
  NewExpr, ComputeType: XTree_Node;
begin
  if Self.Expr <> nil then NewExpr := Self.Expr.Copy else NewExpr := nil;
  Result := XTree_VarDecl.Create(CopyIdentNodeList(Self.Variables), NewExpr, Self.VarType, Self.IsConst, FContext, FDocPos);
  (Result as XTree_VarDecl).IsTypeOf := Self.IsTypeOf;

  if (Self.VarType <> nil) and (Self.VarType.TypeOfExpr <> nil) then
  begin
    ComputeType := XTree_Node(Self.VarType.TypeOfExpr).Copy();
    (Result as XTree_VarDecl).VarType := XType.Create(xtUnknown);
    (Result as XTree_VarDecl).VarType.TypeOfExpr := ComputeType;
  end;
end;

{ XTree_DestructureDecl }
function XTree_DestructureDecl.Copy(): XTree_Node;
begin
  Result := XTree_DestructureDecl.Create(Self.Pattern.Copy as XTree_Destructure, Self.Expression.Copy, FContext, FDocPos);
end;

{ XTree_InitializerList }
function XTree_InitializerList.Copy(): XTree_Node;
begin
  Result := XTree_InitializerList.Create(CopyNodeArray(Self.Items), FContext, FDocPos);
end;

{ XTree_TypeCast }
function XTree_TypeCast.Copy(): XTree_Node;
begin
  Result := XTree_TypeCast.Create(Self.TargetType, Self.Expression.Copy, FContext, FDocPos);
end;

{ XTree_ClassDecl }
function XTree_ClassDecl.Copy(): XTree_Node;
begin
  Result := XTree_ClassDecl.Create(Self.ClassDeclName, Self.ParentName, CopyNodeArray(Self.Fields), CopyNodeArray(Self.Methods), FContext, FDocPos);
end;

{ XTree_ClassCreate }
function XTree_ClassCreate.Copy(): XTree_Node;
var NewNode: XTree_ClassCreate;
begin
  NewNode := XTree_ClassCreate.Create(Self.ClassIdent, CopyNodeArray(Self.Args), FContext, FDocPos);
  NewNode.ClassTyp := Self.ClassTyp;
  Result := NewNode;
end;

{ XTree_DynCast }
function XTree_DynCast.Copy(): XTree_Node;
begin
  Result := XTree_DynCast.Create(Self.Expression.Copy, Self.TargetTypeNode.Copy, FContext, FDocPos);
end;

{ XTree_TypeIs }
function XTree_TypeIs.Copy(): XTree_Node;
begin
  Result := XTree_TypeIs.Create(Self.Expression.Copy, Self.TargetTypeNode.Copy, FContext, FDocPos);
end;

{ XTree_IfExpr }
function XTree_IfExpr.Copy(): XTree_Node;
begin
  Result := XTree_IfExpr.Create(Self.Condition.Copy, Self.ThenExpr.Copy, Self.ElseExpr.Copy, FContext, FDocPos);
end;

{ XTree_Return }
function XTree_Return.Copy(): XTree_Node;
var
  NewExpr: XTree_Node;
begin
  if Self.Expr <> nil then NewExpr := Self.Expr.Copy else NewExpr := nil;
  Result := XTree_Return.Create(NewExpr, FContext, FDocPos);
end;

{ XTree_Break }
function XTree_Break.Copy(): XTree_Node;
begin
  Result := XTree_Break.Create(FContext, FDocPos);
end;

{ XTree_Continue }
function XTree_Continue.Copy(): XTree_Node;
begin
  Result := XTree_Continue.Create(FContext, FDocPos);
end;

{ XTree_Function }
function XTree_Function.Copy(): XTree_Node;
var
  NewNode: XTree_Function;
begin
  NewNode := XTree_Function.Create(Self.Name, Self.ArgNames, Self.ArgPass, Self.ArgTypes, Self.RetType, Self.PorgramBlock.Copy as XTree_ExprList, FContext, FDocPos);
  NewNode.IsNested := Self.IsNested;
  NewNode.SingleExpression := Self.SingleExpression;
  NewNode.Extra    := Self.Extra;
  NewNode.SelfType := Self.SelfType;
  NewNode.TypeName := Self.TypeName;
  NewNode.InternalFlags := Self.InternalFlags;
  // Do not copy compilation state (PreCompiled, MethodVar, etc.)
  Result := NewNode;
end;

{ XTree_GenericFunction }
function XTree_GenericFunction.Copy(): XTree_Node;
begin
  Result := XTree_GenericFunction.Create(Self.GenericFunction.Copy, FContext, FDocPos);
end;

{ XTree_Field }
function XTree_Field.Copy(): XTree_Node;
begin
  Result := XTree_Field.Create(Self.Left.Copy, Self.Right.Copy, FContext, FDocPos);
end;

{ XTree_Invoke }
function XTree_Invoke.Copy(): XTree_Node;
var
  NewSelf: XTree_Node;
  NewNode: XTree_Invoke;
begin
  if Self.SelfExpr <> nil then NewSelf := Self.SelfExpr.Copy else NewSelf := nil;
  NewNode := XTree_Invoke.Create(Self.Method.Copy, CopyNodeArray(Self.Args), FContext, FDocPos);
  NewNode.SelfExpr := NewSelf;
  Result := NewNode;
end;

{ XTree_InheritedCall }
function XTree_InheritedCall.Copy(): XTree_Node;
var
  NewSelf: XTree_Node;
  NewNode: XTree_InheritedCall;
begin
  if Self.SelfExpr <> nil then NewSelf := Self.SelfExpr.Copy else NewSelf := nil;
  NewNode := XTree_InheritedCall.Create(CopyNodeArray(Self.Args), FContext, FDocPos);
  NewNode.SelfExpr := NewSelf;
  Result := NewNode;
end;

{ XTree_Index }
function XTree_Index.Copy(): XTree_Node;
var NewNode: XTree_Index;
begin
  NewNode := XTree_Index.Create(Self.Expr.Copy, Self.Index.Copy, FContext, FDocPos);
  NewNode.ForceTypeSize := Self.ForceTypeSize;
  Result := NewNode;
end;

{ XTree_If }
function XTree_If.Copy(): XTree_Node;
var
  NewElse: XTree_ExprList;
begin
  if Self.ElseBody <> nil then NewElse := Self.ElseBody.Copy as XTree_ExprList else NewElse := nil;
  Result := XTree_If.Create(CopyNodeArray(Self.Conditions), CopyNodeArray(Self.Bodys), NewElse, FContext, FDocPos);
end;

{ XTree_Case }
function XTree_Case.Copy(): XTree_Node;
var
  NewBranches: TCaseBranchArray;
  NewElse: XTree_Node;
  i: Integer;
begin
  SetLength(NewBranches, Length(Self.Branches));
  for i := 0 to High(Self.Branches) do
  begin
    NewBranches[i].Labels.Init(Branches[i].Labels.RawOfManaged());
    NewBranches[i].Body := Branches[i].Body.Copy;
  end;
  if Self.ElseBody <> nil then NewElse := Self.ElseBody.Copy else NewElse := nil;
  Result := XTree_Case.Create(Self.Expression.Copy, NewBranches, NewElse, FContext, FDocPos);
end;

{ XTree_While }
function XTree_While.Copy(): XTree_Node;
begin
  Result := XTree_While.Create(Self.Condition.Copy, Self.Body.Copy as XTree_ExprList, FContext, FDocPos);
end;

{ XTree_Raise }
function XTree_Raise.Copy(): XTree_Node;
begin
  Result := XTree_Raise.Create(Self.ExceptionObject.Copy, FContext, FDocPos);
end;

{ XTree_Try }
function XTree_Try.Copy(): XTree_Node;
var
  NewHandlers: TExceptionHandlerArray;
  NewElse: XTree_Node;
  i: Integer;
begin
  SetLength(NewHandlers, Length(Self.Handlers));
  for i := 0 to High(Self.Handlers) do
  begin
    NewHandlers[i].VarName := Self.Handlers[i].VarName;
    NewHandlers[i].ExceptionType := Self.Handlers[i].ExceptionType;
    NewHandlers[i].Body := Self.Handlers[i].Body.Copy;
  end;
  if Self.ElseBody <> nil then NewElse := Self.ElseBody.Copy else NewElse := nil;
  Result := XTree_Try.Create(Self.TryBody.Copy as XTree_ExprList, NewHandlers, NewElse, FContext, FDocPos);
end;

{ XTree_For }
function XTree_For.Copy(): XTree_Node;
var
  NewEntry, NewCond, NewLoop: XTree_Node;
begin
  if Self.EntryStmt <> nil then NewEntry := Self.EntryStmt.Copy else NewEntry := nil;
  if Self.Condition <> nil then NewCond := Self.Condition.Copy else NewCond := nil;
  if Self.LoopStmt <> nil then NewLoop := Self.LoopStmt.Copy else NewLoop := nil;
  Result := XTree_For.Create(NewEntry, NewCond, NewLoop, Self.Body.Copy as XTree_ExprList, FContext, FDocPos);
end;

{ XTree_Repeat }
function XTree_Repeat.Copy(): XTree_Node;
begin
  Result := XTree_Repeat.Create(Self.Condition.Copy, Self.Body.Copy as XTree_ExprList, FContext, FDocPos);
end;

{ XTree_UnaryOp }
function XTree_UnaryOp.Copy(): XTree_Node;
begin
  Result := XTree_UnaryOp.Create(Self.OP, Self.Left.Copy, FContext, FDocPos);
end;

{ XTree_BinaryOp }
function XTree_BinaryOp.Copy(): XTree_Node;
begin
  Result := XTree_BinaryOp.Create(Self.OP, Self.Left.Copy, Self.Right.Copy, FContext, FDocPos);
end;

{ XTree_Assign }
function XTree_Assign.Copy(): XTree_Node;
begin
  Result := XTree_Assign.Create(Self.OP, Self.Left.Copy, Self.Right.Copy, FContext, FDocPos);
end;

{ XTree_Print }
function XTree_Print.Copy(): XTree_Node;
begin
  Result := XTree_Print.Create(CopyNodeArray(Self.Args), FContext, FDocPos);
end;


end.
