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
    destructor Destroy(); override;
    function ToString(offset:string=''): string; override;

    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;

    function Copy(): XTree_Node; override;
  end;

  XTree_Annotation = class(XTree_Node)
    Identifier: XTree_Node;
    Value: XTree_Node;
    constructor Create(ACTX: TCompilerContext; DocPos: TDocPos); override;
    function Copy(): XTree_Node; override;
  end;

  XTree_Annotating = class(XTree_Node)
    Annotations: XTree_ExprList;
    Values: TStringToVarDict;
    procedure ProcessAnnotations();
    procedure PushCompilerSetting();
    procedure PopCompilerSetting();
    destructor Destroy; override;
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
    function ToString(Offset:string=''): string; override;
    function Copy(): XTree_Node; override;
  end;

  XTree_Pointer  = class(XTree_Const)
    Value: PtrInt;
    constructor Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function ToString(Offset:string=''): string; override;
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
    function ToString(Offset:string=''): string; override;
    function Copy(): XTree_Node; override;
  end;

  XTree_Float = class(XTree_Const)
    Value: Double;
    constructor Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function SetExpectedType(ExpectedType: EExpressBaseType): Boolean; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function ToString(Offset:string=''): string; override;
    function Copy(): XTree_Node; override;
  end;

  XTree_String = class(XTree_Const)
    constructor Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (*
    import 'path/test.xpr' as *
  *)
  XTree_ImportUnit = class(XTree_Node)
    UnitPath: string;
    UnitAlias: string;
    constructor Create(APath, AAlias: string; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function ToString(offset: string = ''): string; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  XTree_TypeDecl = class(Xtree_Node)
    Name: string;
    TypeDef: XType;
    // Generic type parameters e.g. type TArray<T> = array of T
    TypeParams:      TStringArray;
    TypeConstraints: TStringArray;

    constructor Create(AName:String; ATypeDef: XType; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
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
    ParentExplicitTypes: XTypeArray;
    TypeDecls: XNodeArray;
    Fields: XNodeArray;
    Methods: XNodeArray;
    ClassDeclType: XType;
    // Generic type parameters e.g. class<K, V>
    TypeParams:      TStringArray;
    TypeConstraints: TStringArray;

    constructor Create(AName, AParentName: string; ATypeDecls, AFields, AMethods: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function ToString(offset:string=''): string; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  //
  XTree_ClassCreate = class(XTree_Node)
    ClassTyp:           XType;
    ClassIdent:         string;
    Args:               XNodeArray;
    ExplicitTypeParams: XTypeArray;  // from new TPair<Int, String>(...) syntax

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

  // 'expr as func(T): R' - selects a specific overload of a function by signature.
  // Resolves at compile time; returns the matched method var.
  XTree_FuncSelect = class(XTree_Node)
    Expression:  XTree_Node;  // the overloaded name (XTree_Identifier or field)
    TargetType:  XType;       // the func(T): R type parsed from the RHS of 'as'

    constructor Create(AExpr: XTree_Node; ATargetType: XType;
                       ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
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

  // Named argument in a call
  // Test(z:=55, y:=11)
  // Test(x:=pass, z:=999)
  XTree_NamedArg = class(XTree_Node)
    ArgName: string;
    Value:   XTree_Node;  // nil means pass/use default
    constructor Create(AName: string; AValue: XTree_Node;
                       ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
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
  XTree_Function = class(XTree_Annotating)
    Name:     string;
    ArgNames: TStringArray;
    ArgPass:  TPassArgsBy;
    ArgTypes: XTypeArray;
    ArgDefaults: XNodeArray;  // parallel to ArgNames; nil = required param
    RetType:  XType;
    ProgramBlock: XTree_ExprList;
    IsNested: Boolean;
    MiniCTX: TMiniContext;
    SingleExpression: Boolean;
    isProperty: Boolean;
    isConstructor: Boolean;

    // Currently only used for VMT info
    Extra: SizeInt;

    // Two ways to achieve the same (This is an afterthought)
    SelfType: XType;
    TypeName: String;
    CallingConvention: string;

    // populated after .Compile for .DelayedCompile
    PreCompiled: Boolean;
    FullyCompiled: Boolean;
    MethodVar: TXprVar;

    // Generic type parameter names declared on this function: func Foo<T, U>(...)
    // Empty for non-generic functions.
    TypeParams: TStringArray;
    // Parallel to TypeParams: constraint name per param, e.g. 'numeric', 'array', 'class', ''
    TypeConstraints: TStringArray;

    //internal function properties
    InternalFlags: TCompilerFlags;

    //constructor Create(AName: string; AArgNames: TStringArray; AArgTypes: XTypeArray; AProg: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    constructor Create(AName: string; AArgNames: TStringArray; ByRef: TPassArgsBy; AArgTypes: XTypeArray; ARet:XType; AProg: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    destructor Destroy(); override;

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
    function CopyMethod(ArgTypes: XTypeArray; ASelfType, ARetType: XType;
                        ExplicitParams: XTypeArray; DocPos: TDocPos): XTree_Function;
    function Copy(): XTree_Node; override;
  end;

  // specialize(FuncName<ConcreteType1, ConcreteType2>)
  // Forces specialization of a generic function and returns its method pointer.
  // Used to obtain a typed function pointer or to pre-instantiate a generic.
  XTree_Specialize = class(XTree_Node)
    FuncName:             string;
    ExplicitTypes:        XTypeArray;
    IsTypeSpecialization: Boolean;  // True when used as: type TA = specialize TMap<K,V>
    ConcreteTypeName:     string;   // name to register (the TA in the example above)

    constructor Create(AFuncName: string; ATypes: XTypeArray;
                       ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    constructor CreateTypeSpec(AConcreteTypeName, ASourceName: string;
                               ATypes: XTypeArray;
                               ACTX: TCompilerContext; DocPos: TDocPos); reintroduce;
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  XTree_ClosureFunction = class(XTree_Node)
    ClosureFunction: XTree_Function;

    constructor Create(AMethod: XTree_Function; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function CompileLValue(Dest: TXprVar): TXprVar; override;
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
    ExplicitTypeParams: XTypeArray;  // from Compare<Int>(...) syntax
    PropertyAccess: Boolean;

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
    Expr: XTree_Node;
    Indices: XNodeArray;
    ForceTypeSize: Int32; // useful for length and refcount
    FIsProperty: Boolean;

    constructor Create(AExpr, AIndex: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    constructor Create(AExpr: XTree_Node; const AIndices:array of XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(Offset:string=''): string; override;
    function GetIndicesTypes(): XTypeArray;
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function CompileLValue(Dest: TXprVar): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;


  (* if statement *)
  XTree_If = class(XTree_Annotating)
    Conditions: XNodeArray;
    Bodys: XNodeArray;
    ElseBody: XTree_ExprList;
    constructor Create(AConds, ABodys: XNodeArray; AElseBody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset:string=''): string; override;

    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (* switch...of...case...end *)
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
  XTree_While = class(XTree_Annotating)
    Condition: XTree_Node;
    Body: XTree_ExprList;
    constructor Create(ACond: XTree_Node; ABody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset:string=''): string; override;

    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (* pass / nop *)
  XTree_Pass = class(XTree_Node)
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
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
  XTree_For = class(XTree_Annotating)
    EntryStmt, Condition, LoopStmt: XTree_Node;
    Body: XTree_ExprList;
    constructor Create(AEntryStmt, ACondition, ALoopStmt: XTree_Node; ABody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset:string=''): string; override;

    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (* for..in loop *)
  XTree_ForIn = class(XTree_Annotating)
    ItemVar: XTree_Node;
    Collection: XTree_Node;
    Body: XTree_ExprList;
    DeclareIdent: Byte;

    constructor Create(AItemVar: XTree_Node; ACollection: XTree_Node; ADeclareIdent: Byte; ABody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset:string=''): string; override;

    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;


  (* repeat..until loop *)
  XTree_Repeat = class(XTree_Annotating)
    Condition: XTree_Node;
    Body: XTree_ExprList;
    constructor Create(ACond: XTree_Node; ABody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos); virtual; reintroduce;
    function ToString(offset: string = ''): string; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags = []): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

  (* operator types *)
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

  XTree_ListComp = class(XTree_Node)
    ItemVar:    XTree_Node;   // XTree_Identifier or XTree_Destructure
    Collection: XTree_Node;   // for-in: the iterable; nil for range
    StartExpr:  XTree_Node;   // range: start value; nil for for-in
    EndExpr:    XTree_Node;   // range: end value (inclusive); nil for for-in
    FilterExpr: XTree_Node;   // where(...); nil = no filter
    YieldExpr:  XTree_Node;   // expression to collect
    IsRange:    Boolean;
    DeclareVar: Boolean;

    constructor Create(AItemVar, ACollection, AStart, AEnd, AFilter, AYield: XTree_Node;
                       AIsRange, ADeclareVar: Boolean; ACTX: TCompilerContext; DocPos: TDocPos);
                       virtual; reintroduce;
    function ResType(): XType; override;
    function Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar; override;
    function Copy(): XTree_Node; override;
  end;

function CompileAST(astnode:XTree_Node; writeTree: Boolean=False; doFree:Boolean = True): TIntermediateCode;

operator + (left: XNodeArray; Right: XTree_Node): XNodeArray;
function NodeArray(Arr: array of XTree_Node): XNodeArray;
function ScanFunctionCaptures(Func: XTree_Function; ctx: TCompilerContext): TStringArray;

implementation

uses
  xpr.Utils,
  xpr.Vartypes,
  xpr.Errors,
  xpr.Langdef,
  xpr.MagicIntrinsics,
  xpr.Dictionary,
  xpr.ffi,
  ffi,
  Math, Variants;


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

function TransferableData(x: TInstructionData; y: TXprVar): Boolean;
begin
  Result := (x.Pos = y.MemPos) and (BaseIntType(x.BaseType) = BaseIntType(y.VarType.BaseType)) and
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



function CompileAST(astnode: XTree_Node; writeTree: Boolean = False; doFree: Boolean = True): TIntermediateCode;
var
  i, tryFinallyPatch: Int32;
  managed: TXprVarList;
  FreeLast: array[0..1] of TXprVar;
begin
  if writeTree then
  begin
    WriteLn('----| TREE STRUCTURE |--------------------------------------------');
    WriteFancy(astnode.ToString());
    WriteLn('------------------------------------------------------------------'+#13#10);
  end;

  // TRY ->
  tryFinallyPatch := astnode.Emit(GetInstr(icIncTry, [NullVar]), astnode.FDocPos);

  astnode.Compile(NullResVar, [cfRootBody]);

  // FINALLY ->
  astnode.ctx.PatchArg(tryFinallyPatch, ia1, astnode.ctx.CodeSize());


  // ---------------------------------------------------------------------------
  // Finalize globals explicitly - EmitFinalizeVar skips globals by default,
  // so we call it with ForceGlobal=True here at program exit.

  // Late releases:
  FreeLast[0] := astnode.ctx.TryGetVar('__G_RangeExceptionTemplate');
  FreeLast[1] := astnode.ctx.TryGetVar('__G_NativeExceptionTemplate');

  managed := astnode.ctx.GetGlobalManagedDeclarations();
  for i := managed.High downto 0 do
  begin
    // Pass by exception handlers for now
    if (FreeLast[0] = managed.Data[i]) or (FreeLast[1] = managed.Data[i]) then
      continue;

    astnode.ctx.EmitFinalizeVar(managed.Data[i], True);
  end;
  // Free Exception handlers
  if FreeLast[1] <> NullVar then astnode.ctx.EmitFinalizeVar(FreeLast[1], True);
  if FreeLast[0] <> NullVar then astnode.ctx.EmitFinalizeVar(FreeLast[0], True);


  // Emit final program RET directly. XTree_Return would call GetManagedDeclarations
  // which skips globals, so globals would not be finalized if we used it here.
  astnode.ctx.Emit(GetInstr(icRET, []), astnode.ctx.CurrentDocPos(), astnode.ctx.FSettings);

  astnode.DelayedCompile(NullResVar);

  while Length(astnode.ctx.DelayedNodes) > 0 do
  begin
    XTree_ExprList(astnode).DelayedList := astnode.ctx.DelayedNodes;
    astnode.ctx.DelayedNodes := [];
    astnode.DelayedCompile(NullResVar);
  end;

  Result := astnode.ctx.Intermediate;
end;

function IsSelf(Node: XTree_Node): Boolean;
begin
  Result := (Node is XTree_Identifier) and (XprCase(XTree_Identifier(Node).Name) = 'self');
end;

function IsConstructor(Node: XTree_Node): Boolean;
var Name: string;
begin
  Result := False;
  if (Node is XTree_Identifier) then
  begin
    Name := XprCase(XTree_Identifier(Node).Name);
    Result := (Name = 'create') or (Name = '!init_defaults');
  end;
end;

function IsConstructor(Typ: XType): Boolean;
var Name: string;
begin
  Result := False;
  if (Typ is XType_Method) then
  begin
    Name := XprCase(Typ.Name);
    Result := ((Name = 'create') or (Name = '!init_defaults')) and XType_Method(Typ).ClassMethod;
  end;
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
  inherited Create(ACTX, DocPos);

  Self.List := AList;
  Self.DelayedList := [];
end;

constructor XTree_ExprList.Create(AStmt: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);

  SetLength(Self.List, 1);
  Self.List[0] := AStmt;
end;

destructor XTree_ExprList.Destroy();
begin
  inherited;
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
var
  i, hwm,localVarWatermark: Int32;
  needsBlockScope, isFunctionBody: Boolean;
  V: TXprVar;
begin
  isFunctionBody := cfFunctionBody in Flags;
  needsBlockScope := not (cfFunctionBody in Flags) and not (cfRootBody in Flags);
  Flags -= [cfRootBody, cfFunctionBody];

  if needsBlockScope then
    ctx.PushNameScope();

  try
    i := 0;
    localVarWatermark := ctx.TempHighWater();
    while i <= High(Self.List) do
    begin
      if Self.List[i] <> nil then
      begin
        { Snapshot the variable pool size before each statement.
          Any TXprVar allocated after this point belongs to this statement. }
        hwm := ctx.TempHighWater();

        Self.List[i].Compile(NullResVar, Flags);

        { After the statement: release any managed temps that were not claimed
          by an assignment. This covers:
            - Discarded function return values: SomeFunc() as a statement
            - String/array expression intermediates: "a"+"b"+"c" where the
              result is not assigned
            - Any other expression whose managed result goes unused
          Vars that were claimed (IncRef'd by ManageMemory) have their Collect
          emitted here too, but Collect only decrements when rc>1, so claimed
          vars correctly end up at rc=1 (owned by the named destination). }
        if not (cfNoCollect in Flags) then
          ctx.EmitAbandonedTempCleanup(hwm);
      end;
      Inc(i);
    end;


    { This handles the case of managed type declared within branches
      See refcount test 13, and further test 15
      Named-var cleanup for loop-body declared vars (e.g. test 15).
      Skip when we ARE the function body - EmitFinalizeScope in the final
      block handles those. }
    if not isFunctionBody then
    begin
      if not (cfNoCollect in Flags) then
      begin
        for i := localVarWatermark to ctx.Variables.High do
        begin
          V := ctx.Variables.Data[i];
          if V.IsTemporary then Continue;
          if V.Reference then Continue;
          if not V.VarType.IsManagedType(ctx) then Continue;
          ctx.EmitFinalizeVar(V);
        end;
      end;
    end;
  finally
    if needsBlockScope then
      ctx.PopNameScope();
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

constructor XTree_Annotation.Create(ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Identifier := nil;
  Value := nil;
end;

procedure XTree_Annotating.ProcessAnnotations();
var
  i,j: Int32;
  a: XTree_Annotation;
  strval, name: string;
  argval, arguments: TStringArray;
begin
  // create only once!
  if(Self.Values = nil) then
    Self.Values := TStringToVarDict.Create(@HashStr); //Dict[str]:=variant

  if Self.Annotations = nil then
    Exit;

  for i:=0 to High(Self.Annotations.List) do
  begin
    a := XTree_Annotation(Self.Annotations.List[i]);
    name := XprCase(XTree_Identifier(a.Identifier).Name);

    if a.Value = nil then
      Values.Add(name, True)
    else if a.Value is XTree_String then
    begin
      strval := XTree_String(a.Value).StrValue;
      strval := strval.Replace(' ', '', [rfReplaceAll]);

      arguments := strval.Split([';']);

      if Length(arguments) = 1 then
        //@jit('max') || @native('kernel32.dll::GetTickCount')
        Values.Add(name, strval)
      else if name = 'opt' then
        //@opt('jit:max; unroll:true; inline:true');
        for j:=0 to High(arguments) do
        begin
          argval := arguments[j].Split([':']);

          if Length(argval) >= 2 then
            Values.Add(XprCase(argval[0]), argval[1])
          else if Length(argval) = 1 then
            Values.Add(XprCase(argval[0]), True);
        end;
    end
    else if a.Value is XTree_Bool then
      Values.Add(name, XTree_Bool(a.Value).Value)
    else if a.Value is XTree_Int then
      Values.Add(name, XTree_Int(a.Value).Value);
  end;
end;

procedure XTree_Annotating.PushCompilerSetting();
var
  value: Variant;
  strval: string;
  setting: TCompilerSettings;
begin
  setting := ctx.CurrentSetting(Self.FSettings);

  if Self.Values.Get('jit', value) then
  begin
    strval := XprCase(VarToStr(value));
    if (strval = 'full') then
      setting.JIT := 3;
    if (strval = 'max') or (strval='true') or (strval = 'on') then
      setting.JIT := 2;
    if (strval = 'low') then
      setting.JIT := 1;
    if (strval = 'off') or (strval='false') then
      setting.JIT := 0;

    if (Length(strval) = 1) and (strval[1] in ['0','1','2','3']) then  setting.JIT := StrToInt(strval);
  end;

  if Self.Values.Get('r', value) or Self.Values.Get('rangechecks', value)  then
  begin
    strval := XprCase(VarToStr(value));
    if (strval = 'on')  or (strval = 'true')  or (strval='1') then setting.RangeChecks:=True;
    if (strval = 'off') or (strval = 'false') or (strval='0') then setting.RangeChecks:=False;
  end;

  if Self.Values.Get('cse', value)  then
  begin
    strval := XprCase(VarToStr(value));
    if (strval = 'on')  or (strval = 'true')  or (strval='1') then ;//setting.CSE:=True;
    if (strval = 'off') or (strval = 'false') or (strval='0') then ;//setting.CSE:=False;
  end;

  if Self.Values.Get('inline', value)  then
  begin
    strval := XprCase(VarToStr(value));
    if (strval = 'on')  or (strval = 'true')  or (strval='1') then setting.CanInline:=True;
    if (strval = 'off') or (strval = 'false') or (strval='0') then setting.CanInline:=False;
  end;

  if Self.Values.Get('regcost', value)  then
  begin
    strval := VarToStr(value);
    setting.JITPenalty:=StrToIntDef(strval,0);
  end;

  ctx.PushSettingOverride(setting);
end;

procedure XTree_Annotating.PopCompilerSetting();
begin
  ctx.PopSettingOverride();
end;

destructor XTree_Annotating.Destroy;
begin
  Values.Free;  // TStringToVarDict created in ProcessAnnotations
  inherited;
end;

constructor XTree_VarStub.Create(AVar: TXprVar; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);

  Self.VarDecl := AVar;
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

// BOOL
constructor XTree_Bool.Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);

  Self.StrValue := AValue;
  Self.Value    := AValue.ToBoolean();
  Self.Expected := xtBool;
end;

function XTree_Bool.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  Result := ctx.RegConst(Constant(Value, Expected));
end;

function XTree_Bool.ToString(Offset:string=''): string;
begin
  Result := Offset + Self.ClassName+'('+_PURPLE_+BoolToStr(Self.Value)+_WHITE_+')';
end;

// POINTER
constructor XTree_Pointer.Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);

  Self.StrValue := AValue;
  if AValue = 'nil' then Self.Value := 0
  else                   Self.Value := AValue.ToInt64();
  Self.Expected := xtPointer;
end;

function XTree_Pointer.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  Result := ctx.RegConst(Pointer(Value));
end;

function XTree_Pointer.ToString(Offset:string=''): string;
begin
  Result := Offset + Self.ClassName+'('+_PURPLE_+IntToStr(Self.Value)+_WHITE_+')';
end;

// CHAR
constructor XTree_Char.Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);

  if Length(AValue) <> 1 then ctx.RaiseException(eUnexpected, DocPos);
  Self.StrValue := AValue[1];
  Self.Value    := AValue[1];
  Self.Expected := xtAnsiChar;
end;

function XTree_Char.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  Result := ctx.RegConst(Constant(Value, Expected));
end;

// INT
constructor XTree_Int.Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);

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

function XTree_Int.ToString(Offset:string=''): string;
begin
  Result := Offset + Self.ClassName+'('+_PURPLE_+IntToStr(Self.Value)+_WHITE_+')';
end;


// FLOAT
constructor XTree_Float.Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);

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

function XTree_Float.ToString(Offset:string=''): string;
begin
  Result := Offset + Self.ClassName+'('+_PURPLE_+FloatToStr(Self.Value)+_WHITE_+')';
end;

// STRING
constructor XTree_String.Create(AValue: string; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);

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
    Self.Emit(GetInstr(icMOV, [dest, constString]), FDocPos);
    Result := dest;
  end else
  begin
    Result := ctx.GetTempVar(ctx.GetType(xtAnsiString));
    Self.Emit(GetInstr(icMOV, [Result, constString]), FDocPos);
  end;

  (*
  with XTree_UnaryOp.Create(op_INCREF, nil, FContext, FDocPos) do
  try
    FSettings := ctx.CurrentSetting(Self.FSettings);
    Left  := XTree_VarStub.Create(Result, FContext, FDocPos);
    Compile();
  finally
    Free();
  end;
  *)
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




constructor XTree_TypeDecl.Create(AName:String; ATypeDef: XType; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);

  Self.Name:= AName;
  Self.TypeDef := ATypeDef;
end;

function XTree_TypeDecl.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  // Generic type declaration - store as template, don't compile yet
  if Length(TypeParams) > 0 then
  begin
    ctx.GenericTypeMap[XprCase(Self.Name)] := Self;
    Exit(NullResVar);
  end;

  ctx.ResolveToFinalType(Self.TypeDef);
  ctx.AddType(Self.Name, Self.TypeDef, True);
  Result := NullResVar;
end;

function XTree_TypeDecl.Copy(): XTree_Node;
var
  clone: XTree_TypeDecl;
begin
  clone := XTree_TypeDecl.Create(Self.Name, Self.TypeDef, FContext, FDocPos);
  clone.TypeParams      := Self.TypeParams;
  clone.TypeConstraints := Self.TypeConstraints;
  clone.FSettings       := Self.FSettings;
  Result := clone;
end;

// ============================================================================
// Holding an identifier, it has no other purpose than to be used as a lookup
//
constructor XTree_Identifier.Create(AName:String; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);

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
        ctx.RaiseExceptionFmt('Identifier `%s` not declared', [Self.Name], FDocPos);

      Self.FResType := foundVar.VarType;
      if Self.FResType = nil then
        ctx.RaiseExceptionFmt('Variable `%s` has no defined type', [Self.Name], FDocPos);

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
  foundVar, localVar: TXprVar;
begin
  Result := NullResVar;
  foundVar := ctx.GetVar(Self.Name, FDocPos);

  if (foundVar.MemPos = mpHeap) then
  begin
   // Do nothing - heap variables pass straight to the VM as mpHeap
   //
   Result := foundVar;
  end
  else if (foundVar.IsGlobal and ctx.IsInsideFunction()) then
  begin
    if (foundVar.VarType.BaseType <> xtMethod) then
    begin
      // XXX we are moving away from this.. bit by bit.
      // Note: JIT doesnt have global support at the moment, none-local path is
      // recomended still.
      WriteLn('[Hint] Use `ref '+Self.Name+'` to declare intent to use global variables at '+Self.FDocPos.ToString);
    end;
    (*
    // create a local reference of global
    localVar :=  ctx.GetTempVar(foundVar.VarType);
    localVar.Reference := True;
    ctx.Variables.Data[ctx.Variables.High].Reference := True;
    // dont register it, force new load every time - this handles uses without explicit ref declaration
    Self.Emit(GetInstr(icLOAD_GLOBAL, [localVar, foundVar]), FDocPos);
    foundVar.MemPos := mpGlobal;
    Result := localVar;
    *)

    foundVar.MemPos := mpGlobal; // just set global
    Result := foundVar;
  end
  else
  begin
    if foundVar.NestingLevel <> 0 then ctx.RaiseException('This is bad', FDocPos);

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
  inherited Create(ACTX, DocPos);

  Self.Variables := AVariables;
end;

function XTree_NonLocalDecl.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  foundVar, localVar: TXprVar;
  i: Int32;
begin
  Result := NullResVar;
  for i:=0 to Self.Variables.High() do
  begin
    Result := NullResVar;
    foundVar := ctx.GetVar(Self.Variables.Data[i].Name, FDocPos);

    if (foundVar.IsGlobal and ctx.IsInsideFunction()) then
    begin
      // create a local reference of global
      localVar := TXprVar.Create(foundVar.VarType);
      localVar.Reference := True; // this should be enough
      ctx.RegVar(Self.Variables.Data[i].Name, localVar, FDocPos);
      foundVar.MemPos := mpGlobal;
      Self.Emit(GetInstr(icLOAD_GLOBAL, [localVar, foundVar]), FDocPos);
    end else if foundVar <> NullResVar then
      ctx.RaiseException('Cannot load local var as nonlocal!', FDocPos)
    else
      ctx.RaiseException(eUndefinedIdentifier, FDocPos);
  end;
end;

function XTree_NonLocalDecl.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  Result := inherited;
end;

// ============================================================================
// Variable declaration
//
constructor XTree_VarDecl.Create(AVariables: XIdentNodeList; AExpr: XTree_Node; AType: XType; Constant:Boolean; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);

  Self.Variables := AVariables;
  Self.VarType   := AType;     // this is resolved in parsing. It's too early though!
  Self.Expr      := AExpr;
  Self.IsConst   := Constant;
end;

constructor XTree_VarDecl.Create(AVariable: string; AExpr: XTree_Node; AType: XType; Constant:Boolean; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);

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
  i, varidx: Int32;
  tmpRes: TXprVar;
  CreatedIndices: array of Int32;
  stub: XTree_VarStub;
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

  // immediate resolve!
  if Self.VarType <> nil then
    ctx.ResolveToFinalType(Self.VarType);

  if VarType = nil then
  begin
    if Self.Expr = nil then
      ctx.RaiseException('Variable declaration requires an explicit type or an initial assignment', FDocPos);

    Self.VarType := Self.Expr.ResType();
    if Self.VarType = nil then
      ctx.RaiseExceptionFmt('Could not infer type for variable `%s`', [Self.Variables.Data[0].Name], FDocPos);
  end;

  // Track the exact variable indices we allocate
  CreatedIndices := nil;
  SetLength(CreatedIndices, Self.Variables.Size);

  for i:=0 to Self.Variables.High do
  begin
    Self.Variables.Data[i].FResType := self.VarType;
    ctx.RegVar(Self.Variables.Data[i].Name, self.VarType, Self.FDocPos, varidx);
    CreatedIndices[i] := varidx;
  end;

  if Self.Expr <> nil then
  begin
    for i:=0 to Self.Variables.High do
    begin
      varidx := CreatedIndices[i];

      // Use VarStub so we write directly to the exact memory slot we just allocated,
      // completely bypassing identifier name lookup to prevent overload cross-talk.
      stub := XTree_VarStub.Create(ctx.Variables.Data[varidx], ctx, Self.FDocPos);
      with XTree_Assign.Create(op_Asgn, stub, Self.Expr, ctx, Self.Expr.FDocPos) do
      try
        FSettings := ctx.CurrentSetting(Self.FSettings);
        Compile(NullResVar, Flags);

        // Magic variable, store the error handler in the Interpreter!
        if Self.Variables.Data[i].Name = '__G_NativeExceptionTemplate' then
          Self.Emit(GetInstr(icSET_ERRHANDLER, [ctx.Variables.Data[varidx], Immediate(0)]), FDocPos);
      finally
        stub.Free();
        Free();
      end;
    end;
  end else
  begin
    {fill with 0}
    for i:=0 to Self.Variables.High do
    begin
      tmpRes := ctx.Variables.Data[CreatedIndices[i]];
      Self.Emit(GetInstr(icFILL, [TmpRes, Immediate(TmpRes.VarType.Size()), Immediate(0)]), FDocPos);
    end;
  end;

  for i:=0 to High(CreatedIndices) do
    ctx.Variables.Data[CreatedIndices[i]].NonWriteable := Self.IsConst;

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
  if not (Expression is XTree_InitializerList) then
  begin
    SourceType := Expression.ResType();
    if not (SourceType is XType_Record) then
      ctx.RaiseException('The right-hand side of a destructuring declaration must be a record type.', Expression.FDocPos);

    RecType := SourceType as XType_Record;
    if RecType.FieldNames.Size <> Length(Pattern.Targets) then
      ctx.RaiseExceptionFmt('The number of variables to declare (%d) does not match the number of fields in the source record (%d).',
        [Length(Pattern.Targets), RecType.FieldNames.Size], Pattern.FDocPos);

    // Declare each new variable as per the record field types.
    for i := 0 to High(Pattern.Targets) do
    begin
      TargetIdent := Pattern.Targets[i] as XTree_Identifier;
      ctx.RegVar(TargetIdent.Name, RecType.FieldTypes.Data[i], TargetIdent.FDocPos);
    end;
  end else
  begin
    // Handle initializer lists on right hand side.
    // >> var (a,b) := [100,100];
    //
    // Declare each new variable as per the record field types.
    if Length(XTree_InitializerList(Expression).Items) <> Length(Pattern.Targets) then
          ctx.RaiseExceptionFmt('The number of variables to declare (%d) does not match the number of fields in the list (%d).',
            [Length(Pattern.Targets), Length(XTree_InitializerList(Expression).Items)], Pattern.FDocPos);

    for i := 0 to High(Pattern.Targets) do
    begin
      TargetIdent := Pattern.Targets[i] as XTree_Identifier;
      ctx.RegVar(TargetIdent.Name, XTree_InitializerList(Expression).Items[i].ResType(), TargetIdent.FDocPos);
    end;
  end;

  // Let assign handle it as per usual
  with XTree_Assign.Create(op_Asgn, Pattern, Expression, ctx, FDocPos) do
  try
    FSettings := ctx.CurrentSetting(Self.FSettings);
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

          if not ItemResult.VarType.IsManagedType(ctx) then
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

          if not ItemResult.VarType.IsManagedType(ctx) then
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
  if SElf.FResType = nil then
  begin
    ctx.ResolveToFinalType(Self.TargetType);
    Self.FResType := Self.TargetType;
  end;

  // The result of a cast is always the target type.
  Result := Self.FResType;
end;

function XTree_TypeCast.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  SourceVar: TXprVar;
  InstrCast: EIntermediate;
  IsReinterpretation: Boolean;
begin
  if Self.TargetType is XType_Record then
  begin
    Result := Dest;
    if Result = NullResVar then
      Result := ctx.GetTempVar(Self.TargetType);
    Expression.SetResTypeHint(Self.TargetType);
    Expression.Compile(Result, Flags);
    Exit;
  end;

  // reinterpret constant
  if ((Expression is XTree_Int)) and (Self.TargetType.BaseType in XprOrdinalTypes) then
  begin
    XTree_Int(Expression).SetExpectedType(Self.TargetType.BaseType);
    SourceVar := Expression.Compile(NullResVar, Flags).IfRefDeref(ctx);
    Result := SourceVar;
    Result.VarType := Self.ResType();
    Exit;
  end;

  SourceVar := Expression.Compile(NullResVar, Flags).IfRefDeref(ctx);


  // It's a reinterpretation if types are compatible pointers, or if they are
  // non-pointer ordinals of the exact same size.
  IsReinterpretation :=
    ( (Self.ResType() is XType_Pointer) and (SourceVar.VarType is XType_Pointer) ) or
    (
      (Self.ResType().BaseType in XprOrdinalTypes) and
      (SourceVar.VarType.BaseType in XprOrdinalTypes) and
      (Self.ResType().Size = SourceVar.VarType.Size)
    );

  if IsReinterpretation then
  begin
    // reinterpretation cast
    Result := SourceVar;
    Result.VarType := Self.ResType();
  end
  else
  begin
    // Dynamic conversion cast
    Result := Dest;
    if Result = NullResVar then
      Result := ctx.GetTempVar(Self.ResType());

    InstrCast := Self.ResType().EvalCode(op_Asgn, SourceVar.VarType);
    if InstrCast = icNOOP then
      ctx.RaiseExceptionFmt('Invalid cast: Cannot convert type `%s` to `%s`.',
        [SourceVar.VarType.ToString(), Self.ResType().ToString()], FDocPos);

    Self.Emit(GetInstr(InstrCast, [Result, SourceVar]), FDocPos);
  end;
end;

function XTree_TypeCast.CompileLValue(Dest: TXprVar): TXprVar;
var
  SourceVar: TXprVar;
begin
  if (Self.TargetType is XType_Pointer) or (Self.TargetType.BaseType in XprOrdinalTypes) then
  begin
    SourceVar := Expression.CompileLValue(Dest);
    Result := SourceVar;
    Result.VarType := Self.TargetType;
    Exit;
  end;

  // Record cast - allocate a temp and fill it via Compile (which now works correctly)
  if Self.TargetType is XType_Record then
  begin
    Result := ctx.GetTempVar(Self.TargetType);
    Self.Compile(Result, []);
    Exit;
  end;

  // All other casts produce non-addressable temporaries
  ctx.RaiseExceptionFmt(
    'Invalid assignment: The result of a `%s` cast is not a variable that can be assigned to.',
    [Self.TargetType.ToString()], FDocPos);
  Result := NullResVar;
end;


// ============================================================================
// Class Declaration
//
constructor XTree_ClassDecl.Create(AName, AParentName: string; ATypeDecls, AFields, AMethods: XNodeArray; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.ClassDeclName  := AName;
  Self.ParentName     := AParentName;
  Self.TypeDecls      := ATypeDecls;
  Self.Fields         := AFields;
  Self.Methods        := AMethods;
  Self.ClassDeclType  := nil;
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
  Builds the class metadata (XType_Class).
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

  i, j: Int32;
  FieldDecl: XTree_VarDecl;
  MethodNode: XTree_Function;
  typ: XType;
  items: TVMList;

  InitStmts: XNodeArray;
  InitFunc: XTree_Function;
  FieldLeft: XTree_Field;
  AssignStmt: XTree_Assign;
  ChildDefaultsCount: Int32;

  mangledParentName: string;
begin
  // Generic class template - store for later specialization, don't compile yet
  if Length(TypeParams) > 0 then
  begin
    ctx.GenericTypeMap[XprCase(Self.ClassDeclName)] := Self;
    Exit(NullResVar);
  end;

  // Preamble: Compile inner type aliases
  for i := 0 to High(TypeDecls) do
    TypeDecls[i].Compile(NullResVar, Flags);

  // --- Step 1 & 2: Resolve Parent and Build Field Lists ---
  ParentType := nil;
  if ParentName <> '' then
  begin
    // --- Generic Parent Specialization ---
    if Length(ParentExplicitTypes) > 0 then
    begin
      mangledParentName := ParentName;
      for i := 0 to High(ParentExplicitTypes) do
      begin
        ctx.ResolveToFinalType(ParentExplicitTypes[i]); // Ensure they are concrete
        mangledParentName += '_' + ParentExplicitTypes[i].Hash();
      end;

      if ctx.GetType(XprCase(mangledParentName)) = nil then
        ctx.SpecializeType(ParentName, mangledParentName, ParentExplicitTypes, FDocPos);

      ParentName := mangledParentName; // Swap name to specialized version
    end;
    // -------------------------------------

    typ := ctx.GetType(ParentName);
    if not (typ is XType_Class) then
      ctx.RaiseExceptionFmt('Parent `%s` is not a class type.', [ParentName], FDocPos);
    ParentType := typ as XType_Class;
  end;

  FieldNames.Init([]);
  FieldTypes.Init([]);
  FieldInfo.Init([]);

  ChildDefaultsCount := 0;
  InitStmts := nil;

  // If parent has defaults, prepare an inherited call first
  if (ParentType <> nil) and ParentType.VMT.Contains('!init_defaults') then
  begin
    SetLength(InitStmts, 1);
    InitStmts[0] := XTree_InheritedCall.Create([], ctx, FDocPos);
  end;

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
      begin
        ctx.ResolveToFinalType(FieldDecl.VarType);
        FieldTypes.Add(FieldDecl.VarType);

        if FieldDecl.VarType = nil then
           ctx.RaiseExceptionFmt('Can not deterimine the type of %s.', [FieldDecl.Variables.Data[j].Name], FieldDecl.FDocPos);
      end
      else if FieldDecl.Expr <> nil then
        FieldTypes.Add(FieldDecl.Expr.ResType())
      else
        ctx.RaiseExceptionFmt('Field `%s` must have an explicit type or an initializer.', [FieldDecl.Variables.Data[j].Name], FieldDecl.FDocPos);

      // Synthesize assignment statement for the default value
      if FieldDecl.Expr <> nil then
      begin
        Inc(ChildDefaultsCount);
        FieldLeft := XTree_Field.Create(
          XTree_Identifier.Create('self', ctx, FieldDecl.FDocPos),
          XTree_Identifier.Create(FieldDecl.Variables.Data[j].Name, ctx, FieldDecl.FDocPos),
          ctx, FieldDecl.FDocPos
        );
        // Deep copy Expr so multiple vars (var x, y := 5) evaluate distinctly
        AssignStmt := XTree_Assign.Create(op_Asgn, FieldLeft, FieldDecl.Expr.Copy(), ctx, FieldDecl.FDocPos);
        SetLength(InitStmts, Length(InitStmts) + 1);
        InitStmts[High(InitStmts)] := AssignStmt;
      end;
    end;
  end;

  // Synthesize the hidden method if this class specifically declared new defaults
  if ChildDefaultsCount > 0 then
  begin
    InitFunc := XTree_Function.Create('!init_defaults', [], [], [], nil,
                                      XTree_ExprList.Create(InitStmts, ctx, FDocPos),
                                      ctx, FDocPos);
    InitFunc.ProcessAnnotations(); // set up anotations
    InitFunc.Values.Add('virtual', True);

    SetLength(Methods, Length(Methods) + 1);
    Methods[High(Methods)] := InitFunc;
  end
  else if Length(InitStmts) > 0 then
  begin
    // We pre-allocated the inherited call, but there were no child defaults.
    // The VMT copy mechanism will inherit the parent's !init_defaults naturally.
    InitStmts[0].Free;
  end;

  // --- Step 3 & 4: Create Types and Runtime VMT Shell ---
  NewClassType := XType_Class.Create(ParentType, FieldNames, FieldTypes, FieldInfo);
  NewClassType.Name := ClassDeclName;
  ctx.ResolveToFinalType(XType(NewClassType));
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
  // XTree_Function.Compile will handle the rest of the VMT for us.
  for i := 0 to High(Methods) do
  begin
    MethodNode := XTree_Function(Methods[i]);
    MethodNode.ProcessAnnotations();
    if MethodNode.isConstructor then MethodNode.Values.Add('virtual', True);

    MethodNode.SelfType := NewClassType;
    MethodNode.Compile(NullResVar, Flags);
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
var
  mangledName: string;
  i: Int32;
begin
  // If explicit type params given, specialize first then look up
  if (ClassTyp = nil) and (Length(ExplicitTypeParams) > 0) then
  begin
    mangledName := ClassIdent;
    for i := 0 to High(ExplicitTypeParams) do
    begin
      mangledName += '_' + ExplicitTypeParams[i].Hash();
    end;

    // Trigger specialization if not yet done
    if ctx.GetType(XprCase(mangledName)) = nil then
      ctx.SpecializeType(ClassIdent, mangledName, ExplicitTypeParams, FDocPos);
    ClassTyp := ctx.GetType(XprCase(mangledName));
  end;

  if ClassTyp = nil then
    Result := ctx.GetType(Self.ClassIdent)
  else
    Result := ClassTyp;

  FResType := Result;

  if Result = nil then
    RaiseException('Undefined class type', FDocPos);
end;

function XTree_ClassCreate.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  initInvoke:  XTree_Invoke;
  mangledName: string;
  i:           Int32;
begin
  // Specialize a generic class on demand: new TPair<Int, String>(...)
  if (Self.ClassTyp = nil) and (Length(ExplicitTypeParams) > 0) then
  begin
    mangledName := ClassIdent;
    for i := 0 to High(ExplicitTypeParams) do
      mangledName += '_' + ExplicitTypeParams[i].Hash();

    if ctx.GetType(XprCase(mangledName)) = nil then
      ctx.SpecializeType(ClassIdent, mangledName, ExplicitTypeParams, FDocPos);

    Self.ClassTyp := ctx.GetType(XprCase(mangledName)) as XType_Class;
  end;

  if Self.ClassTyp = nil then
    Self.ClassTyp := ctx.GetType(Self.ClassIdent);

  // 1. Determine the destination variable for the new object pointer.
  if Dest = NullResVar then
    Result := ctx.GetTempVar(ClassTyp)
  else
    Result := Dest;

  // 2. Emit the NEW instruction with the ClassID and InstanceSize.
  Self.Emit(GetInstr(icNEW, [Result, Immediate(XType_Class(ClassTyp).ClassID), Immediate(XType_Class(ClassTyp).GetInstanceSize())]), FDocPos);

  if XType_Class(ClassTyp).VMT.Contains('!init_defaults') then
  begin
    initInvoke := XTree_Invoke.Create(
      XTree_Identifier.Create('!init_defaults', ctx, FDocPos),
      [], ctx, FDocPos
    );
    initInvoke.SelfExpr := XTree_VarStub.Create(Result, ctx, FDocPos);
    try
      initInvoke.Compile(NullResVar, Flags);
    finally
      initInvoke.Free;
    end;
  end;

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
    // Class has no 'create' method. This is only an error if arguments were provided.
    if Length(Args) > 0 then
      ctx.RaiseExceptionFmt('Class `%s` has no "create" method that accepts arguments.', [ClassTyp.Name], FDocPos);
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
  if ResType() is XType_Method then
  begin
    with XTree_FuncSelect.Create(Self.Expression, ResType(), FContext, FDocPos) do
    try
      Result := Compile(Dest, flags);
    finally
      Free();
    end;
    Exit;
  end;

  // Determine the target type from our ResType method.
  if not (ResType() is XType_Class) then
    ctx.RaiseException('Dynamic cast target must be a class type.', TargetTypeNode.FDocPos);
  TargetType := ResType() as XType_Class;

  // 1. Compile the expression being cast.
  SourceVar := Expression.Compile(NullResVar, Flags);
  if not (SourceVar.VarType is XType_Class) and not (SourceVar.VarType.BaseType = xtPointer) then
    ctx.RaiseExceptionFmt('Only class types can be dynamically cast, got %s', [SourceVar.VarType.ToString()], Expression.FDocPos);

  // 2. Determine the destination variable for the result of the cast.
  if Dest = NullResVar then
    Result := ctx.GetTempVar(TargetType)
  else
    Result := Dest;

  // 3. Emit the DYN_CAST instruction.
  // Args: [DestVar], [SourceVar], [TargetClassID]
  Self.Emit(GetInstr(icDYNCAST, [Result, SourceVar, Immediate(TargetType.ClassID)]), FDocPos);
end;

function XTree_DynCast.CompileLValue(Dest: TXprVar): TXprVar;
begin
  Result := Compile(Dest, []);
end;

function XTree_DynCast.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  Expression.DelayedCompile(Dest, Flags);
  Result := NullResVar;
end;


// ============================================================================
// Overload selection via 'as func(T): R'
// expr as func(T): R  -  picks the overload of expr whose signature matches.
//
constructor XTree_FuncSelect.Create(AExpr: XTree_Node; ATargetType: XType;
                                     ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.Expression := AExpr;
  Self.TargetType := ATargetType;
end;

function XTree_FuncSelect.ResType(): XType;
begin
  Result := TargetType;
end;

function XTree_FuncSelect.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  name:         string;
  candidates:   TXprVarList;
  cand:         TXprVar;
  candMethod:   XType_Method;
  targetMethod: XType_Method;
  i, j, implOff: Int32;
  allMatch:     Boolean;
begin
  if not (Expression is XTree_Identifier) then
    ctx.RaiseException(
      '`as func(...)` overload selection requires a bare function name on the left', FDocPos);

  name := XTree_Identifier(Expression).Name;

  if TargetType is XType_Lambda then
    targetMethod := XType_Lambda(TargetType).FieldTypes.Data[0] as XType_Method
  else if TargetType is XType_Method then
    targetMethod := XType_Method(TargetType)
  else
    ctx.RaiseException(
      '`as` overload selection: right-hand side must be a function type (func(...))', FDocPos);

  candidates := ctx.GetVarList(name);
  Result := NullResVar;

  for i := 0 to candidates.High do
  begin
    cand := candidates.Data[i];
    if not ((cand.VarType is XType_Method) or (cand.VarType is XType_Lambda)) then Continue;

    if cand.VarType is XType_Lambda then
      candMethod := XType_Lambda(cand.VarType).FieldTypes.Data[0] as XType_Method
    else
      candMethod := XType_Method(cand.VarType);

    if candMethod.RealParamcount <> targetMethod.RealParamcount then Continue;

    if ((candMethod.ReturnType = nil) <> (targetMethod.ReturnType = nil)) then Continue;
    if (candMethod.ReturnType <> nil) and
       not candMethod.ReturnType.Equals(targetMethod.ReturnType) then Continue;

    // implOff: cand may have self prepended; target type has no self
    implOff := Length(candMethod.Params) - Length(targetMethod.Params);

    allMatch := True;
    for j := 0 to High(targetMethod.Params) do
    begin
      if (j + implOff < 0) or (j + implOff > High(candMethod.Params)) then
      begin
        allMatch := False;
        Break;
      end;
      if not candMethod.Params[j + implOff].Equals(targetMethod.Params[j]) then
      begin
        allMatch := False;
        Break;
      end;
    end;
    if not allMatch then Continue;

    cand.MemPos := mpGlobal;
    Result := cand;
    Exit;
  end;

  if Result = NullResVar then
    ctx.RaiseExceptionFmt(
      'No overload of `%s` matches the signature `%s`',
      [name, TargetType.ToString()], FDocPos);
end;

function XTree_FuncSelect.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  Expression.DelayedCompile(Dest, Flags);
  Result := NullResVar;
end;

function XTree_FuncSelect.Copy(): XTree_Node;
begin
  Result := XTree_FuncSelect.Create(Expression.Copy(), TargetType, FContext, FDocPos);
  Result.FSettings := Self.FSettings;
end;


//
constructor XTree_TypeIs.Create(AExpr: XTree_Node; ATargetType: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.Expression := AExpr;
  Self.TargetTypeNode := ATargetType as XTree_Identifier;
end;

function XTree_TypeIs.ResType(): XType;
begin
  // The result of an 'is' operator is always a boolean.
  if FResType = nil then
  begin
    FResType := ctx.GetType(xtBool);
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
    Result := ctx.GetTempVar(ctx.GetType(xtBool))
  else
    Result := Dest;

  case TargetType.BaseType of
    xtClass:
      begin
        if not (SourceVar.VarType is XType_Class) and not (SourceVar.VarType.BaseType = xtPointer) then
          ctx.RaiseException('Only class types can be checked with the "is" operator against another class.', Expression.FDocPos);

        Self.Emit(GetInstr(icIS, [Result, SourceVar, Immediate(XType_Class(TargetType).ClassID)]), FDocPos);
      end;

    // Future extension that we can do with some type info (RTTI) if we wanna get fancy:
    // This may make further more complex JIT harder to achieve.
    //
    // xtArray:
    //   begin
    //     // Emit a different instruction, e.g., icIS_ARRAY
    //     Self.Emit(GetInstr(icIS_ARRAY, [Result, SourceVar]), FDocPos);
    //   end;
    //
    // xtInt8..xtUInt64: ...
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
    // This may be invalid (int := str), but assign should catch it then.
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
  ResultStub: XTree_VarStub;
begin
  finalType := Self.ResType();

  if Dest = NullResVar then
  begin
    Result := ctx.GetTempVar(finalType);
    if finalType.IsManagedType(ctx) then
      Self.Emit(GetInstr(icFILL, [Result, Immediate(finalType.Size()), Immediate(0)]), FDocPos);
  end
  else
    Result := Dest;

  boolVar := Condition.Compile(NullResVar, Flags);
  if not (boolVar.VarType.BaseType = xtBool) then
    ctx.RaiseExceptionFmt('If expression condition must be a boolean, got `%s`', [boolVar.VarType.ToString], Condition.FDocPos);

  elseJump := Self.Emit(GetInstr(icJZ, [boolVar.IfRefDeref(ctx), NullVar]), Condition.FDocPos);

  // Then branch - forward through assign for full refcount/type handling
  thenResult := ThenExpr.Compile(NullResVar, Flags);
  ResultStub := XTree_VarStub.Create(Result, ctx, ThenExpr.FDocPos);
  with XTree_Assign.Create(op_Asgn, ResultStub,
       XTree_VarStub.Create(thenResult, ctx, ThenExpr.FDocPos), ctx, ThenExpr.FDocPos) do
  try
    FSettings := ctx.CurrentSetting(Self.FSettings);
    Compile(NullResVar, Flags);
  finally
    Free;
  end;
  ResultStub.Free;

  endJump := Self.Emit(GetInstr(icRELJMP, [NullVar]), ThenExpr.FDocPos);
  ctx.PatchJump(elseJump);

  // Else branch
  elseResult := ElseExpr.Compile(NullResVar, Flags);
  ResultStub := XTree_VarStub.Create(Result, ctx, ElseExpr.FDocPos);
  with XTree_Assign.Create(op_Asgn, ResultStub,
       XTree_VarStub.Create(elseResult, ctx, ElseExpr.FDocPos), ctx, ElseExpr.FDocPos) do
  try
    FSettings := ctx.CurrentSetting(Self.FSettings);
    Compile(NullResVar, Flags);
  finally
    Free;
  end;
  ResultStub.Free;

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
// XTree_NamedArg - named/keyword argument node: ArgName := Value
//
constructor XTree_NamedArg.Create(AName: string; AValue: XTree_Node;
                                   ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.ArgName := AName;
  Self.Value   := AValue;
end;

function XTree_NamedArg.ResType(): XType;
begin
  if Value <> nil then Result := Value.ResType()
  else Result := nil;
end;

function XTree_NamedArg.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  // Should never be compiled directly - XTree_Invoke.PushArgsToStack
  // extracts named args and reorders before compiling individual values.
  if Value <> nil then Result := Value.Compile(Dest, Flags)
  else Result := NullResVar;
end;

// ============================================================================
// return statement
//
constructor XTree_Return.Create(AExpr: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);

  Self.Expr := AExpr;
end;

function XTree_Return.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  managed: TXprVarList;
  i: Int32;
  hwm: Int32;
begin
  Result := NullResVar;
  hwm := ctx.TempHighWater();

  if Self.Expr <> nil then
  begin
    with XTree_Assign.Create(op_Asgn, nil, nil, ctx, FDocPos) do
    try
      FSettings := ctx.CurrentSetting(Self.FSettings);
      Left  := XTree_Identifier.Create('result', ctx, FDocPos);
      Right := Self.Expr;
      Compile(NullResVar, Flags);
    finally
      Free();
    end;
  end;

  if not (cfNoCollect in Flags) then
  begin
    ctx.EmitAbandonedTempCleanup(hwm);
    ctx.EmitScopeCleanupTo(ctx.FunctionScopeLevel());
  end;

  if ctx.IsInsideFunction() or (cfFunctionBody in Flags) then
    Self.Emit(GetInstr(icDecTry, []), FDocPos);

  Self.Emit(GetInstr(icRET, []), FDocPos);
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
  //XXX: NOT NEEDED! WAS FOR TRUE BLOCKSCOPES
  //if not (cfNoCollect in Flags) then
  //  ctx.EmitScopeCleanupTo(ctx.LoopScopeStack.Data[ctx.LoopScopeStack.High]);

  // Emit the placeholder opcode. The parent loop's RunPatch will find and replace it.
  Self.Emit(GetInstr(icJBREAK, [NullVar]), FDocPos);
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
  //XXX: NOT NEEDED! WAS FOR TRUE BLOCKSCOPES
  //if not (cfNoCollect in Flags) then
  //  ctx.EmitScopeCleanupTo(ctx.LoopScopeStack.Data[ctx.LoopScopeStack.High]);

  // Emit the placeholder opcode. The parent loop's RunPatch will find and replace it.
  Self.Emit(GetInstr(icJCONT, [NullVar]), FDocPos);
  Result := NullResVar;
end;

// ============================================================================
// Function declaration
//
// Todo: Handle differnet types of passing parameters
//
constructor XTree_Function.Create(AName: string; AArgNames: TStringArray; ByRef: TPassArgsBy; AArgTypes: XTypeArray; ARet:XType; AProg: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);

  Name     := AName;
  ArgNames := AArgNames;
  ArgPass  := ByRef;
  ArgTypes := AArgTypes;
  RetType  := ARet;
  ProgramBlock := AProg;
  IsNested := actx.IsInsideFunction();
  SetLength(TypeName, 0);

  PreCompiled := False;
  FullyCompiled := False;
  Extra := 0;

  InternalFlags := [];

  Self.MiniCTX := nil;
end;

destructor XTree_Function.Destroy();
begin
  Self.MiniCTX.Free();
  inherited;
end;

// Note: Returns the return type a function-call, which
// may not really be what we want, that's the restype of invoke, right?
// but if we do `var a := func` what is this restype.. ugh.
//
function XTree_Function.ResType(): XType;
var
  HasUnknownRet: Boolean;
  RetName: string;
begin
  if FResType = nil then
  begin
    HasUnknownRet := (Self.RetType <> nil) and (Self.RetType.BaseType = xtUnknown);
    if HasUnknownRet then RetName := Self.RetType.Name;

    ctx.ResolveToFinalType(Self.RetType);

    if HasUnknownRet and (Self.RetType = nil) then
      ctx.RaiseExceptionFmt(eUndefinedType, [RetName], Self.FDocPos);

    FResType := Self.RetType;
  end;
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
  Result += Self.ProgramBlock.ToString(Offset + '  ') + LineEnding;
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

    // add 'self' to the formal parameter list.
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
    i,k: Int32;
    VMTIndex: Int32;
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
          if(not Self.Values.Contains('override')) then
            ctx.RaiseException('Method attempts to override virtual method implicitly', FDocPos);
          // OVERRIDE
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
      if(Self.Values.Contains('override')) then
        ctx.RaiseException('There is no method in an ancestor class to be overridden', FDocPos);

      NewEntry.MethodDef := Method;

      NewEntry.Index := 0;
      for i := 0 to ClassT.VMT.RealSize - 1 do
        for k := 0 to High(ClassT.VMT.Items[i]) do
          NewEntry.Index += ClassT.VMT.Items[i][k].val.Size;

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

  // --- Builds the arg from the current scope. ---
  procedure AddImpliedNestedArgs();
  var
    capturedNames: TStringArray;
    capturedVar: TXprVar;
    i, oldLen: Int32;
  begin
    capturedNames := ScanFunctionCaptures(Self, ctx);

    oldLen := Length(ArgTypes);
    SetLength(ArgTypes, oldLen + Length(capturedNames));
    SetLength(ArgNames, oldLen + Length(capturedNames));
    SetLength(ArgPass,  oldLen + Length(capturedNames));

    for i := 0 to High(capturedNames) do
    begin
      capturedVar := ctx.TryGetVar(capturedNames[i]);
      ArgTypes[oldLen + i] := capturedVar.VarType;
      ArgNames[oldLen + i] := capturedNames[i];

      // Methods passed by copy (they're code pointers); everything else by ref
      if capturedVar.VarType is XType_Method then
        ArgPass[oldLen + i] := pbCopy
      else
        ArgPass[oldLen + i] := pbRef;
    end;
  end;

  procedure ValidateTypes();
  var
    i: Int32;
    argTypeName: string;
  begin
    for i:=0 to High(Self.ArgTypes) do
    begin
      argTypeName := '???';
      if Self.ArgTypes[i] <> nil then
        argTypeName := Self.ArgTypes[i].Name;

      ctx.ResolveToFinalType(Self.ArgTypes[i]);
      if Self.ArgTypes[i] = nil then
        ctx.RaiseExceptionFmt('Undefined type "%s" at parameter %d', [argTypeName, i+1], Self.FDocPos);
    end;
  end;

var
  i, numRealParameters, impliedOffset, userIdx: Int32;
  defName: string;
  defVal, defCompiled: TXprVar;
begin
  if PreCompiled then
    Exit(methodVar);

  Self.ProcessAnnotations();
  if (TypeName <> '') or (SelfType <> nil) then
    AddSelf();

  if(Length(self.ProgramBlock.List) <> 0) and Self.Values.Contains('native') then
  begin
    ctx.RaiseException('Native methods cannot have a program-block', ProgramBlock.FDocPos);
  end;


  {nested method parameters - implied}
  numRealParameters := Length(Self.ArgTypes);
  if ctx.IsInsideFunction() then
  begin
    AddImpliedNestedArgs();
  end;
  {...}

  ValidateTypes();

  method := XType_Method.Create(Name, ArgTypes, ArgPass, ResType(), SelfType <> nil);
  method.ParamNames     := Self.ArgNames;
  method.IsNested       := ctx.IsInsideFunction();
  method.NestingLevel   := CTX.Scope;
  method.ClassMethod    := Self.Values.Contains('virtual') or Self.Values.Contains('override'); //cfClassMethod in Flags;
  method.RealParamcount := numRealParameters;
  method.CallingConvention := Self.CallingConvention;
  method.AccessStyle    := EMethodType.mtMethod;

  // properties need flags:
  if Self.isProperty then
  begin
    if method.ReturnType <> nil then method.AccessStyle := EMethodType.mtRead
    else                             method.AccessStyle := EMethodType.mtWrite;
  end;

  // Compile default expressions into global vars (evaluated once at definition time).
  // Store NullVar for required params (no default).
  // ArgDefaults from the parser has NO self entry; ParamDefaults[i] maps to
  // the full param array (which includes self at index 0 for type methods).
  // Use impliedOffset to bridge the two index spaces.
  impliedOffset := 0;
  if SelfType <> nil then impliedOffset := 1;

  SetLength(method.ParamDefaults, numRealParameters);
  for i := 0 to numRealParameters - 1 do
  begin
    userIdx := i - impliedOffset;  // index into user-declared ArgDefaults
    if (userIdx >= 0) and
       (userIdx < Length(Self.ArgDefaults)) and
       (Self.ArgDefaults[userIdx] <> nil) then
    begin
      defName := '__default_' + Name + '_' + ArgNames[i];
      defVal  := TXprVar.Create(Self.ArgTypes[i], 0);
      ctx.RegGlobalVar(defName, defVal, FDocPos);
      defCompiled := Self.ArgDefaults[userIdx].Compile(NullVar, []);
      ctx.Emit(GetInstr(icMOV, [defVal, defCompiled]), FDocPos, ctx.FSettings);
      method.ParamDefaults[i] := defVal;
    end
    else
      method.ParamDefaults[i] := NullVar;
  end;

  ctx.AddManagedType(method);
  ctx.ResolveToFinalType(XType(method));

  // Address is resolved after compilation as part of linking
  // this variable will be pushed to a list for late resolution.
  methodVar := TXprVar.Create(method, 0);
  ctx.RegMethod(Name, methodVar, FDocPos);

  // If this was a class method, we need to update the compile-time VMT entry
  // with the final, complete method type definition.
  if method.ClassMethod and (SelfType <> nil) then
     ctx.ResolveToFinalType(SelfType);

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
  arg, ptrVar: TXprVar;
  i, ptrIdx, allocFrame: Int32;
  CreationCTX: TMiniContext;
  SelfClass: XType_Class;
  tmpVar: TXprVar;
  tryExceptPatch: SizeInt;

  // FFI state
  isNative: Boolean;
  nativeVal: Variant;

  function FFICall(): TXprVar;
  var
    nativeStr, libName, symName: string;
    p: Int32;
    Import: PXprNativeImport;
  begin
    // nativeVal already resolved in outer scope
    nativeStr := VarToStr(nativeVal);
    p := Pos('::', nativeStr);
    if p > 0 then
    begin
      libName := System.Copy(nativeStr, 1, p - 1);
      symName := System.Copy(nativeStr, p + 2, MaxInt);
    end else
    begin
      libName := nativeStr;
      symName := Self.Name;
    end;

    if not FFILoaded() then
      ctx.RaiseException('FFI module is not loaded!');

    Import := AllocMem(SizeOf(TXprNativeImport));
    if not XprResolveImport(Import^, libName, symName,
                            XType_Method(Self.MethodVar.VarType),
                            XprCCToABI(XType_Method(Self.MethodVar.VarType).CallingConvention)) then
    begin
      FreeMem(Import);
      ctx.RaiseExceptionFmt('Could not load "%s" from "%s"',
                            [symName, libName], FDocPos);
    end;

    ctx.Intermediate.AddNativeImport(Import);

    Self.Emit(GetInstr(icFFICALL,
      [Immediate(PtrInt(Import), ctx.GetType(xtPointer)),
       Immediate(Length(ArgTypes)),
       Immediate(Ord(Self.ResType() <> nil))]),
      FDocPos);
    Self.Emit(GetInstr(icRET, []), FDocPos);

    // Patch the frame size - same as normal path
    ctx.PatchArg(allocFrame, ia1, ctx.FrameSize());
    Result := NullResVar;
  end;

begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName(), ', Name: ', name);{$ENDIF}
  if FullyCompiled then Exit(NullResVar);

  Self.PushCompilerSetting();

  isNative := Self.Values.Get('native', nativeVal);

  Flags += InternalFlags;

  if XType_Method(Self.MethodVar.VarType).ClassMethod then
  begin
    SelfClass := XType_Method(Self.MethodVar.VarType).GetClass() as XType_Class;
    ctx.PushVirtualMethod(MethodVar.Addr, SelfClass.ClassID, Self.Extra);
  end
  else
    ctx.PushFunction(MethodVar.Addr);

  Self.Emit(GetInstr(icPASS, [ctx.RegConst(Name)]), Self.FDocPos);

  ctx.PushCurrentMethod(Self.MethodVar.VarType);
  CreationCTX := ctx.GetMiniContext();
  ctx.SetMiniContext(MiniCTX);
  try
    ctx.IncScope();

    allocFrame := Self.Emit(GetInstr(icNEWFRAME, [NullVar]), Self.FDocPos);

    // FFI path exits eary - finally block will clean up
    if isNative then
    begin
      Result := FFICall();
      Exit;
    end;

    for i:=High(ArgTypes) downto 0 do
    begin
      if (ArgPass[i] = pbRef) then
      begin
        // XXX
        ptrVar := ctx.RegVar(ArgNames[i], ctx.GetType(xtPointer), Self.FDocPos, ptrIdx);
        ctx.Variables.Data[ptrIdx].Reference := True;
        ctx.Variables.Data[ptrIdx].VarType   := ArgTypes[i];
        ptrVar := ctx.Variables.Data[ptrIdx];
        Self.Emit(GetInstr(icPOPH, [ptrVar]), Self.FDocPos);
      end else
      begin
        // Note: Trigger IncRef here by using a temp, then assigning to var
        //       Instead of caller handled.
        arg := ctx.RegVar(ArgNames[i], ArgTypes[i], Self.FDocPos);
        if arg.IsManaged(ctx) then
        begin
          tmpVar := ctx.GetTempVar(arg.VarType);
          Self.Emit(GetInstr(icPOP, [Immediate(arg.VarType.Size), tmpVar]), FDocPos);

          // Assign triggers refcounting and cleanup, we disable cleanup though
          with XTree_Assign.Create(op_Asgn, nil, nil, ctx, FDocPos) do
          try
            FSettings := ctx.CurrentSetting(Self.FSettings);
            Left  := XTree_VarStub.Create(Arg, ctx, fdocpos);
            Right := XTree_VarStub.Create(tmpVar, ctx, fdocpos);
            Compile(NullResVar, [cfNoCollect]); // we dont need to collect
          finally;
            Free();
          end;
        end else
        begin
          Self.Emit(GetInstr(icPOP, [Immediate(arg.VarType.Size), arg]), FDocPos);
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
      Self.Emit(GetInstr(icPOPH, [ptrVar]), Self.FDocPos);
    end;

    tryExceptPatch := Self.Emit(GetInstr(icIncTry, [NullVar]), Self.FDocPos);

    ProgramBlock.Compile(NullResVar, Flags + [cfFunctionBody]);

    with XTree_Return.Create(nil, FContext, ctx.CurrentDocPos()) do
    try
      FSettings := ctx.CurrentSetting(Self.FSettings);
      Compile(NullResVar, Flags + [cfFunctionBody]);
    finally
      Free();
    end;

    // Exception entry - try frame already popped by VM when jumping here
    ctx.PatchArg(tryExceptPatch, ia1, ctx.CodeSize());
    if not (cfNoCollect in Flags) then
      ctx.EmitFinalizeScope(Flags);
    Self.Emit(GetInstr(icRET_RAISE, []), Self.FDocPos);

    ctx.PatchArg(allocFrame, ia1, ctx.FrameSize());
  finally
    ctx.DecScope();
    ctx.SetMiniContext(CreationCTX);
    CreationCTX.Free();
    ctx.PopCurrentMethod();
    Self.PopCompilerSetting();
    FullyCompiled := True;
  end;

  Result := NullResVar;
end;



// ============================================================================
// Generic function magic
//
constructor XTree_GenericFunction.Create(AMethod: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);

  Self.GenericFunction := AMethod as XTree_Function;
end;

function XTree_GenericFunction.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  // Register the generic template. TypeParams are already stored on GenericFunction.
  ctx.GenericMap[XprCase(Self.GenericFunction.Name)] := Self;
  Result := NullResVar;
end;

function XTree_GenericFunction.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  Result := NullResVar; (* nothing *)
end;

// var x:=Function(arg): returntype
// Function(arg): returntype
function XTree_GenericFunction.CopyMethod(ArgTypes: XTypeArray; ASelfType, ARetType: XType;
                                           ExplicitParams: XTypeArray; Docpos: TDocPos): XTree_Function;
var
  i: Int32;
  SubstMap: specialize TDictionary<string, XType>;
  TypeParams: TStringArray;

  // Is this name a legacy constraint (accepts any value, just validates)?
  function IsConstraint(const Name: string): Boolean;
  begin
    Result := (Name = 'type') or (Name = 'array') or
              (Name = 'class') or (Name = 'numeric');
  end;

  // Validate a legacy constraint against a concrete type.
  procedure CheckConstraint(const Name: string; ArgType: XType);
  var
    msg: string;
  begin
    if ArgType = nil then Exit;
    case Name of
      'array':
        if not (ArgType is XType_Array) then
          ctx.RaiseExceptionFmt(
            'Generic constraint <array>: expected an array type, got `%s`',
            [ArgType.ToString()], DocPos);
      'class':
        if not (ArgType is XType_Class) then
          ctx.RaiseExceptionFmt(
            'Generic constraint <class>: expected a class type, got `%s`',
            [ArgType.ToString()], DocPos);
      'numeric':
        begin
          // XType_String/Array/Class/Method all inherit XType_Numeric through
          // XType_Pointer - they must be explicitly excluded.
          // Only plain numeric scalar types (int, float, char, bool) are allowed.
          if (ArgType is XType_Pointer) or           // excludes array/string/class/method/pointer
             not (ArgType is XType_Numeric) then
            ctx.RaiseExceptionFmt(
              'Generic constraint <numeric>: expected a numeric scalar type, got `%s`',
              [ArgType.ToString()], DocPos);
        end;
      // 'type' accepts anything - no check needed
    end;
  end;

  // Walk Template vs ArgType to populate SubstMap.
  procedure Learn(Template, ArgType: XType);
  var
    key: string;
    existing: XType;
    TM: XType_Method;
    AM: XType_Method;
    j: Int32;
  begin
    if Template = nil then Exit;
    if ArgType  = nil then Exit;

    if Template.BaseType = xtUnknown then
    begin
      key := XprCase(Template.Name);

      if IsConstraint(key) then
      begin
        CheckConstraint(key, ArgType);
        Exit;
      end;

      // Named type param: record if not yet resolved
      if SubstMap.Get(key, existing) then
      begin
        if existing = nil then
          SubstMap[key] := ArgType;
      end;
      Exit;
    end;

    // array of T → recurse into item type
    if (Template is XType_Array) and (ArgType is XType_Array) then
    begin
      Learn(XType_Array(Template).ItemType, XType_Array(ArgType).ItemType);
      Exit;
    end;

    // func(T): U → recurse into params and return type
    // Handles both XType_Method and XType_Lambda (lambda wraps a method)
    if (Template is XType_Method) and (ArgType is XType_Method) then
    begin
      TM := XType_Method(Template);
      AM := XType_Method(ArgType);
      Learn(TM.ReturnType, AM.ReturnType);
      for j := 0 to High(TM.Params) do
        if j <= High(AM.Params) then
          Learn(TM.Params[j], AM.Params[j]);
      Exit;
    end;

    // Lambda wraps a method at field index 0
    if (Template is XType_Lambda) and (ArgType is XType_Method) then
    begin
      TM := XType_Lambda(Template).FieldTypes.Data[0] as XType_Method;
      Learn(TM, ArgType);
      Exit;
    end;
  end;

  // Resolve a template type using SubstMap.
  function Resolve(Template: XType; FallbackArg: XType): XType;
  var
    key: string;
    concrete, inner, newRet: XType;
    TM: XType_Method;
    newParams: XTypeArray;
    newPassing: TPassArgsBy;
    changed: Boolean;
    j: Int32;
    lam: XType_Lambda;
    newFN: XStringList;
    newFT: XTypeList;
  begin
    if Template = nil then Exit(FallbackArg);

    if Template.BaseType = xtUnknown then
    begin
      key := XprCase(Template.Name);

      if IsConstraint(key) then
        if FallbackArg <> nil then
          Exit(FallbackArg)
        else
          Exit(Template);

      if SubstMap.Get(key, concrete) then
      begin
        if concrete <> nil then Exit(concrete);
        // Unresolved - bind now from fallback (e.g. return type T)
        if FallbackArg <> nil then
        begin
          SubstMap[key] := FallbackArg;
          Exit(FallbackArg);
        end;
      end;

      // Not a type param, not a constraint - could be a generic type name
      // used as a self type placeholder (e.g. xtUnknown('TArray') for
      // func TArray<T>.Append). Use the concrete actual type as fallback.
      if FallbackArg <> nil then Exit(FallbackArg);

      Exit(Template); // still unresolved
    end;

    // Recurse into array of T
    if Template is XType_Array then
    begin
      if (FallbackArg is XType_Array) then
        inner := Resolve(XType_Array(Template).ItemType, XType_Array(FallbackArg).ItemType)
      else
        inner := Resolve(XType_Array(Template).ItemType, nil);

      if inner <> XType_Array(Template).ItemType then
      begin
        Result := XType_Array.Create(inner);
        ctx.AddManagedType(Result);
        Exit;
      end;
      Result := Template;
      Exit;
    end;

    // Recurse into func(T): U - substitute T and U inside method types
    if Template is XType_Method then
    begin
      TM := XType_Method(Template);
      newRet := Resolve(TM.ReturnType, nil);
      changed := newRet <> TM.ReturnType;

      SetLength(newParams,  Length(TM.Params));
      SetLength(newPassing, Length(TM.Passing));
      for j := 0 to High(TM.Params) do
      begin
        newParams[j]  := Resolve(TM.Params[j], nil);
        newPassing[j] := TM.Passing[j];
        if newParams[j] <> TM.Params[j] then changed := True;
      end;

      if changed then
      begin
        Result := XType_Method.Create(TM.Name, newParams, newPassing, newRet, TM.TypeMethod);
        XType_Method(Result).ParamNames      := TM.ParamNames;
        XType_Method(Result).RealParamcount  := TM.RealParamcount;
        XType_Method(Result).CallingConvention := TM.CallingConvention;
        ctx.AddManagedType(Result);
        Exit;
      end;
      Result := Template;
      Exit;
    end;

    // Lambda is a fat function - resolve its inner method type (field 0).
    // Fields 1+ (size: Int, args: closurearray) never contain type params.
    if Template is XType_Lambda then
    begin
      lam := XType_Lambda(Template);
      TM := lam.FieldTypes.Data[0] as XType_Method;
      inner := XType(Resolve(TM, nil));
      if inner <> XType(TM) then
      begin
        newFN.Init([]);
        newFT.Init([]);
        for j := 0 to lam.FieldTypes.High do
        begin
          newFN.Add(lam.FieldNames.Data[j]);
          if j = 0 then newFT.Add(inner)
          else          newFT.Add(lam.FieldTypes.Data[j]);
        end;
        Result := XType_Lambda.Create(newFN, newFT);
        ctx.AddManagedType(Result);
        Exit;
      end;
      Result := Template;
      Exit;
    end;

    Result := Template;
  end;


var
  callsiteArg, elemType,stillUnbound,bound,resolved: XType;
  constraint: string;
  canBind: Boolean;
  oldLen, declCount, slot: Int32;
  body: XTree_ExprList;
begin
  TypeParams := Self.GenericFunction.TypeParams;

  SubstMap := specialize TDictionary<string, XType>.Create(@HashStr);
  try
    // 1. Pre-populate named type params as unresolved (nil)
    for i := 0 to High(TypeParams) do
      SubstMap[XprCase(TypeParams[i])] := nil;

    // 2. If explicit params were supplied (Method<Int>(...)), bind them directly
    //    and validate against constraints.
    if Length(ExplicitParams) > 0 then
    begin
      if Length(ExplicitParams) <> Length(TypeParams) then
        ctx.RaiseException(
          Format('Generic `%s` expects %d type parameter(s), got %d.',
                 [Self.GenericFunction.Name, Length(TypeParams), Length(ExplicitParams)]),
          DocPos);
      for i := 0 to High(TypeParams) do
      begin
        SubstMap[XprCase(TypeParams[i])] := ExplicitParams[i];
        // Validate constraint if one was declared: <T:numeric>
        if (i <= High(Self.GenericFunction.TypeConstraints)) and
           (Self.GenericFunction.TypeConstraints[i] <> '') then
          CheckConstraint(Self.GenericFunction.TypeConstraints[i], ExplicitParams[i]);
      end;

      // For array extension methods: if a param would be inferred from the
      // element type (it's element-bound), the explicit value must match.
      // e.g. arr.QuickSort<Double> where arr: array of Int is wrong.
      if ASelfType is XType_Array then
      begin
        elemType := XType_Array(ASelfType).ItemType;
        for i := 0 to High(TypeParams) do
        begin
          constraint := '';
          if i <= High(Self.GenericFunction.TypeConstraints) then
            constraint := XprCase(Self.GenericFunction.TypeConstraints[i]);

          // Same canBind logic as step 3a - does this param bind from elem type?
          canBind :=
            (constraint = '')      or
            (constraint = 'type')  or
            ((constraint = 'numeric') and (elemType is XType_Numeric) and not (elemType is XType_Pointer)) or
            ((constraint = 'array')   and (elemType is XType_Array)) or
            ((constraint = 'class')   and (elemType is XType_Class));

          if canBind and not ExplicitParams[i].Equals(elemType) then
            ctx.RaiseExceptionFmt(
              'Explicit type `%s` for `<%s>` conflicts with the array element type `%s`. ' +
              'This parameter is bound to the element type of the array.',
              [ExplicitParams[i].ToString(), TypeParams[i], elemType.ToString()],
              DocPos);
        end;
      end;
    end
    else
    begin
      // 3. Infer: learn from self type and arguments
      if Length(Self.GenericFunction.ArgTypes) <> Length(ArgTypes) then
        ctx.RaiseException('Unmatched generic function: incorrect number of arguments.', DocPos);

      if (Self.GenericFunction.SelfType <> nil) and (ASelfType <> nil) then
        Learn(Self.GenericFunction.SelfType, ASelfType);

      for i := 0 to High(ArgTypes) do
        Learn(Self.GenericFunction.ArgTypes[i], ArgTypes[i]);

      // 3a. For array extension methods, bind any still-unresolved type params
      //     from the self array's element type.
      //     e.g. arr.QuickSort<T:numeric>() - T never appears in arg types,
      //     but the intent is T = element type of arr.
      if (ASelfType is XType_Array) then
      begin
        elemType := XType_Array(ASelfType).ItemType;
        for i := 0 to High(TypeParams) do
        begin
          if SubstMap.Get(XprCase(TypeParams[i]), stillUnbound) and (stillUnbound = nil) then
          begin
            // Verify constraint is satisfied by the element type before binding
            constraint := '';
            if i <= High(Self.GenericFunction.TypeConstraints) then
              constraint := XprCase(Self.GenericFunction.TypeConstraints[i]);
            canBind :=
              (constraint = '')        or
              (constraint = 'type')    or
              ((constraint = 'numeric') and (elemType is XType_Numeric) and not (elemType is XType_Pointer)) or
              ((constraint = 'array')   and (elemType is XType_Array))  or
              ((constraint = 'class')   and (elemType is XType_Class));
            if canBind then
              SubstMap[XprCase(TypeParams[i])] := elemType;
          end;
        end;
      end;

      // 3b. After inference, check that all named type params were resolved.
      //     Params that appear only in the return type can't be inferred.
      for i := 0 to High(TypeParams) do
      begin
        if SubstMap.Get(XprCase(TypeParams[i]), bound) and (bound = nil) then
          ctx.RaiseExceptionFmt(
            'Cannot infer type parameter `%s` for generic `%s`. ' +
            'Specify it explicitly: %s<%s>(...)',
            [TypeParams[i], Self.GenericFunction.Name,
             Self.GenericFunction.Name, TypeParams[i]],
            DocPos);
      end;

      // 3c. Validate constraints against the inferred types.
      for i := 0 to High(TypeParams) do
      begin
        if (i > High(Self.GenericFunction.TypeConstraints)) then Break;
        if Self.GenericFunction.TypeConstraints[i] = '' then Continue;
        if SubstMap.Get(XprCase(TypeParams[i]), bound) and (bound <> nil) then
          CheckConstraint(Self.GenericFunction.TypeConstraints[i], bound);
      end;
    end;

    // 4. Clone the AST
    Result := XTree_Function.Create(
      Self.GenericFunction.Name,
      Self.GenericFunction.ArgNames,
      Self.GenericFunction.ArgPass,
      [],
      Self.GenericFunction.RetType,
      Self.GenericFunction.ProgramBlock.Copy() as XTree_ExprList,
      FContext,
      FDocPos
    );
    Result.SingleExpression := Self.GenericFunction.SingleExpression;
    Result.Annotations      := Self.GenericFunction.Annotations;
    Result.FSettings        := Self.GenericFunction.FSettings;
    Result.TypeParams       := TypeParams;
    Result.TypeConstraints  := Self.GenericFunction.TypeConstraints;

    // 4b. Prepend `type T = Int8` declarations for every resolved type param.
    //     XTree_TypeDecl.Compile calls ctx.AddType, so T becomes a real type
    //     for the duration of this specialization's body compilation.
    //     This handles `var tmp: T`, casts, etc. with zero changes to DelayedCompile.
    declCount := 0;
    for i := 0 to High(TypeParams) do
    begin
      if SubstMap.Get(XprCase(TypeParams[i]), resolved) and (resolved <> nil) then
        Inc(declCount);
    end;

    if declCount > 0 then
    begin
      body := Result.ProgramBlock;
      OldLen := Length(body.List);
      SetLength(body.List, oldLen + declCount);
      // Shift existing statements right to make room at the front
      if oldLen > 0 then
        Move(body.List[0], body.List[declCount], oldLen * SizeOf(XTree_Node));
      // Insert one XTree_TypeDecl per resolved param at the front
      slot := 0;
      for i := 0 to High(TypeParams) do
      begin
        if SubstMap.Get(XprCase(TypeParams[i]), resolved) and (resolved <> nil) then
        begin
          body.List[slot] := XTree_TypeDecl.Create(
            TypeParams[i], resolved, FContext, FDocPos);
          Inc(slot);
        end;
      end;
    end;

    // 5. Resolve SelfType
    Result.SelfType := nil;
    if Self.GenericFunction.SelfType <> nil then
      Result.SelfType := Resolve(Self.GenericFunction.SelfType, ASelfType);

    // 6. Resolve argument types.
    //    Iterate over the TEMPLATE's arg count, not the callsite ArgTypes length.
    //    When called from specialize(...) ArgTypes is empty, but SubstMap already
    //    has all bindings from ExplicitParams
    SetLength(Result.ArgTypes, Length(Self.GenericFunction.ArgTypes));
    for i := 0 to High(Self.GenericFunction.ArgTypes) do
    begin
      callsiteArg := nil;
      if i <= High(ArgTypes) then callsiteArg := ArgTypes[i];
      Result.ArgTypes[i] := Resolve(Self.GenericFunction.ArgTypes[i], callsiteArg);
    end;

    // 7. Resolve return type
    if Self.GenericFunction.RetType <> nil then
    begin
      Result.RetType := Resolve(Self.GenericFunction.RetType, ARetType);
      if Result.RetType = nil then
        ctx.RaiseException(
          'Could not resolve return type for generic `' + Self.GenericFunction.Name + '`', DocPos);
    end;

  finally
    SubstMap.Free;
  end;
end;


// ============================================================================
// specialize FuncName<ConcreteType1, ...>
// In expression: forces specialization of a generic function, returns method ptr.
// In type position: type TA = specialize TMap<Int, String>
//
constructor XTree_Specialize.Create(AFuncName: string; ATypes: XTypeArray;
                                     ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.FuncName             := AFuncName;
  Self.ExplicitTypes        := ATypes;
  Self.IsTypeSpecialization := False;
  Self.ConcreteTypeName     := '';
end;

constructor XTree_Specialize.CreateTypeSpec(AConcreteTypeName, ASourceName: string;
                                             ATypes: XTypeArray;
                                             ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.FuncName             := ASourceName;
  Self.ExplicitTypes        := ATypes;
  Self.IsTypeSpecialization := True;
  Self.ConcreteTypeName     := AConcreteTypeName;
end;

function XTree_Specialize.ResType(): XType;
var
  MethodVar: TXprVar;
begin
  if IsTypeSpecialization then
  begin
    // Return the concrete class/type produced (or already cached)
    Result := ctx.SpecializeType(FuncName, ConcreteTypeName, ExplicitTypes, FDocPos);
    Exit;
  end;

  MethodVar := ctx.ResolveMethod(FuncName, [], nil, nil, ExplicitTypes, FDocPos);
  if MethodVar <> NullResVar then
    Result := MethodVar.VarType
  else
    Result := nil;
end;

function XTree_Specialize.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  MethodVar:    TXprVar;
  ConcreteType: XType;
  mangledName:  string;
  i:            Int32;
  template:     XTree_Node;
begin
  if IsTypeSpecialization then
  begin
    // Specialise the generic type and register it under ConcreteTypeName.
    ConcreteType := ctx.SpecializeType(FuncName, ConcreteTypeName, ExplicitTypes, FDocPos);

    if ConcreteType = nil then
      ctx.RaiseExceptionFmt(
        'specialize: could not specialize generic type `%s`', [FuncName], FDocPos);
    Exit(NullResVar);
  end;

  // Function specialization: try ResolveMethod first.
  MethodVar := ctx.ResolveMethod(FuncName, [], nil, nil, ExplicitTypes, FDocPos);
  if MethodVar <> NullResVar then
  begin
    Result := MethodVar;
    Exit;
  end;

  // Not a generic function - try as a generic TYPE (standalone pre-instantiation).
  // e.g. `specialize TBox<Bool>` in statement position to ensure the type exists.
  // Build a mangled name: TBox_Bool, TMap_Int_String, etc.
  if ctx.GenericTypeMap.Contains(XprCase(FuncName)) then
  begin
    mangledName := FuncName;
    for i := 0 to High(ExplicitTypes) do
      mangledName += '_' + ExplicitTypes[i].Hash();

    ctx.SpecializeType(FuncName, mangledName, ExplicitTypes, FDocPos);
    Exit(NullResVar);
  end;

  ctx.RaiseExceptionFmt(
    'specialize: `%s` is neither a generic function nor a generic type', [FuncName], FDocPos);
  Result := NullResVar;
end;

function XTree_Specialize.Copy(): XTree_Node;
var
  clone: XTree_Specialize;
begin
  if IsTypeSpecialization then
    clone := XTree_Specialize.CreateTypeSpec(
               ConcreteTypeName, FuncName, ExplicitTypes, FContext, FDocPos)
  else
    clone := XTree_Specialize.Create(FuncName, ExplicitTypes, FContext, FDocPos);
  clone.FSettings := Self.FSettings;
  Result := clone;
end;


// ============================================================================
// Lambda / anonymous functions
//
constructor XTree_ClosureFunction.Create(AMethod: XTree_Function; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);

  Self.ClosureFunction := AMethod;
end;

function XTree_ClosureFunction.ResType(): XType;
var
  FieldNames: XStringList;
  FieldTypes: XTypeList;
  method: XType_Method;
  i: Int32;
begin
  if Self.FResType = nil then
  begin
    // 1. Get the underlying function's signature.
    // XXX: This is sketchy but should hopefully not cause compilation in the wrong location
    // FIX
    method := (Self.ClosureFunction as XTree_Function).Compile(NullResVar, []).VarType as XType_Method;

    // 2. Build the field list for the closure record.
    FieldNames.Init([]);
    FieldTypes.Init([]);

    // Field 0 is always the method pointer.
    FieldNames.Add('method');
    FieldTypes.Add(method);

    FieldNames.Add('size');
    FieldTypes.Add(ctx.GetType(xtInt));

    FieldNames.Add('args');
    FieldTypes.Add(ctx.GetType('!closurearray'));

    // 3. Create and return the new anonymous record type.
    FResType := XType_Lambda.Create(FieldNames, FieldTypes);
    ctx.AddManagedType(FResType);
  end;

  Result := FResType;
end;

function XTree_ClosureFunction.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  MethodPtrVar, closure: TXprVar;
  closure_node: XTree_VarStub;
  method: XType_Method;
  argName: string;
  i, NumCaptured: Int32;
  ArgsFieldNode, SizeFieldNode: XTree_Field;
  SetLenCall: XTree_Invoke;

begin
  closure := ctx.GetTempVar(Self.ResType());
  Self.Emit(GetInstr(icFILL, [closure, Immediate(closure.VarType.Size()), Immediate(0)]), FDocPos);
  closure_node := XTree_VarStub.Create(closure, FContext, FDocPos);

  // write the function into the closurenode
  MethodPtrVar := Self.ClosureFunction.Compile(NullResVar, Flags);
  method := XType_Method(MethodPtrVar.VarType);

  // First arg is always the ptr.
  MethodPtrVar.MemPos := mpGlobal;
  Self.Emit(GetInstr(icCOPY_GLOBAL, [closure, MethodPtrVar]), FDocPos);

  // second arg is always the number of hidden elements (size)
  // for dynamic invoke
  with XTree_Assign.Create(op_Asgn, nil, nil, Ctx, Fdocpos) do
  try
    FSettings := ctx.CurrentSetting(Self.FSettings);
    Left  := XTree_Field.Create(closure_node, XTree_Identifier.Create('size', FContext, FDocPos), FContext, FDocPos);
    Right := XTree_Int.Create(IntToStr(Length(Method.Params)-Method.RealParamcount), FContext, FDocPos);

    Compile(NullResVar, Flags);
  finally
    Free();
  end;

  // lift the varpointers into the record field named args
  // This is an array
  // 1) SetLen(__size)
  // 2) Add args
  NumCaptured := Length(Method.Params)-Method.RealParamcount;
  if NumCaptured > 0 then
  begin
    // A. Get a node representing the '__args' field itself.
    ArgsFieldNode := XTree_Field.Create(closure_node, XTree_Identifier.Create('args', FContext, FDocPos), FContext, FDocPos);
    SizeFieldNode := XTree_Field.Create(closure_node, XTree_Identifier.Create('size', FContext, FDocPos), FContext, FDocPos);

    // B. Generate code to set the length of the array: closure.__args.SetLen(size)
    SetLenCall := XTree_Invoke.Create(
      XTree_Identifier.Create('SetLen', FContext, FDocPos),
      [XTree_VarStub.Create(SizeFieldNode.Compile(NullResVar, Flags), ctx, FDocPos)], // Pass the __size field as the argument
      ctx, FDocPos
    );
    SetLenCall.SelfExpr := ArgsFieldNode; // The array we are calling SetLen on
    try
      SetLenCall.Compile(NullResVar, Flags);
    finally
      SetLenCall.Free;
    end;

    // C. Loop through the captured variables and assign their addresses to the array elements.
    for i := 0 to NumCaptured - 1 do
    begin
      argName := method.ParamNames[method.RealParamcount + i];

      // Create and compile the assignment.
      with XTree_Assign.Create(op_Asgn, nil, nil, ctx, FDocPos) do
      try
        FSettings := ctx.CurrentSetting(Self.FSettings);
        Left  := XTree_Index.Create(ArgsFieldNode, XTree_Int.Create(IntToStr(i), FContext, FDocPos), FContext, FDocPos);
        Right := XTree_UnaryOp.Create(op_ADDR, XTree_Identifier.Create(argName, FContext, FDocPos), FContext, FDocPos);

        Compile(NullResVar, Flags);
      finally
        Free;
      end;
    end;
  end;

  Result := closure;
end;

function XTree_ClosureFunction.CompileLValue(Dest: TXprVar): TXprVar;
begin
  Result := Self.Compile(Dest, []);
end;

function XTree_ClosureFunction.Copy(): XTree_Node;
begin
  Result := XTree_ClosureFunction.Create(
    Self.ClosureFunction.Copy() as XTree_Function, FContext, FDocPos);
  Result.FSettings := Self.FSettings;
end;

// ============================================================================
// Resolve (record, class) field-lookups
//
constructor XTree_Field.Create(ALeft, ARight: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);

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
  EnumType: XType;
  FunctionName: string;
  PropMethod: TXprVar;
begin
  if (Self.FResType <> nil) then
    Exit(inherited);

  // -- Enum member access: EColor.Red -----------------------------------------
  if (Self.Left is XTree_Identifier) and (Self.Right is XTree_Identifier) then
  begin
    EnumType := ctx.GetType(XTree_Identifier(Self.Left).Name);
    if EnumType is XType_Enum then
    begin
      FResType := EnumType;
      Exit(inherited);
    end;
  end;

  // This handles 'myRecord.field' or 'myRecord.Method()'.
  if Self.Right is XTree_Identifier then
  begin
    FunctionName := XTree_Identifier(Self.Right).Name;

    // 1. Try to resolve as a Property (Read) method first
    if Self.Left.ResType() <> nil then
    begin
      PropMethod := ctx.ResolveMethod(FunctionName, [], Self.Left.ResType(), nil, [], FDocPos);
      if (PropMethod <> NullResVar) and (PropMethod.VarType is XType_Method) and
         (XType_Method(PropMethod.VarType).AccessStyle = mtRead) then
      begin
        FResType := XType_Method(PropMethod.VarType).ReturnType;
        Exit(inherited);
      end;
    end;

    // 2. Fallback to normal field access
    if (Self.Left.ResType() is XType_Record) then
      FResType := XType_Record(Self.Left.ResType()).FieldType(FunctionName)
    else if (Self.Left.ResType() is XType_Class) then
      FResType := XType_Class(Self.Left.ResType()).FieldType(FunctionName)
    else
      ctx.RaiseExceptionFmt('Cannot access fields on type `%s`', [Self.Left.ResType().ToString], Self.Left.FDocPos);

    if FResType = nil then
      ctx.RaiseExceptionFmt('Unrecognized field or property `%s` on type `%s`', [FunctionName, Self.Left.ResType().ToString()], Self.Right.FDocPos);
  end
  else if Self.Right is XTree_Invoke then
  begin
    Invoke := XTree_Invoke(Self.Right);
    Invoke.SelfExpr := Left;
    FResType := Invoke.ResType();
  end
  else
  begin
    WriteFancy(Right.ToString());
    ctx.RaiseException('Unsupported right side in field access expression', FDocPos);
  end;

  Result := inherited;
end;


function XTree_Field.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  Offset: PtrInt;
  leftVar, objectPtr, PropMethod: TXprVar;
  Field: XTree_Identifier;
  invoke: XTree_Invoke;
  EnumType: XType;
  MemberVal: Int64;
begin
  Result := NullResVar;

  // -- Enum member access: EColor.Red -> named constant -----------
  if (Self.Left is XTree_Identifier) and (Self.Right is XTree_Identifier) then
  begin
    EnumType := ctx.GetType(XTree_Identifier(Self.Left).Name);
    if EnumType is XType_Enum then
    begin
      Field := Self.Right as XTree_Identifier;
      if not XType_Enum(EnumType).MemberValue(Field.Name, MemberVal) then
        ctx.RaiseExceptionFmt('`%s` is not a member of enum `%s`', [Field.Name, EnumType.Name], Field.FDocPos);

      // Emit as a constant of the storage integer type so the bytecode uses
      // the correct slot width (1, 2, or 4 bytes).
      Result := ctx.RegConst(Constant(MemberVal, XType_Enum(EnumType).BaseType));
      Result.VarType := EnumType;
      Exit;
    end;
  end;

  Right.SetResTypeHint(Self.FResTypeHint); // pass on to whatever is right

  // --- PATH 2: Fallback to compiling as record/class member access ---
  if (Self.Right is XTree_Identifier) then
  begin
    Field := Right as XTree_Identifier;

    // 1. Check for Property (Read) Access
    if Self.Left.ResType() <> nil then
    begin
      PropMethod := ctx.ResolveMethod(Field.Name, [], Self.Left.ResType(), nil, [], FDocPos);
      if (PropMethod <> NullResVar) and (PropMethod.VarType is XType_Method) and
         (XType_Method(PropMethod.VarType).AccessStyle = mtRead) then
      begin
        with XTree_Invoke.Create(Self.Right, [], FContext, FDocPos) do
        try
          PropertyAccess := True;
          SelfExpr := Self.Left;
          Result := Compile(Dest, Flags);
        finally
          Free;
        end;
        Exit;
      end;
    end;

    // --- CLASS FIELD ACCESS ---
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
      Self.Emit(GetInstr(icADD, [LeftVar, ctx.RegConst(Offset), objectPtr]), Self.FDocPos);
      Result := ctx.GetTempVar(Self.ResType());
      Self.Emit(GetInstr(icDREF, [Result, objectPtr, Immediate(Result.VarType.Size)]), FDocPos);

      // Solves classes with array refcounting
      if Result.VarType.IsManagedType(ctx) then
      begin
        Result.IsBorrowedRef := True;
        ctx.Variables.Data[ctx.Variables.High].IsBorrowedRef := True;
      end;
    end
    // --- RECORD FIELD ACCESS ---
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
        Self.Emit(GetInstr(icADD, [LeftVar, ctx.RegConst(Offset), objectPtr]), Self.FDocPos);
        Result := objectPtr;
        Result.VarType := Self.ResType();
      end else
      begin
        Result := LeftVar;
        Result.Addr += Offset;
        Result.VarType := Self.ResType();
      end;

      Result.IsTemporary := LeftVar.IsTemporary;

      if Result.VarType.IsManagedType(ctx) then
      begin
        Result.IsBorrowedRef := True;
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
    WriteFancy(Right.ToString());
    ctx.RaiseException('Unsupported right side in field access expression', FDocPos);
  end;
end;

function XTree_Field.CompileLValue(Dest: TXprVar): TXprVar;
var
  Offset: PtrInt;
  Field: XTree_Identifier;
  LocalVar, LeftVar, objectPtr: TXprVar;
begin
  // If it wasn't a namespace lookup, proceed with the record L-Value logic.
  if (Self.Left is XTree_Identifier) and (Self.Right is XTree_Identifier) then
    if ctx.GetType(XTree_Identifier(Self.Left).Name) is XType_Enum then
      Exit(NullResVar);

  if Self.Right is XTree_Identifier then
  begin
    // --- CLASS FIELD ACCESS ---
    if (Self.Left.ResType() is XType_Class) then
    begin
      Field   := Right as XTree_Identifier;
      Offset  := XType_Class(Self.Left.ResType()).FieldOffset(Field.Name);
      if Offset = -1 then
        ctx.RaiseExceptionFmt('Unrecognized fieldname `%s` in class `%s`', [Field.Name, Self.Left.ResType().ToString()], Field.FDocPos);

      if (not XType_Class(Self.Left.ResType()).IsWritable(Field.Name)) then
      begin
        if (ctx.GetCurrentMethod() <> nil) and
           ( (IsSelf(Left) and IsConstructor(ctx.GetCurrentMethod())) or
             (XprCase(ctx.GetCurrentMethod().Name) = 'default') ) then
        begin
          // nothing - allow writing in constructor
        end
        else
          Exit(NullResVar); // Signal it's not a valid LValue
      end;

      // 1. Compile the 'Left' side to get the variable holding the object pointer.
      leftVar := Self.Left.CompileLValue(NullResVar);
      leftVar.VarType := ctx.GetType(xtPointer);
      LocalVar := leftVar.IfRefDeref(ctx);

      // 2. DEREFERENCE the object pointer to get the actual address of the object on the heap.
      // We need a temporary variable to hold this heap address.
      objectPtr := ctx.GetTempVar(ctx.GetType(xtPointer));
      Self.Emit(GetInstr(icADD, [LocalVar, ctx.RegConst(Offset), objectPtr]), Self.FDocPos);

      Result := objectPtr;
      Result.VarType   := Self.ResType();
      Result.Reference := True;
      Result.IsTemporary:=LeftVar.IsTemporary;
    end
    // --- RECORD FIELD ACCESS ---
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
        Self.Emit(GetInstr(icADD, [LeftVar, ctx.RegConst(Offset), objectPtr]), Self.FDocPos);
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
  end
  else if Self.Right is XTree_Invoke then
  begin
    // Method call result - not directly addressable.
    // Return NullResVar so PushArgsToStack falls back to the spill path,
    // which will call Compile (not CompileLValue) and get the value correctly.
    Result := NullResVar;
  end
  else
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
  inherited Create(ACTX, DocPos);

  Method := AFunc;
  Args   := ArgList;
  SelfExpr := nil;
  SpecializeResType := '';
  PropertyAccess := False;
end;

function XTree_Invoke.ResolveMethod(out Func: TXprVar; out FuncType: XType): Boolean;
var
  i: Int32;
  arguments: XTypeArray;
  rettype, fieldType, selfResType, innerMethod: XType;
  methType: XType;
  inner: XType;
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
      Arguments[i] := nil   // positional pass/default - treated as wildcard
    else if Args[i] is XTree_NamedArg then
    begin
      // Named args contribute nil type here; reordering happens at Compile time.
      // We leave slot as nil so overload scoring treats it as compatible-with-default.
      Arguments[i] := nil;
    end
    else
      Arguments[i] := Self.Args[i].ResType();
  end;

  if not(Self.Method is XTree_Identifier) then
  begin
    // Non-identifier method: e.g. (Decr as func(Double): Double)(z)
    // Method is an expression that evaluates to a callable.
    // Resolve FuncType from ResType() and leave Func = NullResVar
    // Same signal used for field-stored lambdas; Compile handles it.
    methType := Self.Method.ResType();
    if methType = nil then
      ctx.RaiseException('Cannot determine type of method expression', Self.Method.FDocPos);

    if methType is XType_Lambda then
    begin
      inner := XType_Lambda(methType).FieldType('method');
      if not (inner is XType_Method) then
        ctx.RaiseException('Lambda field ''method'' is not an XType_Method', FDocPos);
      FuncType := inner as XType_Method;
    end
    else if methType is XType_Method then
      FuncType := XType_Method(methType)
    else
      ctx.RaiseException('Method expression does not resolve to a callable type', Self.Method.FDocPos);

    Func   := NullResVar;
    Result := True;
    Exit;
  end;

  if SelfExpr <> nil then
  begin
    if SelfExpr.ResType = nil then
      ctx.RaiseException('Self expression has no type', SelfExpr.FDocPos);

    Func := ctx.ResolveMethod(XTree_Identifier(Method).Name, Arguments, SelfExpr.ResType(), rettype, ExplicitTypeParams, SelfExpr.FDocPos);
  end
  else
    Func := ctx.ResolveMethod(XTree_Identifier(Method).Name, Arguments, nil, rettype, ExplicitTypeParams, Self.FDocPos);

  // fallback for field stored function pointers
  if (Func = NullResVar) and (SelfExpr <> nil) and
     (Method is XTree_Identifier) then
  begin
    fieldType := nil;
    selfResType := SelfExpr.ResType();

    if selfResType is XType_Record then
      fieldType := XType_Record(selfResType).FieldType(XTree_Identifier(Method).Name)
    else if selfResType is XType_Class then
      fieldType := XType_Class(selfResType).FieldType(XTree_Identifier(Method).Name);

    if (fieldType <> nil) and
       ((fieldType is XType_Method) or (fieldType is XType_Lambda)) then
    begin
      // Set FuncType so ResType() and Compile() know what to expect
      // Leave Func = NullResVar as signal for Compile to do field access
      if fieldType is XType_Lambda then
      begin
        innerMethod := XType_Lambda(fieldType).FieldType('method');
        if not (innerMethod is XType_Method) then
          ctx.RaiseException('Lambda field ''method'' is not an XType_Method', FDocPos);
        FuncType := innerMethod as XType_Method;
      end
      else
        FuncType := fieldType as XType_Method;

      Result := True;
      Exit;
    end;
  end;

  if Func.VarType is XType_Lambda then
    FuncType := XType_Lambda(Func.VarType).FieldType('method') as XType_Method
  else
    FuncType := Func.VarType;

  Result := FuncType <> nil;
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
    begin
      if funcType is XType_Method then
        FResType := XType_Method(funcType).ReturnType
      else if funcType is XType_Lambda then
        FResType := (XType_Lambda(funcType).FieldType('method') as XType_Method).ReturnType
      else
        ctx.RaiseException('Resolved function has unexpected type', FDocPos);
    end else
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
        XTree_Invoke(MagicNode).FResType := nil;
        XTree_Invoke(MagicNode).FResTypeHint := nil;
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
    i, k, paramIndex, impliedArgs, positionalSlot, namedIdx: Int32;
    initialArg, finalArg, tempVal, wrapper, methodField, codeIdx, funcArg: TXprVar;
    expectedType: XType;
    resolvedArgs: XNodeArray;
    namedArg: XTree_NamedArg;
  begin
    SelfVar     := NullVar;
    impliedArgs := 0;

    // ── self ──────────────────────────────────────────────────────────────
    if SelfExpr <> nil then
    begin
      impliedArgs := 1;
      SelfVar := SelfExpr.CompileLValue(NullVar);
      if SelfVar = NullResVar then
      begin
        tempVal := SelfExpr.Compile(NullResVar, Flags);
        tempVal := tempVal.IfRefDeref(ctx);
        with XTree_VarStub.Create(tempVal, ctx, SelfExpr.FDocPos) do
        try
          SelfVar := CompileLValue(NullResVar);
        finally
          Free;
        end;
      end;
      if SelfVar.Reference then Self.Emit(GetInstr(icPUSHREF, [SelfVar]), FDocPos)
      else                      Self.Emit(GetInstr(icPUSH,    [SelfVar]), FDocPos);
    end;

    // ── Pass 1: build resolvedArgs[0..RealParamcount-1] ───────────────────
    SetLength(resolvedArgs, FuncType.RealParamcount - impliedArgs);
    for k := 0 to High(resolvedArgs) do resolvedArgs[k] := nil;

    positionalSlot := 0;
    for k := 0 to High(Args) do
    begin
      if (Args[k] <> nil) and (Args[k] is XTree_NamedArg) then
      begin
        namedArg := XTree_NamedArg(Args[k]);
        namedIdx := -1;
        for i := 0 to FuncType.RealParamcount - 1 do
          if XprCase(FuncType.ParamNames[i + impliedArgs]) = XprCase(namedArg.ArgName) then
          begin
            namedIdx := i;
            Break;
          end;
        if namedIdx < 0 then
          ctx.RaiseExceptionFmt('Unknown parameter `%s`', [namedArg.ArgName], FDocPos);
        // Store the namedArg node itself as sentinel; unwrapped in pass 2
        resolvedArgs[namedIdx] := namedArg;
      end
      else
      begin
        if positionalSlot < FuncType.RealParamcount then
        begin
          resolvedArgs[positionalSlot] := Args[k]; // nil = use default
          Inc(positionalSlot);
        end;
      end;
    end;

    // ── Pass 2: unwrap named-arg sentinels ────────────────────────────────
    for k := 0 to High(resolvedArgs) do
      if (resolvedArgs[k] <> nil) and (resolvedArgs[k] is XTree_NamedArg) then
        resolvedArgs[k] := XTree_NamedArg(resolvedArgs[k]).Value; // may be nil=pass

    // ── Pass 3: fill nil slots from ParamDefaults ─────────────────────────
    for k := 0 to High(resolvedArgs) do
    begin
      if resolvedArgs[k] = nil then
      begin
        if (k < Length(FuncType.ParamDefaults)) and
           (FuncType.ParamDefaults[k] <> NullVar) then
          resolvedArgs[k] := XTree_VarStub.Create(FuncType.ParamDefaults[k], ctx, FDocPos)
        else
          ctx.RaiseExceptionFmt('Required parameter `%s` not provided',
            [FuncType.ParamNames[k + impliedArgs]], FDocPos);
      end;
    end;

    // ── Push resolved args ────────────────────────────────────────────────
    for i := 0 to High(resolvedArgs) do
    begin
      paramIndex   := i + impliedArgs;
      initialArg   := resolvedArgs[i].Compile(NullVar, Flags);
      if initialArg = NullResVar then
        ctx.RaiseExceptionFmt('Argument %d compiled to NullResVar', [i], FDocPos);
      finalArg     := initialArg;
      expectedType := FuncType.Params[paramIndex];

      // Auto-wrap func → lambda if needed
      if (expectedType is XType_Lambda) and
         (initialArg.VarType is XType_Method) and
         not (initialArg.VarType is XType_Lambda) then
      begin
        wrapper := ctx.GetTempVar(expectedType);
        Self.Emit(GetInstr(icFILL, [wrapper,
          Immediate(wrapper.VarType.Size()), Immediate(0)]), FDocPos);
        funcArg := initialArg.IfRefDeref(ctx);
        methodField := wrapper;
        methodField.VarType := ctx.GetType(xtPointer);
        codeIdx := funcArg;
        codeIdx.VarType := ctx.GetType(xtPointer);
        Self.Emit(GetInstr(icMOV, [methodField, codeIdx, Immediate(SizeOf(Pointer))]), FDocPos);
        finalArg := wrapper;
      end
      else if FuncType.Passing[paramIndex] = pbCopy then
        finalArg := ctx.EmitUpcastIfNeeded(initialArg, expectedType, True);

      if finalArg.Reference then Self.Emit(GetInstr(icPUSHREF, [finalArg]), FDocPos)
      else                        Self.Emit(GetInstr(icPUSH,    [finalArg]), FDocPos);
    end;

    if not (func.VarType is XType_Lambda) then
    begin
      // implicit captured args
      for i := FuncType.RealParamcount to High(FuncType.Params) do
      begin
        with XTree_Identifier.Create(FuncType.ParamNames[i], FContext, FDocPos) do
        try
          FSettings := ctx.CurrentSetting(Self.FSettings);
          finalArg := Compile(NullResVar, Flags);
        finally
          Free();
        end;
        if finalArg.Reference then Self.Emit(GetInstr(icPUSHREF, [finalArg]), FDocPos)
        else                        Self.Emit(GetInstr(icPUSH,    [finalArg]), FDocPos);
      end;
    end else
    begin
      if Func <> NullResVar then
        Self.Emit(GetInstr(icPUSH_CLOSURE, [Func]), FDocPos)
      else
        Self.Emit(GetInstr(icPUSH_CLOSURE, [Self.Method.Compile(NullResVar, Flags)]), FDocPos);
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
        FSettings := ctx.CurrentSetting(Self.FSettings);
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
  debug_name_id: TXprVar;
  err_str: string;
  i: Int32;
  Import: PXprNativeImport;
  paramOffset, expectedArgs: Int32;
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

    // should we deny access?
    if (FuncType <> nil) and (FuncType.AccessStyle in [mtRead, mtWrite]) and (not PropertyAccess) then
      ctx.RaiseExceptionFmt('Property "%s" cannot be invoked', [XTree_Identifier(Self.Method).Name], FDocPos);

    // --- Field stored function pointer
    // --- reroute
    if (Func = NullResVar) and (FuncType <> nil) and
       (SelfExpr <> nil) and (Method is XTree_Identifier) then
    begin
      with XTree_Field.Create(SelfExpr, Method, ctx, FDocPos) do
      try
        FSettings := ctx.CurrentSetting(Self.FSettings);
        Func := Compile(NullResVar, Flags);
      finally
        Free;
      end;

      SelfExpr := nil;
    end;

    // class destructor handling
    // handled here as just another call (which it usually is)
    FreeingInstance := (SelfExpr <> nil) and (SelfExpr.ResType() is XType_Class) and
                       IsDestructor(Method);

    if (Func = NullResVar) and FreeingInstance then
    begin
      SelfVar := SelfExpr.CompileLValue(NullVar);
      //Self.Emit(GetInstr(icRELEASE, [SelfVar.IfRefDeref(ctx)]), FDocPos);
      Exit;
      (* nothing special - collect handles it *)
    end;
  end else
  begin
    Func := Method.Compile(NullVar, Flags);

    if Func.VarType is XType_Lambda then
      FuncType := XType_Lambda(Func.VarType).FieldType('method') as XType_Method
    else if Func.VarType is XType_Method then
      FuncType := XType_Method(Func.VarType)
    else
      FuncType := nil;

    // Validate argument count and types for expression-based calls (e.g. FuncSelect, Inherited).
    if FuncType <> nil then
    begin
      paramOffset := 0;
      if SelfExpr <> nil then paramOffset := 1;
      expectedArgs := FuncType.RealParamcount - paramOffset;

      if Length(Args) <> expectedArgs then
        ctx.RaiseExceptionFmt('Expected %d argument(s), got %d',
          [expectedArgs, Length(Args)], FDocPos);

      for i := 0 to High(Args) do
        if (Args[i] <> nil) and
           not FuncType.Params[i + paramOffset].CanAssign(Args[i].ResType()) then
          ctx.RaiseExceptionFmt('Cannot pass `%s` as `%s` (argument %d)',
            [Args[i].ResType().ToString(), FuncType.Params[i + paramOffset].ToString(), i+1], FDocPos);
    end;
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
      XTree_Invoke(MagicNode).FResType := nil;
      XTree_Invoke(MagicNode).FResTypeHint := nil;

      Result := XTree_Invoke(MagicNode).Compile(Dest, Flags);
      XTree_Invoke(MagicNode).FContext := nil; // dont hold!
      Exit;
    end;
  end;

  if Func = NullResVar then
  begin
    err_str := Format('Function not matched `%s`', [XTree_Identifier(Method).name]);
    if High(Self.Args) >= 0 then
      err_str += ' for arguments: ';
    for i:=0 to High(Self.Args) do
    begin
      err_str += Self.Args[i].ResType().ToString();
      if i <> High(Self.Args) then err_str += ', ';
    end;

    ctx.RaiseException(err_str, FDocPos);
  end;


  if not((func.VarType is XType_Method) or (func.VarType is XType_Lambda)) then
    ctx.RaiseException('Cannot invoke an identifier that is not a function', FDocPos);


  if (func.VarType is XType_Lambda) and (XType(FuncType) is XType_Lambda) then
    FuncType := XType_Lambda(XType(FuncType)).FieldType('method') as XType_Method;

  // res, stackptr, args
  if (FuncType.ReturnType <> nil) then
  begin
    Result := Dest;
    if Dest = NullResVar then
      Result := ctx.GetTempVar(FuncType.ReturnType);

    if Result.VarType.IsManagedType(ctx) then
      Self.Emit(GetInstr(icFILL, [Result, Immediate(Result.VarType.Size()), Immediate(0)]), FDocPos)
    else
      ctx.VarToDefault(Result);

    Self.Emit(GetInstr(icPUSH, [Result]), FDocPos);
  end;

  PushArgsToStack();

  totalSlots := Length(Args);
  if (FuncType.ReturnType <> nil) then
    Inc(totalSlots);
  if SelfExpr <> nil then
    Inc(totalSlots);

  if (FuncType.BaseType = xtExternalMethod) then
  begin
    // Dynamic FFI path.
    // PushArgsToStack() already did the right thing:
    // Now push the function pointer itself last, so the interpreter
    // can pop it before dispatching to XprCallImport.
    Self.Emit(GetInstr(icPUSH, [Func]), FDocPos);

    Import := AllocMem(SizeOf(TXprNativeImport));
    if not XprBuildImportCIF(Import^, FuncType,
                              XprCCToABI(FuncType.CallingConvention)) then
    begin
      FreeMem(Import);
      ctx.RaiseException('Failed to build FFI CIF for dynamic call', FDocPos);
    end;
    ctx.Intermediate.AddNativeImport(Import);

    Self.Emit(GetInstr(icFFICALL_DYN,
      [Immediate(PtrInt(Import), ctx.GetType(xtPointer)),
       Immediate(FuncType.RealParamcount),
       Immediate(Ord(FuncType.ReturnType <> nil))]),
      FDocPos);
  end
  else if FuncType.ClassMethod then
  begin
    Self.Emit(GetInstr(icINVOKE_VIRTUAL, [Immediate(FuncType.GetVMTIndex()), Immediate(totalSlots), Immediate(Ord(FuncType.ReturnType <> nil))]), FDocPos);
  end else if Func.MemPos = mpHeap then
    Self.Emit(GetInstr(icINVOKEX, [Func, Immediate(totalSlots), Immediate(Ord(FuncType.ReturnType <> nil))]), FDocPos)
  else
  begin
    if Func.IsGlobal then Func.MemPos := mpGlobal;
    Self.Emit(GetInstr(icINVOKE, [Func, debug_name_id]), FDocPos);
  end;

  //--
  if FreeingInstance and (SelfVar <> NullVar) then
  begin
    (* nothing special - collect handles it *)
    //Self.Emit(GetInstr(icRELEASE, [SelfVar.IfRefDeref(ctx)]), FDocPos);
  end;
end;


function XTree_Invoke.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var i:Int32;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName);{$ENDIF}

  for i:=0 to High(Self.Args) do
    if Self.Args[i] <> nil then  // nil = pass/default slot, nothing to delay-compile
      Self.Args[i].DelayedCompile(Dest, Flags);

  Self.Method.DelayedCompile(Dest, Flags);
  if SelfExpr <> nil then
    Self.SelfExpr.DelayedCompile(Dest, Flags);

  Result := NullResVar;
end;


function XTree_Invoke.CompileLValue(Dest: TXprVar): TXprVar;
var vType: XType;
begin
  // Type cast path
  if (Length(Args) = 1) and (SelfExpr = nil) and (Method is XTree_Identifier) then
  begin
    vType := ctx.GetType(XTree_Identifier(Self.Method).Name);
    if vType <> nil then
    begin
      with XTree_TypeCast.Create(vType, Self.Args[0], FContext, FDocPos) do
      try
        FSettings := ctx.CurrentSetting(Self.FSettings);
        Result := CompileLValue(Dest);
      finally
        Free;
      end;
      Exit;
    end;
  end;

  // A function call returning a record is addressable - the return value
  // lives on the stack and can be used as an lvalue for field access.
  if Self.ResType() is XType_Record then
  begin
    Result := Self.Compile(Dest, []);
    Exit;
  end;

  // Not addressable - return NullResVar so PushArgsToStack can spill to temp
  Result := NullResVar;
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

      Self.ResolvedParentMethod := ctx.ResolveMethod(CurrentMethod.Name, ArgList, CurrentClass.Parent, nil, [], FDocPos);

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
var i: Int32;
begin
  // An inherited call itself has no delayed logic, but its arguments might.
  for i := 0 to High(Args) do
    Args[i].DelayedCompile(Dest, Flags);
  Result := NullResVar;
end;


// ============================================================================
// Simple index operation
//
constructor XTree_Index.Create(AExpr: XTree_Node; const AIndices:array of XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
var i:Int32;
begin
  inherited Create(ACTX, DocPos);

  Self.Expr  := AExpr;

  SetLength(Self.Indices, Length(AIndices));
  Move(AIndices[0], Self.Indices[0], SizeOf(XTree_Node) * Length(AIndices));

  Self.ForceTypeSize := 0;
end;

constructor XTree_Index.Create(AExpr, AIndex: XTree_Node; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);

  Self.Expr    := AExpr;
  Self.Indices := [AIndex];

  Self.ForceTypeSize := 0;

  Self.FIsProperty := False;
end;

function XTree_Index.ToString(Offset:string=''): string;
begin
  Result := Offset + 'Index('+Self.Expr.ToString() +', indices..)';
end;

function XTree_Index.GetIndicesTypes(): XTypeArray;
var i: Int32;
begin
  SetLength(Result, Length(Indices));
  for i:=0 to High(Indices) do
  begin
    if Indices[i] = nil then
      ctx.RaiseException('Array index cannot be empty', FDocPos);
    Result[i] := Indices[i].ResType();
  end;
end;

function XTree_Index.ResType(): XType;
var
  exprType: XType;
  i: Int32;
  func: TXprVar;
  IsArrayProp: Boolean;
  FunctionName: string;
  PropSelfType: XType;
begin
  if (Self.FResType <> nil) then
    Exit(inherited);

  // -- Array property ---------------------------------------------------------
  // Class.Prop[x,y]

  IsArrayProp := False;
  FunctionName := '';
  PropSelfType := nil;

  if Self.Expr is XTree_Identifier then
  begin
    FunctionName := XTree_Identifier(Self.Expr).Name;
    PropSelfType := nil;
    IsArrayProp := True;
  end
  else if (Self.Expr is XTree_Field) and (XTree_Field(Self.Expr).Right is XTree_Identifier) then
  begin
    FunctionName := XTree_Identifier(XTree_Field(Self.Expr).Right).Name;
    PropSelfType := XTree_Field(Self.Expr).Left.ResType();
    IsArrayProp := True;
  end;

  if IsArrayProp then
  begin
    func := ctx.ResolveMethod(
      FunctionName,
      GetIndicesTypes(),
      PropSelfType,
      nil, [],
      FDocPos
    );

    if (func <> NullResVar) and (func.VarType is XType_Method) and (XType_Method(func.VarType).AccessStyle = mtRead) then
    begin
      Self.FResType := XType_Method(func.VarType).ReturnType;
      Self.FIsProperty := True;
      Exit(inherited);
    end;
  end;

  // Early resolve will error for the above properties
  // Properties are fake fields.
  exprType := Self.Expr.ResType();


  // -- Array default property -------------------------------------------------
  // Class[x,y]
  // This single rewrite solves both read and write properties. But can only do
  // so in the presence of a read property
  if (exprType is XType_Class) then
  begin
    func := ctx.ResolveMethod( '__index__', GetIndicesTypes(), exprType, nil, [], FDocPos);

    // Rewrite Self.Expr, let it compile using the existing paths.
    // Direct index on a class can ONLY ever mean a property invoke to __index__
    // Any errorhandling will be handled by the correct tree nodes, not here.
    Self.Expr := XTree_Field.Create(Self.Expr, XTree_Identifier.Create('__index__', FContext, FDocPos), FContext, FDocPos);

    if (func <> NullResVar) and (func.VarType is XType_Method) and (XType_Method(func.VarType).AccessStyle in [mtRead, mtWrite]) then
    begin
      Self.FResType := XType_Method(func.VarType).ReturnType;
      Self.FIsProperty := True;
    end else
      Self.FResType := ctx.GetType(xtUnknown);

    Exit(inherited);
  end;


  // -- Regular index ----------------------------------------------------------
  for i := 0 to High(Self.Indices) do
  begin
    if exprType = nil then
      Exit(nil); // Prevent AV if the field/expr type could not be resolved

    if (not (exprType is XType_Array)) and (not (exprType.BaseType = xtPointer)) then
      ctx.RaiseExceptionFmt('Cannot index into non-array type `%s`', [exprType.ToString], Self.Expr.FDocPos);

    case exprType.BaseType of
      xtArray, xtAnsiString, xtUnicodeString:
        exprType := XType_Array(exprType).ItemType;
      xtPointer:
      begin
        exprType := XType_Pointer(exprType).ItemType;
        if exprType = nil then exprType := ctx.GetType(xtUnknown);
      end;
    end;

    if exprType = nil then
    begin
      if Self.Expr.ResType() <> nil then
        ctx.RaiseExceptionFmt('Item type is nil for indexable type `%s`', [Self.Expr.ResType().ToString], Self.Expr.FDocPos)
      else
        ctx.RaiseException('Item type is nil for indexable type', Self.Expr.FDocPos);
    end;
  end;

  FResType := exprType;
  Exit(inherited);
end;

function XTree_Index.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  ArrVar, IndexVar, AddressVar: TXprVar;
  Index: XTree_Node;
  ItemSize, i: Int32;
  CurrentType: XType;
  func: TXprVar;
begin
  if (Self.ResType() = nil) then
  begin
    if Self.Expr.ResType() <> nil then
      ctx.RaiseExceptionFmt('Index target must be indexable, got `%s`', [Self.Expr.ResType().ToString], FDocPos)
    else
      ctx.RaiseException('Index target type could not be resolved', FDocPos);
  end;

  // ensure restype is computed!
  Self.ResType();

  if FIsProperty then
  begin
    // Field.Foo[1,2,3]
    if (Self.Expr is XTree_Field) and (XTree_Field(Self.Expr).Right is XTree_Identifier) then
    begin
      with XTree_Invoke.Create(XTree_Field(Self.Expr).Right, Self.Indices, FContext, FDocPos) do
      try
        PropertyAccess := True;
        SelfExpr := XTree_Field(Self.Expr).Left;
        Result := Compile(Dest, Flags);
      finally
        Free;
      end;
      Exit;
    end
    // Foo[1,2,3] (... questionable..)
    else if Self.Expr is XTree_Identifier then
    begin
      with XTree_Invoke.Create(Self.Expr, Self.Indices, FContext, FDocPos) do
      try
        PropertyAccess := True;
        SelfExpr := nil;
        Result := Compile(Dest, Flags);
      finally
        Free;
      end;
      Exit;
    end;
  end;

  // -- Indices[x] ---------------------
  Index  := Indices[0];
  ArrVar := Expr.Compile(NullResVar, Flags);
  if ArrVar = NullResVar then
    ctx.RaiseException('Left expression compiled to NullResVar', Expr.FDocPos);

  IndexVar := Index.Compile(NullResVar, Flags);
  if IndexVar = NullResVar then
    ctx.RaiseException('Index expression compiled to NullResVar', Index.FDocPos);

  ArrVar   := ArrVar.IfRefDeref(ctx);
  IndexVar := IndexVar.IfRefDeref(ctx);

  if ForceTypeSize = 0 then
  begin
    case Expr.ResType().BaseType of
      xtArray, xtAnsiString, xtUnicodeString:
        ItemSize := XType_Array(Expr.ResType()).ItemType.Size();
      xtPointer:
        begin
          if XType_Pointer(Expr.ResType()).ItemType <> nil then
            ItemSize := XType_Pointer(Expr.ResType()).ItemType.Size()
          else
            ItemSize := 1;
        end;
    end;
  end else
    ItemSize := ForceTypeSize;

  AddressVar := ctx.GetTempVar(ctx.GetType(EExpressBaseType.xtPointer));

  if (ArrVar.VarType is XType_Array) and ctx.CurrentSetting(Self.FSettings).RangeChecks then
    ctx.EmitRangeCheck(ArrVar, IndexVar, ctx.TryGetVar('__G_RangeExceptionTemplate'), FDocPos);

  Self.Emit(GetInstr(icFMA, [IndexVar, Immediate(ItemSize), ArrVar, AddressVar]), FDocPos);

  AddressVar.Reference := False;
  AddressVar.VarType   := ResType();

  // Single index: original exit path — byte-for-byte identical to before
  if High(Self.Indices) = 0 then
  begin
    Result := AddressVar.Deref(ctx, Dest);
    Exit;
  end;

  // -- Additional indices: chain using previous address as base ---------------
  // Peel one type level from the first index to get the intermediate base type.
  case Expr.ResType().BaseType of
    xtArray, xtAnsiString, xtUnicodeString:
      CurrentType := XType_Array(Expr.ResType()).ItemType;
    xtPointer:
      CurrentType := XType_Pointer(Expr.ResType()).ItemType;
    else CurrentType := ResType();
  end;
  AddressVar.VarType := CurrentType;
  ArrVar := AddressVar.IfRefDeref(ctx);  // load the inner pointer for next level

  for i := 1 to High(Self.Indices) do
  begin
    if Self.Indices[i] = nil then
      ctx.RaiseException('Array index cannot be empty', FDocPos);

    IndexVar := Self.Indices[i].Compile(NullResVar, Flags);
    if IndexVar = NullResVar then
      ctx.RaiseException('Index expression compiled to NullResVar', Self.Indices[i].FDocPos);
    IndexVar := IndexVar.IfRefDeref(ctx);

    case CurrentType.BaseType of
      xtArray, xtAnsiString, xtUnicodeString:
        ItemSize := XType_Array(CurrentType).ItemType.Size();
      xtPointer:
        if XType_Pointer(CurrentType).ItemType <> nil then
          ItemSize := XType_Pointer(CurrentType).ItemType.Size()
        else
          ItemSize := 1;
      else ItemSize := 1;
    end;

    AddressVar := ctx.GetTempVar(ctx.GetType(EExpressBaseType.xtPointer));

    if (ArrVar.VarType is XType_Array) and ctx.CurrentSetting(Self.FSettings).RangeChecks then
      ctx.EmitRangeCheck(ArrVar, IndexVar, ctx.TryGetVar('__G_RangeExceptionTemplate'), FDocPos);

    Self.Emit(GetInstr(icFMA, [IndexVar, Immediate(ItemSize), ArrVar, AddressVar]), FDocPos);

    // Peel one more type level
    case CurrentType.BaseType of
      xtArray, xtAnsiString, xtUnicodeString:
        CurrentType := XType_Array(CurrentType).ItemType;
      xtPointer:
        CurrentType := XType_Pointer(CurrentType).ItemType;
    end;

    AddressVar.Reference := False;
    AddressVar.VarType   := CurrentType;

    if i < High(Self.Indices) then
      ArrVar := AddressVar.IfRefDeref(ctx)  // load pointer for next level
    else
      Result := AddressVar.Deref(ctx, Dest); // final: deref for read
  end;
end;

function XTree_Index.CompileLValue(Dest: TXprVar): TXprVar;
var
  ArrVar, LocalVar, IndexVar, AddressVar: TXprVar;
  ItemSize, i: Int32;
  CurrentType: XType;
begin
  // -- Indices[x]: -------------------------------------
  ArrVar   := Expr.CompileLValue(NullResVar);
  IndexVar := Indices[0].Compile(NullResVar, []);

  LocalVar := ArrVar.IfRefDeref(ctx);
  IndexVar := IndexVar.IfRefDeref(ctx);

  if ForceTypeSize = 0 then
  begin
    case Expr.ResType().BaseType of
      xtArray, xtAnsiString, xtUnicodeString:
        ItemSize := XType_Array(Expr.ResType()).ItemType.Size();
      xtPointer:
        ItemSize := XType_Pointer(Expr.ResType()).ItemType.Size();
      else
        ItemSize := 1;
    end;
  end else
    ItemSize := ForceTypeSize;

  AddressVar := ctx.GetTempVar(ctx.GetType(EExpressBaseType.xtPointer));

  if (LocalVar.VarType is XType_Array) and ctx.CurrentSetting(Self.FSettings).RangeChecks then
    ctx.EmitRangeCheck(LocalVar, IndexVar, ctx.TryGetVar('__G_RangeExceptionTemplate'), FDocPos);

  Self.Emit(GetInstr(icFMA, [IndexVar, Immediate(ItemSize), LocalVar, AddressVar]), FDocPos);

  AddressVar.Reference := False;
  AddressVar.VarType   := ResType();

  // Single index: original exit path
  if High(Self.Indices) = 0 then
  begin
    Result             := AddressVar;
    Result.Reference   := True;
    Result.IsTemporary := ArrVar.IsTemporary;
    Exit;
  end;

  // -- Additional indices: chain --------------------------
  case Expr.ResType().BaseType of
    xtArray, xtAnsiString, xtUnicodeString:
      CurrentType := XType_Array(Expr.ResType()).ItemType;
    xtPointer:
      CurrentType := XType_Pointer(Expr.ResType()).ItemType;
    else CurrentType := ResType();
  end;
  AddressVar.Reference := True;   // it IS a reference; we need the load
  AddressVar.VarType   := CurrentType;
  LocalVar := AddressVar.IfRefDeref(ctx);  // emits the LOAD of the inner ptr

  for i := 1 to High(Self.Indices) do
  begin
    IndexVar := Self.Indices[i].Compile(NullResVar, []);
    IndexVar := IndexVar.IfRefDeref(ctx);

    case CurrentType.BaseType of
      xtArray, xtAnsiString, xtUnicodeString:
        ItemSize := XType_Array(CurrentType).ItemType.Size();
      xtPointer:
        ItemSize := XType_Pointer(CurrentType).ItemType.Size();
      else ItemSize := 1;
    end;

    AddressVar := ctx.GetTempVar(ctx.GetType(EExpressBaseType.xtPointer));

    if (LocalVar.VarType is XType_Array) and ctx.CurrentSetting(Self.FSettings).RangeChecks then
      ctx.EmitRangeCheck(LocalVar, IndexVar, ctx.TryGetVar('__G_RangeExceptionTemplate'), FDocPos);

    Self.Emit(GetInstr(icFMA, [IndexVar, Immediate(ItemSize), LocalVar, AddressVar]), FDocPos);

    case CurrentType.BaseType of
      xtArray, xtAnsiString, xtUnicodeString:
        CurrentType := XType_Array(CurrentType).ItemType;
      xtPointer:
        CurrentType := XType_Pointer(CurrentType).ItemType;
    end;

    AddressVar.Reference := False;
    AddressVar.VarType   := CurrentType;

    if i < High(Self.Indices) then
    begin
      AddressVar.Reference := True;
      LocalVar := AddressVar.IfRefDeref(ctx);
    end;
  end;

  Result             := AddressVar;
  Result.Reference   := True;
  Result.IsTemporary := ArrVar.IsTemporary;
end;

function XTree_Index.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var i: Int32;
begin
  {$IFDEF DEBUGGING_TREE}WriteLn('Delayed @ ', Self.ClassName);{$ENDIF}

  Self.Expr.DelayedCompile(Dest, Flags);
  for i:=0 to High(Self.Indices) do
    Self.Indices[i].DelayedCompile(Dest, Flags);
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
  inherited Create(ACTX, DocPos);

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

  Self.ProcessAnnotations();

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
    if not (boolVar.VarType.BaseType = xtBool) then
      ctx.RaiseExceptionFmt('If condition must be a boolean, got `%s`', [boolVar.VarType.ToString], Self.Conditions[i].FDocPos);

    // Emit jump if false → skip to next condition check
    nextCondJumps[i] := Self.Emit(
      GetInstr(icJZ, [boolVar.IfRefDeref(ctx), NullVar]),
      Self.Conditions[i].FDocPos
    );

    if Self.Bodys[i] <> nil then
    begin
      Self.PushCompilerSetting();
      Self.Bodys[i].Compile(NullResVar, Flags);
      Self.PopCompilerSetting();
    end;

    // After executing this body, skip the rest of the if-chain
    if (i < High(Self.Conditions)) or (Self.ElseBody <> nil) then
    begin
      skipRestJump := Self.Emit(
        GetInstr(icRELJMP, [NullVar]),
        Self.Conditions[i].FDocPos
      );
      ctx.PatchJump(nextCondJumps[i]);
      nextCondJumps[i] := skipRestJump;
    end else
      ctx.PatchJump(nextCondJumps[i]);
  end;


  if Self.ElseBody <> nil then
  begin
    Self.PushCompilerSetting();
    Self.ElseBody.Compile(NullResVar, Flags);
    Self.PopCompilerSetting();
  end;

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
var i,j: Int32;
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
    i, j: Int32;
    Branch: TCaseBranch;
    Condition, SubCondition: XTree_Node;
  begin
    Conditions := nil;
    Bodys := nil;

    SetLength(Conditions, Length(Branches));
    SetLength(Bodys, Length(Branches));

    for i := 0 to High(Branches) do
    begin
      Branch := Branches[i];
      Condition := nil;

      // Build the condition
      for j := 0 to Branch.Labels.High do
      begin
        if High(Branch.Labels.Data) < j then
          break;

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
var i: Int32;
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
  inherited Create(ACTX, DocPos);

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

  Self.ProcessAnnotations();
  Self.PushCompilerSetting();

  // Mark the start of the patching scope for this loop.
  ctx.PreparePatch();

  // The 'continue' target is the beginning of the condition check.
  loopStart := ctx.CodeSize();
  boolVar := Condition.Compile(NullResVar, Flags);
  if boolVar = NullResVar then
    ctx.RaiseException('While loop condition failed to compile', Condition.FDocPos);
  if not (boolVar.VarType.BaseType = xtBool) then
    ctx.RaiseExceptionFmt('While loop condition must be a boolean, got `%s`', [boolVar.VarType.ToString], Condition.FDocPos);

  // Emit the jump that will exit the loop.
  loopEnd := Self.Emit(GetInstr(icJZ, [boolVar.IfRefDeref(ctx), NullVar]), Condition.FDocPos);

  // Compile the loop body. Any 'break' or 'continue' nodes inside
  // will emit their respective placeholder opcodes.
  ctx.PushLoopScope();
  Body.Compile(NullVar, Flags - [cfFunctionBody]);
  ctx.PopLoopScope();

  // Emit the jump that brings execution back to the top of the loop.
  Self.Emit(GetInstr(icRELJMP, [ctx.RelAddr(loopStart)]), FDocPos);

  // Patch the main exit jump to point to the instruction after the loop.
  ctx.PatchJump(loopEnd);

  // Now, run the patcher for all placeholders generated within our scope.
  ctx.RunPatch(icJCONT, loopStart);
  ctx.RunPatch(icJBREAK, ctx.CodeSize());

  // Clean up the patching scope.
  ctx.PopPatch();

  Result := NullResVar;
  Self.PopCompilerSetting();
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
// Pass / emit legal no op.
//
function XTree_Pass.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  Self.Emit(GetInstr(icPASS), FDocPos);
  Result := NullResVar;
end;

function XTree_Pass.Copy(): XTree_Node;
begin
  Result := XTree_Pass.Create(FContext, FDocPos);
  Result.FSettings := Self.FSettings;
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
  ExType: XType;
  MessageType: XType;
  MessageOffset: PtrInt;
begin
  ExType := ExceptionObject.ResType();

  if not (ExType is XType_Class) then
    ctx.RaiseExceptionFmt('Cannot raise a non-class type `%s`. Raised objects should inherit from Exception.',
      [ExType.ToString()], FDocPos);

  MessageType   := (ExType as XType_Class).FieldType('message');
  MessageOffset := (ExType as XType_Class).FieldOffset('message');

  if MessageType = nil then
    ctx.RaiseExceptionFmt('Class `%s` cannot be raised: no `Message` field found in class or its ancestors.',
      [ExType.ToString()], FDocPos);

  if not (MessageType is XType_String) then
    ctx.RaiseExceptionFmt('Class `%s` cannot be raised: `Message` field must be of type String, got `%s`.',
      [ExType.ToString(), MessageType.ToString()], FDocPos);

  if MessageOffset <> 0 then
    ctx.RaiseExceptionFmt('Class `%s` cannot be raised: `Message` field must be at offset 0 (got %d). Declare it as the first field of the root exception class.',
      [ExType.ToString(), MessageOffset], FDocPos);

  // in case we created a class or rather if we did NOT, incref!
  ExceptionVar := ExceptionObject.Compile(NullResVar, Flags);
  if not ExceptionVar.IsTemporary then
    Self.Emit(GetInstr(icINCLOCK, [ExceptionVar]), FDocPos);

  Self.Emit(GetInstr(icRAISE, [ExceptionVar]), FDocPos);
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
  Conditions: XNodeArray;
  Bodys: XNodeArray;
  i, j: Int32;
  ThenNode: XTree_ExprList;
  HandlerChainAST: XTree_If;
  ErrorIdent: XTree_Identifier;
  ExceptionBase: XType;
  EffectiveElseBody: XTree_Node;   // Dont mutate Self
begin
  HandlerLogicStart := Self.Emit(GetInstr(icIncTry, [NullVar]), FDocPos);
  TryBody.Compile(NullVar, Flags);
  Self.Emit(GetInstr(icDecTry), TryBody.FDocPos);
  EndOfTryBlockJump := Self.Emit(GetInstr(icRELJMP, [NullVar]), FDocPos);
  ctx.PatchArg(HandlerLogicStart, ia1, ctx.CodeSize());

  ExceptionBase := ctx.GetType('Exception');
  if ExceptionBase = nil then
    ctx.RaiseException('No exception handlers are implemented, declare `type Exception = class`');

  ErrorIdent := XTree_Identifier.Create('!E', FContext, FDocPos);
  ctx.RegVar('!E', ExceptionBase, FDocPos);

  ExceptionTempVar := ErrorIdent.Compile(NullResVar, Flags);
  Self.Emit(GetInstr(icGET_EXCEPTION, [ExceptionTempVar]), FDocPos);

  SetLength(Conditions, Max(1, Length(Self.Handlers)));
  SetLength(Bodys,      Max(1, Length(Self.Handlers)));

  for i := 0 to High(Handlers) do
  begin
    if Handlers[i].ExceptionType.BaseType = xtUnknown then
      Handlers[i].ExceptionType := ctx.GetType(Handlers[i].ExceptionType.Name);

    Conditions[i] := XTree_TypeIs.Create(
      ErrorIdent,
      XTree_Identifier.Create(Handlers[i].ExceptionType.Name, ctx, Handlers[i].Body.FDocPos),
      ctx, Handlers[i].Body.FDocPos
    );

    ThenNode := XTree_ExprList.Create(ctx, FDocPos);
    ThenNode.List += XTree_VarDecl.Create(
      Handlers[i].VarName,
      XTree_DynCast.Create(
        ErrorIdent,
        XTree_Identifier.Create(Handlers[i].ExceptionType.Name, ctx, Handlers[i].Body.FDocPos),
        ctx, Handlers[i].Body.FDocPos
      ),
      Handlers[i].ExceptionType,
      False,
      ctx, Handlers[i].Body.FDocPos
    );

    for j := 0 to High(XTree_ExprList(Handlers[i].Body).List) do
      ThenNode.List += XTree_ExprList(Handlers[i].Body).List[j];

    Bodys[i] := ThenNode;
  end;

  if Length(Handlers) = 0 then
  begin
    Conditions[0] := XTree_Bool.Create('false', ctx, FDocPos);
    Bodys[0]      := nil;
  end;

  // Use a local - never mutate Self.ElseBody.
  // Self.ElseBody = nil means "re-raise by default"; build the node locally
  // so repeated compilation (generics, etc.) always sees the original nil.
  if Self.ElseBody <> nil then
    EffectiveElseBody := Self.ElseBody
  else
    EffectiveElseBody := XTree_ExprList.Create([
      XTree_Raise.Create(XTree_VarStub.Create(ExceptionTempVar, ctx, FDocPos), ctx, FDocPos)
    ], ctx, FDocPos);

  HandlerChainAST := XTree_If.Create(
    Conditions,
    Bodys,
    EffectiveElseBody as XTree_ExprList,
    ctx,
    FDocPos
  );

  try
    HandlerChainAST.Compile(NullResVar, Flags);
  finally
    HandlerChainAST.Free;

    // Free the locally-created else body if we built it ourselves
    if Self.ElseBody = nil then
      EffectiveElseBody.Free;
  end;

  ctx.PatchJump(EndOfTryBlockJump);
  Self.Emit(GetInstr(icUNSET_EXCEPTION), FDocPos);

  Result := NullResVar;
end;

function XTree_Try.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var i: Int32;
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
  Inherited Create(ACTX, DocPos);

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

  Self.ProcessAnnotations();
  Self.PushCompilerSetting();

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
    if not (boolVar.VarType.BaseType = xtBool) then
      ctx.RaiseExceptionFmt('For loop condition must be a boolean, got `%s`', [boolVar.VarType.ToString], Condition.FDocPos);

    // Emit the jump that will exit the loop.
    loopEnd := Self.Emit(GetInstr(icJZ, [boolVar.IfRefDeref(ctx), NullVar]), Condition.FDocPos);
  end;

  // Compile the loop body.
  ctx.PushLoopScope();
  Body.Compile(NullVar, Flags - [cfFunctionBody]);
  ctx.PopLoopScope();

  // Mark the position of the increment statement. This is the 'continue' target.
  continueTarget := ctx.CodeSize();
  if LoopStmt <> nil then
    LoopStmt.Compile(NullVar, Flags);

  // Emit the jump that brings execution back to the top of the loop.
  Self.Emit(GetInstr(icRELJMP, [ctx.RelAddr(loopStart)]), FDocPos);

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
  Self.PopCompilerSetting();

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
// FOR-ITEM-IN-ARRAY loop
//
constructor XTree_ForIn.Create(AItemVar: XTree_Node; ACollection: XTree_Node; ADeclareIdent: Byte; ABody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  Self.FContext := ACTX;
  Self.FDocPos  := DocPos;
  Self.ItemVar := AItemVar;
  Self.Collection := ACollection;
  Self.DeclareIdent:=ADeclareIdent;
  Self.Body := ABody;
end;

function XTree_ForIn.ToString(offset: string): string;
begin
  Result := offset + _AQUA_+'ForIn'+_WHITE_+'(' + LineEnding;
  Result += Self.ItemVar.ToString(offset+'  ') + ' in ' + Self.Collection.ToString('') + ',' + LineEnding;
  Result += Self.Body.ToString(Offset+'  ') + LineEnding;
  Result += offset + ')';
end;

function XTree_ForIn.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  // A for-in loop itself has no delayed logic, but its children might.
  Self.Collection.DelayedCompile(Dest, Flags);
  Self.Body.DelayedCompile(Dest, Flags);
  Result := NullResVar;
end;

(*
  This is the core of the implementation. It doesn't emit bytecode directly.
  Instead, it builds an equivalent C-style XTree_For node and then compiles that.
  This is the "desugaring" process.
*)
function XTree_ForIn.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  // Nodes for the generated C-style for loop
  highVarDecl, indexVarDecl: XTree_VarDecl;
  forLoopCondition, forLoopIncrement: XTree_Node;
  forLoop: XTree_For;
  itemAssignment: XTree_Node;
  newBody: XTree_ExprList;

  // Names for compiler-generated internal variables
  highVarName, indexVarName: string;

  // Type information
  collectionType: XType;
  itemType: XType;
  refItemVar: TXprVar;
  _ptrIdx: Int32;
  uniqueSuffix: string;
begin
  Self.ProcessAnnotations();
  Self.PushCompilerSetting();

  // --- Type Checking --------
  collectionType := Self.Collection.ResType();

  // todo: once classes has properties, allow classes!
  if not((collectionType is XType_Array) or (collectionType is XType_Class)) then // Also covers XType_String
    ctx.RaiseExceptionFmt('Cannot iterate over non-array type `%s` in a for-in loop.',
      [collectionType.ToString()], Self.Collection.FDocPos);

  // resolve the vartype of the item
  if collectionType is XType_Array then
    itemType := (collectionType as XType_Array).ItemType
  else if collectionType is XType_Class then
  begin
    // classes can be resolved though a simple index-restype test
    // should error if it doesnt act as an iterable
    with XTree_Index.Create(Self.Collection, XTree_Int.Create('0', ctx, FDocPos), ctx, FDocPos) do
    try
      itemType := ResType();
      if (itemType <> nil) and (ItemType.BaseType = xtUnknown) then
        ctx.RaiseExceptionFmt('Class %s has no read property, define __index__(ordinal):type', [collectionType.ToString()], collection.FDocPos);
    finally
      Free();
    end;
  end;

  uniqueSuffix := '_' + IntToStr(ctx.CodeSize());
  highVarName  := '!h' + uniqueSuffix;
  indexVarName := '!i' + uniqueSuffix;

  // --- 3. Build the AST for: `var !high := collection.High()` ---
  highVarDecl := XTree_VarDecl.Create(
    highVarName,
    XTree_Invoke.Create(
      XTree_Identifier.Create('High', ctx, Self.Collection.FDocPos),
      [], ctx, Self.Collection.FDocPos
    ),
    nil, False, ctx, FDocPos
  );
  XTree_Invoke(highVarDecl.Expr).SelfExpr := Self.Collection; // Attach the collection to the invoke node


  // Entry: `var index := 0`
  indexVarDecl := XTree_VarDecl.Create(
    indexVarName,
    XTree_Int.Create('0', ctx, FDocPos),
    nil, False, ctx, FDocPos
  );

  // Condition: `index <= __high`
  forLoopCondition := XTree_BinaryOp.Create(
    op_LTE,
    XTree_Identifier.Create(indexVarName, ctx, FDocPos),
    XTree_Identifier.Create(highVarName, ctx, FDocPos),
    ctx, FDocPos
  );

  // Increment: `index += 1`
  forLoopIncrement := XTree_Assign.Create(op_Asgn,
    XTree_Identifier.Create(indexVarName, ctx, FDocPos),
    XTree_BinaryOp.Create(
      op_add,
      XTree_Identifier.Create(indexVarName, ctx, FDocPos),
      XTree_Int.Create('1', ctx, FDocPos),
      ctx, Self.Collection.FDocPos
    ),
    ctx, FDocPos
  );

   // `var expr := collection[__index]`
  if DeclareIdent = 1 then
  begin
    if Self.ItemVar is XTree_Identifier then
    begin
      itemAssignment := XTree_VarDecl.Create(
        XTree_Identifier(Self.ItemVar).Name,
        XTree_Index.Create(
          Self.Collection,
          XTree_Identifier.Create(indexVarName, ctx, FDocPos),
          ctx, FDocPos
        ),
        itemType,
        // Explicitly provide the type
        False, ctx, Self.ItemVar.FDocPos
      );
    end else if Self.ItemVar is XTree_Destructure then
    begin
      itemAssignment := XTree_DestructureDecl.Create(
        XTree_Destructure(Self.ItemVar),
        XTree_Index.Create(
          Self.Collection,
          XTree_Identifier.Create(indexVarName, ctx, FDocPos),
          ctx, FDocPos
        ),
        ctx, Self.ItemVar.FDocPos
      );
    end else
      ctx.RaiseException('Illegal var declaration in for loop', FDocPos);
  end else
  // `ref ident := collection[__index]`
  if DeclareIdent = 2 then
  begin
    if not(Self.ItemVar is XTree_Identifier) then
      ctx.RaiseException('Reference variable in for-in-loop must be an identifier', Self.ItemVar.FDocPos);

    refItemVar := ctx.RegVar(XTree_Identifier(Self.ItemVar).Name, ctx.GetType(xtPointer), FDocPos, _ptrIdx);
    itemAssignment := XTree_Assign.Create(op_Asgn,
      XTree_VarStub.Create(refItemVar, ctx, FDocPos),
      XTree_UnaryOp.Create(op_Addr,
        XTree_Index.Create(Self.Collection, XTree_Identifier.Create(indexVarName, ctx, FDocPos), ctx, FDocPos),
        ctx, FDocPos
      ),
      ctx, FDocPos
    );

    // all future uses is reference uses
    ctx.Variables.Data[_ptrIdx].Reference := True;
    ctx.Variables.Data[_ptrIdx].VarType   := ItemType;
  end else
  begin
    itemAssignment := XTree_Assign.Create(op_asgn,
      Self.ItemVar,
      XTree_Index.Create(
        Self.Collection,
        XTree_Identifier.Create(indexVarName, ctx, FDocPos),
        ctx, FDocPos
      ),
      ctx, Self.ItemVar.FDocPos
    );
  end;

  // Create the new body, starting with our assignment, then adding the user's code.
  newBody := XTree_ExprList.Create(ctx, FDocPos);

  SetLength(newBody.List, Length(Self.Body.List)+1);
  if Length(Self.Body.List) > 0 then
    Move(Self.Body.List[0], NewBody.List[1], Length(Self.Body.List)*SizeOf(XTree_Node));

  newBody.List[0] := itemAssignment;

  forLoop := XTree_For.Create(
    indexVarDecl,
    forLoopCondition,
    forLoopIncrement,
    newBody,
    ctx, FDocPos
  );

  try
    // Compile the pre-loop statement: var !high := ...
    highVarDecl.Compile(NullResVar, Flags);

    // Compile the entire generated for-loop structure
    forLoop.Compile(NullResVar, Flags);
  finally
    highVarDecl.Free;
    forLoop.Free;
  end;

  Result := NullResVar;
  Self.PopCompilerSetting();
end;



// ============================================================================
// REPEAT-UNTIL loop
//
constructor XTree_Repeat.Create(ACond: XTree_Node; ABody: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
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
  ctx.PushLoopScope();
  Body.Compile(NullVar, Flags - [cfFunctionBody]);
  ctx.PopLoopScope();

  // The 'continue' target is the address of the condition check.
  continueTarget := ctx.CodeSize();
  boolVar := Condition.Compile(NullResVar, Flags);
  if boolVar = NullResVar then
    ctx.RaiseException('Repeat..Until condition failed to compile', Condition.FDocPos);
  if not (boolVar.VarType.BaseType = xtBool) then
    ctx.RaiseExceptionFmt('Repeat..Until condition must be a boolean, got `%s`', [boolVar.VarType.ToString], Condition.FDocPos);

  // Emit the conditional jump. The loop continues if the condition is FALSE (zero).
  Self.Emit(GetInstr(icJZ, [boolVar.IfRefDeref(ctx), ctx.RelAddr(loopStart)]), Condition.FDocPos);

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
  inherited Create(ACTX, DocPos);
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
          ctx.AddManagedType(FResType);
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

      op_NOT:
        FResType := ctx.GetType(xtBool);

      op_INV:
        begin
          if not(leftType is XType_Ordinal) then
            ctx.RaiseExceptionFmt('`bitwise not` only applicable to ordinal types, got `%s`', [leftType.ToString], Self.Left.FDocPos);
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
  NewLeft, NewRight: XTree_Node; // NewRight is already 'Left'
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
          Self.Emit(GetInstr(OP2IC(OP), [Result, LeftVar]), FDocPos)
        else
        begin
          Result := LeftVar;
          Result.VarType := Self.ResType();
        end;

        Result.Reference := False;
      end;

    op_DEREF:
      begin
        LeftVar := Left.Compile(NullResVar, Flags).IfRefDeref(ctx);

        if LeftVar = NullResVar then
          ctx.RaiseException('Left operand for dereference operator compiled to NullResVar', Left.FDocPos);

        if not (LeftVar.VarType is XType_Pointer) then
          ctx.RaiseExceptionFmt('Cannot dereference non-pointer variable `%s`', [LeftVar.VarType.ToString], Left.FDocPos);

        Self.Emit(GetInstr(icDREF, [Result, LeftVar, Immediate(Result.VarType.Size)]), FDocPos);
        Result.Reference := False;
      end;

    op_NOT:
      begin
       NewRight := XTree_Bool.Create('False', ctx, FDocPos);

       with XTree_BinaryOp.Create(op_EQ, Left, NewRight, ctx, FDocPos) do
       try
         FSettings := ctx.CurrentSetting(Self.FSettings);
         Result := Compile(Dest, Flags);
       finally
         Free();
       end;
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
        try
          FSettings := ctx.CurrentSetting(Self.FSettings);
          Result := Compile(Dest, Flags);
        finally
          Free();
        end;
      end;

    op_INV: // Bitwise NOT (~): rewritten as operand XOR -1
      begin
        leftType := Self.Left.ResType();
        if not (leftType.BaseType in XprIntTypes) then
          ctx.RaiseExceptionFmt('Bitwise NOT operator (~) can only be applied to Int32 types, got `%s`.', [leftType.ToString()], FDocPos);

        NewRight := XTree_Int.Create('-1', ctx, FDocPos);
        XTree_Int(NewRight).SetExpectedType(leftType.BaseType);

        // Create and compile a temporary BinaryOp node for the XOR operation.
        with XTree_BinaryOp.Create(op_XOR, Self.Left, NewRight, ctx, FDocPos) do
        try
          FSettings := ctx.CurrentSetting(Self.FSettings);
          Result := Compile(Dest, Flags);
        finally
          Free;
        end;
      end;

      op_INCREF:
        begin
          Result := Left.Compile(NullResVar, Flags);
          Self.Emit(GetInstr(icINCLOCK, [Result]), FDocPos);
        end;

      op_DECREF:
        Self.Emit(GetInstr(icDECLOCK, [Left.Compile(NullResVar, Flags)]), FDocPos);
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
  inherited Create(ACTX, DocPos);
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
      ctx.RaiseExceptionFmt('Left operand type could not be resolved for operator `%s`', [OperatorToStr(OP)], Left.FDocPos);

    if rightType = nil then
      ctx.RaiseExceptionFmt('Right operand type could not be resolved for operator `%s`', [OperatorToStr(OP)], Right.FDocPos);

    FResType := leftType.ResType(OP, rightType, FContext);
    if FResType = nil then
      ctx.RaiseExceptionFmt(eNotCompatible3, [OperatorToStr(OP), leftType.ToString(), rightType.ToString()], Right.FDocPos);
  end;
  Result := FResType; // Should return FResType not inherited
end;


(*
  Compiles the binary operation, generating intermediate code.
  Handles type promotion for arithmetic operations and short-circuiting for logical AND/OR.
*)
function XTree_BinaryOp.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  LeftVar, RightVar: TXprVar;
  Instr: EIntermediate;
  CommonTypeVar: XType;

  function DoShortCircuitOp(): TXprVar;
  var
    Instr: EIntermediate;
    PatchPos: PtrInt;
    LVar, RVar: TXprVar;
  begin
    if Dest = NullResVar then Result := ctx.GetTempVar(ctx.GetType(xtBool))
    else                      Result := Dest;

    // eval lhs
    LVar := Left.Compile(NullResVar, Flags);
    if LVar = NullResVar then
      ctx.RaiseException('Left operand of short-circuit operation compiled to NullResVar', Left.FDocPos);
    LVar := LVar.IfRefDeref(ctx);

    if not (LVar.VarType.BaseType = xtBool) then
      ctx.RaiseExceptionFmt('Short-circuit operator requires boolean operand, got `%s`', [LVar.VarType.ToString()], Left.FDocPos);

    // move left's value into result
    if (Result.MemPos <> LVar.MemPos) or (Result.Addr <> LVar.Addr) then
      Self.Emit(GetInstr(icMOV, [Result, LVar]), FDocPos);

    if OP = op_AND then Instr := icJZ
    else {op_OR}        Instr := icJNZ;

    // emit the test and the jump placeholder
    PatchPos := Self.Emit(GetInstr(Instr, [Result, NullVar]), FDocPos);

    // conditional evaluate rhs([dest:=result])
    RVar := Right.Compile(Result, Flags);
    if RVar = NullResVar then
      ctx.RaiseException('Right operand of short-circuit operation compiled to NullResVar', Right.FDocPos);
    RVar := RVar.IfRefDeref(ctx);

    if not (RVar.VarType.BaseType = xtBool) then
      ctx.RaiseExceptionFmt('Short-circuit operator requires boolean operand, got `%s`', [RVar.VarType.ToString()], Right.FDocPos);

    // Right.Compile ignored given dest, move it (constants and ident for example)
    if (Result.MemPos <> RVar.MemPos) or (Result.Addr <> RVar.Addr) then
      Self.Emit(GetInstr(icMOV, [Result, RVar]), FDocPos);

    // patch the jump destination to point here
    ctx.PatchJump(PatchPos);
  end;

var
  funcstr: string;
  LeftTemp, RightTemp: XTree_Node;
begin
  Assert(not(OP in AssignOps), 'Assignment does not belong here, dont come again!');

  if Self.Left = nil then
    ctx.RaiseException('Left operand of binary operator cannot be nil during compilation', FDocPos);
  if Self.Right = nil then
    ctx.RaiseException('Right operand of binary operator cannot be nil during compilation', FDocPos);


  // ---------------------------------------------------------------------------
  RedefineConstant(Left, Right);
  RedefineConstant(Right, Left);

  NodeTypeHint(Left, Right);

  // ---------------------------------------------------------------------------
  // short circuit logical operators
  if OP in [op_AND, op_OR] then
    Exit(DoShortCircuitOp());


  // ---------------------------------------------------------------------------
  // array = array, and array != array
  // this includes strings, fall back to internal method
  if (OP in [op_EQ, op_NEQ]) and
     (Left.ResType().BaseType in [xtAnsiString, xtUnicodeString, xtArray]) and
     (Right.ResType().BaseType = Left.ResType().BaseType) then
  begin
    if OP = op_EQ then funcstr := '__eq__'
    else               funcstr := '__neq__';

    with XTree_Invoke.Create(
      XTree_Identifier.Create(funcstr, ctx, FDocPos),
      [Right], ctx, FDocPos) do
    try
      SelfExpr := Left;
      Result := Compile(Dest, Flags);
    finally
      Free;
    end;
    Exit();
  end;


  // ---------------------------------------------------------------------------
  // lambda = nil, and lambda != nil
  // implicitly compare the 'method' function pointer field.
  // XType_Lambda is a record whose field[0] ('method') is the function pointer;
  // a nil check on the whole lambda is a nil check on that pointer.
  LeftTemp  := Left;
  RightTemp := Right;

  // swap order (nil = lambda) and (lambda = nil)
  if ((Right.ResType() is XType_Lambda)) and (Left.ResType() is XType_Pointer) and
     (XType_Pointer(Left.ResType()).ItemType = nil) then
  begin
    LeftTemp := Right;
    RightTemp := Left;
  end;

  if (OP in [op_EQ, op_NEQ]) and
     (LeftTemp.ResType() is XType_Lambda) and (RightTemp.ResType() is XType_Pointer) and
     (XType_Pointer(RightTemp.ResType()).ItemType = nil) then
  begin
    with XTree_BinaryOp.Create(OP,
        XTree_Field.Create(
          LeftTemp,
          XTree_Identifier.Create('method', ctx, FDocPos),
          ctx, FDocPos
        ), RightTemp, ctx, FDocPos) do
    try
      Result := Compile(Dest, Flags);
    finally
      Free();
    end;
    Exit;
  end;


  // ---------------------------------------------------------------------------
  // Determine the result variable.
  Result := Dest;
  if Dest = NullResVar then
  begin
    Result := ctx.GetTempVar(Self.ResType()); // Self.ResType() will handle type errors

    // dont really like this, but it solves some unexpected problems
    // we check on vartype as we always want to zero fill anything complex
    if Result.VarType.IsManagedType(ctx) then
      Self.Emit(GetInstr(icFILL, [Result, Immediate(Result.VarType.Size), Immediate(0)]), FDocPos);
  end;

  if Left.ResType() = nil then
    ctx.RaiseException('Cannot infer type from Left operand', FDocPos);
  if Right.ResType() = nil then
    ctx.RaiseException('Cannot infer type from Right operand', FDocPos);


  // ---------------------------------------------------------------------------
  // String/Char concatenation - promote chars to strings before adding
  if (OP = op_ADD) and
     (Left.ResType().BaseType in XprStringTypes + XprCharTypes) and
     (Right.ResType().BaseType in XprStringTypes + XprCharTypes) then
  begin
    if (Left.ResType().BaseType = xtUnicodeString) or
       (Right.ResType().BaseType = xtUnicodeString) then
      CommonTypeVar := ctx.GetType(xtUnicodeString)
    else
      CommonTypeVar := ctx.GetType(xtAnsiString);

    LeftVar  := Left.Compile(NullResVar, Flags).IfRefDeref(ctx);
    RightVar := Right.Compile(NullResVar, Flags).IfRefDeref(ctx);
    LeftVar  := ctx.EmitUpcastIfNeeded(LeftVar,  CommonTypeVar, False);
    RightVar := ctx.EmitUpcastIfNeeded(RightVar, CommonTypeVar, False);

    Instr := LeftVar.VarType.EvalCode(op_ADD, RightVar.VarType);
    if Instr = icNOOP then
      ctx.RaiseExceptionFmt(eNotCompatible3,
        [OperatorToStr(OP), BT2S(Left.ResType.BaseType), BT2S(Right.ResType.BaseType)], FDocPos);

    Self.Emit(GetInstr(Instr, [LeftVar, RightVar, Result]), FDocPos);
    Exit;
  end;

  // ---------------------------------------------------------------------------
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


  // ---------------------------------------------------------------------------
  // Emit the binary operation. This logic remains the same.
  Instr := LeftVar.VarType.EvalCode(OP, RightVar.VarType);
  if Instr <> icNOOP then
  begin
    Self.Emit(GetInstr(Instr, [LeftVar, RightVar, Result]), FDocPos);
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
  StartBlockID: Int32;

  function TryNoMOV(): Boolean;
  var
    last_instr_ptr: ^TInstruction;
  begin
    Result := False;

    // fallback, a jump happend somewhere in this assign
    if ctx.BasicBlockID <> StartBlockID then
      Exit(False);

    if (not LeftVar.Reference) and (RightVar.IsTemporary) and (RightVar.VarType.Equals(LeftVar.VarType)) then
    begin
      last_instr_ptr := @ctx.Intermediate.Code.Data[ctx.Intermediate.Code.High];

      case last_instr_ptr^.Code of
        icADD..icSAR:
        begin
          if TransferableData(last_instr_ptr^.Args[2], RightVar) then
          begin
            last_instr_ptr^.Args[2].Addr := LeftVar.Addr;
            last_instr_ptr^.Args[2].Pos  := LeftVar.MemPos;
            Exit(True);
          end;
        end;

        icDREF:
        begin
          if TransferableData(last_instr_ptr^.Args[0], RightVar) then
          begin
            last_instr_ptr^.Args[0].Addr := LeftVar.Addr;
            last_instr_ptr^.Args[0].Pos  := LeftVar.MemPos;
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
    RightTemp: TXprVar;
  begin
    RecType := Left.ResType as XType_Record;

    // 1) Evaluate the LHS address
    LeftVar  := Left.CompileLValue(NullResVar);

    // 2) Evaluate RHS - if not addressable (e.g. function call result),
    //    spill to a temp so we can access its fields by address
    RightVar := Right.CompileLValue(NullResVar);
    if RightVar = NullResVar then
    begin
      RightTemp := Right.Compile(NullResVar, Flags).IfRefDeref(ctx);
      RightVar  := ctx.GetTempVar(RightTemp.VarType);
      // copy the value into the temp via MOV
      ctx.Emit(GetInstr(icMOV, [RightVar, RightTemp]), FDocPos, ctx.FSettings);
      RightVar.Reference := False;
    end;

    // 3) Create stub AST nodes
    LeftStub  := XTree_VarStub.Create(LeftVar,  ctx, Left.FDocPos);
    RightStub := XTree_VarStub.Create(RightVar, ctx, Right.FDocPos);

    try
      for i := 0 to RecType.FieldTypes.High do
      begin
        IdentNode   := XTree_Identifier.Create(RecType.FieldNames.Data[i], ctx, FDocPos);
        FieldDest   := XTree_Field.Create(LeftStub,  IdentNode, ctx, FDocPos);
        FieldSource := XTree_Field.Create(RightStub, IdentNode, ctx, FDocPos);

        with XTree_Assign.Create(op_Asgn, FieldDest, FieldSource, ctx, FDocPos) do
        try
          FSettings := ctx.CurrentSetting(Self.FSettings);
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
    i, k: Int32;
    TargetNode: XTree_Node;
    SourceFieldNode: XTree_Field;
    AssignNode: XTree_Assign;
    RightHandSideValues: array of TXprVar;
    InitializerList: XTree_InitializerList;
  begin
    // special case of (a,b) := [b,a]
    if RHS_Node is XTree_InitializerList then
    begin
      InitializerList := RHS_Node as XTree_InitializerList;

      if Length(PatternNode.Targets) <> Length(InitializerList.Items) then
        ctx.RaiseExceptionFmt('The number of variables in the pattern (%d) does not match the number of values in the initializer list (%d).',
          [Length(PatternNode.Targets), Length(InitializerList.Items)], PatternNode.FDocPos);

      // Evaluate all RHS expressions into temps before any assignment fires.
      RightHandSideValues := [];
      SetLength(RightHandSideValues, Length(InitializerList.Items));
      for i := 0 to High(InitializerList.Items) do
        RightHandSideValues[i] := InitializerList.Items[i].Compile(NullResVar, Flags);

      // Protect any borrowed managed refs before the assignment loop.
      // Without this, ManagedAssign's Collect(old LHS) can free a value that
      // also appears on the RHS - the classic swap case:
      //   (a[j], a[jp]) := [a[jp], a[j]]
      // a[j]'s old value has rc=1; Collect during the first assignment drops
      // it to 0 and frees it, then the second assignment IncLocks freed memory.
      // IncLocking every borrowed managed temp now raises rc to >= 2 so the
      // intermediate Collect never reaches 0. Clearing IsBorrowedRef lets
      // EmitAbandonedTempCleanup emit the balancing Collect afterward.
      for i := 0 to High(RightHandSideValues) do
      begin
        if RightHandSideValues[i].IsBorrowedRef and
           RightHandSideValues[i].VarType.IsManagedType(ctx) then
        begin
          Self.Emit(GetInstr(icINCLOCK, [RightHandSideValues[i]]), FDocPos);
          RightHandSideValues[i].IsBorrowedRef := False;
          // Write the cleared flag back to the canonical Variables entry.
          for k := ctx.Variables.High downto 0 do
            if (ctx.Variables.Data[k].Addr   = RightHandSideValues[i].Addr) and
               (ctx.Variables.Data[k].MemPos = RightHandSideValues[i].MemPos) then
            begin
              ctx.Variables.Data[k].IsBorrowedRef := False;
              break;
            end;
        end;
      end;

      // Now assign each RHS temp to its LHS target.
      for i := 0 to High(PatternNode.Targets) do
      begin
        AssignNode := XTree_Assign.Create(
          op_Asgn,
          PatternNode.Targets[i],
          XTree_VarStub.Create(RightHandSideValues[i], ctx, RHS_Node.FDocPos),
          ctx,
          FDocPos
        );
        try
          AssignNode.Compile(NullResVar, Flags);
        finally
          AssignNode.Free;
        end;
      end;
      Exit;
    end;

    // Record destructuring: (a, b) := someRecord
    SourceVar := RHS_Node.Compile(NullResVar, Flags);
    SourceType := SourceVar.VarType;

    if not (SourceType is XType_Record) then
    begin
      ctx.RaiseException('The right-hand side of a destructuring assignment must be a record.', RHS_Node.FDocPos);
      Exit;
    end;

    RecType := SourceType as XType_Record;

    if RecType.FieldNames.Size <> Length(PatternNode.Targets) then
    begin
      ctx.RaiseExceptionFmt('The number of variables in the pattern (%d) does not match the number of fields in the record type `%s` (%d).',
        [Length(PatternNode.Targets), RecType.Name, RecType.FieldNames.Size], PatternNode.FDocPos);
      Exit;
    end;

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
        FSettings := ctx.CurrentSetting(Self.FSettings);
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
      Self.Emit(GetInstr(icINCLOCK, [RightVar]), FDocPos);
      // any decrement will be handled by collect
    end;

    if (LeftVar.VarType.BaseType in XprRefcountedTypes) and (not(cfNoCollect in Flags)) then
      ctx.EmitCollect(LeftVar);
  end;

  procedure ManagedAssign();
  var RightForIncRef: TXprVar;
  begin
    if (not LeftVar.Reference) and (LeftVar.Addr = RightVar.Addr) and
       (LeftVar.IsGlobal = RightVar.IsGlobal) then
      Exit;

    // Get the OLD value from the LHS before we overwrite it.
    if LeftVar.Reference then
    begin
      // If LHS is a pointer (e.g., a[j].str), DEREF it to get the old string pointer.
      OldLeftValueVar := ctx.GetTempVar(LeftVar.VarType);
      OldLeftValueVar.IsTemporary := True; // this menas no collect, hmm.
      Self.Emit(GetInstr(icDREF, [OldLeftValueVar, LeftVar, Immediate(LeftVar.VarType.Size)]), FDocPos);
    end
    else
    begin
      // If LHS is a simple local variable, its current value is the old value.
      // We don't need to copy it, we will just use LeftVar itself before the MOV.
      OldLeftValueVar := LeftVar;
    end;

    // Retain Right! IncRef if the DESTINATION is managed, regardless of source type.
    // The source may be a raw Pointer cast to class/array/string - the destination
    // type is the authoritative signal that refcounting is required.
    if LeftVar.VarType.IsManagedType(ctx) and (not (cfNoRefcount in Flags)) and
      (not (RightVar.MemPos in [mpConst, mpImm])) then
    begin
      RightForIncRef := RightVar;
      RightForIncRef.VarType := LeftVar.VarType;  // When the source is itself unmanaged
      Self.Emit(GetInstr(icINCLOCK, [RightForIncRef]), FDocPos);
    end;

    // Release the OLD value of Left
    if (not (cfNoCollect in Flags)) then
    begin
      ctx.EmitCollect(OldLeftValueVar);
      // Prevent EmitAbandonedTempCleanup from double-collecting this slot.
      // We just explicitly released it; mark it borrowed so cleanup skips it.
      if LeftVar.Reference then
      begin
        OldLeftValueVar.IsBorrowedRef := True;
        ctx.Variables.Data[ctx.Variables.High].IsBorrowedRef := True;
      end;
    end;

    // Perform the actual assignment (pointer copy)
    if LeftVar.Reference then
      Self.Emit(STORE_FAST(LeftVar, RightVar, True), FDocPos)
    else
      Self.Emit(GetInstr(icMOV, [LeftVar, RightVar]), FDocPos);
  end;

var
  argz: XTypeArray;
  func: TXprVar;
  IsProperty: Boolean;
  FunctionName: string;
  PropSelfExpr, PropMethodNode: XTree_Node;
  PropSelfType: XType;
  PropIndices: XNodeArray;
  PropArgTypes: XTypeArray;
  i: Int32;
begin
  Result := NullResVar;

  StartBlockID := ctx.BasicBlockID;

  if Left = nil then
    ctx.RaiseException(eSyntaxError, 'Left hand side of assignment cannot be nil', FDocPos);
  if Right = nil then
    ctx.RaiseException(eSyntaxError, 'Right hand side of assignment cannot be nil', FDocPos);
  if Left is XTree_Const then // just fuck off
    ctx.RaiseException(eSyntaxError, eExpectedVar, Left.FDocPos);

  // Force a manual sync of the stack pos from local up to global BEFORE
  // the right-hand side evaluates any potential closure functions or global hints.
  if not ctx.IsInsideFunction() then
    ctx.SyncStackPosMax(GLOBAL_SCOPE, ctx.Scope);

  // hint at whatever to let it know we have a type for resolution
  if not(Left is XTree_Destructure) then
    Right.SetResTypeHint(Left.ResType());

  // try to make the constant the same type as the value we are assigning to.
  if (Right is XTree_Const) and (Left.ResType() <> nil) and (Right.ResType() <> nil) and (Left.ResType() <> Right.ResType()) then
    XTree_Const(Right).SetExpectedType(Left.ResType.BaseType);

  // --- Solve cases that can exit early ---
  // ---------------------------------------

  // -- Possible property access (Index or Field) ------------------------------
  IsProperty := False;
  FunctionName := '';
  PropSelfExpr := nil;
  PropSelfType := nil;
  PropMethodNode := nil;
  PropIndices := nil;

  if Left is XTree_Index then
  begin
    if XTree_Index(Left).Expr is XTree_Identifier then
    begin
      FunctionName := XTree_Identifier(XTree_Index(Left).Expr).Name;
      PropSelfExpr := nil;
      PropSelfType := nil;
      PropMethodNode := XTree_Index(Left).Expr;
      PropIndices := XTree_Index(Left).Indices;
      IsProperty := True;
    end
    else if (XTree_Index(Left).Expr is XTree_Field) and
            (XTree_Field(XTree_Index(Left).Expr).Right is XTree_Identifier) then
    begin
      FunctionName := XTree_Identifier(XTree_Field(XTree_Index(Left).Expr).Right).Name;
      PropSelfExpr := XTree_Field(XTree_Index(Left).Expr).Left;
      PropSelfType := PropSelfExpr.ResType();
      PropMethodNode := XTree_Field(XTree_Index(Left).Expr).Right;
      PropIndices := XTree_Index(Left).Indices;
      IsProperty := True;
    end;
  end
  else if Left is XTree_Field then
  begin
    if XTree_Field(Left).Right is XTree_Identifier then
    begin
      FunctionName := XTree_Identifier(XTree_Field(Left).Right).Name;
      PropSelfExpr := XTree_Field(Left).Left;
      PropSelfType := PropSelfExpr.ResType();
      PropMethodNode := XTree_Field(Left).Right;
      PropIndices := nil; // Simple properties have no indices
      IsProperty := True;
    end;
  end;

  if IsProperty then
  begin
    // Pack the resolved arguments (Indices + RHS Assignment value)
    SetLength(PropArgTypes, Length(PropIndices) + 1);
    for i := 0 to High(PropIndices) do
      PropArgTypes[i] := PropIndices[i].ResType();
    PropArgTypes[High(PropArgTypes)] := Self.Right.ResType();

    // does it resolve to a method? then it is a method!
    func := ctx.ResolveMethod(
      FunctionName,
      PropArgTypes,
      PropSelfType,
      nil, [],
      FDocPos
    );

    // write properties will be resolved here!
    if (func <> NullResVar) and (func.VarType is XType_Method) and
       (XType_Method(func.VarType).AccessStyle = mtWrite) then
    begin
      with XTree_Invoke.Create(PropMethodNode, [], FContext, FDocPos) do
      try
        PropertyAccess := True;
        Args     := PropIndices + Self.Right;
        SelfExpr := PropSelfExpr;
        Result   := Compile(Dest, Flags);
      finally
        Free();
      end;
      Exit;
    end;
  end;

  // -- Destructure List Assignment --------------------------------------------
  if Left is XTree_Destructure then
  begin
    AssignByDestructuring(Left as XTree_Destructure, Right);
    Exit(NullResVar);
  end;

  // -- Initializer List Assignment --------------------------------------------
  if Right is XTree_InitializerList then
  begin
    LeftVar := Left.CompileLValue(NullResVar);
    if LeftVar = NullResVar then
      ctx.RaiseException('Left hand side of assignment did not compile to a valid LValue', Left.FDocPos);

    Right.Compile(LeftVar, Flags);
    Exit(NullResVar);
  end;

  // -- lambda := func ---------------------------------------------------------
  if (Left.ResType() is XType_Lambda) and
     (Right.ResType() is XType_Method) and
     not (Right.ResType() is XType_Lambda) then
  begin
    RightVar := Right.Compile(NullResVar, Flags).IfRefDeref(ctx);
    LeftVar  := Left.CompileLValue(NullResVar);

    // Build the zero-capture lambda struct directly into LeftVar
    // 1. Zero fill the whole struct
    Self.Emit(GetInstr(icFILL, [LeftVar,
      Immediate(LeftVar.VarType.Size()), Immediate(0)]), FDocPos);

    // 2. Copy the function address into the method field (offset 0)
    RightVar.MemPos := mpGlobal;
    Self.Emit(GetInstr(icCOPY_GLOBAL, [LeftVar, RightVar]), FDocPos);

    // 3. size field = 0, args = nil
    Exit(NullResVar);
  end;

  // -- lambda := nil ----------------------------------------------------------
  if (Left.ResType() is XType_Lambda) and
     (Right.ResType() is XType_Pointer) and
     (XType_Pointer(Right.ResType()).ItemType = nil) then
  begin
    RightVar := Right.Compile(NullResVar, Flags).IfRefDeref(ctx);
    with XTree_Field.Create(Left, XTree_Identifier.Create('method', ctx, FDocPos), ctx, FDocPos) do
    try
      LeftVar := CompileLValue(Dest);
    finally
      Free();
    end;

    Self.Emit(STORE_FAST(LeftVar, RightVar, LeftVar.Reference), FDocPos);
    Exit(NullResVar);
  end;

  // -- record := record, element wise assign ----------------------------------
  if (Left.ResType() is XType_Record) and (Right.ResType() is XType_Record) then
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

  // reject incompatible types before any store
  if not LeftVar.VarType.CanAssign(RightVar.VarType) then
    ctx.RaiseExceptionFmt(
      'Cannot assign `%s` to `%s`',
      [RightVar.VarType.ToString(), LeftVar.VarType.ToString()], FDocPos);

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
    Self.Emit(STORE_FAST(LeftVar, RightVar, True), FDocPos)
  else
    Self.Emit(GetInstr(icMOV, [LeftVar, RightVar]), FDocPos);
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
  inherited Create(ACTX, DocPos);

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
var
  i: Int32;
  argType: XType;
  argNode, toStrNode, combined: XTree_Node;
  argVar: TXprVar;
  combinedVar: TXprVar;
begin
  combined := nil;
  for i := 0 to High(Self.Args) do
  begin
    argNode := Self.Args[i];
    argType := argNode.ResType();

    if (argType <> nil) and (argType.BaseType in [xtAnsiString, xtUnicodeString]) then
    begin
      // String - no ToStr needed
      toStrNode := argNode;
    end
    else
    begin
      // Compile to a stable var first so SelfExpr has an addressable target
      argVar := argNode.Compile(NullResVar, Flags);
      argVar := argVar.IfRefDeref(ctx);

      toStrNode := XTree_Invoke.Create(
        XTree_Identifier.Create('ToStr', ctx, FDocPos),
        [], ctx, FDocPos
      );
      XTree_Invoke(toStrNode).SelfExpr :=
        XTree_VarStub.Create(argVar, ctx, FDocPos);
    end;

    if combined = nil then
      combined := toStrNode
    else
      combined := XTree_BinaryOp.Create(
        op_Add,
        XTree_BinaryOp.Create(op_Add, combined,
          XTree_String.Create(' ', ctx, FDocPos), ctx, FDocPos),
        toStrNode,
        ctx, FDocPos
      );
  end;

  if combined <> nil then
  begin
    combinedVar := combined.Compile(NullResVar, Flags);
    if combinedVar.Reference then
      combinedVar := combinedVar.DerefToTemp(ctx);


    Self.Emit(GetInstr(icPRINT, [combinedVar, Immediate(combinedVar.VarType.Size)]), FDocPos);
  end else
    Self.Emit(GetInstr(icPRINT, [NullResVar, Immediate(0)]), FDocPos);

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





constructor XTree_ListComp.Create(AItemVar, ACollection, AStart, AEnd, AFilter, AYield: XTree_Node;
                                   AIsRange, ADeclareVar: Boolean; ACTX: TCompilerContext; DocPos: TDocPos);
begin
  inherited Create(ACTX, DocPos);
  Self.ItemVar    := AItemVar;
  Self.Collection := ACollection;
  Self.StartExpr  := AStart;
  Self.EndExpr    := AEnd;
  Self.FilterExpr := AFilter;
  Self.YieldExpr  := AYield;
  Self.IsRange    := AIsRange;
  Self.DeclareVar := ADeclareVar;
end;

function XTree_ListComp.ResType(): XType;
var
  itemType, loopVarType: XType;
  arrayType:   XType_Array;
  tempVar:     TXprVar;
begin
  if FResType = nil then
  begin
    if (FResTypeHint <> nil) and (FResTypeHint is XType_Array) then
    begin
      ctx.ResolveToFinalType(FResTypeHint);  // resolve inner placeholders in the hint
      FResType := FResTypeHint;
      Exit(inherited);
    end;

    // Determine loop variable type so YieldExpr can resolve it
    if IsRange then
      loopVarType := StartExpr.ResType()
    else
    begin
      if not (Collection.ResType() is XType_Array) then
        ctx.RaiseExceptionFmt('Cannot iterate over non-array type `%s`',
          [Collection.ResType().ToString], Collection.FDocPos);
      loopVarType := (Collection.ResType() as XType_Array).ItemType;
    end;

    // Register the loop variable temporarily so YieldExpr.ResType() can see it.
    // Compile() will re-register it properly via VarDecl - that's fine, it shadows.
    if DeclareVar and (ItemVar is XTree_Identifier) then
    begin
      tempVar      := TXprVar.Create(loopVarType);
      tempVar.Addr := ctx.StackPos;
      ctx.RegVar(XTree_Identifier(ItemVar).Name, tempVar, FDocPos);
    end;

    itemType  := Self.YieldExpr.ResType();
    arrayType := XType_Array.Create(itemType);
    ctx.AddManagedType(arrayType);
    FResType  := arrayType;
  end;
  Result := inherited;
end;

function XTree_ListComp.Compile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
var
  resultVar:     TXprVar;
  resultStub:    XTree_VarStub;
  resultVarDecl: XTree_VarDecl;
  loopBody:      XTree_ExprList;
  ifGuard:       XTree_If;
  forNode:       XTree_Node;
  yieldBody:     XTree_ExprList;
  endVarName, varName, uniqueSuffix, resultName, idxName, highVarName, indexVarName:  string;
  highVarDecl, indexVarDecl, idxVarDecl, endVarDecl:     XTree_VarDecl;
  loopCond, loopInc, loopInit, itemAssign, yieldAssign:  XTree_Node;
  idxInc, capacityExpr: XTree_Node;
  itemType:             XType;
begin
  uniqueSuffix := '_lc' + IntToStr(ctx.CodeSize());
  resultName   := '!result' + uniqueSuffix;
  idxName      := '!idx'    + uniqueSuffix;
  //ctx.PushBlockScope();

  // -- 1. Hoist end/high so capacity is available before result declaration ---

  if IsRange then
  begin
    // var !end := endExpr  (hoisted once)
    endVarName := '!end' + uniqueSuffix;
    endVarDecl := XTree_VarDecl.Create(
      endVarName, Self.EndExpr, nil, False, ctx, FDocPos);
    endVarDecl.FSettings := ctx.CurrentSetting(Self.FSettings);
    endVarDecl.Compile(NullResVar, Flags);
    endVarDecl.Free;

    // capacity = !end - start + 1
    capacityExpr := XTree_BinaryOp.Create(
      op_ADD,
      XTree_BinaryOp.Create(
        op_SUB,
        XTree_Identifier.Create(endVarName, ctx, FDocPos),
        Self.StartExpr,
        ctx, FDocPos),
      XTree_Int.Create('1', ctx, FDocPos),
      ctx, FDocPos);
  end
  else
  begin
    // var !h := collection.High()  (hoisted once)
    highVarName := '!h' + uniqueSuffix;
    highVarDecl := XTree_VarDecl.Create(
      highVarName,
      XTree_Invoke.Create(
        XTree_Identifier.Create('High', ctx, FDocPos),
        [], ctx, FDocPos),
      nil, False, ctx, FDocPos);
    XTree_Invoke(highVarDecl.Expr).SelfExpr := Self.Collection;
    highVarDecl.FSettings := ctx.CurrentSetting(Self.FSettings);
    highVarDecl.Compile(NullResVar, Flags);
    highVarDecl.Free;

    // capacity = !h + 1  (== collection.Len())
    capacityExpr := XTree_BinaryOp.Create(
      op_ADD,
      XTree_Identifier.Create(highVarName, ctx, FDocPos),
      XTree_Int.Create('1', ctx, FDocPos),
      ctx, FDocPos);
  end;

  // -- 2. Declare result array and pre-allocate to capacity ------------------
  resultVarDecl := XTree_VarDecl.Create(
    resultName, nil, Self.ResType(), False, ctx, FDocPos);
  resultVarDecl.FSettings := ctx.CurrentSetting(Self.FSettings);
  resultVarDecl.Compile(NullResVar, Flags);
  resultVarDecl.Free;

  resultVar  := ctx.GetVar(resultName, FDocPos);
  resultStub := XTree_VarStub.Create(resultVar, ctx, FDocPos);

  // result.SetLen(capacity)
  with XTree_Invoke.Create(
      XTree_Identifier.Create('SetLen', ctx, FDocPos),
      [capacityExpr],
      ctx, FDocPos) do
  try
    SelfExpr  := resultStub;
    FSettings := ctx.CurrentSetting(Self.FSettings);
    Compile(NullResVar, Flags);
  finally
    Free;
  end;

  // -- 3. Declare write index: var !idx := 0 ---------------------------------
  idxVarDecl := XTree_VarDecl.Create(
    idxName,
    XTree_Int.Create('0', ctx, FDocPos),
    nil, False, ctx, FDocPos);
  idxVarDecl.FSettings := ctx.CurrentSetting(Self.FSettings);
  idxVarDecl.Compile(NullResVar, Flags);
  idxVarDecl.Free;

  // -- 4. Build yield body: result[!idx] := YieldExpr; !idx += 1 -------------
  yieldAssign := XTree_Assign.Create(op_Asgn,
    XTree_Index.Create(
      XTree_VarStub.Create(resultVar, ctx, FDocPos),
      XTree_Identifier.Create(idxName, ctx, FDocPos),
      ctx, FDocPos),
    Self.YieldExpr,
    ctx, FDocPos);

  idxInc := XTree_Assign.Create(op_Asgn,
    XTree_Identifier.Create(idxName, ctx, FDocPos),
    XTree_BinaryOp.Create(
      op_ADD,
      XTree_Identifier.Create(idxName, ctx, FDocPos),
      XTree_Int.Create('1', ctx, FDocPos),
      ctx, FDocPos),
    ctx, FDocPos);

  // yieldAssign + idxInc as a two-statement body
  yieldBody := XTree_ExprList.Create(
    [yieldAssign, idxInc], ctx, FDocPos);

  // -- 5. Optionally wrap in where(...) guard --------------------------------
  if FilterExpr <> nil then
  begin
    ifGuard  := XTree_If.Create(
      [FilterExpr],
      [yieldBody],
      nil, ctx, FDocPos);
    loopBody := XTree_ExprList.Create(ifGuard, ctx, FDocPos);
  end else
    loopBody := yieldBody;

  loopBody.FSettings := ctx.CurrentSetting(Self.FSettings);

  // -- 6. Build loop ---------------------------------------------------------
  if IsRange then
  begin
    varName := XTree_Identifier(Self.ItemVar).Name;

    if DeclareVar then
      loopInit := XTree_VarDecl.Create(
        varName, Self.StartExpr, nil, False, ctx, FDocPos)
    else
      loopInit := XTree_Assign.Create(op_Asgn,
        XTree_Identifier.Create(varName, ctx, FDocPos),
        Self.StartExpr,
        ctx, FDocPos);

    loopCond := XTree_BinaryOp.Create(
      op_LTE,
      XTree_Identifier.Create(varName, ctx, FDocPos),
      XTree_Identifier.Create(endVarName, ctx, FDocPos),
      ctx, FDocPos);

    loopInc := XTree_Assign.Create(op_Asgn,
      XTree_Identifier.Create(varName, ctx, FDocPos),
      XTree_BinaryOp.Create(
        op_ADD,
        XTree_Identifier.Create(varName, ctx, FDocPos),
        XTree_Int.Create('1', ctx, FDocPos),
        ctx, FDocPos),
      ctx, FDocPos);

    forNode := XTree_For.Create(loopInit, loopCond, loopInc, loopBody, ctx, FDocPos);
    forNode.FSettings := ctx.CurrentSetting(Self.FSettings);
    forNode.Compile(NullResVar, Flags);
    forNode.Free;
  end
  else
  begin
    itemType     := (Self.Collection.ResType() as XType_Array).ItemType;
    indexVarName := '!i' + uniqueSuffix;

    // var !i := 0
    indexVarDecl := XTree_VarDecl.Create(
      indexVarName,
      XTree_Int.Create('0', ctx, FDocPos),
      nil, False, ctx, FDocPos);

    loopCond := XTree_BinaryOp.Create(
      op_LTE,
      XTree_Identifier.Create(indexVarName, ctx, FDocPos),
      XTree_Identifier.Create(highVarName,  ctx, FDocPos),
      ctx, FDocPos);

    loopInc := XTree_Assign.Create(op_Asgn,
      XTree_Identifier.Create(indexVarName, ctx, FDocPos),
      XTree_BinaryOp.Create(
        op_ADD,
        XTree_Identifier.Create(indexVarName, ctx, FDocPos),
        XTree_Int.Create('1', ctx, FDocPos),
        ctx, FDocPos),
      ctx, FDocPos);

    // item assignment
    if Self.ItemVar is XTree_Identifier then
    begin
      varName := XTree_Identifier(Self.ItemVar).Name;
      if DeclareVar then
        itemAssign := XTree_VarDecl.Create(
          varName,
          XTree_Index.Create(
            Self.Collection,
            XTree_Identifier.Create(indexVarName, ctx, FDocPos),
            ctx, FDocPos),
          itemType, False, ctx, FDocPos)
      else
        itemAssign := XTree_Assign.Create(op_Asgn,
          XTree_Identifier.Create(varName, ctx, FDocPos),
          XTree_Index.Create(
            Self.Collection,
            XTree_Identifier.Create(indexVarName, ctx, FDocPos),
            ctx, FDocPos),
          ctx, FDocPos);
    end
    else
      itemAssign := XTree_DestructureDecl.Create(
        XTree_Destructure(Self.ItemVar),
        XTree_Index.Create(
          Self.Collection,
          XTree_Identifier.Create(indexVarName, ctx, FDocPos),
          ctx, FDocPos),
        ctx, FDocPos);

    // Prepend item assignment to loop body
    SetLength(loopBody.List, Length(loopBody.List) + 1);
    Move(loopBody.List[0], loopBody.List[1],
         (Length(loopBody.List) - 1) * SizeOf(XTree_Node));
    loopBody.List[0] := itemAssign;

    forNode := XTree_For.Create(
      indexVarDecl, loopCond, loopInc, loopBody, ctx, FDocPos);
    forNode.FSettings := ctx.CurrentSetting(Self.FSettings);
    forNode.Compile(NullResVar, Flags);
    forNode.Free;
  end;

  // -- 7. Trim result to actual written count ---------------------------------
  // result.SetLen(!idx)
  resultStub := XTree_VarStub.Create(resultVar, ctx, FDocPos);
  with XTree_Invoke.Create(
      XTree_Identifier.Create('SetLen', ctx, FDocPos),
      [XTree_Identifier.Create(idxName, ctx, FDocPos)],
      ctx, FDocPos) do
  try
    SelfExpr  := resultStub;
    FSettings := ctx.CurrentSetting(Self.FSettings);
    Compile(NullResVar, Flags);
  finally
    Free;
  end;

  //ctx.PopBlockScope();

  // -- 8. Return result array -------------------------------------------------
  Result := resultVar;
end;

function XTree_ListComp.DelayedCompile(Dest: TXprVar; Flags: TCompilerFlags): TXprVar;
begin
  if ItemVar    <> nil then ItemVar.DelayedCompile(Dest, Flags);
  if Collection <> nil then Collection.DelayedCompile(Dest, Flags);
  if StartExpr  <> nil then StartExpr.DelayedCompile(Dest, Flags);
  if EndExpr    <> nil then EndExpr.DelayedCompile(Dest, Flags);
  if FilterExpr <> nil then FilterExpr.DelayedCompile(Dest, Flags);
  if YieldExpr  <> nil then YieldExpr.DelayedCompile(Dest, Flags);
  Result := NullResVar;
end;

function XTree_ListComp.Copy(): XTree_Node;
var
  clone: XTree_ListComp;
  cItemVar, cCollection, cStart, cEnd, cFilter, cYield: XTree_Node;
begin
  cItemVar    := nil; if ItemVar    <> nil then cItemVar    := ItemVar.Copy();
  cCollection := nil; if Collection <> nil then cCollection := Collection.Copy();
  cStart      := nil; if StartExpr  <> nil then cStart      := StartExpr.Copy();
  cEnd        := nil; if EndExpr    <> nil then cEnd        := EndExpr.Copy();
  cFilter     := nil; if FilterExpr <> nil then cFilter     := FilterExpr.Copy();
  cYield      := nil; if YieldExpr  <> nil then cYield      := YieldExpr.Copy();
  clone := XTree_ListComp.Create(
    cItemVar, cCollection, cStart, cEnd, cFilter, cYield,
    IsRange, DeclareVar, FContext, FDocPos);
  clone.FSettings := Self.FSettings;
  Result := clone;
end;






// -----------------------------------------------------------------------------
// --- Copy methods ------------------------------------------------------------

{ XTree_ExprList }
function XTree_ExprList.Copy(): XTree_Node;
begin
  Result := XTree_ExprList.Create(CopyNodeArray(Self.List), FContext, FDocPos);
  (Result as XTree_ExprList).DelayedList := CopyNodeArray(Self.DelayedList);
  Result.FSettings   := Self.FSettings;
end;

{ XTree_Annotation }
function XTree_Annotation.Copy(): XTree_Node;
begin
  Result := XTree_Annotation.Create(FContext, FDocPos);
  XTree_Annotation(Result).Value      := Value.Copy();
  XTree_Annotation(Result).Identifier := Identifier.Copy();
  Result.FSettings   := Self.FSettings;
end;

{ XTree_VarStub }
function XTree_VarStub.Copy(): XTree_Node;
begin
  Result := XTree_VarStub.Create(Self.VarDecl, FContext, FDocPos);
  Result.FSettings   := Self.FSettings;
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
  Result.FSettings   := Self.FSettings;
end;

{ XTree_Pointer }
function XTree_Pointer.Copy(): XTree_Node;
begin
  Result := XTree_Pointer.Create(Self.StrValue, FContext, FDocPos);
  Result.FSettings   := Self.FSettings;
end;

{ XTree_Char }
function XTree_Char.Copy(): XTree_Node;
begin
  Result := XTree_Char.Create(Self.StrValue, FContext, FDocPos);
  Result.FSettings := Self.FSettings;
end;

{ XTree_Int }
function XTree_Int.Copy(): XTree_Node;
begin
  Result := XTree_Int.Create(Self.StrValue, FContext, FDocPos);
  Result.FSettings := Self.FSettings;
end;

{ XTree_Float }
function XTree_Float.Copy(): XTree_Node;
begin
  Result := XTree_Float.Create(Self.StrValue, FContext, FDocPos);
  Result.FSettings   := Self.FSettings;
end;

{ XTree_String }
function XTree_String.Copy(): XTree_Node;
begin
  Result := XTree_String.Create(Self.StrValue, FContext, FDocPos);
  Result.FSettings := Self.FSettings;
end;

{ XTree_ImportUnit }
function XTree_ImportUnit.Copy(): XTree_Node;
begin
  Result := XTree_ImportUnit.Create(Self.UnitPath, Self.UnitAlias, FContext, FDocPos);
  Result.FSettings   := Self.FSettings;
end;

{ XTree_Identifier }
function XTree_Identifier.Copy(): XTree_Node;
begin
  Result := XTree_Identifier.Create(Self.Name, FContext, FDocPos);
  Result.FSettings   := Self.FSettings;
end;

{ XTree_Destructure }
function XTree_Destructure.Copy(): XTree_Node;
begin
  Result := XTree_Destructure.Create(CopyNodeArray(Self.Targets), FContext, FDocPos);
  Result.FSettings   := Self.FSettings;
end;

{ XTree_NonLocalDecl }
function XTree_NonLocalDecl.Copy(): XTree_Node;
begin
  Result := XTree_NonLocalDecl.Create(CopyIdentNodeList(Self.Variables), FContext, FDocPos);
  Result.FSettings   := Self.FSettings;
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

  Result.FSettings := Self.FSettings;
end;

{ XTree_DestructureDecl }
function XTree_DestructureDecl.Copy(): XTree_Node;
begin
  Result := XTree_DestructureDecl.Create(Self.Pattern.Copy as XTree_Destructure, Self.Expression.Copy, FContext, FDocPos);
  Result.FSettings   := Self.FSettings;
end;

{ XTree_InitializerList }
function XTree_InitializerList.Copy(): XTree_Node;
begin
  Result := XTree_InitializerList.Create(CopyNodeArray(Self.Items), FContext, FDocPos);
  Result.FSettings   := Self.FSettings;
end;

{ XTree_TypeCast }
function XTree_TypeCast.Copy(): XTree_Node;
begin
  Result := XTree_TypeCast.Create(Self.TargetType, Self.Expression.Copy, FContext, FDocPos);
  Result.FSettings   := Self.FSettings;
end;

{ XTree_ClassDecl }
function XTree_ClassDecl.Copy(): XTree_Node;
var clone: XTree_ClassDecl;
begin
  clone := XTree_ClassDecl.Create(Self.ClassDeclName, Self.ParentName,
             CopyNodeArray(Self.TypeDecls), CopyNodeArray(Self.Fields), CopyNodeArray(Self.Methods), FContext, FDocPos);
  clone.TypeParams          := Self.TypeParams;
  clone.TypeConstraints     := Self.TypeConstraints;
  clone.ParentExplicitTypes := Self.ParentExplicitTypes;
  clone.FSettings           := Self.FSettings;
  Result := clone;
end;

{ XTree_ClassCreate }
function XTree_ClassCreate.Copy(): XTree_Node;
var NewNode: XTree_ClassCreate;
begin
  NewNode := XTree_ClassCreate.Create(Self.ClassIdent, CopyNodeArray(Self.Args), FContext, FDocPos);
  NewNode.ClassTyp           := Self.ClassTyp;
  NewNode.ExplicitTypeParams := Self.ExplicitTypeParams;
  Result := NewNode;
  Result.FSettings := Self.FSettings;
end;

{ XTree_DynCast }
function XTree_DynCast.Copy(): XTree_Node;
begin
  Result := XTree_DynCast.Create(Self.Expression.Copy, Self.TargetTypeNode.Copy, FContext, FDocPos);
  Result.FSettings := Self.FSettings;
end;

{ XTree_TypeIs }
function XTree_TypeIs.Copy(): XTree_Node;
begin
  Result := XTree_TypeIs.Create(Self.Expression.Copy, Self.TargetTypeNode.Copy, FContext, FDocPos);
  Result.FSettings := Self.FSettings;
end;

{ XTree_IfExpr }
function XTree_IfExpr.Copy(): XTree_Node;
begin
  Result := XTree_IfExpr.Create(Self.Condition.Copy, Self.ThenExpr.Copy, Self.ElseExpr.Copy, FContext, FDocPos);
  Result.FSettings := Self.FSettings;
end;

{ XTree_NamedArg }
function XTree_NamedArg.Copy(): XTree_Node;
var v: XTree_Node;
begin
  v := nil;
  if Value <> nil then v := Value.Copy();
  Result := XTree_NamedArg.Create(ArgName, v, FContext, FDocPos);
  Result.FSettings := Self.FSettings;
end;

{ XTree_Return }
function XTree_Return.Copy(): XTree_Node;
var
  NewExpr: XTree_Node;
begin
  if Self.Expr <> nil then NewExpr := Self.Expr.Copy else NewExpr := nil;
  Result := XTree_Return.Create(NewExpr, FContext, FDocPos);
  Result.FSettings := Self.FSettings;
end;

{ XTree_Break }
function XTree_Break.Copy(): XTree_Node;
begin
  Result := XTree_Break.Create(FContext, FDocPos);
  Result.FSettings := Self.FSettings;
end;

{ XTree_Continue }
function XTree_Continue.Copy(): XTree_Node;
begin
  Result := XTree_Continue.Create(FContext, FDocPos);
  Result.FSettings := Self.FSettings;
end;

{ XTree_Function }
function XTree_Function.Copy(): XTree_Node;
var
  NewNode:     XTree_Function;
  copiedArgs:  XTypeArray;
  i:           Int32;
begin
  // Deep-copy ArgTypes so specialization can overwrite them
  // without mutating the template's shared array.
  SetLength(copiedArgs, Length(Self.ArgTypes));
  for i := 0 to High(Self.ArgTypes) do
    copiedArgs[i] := Self.ArgTypes[i];

  NewNode := XTree_Function.Create(Self.Name, Self.ArgNames, Self.ArgPass, copiedArgs, Self.RetType, Self.ProgramBlock.Copy as XTree_ExprList, FContext, FDocPos);
  NewNode.IsNested       := Self.IsNested;
  NewNode.SingleExpression := Self.SingleExpression;
  NewNode.Extra          := Self.Extra;
  NewNode.SelfType       := Self.SelfType;
  NewNode.TypeName       := Self.TypeName;
  NewNode.InternalFlags  := Self.InternalFlags;
  NewNode.Annotations    := Self.Annotations;
  NewNode.TypeParams     := Self.TypeParams;
  NewNode.TypeConstraints:= Self.TypeConstraints;
  NewNode.ArgDefaults    := CopyNodeArray(Self.ArgDefaults);
  NewNode.isProperty     := Self.isProperty;
  NewNode.isConstructor  := Self.isConstructor;

  Result := NewNode;
end;

{ XTree_GenericFunction }
function XTree_GenericFunction.Copy(): XTree_Node;
begin
  Result := XTree_GenericFunction.Create(Self.GenericFunction.Copy, FContext, FDocPos);
  Result.FSettings := Self.FSettings;
end;

{ XTree_Field }
function XTree_Field.Copy(): XTree_Node;
begin
  Result := XTree_Field.Create(Self.Left.Copy, Self.Right.Copy, FContext, FDocPos);
  Result.FSettings := Self.FSettings;
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
  Result.FSettings := Self.FSettings;
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
  Result.FSettings := Self.FSettings;
end;

{ XTree_Index }
function XTree_Index.Copy(): XTree_Node;
var
  i: Int32;
  NewNode: XTree_Index;
  newIndices: XNodeArray;
begin
  SetLength(newIndices, length(self.Indices));
  for i:=0 to High(self.Indices) do
    newIndices[i] := self.Indices[i].Copy();

  NewNode := XTree_Index.Create(Self.Expr.Copy, newIndices, FContext, FDocPos);
  NewNode.ForceTypeSize := Self.ForceTypeSize;
  Result := NewNode;
  Result.FSettings := Self.FSettings;
end;

{ XTree_If }
function XTree_If.Copy(): XTree_Node;
var
  NewElse: XTree_ExprList;
begin
  if Self.ElseBody <> nil then NewElse := Self.ElseBody.Copy as XTree_ExprList else NewElse := nil;
  Result := XTree_If.Create(CopyNodeArray(Self.Conditions), CopyNodeArray(Self.Bodys), NewElse, FContext, FDocPos);
  Result.FSettings := Self.FSettings;
end;

{ XTree_Case }
function XTree_Case.Copy(): XTree_Node;
var
  NewBranches: TCaseBranchArray;
  NewElse: XTree_Node;
  i: Int32;
begin
  SetLength(NewBranches, Length(Self.Branches));
  for i := 0 to High(Self.Branches) do
  begin
    NewBranches[i].Labels.Init(Branches[i].Labels.RawOfManaged());
    NewBranches[i].Body := Branches[i].Body.Copy;
  end;
  if Self.ElseBody <> nil then NewElse := Self.ElseBody.Copy else NewElse := nil;
  Result := XTree_Case.Create(Self.Expression.Copy, NewBranches, NewElse, FContext, FDocPos);
  Result.FSettings := Self.FSettings;
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
  Result.FSettings := Self.FSettings;
end;

{ XTree_Try }
function XTree_Try.Copy(): XTree_Node;
var
  NewHandlers: TExceptionHandlerArray;
  NewElse: XTree_Node;
  i: Int32;
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
  Result.FSettings := Self.FSettings;
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
  Result.FSettings := Self.FSettings;
end;

{ XTree_ForIn }
function XTree_ForIn.Copy(): XTree_Node;
begin
  Result := XTree_ForIn.Create(
    Self.ItemVar.Copy(),
    Self.Collection.Copy(),
    Self.DeclareIdent,
    Self.Body.Copy as XTree_ExprList,
    FContext,
    FDocPos
  );
  Result.FSettings   := Self.FSettings;
end;

{ XTree_Repeat }
function XTree_Repeat.Copy(): XTree_Node;
begin
  Result := XTree_Repeat.Create(Self.Condition.Copy, Self.Body.Copy as XTree_ExprList, FContext, FDocPos);
  Result.FSettings := Self.FSettings;
end;

{ XTree_UnaryOp }
function XTree_UnaryOp.Copy(): XTree_Node;
begin
  Result := XTree_UnaryOp.Create(Self.OP, Self.Left.Copy, FContext, FDocPos);
  Result.FSettings := Self.FSettings;
end;

{ XTree_BinaryOp }
function XTree_BinaryOp.Copy(): XTree_Node;
begin
  Result := XTree_BinaryOp.Create(Self.OP, Self.Left.Copy, Self.Right.Copy, FContext, FDocPos);
  Result.FSettings := Self.FSettings;
end;

{ XTree_Assign }
function XTree_Assign.Copy(): XTree_Node;
begin
  Result := XTree_Assign.Create(Self.OP, Self.Left.Copy, Self.Right.Copy, FContext, FDocPos);
  Result.FSettings := Self.FSettings;
end;

{ XTree_Print }
function XTree_Print.Copy(): XTree_Node;
begin
  Result := XTree_Print.Create(CopyNodeArray(Self.Args), FContext, FDocPos);
  Result.FSettings := Self.FSettings;
end;







// ============================================================================
// Capture Analysis
// Pre-compile AST scan to find exactly which enclosing-scope locals a nested
// function or lambda actually references, rather than capturing everything.
// Called before DelayedCompile so the enclosing scope is fully populated.
//
// This is used for lambda's and local functions.
// ============================================================================
type
  TCaptureNameSet = record
    Items: TStringArray;
    procedure Include(const Name: string);
    function Contains(const Name: string): Boolean;
  end;

procedure TCaptureNameSet.Include(const Name: string);
var i: Int32;
begin
  for i := 0 to High(Items) do
    if Items[i] = Name then Exit;
  SetLength(Items, Length(Items)+1);
  Items[High(Items)] := Name;
end;

function TCaptureNameSet.Contains(const Name: string): Boolean;
var i: Int32;
begin
  for i := 0 to High(Items) do
    if Items[i] = Name then Exit(True);
  Result := False;
end;

function ScanFunctionCaptures(Func: XTree_Function; ctx: TCompilerContext): TStringArray;
var
  Declared: TCaptureNameSet;
  Captured: TCaptureNameSet;

  procedure Scan(Node: XTree_Node);
  var
    i, j, l: Int32;
    cased: string;
    found: TXprVar;
    DeepList: TStringArray;
  begin
    if Node = nil then Exit;

    // -- Terminal: variable reference -----------------------------------------
    if Node is XTree_Identifier then
    begin
      cased := XprCase(XTree_Identifier(Node).Name);

      if not Declared.Contains(cased) then
      begin
        found := ctx.TryGetVar(cased);
        if (found <> NullResVar) and
           (not found.IsGlobal) and (not(found.MemPos in [mpUnknown, mpHeap, mpConst, mpGlobal])) and
           (found.VarType <> nil) then  // global funcs are XType_Method
          Captured.Include(cased);
      end;
      Exit;
    end;

    // -- Indirect container nodes ---------------------------------------------
    // -- Dont stop: While nested function body is its own capture unit
    // -- It will only capture from it's parent, so parent has to capture for
    // -- every single nested child
    if (Node is XTree_Function) or (Node is XTree_ClosureFunction) then
    begin
      if node is XTree_ClosureFunction then
        DeepList := ScanFunctionCaptures(XTree_ClosureFunction(Node).ClosureFunction, ctx)
      else
        DeepList := ScanFunctionCaptures(XTree_Function(Node), ctx);

      l := Length(Captured.Items);
      SetLength(Captured.Items, Length(Captured.Items)+Length(DeepList));
      for i:=0 to High(DeepList) do
        Captured.Items[i+l] := DeepList[i];
    end;


    // -- Declaration: initializer scanned before name enters scope ------------
    if Node is XTree_VarDecl then
    begin
      Scan(XTree_VarDecl(Node).Expr);
      for i := 0 to XTree_VarDecl(Node).Variables.High do
        Declared.Include(XprCase(XTree_VarDecl(Node).Variables.Data[i].Name));
      Exit;
    end;

    if Node is XTree_DestructureDecl then
    begin
      Scan(XTree_DestructureDecl(Node).Expression);
      for i := 0 to High(XTree_Destructure(XTree_DestructureDecl(Node).Pattern).Targets) do
        if XTree_Destructure(XTree_DestructureDecl(Node).Pattern).Targets[i] is XTree_Identifier then
          Declared.Include(XprCase(XTree_Identifier(
            XTree_Destructure(XTree_DestructureDecl(Node).Pattern).Targets[i]).Name));
      Exit;
    end;

    // We may also need to hoist for local class declarations with methods!
    if Node is XTree_ClassDecl then
    begin
      for i:=0 to High(XTree_ClassDecl(Node).Methods) do
      begin
        DeepList := ScanFunctionCaptures(XTree_Function(XTree_ClassDecl(Node).Methods[i]), ctx);

        l := Length(Captured.Items);
        SetLength(Captured.Items, Length(Captured.Items)+Length(DeepList));
        for j:=0 to High(DeepList) do
          Captured.Items[j+l] := DeepList[j];
      end;
    end;

    // -- Container nodes ------------------------------------------------------

    if Node is XTree_ExprList then
    begin
      for i := 0 to High(XTree_ExprList(Node).List) do
        Scan(XTree_ExprList(Node).List[i]);
      Exit;
    end;

    // BinaryOp covers XTree_Assign too (it's a descendant)
    if Node is XTree_BinaryOp then
    begin
      Scan(XTree_BinaryOp(Node).Left);
      Scan(XTree_BinaryOp(Node).Right);
      Exit;
    end;

    if Node is XTree_UnaryOp then
    begin
      Scan(XTree_UnaryOp(Node).Left);
      Exit;
    end;

    if Node is XTree_Field then
    begin
      Scan(XTree_Field(Node).Left);
      // Right is a bare field name identifier -> do NOT scan (not a var ref).
      // But if it is an Invoke (method call), scan its args.
      if XTree_Field(Node).Right is XTree_Invoke then
      begin
        for i := 0 to High(XTree_Invoke(XTree_Field(Node).Right).Args) do
          Scan(XTree_Invoke(XTree_Field(Node).Right).Args[i]);
      end;
      Exit;
    end;

    if Node is XTree_Invoke then
    begin
      // Method name: scan it so stored function-pointer variables are caught,
      // but the Identifier check already filters global methods by VarType.
      if not (XTree_Invoke(Node).Method is XTree_Identifier) then
        Scan(XTree_Invoke(Node).Method)  // e.g. field/index returning a callable
      else
        Scan(XTree_Invoke(Node).Method); // identifier - let the ref check decide
      if XTree_Invoke(Node).SelfExpr <> nil then
        Scan(XTree_Invoke(Node).SelfExpr);
      for i := 0 to High(XTree_Invoke(Node).Args) do
        Scan(XTree_Invoke(Node).Args[i]);
      Exit;
    end;

    if Node is XTree_Index then
    begin
      Scan(XTree_Index(Node).Expr);
      for i:=0 to High(XTree_Index(Node).Indices) do
        Scan(XTree_Index(Node).Indices[i]);
      Exit;
    end;

    if Node is XTree_If then
    begin
      for i := 0 to High(XTree_If(Node).Conditions) do Scan(XTree_If(Node).Conditions[i]);
      for i := 0 to High(XTree_If(Node).Bodys)      do Scan(XTree_If(Node).Bodys[i]);
      Scan(XTree_If(Node).ElseBody);
      Exit;
    end;

    if Node is XTree_IfExpr then
    begin
      Scan(XTree_IfExpr(Node).Condition);
      Scan(XTree_IfExpr(Node).ThenExpr);
      Scan(XTree_IfExpr(Node).ElseExpr);
      Exit;
    end;

    if Node is XTree_While then
    begin
      Scan(XTree_While(Node).Condition);
      Scan(XTree_While(Node).Body);
      Exit;
    end;

    if Node is XTree_For then
    begin
      // EntryStmt may be a VarDecl - Scan handles declaration ordering
      Scan(XTree_For(Node).EntryStmt);
      Scan(XTree_For(Node).Condition);
      Scan(XTree_For(Node).Body);
      Scan(XTree_For(Node).LoopStmt);
      Exit;
    end;

    if Node is XTree_ForIn then
    begin
      Scan(XTree_ForIn(Node).Collection);
      if XTree_ForIn(Node).DeclareIdent > 0 then
      begin
        // The item var is declared - add to scope before scanning body
        if XTree_ForIn(Node).ItemVar is XTree_Identifier then
          Declared.Include(XprCase(XTree_Identifier(XTree_ForIn(Node).ItemVar).Name));
        // Destructure case: targets become declared
        if XTree_ForIn(Node).ItemVar is XTree_Destructure then
          for i := 0 to High(XTree_Destructure(XTree_ForIn(Node).ItemVar).Targets) do
            if XTree_Destructure(XTree_ForIn(Node).ItemVar).Targets[i] is XTree_Identifier then
              Declared.Include(XprCase(XTree_Identifier(
                XTree_Destructure(XTree_ForIn(Node).ItemVar).Targets[i]).Name));
      end else
        Scan(XTree_ForIn(Node).ItemVar); // pre-existing variable being assigned into
      Scan(XTree_ForIn(Node).Body);
      Exit;
    end;

    if Node is XTree_Repeat then
    begin
      Scan(XTree_Repeat(Node).Body);
      Scan(XTree_Repeat(Node).Condition);
      Exit;
    end;

    if Node is XTree_Try then
    begin
      Scan(XTree_Try(Node).TryBody);
      for i := 0 to High(XTree_Try(Node).Handlers) do
      begin
        Declared.Include(XprCase(XTree_Try(Node).Handlers[i].VarName));
        Scan(XTree_Try(Node).Handlers[i].Body);
      end;
      Scan(XTree_Try(Node).ElseBody);
      Exit;
    end;

    if Node is XTree_Case then
    begin
      Scan(XTree_Case(Node).Expression);
      for i := 0 to High(XTree_Case(Node).Branches) do
        Scan(XTree_Case(Node).Branches[i].Body);
      Scan(XTree_Case(Node).ElseBody);
      Exit;
    end;

    if Node is XTree_Return    then begin Scan(XTree_Return(Node).Expr);             Exit; end;
    if Node is XTree_Raise     then begin Scan(XTree_Raise(Node).ExceptionObject);   Exit; end;
    if Node is XTree_TypeCast  then begin Scan(XTree_TypeCast(Node).Expression);     Exit; end;
    if Node is XTree_DynCast   then begin Scan(XTree_DynCast(Node).Expression);      Exit; end;
    if Node is XTree_TypeIs    then begin Scan(XTree_TypeIs(Node).Expression);       Exit; end;

    if Node is XTree_ClassCreate then
    begin
      for i := 0 to High(XTree_ClassCreate(Node).Args) do
        Scan(XTree_ClassCreate(Node).Args[i]);
      Exit;
    end;

    if Node is XTree_InheritedCall then
    begin
      if XTree_InheritedCall(Node).SelfExpr <> nil then
        Scan(XTree_InheritedCall(Node).SelfExpr);
      for i := 0 to High(XTree_InheritedCall(Node).Args) do
        Scan(XTree_InheritedCall(Node).Args[i]);
      Exit;
    end;

    if Node is XTree_Print then
    begin
      for i := 0 to High(XTree_Print(Node).Args) do
        Scan(XTree_Print(Node).Args[i]);
      Exit;
    end;

    // XTree_Const subtypes, XTree_VarStub, XTree_Pass, XTree_Break,
    // XTree_Continue, XTree_TypeDecl, XTree_ImportUnit - no children, fall through.
  end;

var
  i,l: Int32;
begin
  Declared.Items := [];
  Captured.Items := [];

  // Params are declared within the function - never captures
  for i := 0 to High(Func.ArgNames) do
    Declared.Include(XprCase(Func.ArgNames[i]));

  // Built-ins always in scope inside a function body
  Declared.Include('self');
  Declared.Include('result');

  Scan(Func.ProgramBlock);
  Result := Captured.Items;
end;


end.
