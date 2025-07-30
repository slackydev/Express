unit xprTypeIntrinsics;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  This unit provides functions to generate Abstract Syntax Tree (AST) branches
  for intrinsic array operations like Length and SetLength.
  These operations are implemented by synthesizing calls to low-level
  memory management functions (AllocMem, ReallocMem, FreeMem) and
  direct manipulation of the array's internal [refcount, length, dataptr] structure.
}
{$I header.inc}
{$hints off}

interface

uses
  SysUtils,
  xprTypes,           // For EExpressBaseType, SizeInt, CompilerContextBase etc.
  xprTokenizer,       // For TDocPos
  xprTree,            // For XTree_Node and other AST nodes
  xprVartypes,        // For XType_Array, XType_Integer, etc.
  xprErrors,
  xprCompilerContext;

type
  TTypeIntrinsics = class(TIntrinsics)
  private
    // Helper to get a literal integer node
    function GetIntLiteral(Value: Int64): XTree_Int;
    // Helper to get a literal nil pointer node
    function GetNilPointerLiteral: XTree_Int;
    // Helper to get a variable reference node
    function GetVarRef(AVar: TXprVar): XTree_VarStub;
    // Helper to get the address of a TXprVar
    function GetAddrOfVar(AVar: TXprVar): XTree_Node;
    // Helper to dereference a pointer
    function GetDeref(APointerExpr: XTree_Node; AType: XType): XTree_Node;
    // Helper to create a binary operation node
    function GetBinaryOp(Op: EOperator; Left, Right: XTree_Node): XTree_BinaryOp;
    // Helper to create an assignment node
    function GetAssignment(LHS, RHS: XTree_Node): XTree_Assign;
    // Helper to create a function call node (for intrinsics like AllocMem)
    function GetCall(FuncName: string; Args: XNodeArray): XTree_Invoke;
    // Helper to create an expression list (sequence of statements)
    function GetExprList(Nodes: XNodeArray): XTree_ExprList;

    // Internal helper to get the TXprVar for intrinsic functions
    function GetIntrinsicFuncVar(FuncName: string; Arguments: array of XType; SelfType: XType = nil): TXprVar;

  public
    FContext: TCompilerContext;
    FDocPos: TDocPos; // Current document position for error reporting/node creation

    constructor Create(AContext: TCompilerContext; ADocPos: TDocPos);

    function GenerateFunction(FuncName: string;
                     Argnames: TStringArray;
                     ByRef: TPassArgsBy;
                     Arguments: XTypeArray;
                     ReturnType: XType;
                     ExprList: XTree_ExprList
    ): XTree_Function;


    function GenerateHigh(SelfType:XType; Args: array of XType): XTree_Function;
    function GenerateCollect(SelfType: XType; Args: array of XType): XTree_Function;
    function GenerateSetLen(SelfType: XType; Args: array of XType): XTree_Function;

    // You might add other "magic functions" here later, e.g.,
    // function GenerateCopy(DestArray, SrcArray, Count: XTree_Node): XTree_Node;
  end;

implementation

{ TTypeIntrinsics }

constructor TTypeIntrinsics.Create(AContext: TCompilerContext; ADocPos: TDocPos);
begin
  inherited Create;
  FContext := AContext;
  FDocPos := ADocPos;
end;

function TTypeIntrinsics.GetIntLiteral(Value: Int64): XTree_Int;
begin
  Result := XTree_Int.Create(IntToStr(Value), FContext, FDocPos);
  Result.FResType := FContext.GetType(xtInt);
  Result.Value := Value;
end;

function TTypeIntrinsics.GetNilPointerLiteral: XTree_Int;
begin
  Result := XTree_Int.Create('0', FContext, FDocPos);
  Result.FResType := FContext.GetType(xtPointer);
  Result.Value := 0;
  Result.FResType.BaseType := xtPointer;
end;

function TTypeIntrinsics.GetVarRef(AVar: TXprVar): XTree_VarStub;
begin
  Result := XTree_VarStub.Create(AVar, FContext, FDocPos);
end;

function TTypeIntrinsics.GetAddrOfVar(AVar: TXprVar): XTree_Node;
begin
  // This would generate an ADDR instruction for the variable
  // In the AST, this might be a special UnaryOp or a direct node.
  // For simplicity, let's assume a conceptual XTree_AddrOf node or similar.
  // Alternatively, the Compile method of XTree_VarRef could return its address
  // when a specific flag is set. For now, let's make a conceptual node.
  Result := XTree_UnaryOp.Create(op_Addr, GetVarRef(AVar), FContext, FDocPos);
  Result.FResType := FContext.GetType(xtPointer); // Address is a pointer
end;

function TTypeIntrinsics.GetDeref(APointerExpr: XTree_Node; AType: XType): XTree_Node;
begin
  // This would generate a DREF instruction
  Result := XTree_UnaryOp.Create(op_Deref, APointerExpr, FContext, FDocPos);
  Result.FResType := AType; // Dereferenced value has the target type
end;

function TTypeIntrinsics.GetBinaryOp(Op: EOperator; Left, Right: XTree_Node): XTree_BinaryOp;
begin
  Result := XTree_BinaryOp.Create(Op, Left, Right, FContext, FDocPos);
  // ResType will be determined during compilation of this node
end;

function TTypeIntrinsics.GetAssignment(LHS, RHS: XTree_Node): XTree_Assign;
begin
  Result := XTree_Assign.Create(op_Asgn, LHS, RHS, FContext, FDocPos);
end;

function TTypeIntrinsics.GetCall(FuncName: string; Args: XNodeArray): XTree_Invoke;
var
  FuncVar: TXprVar;
begin
  Result := XTree_Invoke.Create(
    XTree_Identifier.Create(FuncName, FContext, FDocPos),
    Args,
    FContext,
    FDocPos
  );
end;

function TTypeIntrinsics.GetExprList(Nodes: XNodeArray): XTree_ExprList;
begin
  Result := XTree_ExprList.Create(Nodes, FContext, FDocPos);
end;

function TTypeIntrinsics.GetIntrinsicFuncVar(FuncName: string; Arguments: array of XType; SelfType: XType = nil): TXprVar;
begin
  // This is a conceptual lookup. In reality, FContext would have a way to
  // retrieve pre-defined intrinsic function symbols.
  // For example, they might be stored in a special dictionary in TCompilerContext
  // or resolved via a dedicated lookup mechanism.
  // For now, we'll assume they exist and have a pointer type.
  Result := FContext.ResolveMethod(FuncName,Arguments, SelfType); // Assuming FindSymbol can find intrinsics
  if Result.VarType = nil then
    RaiseExceptionFmt('Intrinsic function "%s" is not declared in compiler context.', [FuncName], FDocPos);
end;

function TTypeIntrinsics.GenerateFunction(
    FuncName: string;
    Argnames: TStringArray;
    ByRef: TPassArgsBy;
    Arguments: XTypeArray;
    ReturnType: XType;
    ExprList: XTree_ExprList): XTree_Function;
begin
//XTree_Function.Create(AName: string; AArgNames: TStringArray; ByRef: TPassArgsBy; AArgTypes: XTypeArray; ARet:XType; AProg: XTree_ExprList; ACTX: TCompilerContext; DocPos: TDocPos);
  Result := XTree_Function.Create(FuncName, Argnames, ByRef, Arguments, ReturnType, ExprList, Self.FContext, Self.FDocPos);
  //Result.TypeName := TypeName;
end;

(*
  Generates a function, to be placed ontop in the global expression list
*)
function TTypeIntrinsics.GenerateHigh(SelfType:XType; Args: Array of XType): XTree_Function;
var
  ArrayIdent, CompareTest, CheckHigh, ResultVar: XTree_Node;
  ProgramData: XTree_ExprList;
  IndexNode: XTree_Index;
  LocalVars: XIdentNodeList;
begin
  if Length(Args) > 0 then Exit(nil);
  if SelfType.BaseType <> xtArray then Exit(nil);

  ArrayIdent := XTree_Identifier.Create('Self', FContext, FDocPos);
  CompareTest := XTree_BinaryOp.Create(op_NEQ, ArrayIdent, GetNilPointerLiteral(), FContext, FDocPos);

  IndexNode := XTree_Index.Create(ArrayIdent, GetIntLiteral(-1), FContext, FDocPos);
  IndexNode.FResType := FContext.GetType(xtInt);
  IndexNode.ForceTypeSize := SizeOf(SizeInt);

  LocalVars.Init([]);
  ResultVar := XTree_Identifier.Create('Result', FContext, FDocPos);
  LocalVars.Add(XTree_Identifier( ResultVar));

  ProgramData := XTree_ExprList.Create(FContext, FDocPos);
  ProgramData.List += XTree_VarDecl.Create(LocalVars, nil, FContext.GetType(xtInt), FContext, FDocPos);
  ProgramData.List += XTree_Assign.Create(op_Asgn, ResultVar, GetIntLiteral(-1), FContext, FDocPos);

  CheckHigh := XTree_If.Create(
    [CompareTest],
    [XTree_Return.Create(IndexNode, FContext, FDocPos)],
    XTree_ExprList.Create(XTree_Return.Create(ResultVar, FContext, FDocPos), FContext, FDocPos),
    FContext, FDocPos
  );

  ProgramData.List += CheckHigh;
  Result := GenerateFunction('High', nil, nil, nil, FContext.GetType(xtInt), ProgramData);
  Result.SelfType := SelfType;
end;

function TTypeIntrinsics.GenerateCollect(SelfType: XType; Args: array of XType): XTree_Function;
var
  ProgramData: XTree_ExprList;
  Body: XTree_ExprList;
  ElmntNode, ElmntRefcountNode, RefcountNode, IndexNode: XTree_Node; // The reference count field

  Iterator, RefElmnt: XTree_Identifier;
  TestRefcountZero,Binary_DecRefCount: XTree_BinaryOp;

  LoopBody: XTree_ExprList;
  FieldPtr, ElementPtr, CompareTest: XTree_Node;
  ObjectIdent, ArrayIdentAsInt: XTree_Identifier;
  i: Integer;
  Invoke_SetLength, Invoke_High, Invoke_Collect: XTree_Invoke;

  FieldType: XType;
  LocalVars: XIdentNodeList;
begin
  // The Collect routine will take a pointer to the data as its argument
  ObjectIdent := XTree_Identifier.Create('Self', FContext, FDocPos);

  ProgramData := XTree_ExprList.Create([], FContext, FDocPos);

  LocalVars.Init([]);
  case SelfType.BaseType of
    xtArray:
    begin
      //ArrayIdentAsPtr := XTree_Identifier.Create('Self', FContext, FDocPos);
      //ArrayIdentAsPtr.FResType := FContext.GetType(xtPointer);

      CompareTest := XTree_BinaryOp.Create(op_EQ, ObjectIdent, GetIntLiteral(0), FContext, FDocPos);
      CompareTest.FResType := FContext.GetType(xtBoolean);

      ProgramData.List += XTree_If.Create(
        [CompareTest],
        [XTree_Return.Create(nil, FContext, FDocPos)], // Return early if nil
        nil, FContext, FDocPos
      );


      // 1. Check the refcount, no decrement here, this is handled elsewhere.
      RefcountNode := XTree_Index.Create(ObjectIdent, GetIntLiteral(-2), FContext, FDocPos);
      RefcountNode.FResType := FContext.GetType(xtInt);
      XTree_Index(RefcountNode).ForceTypeSize := SizeOf(SizeInt);

      Body := XTree_ExprList.Create([], FContext, FDocPos);
      (*
        Array of array
      *)
      if XType_Array(SelfType).ItemType.BaseType = xtArray then
      begin
        // Loop through elements to collect children recursively
        Iterator := XTree_Identifier.Create('!i', FContext, FDocPos);
        RefElmnt := XTree_Identifier.Create('!r', FContext, FDocPos);
        LocalVars.Add(Iterator);
        LocalVars.Add(RefElmnt);
        Body.List += XTree_VarDecl.Create(LocalVars, nil, FContext.GetType(xtInt), FContext, FDocPos);

        // Get the element:
        ElmntNode := XTree_Index.Create(ObjectIdent, Iterator, FContext, FDocPos);
        ElmntNode.FResType := XType_Array(SelfType).ItemType;


        // Decrement refcount {array[!i][-2] -= 1}, we are treating array as array of SizeInt
        ElmntRefcountNode := XTree_Index.Create(ElmntNode, GetIntLiteral(-2), FContext, FDocPos);
        XTree_Index(ElmntRefcountNode).ForceTypeSize := SizeOf(SizeInt);
        ElmntRefcountNode.FResType := FContext.GetType(xtInt);

        Binary_DecRefCount := GetBinaryOp(op_Sub, ElmntRefcountNode, GetIntLiteral(1));
        Binary_DecRefCount.FResType := FContext.GetType(xtInt);

        LoopBody := XTree_ExprList.Create([], FContext, FDocPos);

        LoopBody.List += GetAssignment(RefElmnt, Binary_DecRefCount);
        LoopBody.List += GetAssignment(ElmntRefcountNode, RefElmnt);

        // prepare call to ObjectIdent[!i].setlength for child in case we need it
        Invoke_SetLength := XTree_Invoke.Create(
            XTree_Identifier.Create('SetLen', FContext, FDocPos),
            [GetIntLiteral(0)],
            FContext, FDocPos
        );
        Invoke_SetLength.SelfExpr := ElmntNode;

        TestRefcountZero := GetBinaryOp(op_EQ, RefElmnt, GetIntLiteral(0));
        TestRefcountZero.FResType := FContext.GetType(xtBoolean);
        // conditional if ItemNode is now 0
        LoopBody.List += XTree_If.Create(
            [TestRefcountZero],
            [Invoke_SetLength],
            nil,
            FContext, FDocPos
        );

        Invoke_High := XTree_Invoke.Create(
                    XTree_Identifier.Create('High', FContext, FDocPos),
                    [], FContext, FDocPos
        );
        Invoke_High.SelfExpr := ObjectIdent;

        // Add the conditional loop to the body
        Body.List +=
          XTree_If.Create(
            [GetBinaryOp(op_EQ, RefcountNode, GetIntLiteral(0))],
            [XTree_For.Create(
              GetAssignment(Iterator, GetIntLiteral(0)),   // i := 0
              GetBinaryOp(op_LTE, Iterator, Invoke_High),  // i <= High
              GetAssignment(Iterator, GetBinaryOp(op_ADD, Iterator, GetIntLiteral(1))), // Inc(i)
              LoopBody, FContext, FDocPos
            )],
            nil, FContext, FDocPos // No else branch
          );

      end;

      // prepare call to ObjectIdent.setlength in case we need it
      Invoke_SetLength := XTree_Invoke.Create(
          XTree_Identifier.Create('SetLen', FContext, FDocPos),
          [GetIntLiteral(0)],
          FContext, FDocPos
      );
      Invoke_SetLength.SelfExpr := ObjectIdent;

      // Condition: RefcountNode = 0
      Body.List += XTree_If.Create(
        [GetBinaryOp(op_EQ, RefcountNode, GetIntLiteral(0))],
        [Invoke_SetLength],
        nil, FContext, FDocPos // No else branch
      );

      ProgramData.List += Body; // Add the entire if-block to the main program data
    end;

    xtRecord:
      // For records, iterate through fields and call Collect on managed fields.
      // The generated Record.Collect function just cleans up its _managed fields_.
      for i := 0 to XType_Record(SelfType).FieldTypes.High do
      begin
        FieldType := XType_Record(SelfType).FieldTypes.Data[i]; // Get field info

        // Check if the field's type is reference-counted (Array, String, Object, Interface)
        case FieldType.BaseType of
        xtArray:
          begin
            // Calculate address of the field within the record
            FieldPtr := XTree_Field.Create(ObjectIdent, XTree_Identifier.Create(XType_Record(SelfType).FieldNames.Data[i], FContext, FDocPos), FContext, FDocPos);

            Invoke_SetLength := XTree_Invoke.Create(
              XTree_Identifier.Create('SetLen', FContext, FDocPos),
              [GetIntLiteral(0)],
              FContext, FDocPos
            );
            Invoke_SetLength.SelfExpr := FieldPtr;

            ProgramData.List += Invoke_SetLength;
          end;
        xtRecord:
          begin
            FieldPtr := XTree_Field.Create(ObjectIdent, XTree_Identifier.Create(XType_Record(SelfType).FieldNames.Data[i], FContext, FDocPos), FContext, FDocPos);
            FieldPtr.FResType := FieldType; // Set result type to the record type

            // Call Collect for the record field (e.g., RecordVar.Field.Collect())
            Invoke_Collect := XTree_Invoke.Create(
                  XTree_Identifier.Create('Collect', FContext, FDocPos),
                  [], // No args, SelfExpr handles the target
                  FContext, FDocPos
            );
            Invoke_Collect.SelfExpr := FieldPtr;
            ProgramData.List += Invoke_Collect;
          end;
        end;
      end;
  end;

  Result := GenerateFunction('Collect', nil, nil, nil, nil, ProgramData);
  Result.SelfType := SelfType;
end;

function TTypeIntrinsics.GenerateSetLen(SelfType: XType; Args: array of XType): XTree_Function;
var
  ProgramData: XTree_ExprList;
  Iterator, ArrayIdent, ArrayIdentAsPtr, NewLength, DataSizeNode, Raw, SizeNode: XTree_Identifier;
  ElmntNode, RefcountNode, HighNode: XTree_Index;
  Invoke_Collect, Invoke_High: XTree_Invoke;
  ArrayRawData: XTree_BinaryOp;
  LocalVars, LocalIntVars: XIdentNodeList;
  LoopBody, ShrinkBody: XTree_ExprList;
  IfShrinking: XTree_If;

const
  ARRAY_HEADER_SIZE: Int32 = 2 * SizeOf(SizeInt);

begin
  if Length(Args) <= 0 then Exit(nil);
  if SelfType.BaseType <> xtArray then Exit(nil);

  ArrayIdent := XTree_Identifier.Create('Self', FContext, FDocPos);
  ArrayIdentAsPtr := XTree_Identifier.Create('Self', FContext, FDocPos);
  ArrayIdentAsPtr.FResType := FContext.GetType(xtPointer);

  NewLength  := XTree_Identifier.Create('NewLength', FContext, FDocPos);
  ProgramData := XTree_ExprList.Create([], FContext, FDocPos);

  LocalVars.Init([]);
  LocalIntVars.Init([]);

  Iterator    := XTree_Identifier.Create('!i', FContext, FDocPos);
  SizeNode    := XTree_Identifier.Create('!size', FContext, FDocPos);
  DataSizeNode := XTree_Identifier.Create('!newsize', FContext, FDocPos);

  LocalIntVars.Add(Iterator);
  LocalIntVars.Add(SizeNode);
  LocalIntVars.Add(DataSizeNode);

  ProgramData.List += XTree_VarDecl.Create(LocalIntVars, nil, FContext.GetType(xtInt), FContext, FDocPos);

  Raw := XTree_Identifier.Create('!raw', FContext, FDocPos);
  LocalVars.Add(Raw);
  ProgramData.List += XTree_VarDecl.Create(LocalVars, nil, FContext.GetType(xtPointer), FContext, FDocPos);


  ArrayRawData := XTree_BinaryOp.Create(op_SUB, ArrayIdent, GetIntLiteral(ARRAY_HEADER_SIZE), FContext, FDocPos);
  ArrayRawData.FResType := FContext.GetType(xtPointer);
  ProgramData.List += XTree_Assign.Create(op_asgn, Raw, ArrayRawData, FContext, FDocPos);


  Invoke_High := XTree_Invoke.Create(
      XTree_Identifier.Create('High', FContext, FDocPos),
      [], FContext, FDocPos
  );
  Invoke_High.SelfExpr := ArrayIdent;
  ProgramData.List += GetAssignment(SizeNode, Invoke_High);



  {IF SHRINKING}
  if (XType_Array(SelfType).ItemType is XType_Array) or
    ((XType_Array(SelfType).ItemType is XType_Record) and FContext.IsManagedRecord(XType_Array(SelfType).ItemType)) then
  begin
    // Get the element:
    ElmntNode := XTree_Index.Create(ArrayIdent, Iterator, FContext, FDocPos);

    // prepare call to ObjectIdent[!i].collect for child
    Invoke_Collect := XTree_Invoke.Create(
        XTree_Identifier.Create('Collect', FContext, FDocPos),
        [],
        FContext, FDocPos
    );
    Invoke_Collect.SelfExpr := ElmntNode;

    LoopBody := XTree_ExprList.Create([], FContext, FDocPos);
    LoopBody.List += Invoke_Collect;

    // Add the loop to the body
    ShrinkBody := XTree_ExprList.Create([], FContext, FDocPos);
    ShrinkBody.List += XTree_For.Create(
      GetAssignment(Iterator, NewLength),          // i := NewLength
      GetBinaryOp(op_LTE, Iterator, SizeNode),     // i <= High
      GetAssignment(Iterator, GetBinaryOp(op_ADD, Iterator, GetIntLiteral(1))), // Inc(i)
      LoopBody, FContext, FDocPos
    );

    IfShrinking := XTree_If.Create(
      [XTree_BinaryOp.Create(op_LT, NewLength, SizeNode, FContext, FDocPos)],
      [ShrinkBody],
      nil,
      FContext, FDocPos
    );

    ProgramData.List += IfShrinking;
  end;
  {ENDIF}

  ProgramData.List += XTree_If.Create(
    [XTree_BinaryOp.Create(op_NEQ, NewLength, GetIntLiteral(0), FContext, FDocPos)],
    [
      XTree_Assign.Create(op_Asgn,
        DataSizeNode,
        XTree_BinaryOp.Create(op_add,
           XTree_BinaryOp.Create(
             op_MUL,
             NewLength,
             GetIntLiteral(XType_Array(SelfType).ItemType.Size),
             FContext, FDocPos
           ),
           GetIntLiteral(ARRAY_HEADER_SIZE),
           FContext,
           FDocPos
         ),
         FContext,
         FDocPos
      )
    ],
    XTree_ExprList.Create( XTree_Assign.Create(op_Asgn, DataSizeNode, GetIntLiteral(0), FContext, FDocPos), FContext, FDocPos ),
    FContext, FDocPos
  );

  // sets size field, and refcount to 1 if new array
  ProgramData.List += XTree_Assign.Create(
    op_Asgn,
    Raw,
    XTree_Invoke.Create(
        XTree_Identifier.Create('AllocArray', FContext, FDocPos),
        [Raw, DataSizeNode, NewLength, GetIntLiteral(XType_Array(SelfType).ItemType.Size)],
        FContext, FDocPos
    ),
    FContext, FDocPos
  );


  ProgramData.List += XTree_Assign.Create(
        op_Asgn,
        ArrayIdentAsPtr,
        Raw,
        FContext, FDocPos
      );

  // Finalize) conditional assign result array
  ProgramData.List += XTree_If.Create(
    [XTree_BinaryOp.Create(op_NEQ, NewLength, GetIntLiteral(0), FContext, FDocPos)],
    [ XTree_ExprList.Create([
          XTree_Assign.Create(
            op_Asgn,
            ArrayIdentAsPtr,
            XTree_BinaryOp.Create(op_ADD, ArrayIdentAsPtr, GetIntLiteral(2*SizeOf(SizeInt)), FContext, FDocPos),
            FContext,
            FDocPos
          )
        ],
        FContext, FDocPos
      )
    ],
    nil,
    FContext, FDocPos
  );


  Result := GenerateFunction('SetLen', ['NewLength'], [pbCopy], [FContext.GetType(xtInt)], nil, ProgramData);
  Result.SelfType := SelfType;
end;


end.
