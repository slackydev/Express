unit xprBytecodeEmitter;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Compiles the generic intermediate code into a standalone fully allocated
  executable bytecode.

  Everything should be ready for run after this.
  Stack is allocated, constants are populated.
}
{$I header.inc}
{$hints off}

interface

uses
  SysUtils, xprTypes, xprBytecode, xprIntermediate, xprErrors;

const
  STACK_SIZE = 16 * 1024 * 1024; // 16MB stacksize

type
  EOptimizerFlag = (optCmpFlag, optSpecializeExpr);
  EOptimizerFlags = set of EOptimizerFlag;
  TStackArray = array of Byte;

  TBytecodeEmitter = record
    Intermediate: TIntermediateCode;
    Bytecode: TBytecode;
    Stack: TStackArray;
    UsedStackSize: SizeInt;

    // for rewrites
    JumpTable: specialize TArrayList<PtrInt>;

    constructor New(IC: TIntermediateCode);

    procedure Compile();
    function SpecializeBINOP(Arg: TInstruction): EBytecode;
    function SpecializeMOV(var Arg: TInstruction): EBytecode;
    function SpecializeFMA(Arg: TInstruction): EBytecode;
    function SpecializeDREF(Arg: TInstruction): EBytecode;
    function SpecializeJUMPS(Arg: TInstruction): EBytecode;

  end;

implementation

uses
  Math, xprLangdef;


// --- Encoding Functions ---

function EncodeTernary(
  const IR: TInstruction;
  const BaseOpcodes: array of EBytecode;
  const TypeOffset: array of Int32
): EBytecode;
var
  idx: Integer;
  src1, src2: Integer;

  function MapPos(Pos: EMemPos): Integer;
  begin
    Result := 0;
    case Pos of
      mpLocal: Result := 0;
      mpImm:   Result := 1;
    else
      RaiseException('Ternary mapping error');
    end;
  end;
begin
  src1 := MapPos(IR.Args[0].Pos);
  src2 := MapPos(IR.Args[1].Pos);
  idx := (src1 * 2) + src2;
  WriteLn(idx,'>>>', TypeOffset[Ord(IR.Args[0].Typ) - Ord(xtInt32)]);

  Result := EBytecode(Ord(BaseOpcodes[idx]) + TypeOffset[Ord(IR.Args[0].Typ) - Ord(xtInt32)]);
end;





constructor TBytecodeEmitter.New(IC: TIntermediateCode);
begin
  Self.Intermediate  := IC;
  Self.UsedStackSize := IC.StackPosArr[0];
  Self.Bytecode.Init();
  SetLength(Self.Stack, STACK_SIZE);
end;

procedure TBytecodeEmitter.Compile();
var
  IR: TInstruction;
  BCInstr: TBytecodeInstruction;
  i,j: Int32;
  heapptr: PtrUInt;
begin
  // data allocation
  for i:=0 to Intermediate.Code.Size-1 do
  begin
    for j:=0 to Intermediate.Code.Data[i].nArgs-1 do
    begin
      // prepare constants, move them to imm
      if Intermediate.Code.Data[i].Args[j].Pos = mpConst then
      begin
        Intermediate.Code.Data[i].Args[j].Pos := mpImm;
        Intermediate.Code.Data[i].Args[j].Arg := Intermediate.Constants.Data[Intermediate.Code.Data[i].Args[j].Arg].val_u64;
      end;
       (*
      // allocate dataspace for globals, link in stack
      if Intermediate.Code.Data[i].Args[j].Pos = mpGlobal then
      begin
        heapptr := PtrUInt(AllocMem(XprTypeSize[Intermediate.Code.Data[i].Args[j].Typ]));

        Move(
          heapptr,
          Stack[Intermediate.Code.Data[i].Args[j].Arg],
          SizeOf(Pointer)
        );
      end;
      *)
    end;
  end;



  // specialize
  for i := 0 to Intermediate.Code.Size - 1 do
  begin
    IR := Intermediate.Code.Data[i];

    FillChar(BCInstr, SizeOf(BCInstr), 0);

    case IR.Code of
      icNOOP:
        BCInstr.Code := bcNOOP;

      icADD, icSUB, icMUL, icDIV, icMOD,
      icBND, icBOR, icSHL, icSHR, icSAR, icXOR,
      icEQ, icNEQ, icLT, icLTE, icGT, icGTE:
        BCInstr.Code := SpecializeBinop(IR);

      icMOV, icMOVH:
        BCInstr.Code := SpecializeMOV(IR);

      icFMA:
        BCInstr.Code := SpecializeFMA(IR);

      icDREF:
        BCInstr.Code := SpecializeDREF(IR);

      icJZ:
        case  ir.Args[0].Pos of
          mpLocal:  BCInstr.Code :=bcJZ;
          mpImm:    BCInstr.Code :=bcJZ_i;
        end;

      icJNZ:
        case  ir.Args[0].Pos of
          mpLocal:  BCInstr.Code :=bcJNZ;
          mpImm:    BCInstr.Code :=bcJNZ_i;
        end;

      icJMP:
        BCInstr.Code := bcJMP;

      icRELJMP, icJBREAK, icJCONT, icJFUNC:
        BCInstr.Code := bcRELJMP;

      icLOAD_GLOBAL:
        BCInstr.Code := bcLOAD_GLOBAL;

      icNEWFRAME:
        BCInstr.Code := bcNEWFRAME;

      icPUSH:
        BCInstr.Code := bcPUSH;

      icPOP:
        BCInstr.Code := bcPOP;

      icPOPH:
        BCInstr.Code := bcPOPH;

      icRET:
        BCInstr.Code := bcRET;

      icINVOKE:
        BCInstr.Code := bcINVOKE;

      icINVOKEX:
        BCInstr.Code := bcINVOKEX;

      icIncTry: BCInstr.Code := bcIncTry;
      icDecTry: BCInstr.Code := bcDecTry;

      icPRINT:
        if IR.Args[0].Typ in XprIntTypes+XprPointerTypes then
          BCInstr.Code := bcPRTi
        else if IR.Args[0].Typ in XprBoolTypes then
          BCInstr.Code := bcPRTb
        else if IR.Args[0].Typ in XprFloatTypes then
          BCInstr.Code := bcPRTf;


      // etc.
    else
      BCInstr.Code := bcNOOP;
    end;

    BCInstr.nArgs := IR.nArgs;
    Move(IR.Args, BCInstr.Args, SizeOf(IR.Args));

    Bytecode.Code.Add(BCInstr);
    Bytecode.Docpos.Add(Intermediate.DocPos.Data[i]);
  end;
end;







function TBytecodeEmitter.SpecializeBinop(Arg: TInstruction): EBytecode;
var
  canSpecialize: Boolean;
begin
  Result := bcNOOP;
  // 2       0      1
  // dest := left . right
  canSpecialize := (Arg.Args[2].Pos = mpLocal) and (Arg.Args[0].Pos in [mpLocal, mpImm]) and (Arg.Args[1].Pos in [mpLocal, mpImm]);


  if canSpecialize and (XprTypeSize[ Arg.Args[0].Typ ] >= 4) then
  begin
    case Arg.Code of
      icADD: Exit(EncodeTernary(Arg, [bcADD_lll_i32, bcADD_lil_i32, bcADD_ill_i32, bcADD_iil_i32], [0,2,0,0, 1,3, 4,5]));
      icSUB: Exit(EncodeTernary(Arg, [bcSUB_lll_i32, bcSUB_lil_i32, bcSUB_ill_i32, bcSUB_iil_i32], [0,2,0,0, 1,3, 4,5]));
      icMUL: Exit(EncodeTernary(Arg, [bcMUL_lll_i32, bcMUL_lil_i32, bcMUL_ill_i32, bcMUL_iil_i32], [0,2,0,0, 1,3, 4,5]));
      icDIV: Exit(EncodeTernary(Arg, [bcDIV_lll_i32, bcDIV_lil_i32, bcDIV_ill_i32, bcDIV_iil_i32], [0,2,0,0, 1,3, 4,5]));
      icMOD: Exit(EncodeTernary(Arg, [bcMOD_lll_i32, bcMOD_lil_i32, bcMOD_ill_i32, bcMOD_iil_i32], [0,2,0,0, 1,3, 4,5]));
      icPOW: Exit(EncodeTernary(Arg, [bcPOW_lll_i32, bcPOW_lil_i32, bcPOW_ill_i32, bcPOW_iil_i32], [0,2,0,0, 1,3, 4,5]));

      icEQ:  Exit(EncodeTernary(Arg, [bcEQ_lll_i32, bcEQ_lil_i32, bcEQ_ill_i32, bcEQ_iil_i32],  [0,2,0,0, 1,3, 4,5]));
      icNEQ: Exit(EncodeTernary(Arg, [bcNE_lll_i32, bcNE_lil_i32, bcNE_ill_i32, bcNE_iil_i32],  [0,2,0,0, 1,3, 4,5]));
      icLT:  Exit(EncodeTernary(Arg, [bcLT_lll_i32, bcLT_lil_i32, bcLT_ill_i32, bcLT_iil_i32],  [0,2,0,0, 1,3, 4,5]));
      icGT:  Exit(EncodeTernary(Arg, [bcGT_lll_i32, bcGT_lil_i32, bcGT_ill_i32, bcGT_iil_i32],  [0,2,0,0, 1,3, 4,5]));
      icLTE: Exit(EncodeTernary(Arg, [bcLTE_lll_i32, bcLTE_lil_i32, bcLTE_ill_i32, bcLTE_iil_i32], [0,2,0,0, 1,3, 4,5]));
      icGTE: Exit(EncodeTernary(Arg, [bcGTE_lll_i32, bcGTE_lil_i32, bcGTE_ill_i32, bcGTE_iil_i32], [0,2,0,0, 1,3, 4,5]));

      icBND: Exit(EncodeTernary(Arg, [bcBND_lll_i32, bcBND_lil_i32, bcBND_ill_i32, bcBND_iil_i32], [0,2,0,0, 1,3]));
      icBOR: Exit(EncodeTernary(Arg, [bcBOR_lll_i32, bcBOR_lil_i32, bcBOR_ill_i32, bcBOR_iil_i32], [0,2,0,0, 1,3]));
      icXOR: Exit(EncodeTernary(Arg, [bcXOR_lll_i32, bcXOR_lil_i32, bcXOR_ill_i32, bcXOR_iil_i32], [0,2,0,0, 1,3]));
      icSHL: Exit(EncodeTernary(Arg, [bcSHL_lll_i32, bcSHL_lil_i32, bcSHL_ill_i32, bcSHL_iil_i32], [0,2,0,0, 1,3]));
      icSHR: Exit(EncodeTernary(Arg, [bcSHR_lll_i32, bcSHR_lil_i32, bcSHR_ill_i32, bcSHR_iil_i32], [0,2,0,0, 1,3]));
      icSAR: Exit(EncodeTernary(Arg, [bcSAR_lll_i32, bcSAR_lil_i32, bcSAR_ill_i32, bcSAR_iil_i32], [0,2,0,0, 1,3]));
    end;
  end else
  begin
      case Arg.Code of
        icADD: Result := bcADD;
        icSUB: Result := bcSUB;
        icMUL: Result := bcMUL;
        icDIV: Result := bcDIV;
        icMOD: Result := bcMOD;
        icPOW: Result := bcPOW;

        icEQ:  Result := bcEQ;
        icNEQ: Result := bcNE;
        icLT:  Result := bcLT;
        icGT:  Result := bcGT;
        icLTE: Result := bcLTE;
        icGTE: Result := bcGTE;

        icBND: Result := bcBND;
        icBOR: Result := bcBOR;
        icXOR: Result := bcXOR;
        icSHL: Result := bcSHL;
        icSHR: Result := bcSHR;
        icSAR: Result := bcSAR;
        else   Result := bcNOOP;
      end;
  end;
end;

function TBytecodeEmitter.SpecializeMOV(var Arg: TInstruction): EBytecode;
var leftType, rightType: EExpressBaseType;
begin
  Result := bcNOOP;
  if (Arg.Args[0].Typ in XprOrdinalTypes+XprFloatTypes+XprPointerTypes) and (Arg.Args[0].Pos = mpLocal) then
  begin
    leftType  := Arg.Args[0].Typ;
    rightType := Arg.Args[1].Typ;
    if leftType in XprPointerTypes  then leftType  := xtInt;
    if rightType in XprPointerTypes then rightType := xtInt;

    Arg.nArgs := 2;
    Result := op2instruct[op_Asgn][leftType][rightType];
    if Arg.Code = icMOVH then Inc(Result, Ord(bcMOVH)-Ord(bcMOV));

    // right hand shift
    case Arg.Args[1].Pos of
      mpLocal:  ;
      mpImm:    Inc(Result, 1);
    end;
  end else
    case Arg.Code of
      icMOVH: Result := bcMOVH;
      icMOV:  Result := bcMOV;
    end;
end;

function TBytecodeEmitter.SpecializeFMA(Arg: TInstruction): EBytecode;
const
  FMA_TypeTranslationOffset: array [xtInt8..xtDouble] of Int8 =
    (0,2,4,6, 1,3,5,7, 8,10); // Extended with offsets for floats if needed
begin
  Result := EByteCode(Ord(bcFMA_i8) + FMA_TypeTranslationOffset[Arg.Args[0].Typ]);
end;

function TBytecodeEmitter.SpecializeDREF(Arg: TInstruction): EBytecode;
begin
  case XprTypeSize[Arg.Args[0].Typ] of
    4: Result := bcDREF_32;
    8: Result := bcDREF_64;
  else
    Result := bcDREF;
  end;
end;

function TBytecodeEmitter.SpecializeJUMPS(Arg: TInstruction): EBytecode;
begin

end;


end.

