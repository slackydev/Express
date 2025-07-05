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


function GetPosValue(aPos: EMemPos; IsDest: Boolean): Integer;
begin
  Result := -1;
  if IsDest then
  begin
    // Destination can only be Local (0) or Global (1)
    case aPos of
      mpLocal:  Result := 0;
      mpGlobal: Result := 1;
      mpImm:    RaiseException('Destination argument cannot be immediate (mpImm).');
    end;
  end
  else
  begin
    // Source/Operand can be Local (0), Global (1), or Immediate (2)
    case aPos of
      mpLocal:  Result := 0;
      mpGlobal: Result := 1;
      mpImm:    Result := 2;
    end;
  end;
end;

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

  Result := EBytecode(Ord(BaseOpcodes[idx]) + TypeOffset[Ord(IR.Args[0].Typ) - Ord(xtInt8)]);
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
    end;
  end;



  // specialize
  for i := 0 to Intermediate.Code.Size - 1 do
  begin
    IR := Intermediate.Code.Data[i];

    FillChar(BCInstr, SizeOf(BCInstr), 0);

    case IR.Code of

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
          mpGlobal: BCInstr.Code :=bcJZ_g;
          mpImm:    BCInstr.Code :=bcJZ_i;
        end;

      icJNZ:
        case  ir.Args[0].Pos of
          mpLocal:  BCInstr.Code :=bcJNZ;
          mpGlobal: BCInstr.Code :=bcJNZ_g;
          mpImm:    BCInstr.Code :=bcJNZ_i;
        end;

      icJMP:
        BCInstr.Code := bcJMP;

      icRELJMP, icJBREAK, icJCONT, icJFUNC:
        BCInstr.Code := bcRELJMP;

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


  if canSpecialize then
  begin
    case Arg.Code of
      icADD: Exit(EncodeTernary(Arg, [bcADD_lll_i8, bcADD_lil_i8, bcADD_ill_i8, bcADD_iil_i8], [0,2,4,6, 1,3,5,7, 8,9]));
      icSUB: Exit(EncodeTernary(Arg, [bcSUB_lll_i8, bcSUB_lil_i8, bcSUB_ill_i8, bcSUB_iil_i8], [0,2,4,6, 1,3,5,7, 8,9]));
      icMUL: Exit(EncodeTernary(Arg, [bcMUL_lll_i8, bcMUL_lil_i8, bcMUL_ill_i8, bcMUL_iil_i8], [0,2,4,6, 1,3,5,7, 8,9]));
      icDIV: Exit(EncodeTernary(Arg, [bcDIV_lll_i8, bcDIV_lil_i8, bcDIV_ill_i8, bcDIV_iil_i8], [0,2,4,6, 1,3,5,7, 8,9]));
      icMOD: Exit(EncodeTernary(Arg, [bcMOD_lll_i8, bcMOD_lil_i8, bcMOD_ill_i8, bcMOD_iil_i8], [0,2,4,6, 1,3,5,7, 8,9]));
      icPOW: Exit(EncodeTernary(Arg, [bcPOW_lll_i8, bcPOW_lil_i8, bcPOW_ill_i8, bcPOW_iil_i8], [0,2,4,6, 1,3,5,7, 8,9]));

      icEQ:  Exit(EncodeTernary(Arg, [bcEQ_lll_i8, bcEQ_lil_i8, bcEQ_ill_i8, bcEQ_iil_i8],    [0,2,4,6, 1,3,5,7, 8,9]));
      icNEQ: Exit(EncodeTernary(Arg, [bcNE_lll_i8, bcNE_lil_i8, bcNE_ill_i8, bcNE_iil_i8],    [0,2,4,6, 1,3,5,7, 8,9]));
      icLT:  Exit(EncodeTernary(Arg, [bcLT_lll_i8, bcLT_lil_i8, bcLT_ill_i8, bcLT_iil_i8],    [0,2,4,6, 1,3,5,7, 8,9]));
      icGT:  Exit(EncodeTernary(Arg, [bcGT_lll_i8, bcGT_lil_i8, bcGT_ill_i8, bcGT_iil_i8],    [0,2,4,6, 1,3,5,7, 8,9]));
      icLTE: Exit(EncodeTernary(Arg, [bcLTE_lll_i8, bcLTE_lil_i8, bcLTE_ill_i8, bcLTE_iil_i8], [0,2,4,6, 1,3,5,7, 8,9]));
      icGTE: Exit(EncodeTernary(Arg, [bcGTE_lll_i8, bcGTE_lil_i8, bcGTE_ill_i8, bcGTE_iil_i8], [0,2,4,6, 1,3,5,7, 8,9]));

      icBND: Exit(EncodeTernary(Arg, [bcBND_lll_i8, bcBND_lil_i8, bcBND_ill_i8, bcBND_iil_i8], [0,2,4,6, 1,3,5,7]));
      icBOR: Exit(EncodeTernary(Arg, [bcBOR_lll_i8, bcBOR_lil_i8, bcBOR_ill_i8, bcBOR_iil_i8], [0,2,4,6, 1,3,5,7]));
      icXOR: Exit(EncodeTernary(Arg, [bcXOR_lll_i8, bcXOR_lil_i8, bcXOR_ill_i8, bcXOR_iil_i8], [0,2,4,6, 1,3,5,7]));
      icSHL: Exit(EncodeTernary(Arg, [bcSHL_lll_i8, bcSHL_lil_i8, bcSHL_ill_i8, bcSHL_iil_i8], [0,2,4,6, 1,3,5,7]));
      icSHR: Exit(EncodeTernary(Arg, [bcSHR_lll_i8, bcSHR_lil_i8, bcSHR_ill_i8, bcSHR_iil_i8], [0,2,4,6, 1,3,5,7]));
      icSAR: Exit(EncodeTernary(Arg, [bcSAR_lll_i8, bcSAR_lil_i8, bcSAR_ill_i8, bcSAR_iil_i8], [0,2,4,6, 1,3,5,7]));
    end;
  end else
  begin
    if Arg.Args[2].Typ in [xtSingle, xtDouble] then
    begin
      case Arg.Code of
        icADD: Result := bcFADD;
        icSUB: Result := bcFSUB;
        icMUL: Result := bcFMUL;
        icDIV: Result := bcFDIV;
        icMOD: Result := bcFMOD;
        icPOW: Result := bcFPOW;

        icEQ:  Result := bcFEQ;
        icNEQ: Result := bcFNEQ;
        icLT:  Result := bcFLT;
        icGT:  Result := bcFGT;
        icLTE: Result := bcFLTE;
        icGTE: Result := bcFGTE;
        else   Result := bcNOOP;
      end;
    end else
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
begin
  Result := bcNOOP;
  if (Arg.Args[0].Typ in XprOrdinalTypes+XprFloatTypes+XprPointerTypes) and (Arg.Args[0].Pos = mpLocal) then
  begin
    Arg.nArgs := 2;
    Result := op2instruct[op_Asgn][Arg.Args[0].Typ][Arg.Args[1].Typ];
    if Arg.Code = icMOVH then Inc(Result, Ord(bcMOVH)-Ord(bcMOV));

    // right hand shift
    case Arg.Args[1].Pos of
      mpLocal:  ;
      mpImm:    Inc(Result, 1);
      mpGlobal: Inc(Result, 2);
    end;
  end else
    case Arg.Code of
      icMOVH: Result := bcMOVH;
      icMOV:  Result := bcMOV;
    end;
end;

function TBytecodeEmitter.SpecializeFMA(Arg: TInstruction): EBytecode;
var
  Code: EBytecode;
  Idx: Integer;
const
  FMA_TypeTranslationOffset: array [xtInt8..xtDouble] of Int8 =
    (0,2,4,6, 1,3,5,7, 8,10); // Extended with offsets for floats if needed
begin
  Idx :=
    (Ord(Arg.Args[3].Pos = mpGlobal) shl 2) or
    (Ord(Arg.Args[2].Pos = mpGlobal) shl 1) or
     Ord(Arg.Args[0].Pos = mpGlobal);

  case Idx of
    0: Code := bcFMA_lll_i8;
    1: Code := bcFMA_llg_i8;
    2: Code := bcFMA_lgl_i8;
    3: Code := bcFMA_lgg_i8;
    4: Code := bcFMA_gll_i8;
    5: Code := bcFMA_glg_i8;
    6: Code := bcFMA_ggl_i8;
    7: Code := bcFMA_ggg_i8;
  else
    Code := bcNOOP;
  end;

  Result := EByteCode(Ord(Code) + FMA_TypeTranslationOffset[Arg.Args[0].Typ]);
end;

function TBytecodeEmitter.SpecializeDREF(Arg: TInstruction): EBytecode;
var
  Code: EBytecode;
  Size: Integer;
begin
  Code := bcNOOP;

  case Arg.Args[0].Pos of
    mpLocal:
      case Arg.Args[1].Pos of
        mpLocal:  Code := bcDREF_ll;
        mpGlobal: Code := bcDREF_lg;
      end;
    mpGlobal:
      case Arg.Args[1].Pos of
        mpLocal:  Code := bcDREF_gl;
        mpGlobal: Code := bcDREF_gg;
      end;
  end;

  Size := XprTypeSize[Arg.Args[0].Typ];
  case Size of
    4: Inc(Code, 4);
    8: Inc(Code, 8);
    // Optional: add handling for other sizes (1, 2, 16, etc)
  end;

  Result := Code;
end;

function TBytecodeEmitter.SpecializeJUMPS(Arg: TInstruction): EBytecode;
begin

end;


end.
