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

  EJumpKind = (jkRelative, jkAbsolute, jkAbsoluteLoad);

  TJumpZone = record
    JmpFrom: Int32;
    JmpTo: Int32;
    Kind: EJumpKind; // The new field
    // You could also store the argument index to patch (e.g., 0 or 1)
    // for even more robustness, but this is a great start.
  end;

  TBytecodeEmitter = record
    Intermediate: TIntermediateCode;
    Bytecode: TBytecode;
    Stack: TStackArray;
    UsedStackSize: SizeInt;

    // for rewrites
    JumpZones: specialize TArrayList<TJumpZone>;

    constructor New(IC: TIntermediateCode);

    procedure Compile();
    function SpecializeBINOP(Arg: TInstruction): EBytecode;
    function SpecializeMOV(var Arg: TInstruction): EBytecode;
    function SpecializeFMA(Arg: TInstruction): EBytecode;
    function SpecializeDREF(Arg: TInstruction): EBytecode;

    procedure Fuse();
    procedure Sweep();

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

  Result := EBytecode(Ord(BaseOpcodes[idx]) + TypeOffset[Ord(IR.Args[0].BaseType) - Ord(xtInt32)]);
end;





constructor TBytecodeEmitter.New(IC: TIntermediateCode);
begin
  Self.Intermediate  := IC;
  Self.UsedStackSize := IC.StackPosArr[0];
  Self.Bytecode.Init();
  SetLength(Self.Stack, STACK_SIZE);

  Self.Bytecode.FunctionTable := Self.Intermediate.FunctionTable;

  JumpZones.Init([]);
end;

procedure TBytecodeEmitter.Compile();
var
  IR: TInstruction;
  BCInstr: TBytecodeInstruction;
  i,j,k: Int32;
  Zone: TJumpZone;

begin
  // data allocation
  for i:=0 to Intermediate.Code.Size-2 do {last opcode is always RET}
  begin
    for j:=0 to Min(High(TInstruction.Args), Intermediate.Code.Data[i].nArgs-1) do
    begin
      // prepare constants, move them to imm
      if Intermediate.Code.Data[i].Args[j].Pos = mpConst then
      begin
        Intermediate.Code.Data[i].Args[j].Pos := mpImm;
        Intermediate.Code.Data[i].Args[j].Arg := Intermediate.Constants.Data[Intermediate.Code.Data[i].Args[j].Arg].val_i64;
      end;
    end;

    // Mark jump-ranges table
    case Intermediate.Code.Data[i].Code of
      icJMP:
        begin
          Zone.JmpFrom := i;
          Zone.JmpTo   := Intermediate.Code.Data[i].Args[0].Addr;
          Zone.Kind    := jkAbsolute; // This is an absolute jump
          JumpZones.Add(Zone);
        end;

      icRELJMP, icJBREAK, icJCONT, icJFUNC:
        begin
          Zone.JmpFrom := i;
          Zone.JmpTo   := i + Intermediate.Code.Data[i].Args[0].Addr;
          Zone.Kind    := jkRelative; // This is a relative jump
          JumpZones.Add(Zone);
        end;

      icJNZ, icJZ:
        begin
          Zone.JmpFrom := i;
          Zone.JmpTo   := i + Intermediate.Code.Data[i].Args[1].Addr;
          Zone.Kind    := jkRelative; // This is a conditional relative jump
          JumpZones.Add(Zone);
        end;
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

      icFILL:
        BCInstr.Code := bcFILL;

      icMOV, icMOVH:
        BCInstr.Code := SpecializeMOV(IR);

      icINCLOCK:
        BCInstr.Code := bcINCLOCK;
      icDECLOCK:
        BCInstr.Code := bcDECLOCK;

      icREFCNT:
        case ir.Args[1].Pos of
          mpLocal:  BCInstr.Code := bcREFCNT;
          mpImm:    BCInstr.Code := bcREFCNT_imm;
        end;

      icBCHK:
        BCInstr.Code := bcBCHK;

      icFMA:
        BCInstr.Code := SpecializeFMA(IR);

      icDREF:
        BCInstr.Code := SpecializeDREF(IR);

      // conditional jump, location in args[1]
      icJZ:
        case ir.Args[0].Pos of
          mpLocal:  BCInstr.Code :=bcJZ;
          mpImm:    BCInstr.Code :=bcJZ_i;
        end;

      icJNZ:
        case ir.Args[0].Pos of
          mpLocal:  BCInstr.Code := bcJNZ;
          mpImm:    BCInstr.Code := bcJNZ_i;
        end;

      // static jump
      icJMP:
        BCInstr.Code := bcJMP;

      // reljmp and aliases
      icRELJMP, icJBREAK, icJCONT:
        BCInstr.Code := bcRELJMP;

      icJFUNC:
        BCInstr.Code := bcRELJMP;

      icLOAD_GLOBAL:
        BCInstr.Code := bcLOAD_GLOBAL;

      icCOPY_GLOBAL:
        BCInstr.Code := bcCOPY_GLOBAL;

      icNEWFRAME:
        BCInstr.Code := bcNEWFRAME;

      icPUSH:
        BCInstr.Code := bcPUSH;

      icPUSHREF:
        BCInstr.Code := bcPUSHREF;

      icPOP:
        BCInstr.Code := bcPOP;

      icPOPH:
        BCInstr.Code := bcPOPH;

      icRET:
        BCInstr.Code := bcRET;

      // jump, location is unset, stored on stack, to find it match for:
      // icMOV stack_location, imm(jump_location)
      // JFUNC imm(skip_function)
      //
      // the MOV contains the function-start, and should be a table index.
      icINVOKE:
        BCInstr.Code := bcINVOKE;

      icINVOKEX:
        BCInstr.Code := bcINVOKEX;

      icIncTry: BCInstr.Code := bcIncTry;
      icDecTry: BCInstr.Code := bcDecTry;

      icPRINT:
        if IR.Args[0].BaseType in XprIntTypes+XprPointerTypes then
          BCInstr.Code := bcPRTi
        else if IR.Args[0].BaseType in XprBoolTypes then
          BCInstr.Code := bcPRTb
        else if IR.Args[0].BaseType in XprFloatTypes then
          BCInstr.Code := bcPRTf;


      // etc.
    else
      BCInstr.Code := bcNOOP;
    end;

    if (IR.Code = icADD) and
       (IR.Args[0].Pos = mpLocal) and (IR.Args[1].Pos = mpImm) and (IR.Args[2].Pos = mpLocal) and
       (IR.Args[1].Arg = 1) and (IR.Args[0].Arg = IR.Args[2].Arg) then
       case IR.Args[0].BaseType of
         xtInt32: BCInstr.Code := bcINC_i32;
         xtInt64: BCInstr.Code := bcINC_i64;
         xtUInt32:BCInstr.Code := bcINC_u32;
         xtUInt64:BCInstr.Code := bcINC_u64;
       end;

    BCInstr.nArgs := IR.nArgs;
    for k:=0 to High(IR.Args) do
    begin
      BCInstr.Args[k].BaseType := IR.Args[k].BaseType;
      BCInstr.Args[k].Pos      := IR.Args[k].Pos;
      BCInstr.Args[k].Data.Arg := IR.Args[k].Arg;
    end;

    Bytecode.Code.Add(BCInstr);
    Bytecode.Docpos.Add(Intermediate.DocPos.Data[i]);
  end;

  Fuse();
  Sweep();
end;




function TBytecodeEmitter.SpecializeBinop(Arg: TInstruction): EBytecode;
var
  canSpecialize: Boolean;
begin
  Result := bcNOOP;
  // 2       0      1
  // dest := left . right
  canSpecialize := (Arg.Args[2].Pos = mpLocal) and (Arg.Args[0].Pos in [mpLocal, mpImm]) and (Arg.Args[1].Pos in [mpLocal, mpImm]);


  if canSpecialize and (XprTypeSize[ Arg.Args[0].BaseType ] >= 4) then
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
  if (Arg.Args[0].BaseType in XprOrdinalTypes+XprFloatTypes+XprPointerTypes) and (Arg.Args[0].Pos = mpLocal) then
  begin
    leftType  := Arg.Args[0].BaseType;
    rightType := Arg.Args[1].BaseType;
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
  if Arg.Args[0].Pos = mpLocal then
    Result := EByteCode(Ord(bcFMA_i8) + FMA_TypeTranslationOffset[Arg.Args[0].BaseType])
  else
    Result := EByteCode(Ord(bcFMA_imm_i8) + FMA_TypeTranslationOffset[Arg.Args[0].BaseType])
end;

function TBytecodeEmitter.SpecializeDREF(Arg: TInstruction): EBytecode;
begin
  case XprTypeSize[Arg.Args[0].BaseType] of
    4: Result := bcDREF_32;
    8: Result := bcDREF_64;
  else
    Result := bcDREF;
  end;
end;

procedure TBytecodeEmitter.Fuse();
var
  i, removals: Int32;
begin
  removals := 0;
  for i:=0 to Bytecode.Code.Size-2 do
  begin
    if not (Bytecode.Code.Data[i+1].Args[0].BaseType in XprSimpleTypes) then
      Continue;

    if (Bytecode.Code.Data[i+0].Code = bcFMA_i64) and (Bytecode.Code.Data[i+1].Code = bcDREF_64) then
    begin
      Bytecode.Code.Data[i+0].Code := bcFMAD_d64_64;
      Bytecode.Code.Data[i+0].Args[3] := Bytecode.Code.Data[i+1].Args[0];
      Bytecode.Code.Data[i+1].Code := bcERROR;
      Inc(removals);
    end;

    if (Bytecode.Code.Data[i+0].Code = bcFMA_i64) and (Bytecode.Code.Data[i+1].Code = bcDREF_32)then
    begin
      Bytecode.Code.Data[i+0].Code := bcFMAD_d32_64;
      Bytecode.Code.Data[i+0].Args[3] := Bytecode.Code.Data[i+1].Args[0];
      Bytecode.Code.Data[i+1].Code := bcERROR;
      Inc(removals);
    end;

    if (Bytecode.Code.Data[i+0].Code = bcFMA_i32) and (Bytecode.Code.Data[i+1].Code = bcDREF_64)then
    begin
      Bytecode.Code.Data[i+0].Code := bcFMAD_d64_32;
      Bytecode.Code.Data[i+0].Args[3] := Bytecode.Code.Data[i+1].Args[0];
      Bytecode.Code.Data[i+1].Code := bcERROR;
      Inc(removals);
    end;

    if (Bytecode.Code.Data[i+0].Code = bcFMA_i32) and (Bytecode.Code.Data[i+1].Code = bcDREF_32)then
    begin
      Bytecode.Code.Data[i+0].Code := bcFMAD_d32_32;
      Bytecode.Code.Data[i+0].Args[3] := Bytecode.Code.Data[i+1].Args[0];
      Bytecode.Code.Data[i+1].Code := bcERROR;
      Inc(removals);
    end;
  end;
end;

procedure TBytecodeEmitter.Sweep();
var
  NewIndex: array of Int32;
  removalCount, i: Int32;
  zone: TJumpZone;
  instr: ^TBytecodeInstruction;
  newRelativeOffset, newAbsoluteOffset: Int32;
  funcEntry: TFunctionEntry;
begin
  if Bytecode.Code.Size = 0 then Exit;

  // Step 1: Precompute instruction mapping table (Correct)
  SetLength(NewIndex, Bytecode.Code.Size);
  removalCount := 0;
  for i := 0 to Bytecode.Code.High do
  begin
    if Bytecode.Code.Data[i].Code = bcERROR then
    begin
      NewIndex[i] := -1; // Mark as removed
      Inc(removalCount);
    end
    else
      NewIndex[i] := i - removalCount; // Map old index to new compacted index
  end;

  // Step 2: Process jump zones to adjust targets
  for i := 0 to JumpZones.High do
  begin
    zone := JumpZones.Data[i];

    // If the jump source or target was removed, skip this jump zone
    if (NewIndex[zone.JmpFrom] = -1) or (NewIndex[zone.JmpTo] = -1) then
      Continue;

    instr := @Bytecode.Code.Data[zone.JmpFrom];
    newAbsoluteOffset := NewIndex[zone.JmpTo];

    case zone.Kind of
      jkAbsolute:
        instr^.Args[0].Data.Arg := newAbsoluteOffset;

      jkRelative:
      begin
        newRelativeOffset := newAbsoluteOffset - NewIndex[zone.JmpFrom];

        case instr^.Code of
          bcRELJMP:
            instr^.Args[0].Data.Arg := newRelativeOffset;

          bcJZ, bcJNZ, bcJZ_i, bcJNZ_i:
            instr^.Args[1].Data.Arg := newRelativeOffset;
        end;
      end;
    end;
  end;

  // Step 3: Update FunctionTable.CodeLocation
  // This is crucial for delayed-compiled functions.
  // Their CodeLocation stored in the FunctionTable points to their start in Intermediate Code.
  // We need to map these to their new locations in the compacted Bytecode.
  for i := 0 to High(Bytecode.FunctionTable) do
  begin
    funcEntry := Bytecode.FunctionTable[i];
    // funcEntry.CodeLocation holds the *original* intermediate code index.
    // If this original index maps to a removed instruction (-1), it's an error or dead code.
    // Otherwise, update it to the new, compacted bytecode index.
    if NewIndex[funcEntry.CodeLocation] <> -1 then
      funcEntry.CodeLocation := NewIndex[funcEntry.CodeLocation]
    else
      // Handle case where function entry might point to removed code (e.g., unreachable functions)
      // This might imply setting CodeLocation to an invalid value or a special "null" entry.
      // For now, let's assume valid locations. If a function is truly removed, it probably
      // shouldn't be in the FunctionTable to begin with, or its entry will point to -1.
      // A more robust system might remove the entry from the table or mark it as invalid.
      // For now, we'll let it point to -1 if it was removed, which the interpreter should handle.
      funcEntry.CodeLocation := -1; // Or some other appropriate "invalid" marker.

    Bytecode.FunctionTable[i] := funcEntry; // Assign back the updated record
  end;


  // Step 4: Remove dead instructions (Correct)
  for i := Bytecode.Code.High downto 0 do
    if Bytecode.Code.Data[i].Code = bcERROR then
    begin
      Bytecode.Code.Delete(i);
      Bytecode.Docpos.Delete(i);
    end;
end;

end.


end.

