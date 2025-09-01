unit xpr.BytecodeEmitter;
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
  SysUtils, xpr.Types, xpr.Bytecode, xpr.Intermediate, xpr.Errors,
  xpr.Dictionary;

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


  TScopeRanges   = specialize TArrayList<TJumpZone>; // global..global-end, func1..end, func2..end
  TVarLocations  = specialize TArrayList<Int64>;     // stores all references to a local variable
  TLocals        = specialize TDictionary<int64, TVarLocations>; //local[x] = List([x,y,z])
  TScopeLocals   = specialize TArrayList<TLocals>;


  TBytecodeEmitter = record
    Intermediate: TIntermediateCode;
    Bytecode: TBytecode;
    Stack: TStackArray;
    UsedStackSize: SizeInt;

    // for rewrites and optimizations
    JumpZones: specialize TArrayList<TJumpZone>;
    JumpSites: array of Boolean;

    constructor New(IC: TIntermediateCode);

    procedure Compile();
    procedure BuildJumpZones();
    function SameData(x, y: TInstructionData): Boolean;

    function SpecializeBINOP(Arg: TInstruction): EBytecode;
    function SpecializeMOV(var Arg: TInstruction): EBytecode;
    function SpecializeFMA(Arg: TInstruction): EBytecode;
    function SpecializeDREF(Arg: TInstruction): EBytecode;

    procedure GetScopes(out ALocals: TScopeLocals; out AZones: TScopeRanges);
    function IsBasicInstruction(i: Int32): Boolean;
    procedure ConstantFolding();
    procedure CommonSubexpressionElimination();
    procedure CopyPropagation();
    procedure DeadStoreElimination();

    procedure Fuse();
    procedure Sweep();

  end;

implementation

uses
  Math, Variants,
  xpr.Langdef;



{$I interpreter.functions.inc}

// --- Encoding Functions ---

function EncodeTernary(
  const IR: TInstruction;
  const BaseOpcodes: array of EBytecode;
  const TypeOffset: array of Int32
): EBytecode;
var
  idx: Integer;
  src1, src2: Integer;
  basetype: EExpressBaseType;

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

  // rewrite pointer to comparable integer (if earlie stage didnt convert)
  basetype := IR.Args[0].BaseType;
  if basetype in XprPointerTypes then basetype := BaseIntType(basetype);

  Result := EBytecode(Ord(BaseOpcodes[idx]) + TypeOffset[Ord(basetype) - Ord(xtInt32)]);
end;



constructor TBytecodeEmitter.New(IC: TIntermediateCode);
begin
  Self.Intermediate  := IC;
  Self.UsedStackSize := IC.StackPosArr[0];
  Self.Bytecode.Init();
  SetLength(Self.Stack, STACK_SIZE);

  Self.Bytecode.FunctionTable := Self.Intermediate.FunctionTable;
  Self.Bytecode.StringTable   := Self.Intermediate.StringTable;
  Self.Bytecode.ClassVMTs     := Self.Intermediate.ClassVMTs;

  JumpZones.Init([]);
end;

procedure TBytecodeEmitter.Compile();
var
  IR: TInstruction;
  BCInstr: TBytecodeInstruction;
  i,j,k: Int32;
  Zone: TJumpZone;

begin
  Self.BuildJumpZones();

  // --- STEP 1: PERFORM OPTIMIZATIONS ON THE GENERIC INTERMEDIATE CODE ---

  //Self.ConstantFolding();
  //Self.CopyPropagation(); //broken
  //Self.CommonSubexpressionElimination(); //broken (?)
  //Self.CopyPropagation(); //borken
  //Self.DeadStoreElimination();

  // --- STEP 2: PREPARE JUMPS and CONSTANTS ---
  for i:=0 to Intermediate.Code.Size-2 do {last opcode is always RET}
  begin
    for j:=0 to Min(High(TInstruction.Args), Intermediate.Code.Data[i].nArgs-1) do
    begin
      // prepare constants, move them to imm
      if  (Intermediate.Code.Data[i].Args[j].Pos = mpConst) then
      begin
        Intermediate.Code.Data[i].Args[j].Pos := mpImm;
        if not(Intermediate.Code.Data[i].Args[j].BaseType in XprStringTypes) then
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


  // --- STEP 3: SPECIALIZE AND EMIT FINAL BYTECODE ---
  for i := 0 to Intermediate.Code.Size - 1 do
  begin
    IR := Intermediate.Code.Data[i];

    FillChar(BCInstr, SizeOf(BCInstr), 0);

    case IR.Code of
      icNOOP:
        BCInstr.Code := bcNOOP;

      icERROR:
        BCInstr.Code := bcERROR;

      icADD, icSUB, icMUL, icDIV, icMOD,
      icBND, icBOR, icSHL, icSHR, icSAR, icXOR,
      icEQ, icNEQ, icLT, icLTE, icGT, icGTE:
        BCInstr.Code := SpecializeBinop(IR);

      icNEW:
        BCInstr.Code := bcNEW;

      icRELEASE:
        BCInstr.Code := bcRELEASE;

      icDYNCAST:
        BCInstr.Code := bcDYNCAST;

      icIS:
        BCInstr.Code := bcIS;

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

      icADDR:
        BCInstr.Code := bcADDR;

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

      icLOAD_NONLOCAL:
        BCInstr.Code := bcLOAD_NONLOCAL;

      icCOPY_GLOBAL:
        BCInstr.Code := bcCOPY_GLOBAL;

      icNEWFRAME:
        BCInstr.Code := bcNEWFRAME;

      icPUSH:
        BCInstr.Code := bcPUSH;

      icPUSHREF:
        BCInstr.Code := bcPUSHREF;

      icPUSH_FP:
        BCInstr.Code := bcPUSH_FP;

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

      icINVOKE_VIRTUAL:
        BCInstr.Code := bcINVOKE_VIRTUAL;

      icSET_ERRHANDLER: BCInstr.Code := bcSET_ERRHANDLER;
      icRAISE:  BCInstr.Code := bcRAISE;
      icGET_EXCEPTION: BCInstr.Code := bcGET_EXCEPTION;
      icIncTry: BCInstr.Code := bcIncTry;
      icDecTry: BCInstr.Code := bcDecTry;

      icPRINT:
        if IR.Args[0].BaseType in XprIntTypes+XprCharTypes+XprPointerTypes-XprStringTypes then
          BCInstr.Code := bcPRTi
        else if IR.Args[0].BaseType in XprBoolTypes then
          BCInstr.Code := bcPRTb
        else if IR.Args[0].BaseType in XprFloatTypes then
          BCInstr.Code := bcPRTf
        else if IR.Args[0].BaseType in XprStringTypes then
          BCInstr.Code := bcPRT;


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

function TBytecodeEmitter.SameData(x, y: TInstructionData): Boolean;
begin
  if not((x.BaseType in XprNumericTypes) and (y.BaseType in XprNumericTypes)) then Exit(False);
  if (x.Pos <> y.Pos) then Exit(False);
  if x.Pos = mpConst then
    Exit(CompareMem(
      @Intermediate.Constants.Data[x.Arg].raw,
      @Intermediate.Constants.Data[y.Arg].raw,
      XprTypeSize[Intermediate.Constants.Data[x.Arg].typ]
    ))
  else
    Exit(CompareMem(@x.Arg, @y.Arg, XprTypeSize[x.BaseType]));
end;


procedure TBytecodeEmitter.BuildJumpZones();
var
  i: Int32;
  Zone: TJumpZone;
begin
  SetLength(JumpSites, Intermediate.Code.Size);
  for i:=0 to Intermediate.Code.Size-2 do {last opcode is always RET}
  begin
    // Mark jump-ranges table
    case Intermediate.Code.Data[i].Code of
      icJMP:
        begin
          Zone.JmpFrom := i;
          Zone.JmpTo   := Intermediate.Code.Data[i].Args[0].Addr;
          Zone.Kind    := jkAbsolute; // This is an absolute jump
          JumpZones.Add(Zone);
          JumpSites[Zone.JmpFrom] := True;
          JumpSites[Zone.JmpTo]   := True;
        end;

      icRELJMP, icJBREAK, icJCONT, icJFUNC:
        begin
          Zone.JmpFrom := i;
          Zone.JmpTo   := i + Intermediate.Code.Data[i].Args[0].Addr;
          Zone.Kind    := jkRelative; // This is a relative jump
          JumpZones.Add(Zone);
          JumpSites[Zone.JmpFrom] := True;
          JumpSites[Zone.JmpTo]   := True;
        end;

      icJNZ, icJZ:
        begin
          Zone.JmpFrom := i;
          Zone.JmpTo   := i + Intermediate.Code.Data[i].Args[1].Addr;
          Zone.Kind    := jkRelative; // This is a conditional relative jump
          JumpZones.Add(Zone);
          JumpSites[Zone.JmpFrom] := True;
          JumpSites[Zone.JmpTo]   := True;
        end;
    end;
  end;
end;

function TBytecodeEmitter.SpecializeBinop(Arg: TInstruction): EBytecode;
var
  canSpecialize: Boolean;
begin
  Result := bcNOOP;

  // handle strings:
  if  (Arg.Args[0].BaseType = xtAnsiString)
  and (Arg.Args[1].BaseType = xtAnsiString)
  and (Arg.Args[2].BaseType = xtAnsiString)  then
  begin
    Result := bcADD_STR;
    Exit;
  end;

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
  Result := bcNOOP;  // should raise if we end up with NOOP


  // strings imm are table lookups, handle magic!
  if (Arg.Args[0].BaseType in XprStringTypes) and (Arg.Args[1].BaseType in XprStringTypes) and (Arg.Args[1].Pos = mpImm) then
  begin
    Result := bcLOAD_STR;
    Exit;
  end;

  if (Arg.Args[0].BaseType in XprOrdinalTypes+XprFloatTypes+XprPointerTypes) and (Arg.Args[0].Pos = mpLocal) then
  begin
    leftType  := Arg.Args[0].BaseType;
    rightType := Arg.Args[1].BaseType;
    if leftType  in XprPointerTypes+XprOrdinalTypes then leftType  := BaseIntType(leftType);
    if rightType in XprPointerTypes+XprOrdinalTypes then rightType := BaseIntType(rightType);

    Arg.nArgs := 2;
    Result := op2instruct[op_Asgn][leftType][rightType];
    if Arg.Code = icMOVH then Inc(Result, Ord(bcMOVH)-Ord(bcMOV));

    // right hand shift
    if Arg.Args[1].Pos = mpImm then
      Inc(Result, 1);
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


function BinaryOp(Code:EIntermediate; Left, Right: Variant; BaseType: EExpressBaseType): Variant;
begin
  case Code of
    icADD: Result := Left + Right;
    icSUB: Result := Left - Right;
    icMUL: Result := Left * Right;
    icDIV:
      if BaseType in [xtInt8..xtInt64, xtUInt8..xtUInt64] then
        Result := Left div Right
      else
        Result := Left / Right;
    icMOD:
      if BaseType in [xtInt8..xtInt64] then
        Result := Modulo(Int64(Left), Int64(Right))
      else if BaseType in [xtUInt8..xtUInt64] then
        Result := UInt64(Left) mod UInt64(Right)
      else if BaseType in [xtSingle..xtDouble] then
        Result := Modulo(Double(Left), Double(Right))
      else
        Exit(Null);
    icPOW:
      if BaseType in [xtInt8..xtInt64] then
        Result := ipow(Int64(Left), Int64(Right))
      else if BaseType in [xtUInt8..xtUInt64] then
        Result := ipow(UInt64(Left), UInt64(Right))
      else if BaseType in [xtSingle..xtDouble] then
        Result := Power(Double(Left), Double(Right))
      else
        Exit(Null);
    icEQ:  Result := (Left =  Right);
    icNEQ: Result := (Left <> Right);
    icLT:  Result := (Left <  Right);
    icLTE: Result := (Left <= Right);
    icGT:  Result := (Left >  Right);
    icGTE: Result := (Left >= Right);
  else
    Result := Null;
  end;
end;


procedure TBytecodeEmitter.GetScopes(out ALocals: TScopeLocals; out AZones: TScopeRanges);
var
  i, j, k: Integer;
  CurrentLocals: TLocals;
  CurrentZone: TJumpZone;
  VarLocations: TVarLocations;
  VarAddr: PtrInt;
  Instr: TInstruction;
  IsUse: Boolean;
begin
  ALocals.Init([]);
  AZones.Init([]);

  if Intermediate.Code.Size = 0 then
    Exit;

  i := 0;
  while i <= Intermediate.Code.High do
  begin
    CurrentZone.JmpFrom := i;
    CurrentLocals := TLocals.Create(@HashInt64);

    j := i;
    while j <= Intermediate.Code.High do
    begin
      Instr := Intermediate.Code.Data[j];

      for k := 0 to Instr.nArgs - 1 do
      begin
        // --- Main Guard: Only consider local variables ---
        if Instr.Args[k].Pos <> mpLocal then
          Continue;

        // --- Handle Special Cases ---
        IsUse := True; // Assume it's a valid use unless a special case says otherwise.
        case Instr.Code of
          icLOAD_GLOBAL, icLOAD_NONLOCAL:
            // For these instructions, only the first argument (the destination)
            // is a true local variable use in this scope.
            if k > 0 then IsUse := False;

          icINVOKE:
            // For INVOKE, Arg[0] is the function to call. It's only a local variable
            // use if it's a function pointer, not a static function address.
            if (k > 0) or ((Instr.nArgs <> 3) and (Instr.Args[2].i32 <> 0)) then
              IsUse := False;

          icERROR:
            IsUse := False;
        end;

        if not IsUse then
          Continue;

        // --- Default Logic: Add the valid use to the map ---
        VarAddr := Instr.Args[k].Addr;
        if CurrentLocals.Get(VarAddr, VarLocations) then
        begin
          VarLocations.Add(j);
          CurrentLocals.AddOrModify(VarAddr, VarLocations);
        end
        else
        begin
          VarLocations.Init([]);
          VarLocations.Add(j);
          CurrentLocals.Add(VarAddr, VarLocations);
        end;
      end;

      // Check for the end of the current scope.
      if (j + 1 <= Intermediate.Code.High) and (Intermediate.Code.Data[j + 1].Code = icNEWFRAME) then
      begin
        CurrentZone.JmpTo := j;
        break;
      end;
      if j = Intermediate.Code.High then
      begin
        CurrentZone.JmpTo := j;
        break;
      end;
      Inc(j);
    end;

    ALocals.Add(CurrentLocals);
    AZones.Add(CurrentZone);
    i := j + 1;
  end;
end;

// easier to say what is safe, than what is unsafe
function TBytecodeEmitter.IsBasicInstruction(i: Int32): Boolean;
begin
  Result := (Self.Intermediate.Code.Data[i].Code in [icADD..icINV, icMOV, icMOVH, icDREF, icADDR]);
end;

procedure TBytecodeEmitter.ConstantFolding();
var
  i, FuncStart, FuncEnd: Integer;
  Instr: TInstruction;

  // This nested procedure contains the entire stateful analysis for a single function.
  procedure PerformFunctionWideFolding(StartPC, EndPC: Integer);
  type
    TKnownConstant = record
      VarData: TInstructionData;
      ConstData: TInstructionData;
    end;
  var
    j, k, NewConstIndex: Integer;
    KnownConstants: array of TKnownConstant;
    TargetInstr: ^TInstruction;
    ResultValue, Left, Right: Variant;
    DataType: EExpressBaseType;
    IsKilled, IsFoldable, IsComparison: Boolean;
    ResultConstant: TConstant;
    Const1, Const2: TConstant;
    DestVar, ConstData: TInstructionData;

    function FindConstant(VarData: TInstructionData; out AConstData: TInstructionData): Boolean;
    var idx: integer;
    begin
      Result := False;
      for idx := 0 to High(KnownConstants) do
        if KnownConstants[idx].VarData = VarData then
        begin
          AConstData := KnownConstants[idx].ConstData;
          Result := True;
          Exit;
        end;
    end;

    procedure KillConstant(VarData: TInstructionData);
    var idx: integer;
    begin
      for idx := High(KnownConstants) downto 0 do
        if KnownConstants[idx].VarData = VarData then
          Delete(KnownConstants, idx, 1);
    end;

    function GetDestination(const InstrToTest: TInstruction; out Dest: TInstructionData): Boolean;
    begin
      Result := True;
      case InstrToTest.Code of
        icFMA: if InstrToTest.nArgs = 4 then Dest := InstrToTest.Args[3] else Result := False;
        icADD..icSAR: if InstrToTest.nArgs = 3 then Dest := InstrToTest.Args[2] else Result := False;
        icMOV, icDREF, icLOAD_GLOBAL, icLOAD_NONLOCAL, icNEW, icDYNCAST, icIS: Dest := InstrToTest.Args[0];
      else
        Result := False;
      end;
    end;

    function IsDestinationOperand(const InstrToTest: TInstruction; Index: Integer): Boolean;
    begin
      Result := False;
      case InstrToTest.Code of
        icFMA: if Index = 3 then Result := True;
        icADD..icSAR: if Index = 2 then Result := True;
        icPUSH, icPUSHREF, icMOV, icDREF, icLOAD_GLOBAL, icLOAD_NONLOCAL, icNEW, icDYNCAST, icIS: if Index = 0 then Result := True;
      end;
    end;

    function IsVarReadAgain(StartIndex: Integer; VarData: TInstructionData): Boolean;
    var idx, opIdx: Integer; CheckInstr: TInstruction; TempDest: TInstructionData;
    begin
      Result := False;
      for idx := StartIndex to EndPC do
      begin
        CheckInstr := Intermediate.Code.Data[idx];
        if Self.JumpSites[idx] then Exit(True);
        if CheckInstr.Code in [icPUSH, icPUSHREF, icINVOKE, icINVOKEX, icINVOKE_VIRTUAL] then Exit(True);

        if GetDestination(CheckInstr, TempDest) and (TempDest = VarData) then
          Exit(False);

        for opIdx := 0 to CheckInstr.nArgs - 1 do
          if not IsDestinationOperand(CheckInstr, opIdx) and (CheckInstr.Args[opIdx] = VarData) then
            Exit(True);
      end;
    end;

  begin // Start of PerformFunctionWideFolding
    SetLength(KnownConstants, 0);
    for j := StartPC to EndPC do
    begin
      TargetInstr := @Intermediate.Code.Data[j];
      if TargetInstr^.Code in [icERROR, icNOOP] then Continue;
      if Self.JumpSites[j] then SetLength(KnownConstants, 0);

      for k := 0 to TargetInstr^.nArgs - 1 do
      begin
        if IsDestinationOperand(TargetInstr^, k) then Continue;
        if (TargetInstr^.Args[k].Pos = mpLocal) and (TargetInstr^.Args[k].BaseType in XprNumericTypes) then
          if FindConstant(TargetInstr^.Args[k], ConstData) then
            TargetInstr^.Args[k] := ConstData;
      end;

      if (TargetInstr^.nArgs = 3) and
         (TargetInstr^.Args[0].Pos = mpConst) and
         (TargetInstr^.Args[1].Pos = mpConst) and
         (TargetInstr^.Args[2].Pos = mpLocal) and
         (TargetInstr^.Code in [icADD..icGTE]) then
      begin
        Const1 := Intermediate.Constants.Data[TargetInstr^.Args[0].Arg];
        Const2 := Intermediate.Constants.Data[TargetInstr^.Args[1].Arg];
        DataType := TargetInstr^.Args[2].BaseType;

        // Ensure both constants are of the same family (int, uint, or float).
        if not ((Const1.Typ in XprNumericTypes) and
                (Const2.Typ in XprNumericTypes)) then
        begin
          Continue; // This is NOT a numeric operation, do not fold it.
        end;

        // Ensure the destination type matches the operation type.
        if not ( ((DataType in XprIntTypes)   and (Const1.Typ in XprIntTypes)) or
                 ((DataType in XprFloatTypes) and (Const1.Typ in XprFloatTypes)) ) then
        begin
          Continue;
        end;

        // --- End of Safety Checks ---
        IsFoldable := True;
        case DataType of
          xtInt8:   begin Left := Const1.val_i8;  Right := Const2.val_i8;  end;
          xtInt16:  begin Left := Const1.val_i16; Right := Const2.val_i16; end;
          xtInt32:  begin Left := Const1.val_i32; Right := Const2.val_i32; end;
          xtInt64:  begin Left := Const1.val_i64; Right := Const2.val_i64; end;
          xtUInt8:  begin Left := Const1.val_u8;  Right := Const2.val_u8;  end;
          xtUInt16: begin Left := Const1.val_u16; Right := Const2.val_u16; end;
          xtUInt32: begin Left := Const1.val_u32; Right := Const2.val_u32; end;
          xtUInt64: begin Left := Const1.val_u64; Right := Const2.val_u64; end;
          xtSingle: begin Left := Const1.val_f32; Right := Const2.val_f32; end;
          xtDouble: begin Left := Const1.val_f64; Right := Const2.val_f64; end;
        else
          IsFoldable := False;
        end;

        if IsFoldable then
        begin
          ResultValue := BinaryOp(TargetInstr^.Code, Left, Right, DataType);
          if VarIsNull(ResultValue) then Continue;

          IsComparison := TargetInstr^.Code in [icEQ..icGTE];

          if IsComparison then ResultConstant := Constant(Boolean(ResultValue), xtBoolean)
          else if DataType in XprSignedInts then ResultConstant := Constant(Int64(ResultValue), DataType)
          else if DataType in XprUnsignedInts then ResultConstant := Constant(UInt64(ResultValue), DataType)
          else ResultConstant := Constant(Double(ResultValue), DataType);

          NewConstIndex := Intermediate.Constants.Add(ResultConstant);

          TargetInstr^.Code    := icMOV;
          TargetInstr^.nArgs   := 2;
          TargetInstr^.Args[0] := TargetInstr^.Args[2]; // Original Dest
          TargetInstr^.Args[1].Pos      := mpConst;
          TargetInstr^.Args[1].Arg      := NewConstIndex;
          TargetInstr^.Args[1].BaseType := ResultConstant.Typ;
        end;
      end;

      if GetDestination(TargetInstr^, DestVar) then KillConstant(DestVar);

      if (TargetInstr^.Code = icMOV) and (TargetInstr^.nArgs = 2) and
         (TargetInstr^.Args[0].Pos = mpLocal) and (TargetInstr^.Args[1].Pos = mpConst) and
         (TargetInstr^.Args[1].BaseType in XprNumericTypes) then // Only track numeric constants
      begin
        SetLength(KnownConstants, Length(KnownConstants) + 1);
        KnownConstants[High(KnownConstants)].VarData   := TargetInstr^.Args[0];
        KnownConstants[High(KnownConstants)].ConstData := TargetInstr^.Args[1];
      end;
    end;

    for j := StartPC to EndPC do
    begin
      TargetInstr := @Intermediate.Code.Data[j];
      if (TargetInstr^.Code = icMOV) and (TargetInstr^.nArgs = 2) and
         (TargetInstr^.Args[0].Pos = mpLocal) and (TargetInstr^.Args[1].Pos = mpConst) then
        if not IsVarReadAgain(j + 1, TargetInstr^.Args[0]) then
          TargetInstr^.Code := icERROR;
    end;
  end;

begin // --- Main ConstantFolding Body ---
  if Intermediate.Code.Size = 0 then Exit;

  FuncStart := 0;
  for i := 0 to Intermediate.Code.High do
  begin
    if (Intermediate.Code.Data[i].Code = icNEWFRAME) or (i = Intermediate.Code.High) then
    begin
      if (i = Intermediate.Code.High) then FuncEnd := i else FuncEnd := i - 1;
      if FuncEnd >= FuncStart then PerformFunctionWideFolding(FuncStart, FuncEnd);
      FuncStart := i;
    end;
  end;
end;

procedure TBytecodeEmitter.CommonSubexpressionElimination();
var
  i, j, k, TargetAddr: Integer;
  IsBlockStart: array of Boolean;
  AllLocals: TScopeLocals;
  AllZones: TScopeRanges;
  CurrentScopeIndex, CurrentBlockStart: Integer;
  AvailableExprs: array of TInstruction;
  FoundCSE: Boolean;
  TargetInstr, UseInstr: ^TInstruction;
  OldResultVar, NewResultVar, UsesVar: TInstructionData;
  UsesList: TVarLocations;
  IsSafeToEliminate: Boolean;

  // Helper to get the destination variable of an instruction.
  function GetDestination(const Instr: TInstruction; out Dest: TInstructionData): Boolean;
  begin
    Result := True;
    case Instr.Code of
      icADD..icSAR: if Instr.nArgs = 3 then Dest := Instr.Args[2] else Result := False;
      icMOV, icDREF, icLOAD_GLOBAL, icLOAD_NONLOCAL, icNEW, icDYNCAST, icIS: Dest := Instr.Args[0];
    else
      Result := False;
    end;
  end;

  // Helper to determine if an operand is a source (i.e., not a destination)
  function IsSourceOperand(const Instr: TInstruction; Index: Integer): Boolean;
  begin
    Result := True; // Assume it's a source
    case Instr.Code of
      icADD..icSAR: if Index = 2 then Result := False;
      icMOV, icDREF, icLOAD_GLOBAL, icLOAD_NONLOCAL, icNEW, icDYNCAST, icIS: if Index = 0 then Result := False;
    end;
  end;
var
  Instr: TInstruction;
  UseIndex, opIdx: Int32;
  DestVar: TInstructionData;
begin
  if Intermediate.Code.Size = 0 then Exit;

  // --- Phase 1: Pre-compute Scopes and Basic Block boundaries ---
  GetScopes(AllLocals, AllZones);

  SetLength(IsBlockStart, Intermediate.Code.Size);
  for i := 0 to High(IsBlockStart) do IsBlockStart[i] := True;
  IsBlockStart[0] := True;
  for i := 0 to Intermediate.Code.High do
  begin
    Instr := Intermediate.Code.Data[i];
    if Self.IsBasicInstruction(i) then IsBlockStart[i + 1] := False;
    if Instr.Code in [icJMP, icRELJMP, icJZ, icJNZ] then
    begin
      if Instr.Code = icJMP then TargetAddr := Instr.Args[0].Addr
      else if Instr.Code = icRELJMP then TargetAddr := i + Instr.Args[0].Addr
      else TargetAddr := i + Instr.Args[1].Addr;
      if InRange(TargetAddr, 0, High(IsBlockStart)) then
        IsBlockStart[TargetAddr+1] := True;
    end;
  end;

  (*
  IsBlockStart[0] := True;
  for i := 0 to Intermediate.Code.High do
  begin
    Instr := Intermediate.Code.Data[i];
    if (i + 1 <= High(IsBlockStart)) and (Instr.Code in [icJMP, icRELJMP, icJZ, icJNZ, icRET, icINVOKE, icINVOKEX, icINVOKE_VIRTUAL]) then
      IsBlockStart[i + 1] := True
    else
      IsBlockStart[i] := Self.JumpSites[i];
  end;
  *)

  // --- Phase 2: Main CSE Loop, operating one function at a time ---
  CurrentScopeIndex := -1;
  for i := 0 to Intermediate.Code.High do
  begin
    if (Intermediate.Code.Data[i].Code = icNEWFRAME) or (i = 0) then
    begin
      Inc(CurrentScopeIndex);
      SetLength(AvailableExprs, 0);
    end;

    if IsBlockStart[i] then
      SetLength(AvailableExprs, 0);

    TargetInstr := @Intermediate.Code.Data[i];
    if TargetInstr^.Code in [icERROR, icNOOP] then Continue;

    FoundCSE := False;
    if (TargetInstr^.nArgs = 3) and (TargetInstr^.Code in [icADD..icSAR]) then
    begin
      for j := 0 to High(AvailableExprs) do
      begin
        if (AvailableExprs[j].Code = TargetInstr^.Code) and
           SameData(AvailableExprs[j].Args[0], TargetInstr^.Args[0]) and
           SameData(AvailableExprs[j].Args[1], TargetInstr^.Args[1]) then
        begin
          FoundCSE := True;
          OldResultVar := AvailableExprs[j].Args[2];
          NewResultVar := TargetInstr^.Args[2];


          IsSafeToEliminate := True;
          // Look up all uses of the temporary variable we want to eliminate.
          if AllLocals.Data[CurrentScopeIndex].Get(NewResultVar.Addr, UsesList) then
          begin
            // this is a variable
            if UsesList.Size > 2 then
              IsSafeToEliminate := False;

            for k := 0 to UsesList.High do
            begin
              UseIndex := UsesList.Data[k];
              // Is this use outside our current, safe basic block?
              if IsBlockStart[UseIndex] and (UseIndex <> i) then
              begin
                IsSafeToEliminate := False;
                break;
              end;
            end;
          end;

          if IsSafeToEliminate then
          begin
            // All uses are local. Propagate the original result and kill the redundant instruction.
            if AllLocals.Data[CurrentScopeIndex].Get(NewResultVar.Addr, UsesList) then
            begin
              for k := 0 to UsesList.High do
              begin
                UseIndex := UsesList.Data[k];
                UseInstr := @Intermediate.Code.Data[UseIndex];
                // Propagate into all source operands

                for opIdx := 0 to UseInstr^.nArgs - 1 do
                  if IsSourceOperand(UseInstr^, opIdx) and SameData(UseInstr^.Args[opIdx], NewResultVar) then
                    UseInstr^.Args[opIdx] := OldResultVar;
              end;
            end;

            TargetInstr^.Code := icERROR; // The redundant calculation is now dead
          end
          else
          begin
            // Unsafe to eliminate completely. Fall back to a simple MOV.
            TargetInstr^.Code    := icMOV;
            TargetInstr^.nArgs   := 2;
            TargetInstr^.Args[0] := NewResultVar;
            TargetInstr^.Args[1] := OldResultVar;
          end;

          break; // Stop searching for CSEs
        end;
      end;
    end;

    if FoundCSE then Continue;

    if GetDestination(TargetInstr^, DestVar) then
    begin
      for j := High(AvailableExprs) downto 0 do
        if SameData(AvailableExprs[j].Args[0], DestVar) or SameData(AvailableExprs[j].Args[1], DestVar) then
          Delete(AvailableExprs, j, 1);
    end;

    if (TargetInstr^.nArgs = 3) and (TargetInstr^.Code in [icADD..icSAR]) then
    begin
      SetLength(AvailableExprs, Length(AvailableExprs) + 1);
      AvailableExprs[High(AvailableExprs)] := TargetInstr^;
    end;
  end;

  // Free the dictionaries in the TScopeLocals array
  for i := 0 to AllLocals.High do
    AllLocals.Data[i].Free;
end;



procedure TBytecodeEmitter.CopyPropagation();
type
  TAlias = record
    Dest, Src: TInstructionData;
  end;
var
  i, j, k, TargetAddr: Integer;
  IsBlockStart: array of Boolean;
  AliasMap: array of TAlias;
  Instr: TInstruction;
  TargetInstr: ^TInstruction;
  Propagated, DestKills: Boolean;
  DestVar: TInstructionData;

  function GetDestination(const InstrToTest: TInstruction; out Dest: TInstructionData): Boolean;
  begin
    Result := True;
    case InstrToTest.Code of
      icADD..icSAR: if InstrToTest.nArgs = 3 then Dest := InstrToTest.Args[2] else Result := False;
      icMOV, icDREF, icLOAD_GLOBAL, icLOAD_NONLOCAL, icNEW, icDYNCAST, icIS: Dest := InstrToTest.Args[0];
    else
      Result := False;
    end;
  end;

  function IsDestinationOperand(const InstrToTest: TInstruction; Index: Integer): Boolean;
  begin
    Result := False;
    case InstrToTest.Code of
      icADD..icSAR: if Index = 2 then Result := True;
      icMOV, icDREF, icLOAD_GLOBAL, icLOAD_NONLOCAL, icNEW, icDYNCAST, icIS: if Index = 0 then Result := True;
    end;
  end;

begin
  if Intermediate.Code.Size = 0 then Exit;

  // --- Phase 1: Pre-compute Basic Block boundaries ---
  SetLength(IsBlockStart, Intermediate.Code.Size);

  IsBlockStart[0] := True;
  for i := 0 to Intermediate.Code.High do
  begin
    Instr := Intermediate.Code.Data[i];
    if (i + 1 <= High(IsBlockStart)) and (Instr.Code in [icJMP, icRELJMP, icJZ, icJNZ, icRET, icINVOKE, icINVOKEX, icINVOKE_VIRTUAL]) then
      IsBlockStart[i + 1] := True
    else
      IsBlockStart[i] := Self.JumpSites[i];
  end;

  // --- Phase 2: Main Copy Propagation Loop ---
  SetLength(AliasMap, 0);
  for i := 0 to Intermediate.Code.High do
  begin
    if IsBlockStart[i] then
      SetLength(AliasMap, 0);

    TargetInstr := @Intermediate.Code.Data[i];
    if TargetInstr^.Code in [icERROR, icNOOP] then Continue;

    // --- A. Propagate known aliases into SOURCES ---
    for j := 0 to TargetInstr^.nArgs - 1 do
    begin
      if IsDestinationOperand(TargetInstr^, j) then Continue;
      if TargetInstr^.Args[j].Pos <> mpLocal then Continue;

      // *** TYPE SAFETY CHECK ***
      // Only propagate numeric and pointer types. Leave managed types alone.
      if not (TargetInstr^.Args[j].BaseType in XprNumericTypes) then
        Continue;

      repeat
        Propagated := False;
        for k := 0 to High(AliasMap) do
        begin
          if SameData(AliasMap[k].Dest, TargetInstr^.Args[j]) then
          begin
            TargetInstr^.Args[j] := AliasMap[k].Src;
            Propagated := True;
          end;
        end;
      until not Propagated;
    end;

    // --- B. Handle "Kills" ---
    DestKills := GetDestination(TargetInstr^, DestVar);
    if DestKills then
    begin
      for j := High(AliasMap) downto 0 do
      begin
        if SameData(AliasMap[j].Dest, DestVar) or SameData(AliasMap[j].Src, DestVar) then
          Delete(AliasMap, j, 1);
      end;
    end;

    // --- C. Register New Copies ---
    if (TargetInstr^.Code = icMOV) and (TargetInstr^.nArgs = 2) and
       (TargetInstr^.Args[0].Pos = mpLocal) and (TargetInstr^.Args[1].Pos in [mpLocal, mpConst]) then
    begin
       // *** TYPE SAFETY CHECK ***
       // Only register copies of numeric or pointer types.
       if (TargetInstr^.Args[0].BaseType in XprNumericTypes) and
          (TargetInstr^.Args[1].BaseType in XprNumericTypes) and
          (TargetInstr^.Args[2].BaseType in XprNumericTypes)then
       begin
         if not SameData(TargetInstr^.Args[0], TargetInstr^.Args[1]) then
         begin
           SetLength(AliasMap, Length(AliasMap) + 1);
           AliasMap[High(AliasMap)].Dest := TargetInstr^.Args[0];
           AliasMap[High(AliasMap)].Src  := TargetInstr^.Args[1];
         end;
       end;
    end;
  end;
end;



procedure TBytecodeEmitter.DeadStoreElimination();
var
  AllScopes: TScopeLocals;
  AllZones: TScopeRanges;
  i, j: Integer;
  CurrentScope: TLocals;
  ScopeUses: TVarLocations;
  Zone: TJumpZone;
  Instr: ^TInstruction;
  DestVarAddr: PtrInt;
begin
  GetScopes(AllScopes, AllZones);

  // THIS CAN NOT BE DONE THIS EASILY, MAY BE GLOBAL OR NON-LOCAL
  // For each function scope...
  for i := 1 to AllZones.High do
  begin
    Zone := AllZones.Data[i];
    CurrentScope := AllScopes.Data[i];
     WriteLN('Zone: ', Zone.JmpFrom, '..', Zone.JmpTo);
    // For each instruction in that scope...
    for j := Zone.JmpFrom to Zone.JmpTo-1 do
    begin
      Instr := @Intermediate.Code.Data[j];

      // Is this a potential dead store (e.g., MOV local, ...)?
      if (Instr^.Code = icMOV) and (Instr^.Args[0].Pos = mpLocal) then
      begin
        DestVarAddr := Instr^.Args[0].Addr;

        // Get the list of all uses for this variable in this scope.
        if CurrentScope.Get(DestVarAddr, ScopeUses) then
        begin
          if ScopeUses.Size <= 1 then //only stored, never used, delete
          Instr^.Code:=icERROR;
        end;
      end;
    end;
  end;

  // Remember to free the dictionaries inside the AllScopes list.
  for i := 0 to AllScopes.High do
    AllScopes.Data[i].Free;
end;


(*
  Optimizaton stage
*)
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
    // If this original index maps to a removed instruction ($FFFFFFFF), it's an error or dead code.
    // Otherwise, update it to the new, compacted bytecode index.
    if NewIndex[funcEntry.CodeLocation] <> $FFFFFFFF then
      funcEntry.CodeLocation := NewIndex[funcEntry.CodeLocation]
    else
      // Handle case where function entry might point to removed code (e.g., unreachable functions)
      // This might imply setting CodeLocation to an invalid value or a special "null" entry.
      // For now, let's assume valid locations. If a function is truly removed, it probably
      // shouldn't be in the FunctionTable to begin with, or its entry will point to -1.
      // A more robust system might remove the entry from the table or mark it as invalid.
      // For now, we'll let it point to -1 if it was removed, which the interpreter should handle.
      funcEntry.CodeLocation := $FFFFFFFF; // Or some other appropriate "invalid" marker.

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

