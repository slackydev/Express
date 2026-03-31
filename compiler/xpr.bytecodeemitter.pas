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

    procedure Fuse();
    procedure Sweep();
    procedure UpdateJumpsAtIR(InsertAt, Offset: Int32);
    procedure ConstantFold();
    procedure OptInlineIR();
  end;

implementation

uses
  Math, Variants,
  xpr.Langdef,
  xpr.tokenizer,
  xpr.Utils;

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
  Self.Bytecode.NativeImports := Self.Intermediate.NativeImports;

  JumpZones.Init([]);
end;

procedure TBytecodeEmitter.Compile();
var
  IR: TInstruction;
  BCInstr: TBytecodeInstruction;
  i,j,k: Int32;
begin
  Self.BuildJumpZones();
  OptInlineIR();
  ConstantFold();
  Self.Bytecode.FunctionTable := Self.Intermediate.FunctionTable;
  Self.BuildJumpZones();

  for i:=0 to Intermediate.Code.High do {last opcode is always RET}
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
  end;

  // --- STEP 3: SPECIALIZE AND EMIT FINAL BYTECODE ---
  for i := 0 to Intermediate.Code.Size - 1 do
  begin
    IR := Intermediate.Code.Data[i];

    FillChar(BCInstr, SizeOf(BCInstr), 0);

    case IR.Code of
      icNOOP, icPASS:
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

      icLOAD_EXTERN:
        BCInstr.Code := bcLOAD_EXTERN;

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

      icPUSH_CLOSURE:
        BCInstr.Code := bcPUSH_CLOSURE;

      icPOP:
        BCInstr.Code := bcPOP;

      icPOPH:
        BCInstr.Code := bcPOPH;

      icRET:
        BCInstr.Code := bcRET;

      icRET_RAISE:
        BCInstr.Code := bcRET_RAISE;

      icSPAWN:
        BCInstr.Code := bcSPAWN;

      icCREATE_CALLBACK:
        BCInstr.Code := bcCREATE_CALLBACK;

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

      icFFICALL:
        BCInstr.Code := bcFFICALL;

      // exceptions
      icSET_ERRHANDLER:  BCInstr.Code := bcSET_ERRHANDLER;
      icRAISE:           BCInstr.Code := bcRAISE;
      icGET_EXCEPTION:   BCInstr.Code := bcGET_EXCEPTION;
      icUNSET_EXCEPTION: BCInstr.Code := bcUNSET_EXCEPTION;
      icIncTry:          BCInstr.Code := bcIncTry;
      icDecTry:          BCInstr.Code := bcDecTry;

      //
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

    Bytecode.Settings.Add(Intermediate.Settings.Data[i]);
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
  JumpZones.Init([]);
  SetLength(JumpSites, Intermediate.Code.Size);
  FillChar(JumpSites[0], Length(JumpSites), 0);

  for i:=0 to Intermediate.Code.Size-1 do
  begin
    case Intermediate.Code.Data[i].Code of
      icJMP, icIncTry:
        begin
          Zone.JmpFrom := i;
          Zone.JmpTo   := Intermediate.Code.Data[i].Args[0].Addr;
          Zone.Kind    := jkAbsolute;
          JumpZones.Add(Zone);
          JumpSites[Zone.JmpFrom] := True;
          if (Zone.JmpTo >= 0) and (Zone.JmpTo < Length(JumpSites)) then
            JumpSites[Zone.JmpTo] := True;
        end;

      icRELJMP, icJBREAK, icJCONT, icJFUNC:
        begin
          Zone.JmpFrom := i;
          Zone.JmpTo   := i + Intermediate.Code.Data[i].Args[0].Addr; // Standard mapping
          Zone.Kind    := jkRelative;
          JumpZones.Add(Zone);
          JumpSites[Zone.JmpFrom] := True;
          if (Zone.JmpTo >= 0) and (Zone.JmpTo < Length(JumpSites)) then
            JumpSites[Zone.JmpTo]   := True;
        end;

      icJNZ, icJZ:
        begin
          Zone.JmpFrom := i;
          Zone.JmpTo   := i + Intermediate.Code.Data[i].Args[1].Addr; // Standard mapping
          Zone.Kind    := jkRelative;
          JumpZones.Add(Zone);
          JumpSites[Zone.JmpFrom] := True;
          if (Zone.JmpTo >= 0) and (Zone.JmpTo < Length(JumpSites)) then
            JumpSites[Zone.JmpTo]   := True;
        end;
    end;
  end;
end;

function TBytecodeEmitter.SpecializeBinop(Arg: TInstruction): EBytecode;
var
  canSpecialize: Boolean;

  procedure SpecializeString();
  begin
    if (Arg.Args[2].BaseType = xtUnicodeString) then
      Result := bcADD_USTR
    else if (Arg.Args[0].BaseType in [xtAnsiChar, xtAnsiString]) and
            (Arg.Args[1].BaseType in [xtAnsiChar, xtAnsiString]) then
      Result := bcADD_STR
    else
      raise Exception.Create('Internal error - not supported[25205378]');
  end;
begin
  Result := bcNOOP;

  if (Arg.Args[2].BaseType = xtAnsiString) then
  begin
    SpecializeString();
    Exit;
  end;

  canSpecialize := (Arg.Args[2].Pos = mpLocal) and (Arg.Args[0].Pos in [mpLocal, mpImm]) and (Arg.Args[1].Pos in[mpLocal, mpImm]);

  if canSpecialize and (XprTypeSize[ Arg.Args[0].BaseType ] >= 4) then
  begin
    case Arg.Code of
      icADD: Exit(EncodeTernary(Arg,[bcADD_lll_i32, bcADD_lil_i32, bcADD_ill_i32, bcADD_iil_i32],[0,2,0,0, 1,3, 4,5]));
      icSUB: Exit(EncodeTernary(Arg,[bcSUB_lll_i32, bcSUB_lil_i32, bcSUB_ill_i32, bcSUB_iil_i32],[0,2,0,0, 1,3, 4,5]));
      icMUL: Exit(EncodeTernary(Arg,[bcMUL_lll_i32, bcMUL_lil_i32, bcMUL_ill_i32, bcMUL_iil_i32],[0,2,0,0, 1,3, 4,5]));
      icDIV: Exit(EncodeTernary(Arg,[bcDIV_lll_i32, bcDIV_lil_i32, bcDIV_ill_i32, bcDIV_iil_i32],[0,2,0,0, 1,3, 4,5]));
      icMOD: Exit(EncodeTernary(Arg,[bcMOD_lll_i32, bcMOD_lil_i32, bcMOD_ill_i32, bcMOD_iil_i32],[0,2,0,0, 1,3, 4,5]));
      icPOW: Exit(EncodeTernary(Arg,[bcPOW_lll_i32, bcPOW_lil_i32, bcPOW_ill_i32, bcPOW_iil_i32],[0,2,0,0, 1,3, 4,5]));

      icEQ:  Exit(EncodeTernary(Arg,[bcEQ_lll_i32, bcEQ_lil_i32, bcEQ_ill_i32, bcEQ_iil_i32],[0,2,0,0, 1,3, 4,5]));
      icNEQ: Exit(EncodeTernary(Arg,[bcNE_lll_i32, bcNE_lil_i32, bcNE_ill_i32, bcNE_iil_i32],[0,2,0,0, 1,3, 4,5]));
      icLT:  Exit(EncodeTernary(Arg,[bcLT_lll_i32, bcLT_lil_i32, bcLT_ill_i32, bcLT_iil_i32],[0,2,0,0, 1,3, 4,5]));
      icGT:  Exit(EncodeTernary(Arg,[bcGT_lll_i32, bcGT_lil_i32, bcGT_ill_i32, bcGT_iil_i32],[0,2,0,0, 1,3, 4,5]));
      icLTE: Exit(EncodeTernary(Arg,[bcLTE_lll_i32, bcLTE_lil_i32, bcLTE_ill_i32, bcLTE_iil_i32],[0,2,0,0, 1,3, 4,5]));
      icGTE: Exit(EncodeTernary(Arg,[bcGTE_lll_i32, bcGTE_lil_i32, bcGTE_ill_i32, bcGTE_iil_i32],[0,2,0,0, 1,3, 4,5]));

      icBND: Exit(EncodeTernary(Arg,[bcBND_lll_i32, bcBND_lil_i32, bcBND_ill_i32, bcBND_iil_i32],[0,2,0,0, 1,3]));
      icBOR: Exit(EncodeTernary(Arg,[bcBOR_lll_i32, bcBOR_lil_i32, bcBOR_ill_i32, bcBOR_iil_i32],[0,2,0,0, 1,3]));
      icXOR: Exit(EncodeTernary(Arg,[bcXOR_lll_i32, bcXOR_lil_i32, bcXOR_ill_i32, bcXOR_iil_i32],[0,2,0,0, 1,3]));
      icSHL: Exit(EncodeTernary(Arg,[bcSHL_lll_i32, bcSHL_lil_i32, bcSHL_ill_i32, bcSHL_iil_i32],[0,2,0,0, 1,3]));
      icSHR: Exit(EncodeTernary(Arg,[bcSHR_lll_i32, bcSHR_lil_i32, bcSHR_ill_i32, bcSHR_iil_i32],[0,2,0,0, 1,3]));
      icSAR: Exit(EncodeTernary(Arg,[bcSAR_lll_i32, bcSAR_lil_i32, bcSAR_ill_i32, bcSAR_iil_i32],[0,2,0,0, 1,3]));
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

  if (Arg.Args[0].BaseType = xtAnsiString) and (Arg.Args[1].BaseType = xtAnsiString) and (Arg.Args[1].Pos = mpImm) then
  begin
    Result := bcLOAD_STR;
    Exit;
  end;

  if (Arg.Args[0].BaseType = xtAnsiString) and (Arg.Args[1].BaseType = xtAnsiChar) then
  begin
    Result := bcCh2Str;
    Exit;
  end;

  if (Arg.Args[0].BaseType = xtUnicodeString) and (Arg.Args[1].Pos = mpImm) then
  begin
    Result := bcLOAD_USTR;
    Exit;
  end;

  if (Arg.Args[0].BaseType = xtUnicodeString) and (Arg.Args[1].BaseType = xtUnicodeChar) then
  begin
    Result := bcCh2UStr;
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
    (0,2,4,6, 1,3,5,7, 8,10);
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
      if BaseType in[xtInt8..xtInt64, xtUInt8..xtUInt64] then
        Result := Left div Right
      else
        Result := Left / Right;
    icMOD:
      if BaseType in[xtInt8..xtInt64] then
        Result := Modulo(Int64(Left), Int64(Right))
      else if BaseType in [xtUInt8..xtUInt64] then
        Result := UInt64(Left) mod UInt64(Right)
      else if BaseType in [xtSingle..xtDouble] then
        Result := Modulo(Double(Left), Double(Right))
      else
        Exit(Null);
    icPOW:
      if BaseType in[xtInt8..xtInt64] then
        Result := ipow(Int64(Left), Int64(Right))
      else if BaseType in[xtUInt8..xtUInt64] then
        Result := ipow(UInt64(Left), UInt64(Right))
      else if BaseType in[xtSingle..xtDouble] then
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
        if Instr.Args[k].Pos <> mpLocal then
          Continue;

        IsUse := True;
        case Instr.Code of
          icLOAD_GLOBAL:
            if k > 0 then IsUse := False;
          icINVOKE:
            if (k > 0) or ((Instr.nArgs <> 3) and (Instr.Args[2].i32 <> 0)) then
              IsUse := False;
          icERROR:
            IsUse := False;
        end;

        if not IsUse then
          Continue;

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

function TBytecodeEmitter.IsBasicInstruction(i: Int32): Boolean;
begin
  Result := (Self.Intermediate.Code.Data[i].Code in[icADD..icINV, icMOV, icMOVH, icDREF, icADDR]);
end;

procedure TBytecodeEmitter.Fuse();
var
  i, removals: Int32;
begin
  removals := 0;
  if Bytecode.Code.Size < 3 then Exit;

  for i:=0 to Bytecode.Code.Size-3 do
  begin
    // DEAD JUMP REMOVAL
    if (Bytecode.Code.Data[i].Code = bcRELJMP) and (Bytecode.Code.Data[i].Args[0].Data.Addr = 0) then
    begin
      Bytecode.Code.Data[i].Code := bcERROR;
      Inc(removals);
    end;

    if not (InRange(Ord(Bytecode.Code.Data[i+0].Code), Ord(bcFMA_i32), Ord(bcFMA_i64)) and
            InRange(Ord(Bytecode.Code.Data[i+1].Code), Ord(bcDREF_32), Ord(bcDREF_64))) then
      continue;

    if (Bytecode.Code.Data[i].Args[3].Pos <> Bytecode.Code.Data[i+1].Args[1].Pos) or
       (Bytecode.Code.Data[i].Args[3].Data.Addr <> Bytecode.Code.Data[i+1].Args[1].Data.Addr) then
      continue;

    if Bytecode.Code.Data[i+2].Code = bcINCLOCK then
      continue;

    if not Intermediate.Code.Data[i].Args[3].IsTemporary then
      continue;

    if (Bytecode.Code.Data[i+0].Code = bcFMA_i64) and (Bytecode.Code.Data[i+1].Code = bcDREF_64) then
    begin
      Bytecode.Code.Data[i+0].Code := bcFMAD_d64_64;
      Bytecode.Code.Data[i+0].Args[3] := Bytecode.Code.Data[i+1].Args[0];
      Bytecode.Code.Data[i+1].Code := bcERROR;
      Inc(removals);
    end
    else if (Bytecode.Code.Data[i+0].Code = bcFMA_i64) and (Bytecode.Code.Data[i+1].Code = bcDREF_32)then
    begin
      Bytecode.Code.Data[i+0].Code := bcFMAD_d32_64;
      Bytecode.Code.Data[i+0].Args[3] := Bytecode.Code.Data[i+1].Args[0];
      Bytecode.Code.Data[i+1].Code := bcERROR;
      Inc(removals);
    end
    else if (Bytecode.Code.Data[i+0].Code = bcFMA_i32) and (Bytecode.Code.Data[i+1].Code = bcDREF_64)then
    begin
      Bytecode.Code.Data[i+0].Code := bcFMAD_d64_32;
      Bytecode.Code.Data[i+0].Args[3] := Bytecode.Code.Data[i+1].Args[0];
      Bytecode.Code.Data[i+1].Code := bcERROR;
      Inc(removals);
    end
    else if (Bytecode.Code.Data[i+0].Code = bcFMA_i32) and (Bytecode.Code.Data[i+1].Code = bcDREF_32)then
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
  writeIdx: Int32;
begin
  if Bytecode.Code.Size = 0 then Exit;

  SetLength(NewIndex, Bytecode.Code.Size);
  removalCount := 0;
  for i := 0 to Bytecode.Code.High do
  begin
    if Bytecode.Code.Data[i].Code = bcERROR then
    begin
      NewIndex[i] := i - removalCount;
      Inc(removalCount);
    end
    else
      NewIndex[i] := i - removalCount;
  end;

  if removalCount = 0 then Exit;

  for i := 0 to JumpZones.High do
  begin
    zone := JumpZones.Data[i];

    if Bytecode.Code.Data[zone.JmpFrom].Code = bcERROR then
      Continue;

    instr := @Bytecode.Code.Data[zone.JmpFrom];

    case zone.Kind of
      jkAbsolute:
      begin
        if zone.JmpTo >= Length(NewIndex) then
          newAbsoluteOffset := Bytecode.Code.Size - removalCount
        else
          newAbsoluteOffset := NewIndex[zone.JmpTo];

        instr^.Args[0].Data.Arg := newAbsoluteOffset;
      end;

      jkRelative:
      begin
        // If the instruction exactly AT the target was removed, find the NEXT valid target index!
        if zone.JmpTo + 1 >= Length(NewIndex) then
          newAbsoluteOffset := Bytecode.Code.Size - removalCount
        else
          newAbsoluteOffset := NewIndex[zone.JmpTo + 1] - 1;

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

  for i := 0 to High(Bytecode.FunctionTable) do
  begin
    funcEntry := Bytecode.FunctionTable[i];
    if funcEntry.CodeLocation < Length(NewIndex) then
      funcEntry.CodeLocation := NewIndex[funcEntry.CodeLocation];
    Bytecode.FunctionTable[i] := funcEntry;
  end;

  writeIdx := 0;
  for i := 0 to Bytecode.Code.High do
  begin
    if Bytecode.Code.Data[i].Code <> bcERROR then
    begin
      if i <> writeIdx then
      begin
        Bytecode.Code.Data[writeIdx] := Bytecode.Code.Data[i];
        Bytecode.Docpos.Data[writeIdx] := Bytecode.Docpos.Data[i];
        Bytecode.Settings.Data[writeIdx] := Bytecode.Settings.Data[i];
      end;
      Inc(writeIdx);
    end;
  end;

  Bytecode.Code.FTop := writeIdx - 1;
  Bytecode.Docpos.FTop := writeIdx - 1;
  Bytecode.Settings.FTop := writeIdx - 1;
end;

procedure TBytecodeEmitter.UpdateJumpsAtIR(InsertAt, Offset: Int32);
var
  NewIndex: array of Int32;
  i, oldFrom, newFrom, newTo: Int32;
  zone: TJumpZone;
  instrIR: ^TInstruction;
  newRelativeOffset: Int32;
  fEntry: TFunctionEntry;
begin
  if Intermediate.Code.Size = 0 then Exit;
  if Offset = 0 then Exit;

  SetLength(NewIndex, Intermediate.Code.Size + 1);
  for i := 0 to Intermediate.Code.Size do
    if i < InsertAt then NewIndex[i] := i else NewIndex[i] := i + Offset;

  for i := 0 to JumpZones.High do
  begin
    zone := JumpZones.Data[i];

    if (zone.JmpFrom < 0) or (zone.JmpFrom >= Intermediate.Code.Size) then Continue;
    if (zone.JmpTo   < 0) or (zone.JmpTo   >= Intermediate.Code.Size) then Continue;

    oldFrom := zone.JmpFrom;
    newFrom := NewIndex[zone.JmpFrom];
    newTo   := NewIndex[zone.JmpTo];

    instrIR := @Intermediate.Code.Data[oldFrom];

    case zone.Kind of
      jkAbsolute:
        case instrIR^.Code of
          icJMP, icIncTry: instrIR^.Args[0].Arg := newTo;
        end;
      jkRelative:
      begin
        newRelativeOffset := newTo - newFrom;

        case instrIR^.Code of
          icRELJMP, icJCONT, icJBREAK, icJFUNC:
            instrIR^.Args[0].Arg := newRelativeOffset;
          icJZ, icJNZ:
            instrIR^.Args[1].Arg := newRelativeOffset;
        end;
      end;
    end;

    zone.JmpFrom := newFrom;
    zone.JmpTo   := newTo;
    JumpZones.Data[i] := zone;
  end;

  for i := 0 to High(Intermediate.FunctionTable) do
  begin
    fEntry := Intermediate.FunctionTable[i];
    if fEntry.CodeLocation >= InsertAt then
      fEntry.CodeLocation := fEntry.CodeLocation + Offset;
    Intermediate.FunctionTable[i] := fEntry;
  end;
end;



(*
  Very basic constant folder, removes some true constant arith. ops.
  Replaces ADD/SUB/DIV/MUL etc.. of const with MOV result.
  Impact will generally be very small.
*)
procedure TBytecodeEmitter.ConstantFold();
var
  i: Int32;
  IR: ^TInstruction;
  left, right, result_v: Variant;
  destType: EExpressBaseType;

  // Read a compile-time value from an operand.
  // Only valid when Pos is mpImm or mpConst.
  function ReadOperand(const D: TInstructionData): Variant;
  var
    c: TConstant;
    tmp1: single;
    tmp2: double;
  begin
    if D.Pos = mpImm then
    begin
      case D.BaseType of
        xtInt8..xtInt64:   Result := D.Arg;
        xtUInt8..xtUInt64: Result := UInt64(D.Arg);
        xtSingle:
          begin
            Move(D.Arg, tmp1, SizeOf(Single));
            Result := tmp1;
          end;
        xtDouble:
          begin
            Move(D.Arg, tmp2, SizeOf(Double));
            Result := tmp2;
          end;
      else
        Result := Null;
      end;
    end
    else // mpConst
    begin
      c := Intermediate.Constants.Data[D.Arg];
      case D.BaseType of
        xtInt8..xtInt64:   Result := c.val_i64;
        xtUInt8..xtUInt64: Result := UInt64(c.val_i64);
        xtSingle:          Result := Single(c.val_f64);
        xtDouble:          Result := c.val_f64;
      else
        Result := Null;
      end;
    end;
  end;

  // Write a folded value back as mpImm into an instruction arg.
  procedure WriteImm(var D: TInstructionData; const V: Variant;
                     AType: EExpressBaseType);
  var
    i64: Int64;
    f32: Single;
    f64: Double;
  begin
    D.Pos      := mpImm;
    D.BaseType := AType;
    D.Arg      := 0;
    case AType of
      xtInt8..xtInt64:
        begin
          i64 := Int64(V);
          Move(i64, D.Arg, SizeOf(Int64));
        end;
      xtUInt8..xtUInt64:
        begin
          i64 := Int64(UInt64(V));
          Move(i64, D.Arg, SizeOf(Int64));
        end;
      xtSingle:
        begin
          f32 := Single(V);
          Move(f32, D.Arg, SizeOf(Single));
        end;
      xtDouble:
        begin
          f64 := Double(V);
          Move(f64, D.Arg, SizeOf(Double));
        end;
    end;
  end;

  function IsFoldableType(T: EExpressBaseType): Boolean;
  begin
    Result := T in (XprIntTypes + XprFloatTypes);
  end;

  function IsFoldablePos(Pos: EMemPos): Boolean;
  begin
    Result := Pos in [mpImm, mpConst];
  end;

begin
  for i := 0 to Intermediate.Code.High do
  begin
    IR := @Intermediate.Code.Data[i];

    // Only fold binary arithmetic and comparison ops
    if not (IR^.Code in [icADD, icSUB, icMUL, icDIV, icMOD, icPOW,
                          icEQ, icNEQ, icLT, icLTE, icGT, icGTE]) then
      Continue;

    // Both source operands must be compile-time constants
    if not IsFoldablePos(IR^.Args[0].Pos) then Continue;
    if not IsFoldablePos(IR^.Args[1].Pos) then Continue;

    // Only fold numeric types - no strings, pointers, managed types
    if not IsFoldableType(IR^.Args[0].BaseType) then Continue;
    if not IsFoldableType(IR^.Args[1].BaseType) then Continue;

    // Destination must be a local (something to write to)
    if IR^.Args[2].Pos <> mpLocal then Continue;

    // Guard against division by zero
    if (IR^.Code in [icDIV, icMOD]) then
    begin
      right := ReadOperand(IR^.Args[1]);
      if (VarType(right) in [varInteger, varInt64, varShortInt,
                              varSmallint, varByte, varWord,
                              varLongWord]) and (Int64(right) = 0) then
        Continue;
      if (VarType(right) in [varSingle, varDouble]) and (Double(right) = 0.0) then
        Continue;
    end;

    left     := ReadOperand(IR^.Args[0]);
    right    := ReadOperand(IR^.Args[1]);
    destType := IR^.Args[0].BaseType;

    if VarIsNull(left) or VarIsNull(right) then Continue;

    result_v := BinaryOp(IR^.Code, left, right, destType);

    if VarIsNull(result_v) then Continue;

    // Comparisons produce a boolean result
    if IR^.Code in [icEQ, icNEQ, icLT, icLTE, icGT, icGTE] then
      destType := xtBool;

    // Rewrite as icMOV dest, imm[folded_value]
    IR^.Code  := icMOV;
    IR^.nArgs := 2;
    // Args[0] = destination (keep as-is, it's already mpLocal)
    // Shift: dest moves to Args[0], result into Args[1]
    IR^.Args[0] := IR^.Args[2];  // dest local
    WriteImm(IR^.Args[1], result_v, destType);
  end;
end;





procedure TBytecodeEmitter.OptInlineIR();
type
  TPushRec = record
    OldIdx: Int32;
    Instr:  TInstruction;
  end;
var
  CSI, i, j, k: Int32;
  CallInstr, Repl: TInstruction;
  CFunc: TFunctionEntry;
  CBody: TInstructionList;
  CFrameSize, CallerNFIdx, CallerSTop, LOffset: Int32;
  PushRecs: array of TPushRec;
  PushCount: Int32;
  PopIdx: array of Int32;
  PopCount: Int32;
  RetJumps: specialize TArrayList<Int32>;
  BodyLen, pi, target, reloff, funcIdx: Int32;
  Zone: TJumpZone;

  function IsCandidate(const Instr: TInstruction): Boolean;
  var
    funcIdx, fl, k: Int32;
  begin
    Result := False;
    if Instr.Code <> icINVOKE then Exit;
    if Instr.Args[0].Pos = mpLocal then Exit;

    funcIdx := -1;
    for k := 0 to High(Self.Intermediate.FunctionTable) do
      if Self.Intermediate.FunctionTable[k].DataLocation = Instr.Args[0].Addr then
      begin
        funcIdx := k;
        break;
      end;

    if funcIdx < 0 then Exit;

    fl := Self.Intermediate.FunctionTable[funcIdx].CodeLocation + 1;
    if fl >= Self.Intermediate.Code.Size then Exit;
    if Self.Intermediate.Code.Data[fl].Code <> icNEWFRAME then Exit;

    Result := Self.Intermediate.Code.Data[fl].Args[0].Arg <= 256; // Express loves temps..
    Result := Result and Self.Intermediate.Settings.Data[fl+1].CanInline = True;
  end;

  function Clone(const AFunc: TFunctionEntry): TInstructionList;
  var
    S, cur, E, B, N, i: Int32;
    exceptStart, exceptInClone: Int32;
    Instr: TInstruction;
  begin
    Result.Init([]);

    S   := AFunc.CodeLocation;
    cur := S + 1;
    E   := Self.Intermediate.Code.Size - 1;

    while cur < Self.Intermediate.Code.Size do
    begin
      Instr := Self.Intermediate.Code.Data[cur];
      if (Instr.Code = icPASS) and (Instr.nArgs > 0) and
         (Instr.Args[0].Pos in [mpImm, mpConst]) and
         (Instr.Args[0].BaseType in XprStringTypes) then
        begin E := cur - 1; break; end;
      Inc(cur);
    end;

    B := S + 1;
    N := E - B + 1;
    if N <= 0 then Exit;

    SetLength(Result.Data, N);
    Result.FTop := N - 1;
    System.Move(Self.Intermediate.Code.Data[B], Result.Data[0], N * SizeOf(TInstruction));

    exceptStart   := -1;
    exceptInClone := -1;

    for i := 0 to Result.Size - 1 do
      case Result.Data[i].Code of
        icNEWFRAME: ;
        icIncTry:
        begin
          exceptStart   := Result.Data[i].Args[0].Addr;
          exceptInClone := exceptStart - B;
          Result.Data[i].Code  := icERROR;
          Result.Data[i].nArgs := 0;
        end;
        icDecTry, icRET_RAISE:
        begin
          Result.Data[i].Code  := icERROR;
          Result.Data[i].nArgs := 0;
        end;
      end;

    if (exceptInClone > 0) and (exceptInClone < Result.Size) then
      for i := exceptInClone to Result.Size - 1 do
      begin
        Result.Data[i].Code  := icERROR;
        Result.Data[i].nArgs := 0;
      end;
  end;

  function FindNF(From: Int32): Int32;
  var k: Int32;
  begin
    for k := From downto 0 do
      if Self.Intermediate.Code.Data[k].Code = icNEWFRAME then Exit(k);
    Result := -1;
  end;

  function BodyNF(const Body: TInstructionList): Int32;
  var k: Int32;
  begin
    for k := 0 to Body.Size - 1 do
      if Body.Data[k].Code = icNEWFRAME then Exit(Body.Data[k].Args[0].Arg);
    Result := -1;
  end;

  procedure IRInsert(From, N: Int32);
  var
    NewCode:     TInstructionList;
    NewDocPos:   TDocPosList;
    NewSettings: TSettingsList;
    Empty:       TInstruction;
    EmptyDP:     TDocPos;
    EmptyS:      TCompilerSettings;
    k, OldSz:   Int32;
  begin
    OldSz := Self.Intermediate.Code.Size;
    NewCode.Init([]);
    NewDocPos.Init([]);
    NewSettings.Init([]);

    FillChar(Empty,   SizeOf(Empty),   0);
    FillChar(EmptyDP, SizeOf(EmptyDP), 0);
    FillChar(EmptyS,  SizeOf(EmptyS),  0);

    for k := 0 to From - 1 do
    begin
      NewCode.Add(Self.Intermediate.Code.Data[k]);
      NewDocPos.Add(Self.Intermediate.DocPos.Data[k]);
      NewSettings.Add(Self.Intermediate.Settings.Data[k]);
    end;

    for k := 0 to N - 1 do
    begin
      NewCode.Add(Empty);
      NewDocPos.Add(EmptyDP);
      NewSettings.Add(EmptyS);
    end;

    for k := From to OldSz - 1 do
    begin
      NewCode.Add(Self.Intermediate.Code.Data[k]);
      NewDocPos.Add(Self.Intermediate.DocPos.Data[k]);
      NewSettings.Add(Self.Intermediate.Settings.Data[k]);
    end;

    Self.Intermediate.Code     := NewCode;
    Self.Intermediate.DocPos   := NewDocPos;
    Self.Intermediate.Settings := NewSettings;
  end;

  procedure SweepIntermediate();
  var
    NewIdx: array of Int32;
    removed, k: Int32;
    zone2: TJumpZone;
    ip: ^TInstruction;
    newRel, newAbs: Int32;
    fe: TFunctionEntry;
    writeIdx: Int32;
  begin
    if Self.Intermediate.Code.Size = 0 then Exit;

    SetLength(NewIdx, Self.Intermediate.Code.Size);
    removed := 0;
    for k := 0 to Self.Intermediate.Code.High do
    begin
      if Self.Intermediate.Code.Data[k].Code = icERROR then
      begin
        NewIdx[k] := k - removed;
        Inc(removed);
      end
      else
        NewIdx[k] := k - removed;
    end;

    if removed = 0 then Exit;

    Self.BuildJumpZones();

    for k := 0 to JumpZones.High do
    begin
      zone2 := JumpZones.Data[k];

      if Self.Intermediate.Code.Data[zone2.JmpFrom].Code = icERROR then Continue;

      ip := @Self.Intermediate.Code.Data[zone2.JmpFrom];

      case zone2.Kind of
        jkAbsolute:
        begin
          if zone2.JmpTo >= Length(NewIdx) then
            newAbs := Self.Intermediate.Code.Size - removed
          else
            newAbs := NewIdx[zone2.JmpTo];

          case ip^.Code of
            icJMP, icIncTry: ip^.Args[0].Arg := newAbs;
          end;
        end;
        jkRelative:
        begin
          if zone2.JmpTo + 1 >= Length(NewIdx) then
            newAbs := Self.Intermediate.Code.Size - removed
          else
            newAbs := NewIdx[zone2.JmpTo + 1] - 1;

          newRel := newAbs - NewIdx[zone2.JmpFrom];
          case ip^.Code of
            icRELJMP, icJCONT, icJBREAK, icJFUNC: ip^.Args[0].Arg := newRel;
            icJZ, icJNZ:                          ip^.Args[1].Arg := newRel;
          end;
        end;
      end;
    end;

    for k := 0 to High(Self.Intermediate.FunctionTable) do
    begin
      fe := Self.Intermediate.FunctionTable[k];
      if (fe.CodeLocation < Length(NewIdx)) then
        fe.CodeLocation := NewIdx[fe.CodeLocation];
      Self.Intermediate.FunctionTable[k] := fe;
    end;

    writeIdx := 0;
    for k := 0 to Self.Intermediate.Code.High do
    begin
      if Self.Intermediate.Code.Data[k].Code <> icERROR then
      begin
        if k <> writeIdx then
        begin
          Self.Intermediate.Code.Data[writeIdx] := Self.Intermediate.Code.Data[k];
          Self.Intermediate.DocPos.Data[writeIdx] := Self.Intermediate.DocPos.Data[k];
          Self.Intermediate.Settings.Data[writeIdx] := Self.Intermediate.Settings.Data[k];
        end;
        Inc(writeIdx);
      end;
    end;

    Self.Intermediate.Code.FTop := writeIdx - 1;
    Self.Intermediate.DocPos.FTop := writeIdx - 1;
    Self.Intermediate.Settings.FTop := writeIdx - 1;
  end;

begin
  CSI := 0;
  while CSI < Self.Intermediate.Code.Size do
  begin
    CallInstr := Self.Intermediate.Code.Data[CSI];

    if not IsCandidate(CallInstr) then
    begin
      Inc(CSI);
      Continue;
    end;

    CallerNFIdx := FindNF(CSI);
    if CallerNFIdx = -1 then begin Inc(CSI); Continue; end;

    funcIdx := -1;
    for k := 0 to High(Self.Intermediate.FunctionTable) do
      if Self.Intermediate.FunctionTable[k].DataLocation = CallInstr.Args[0].Addr then
      begin
        funcIdx := k;
        break;
      end;
    if funcIdx < 0 then begin Inc(CSI); Continue; end;

    CFunc := Self.Intermediate.FunctionTable[funcIdx];
    CBody := Clone(CFunc);

    if CBody.Size = 0 then begin Inc(CSI); Continue; end;

    CFrameSize := BodyNF(CBody);
    if CFrameSize < 0 then begin Inc(CSI); Continue; end;

    CallerSTop := Self.Intermediate.Code.Data[CallerNFIdx].Args[0].Arg;
    LOffset    := CallerSTop;

    j := CSI - 1;
    while (j >= 0) and (Self.Intermediate.Code.Data[j].Code in[icPUSH, icPUSHREF]) do
      Dec(j);
    Inc(j);
    PushCount := CSI - j;

    SetLength(PushRecs, PushCount);
    for i := 0 to PushCount - 1 do
    begin
      PushRecs[i].OldIdx := j + i;
      PushRecs[i].Instr  := Self.Intermediate.Code.Data[j + i];
    end;

    for i := 0 to CBody.Size - 1 do
      for k := 0 to CBody.Data[i].nArgs - 1 do
        if CBody.Data[i].Args[k].Pos = mpLocal then
          Inc(CBody.Data[i].Args[k].Arg, LOffset);

    CBody.Data[0].Code  := icERROR;
    CBody.Data[0].nArgs := 0;

    PopCount := 0;
    i := 1;
    while (i < CBody.Size) and (CBody.Data[i].Code in[icPOP, icPOPH]) do
      begin Inc(PopCount); Inc(i); end;

    if PopCount <> PushCount then begin Inc(CSI); Continue; end;

    SetLength(PopIdx, PopCount);
    for i := 0 to PopCount - 1 do
      PopIdx[i] := 1 + i;

    for k := 0 to PopCount - 1 do
    begin
      pi := PushCount - 1 - k;
      FillChar(Repl, SizeOf(Repl), 0);

      if CBody.Data[PopIdx[k]].Code = icPOPH then
      begin
        Repl.Code    := icADDR;
        Repl.nArgs   := 2;
        Repl.Args[0] := CBody.Data[PopIdx[k]].Args[0];
        Repl.Args[1] := PushRecs[pi].Instr.Args[0];
      end
      else
      begin
        Repl.Code             := icMOV;
        Repl.nArgs            := 2;
        Repl.Args[0]          := CBody.Data[PopIdx[k]].Args[1];
        Repl.Args[0].BaseType := CBody.Data[PopIdx[k]].Args[1].BaseType;
        Repl.Args[1]          := PushRecs[pi].Instr.Args[0];
      end;

      CBody.Data[PopIdx[k]] := Repl;
    end;

    RetJumps.Init([]);
    for i := 0 to CBody.Size - 1 do
      if CBody.Data[i].Code = icRET then
      begin
        FillChar(CBody.Data[i].Args[0], SizeOf(TInstructionData), 0);
        CBody.Data[i].Code        := icRELJMP;
        CBody.Data[i].Args[0].Arg := $7FFFFFFE; // sentinel
        CBody.Data[i].Args[0].Pos := mpImm;
        CBody.Data[i].Args[0].BaseType := xtUnknown;
        CBody.Data[i].nArgs       := 1;
        RetJumps.Add(i);
      end;

    BodyLen := CBody.Size;

    UpdateJumpsAtIR(CSI, BodyLen);
    IRInsert(CSI, BodyLen);

    for i := 0 to BodyLen - 1 do
    begin
      Self.Intermediate.Code.Data[CSI + i] := CBody.Data[i];
      Self.Intermediate.DocPos.Data[CSI + i]   := Self.Intermediate.DocPos.Data[CSI + BodyLen];
      Self.Intermediate.Settings.Data[CSI + i] := Self.Intermediate.Settings.Data[CSI + BodyLen];
    end;

    target := CSI + BodyLen;
    for i := 0 to RetJumps.High do
    begin
      pi     := CSI + RetJumps.Data[i];
      reloff := target - pi - 1;
      Self.Intermediate.Code.Data[pi].Args[0].Arg := reloff;
      Zone.JmpFrom := pi;
      Zone.JmpTo   := target - 1;
      Zone.Kind    := jkRelative;
      JumpZones.Add(Zone);
    end;

    for i := 0 to PushCount - 1 do
      Self.Intermediate.Code.Data[j + i].Code := icERROR;
    Self.Intermediate.Code.Data[CSI + BodyLen].Code := icERROR;

    Self.Intermediate.Code.Data[CallerNFIdx].Args[0].Arg :=
      CallerSTop + CFrameSize;

    Self.BuildJumpZones();

    Inc(CSI, BodyLen);
  end;

  SweepIntermediate();
end;

end.
