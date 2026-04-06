unit xpr.Bytecode;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
{$I header.inc}

interface

uses
  Classes, SysUtils,
  xpr.Types,
  xpr.Tokenizer,
  xpr.Intermediate;

type
  EBytecode = (
    bcNOOP,
    bcSUPER, bcHOTLOOP, bcJIT,

    // flow control
    bcJMP, bcRELJMP,
    bcJZ,  bcJNZ,

    // dynamic objects
    bcNEW,
    bcRELEASE,
    bcDYNCAST,
    bcIS,

    // functions
    bcNEWFRAME,

    bcPUSH, bcPUSHREF,
    bcPUSH_FP, bcPUSH_CLOSURE,
    bcPOP, bcRPOP,
    bcPOPH,

    bcLOAD_GLOBAL, bcLOAD_EXTERN, bcCOPY_GLOBAL,

    bcPRINT,
    bcINVOKE, bcINVOKEX, bcINVOKE_VIRTUAL,
    bcFFICALL, bcFFICALL_DYN,
    bcRET,
    bcRET_RAISE,

    bcSPAWN,
    bcCREATE_CALLBACK,

    // fill byte
    bcFILL,

    // try-except
    bcSET_ERRHANDLER,
    bcRAISE,
    bcGET_EXCEPTION,
    bcUNSET_EXCEPTION,
    bcIncTry,
    bcDecTry,

    // array managment
    bcREFCNT,
    bcINCLOCK, bcDECLOCK,
    bcBCHK,

    // handling string table to var assignment, and basic string operators
    // bs is a byte string, us is a unicode string
    bcLOAD_STR, bcLOAD_USTR,
    bcADD_STR, bcADD_USTR,
    bcCh2Str, bcCh2UStr,

    // ----- WARNING -------------------------------------------
    // ORDER BEYOND HERE WILL AFFECT SUPERINSTRUCTIONS & JIT
    // ---- DONT TOUCH -----------------------------------------
    bcADDR,
    bcDREF, bcDREF32, bcDREF64,

    bcINC_i32, bcINC_u32, bcINC_i64, bcINC_u64,

    // specialized ops
    bcFMAD64_64, bcFMAD64_32, bcFMAD32_64, bcFMAD32_32,
    bcFMA_i8, bcFMA_u8, bcFMA_i16, bcFMA_u16, bcFMA_i32, bcFMA_u32, bcFMA_i64, bcFMA_u64,

    // "slowpath" [< i32]
    bcADD, bcSUB, bcMUL, bcDIV, bcMOD, bcPOW,
    bcEQ,  bcNE,  bcLT,  bcGT,  bcGTE, bcLTE,
    bcBND, bcSHL, bcSHR, bcXOR, bcBOR, bcSAR,

    // Specialize for local dest
    bcADD_i32, bcADD_u32, bcADD_i64, bcADD_u64, bcADD_f32, bcADD_f64,
    bcSUB_i32, bcSUB_u32, bcSUB_i64, bcSUB_u64, bcSUB_f32, bcSUB_f64,
    bcMUL_i32, bcMUL_u32, bcMUL_i64, bcMUL_u64, bcMUL_f32, bcMUL_f64,
    bcDIV_i32, bcDIV_u32, bcDIV_i64, bcDIV_u64, bcDIV_f32, bcDIV_f64,
    bcMOD_i32, bcMOD_u32, bcMOD_i64, bcMOD_u64, bcMOD_f32, bcMOD_f64,
    bcPOW_i32, bcPOW_u32, bcPOW_i64, bcPOW_u64, bcPOW_f32, bcPOW_f64,

    bcEQ_i32, bcEQ_u32, bcEQ_i64, bcEQ_u64, bcEQ_f32, bcEQ_f64,
    bcNE_i32, bcNE_u32, bcNE_i64, bcNE_u64, bcNE_f32, bcNE_f64,
    bcLT_i32, bcLT_u32, bcLT_i64, bcLT_u64, bcLT_f32, bcLT_f64,
    bcGT_i32, bcGT_u32, bcGT_i64, bcGT_u64, bcGT_f32, bcGT_f64,
    bcGTE_i32, bcGTE_u32, bcGTE_i64, bcGTE_u64, bcGTE_f32, bcGTE_f64,
    bcLTE_i32, bcLTE_u32, bcLTE_i64, bcLTE_u64, bcLTE_f32, bcLTE_f64,

    bcBND_i32, bcBND_u32, bcBND_i64, bcBND_u64,
    bcSHL_i32, bcSHL_u32, bcSHL_i64, bcSHL_u64,
    bcSHR_i32, bcSHR_u32, bcSHR_i64, bcSHR_u64,
    bcXOR_i32, bcXOR_u32, bcXOR_i64, bcXOR_u64,
    bcBOR_i32, bcBOR_u32, bcBOR_i64, bcBOR_u64,
    bcSAR_i32, bcSAR_u32, bcSAR_i64, bcSAR_u64,

    // ---------------------------------------------
    // MOV Operations (Destination, Source)
    // ---------------------------------------------
    bcMOV,

    // --- Same-Size or Truncating (Narrowing)
    bcMOV8, bcMOV16, bcMOV32, bcMOV64,

    // --- Widening Signed (Sign Extension)
    bcMOVSX16_8,
    bcMOVSX32_8, bcMOVSX32_16,
    bcMOVSX64_8, bcMOVSX64_16, bcMOVSX64_32,

    // --- Widening Unsigned (Zero Extension)
    bcMOVZX16_8,
    bcMOVZX32_8, bcMOVZX32_16,
    bcMOVZX64_8, bcMOVZX64_16, bcMOVZX64_32,

    // --- Float Source Types (f32, f64) ---
    // --- Destination Types (i32, u32, i64, u64, f32, f64) ---
    bcMOVF32_i32, bcMOVF32_u32, bcMOVF32_i64, bcMOVF32_u64, bcMOVF32_f32, bcMOVF32_f64,
    bcMOVF64_i32, bcMOVF64_u32, bcMOVF64_i64, bcMOVF64_u64, bcMOVF64_f32, bcMOVF64_f64,

    // ---------------------------------------------
    // STORE Operations (Destination Address, Source)
    // Write from stack into memory
    // ---------------------------------------------
    bcSTORE,

    // --- Same-Size or Truncating (Narrowing) ---
    bcSTORE8, bcSTORE16, bcSTORE32, bcSTORE64,

    // --- Widening Signed (Sign Extension) ---
    bcSTORESX16_8,
    bcSTORESX32_8, bcSTORESX32_16,
    bcSTORESX64_8, bcSTORESX64_16, bcSTORESX64_32,

    // --- Widening Unsigned (Zero Extension) ---
    bcSTOREZX16_8,
    bcSTOREZX32_8, bcSTOREZX32_16,
    bcSTOREZX64_8, bcSTOREZX64_16, bcSTOREZX64_32,

    // --- Float Source Types (f32, f64) ---
    // --- Destination Types (i32, u32, i64, u64, f32, f64) ---
    bcSTOREF32_i32, bcSTOREF32_u32, bcSTOREF32_i64, bcSTOREF32_u64, bcSTOREF32_f32, bcSTOREF32_f64,
    bcSTOREF64_i32, bcSTOREF64_u32, bcSTOREF64_i64, bcSTOREF64_u64, bcSTOREF64_f32, bcSTOREF64_f64,

    bcERROR
  );


  TOperandData = record
    case Byte of
      0: (Raw: array[0..7] of Byte);
      1: (Raw32: array[0..3] of Byte);

      2: (Addr: PtrInt);

      3: (Arg: Int64);
      4: (u64: UInt64);

      5: (i32: Int32);
      6: (i16: Int16);
      7: (i8: Int8);

      8: (u32: UInt32);
      9: (u16: UInt16);
      10: (u8: UInt8);
  end;

  // Instruction argument metadata
  TOperand = packed record
    Data: TOperandData;
    Pos: EMemPos;
    BaseType: EExpressBaseType;
  end;

  // Intermediate instruction record (fits in one cache line)
  TBytecodeInstruction = record
    Args: array[0..4] of TOperand;
    Code: EBytecode;
    nArgs: Byte;
  end;
  PBytecodeInstruction = ^TBytecodeInstruction;

  TProgramData = specialize TArrayList<TBytecodeInstruction>;

  TBytecode = record
    Code: TProgramData;
    Docpos: TDocPosList;
    Settings: TSettingsList;
    FunctionTable: TFunctionTable;
    StringTable: TStringArray;
    ClassVMTs: TVMTList;
    NativeImports: array of Pointer;
    Constants: TConstantList;

    procedure Init();
    procedure Free();

    function Add(OP: TBytecodeInstruction; Pos: TDocPos): Int32;
    function Delete(Index: Int32): TBytecodeInstruction;

    function ToString(Colorize: Boolean = False): string;
  end;
  PBytecode = ^TBytecode;



implementation

uses math, typinfo, xpr.Utils;


procedure TBytecode.Init();
begin
  Self.Code.Init([]);
  Self.Docpos.Init([]);
  Self.Settings.Init([]);
end;

procedure TBytecode.Free();
begin
  Self.Code.Free();
  Self.Docpos.Free();
end;

function TBytecode.Add(OP: TBytecodeInstruction; Pos: TDocPos): Int32;
begin
  Self.DocPos.Add(Pos);
  Result := Self.Code.Add(OP);
end;

function TBytecode.Delete(Index: Int32): TBytecodeInstruction;
begin
  Self.DocPos.Delete(Index);
  Result := Self.Code.Pop(Index);
end;


function TBytecode.ToString(Colorize: Boolean = False): string;
var
  i, j: Integer;
  this: TBytecodeInstruction;
  lineStr, idxStr, opStr, argStr, posColor, posName, typeStr, valStr: string;
  TSA: TStringArray;
  isFunction: Boolean;
begin
  Result := '';
  for i := 0 to Code.High do
  begin
    this := Code.Data[i];

    if (this.Code = bcNOOP) and (this.nArgs = 1) and (this.Args[0].BaseType = xtString) then
    begin
      Result += 'Function body: '+Self.StringTable[this.Args[0].Data.Addr] + LineEnding;
      continue;
    end;

    // Format line and index
    if Colorize then
    begin
      lineStr := _LWHITE_ + 'L' + DocPos.Data[i].Line.ToString + _WHITE_;
      idxStr  := _LWHITE_ + '#' + i.ToString + _WHITE_;

      // debugging purpose coloring
      if (this.Code = bcREFCNT) then
        opStr := _LYELLOW_ + GetEnumName(TypeInfo(EBytecode), Ord(this.Code)) + _WHITE_
      else if (this.Code = bcDECLOCK) then
        opStr := _LRED_ + GetEnumName(TypeInfo(EBytecode), Ord(this.Code)) + _WHITE_
      else if (this.Code = bcINCLOCK) then
        opStr := _LGREEN_ + GetEnumName(TypeInfo(EBytecode), Ord(this.Code)) + _WHITE_
      else
        opStr := _AQUA_ + GetEnumName(TypeInfo(EBytecode), Ord(this.Code)) + _WHITE_;
    end
    else
    begin
      lineStr := 'L' + DocPos.Data[i].Line.ToString;
      idxStr  := '#' + i.ToString;
      opStr   := GetEnumName(TypeInfo(EBytecode), Ord(this.Code));
    end;

    // Build aligned argument string: pos(type:value)
    argStr := '';
    for j := 0 to this.nArgs - 1 do
    begin
      with this.Args[j] do
      begin
        // Memory position label
        case Pos of
          mpImm:    begin posName := 'imm  '; if Colorize then posColor := _PURPLE_ else posColor := ''; end;
          mpLocal:  begin posName := 'loc  '; if Colorize then posColor := _YELLOW_ else posColor := ''; end;
          mpGlobal: begin posName := 'glob '; if Colorize then posColor := _LGREEN_ else posColor := ''; end;
          mpHeap:   begin posName := 'heap '; if Colorize then posColor := _GREEN_  else posColor := ''; end;
          mpConst:  begin posName := 'const'; if Colorize then posColor := _BLUE_   else posColor := ''; end;
          else      begin posName := 'unk  '; if Colorize then posColor := _RED_    else posColor := ''; end;
        end;

        // Type and value
        typeStr := BT2SM(BaseType);
        if (this.Code = bcINVOKE) and (j = 0) and (this.Args[1].BaseType = xtString) then
        begin
          valStr := Self.StringTable[this.Args[1].Data.Addr];
          if Length(valStr) > 8 then begin SetLength(ValStr, 6); valStr += '..'; end;
        end else  begin
          valStr := IntToStr(Data.Arg);
          if Length(valStr) > 8 then begin SetLength(ValStr, 6); valStr += '..'; end;
        end;
        // Compose formatted argument string
        argStr += Format('%s%s[%-3s:%s] %s|', [posColor, posName, typeStr, valStr, _WHITE_]);
      end;
    end;

    // Final line assembly
    lineStr := Format('%-15s %-12s %-28s', [lineStr, idxStr, opStr]);

    TSA := argStr.Split('|');
    argStr := '';
    for j:=0 to High(TSA)-1 do
      argStr += '%-29s';
    argStr += '%s';

    case Length(TSA) of
      1: argStr := Format(argStr, [TSA[0]]);
      2: argStr := Format(argStr, [TSA[0], TSA[1]]);
      3: argStr := Format(argStr, [TSA[0], TSA[1], TSA[2]]);
      4: argStr := Format(argStr, [TSA[0], TSA[1], TSA[2], TSA[3]]);
      5: argStr := Format(argStr, [TSA[0], TSA[1], TSA[2], TSA[3], TSA[4]]);
      6: argStr := Format(argStr, [TSA[0], TSA[1], TSA[2], TSA[3], TSA[4], TSA[5]]);
    else argStr := '';
    end;

    Result += lineStr + argStr + LineEnding;
  end;
end;


end.
