unit xpr.Intermediate;
{
  Author: Jarl K. Holta  
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
  
  Intermediate representation for Expr language
  Simplified, generic operations before final bytecode lowering

}
{$I header.inc}

interface

uses 
  SysUtils, xpr.Types, xpr.Tokenizer, xpr.Dictionary;

type
  EIntermediate = (
    icNOOP,
    icERROR,

    icPASS,
    // Control flow
    icJMP, icRELJMP, icJFUNC, icJCONT, icJBREAK,
    // Dynamic Object
    icNEW,
    icRELEASE,
    icDYNCAST,
    icIS,
    // Function frame allocation
    icNEWFRAME,
    // Stack manipulation
    icPUSH, icPUSHREF,
    icPUSH_FP,
    icPOP, icRPOP,
    icPOPH, icPOPPtr,
    icLOAD_GLOBAL, icLOAD_NONLOCAL, icCOPY_GLOBAL,
    // Function calls
    icPRINT,
    icINVOKE, icINVOKEX, icINVOKE_VIRTUAL,
    icRET,
    // Conditional jumps
    icJZ,
    icJNZ,
    // try-except
    icRAISE,
    icGET_EXCEPTION,
    icSET_ERRHANDLER,
    icIncTry,
    icDecTry,
    // Data movement and management
    icFILL,
    icMOV,
    icMOVH,
    icADDR,
    icDREF,
    // Array management
    icREFCNT, icINCLOCK, icDECLOCK,
    icBCHK,
    // Fused multiply-add (generic)
    icFMA,
    // Unary minus
    icUSUB,
    // Binary arithmetic and logic
    icADD, icSUB, icMUL, icDIV, icMOD, icPOW,
    icEQ, icGTE, icGT, icLT, icLTE, icNEQ,
    icBND, icBOR, icSHL, icSHR, icXOR, icSAR,
    icNOT, icINV
  );

  TConstant = record
    typ: EExpressBaseType;
    case Byte of
      0: (raw: array[0..7] of Byte);
      1: (val_b: Boolean);
      2: (val_c: AnsiChar);
      3: (val_uc: WideChar);
      4: (val_i8: Int8);
      5: (val_i16: Int16);
      6: (val_i32: Int32);
      7: (val_i64: Int64);
      8: (val_u8: UInt8);
      9: (val_u16: UInt16);
      10: (val_u32: UInt32);
      11: (val_u64: UInt64);
      12: (val_f32: Single);
      13: (val_f64: Double);
      14: (val_p: Pointer);
  end;
  TConstantList = specialize TArrayList<TConstant>;

  // Instruction argument metadata
  TInstructionData = packed record
    Pos: EMemPos;
    BaseType: EExpressBaseType;
    IsTemporary: Boolean;

    case Byte of
      0: (Arg: Int64);
      1: (Addr: PtrInt);
      2: (i32: Int32);
  end;
  TInstructionDataList = specialize TArrayList<TInstructionData>;

  TInstruction = record
    Args: array[0..4] of TInstructionData;
    Code: EIntermediate;
    nArgs: Byte;
  end;
  TInstructionList = specialize TArrayList<TInstruction>;
  TDocPosList      = specialize TArrayList<TDocPos>;

  TIntermediateCode = record
    Code: TInstructionList;
    DocPos: TDocPosList;
    Constants: TConstantList;
    StackPosArr: array of SizeInt;
    FunctionTable: TFunctionTable;
    StringTable: TStringArray;
    ClassVMTs: specialize TArrayList<TVirtualMethodTable>;

    procedure Init();
    procedure Free();

    function AddInstruction(const OP: TInstruction; Pos: TDocPos): Int32; inline;
    function RemoveInstructionId(Index: Int32): TInstruction; inline;
    function GetTop(): EIntermediate;
    function ToString(Colorize: Boolean = False): string;
  end;

function Constant(constref Value: Variant; Typ: EExpressBaseType): TConstant;
procedure Swap(var x,y: TInstructionData); inline;

operator = (Left, Right: TInstructionData): Boolean;

implementation

uses typinfo, xpr.Utils;

operator = (Left, Right: TInstructionData): Boolean;
begin
  // Dont check basetype, as it might be irrelevant, legal casts.
  Result := (Left.Pos = Right.Pos) and (Left.Addr = Right.Addr);
end;

procedure Swap(var x,y: TInstructionData);
var t: TInstructionData;
begin
  t := y;
  y := x;
  x := t;
end;

function Constant(constref Value: Variant; Typ: EExpressBaseType): TConstant;
begin
  Result.Typ := Typ;
  Result.val_i64 := 0; // zero fill
  case Typ of
    xtBoolean:    Result.val_b   := Boolean(Value);
    xtAnsiChar:   Result.val_c   := AnsiChar(Value);
    xtUnicodeChar: Result.val_uc  := UnicodeChar(Value);
    xtInt8:       Result.val_i8  := Int8(Value);
    xtInt16:      Result.val_i16 := Int16(Value);
    xtInt32:      Result.val_i32 := Int32(Value);
    xtInt64:      Result.val_i64 := Int64(Value);
    xtUInt8:      Result.val_u8  := UInt8(Value);
    xtUInt16:     Result.val_u16 := UInt16(Value);
    xtUInt32:     Result.val_u32 := UInt32(Value);
    xtUInt64:     Result.val_u64 := UInt64(Value);
    xtSingle:     Result.val_f32 := Single(Value);
    xtDouble:     Result.val_f64 := Double(Value);
    xtPointer:    Result.val_p   := Pointer(PtrUInt(Value));
    xtAnsiString,
    xtUnicodeString: Result.val_p := @Value;
  end;
end;

procedure TIntermediateCode.Init();
begin
  ClassVMTs.Init([]);
  Code.Init([]);
  DocPos.Init([]);
  Constants.Init([]);
end;

procedure TIntermediateCode.Free();
begin
  Code.Free;
  DocPos.Free;
  Constants.Free;
end;

function TIntermediateCode.AddInstruction(const OP: TInstruction; Pos: TDocPos): Int32;
begin
  DocPos.Add(Pos);
  Result := Code.Add(OP);
end;

function TIntermediateCode.RemoveInstructionId(Index: Int32): TInstruction;
begin
  DocPos.Delete(Index);
  Result := Code.Pop(Index);
end;

function TIntermediateCode.GetTop(): EIntermediate;
begin
  if Code.High < 0 then
    Exit(icNOOP);
  Result := Code.Data[Code.High].Code;
end;

function TIntermediateCode.ToString(Colorize: Boolean = False): string;
var
  i, j: Integer;
  this: TInstruction;
  lineStr, idxStr, opStr, argStr, posColor, posName, typeStr, valStr: string;
  TSA: TStringArray;
begin
  Result := '';
  for i := 0 to Code.High do
  begin
    this := Code.Data[i];

    if (this.Code = icPASS) and (this.nArgs = 1) and (this.Args[0].BaseType = xtString) then
    begin
      Result += 'Function body: '+Self.StringTable[this.Args[0].Addr] + LineEnding;
      continue;
    end;

    // Format line and index
    if Colorize then
    begin
      lineStr := _LWHITE_ + 'L' + DocPos.Data[i].Line.ToString + _WHITE_;
      idxStr  := _LWHITE_ + '#' + i.ToString + _WHITE_;
      opStr   := _AQUA_    + GetEnumName(TypeInfo(EIntermediate), Ord(this.Code)) + _WHITE_;
    end
    else
    begin
      lineStr := 'L' + DocPos.Data[i].Line.ToString;
      idxStr  := '#' + i.ToString;
      opStr   := GetEnumName(TypeInfo(EIntermediate), Ord(this.Code));
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
          mpHeap:   begin posName := 'heap '; if Colorize then posColor := _GREEN_  else posColor := ''; end;
          mpConst:  begin posName := 'const'; if Colorize then posColor := _BLUE_   else posColor := ''; end;
        else        begin posName := 'unk  '; if Colorize then posColor := _RED_    else posColor := ''; end;
        end;

        // Type and value
        typeStr := BT2SM(BaseType);
        if (this.Code = icINVOKE) and (j = 0) and (this.Args[2].BaseType = xtString) then
        begin
          valStr := Self.StringTable[this.Args[2].Addr];
        end else  begin
          valStr := IntToStr(Arg);
        end;
        if Length(valStr) > 8 then begin SetLength(ValStr, 6); valStr += '..'; end;

        // Compose formatted argument string
        argStr += Format('%s%s[%-3s:%s] %s|',
          [posColor, posName, typeStr, valStr, _WHITE_]);
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
