unit xpr.Langdef;
{
  Author: Jarl K. Holta  
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
{$I header.inc}

interface

uses
  SysUtils, xpr.Dictionary, xpr.Types, xpr.Tokenizer, xpr.Intermediate, xpr.Bytecode;

type
  TOperatorPrecedence = record
    Prec, Assoc: Int8;
  end;
  TOperatorPrecedenceMap = specialize TDictionary<ETokenKind, TOperatorPrecedence>;

  TOperatorDef = record
    OP: ETokenKind;
    Prec, Assoc: Int8;
  end;

const
  BinPrecedence: array [0..37] of TOperatorDef = (
    // Level 0: Assignments
    (OP:tkASGN;       Prec:0;   Assoc:-1),
    (OP:tkPLUS_ASGN;  Prec:0;   Assoc:-1),
    (OP:tkMINUS_ASGN; Prec:0;   Assoc:-1),
    (OP:tkMUL_ASGN;   Prec:0;   Assoc:-1),
    (OP:tkDIV_ASGN;   Prec:0;   Assoc:-1),
    (OP:tkBND_ASGN;   Prec:0;   Assoc:-1),
    (OP:tkBOR_ASGN;   Prec:0;   Assoc:-1),
    (OP:tkXOR_ASGN;   Prec:0;   Assoc:-1),

    // Level 1: Ternary If Expression
    (OP:tkKW_IF;Prec:1;   Assoc:-1),

     // Level 2 & 3: Logical Operators
    (OP:tkAND;  Prec:2;   Assoc:-1),
    (OP:tkOR;   Prec:3;   Assoc:-1),

    // level 4: Dynamic cast
    (OP:tkKW_AS;Prec:4;   Assoc:1),

    // Level 5: Equality Operators
    (OP:tkEQ;         Prec:5;   Assoc:1),
    (OP:tkNE;         Prec:5;   Assoc:1),
    (OP:tkKW_IS;      Prec:5;   Assoc:1),
    (OP:tkKW_ISNOT;   Prec:5;   Assoc:1),

    // Level 6: Relational Operators
    (OP:tkLT;         Prec:6;   Assoc:1),
    (OP:tkGT;         Prec:6;   Assoc:1),
    (OP:tkLTE;        Prec:6;   Assoc:1),
    (OP:tkGTE;        Prec:6;   Assoc:1),

    // Level 7: Bitwise Operators
    (OP:tkBOR;        Prec:7;   Assoc:1),
    (OP:tkBND;        Prec:7;   Assoc:1),
    (OP:tkXOR;        Prec:7;   Assoc:1),

    // Level 8: Additive Operators
    (OP:tkPLUS;       Prec:8;   Assoc:1),
    (OP:tkMINUS;      Prec:8;   Assoc:1),

    // Level 9: Multiplicative and Shift Operators
    (OP:tkMUL;        Prec:9;   Assoc:1),
    (OP:tkDIV;        Prec:9;   Assoc:1),
    (OP:tkMOD;        Prec:9;   Assoc:1),
    (OP:tkSHL;        Prec:9;   Assoc:1),
    (OP:tkSHR;        Prec:9;   Assoc:1),
    (OP:tkSAR;        Prec:9;   Assoc:1),

    // Level 10: Exponentiation
    (OP:tkPOW;        Prec:10;  Assoc:1),

    // Level 11: Other high-precedence operators
    (OP:tkIN;         Prec:11;  Assoc:1),

    // Level 12: Member Access and Function Call (highest precedence)
    (OP:tkDOT;         Prec:12;  Assoc:1),
    (OP:tkINDEX;       Prec:12;  Assoc:1),
    (OP:tkLPARENTHESES;Prec:12;  Assoc:-1),
    (OP:tkLCURLY;      Prec:12;  Assoc:1),
    (OP:tkDEREF;       Prec:12;  Assoc:1)
  );
  
  UnaryPrecedence: array [0..4] of TOperatorDef = (
  //(OP:tkInc;   Prec:0;  Assoc:0),
  //(OP:tkDec;   Prec:0;  Assoc:0),
    (OP:tkPLUS;  Prec:0;  Assoc:0),
    (OP:tkMINUS; Prec:0;  Assoc:0),

    (OP:tkINV;   Prec:0;  Assoc:0),
    (OP:tkNOT;   Prec:0;  Assoc:0),
    (OP:tkAT;    Prec:0;  Assoc:0)
  );

var
  PrecedenceMap: TOperatorPrecedenceMap;
  UnaryPrecedenceMap: TOperatorPrecedenceMap;

function OP2IC(OP: EOperator): EIntermediate;
function GetEvalRes(OP: EOperator; Left, Right: EExpressBaseType): EExpressBaseType; inline;


var
  EvalResArr:  array [EOperator, EExpressBaseType, EExpressBaseType] of EExpressBaseType;
  op2instruct: array [EOperator, EExpressBaseType, EExpressBaseType] of EByteCode;

implementation

uses
  Math, xpr.CompilerContext;


function OP2IC(OP: EOperator): EIntermediate;
begin
  case OP of
    op_AS:    Result := icDYNCAST;
    op_IS:    Result := icIS;
    op_Addr:  Result := icADDR;
    op_AND:   Result := icJZ;    // logical AND compiled to “jump if zero”
    op_OR:    Result := icJNZ;   // logical OR compiled to “jump if not zero”
    op_Deref: Result := icDREF;
    op_Asgn:  Result := icMOV;
    op_ADD:   Result := icADD;
    op_BND:   Result := icBND;
    op_BOR:   Result := icBOR;
    op_DIV:   Result := icDIV;
    op_EQ:    Result := icEQ;
    op_GT:    Result := icGT;
    op_GTE:   Result := icGTE;
    op_IN:    Result := icNOOP;
    op_IF:    Result := icNOOP;
    op_INV:   Result := icINV;
    op_LT:    Result := icLT;
    op_LTE:   Result := icLTE;
    op_MUL:   Result := icMUL;
    op_MOD:   Result := icMOD;
    op_NEQ:   Result := icNEQ;
    op_NOT:   Result := icNOT;
    op_POW:   Result := icPOW;
    op_SAR:   Result := icSAR;
    op_SHL:   Result := icSHL;
    op_SHR:   Result := icSHR;
    op_SUB:   Result := icSUB;
    op_XOR:   Result := icXOR;
    op_USUB:  Result := icUSUB;
  else
    Result := icNOOP;
  end;
end;

function GetEvalRes(OP: EOperator; Left, Right: EExpressBaseType): EExpressBaseType;
begin
  if      (OP in ArithOps) then
    Result := CommonArithmeticCast(Left, Right)
  else if (OP in LogicalOps) then
    Result := xtBoolean
  else if (OP = op_USUB) and (Left in XprIntTypes) then
    Result := xtInt
  else if (OP = op_USUB) and (Left in XprFloatTypes) then
    Result := xtFloat
  else if (OP = op_USUB) then
    Result := xtUnknown
  else if (OP = op_ADDR) then
    Result := xtPointer
  else if (OP = op_DEREF) then
    Result := Left
  else
    Result := xtUnknown;

//Result := EvalResArr[OP, Left, Right];
end;

function HashEToken(constref x:ETokenKind): UInt32;
begin
  Result := Ord(x);
end;


procedure InitUnit();
var
  i: Int32;
  myOperator: TOperatorPrecedence;
begin
  (* load Operators: Assoc, and Precedence *)
  PrecedenceMap := TOperatorPrecedenceMap.Create(@HashEToken);
  for i:=0 to High(BinPrecedence) do
  begin
    myOperator.Assoc := BinPrecedence[i].Assoc;
    myOperator.Prec  := BinPrecedence[i].Prec;
    PrecedenceMap.Add(BinPrecedence[i].OP, myOperator);
  end;
  
  UnaryPrecedenceMap := TOperatorPrecedenceMap.Create(@HashEToken);
  for i:=0 to High(UnaryPrecedence) do
  begin
    myOperator.Assoc := UnaryPrecedence[i].Assoc;
    myOperator.Prec  := UnaryPrecedence[i].Prec;
    UnaryPrecedenceMap.Add(UnaryPrecedence[i].OP, myOperator);
  end;

  (* load some arrays *)
  {$I op2instr.inc}
end;

initialization
  InitUnit();

end.
