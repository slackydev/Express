unit xpr.Types;
{
  Author: Jarl K. Holta  
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
{$I header.inc}
{.$hints off}

interface

uses SysUtils, xpr.Tokenizer, xpr.Dictionary;
  
type
  UInt8  = Byte;      {■}   PUInt8  = ^UInt8;
  Int8   = ShortInt;  {■}   PInt8   = ^Int8;
  UInt16 = Word;      {■}   PUInt16 = ^UInt16;
  Int16  = SmallInt;  {■}   PInt16  = ^Int16;
  UInt32 = LongWord;  {■}   PUInt32 = ^UInt32;
  Int32  = LongInt;   {■}   PInt32  = ^Int32;
  UInt64 = QWord;     {■}   PUInt64 = ^UInt64;
 {Int64  = Int64;}    {■}   PInt64  = ^Int64;

  Float32 = Single;   {■}   PFloat32 = ^Single;
  Float64 = Double;   {■}   PFloat64 = ^Double;

  TBoolArray = array of Boolean;

  EExpressBaseType = ( 
    xtUnknown,
    xtBoolean,
    xtAnsiChar, xtWideChar,
    xtInt8,  xtInt16,  xtInt32,  xtInt64,
    xtUInt8, xtUInt16, xtUInt32, xtUInt64, (* unsigned may be overkill *)
    xtSingle, xtDouble,
    xtAnsiString, xtUnicodeString,
    xtPointer,
    xtRecord,
    xtArray,
    xtMethod, xtExternalMethod,
    xtClass
  );


  EOperator = (
    op_Unknown,

    op_AS, op_IS,

    // special operators
    op_ADDR, op_DEREF, op_If,

    // logical operators
    op_AND, op_EQ, op_GT,  op_GTE, op_IN, op_LT, op_LTE, op_NEQ, op_OR,

    // arithmetic operators
    op_ADD, op_BND, op_BOR, op_DIV, op_INV, op_MUL, op_MOD, op_POW, op_SAR,
    op_SHL, op_SHR, op_SUB, op_XOR,

    // unary operators
    op_NOT, op_USUB, op_INCREF, op_DECREF,

    // assignment operators
    op_Asgn,
    op_AsgnADD, op_AsgnBND, op_AsgnBOR, op_AsgnDIV,  op_AsgnMOD, op_AsgnMUL,
    op_AsgnSUB, op_AsgnXOR, op_AsgnSHL, op_AsgnSHR,

    // symbols
    op_Index, op_Dot, op_Invoke
  );

  // for parameter passing
  EPassBy = (pbRef, pbCopy);
  TPassArgsBy = array of EPassBy;

  ECompilerFlag = (cfNoCollect);
  TCompilerFlags = set of ECompilerFlag;


  TFunctionEntry = record
    CodeLocation, DataLocation: PtrUInt;
    ClassID, VMTIndex: Int32;
  end;
  TFunctionTable = array of TFunctionEntry;

  EMemPos = (
    mpUnknown,
    mpGlobal, // Does not exist at runtime - flag for compiletime
    mpLocal,  // StackPos + Offset
    mpImm,    // Immediate values that comes in the opcode
    mpHeap,   // Does not exist at runtime - flag for compiletime
    mpConst   // Does not exist at runtime - flag for compiletime
  );
  
  // argument passing to external functions
  PParamArray = ^TParamArray;
  TParamArray = array [UInt16] of Pointer;

  TExternalProc = procedure(const Params: PParamArray); cdecl;
  TExternalFunc = procedure(const Params: PParamArray; const Result: Pointer); cdecl;

const
  XprTypeSize : array[EExpressBaseType] of SizeInt = (
    -1,
    SizeOf(Boolean),
    SizeOf(AnsiChar), SizeOf(WideChar),
    SizeOf(Int8),   SizeOf(Int16),  SizeOf(Int32),  SizeOf(Int64),
    SizeOf(UInt8),  SizeOf(UInt16), SizeOf(UInt32), SizeOf(UInt64),
    SizeOf(Single), SizeOf(Double), 
    SizeOf(AnsiString), SizeOf(UnicodeString),
    SizeOf(Pointer),
    -1, (* record is unknown size *)
    SizeOf(Pointer),
    SizeOf(Pointer), SizeOf(Pointer),
    SizeOf(Pointer)
  );
  
  ArithOps    = [op_ADD..op_XOR];
  AssignOps   = [op_ASGN..op_AsgnXOR];
  CompoundOps = [op_AsgnAdd..op_AsgnXOR];
  LogicalOps  = [op_AND..op_OR];
  UnaryOps    = [op_NOT..op_USUB];

  XprBoolTypes    = [xtBoolean];
  XprCharTypes    = [xtAnsiChar..xtWideChar];
  XprIntTypes     = [xtInt8..xtUInt64];
  XprSignedInts   = [xtInt8..xtInt64];
  XprUnsignedInts = [xtUInt8..xtUInt64];
  XprFloatTypes   = [xtSingle..xtDouble];
  XprStringTypes  = [xtAnsiString..xtUnicodeString];

  xtInt = {$If SizeOf(PtrInt) = SizeOf(Int64)}xtInt64{$ELSE}xtInt32{$ENDIF};
  xtFloat = xtDouble;
  xtChar  = xtAnsiChar;
  xtString = xtAnsiString;


  XprPointerTypes    = [xtPointer, xtArray, xtString, xtClass, xtUnicodeString, xtMethod, xtExternalMethod];
  XprSimpleTypes     = XprBoolTypes + XprCharTypes + XprIntTypes + XprFloatTypes;
  XprOrdinalTypes    = XprBoolTypes + XprCharTypes + XprIntTypes;
  XprNumericTypes    = XprSignedInts + XprUnsignedInts + XprFloatTypes;
  XprRefcountedTypes = [xtArray, xtAnsiString, xtUnicodeString{, xtClass}];


const
  ARRAYLIST_MIN = 32;


type
  TIntArray    = array of Int32;
  TStringArray = array of string;

  generic TArrayList<T> = record
  public type
    _TArrayType = array of T;
  public
    FTop: Int32;
    Data: _TArrayType;

    procedure CheckResizeLow();  {$ifdef xinline}inline;{$endif}
    procedure CheckResizeHigh(); {$ifdef xinline}inline;{$endif}

    procedure Init(Items: array of T);
    procedure Free;

    function Size: Int32;
    function High: Int32;

    function Add(Value: T): Int32;
    procedure Delete(Index: Int32);
    function Pop(): T;
    function PopFast(defVar: T): T;
    function Pop(Index: Int32): T;
    procedure Insert(value: T; Position: Int32);
    function Raw(): _TArrayType;
    function RawOfManaged(): _TArrayType;
  end;
  XStringList = specialize TArrayList<string>;
  XIntList    = specialize TArrayList<Int64>;
  TTokenizerList = specialize TArrayList<TTokenizer>;

  TStringToIntDict = specialize TDictionary<string, SizeInt>;
  TVarDeclDictionary = specialize TDictionary<string, XIntList>;
  TIntSet = specialize TDictionary<SizeInt, Boolean>;

  TVirtualMethodTable = class(TObject)
    ParentID: Int32;
    SelfID: Int32;
    {nMethods: Int32}
    Methods: array [0..511] of PtrUInt;

    constructor Create(ASelfID, AParentID: Int32);
  end;

  TVMTList = specialize TArrayList<TVirtualMethodTable>;


var
  TokenToOperatorArr: array [ETokenKind] of EOperator;
  NULL_INT_LIST: XIntList = (FTop: 0; Data: nil);

function Xprcase(s: string): string; {$ifdef xinline}inline;{$endif}
function BaseTypeToStr(typ: EExpressBaseType): string; {$ifdef xinline}inline;{$endif}
function BT2S(typ: EExpressBaseType): string; {$ifdef xinline}inline;{$endif}
function BT2SM(typ: EExpressBaseType): string; {$ifdef xinline}inline;{$endif}
function ExpressInt(Size:Int32; Signed:Boolean = True): EExpressBaseType; {$ifdef xinline}inline;{$endif}
function ExpressFloat(Size: Int32): EExpressBaseType; {$ifdef xinline}inline;{$endif}
function SmallestIntSize(Value: Int64; minTyp:EExpressBaseType = xtInt8): EExpressBaseType; {$ifdef xinline}inline;{$endif}
function BaseIntType(BaseType: EExpressBaseType): EExpressBaseType; {$ifdef xinline}inline;{$endif}
function CommonArithmeticCast(Left, Right:EExpressBaseType): EExpressBaseType;

function AsOperator(typ: ETokenKind): EOperator; {$ifdef xinline}inline;{$endif}
function OperatorToStr(op: EOperator): string;

// Post-Increse and Post-Decrease but with a result (Similar to i++, and i-- in C)
function Asc(var i: Int32): Int32; inline; //i++
function Asc(var i: uInt32): uInt32; inline; //i++
function Asc(var i: Int64): Int64; inline; //i++
function Asc(var i: uInt64): uInt64; inline; //i++

function Desc(var i: Int32): Int32; inline; //i--
function Desc(var i: uInt32): uInt32; inline; //i--
function Desc(var i: Int64): Int64; inline; //i--
function Desc(var i: uInt64): uInt64; inline; //i--

operator + (left: TStringArray; Right: String): TStringArray;

implementation

uses xpr.Errors, Math;

constructor TVirtualMethodTable.Create(ASelfID, AParentID: Int32);
begin
  Self.SelfID   := ASelfID;
  Self.ParentID := AParentID;
  FillByte(Self.Methods[0], Length(Self.Methods)*SizeOf(Pointer), $FF);
end;

function Xprcase(s: string): string;
begin
  Result := Lowercase(s);
end;

function BaseTypeToStr(typ:EExpressBaseType): string;
begin
  WriteStr(Result, typ);
  Result := Copy(Result, 3, Length(Result)-2);
end;

function BT2S(typ:EExpressBaseType): string;
begin
  WriteStr(Result, typ);
  Result := Copy(Result, 3, Length(Result)-2);
end;

function BT2SM(typ: EExpressBaseType): string;
begin
  case typ of
    xtUnknown:  Result := '---';
    xtBoolean:  Result := 'b';
    xtAnsiChar: Result := 'c';
    xtWideChar: Result := 'wc';
    xtInt8:     Result := 'i8';  xtUInt8:  Result := 'u8';
    xtInt16:    Result := 'i16'; xtUInt16: Result := 'u16';
    xtInt32:    Result := 'i32'; xtUInt32: Result := 'u32';
    xtInt64:    Result := 'i64'; xtUInt64: Result := 'u64';
    xtSingle:   Result := 'f32';
    xtDouble:   Result := 'f64';
    xtAnsiString: Result := 'str';
    xtUnicodeString: Result := 'us';
    xtPointer: Result := 'ptr';
    xtRecord:  Result := 'rec';
    xtArray:   Result := 'arr';
    xtMethod:  Result := 'm';
    xtClass :  Result := 'cls';
    xtExternalMethod: Result := 'mx';
  else Result := '';
  end;
end;

//convert a token into an operator. This will error if given an invalid operator
function AsOperator(typ: ETokenKind): EOperator;
begin
  Result := TokenToOperatorArr[typ];
  if (Result = op_Unknown) and (typ <> tkUNKNOWN) then
    RaiseException('Invalid operator encountered: '+ TokenToString(typ));
end;

//used in error messages
function OperatorToStr(op: EOperator): string;
var i:ETokenKind;
begin
  Result := '<unknown>';
  for i:=tkUNKNOWN to High(TokenToOperatorArr) do
    if TokenToOperatorArr[i] = op then
      Exit(TokenString[i]);
end;

function ExpressInt(Size:Int32; Signed:Boolean = True): EExpressBaseType;
var
  i: EExpressBaseType;
begin
  Result := xtUnknown;
  if Signed then
  begin
    for i:=xtInt8 to xtInt64 do
      if XprTypeSize[i] >= Size then Exit(i);
    Result := xtInt64;
  end else
  begin
    for i:=xtUInt8 to xtUInt64 do
      if XprTypeSize[i] >= Size then Exit(i);
    Result := xtUInt64;
  end;
end;

function ExpressFloat(Size: Int32): EExpressBaseType;
var
  i: EExpressBaseType;
begin
  Result := xtUnknown;
  for i:=xtSingle to xtDouble do
    if XprTypeSize[i] >= Size then Exit(i);
end;

function SmallestIntSize(Value: Int64; minTyp:EExpressBaseType = xtInt8): EExpressBaseType;
begin
  Result := xtInt;
  if      ((Value >= Low(Int8 )) and (Value <= High(Int8 ))) and (xtInt8  >= minTyp) then
    Result := xtInt8
  else if ((Value >= Low(Int16)) and (Value <= High(Int16))) and (xtInt16 >= minTyp) then
    Result := xtInt16
  else if ((Value >= Low(Int32)) and (Value <= High(Int32))) and (xtInt32 >= minTyp) then
    Result := xtInt32
  else
    Result := xtInt64;
end;

function BaseIntType(BaseType: EExpressBaseType): EExpressBaseType;
begin
  Result := xtUnknown;
  case BaseType of
    xtInt8..xtInt64:   Result := BaseType;
    xtUInt8..xtUInt64: Result := BaseType;
    xtAnsiChar: Result := xtInt8;
    xtWideChar: Result := xtInt16;
    xtBoolean:  Result := xtInt8;
    xtPointer, xtArray, xtString, xtUnicodeString, xtClass, xtMethod:
      Result := xtInt;
  end;
end;

function CommonArithmeticCast(Left, Right:EExpressBaseType): EExpressBaseType;
const
  UnsignedTypes = XprUnsignedInts + [xtBoolean, xtAnsiChar, xtWideChar];
begin
  Result := xtUnknown;

  // maybe already equal
  if Left = Right then
    Exit(Left);

  if Left in XprOrdinalTypes+XprPointerTypes then
    Left := BaseIntType(Left);

  if Right in XprOrdinalTypes+XprPointerTypes then
    Right := BaseIntType(Right);

  // maybe cast was enough, then we can mix I think..
  if Left = Right then
    Exit(Left);

  if (Left In XprSimpleTypes) and (Right In XprSimpleTypes) then
  begin
    // if they are the same, no conversion is needed.
    if Left = Right then
      Exit(Left);

    // first if any operand is float, result a float equal to the largest operand.
    if (Left in XprFloatTypes) or (Right in XprFloatTypes) then
      Exit(ExpressFloat(Max(XprTypeSize[left], XprTypeSize[right])));

    //-- both types are ordinal

    // if both operands are unsigned - then result is equal the larger operand
    if (Left in UnsignedTypes) and (Right in UnsignedTypes) then
       Exit(ExpressInt(Max(XprTypeSize[left], XprTypeSize[right]), False));

    // if both operands are signed - then result is equal the larger operand
    if (Left in XprSignedInts) and (Right in XprSignedInts) then
       Exit(ExpressInt(Max(XprTypeSize[left], XprTypeSize[right]), True));

    // if the operand that has unsigned int type has size greater or equal to
    // the size of the type of the other operand, then the operand with signed int
    // type is converted to the type of the operand with unsigned int type.
    if ((Left in XprSignedInts) and (Right in UnsignedTypes)) and (XprTypeSize[Left]  <= XprTypeSize[Right]) then
      Exit(Right);
    if ((Right in XprSignedInts) and (Left in UnsignedTypes)) and (XprTypeSize[Right] <= XprTypeSize[Left] ) then
      Exit(Left);

    // if the type of the operand with signed integer type can represent all of
    // the values of the type of the operand with unsigned integer type, then the
    // operand with unsigned integer type is converted to the type of the operand
    // with signed integer type.
    if ((Left in XprSignedInts) and (Right in UnsignedTypes)) and (XprTypeSize[Left]  > XprTypeSize[Right]) then
      Exit(Left);
    if ((Right in XprSignedInts) and (Left in UnsignedTypes)) and (XprTypeSize[Right] > XprTypeSize[Left] ) then
      Exit(Right);

     // both operands are converted to the unsigned integer type corresponding
     // to the type of the operand with signed integer type.
     if (Left in XprSignedInts) then
       Exit(ExpressInt(XprTypeSize[left], False))
     else
       Exit(ExpressInt(XprTypeSize[right], False));
  end
  else if (Left In XprStringTypes) and (Right In XprStringTypes) then
    Result := EExpressBaseType(Max(Ord(Left), Ord(Right)))
  else
    Result := xtUnknown;
end;

// ----------------------------------------------------------------------------
// Increase or decrease the value by 1, returning the current value
function Asc(var i: Int32):  Int32;  begin Result:=i; i+=1; end;
function Asc(var i: UInt32): UInt32; begin Result:=i; i+=1; end;
function Asc(var i: Int64):  Int64;  begin Result:=i; i+=1; end;
function Asc(var i: UInt64): UInt64; begin Result:=i; i+=1; end;

function Desc(var i: Int32):  Int32;  begin Result:=i; i-=1; end;
function Desc(var i: UInt32): UInt32; begin Result:=i; i-=1; end;
function Desc(var i: Int64):  Int64;  begin Result:=i; i-=1; end;
function Desc(var i: UInt64): UInt64; begin Result:=i; i-=1; end;


// ----------------------------------------------------------------------------
// It's often simpler to work with a type like this versus raw arrays, no manual
// resizing or anything. It also avoids allocating very often by overallocating when it does.

procedure TArrayList.Init(Items: array of T);
var i:Int32;
begin
  Self.FTop := -1;
  SetLength(Self.Data, ARRAYLIST_MIN);
  for i:=0 to Length(Items)-1 do
    Self.Add(Items[i]);
end;

procedure TArrayList.Free;
begin
  Self.FTop := -1;
  SetLength(Self.Data, 0);
end;

procedure TArrayList.CheckResizeHigh();
begin
  if (Self.FTop = Length(Self.Data)) then
    SetLength(Self.Data, Length(Self.Data) * 2);
end;

procedure TArrayList.CheckResizeLow();
begin
  if (Self.FTop < ARRAYLIST_MIN) then
  begin
    if (Length(Self.Data) >= ARRAYLIST_MIN) then
      SetLength(Data, ARRAYLIST_MIN);
  end
  else if (Length(Self.Data) div 2) > Self.FTop then
    SetLength(Self.Data, Length(Self.Data) div 2);
end;

function  TArrayList.Size: Int32;
begin
  Result := FTop+1;
end;

function  TArrayList.High: Int32;
begin
  Result := FTop;
end;

function TArrayList.Add(Value: T): Int32;
begin
  Inc(Self.FTop);
  CheckResizeHigh();
  Self.Data[Self.FTop] := Value;
  Result := Self.FTop;
end;

procedure TArrayList.Delete(Index: Int32);
var i, j: Int32;
begin
  if (FTop = -1) then Exit;
  if (Index > FTop) or (Index < 0) then Exit;
  j := 0;
  for i:=Index to FTop do
  begin
    if (i=Index) then
      j := i
    else
    begin
      Data[j] := Data[i];
      Inc(j);
    end;
  end;
  Dec(FTop);
  CheckResizeLow();
end;

// Creating exceptions quickly adds up, if possible by use PopFast instead
// I could probably create the exception on initalization tho..
function TArrayList.Pop(): T;
begin
  if Self.FTop > -1 then
  begin
    Result := Self.Data[Self.FTop];
    Dec(Self.FTop);
    CheckResizeLow();
  end else
    raise OutOfRangeError.Create('Index out of range: '+ IntToStr(Self.FTop));
end;

function TArrayList.PopFast(defVar: T): T;
begin
  if Self.FTop > -1 then
  begin
    Result := Self.Data[Self.FTop];
    Dec(Self.FTop);
    CheckResizeLow();
  end else
    Result := defVar;
end;

function TArrayList.Pop(Index: Int32): T;
begin
  if Index <= Self.FTop then
  begin
    Result := Self.Data[Index];
    Self.Delete(Index);
  end else
    raise OutOfRangeError.Create('Index out of range: '+ IntToStr(Self.FTop));
end;

procedure TArrayList.Insert(value: T; Position: Int32);
var
  i: Int32;
begin
  if (Position < 0) or (Position > FTop + 1) then
    raise OutOfRangeError.Create('Index out of range: ' + IntToStr(Position));

  Inc(Self.FTop);
  CheckResizeHigh();

  for i := FTop downto Position + 1 do
    Data[i] := Data[i - 1];

  Data[Position] := value;
end;

function TArrayList.Raw(): _TArrayType;
begin
  Result := nil;
  if Self.High >= 0 then
  begin
    SetLength(Result, Self.Size);
    Move(Self.Data[0], Result[0], Length(Result)*SizeOf(Result[0]));
  end;
end;

function TArrayList.RawOfManaged(): _TArrayType;
var i: Int32;
begin
  Result := nil;
  SetLength(Result, Self.Size);
  for i:=0 to Self.High do
    Result[i] := Self.Data[i];
end;

// ----------------------------------------------------------------------------
// Some other shit...

procedure DoInit();
begin
  TokenToOperatorArr[tkUNKNOWN] := op_Unknown;

  TokenToOperatorArr[tkKW_AS]  := op_AS;
  TokenToOperatorArr[tkKW_IS]  := op_IS;

  TokenToOperatorArr[tkPLUS]  := op_ADD;
  TokenToOperatorArr[tkAND]   := op_AND;
  TokenToOperatorArr[tkAT]    := op_ADDR;
  TokenToOperatorArr[tkBND]   := op_BND;
  TokenToOperatorArr[tkBOR]   := op_BOR;
  TokenToOperatorArr[tkINV]   := op_INV;
  TokenToOperatorArr[tkDEREF] := op_Deref;
  TokenToOperatorArr[tkDIV]   := op_DIV;
  TokenToOperatorArr[tkEQ]    := op_EQ;
  TokenToOperatorArr[tkGT]    := op_GT;
  TokenToOperatorArr[tkGTE]   := op_GTE;
  TokenToOperatorArr[tkIN]    := op_IN;
  TokenToOperatorArr[tkKW_IF] := op_If;
  TokenToOperatorArr[tkLT]    := op_LT;
  TokenToOperatorArr[tkLTE]   := op_LTE;
  TokenToOperatorArr[tkMUL]   := op_MUL;
  TokenToOperatorArr[tkMOD]   := op_MOD;
  TokenToOperatorArr[tkNE]    := op_NEQ;
  TokenToOperatorArr[tkNOT]   := op_Not;
  TokenToOperatorArr[tkOR]    := op_OR;
  TokenToOperatorArr[tkPOW]   := op_POW;
  TokenToOperatorArr[tkSAR]   := op_SAR;
  TokenToOperatorArr[tkSHL]   := op_SHL;
  TokenToOperatorArr[tkSHR]   := op_SHR;
  TokenToOperatorArr[tkMINUS] := op_SUB;
  TokenToOperatorArr[tkXOR]   := op_XOR;
  TokenToOperatorArr[tkUMINUS]:= op_USUB;

  TokenToOperatorArr[tkASGN]      := op_Asgn;
  TokenToOperatorArr[tkPLUS_ASGN] := op_AsgnADD;
  TokenToOperatorArr[tkBND_ASGN]  := op_AsgnBND;
  TokenToOperatorArr[tkBOR_ASGN]  := op_AsgnBOR;
  TokenToOperatorArr[tkDIV_ASGN]  := op_AsgnDIV;
  TokenToOperatorArr[tkMOD_ASGN]  := op_AsgnMOD;
  TokenToOperatorArr[tkMUL_ASGN]  := op_AsgnMUL;
  TokenToOperatorArr[tkMINUS_ASGN]:= op_AsgnSUB;
  TokenToOperatorArr[tkXOR_ASGN]  := op_AsgnXOR;

  //TokenToOperatorArr[tkASGNMULADD] := op_AsgnMulAdd;
  TokenToOperatorArr[tkINDEX]          := op_Index;
  TokenToOperatorArr[tkDOT]            := op_Dot;
  TokenToOperatorArr[tkLPARENTHESES]   := op_Invoke;
end;

operator + (left: TStringArray; Right: String): TStringArray;
begin
  Result := Left;
  SetLength(Result, Length(Result)+1);
  Result[High(Result)] := Right;
end;


initialization
  DoInit();


end.
