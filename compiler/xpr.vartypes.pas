unit xpr.Vartypes;
{
  Copyright 2026 Jarl K. Holta

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}
{$I header.inc}
{.$hints off}

interface

uses
  SysUtils, xpr.Types, xpr.Tokenizer, xpr.CompilerContext, xpr.Intermediate, xpr.Dictionary;

type
  XTypeList = specialize TArrayList<XType>;

  TFieldInfo = packed record
    IsWritable: Boolean;
    IsPrivate: Boolean;
  end;

  XFieldInfoList = specialize TArrayList<TFieldInfo>;

  // this is compile time helper
  TVMItem = record Index: Int32; MethodDef: XType; end;
  TVMList = specialize TArrayList<TVMItem>; // overload resolution
  TVMT     = specialize TDictionary<string, TVMList>;


  XType_Numeric = class(XType)
    function CanAssign(Other: XType): Boolean; override;
  end;

  XType_Ordinal = class(XType_Numeric)
    function BaseIntType: EExpressBaseType;
    function EvalCode(OP: EOperator; Other: XType): EIntermediate; override;
    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; override;
    function ToString(): string; override;
    function CanAssign(Other: XType): Boolean; override;
  end;

  XType_Bool = class(XType_Ordinal);

  XType_Char = class(XType_Ordinal)
    function CanAssign(Other: XType): Boolean; override;
    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; override;
  end;

  XType_Integer = class(XType_Ordinal);

  XType_Float = class(XType_Numeric)
    function EvalCode(OP: EOperator; Other: XType): EIntermediate; override;
    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; override;
    function ToString(): string; override;
  end;

  XType_Record = class(XType)
    FieldNames: XStringList;
    FieldTypes: XTypeList;
    Aligned: Boolean;

    constructor Create(AFieldNames: XStringList; AFieldTypes: XTypeList); reintroduce; virtual;
    function Size: SizeInt; override;
    procedure Assert(); override;
    function CanAssign(Other: XType): Boolean; override;
    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; override;
    function FieldType(FieldName: string): XType;
    function FieldOffset(FieldName: string): PtrInt;
    function ToString(): string; override;
    function Equals(Other: XType): Boolean; override;
    function Hash(): string; override;
    function Alignment(): SizeInt;
  end;

  XType_Pointer = class(XType_Integer)
    ItemType: XType;
    constructor Create(APointsTo: XType); reintroduce; virtual;
    function Size: SizeInt; override;
    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; override;
    function CanAssign(Other: XType): Boolean; override;
    function Equals(Other: XType): Boolean; override;
    function ToString(): string; override;
    function Hash(): string; override;
  end;

  XType_Array = class(XType_Pointer)
    constructor Create(AType: XType); reintroduce; virtual;
    function CanAssign(Other: XType): Boolean; override;
    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; override;
    function Equals(Other: XType): Boolean; override;
    function EvalCode(OP: EOperator; Other: XType): EIntermediate; override;
    function ToString(): string; override;
    function Hash(): string; override;
  end;

  XType_String = class(XType_Array)
    constructor Create(AType: XType); reintroduce; virtual;
    function CanAssign(Other: XType): Boolean; override;
    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; override;
    function Equals(Other: XType): Boolean; override;
    function EvalCode(OP: EOperator; Other: XType): EIntermediate; override;
    function ToString(): string; override;
    function Hash(): string; override;
  end;

  XType_Method = class(XType_Pointer)
    Params: XTypeArray;
    Passing: TPassArgsBy;
    ParamNames: TStringArray;
    ParamDefaults: array of TXprVar;  // parallel to Params; NullVar = required
    ReturnType: XType;
    Addr: SizeInt;
    TypeMethod, ClassMethod: Boolean;
    IsNested: Boolean;
    NestingLevel: Int32;
    RealParamcount: Int32;
    CallingConvention: string;
    AccessStyle: EMethodType;
    IsAutocast: Boolean;

    constructor Create(AName: string; AParams: XTypeArray; APassBy: TPassArgsBy; ARetType: XType; ATypeMethod: Boolean); reintroduce; virtual;
    function GetClassID(): Int32;
    function GetClass(): XType;
    function GetVMTIndex(): Int32;
    function Equals(Other: XType): Boolean; override;
    function ToString(): string; override;
    function Hash(): string; override;
  end;


  XType_Lambda = class(XType_Record)
    constructor Create(AFieldNames: XStringList; AFieldTypes: XTypeList); reintroduce; virtual;
    function Equals(Other: XType): Boolean; override;
    function CanAssign(Other: XType): Boolean; override;
    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; override;
    function ToString(): string; override;
  end;


  // Enumerated type: ordinal with compile time named members.
  // Access exclusively via EColor.Red - no global namespace pollution.
  XType_Enum = class(XType_Ordinal)
    MemberNames:  TStringArray;
    MemberValues: array of Int64;

    constructor Create(const ANames:  TStringArray;
                       const AValues: array of Int64); reintroduce; virtual;

    function CanAssign(Other: XType): Boolean; override;
    function Equals(Other: XType): Boolean; override;
    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; override;

    function Hash(): string; override;
    function ToString(): string; override;
    function MemberValue(const AName: string; out AValue: Int64): Boolean;
    function MemberName(const AValue: Int64): string;
  end;


  XType_Class = class(XType_Pointer)
    Parent: XType_Class;
    ClassID: SizeInt;
    VMT: TVMT;

    FieldNames: XStringList;
    FieldTypes: XTypeList;
    FieldInfo:  XFieldInfoList;

    constructor Create(AParent: XType_Class; AFieldNames: XStringList; AFieldTypes: XTypeList;AFieldInfo: XFieldInfoList); reintroduce; virtual;
    destructor Destroy; override;
    function Size: SizeInt; override;

    function EvalCode(OP: EOperator; Other: XType): EIntermediate; override;
    function Equals(Other: XType): Boolean; override;
    function CanAssign(Other: XType): Boolean; override;

    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; override;
    function IsWritable(FieldName: string): Boolean;
    function FieldType(FieldName: string): XType;
    function FieldOffset(FieldName: string): PtrInt;
    function ToString(): string; override;
    function GetInstanceSize(): SizeInt;
    function Hash(): string; override;
  end;


implementation

uses
  xpr.Langdef;

function XType_Numeric.CanAssign(Other: XType): Boolean;
begin
  Result := (Other is XType_Numeric) or (Other is XType_Char);
  Result := Result and not(Other is XType_Array) and not(Other is XType_Class);
end;

function XType_Ordinal.BaseIntType: EExpressBaseType;
begin
  case BaseType of
    xtInt8..xtInt64: Result := BaseType;
    xtAnsiChar:      Result := xtInt8;
    xtUnicodeChar:   Result := xtInt16;
    xtBool:          Result := xtInt8;
    xtPointer, xtArray, xtString, xtUnicodeString:
                     Result := xtInt;
    else
                     Result := xtUnknown;
  end;
end;

function XType_Ordinal.EvalCode(OP: EOperator; Other: XType): EIntermediate;
begin
  if (Other = nil) then
  begin
    Result := OP2IC(OP);
    if (Result = icNOOP) then Result := OP2IC(OP);
    Exit;
  end;

  Result := OP2IC(OP);
  //if (Result = icNOOP) and (Other is XType_Ordinal) then
  //  Result := OP2IC(OP);
end;

function XType_Ordinal.ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType;
begin
  if (Other = nil) then
  begin
    Result := ctx.GetType(GetEvalRes(OP, Self.BaseType, xtUnknown));
    if (Result = nil) then Result := ctx.GetType(GetEvalRes(OP, Self.BaseIntType, xtUnknown));
    Exit;
  end;

  Result := ctx.GetType(GetEvalRes(OP, Self.BaseType, Other.BaseType));
  if (Result = nil) and (Other is XType_Ordinal) then
    Result := ctx.GetType(GetEvalRes(OP, Self.BaseIntType, XType_Ordinal(Other).BaseIntType));
end;

function XType_Ordinal.CanAssign(Other: XType): Boolean;
begin
  Result := Other is XType_Ordinal;
  Result := Result and not(Other is XType_Array) and not(Other is XType_Class);
end;

function XType_Ordinal.ToString(): string;
begin
  case Self.BaseType of
    xtBool:        Result := 'Bool';
    xtAnsiChar:    Result := 'Char';
    xtUnicodeChar: Result := 'WideChar';
    xtInt8:        Result := 'Int8';
    xtInt16:       Result := 'Int16';
    xtInt32:       Result := 'Int32';
    xtInt64:       Result := 'Int64';
    xtUInt8:       Result := 'UInt8';
    xtUInt16:      Result := 'UInt16';
    xtUInt32:      Result := 'UInt32';
    xtUInt64:      Result := 'UInt64';
  else
    if Self.Name <> '' then Result := Self.Name
    else                    Result := 'Ordinal';
  end;
end;

//--------------

function XType_Char.ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType;
begin
  if (OP = op_ADD) and (other is XType_String) then
    Result := other
  else
    Result := inherited;
end;

function XType_Char.CanAssign(Other: XType): Boolean;
begin
  Result := inherited;
end;

//--------------

function XType_Float.EvalCode(OP: EOperator; Other: XType): EIntermediate;
begin
  if Other = nil then
  begin
    Result := OP2IC(OP);
    Exit;
  end;

  Result := OP2IC(OP);
end;

function XType_Float.ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType;
begin
  if Other = nil then
    Exit(ctx.GetType(GetEvalRes(OP, Self.BaseType, xtUnknown)));

  Result := ctx.GetType(GetEvalRes(OP, Self.BaseType, Other.BaseType));
  if (Result = nil) and (Other is XType_Ordinal) then
    Result := ctx.GetType(GetEvalRes(OP, Self.BaseType, XType_Ordinal(Other).BaseIntType));
end;

function XType_Float.ToString(): string;
begin
  case Self.BaseType of
    xtSingle: Result := 'Single';
    xtDouble: Result := 'Double';
  else
    if Self.Name <> '' then Result := Self.Name
    else                     Result := 'Float';
  end;
end;

//--------------

constructor XType_Record.Create(AFieldNames: XStringList; AFieldTypes: XTypeList);
begin
  Self.BaseType   := xtRecord;
  Self.FieldNames := AFieldNames;
  Self.FieldTypes := AFieldTypes;
  Self.Aligned    := True;
end;

function XType_Record.Size: SizeInt;
var
  i: Int32;
  FieldAlign, MaxAlign: SizeInt;
begin
  Result := 0;
  MaxAlign := 1;

  for i := 0 to Self.FieldTypes.High do
  begin
    if Self.Aligned then
    begin
      FieldAlign := Self.FieldTypes.Data[i].Alignment();
      if FieldAlign > MaxAlign then MaxAlign := FieldAlign;
      Result := AlignOffset(Result, FieldAlign);
    end;
    Result += Self.FieldTypes.Data[i].Size();
  end;

  if Self.Aligned then
    Result := AlignOffset(Result, MaxAlign);
end;

procedure XType_Record.Assert();
var
  i: Int32;
begin
  for i := 0 to Self.FieldTypes.High do
  begin
    if (Self.FieldTypes.Data[i] = nil) then
      raise Exception.CreateFmt('Unrecognized type in record for field: %s', [Self.FieldNames.Data[i]])
    else if (Self.FieldTypes.Data[i].BaseType = xtUnknown) then
      raise Exception.CreateFmt('Cannot resolve type in record for field: %s: %s', [Self.FieldNames.Data[i], Self.FieldTypes.Data[i].Name]);

    Self.FieldTypes.Data[i].Assert();
  end;
end;

function XType_Record.CanAssign(Other: XType): Boolean;
var i: Int32;
begin
  if (Other.BaseType <> xtRecord) or (XType_Record(Other).FieldTypes.Size <> Self.FieldTypes.Size) then
    Exit(False);

  Result := True;
  for i:=0 to Self.FieldTypes.High do
  begin
    if not FieldTypes.Data[i].CanAssign(XType_Record(Other).FieldTypes.Data[i]) then
      Exit(False);
  end;

  Result := True;
end;

function XType_Record.ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType;
begin
  if OP in [op_ADD..op_XOR] then
    Exit(nil);
  Result := inherited;
end;

function XType_Record.FieldType(FieldName: string): XType;
var i:Int32;
begin
  Result := nil;
  FieldName := Xprcase(FieldName);
  for i:=0 to Self.FieldNames.High do
    if Xprcase(Self.FieldNames.Data[i]) = FieldName then
      Exit(Self.FieldTypes.Data[i]);
end;

function XType_Record.FieldOffset(FieldName: string): PtrInt;
var
  i: Int32;
  FieldAlign: SizeInt;
begin
  Result := 0;
  FieldName := Xprcase(FieldName);

  for i := 0 to Self.FieldNames.High do
  begin
    if Self.Aligned then
    begin
      FieldAlign := Self.FieldTypes.Data[i].Alignment();
      Result := AlignOffset(Result, FieldAlign);
    end;

    if Xprcase(Self.FieldNames.Data[i]) = FieldName then
      Exit(Result);

    Result += Self.FieldTypes.Data[i].Size();
  end;

  Result := -1;
end;


function XType_Record.ToString(): string;
var i: Int32;
begin
  Self.Assert();

  if Self.Name <> '' then
  begin
    Result := Self.Name;
    Exit;
  end;
  Result := 'record(';
  for i:=0 to Self.FieldNames.High do
  begin
    Result += Self.FieldNames.Data[i]+': '+ Self.FieldTypes.Data[i].ToString();
    if i <> Self.FieldNames.High then Result += '; ';
  end;
  Result += ')';
end;

function XType_Record.Equals(Other: XType): Boolean;
var i: Int32;
begin
  Result := (Other.BaseType = xtRecord) and (Other is XType_Record);

  if Result = True then
  begin
    if XType_Record(Other).FieldNames.Size <> XType_Record(Self).FieldNames.Size then
      Exit(False);

    for i:=0 to Self.FieldNames.High do
      if not Self.FieldTypes.Data[i].Equals(XType_Record(Other).FieldTypes.Data[i]) then
        Exit(False);
  end;
end;

function XType_Record.Hash(): string;
var
  i: Int32;
begin
  // A record's hash is its structure: R{field1:type1_hash;field2:type2_hash}
  Result := 'rec{';
  for i := 0 to Self.FieldNames.High do
  begin
    Result := Result + Self.FieldNames.Data[i] + ':' + Self.FieldTypes.Data[i].Hash();
    if i < Self.FieldNames.High then Result := Result + ';';
  end;
  Result := Result + '}';
end;

function XType_Record.Alignment(): SizeInt;
var
  i: Int32;
  FieldAlign: SizeInt;
begin
  if not Self.Aligned then
    Exit(1); // Packed records have a 1-byte alignment

  Result := 1;
  for i := 0 to Self.FieldTypes.High do
  begin
    FieldAlign := Self.FieldTypes.Data[i].Alignment();
    if FieldAlign > Result then
      Result := FieldAlign;
  end;
end;


//--------------
constructor XType_Pointer.Create(APointsTo: XType);
begin
  Self.BaseType := xtPointer;
  Self.ItemType := APointsTo;
end;

function XType_Pointer.Size: SizeInt;
begin
  Result := SizeOf(Pointer); // The pointer itself is always pointer-sized.
end;

function XType_Pointer.ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType;
begin
  System.Assert(Self  <> nil);

  // unary operators:
  if Other = nil then
    Exit(nil);

  // Add and sub
  if(OP in [op_ADD, op_SUB]) and (Other.BaseType in XprOrdinalTypes+[xtPointer]) then
    Exit(Self);

  // =, !=, <, .., >=
  if OP in ComparisonOps then
    Exit(Inherited);

  Result := nil;
end;

function XType_Pointer.CanAssign(Other: XType): Boolean;
begin
  System.Assert(Other <> nil, 'XType_Pointer.CanAssign: Other = nil, How can this be!?');

  // A pointer can be assigned 'nil' and untyped pointer
  if (Other is XType_Pointer) and ((Other as XType_Pointer).ItemType = nil) then
    Exit(True);

  // A pointer can be assigned another pointer of the exact same type.
  if (Other is XType_Pointer) and (Self.Equals(Other)) then
    Exit(True);

  // An untyped pointer can be assigned from any other pointer type.
  if (Self.ItemType = nil) and (Other is XType_Pointer) then
    Exit(True);

  // Integers can be written to pointers if it's ptr sized.
  if (Other is XType_Integer) and (Other.BaseType = xtInt) then
    Exit(True);

  Result := False;
end;

function XType_Pointer.Equals(Other: XType): Boolean;
begin
  if not (Other is XType_Pointer) then Exit(False);
  // Two pointer types are equal if they point to the same type.
  // nil PointsTo means it's a generic, untyped pointer.
  if (Self.ItemType = nil) or ((Other as XType_Pointer).ItemType = nil) then
    Result := (Self.ItemType = (Other as XType_Pointer).ItemType)
  else
    Result := Self.ItemType.Equals((Other as XType_Pointer).ItemType);
end;

function XType_Pointer.Hash(): string;
begin
  if ItemType <> nil then
    Result := 'ptr{' + ItemType.Hash() + '}' // e.g., "P[I32]"
  else
    Result := 'ptr'; // Untyped pointer
end;

function XType_Pointer.ToString(): string;
begin
  if Self.Name <> '' then
    Result := Self.Name
  else if ItemType <> nil then
    Result := '^' + ItemType.ToString()
  else
    Result := 'Pointer';
end;

//--------------

constructor XType_Array.Create(AType: XType);
begin
  Self.BaseType := xtArray;
  Self.ItemType := AType;
end;

function XType_Array.CanAssign(Other: XType): Boolean;
begin
  Result := ((Other is XType_Array) and (XType_Array(Other).ItemType.Equals(Self.ItemType)))
         or ((Other is XType_Pointer) and not (Other is XType_Array) and (XType_Pointer(Other).ItemType = nil));

  Result := Result and not(Other is XType_String);
end;

function XType_Array.ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType;
begin
  if (OP = op_Index) and (Other is XType_Ordinal) then
    Exit(Self.ItemType); // common type?

  // we should compare to basetype - always, this allows tricks.
  // ... XXX
  if ((OP = op_EQ) or (OP = op_NEQ)) and (Other is XType_Array) then
    Exit(ctx.GetType(xtBool));
  if ((OP = op_EQ) or (OP = op_NEQ)) and (Other.BaseType = xtPointer) then
    Exit(ctx.GetType(xtBool));

  // XXX Find the real cause of this:
  if ((OP = op_EQ) or (OP = op_NEQ)) and ((Other is XType_Integer) and (Other.BaseType = xtInt)) then
    Exit(ctx.GetType(xtBool));

  Result := nil; // strict type!
  //Result := inherited;
end;

function XType_Array.Equals(Other: XType): Boolean;
begin
  if (Other is XType_String) then Exit(False); //XXX
  Result := (Other is XType_Array) and (XType_Array(other).ItemType.Equals(XType_Array(self).ItemType));
end;

function XType_Array.EvalCode(OP: EOperator; Other: XType): EIntermediate;
begin
  if OP = op_Asgn then
  begin
    Result := icNOOP;

    if (Other is XType_Array) and (XType_Array(Other).ItemType.Size = Self.ItemType.Size) then
      Result := icMOV;

    if (Other is XType_Pointer) and not(Other is XType_Array) then
      Result := icMOV;

    Exit(Result);
  end;

  Result := inherited;
end;

function XType_Array.ToString(): string;
begin
  if Self.Name <> '' then
    Result := Self.Name
  else if ItemType <> nil then
    Result := 'array of ' + ItemType.ToString()
  else
    Result := 'array';
end;


function XType_Array.Hash(): string;
begin
  // An array's hash is structural and recursive: A[<item_type_hash>]
  Result := 'arr{' + Self.ItemType.Hash() + '}';
end;

//--------------

constructor XType_String.Create(AType: XType);
begin
  if AType.BaseType = xtAnsiChar then
    Self.BaseType := xtAnsiString
  else
    Self.BaseType := xtUnicodeString;
  Self.ItemType := AType;
end;

function XType_String.CanAssign(Other: XType): Boolean;
begin
  // we cannot safely cast to and from array!
  if (Other.BaseType = xtArray) then
    Exit(False);

  Result := (Other is XType_String) and (XType_String(Other).ItemType = Self.ItemType);
  Result := Result or ((Other is XType_Pointer));
  Result := Result or (Other is XType_Char);   // we can assign char to string through dynamic cast
end;

function XType_String.ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType;
begin
  if (OP = op_Index) and (Other is XType_Ordinal) then
    Exit(Self.ItemType); // common type?

  // we should compare to basetype - always, this allows tricks.
  // ... XXX
  if (OP = op_Add) and ((Other is XType_String) or (Other is XType_Char)) then
    Exit(Self);
  if ((OP = op_EQ) or (OP = op_NEQ)) and ((Other is XType_String) or (Other is XType_Char)) then
    Exit(ctx.GetType(xtBool));
  if ((OP = op_EQ) or (OP = op_NEQ)) and ((Other.BaseType = xtPointer)) then
    Exit(ctx.GetType(xtBool));

  // XXX Find the real cause of this:
  if ((OP = op_EQ) or (OP = op_NEQ)) and ((Other is XType_Integer) and (Other.BaseType = xtInt)) then
    Exit(ctx.GetType(xtBool));

  Result := nil; // strict type!
  //Result := inherited;
end;

function XType_String.Equals(Other: XType): Boolean;
begin
  Result := (Other is XType_String) and (XType_String(other).ItemType.Equals(XType_String(self).ItemType));
end;

function XType_String.EvalCode(OP: EOperator; Other: XType): EIntermediate;
begin
  if (OP = op_Asgn) then
  begin
    Result := icNOOP;
    if Self.CanAssign(Other) then
      Result := icMOV;
  end else
    Result := inherited;
end;

function XType_String.ToString(): string;
begin
  Result := 'String';
end;

function XType_String.Hash(): string;
begin
  // A string is a special, non-recursive type. A simple hash is best.
  Result := 'str{' + Self.ItemType.Hash() + '}';
end;


//--------------

constructor XType_Method.Create(AName: string; AParams: XTypeArray; APassBy: TPassArgsBy; ARetType: XType; ATypeMethod: Boolean);
begin
  Self.BaseType   := xtMethod;

  Self.Name       := AName;
  Self.Params     := AParams;
  Self.Passing    := APassBy;
  Self.ReturnType := ARetType;
  Self.TypeMethod := ATypeMethod;
  Self.IsNested   := False;
  Self.NestingLevel := 0;
  Self.AccessStyle := mtMethod;

  Self.RealParamcount := Length(Self.Params);
end;

function XType_Method.GetClassID(): Int32;
begin
  Result := -1;
  if Self.ClassMethod then
    Result := XType_Class(Self.Params[0]).ClassID;
end;

function XType_Method.GetClass(): XType;
begin
  Result := nil;
  if Self.ClassMethod then
    Result := XType_Class(Self.Params[0]);
end;

function XType_Method.GetVMTIndex(): Int32;
var
  i: Int32;
  list: TVMList;
begin
  Result := -1;
  if Self.ClassMethod then
  begin
    list := XType_Class(Self.Params[0]).VMT[XprCase(Self.Name)];

    for i:=0 to list.High do
    begin
      if Self.Equals(list.data[i].MethodDef) then
        Exit(list.data[i].Index);
    end;
  end;
end;


function XType_Method.Equals(Other: XType): Boolean;
var
  i, selfParam, n: Int32;
  func: XType_Method;
begin
  if not (Other is XType_Method) then Exit(False);
  Func := XType_Method(Other);

  Result := (Func.ClassMethod    = Self.ClassMethod)
        and (Func.RealParamcount = Self.RealParamcount)
        and (Func.ReturnType     = Self.ReturnType);
  if not Result then Exit(False);

  selfParam := 0;
  if Self.ClassMethod then
  begin
    selfParam := 1;
    if Self.Params[0].BaseType <> Func.Params[0].BaseType then
      Exit(False);
  end;

  // Guard: Passing arrays may be shorter than Params if implied args
  // were added to Params after Passing was set.
  n := High(Self.Params);
  if (n >= Length(Self.Passing)) or (n >= Length(Func.Passing)) then
    Exit(False);

  for i := selfParam to n do
    if (Self.Passing[i] <> Func.Passing[i]) or
       (not Self.Params[i].Equals(Func.Params[i])) then
      Exit(False);
end;


function XType_Method.Hash(): string;
var
  i,start: Int32;
begin
  start := 0;
  Result := 'func{';
  if Self.TypeMethod then
  begin
    Result += 'self='+ Params[0].Hash();
    start  += 1;
  end;

  if High(Params) >= start then
  begin
    Result += '(';
    for i:=start to High(Params) do
    begin
      if (i < Length(Passing)) and (Passing[i] = pbRef) then
        Result += 'ref ';
      Result += Params[i].Hash();
      if i < High(Params) then Result += ',';
    end;
    Result += ')';
  end;

  if ReturnType <> nil then
    Result := Result + ':' + ReturnType.Hash();

  if Self.ClassMethod then Result += '@CM'
  else if TypeMethod  then Result += '@TM';
  if IsNested         then Result += '@N';
  result += '}';
end;

function XType_Method.ToString(): string;
var
  i, start: Int32;
begin
  start := 0;
  if Self.Name <> '' then
    Result := Self.Name + ': '
  else
    Result := '';

  Result += 'func(';
  if Self.TypeMethod and (Length(Params) > 0) then
    start := 1;  // skip implicit self
  for i := start to High(Params) do
  begin
    if (i < Length(Passing)) and (Passing[i] = pbRef) then
      Result += 'ref ';
    Result += Params[i].ToString();
    if i < High(Params) then Result += ', ';
  end;
  Result += ')';
  if ReturnType <> nil then
    Result += ': ' + ReturnType.ToString();

  if Self.TypeMethod then Result += ' of ' + Params[0].ToString();
end;




constructor XType_Lambda.Create(AFieldNames: XStringList; AFieldTypes: XTypeList);
begin
  Self.BaseType   := xtRecord;
  Self.FieldNames := AFieldNames;
  Self.FieldTypes := AFieldTypes;
  Self.Aligned    := False;
end;

function XType_Lambda.ToString(): string;
var
  inner: XType_Method;
begin
  inner := Self.FieldType('method') as XType_Method;
  if inner <> nil then
    Result := inner.ToString()
  else
    Result := 'func(?)';
end;


function XType_Lambda.Equals(Other: XType): Boolean;
var
  i,selfParam,n: Int32;
  func, SelfMethod: XType_Method;
begin
  if not ((Other is XType_Method) or
          (Other is XType_Lambda) or
          (Other is XType_Record)) then Exit(False);

  if(Other is XType_Record) then
  begin
    Result := inherited;
    Exit;
  end;

  if Other is XType_Lambda then
    Func := XType_Lambda(Other).FieldTypes.Data[0] as XType_Method
  else
    Func := XType_Method(other);

  SelfMethod := Self.FieldTypes.Data[0] as XType_Method;

  Result := (Func.ClassMethod     = False)
        and (Func.RealParamcount  = SelfMethod.RealParamcount)
        and (Func.ReturnType      = SelfMethod.ReturnType);

  if not Result then Exit(False);

  for i:=0 to SelfMethod.RealParamcount-1 do
    if (SelfMethod.Passing[i] <> Func.Passing[i]) or (not SelfMethod.Params[i].Equals(Func.Params[i])) then
      Exit(False);
end;

function XType_Lambda.CanAssign(Other: XType): Boolean;
begin
  Result := inherited;
  // Also accept a plain func — it will be auto-wrapped at codegen time
  if not Result then
    Result := (Other is XType_Method) and not (Other is XType_Lambda);
end;

function XType_Lambda.ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType;
begin
  // Nil-check on a lambda: transparently forward to the method pointer field.
  if (OP in [op_EQ, op_NEQ]) and (Other <> nil) and
     (Other is XType_Pointer) and (XType_Pointer(Other).ItemType = nil) then
    Result := FieldTypes.Data[0].ResType(OP, Other, ctx)
  else
    Result := inherited;
end;

//--------------

constructor XType_Class.Create(AParent: XType_Class; AFieldNames: XStringList; AFieldTypes: XTypeList; AFieldInfo: XFieldInfoList);
begin
  // A class variable is always a pointer type. The actual object is on the heap.
  Self.BaseType   := xtClass;
  Self.Parent     := AParent;
  Self.FieldNames := AFieldNames;
  Self.FieldTypes := AFieldTypes;
  Self.FieldInfo  := AFieldInfo;
  Self.VMT        := TVMT.Create(@HashStr); // Initialize the VMT dictionary
end;

destructor XType_Class.Destroy();
begin
  Self.VMT.Free();
  inherited;
end;

(*
  The size of a class VARIABLE on the stack or in a record is always the
  size of a pointer. The size of the OBJECT on the heap is different and
  is handled by the memory allocator.
*)
function XType_Class.Size: SizeInt;
begin
  Result := SizeOf(Pointer);
end;

(*
  Determines if an object of another type can be assigned to a variable of this class type.
  This is the core of polymorphism.
*)
function XType_Class.EvalCode(OP: EOperator; Other: XType): EIntermediate;
begin
  if OP = op_Asgn then
  begin
    Result := icNOOP;

    if (Other is XType_Class) and (XType_Class(Self).CanAssign(Other)) then
      Result := icMOV;

    if (Other is XType_Pointer) then
      Result := icMOV;

    Exit(Result);
  end;

  Result := inherited;
end;

function XType_Class.Equals(Other: XType): Boolean;
begin
  Result := (Other is XType_Class) and (XType_Class(Other).ClassID = Self.ClassID);
end;

(*
  Determines if an object of another type can be assigned to a variable of this class type.
  This is the core of polymorphism.
*)
function XType_Class.CanAssign(Other: XType): Boolean;
var
  OtherClass: XType_Class;
begin
  // A class variable can be assigned 'nil' (a pointer).
  if (Other.BaseType = xtPointer) then
    Exit(True);

  // It can only be assigned from other class types.
  if not (Other is XType_Class) then
    Exit(False);

  OtherClass := Other as XType_Class;
  // Walk up the inheritance chain of the 'Other' class. If we find 'Self'
  // anywhere in its ancestry, the assignment is valid.
  // Example: Can a TShape variable hold a TCircle?
  //          (Self = TShape, OtherClass = TCircle)
  //          Is TCircle the same as TShape? No.
  //          Is TCircle's parent (TShape) the same as TShape? Yes. -> Valid.
  while OtherClass <> nil do
  begin
    if OtherClass.Equals(Self) then
      Exit(True);
    OtherClass := OtherClass.Parent;
  end;

  Result := False;
end;


function XType_Class.ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType;
begin
  if op = op_IS then
    Exit(ctx.GetType(xtBool));

  // For now, classes don't support binary operators.
  // This could be extended later for operator overloading.
  Result := inherited;
end;

(*
  Finds the type of a field by its name.
  This must search the current class and then all parent classes recursively.
*)
function XType_Class.FieldType(FieldName: string): XType;
var
  i: Int32;
  CurrentClass: XType_Class;
begin
  Result := nil;
  FieldName := XprCase(FieldName);
  CurrentClass := Self;

  // Walk up the inheritance chain from self to the base class.
  while CurrentClass <> nil do
  begin
    // Search for the field in the current class's local fields.
    for i:=0 to CurrentClass.FieldNames.High do
      if XprCase(CurrentClass.FieldNames.Data[i]) = FieldName then
        Exit(CurrentClass.FieldTypes.Data[i]);

    // If not found, move to the parent class.
    CurrentClass := CurrentClass.Parent;
  end;
end;

function XType_Class.IsWritable(FieldName: string): Boolean;
var
  i: Int32;
  CurrentClass: XType_Class;
begin
  Result := True;
  FieldName := XprCase(FieldName);
  CurrentClass := Self;

  // Walk up the inheritance chain from self to the base class.
  while CurrentClass <> nil do
  begin
    // Search for the field in the current class's local fields.
    for i:=0 to CurrentClass.FieldNames.High do
      if XprCase(CurrentClass.FieldNames.Data[i]) = FieldName then
        Exit(CurrentClass.FieldInfo.Data[i].IsWritable);

    // If not found, move to the parent class.
    CurrentClass := CurrentClass.Parent;
  end;
end;

(*
  Calculates the memory offset of a field within a class instance.
  The offset is the size of all parent classes plus the local offset.
*)
function XType_Class.FieldOffset(FieldName: string): PtrInt;
var
  i: Int32;
  CurrentClass: XType_Class;
  ParentSize: SizeInt;

  // Helper function to calculate the total size of all fields in a parent hierarchy.
  function GetTotalParentSize(AClass: XType_Class): SizeInt;
  var i: Int32;
  begin
    Result := 0;
    if AClass.Parent <> nil then
    begin
      // Recursively get the size of the grandparent...
      Result := GetTotalParentSize(AClass.Parent);
      // ...and add the size of the parent's local fields.
      for i := 0 to AClass.Parent.FieldTypes.High do
        Result += AClass.Parent.FieldTypes.data[i].Size();
    end;
  end;

begin
  Result := -1; // Return -1 if not found
  FieldName := XprCase(FieldName);
  CurrentClass := Self;

  // Walk up the inheritance chain to find which class defines the field.
  while CurrentClass <> nil do
  begin
    // Search the local fields of this class.
    Result := 0;
    for i := 0 to CurrentClass.FieldNames.High do
    begin
      if XprCase(CurrentClass.FieldNames.Data[i]) = FieldName then
      begin
        // We found the field. Its final offset is the size of all parent classes
        // plus its local offset within this class definition.
        ParentSize := GetTotalParentSize(CurrentClass);
        Exit(Result + ParentSize);
      end;
      Result += CurrentClass.FieldTypes.data[i].Size();
    end;
    // If not found, move to the parent.
    CurrentClass := CurrentClass.Parent;
  end;

  Result := -1; // Field was not found in the entire hierarchy.
end;

function XType_Class.ToString(): string;
begin
  if Self.Name <> '' then
    Result := Self.Name
  else if Parent <> nil then
    Result := 'class(' + Parent.Name + ')'
  else
    Result := 'class';
end;

// VMT      @ -24  / -12
// Refcount @ -16  / -8
// Size     @ -8   / -4
function XType_Class.GetInstanceSize(): SizeInt;
var i: Int32; c: XType_Class;
begin
  Result := 3 * SizeOf(SizeInt); // header: VMT + refcount + size
  c := Self;
  while c <> nil do
  begin
    for i := 0 to c.FieldTypes.High do
      Result += c.FieldTypes.Data[i].Size();
    c := c.Parent;
  end;
end;

function XType_Class.Hash(): string;
begin
  Result := 'class{' + IntToStr(Self.ClassID) + '}';
end;


{ XType_Enum }

constructor XType_Enum.Create(const ANames: TStringArray;
  const AValues: array of Int64);
var i: Int32; MinV, MaxV: Int64;
begin
  inherited Create;

  SetLength(MemberNames,  Length(ANames));
  SetLength(MemberValues, Length(AValues));
  for i := 0 to High(ANames) do
  begin
    MemberNames[i]  := ANames[i];
    MemberValues[i] := AValues[i];
  end;

  // Choose the smallest integer type that covers the full declared value range.
  MinV := 0; MaxV := 0;
  for i := 0 to High(MemberValues) do
  begin
    if MemberValues[i] < MinV then MinV := MemberValues[i];
    if MemberValues[i] > MaxV then MaxV := MemberValues[i];
  end;

  if      (MinV >= 0)      and (MaxV <= $FF)   then Self.BaseType := xtUInt8
  else if (MinV >= -128)   and (MaxV <= 127)   then Self.BaseType := xtInt8
  else if (MinV >= 0)      and (MaxV <= $FFFF) then Self.BaseType := xtUInt16
  else if (MinV >= -32768) and (MaxV <= 32767) then Self.BaseType := xtInt16
  else                                              Self.BaseType := xtInt32;
end;

// Only same-enum-type assignment is implicit.
function XType_Enum.CanAssign(Other: XType): Boolean;
begin
  Result := (Other = Self)//Other is XType_Ordinal;
end;

// Type identity only - two distinct enum declarations are never equal,
// even if they share the same member names and values.
function XType_Enum.Equals(Other: XType): Boolean;
begin
  Result := (Other = Self);
end;

function XType_Enum.ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType;
begin
  if(OP in ArithOps) and ((Other = Self) or (Other is XType_Integer)) then
    Result := Self
  else if ((Other = Self) or (Other is XType_Integer)) and (OP in LogicalOps) then
    Result := ctx.GetType(xtBool)
  else if (Other = Self) then
    Result := inherited
  else
    Result := nil;
end;

function XType_Enum.Hash(): string;
begin
  Result := 'enum:' + Self.Name;
end;

function XType_Enum.ToString(): string;
begin
  if Self.Name <> '' then Result := Self.Name
  else                    Result := 'enum';
end;

function XType_Enum.MemberValue(const AName: string; out AValue: Int64): Boolean;
var i: Int32;
begin
  for i := 0 to High(MemberNames) do
    if SameText(MemberNames[i], AName) then
    begin
      AValue := MemberValues[i];
      Exit(True);
    end;
  Result := False;
end;

function XType_Enum.MemberName(const AValue: Int64): string;
var i: Int32;
begin
  for i := 0 to High(MemberNames) do
    if (MemberValues[i] = AValue) then
      Exit(MemberNames[i]);

  Result := 'Invalid('+IntToStr(AValue)+')';
end;

end.
