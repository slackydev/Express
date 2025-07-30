unit xprVartypes;
{
  Author: Jarl K. Holta  
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
{$I header.inc}
{.$hints off}

interface

uses 
  SysUtils, xprTypes, xprTokenizer, xprCompilerContext, xprIntermediate;

type
  XTypeList = specialize TArrayList<XType>;

  XType_Numeric = class(XType)
    function CanAssign(Other: XType): Boolean; override;
  end;

  XType_Ordinal = class(XType_Numeric)
    function BaseIntType: EExpressBaseType;
    function EvalCode(OP: EOperator; Other: XType): EIntermediate; override;
    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; override;
  end;

  XType_Bool = class(XType_Ordinal);

  XType_Char = class(XType_Ordinal);

  XType_Integer = class(XType_Ordinal);
  
  XType_Float = class(XType_Numeric)
    function EvalCode(OP: EOperator; Other: XType): EIntermediate; override;
    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; override;
  end;

  XType_Record = class(XType)
    FieldNames: XStringList;
    FieldTypes: XTypeList;
    constructor Create(AFieldNames: XStringList; AFieldTypes: XTypeList); reintroduce; virtual;
    function Size: SizeInt; override;
    function CanAssign(Other: XType): Boolean; override;
    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; override;
    function FieldType(FieldName: string): XType;
    function FieldOffset(FieldName: string): PtrInt;
    function ToString(): string; override;
  end;

  XType_Pointer = class(XType_Integer);

  XType_Array = class(XType_Pointer)
    ItemType: XType;
    constructor Create(AType: XType); reintroduce; virtual;
    function CanAssign(Other: XType): Boolean; override;
    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; override;
    function Equals(Other: XType): Boolean; override;
    function EvalCode(OP: EOperator; Other: XType): EIntermediate; override;
    function GetArrayDimensions(): Int32;
  end;

  XType_String = class(XType_Array)
    constructor Create(AType: XType); reintroduce; virtual;
    function CanAssign(Other: XType): Boolean; override;
    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; override;
  end;

  XType_Method = class(XType_Pointer)
    Name: string;
    Params: XTypeArray;
    Passing: TPassArgsBy;
    ReturnType: XType;
    Addr: SizeInt;
    TypeMethod: Boolean;

    constructor Create(AName: string; AParams: XTypeArray; APassBy: TPassArgsBy; ARetType: XType; ATypeMethod: Boolean); reintroduce; virtual;
  end;


implementation

uses
  xprLangdef;

function XType_Numeric.CanAssign(Other: XType): Boolean;
begin
  Result := True;
end;

function XType_Ordinal.BaseIntType: EExpressBaseType;
begin
  case BaseType of
    xtInt8..xtInt64: Result := BaseType;
    xtAnsiChar: Result := xtInt8;
    xtWideChar: Result := xtInt16;
    xtBoolean:  Result := xtInt8;
    xtPointer, xtArray, xtString, xtWideString:
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
  if (Result = icNOOP) and (Other is XType_Ordinal) then
    Result := OP2IC(OP);
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

//--------------

constructor XType_Record.Create(AFieldNames: XStringList; AFieldTypes: XTypeList);
begin
  Self.BaseType   := xtRecord;
  Self.FieldNames := AFieldNames;
  Self.FieldTypes := AFieldTypes;
end;

function XType_Record.Size: SizeInt;
var i:Int32;
begin
  Result := 0;
  for i:=0 to FieldTypes.High do
    Result += FieldTypes.data[i].Size();
end;

function XType_Record.CanAssign(Other: XType): Boolean;
var
  i:Int32;
begin
  Result := False;
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
var i:Int32;
begin
  Result := 0;
  FieldName := Xprcase(FieldName);
  for i:=0 to Self.FieldNames.High do
    if Xprcase(Self.FieldNames.Data[i]) = FieldName then
      Exit(Result)
    else
      Result += Self.FieldTypes.data[i].Size();
  Result := -1;
end;


function XType_Record.ToString(): string;
var i: Int32;
begin
  Result := 'Record(';
  for i:=0 to Self.FieldNames.High do
  begin
    Result += Self.FieldNames.Data[i]+': '+ BT2S(Self.FieldTypes.Data[i].BaseType);
    if i <> Self.FieldNames.High then Result += '; ';
  end;
  Result += ')';

end;

//--------------

constructor XType_Array.Create(AType: XType);
begin
  Self.BaseType := xtArray;
  Self.ItemType := AType;
end;

function XType_Array.CanAssign(Other: XType): Boolean;
begin
  Result := (Other is XType_Array) and (XType_Array(Other).ItemType = Self.ItemType);
end;

function XType_Array.ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType;
begin
  if (OP = op_Index) and (Other is XType_Ordinal) then
    Exit(Self.ItemType);
  Result := inherited;
end;

function XType_Array.Equals(Other: XType): Boolean;
begin
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

function XType_Array.GetArrayDimensions(): Int32;
begin
  (*
  Result := 0;
  while True do
    if Self.Ite  *)
end;

//--------------

constructor XType_String.Create(AType: XType);
begin
  if AType.BaseType = xtAnsiChar then
    Self.BaseType := xtAnsiString
  else
    Self.BaseType := xtWideString;
  Self.ItemType := AType;
end;

function XType_String.CanAssign(Other: XType): Boolean;
begin
  Result := (Other is XType_Array) and (XType_Array(Other).ItemType = Self.ItemType);
end;

function XType_String.ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType;
begin
  if (OP = op_Index) and (Other is XType_Ordinal) then
    Exit(Self.ItemType);
  Result := inherited;
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
end;




end.
