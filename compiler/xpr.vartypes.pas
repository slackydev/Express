unit xpr.Vartypes;
{
  Author: Jarl K. Holta  
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
{$I header.inc}
{.$hints off}

interface

uses 
  SysUtils, xpr.Types, xpr.Tokenizer, xpr.CompilerContext, xpr.Intermediate, xpr.Dictionary;

type
  XTypeList = specialize TArrayList<XType>;

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
    function Hash(): string; override;
  end;

  XType_Pointer = class(XType_Integer)
    PointsTo: XType; // NEW: Stores the type of the data it points to.
    constructor Create(APointsTo: XType); reintroduce; virtual;
    function Size: SizeInt; override;
    function CanAssign(Other: XType): Boolean; override;
    function Equals(Other: XType): Boolean; override;
    function Hash(): string; override;
  end;

  XType_Array = class(XType_Pointer)
    ItemType: XType;
    constructor Create(AType: XType); reintroduce; virtual;
    function CanAssign(Other: XType): Boolean; override;
    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; override;
    function Equals(Other: XType): Boolean; override;
    function EvalCode(OP: EOperator; Other: XType): EIntermediate; override;
    function Hash(): string; override;
  end;

  XType_String = class(XType_Array)
    constructor Create(AType: XType); reintroduce; virtual;
    function CanAssign(Other: XType): Boolean; override;
    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; override;
    function ToString(): string; override;
    function Hash(): string; override;
  end;

  XType_Method = class(XType_Pointer)
    Params: XTypeArray;
    Passing: TPassArgsBy;
    ReturnType: XType;
    Addr: SizeInt;
    TypeMethod: Boolean;
    IsNested: Boolean;

    constructor Create(AName: string; AParams: XTypeArray; APassBy: TPassArgsBy; ARetType: XType; ATypeMethod: Boolean); reintroduce; virtual;
    function IsClassMethod(): Boolean;
    function GetClassID(): Int32;
    function GetClass(): XType;
    function GetVMTIndex(): Int32;
    function Equals(Other: XType): Boolean; override;
    function Hash(): string; override;
  end;

  XType_Class = class(XType_Pointer)
    Parent: XType_Class;
    ClassID: SizeInt;
    VMT: TVMT;

    FieldNames: XStringList;
    FieldTypes: XTypeList;

    constructor Create(AParent: XType_Class; AFieldNames: XStringList; AFieldTypes: XTypeList); reintroduce; virtual;
    function Size: SizeInt; override;

    function EvalCode(OP: EOperator; Other: XType): EIntermediate; override;
    function Equals(Other: XType): Boolean; override;
    function CanAssign(Other: XType): Boolean; override;

    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; override;
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
  Result := True;
end;

function XType_Ordinal.BaseIntType: EExpressBaseType;
begin
  case BaseType of
    xtInt8..xtInt64: Result := BaseType;
    xtAnsiChar: Result := xtInt8;
    xtWideChar: Result := xtInt16;
    xtBoolean:  Result := xtInt8;
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

function XType_Record.Hash(): string;
var
  i: Int32;
begin
  // A record's hash is its structure: R{field1:type1_hash;field2:type2_hash}
  Result := 'R{';
  for i := 0 to Self.FieldNames.High do
  begin
    Result := Result + Self.FieldNames.Data[i] + ':' + Self.FieldTypes.Data[i].Hash();
    if i < Self.FieldNames.High then Result := Result + ';';
  end;
  Result := Result + '}';
end;


//--------------
constructor XType_Pointer.Create(APointsTo: XType);
begin
  Self.BaseType := xtPointer;
  Self.PointsTo := APointsTo;
end;

function XType_Pointer.Size: SizeInt;
begin
  Result := SizeOf(Pointer); // The pointer itself is always pointer-sized.
end;

function XType_Pointer.CanAssign(Other: XType): Boolean;
begin
  // A pointer can be assigned 'nil' and untyped pointer
  if (Other.BaseType = xtPointer) and ((Other as XType_Pointer).PointsTo = nil) then
    Exit(True);

  // A pointer can be assigned another pointer of the exact same type.
  if (Other is XType_Pointer) and (Self.Equals(Other)) then
    Exit(True);

  // An untyped pointer can be assigned from any other pointer type.
  if (Self.PointsTo = nil) and (Other is XType_Pointer) then
    Exit(True);

  // Integers can be written to pointers
  if (Other is XType_Integer) then
    Exit(True);

  Result := False;
end;

function XType_Pointer.Equals(Other: XType): Boolean;
begin
  if not (Other is XType_Pointer) then Exit(False);
  // Two pointer types are equal if they point to the same type.
  // nil PointsTo means it's a generic, untyped pointer.
  if (Self.PointsTo = nil) or ((Other as XType_Pointer).PointsTo = nil) then
    Result := (Self.PointsTo = (Other as XType_Pointer).PointsTo)
  else
    Result := Self.PointsTo.Equals((Other as XType_Pointer).PointsTo);
end;

function XType_Pointer.Hash(): string;
begin
  if PointsTo <> nil then
    Result := 'P[' + PointsTo.Hash() + ']' // e.g., "P[I32]"
  else
    Result := 'P[*]'; // Untyped pointer
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

function XType_Array.Hash(): string;
begin
  // An array's hash is structural and recursive: A[<item_type_hash>]
  Result := 'A[' + Self.ItemType.Hash() + ']';
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
  Result := (Other is XType_Array) and (XType_Array(Other).ItemType = Self.ItemType);
end;

function XType_String.ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType;
begin
  if (OP = op_Index) and (Other is XType_Ordinal) then
    Exit(Self.ItemType); // common type?
  if (OP = op_Add) and (Other is XType_String) then
    Exit(Self);
  Result := inherited;
end;


function XType_String.ToString(): string;
begin
  Result := 'String';
end;

function XType_String.Hash(): string;
begin
  // A string is a special, non-recursive type. A simple hash is best.
  Result := 'S[' + Self.ItemType.Hash() + ']';
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
end;

function XType_Method.IsClassMethod(): Boolean;
begin
  Result := Self.TypeMethod and (Length(Self.Params) > 0) and (Self.Params[0] is XType_Class);
end;

function XType_Method.GetClassID(): Int32;
begin
  Result := -1;
  if Self.IsClassMethod() then
    Result := XType_Class(Self.Params[0]).ClassID;
end;

function XType_Method.GetClass(): XType;
begin
  Result := nil;
  if Self.IsClassMethod() then
    Result := XType_Class(Self.Params[0]);
end;

function XType_Method.GetVMTIndex(): Int32;
var
  i: Int32;
  list: TVMList;
begin
  Result := -1;
  if Self.IsClassMethod() then
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
  i,selfParam: Int32;
  func: XType_Method;
begin
  if not (Other is XType_Method) then Exit(False);
  Func := XType_Method(other);

  Result := (Other is XType_Method)
        and (Func.IsClassMethod() = Self.IsClassMethod())
        and (Length(Func.Params)  = Length(self.Params))
        and (Func.ReturnType      = Self.ReturnType);

  if not Result then Exit(False);


  selfParam := 0;
  if Self.IsClassMethod() then
  begin
    selfParam := 1;
    if Self.Params[0].BaseType <> Func.Params[0].BaseType then
      Exit(False);
  end;

  for i:=selfParam to High(Self.Params) do
    if (Self.Passing[i] <> Func.Passing[i]) or (not Self.Params[i].Equals(Func.Params[i])) then
      Exit(False);
end;


function XType_Method.Hash(): string;
var
  i,start: Int32;
begin
  // A method's hash uniquely identifies its signature for overload resolution.
  // It specifically EXCLUDES the name.
  start := 0;
  Result := '';
  if Self.IsClassMethod() then
  begin
    Result += '[CM]';
    start  += 1;
  end;

  Result += '(';
  for i:=start to High(Params) do
  begin
    if (i < Length(Passing)) and (Passing[i] = pbRef) then
      Result += 'ref ';

    Result += Params[i].Hash();

    if i < High(Params) then Result += ',';
  end;
  Result := Result + ')';

  // Add return type
  if ReturnType <> nil then
    Result := Result + ':' + ReturnType.Hash();

  // Add flags for other signature properties
  if TypeMethod then Result += '[TM]';
  if IsNested   then Result += '[N]';
end;

//--------------

constructor XType_Class.Create(AParent: XType_Class; AFieldNames: XStringList; AFieldTypes: XTypeList);
begin
  // A class variable is always a pointer type. The actual object is on the heap.
  Self.BaseType   := xtClass;
  Self.Parent     := AParent;
  Self.FieldNames := AFieldNames;
  Self.FieldTypes := AFieldTypes;
  Self.VMT        := TVMT.Create(@HashStr); // Initialize the VMT dictionary
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
    Exit(ctx.GetType(xtBoolean));

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
        // The VMT pointer is always the first field in an object instance.
        Result := Result + ParentSize + SizeOf(Pointer);
        Exit;
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
  Result := Self.Name + '= class';
  if Parent <> nil then
    Result := Result + '(' + Parent.Name + ')';
end;

function XType_Class.GetInstanceSize(): SizeInt;
var i:Int32;
begin
  Result := 0;

  // VMT      @ -24  / -12
  // Refcount @ -16  / -8
  // Size     @ -8   / -4
  Result += 3*SizeOf(SizeInt);

  if Self.Parent <> nil then
    Result += Self.Parent.GetInstanceSize() - 3*SizeOf(SizeInt);

  for i:=0 to FieldTypes.High do
    Result += FieldTypes.data[i].Size();
end;

function XType_Class.Hash(): string;
begin
  Result := 'C[' + IntToStr(Self.ClassID) + ']';
end;

end.
