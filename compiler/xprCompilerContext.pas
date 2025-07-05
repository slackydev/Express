unit xprCompilerContext;
{
  Author: Jarl K. Holta  
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
{$I header.inc}
{.$hints OFF}
interface

uses 
  SysUtils, xprDictionary, xprTypes, xprTokenizer, xprIntermediate;

const
  STACK_ITEM_ALIGN = 8; //each element in stack takes up 8 bytes no matter - no option.

type
  EInstructionArg = (ia1, ia2, ia3, ia4, ia5, ia6);
  
  TCompilerContext = class;
  XType = class(TObject)
    BaseType: EExpressBaseType;

    constructor Create(ABaseType: EExpressBaseType=xtUnknown);
    function Size: SizeInt; virtual;
    function EvalCode(OP: EOperator; Other: XType): EIntermediate; virtual;
    function CanAssign(Other: XType): Boolean; virtual;
    function ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType; virtual;
    function Equals(Other: XType): Boolean;  virtual; reintroduce;
  end;
  XTypeArray = array of XType;

  TXprVar = record
    FType: XType;
    FAddr: PtrInt;
    FMemPos: EMemPos;
    FReference: Boolean;

    constructor Create(AType: XType; AAddr: PtrInt=0; AMemPos: EMemPos=mpLocal);
    function DerefToTemp(ctx: TCompilerContext): TXprVar;
    function Deref(ctx: TCompilerContext; Dest: TXprVar): TXprVar;
  end;

  TXprVarList = specialize TArrayList<TXprVar>;
  TStringToObject = specialize TDictionary<string, XType>;

  TCompilerContext = class(TObject)
  public
    Intermediate: TIntermediateCode;

    Variables: TXprVarList; 
    Constants: TXprVarList;

    Scope: SizeInt;

    GlobalVarCount: Int32;

    // all these relate to current scope
    VarDecl:  array of TVarDeclDictionary;
    TypeDecl: array of TStringToObject;
    StackPosArr: array of SizeInt;
  
 {methods}  
    constructor Create();
    
    // stack
    function FrameSize(): SizeInt;
    procedure IncScope();                {$ifdef xinline}inline;{$endif}
    procedure DecScope();                {$ifdef xinline}inline;{$endif}
    function  GetStackPos(): SizeInt;    {$ifdef xinline}inline;{$endif}
    procedure IncStackPos(Size:Int32=STACK_ITEM_ALIGN); {$ifdef xinline}inline;{$endif}
    procedure DecStackPos(Size:Int32=STACK_ITEM_ALIGN); {$ifdef xinline}inline;{$endif}
    
    // ir code generation
    function CodeSize(): SizeInt;     {$ifdef xinline}inline;{$endif}
    
    function  Emit(Opcode: TInstruction; Pos: TDocPos): PtrInt; {$ifdef xinline}inline;{$endif}
    procedure PatchArg(Pos: SizeInt; ArgID:EInstructionArg; NewArg: PtrInt);
    procedure PatchJump(Addr: PtrInt; NewAddr: PtrInt=0);
    function  RelAddr(Addr: PtrInt): TXprVar;
    
    // ------------------------------------------------------ 
    function GetVar(Name: string; Pos:TDocPos): TXprVar;
    function GetVarList(Name: string; Pos:TDocPos): TXprVarList;
    function GetTempVar(Typ: XType): TXprVar;
    
    // ------------------------------------------------------ 
    function GetType(Name: string; Pos: TDocPos): XType;
    function GetType(BaseType: EExpressBaseType; Pos: TDocPos): XType;
    function GetType(Name: string): XType;
    function GetType(BaseType: EExpressBaseType): XType;
    
    // ------------------------------------------------------
    procedure AddType(Name: string; Typ:XType);

    function RegConst(Value: TXprVar): Int32; overload;
    function RegConst(constref Value: TConstant): TXprVar;
    function RegConst(Value: Boolean):  TXprVar; overload;
    function RegConst(Value: Int64):  TXprVar; overload;
    function RegConst(Value: Double): TXprVar; overload;

    function RegVar(Name: string; var Value: TXprVar; DocPos: TDocPos): Int32; overload;
    function RegVar(Name: string; VarType: XType; DocPos: TDocPos; out Index: Int32): TXprVar; overload;
    function RegVar(Name: string; VarType: XType; DocPos: TDocPos): TXprVar; overload;

    function AddVar(constref Value; Name: string; BaseType: EExpressBaseType; DocPos: TDocPos): TXprVar; overload;
    function AddVar(Value: Boolean; Name: string; DocPos: TDocPos): TXprVar; overload;
    function AddVar(Value: Int64;   Name: string; DocPos: TDocPos): TXprVar; overload;
    function AddVar(Value: Double;  Name: string; DocPos: TDocPos): TXprVar; overload;

    function AddExternalFunc(Addr: TExternalProc; Name: string; Params: array of XType; PassBy: array of EPassBy; ResType: XType): TXprVar;
    
    // ------------------------------------------------------
    procedure RegisterInternals;
    
    // ------------------------------------------------------
    property StackPos: SizeInt read GetStackPos;
  end;


const
  NullVar:    TXprVar = (FType:nil; FAddr:0; FMemPos:mpImm; FReference:False);
  NullResVar: TXprVar = (FType:nil; FAddr:0; FMemPos:mpImm; FReference:False);

  GLOBAL_SCOPE = 0;

function GetInstr(OP: EIntermediate; args: array of TXprVar): TInstruction; {$ifdef xinline}inline;{$endif}
function GetInstr(OP: EIntermediate): TInstruction; {$ifdef xinline}inline;{$endif}
function STORE_FAST(Left, Right: TXprVar; Heap: Boolean): TInstruction; {$ifdef xinline}inline;{$endif}

function Immediate(v: PtrInt; Typ: XType = nil): TXprVar; {$ifdef xinline}inline;{$endif}
function OpAddr(v: PtrInt; loc:EMemPos=mpHeap): TXprVar; {$ifdef xinline}inline;{$endif}

operator =  (L,R: TXprVar): Boolean;
operator <> (L,R: TXprVar): Boolean;

implementation

uses
  Math, xprErrors, xprVartypes, xprLangdef;


(*~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~*)

constructor TCompilerContext.Create();
begin
  Intermediate.Init();
  Constants.Init([]);
  Variables.Init([]);
  Scope := -1;
  IncScope();
  Self.RegisterInternals;
end;


// ----------------------------------------------------------------------------
//
function TCompilerContext.FrameSize(): SizeInt;
begin
  result := StackPosArr[Scope];
end;

procedure TCompilerContext.IncScope();
begin
  Inc(Scope);
  SetLength(StackPosArr, Scope+1);
  SetLength(VarDecl,     Scope+1);
  SetLength(TypeDecl,    Scope+1);

  if Scope = GLOBAL_SCOPE then
  begin
    VarDecl[Scope]  := TVarDeclDictionary.Create(@HashStr);
    TypeDecl[Scope] := TStringToObject.Create(@HashStr);
  end else
  begin
    VarDecl[Scope]  := VarDecl[Scope-1].Copy();
    TypeDecl[Scope] := TypeDecl[Scope-1].Copy();
  end;
end;

procedure TCompilerContext.DecScope();
begin
  if Scope = GLOBAL_SCOPE then
    Exit;

  VarDecl[scope].Free();
  TypeDecl[scope].Free();

  SetLength(StackPosArr, Scope);
  SetLength(VarDecl,  Scope);
  SetLength(TypeDecl, Scope);
  Dec(Scope);
end;

function TCompilerContext.GetStackPos(): SizeInt;
begin
  Result := StackPosArr[Scope];
end;

procedure TCompilerContext.IncStackPos(Size:Int32=STACK_ITEM_ALIGN);
begin
  if (Size mod STACK_ITEM_ALIGN <> 0) or (Size = 0) then
    Size := STACK_ITEM_ALIGN * (Size div STACK_ITEM_ALIGN + 1);
  Inc(StackPosArr[Scope], Size);
end;

procedure TCompilerContext.DecStackPos(Size:Int32=STACK_ITEM_ALIGN);
begin
  if (Size mod STACK_ITEM_ALIGN <> 0) or (Size = 0) then
    Size := STACK_ITEM_ALIGN * (Size div STACK_ITEM_ALIGN + 1);
  Dec(StackPosArr[Scope], Size);
end;

// ----------------------------------------------------------------------------
//
function TCompilerContext.CodeSize(): SizeInt;
begin
  Result := Intermediate.Code.Size;
end;

function TCompilerContext.Emit(Opcode: TInstruction; Pos: TDocPos): PtrInt;
begin
  if Opcode.Code = icNOOP then RaiseException('Tried to emit `NO_OPCODE`', Pos);
  Result := Intermediate.AddInstruction(Opcode, Pos);
end;

procedure TCompilerContext.PatchArg(Pos: SizeInt; ArgID:EInstructionArg; NewArg: PtrInt);
begin
  case ArgID of
    ia1: Intermediate.Code.Data[Pos].args[0].arg := NewArg;
    ia2: Intermediate.Code.Data[Pos].args[1].arg := NewArg;
    ia3: Intermediate.Code.Data[Pos].args[2].arg := NewArg;
    ia4: Intermediate.Code.Data[Pos].args[3].arg := NewArg;
    ia5: Intermediate.Code.Data[Pos].args[4].arg := NewArg;
  //ia6: Intermediate.Code.Data[Pos].args[5].arg := NewArg;
  end;
end;

procedure TCompilerContext.PatchJump(Addr: PtrInt; NewAddr: PtrInt=0);
begin
  if NewAddr = 0 then
    NewAddr := Self.CodeSize();

  case Intermediate.Code.Data[Addr].Code of
    icJMP:
      Intermediate.Code.Data[Addr].Args[0].arg := NewAddr;
    icRELJMP, icJFUNC, icJCONT, icJBREAK:
      Intermediate.Code.Data[Addr].Args[0].arg := NewAddr-Addr-1;
    icJZ, icJNZ:
      Intermediate.Code.Data[Addr].Args[1].arg := NewAddr-Addr-1;
    else
      RaiseException('Tried to patch none-jump instruction', Intermediate.DocPos.Data[Addr]);
  end;
end;

function TCompilerContext.RelAddr(Addr: PtrInt): TXprVar;
begin
  Result := TXprVar.Create(nil, Addr - Self.CodeSize() - 1, mpImm);
end;


// ----------------------------------------------------------------------------
//

function TCompilerContext.GetVar(Name: string; Pos:TDocPos): TXprVar;
var
  idx: XIntList;
begin
  Result := NullResVar;

  idx := Self.VarDecl[scope].GetDef(XprCase(Name), NULL_INT_LIST);
  if (idx.Data = nil) then
    RaiseExceptionFmt(eUndefinedIdentifier, [Name], Pos)
  else
  begin
    Result := Self.Variables.Data[idx.Data[0]];
  end;
end;

function TCompilerContext.GetVarList(Name: string; Pos:TDocPos): TXprVarList;
var
  list: XIntList;
  i: Int32;
begin
  Result.FTop := 0;
  Result.Data := nil;

  list := Self.VarDecl[scope].GetDef(XprCase(Name), NULL_INT_LIST);
  if (list.Data = nil) then
    RaiseExceptionFmt(eUndefinedIdentifier, [Name], Pos)
  else
  begin
    Result.Init([]);
    for i:=0 to list.High do
      Result.Add(Self.Variables.Data[list.data[i]]);
  end;
end;

function TCompilerContext.GetTempVar(Typ: XType): TXprVar;
begin
  Result := TXprVar.Create(Typ);
  if Scope = GLOBAL_SCOPE then
    Result.FMemPos := mpGlobal
  else
    Result.FMemPos := mpLocal;

  Result.FAddr := StackPos;
  IncStackPos(Result.FType.Size);
  Variables.Add(Result);
end;


// ----------------------------------------------------------------------------
//

function TCompilerContext.GetType(BaseType: EExpressBaseType; Pos:TDocPos): XType;
begin
  Result := Self.TypeDecl[scope].GetDef(XprCase(BT2S(BaseType)), nil);
  if Result = nil then
    RaiseExceptionFmt(eUndefinedIdentifier, [BT2S(BaseType)], Pos);
end;

function TCompilerContext.GetType(Name: string; Pos:TDocPos): XType;
begin
  Result := Self.TypeDecl[scope].GetDef(XprCase(Name), nil);
  if Result = nil then
    RaiseExceptionFmt(eUndefinedIdentifier, [Name], Pos);
end;

function TCompilerContext.GetType(Name: string): XType;
begin
  Result := Self.TypeDecl[scope].GetDef(XprCase(Name), nil);
end;

function TCompilerContext.GetType(BaseType: EExpressBaseType): XType;
begin
  Result := Self.TypeDecl[scope].GetDef(XprCase(BT2S(BaseType)), nil);
end;


// ----------------------------------------------------------------------------
//

procedure TCompilerContext.AddType(Name: string; Typ: XType);
begin
  self.TypeDecl[scope][XprCase(Name)] := Typ;
end;

// ----------------------------------------------------------------------------
//

(*
  Heap allocate constants later on before runtime
  We will update the bytecode to contain the address of the constant

  Some constants can go into Immediate (fits 64 bits) for higher performance
*)
function TCompilerContext.RegConst(Value: TXprVar): Int32;
begin
  Value.FMemPos := mpConst;
  Result := Self.Constants.Add(Value);
end;

function TCompilerContext.RegConst(constref Value: TConstant): TXprVar;
begin
  Result := TXprVar.Create(GetType(Value.Typ));
  Result.FMemPos := mpConst;
  Result.FAddr   := Intermediate.Constants.Add(Value) ;//GetMem(Result.FType.Size);
  RegConst(Result);
end;

function TCompilerContext.RegConst(Value: Boolean): TXprVar; begin Result := RegConst(Constant(Value, xtBoolean)); end;
function TCompilerContext.RegConst(Value: Int64):   TXprVar; begin Result := RegConst(Constant(Value, xtInt64)); end;
function TCompilerContext.RegConst(Value: Double):  TXprVar; begin Result := RegConst(Constant(Value, xtDouble)); end;


// ----------------------------------------------------------------------------
//

function TCompilerContext.RegVar(Name: string; var Value: TXprVar; DocPos: TDocPos): Int32;
var
  declList: XIntList = (FTop:0; Data:nil);
  exists: Boolean;
  i: Int32;
begin
  //if self.VarDecl[scope].Contains(Xprcase(Name)) then
  //  RaiseExceptionFmt(eSyntaxError, eIdentifierExists, [Name], DocPos);

  if Scope = GLOBAL_SCOPE then
  begin
    Value.FMemPos := mpGlobal;       //allocate and store address in opcode later on
    Inc(GlobalVarCount);
  end;
  Value.FAddr := StackPos;

  // add declaration
  Result := Self.Variables.Add(Value);

  exists := self.VarDecl[scope].Get(Xprcase(Name), declList);

  // if already declared variable, we overwrite it, unless we are adding functions
  // variables under the same name must ALWAYS be pushed into index 0
  if exists then
  begin
    if (Value.FType.BaseType in [xtMethod, xtExternalMethod]) then
      declList.Add(Result)
    else
      declList.Insert(Result, 0);
  end else
    declList.Init([Result]);

  Self.VarDecl[scope][Xprcase(Name)] := declList;
  IncStackPos(Value.FType.Size);
end;

function TCompilerContext.RegVar(Name: string; VarType: XType; DocPos: TDocPos; out Index: Int32): TXprVar;
begin
  Assert(VarType <> nil);
  Result := TXprVar.Create(VarType);
  if Scope <> GLOBAL_SCOPE then
    Result.FMemPos := mpLocal;

  Result.FAddr := StackPos;
  Result.FReference := False;
  Index := Self.RegVar(Name, Result, DocPos);
end;

function TCompilerContext.RegVar(Name: string; VarType: XType; DocPos: TDocPos): TXprVar;
var _: Int32;
begin
  Result := Self.RegVar(Name, VarType, DocPos, _);
end;



function TCompilerContext.AddVar(constref Value; Name: string; BaseType: EExpressBaseType; DocPos: TDocPos): TXprVar; overload;
begin
  (*
  Result := RegVar(Name, GetType(BaseType), DocPos);
  case BaseType of
    xtBoolean:  Boolean(Result.FPtr^)  := UInt8(Value) <> 0;
    xtAnsiChar: AnsiChar(Result.FPtr^) := AnsiChar(Value);
    xtWideChar: WideChar(Result.FPtr^) := WideChar(Value);
    xtInt8:    Int8(Result.FPtr^)   := Int8(Value);
    xtInt16:   Int16(Result.FPtr^)  := Int16(Value);
    xtInt32:   Int32(Result.FPtr^)  := Int32(Value);
    xtInt64:   Int64(Result.FPtr^)  := Int64(Value);
    xtUInt8:   UInt8(Result.FPtr^)  := UInt8(Value);
    xtUInt16:  UInt16(Result.FPtr^) := UInt16(Value);
    xtUInt32:  UInt32(Result.FPtr^) := UInt32(Value);
    xtUInt64:  UInt64(Result.FPtr^) := UInt64(Value);
    xtSingle:  Single(Result.FPtr^) := Single(Value);
    xtDouble:  Double(Result.FPtr^) := Double(Value);
    xtAnsiString: AnsiString(Result.FPtr^) := AnsiString(Value);
    xtWideString: WideString(Result.FPtr^) := WideString(Value);
  end;
  *)
end;

function TCompilerContext.AddVar(Value: Int64; Name: string; DocPos: TDocPos): TXprVar;
begin
  //Result := RegVar(Name, GetType(xtInt64), DocPos);
  //Move(Value, Result.FAddr^, Result.FType.Size);
end;

function TCompilerContext.AddVar(Value: Double;  Name: string; DocPos: TDocPos): TXprVar;
begin
  //Result := RegVar(Name, GetType(xtDouble), DocPos);
  //Move(Value, Result.FAddr^, Result.FType.Size);
end;

function TCompilerContext.AddVar(Value: Boolean; Name: string; DocPos: TDocPos): TXprVar;
begin
  //Result := RegVar(Name, GetType(xtBoolean), DocPos);
  //Move(Value, Result.FAddr^, Result.FType.Size);
end;

// ----------------------------------------------------------------------------
//
function TCompilerContext.AddExternalFunc(Addr: TExternalProc; Name: string; Params: array of XType; PassBy: array of EPassBy; ResType: XType): TXprVar;
var
  i: Int32;
  argtypes: XTypeArray;
  passing: TPassArgsBy;
  exists: Boolean;
  declList: XIntList = (FTop:0; Data:nil);
begin
  //if self.VarDecl[scope].Contains(Xprcase(Name)) then
  //  RaiseExceptionFmt(eSyntaxError, eIdentifierExists, [Name], NoDocPos);

  if Length(Params) <> Length(PassBy) then
    RaiseException('Lengths must be the same');

  SetLength(argtypes, Length(Params));
  SetLength(passing, Length(PassBy));
  for i:=0 to High(params) do
  begin
    passing[i]  := PassBy[i];
    argtypes[i] := Params[i];
  end;

  Result := TXprVar.Create(XType_Method.Create(Name, argtypes, passing, ResType, False), PtrInt(Addr), mpHeap);

  exists := self.VarDecl[scope].Get(Xprcase(Name), declList);
  if exists then
    declList.Add(Self.Variables.Add(Result))
  else
    declList.Init([Self.Variables.Add(Result)]);

  Self.VarDecl[scope][Xprcase(Name)] := declList;
end;


// ----------------------------------------------------------------------------
//
procedure TCompilerContext.RegisterInternals;
begin
  AddType(BT2S(xtBoolean),  XType_Bool.Create(xtBoolean));
  AddType(BT2S(xtAnsiChar), XType_Char.Create(xtAnsiChar));
  AddType(BT2S(xtWideChar), XType_Char.Create(xtWideChar));

  AddType(BT2S(xtInt8),     XType_Integer.Create(xtInt8));
  AddType(BT2S(xtInt16),    XType_Integer.Create(xtInt16));
  AddType(BT2S(xtInt32),    XType_Integer.Create(xtInt32));
  AddType(BT2S(xtInt64),    XType_Integer.Create(xtInt64));
  AddType(BT2S(xtUInt8),    XType_Integer.Create(xtUInt8));
  AddType(BT2S(xtUInt16),   XType_Integer.Create(xtUInt16));
  AddType(BT2S(xtUInt32),   XType_Integer.Create(xtUInt32));
  AddType(BT2S(xtUInt64),   XType_Integer.Create(xtUInt64));

  AddType(BT2S(xtSingle),   XType_Float.Create(xtSingle));
  AddType(BT2S(xtDouble),   XType_Float.Create(xtDouble));

  AddType(BT2S(xtPointer),  XType_Pointer.Create(xtPointer));

  AddType(BT2S(xtAnsiString), XType_String.Create(self.GetType(xtAnsiChar)));
  AddType(BT2S(xtWideString), XType_String.Create(self.GetType(xtWideChar)));

  (* alias and system specific *)
  AddType('Int',    self.GetType(xtInt));
  AddType('Float',  self.GetType(xtDouble));
  AddType('Char',   self.GetType(xtChar));
  AddType('String', self.GetType(xtString));

  AddType('NativeInt', self.GetType(xtInt));
  AddType('PtrInt',    self.GetType(xtInt));
end;


(*~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~*)

constructor XType.Create(ABaseType: EExpressBaseType = xtUnknown);
begin
  BaseType := ABaseType;
end;

function XType.Size: SizeInt;
begin
  Result := XprTypeSize[BaseType];
end;

function XType.EvalCode(OP: EOperator; Other: XType): EIntermediate;
begin
  if other = nil then
    Exit(OP2IC(OP));
  Result := OP2IC(OP);
end;

function XType.CanAssign(Other: XType): Boolean;
begin
  Result := True;
end;

function XType.ResType(OP: EOperator; Other: XType; ctx: TCompilerContext): XType;
begin
  Result := ctx.GetType(GetEvalRes(OP, Self.BaseType, Other.BaseType));
end;

function XType.Equals(Other: XType): Boolean;
begin
  Result := (Self = Other) and (Self.BaseType = Other.BaseType);
end;

(*~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~*)

constructor TXprVar.Create(AType: XType; AAddr:PtrInt=0; AMemPos: EMemPos=mpLocal);
begin
  Self.FType   := AType;
  Self.FAddr   := AAddr;
  Self.FMemPos := AMemPos;
  Self.FReference := False;
end;

function TXprVar.DerefToTemp(ctx: TCompilerContext): TXprVar;
begin
  Result := Self.Deref(ctx, NullResVar);
end;

function TXprVar.Deref(ctx: TCompilerContext; Dest: TXprVar): TXprVar;
var
  instr: EIntermediate;
begin
  Result := Dest;
  if Result = NullResVar then
    Result := ctx.GetTempVar(Self.FType);
  instr := Result.FType.EvalCode(op_Deref, nil);
  if instr <> icNOOP then
    ctx.Emit(GetInstr(instr, [Result, Self, Immediate(Self.FType.Size)]), NoDocPos)
  else
    ctx.Emit(GetInstr(icDREF, [Result, Self, Immediate(Self.FType.Size)]), NoDocPos);
end;

(*~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~*)

operator = (L,R: TXprVar): Boolean;
begin
  Result := (L.FType = R.FType) and (L.FAddr = R.FAddr)
end;

operator <> (L,R: TXprVar): Boolean;
begin
  Result := (L.FType <> R.FType) or (L.FAddr <> R.FAddr);
end;


// ----------------------------------------------------------------------------
//
function GetInstr(OP: EIntermediate; args: array of TXprVar): TInstruction;
var
  i: Int32;
begin
  Result.Code  := OP;
  Result.nArgs := Length(args);
  for i:=0 to Min(8, Result.nArgs)-1 do
  begin
    Result.Args[i].Arg := args[i].FAddr;
    if(args[i].FType <> nil) then
      Result.Args[i].Typ := args[i].FType.BaseType
    else
      Result.Args[i].Typ := xtUnknown;
    Result.Args[i].Pos := args[i].FMemPos;
  end;
end;

function GetInstr(OP: EIntermediate): TInstruction;
begin
  Result.Code  := OP;
  Result.nArgs := 0;
end;

function STORE_FAST(Left, Right: TXprVar; Heap: Boolean): TInstruction;
begin
  if Heap then
    Result := GetInstr(icMOVH, [Left, Right, Immediate(Right.FType.Size)])
  else
    Result := GetInstr(icMOV,  [Left, Right, Immediate(Right.FType.Size)]);
end;

function Immediate(v: PtrInt; Typ: XType = nil): TXprVar;
begin
  Result := TXprVar.Create(Typ, v, mpImm);
end;

function OpAddr(v: PtrInt; loc:EMemPos=mpHeap): TXprVar;
begin
  Result := TXprVar.Create(nil, v, loc);
end;

end.
