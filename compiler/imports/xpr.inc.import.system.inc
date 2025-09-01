{ ============================================================================ }
{                          NATIVE FPC IMPLEMENTATIONS                          }
{ ============================================================================ }

// ------------------------------
// Time
// ------------------------------
procedure _GetTickCount(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := MarkTime();
end;

procedure _Sleep(const Params: PParamArray); cdecl;
begin
  Sleep(Int64(Params^[0]^));
end;

// ------------------------------
// String Conversions
// ------------------------------
procedure _IntToStr(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  AnsiString(Result^) := IntToStr(Int64(Params^[0]^));
end;

procedure _FloatToStr(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  AnsiString(Result^) := FloatToStr(Double(Params^[0]^));
end;

procedure _PtrToStr(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  AnsiString(Result^) := IntToHex(PtrUInt(Params^[0]^));
end;

procedure _StrToInt(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int64(Result^) := StrToIntDef(PAnsiString(Params^[0])^, 0);
end;

procedure _StrToFloat(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := StrToFloatDef(PAnsiString(Params^[0])^, 0.0);
end;

// ------------------------------
// Character/Ordinal Conversions
// ------------------------------
procedure _Ord(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int64(Result^) := Ord(AnsiChar(Params^[0]^));
end;

procedure _Chr(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  AnsiChar(Result^) := Chr(Int64(Params^[0]^));
end;

// ------------------------------
// Math
// ------------------------------
procedure _Inc64(const Params: PParamArray); cdecl;
begin
  Inc(Int64(Params^[0]^));
end;

procedure _Dec64(const Params: PParamArray); cdecl;
begin
  Dec(Int64(Params^[0]^));
end;

procedure _RandInt(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  if Int64(Params^[1]^) <= Int64(Params^[0]^) then
    Int64(Result^) := Int64(Params^[0]^)
  else
    Int64(Result^) := System.Random(Int64(Params^[1]^) - Int64(Params^[0]^) + 1) + Int64(Params^[0]^);
end;

procedure _RandomFloat(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := Random();
end;

procedure _Sin(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := Sin(Double(Params^[0]^));
end;

procedure _Cos(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := Cos(Double(Params^[0]^));
end;

procedure _Sqrt(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := Sqrt(Double(Params^[0]^));
end;

procedure _Ln(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := Ln(Double(Params^[0]^));
end;

procedure _Trunc(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int64(Result^) := Trunc(Double(Params^[0]^));
end;

procedure _Round(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int64(Result^) := Round(Double(Params^[0]^));
end;

procedure _AbsInt(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int64(Result^) := Abs(Int64(Params^[0]^));
end;

procedure _AbsFloat(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := Abs(Double(Params^[0]^));
end;

procedure _MinInt(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int64(Result^) := Min(Int64(Params^[0]^), Int64(Params^[1]^));
end;

procedure _MaxInt(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int64(Result^) := Max(Int64(Params^[0]^), Int64(Params^[1]^));
end;

procedure _Tan(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := Tan(Double(Params^[0]^));
end;

procedure _ArcTan(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := ArcTan(Double(Params^[0]^));
end;

procedure _ArcTan2(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := ArcTan2(Double(Params^[0]^), Double(Params^[1]^));
end;

procedure _Exp(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := Exp(Double(Params^[0]^));
end;

procedure _Frac(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := Frac(Double(Params^[0]^));
end;

procedure _IntPower(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := IntPower(Double(Params^[0]^), Int64(Params^[1]^));
end;


// -----------------------------------
// Pointer Manipulation
// -----------------------------------
procedure _FreeMem(const Params: PParamArray); cdecl;
begin
  FreeMem(Pointer(Params^[0]^));
end;

procedure _AllocMem(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Pointer(Result^) := AllocMem(SizeInt(Params^[0]^));
end;

procedure _GetMem(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Pointer(Result^) := GetMem(SizeInt(Params^[0]^));
end;

procedure _ReallocMem(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Pointer(Result^) := ReAllocMem(PPointer(Params^[0])^, SizeInt(Params^[1]^));
end;

procedure _FillByte(const Params: PParamArray); cdecl;
begin
  FillByte(Pointer(Params^[0]^)^, SizeInt(Params^[1]^), Byte(Params^[2]^));
end;

procedure _Move(const Params: PParamArray); cdecl;
begin
  Move(Pointer(Params^[0]^)^, Pointer(Params^[1]^)^, SizeInt(Params^[2]^));
end;
