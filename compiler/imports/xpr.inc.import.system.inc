{ ============================================================================ }
{                          NATIVE FPC IMPLEMENTATIONS                          }
{ ============================================================================ }


procedure _AtomicIncRef(const Params: PParamArray); cdecl;
var p: PSizeInt;
begin
  p := PSizeInt(Pointer(Params^[0]^) - 2*SizeOf(SizeInt));
  if p^ > 0 then  // skip rc=-1 (constants)
    {$IFDEF CPU64}
    InterlockedIncrement64(p^);
    {$ELSE}
    InterlockedIncrement(p^);
    {$ENDIF}
end;

procedure _AtomicDecRef(const Params: PParamArray); cdecl;
var p: PSizeInt;
begin
  p := PSizeInt(Pointer(Params^[0]^) - 2*SizeOf(SizeInt));
  if p^ > 0 then  // skip rc=-1 (constants)
    {$IFDEF CPU64}
    InterlockedDecrement64(p^);
    {$ELSE}
    InterlockedDecrement(p^);
    {$ENDIF}
end;



// -----------------------------------
// Native string
// -----------------------------------
procedure _AnsiSetLength(const Params: PParamArray); cdecl;
begin
  SetLength(AnsiString(Params^[0]^), SizeInt(Params^[1]^));
end;

procedure _UnicodeSetLength(const Params: PParamArray); cdecl;
begin
  SetLength(UnicodeString(Params^[0]^), SizeInt(Params^[1]^));
end;

procedure _AnsiRefcount(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  PSizeInt(Result)^ := StringRefCount(AnsiString(Params^[0]^));
end;

procedure _UnicodeRefcount(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  PSizeInt(Result)^ := StringRefCount(UnicodeString(Params^[0]^));
end;

// ------------------------------
// Time & Date
// ------------------------------
procedure _GetTickCount(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int64(Result^) := SysUtils.GetTickCount64();
end;

procedure _MarkTime(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := MarkTime();
end;

procedure _UnixTime(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int64(Result^) := DateUtils.DateTimeToUnix(Now());
end;

procedure _Now(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := SysUtils.Now();
end;

procedure _Date(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := SysUtils.Date();
end;

procedure _Time(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := SysUtils.Time();
end;

procedure _EncodeDate(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := SysUtils.EncodeDate(Int64(Params^[0]^), Int64(Params^[1]^), Int64(Params^[2]^));
end;

procedure _EncodeTime(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := SysUtils.EncodeTime(Int64(Params^[0]^), Int64(Params^[1]^), Int64(Params^[2]^), Int64(Params^[3]^));
end;

procedure _DecodeDate(const Params: PParamArray); cdecl;
var
  y, m, d: Word;
begin
  SysUtils.DecodeDate(Double(Params^[0]^), y, m, d);
  PInt64(Pointer(Params^[1]^))^ := y;
  PInt64(Pointer(Params^[2]^))^ := m;
  PInt64(Pointer(Params^[3]^))^ := d;
end;

procedure _DecodeTime(const Params: PParamArray); cdecl;
var
  h, m, s, ms: Word;
begin
  SysUtils.DecodeTime(Double(Params^[0]^), h, m, s, ms);
  PInt64(Pointer(Params^[1]^))^ := h;
  PInt64(Pointer(Params^[2]^))^ := m;
  PInt64(Pointer(Params^[3]^))^ := s;
  PInt64(Pointer(Params^[4]^))^ := ms;
end;

procedure _DateTimeToUnix(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int64(Result^) := DateUtils.DateTimeToUnix(Double(Params^[0]^));
end;

procedure _UnixToDateTime(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := DateUtils.UnixToDateTime(Int64(Params^[0]^));
end;

procedure _FormatDateTime(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  AnsiString(Result^) := SysUtils.FormatDateTime(PAnsiString(Params^[0])^, Double(Params^[1]^));
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

procedure _Floor(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int64(Result^) := Floor(Double(Params^[0]^));
end;

procedure _Ceil(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int64(Result^) := Ceil(Double(Params^[0]^));
end;

procedure _Round(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int64(Result^) := Round(Double(Params^[0]^));
end;

procedure _RoundTo(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Double(Result^) := RoundTo(Double(Params^[0]^), TRoundToRange(-Int64(Params^[1]^)));
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
// File & Directory Manipulation
// -----------------------------------
procedure _FileExists(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Boolean(Result^) := SysUtils.FileExists(PAnsiString(Params^[0])^);
end;

procedure _DirectoryExists(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Boolean(Result^) := SysUtils.DirectoryExists(PAnsiString(Params^[0])^);
end;

procedure _DeleteFile(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Boolean(Result^) := SysUtils.DeleteFile(PAnsiString(Params^[0])^);
end;

procedure _ExtractFilePath(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  AnsiString(Result^) := SysUtils.ExtractFilePath(PAnsiString(Params^[0])^);
end;

procedure _ExtractFileName(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  AnsiString(Result^) := SysUtils.ExtractFileName(PAnsiString(Params^[0])^);
end;

procedure _ExtractFileExt(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  AnsiString(Result^) := SysUtils.ExtractFileExt(PAnsiString(Params^[0])^);
end;

procedure _GetFileSize(const Params: PParamArray; const Result: Pointer); cdecl;
var
  F: file of byte;
begin
  AssignFile(F, PAnsiString(Params^[0])^);
  {$I-}
  Reset(F);
  if IOResult = 0 then
  begin
    Int64(Result^) := FileSize(F);
    CloseFile(F);
  end else
    Int64(Result^) := -1;
  {$I+}
end;

procedure _ReadFile(const Params: PParamArray; const Result: Pointer); cdecl;
var
  Stream: TMemoryStream;
  S: AnsiString;
begin
  if SysUtils.FileExists(PAnsiString(Params^[0])^) then
  begin
    Stream := TMemoryStream.Create;
    try
      Stream.LoadFromFile(PAnsiString(Params^[0])^);
      SetLength(S, Stream.Size);
      if Stream.Size > 0 then
        Stream.ReadBuffer(S[1], Stream.Size);
      AnsiString(Result^) := S;
    finally
      Stream.Free;
    end;
  end else
    AnsiString(Result^) := '';
end;

procedure _WriteFile(const Params: PParamArray; const Result: Pointer); cdecl;
var
  Stream: TMemoryStream;
  S: AnsiString;
begin
  Stream := TMemoryStream.Create;
  try
    S := PAnsiString(Params^[1])^;
    if Length(S) > 0 then
      Stream.WriteBuffer(S[1], Length(S));
    try
      Stream.SaveToFile(PAnsiString(Params^[0])^);
      Boolean(Result^) := True;
    except
      Boolean(Result^) := False;
    end;
  finally
    Stream.Free;
  end;
end;

procedure _AppendFile(const Params: PParamArray; const Result: Pointer); cdecl;
var
  Stream: TMemoryStream;
  S: AnsiString;
begin
  Stream := TMemoryStream.Create;
  try
    if SysUtils.FileExists(PAnsiString(Params^[0])^) then
      Stream.LoadFromFile(PAnsiString(Params^[0])^);
    Stream.Position := Stream.Size;
    S := PAnsiString(Params^[1])^;
    if Length(S) > 0 then
      Stream.WriteBuffer(S[1], Length(S));
    try
      Stream.SaveToFile(PAnsiString(Params^[0])^);
      Boolean(Result^) := True;
    except
      Boolean(Result^) := False;
    end;
  finally
    Stream.Free;
  end;
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

procedure _GetMemoryManager(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  GetMemoryManager(TMemoryManager(Result^));
end;

// ---- threading
procedure _ThreadJoin(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  WaitForThreadTerminate(TThreadID(Int64(Params^[0]^)), High(LongInt));
  UInt32(Result^) := CloseThread(TThreadID(Int64(Params^[0]^)));
end;

procedure _ThreadSuspend(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  UInt32(Result^) := SuspendThread(TThreadID(Int64(Params^[0]^)));
end;

procedure _ThreadResume(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  UInt32(Result^) := ResumeThread(TThreadID(Int64(Params^[0]^)));
end;

procedure _ThreadClose(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  UInt32(Result^) := CloseThread(TThreadID(Int64(Params^[0]^)));
end;

procedure _ThreadID(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  UInt32(Result^) := GetThreadID;
end;

procedure _CSInit(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Pointer(Result^) := GetMem(SizeOf(TRTLCriticalSection));
  InitCriticalSection(TRTLCriticalSection(Pointer(Result^)^));
end;

procedure _CSLock(const Params: PParamArray); cdecl;
begin
  EnterCriticalSection(PRTLCriticalSection(PPointer(Params^[0])^)^);
end;

procedure _CSUnlock(const Params: PParamArray); cdecl;
begin
  LeaveCriticalSection(PRTLCriticalSection(PPointer(Params^[0])^)^);
end;

procedure _CSDestroy(const Params: PParamArray); cdecl;
begin
  DoneCriticalSection(PRTLCriticalSection(PPointer(Params^[0])^)^);
  FreeMem(PPointer(Params^[0])^);
  PPointer(Params^[0])^ := nil;
end;


// ------------------------------
// FFI / Dynamic Library Loading
// ------------------------------

procedure _LoadLib(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int64(Result^) := Int64(LoadLibrary(PAnsiString(Params^[0])^));
end;

procedure _FreeLib(const Params: PParamArray); cdecl;
begin
  FreeLibrary(TLibHandle(Int64(Params^[0]^)));
end;

procedure _GetProc(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int64(Result^) := Int64(PtrUInt(GetProcAddress(TLibHandle(Int64(Params^[0]^)),  PAnsiString(Params^[1])^)));
end;


procedure _FreeCallback(const Params: PParamArray); cdecl;
var FuncPtr: Pointer;
begin
  FuncPtr := Pointer(Params^[0]^);
  XprUnregisterAndFreeClosure(FuncPtr);
end;


// ------------------------------
// Sockets
// ------------------------------

procedure _fpSocket(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int32(Result^) := fpSocket(
    Int32(Params^[0]^),
    Int32(Params^[1]^),
    Int32(Params^[2]^)
  );
end;

procedure _fpConnect(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int32(Result^) := fpConnect(
    Int32(Params^[0]^),
    PSockAddr(Params^[1]^),
    Int32(Params^[2]^)
  );
end;

procedure _fpBind(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int32(Result^) := fpBind(
    Int32(Params^[0]^),
    PSockAddr(Params^[1]^),
    TSockLen(Params^[2]^)
  );
end;

procedure _fpListen(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int32(Result^) := fpListen(
    Int32(Params^[0]^),
    Int32(Params^[1]^)
  );
end;

procedure _fpAccept(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int32(Result^) := fpAccept(
    Int32(Params^[0]^),
    PSockAddr(Params^[1]^),
    PSockLen(Params^[2]^)
  );
end;

procedure _fpRecv(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int32(Result^) := fpRecv(
    Int32(Params^[0]^),
    Pointer(Params^[1]^),
    Int32(Params^[2]^),
    Int32(Params^[3]^)
  );
end;

procedure _fpRecvFrom(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int32(Result^) := fpRecvFrom(
    Int32(Params^[0]^),
    Pointer(Params^[1]^),
    Int32(Params^[2]^),
    Int32(Params^[3]^),
    PSockAddr(Params^[4]^),
    PSockLen(Params^[5]^)
  );
end;

procedure _fpSend(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int32(Result^) := fpSend(
    Int32(Params^[0]^),
    Pointer(Params^[1]^),
    Int32(Params^[2]^),
    Int32(Params^[3]^)
  );
end;

procedure _fpSendTo(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int32(Result^) := fpSendTo(
    Int32(Params^[0]^),
    Pointer(Params^[1]^),
    Int32(Params^[2]^),
    Int32(Params^[3]^),
    PSockAddr(Params^[4]^),
    TSockLen(Params^[5]^)
  );
end;

procedure _fpClose(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int32(Result^) := CloseSocket(Int32(Params^[0]^));
end;

procedure _fpSetSockOpt(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int32(Result^) := fpSetSockOpt(
    Int32(Params^[0]^),
    Int32(Params^[1]^),
    Int32(Params^[2]^),
    Pointer(Params^[3]^),
    Int32(Params^[4]^)
  );
end;

procedure _fpGetSockOpt(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int32(Result^) := fpGetSockOpt(
    Int32(Params^[0]^),
    Int32(Params^[1]^),
    Int32(Params^[2]^),
    Pointer(Params^[3]^),
    PSockLen(Params^[4]^)
  );
end;

procedure _fpSelect(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  (*
  Int32(Result^) := fpSelect(
    Int32(Params^[0]^),
    PFDSet(Params^[1]^),
    PFDSet(Params^[2]^),
    PFDSet(Params^[3]^),
    PTimeVal(Params^[4]^)
  );
  *)
end;

procedure _fpShutdown(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int32(Result^) := fpShutdown(
    Int32(Params^[0]^),
    Int32(Params^[1]^)
  );
end;


// ------------------------------
// Networking
// ------------------------------

procedure _htons(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  UInt16(Result^) := htons(UInt16(Params^[0]^));
end;

procedure _htonl(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  UInt32(Result^) := htonl(UInt32(Params^[0]^));
end;

procedure _ntohs(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  UInt16(Result^) := ntohs(UInt16(Params^[0]^));
end;

procedure _ntohl(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  UInt32(Result^) := ntohl(UInt32(Params^[0]^));
end;



// ------------------------------
// Serial
// ------------------------------

procedure _SerOpen(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int32(Result^) := SerOpen(string(Params^[0]^));
end;

procedure _SerClose(const Params: PParamArray); cdecl;
begin
  SerClose(Int32(Params^[0]^));
end;

procedure _SerSetParams(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  SerSetParams(
    TSerialHandle(Params^[0]^),
    Int32(Params^[1]^),
    Int32(Params^[2]^),
    TParityType(Params^[3]^),
    Int32(Params^[4]^),
    TSerialFlags(Params^[5]^)
  );
end;

procedure _SerRead(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int32(Result^) := SerRead(
    Int32(Params^[0]^),
    PChar(Params^[1]^)^,
    Int32(Params^[2]^)
  );
end;

procedure _SerWrite(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  Int32(Result^) := SerWrite(
    Int32(Params^[0]^),
    PChar(Params^[1]^)^,
    Int32(Params^[2]^)
  );
end;

procedure _SerSync(const Params: PParamArray); cdecl;
begin
  SerSync(Int32(Params^[0]^));
end;

procedure _SerFlushOutput(const Params: PParamArray); cdecl;
begin
  SerFlushOutput(Int32(Params^[0]^));
end;
