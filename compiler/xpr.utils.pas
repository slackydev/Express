unit xpr.Utils;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
{$I header.inc}
{.$hints off}

interface

uses
  Classes, SysUtils;

type
  UTStringArray = array of string;
  UTIntArray    = array of Int32;

procedure WriteFancy(Text: string); overload;
procedure WriteFancy(Text: string; Args: array of const); overload;

function LoadFileContents(fileName: string): string;
function MarkTime(): Double; inline;

function NextPow2(n: Int32): Int32; inline;

function StrToFloatDot(const S: string): Extended; inline;
function FloatToStrDot(f: Extended): string; inline;

function StringContains(arr: array of string; v: string): Boolean;
function StrPosEx(const SubStr, Text: string): UTIntArray;
function StrExplode(const Text, Sep: string): UTStringArray;
function StartsWith(const Prefix, Text: string): Boolean;
function xprHash(P: PByte; const Len: UInt32; const Seed: UInt32 = 0): UInt32;

const
  //asssuming default color theme
  _BLACK_   = {$IF Defined(WINDOWS) AND Defined(COLORIZE)}'{$0}'{$ELSE}''{$ENDIF};
  _BLUE_    = {$IF Defined(WINDOWS) AND Defined(COLORIZE)}'{$1}'{$ELSE}''{$ENDIF};
  _GREEN_   = {$IF Defined(WINDOWS) AND Defined(COLORIZE)}'{$2}'{$ELSE}''{$ENDIF};
  _AQUA_    = {$IF Defined(WINDOWS) AND Defined(COLORIZE)}'{$3}'{$ELSE}''{$ENDIF};
  _RED_     = {$IF Defined(WINDOWS) AND Defined(COLORIZE)}'{$4}'{$ELSE}''{$ENDIF};
  _PURPLE_  = {$IF Defined(WINDOWS) AND Defined(COLORIZE)}'{$5}'{$ELSE}''{$ENDIF};
  _YELLOW_  = {$IF Defined(WINDOWS) AND Defined(COLORIZE)}'{$6}'{$ELSE}''{$ENDIF};
  _WHITE_   = {$IF Defined(WINDOWS) AND Defined(COLORIZE)}'{$7}'{$ELSE}''{$ENDIF};
  _GRAY_    = {$IF Defined(WINDOWS) AND Defined(COLORIZE)}'{$8}'{$ELSE}''{$ENDIF};
  _LBLUE_   = {$IF Defined(WINDOWS) AND Defined(COLORIZE)}'{$9}'{$ELSE}''{$ENDIF};
  _LGREEN_  = {$IF Defined(WINDOWS) AND Defined(COLORIZE)}'{$A}'{$ELSE}''{$ENDIF};
  _LAQUA_   = {$IF Defined(WINDOWS) AND Defined(COLORIZE)}'{$B}'{$ELSE}''{$ENDIF};
  _LRED_    = {$IF Defined(WINDOWS) AND Defined(COLORIZE)}'{$C}'{$ELSE}''{$ENDIF};
  _LPURPLE_ = {$IF Defined(WINDOWS) AND Defined(COLORIZE)}'{$D}'{$ELSE}''{$ENDIF};
  _LYELLOW_ = {$IF Defined(WINDOWS) AND Defined(COLORIZE)}'{$E}'{$ELSE}''{$ENDIF};
  _LWHITE_  = {$IF Defined(WINDOWS) AND Defined(COLORIZE)}'{$F}'{$ELSE}''{$ENDIF};

implementation

uses
  Math, DateUtils, {$IFDEF WINDOWS}Windows{$ELSE}BaseUnix,Unix{$ENDIF};

type
  TColorAttribute = record color:string; id:Int32; end;

const
  COLOR_ATTRIBUTES: array [0..15] of TColorAttribute = (
    (color:'{$0}'; id:0),
    (color:'{$1}'; id:1),
    (color:'{$2}'; id:2),
    (color:'{$3}'; id:3),
    (color:'{$4}'; id:4),
    (color:'{$5}'; id:5),
    (color:'{$6}'; id:6),
    (color:'{$7}'; id:7),
    (color:'{$8}'; id:8),
    (color:'{$9}'; id:9),
    (color:'{$A}'; id:10),
    (color:'{$B}'; id:11),
    (color:'{$C}'; id:12),
    (color:'{$D}'; id:13),
    (color:'{$E}'; id:14),
    (color:'{$F}'; id:15)
   );

procedure WriteFancy(Text: string);
var
  {$IFDEF WINDOWS}
  hConsole:HANDLE;
  consoleInfo:CONSOLE_SCREEN_BUFFER_INFO;
  {$ENDIF}
  attr: TColorAttribute;
  old, oldpos, newpos:Int32;

  function MatchStrAt(const Prefix, Text:String; StartAt:Int32=1): Boolean;
  begin
    Result := Copy(Text, StartAt, Length(Prefix)) = Prefix;
  end;

  function FindAttr(StartAt:Int32; out attr:TColorAttribute): Int32;
  var i,j:Int32;
  begin
    Result := Length(Text)+1;
    attr.color := '';
    attr.id    := 7;
    for i:=StartAt to Length(Text) do
    begin
      for j:=0 to High(COLOR_ATTRIBUTES) do
        if MatchStrAt(COLOR_ATTRIBUTES[j].color, Text, i) then
        begin
          attr := COLOR_ATTRIBUTES[j];
          Exit(i);
        end;
    end;
  end;

begin
  {$IFDEF WINDOWS}
  hConsole := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(hConsole, consoleInfo);
  old := consoleInfo.wAttributes;
  {$ENDIF}
  Text += #0;

  newpos := 1;
  repeat
    oldpos := newpos;
    newpos := FindAttr(oldpos, attr);
    Write(Copy(Text, oldpos, newpos-oldpos)); //copy from here to the code
    newpos += Length(attr.color);
    {$IFDEF WINDOWS}
    SetConsoleTextAttribute(hConsole, attr.id);
    {$ENDIF}
  until newpos >= Length(Text);
  {$IFDEF WINDOWS}
  SetConsoleTextAttribute(hConsole, old);
  {$ENDIF}
  WriteLn('');
  Flush(Output);
end;

procedure WriteFancy(Text: string; Args: array of const);
begin
  WriteFancy(Format(Text, Args));
end;


function LoadFileContents(fileName: string): string;
var
  f: TStringList;
begin
  if FileExists(fileName) then
  begin
    f := TStringList.Create();
    f.LoadFromFile(fileName);
    Result := f.Text;
    f.Free();
  end else
    raise Exception.CreateFmt('File `%s` not found!', [fileName]);
end;


function MarkTime(): Double;
var
  count, frequency: Int64;
  {$IFDEF UNIX}
  TV:TTimeVal;
  TZ:PTimeZone;
  {$ENDIF}
begin
  {$IFDEF UNIX}
  TZ := nil;
  fpGetTimeOfDay(@TV, TZ);
  count := Int64(TV.tv_sec) * 1000000 + Int64(TV.tv_usec);
  Result := count / 1000;
  {$ELSE}
  QueryPerformanceFrequency(frequency);
  QueryPerformanceCounter(count);
  Result := count / frequency * 1000;
  {$ENDIF}
end;


function NextPow2(n: Int32): Int32;
begin
  n := n or (n shr 1);
  n := n or (n shr 2);
  n := n or (n shr 4);
  n := n or (n shr 8);
  n := n or (n shr 16);
  n := n or (n shr 32);
  Result := n;
end;

function StrToFloatDot(const S: string): Extended;
begin
  Result := StrToFloat(StringReplace(S, '.', FormatSettings.DecimalSeparator, []));
end;

function FloatToStrDot(f:Extended): string;
begin
  Result := StringReplace(FloatToStr(f), FormatSettings.DecimalSeparator, '.', []);
end;

function StringContains(arr:array of string; v:string): Boolean;
var i:Int32;
begin
  Result := False;
  for i:=0 to High(arr) do
    if arr[i] = v then
      Exit(True);
end;

{*
  Returns all positions of the given pattern/substring.
*}
function StrPosEx(const SubStr, Text:String): UTIntArray;
var
  HitPos,LenSub,h,q,i: UInt32;
begin
  LenSub := Length(SubStr);
  if LenSub = 0 then Exit(nil);
  HitPos := 1;
  h := 0;
  q := 1;
  SetLength(Result, q);
  for i:=1 to Length(Text) do
    if Text[i] <> SubStr[HitPos] then
      HitPos := 1
    else begin
      if (HitPos <> LenSub) then
        Inc(HitPos)
      else begin
        if q <= h then
        begin
          q := q+q;
          SetLength(Result, q);
        end;
        Result[h] := (i - HitPos) + 1;
        Inc(h);
        HitPos := 1;
      end;
    end;
  SetLength(Result, h);
end;

function StrExplode(const Text, Sep: String): UTStringArray;
var
  Subs: UTIntArray;
  Hi,i,Curr,Prev,HiSep: UInt32;
begin
  Hi := Length(Text);
  if Hi = 0 then Exit(nil);

  Subs := StrPosEx(Sep, Text);
  if Length(Subs) = 0 then
  begin
    SetLength(Result, 1);
    Result[0] := Copy(Text, 1,Hi);
    Exit;
  end;
  HiSep := Length(Sep);
  Prev := 1;
  SetLength(Result, Length(Subs));
  for i:=0 to High(Subs) do
  begin
    Curr := Subs[i];
    Result[i] := Copy(Text, Prev, (Curr-Prev));
    Prev := Curr+HiSep;
  end;
  if Prev <= Hi then
  begin
    SetLength(Result, Length(Subs)+1);
    Result[Length(Subs)] := Copy(Text, Prev, Hi);
  end;
end;

function StartsWith(const Prefix, Text:String): Boolean;
begin
  Result := Copy(Text,1,Length(Prefix)) = Prefix;
end;

// xxHash32
// Pascal implementation from https://github.com/synopse/mORMot2
function xprHash(P: PByte; const Len: UInt32; const Seed: UInt32): UInt32;
const
  PRIME32_1 = 2654435761;
  PRIME32_2 = 2246822519;
  PRIME32_3 = 3266489917;
  PRIME32_4 = 668265263;
  PRIME32_5 = 374761393;

  function Rol13(const Value: UInt32): UInt32; inline;
  begin
    Result := RolDWord(Value, 13);
  end;

var
  c1, c2, c3, c4: UInt32;
  PLimit, PEnd: PByte;
begin
  PEnd := P + Len;
  if Len >= 16 then
  begin
    PLimit := PEnd - 16;
    c3 := Seed;
    c2 := c3 + PRIME32_2;
    c1 := c2 + PRIME32_1;
    c4 := c3 - PRIME32_1;
    repeat
      c1 := PRIME32_1 * Rol13(c1 + PRIME32_2 * PUInt32(P)^);
      c2 := PRIME32_1 * Rol13(c2 + PRIME32_2 * PUInt32(P + 4)^);
      c3 := PRIME32_1 * Rol13(c3 + PRIME32_2 * PUInt32(P + 8)^);
      c4 := PRIME32_1 * Rol13(c4 + PRIME32_2 * PUInt32(P + 12)^);
      Inc(P, 16);
    until not (P <= PLimit);
    Result := RolDWord(c1, 1) + RolDWord(c2, 7) + RolDWord(c3, 12) + RolDWord(c4, 18);
  end else
    Result := Seed + PRIME32_5;

  Inc(Result, Len);
  while P + 4 <= PEnd do
  begin
    Inc(Result, PUInt32(P)^ * PRIME32_3);
    Result := RolDWord(Result, 17) * PRIME32_4;
    Inc(P, 4);
  end;
  while P < PEnd do
  begin
    Inc(Result, PByte(P)^ * PRIME32_5);
    Result := RolDWord(Result, 11) * PRIME32_1;
    Inc(P);
  end;
  Result := Result xor (Result shr 15); // inlined xxHash32Mixup()
  Result := Result * PRIME32_2;
  Result := Result xor (Result shr 13);
  Result := Result * PRIME32_3;
  Result := Result xor (Result shr 16);
end;

end.
