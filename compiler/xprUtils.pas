unit xprUtils;
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

procedure WriteFancy(s: string);
function LoadFileContents(fileName: string): string;
function MarkTime(): Double; inline;

function NextPow2(n: Int32): Int32; inline;

function StrToFloatDot(const S: string): Extended; inline;
function FloatToStrDot(f: Extended): string; inline;

function StringContains(arr: array of string; v: string): Boolean;
function Slice(const S: string; Start: Int64=High(Int32); Stop: Int64=High(Int32); Step: Int32=1): string;
function StrPosEx(const SubStr, Text: string): UTIntArray;
function StrExplode(const Text, Sep: string): UTStringArray;
function StartsWith(const Prefix, Text: string): Boolean;

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


procedure WriteFancy(s:string);
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
    Result := Length(s)+1;
    attr.color := '';
    attr.id    := 7;
    for i:=StartAt to Length(s) do
    begin
      for j:=0 to High(COLOR_ATTRIBUTES) do
        if MatchStrAt(COLOR_ATTRIBUTES[j].color, s, i) then
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
  s += #0;

  newpos := 1;
  repeat
    oldpos := newpos;
    newpos := FindAttr(oldpos, attr);
    Write(Copy(s, oldpos, newpos-oldpos)); //copy from here to the code
    newpos += Length(attr.color);
    {$IFDEF WINDOWS}
    SetConsoleTextAttribute(hConsole, attr.id);
    {$ENDIF}
  until newpos >= Length(s);
  {$IFDEF WINDOWS}
  SetConsoleTextAttribute(hConsole, old);
  {$ENDIF}
  WriteLn('');
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
  {$IFDEF WINDOWS}
  count, frequency:Int64;
  {$ELSE}
  TV:TTimeVal;
  TZ:PTimeZone;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  QueryPerformanceFrequency(frequency);
  QueryPerformanceCounter(count);
  Result := count / frequency * 1000;
  {$ELSE}
  TZ := nil;
  fpGetTimeOfDay(@TV, TZ);
  count := Int64(TV.tv_sec) * 1000000 + Int64(TV.tv_usec);
  Result := count / 1000;
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

function Slice(const S:String; Start:Int64=High(Int32); Stop:Int64=High(Int32); Step:Int32=1): String;
var
  P,R:PChar;
  l:Int32;
  H:PtrUInt;
  function Modulo(X,Y:Int64):  Int64; begin Result := X - Floor(X / Y) * Y; end;
begin
  if (Start = High(Int32)) then
    if Step < 0 then Start := -1
    else Start := 1;
  if (Stop = High(Int32)) then
    if Step > 0 then Stop := -1
    else Stop := 1;

  h := Length(S);
  case (Step > 0) of
    True:  if (Stop > h) then Stop := h;
    False: if (Start > h) then Start := h;
  end;
  Start := Modulo(start, h+1);
  Stop  := Modulo(stop, h+1);

  if (Start > Stop) and (Step > 0) then
    Exit('');

  if (not InRange(Start,1,Length(S))) or (not InRange(Stop,1,Length(S))) then
    Exit('');

  SetLength(Result, ((Stop-Start) div step)+1);
  P := @S[start];
  R := @Result[1];
  L := PtrUInt(@Result[Length(Result)]);
  while PtrUInt(R) <= L do
  begin
    R^ := P^;
    Inc(R);
    Inc(P, step);
  end;
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


end.
