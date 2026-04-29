unit xpr.Import.System;
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
{
  Language exports from Free Pascal.
}
{$I header.inc}

interface

uses
  SysUtils, DateUtils, classes, serial, sockets, StrUtils, xpr.Types, xpr.CompilerContext;

procedure ImportExternalMethods(ctx: TCompilerContext);
procedure ImportSystemModules(ctx: TCompilerContext);
procedure ImportPascalCompatModules(ctx: TCompilerContext);

implementation

uses 
  xpr.Tree, xpr.Utils, xpr.Tokenizer, Math,
  xpr.Vartypes,
  xpr.Parser,
  xpr.ffi,
  ffi
  {$IFDEF UNIX},BaseUnix{$ENDIF};

const
  SystemDocPos:TDocPos = (Document:'__system__'; Line:0; Column:0);

{$I xpr.inc.import.system.inc}

procedure ImportExternalMethods(ctx: TCompilerContext);
var
  tCS, tSizeInt, tInt, tInt32, tInt64, tFloat32, tFloat64, tFloat,
  tString, tUString, tChar, tPointer, tInt8,
  tUInt8, tUInt32, tUInt64: XType;
begin
  // Cache common types to make definitions cleaner
  tSizeInt := ctx.GetType('Int');
  tInt     := ctx.GetType('Int64');
  tInt64   := ctx.GetType('Int64');
  tInt32   := ctx.GetType('Int32');
  tFloat   := ctx.GetType('Double');
  tFloat32 := ctx.GetType('Single');
  tFloat64 := ctx.GetType('Double');
  tString  := ctx.GetType('String');
  tUString := ctx.GetType('UnicodeString');
  tChar    := ctx.GetType('Char');
  tPointer := ctx.GetType('Pointer');
  tInt8    := ctx.GetType('Int8');
  tUInt8   := ctx.GetType('UInt8');
  tUInt32  := ctx.GetType('UInt32');
  tUInt64  := ctx.GetType('UInt64');

  ctx.ParseNativeDecls(
    'type TMemoryManager = record' + LineEnding +
    '  NeedLock: Bool'             + LineEnding +
    '  Getmem: Pointer'            + LineEnding +
    '  Freemem: Pointer'           + LineEnding +
    '  FreememSize: Pointer'       + LineEnding +
    '  AllocMem: Pointer'          + LineEnding +
    '  ReAllocMem: Pointer'        + LineEnding +
    '  MemSize: Pointer'           + LineEnding +
    '  InitThread: Pointer'        + LineEnding +
    '  DoneThread: Pointer'        + LineEnding +
    '  RelocateHeap: Pointer'      + LineEnding +
    '  GetHeapStatus: Pointer'     + LineEnding +
    '  GetFPCHeapStatus: Pointer'  + LineEnding, []);


  // --- native internals ---
  ctx.ParseNativeDecls(
    'func __atomic_inc_ref(p: pointer)'                          + LineEnding +
    'func __atomic_dec_ref(p: pointer)'                          + LineEnding +
    'func __astring_setlength(ref s:ansistring; sz:sizeint)'     + LineEnding +
    'func __ustring_setlength(ref s:unicodestring; sz:sizeint)'  + LineEnding +
    'func __astring_refcount(ref p: ansistring): sizeint'        + LineEnding +
    'func __ustring_refcount(ref p: unicodestring): sizeint'     + LineEnding,
    [Bind('__atomic_inc_ref',     @_AtomicIncRef),
     Bind('__atomic_dec_ref',     @_AtomicDecRef),
     Bind('__astring_setlength',  @_AnsiSetLength),
     Bind('__ustring_setlength',  @_UnicodeSetLength),
     Bind('__astring_refcount',   @_Ansirefcount),
     Bind('__ustring_refcount',   @_UnicodeRefcount)]
  );

  ctx.ParseNativeDecls(
    'func FreeMem(ref p: pointer)'                               + LineEnding +
    'func ReAllocMem(ref p: pointer; ref sz:sizeint): pointer'   + LineEnding +
    'func AllocMem(sz:sizeint): pointer'                         + LineEnding +
    'func GetMem(sz:sizeint): pointer'                           + LineEnding +
    'func FillByte(p:pointer; sz:sizeint; v:int8)'               + LineEnding +
    'func Memmove(src, dst:pointer; sz:sizeint)'                 + LineEnding +
    'func GetMemoryManager(): TMemoryManager'                    + LineEnding,
    [Bind('FreeMem',    @_FreeMem),
     Bind('ReAllocMem', @_ReallocMem),
     Bind('AllocMem',   @_AllocMem),
     Bind('GetMem',     @_GetMem),
     Bind('FillByte',   @_FillByte),
     Bind('Memmove',    @_Move),
     Bind('GetMemoryManager', @_GetMemoryManager)]
  );

  // pascal compatible wrapper using generics
  ctx.ParseNativeDecls(
    'func Move<T,U>(ref left:T; ref right:U; Size:SizeInt)' + LineEnding +
    '  MemMove(Addr(left), Addr(right), size);'             + LineEnding,[]
  );



  // --- Time & Date --------------
  ctx.ParseNativeDecls(
    'func GetTickCount(): Int64' + LineEnding +
    'func MarkTime(): Double' + LineEnding +
    'func UnixTime(): Int64' + LineEnding +
    'func Now(): Double' + LineEnding +
    'func Date(): Double' + LineEnding +
    'func Time(): Double' + LineEnding +
    'func EncodeDate(y, m, d: Int64): Double' + LineEnding +
    'func EncodeTime(h, m, s, ms: Int64): Double' + LineEnding +
    'func DecodeDate(d: Double; ref y, m, day: Int64)' + LineEnding +
    'func DecodeTime(d: Double; ref h, m, s, ms: Int64)' + LineEnding +
    'func DateTimeToUnix(d: Double): Int64' + LineEnding +
    'func UnixToDateTime(u: Int64): Double' + LineEnding +
    'func FormatDateTime(fmt: String; d: Double): String' + LineEnding +
    'func Sleep(t: Int64)' + LineEnding,

    [Bind('GetTickCount',   @_GetTickCount),
     Bind('MarkTime',       @_MarkTime),
     Bind('UnixTime',       @_UnixTime),
     Bind('Now',            @_Now),
     Bind('Date',           @_Date),
     Bind('Time',           @_Time),
     Bind('EncodeDate',     @_EncodeDate),
     Bind('EncodeTime',     @_EncodeTime),
     Bind('DecodeDate',     @_DecodeDate),
     Bind('DecodeTime',     @_DecodeTime),
     Bind('DateTimeToUnix', @_DateTimeToUnix),
     Bind('UnixToDateTime', @_UnixToDateTime),
     Bind('FormatDateTime', @_FormatDateTime),
     Bind('Sleep',          @_Sleep)]
  );

  // --- Random ------------------
  ctx.ParseNativeDecls(
    'var RandSeed: Int32' + LineEnding +
    'func RandInt(min, max: Int64): Int64' + LineEnding +
    'func Random(min, max: Int64): Int64' + LineEnding +
    'func Random(): Double' + LineEnding,

    [Bind('RandSeed',  @RandSeed),
     Bind('RandInt',   @_RandInt),
     BindOverload('Random', @_RandInt,     [tInt64,   tInt64]),
     BindOverload('Random', @_RandomFloat, [])]
  );


  // --- Math -------------------
  ctx.ParseNativeDecls(
    'const PI   : Double = 3.14159265358979' + LineEnding +
    'const Euler: Double = 2.71828182845904' + LineEnding +

    '@inline' + LineEnding +
    'func Inc<T: numeric>(ref x: T) x += 1' + LineEnding +

    '@inline' + LineEnding +
    'func Dec<T: numeric>(ref x: T) x -= 1' + LineEnding +

    '@inline' + LineEnding +
    'func Abs<T: numeric>(x: T): T => if(x >= 0) x else -x' + LineEnding +

    '@inline' + LineEnding +
    'func Min<T: numeric>(x,y: T): T => if(x <= y) x else y' + LineEnding +

    '@inline' + LineEnding +
    'func Max<T: numeric>(x,y: T): T => if(x >= y) x else y' + LineEnding +

    'func Sin(v: Double): Double'       + LineEnding +
    'func Cos(v: Double): Double'       + LineEnding +
    'func Tan(v: Double): Double'       + LineEnding +
    'func ArcTan(v: Double): Double'    + LineEnding +
    'func ArcTan2(y,x: Double): Double' + LineEnding +
    'func Sqrt(v: Double): Double'      + LineEnding +
    'func Ln(v: Double): Double'        + LineEnding +
    'func Exp(v: Double): Double'       + LineEnding +
    'func Frac(v: Double): Double'      + LineEnding +
    'func Power(b,e: Double): Double'   + LineEnding +
    'func Trunc(v: Double): Int64'      + LineEnding +
    'func Floor(v: Double): Int64'      + LineEnding +
    'func Ceil(v: Double): Int64'       + LineEnding +
    'func Round(v: Double): Int64'            + LineEnding +
    'func Round(v: Double; p: Int64): Double' + LineEnding,

    [Bind('Sin',        @_Sin),
     Bind('Cos',        @_Cos),
     Bind('Tan',        @_Tan),
     Bind('ArcTan',     @_ArcTan),
     Bind('ArcTan2',    @_ArcTan2),
     Bind('Sqrt',       @_Sqrt),
     Bind('Ln',         @_Ln),
     Bind('Exp',        @_Exp),
     Bind('Frac',       @_Frac),
     Bind('Power',      @_Power),
     Bind('Trunc',      @_Trunc),
     Bind('Floor',      @_Floor),
     Bind('Ceil',       @_Ceil),

     BindOverload('Round', @_Round,    [tFloat64]),
     BindOverload('Round', @_RoundTo,  [tFloat64, tInt64])]
     //BindOverload('Abs',   @_AbsInt,   [tInt64]),
     //BindOverload('Abs',   @_AbsFloat, [tFloat64]),
     //BindOverload('Min',   @_MinInt,   [tInt64, tInt64]),
     //BindOverload('Max',   @_MaxInt,   [tInt64, tInt64]),
  );


  // --- File & Directory -------------------------
  ctx.ParseNativeDecls(
    'func FileExists(path: String): Bool' + LineEnding +
    'func DirectoryExists(path: String): Bool' + LineEnding +
    'func DeleteFile(path: String): Bool' + LineEnding +
    'func ExtractFilePath(path: String): String' + LineEnding +
    'func ExtractFileName(path: String): String' + LineEnding +
    'func ExtractFileExt(path: String): String' + LineEnding +
    'func GetFileSize(path: String): Int64' + LineEnding +
    'func ReadFile(path: String): String' + LineEnding +
    'func WriteFile(path, content: String): Bool' + LineEnding +
    'func AppendFile(path, content: String): Bool' + LineEnding,
    [Bind('FileExists',      @_FileExists),
     Bind('DirectoryExists', @_DirectoryExists),
     Bind('DeleteFile',      @_DeleteFile),
     Bind('ExtractFilePath', @_ExtractFilePath),
     Bind('ExtractFileName', @_ExtractFileName),
     Bind('ExtractFileExt',  @_ExtractFileExt),
     Bind('GetFileSize',     @_GetFileSize),
     Bind('ReadFile',        @_ReadFile),
     Bind('WriteFile',       @_WriteFile),
     Bind('AppendFile',      @_AppendFile)]
  );

  // --- String & Type Conversion ---
  ctx.AddExternalFunc(@_IntToStr,   'IntToStr',   [tInt],   [pbCopy], tString);
  ctx.AddExternalFunc(@_FloatToStr, 'FloatToStr', [tFloat], [pbCopy], tString);
  ctx.AddExternalFunc(@_PtrToStr,   'PtrToStr',   [tPointer], [pbCopy], tString);
  ctx.AddExternalFunc(@_StrToInt,   'StrToInt',   [tString], [pbCopy], tInt);
  ctx.AddExternalFunc(@_StrToFloat, 'StrToFloat', [tString], [pbCopy], tFloat64);
  ctx.AddExternalFunc(@_StrToFloatDef,'StrToFloat',[tString, tFloat64], [pbCopy, pbCopy], tFloat64);
  ctx.AddExternalFunc(@_Ord,        'Ord',        [tChar], [pbCopy], tInt);
  ctx.AddExternalFunc(@_Chr,        'Chr',        [tInt], [pbCopy], tChar);
  ctx.AddExternalFunc(@_Lowercase,  'Lowercase',  [tString], [pbCopy], tString);
  ctx.AddExternalFunc(@_Uppercase,  'Uppercase',  [tString], [pbCopy], tString);
  ctx.AddExternalFunc(@_Capitalize, 'Capitalize', [tString], [pbCopy], tString);

  // Formating
  ctx.AddExternalFunc(@_FormatInt,   'FormatInt',     [tString, tInt], [pbCopy, pbCopy], tString);
  ctx.AddExternalFunc(@_FormatFloat, 'FormatFloat',   [tString, tFloat], [pbCopy, pbCopy], tString);
  ctx.AddExternalFunc(@_FormatStr,   'FormatString',  [tString, tString], [pbCopy, pbCopy], tString);



  // --- Threading ---
  ctx.AddExternalFunc(@_ThreadJoin,    'ThreadJoin',    [tInt], [pbCopy], tUInt32);
  ctx.AddExternalFunc(@_ThreadSuspend, 'ThreadSuspend', [tInt], [pbCopy], tUInt32);
  ctx.AddExternalFunc(@_ThreadResume,  'ThreadResume',  [tInt], [pbCopy], tUInt32);
  ctx.AddExternalFunc(@_ThreadClose,   'ThreadClose',   [tInt], [pbCopy], tUInt32);
  ctx.AddExternalFunc(@_ThreadID,      'GetThreadID',   [], [], tUInt32);

  TCS := XType_Pointer.Create(nil);  // opaque pointer
  TCS.Name := 'TCriticalSection';
  ctx.AddType('TCriticalSection', TCS, True);

  ctx.AddExternalMethod(@_CSInit,    'Create',  TCS, [], [], TCS);
  ctx.AddExternalMethod(@_CSDestroy, 'Destroy', TCS, [], [], nil);
  ctx.AddExternalMethod(@_CSLock,    'Lock',    TCS, [], [], nil);
  ctx.AddExternalMethod(@_CSUnlock,  'Unlock',  TCS, [], [], nil);


  // --- FFI / Dynamic Library ---
  ctx.AddExternalFunc(@_LoadLib,        'LoadLib',        [tString],         [pbCopy],         tInt);
  ctx.AddExternalFunc(@_FreeLib,        'FreeLib',        [tInt],            [pbCopy],         nil);
  ctx.AddExternalFunc(@_GetProc,        'GetProc',        [tInt, tString],   [pbCopy, pbCopy], tInt);
  ctx.AddExternalFunc(@_FreeCallback,   'free_callback',  [tInt],            [pbCopy],         nil);


  // --- Sockets & Serial -------------------
  ctx.ParseNativeDecls(
    // --- Types ---
    'type TSerialParity = enum(NoneParity, OddParity, EvenParity, MarkParity, SpaceParity)' + LineEnding +
    // --- Socket constants ---
    'const AF_INET:  Int32 = 2'   + LineEnding +
    'const AF_INET6: Int32 = 10'  + LineEnding +
    'const AF_UNIX:  Int32 = 1'   + LineEnding +

    'const SOCK_STREAM: Int32 = 1'   + LineEnding +
    'const SOCK_DGRAM:  Int32 = 2'   + LineEnding +
    'const SOCK_RAW:    Int32 = 3'   + LineEnding +

    'const IPPROTO_TCP: Int32 = 6'   + LineEnding +
    'const IPPROTO_UDP: Int32 = 17'  + LineEnding +

    'const SOL_SOCKET:  Int32 = 1'  + LineEnding +

    'const SO_REUSEADDR:Int32 = 2'  + LineEnding +
    'const SO_KEEPALIVE:Int32 = 9'  + LineEnding +
    'const SO_RCVBUF:   Int32 = 8'  + LineEnding +
    'const SO_SNDBUF:   Int32 = 7'  + LineEnding +
    'const SO_RCVTIMEO: Int32 = 20' + LineEnding +
    'const SO_SNDTIMEO: Int32 = 21' + LineEnding +

    'const SHUT_RD:   Int32 = 0' + LineEnding +
    'const SHUT_WR:   Int32 = 1' + LineEnding +
    'const SHUT_RDWR: Int32 = 2' + LineEnding +

    'const INADDR_ANY:       Int32 = 0'         + LineEnding +
    'const INADDR_LOOPBACK:  Int32 = $7F000001' + LineEnding +
    'const INADDR_BROADCAST: Int32 = $FFFFFFFF' + LineEnding +

    'const MSG_PEEK:     Int32 = 2'   + LineEnding +
    'const MSG_WAITALL:  Int32 = 256' + LineEnding +
    'const MSG_DONTWAIT: Int32 = 64'  + LineEnding +

    // --- Sockets ---
    'func fpSocket(domain, typ, protocol: Int32): Int32' + LineEnding +
    'func fpConnect(sockfd: Int32; addr: Pointer; addrlen: Int32): Int32' + LineEnding +
    'func fpBind(sockfd: Int32; addr: Pointer; addrlen: Int32): Int32' + LineEnding +
    'func fpListen(sockfd: Int32; backlog: Int32): Int32' + LineEnding +
    'func fpAccept(sockfd: Int32; addr: Pointer; ref addrlen: Int32): Int32' + LineEnding +
    'func fpRecv(sockfd: Int32; buf: Pointer; len, flags: Int32): Int32' + LineEnding +
    'func fpRecvFrom(sockfd: Int32; buf: Pointer; len, flags: Int32; addr: Pointer; ref addrlen: Int32): Int32' + LineEnding +
    'func fpSend(sockfd: Int32; buf: Pointer; len, flags: Int32): Int32' + LineEnding +
    'func fpSendTo(sockfd: Int32; buf: Pointer; len, flags: Int32; addr: Pointer; addrlen: Int32): Int32' + LineEnding +
    'func fpClose(sockfd: Int32): Int32' + LineEnding +
    'func fpSetSockOpt(sockfd, level, optname: Int32; optval: Pointer; optlen: Int32): Int32' + LineEnding +
    'func fpGetSockOpt(sockfd, level, optname: Int32; optval: Pointer; ref optlen: Int32): Int32' + LineEnding +
    'func fpSelect(nfds: Int32; readfds, writefds, exceptfds: Pointer; timeout: Pointer): Int32' + LineEnding +
    'func fpShutdown(sockfd, how: Int32): Int32' + LineEnding +

    // --- Byte order ---
    'func htons(v: UInt16): UInt16' + LineEnding +
    'func htonl(v: UInt32): UInt32' + LineEnding +
    'func ntohs(v: UInt16): UInt16' + LineEnding +
    'func ntohl(v: UInt32): UInt32' + LineEnding +

    // --- Serial ---
    'func SerOpen(device: string): Int32' + LineEnding +
    'func SerClose(handle: Int32)'        + LineEnding +
    'func SerSetParams(handle: Int32; baud, bits: Int32; parity: TSerialParity; stopBits: Int32; flags: Int32)' + LineEnding +
    'func SerRead(handle: Int32; buffer: Pointer; count: Int32): Int32' + LineEnding +
    'func SerWrite(handle: Int32; buffer: Pointer; count: Int32): Int32' + LineEnding +
    'func SerSync(handle: Int32)' + LineEnding +
    'func SerFlushOutput(handle: Int32)' + LineEnding,

    [Bind('fpSocket',        @_fpSocket),
     Bind('fpConnect',       @_fpConnect),
     Bind('fpBind',          @_fpBind),
     Bind('fpListen',        @_fpListen),
     Bind('fpAccept',        @_fpAccept),
     Bind('fpRecv',          @_fpRecv),
     Bind('fpRecvFrom',      @_fpRecvFrom),
     Bind('fpSend',          @_fpSend),
     Bind('fpSendTo',        @_fpSendTo),
     Bind('fpClose',         @_fpClose),
     Bind('fpSetSockOpt',    @_fpSetSockOpt),
     Bind('fpGetSockOpt',    @_fpGetSockOpt),
     Bind('fpSelect',        @_fpSelect),
     Bind('fpShutdown',      @_fpShutdown),

     Bind('htons',           @_htons),
     Bind('htonl',           @_htonl),
     Bind('ntohs',           @_ntohs),
     Bind('ntohl',           @_ntohl),

     Bind('SerOpen',         @_SerOpen),
     Bind('SerClose',        @_SerClose),
     Bind('SerSetParams',    @_SerSetParams),
     Bind('SerRead',         @_SerRead),
     Bind('SerWrite',        @_SerWrite),
     Bind('SerSync',         @_SerSync),
     Bind('SerFlushOutput',  @_SerFlushOutput)]
  );
end;


procedure ImportSystemModules(ctx: TCompilerContext);
var
  DocPos: TDocPos;
  oldFileContents: string;
begin
  DocPos := SystemDocPos;

  oldFileContents := ctx.MainFileContents;

  // Critical, everything refcounted needs this
  // This is imported before exception handling exists, it will cause
  // internal kaboom on failure.
  with XTree_ImportUnit.Create('system/internals.xpr', '__internal', ctx, DocPos) do 
  try
    Compile(NullResVar, []);
  finally
    Free();
  end;  
  
  // Everything with exception handling needs this!
  // This uses refcounted classes, so it needs the above!
  with XTree_ImportUnit.Create('system/exception.xpr', '', ctx, DocPos) do
  try
    Compile(NullResVar, []);
  finally
    Free();
  end;

  ctx.MainFileContents:=oldFileContents;
end;

procedure ImportPascalCompatModules(ctx: TCompilerContext);
var
  DocPos: TDocPos;
  oldFileContents: string;
begin
  DocPos := SystemDocPos;

  oldFileContents := ctx.MainFileContents;

  // priority 1, everything refcounted needs this
  // This is imported before exception handling exists, it will cause
  // internal kaboom on failure.
  with XTree_ImportUnit.Create('system/pascal.xpr', '', ctx, DocPos) do
  try
    Compile(NullResVar, []);
  finally
    Free();
  end;

  ctx.MainFileContents:=oldFileContents;
end;

end.
