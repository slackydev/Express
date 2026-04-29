unit ffi;
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
  The design of these binding are inspired by ffi-bindings within the Lape
  project offered by Merlijn Wajer and Niels AD.
  https://github.com/nielsAD/lape/tree/master/extensions/ffi


  Bindings to libffi for the Express language runtime.
  Supports both Dynamic and Static loading via conditional defines.

  Provides the type definitions and function pointers required by xpr.ffi:
    - TFFIType / TFFICif / TFFIClosure records mirroring the libffi C ABI.
    - Pre-initialised ffi_type_* globals for all primitive types.
    - Dynamic library loading on both Windows and Unix.
    - FFILoaded() guard used throughout xpr.ffi before any ffi_* call.
    - ABIToStr / StrToABI helpers for calling-convention annotation parsing.

  Platform coverage:
    - x86      (CPUI386)    Windows and Unix
    - x86-64   (CPUX86_64)  Win64 and Unix64
    - ARM32    (CPUARM)     cdecl / VFP
    - ARM64    (CPUAARCH64) SysV
}

// Uncomment or compile with -dStaticFFI to link at compile time
{.$DEFINE StaticFFI}

{$IFNDEF StaticFFI}
  {$DEFINE DynamicFFI}
{$ENDIF}

{$I header.inc}
{$hints off}
{$packrecords c}

interface

{$IFDEF StaticFFI}
  {$LINKLIB libffi}
  {$IFDEF WINDOWS}
    {$LINKLIB libgcc}
    {$LINKLIB libmsvcrt}
    {$LINKLIB libkernel32}
  {$ENDIF}
{$ENDIF}

uses
  SysUtils,
  {$IFDEF WINDOWS}Windows{$ELSE}dynlibs{$ENDIF};

// =============================================================================
// Platform detection helpers
// =============================================================================

{$IF DEFINED(CPUI386)}
  {$DEFINE TARGET_X86}
{$ELSEIF DEFINED(CPUX86_64)}
  {$DEFINE TARGET_X64}
{$ELSEIF DEFINED(CPUARM)}
  {$DEFINE TARGET_ARM32}
{$ELSEIF DEFINED(CPUAARCH64)}
  {$DEFINE TARGET_ARM64}
{$IFEND}

// =============================================================================
// Shared-library name
// =============================================================================

const
  {$IFDEF WINDOWS}
  FFI_LIBNAME = 'libffi-8.dll';
  {$ELSE}
  FFI_LIBNAME = 'libffi.so.8';
  {$ENDIF}


// =============================================================================
// Status codes  (ffi_status)
// =============================================================================

type
  TFFIStatus = (
    FFI_OK          = 0,
    FFI_BAD_TYPEDEF = 1,
    FFI_BAD_ABI     = 2
  );


// =============================================================================
// ABI identifiers  (ffi_abi)
// =============================================================================

type
  TFFIABI = (
    FFI_UNKNOWN_ABI = 0,

    {$IFDEF TARGET_X86}
      {$IFDEF UNIX}
      FFI_SYSV     = 1,
      FFI_THISCALL = 3,
      FFI_FASTCALL = 4,
      FFI_STDCALL  = 5,
      FFI_PASCAL   = 6,
      FFI_REGISTER = 7,
      FFI_MS_CDECL = 8,
      {$ENDIF}
      {$IFDEF WINDOWS}
      FFI_SYSV     = 1,
      FFI_STDCALL  = 2,
      FFI_THISCALL = 3,
      FFI_FASTCALL = 4,
      FFI_MS_CDECL = 5,
      FFI_PASCAL   = 6,
      FFI_REGISTER = 7,
      {$ENDIF}
    {$ENDIF}

    {$IFDEF TARGET_X64}
      {$IFDEF UNIX}
      FFI_UNIX64   = 2,
      {$ENDIF}
      {$IFDEF WINDOWS}
      FFI_WIN64    = 1,
      {$ENDIF}
    {$ENDIF}

    {$IFDEF TARGET_ARM32}
      FFI_SYSV     = 1,
      FFI_VFP      = 2,
    {$ENDIF}

    {$IFDEF TARGET_ARM64}
      FFI_SYSV     = 1,
    {$ENDIF}

    FFI_LAST_ABI
  );

const
  // The default ABI for the current platform/OS.
  // Default x86 is register in a pascal environment, not stdcall or cdecl
  FFI_DEFAULT_ABI =
    {$IFDEF TARGET_X86}   FFI_REGISTER {$ENDIF}
    {$IFDEF TARGET_X64}
      {$IFDEF WINDOWS}    FFI_WIN64
      {$ELSE}             FFI_UNIX64
      {$ENDIF}
    {$ENDIF}
    {$IFDEF TARGET_ARM32}
      {$IFDEF FPUVFP}     FFI_VFP
      {$ELSE}             FFI_SYSV
      {$ENDIF}
    {$ENDIF}
    {$IFDEF TARGET_ARM64} FFI_SYSV {$ENDIF};

  // Alias recognised by xpr.ffi for the cdecl ABI on all platforms.
  {$IFDEF TARGET_X86}
  FFI_CDECL = FFI_SYSV;
  {$ENDIF}


// =============================================================================
// Type tags  (ffi_type._type field)
// =============================================================================

type
  TFFI_CTYPE = (
    FFI_CTYPE_VOID       = 0,
    FFI_CTYPE_INT        = 1,
    FFI_CTYPE_FLOAT      = 2,
    FFI_CTYPE_DOUBLE     = 3,
    FFI_CTYPE_LONGDOUBLE = 4,
    FFI_CTYPE_UINT8      = 5,
    FFI_CTYPE_SINT8      = 6,
    FFI_CTYPE_UINT16     = 7,
    FFI_CTYPE_SINT16     = 8,
    FFI_CTYPE_UINT32     = 9,
    FFI_CTYPE_SINT32     = 10,
    FFI_CTYPE_UINT64     = 11,
    FFI_CTYPE_SINT64     = 12,
    FFI_CTYPE_STRUCT     = 13,
    FFI_CTYPE_POINTER    = 14
  );


// =============================================================================
// Core type descriptor  (ffi_type)
// =============================================================================

type
  PFFIType = ^TFFIType;

  TFFITypeArray  = array[0..0] of PFFIType;
  PFFITypeArray  = ^TFFITypeArray;

  TFFIType = packed record
    size:      SizeUInt;      // byte size of the type (set by libffi)
    alignment: Word;          // natural alignment (set by libffi)
    _type:     Word;          // TFFI_CTYPE tag
    elements:  PFFITypeArray; // nil for primitives; non-nil for structs
  end;


// =============================================================================
// Call-interface descriptor  (ffi_cif)
// =============================================================================

type
  PFFICif = ^TFFICif;

  TFFICif = packed record
    abi:       TFFIABI;
    nargs:     LongWord;      // number of fixed arguments
    arg_types: PFFITypeArray; // pointer to array of PFFIType
    rtype:     PFFIType;      // return type descriptor
    bytes:     LongWord;      // total arg bytes (platform-internal)
    flags:     LongWord;      // platform-internal flags
  end;


// =============================================================================
// Closure descriptor  (ffi_closure)
// =============================================================================

const
  FFI_TRAMPOLINE_SIZE =
    {$IFDEF TARGET_X86}   10 {$ENDIF}
    {$IFDEF TARGET_X64}   24 {$ENDIF}
    {$IFDEF TARGET_ARM32} 12 {$ENDIF}
    {$IFDEF TARGET_ARM64} 24 {$ENDIF};

type
  PFFIClosure = ^TFFIClosure;

  TFFIClosure = packed record
    _opaque: array[0..63] of Byte;
  end;


// =============================================================================
// Pointer array types used by closure callbacks and ffi_call
// =============================================================================

type
  TPointerArray  = array[0..High(Integer) div SizeOf(Pointer) - 1] of Pointer;
  PPointerArray  = ^TPointerArray;


// =============================================================================
// Closure callback signature
// =============================================================================

type
  TClosureBindingFunction = procedure(
    var cif:      TFFICif;
        ret:      Pointer;
        args:     PPointerArray;
        userdata: Pointer); cdecl;


// =============================================================================
// Function-pointer types matching the libffi C API
// =============================================================================

type
  TFFIPrepCif = function(
    out cif:    TFFICif;
        abi:    TFFIABI;
        nargs:  LongWord;
        rtype:  PFFIType;
        atypes: PFFITypeArray): TFFIStatus; cdecl;

  TFFICall = procedure(
    var cif:    TFFICif;
        fn:     Pointer;
        rvalue: Pointer;
        avalue: PPointerArray); cdecl;

  TFFIClosureAlloc = function(
        size:   SizeUInt;
    var code:   Pointer): Pointer; cdecl;

  TFFIClosureFree = procedure(
        closure: Pointer); cdecl;

  TFFIPrepClosureLoc = function(
    out closure:   TFFIClosure;
    var cif:       TFFICif;
        fun:       TClosureBindingFunction;
        user_data: Pointer;
        codeloc:   Pointer): TFFIStatus; cdecl;


// =============================================================================
// Function pointers
// =============================================================================

var
  ffi_prep_cif:         TFFIPrepCif        = nil;
  ffi_call:             TFFICall           = nil;
  ffi_closure_alloc:    TFFIClosureAlloc   = nil;
  ffi_closure_free:     TFFIClosureFree    = nil;
  ffi_prep_closure_loc: TFFIPrepClosureLoc = nil;


// =============================================================================
// Pre-initialised type descriptors for all primitive types
// =============================================================================

var
{$IFDEF StaticFFI}
  ffi_type_void:       TFFIType; cvar; external;
  ffi_type_uint8:      TFFIType; cvar; external;
  ffi_type_sint8:      TFFIType; cvar; external;
  ffi_type_uint16:     TFFIType; cvar; external;
  ffi_type_sint16:     TFFIType; cvar; external;
  ffi_type_uint32:     TFFIType; cvar; external;
  ffi_type_sint32:     TFFIType; cvar; external;
  ffi_type_uint64:     TFFIType; cvar; external;
  ffi_type_sint64:     TFFIType; cvar; external;
  ffi_type_float:      TFFIType; cvar; external;
  ffi_type_double:     TFFIType; cvar; external;
  ffi_type_longdouble: TFFIType; cvar; external;
  ffi_type_pointer:    TFFIType; cvar; external;
{$ELSE}
  ffi_type_void:       TFFIType;
  ffi_type_uint8:      TFFIType;
  ffi_type_sint8:      TFFIType;
  ffi_type_uint16:     TFFIType;
  ffi_type_sint16:     TFFIType;
  ffi_type_uint32:     TFFIType;
  ffi_type_sint32:     TFFIType;
  ffi_type_uint64:     TFFIType;
  ffi_type_sint64:     TFFIType;
  ffi_type_float:      TFFIType;
  ffi_type_double:     TFFIType;
  ffi_type_longdouble: TFFIType;
  ffi_type_pointer:    TFFIType;
{$ENDIF}


// =============================================================================
// Public interface
// =============================================================================

{ Returns True when libffi was successfully loaded and all entry-points resolved. }
function FFILoaded(): Boolean;

{ Converts a TFFIABI value to its lowercase string name (e.g. FFI_UNIX64 -> 'unix64').
  Returns '' for unknown or internal sentinel values. }
function ABIToStr(ABI: TFFIABI): string;

{ Parses a calling-convention string (e.g. 'stdcall', 'cdecl', 'unix64') into
  the matching TFFIABI.  Accepts both the bare name and the 'ffi_'-prefixed form.
  Returns FFI_UNKNOWN_ABI when the string is not recognised. }
function StrToABI(const Name: string): TFFIABI;

{ Attempt to load libffi from LibPath + LibName.
  If LibPath is empty the OS library search path is used.
  Returns True on success. }
function LoadFFI(const LibPath: string = ''; const LibName: string = FFI_LIBNAME): Boolean;

{ Release the loaded libffi handle and nil all function pointers. }
procedure UnloadFFI();


// =============================================================================
implementation
// =============================================================================

// ---------------------------------------------------------------------------
// Static externals
// ---------------------------------------------------------------------------

{$IFDEF StaticFFI}
  function _ffi_prep_cif(out cif: TFFICif; abi: TFFIABI; nargs: LongWord; rtype: PFFIType; atypes: PFFITypeArray): TFFIStatus; cdecl; external name 'ffi_prep_cif';
  procedure _ffi_call(var cif: TFFICif; fn: Pointer; rvalue: Pointer; avalue: PPointerArray); cdecl; external name 'ffi_call';
  function _ffi_closure_alloc(size: SizeUInt; var code: Pointer): Pointer; cdecl; external name 'ffi_closure_alloc';
  procedure _ffi_closure_free(closure: Pointer); cdecl; external name 'ffi_closure_free';
  function _ffi_prep_closure_loc(out closure: TFFIClosure; var cif: TFFICif; fun: TClosureBindingFunction; user_data: Pointer; codeloc: Pointer): TFFIStatus; cdecl; external name 'ffi_prep_closure_loc';
{$ENDIF}


// ---------------------------------------------------------------------------
// Library handle (Dynamic Only)
// ---------------------------------------------------------------------------

{$IFDEF DynamicFFI}
var
  GLibHandle: TLibHandle = NilHandle;
{$ENDIF}


// ---------------------------------------------------------------------------
// Type descriptor initialisation (Dynamic Only)
// ---------------------------------------------------------------------------

{$IFDEF DynamicFFI}
{ Build a TFFIType descriptor for a primitive type. }
function MakeFFIType(ASize: SizeUInt; AAlign: Word; ATypeTag: TFFI_CTYPE): TFFIType;
begin
  Result.size      := ASize;
  Result.alignment := AAlign;
  Result._type     := Word(ATypeTag);
  Result.elements  := nil;
end;

{ Natural alignment for a value of the given byte size on the current platform. }
function NaturalAlign(ASize: SizeUInt): Word;
begin
  if ASize >= SizeOf(Pointer) then
    Result := Word(SizeOf(Pointer))
  else if ASize >= 4 then
    Result := 4
  else if ASize >= 2 then
    Result := 2
  else
    Result := 1;
end;

procedure InitFFITypes();
begin
  // void: size 1, align 1 (libffi convention for the return-void sentinel)
  ffi_type_void       := MakeFFIType(1,                1,                     FFI_CTYPE_VOID);
  ffi_type_uint8      := MakeFFIType(SizeOf(Byte),     1,                     FFI_CTYPE_UINT8);
  ffi_type_sint8      := MakeFFIType(SizeOf(ShortInt), 1,                     FFI_CTYPE_SINT8);
  ffi_type_uint16     := MakeFFIType(SizeOf(Word),     2,                     FFI_CTYPE_UINT16);
  ffi_type_sint16     := MakeFFIType(SizeOf(SmallInt), 2,                     FFI_CTYPE_SINT16);
  ffi_type_uint32     := MakeFFIType(SizeOf(LongWord), 4,                     FFI_CTYPE_UINT32);
  ffi_type_sint32     := MakeFFIType(SizeOf(LongInt),  4,                     FFI_CTYPE_SINT32);
  ffi_type_uint64     := MakeFFIType(SizeOf(QWord),    NaturalAlign(8),       FFI_CTYPE_UINT64);
  ffi_type_sint64     := MakeFFIType(SizeOf(Int64),    NaturalAlign(8),       FFI_CTYPE_SINT64);
  ffi_type_float      := MakeFFIType(SizeOf(Single),   4,                     FFI_CTYPE_FLOAT);
  ffi_type_double     := MakeFFIType(SizeOf(Double),   NaturalAlign(8),       FFI_CTYPE_DOUBLE);
  ffi_type_longdouble := MakeFFIType(SizeOf(Extended), NaturalAlign(SizeOf(Extended)), FFI_CTYPE_LONGDOUBLE);
  ffi_type_pointer    := MakeFFIType(SizeOf(Pointer),  SizeOf(Pointer),       FFI_CTYPE_POINTER);
end;
{$ENDIF}


// ---------------------------------------------------------------------------
// Loading and Unloading
// ---------------------------------------------------------------------------

function FFILoaded(): Boolean;
begin
{$IFDEF StaticFFI}
  Result := True;
{$ELSE}
  Result := GLibHandle <> NilHandle;
{$ENDIF}
end;

function LoadFFI(const LibPath: string = ''; const LibName: string = FFI_LIBNAME): Boolean;
{$IFDEF DynamicFFI}
var
  FullPath: string;
{$ENDIF}
begin
{$IFDEF StaticFFI}
  Result := True;
{$ELSE}
  Result := False;
  UnloadFFI();

  if LibPath <> '' then
    FullPath := IncludeTrailingPathDelimiter(LibPath) + LibName
  else
    FullPath := LibName;

  GLibHandle := LoadLibrary(PChar(FullPath));
  if GLibHandle = NilHandle then
    Exit;

  Pointer(ffi_prep_cif)         := GetProcAddress(GLibHandle, 'ffi_prep_cif');
  Pointer(ffi_call)             := GetProcAddress(GLibHandle, 'ffi_call');
  Pointer(ffi_closure_alloc)    := GetProcAddress(GLibHandle, 'ffi_closure_alloc');
  Pointer(ffi_closure_free)     := GetProcAddress(GLibHandle, 'ffi_closure_free');
  Pointer(ffi_prep_closure_loc) := GetProcAddress(GLibHandle, 'ffi_prep_closure_loc');

  // All five entry-points are mandatory; bail if any is absent.
  if (ffi_prep_cif         = nil) or
     (ffi_call             = nil) or
     (ffi_closure_alloc    = nil) or
     (ffi_closure_free     = nil) or
     (ffi_prep_closure_loc = nil) then
  begin
    UnloadFFI();
    Exit;
  end;

  Result := True;
{$ENDIF}
end;

procedure UnloadFFI();
begin
{$IFDEF DynamicFFI}
  if GLibHandle = NilHandle then
    Exit;

  {$IFDEF WINDOWS}
  FreeLibrary(GLibHandle);
  {$ELSE}
  UnloadLibrary(GLibHandle);
  {$ENDIF}

  GLibHandle           := NilHandle;
  ffi_prep_cif         := nil;
  ffi_call             := nil;
  ffi_closure_alloc    := nil;
  ffi_closure_free     := nil;
  ffi_prep_closure_loc := nil;
{$ENDIF}
end;


// ---------------------------------------------------------------------------
// ABI name helpers
// ---------------------------------------------------------------------------

function ABIToStr(ABI: TFFIABI): string;
begin
  case ABI of
    FFI_UNKNOWN_ABI: Result := '';
    FFI_LAST_ABI:    Result := '';
    {$IFDEF TARGET_X86}
    FFI_SYSV:        Result := 'cdecl';
    FFI_STDCALL:     Result := 'stdcall';
    FFI_THISCALL:    Result := 'thiscall';
    FFI_FASTCALL:    Result := 'fastcall';
    FFI_PASCAL:      Result := 'pascal';
    FFI_REGISTER:    Result := 'register';
    FFI_MS_CDECL:    Result := 'ms_cdecl';
    {$ENDIF}
    {$IFDEF TARGET_X64}
      {$IFDEF WINDOWS}
      FFI_WIN64:     Result := 'win64';
      {$ELSE}
      FFI_UNIX64:    Result := 'unix64';
      {$ENDIF}
    {$ENDIF}
    {$IFDEF TARGET_ARM32}
    FFI_SYSV:        Result := 'cdecl';
    FFI_VFP:         Result := 'vfp';
    {$ENDIF}
    {$IFDEF TARGET_ARM64}
    FFI_SYSV:        Result := 'sysv';
    {$ENDIF}
  else
    Result := '';
  end;
end;

function StrToABI(const Name: string): TFFIABI;
var
  Lower: string;
begin
  Lower := LowerCase(Name);

  // Strip the optional 'ffi_' prefix if given.
  if (Length(Lower) > 4) and (Copy(Lower, 1, 4) = 'ffi_') then
    Delete(Lower, 1, 4);

  {$IFDEF TARGET_X86}
  if (Lower = 'cdecl')    or (Lower = 'sysv')    then Exit(FFI_SYSV);
  if Lower = 'stdcall'                           then Exit(FFI_STDCALL);
  if Lower = 'thiscall'                          then Exit(FFI_THISCALL);
  if Lower = 'fastcall'                          then Exit(FFI_FASTCALL);
  if Lower = 'pascal'                            then Exit(FFI_PASCAL);
  if Lower = 'register'                          then Exit(FFI_REGISTER);
  if (Lower = 'ms_cdecl') or (Lower = 'mscdecl') then Exit(FFI_MS_CDECL);
  {$ENDIF}

  {$IFDEF TARGET_X64}
    {$IFDEF WINDOWS}
    if (Lower = 'win64') or (Lower = 'default')  then Exit(FFI_WIN64);
    {$ELSE}
    if (Lower = 'unix64') or (Lower = 'default') then Exit(FFI_UNIX64);
    {$ENDIF}
  {$ENDIF}

  {$IFDEF TARGET_ARM32}
  if (Lower = 'cdecl') or (Lower = 'sysv')       then Exit(FFI_SYSV);
  if Lower = 'vfp'                               then Exit(FFI_VFP);
  {$ENDIF}

  {$IFDEF TARGET_ARM64}
  if (Lower = 'sysv') or (Lower = 'default')     then Exit(FFI_SYSV);
  {$ENDIF}

  // everything else falls back to default
  Result := FFI_DEFAULT_ABI;
end;


// ---------------------------------------------------------------------------
// Unit lifecycle
// ---------------------------------------------------------------------------

initialization
{$IFDEF StaticFFI}
  ffi_prep_cif         := @_ffi_prep_cif;
  ffi_call             := @_ffi_call;
  ffi_closure_alloc    := @_ffi_closure_alloc;
  ffi_closure_free     := @_ffi_closure_free;
  ffi_prep_closure_loc := @_ffi_prep_closure_loc;
{$ELSE}
  InitFFITypes();
  LoadFFI();      // best-effort; callers guard with FFILoaded()
{$ENDIF}

finalization
{$IFDEF DynamicFFI}
  UnloadFFI();
{$ENDIF}

end.
