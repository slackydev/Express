unit xpr.Import.System;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
  Error handling - In part taken from https://github.com/nielsAD/lape/
}
{$I header.inc}

interface

uses
  SysUtils, xpr.Types, xpr.CompilerContext;

procedure ImportExternalMethods(ctx: TCompilerContext);
procedure ImportSystemModules(ctx: TCompilerContext);

implementation

uses 
  xpr.Tree, xpr.Utils, xpr.Tokenizer, Math;

const
  SystemDocPos:TDocPos = (Document:'__system__'; Line:0; Column:0);

{$I xpr.inc.import.system.inc}

procedure ImportExternalMethods(ctx: TCompilerContext);
var
  tSizeInt, tInt, tFloat, tString, tUString, tChar, tPointer, tInt8: XType;
begin
  // Cache common types to make definitions cleaner
  tSizeInt := ctx.GetType('Int');
  tInt     := ctx.GetType('Int64');
  tFloat   := ctx.GetType('Double');
  tString  := ctx.GetType('String');
  tUString := ctx.GetType('UnicodeString');
  tChar    := ctx.GetType('Char');
  tPointer := ctx.GetType('Pointer');
  tInt8    := ctx.GetType('Int8');

  // --- Time ---
  ctx.AddExternalFunc(@_GetTickCount, 'GetTickCount', [], [], tFloat);
  ctx.AddExternalFunc(@_Sleep,        'Sleep', [tInt], [pbCopy], nil);

  // --- Math ---
  ctx.AddExternalFunc(@_Inc64,       'Inc',   [tInt], [pbRef], nil);
  ctx.AddExternalFunc(@_Dec64,       'Dec',   [tInt], [pbRef], nil);
  ctx.AddExternalFunc(@_RandInt,     'RandInt', [tInt, tInt], [pbCopy, pbCopy], tInt);
  ctx.AddExternalFunc(@_RandomFloat, 'Random', [], [], tFloat);
  ctx.AddExternalFunc(@_Sin,         'Sin',    [tFloat], [pbCopy], tFloat);
  ctx.AddExternalFunc(@_Cos,         'Cos',    [tFloat], [pbCopy], tFloat);
  ctx.AddExternalFunc(@_Tan,         'Tan',    [tFloat], [pbCopy], tFloat);
  ctx.AddExternalFunc(@_ArcTan,      'ArcTan', [tFloat], [pbCopy], tFloat);
  ctx.AddExternalFunc(@_ArcTan2,     'ArcTan2',[tFloat, tFloat], [pbCopy, pbCopy], tFloat);
  ctx.AddExternalFunc(@_Sqrt,        'Sqrt',   [tFloat], [pbCopy], tFloat);
  ctx.AddExternalFunc(@_Ln,          'Ln',     [tFloat], [pbCopy], tFloat);
  ctx.AddExternalFunc(@_Exp,         'Exp',    [tFloat], [pbCopy], tFloat);
  ctx.AddExternalFunc(@_Frac,        'Frac',   [tFloat], [pbCopy], tFloat);
  ctx.AddExternalFunc(@_IntPower,    'Power',  [tFloat, tInt], [pbCopy, pbCopy], tFloat);
  ctx.AddExternalFunc(@_AbsInt,      'Abs',    [tInt], [pbCopy], tInt);
  ctx.AddExternalFunc(@_AbsFloat,    'Abs',    [tFloat], [pbCopy], tFloat);
  ctx.AddExternalFunc(@_MinInt,      'Min',    [tInt, tInt], [pbCopy, pbCopy], tInt);
  ctx.AddExternalFunc(@_MaxInt,      'Max',    [tInt, tInt], [pbCopy, pbCopy], tInt);
  ctx.AddExternalFunc(@_Trunc,       'Trunc',  [tFloat], [pbCopy], tInt);
  ctx.AddExternalFunc(@_Round,       'Round',  [tFloat], [pbCopy], tInt);

  // --- Pointer Manipulation ---
  ctx.AddExternalFunc(@_FreeMem,    'FreeMem',    [tPointer], [pbRef], nil);
  ctx.AddExternalFunc(@_ReallocMem, 'ReAllocMem', [tPointer, tSizeInt], [pbRef, pbRef], tPointer);
  ctx.AddExternalFunc(@_AllocMem,   'AllocMem',   [tSizeInt], [pbCopy], tPointer);
  ctx.AddExternalFunc(@_GetMem,     'GetMem',     [tSizeInt], [pbCopy], tPointer);
  ctx.AddExternalFunc(@_FillByte,   'FillByte',   [tPointer, tSizeInt, tInt8], [pbCopy, pbCopy, pbCopy], nil);
  ctx.AddExternalFunc(@_Move,       'Move',       [tPointer, tPointer, tSizeInt], [pbCopy, pbCopy, pbCopy], nil);

  // --- String allocation ---
  ctx.AddExternalFunc(@_AnsiSetLength,    '_AnsiSetLength',    [tString, tInt],  [pbRef,pbCopy], nil);
  ctx.AddExternalFunc(@_UnicodeSetLength, '_UnicodeSetLength', [tUString,tInt],  [pbRef,pbCopy], nil);

  // --- String & Type Conversion ---
  ctx.AddExternalFunc(@_IntToStr,   'IntToStr',   [tInt],   [pbCopy], tString);
  ctx.AddExternalFunc(@_FloatToStr, 'FloatToStr', [tFloat], [pbCopy], tString);
  ctx.AddExternalFunc(@_PtrToStr,   'PtrToStr',   [tPointer], [pbCopy], tString);
  ctx.AddExternalFunc(@_StrToInt,   'StrToInt',   [tString], [pbCopy], tInt);
  ctx.AddExternalFunc(@_StrToFloat, 'StrToFloat', [tString], [pbCopy], tFloat);
  ctx.AddExternalFunc(@_Ord,        'Ord',        [tChar], [pbCopy], tInt);
  ctx.AddExternalFunc(@_Chr,        'Chr',        [tInt], [pbCopy], tChar);
end;


procedure ImportSystemModules(ctx: TCompilerContext);
var
  DocPos: TDocPos;
  oldFileContents: string;
begin
  DocPos := SystemDocPos;

  oldFileContents := ctx.MainFileContents;

  // priority 1, everything refcounted needs this
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

end.
