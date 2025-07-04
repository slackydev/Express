unit xprErrors;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
  Error handling - In part taken from https://github.com/nielsAD/lape/
}
{$I header.inc}

interface

uses
  SysUtils, xprTypes, xprTokenizer;

type
  ExpressError = class(Exception)
    DocPos: TDocPos;
    constructor Create(Msg: string; ADocPos: TDocPos); overload;
  end;
  SyntaxError     = class(ExpressError);
  RuntimeError    = class(ExpressError);
  OutOfRangeError = class(ExpressError);
  UnknownError    = class(ExpressError);

  EExceptionType = (eGeneralError, eUnknownError, eSyntaxError, eRuntimeError);
  
resourcestring
  eExpectedButFound      = 'Expected `%s` but found `%s`';
  eExpected              = 'Expected `%s`';
  eExpectedVar           = 'Expected variable';
  eInvalidExpression     = 'Invalid expression';
  eExpectedArgCount      = 'Function `%s` expected `%d` arguments';
  eIndexOutOfRange       = 'Index out of range (index:%d; length:%d)';
  eNotImplemented        = 'Not implemented';
  eNotCompatible1        = 'Operation `%s` is not compatible with (`%s`)';
  eNotCompatible2        = 'Operation is not compatible with (`%s`, `%s`)';
  eNotCompatible3        = 'Operation `%s` is not compatible with (`%s`, `%s`)';
  eNotAllowedOutsideLoop = '`%s` is not allowed outside a loop';
  eUnexpected            = 'An unexpected error occurred';
  eUnexpectedOperation   = 'Unexpected operation `%s`';
  eUnexpectedKeyword     = 'Unexpected keyword `%s`';
  eUndefinedIdentifier   = 'Identifier `%s` is not defined';
  eTypeIdentifier        = 'Type `%s` is not defined';
  eIdentifierExists      = 'Identifier `%s` has already been declared';


procedure RaiseException(Msg:string);
procedure RaiseException(Msg:string; DocPos: TDocPos);
procedure RaiseExceptionFmt(Msg:string; Args: array of const; DocPos: TDocPos);
procedure RaiseException(Typ:EExceptionType; Msg:string; DocPos: TDocPos);
procedure RaiseExceptionFmt(Typ:EExceptionType; Msg:string; Args: array of const; DocPos: TDocPos);

implementation

constructor ExpressError.Create(Msg:string; ADocPos: TDocPos); overload;
begin
  DocPos := ADocPos;
  inherited Create(Msg);
end;
  
{$IF DEFINED(Delphi) AND (CompilerVersion <= 21.00)}
function ReturnAddress: Pointer;
asm
  MOV  EAX, [EBP+4]
end;
{$IFEND}

procedure _RaiseException(e:ExpressError); inline;
{$IFDEF FPC}
begin
  raise e at get_caller_addr(get_frame);
end;
{$ELSE}
begin
  raise e at ReturnAddress;
end;
{$ENDIF}

function AtPos(Pos: TDocPos): string;
begin
  if not isNoDocPos(Pos) then
    Result := ' at ' + Pos.ToString;
end;

procedure RaiseException(Msg:string);
begin
  _RaiseException(ExpressError.Create(Msg, NoDocPos));
end;

procedure RaiseException(Msg:string; DocPos: TDocPos);
begin
  _RaiseException(ExpressError.Create(Msg + AtPos(DocPos), DocPos));
end;

procedure RaiseExceptionFmt(Msg:string; Args: array of const; DocPos: TDocPos);
begin
  _RaiseException(ExpressError.Create(Format(Msg, Args) + AtPos(DocPos), DocPos));
end;

procedure RaiseException(Typ:EExceptionType; Msg:string; DocPos: TDocPos);
begin
  case typ of
    eGeneralError: _RaiseException(ExpressError.Create(Msg + AtPos(DocPos), DocPos));
    eRuntimeError: _RaiseException(RuntimeError.Create(Msg + AtPos(DocPos), DocPos));
    eSyntaxError:  _RaiseException( SyntaxError.Create(Msg + AtPos(DocPos), DocPos));
    eUnknownError: _RaiseException( SyntaxError.Create(Msg + AtPos(DocPos), DocPos));
  end;
end;

procedure RaiseExceptionFmt(Typ:EExceptionType; Msg:string; Args: array of const; DocPos: TDocPos);
begin
  case typ of
    eGeneralError: _RaiseException(ExpressError.Create(Format(Msg, Args) + AtPos(DocPos), DocPos));
    eRuntimeError: _RaiseException(RuntimeError.Create(Format(Msg, Args) + AtPos(DocPos), DocPos));
    eSyntaxError:  _RaiseException( SyntaxError.Create(Format(Msg, Args) + AtPos(DocPos), DocPos));
    eUnknownError: _RaiseException(UnknownError.Create(Msg + AtPos(DocPos), DocPos));
  end;
end;

end.
