(* basedefines *)
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$macro on}
{$inline on}

{$packenum 1} // needed for opcode size | 2x faster
{.$align 4}

// FPC default=4 on x64 it seems - optimal on x86 is 16 byte boundry
{$IFDEF CPU64}
  {$codealign jump=16}
  {$codealign varmin=16}
  {$codealign loop=32}
  {$codealign proc=32}
{$ENDIF}

{$IFDEF CPU32}
  {$codealign jump=4}
{$ENDIF}

// other
{$DEFINE colorize} // for debug outputs like tree
{$DEFINE xinline}  // inline internally where reasonable
{.$DEFINE verbose}  // writelns where needed, like in the JIT for debugging
{.$DEFINE xpr_DisableJIT}
{.$DEFINE xpr_NoFFIJIT}

//{$IFDEF CPUAARCH64}
//  {$DEFINE xpr_DisableJIT}
//{$ENDIF}
