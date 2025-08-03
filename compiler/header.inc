(* basedefines *)
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$macro on}
{$inline on}

{$packenum 1} // needed for opcode size | 2x faster
{.$align 4}

// FPC default=4 on x64 it seems - optimal on x86 is 16 byte boundry
{$codealign jump=16}

// other
{.$codealign varmin=16}
{.$codealign loop=16}
{.$codealign proc=16}

{$define colorize}
{$DEFINE xinline}

{$IF Defined(WINDOWS)}
  {$DEFINE xpr_UseSuperInstructions}
{$ENDIF}
