(* basedefines *)
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$macro on}
{$inline on}

// needed for opcode size to fit in cacheline, much faster VM.
{$packenum 1}

// FPC default=4 on x64 it seems - optimal on x86-64 is 16 byte boundry
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

// writelns where needed, like in the JIT for debugging
{.$DEFINE verbose}
{.$DEFINE verbose_ex}


// -----------------------------------------------------------------------------
// JIT and FFI control below:
// -----------------------------------------------------------------------------

// Disables the JIT completely
{.$DEFINE xpr_DisableJIT}

// Disables fast callbacks for fully JITed functions..
// Avoids the VM when possible - 2x faster:
{.$DEFINE xpr_NoFFIJIT}

// No specialized 64bit FFI (always use libffi).
// This is ~10x faster than pure libffi + VM, ~5x faster than libffi + JIT
{.$DEFINE xpr_NoX64_FFI}

// ARM NEEDS ITS OWN MAGIC SAUSE, ARM JIT PERHAPS
{$IF NOT (Defined(CPUX86_64) or Defined(CPUI386))}
  {$DEFINE xpr_DisableJIT}
{$ENDIF}

// UNTESTED:
{$IFDEF UNIX}
{$DEFINE xpr_NoX64_FFI}
{$ENDIF}
