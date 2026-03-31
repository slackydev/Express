// interpreter_jitcode.inc
//
// Drop-in replacement for the original file.
//
// EmitJITBlock is in a separate include to keep file sizes manageable.
// Everything else (CanJITSuper, CanJIT_x86_64, EmitCodeBlock,
// GenerateSuperInstructions, x86_64_Compile, FreeCodeBlock, FreeJIT)
// is UNCHANGED from the original.

{$I interpreter.jitcode_emitjit.inc}

const
  MIN_JIT_OPCODE_COUNT = 2;

// Deallocates memory created by EmitCodeBlock.
// Should be called on each opcode of bcSUPER and bcHOTLOOP.
procedure TInterpreter.FreeCodeBlock(CodePtr: Pointer; TotalSize: SizeInt);
begin
  if CodePtr = nil then Exit;
  {$IFDEF WINDOWS}
  VirtualFree(CodePtr, 0, MEM_RELEASE);
  {$ELSE}
  fpmunmap(CodePtr, TotalSize);
  {$ENDIF}
end;

procedure TInterpreter.FreeJIT(var BC: TBytecode);
var i: Int32;
begin
  for i := 0 to BC.Code.High do
    if (BC.Code.Data[i].Code = bcSUPER) or (BC.Code.Data[i].Code = bcHOTLOOP) then
      Self.FreeCodeBlock(Pointer(BC.Code.Data[i].Args[4].Data.Addr), 0);
end;

//----------------------------------------------------------------------------
// Opcodes/bytecodes that can be merged into a single super-instruction block.
// None of these may rely on methods that cannot be inlined.
//----------------------------------------------------------------------------
function CanJITSuper(Arg: TBytecodeInstruction): Boolean;
var
  code: Int32;
  isBinary: Boolean;
begin
  code     := Ord(Arg.Code);
  isBinary := InRange(Ord(Code), Ord(bcADD_lll_i32), Ord(bcBOR_iil_u64));

  Result := isBinary or (InRange(Code, Ord(bcMOV_i8_i8_ll),  Ord(bcMOV_f64_f64_li)));
  Result := Result   or (InRange(Code, Ord(bcMOVH_i8_i8_ll), Ord(bcMOVH_f64_f64_li)));
  Result := Result   or (InRange(Code, Ord(bcINC_i32),        Ord(bcFMAD_d32_32)));
  Result := Result   or (Code = Ord(bcADDR));

  // deref uses Move-function, so that's a no-go
  Result := Result and (Arg.Code <> bcDREF);
  // power for floats uses Power-function, also a no-go
  Result := Result and not(InRange(Code, Ord(bcPOW_lll_i32), Ord(bcPOW_iil_f64))
              and (Arg.Args[2].BaseType in XprFloatTypes));
  // modulo for floats uses mod-function, also a no-go
  Result := Result and not(InRange(Code, Ord(bcMOD_lll_i32), Ord(bcMOD_iil_f64))
              and (Arg.Args[2].BaseType in XprFloatTypes));

  // let's not JIT binary that is over the native computation size
  {$IFDEF CPU32}
  if Result and isBinary and (Arg.Args[2].BaseType in XprOrdinalTypes)
     and (XprTypeSize[Arg.Args[0].BaseType] > SizeOf(SizeInt)) then
    Result := False;
  {$ENDIF}
end;

function CanJIT_x86_64(Arg: TBytecodeInstruction; AllowJumps: Boolean): Boolean;
var
  code: Int32;
  isBinary: Boolean;
begin
  code     := Ord(Arg.Code);
  isBinary := InRange(Ord(Code), Ord(bcADD_lll_i32), Ord(bcBOR_iil_u64));

  Result := isBinary or (InRange(Code, Ord(bcMOV_i8_i8_ll),  Ord(bcMOV_f64_f64_li)));
  Result := Result   or (InRange(Code, Ord(bcMOVH_i8_i8_ll), Ord(bcMOVH_f64_f64_li)));
  Result := Result   or (InRange(Code, Ord(bcINC_i32),        Ord(bcFMAD_d32_32)));
  Result := Result   or (Code = Ord(bcADDR));

  // deref uses Move-function, so that's a no-go
  Result := Result and (Arg.Code <> bcDREF);
  // power for floats uses Power-function, also a no-go
  Result := Result and not(InRange(Code, Ord(bcPOW_lll_i32), Ord(bcPOW_iil_f64))
              and (Arg.Args[2].BaseType in XprFloatTypes));
  // modulo for floats uses mod-function, also a no-go
  Result := Result and not(InRange(Code, Ord(bcMOD_lll_i32), Ord(bcMOD_iil_f64))
              and (Arg.Args[2].BaseType in XprFloatTypes));

  // branching
  if AllowJumps then
    Result := Result or (Arg.Code = bcJZ) or (Arg.Code = bcJNZ)
                     or (Arg.Code = bcRELJMP);
end;

//----------------------------------------------------------------------------
// Copies exactly Count opcode native-code blocks into a fresh executable
// allocation and appends a RET.
// TotalSize returns the number of bytes written.
//----------------------------------------------------------------------------
function TInterpreter.EmitCodeBlock(CodeList: PBytecodeInstruction;
                                    Translation: TTranslateArray;
                                    Count: Int32;
                                var TotalSize: SizeInt): Pointer;
var
  offset  : NativeUInt;
  dataPtrs: array of record start_, stop_: Pointer; end;
  oldProt : DWORD;
  j       : Integer;
  code    : EBytecode;
  ExecMem : Pointer;
  RetInstructionSize: Byte;
begin
  {$IFDEF CPUAARCH64}
  RetInstructionSize := 4; // ARM64 RET is 4 bytes
  {$ELSE}
  RetInstructionSize := 1; // x86/x64 RET is 1 byte ($C3)
  {$ENDIF}

  // 1) Build table of (start, stop) pointers for each snippet
  SetLength(dataPtrs, Count);
  TotalSize := RetInstructionSize;

  for j := 0 to Count - 1 do
  begin
    code := (CodeList + j)^.Code;
    dataPtrs[j].start_ := Pointer(Translation[code]);
    dataPtrs[j].stop_  := Pointer(Translation[EBytecode(Ord(code) + 1)]);
    Inc(TotalSize, NativeUInt(dataPtrs[j].stop_) - NativeUInt(dataPtrs[j].start_));
  end;

  // 2) Allocate writable memory
  ExecMem := AllocateExecutableMemory(TotalSize);
  if ExecMem = nil then
  begin
    {$IFDEF VERBOSE}
    WriteLn('EmitCodeBlock: JIT allocation has failed');
    {$ENDIF}
    Exit(nil);
  end;

  // 3) Copy each snippet in order
  offset := 0;
  for j := 0 to Count - 1 do
  begin
    Move(dataPtrs[j].start_^,
         Pointer(NativeUInt(ExecMem) + offset)^,
         NativeUInt(dataPtrs[j].stop_) - NativeUInt(dataPtrs[j].start_));
    Inc(offset, NativeUInt(dataPtrs[j].stop_) - NativeUInt(dataPtrs[j].start_));
  end;

  // 4) Append the final RET instruction
  {$IFDEF CPUAARCH64}
  PDword(NativeUInt(ExecMem) + offset)^ := $D65F03C0;
  {$ELSE}
  PByte(NativeUInt(ExecMem) + offset)^  := $C3;
  {$ENDIF}
  Inc(offset, RetInstructionSize);

  // 5) Switch memory protection from Writable to Executable
  SetMemoryExecutable(ExecMem, offset);

  Result := ExecMem;
end;

procedure TInterpreter.GenerateSuperInstructions(var BC: TBytecode; Translation: TTranslateArray);
var
  i, k, n: Int32;
  total: SizeInt;
  execMem: Pointer;

  function isLoop(): Boolean; inline;
  begin
    Result := (n-2 >= 0)
        and InRange(Ord(BC.Code.Data[n-2].Code), Ord(bcEQ_lll_i32), Ord(bcLTE_iil_f64))
        and (BC.Code.Data[n-1].Code = bcJZ)
        and (BC.Code.Data[i].Code   = bcRELJMP);
    if Result then
    begin
      // type conversions can cause off-by-one in the comparator,
      // disqualifying it from hotlooping
      Result := (BC.Code.Data[i].Args[0].Data.i32 = n-i-3);
    end;
  end;

begin
  i := 0;
  while i <= BC.Code.High do
  begin
    if CanJITSuper(BC.Code.Data[i]) and
       (BC.Settings.Data[i].JIT in [1, {$IFNDEF CPUX86_64}2,{$ENDIF}3]) then
    begin
      n := i;

      // Find how many eligible opcodes follow
      while (i <= BC.Code.High) and CanJITSuper(BC.Code.Data[i]) and
            (BC.Settings.Data[i].JIT in [1, {$IFNDEF CPUX86_64}2,{$ENDIF}3]) do
        Inc(i);

      // Control flow: what comes before is likely a cmp operation.
      // Don't absorb it here; it may be picked up as a hotloop in the next pass.
      if (i - n >= 2) and (BC.Code.Data[i].Code = bcJZ) then
        Dec(i);

      // Only generate a super-instruction if 2 or more instructions can be merged
      if (i - n >= MIN_JIT_OPCODE_COUNT) or isLoop() then
      begin
        if isLoop() then
        begin
          execMem := EmitCodeBlock(@BC.Code.Data[n-2], Translation, 1, total);
          if execMem = nil then Break;
          BC.Code.Data[n-2].Code              := bcHOTLOOP;
          BC.Code.Data[n-2].Args[4].Data.Addr := NativeInt(execMem);
        end;

        execMem := EmitCodeBlock(@BC.Code.Data[n], Translation, i - n, total);
        if execMem = nil then Break;
        BC.Code.Data[n].Code              := bcSUPER;
        BC.Code.Data[n].Args[4].Data.Addr := NativeInt(execMem);
        BC.Code.Data[n].nArgs             := 4;
      end;
    end else
      Inc(i);
  end;
end;

procedure TInterpreter.x86_64_Compile(var BC: TBytecode; AllowJumps: Boolean);
var
  i, trace_start, targetIdx, trace_len, k: Integer;
  loopDepth, totalSize: SizeInt;
  execMem: Pointer;
  ValidRelJmpLocations: specialize TArrayList<PtrUInt>;
  current_instr: TBytecodeInstruction;
  found: Boolean;
begin
  i := 0;

  while i <= BC.Code.High do
  begin
    if CanJIT_x86_64(BC.Code.Data[i], False) and
       (BC.Settings.Data[i].JIT in [2, 3]) then
    begin
      trace_start := i;
      ValidRelJmpLocations.Init([]);
      loopDepth := 0;

      // Find the end of the trace
      while (i <= BC.Code.High) and
            CanJIT_x86_64(BC.Code.Data[i], AllowJumps) and
            (BC.Settings.Data[i].JIT in [2, 3]) do
      begin
        current_instr := BC.Code.Data[i];
        if not AllowJumps then
        begin
          Inc(i);
          Continue;
        end;

        // 1. Conditional jump — record where its RELJMP counterpart should be
        if current_instr.Code in [bcJZ, bcJNZ] then
        begin
          targetIdx := i + current_instr.Args[1].Data.i32;
          ValidRelJmpLocations.Add(targetIdx);

          // Heuristic: detect a loop
          if (targetIdx > 0) and (BC.Code.Data[targetIdx].Code = bcRELJMP) then
            Inc(loopDepth);
        end;

        // 2. RELJMP — only continue the trace if we expected it
        if current_instr.Code = bcRELJMP then
        begin
          found := False;
          for k := 0 to ValidRelJmpLocations.High() do
            if ValidRelJmpLocations.Data[k] = i then
            begin found := True; Break; end;

          if not found then Break;
          Dec(loopDepth);

          // Finished a complete loop — end the trace here to avoid
          // accidentally merging into the next loop's header
          if loopDepth = 0 then
          begin
            Inc(i, 1); // step past the RELJMP
            Break;
          end;
        end;

        Inc(i);
      end;

      trace_len := i - trace_start;

      // Compile the trace
      if trace_len >= MIN_JIT_OPCODE_COUNT then
      begin
        execMem := EmitJITBlock(@BC.Code.Data[trace_start], trace_len, totalSize, @BC.Settings.Data[trace_start], True);
        if execMem <> nil then
        begin
          {$IFDEF VERBOSE}
          WriteLn('SUCCESS (JIT x64): trace ', trace_start, '..', i-1);
          for k := 0 to totalSize-1 do Write(IntToHex(PByte(execMem+k)^));
          WriteLn();
          {$ENDIF}
          BC.Code.Data[trace_start].Code              := bcJIT;
          BC.Code.Data[trace_start].Args[4].Data.Addr := NativeInt(execMem);
          BC.Code.Data[trace_start].nArgs             := trace_len;
        end
        else
        begin
          {$IFDEF VERBOSE}
          WriteLn('INFO (JIT x64): bailed on trace ', trace_start, '..', i-1);
          {$ENDIF}
          i := trace_start + 1;
        end;
      end;
    end
    else
      Inc(i);
  end;
end;
