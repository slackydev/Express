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
    if (BC.Code.Data[i].Code = bcSUPER) then
      Self.FreeCodeBlock(Pointer(BC.Code.Data[i].Args[4].Data.Addr), 0);
end;

//----------------------------------------------------------------------------
// Opcodes/bytecodes that can be merged into a single super-instruction block.
// None of these may rely on methods that cannot be inlined.
//----------------------------------------------------------------------------
function CanJITSuper(Arg: TBytecodeInstruction): Boolean;
var
  code: Int32;
  isBinary, isSlowBinary: Boolean;
begin
  // We are gonna limit this to PURE local variables. mpImm is irrelevant here, not actual
  // variable data stored in there, just possible "extra" stuff for example for FMA
  for code := 0 to Arg.nArgs - 1 do
    if not (Arg.Args[code].Pos in [mpLocal, mpConst, mpImm]) then
      Exit(False);

  for code := 2 to Arg.nArgs - 1 do
    if not (Arg.Args[code].Pos in [mpLocal, mpImm]) then
      Exit(False);

  code := Ord(Arg.Code);
  
  // Typed binary math
  isBinary     := InRange(code, Ord(bcADD_i32), Ord(bcSAR_u64));
  
  // Generic math
  isSlowBinary := Arg.Code in [bcADD..bcSAR];

  Result := isBinary and (not isSlowBinary);
  Result := Result or InRange(code, Ord(bcMOV8  ), Ord(bcMOVF64_f64  ));
  Result := Result or (Arg.Code = bcSTORE) or InRange(code, Ord(bcSTORE8), Ord(bcSTOREF64_f64));
  Result := Result or (InRange(code, Ord(bcADDR), Ord(bcFMA_u64)));

  // uspecified deref uses Move-function, so that's a no-go
  Result := Result and (Arg.Code <> bcDREF);
  
  // power and modulo for floats use external math functions, a no-go
  Result := Result and not((Arg.Code in [bcPOW, bcMOD]) and (Arg.Args[2].BaseType in XprFloatTypes));
  Result := Result and not(Arg.Code in [bcPOW_f32, bcPOW_f64, bcMOD_f32, bcMOD_f64]);

  {$IFDEF CPU32}
  if Result and (isBinary or isSlowBinary) and (Arg.Args[2].BaseType in XprOrdinalTypes)
     and (XprTypeSize[Arg.Args[0].BaseType] > SizeOf(SizeInt)) then
    Result := False;
  {$ENDIF}
end;

function CanJIT_x86_64(Arg: TBytecodeInstruction; AllowJumps: Boolean): Boolean;
var
  code: Int32;
  isBinary, isSlowBinary: Boolean;
begin
  if Arg.Code = bcBCHK then
  begin
    Result := (Arg.Args[0].Pos = mpLocal) and (Arg.Args[1].Pos = mpLocal) and
              (BaseIntType(Arg.Args[1].BaseType) in [xtInt32, xtInt64, xtUInt32, xtUInt64]);
    Exit;
  end;

  // DEFENSIVE JIT SANDBOXING, no globals, no externals, nothing special.
  for code := 0 to Arg.nArgs - 1 do
    if not (Arg.Args[code].Pos in [mpLocal, mpConst, mpImm]) then
      Exit(False);

  code := Ord(Arg.Code);
  
  // Typed binary math
  isBinary     := InRange(code, Ord(bcADD_i32), Ord(bcSAR_u64));
  
  // Generic math
  isSlowBinary := Arg.Code in [bcADD..bcSAR];

  Result := isBinary or isSlowBinary;
  Result := Result or (Arg.Code = bcMOV)   or InRange(code, Ord(bcMOV8  ), Ord(bcMOVF64_f64  ));
  Result := Result or (Arg.Code = bcSTORE) or InRange(code, Ord(bcSTORE8), Ord(bcSTOREF64_f64));
  Result := Result or (InRange(code, Ord(bcADDR), Ord(bcFMA_u64)));

  // uspecified deref uses Move-function, so that's a no-go
  if (Arg.Code = bcDREF) and not(Arg.Args[2].Data.Addr in [1,2,4,8]) then
     Exit(False);
  
  // power and modulo for floats use external math functions, a no-go
  Result := Result and not((Arg.Code in [bcPOW, bcMOD]) and (Arg.Args[2].BaseType in XprFloatTypes));
  Result := Result and not(Arg.Code in [bcPOW_f32, bcPOW_f64, bcMOD_f32, bcMOD_f64]);

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
const
  // Sentinels baked into JIT template bodies via JITBASE_N defines.
  // < $80000000: safe as sign-extended imm32 on x86-64.
  // Distinct from each other and from any realistic frame offset.
  JIT_SENTINEL: array[0..3] of UInt32 = (JIT_OFF0, JIT_OFF1, JIT_OFF2, JIT_OFF3);

  JIT_CONST_SENTINEL: array[0..1] of UInt32 = (JIT_CONST0, JIT_CONST1);

var
  offset  : NativeUInt;
  dataPtrs: array of record start_, stop_: Pointer; end;
  j       : Integer;
  code    : EBytecode;
  ExecMem : Pointer;
  RetInstructionSize: Byte;
  raw: TByteArray; k: Int32;

  procedure PatchLocalSentinels(Block: PByte; Size: NativeUInt; const Instr: TBytecodeInstruction);
  var
    p: PByte;
    v, patched: UInt32;
    s: Integer;
  begin
    if Size < SizeOf(UInt32) then Exit;
    p := Block;
    while NativeUInt(p) <= NativeUInt(Block) + Size - SizeOf(UInt32) do
    begin
      Move(p^, v, SizeOf(UInt32));
      for s := 0 to High(JIT_SENTINEL) do
        if v = JIT_SENTINEL[s] then
        begin
          patched := UInt32(Instr.Args[s].Data.Addr);
          Move(patched, p^, SizeOf(UInt32));
          Break;
        end;
      Inc(p);
    end;
  end;

  procedure PatchConstSentinels(Block: PByte; Size: NativeUInt; const Instr: TBytecodeInstruction);
  var
    p: PByte;
    v, patched: UInt32;
    s: Integer;
  begin
    if Size < SizeOf(UInt32) then Exit;
    p := Block;
    while NativeUInt(p) <= NativeUInt(Block) + Size - SizeOf(UInt32) do
    begin
      Move(p^, v, SizeOf(UInt32));
      for s := 0 to High(JIT_CONST_SENTINEL) do
        if v = JIT_CONST_SENTINEL[s] then
        begin
          patched := UInt32(Instr.Args[s].Data.Addr);
          Move(patched, p^, SizeOf(UInt32));
          Break;
        end;
      Inc(p);
    end;
  end;
var
  SnipSize: NativeUInt;
  IsArg0Const, IsArg1Const: Boolean;
  Entry: TTranslateArray;
  Instr: TBytecodeInstruction;
begin
  {$IFDEF CPUAARCH64}
  RetInstructionSize := 4;
  {$ELSE}
  RetInstructionSize := 1;
  {$ENDIF}

  // 1) Build (start, stop) table and total size
  SetLength(dataPtrs, Count);
  TotalSize := RetInstructionSize;
  for j := 0 to Count - 1 do
  begin
    Instr := (CodeList + j)^;
    code  := Instr.Code;

    IsArg0Const := Instr.Args[0].Pos = mpConst;
    IsArg1Const := Instr.Args[1].Pos = mpConst;

    if (not IsArg0Const) and (not IsArg1Const) then
      Entry := JumpTable
    else if (not IsArg0Const) and IsArg1Const then
      Entry := JumpTable_LC
    else if IsArg0Const and (not IsArg1Const) then
      Entry := JumpTable_CL
    else
      Exit(nil);

    dataPtrs[j].start_ := Pointer(Entry[code]);
    dataPtrs[j].stop_  := Pointer(Entry[EBytecode(Ord(code) + 1)]);
    Inc(TotalSize, NativeUInt(dataPtrs[j].stop_) - NativeUInt(dataPtrs[j].start_));
  end;

  // 2) Allocate writable+executable memory
  ExecMem := AllocateExecutableMemory(TotalSize);
  if ExecMem = nil then
  begin
    {$IFDEF VERBOSE}
    WriteLn('EmitCodeBlock: SUPER allocation has failed');
    {$ENDIF}
    Exit(nil);
  end;

  // 3) Copy each snippet then immediately patch its sentinels
  offset := 0;
  for j := 0 to Count - 1 do
  begin
    SnipSize := NativeUInt(dataPtrs[j].stop_) - NativeUInt(dataPtrs[j].start_);

    Move(dataPtrs[j].start_^, Pointer(NativeUInt(ExecMem) + offset)^, SnipSize);

    PatchLocalSentinels(PByte(NativeUInt(ExecMem) + offset),SnipSize,(CodeList + j)^);
    PatchConstSentinels(PByte(NativeUInt(ExecMem) + offset),SnipSize,(CodeList + j)^);

    {$IFDEF VERBOSE_EX}
    code := (CodeList + j)^.Code;
    SetLength(raw, SnipSize);
    Move(Pointer(NativeUInt(ExecMem) + offset)^, raw[0], SnipSize);
    Write('Copy+Patch: ', code, ' = (');
    for k := 0 to Length(raw) - 1 do Write(IntToHex(raw[k], 2), '');
    WriteLn(')');
    {$ENDIF}

    Inc(offset, SnipSize);
  end;

  // 4) Append final RET
  {$IFDEF CPUAARCH64}
  PDWord(NativeUInt(ExecMem) + offset)^ := $D65F03C0;
  {$ELSE}
  PByte(NativeUInt(ExecMem) + offset)^  := $C3;
  {$ENDIF}
  Inc(offset, RetInstructionSize);

  // 5) Flip protection to executable
  SetMemoryExecutable(ExecMem, offset);
  Result := ExecMem;
end;

procedure TInterpreter.GenerateSuperInstructions(var BC: TBytecode; Translation: TTranslateArray);
var
  i, k, n, h: Int32;
  total: SizeInt;
  execMem: Pointer;
  boundries: array of Int32;

  function isBoundary(test: Int32): Boolean;
  var m: Int32;
  begin
    Result := False;
    for m:=0 to High(boundries) do
      if boundries[m] = test then
        Exit(True);
  end;

begin
  // IMPORTANT, PRECOMUTE ALL LANDINGSITES FOR EXISTING RELJMP
  // STORE IT IN A LIST SO THAT WE NEVER CROSS A BASIC BLOCK
  boundries := nil;
  for i:=0 to BC.Code.High do
    if BC.Code.Data[i].Code = bcRELJMP then
    begin
      SetLength(boundries, Length(boundries)+1);
      boundries[High(boundries)] := i+BC.Code.Data[i].Args[0].Data.Arg;
    end else if BC.Code.Data[i].Code in [bcJZ, bcJNZ] then
    begin
      SetLength(boundries, Length(boundries)+1);
      boundries[High(boundries)] := i+BC.Code.Data[i].Args[1].Data.Arg;
    end;

  i := 0;
  while i <= BC.Code.High do
  begin
    if CanJITSuper(BC.Code.Data[i]) and (BC.Settings.Data[i].JIT in [1, 2, 3]) then
    begin
      n := i;

      // Find how many eligible opcodes follow
      while (i <= BC.Code.High) and CanJITSuper(BC.Code.Data[i]) and
            (BC.Settings.Data[i].JIT in [1, 2, 3]) do
      begin
        if isBoundary(i) then
        begin
          Inc(i);
          Break;
        end;
        Inc(i);
      end;

      // Only generate a super-instruction if 2 or more instructions can be merged
      if (i - n >= 1) then
      begin
        execMem := EmitCodeBlock(@BC.Code.Data[n], Translation, i - n, total);
        if execMem = nil then
        begin
          {$IFDEF VERBOSE}WriteLn('BAILED ON: ', n, ' -> ', i-1);{$ENDIF}
          continue;
        end;
        {$IFDEF VERBOSE}WriteLn('SUPER INSTRCTIONS FROM: ', n, ' -> ', i-1);{$ENDIF}
        BC.Code.Data[n].Code              := bcSUPER;
        BC.Code.Data[n].Args[4].Data.Addr := NativeInt(execMem);
        BC.Code.Data[n].nArgs             := 4;
        // We sneak a trace start marker into the pre bcJZ if there is one
        // This is used for hotloop promotation test.
        if BC.Code.Data[i].Code = bcJZ then
          BC.Code.Data[i].Args[4].Data.Addr := n;
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
var
  searchIdx, newFrameIdx: Int32;
  isWholeBody: Boolean;
begin
  i := 0;

  while i <= BC.Code.High do
  begin
    if CanJIT_x86_64(BC.Code.Data[i], False) and
       (BC.Settings.Data[i].JIT in [2, 3]) and (BC.Code.Data[i].Code <> bcBCHK) then
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


          // ===================================================================
          // WHOLE-BODY TRACE DETECTION - Tag opcode `bcNEWFRAME` as JIT
          // ===================================================================
          isWholeBody := True;

          // 1. Check forwards: Must be bcDecTry followed by bcRET
          searchIdx := trace_start + trace_len;
          if (searchIdx <= BC.Code.High) and (BC.Code.Data[searchIdx].Code = bcDecTry) then
            Inc(searchIdx);

          if (searchIdx > BC.Code.High) or (BC.Code.Data[searchIdx].Code <> bcRET) then
            isWholeBody := False;

          // 2. Check backwards: Must be bcIncTry, preceded by POPs, preceded by bcNEWFRAME
          if isWholeBody then
          begin
            searchIdx := trace_start - 1;
            if (searchIdx >= 0) and (BC.Code.Data[searchIdx].Code = bcIncTry) then
              Dec(searchIdx);

            while (searchIdx >= 0) and (BC.Code.Data[searchIdx].Code in [bcPOP, bcPOPH]) do
              Dec(searchIdx);

            if (searchIdx >= 0) and (BC.Code.Data[searchIdx].Code = bcNEWFRAME) then
            begin
              // SUCCESS! Store the JitBodyPtr directly in bcNEWFRAME Args[1]
              newFrameIdx := searchIdx;
              BC.Code.Data[newFrameIdx].Args[1].Data.Addr := NativeInt(execMem);
              BC.Code.Data[newFrameIdx].Args[1].Pos       := mpImm; // Tag it as valid
            end;
          end;
          // =================================================================
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
