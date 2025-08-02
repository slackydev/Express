  lbNOOP:
    DISPATCH;

  lbJMP:
    pc := Data[pc].Args[0].i32; // Target is an absolute instruction index
    DISPATCH;

  lbRELJMP:
    pc := pc + Data[pc].Args[0].i32; // Target is a relative instruction index
    DISPATCH;

  lbJZ:
    if not PBoolean(Stack.Local(Data[pc].Args[0].Addr))^ then
      pc := pc + Data[pc].Args[1].i32; // Jump if condition is true
    DISPATCH; // Continue to next or new jumped instruction

  lbJNZ:
    if PByte(Stack.Local(Data[pc].Args[0].Addr))^ <> 0 then
      pc := pc + Data[pc].Args[1].i32; // Jump if condition is true
    DISPATCH; // Continue to next or new jumped instruction

  lbJZ_i:
    if Data[pc].Args[0].Arg = 0 then
      pc := pc + Data[pc].Args[1].i32;
    DISPATCH;

  lbJNZ_i:
    if Data[pc].Args[0].Arg <> 0 then
      pc := pc + Data[pc].Args[1].i32;
    DISPATCH;

  lbFILL:
    FillByte(Stack.Local(Data[pc].Args[0].Addr)^, Data[pc].Args[1].Addr, Data[pc].Args[2].u8);
    DISPATCH;

  lbIncTry:
    TryStack.Push(Data[pc].Args[0].Addr, Stack.StackPtr);
    DISPATCH;

  lbDecTry:
    TryStack.Pop();
    DISPATCH;

  lbREFCNT:
    ArrayRefcount(Pointer(Stack.Local(Data[pc].Args[0].Addr)^), Pointer(Stack.Local(Data[pc].Args[1].Addr)^));
    DISPATCH;

  lbREFCNT_imm:
    ArrayRefcount(Pointer(Stack.Local(Data[pc].Args[0].Addr)^), Pointer(Data[pc].Args[1].Addr));
    DISPATCH;

  lbBCHK:
    DISPATCH;

  {$I interpreter.binary_code_direct_threaded.inc}
  {$I interpreter.asgn_code_direct_threaded.inc}

  // --- Specialized FMA operations ---
  lbFMA_i8:  PPtrInt(Stack.Local(Data[pc].Args[3].Addr))^ := PPtrInt(Stack.Local(Data[pc].Args[2].Addr))^ + PInt8(Stack.Local(Data[pc].Args[0].Addr))^ * Data[pc].Args[1].Addr; DISPATCH;
  lbFMA_u8:  PPtrInt(Stack.Local(Data[pc].Args[3].Addr))^ := PPtrInt(Stack.Local(Data[pc].Args[2].Addr))^ + PUInt8(Stack.Local(Data[pc].Args[0].Addr))^ * Data[pc].Args[1].Addr; DISPATCH;
  lbFMA_i16: PPtrInt(Stack.Local(Data[pc].Args[3].Addr))^ := PPtrInt(Stack.Local(Data[pc].Args[2].Addr))^ + PInt16(Stack.Local(Data[pc].Args[0].Addr))^ * Data[pc].Args[1].Addr; DISPATCH;
  lbFMA_u16: PPtrInt(Stack.Local(Data[pc].Args[3].Addr))^ := PPtrInt(Stack.Local(Data[pc].Args[2].Addr))^ + PUInt16(Stack.Local(Data[pc].Args[0].Addr))^ * Data[pc].Args[1].Addr; DISPATCH;
  lbFMA_i32: PPtrInt(Stack.Local(Data[pc].Args[3].Addr))^ := PPtrInt(Stack.Local(Data[pc].Args[2].Addr))^ + PInt32(Stack.Local(Data[pc].Args[0].Addr))^ * Data[pc].Args[1].Addr; DISPATCH;
  lbFMA_u32: PPtrInt(Stack.Local(Data[pc].Args[3].Addr))^ := PPtrInt(Stack.Local(Data[pc].Args[2].Addr))^ + PUInt32(Stack.Local(Data[pc].Args[0].Addr))^ * Data[pc].Args[1].Addr; DISPATCH;
  lbFMA_i64: PPtrInt(Stack.Local(Data[pc].Args[3].Addr))^ := PPtrInt(Stack.Local(Data[pc].Args[2].Addr))^ + PInt64(Stack.Local(Data[pc].Args[0].Addr))^ * Data[pc].Args[1].Addr; DISPATCH;
  lbFMA_u64: PPtrInt(Stack.Local(Data[pc].Args[3].Addr))^ := PPtrInt(Stack.Local(Data[pc].Args[2].Addr))^ + PUInt64(Stack.Local(Data[pc].Args[0].Addr))^ * Data[pc].Args[1].Addr; DISPATCH;

  lbFMA_imm_i8:  PPtrInt(Stack.Local(Data[pc].Args[3].Addr))^ := PPtrInt(Stack.Local(Data[pc].Args[2].Addr))^ + Int8(Data[pc].Args[0].Addr) * Data[pc].Args[1].Addr; DISPATCH;
  lbFMA_imm_u8:  PPtrInt(Stack.Local(Data[pc].Args[3].Addr))^ := PPtrInt(Stack.Local(Data[pc].Args[2].Addr))^ + UInt8(Data[pc].Args[0].Addr) * Data[pc].Args[1].Addr; DISPATCH;
  lbFMA_imm_i16: PPtrInt(Stack.Local(Data[pc].Args[3].Addr))^ := PPtrInt(Stack.Local(Data[pc].Args[2].Addr))^ + Int16(Data[pc].Args[0].Addr) * Data[pc].Args[1].Addr; DISPATCH;
  lbFMA_imm_u16: PPtrInt(Stack.Local(Data[pc].Args[3].Addr))^ := PPtrInt(Stack.Local(Data[pc].Args[2].Addr))^ + UInt16(Data[pc].Args[0].Addr) * Data[pc].Args[1].Addr; DISPATCH;
  lbFMA_imm_i32: PPtrInt(Stack.Local(Data[pc].Args[3].Addr))^ := PPtrInt(Stack.Local(Data[pc].Args[2].Addr))^ + Int32(Data[pc].Args[0].Addr) * Data[pc].Args[1].Addr; DISPATCH;
  lbFMA_imm_u32: PPtrInt(Stack.Local(Data[pc].Args[3].Addr))^ := PPtrInt(Stack.Local(Data[pc].Args[2].Addr))^ + UInt32(Data[pc].Args[0].Addr) * Data[pc].Args[1].Addr; DISPATCH;
  lbFMA_imm_i64: PPtrInt(Stack.Local(Data[pc].Args[3].Addr))^ := PPtrInt(Stack.Local(Data[pc].Args[2].Addr))^ + Int64(Data[pc].Args[0].Addr) * Data[pc].Args[1].Addr; DISPATCH;
  lbFMA_imm_u64: PPtrInt(Stack.Local(Data[pc].Args[3].Addr))^ := PPtrInt(Stack.Local(Data[pc].Args[2].Addr))^ + UInt64(Data[pc].Args[0].Addr) * Data[pc].Args[1].Addr; DISPATCH;

  // --- DREF operations ---
  lbDREF: Move(Pointer(Stack.Local(Data[pc].Args[1].Arg)^)^, Stack.Local(Data[pc].Args[0].Arg)^, Data[pc].Args[2].Arg); DISPATCH;
  lbDREF_32: PUInt32(Stack.Local(Data[pc].Args[0].Arg))^ := PUInt32(Stack.Local(Data[pc].Args[1].Arg)^)^; DISPATCH;
  lbDREF_64: PUInt64(Stack.Local(Data[pc].Args[0].Arg))^ := PUInt64(Stack.Local(Data[pc].Args[1].Arg)^)^; DISPATCH;

  // --- FMAD operations ---
  lbFMAD_d64_64: PInt64(Stack.Local(Data[pc].Args[3].Addr))^ := PInt64(PPtrInt(Stack.Local(Data[pc].Args[2].Addr))^ + PInt64(Stack.Local(Data[pc].Args[0].Addr))^ * Data[pc].Args[1].Addr)^; DISPATCH;
  lbFMAD_d64_32: PInt64(Stack.Local(Data[pc].Args[3].Addr))^ := PInt64(PPtrInt(Stack.Local(Data[pc].Args[2].Addr))^ + PInt32(Stack.Local(Data[pc].Args[0].Addr))^ * Data[pc].Args[1].Addr)^; DISPATCH;
  lbFMAD_d32_64: PInt32(Stack.Local(Data[pc].Args[3].Addr))^ := PInt64(PPtrInt(Stack.Local(Data[pc].Args[2].Addr))^ + PInt64(Stack.Local(Data[pc].Args[0].Addr))^ * Data[pc].Args[1].Addr)^; DISPATCH;
  lbFMAD_d32_32: PInt32(Stack.Local(Data[pc].Args[3].Addr))^ := PInt64(PPtrInt(Stack.Local(Data[pc].Args[2].Addr))^ + PInt32(Stack.Local(Data[pc].Args[0].Addr))^ * Data[pc].Args[1].Addr)^; DISPATCH;

  lbMOV:
    HandleASGN(Data[pc]); // Pass the current instruction
    DISPATCH;

  lbMOVH:
    {Nothing}
    DISPATCH;

  lbPUSH:
    if Data[pc].Args[0].Pos = mpLocal then
      ArgStack.Push(Stack.Local(Data[pc].Args[0].Addr))
    else
      ArgStack.Push(@Data[pc].Args[0].Arg); // Pass address of immediate value
    DISPATCH;

  lbPUSHREF:
    ArgStack.Push(Pointer(Stack.Local(Data[pc].Args[0].Addr)^));
    DISPATCH;

  lbPOP:
    Move(ArgStack.Pop()^, Stack.Local(Data[pc].Args[1].Addr)^, Data[pc].Args[0].Addr);
    DISPATCH;

  lbRPOP:
    Move(Stack.Local(Data[pc].Args[0].Addr)^, ArgStack.Pop()^, Data[pc].Args[1].Addr);
    DISPATCH;

  lbPOPH:
    Pointer(Stack.Local(Data[pc].Args[0].Addr)^) := ArgStack.Pop();
    DISPATCH;

  lbLOAD_GLOBAL:
    Pointer(Stack.Local(Data[pc].Args[0].Addr)^) := Stack.Global(Data[pc].Args[1].Addr);
    DISPATCH;

  lbCOPY_GLOBAL:
    Pointer(Stack.Local(Data[pc].Args[0].Addr)^) := Pointer(Stack.Global(Data[pc].Args[1].Addr)^);
    DISPATCH;

  lbNEWFRAME:
    CallStack.Push(PPtrInt(Stack.StackPtr)^, Stack.StackPtr);
    PPtrInt(Stack.StackPtr)^ := 0;
    Stack.StackPtr += Data[pc].Args[0].Addr; //inc by frame
    DISPATCH;

  lbINVOKE:
    // Check recursion depth
    if RecursionDepth >= MAX_RECURSION_DEPTH then
      raise RuntimeError.Create('Recursion depth limit exceeded');

    Inc(RecursionDepth);
    PPtrInt(Stack.StackPtr)^ := pc;
    pc := PtrInt(Stack.Global(Data[pc].Args[0].Addr)^);
    DISPATCH; // Jump to the function's starting instruction

  lbINVOKEX:
    CallExternal(Pointer(Data[pc].Args[0].Addr), Data[pc].Args[1].Arg, Data[pc].Args[2].Arg <> 0);
    DISPATCH;

  lbRET:
    begin
      if CallStack.Top > -1 then
      begin
        // return value
        if Data[pc].nArgs = 2 then
          case Data[pc].Args[0].Pos of
            mpLocal: Move(Stack.Local(Data[pc].Args[0].Addr)^, ArgStack.Pop()^, Data[pc].Args[1].Addr);
            mpImm:   Move(Data[pc].Args[0].Arg, ArgStack.Pop()^, Data[pc].Args[1].Addr);
          end;

        frame := CallStack.Pop;
        Stack.StackPtr := Frame.StackPtr;
        Dec(RecursionDepth);
        pc := frame.ReturnAddress;
      end else
        Exit;

      DISPATCH
    end;

  lbPRTi:
    case Data[pc].Args[0].Pos of
      mpImm:   PrintInt(@Data[pc].Args[0].Arg, 8); // Immediate value as PtrInt (8 bytes for UInt64)
      mpLocal: PrintInt(Stack.Local(Data[pc].Args[0].Addr), XprTypeSize[Data[pc].Args[0].BaseType]);
    end;
    DISPATCH;

  lbPRTf:
    case Data[pc].Args[0].Pos of
      mpLocal: PrintReal(Stack.Local(Data[pc].Args[0].Addr), XprTypeSize[Data[pc].Args[0].BaseType]);
    end;
    DISPATCH;

  lbPRTb:
    case Data[pc].Args[0].Pos of
      mpLocal: WriteLn(Boolean(Stack.Local(Data[pc].Args[0].Addr)^));
    end;
    DISPATCH;

  lbERROR:
    ;
