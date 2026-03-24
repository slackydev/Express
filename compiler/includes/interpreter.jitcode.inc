const
  MIN_JIT_OPCODE_COUNT = 2;


(*
  Todo: Handle integer exceptions before FPC tries to catch them, and fails.
  This mainly deals with DivByZero, we do so by triggering a return with 1 or 0.
*)
function TInterpreter.EmitJITBlock(CodeList: PBytecodeInstruction; Count: Int32;
                                var TotalSize: SizeInt; CanJMP:Boolean=False): Pointer;
type
  TPatch = record
    PatchLocation: PInt32;
    TargetBCIndex: Int32;
  end;
  TPatchList = array of TPatch;
  TLabelMap = array of PByte;
var
  // All variables are declared here, at the top of the function.
  ExecMem: PByte;
  Emitter: TJitEmitter;
  i, j: Integer; // Loop counters
  bc_instr: TBytecodeInstruction;
  code: EBytecode;
  Patches: TPatchList;
  Labels: TLabelMap;
  patch: TPatch;
  loc: PInt32;
  targetAddress: PByte;
  jumpInstructionEnd: PByte;
  relativeOffset: PtrInt;
  arg_type: EExpressBaseType;
  // XMM variables
  SpillLocations: specialize TArrayList<PtrInt>;
  XMMAllocator: TXMMRegisterAllocator;
  ptr_arg, val_arg: TOperand;
  val_type, dest_type: EExpressBaseType;
  reg1,reg2,reg3: EXMMReg;
begin
  // INITIALIZATION
  ExecMem := AllocateExecutableMemory(Count * 50 + 64);
  {$IFDEF VERBOSE}
  WriteLn(PtrUInt(ExecMem));
  {$ENDIF}
  if ExecMem = nil then Exit(nil);

  SpillLocations.Init([]);

  // Analyze ALL JZ/JNZ and Schedule Spills
  for i := 0 to Count - 1 do
  begin
    bc_instr := (CodeList + i)^;
    if bc_instr.Code in [bcJZ, bcJNZ] then
    begin
      j := bc_instr.Args[1].Data.i32;
      // Schedule a spill at the destination of the jump.
      // If we branched then a spill must happen to ensure memory contains updated
      // in case we skipped branching the spill still happens, which is okay.
      // This is the merge point for the "taken-jump" path.
      SpillLocations.Add(i + j + 1);

      // if it's a loop, spill before entry,
      // if it's conditional, spill inside once, no bigger cost.
      bc_instr := (CodeList + i+j)^;
      if bc_instr.Code = bcRELJMP then
      begin
        SpillLocations.Add((i+j) + bc_instr.Args[0].Data.Addr + 1)
      end
      else
        SpillLocations.Add(i);
    end;
  end;


  Emitter.Init(ExecMem);
  SetLength(Patches, 0);
  SetLength(Labels, Count+1);

  Emitter.Preamble;
  XMMAllocator.Init(@Emitter);
  XMMAllocator.InvalidateAll;
  XMMAllocator.AnalyzeTrace(CodeList, Count);

  // --- PASS 1: Generate Code and Record Labels & Patch Sites ---
  for i := 0 to Count - 1 do
  begin
    // I only imagine 0 to 10 jumps normally, should be OK overhead.
    for j:=0 to SpillLocations.High() do
      if SpillLocations.Data[j] = i then
      begin
        XMMAllocator.InvalidateAll;
        break;
      end;

    Labels[i] := Emitter.CurrentPosition;
    bc_instr := (CodeList + i)^;
    code := bc_instr.Code;
    arg_type := BaseJITType(bc_instr.Args[0].BaseType);
    //if code in [bcFMAD_d64_64..bcFMAD_d32_32] then
    //  arg_type := BaseJITType(bc_instr.Args[3].BaseType);

    // 1. Handle special, non-typed, or control-flow instructions first.
    if (CanJMP) and (code = bcRELJMP) or (code = bcJZ) or (code = bcJNZ) then
    begin
      XMMAllocator.SpillAllDirty;

      case code of
        bcJZ:
          begin
            Emitter.Load_Int_Operand(bc_instr.Args[0], rax);
            Emitter.TEST_Reg_Reg(rax, rax);
            loc := Emitter.Jcc_Rel32(je);

            SetLength(Patches, Length(Patches) + 1);
            Patches[High(Patches)].PatchLocation := loc;
            Patches[High(Patches)].TargetBCIndex := i + bc_instr.Args[1].Data.i32 + 1;
          end;
        bcJNZ:
          begin
            Emitter.Load_Int_Operand(bc_instr.Args[0], rax);
            Emitter.TEST_Reg_Reg(rax, rax);
            loc := Emitter.Jcc_Rel32(jne);

            SetLength(Patches, Length(Patches) + 1);
            Patches[High(Patches)].PatchLocation := loc;
            Patches[High(Patches)].TargetBCIndex := i + bc_instr.Args[1].Data.i32 + 1;
          end;
        bcRELJMP:
          begin
            loc := Emitter.JMP_Rel32;

            SetLength(Patches, Length(Patches) + 1);
            Patches[High(Patches)].PatchLocation := loc;
            Patches[High(Patches)].TargetBCIndex := i + bc_instr.Args[0].Data.i32 + 1;
          end;
      end;
    end
    else if arg_type in [xtInt8, xtInt16, xtInt32, xtInt64] then // Simplified for PoC
    begin
      case bc_instr.Code of
        bcMOV_i8_i8_ll..bcMOV_u64_u64_li:
          begin
            // undefined signed := unsigned
            Emitter.Load_Int_Operand(bc_instr.Args[1], rax);
            Emitter.Store_Int_Result(bc_instr.Args[0], rax);
          end;

        bcMOVH_i8_i8_ll..bcMOVH_u64_u64_li:
        begin
          Emitter.MOV_Reg_Mem_i64(rax, rbx, bc_instr.Args[0].Data.Addr);
          Emitter.Load_Int_Operand(bc_instr.Args[1], rcx);
          Emitter.Store_Int_Result_To_Addr(bc_instr.Args[1], rcx, rax);
        end;

        bcEQ_lll_i32..bcLTE_iil_f64:
          begin
            Emitter.Load_Int_Operand(bc_instr.Args[0], rax);
            Emitter.Load_Int_Operand(bc_instr.Args[1], rcx);
            Emitter.CMP_Reg_Reg(rax, rcx); // sets the flag
            // cl = (result of cmp)
            case code of
              bcEQ_lll_i32..bcEQ_iil_f64:   Emitter.SETcc(sete, rcx);
              bcNE_lll_i32..bcNE_iil_f64:   Emitter.SETcc(setne, rcx);
              bcLT_lll_i32..bcLT_iil_f64:   Emitter.SETcc(setl, rcx);
              bcGT_lll_i32..bcGT_iil_f64:   Emitter.SETcc(setg, rcx);
              bcLTE_lll_i32..bcLTE_iil_f64: Emitter.SETcc(setle, rcx);
              bcGTE_lll_i32..bcGTE_iil_f64: Emitter.SETcc(setge, rcx);
            end;

            Emitter.Store_Int_Result(bc_instr.Args[2], rcx);
          end;

        bcADD_lll_i32..bcADD_iil_f64:
          begin
            Emitter.Load_Int_Operand(bc_instr.Args[0], rax);
            Emitter.Load_Int_Operand(bc_instr.Args[1], rcx);
            Emitter.ADD_Reg_Reg(rax, rcx);
            Emitter.Store_Int_Result(bc_instr.Args[2], rax);
          end;

        bcMUL_lll_i32..bcMUL_iil_f64:
          begin
            Emitter.Load_Int_Operand(bc_instr.Args[0], rax);
            Emitter.Load_Int_Operand(bc_instr.Args[1], rcx);
            Emitter.IMUL_Reg_Reg(rax, rcx);
            Emitter.Store_Int_Result(bc_instr.Args[2], rax);
          end;

        bcINC_i64,bcINC_i32:
          begin
            if (bc_instr.Args[1].Pos = mpImm) and (bc_instr.Args[1].Data.i32 = 1) then
            begin
              case arg_type of
                xtInt8, xtUInt8:   Emitter.INC_Mem8(rbx, bc_instr.Args[0].Data.Addr);
                xtInt16, xtUInt16: Emitter.INC_Mem16(rbx, bc_instr.Args[0].Data.Addr);
                xtInt32, xtUInt32: Emitter.INC_Mem32(rbx, bc_instr.Args[0].Data.Addr);
                xtInt64, xtUInt64: Emitter.INC_Mem64(rbx, bc_instr.Args[0].Data.Addr);
              end;
            end else
            begin
              Emitter.Load_Int_Operand(bc_instr.Args[0], rax);
              Emitter.Load_Int_Operand(bc_instr.Args[1], rcx);
              Emitter.ADD_Reg_Reg(rax, rcx);
              Emitter.Store_Int_Result(bc_instr.Args[2], rax);
            end;
          end;


        bcSUB_lll_i32..bcSUB_iil_f64:
          begin
            Emitter.Load_Int_Operand(bc_instr.Args[0], rax);
            Emitter.Load_Int_Operand(bc_instr.Args[1], rcx);
            Emitter.SUB_Reg_Reg(rax, rcx);
            Emitter.Store_Int_Result(bc_instr.Args[2], rax);
          end;

        bcDIV_lll_i32..bcDIV_iil_f64:
          begin
            Emitter.Load_Int_Operand(bc_instr.Args[0], rax);
            Emitter.Load_Int_Operand(bc_instr.Args[1], rcx);
            Emitter.CQO;
            Emitter.IDIV_Reg(rcx);
            Emitter.Store_Int_Result(bc_instr.Args[2], rax);
          end;

        bcMOD_lll_i32..bcMOD_iil_f64:
          begin
            Emitter.Load_Int_Operand(bc_instr.Args[0], rax);
            Emitter.Load_Int_Operand(bc_instr.Args[1], rcx);
            Emitter.CQO;
            Emitter.IDIV_Reg(rcx);
            Emitter.Store_Int_Result(bc_instr.Args[2], rdx);
          end;

        bcBND_lll_i32..bcBND_iil_u64:
          begin
            Emitter.Load_Int_Operand(bc_instr.Args[0], rax);
            Emitter.Load_Int_Operand(bc_instr.Args[1], rcx);
            Emitter.AND_Reg_Reg(rax,rcx);
            Emitter.Store_Int_Result(bc_instr.Args[2], rax);
          end;
        bcBOR_lll_i32..bcBOR_iil_u64:
          begin
            Emitter.Load_Int_Operand(bc_instr.Args[0], rax);
            Emitter.Load_Int_Operand(bc_instr.Args[1], rcx);
            Emitter.OR_Reg_Reg(rax,rcx);
            Emitter.Store_Int_Result(bc_instr.Args[2], rax);
          end;
        bcXOR_lll_i32..bcXOR_iil_u64:
          begin
            Emitter.Load_Int_Operand(bc_instr.Args[0], rax);
            Emitter.Load_Int_Operand(bc_instr.Args[1], rcx);
            Emitter.XOR_Reg_Reg(rax,rcx);
            Emitter.Store_Int_Result(bc_instr.Args[2], rax);
          end;

        // compunded FMAD
        bcFMAD_d32_64, bcFMAD_d32_32, bcFMAD_d64_64, bcFMAD_d64_32:
          begin
            if bc_instr.Args[1].Data.i32 in [1,2,4,8] then
            begin
              Emitter.Load_Int_Operand(bc_instr.Args[2], rax);
              Emitter.Load_Int_Operand(bc_instr.Args[0], rcx);
              Emitter.LEA_Reg_Mem(rdx, rax, rcx, bc_instr.Args[1].Data.i32);

              if bc_instr.Args[3].BaseType in [xtSingle, xtDouble] then
              begin
                reg1 := XMMAllocator.GetReg(bc_instr.Args[3]);
                if bc_instr.Args[3].BaseType = xtSingle then
                  Emitter.MOVSS_XMM_Mem(reg1, rdx, 0)
                else
                  Emitter.MOVSD_XMM_Mem(reg1, rdx, 0);
                XMMAllocator.SetResult(reg1, bc_instr.Args[3]);
                //Emitter.Store_Float_Result(bc_instr.Args[3], reg1);
              end else
              begin
                Emitter.MOV_Reg_Mem_i64(rax, rdx, 0);
                Emitter.Store_Int_Result(bc_instr.Args[3], rax);
              end;
            end else
            begin
              Emitter.Load_Int_Operand(bc_instr.Args[2], rax);
              Emitter.Load_Int_Operand(bc_instr.Args[0], rcx);
              Emitter.IMUL_Reg_Reg_Imm32(rdx, rcx, bc_instr.Args[1].Data.i32);
              Emitter.ADD_Reg_Reg(rax, rdx);
              Emitter.MOV_Reg_Mem_i64(rcx, rax, 0);
              Emitter.Store_Int_Result(bc_instr.Args[3], rcx);
            end;
          end;
        bcFMA_i8..bcFMA_u64, bcFMA_imm_i8..bcFMA_imm_u64:
          begin
            if bc_instr.Args[1].Data.i32 in [1,2,4,8] then
            begin // Optimal LEA Path
              Emitter.Load_Int_Operand(bc_instr.Args[2], rax); // Base -> RAX
              Emitter.Load_Int_Operand(bc_instr.Args[0], rcx); // Index -> RCX
              Emitter.LEA_Reg_Mem(rdx, rax, rcx, bc_instr.Args[1].Data.i32); // Final Address -> RDX
              Emitter.Store_Int_Result(bc_instr.Args[3], rdx); // Store the calculated address
            end
            else
            begin // Fallback IMUL/ADD Path
              Emitter.Load_Int_Operand(bc_instr.Args[2], rax); // Base -> RAX
              Emitter.Load_Int_Operand(bc_instr.Args[0], rcx); // Index -> RCX
              Emitter.IMUL_Reg_Reg_Imm32(rdx, rcx, bc_instr.Args[1].Data.i32); // Offset -> RDX
              Emitter.ADD_Reg_Reg(rax, rdx); // Final Address -> RAX
              Emitter.Store_Int_Result(bc_instr.Args[3], rax); // Store the calculated address
            end;
          end;
        bcDREF_32, bcDREF_64:
          begin
            Emitter.Load_Int_Operand(bc_instr.Args[1], rax);
            Emitter.MOV_Reg_Mem_i64(rcx, rax, 0);
            Emitter.Store_Int_Result(bc_instr.Args[0], rcx);
          end;
        else
          begin
            {$IFDEF VERBOSE}
            WriteLn('JIT Bail-out (SIGNED): Unhandled instruction type or opcode: ', bc_instr.Code);
            {$ENDIF}
            FreeExecutableMemory(ExecMem, TotalSize);
            Exit(nil);
          end;
      end;
    end
    else if arg_type in [xtSingle, xtDouble] then
    begin
      case bc_instr.Code of

        // MOVH: Move to Heap (Address in GPR, Value in XMM)
        bcMOVH_f32_f32_ll, bcMOVH_f32_f32_li,
        bcMOVH_f64_f64_ll, bcMOVH_f64_f64_li,
        bcMOVH_f32_f64_ll, bcMOVH_f32_f64_li,
        bcMOVH_f64_f32_ll, bcMOVH_f64_f32_li:
        begin
          ptr_arg := bc_instr.Args[0];
          val_arg := bc_instr.Args[1];
          val_type := BaseJITType(val_arg.BaseType);
          dest_type := BaseJITType(ptr_arg.BaseType);

          // 1. Get the destination address into a GPR.
          ptr_arg.BaseType:=xtInt;
          Emitter.Load_Int_Operand(ptr_arg, rax);
          ptr_arg.BaseType := dest_type;

          // 2. Get the source value into an allocated XMM register.
          //    We use xmm4 as a scratch register in case a load is needed.
          reg1 := XMMAllocator.GetReg(val_arg);

          // 3. Handle type conversion if necessary.
          if val_type <> dest_type then
          begin
            // Allocate a new register for the conversion result.
            reg2 := XMMAllocator.Allocate();
            if dest_type = xtSingle then
              Emitter.CVTSD2SS_XMM_XMM(reg2, reg1)
            else
              Emitter.CVTSS2SD_XMM_XMM(reg2, reg1);
            // The result is now in reg2. Store it.
            Emitter.Store_Float_Result_To_Addr(ptr_arg, reg2, rax);
          end else
          begin
            // No conversion needed. Store the value from reg1 directly.
            Emitter.Store_Float_Result_To_Addr(ptr_arg, reg1, rax);
          end;
        end;

        // MOVH: integer to float and move to heap
        bcMOVH_f32_i8_ll,  bcMOVH_f32_i8_li,
        bcMOVH_f32_i16_ll, bcMOVH_f32_i16_li,
        bcMOVH_f32_i32_ll, bcMOVH_f32_i32_li,
        bcMOVH_f32_i64_ll, bcMOVH_f32_i64_li,
        bcMOVH_f64_i8_ll,  bcMOVH_f64_i8_li,
        bcMOVH_f64_i16_ll, bcMOVH_f64_i16_li,
        bcMOVH_f64_i32_ll, bcMOVH_f64_i32_li,
        bcMOVH_f64_i64_ll, bcMOVH_f64_i64_li:
          begin
            ptr_arg := bc_instr.Args[0];
            val_arg := bc_instr.Args[1];
            dest_type := BaseJITType(ptr_arg.BaseType);

            // 1. Load integer value and destination address into GPRs.
            ptr_arg.BaseType:=xtInt;
            Emitter.Load_Int_Operand(ptr_arg, rdx); // Dest address -> rdx
            Emitter.Load_Int_Operand(val_arg, rax); // Source integer -> rax
            ptr_arg.BaseType := dest_type;

            // 2. Allocate an XMM register for the conversion.
            reg1 := XMMAllocator.Allocate();
            // 3. Perform integer-to-float conversion.
            if dest_type = xtSingle then
              Emitter.CVTSI2SS_XMM_Reg64(reg1, rax)
            else
              Emitter.CVTSI2SD_XMM_Reg(reg1, rax);

            // 4. Store the converted float value to the address in rdx.
            Emitter.Store_Float_Result_To_Addr(ptr_arg, reg1, rdx);
          end;

        // MOV: Float-to-float move
        bcMOV_f32_f32_ll, bcMOV_f32_f32_li,
        bcMOV_f64_f64_ll, bcMOV_f64_f64_li:
          begin
            reg1 := XMMAllocator.GetReg(bc_instr.Args[1]);
            XMMAllocator.SetResult(reg1, bc_instr.Args[0]);
          end;

        // MOV: Float-to-float move with conversion
        bcMOV_f32_f64_ll, bcMOV_f32_f64_li,
        bcMOV_f64_f32_ll, bcMOV_f64_f32_li:
          begin
            // 1. Get the source value.
            reg1 := XMMAllocator.GetReg(bc_instr.Args[1]);
            // 2. Allocate a new register for the conversion result.
            reg2 := XMMAllocator.Allocate();
            // 3. Perform conversion.
            if BaseJITType(bc_instr.Args[0].BaseType) = xtSingle then
              Emitter.CVTSD2SS_XMM_XMM(reg2, reg1)
            else
              Emitter.CVTSS2SD_XMM_XMM(reg2, reg1);
            // 4. Assign the result.
            XMMAllocator.SetResult(reg2, bc_instr.Args[0]);
          end;

        // float := signed int
        bcMOV_f32_i8_ll,  bcMOV_f32_i8_li,
        bcMOV_f32_i16_ll, bcMOV_f32_i16_li,
        bcMOV_f32_i32_ll, bcMOV_f32_i32_li,
        bcMOV_f64_i8_ll,  bcMOV_f64_i8_li,
        bcMOV_f64_i16_ll, bcMOV_f64_i16_li,
        bcMOV_f64_i32_ll, bcMOV_f64_i32_li,
        bcMOV_f32_i64_ll, bcMOV_f32_i64_li,
        bcMOV_f64_i64_ll, bcMOV_f64_i64_li:
          begin
            // 1. Load the source integer into a GPR.
            Emitter.Load_Int_Operand(bc_instr.Args[1], rax);
            // 2. Allocate an XMM register for the result.
            reg1 := XMMAllocator.Allocate();

            // 3. Perform conversion.
            if arg_type = xtSingle then
              Emitter.CVTSI2SS_XMM_Reg64(reg1, rax)
            else
              Emitter.CVTSI2SD_XMM_Reg(reg1, rax);
            // 4. Assign the result.
            XMMAllocator.SetResult(reg1, bc_instr.Args[0]);
          end;

        bcEQ_lll_i32..bcLTE_iil_f64: // All float comparison opcodes
          begin
            // 1. Get operands into XMM registers.
            reg1 := XMMAllocator.GetReg(bc_instr.Args[0]);
            reg2 := XMMAllocator.GetReg(bc_instr.Args[1]);

            // 2. Compare registers (this sets CPU flags).
            if arg_type = xtSingle then
              Emitter.UCOMISS_XMM_XMM(reg2, reg1)
            else
              Emitter.UCOMISD_XMM_XMM(reg2, reg1);

            // 3. Use SETcc to write a 0 or 1 byte into a GPR based on flags.
            // We use rax as the destination for the boolean result.
            //Emitter.XOR_Reg_Reg(rax, rax); // Zero out rax before SETcc
            case code of
              bcEQ_lll_i32..bcEQ_iil_f64:   Emitter.SETcc(sete, rax);
              bcNE_lll_i32..bcNE_iil_f64:   Emitter.SETcc(setne, rax);
              bcLT_lll_i32..bcLT_iil_f64:   Emitter.SETcc(setb, rax);
              bcGT_lll_i32..bcGT_iil_f64:   Emitter.SETcc(seta, rax);
              bcLTE_lll_i32..bcLTE_iil_f64: Emitter.SETcc(setbe, rax);
              bcGTE_lll_i32..bcGTE_iil_f64: Emitter.SETcc(setae, rax);
            end;

            // 4. Store the integer result (0 or 1) back to memory.
            Emitter.Store_Int_Result(bc_instr.Args[2], rax);
          end;

        // add
        bcADD_lll_i32..bcDIV_iil_f64:
          begin
            // 1. Get the first operand into a register. This will also be the destination.
            reg1 := XMMAllocator.GetReg(bc_instr.Args[0]);

            // 2. Perform the operation. Check if the second operand is a memory location
            //    to use more efficient register-memory instructions.
            // do search though in registers, we might have it...
            reg2 := XMMAllocator.GetReg(bc_instr.Args[1],[reg1]);

            // 3. delayed ensure we dont modify reg1, a true temp
            if (bc_instr.Args[0].Pos = mpLocal) and (bc_instr.Args[0].Data.Addr <> bc_instr.Args[2].Data.Addr) then
            begin
              reg3 := XMMAllocator.GetFreeScratch(False, [reg1, reg2]);
              Emitter.MOVSD_XMM_XMM(reg3, reg1);
              reg1 := reg3;
            end;

            case code of
              bcADD_lll_i32..bcADD_iil_f64: Emitter.ADDSX_XMM_XMM(reg1, reg2, arg_type);
              bcSUB_lll_i32..bcSUB_iil_f64: Emitter.SUBSX_XMM_XMM(reg1, reg2, arg_type);
              bcMUL_lll_i32..bcMUL_iil_f64: Emitter.MULSX_XMM_XMM(reg1, reg2, arg_type);
              bcDIV_lll_i32..bcDIV_iil_f64: Emitter.DIVSX_XMM_XMM(reg1, reg2, arg_type);
            end;

            // 3. The result is in reg1. Assign it to the destination variable.
            XMMAllocator.SetResult(reg1, bc_instr.Args[2]);

            XMMAllocator.ClearAllConst();
          end;

        bcDREF_32: // Note: Assumes DREF on float types is always 32-bit
          begin
            // 1. Load the address from the source operand into a GPR.
            Emitter.MOV_Reg_Mem_i64(rax, rbx, bc_instr.Args[1].Data.Addr);
            // 2. Allocate a destination XMM register.
            reg1 := XMMAllocator.Allocate();
            // 3. Load the float value from the address in rax into the XMM register.
            Emitter.MOVSS_XMM_Mem(reg1, rax, 0);
            // 4. Assign the result.
            XMMAllocator.SetResult(reg1, bc_instr.Args[0]);
          end;

        bcDREF_64: // Note: Assumes DREF on float types is always 64-bit
          begin
            // 1. Load the address from the source operand into a GPR.
            Emitter.MOV_Reg_Mem_i64(rax, rbx, bc_instr.Args[1].Data.Addr);
            // 2. Allocate a destination XMM register.
            reg1 := XMMAllocator.Allocate();
            // 3. Load the float value from the address in rax into the XMM register.
            Emitter.MOVSD_XMM_Mem(reg1, rax, 0);
            // 4. Assign the result.
            XMMAllocator.SetResult(reg1, bc_instr.Args[0]);
          end;
      else
        begin
          {$IFDEF VERBOSE}
          WriteLn('JIT Bail-out (FLOAT): Unhandled instruction type or opcode: ', bc_instr.Code);
          {$ENDIF}
          FreeExecutableMemory(ExecMem, 0);
          Exit(nil);
        end;
      end;
    end else
    begin
      {$IFDEF VERBOSE}
      WriteLn('JIT Bail-out (OTHER): Unhandled instruction type or opcode: ', bc_instr.Code);
      {$ENDIF}
      FreeExecutableMemory(ExecMem, 0);
      Exit(nil);
    end;
  end;

  Labels[Count] := Emitter.CurrentPosition;

  XMMAllocator.SpillAllDirty;
  Emitter.Epilogue;

  // PASS 2: Backpatching
  // Use a standard counter loop instead of for..in
  for j := 0 to High(Patches) do
  begin
    patch := Patches[j];

    if (patch.TargetBCIndex < 0) or (patch.TargetBCIndex > Count) then
    begin
      {$IFDEF VERBOSE}
      WriteLn('JIT Error: Jump target is outside the compiled trace: ', patch.TargetBCIndex);
      {$ENDIF}
      FreeExecutableMemory(ExecMem, 0);
      Exit(nil);
    end;

    targetAddress := Labels[patch.TargetBCIndex];
    jumpInstructionEnd := PByte(patch.PatchLocation) + 4;
    relativeOffset := PtrInt(targetAddress) - PtrInt(jumpInstructionEnd);
    patch.PatchLocation^ := relativeOffset;
  end;

  // --- Finalize and Return ---
  TotalSize := Emitter.CurrentPosition - ExecMem;
  SetMemoryExecutable(ExecMem, TotalSize);
  Result := ExecMem;
end;

// Deallocates memory created by EmitCodeBlock
// This should be ran on each opcode of bcSUPER and bcHOTLOOP
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
  for i:=0 to BC.Code.High do
    if (BC.Code.Data[i].Code = bcSUPER) or (BC.Code.Data[i].Code = bcHOTLOOP) then
      Self.FreeCodeBlock(Pointer(BC.Code.Data[i].Args[4].Data.Addr), 0);
end;

//----------------------------------------------------------------------------
// Opcodes/bytecodes that can actually be merged into a single executable
// None of these opcodes can rely on methods that can not be inlined.
// This means i.'s for simple expressions, and no handling for jumps
//----------------------------------------------------------------------------
function CanJITSuper(Arg: TBytecodeInstruction): Boolean;
var
  code: Int32;
  isBinary: Boolean;
begin
  code := Ord(Arg.Code);
  isBinary := InRange(Ord(Code), Ord(bcADD_lll_i32), Ord(bcBOR_iil_u64));

  Result := isBinary or (InRange(Code, Ord(bcMOV_i8_i8_ll), Ord(bcMOV_f64_f64_li)));
  Result := Result   or (InRange(Code, Ord(bcMOVH_i8_i8_ll), Ord(bcMOVH_f64_f64_li)));
  Result := Result   or (InRange(Code, Ord(bcINC_i32), Ord(bcFMAD_d32_32)));

  // deref uses Move-function, so that's a no-go
  Result := Result and (Arg.Code <> bcDREF);
  // power for floats uses Power-function, also a no-go
  Result := Result and not(InRange(Code, Ord(bcPOW_lll_i32), Ord(bcPOW_iil_f64)) and (Arg.Args[2].BaseType in XprFloatTypes));
  // modulo for floats uses mod-function, also a no-go
  Result := Result and not(InRange(Code, Ord(bcMOD_lll_i32), Ord(bcMOD_iil_f64)) and (Arg.Args[2].BaseType in XprFloatTypes));

  //Result := Result and not(InRange(Code, Ord(bcEQ_lll_i32), Ord(bcLTE_iil_f64)));
  // let's not JIT binary that is over the native computation size
  // hard to tell what FPC does in these cases, may produce calls and jumps.
  {$IFDEF CPU32}
  if Result and isBinary and (Arg.Args[2].BaseType in XprOrdinalTypes) and (XprTypeSize[Arg.Args[0].BaseType] > SizeOf(SizeInt)) then
    Result := False;
  {$ENDIF}
end;

function CanJIT_x86_64(Arg: TBytecodeInstruction; AllowJumps: Boolean): Boolean;
var
  code: Int32;
  isBinary: Boolean;
begin
  code := Ord(Arg.Code);
  isBinary := InRange(Ord(Code), Ord(bcADD_lll_i32), Ord(bcBOR_iil_u64));

  Result := isBinary or (InRange(Code, Ord(bcMOV_i8_i8_ll), Ord(bcMOV_f64_f64_li)));
  Result := Result   or (InRange(Code, Ord(bcMOVH_i8_i8_ll), Ord(bcMOVH_f64_f64_li)));
  Result := Result   or (InRange(Code, Ord(bcINC_i32), Ord(bcFMAD_d32_32)));

  // deref uses Move-function, so that's a no-go
  Result := Result and (Arg.Code <> bcDREF);
  // power for floats uses Power-function, also a no-go
  Result := Result and not(InRange(Code, Ord(bcPOW_lll_i32), Ord(bcPOW_iil_f64)) and (Arg.Args[2].BaseType in XprFloatTypes));
  // modulo for floats uses mod-function, also a no-go
  Result := Result and not(InRange(Code, Ord(bcMOD_lll_i32), Ord(bcMOD_iil_f64)) and (Arg.Args[2].BaseType in XprFloatTypes));

  // branching
  if AllowJumps then
    Result := Result or (Arg.Code = bcJZ) or (Arg.Code = bcJNZ) or (Arg.Code = bcRELJMP);
end;

//----------------------------------------------------------------------------
// Copies exactly Count opcode blocks
// totalSize RETURNS the number of bytes copied + 1 for the RET.
//----------------------------------------------------------------------------


function TInterpreter.EmitCodeBlock(CodeList: PBytecodeInstruction;
                                    Translation: TTranslateArray;
                                    Count: Int32;
                                var TotalSize: SizeInt): Pointer;
var
  offset: NativeUInt;
  dataPtrs: array of record start_, stop_: Pointer; end;
  oldProt: DWORD;
  j: Integer;
  code: EBytecode;
  ExecMem: Pointer;
  RetInstructionSize: Byte;
begin
  {$IFDEF CPUAARCH64}
  RetInstructionSize := 4; // 4 bytes for ARM64 RET instruction
  {$ELSE}
  // Assume x86 or x86-64
  RetInstructionSize := 1; // 1 byte for x86/x64 RET instruction ($C3)
  {$ENDIF}

  // 1) Build temp table of (start,stop) pointers
  SetLength(dataPtrs, Count);
  TotalSize := RetInstructionSize;

  for j := 0 to Count - 1 do
  begin
    code := (CodeList + j)^.Code;

    dataPtrs[j].start_ := Pointer(Translation[code]);
    dataPtrs[j].stop_  := Pointer(Translation[EBytecode(Ord(code) + 1)]);
    Inc(TotalSize, NativeUInt(dataPtrs[j].stop_) - NativeUInt(dataPtrs[j].start_));
  end;

  // 2) Allocate memory
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
    Move(dataPtrs[j].start_^, Pointer(NativeUInt(ExecMem) + offset)^,
         NativeUInt(dataPtrs[j].stop_) - NativeUInt(dataPtrs[j].start_));

    Inc(offset, NativeUInt(dataPtrs[j].stop_) - NativeUInt(dataPtrs[j].start_));
  end;

  // 4) Add the final RET instruction (CPU-Specific) ---
  {$IFDEF CPUAARCH64}{For ARM64}
  PDword(NativeUInt(ExecMem) + offset)^ := $D65F03C0;
  {$ELSE} {For x86/x86-64}
  PByte(NativeUInt(ExecMem) + offset)^ := $C3;
  {$ENDIF}
  Inc(offset, RetInstructionSize);

  // 5. Change memory protection from Writable to Executable.
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
      //WriteLn(i,': ', BC.Code.Data[i].Args[0].Data.i32, ' = ', n-i-3);
      // type conversions can cause off by one in the comparator
      // disqualifying it from hotlooping
      Result := (BC.Code.Data[i].Args[0].Data.i32 = n-i-3);
    end;
  end;
begin
  i := 0;
  while i <= BC.Code.High do
  begin
    // Check if current opcode is eligible for fusion
    if CanJITSuper(BC.Code.Data[i]) and (BC.Settings.Data[i].JIT in [1,{$IFNDEF CPUX86_64}2,{$ENDIF}3]) then
    begin
      n := i;

      // Find how many eligible opcodes follow
      while (i <= BC.Code.High) and CanJITSuper(BC.Code.Data[i]) and
            (BC.Settings.Data[i].JIT in [1,{$IFNDEF CPUX86_64}2,{$ENDIF}3]) do
        Inc(i);

      // control flow, so what comes before is likely cmp operation
      // we dont touch that as that might be a hotloop found in next iteration
      if (i - n >= 2) and (BC.Code.Data[i].Code = bcJZ) then
        Dec(i);

      // Only generate a super-instruction if 2 or more instructions can be merged
      if (i - n >= MIN_JIT_OPCODE_COUNT) or isLoop() then
      begin
        {$IFDEF VERBOSE}
        //WriteLn(n,'..',i,' JIT! Hot:', isLoop());
        {$ENDIF}
        if isLoop() then
        begin
          execMem := EmitCodeBlock(@BC.Code.Data[n-2], Translation, 1, total);
          if execMem = nil then Break;
          BC.Code.Data[n-2].Code := bcHOTLOOP; // body in n+2
          BC.Code.Data[n-2].Args[4].Data.Addr := NativeInt(execMem);
        end;

        {$IFDEF VERBOSE}
        //WriteLn('NAIVE: ', n,'..',i-1);
        {$ENDIF}
        execMem := EmitCodeBlock(@BC.Code.Data[n], Translation, i - n, total);
        if execMem = nil then Break;
        //for k:=n to i-1 do
        //  WriteLn(BC.Code.Data[k].Code);
        //for k:=0 to total-1 do
        //  Write(IntToHex(PByte(execMem+k)^));
        //WriteLn();
        BC.Code.Data[n].Code := bcSUPER;
        BC.Code.Data[n].Args[4].Data.Addr := NativeInt(execMem); // Store ptr to code block
        BC.Code.Data[n].nArgs := 4;
      end;
    end else
      Inc(i);
  end;
end;


procedure TInterpreter.x86_64_Compile(var BC: TBytecode; AllowJumps: Boolean);
var
  i, trace_start, targetIdx, trace_len, k: Integer;
  loopDepth,totalSize: SizeInt;
  execMem: Pointer;
  ValidRelJmpLocations: specialize TArrayList<PtrUInt>;
  current_instr: TBytecodeInstruction;
  found: Boolean;
begin
  i := 0;

  while i <= BC.Code.High do
  begin
    if CanJIT_x86_64(BC.Code.Data[i], False) and (BC.Settings.Data[i].JIT in [2,3]) then
    begin
      trace_start := i;
      ValidRelJmpLocations.Init([]);
      loopDepth := 0;

      // Find the END of the trace
      while (i <= BC.Code.High) and CanJIT_x86_64(BC.Code.Data[i], AllowJumps) and
            (BC.Settings.Data[i].JIT in [2,3]) do
      begin
        current_instr := BC.Code.Data[i];
        if not AllowJumps then
        begin
          Inc(i);
          continue;
        end;

        // 1. If we see a conditional jump
        // record where its corresponding RELJMP should be (LOOP CHECK).
        if current_instr.Code in [bcJZ, bcJNZ] then
        begin
          targetIdx := i + current_instr.Args[1].Data.i32;
          ValidRelJmpLocations.Add(targetIdx);

          // heuristic to detect loop
          if (targetIdx > 0) and (BC.code.Data[targetIdx].Code = bcRELJMP) then
            Inc(loopDepth);
        end;

        // 2. If we see a RELJMP, check if it's one we are expecting.
        if current_instr.Code = bcRELJMP then
        begin
          found := False;
          for k := 0 to ValidRelJmpLocations.High() do
            if ValidRelJmpLocations.Data[k] = i then begin
              found := True;
              break;
            end;

          if not found then break;
          Dec(loopDepth);

          // if we have finalized a complex loop we can end the trace
          // at this point we only risk starting another trace into
          // an invalid loop
          if loopDepth = 0 then
          begin
            Inc(i,1); // RELJMP + 1 (target);
            break;
          end;
        end;

        Inc(i);
      end;

      trace_len := i - trace_start;

      // Compile the trace
      if (trace_len >= MIN_JIT_OPCODE_COUNT) then
      begin
        execMem := EmitJITBlock(@BC.Code.Data[trace_start], trace_len, totalSize, True);
        if execMem <> nil then
        begin
          {$IFDEF VERBOSE}
          WriteLn('SUCCESS (JIT x64): Compiled trace from BC index ', trace_start, ' to ', i - 1);

          for k:=0 to totalSize-1 do
            Write(IntToHex(PByte(execMem+k)^));
          Writeln();
          {$ENDIF}
          BC.Code.Data[trace_start].Code := bcJIT;
          BC.Code.Data[trace_start].Args[4].Data.Addr := NativeInt(execMem);
          BC.Code.Data[trace_start].nArgs := trace_len;
        end
        else
        begin
          {$IFDEF VERBOSE}
          WriteLn('INFO (JIT x64): Bailed out on trace ', trace_start, '..', i - 1);
          {$ENDIF}
          i := trace_start + 1;
        end;
      end;
    end
    else
    begin
      Inc(i);
    end;
  end;
end;         
