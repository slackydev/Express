function TInterpreter.EmitJITBlock(CodeList: PBytecodeInstruction; Count: Int32;
                                var TotalSize: SizeInt; CanJMP: Boolean = False): Pointer;
type
  TPatch = record PatchLocation: PInt32; TargetBCIndex: Int32; end;
  TPatchList = array of TPatch;
  TLabelMap  = array of PByte;

  function IsUnsigned(bt: EExpressBaseType): Boolean; inline;
  begin Result := bt in [xtUInt8, xtUInt16, xtUInt32, xtUInt64]; end;

  function DestIsA(const bc: TBytecodeInstruction): Boolean; inline;
  begin
    Result := (bc.Args[2].Pos = mpLocal) and (bc.Args[0].Pos = mpLocal)
          and (bc.Args[2].Data.Addr = bc.Args[0].Data.Addr);
  end;

  function DestIsB(const bc: TBytecodeInstruction): Boolean; inline;
  begin
    Result := (bc.Args[2].Pos = mpLocal) and (bc.Args[1].Pos = mpLocal)
          and (bc.Args[2].Data.Addr = bc.Args[1].Data.Addr);
  end;

  procedure AddPatch(var Patches: TPatchList; loc: PInt32; target: Int32);
  begin
    SetLength(Patches, Length(Patches)+1);
    Patches[High(Patches)].PatchLocation := loc;
    Patches[High(Patches)].TargetBCIndex := target;
  end;

var
  ExecMem : PByte;
  Emitter : TJitEmitter;
  GPRAlloc: TGPRRegAllocator;
  XMMAlloc: TXMMRegisterAllocator;
  SpillLocations: specialize TArrayList<PtrInt>;
  Patches : TPatchList;
  Labels  : TLabelMap;
  i, j    : Integer;
  bc      : TBytecodeInstruction;
  code    : EBytecode;
  arg_type: EExpressBaseType;
  loc     : PInt32;
  patch   : TPatch;
  tgt, jend : PByte;
  rel     : PtrInt;
  gA, gB, gC, gDest: EReg;
  excl         : Byte;
  reg1, reg2, reg3      : EXMMReg;
  ptr_arg, val_arg      : TOperand;
  val_type, dest_type   : EExpressBaseType;

begin
  ExecMem := AllocateExecutableMemory(Count * 100 + 256);
  if ExecMem = nil then Exit(nil);

  SpillLocations.Init([]);
  for i := 0 to Count-1 do begin
    bc := (CodeList+i)^;
    if bc.Code in [bcJZ, bcJNZ] then begin
      j := bc.Args[1].Data.i32;
      SpillLocations.Add(i + j + 1);
      bc := (CodeList + i + j)^;
      if bc.Code = bcRELJMP then
        SpillLocations.Add((i+j) + bc.Args[0].Data.Addr + 1)
      else
        SpillLocations.Add(i);
    end;
  end;

  Emitter.Init(ExecMem);
  SetLength(Patches, 0);
  SetLength(Labels, Count+1);

  Emitter.Preamble;

  XMMAlloc.Init(@Emitter);
  XMMAlloc.InvalidateAll;
  XMMAlloc.AnalyzeTrace(CodeList, Count);

  GPRAlloc.Init(@Emitter);
  GPRAlloc.AnalyzeTrace(CodeList, Count);

  for i := 0 to Count-1 do begin
    for j := 0 to SpillLocations.High() do
      if SpillLocations.Data[j] = i then begin
        GPRAlloc.InvalidateAll;
        XMMAlloc.InvalidateAll;
        Break;
      end;

    Labels[i] := Emitter.CurrentPosition;
    bc       := (CodeList+i)^;
    code     := bc.Code;
    arg_type := BaseJITType(bc.Args[0].BaseType);

    if CanJMP and ((code = bcRELJMP) or (code = bcJZ) or (code = bcJNZ)) then begin
      case code of
        bcJZ: begin
          gA := GPRAlloc.GetReg(bc.Args[0]);
          GPRAlloc.SpillAll;
          XMMAlloc.SpillAllDirty;
          Emitter.TEST_Reg_Reg(gA, gA);
          loc := Emitter.Jcc_Rel32(je);
          AddPatch(Patches, loc, i + bc.Args[1].Data.i32 + 1);
        end;
        bcJNZ: begin
          gA := GPRAlloc.GetReg(bc.Args[0]);
          GPRAlloc.SpillAll;
          XMMAlloc.SpillAllDirty;
          Emitter.TEST_Reg_Reg(gA, gA);
          loc := Emitter.Jcc_Rel32(jne);
          AddPatch(Patches, loc, i + bc.Args[1].Data.i32 + 1);
        end;
        bcRELJMP: begin
          GPRAlloc.SpillAll;
          XMMAlloc.SpillAllDirty;
          loc := Emitter.JMP_Rel32;
          AddPatch(Patches, loc, i + bc.Args[0].Data.i32 + 1);
        end;
      end;
    end
    else if arg_type in [xtInt8, xtInt16, xtInt32, xtInt64,
                         xtUInt8, xtUInt16, xtUInt32, xtUInt64] then
    begin
      case code of

        bcMOV_i8_i8_ll..bcMOV_u64_u64_li: begin
          gA    := GPRAlloc.GetReg(bc.Args[1]);
          gDest := GPRAlloc.GetResultReg(bc.Args[0], GPRAlloc.MaskOf(gA));
          if gDest <> gA then Emitter.MOV_Reg_Reg(gDest, gA);
          GPRAlloc.SetResult(gDest, bc.Args[0]);
        end;

        bcMOVH_i8_i8_ll..bcMOVH_u64_u64_li: begin
          gA := GPRAlloc.GetReg(bc.Args[0]);
          gB := GPRAlloc.GetReg(bc.Args[1], GPRAlloc.MaskOf(gA));
          Emitter.Store_Int_Result_To_Addr(bc.Args[1], gB, gA);
        end;

        bcEQ_lll_i32..bcLTE_iil_f64: begin
          gA := GPRAlloc.GetReg(bc.Args[0]);
          gB := GPRAlloc.GetReg(bc.Args[1], GPRAlloc.MaskOf(gA));
          Emitter.CMP_Reg_Reg(gA, gB);

          excl  := GPRAlloc.MaskOf(gA) or GPRAlloc.MaskOf(gB);
          gDest := GPRAlloc.GetResultReg(bc.Args[2], excl);

          case code of
            bcEQ_lll_i32..bcEQ_iil_f64:   Emitter.SETcc(sete,  gDest);
            bcNE_lll_i32..bcNE_iil_f64:   Emitter.SETcc(setne, gDest);
            bcLT_lll_i32..bcLT_iil_f64:
              if IsUnsigned(arg_type) then Emitter.SETcc(setb,  gDest)
              else                         Emitter.SETcc(setl,  gDest);
            bcGT_lll_i32..bcGT_iil_f64:
              if IsUnsigned(arg_type) then Emitter.SETcc(seta,  gDest)
              else                         Emitter.SETcc(setg,  gDest);
            bcLTE_lll_i32..bcLTE_iil_f64:
              if IsUnsigned(arg_type) then Emitter.SETcc(setbe, gDest)
              else                         Emitter.SETcc(setle, gDest);
            bcGTE_lll_i32..bcGTE_iil_f64:
              if IsUnsigned(arg_type) then Emitter.SETcc(setae, gDest)
              else                         Emitter.SETcc(setge, gDest);
          end;

          Emitter.MOVZX_Reg_Reg_u8(gDest);
          GPRAlloc.SetResult(gDest, bc.Args[2]);
        end;

        bcADD_lll_i32..bcADD_iil_f64: begin
          if DestIsA(bc) then begin
            gA := GPRAlloc.GetReg(bc.Args[0]);
            gB := GPRAlloc.GetReg(bc.Args[1], GPRAlloc.MaskOf(gA));
            Emitter.ADD_Reg_Reg(gA, gB);
            GPRAlloc.SetResult(gA, bc.Args[2]);
          end else if DestIsB(bc) then begin
            gB := GPRAlloc.GetReg(bc.Args[1]);
            gA := GPRAlloc.GetReg(bc.Args[0], GPRAlloc.MaskOf(gB));
            Emitter.ADD_Reg_Reg(gB, gA);
            GPRAlloc.SetResult(gB, bc.Args[2]);
          end else begin
            gA    := GPRAlloc.GetReg(bc.Args[0]);
            gB    := GPRAlloc.GetReg(bc.Args[1], GPRAlloc.MaskOf(gA));
            excl  := GPRAlloc.MaskOf(gA) or GPRAlloc.MaskOf(gB);
            gDest := GPRAlloc.GetResultReg(bc.Args[2], excl);
            if gDest <> gA then Emitter.MOV_Reg_Reg(gDest, gA);
            Emitter.ADD_Reg_Reg(gDest, gB);
            GPRAlloc.SetResult(gDest, bc.Args[2]);
          end;
        end;

        bcSUB_lll_i32..bcSUB_iil_f64: begin
          if DestIsA(bc) then begin
            gA := GPRAlloc.GetReg(bc.Args[0]);
            gB := GPRAlloc.GetReg(bc.Args[1], GPRAlloc.MaskOf(gA));
            Emitter.SUB_Reg_Reg(gA, gB);
            GPRAlloc.SetResult(gA, bc.Args[2]);
          end else begin
            gA    := GPRAlloc.GetReg(bc.Args[0]);
            gB    := GPRAlloc.GetReg(bc.Args[1], GPRAlloc.MaskOf(gA));
            excl  := GPRAlloc.MaskOf(gA) or GPRAlloc.MaskOf(gB);
            gDest := GPRAlloc.GetResultReg(bc.Args[2], excl);
            if gDest <> gA then Emitter.MOV_Reg_Reg(gDest, gA);
            Emitter.SUB_Reg_Reg(gDest, gB);
            GPRAlloc.SetResult(gDest, bc.Args[2]);
          end;
        end;

        bcMUL_lll_i32..bcMUL_iil_f64: begin
          if DestIsA(bc) then begin
            gA := GPRAlloc.GetReg(bc.Args[0]);
            gB := GPRAlloc.GetReg(bc.Args[1], GPRAlloc.MaskOf(gA));
            Emitter.IMUL_Reg_Reg(gA, gB);
            GPRAlloc.SetResult(gA, bc.Args[2]);
          end else if DestIsB(bc) then begin
            gB := GPRAlloc.GetReg(bc.Args[1]);
            gA := GPRAlloc.GetReg(bc.Args[0], GPRAlloc.MaskOf(gB));
            Emitter.IMUL_Reg_Reg(gB, gA);
            GPRAlloc.SetResult(gB, bc.Args[2]);
          end else begin
            gA    := GPRAlloc.GetReg(bc.Args[0]);
            gB    := GPRAlloc.GetReg(bc.Args[1], GPRAlloc.MaskOf(gA));
            excl  := GPRAlloc.MaskOf(gA) or GPRAlloc.MaskOf(gB);
            gDest := GPRAlloc.GetResultReg(bc.Args[2], excl);
            if gDest <> gA then Emitter.MOV_Reg_Reg(gDest, gA);
            Emitter.IMUL_Reg_Reg(gDest, gB);
            GPRAlloc.SetResult(gDest, bc.Args[2]);
          end;
        end;

        bcDIV_lll_i32..bcDIV_iil_f64: begin
          GPRAlloc.SpillAll;
          GPRAlloc.ForceEvict(rax);
          GPRAlloc.ForceEvict(rcx);
          GPRAlloc.ForceEvict(rdx);

          Emitter.Load_Int_Operand_Full(bc.Args[0], rax);
          Emitter.Load_Int_Operand_Full(bc.Args[1], rcx);
          if IsUnsigned(arg_type) then begin
            Emitter.XOR_Reg_Reg(rdx, rdx);
            Emitter.DIV_Reg(rcx);
          end else begin
            Emitter.CQO;
            Emitter.IDIV_Reg(rcx);
          end;
          GPRAlloc.SetResult(rax, bc.Args[2]);
        end;

        bcMOD_lll_i32..bcMOD_iil_f64: begin
          GPRAlloc.SpillAll;
          GPRAlloc.ForceEvict(rax);
          GPRAlloc.ForceEvict(rcx);
          GPRAlloc.ForceEvict(rdx);

          Emitter.Load_Int_Operand_Full(bc.Args[0], rax);
          Emitter.Load_Int_Operand_Full(bc.Args[1], rcx);
          if IsUnsigned(arg_type) then begin
            Emitter.XOR_Reg_Reg(rdx, rdx);
            Emitter.DIV_Reg(rcx);
          end else begin
            Emitter.CQO;
            Emitter.IDIV_Reg(rcx);
          end;
          GPRAlloc.SetResult(rdx, bc.Args[2]);
        end;

        bcBND_lll_i32..bcBND_iil_u64: begin
          if DestIsA(bc) then begin
            gA := GPRAlloc.GetReg(bc.Args[0]);
            gB := GPRAlloc.GetReg(bc.Args[1], GPRAlloc.MaskOf(gA));
            Emitter.AND_Reg_Reg(gA, gB);
            GPRAlloc.SetResult(gA, bc.Args[2]);
          end else if DestIsB(bc) then begin
            gB := GPRAlloc.GetReg(bc.Args[1]);
            gA := GPRAlloc.GetReg(bc.Args[0], GPRAlloc.MaskOf(gB));
            Emitter.AND_Reg_Reg(gB, gA);
            GPRAlloc.SetResult(gB, bc.Args[2]);
          end else begin
            gA    := GPRAlloc.GetReg(bc.Args[0]);
            gB    := GPRAlloc.GetReg(bc.Args[1], GPRAlloc.MaskOf(gA));
            excl  := GPRAlloc.MaskOf(gA) or GPRAlloc.MaskOf(gB);
            gDest := GPRAlloc.GetResultReg(bc.Args[2], excl);
            if gDest <> gA then Emitter.MOV_Reg_Reg(gDest, gA);
            Emitter.AND_Reg_Reg(gDest, gB);
            GPRAlloc.SetResult(gDest, bc.Args[2]);
          end;
        end;

        bcBOR_lll_i32..bcBOR_iil_u64: begin
          if DestIsA(bc) then begin
            gA := GPRAlloc.GetReg(bc.Args[0]);
            gB := GPRAlloc.GetReg(bc.Args[1], GPRAlloc.MaskOf(gA));
            Emitter.OR_Reg_Reg(gA, gB);
            GPRAlloc.SetResult(gA, bc.Args[2]);
          end else if DestIsB(bc) then begin
            gB := GPRAlloc.GetReg(bc.Args[1]);
            gA := GPRAlloc.GetReg(bc.Args[0], GPRAlloc.MaskOf(gB));
            Emitter.OR_Reg_Reg(gB, gA);
            GPRAlloc.SetResult(gB, bc.Args[2]);
          end else begin
            gA    := GPRAlloc.GetReg(bc.Args[0]);
            gB    := GPRAlloc.GetReg(bc.Args[1], GPRAlloc.MaskOf(gA));
            excl  := GPRAlloc.MaskOf(gA) or GPRAlloc.MaskOf(gB);
            gDest := GPRAlloc.GetResultReg(bc.Args[2], excl);
            if gDest <> gA then Emitter.MOV_Reg_Reg(gDest, gA);
            Emitter.OR_Reg_Reg(gDest, gB);
            GPRAlloc.SetResult(gDest, bc.Args[2]);
          end;
        end;

        bcXOR_lll_i32..bcXOR_iil_u64: begin
          if DestIsA(bc) then begin
            gA := GPRAlloc.GetReg(bc.Args[0]);
            gB := GPRAlloc.GetReg(bc.Args[1], GPRAlloc.MaskOf(gA));
            Emitter.XOR_Reg_Reg(gA, gB);
            GPRAlloc.SetResult(gA, bc.Args[2]);
          end else if DestIsB(bc) then begin
            gB := GPRAlloc.GetReg(bc.Args[1]);
            gA := GPRAlloc.GetReg(bc.Args[0], GPRAlloc.MaskOf(gB));
            Emitter.XOR_Reg_Reg(gB, gA);
            GPRAlloc.SetResult(gB, bc.Args[2]);
          end else begin
            gA    := GPRAlloc.GetReg(bc.Args[0]);
            gB    := GPRAlloc.GetReg(bc.Args[1], GPRAlloc.MaskOf(gA));
            excl  := GPRAlloc.MaskOf(gA) or GPRAlloc.MaskOf(gB);
            gDest := GPRAlloc.GetResultReg(bc.Args[2], excl);
            if gDest <> gA then Emitter.MOV_Reg_Reg(gDest, gA);
            Emitter.XOR_Reg_Reg(gDest, gB);
            GPRAlloc.SetResult(gDest, bc.Args[2]);
          end;
        end;

        bcSHL_lll_i32..bcSHL_iil_u64,
        bcSHR_lll_i32..bcSHR_iil_u64,
        bcSAR_lll_i32..bcSAR_iil_u64: begin
          GPRAlloc.ForceEvict(rcx);

          gA := GPRAlloc.GetReg(bc.Args[0], GPRAlloc.MaskOf(rcx));
          gDest := GPRAlloc.GetResultReg(bc.Args[2], GPRAlloc.MaskOf(rcx));
          if gA <> gDest then Emitter.MOV_Reg_Reg(gDest, gA);

          Emitter.Load_Int_Operand_Full(bc.Args[1], rcx);

          case code of
            bcSHL_lll_i32..bcSHL_iil_u64: Emitter.SHL_Reg_CL(gDest);
            bcSHR_lll_i32..bcSHR_iil_u64: Emitter.SHR_Reg_CL(gDest);
            bcSAR_lll_i32..bcSAR_iil_u64: Emitter.SAR_Reg_CL(gDest);
          end;
          GPRAlloc.SetResult(gDest, bc.Args[2]);
        end;

        bcINC_i32, bcINC_i64: begin
          gA := GPRAlloc.GetReg(bc.Args[0]);
          if (bc.Args[1].Pos = mpImm) and (bc.Args[1].Data.i32 = 1) then
            Emitter.INC_Reg(gA)
          else if bc.Args[1].Pos = mpImm then
            Emitter.ADD_Reg_Imm32(gA, bc.Args[1].Data.i32)
          else begin
            gB := GPRAlloc.GetReg(bc.Args[1], GPRAlloc.MaskOf(gA));
            Emitter.ADD_Reg_Reg(gA, gB);
          end;
          GPRAlloc.SetResult(gA, bc.Args[2]);
        end;

        bcFMA_i8..bcFMA_u64, bcFMA_imm_i8..bcFMA_imm_u64: begin
          // 1. Get inputs. Let the allocator pick any register it wants.
          gA := GPRAlloc.GetReg(bc.Args[2]);
          gB := GPRAlloc.GetReg(bc.Args[0], GPRAlloc.MaskOf(gA));

          excl := GPRAlloc.MaskOf(gA) or GPRAlloc.MaskOf(gB);

          // 2. Allocate the destination register.
          gDest := GPRAlloc.GetResultReg(bc.Args[3], excl);

          // 3. Perform math
          if bc.Args[1].Data.i32 in [1,2,4,8] then begin
            Emitter.LEA_Reg_Mem(gDest, gA, gB, bc.Args[1].Data.i32);
          end else begin
            // We need a scratch for IMUL. RCX is ideal.
            // If RCX is currently holding gA, gB, or gDest, we can't evict it!
            // In a 5-register pool, if 3 are taken, 2 are free. We just ask
            // the allocator for ANY free scratch.
            gC := GPRAlloc.GetScratch(excl or GPRAlloc.MaskOf(gDest));

            Emitter.IMUL_Reg_Reg_Imm32(gC, gB, bc.Args[1].Data.i32);
            Emitter.ADD_Reg_Reg(gC, gA);
            Emitter.MOV_Reg_Reg(gDest, gC);

            // Release the temporary scratch
            GPRAlloc.ClearSlot(gC);
          end;

          GPRAlloc.SetResult(gDest, bc.Args[3]);
        end;

        bcFMAD_d64_64, bcFMAD_d64_32, bcFMAD_d32_64, bcFMAD_d32_32: begin
          // 1. Get inputs. Let allocator pick.
          gA := GPRAlloc.GetReg(bc.Args[2]);
          gB := GPRAlloc.GetReg(bc.Args[0], GPRAlloc.MaskOf(gA));
          excl := GPRAlloc.MaskOf(gA) or GPRAlloc.MaskOf(gB);

          // 2. We need a temporary register for the address calculation.
          gDest := GPRAlloc.GetScratch(excl);

          // 3. Perform math to calculate the address into gDest
          if bc.Args[1].Data.i32 in [1,2,4,8] then begin
            Emitter.LEA_Reg_Mem(gDest, gA, gB, bc.Args[1].Data.i32);
          end else begin
            // We need a SECOND temporary register for the IMUL
            gC := GPRAlloc.GetScratch(excl or GPRAlloc.MaskOf(gDest));

            Emitter.IMUL_Reg_Reg_Imm32(gC, gB, bc.Args[1].Data.i32);
            Emitter.LEA_Reg_Mem(gDest, gA, gC, 1);

            GPRAlloc.ClearSlot(gC); // Release IMUL scratch
          end;

          // 4. Dereference the address in gDest
          if bc.Args[3].BaseType in [xtSingle, xtDouble] then begin
            reg1 := XMMAlloc.Allocate;
            if bc.Args[3].BaseType = xtSingle then Emitter.MOVSS_XMM_Mem(reg1, gDest, 0)
            else                                   Emitter.MOVSD_XMM_Mem(reg1, gDest, 0);
            XMMAlloc.SetResult(reg1, bc.Args[3]);
            //gDest is just a temp address
            GPRAlloc.ClearSlot(gDest);
          end else begin
            // We can reuse gDest to hold the final integer value!
            case BaseJITType(bc.Args[3].BaseType) of
              xtInt8:   Emitter.MOVSX_Reg_Mem_i8(gDest, gDest, 0);
              xtInt16:  Emitter.MOVSX_Reg_Mem_i16(gDest, gDest, 0);
              xtInt32:  Emitter.MOVSXD_Reg_Mem_i32(gDest, gDest, 0);
              xtUInt8:  Emitter.MOVZX_Reg_Mem_i8(gDest, gDest, 0);
              xtUInt16: Emitter.MOVZX_Reg_Mem_i16(gDest, gDest, 0);
              xtUInt32: Emitter.MOV_Reg_Mem_i32(gDest, gDest, 0);
              else      Emitter.MOV_Reg_Mem_i64(gDest, gDest, 0);
            end;
            GPRAlloc.SetResult(gDest, bc.Args[3]);
            // Do NOT ClearSlot(gDest) here, because SetResult claimed it.
          end;
        end;

        bcDREF_32, bcDREF_64: begin
          gA    := GPRAlloc.GetReg(bc.Args[1]);
          gDest := GPRAlloc.GetResultReg(bc.Args[0], GPRAlloc.MaskOf(gA));
          case BaseJITType(bc.Args[0].BaseType) of
            xtInt8:   Emitter.MOVSX_Reg_Mem_i8(gDest, gA, 0);
            xtInt16:  Emitter.MOVSX_Reg_Mem_i16(gDest, gA, 0);
            xtInt32:  Emitter.MOVSXD_Reg_Mem_i32(gDest, gA, 0);
            xtUInt8:  Emitter.MOVZX_Reg_Mem_i8(gDest, gA, 0);
            xtUInt16: Emitter.MOVZX_Reg_Mem_i16(gDest, gA, 0);
            xtUInt32: Emitter.MOV_Reg_Mem_i32(gDest, gA, 0);
            else      Emitter.MOV_Reg_Mem_i64(gDest, gA, 0);
          end;
          GPRAlloc.SetResult(gDest, bc.Args[0]);
        end;

        bcADDR: begin
          gDest := GPRAlloc.GetScratch(0);
          Emitter.MOV_Reg_Imm64(gDest, bc.Args[1].Data.Addr);
          Emitter.ADD_Reg_Reg(gDest, rbx);
          Emitter.MOV_Mem_Reg_i64(rbx, bc.Args[0].Data.Addr, gDest);
          GPRAlloc.SetResult(gDest, bc.Args[0]);
        end;

      else
        begin
          FreeExecutableMemory(ExecMem, 0);
          Exit(nil);
        end;
      end;
    end

    // =======================================================================
    // FLOAT INSTRUCTIONS
    // =======================================================================
    else if arg_type in [xtSingle, xtDouble] then
    begin
      case code of

        bcMOVH_f32_f32_ll, bcMOVH_f32_f32_li,
        bcMOVH_f64_f64_ll, bcMOVH_f64_f64_li,
        bcMOVH_f32_f64_ll, bcMOVH_f32_f64_li,
        bcMOVH_f64_f32_ll, bcMOVH_f64_f32_li: begin
          ptr_arg   := bc.Args[0];
          val_arg   := bc.Args[1];
          val_type  := BaseJITType(val_arg.BaseType);
          dest_type := BaseJITType(ptr_arg.BaseType);
          ptr_arg.BaseType := xtInt;

          gA := GPRAlloc.GetReg(ptr_arg);

          ptr_arg.BaseType := dest_type;
          reg1 := XMMAlloc.GetReg(val_arg);
          if val_type <> dest_type then begin
            reg2 := XMMAlloc.Allocate;
            if dest_type = xtSingle then Emitter.CVTSD2SS_XMM_XMM(reg2, reg1)
            else                         Emitter.CVTSS2SD_XMM_XMM(reg2, reg1);
            Emitter.Store_Float_Result_To_Addr(ptr_arg, reg2, gA);
          end else
            Emitter.Store_Float_Result_To_Addr(ptr_arg, reg1, gA);
        end;

        bcMOVH_f32_i8_ll..bcMOVH_f32_u64_li,
        bcMOVH_f64_i8_ll..bcMOVH_f64_u64_li: begin
          ptr_arg   := bc.Args[0];
          val_arg   := bc.Args[1];
          dest_type := BaseJITType(ptr_arg.BaseType);

          ptr_arg.BaseType := xtInt;
          gA := GPRAlloc.GetReg(ptr_arg);
          gB := GPRAlloc.GetReg(val_arg, GPRAlloc.MaskOf(gA));

          ptr_arg.BaseType := dest_type;
          reg1 := XMMAlloc.Allocate;
          if dest_type = xtSingle then Emitter.CVTSI2SS_XMM_Reg64(reg1, gB)
          else                         Emitter.CVTSI2SD_XMM_Reg(reg1, gB);
          Emitter.Store_Float_Result_To_Addr(ptr_arg, reg1, gA);
        end;

        bcMOV_f32_f32_ll, bcMOV_f32_f32_li,
        bcMOV_f64_f64_ll, bcMOV_f64_f64_li: begin
          reg1 := XMMAlloc.GetReg(bc.Args[1]);
          XMMAlloc.SetResult(reg1, bc.Args[0]);
        end;

        bcMOV_f32_f64_ll, bcMOV_f32_f64_li,
        bcMOV_f64_f32_ll, bcMOV_f64_f32_li: begin
          reg1 := XMMAlloc.GetReg(bc.Args[1]);
          reg2 := XMMAlloc.Allocate;
          if BaseJITType(bc.Args[0].BaseType) = xtSingle then
            Emitter.CVTSD2SS_XMM_XMM(reg2, reg1)
          else
            Emitter.CVTSS2SD_XMM_XMM(reg2, reg1);
          XMMAlloc.SetResult(reg2, bc.Args[0]);
        end;

        bcMOV_f32_i8_ll..bcMOV_f32_u64_li,
        bcMOV_f64_i8_ll..bcMOV_f64_u64_li: begin
          gA := GPRAlloc.GetReg(bc.Args[1]);
          reg1 := XMMAlloc.Allocate;
          if arg_type = xtSingle then Emitter.CVTSI2SS_XMM_Reg64(reg1, gA)
          else                        Emitter.CVTSI2SD_XMM_Reg(reg1, gA);
          XMMAlloc.SetResult(reg1, bc.Args[0]);
        end;

        bcEQ_lll_i32..bcLTE_iil_f64: begin
          reg1 := XMMAlloc.GetReg(bc.Args[0]);
          reg2 := XMMAlloc.GetReg(bc.Args[1]);

          if arg_type = xtSingle then Emitter.UCOMISS_XMM_XMM(reg1, reg2)
          else                        Emitter.UCOMISD_XMM_XMM(reg1, reg2);

          gDest := GPRAlloc.GetResultReg(bc.Args[2], 0);

          case code of
            bcEQ_lll_i32..bcEQ_iil_f64:   Emitter.SETcc(sete,  gDest);
            bcNE_lll_i32..bcNE_iil_f64:   Emitter.SETcc(setne, gDest);
            bcLT_lll_i32..bcLT_iil_f64:   Emitter.SETcc(setb,  gDest);
            bcGT_lll_i32..bcGT_iil_f64:   Emitter.SETcc(seta,  gDest);
            bcLTE_lll_i32..bcLTE_iil_f64: Emitter.SETcc(setbe, gDest);
            bcGTE_lll_i32..bcGTE_iil_f64: Emitter.SETcc(setae, gDest);
          end;
          Emitter.MOVZX_Reg_Reg_u8(gDest);
          GPRAlloc.SetResult(gDest, bc.Args[2]);
        end;

        bcADD_lll_i32..bcDIV_iil_f64: begin
          reg1 := XMMAlloc.GetReg(bc.Args[0]);
          reg2 := XMMAlloc.GetReg(bc.Args[1], [reg1]);

          if (bc.Args[0].Pos = mpLocal) and
             (bc.Args[0].Data.Addr <> bc.Args[2].Data.Addr) then begin
            reg3 := XMMAlloc.GetFreeScratch(False, [reg1, reg2]);
            if arg_type = xtSingle then Emitter.MOVSS_XMM_XMM(reg3, reg1)
            else                        Emitter.MOVSD_XMM_XMM(reg3, reg1);
            reg1 := reg3;
          end;
          case code of
            bcADD_lll_i32..bcADD_iil_f64: Emitter.ADDSX_XMM_XMM(reg1, reg2, arg_type);
            bcSUB_lll_i32..bcSUB_iil_f64: Emitter.SUBSX_XMM_XMM(reg1, reg2, arg_type);
            bcMUL_lll_i32..bcMUL_iil_f64: Emitter.MULSX_XMM_XMM(reg1, reg2, arg_type);
            bcDIV_lll_i32..bcDIV_iil_f64: Emitter.DIVSX_XMM_XMM(reg1, reg2, arg_type);
          end;
          XMMAlloc.SetResult(reg1, bc.Args[2]);
          XMMAlloc.ClearAllConst;
        end;

        bcDREF_32, bcDREF_64: begin
          gA := GPRAlloc.GetReg(bc.Args[1]);
          reg1 := XMMAlloc.Allocate;
          if code = bcDREF_32 then Emitter.MOVSS_XMM_Mem(reg1, gA, 0)
          else                     Emitter.MOVSD_XMM_Mem(reg1, gA, 0);
          XMMAlloc.SetResult(reg1, bc.Args[0]);
        end;

      else
        begin
          FreeExecutableMemory(ExecMem, 0);
          Exit(nil);
        end;
      end;
    end
    else begin
      FreeExecutableMemory(ExecMem, 0);
      Exit(nil);
    end;

  end;

  Labels[Count] := Emitter.CurrentPosition;
  GPRAlloc.SpillAll;
  XMMAlloc.SpillAllDirty;
  Emitter.Epilogue;

  for j := 0 to High(Patches) do begin
    patch := Patches[j];
    if (patch.TargetBCIndex < 0) or (patch.TargetBCIndex > Count) then begin
      FreeExecutableMemory(ExecMem, 0);
      Exit(nil);
    end;
    tgt  := Labels[patch.TargetBCIndex];
    jend := PByte(patch.PatchLocation) + 4;
    rel  := PtrInt(tgt) - PtrInt(jend);
    patch.PatchLocation^ := Int32(rel);
  end;

  TotalSize := Emitter.CurrentPosition - ExecMem;
  SetMemoryExecutable(ExecMem, TotalSize);
  Result := ExecMem;
end;
