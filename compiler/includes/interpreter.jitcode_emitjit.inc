function TInterpreter.EmitJITBlock(CodeList: PBytecodeInstruction; Count: Int32;
                                var TotalSize: SizeInt; Settings:PCompilerSettings; CanJMP: Boolean = False): Pointer;
type
  TPatch = record PatchLocation: PInt32; TargetBCIndex: Int32; end;
  TPatchList = array of TPatch;
  TLabelMap  = array of PByte;

  function IsUnsigned(bt: EExpressBaseType): Boolean; inline;
  begin Result := bt in [xtUInt8, xtUInt16, xtUInt32, xtUInt64]; end;

  function IsUnsignedOrFloat(bt: EExpressBaseType): Boolean; inline;
  begin Result := bt in [xtUInt8, xtUInt16, xtUInt32, xtUInt64, xtSingle, xtDouble]; end;

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
  Patches, BailoutPatches: TPatchList;
  Labels  : TLabelMap;
  i, j, continue_count_i: Int32;
  bc      : TBytecodeInstruction;
  code    : EBytecode;
  arg_type: EExpressBaseType;
  loc, loc2: PInt32;
  patch   : TPatch;
  tgt, jend : PByte;
  rel     : PtrInt;
  gA, gB, gC, gDest: EReg;
  excl         : Byte;
  reg1, reg2, reg3      : EXMMReg;
  ptr_arg, val_arg      : TOperand;
  val_type, dest_type   : EExpressBaseType;
  SuccessLabel: PByte;
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

  Emitter.Init(ExecMem, MemBases[mpConst]);
  SetLength(Patches, 0);
  SetLength(Labels, Count+1);

  Emitter.Preamble;

  XMMAlloc.Init(@Emitter);
  XMMAlloc.InvalidateAll;
  XMMAlloc.AnalyzeTrace(CodeList, Count, Settings);

  GPRAlloc.Init(@Emitter);
  GPRAlloc.AnalyzeTrace(CodeList, Count, Settings);

  continue_count_i := 0;
  for i := 0 to Count-1 do begin
    if continue_count_i > 0 then
    begin
      Dec(continue_count_i);
      continue;
    end;

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

        bcMOV, bcMOV8..bcMOVZX64_32:
        begin
          gA    := GPRAlloc.GetReg(bc.Args[1]);
          gDest := GPRAlloc.GetResultReg(bc.Args[0], GPRAlloc.MaskOf(gA));
          if gDest <> gA then Emitter.MOV_Reg_Reg(gDest, gA);
          GPRAlloc.SetResult(gDest, bc.Args[0]);
        end;

        bcSTORE, bcSTORE8..bcSTOREZX64_32:
        begin
          gA := GPRAlloc.GetReg(bc.Args[0]);
          gB := GPRAlloc.GetReg(bc.Args[1], GPRAlloc.MaskOf(gA));
          Emitter.Store_Int_Result_To_Addr(bc.Args[1], gB, gA);
        end;

        bcEQ..bcLTE, bcEQ_i32..bcLTE_u64:
        begin
          gA := GPRAlloc.GetReg(bc.Args[0]);
          gB := GPRAlloc.GetReg(bc.Args[1], GPRAlloc.MaskOf(gA));
          Emitter.CMP_Reg_Reg(gA, gB);

          excl  := GPRAlloc.MaskOf(gA) or GPRAlloc.MaskOf(gB);
          gDest := GPRAlloc.GetResultReg(bc.Args[2], excl);

          case code of
            bcEQ, bcEQ_i32..bcEQ_f64:   Emitter.SETcc(sete,  gDest);
            bcNE, bcNE_i32..bcNE_f64:   Emitter.SETcc(setne, gDest);
            bcLT, bcLT_i32..bcLT_f64:
              if IsUnsigned(arg_type) then Emitter.SETcc(setb,  gDest)
              else                         Emitter.SETcc(setl,  gDest);
            bcGT, bcGT_i32..bcGT_f64:
              if IsUnsigned(arg_type) then Emitter.SETcc(seta,  gDest)
              else                         Emitter.SETcc(setg,  gDest);
            bcLTE, bcLTE_i32..bcLTE_f64:
              if IsUnsigned(arg_type) then Emitter.SETcc(setbe, gDest)
              else                         Emitter.SETcc(setle, gDest);
            bcGTE, bcGTE_i32..bcGTE_f64:
              if IsUnsigned(arg_type) then Emitter.SETcc(setae, gDest)
              else                         Emitter.SETcc(setge, gDest);
          end;

          Emitter.MOVZX_Reg_Reg_u8(gDest);
          GPRAlloc.SetResult(gDest, bc.Args[2]);
        end;

        bcADD, bcADD_i32, bcADD_u32, bcADD_i64, bcADD_u64: begin
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

        bcSUB, bcSUB_i32, bcSUB_u32, bcSUB_i64, bcSUB_u64: begin
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

        bcMUL, bcMUL_i32, bcMUL_u32, bcMUL_i64, bcMUL_u64: begin
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

        bcDIV, bcDIV_i32, bcDIV_u32, bcDIV_i64, bcDIV_u64: begin
          GPRAlloc.SpillAll;
          GPRAlloc.ForceEvict(rax);
          GPRAlloc.ForceEvict(rcx);
          GPRAlloc.ForceEvict(rdx);

          Emitter.Load_Int_Operand(bc.Args[0], rax);
          Emitter.Load_Int_Operand(bc.Args[1], rcx);
          if IsUnsigned(arg_type) then begin
            Emitter.XOR_Reg_Reg(rdx, rdx);
            Emitter.DIV_Reg(rcx);
          end else begin
            Emitter.CQO;
            Emitter.IDIV_Reg(rcx);
          end;
          GPRAlloc.SetResult(rax, bc.Args[2]);
        end;

        bcMOD, bcMOD_i32, bcMOD_u32, bcMOD_i64, bcMOD_u64: begin
          GPRAlloc.SpillAll;
          GPRAlloc.ForceEvict(rax);
          GPRAlloc.ForceEvict(rcx);
          GPRAlloc.ForceEvict(rdx);

          Emitter.Load_Int_Operand(bc.Args[0], rax);
          Emitter.Load_Int_Operand(bc.Args[1], rcx);
          if IsUnsigned(arg_type) then begin
            Emitter.XOR_Reg_Reg(rdx, rdx);
            Emitter.DIV_Reg(rcx);
          end else begin
            Emitter.CQO;
            Emitter.IDIV_Reg(rcx);
          end;
          GPRAlloc.SetResult(rdx, bc.Args[2]);
        end;

        bcBND, bcBND_i32, bcBND_u32, bcBND_i64, bcBND_u64: begin
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

        bcBOR, bcBOR_i32, bcBOR_u32, bcBOR_i64, bcBOR_u64: begin
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

        bcXOR, bcXOR_i32, bcXOR_u32, bcXOR_i64, bcXOR_u64: begin
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

        bcSHL, bcSHL_i32, bcSHL_u32, bcSHL_i64, bcSHL_u64,
        bcSHR, bcSHR_i32, bcSHR_u32, bcSHR_i64, bcSHR_u64,
        bcSAR, bcSAR_i32, bcSAR_u32, bcSAR_i64, bcSAR_u64: begin
          GPRAlloc.ForceEvict(rcx);

          gA := GPRAlloc.GetReg(bc.Args[0], GPRAlloc.MaskOf(rcx));
          gDest := GPRAlloc.GetResultReg(bc.Args[2], GPRAlloc.MaskOf(rcx));
          if gA <> gDest then Emitter.MOV_Reg_Reg(gDest, gA);

          Emitter.Load_Int_Operand(bc.Args[1], rcx);

          case code of
            bcSHL, bcSHL_i32, bcSHL_u32, bcSHL_i64, bcSHL_u64: Emitter.SHL_Reg_CL(gDest);
            bcSHR, bcSHR_i32, bcSHR_u32, bcSHR_i64, bcSHR_u64: Emitter.SHR_Reg_CL(gDest);
            bcSAR, bcSAR_i32, bcSAR_u32, bcSAR_i64, bcSAR_u64: Emitter.SAR_Reg_CL(gDest);
          end;
          GPRAlloc.SetResult(gDest, bc.Args[2]);
        end;

        bcINC_i32, bcINC_u32, bcINC_i64, bcINC_u64: begin
          gA := GPRAlloc.GetReg(bc.Args[0]);
          if (bc.Args[1].Pos in [mpImm, mpConst]) and
             ( ((bc.Args[1].Pos = mpImm) and (bc.Args[1].Data.i32 = 1)) or
               ((bc.Args[1].Pos = mpConst) and (PInt64(MemBases[mpConst]+bc.Args[1].Data.Addr)^ = 1)) ) then
            Emitter.INC_Reg(gA)
          else if bc.Args[1].Pos = mpImm then
            Emitter.ADD_Reg_Imm32(gA, bc.Args[1].Data.i32)
          else begin
            gB := GPRAlloc.GetReg(bc.Args[1], GPRAlloc.MaskOf(gA));
            Emitter.ADD_Reg_Reg(gA, gB);
          end;
          GPRAlloc.SetResult(gA, bc.Args[2]);
        end;

        bcFMA_i8, bcFMA_u8, bcFMA_i16, bcFMA_u16, bcFMA_i32, bcFMA_u32, bcFMA_i64, bcFMA_u64: begin
          gA := GPRAlloc.GetReg(bc.Args[2]);
          gB := GPRAlloc.GetReg(bc.Args[0], GPRAlloc.MaskOf(gA));

          excl := GPRAlloc.MaskOf(gA) or GPRAlloc.MaskOf(gB);

          gDest := GPRAlloc.GetResultReg(bc.Args[3], excl);

          if bc.Args[1].Data.i32 in [1,2,4,8] then begin
            Emitter.LEA_Reg_Mem(gDest, gA, gB, bc.Args[1].Data.i32);
          end else begin
            gC := GPRAlloc.GetScratch(excl or GPRAlloc.MaskOf(gDest));

            Emitter.IMUL_Reg_Reg_Imm32(gC, gB, bc.Args[1].Data.i32);
            Emitter.ADD_Reg_Reg(gC, gA);
            Emitter.MOV_Reg_Reg(gDest, gC);

            GPRAlloc.ClearSlot(gC);
          end;

          GPRAlloc.SetResult(gDest, bc.Args[3]);
        end;

        bcBCHK:
        begin
          // spill everything first
          GPRAlloc.SpillAll;
          GPRAlloc.InvalidateAll;
          XMMAlloc.SpillAllDirty;
          XMMAlloc.InvalidateAll;

          Emitter.MOV_Reg_Mem_i64(rcx, rbx, bc.Args[0].Data.Addr);

          Emitter.TEST_Reg_Reg(rcx, rcx);
          loc := Emitter.Jcc_Rel32(jne);

          Emitter.MOV_Reg_Imm32(rax, i + 1);
          loc2 := Emitter.JMP_Rel32;
          AddPatch(BailoutPatches, loc2, Count);

          tgt := Emitter.CurrentPosition;
          rel := PtrInt(tgt) - PtrInt(loc) - 4;
          loc^ := rel;

          Emitter.MOV_Reg_Mem_i64(rdx, rcx, -SizeOf(SizeInt));

          case BaseIntType(bc.Args[1].BaseType) of
            xtInt32:  Emitter.MOVSXD_Reg_Mem_i32(rax, rbx, bc.Args[1].Data.Addr);
            xtUInt32: Emitter.MOV_Reg_Mem_i32(rax, rbx, bc.Args[1].Data.Addr);  // zero-extend
            else      Emitter.MOV_Reg_Mem_i64(rax, rbx, bc.Args[1].Data.Addr);
          end;

          Emitter.CMP_Reg_Reg(rax, rdx);
          loc := Emitter.Jcc_Rel32(jle);

          Emitter.MOV_Reg_Imm32(rax, i + 1);
          loc2 := Emitter.JMP_Rel32;
          AddPatch(BailoutPatches, loc2, Count);

          tgt := Emitter.CurrentPosition;
          rel := PtrInt(tgt) - PtrInt(loc) - 4;
          loc^ := rel;
        end;

        bcFMAD64_64, bcFMAD64_32, bcFMAD32_64, bcFMAD32_32: begin
          gA := GPRAlloc.GetReg(bc.Args[2]);
          gB := GPRAlloc.GetReg(bc.Args[0], GPRAlloc.MaskOf(gA));
          excl := GPRAlloc.MaskOf(gA) or GPRAlloc.MaskOf(gB);

          gDest := GPRAlloc.GetScratch(excl);

          if bc.Args[1].Data.i32 in [1,2,4,8] then begin
            Emitter.LEA_Reg_Mem(gDest, gA, gB, bc.Args[1].Data.i32);
          end else begin
            gC := GPRAlloc.GetScratch(excl or GPRAlloc.MaskOf(gDest));

            Emitter.IMUL_Reg_Reg_Imm32(gC, gB, bc.Args[1].Data.i32);
            Emitter.LEA_Reg_Mem(gDest, gA, gC, 1);

            GPRAlloc.ClearSlot(gC);
          end;

          if bc.Args[3].BaseType in [xtSingle, xtDouble] then begin
            reg1 := XMMAlloc.Allocate;
            if bc.Args[3].BaseType = xtSingle then Emitter.MOVSS_XMM_Mem(reg1, gDest, 0)
            else                                   Emitter.MOVSD_XMM_Mem(reg1, gDest, 0);
            XMMAlloc.SetResult(reg1, bc.Args[3]);
            GPRAlloc.ClearSlot(gDest);
          end else begin
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
          end;
        end;

        bcDREF, bcDREF32, bcDREF64: begin
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

        bcMOVF32_f32, bcMOVF64_f64: begin
          reg1 := XMMAlloc.GetReg(bc.Args[1]);
          XMMAlloc.SetResult(reg1, bc.Args[0]);
        end;

        bcSTOREF32_f32, bcSTOREF64_f64,
        bcSTOREF32_f64, bcSTOREF64_f32: begin
          ptr_arg   := bc.Args[0];
          val_arg   := bc.Args[1];
          val_type  := BaseJITType(val_arg.BaseType);
          dest_type := BaseJITType(ptr_arg.BaseType);
          ptr_arg.BaseType := xtInt64;

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

        bcSTORE, // Mixed F := small int
        bcSTOREF32_i32, bcSTOREF32_u32, bcSTOREF32_i64, bcSTOREF32_u64,
        bcSTOREF64_i32, bcSTOREF64_u32, bcSTOREF64_i64, bcSTOREF64_u64:
        begin
          ptr_arg   := bc.Args[0];
          val_arg   := bc.Args[1];
          dest_type := BaseJITType(ptr_arg.BaseType);

          ptr_arg.BaseType := xtInt64;
          gA := GPRAlloc.GetReg(ptr_arg);
          gB := GPRAlloc.GetReg(val_arg, GPRAlloc.MaskOf(gA));

          ptr_arg.BaseType := dest_type;
          reg1 := XMMAlloc.Allocate;
          if dest_type = xtSingle then Emitter.CVTSI2SS_XMM_Reg64(reg1, gB)
          else                         Emitter.CVTSI2SD_XMM_Reg(reg1, gB);
          Emitter.Store_Float_Result_To_Addr(ptr_arg, reg1, gA);
        end;

        bcMOVF32_f64, bcMOVF64_f32: begin
          reg1 := XMMAlloc.GetReg(bc.Args[1]);
          reg2 := XMMAlloc.Allocate;
          if BaseJITType(bc.Args[0].BaseType) = xtSingle then
            Emitter.CVTSD2SS_XMM_XMM(reg2, reg1)
          else
            Emitter.CVTSS2SD_XMM_XMM(reg2, reg1);
          XMMAlloc.SetResult(reg2, bc.Args[0]);
        end;

        bcMOV, // Mixed F := small int
        bcMOVF32_i32, bcMOVF32_u32, bcMOVF32_i64, bcMOVF32_u64,
        bcMOVF64_i32, bcMOVF64_u32, bcMOVF64_i64, bcMOVF64_u64: begin
          gA := GPRAlloc.GetReg(bc.Args[1]);
          reg1 := XMMAlloc.Allocate;
          if arg_type = xtSingle then Emitter.CVTSI2SS_XMM_Reg64(reg1, gA)
          else                        Emitter.CVTSI2SD_XMM_Reg(reg1, gA);
          XMMAlloc.SetResult(reg1, bc.Args[0]);
        end;

        bcEQ..bcLTE, bcEQ_f32..bcLTE_f64:
        begin
          reg1 := XMMAlloc.GetReg(bc.Args[0]);
          reg2 := XMMAlloc.GetReg(bc.Args[1]);

          if arg_type = xtSingle then Emitter.UCOMISS_XMM_XMM(reg1, reg2)
          else                        Emitter.UCOMISD_XMM_XMM(reg1, reg2);

          gDest := GPRAlloc.GetResultReg(bc.Args[2], 0);

          case code of
            bcEQ, bcEQ_f32, bcEQ_f64:   Emitter.SETcc(sete,  gDest);
            bcNE, bcNE_f32, bcNE_f64:   Emitter.SETcc(setne, gDest);
            bcLT, bcLT_f32, bcLT_f64:   Emitter.SETcc(setb,  gDest);
            bcGT, bcGT_f32, bcGT_f64:   Emitter.SETcc(seta,  gDest);
            bcLTE, bcLTE_f32, bcLTE_f64: Emitter.SETcc(setbe, gDest);
            bcGTE, bcGTE_f32, bcGTE_f64: Emitter.SETcc(setae, gDest);
          end;
          Emitter.MOVZX_Reg_Reg_u8(gDest);
          GPRAlloc.SetResult(gDest, bc.Args[2]);
        end;

        bcADD, bcSUB, bcMUL, bcDIV,
        bcADD_f32, bcADD_f64, bcSUB_f32, bcSUB_f64,
        bcMUL_f32, bcMUL_f64, bcDIV_f32, bcDIV_f64: begin
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
            bcADD, bcADD_f32, bcADD_f64: Emitter.ADDSX_XMM_XMM(reg1, reg2, arg_type);
            bcSUB, bcSUB_f32, bcSUB_f64: Emitter.SUBSX_XMM_XMM(reg1, reg2, arg_type);
            bcMUL, bcMUL_f32, bcMUL_f64: Emitter.MULSX_XMM_XMM(reg1, reg2, arg_type);
            bcDIV, bcDIV_f32, bcDIV_f64: Emitter.DIVSX_XMM_XMM(reg1, reg2, arg_type);
          end;
          XMMAlloc.SetResult(reg1, bc.Args[2]);
          XMMAlloc.ClearAllConst;
        end;

        bcDREF32, bcDREF64:
        begin
          gA := GPRAlloc.GetReg(bc.Args[1]);
          reg1 := XMMAlloc.Allocate;
          if code = bcDREF32 then Emitter.MOVSS_XMM_Mem(reg1, gA, 0)
          else                    Emitter.MOVSD_XMM_Mem(reg1, gA, 0);
          XMMAlloc.SetResult(reg1, bc.Args[0]);
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
    else begin
      FreeExecutableMemory(ExecMem, 0);
      Exit(nil);
    end;
  end;

  // regular JZ/RELJMP successful cases land here
  SuccessLabel := Emitter.CurrentPosition;

  GPRAlloc.SpillAll;
  XMMAlloc.SpillAllDirty;
  Emitter.XOR_Reg_Reg(rax, rax);     // return 0, AFTER spill (spill uses rax)

  // All bailout JMPs converge here
  Labels[Count] := Emitter.CurrentPosition;
  for patch in BailoutPatches do
  begin
    tgt := Labels[Count];
    rel := PtrInt(tgt) - PtrInt(patch.PatchLocation) - 4;
    patch.PatchLocation^ := rel;
  end;

  Emitter.Epilogue;   // now safe: no pop rax, just add rsp, 8 + ret

  // Regular patches — use SuccessLabel when exiting the block
  for j := 0 to High(Patches) do begin
    patch := Patches[j];
    if (patch.TargetBCIndex < 0) or (patch.TargetBCIndex > Count) then begin
      FreeExecutableMemory(ExecMem, 0);
      Exit(nil);
    end;
    if patch.TargetBCIndex = Count then
      tgt := SuccessLabel          // ← exit path: needs SpillAll + xor
    else
      tgt := Labels[patch.TargetBCIndex];  // ← internal jump: direct
    jend := PByte(patch.PatchLocation) + 4;
    rel  := PtrInt(tgt) - PtrInt(jend);
    patch.PatchLocation^ := Int32(rel);
  end;

  TotalSize := Emitter.CurrentPosition - ExecMem;
  SetMemoryExecutable(ExecMem, TotalSize);
  Result := ExecMem;
end;
