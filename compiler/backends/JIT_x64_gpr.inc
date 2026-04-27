// ===========================================================================
// JIT_x64_gpr.inc
// TGPRRegAllocator implementation
//
// Pool layout:
//   idx 0 = rax  (Scratch 0 - Hardcoded required for DIV/MUL)
//   idx 1 = rcx  (Scratch 1 - Hardcoded required for Shifts)
//   idx 2 = rdx  (Scratch 2 - Hardcoded required for DIV/MUL)
//   idx 3 = rsi  (VIP 0)
//   idx 4 = rdi  (VIP 1)
// ===========================================================================

function TGPRRegAllocator.IdxOf(Reg: EReg): Integer;
begin
  case Reg of
    rax: Result := 0;
    rcx: Result := 1;
    rdx: Result := 2;
    rsi: Result := 3;
    rdi: Result := 4;
  else
    Result := -1;
  end;
end;

function TGPRRegAllocator.MaskOf(Reg: EReg): Byte;
var i: Integer;
begin
  i := IdxOf(Reg);
  if i >= 0 then Result := Byte(1 shl i) else Result := 0;
end;

function TGPRRegAllocator.IsVIPReg(Reg: EReg): Boolean;
var i: Integer;
begin
  i := IdxOf(Reg);
  Result := (i >= GPR_VIP_IDX_START) and (i < GPR_VIP_IDX_START + VIPCount);
end;

function TGPRRegAllocator.IsSlotDirty(Reg: EReg): Boolean;
var i: Integer;
begin
  i := IdxOf(Reg);
  Result := (i >= 0) and Slots[i].IsSet and Slots[i].IsDirty;
end;

function TGPRRegAllocator.FindCached(Offset: Int64; APos: EMemPos): Integer;
var i: Integer;
begin
  for i := 0 to 4 do
    if Slots[i].IsSet and (Slots[i].Offset = Offset) and (Slots[i].Pos = APos) then Exit(i);
  Result := -1;
end;

function TGPRRegAllocator.VIPSlotOf(Offset: Int64; APos: EMemPos): Integer;
var i: Integer;
begin
  for i := 0 to VIPCount-1 do
    if (VIPOffsets[i] = Offset) and (VIPPos[i] = APos) then Exit(GPR_VIP_IDX_START + i);
  Result := -1;
end;

procedure TGPRRegAllocator.ClearSlotAt(Idx: Integer);
begin
  Slots[Idx].IsSet   := False;
  Slots[Idx].IsDirty := False;
  Slots[Idx].Offset  := -1;
  Slots[Idx].Pos     := mpUnknown;
  Slots[Idx].BType   := xtUnknown;
end;

procedure TGPRRegAllocator.ClearSlot(Reg: EReg);
var i: Integer;
begin
  i := IdxOf(Reg);
  if i >= 0 then ClearSlotAt(i);
end;

procedure TGPRRegAllocator.ForceEvict(Reg: EReg);
var i: Integer;
begin
  i := IdxOf(Reg);
  if i >= 0 then begin
    SpillSlot(i);
    ClearSlotAt(i);
  end;
end;

procedure TGPRRegAllocator.SpillSlot(Idx: Integer);
begin
  if not Slots[Idx].IsSet or (not Slots[Idx].IsDirty) or (Slots[Idx].Offset < 0) then
    Exit;
    
  if Slots[Idx].Pos = mpConst then begin
    Slots[Idx].IsDirty := False;
    Exit;
  end;
  
  case Slots[Idx].BType of
    xtInt8,  xtUInt8:  Emitter^.MOV_Mem_Reg_i8 (rbx, Slots[Idx].Offset, GPR_POOL[Idx]);
    xtInt16, xtUInt16: Emitter^.MOV_Mem_Reg_i16(rbx, Slots[Idx].Offset, GPR_POOL[Idx]);
    xtInt32, xtUInt32: Emitter^.MOV_Mem_Reg_i32(rbx, Slots[Idx].Offset, GPR_POOL[Idx]);
    else               Emitter^.MOV_Mem_Reg_i64(rbx, Slots[Idx].Offset, GPR_POOL[Idx]);
  end;
  Slots[Idx].IsDirty := False;
end;

function TGPRRegAllocator.AllocSlot(ExclMask: Byte): Integer;
var i: Integer;
begin
  for i := 0 to GPR_VIP_IDX_START-1 do
    if ((Byte(1 shl i) and ExclMask) = 0) and not Slots[i].IsSet then Exit(i);
  for i := GPR_VIP_IDX_START to 4 do
    if ((Byte(1 shl i) and ExclMask) = 0) and not Slots[i].IsSet then Exit(i);
  for i := 0 to GPR_VIP_IDX_START-1 do
    if (Byte(1 shl i) and ExclMask) = 0 then
    begin SpillSlot(i); ClearSlotAt(i); Exit(i); end;
  for i := GPR_VIP_IDX_START to 4 do
    if (Byte(1 shl i) and ExclMask) = 0 then
    begin SpillSlot(i); ClearSlotAt(i); Exit(i); end;
  SpillSlot(0); ClearSlotAt(0); Result := 0;
end;

// ---------------------------------------------------------------------------

procedure TGPRRegAllocator.Init(AEmitter: PJitEmitter);
var i: Integer;
begin
  Emitter  := AEmitter;
  VIPCount := 0;
  for i := 0 to 4 do ClearSlotAt(i);
end;

procedure TGPRRegAllocator.AnalyzeTrace(CodeList: PBytecodeInstruction; Count: Integer; Settings: PCompilerSettings);
type
  TFreqEntry = record
    Offset: Int64;
    Pos: EMemPos;
    BType: EExpressBaseType;
    Cnt: UInt64;
    FirstUseIsWrite: Boolean;
  end;
const
  MAX_VARS = 32;
  LOOP_WEIGHT: array[0..6] of Integer = (1, 10, 100, 1000, 10000, 100000, 1000000);
var
  FM: array[0..MAX_VARS-1] of TFreqEntry;
  FC, i, j, k, ri: Int32;
  instr: TBytecodeInstruction;
  arg: TOperand;
  bt: EExpressBaseType;
  found: Boolean;
  tmp: TFreqEntry;
  preload: TOperand;
  Depth, CurrentWeight, TargetIndex: Int32;
  AddrTakenMask: array[0..MAX_VARS-1] of Boolean;  // slots whose addr is taken

  function IsWriteAccess(Code:EBytecode; argId: Int32): Boolean;
  begin
    Result := ((Code in [bcMOV..bcMOVF64_f64, bcDREF..bcDREF64]) and (argId = 0))
           or ((Code in [bcADD..bcSAR_u64]) and (argId = 2));
  end;

  function IsProbabeReadAccess(Code:EBytecode; argId: Int32): Boolean;
  begin
    Result := not IsWriteAccess(Code, ArgId);
  end;

begin
  FC := 0;
  FillChar(FM, SizeOf(FM), 0);

  FillChar(AddrTakenMask, SizeOf(AddrTakenMask), 0);

  for i := 0 to Count-1 do begin
    Depth := (Settings+i)^.LoopDepth;
    if Depth > High(LOOP_WEIGHT) then Depth := High(LOOP_WEIGHT);
    if Depth < 0 then Depth := 0;
    CurrentWeight := LOOP_WEIGHT[Depth];

    instr := (CodeList+i)^;
    for j := 0 to instr.nArgs-1 do begin
      arg := instr.Args[j];

      if instr.Code = bcADDR then
      begin
        // Args[1] is the slot whose address is being taken
        for k := 0 to FC-1 do
          if (FM[k].Offset = instr.Args[1].Data.Addr) and
             (FM[k].Pos = instr.Args[1].Pos) then
          begin
            AddrTakenMask[k] := True;
            Break;
          end;
      end;

      if (arg.Pos <> mpLocal) then Continue;
      if (arg.Data.Addr < 0)  then Continue;
      bt := BaseJITType(arg.BaseType);

      if not (bt in [xtInt8, xtInt16, xtInt32, xtInt64,
                     xtUInt8, xtUInt16, xtUInt32, xtUInt64]) then Continue;

      found := False;
      for k := 0 to FC-1 do
        if (FM[k].Offset = arg.Data.Addr) and (FM[k].Pos = arg.Pos) then begin
          if IsProbabeReadAccess(instr.Code, j) then
            Inc(FM[k].Cnt, CurrentWeight);
          found := True;
          Break;
        end;

      if not found and (FC < MAX_VARS) then begin
        FM[FC].Offset := arg.Data.Addr;
        FM[FC].Pos    := arg.Pos;
        FM[FC].BType  := bt;
        FM[FC].Cnt    := CurrentWeight;
        FM[FC].FirstUseIsWrite := IsWriteAccess(instr.Code, j);
        Inc(FC);
      end;
    end;
  end;

  for i := 0 to FC-2 do
    for j := i+1 to FC-1 do
      if FM[j].Cnt > FM[i].Cnt then
      begin tmp := FM[i]; FM[i] := FM[j]; FM[j] := tmp; end;

  VIPCount := 0;
  for i := 0 to Min(1, FC-1) do begin
    if FM[i].Cnt < 2 then Break;
    if AddrTakenMask[i] then Continue; // aliased.. skip

    ri := GPR_VIP_IDX_START + VIPCount;
    VIPOffsets[VIPCount] := FM[i].Offset;
    VIPPos[VIPCount]     := FM[i].Pos;
    VIPBTypes[VIPCount]  := FM[i].BType;
    Inc(VIPCount);

    Slots[ri].IsSet   := True;
    Slots[ri].IsDirty := False;
    Slots[ri].Offset  := FM[i].Offset;
    Slots[ri].Pos     := FM[i].Pos;
    Slots[ri].BType   := FM[i].BType;

    FillChar(preload, SizeOf(preload), 0);
    preload.Pos       := FM[i].Pos;
    preload.Data.Addr := FM[i].Offset;
    preload.BaseType  := FM[i].BType;
    if not FM[i].FirstUseIsWrite then
      Emitter^.Load_Int_Operand(preload, GPR_POOL[ri]);
  end;
end;

function TGPRRegAllocator.GetReg(const arg: TOperand; ExclMask: Byte): EReg;
var
  bt    : EExpressBaseType;
  idx   : Integer;
  vi    : Integer;
  tmp   : TOperand;
begin
  if arg.Pos in [mpImm, mpConst] then begin
    idx := AllocSlot(ExclMask);
    Emitter^.Load_Int_Operand(arg, GPR_POOL[idx]);
    Slots[idx].IsSet   := True;
    Slots[idx].IsDirty := False;
    Slots[idx].Offset  := -2;
    Slots[idx].Pos     := arg.Pos;
    Slots[idx].BType   := BaseJITType(arg.BaseType);
    Result := GPR_POOL[idx];
    Exit;
  end;

  if arg.Pos = mpLocal then begin
    bt := BaseJITType(arg.BaseType);

    vi := VIPSlotOf(arg.Data.Addr, arg.Pos);
    if vi >= 0 then begin
      if not Slots[vi].IsSet then begin
        FillChar(tmp, SizeOf(tmp), 0);
        tmp.Pos       := arg.Pos;
        tmp.Data.Addr := arg.Data.Addr;
        tmp.BaseType  := bt;
        Emitter^.Load_Int_Operand(tmp, GPR_POOL[vi]);
        Slots[vi].IsSet   := True;
        Slots[vi].IsDirty := False;
        Slots[vi].Offset  := arg.Data.Addr;
        Slots[vi].Pos     := arg.Pos;
        Slots[vi].BType   := bt;
      end;
      Result := GPR_POOL[vi];
      Exit;
    end;

    idx := FindCached(arg.Data.Addr, arg.Pos);
    if idx >= 0 then begin Result := GPR_POOL[idx]; Exit; end;

    idx := AllocSlot(ExclMask);
    FillChar(tmp, SizeOf(tmp), 0);
    tmp.Pos       := arg.Pos;
    tmp.Data.Addr := arg.Data.Addr;
    tmp.BaseType  := bt;
    Emitter^.Load_Int_Operand(tmp, GPR_POOL[idx]);
    Slots[idx].IsSet   := True;
    Slots[idx].IsDirty := False;
    Slots[idx].Offset  := arg.Data.Addr;
    Slots[idx].Pos     := arg.Pos;
    Slots[idx].BType   := bt;
    Result := GPR_POOL[idx];
  end else begin
    idx := AllocSlot(ExclMask);
    Emitter^.Load_Int_Operand(arg, GPR_POOL[idx]);
    Slots[idx].IsSet   := True;
    Slots[idx].IsDirty := False;
    Slots[idx].Offset  := -2;
    Slots[idx].Pos     := arg.Pos;
    Slots[idx].BType   := BaseJITType(arg.BaseType);
    Result := GPR_POOL[idx];
  end;
end;

function TGPRRegAllocator.GetResultReg(const dest: TOperand; ExclMask: Byte): EReg;
var vi: Integer;
begin
  if dest.Pos = mpLocal then begin
    vi := VIPSlotOf(dest.Data.Addr, mpLocal);
    if vi >= 0 then begin Result := GPR_POOL[vi]; Exit; end;
  end;
  Result := GetScratch(ExclMask);
end;

function TGPRRegAllocator.GetScratch(ExclMask: Byte): EReg;
var i: Integer;
begin
  for i := 0 to GPR_VIP_IDX_START-1 do
    if ((Byte(1 shl i) and ExclMask) = 0) and not Slots[i].IsSet then Exit(GPR_POOL[i]);
  for i := GPR_VIP_IDX_START to 4 do
    if ((Byte(1 shl i) and ExclMask) = 0) and not Slots[i].IsSet then Exit(GPR_POOL[i]);
  for i := 0 to GPR_VIP_IDX_START-1 do
    if (Byte(1 shl i) and ExclMask) = 0 then
    begin SpillSlot(i); ClearSlotAt(i); Exit(GPR_POOL[i]); end;
  for i := GPR_VIP_IDX_START to 4 do
    if (Byte(1 shl i) and ExclMask) = 0 then
    begin SpillSlot(i); ClearSlotAt(i); Exit(GPR_POOL[i]); end;
  SpillSlot(0); ClearSlotAt(0); Result := rax;
end;

procedure TGPRRegAllocator.SetResult(Reg: EReg; const dest: TOperand);
var
  ri, vi, fi, si, i: Integer;
  bt: EExpressBaseType;
begin
  if dest.Pos <> mpLocal then Exit;
  bt := BaseJITType(dest.BaseType);
  ri := IdxOf(Reg);
  vi := VIPSlotOf(dest.Data.Addr, mpLocal);

  if vi >= 0 then begin
    fi := vi;
    if ri <> vi then
      Emitter^.MOV_Reg_Reg(GPR_POOL[vi], Reg);
    if (ri >= 0) and (ri <> vi) and (Slots[ri].Offset = -2) then
      ClearSlotAt(ri);
  end else begin
    if (ri >= 0) and (ri < GPR_VIP_IDX_START) then
      fi := ri
    else begin
      si := AllocSlot(MaskOf(Reg));
      Emitter^.MOV_Reg_Reg(GPR_POOL[si], Reg);
      fi := si;
    end;
  end;

  for i := 0 to 4 do
    if (i <> fi) and Slots[i].IsSet and (Slots[i].Offset = dest.Data.Addr) and (Slots[i].Pos = dest.Pos) then
      ClearSlotAt(i);

  Slots[fi].IsSet   := True;
  Slots[fi].IsDirty := True;
  Slots[fi].Offset  := dest.Data.Addr;
  Slots[fi].Pos     := dest.Pos;
  Slots[fi].BType   := bt;
end;

procedure TGPRRegAllocator.SpillAll;
var i: Integer;
begin
  for i := 0 to 4 do SpillSlot(i);
end;

procedure TGPRRegAllocator.InvalidateAll;
var i: Integer;
begin
  SpillAll;
  for i := 0 to GPR_VIP_IDX_START-1 do ClearSlotAt(i);
//for i := 0 to 4 do ClearSlotAt(i);
end;

procedure TGPRRegAllocator.InvalidateAllIncludingVIP;
var
  i: Integer;
  preload: TOperand;
begin
  // Spill and clear non-VIP volatile slots as normal
  for i := 0 to GPR_VIP_IDX_START - 1 do
  begin
    SpillSlot(i);
    ClearSlotAt(i);
  end;

  // VIP slots: keep the slot metadata and physical register binding intact
  for i := GPR_VIP_IDX_START to GPR_VIP_IDX_START + VIPCount - 1 do
  begin
    FillChar(preload, SizeOf(preload), 0);
    preload.Pos       := Slots[i].Pos;
    preload.Data.Addr := Slots[i].Offset;
    preload.BaseType  := Slots[i].BType;
    Emitter^.Load_Int_Operand(preload, GPR_POOL[i]);
  end;
end;

