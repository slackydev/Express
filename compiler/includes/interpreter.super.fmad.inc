lbADDR:   PPointer(MEMBASE_0)^ := MEMBASE_1; Inc(pc);
lbDREF:
lbDREF32: PUInt32(MEMBASE_0)^ := PUInt32(PPointer(MEMBASE_1)^)^; Inc(pc);
lbDREF64: PUInt64(MEMBASE_0)^ := PUInt64(PPointer(MEMBASE_1)^)^; Inc(pc);

lbINC_i32: Inc( PInt32(BASEPTR_0)^); Inc(pc);
lbINC_u32: Inc(PUInt32(BASEPTR_0)^); Inc(pc);
lbINC_i64: Inc( PInt64(BASEPTR_0)^); Inc(pc);
lbINC_u64: Inc(PUInt64(BASEPTR_0)^); Inc(pc);

// --- FAST ADDRESSING OPERATIONS (FMA) ---
// Dest = BaseAddr + ElementIndex * ItemSize
lbFMAD64_64:
  if pc^.Args[3].Pos = mpLocal then PInt64(BASEPTR_3)^ := PUInt64(PPtrInt(MEMBASE_2)^ + PInt64(MEMBASE_0)^ * pc^.Args[1].Data.Addr)^
  else                              PInt64(MEMBASE_3)^ := PUInt64(PPtrInt(MEMBASE_2)^ + PInt64(MEMBASE_0)^ * pc^.Args[1].Data.Addr)^;
  Inc(pc);
lbFMAD64_32:
  if pc^.Args[3].Pos = mpLocal then PInt64(BASEPTR_3)^ := PUInt64(PPtrInt(MEMBASE_2)^ + PInt32(MEMBASE_0)^ * pc^.Args[1].Data.Addr)^
  else                              PInt64(MEMBASE_3)^ := PUInt64(PPtrInt(MEMBASE_2)^ + PInt32(MEMBASE_0)^ * pc^.Args[1].Data.Addr)^;
  Inc(pc);
lbFMAD32_64:
  if pc^.Args[3].Pos = mpLocal then PInt32(BASEPTR_3)^ := PUInt32(PPtrInt(MEMBASE_2)^ + PInt64(MEMBASE_0)^ * pc^.Args[1].Data.Addr)^
  else                              PInt32(MEMBASE_3)^ := PUInt32(PPtrInt(MEMBASE_2)^ + PInt64(MEMBASE_0)^ * pc^.Args[1].Data.Addr)^;
  Inc(pc);
lbFMAD32_32:
  if pc^.Args[3].Pos = mpLocal then PInt32(BASEPTR_3)^ := PUInt32(PPtrInt(MEMBASE_2)^ + PInt32(MEMBASE_0)^ * pc^.Args[1].Data.Addr)^
  else                              PInt32(MEMBASE_3)^ := PUInt32(PPtrInt(MEMBASE_2)^ + PInt32(MEMBASE_0)^ * pc^.Args[1].Data.Addr)^;
  Inc(pc);

// --- FMA (Fused Multiply-Add) ---
lbFMA_i8:
  if pc^.Args[3].Pos = mpLocal then PPtrInt(BASEPTR_3)^ := PPtrInt(MEMBASE_2)^ + PInt8(MEMBASE_0)^ * pc^.Args[1].Data.Addr
  else                              PPtrInt(MEMBASE_3)^ := PPtrInt(MEMBASE_2)^ + PInt8(MEMBASE_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_u8:
  if pc^.Args[3].Pos = mpLocal then PPtrInt(BASEPTR_3)^ := PPtrInt(MEMBASE_2)^ + PUInt8(MEMBASE_0)^ * pc^.Args[1].Data.Addr
  else                              PPtrInt(MEMBASE_3)^ := PPtrInt(MEMBASE_2)^ + PUInt8(MEMBASE_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_i16:
  if pc^.Args[3].Pos = mpLocal then PPtrInt(BASEPTR_3)^ := PPtrInt(MEMBASE_2)^ + PInt16(MEMBASE_0)^ * pc^.Args[1].Data.Addr
  else                              PPtrInt(MEMBASE_3)^ := PPtrInt(MEMBASE_2)^ + PInt16(MEMBASE_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_u16:
  if pc^.Args[3].Pos = mpLocal then PPtrInt(BASEPTR_3)^ := PPtrInt(MEMBASE_2)^ + PUInt16(MEMBASE_0)^ * pc^.Args[1].Data.Addr
  else                              PPtrInt(MEMBASE_3)^ := PPtrInt(MEMBASE_2)^ + PUInt16(MEMBASE_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_i32:
  if pc^.Args[3].Pos = mpLocal then PPtrInt(BASEPTR_3)^ := PPtrInt(MEMBASE_2)^ + PInt32(MEMBASE_0)^ * pc^.Args[1].Data.Addr
  else                              PPtrInt(MEMBASE_3)^ := PPtrInt(MEMBASE_2)^ + PInt32(MEMBASE_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_u32:
  if pc^.Args[3].Pos = mpLocal then PPtrInt(BASEPTR_3)^ := PPtrInt(MEMBASE_2)^ + PUInt32(MEMBASE_0)^ * pc^.Args[1].Data.Addr
  else                              PPtrInt(MEMBASE_3)^ := PPtrInt(MEMBASE_2)^ + PUInt32(MEMBASE_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_i64:
  if pc^.Args[3].Pos = mpLocal then PPtrInt(BASEPTR_3)^ := PPtrInt(MEMBASE_2)^ + PInt64(MEMBASE_0)^ * pc^.Args[1].Data.Addr
  else                              PPtrInt(MEMBASE_3)^ := PPtrInt(MEMBASE_2)^ + PInt64(MEMBASE_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_u64:
  if pc^.Args[3].Pos = mpLocal then PPtrInt(BASEPTR_3)^ := PPtrInt(MEMBASE_2)^ + PUInt64(MEMBASE_0)^ * pc^.Args[1].Data.Addr
  else                              PPtrInt(MEMBASE_3)^ := PPtrInt(MEMBASE_2)^ + PUInt64(MEMBASE_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
  
lbADD:
