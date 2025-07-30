lbINC_i32: Inc( PInt32(Pointer(StackPtr - pc^.Args[0].Data.Addr))^); Inc(pc);
lbINC_u32: Inc(PUInt32(Pointer(StackPtr - pc^.Args[0].Data.Addr))^); Inc(pc);
lbINC_i64: Inc( PInt64(Pointer(StackPtr - pc^.Args[0].Data.Addr))^); Inc(pc);
lbINC_u64: Inc(PUInt64(Pointer(StackPtr - pc^.Args[0].Data.Addr))^); Inc(pc);


// --- Specialized FMA operations ---
lbFMA_i8:  PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PInt8(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr; Inc(pc);
lbFMA_u8:  PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PUInt8(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr; Inc(pc);
lbFMA_i16: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PInt16(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr; Inc(pc);
lbFMA_u16: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PUInt16(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr; Inc(pc);
lbFMA_i32: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PInt32(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr; Inc(pc);
lbFMA_u32: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PUInt32(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr; Inc(pc);
lbFMA_i64: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PInt64(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr; Inc(pc);
lbFMA_u64: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PUInt64(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr; Inc(pc);

lbFMA_imm_i8:  PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + Int8(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr; Inc(pc);
lbFMA_imm_u8:  PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + UInt8(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr; Inc(pc);
lbFMA_imm_i16: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + Int16(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr; Inc(pc);
lbFMA_imm_u16: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + UInt16(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr; Inc(pc);
lbFMA_imm_i32: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + Int32(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr; Inc(pc);
lbFMA_imm_u32: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + UInt32(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr; Inc(pc);
lbFMA_imm_i64: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + Int64(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr; Inc(pc);
lbFMA_imm_u64: PPtrInt(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + UInt64(pc^.Args[0].Data.Addr) * pc^.Args[1].Data.Addr; Inc(pc);

// --- DREF operations ---
lbDREF:    Move(Pointer(Pointer(StackPtr - pc^.Args[1].Data.Addr)^)^, Pointer(StackPtr - pc^.Args[0].Data.Addr)^, pc^.Args[2].Data.Arg); Inc(pc);
lbDREF_32: PUInt32(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ := PUInt32(Pointer(StackPtr - pc^.Args[1].Data.Addr)^)^; Inc(pc);
lbDREF_64: PUInt64(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ := PUInt64(Pointer(StackPtr - pc^.Args[1].Data.Addr)^)^; Inc(pc);

// --- FMAD operations ---
lbFMAD_d64_64: PInt64(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PInt64(PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PInt64(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr)^; Inc(pc);
lbFMAD_d64_32: PInt64(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PInt64(PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PInt32(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr)^; Inc(pc);
lbFMAD_d32_64: PInt32(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PInt64(PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PInt64(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr)^; Inc(pc);
lbFMAD_d32_32: PInt32(Pointer(StackPtr - pc^.Args[3].Data.Addr))^ := PInt64(PPtrInt(Pointer(StackPtr - pc^.Args[2].Data.Addr))^ + PInt32(Pointer(StackPtr - pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.Addr)^; Inc(pc);

lbADD:
