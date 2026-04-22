(*
  Copy-patch JIT
*)

lbADDR:   PPointer(JITMASK_BASEPTR_0)^ := JITMASK_BASEPTR_1; Inc(pc);
lbDREF: (* for copy reference*)

lbDREF32: PUInt32(JITMASK_BASEPTR_0)^ := PUInt32(PPointer(JITMASK_BASEPTR_1)^)^; Inc(pc);
lbDREF64: PUInt64(JITMASK_BASEPTR_0)^ := PUInt64(PPointer(JITMASK_BASEPTR_1)^)^; Inc(pc);

lbINC_i32: Inc( PInt32(JITMASK_BASEPTR_0)^); Inc(pc);
lbINC_u32: Inc(PUInt32(JITMASK_BASEPTR_0)^); Inc(pc);
lbINC_i64: Inc( PInt64(JITMASK_BASEPTR_0)^); Inc(pc);
lbINC_u64: Inc(PUInt64(JITMASK_BASEPTR_0)^); Inc(pc);

// --- FAST ADDRESSING OPERATIONS (FMA) ---
// Dest = BaseAddr + ElementIndex * ItemSize
lbFMAD64_64:
  PInt64(JITMASK_BASEPTR_3)^ := PUInt64(PPtrInt(JITMASK_BASEPTR_2)^ + PInt64(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr)^;
  Inc(pc);
lbFMAD64_32:
  PInt64(JITMASK_BASEPTR_3)^ := PUInt64(PPtrInt(JITMASK_BASEPTR_2)^ + PInt32(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr)^;
  Inc(pc);
lbFMAD32_64:
  PInt32(JITMASK_BASEPTR_3)^ := PUInt32(PPtrInt(JITMASK_BASEPTR_2)^ + PInt64(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr)^;
  Inc(pc);
lbFMAD32_32:
  PInt32(JITMASK_BASEPTR_3)^ := PUInt32(PPtrInt(JITMASK_BASEPTR_2)^ + PInt32(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr)^;
  Inc(pc);

// --- FMA (Fused Multiply-Add) ---
lbFMA_i8:
  PPtrInt(JITMASK_BASEPTR_3)^ := PPtrInt(JITMASK_BASEPTR_2)^ + PInt8(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_u8:
  PPtrInt(JITMASK_BASEPTR_3)^ := PPtrInt(JITMASK_BASEPTR_2)^ + PUInt8(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_i16:
  PPtrInt(JITMASK_BASEPTR_3)^ := PPtrInt(JITMASK_BASEPTR_2)^ + PInt16(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_u16:
  PPtrInt(JITMASK_BASEPTR_3)^ := PPtrInt(JITMASK_BASEPTR_2)^ + PUInt16(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_i32:
  PPtrInt(JITMASK_BASEPTR_3)^ := PPtrInt(JITMASK_BASEPTR_2)^ + PInt32(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_u32:
  PPtrInt(JITMASK_BASEPTR_3)^ := PPtrInt(JITMASK_BASEPTR_2)^ + PUInt32(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_i64:
  PPtrInt(JITMASK_BASEPTR_3)^ := PPtrInt(JITMASK_BASEPTR_2)^ + PInt64(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_u64:
  PPtrInt(JITMASK_BASEPTR_3)^ := PPtrInt(JITMASK_BASEPTR_2)^ + PUInt64(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);


// ----------------------------------------------------------
// --- BINARY OPERATIONS ------------------------------------
// ----------------------------------------------------------

lbADD: (* for copy reference*)

lbADD_i32:
  PInt32  (JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ + PInt32  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbADD_u32:
  PUInt32 (JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ + PUInt32 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbADD_i64:
  PInt64  (JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ + PInt64  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbADD_u64:
  PUInt64 (JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ + PUInt64 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbADD_f32:
  PFloat32(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^ + PFloat32(JITMASK_BASEPTR_1)^;
  Inc(pc);
lbADD_f64:
  PFloat64(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^ + PFloat64(JITMASK_BASEPTR_1)^;
  Inc(pc);

// --- SUB (-) ---
lbSUB_i32:
  PInt32  (JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ - PInt32  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbSUB_u32:
  PUInt32 (JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ - PUInt32 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbSUB_i64:
  PInt64  (JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ - PInt64  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbSUB_u64:
  PUInt64 (JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ - PUInt64 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbSUB_f32:
  PFloat32(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^ - PFloat32(JITMASK_BASEPTR_1)^;
  Inc(pc);
lbSUB_f64:
  PFloat64(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^ - PFloat64(JITMASK_BASEPTR_1)^;
  Inc(pc);

// --- MUL (*) ---
lbMUL_i32:
  PInt32  (JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ * PInt32  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbMUL_u32:
  PUInt32 (JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ * PUInt32 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbMUL_i64:
  PInt64  (JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ * PInt64  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbMUL_u64:
  PUInt64 (JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ * PUInt64 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbMUL_f32:
  PFloat32(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^ * PFloat32(JITMASK_BASEPTR_1)^;
  Inc(pc);
lbMUL_f64:
  PFloat64(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^ * PFloat64(JITMASK_BASEPTR_1)^;
  Inc(pc);

// --- DIV (div / /) ---
lbDIV_i32:
  PInt32  (JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ div PInt32  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbDIV_u32:
  PUInt32 (JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ div PUInt32 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbDIV_i64:
  PInt64  (JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ div PInt64  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbDIV_u64:
  PUInt64 (JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ div PUInt64 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbDIV_f32:
  PFloat32(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^  /  PFloat32(JITMASK_BASEPTR_1)^;
  Inc(pc);
lbDIV_f64:
  PFloat64(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^  /  PFloat64(JITMASK_BASEPTR_1)^;
  Inc(pc);

// --- MOD (mod) ---
lbMOD_i32:
  PInt32  (JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ mod PInt32  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbMOD_u32:
  PUInt32 (JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ mod PUInt32 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbMOD_i64:
  PInt64  (JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ mod PInt64  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbMOD_u64:
  PUInt64 (JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ mod PUInt64 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbMOD_f32:
  PFloat32(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^ mod PFloat32(JITMASK_BASEPTR_1)^;
  Inc(pc);
lbMOD_f64:
  PFloat64(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^ mod PFloat64(JITMASK_BASEPTR_1)^;
  Inc(pc);

// --- POW (Power) ---
lbPOW_i32:
  PInt32  (JITMASK_BASEPTR_2)^ := Power(PInt32  (JITMASK_BASEPTR_0)^, PInt32  (JITMASK_BASEPTR_1)^);
  Inc(pc);
lbPOW_u32:
  PUInt32 (JITMASK_BASEPTR_2)^ := Power(PUInt32 (JITMASK_BASEPTR_0)^, PUInt32 (JITMASK_BASEPTR_1)^);
  Inc(pc);
lbPOW_i64:
  PInt64  (JITMASK_BASEPTR_2)^ := Power(PInt64  (JITMASK_BASEPTR_0)^, PInt64  (JITMASK_BASEPTR_1)^);
  Inc(pc);
lbPOW_u64:
  PUInt64 (JITMASK_BASEPTR_2)^ := Power(PUInt64 (JITMASK_BASEPTR_0)^, PUInt64 (JITMASK_BASEPTR_1)^);
  Inc(pc);
lbPOW_f32:
  PFloat32(JITMASK_BASEPTR_2)^ := Power(PFloat32(JITMASK_BASEPTR_0)^, PFloat32(JITMASK_BASEPTR_1)^);
  Inc(pc);
lbPOW_f64:
  PFloat64(JITMASK_BASEPTR_2)^ := Power(PFloat64(JITMASK_BASEPTR_0)^, PFloat64(JITMASK_BASEPTR_1)^);
  Inc(pc);

// -----------------------------------------------------------------------
// --- COMPARISON OPERATORS (Returns Boolean)                          ---
// -----------------------------------------------------------------------

// --- EQ (=) ---
lbEQ_i32:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ = PInt32  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbEQ_u32:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ = PUInt32 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbEQ_i64:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ = PInt64  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbEQ_u64:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ = PUInt64 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbEQ_f32:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^ = PFloat32(JITMASK_BASEPTR_1)^;
  Inc(pc);
lbEQ_f64:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^ = PFloat64(JITMASK_BASEPTR_1)^;
  Inc(pc);

// --- NE (<>) ---
lbNE_i32:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ <> PInt32  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbNE_u32:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ <> PUInt32 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbNE_i64:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ <> PInt64  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbNE_u64:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ <> PUInt64 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbNE_f32:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^ <> PFloat32(JITMASK_BASEPTR_1)^;
  Inc(pc);
lbNE_f64:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^ <> PFloat64(JITMASK_BASEPTR_1)^;
  Inc(pc);

// --- LT (<) ---
lbLT_i32:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ < PInt32  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbLT_u32:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ < PUInt32 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbLT_i64:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ < PInt64  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbLT_u64:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ < PUInt64 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbLT_f32:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^ < PFloat32(JITMASK_BASEPTR_1)^;
  Inc(pc);
lbLT_f64:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^ < PFloat64(JITMASK_BASEPTR_1)^;
  Inc(pc);

// --- GT (>) ---
lbGT_i32:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ > PInt32  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbGT_u32:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ > PUInt32 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbGT_i64:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ > PInt64  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbGT_u64:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ > PUInt64 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbGT_f32:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^ > PFloat32(JITMASK_BASEPTR_1)^;
  Inc(pc);
lbGT_f64:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^ > PFloat64(JITMASK_BASEPTR_1)^;
  Inc(pc);

// --- GTE (>=) ---
lbGTE_i32:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ >= PInt32  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbGTE_u32:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ >= PUInt32 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbGTE_i64:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ >= PInt64  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbGTE_u64:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ >= PUInt64 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbGTE_f32:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^ >= PFloat32(JITMASK_BASEPTR_1)^;
  Inc(pc);
lbGTE_f64:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^ >= PFloat64(JITMASK_BASEPTR_1)^;
  Inc(pc);

// --- LTE (<=) ---
lbLTE_i32:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ <= PInt32  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbLTE_u32:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ <= PUInt32 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbLTE_i64:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ <= PInt64  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbLTE_u64:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ <= PUInt64 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbLTE_f32:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^ <= PFloat32(JITMASK_BASEPTR_1)^;
  Inc(pc);
lbLTE_f64:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^ <= PFloat64(JITMASK_BASEPTR_1)^;
  Inc(pc);

// -----------------------------------------------------------------------
// --- BITWISE OPERATORS (Integers Only)                               ---
// -----------------------------------------------------------------------

// --- BND (and) ---
lbBND_i32:
  PInt32  (JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ and PInt32  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbBND_u32:
  PUInt32 (JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ and PUInt32 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbBND_i64:
  PInt64  (JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ and PInt64  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbBND_u64:
  PUInt64 (JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ and PUInt64 (JITMASK_BASEPTR_1)^;
  Inc(pc);

// --- SHL (shl) ---
lbSHL_i32:
  PInt32  (JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ shl PInt32  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbSHL_u32:
  PUInt32 (JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ shl PUInt32 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbSHL_i64:
  PInt64  (JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ shl PInt64  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbSHL_u64:
  PUInt64 (JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ shl PUInt64 (JITMASK_BASEPTR_1)^;
  Inc(pc);

// --- SHR (shr) ---
lbSHR_i32:
  PInt32  (JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ shr PInt32  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbSHR_u32:
  PUInt32 (JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ shr PUInt32 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbSHR_i64:
  PInt64  (JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ shr PInt64  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbSHR_u64:
  PUInt64 (JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ shr PUInt64 (JITMASK_BASEPTR_1)^;
  Inc(pc);

// --- XOR (xor) ---
lbXOR_i32:
  PInt32  (JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ xor PInt32  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbXOR_u32:
  PUInt32 (JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ xor PUInt32 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbXOR_i64:
  PInt64  (JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ xor PInt64  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbXOR_u64:
  PUInt64 (JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ xor PUInt64 (JITMASK_BASEPTR_1)^;
  Inc(pc);

// --- BOR (or) ---
lbBOR_i32:
  PInt32  (JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ or PInt32  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbBOR_u32:
  PUInt32 (JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ or PUInt32 (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbBOR_i64:
  PInt64  (JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ or PInt64  (JITMASK_BASEPTR_1)^;
  Inc(pc);
lbBOR_u64:
  PUInt64 (JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ or PUInt64 (JITMASK_BASEPTR_1)^;
  Inc(pc);

// --- SAR (Arithmetic Shift Right) ---
lbSAR_i32:
  PInt32  (JITMASK_BASEPTR_2)^ := Sar(PInt32  (JITMASK_BASEPTR_0)^, PInt32  (JITMASK_BASEPTR_1)^);
  Inc(pc);
lbSAR_u32:
  PUInt32 (JITMASK_BASEPTR_2)^ := Sar(PUInt32 (JITMASK_BASEPTR_0)^, PUInt32 (JITMASK_BASEPTR_1)^);
  Inc(pc);
lbSAR_i64:
  PInt64  (JITMASK_BASEPTR_2)^ := Sar(PInt64  (JITMASK_BASEPTR_0)^, PInt64  (JITMASK_BASEPTR_1)^);
  Inc(pc);
lbSAR_u64:
  PUInt64 (JITMASK_BASEPTR_2)^ := Sar(PUInt64 (JITMASK_BASEPTR_0)^, PUInt64 (JITMASK_BASEPTR_1)^);



// ----------------------------------------------------------
// --- ASSIGN OPERATIONS ------------------------------------
// ----------------------------------------------------------
lbMOV: (* for copy reference*)

  // --- Same-Size or Truncating ---
  lbMOV8:  PInt8 (JITMASK_BASEPTR_0)^ := PInt8 (JITMASK_BASEPTR_1)^; Inc(pc);
  lbMOV16: PInt16(JITMASK_BASEPTR_0)^ := PInt16(JITMASK_BASEPTR_1)^; Inc(pc);
  lbMOV32: PInt32(JITMASK_BASEPTR_0)^ := PInt32(JITMASK_BASEPTR_1)^; Inc(pc);
  lbMOV64: PInt64(JITMASK_BASEPTR_0)^ := PInt64(JITMASK_BASEPTR_1)^; Inc(pc);

  // --- Widening Signed (Sign Extension) ---
  lbMOVSX16_8:  PInt16(JITMASK_BASEPTR_0)^ := Int16(PInt8(JITMASK_BASEPTR_1)^); Inc(pc);
  lbMOVSX32_8:  PInt32(JITMASK_BASEPTR_0)^ := Int32(PInt8(JITMASK_BASEPTR_1)^); Inc(pc);
  lbMOVSX32_16: PInt32(JITMASK_BASEPTR_0)^ := Int32(PInt16(JITMASK_BASEPTR_1)^); Inc(pc);
  lbMOVSX64_8:  PInt64(JITMASK_BASEPTR_0)^ := Int64(PInt8(JITMASK_BASEPTR_1)^); Inc(pc);
  lbMOVSX64_16: PInt64(JITMASK_BASEPTR_0)^ := Int64(PInt16(JITMASK_BASEPTR_1)^); Inc(pc);
  lbMOVSX64_32: PInt64(JITMASK_BASEPTR_0)^ := Int64(PInt32(JITMASK_BASEPTR_1)^); Inc(pc);

  // --- Widening Unsigned (Zero Extension) ---
  lbMOVZX16_8:  PUInt16(JITMASK_BASEPTR_0)^ := UInt16(PUInt8(JITMASK_BASEPTR_1)^); Inc(pc);
  lbMOVZX32_8:  PUInt32(JITMASK_BASEPTR_0)^ := UInt32(PUInt8(JITMASK_BASEPTR_1)^); Inc(pc);
  lbMOVZX32_16: PUInt32(JITMASK_BASEPTR_0)^ := UInt32(PUInt16(JITMASK_BASEPTR_1)^); Inc(pc);
  lbMOVZX64_8:  PUInt64(JITMASK_BASEPTR_0)^ := UInt64(PUInt8(JITMASK_BASEPTR_1)^); Inc(pc);
  lbMOVZX64_16: PUInt64(JITMASK_BASEPTR_0)^ := UInt64(PUInt16(JITMASK_BASEPTR_1)^); Inc(pc);
  lbMOVZX64_32: PUInt64(JITMASK_BASEPTR_0)^ := UInt64(PUInt32(JITMASK_BASEPTR_1)^); Inc(pc);

  // --- Float Conversion ---
  lbMOVF32_i32: PFloat32(JITMASK_BASEPTR_0)^ := Float32(PInt32(JITMASK_BASEPTR_1)^); Inc(pc);
  lbMOVF32_u32: PFloat32(JITMASK_BASEPTR_0)^ := Float32(PUInt32(JITMASK_BASEPTR_1)^); Inc(pc);
  lbMOVF32_i64: PFloat32(JITMASK_BASEPTR_0)^ := Float32(PInt64(JITMASK_BASEPTR_1)^); Inc(pc);
  lbMOVF32_u64: PFloat32(JITMASK_BASEPTR_0)^ := Float32(PUInt64(JITMASK_BASEPTR_1)^); Inc(pc);
  lbMOVF32_f32: PFloat32(JITMASK_BASEPTR_0)^ := PFloat32(JITMASK_BASEPTR_1)^; Inc(pc);
  lbMOVF32_f64: PFloat32(JITMASK_BASEPTR_0)^ := Float32(PFloat64(JITMASK_BASEPTR_1)^); Inc(pc);

  lbMOVF64_i32: PFloat64(JITMASK_BASEPTR_0)^ := Float64(PInt32(JITMASK_BASEPTR_1)^); Inc(pc);
  lbMOVF64_u32: PFloat64(JITMASK_BASEPTR_0)^ := Float64(PUInt32(JITMASK_BASEPTR_1)^); Inc(pc);
  lbMOVF64_i64: PFloat64(JITMASK_BASEPTR_0)^ := Float64(PInt64(JITMASK_BASEPTR_1)^); Inc(pc);
  lbMOVF64_u64: PFloat64(JITMASK_BASEPTR_0)^ := Float64(PUInt64(JITMASK_BASEPTR_1)^); Inc(pc);
  lbMOVF64_f32: PFloat64(JITMASK_BASEPTR_0)^ := Float64(PFloat32(JITMASK_BASEPTR_1)^); Inc(pc);
  lbMOVF64_f64: PFloat64(JITMASK_BASEPTR_0)^ := PFloat64(JITMASK_BASEPTR_1)^; Inc(pc);


lbSTORE: (* for copy reference*)

  // --- Same-Size or Truncating ---
  lbSTORE8:  PInt8 (PPointer(JITMASK_BASEPTR_0)^)^ := PInt8 (JITMASK_BASEPTR_1)^; Inc(pc);
  lbSTORE16: PInt16(PPointer(JITMASK_BASEPTR_0)^)^ := PInt16(JITMASK_BASEPTR_1)^; Inc(pc);
  lbSTORE32: PInt32(PPointer(JITMASK_BASEPTR_0)^)^ := PInt32(JITMASK_BASEPTR_1)^; Inc(pc);
  lbSTORE64: PInt64(PPointer(JITMASK_BASEPTR_0)^)^ := PInt64(JITMASK_BASEPTR_1)^; Inc(pc);

  // --- Widening Signed (Sign Extension) ---
  lbSTORESX16_8:  PInt16(PPointer(JITMASK_BASEPTR_0)^)^ := Int16(PInt8(JITMASK_BASEPTR_1)^); Inc(pc);
  lbSTORESX32_8:  PInt32(PPointer(JITMASK_BASEPTR_0)^)^ := Int32(PInt8(JITMASK_BASEPTR_1)^); Inc(pc);
  lbSTORESX32_16: PInt32(PPointer(JITMASK_BASEPTR_0)^)^ := Int32(PInt16(JITMASK_BASEPTR_1)^); Inc(pc);
  lbSTORESX64_8:  PInt64(PPointer(JITMASK_BASEPTR_0)^)^ := Int64(PInt8(JITMASK_BASEPTR_1)^); Inc(pc);
  lbSTORESX64_16: PInt64(PPointer(JITMASK_BASEPTR_0)^)^ := Int64(PInt16(JITMASK_BASEPTR_1)^); Inc(pc);
  lbSTORESX64_32: PInt64(PPointer(JITMASK_BASEPTR_0)^)^ := Int64(PInt32(JITMASK_BASEPTR_1)^); Inc(pc);

  // --- Widening Unsigned (Zero Extension) ---
  lbSTOREZX16_8:  PUInt16(PPointer(JITMASK_BASEPTR_0)^)^ := UInt16(PUInt8(JITMASK_BASEPTR_1)^); Inc(pc);
  lbSTOREZX32_8:  PUInt32(PPointer(JITMASK_BASEPTR_0)^)^ := UInt32(PUInt8(JITMASK_BASEPTR_1)^); Inc(pc);
  lbSTOREZX32_16: PUInt32(PPointer(JITMASK_BASEPTR_0)^)^ := UInt32(PUInt16(JITMASK_BASEPTR_1)^); Inc(pc);
  lbSTOREZX64_8:  PUInt64(PPointer(JITMASK_BASEPTR_0)^)^ := UInt64(PUInt8(JITMASK_BASEPTR_1)^); Inc(pc);
  lbSTOREZX64_16: PUInt64(PPointer(JITMASK_BASEPTR_0)^)^ := UInt64(PUInt16(JITMASK_BASEPTR_1)^); Inc(pc);
  lbSTOREZX64_32: PUInt64(PPointer(JITMASK_BASEPTR_0)^)^ := UInt64(PUInt32(JITMASK_BASEPTR_1)^); Inc(pc);

  // --- Float Conversion ---
  lbSTOREF32_i32: PFloat32(PPointer(JITMASK_BASEPTR_0)^)^ := Float32(PInt32(JITMASK_BASEPTR_1)^); Inc(pc);
  lbSTOREF32_u32: PFloat32(PPointer(JITMASK_BASEPTR_0)^)^ := Float32(PUInt32(JITMASK_BASEPTR_1)^); Inc(pc);
  lbSTOREF32_i64: PFloat32(PPointer(JITMASK_BASEPTR_0)^)^ := Float32(PInt64(JITMASK_BASEPTR_1)^); Inc(pc);
  lbSTOREF32_u64: PFloat32(PPointer(JITMASK_BASEPTR_0)^)^ := Float32(PUInt64(JITMASK_BASEPTR_1)^); Inc(pc);
  lbSTOREF32_f32: PFloat32(PPointer(JITMASK_BASEPTR_0)^)^ := PFloat32(JITMASK_BASEPTR_1)^; Inc(pc);
  lbSTOREF32_f64: PFloat32(PPointer(JITMASK_BASEPTR_0)^)^ := Float32(PFloat64(JITMASK_BASEPTR_1)^); Inc(pc);

  lbSTOREF64_i32: PFloat64(PPointer(JITMASK_BASEPTR_0)^)^ := Float64(PInt32(JITMASK_BASEPTR_1)^); Inc(pc);
  lbSTOREF64_u32: PFloat64(PPointer(JITMASK_BASEPTR_0)^)^ := Float64(PUInt32(JITMASK_BASEPTR_1)^); Inc(pc);
  lbSTOREF64_i64: PFloat64(PPointer(JITMASK_BASEPTR_0)^)^ := Float64(PInt64(JITMASK_BASEPTR_1)^); Inc(pc);
  lbSTOREF64_u64: PFloat64(PPointer(JITMASK_BASEPTR_0)^)^ := Float64(PUInt64(JITMASK_BASEPTR_1)^); Inc(pc);
  lbSTOREF64_f32: PFloat64(PPointer(JITMASK_BASEPTR_0)^)^ := Float64(PFloat32(JITMASK_BASEPTR_1)^); Inc(pc);
  lbSTOREF64_f64: PFloat64(PPointer(JITMASK_BASEPTR_0)^)^ := PFloat64(JITMASK_BASEPTR_1)^; Inc(pc);        

lbERROR: (* for copy reference*)
