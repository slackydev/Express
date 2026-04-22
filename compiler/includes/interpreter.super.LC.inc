(*
  Copy-patch JIT [LC]
*)

lbADDR_LC:   PPointer(JITMASK_BASEPTR_0)^ := JITMASK_CONSTPTR_1; Inc(pc);
lbDREF_LC: (* for copy reference*)

lbDREF32_LC: PUInt32(JITMASK_BASEPTR_0)^ := PUInt32(PPointer(JITMASK_CONSTPTR_1)^)^; Inc(pc);
lbDREF64_LC: PUInt64(JITMASK_BASEPTR_0)^ := PUInt64(PPointer(JITMASK_CONSTPTR_1)^)^; Inc(pc);

lbINC_i32_LC: Inc( PInt32(JITMASK_BASEPTR_0)^); Inc(pc);
lbINC_u32_LC: Inc(PUInt32(JITMASK_BASEPTR_0)^); Inc(pc);
lbINC_i64_LC: Inc( PInt64(JITMASK_BASEPTR_0)^); Inc(pc);
lbINC_u64_LC: Inc(PUInt64(JITMASK_BASEPTR_0)^); Inc(pc);

// --- FAST ADDRESSING OPERATIONS (FMA) ---
// Dest = BaseAddr + ElementIndex * ItemSize
lbFMAD64_64_LC:
  PInt64(JITMASK_BASEPTR_3)^ := PUInt64(PPtrInt(JITMASK_BASEPTR_2)^ + PInt64(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr)^;
  Inc(pc);
lbFMAD64_32_LC:
  PInt64(JITMASK_BASEPTR_3)^ := PUInt64(PPtrInt(JITMASK_BASEPTR_2)^ + PInt32(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr)^;
  Inc(pc);
lbFMAD32_64_LC:
  PInt32(JITMASK_BASEPTR_3)^ := PUInt32(PPtrInt(JITMASK_BASEPTR_2)^ + PInt64(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr)^;
  Inc(pc);
lbFMAD32_32_LC:
  PInt32(JITMASK_BASEPTR_3)^ := PUInt32(PPtrInt(JITMASK_BASEPTR_2)^ + PInt32(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr)^;
  Inc(pc);

// --- FMA (Fused Multiply-Add) ---
lbFMA_i8_LC:
  PPtrInt(JITMASK_BASEPTR_3)^ := PPtrInt(JITMASK_BASEPTR_2)^ + PInt8(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_u8_LC:
  PPtrInt(JITMASK_BASEPTR_3)^ := PPtrInt(JITMASK_BASEPTR_2)^ + PUInt8(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_i16_LC:
  PPtrInt(JITMASK_BASEPTR_3)^ := PPtrInt(JITMASK_BASEPTR_2)^ + PInt16(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_u16_LC:
  PPtrInt(JITMASK_BASEPTR_3)^ := PPtrInt(JITMASK_BASEPTR_2)^ + PUInt16(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_i32_LC:
  PPtrInt(JITMASK_BASEPTR_3)^ := PPtrInt(JITMASK_BASEPTR_2)^ + PInt32(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_u32_LC:
  PPtrInt(JITMASK_BASEPTR_3)^ := PPtrInt(JITMASK_BASEPTR_2)^ + PUInt32(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_i64_LC:
  PPtrInt(JITMASK_BASEPTR_3)^ := PPtrInt(JITMASK_BASEPTR_2)^ + PInt64(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);
lbFMA_u64_LC:
  PPtrInt(JITMASK_BASEPTR_3)^ := PPtrInt(JITMASK_BASEPTR_2)^ + PUInt64(JITMASK_BASEPTR_0)^ * pc^.Args[1].Data.Addr;
  Inc(pc);


// ----------------------------------------------------------
// --- BINARY OPERATIONS ------------------------------------
// ----------------------------------------------------------

lbADD_LC: (* for copy reference*)

lbADD_i32_LC:
  PInt32  (JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ + PInt32  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbADD_u32_LC:
  PUInt32 (JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ + PUInt32 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbADD_i64_LC:
  PInt64  (JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ + PInt64  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbADD_u64_LC:
  PUInt64 (JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ + PUInt64 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbADD_f32_LC:
  PFloat32(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^ + PFloat32(JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbADD_f64_LC:
  PFloat64(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^ + PFloat64(JITMASK_CONSTPTR_1)^;
  Inc(pc);

// --- SUB (-) ---
lbSUB_i32_LC:
  PInt32  (JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ - PInt32  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbSUB_u32_LC:
  PUInt32 (JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ - PUInt32 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbSUB_i64_LC:
  PInt64  (JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ - PInt64  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbSUB_u64_LC:
  PUInt64 (JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ - PUInt64 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbSUB_f32_LC:
  PFloat32(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^ - PFloat32(JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbSUB_f64_LC:
  PFloat64(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^ - PFloat64(JITMASK_CONSTPTR_1)^;
  Inc(pc);

// --- MUL (*) ---
lbMUL_i32_LC:
  PInt32  (JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ * PInt32  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbMUL_u32_LC:
  PUInt32 (JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ * PUInt32 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbMUL_i64_LC:
  PInt64  (JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ * PInt64  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbMUL_u64_LC:
  PUInt64 (JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ * PUInt64 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbMUL_f32_LC:
  PFloat32(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^ * PFloat32(JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbMUL_f64_LC:
  PFloat64(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^ * PFloat64(JITMASK_CONSTPTR_1)^;
  Inc(pc);

// --- DIV (div / /) ---
lbDIV_i32_LC:
  PInt32  (JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ div PInt32  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbDIV_u32_LC:
  PUInt32 (JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ div PUInt32 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbDIV_i64_LC:
  PInt64  (JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ div PInt64  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbDIV_u64_LC:
  PUInt64 (JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ div PUInt64 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbDIV_f32_LC:
  PFloat32(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^  /  PFloat32(JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbDIV_f64_LC:
  PFloat64(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^  /  PFloat64(JITMASK_CONSTPTR_1)^;
  Inc(pc);

// --- MOD (mod) ---
lbMOD_i32_LC:
  PInt32  (JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ mod PInt32  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbMOD_u32_LC:
  PUInt32 (JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ mod PUInt32 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbMOD_i64_LC:
  PInt64  (JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ mod PInt64  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbMOD_u64_LC:
  PUInt64 (JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ mod PUInt64 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbMOD_f32_LC:
  PFloat32(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^ mod PFloat32(JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbMOD_f64_LC:
  PFloat64(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^ mod PFloat64(JITMASK_CONSTPTR_1)^;
  Inc(pc);

// --- POW (Power) ---
lbPOW_i32_LC:
  PInt32  (JITMASK_BASEPTR_2)^ := Power(PInt32  (JITMASK_BASEPTR_0)^, PInt32  (JITMASK_CONSTPTR_1)^);
  Inc(pc);
lbPOW_u32_LC:
  PUInt32 (JITMASK_BASEPTR_2)^ := Power(PUInt32 (JITMASK_BASEPTR_0)^, PUInt32 (JITMASK_CONSTPTR_1)^);
  Inc(pc);
lbPOW_i64_LC:
  PInt64  (JITMASK_BASEPTR_2)^ := Power(PInt64  (JITMASK_BASEPTR_0)^, PInt64  (JITMASK_CONSTPTR_1)^);
  Inc(pc);
lbPOW_u64_LC:
  PUInt64 (JITMASK_BASEPTR_2)^ := Power(PUInt64 (JITMASK_BASEPTR_0)^, PUInt64 (JITMASK_CONSTPTR_1)^);
  Inc(pc);
lbPOW_f32_LC:
  PFloat32(JITMASK_BASEPTR_2)^ := Power(PFloat32(JITMASK_BASEPTR_0)^, PFloat32(JITMASK_CONSTPTR_1)^);
  Inc(pc);
lbPOW_f64_LC:
  PFloat64(JITMASK_BASEPTR_2)^ := Power(PFloat64(JITMASK_BASEPTR_0)^, PFloat64(JITMASK_CONSTPTR_1)^);
  Inc(pc);

// -----------------------------------------------------------------------
// --- COMPARISON OPERATORS (Returns Boolean)                          ---
// -----------------------------------------------------------------------

// --- EQ (=) ---
lbEQ_i32_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ = PInt32  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbEQ_u32_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ = PUInt32 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbEQ_i64_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ = PInt64  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbEQ_u64_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ = PUInt64 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbEQ_f32_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^ = PFloat32(JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbEQ_f64_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^ = PFloat64(JITMASK_CONSTPTR_1)^;
  Inc(pc);

// --- NE (<>) ---
lbNE_i32_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ <> PInt32  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbNE_u32_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ <> PUInt32 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbNE_i64_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ <> PInt64  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbNE_u64_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ <> PUInt64 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbNE_f32_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^ <> PFloat32(JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbNE_f64_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^ <> PFloat64(JITMASK_CONSTPTR_1)^;
  Inc(pc);

// --- LT (<) ---
lbLT_i32_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ < PInt32  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbLT_u32_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ < PUInt32 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbLT_i64_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ < PInt64  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbLT_u64_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ < PUInt64 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbLT_f32_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^ < PFloat32(JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbLT_f64_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^ < PFloat64(JITMASK_CONSTPTR_1)^;
  Inc(pc);

// --- GT (>) ---
lbGT_i32_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ > PInt32  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbGT_u32_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ > PUInt32 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbGT_i64_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ > PInt64  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbGT_u64_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ > PUInt64 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbGT_f32_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^ > PFloat32(JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbGT_f64_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^ > PFloat64(JITMASK_CONSTPTR_1)^;
  Inc(pc);

// --- GTE (>=) ---
lbGTE_i32_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ >= PInt32  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbGTE_u32_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ >= PUInt32 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbGTE_i64_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ >= PInt64  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbGTE_u64_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ >= PUInt64 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbGTE_f32_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^ >= PFloat32(JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbGTE_f64_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^ >= PFloat64(JITMASK_CONSTPTR_1)^;
  Inc(pc);

// --- LTE (<=) ---
lbLTE_i32_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ <= PInt32  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbLTE_u32_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ <= PUInt32 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbLTE_i64_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ <= PInt64  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbLTE_u64_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ <= PUInt64 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbLTE_f32_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat32(JITMASK_BASEPTR_0)^ <= PFloat32(JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbLTE_f64_LC:
  PBoolean(JITMASK_BASEPTR_2)^ := PFloat64(JITMASK_BASEPTR_0)^ <= PFloat64(JITMASK_CONSTPTR_1)^;
  Inc(pc);

// -----------------------------------------------------------------------
// --- BITWISE OPERATORS (Integers Only)                               ---
// -----------------------------------------------------------------------

// --- BND (and) ---
lbBND_i32_LC:
  PInt32  (JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ and PInt32  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbBND_u32_LC:
  PUInt32 (JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ and PUInt32 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbBND_i64_LC:
  PInt64  (JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ and PInt64  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbBND_u64_LC:
  PUInt64 (JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ and PUInt64 (JITMASK_CONSTPTR_1)^;
  Inc(pc);

// --- SHL (shl) ---
lbSHL_i32_LC:
  PInt32  (JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ shl PInt32  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbSHL_u32_LC:
  PUInt32 (JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ shl PUInt32 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbSHL_i64_LC:
  PInt64  (JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ shl PInt64  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbSHL_u64_LC:
  PUInt64 (JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ shl PUInt64 (JITMASK_CONSTPTR_1)^;
  Inc(pc);

// --- SHR (shr) ---
lbSHR_i32_LC:
  PInt32  (JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ shr PInt32  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbSHR_u32_LC:
  PUInt32 (JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ shr PUInt32 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbSHR_i64_LC:
  PInt64  (JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ shr PInt64  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbSHR_u64_LC:
  PUInt64 (JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ shr PUInt64 (JITMASK_CONSTPTR_1)^;
  Inc(pc);

// --- XOR (xor) ---
lbXOR_i32_LC:
  PInt32  (JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ xor PInt32  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbXOR_u32_LC:
  PUInt32 (JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ xor PUInt32 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbXOR_i64_LC:
  PInt64  (JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ xor PInt64  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbXOR_u64_LC:
  PUInt64 (JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ xor PUInt64 (JITMASK_CONSTPTR_1)^;
  Inc(pc);

// --- BOR (or) ---
lbBOR_i32_LC:
  PInt32  (JITMASK_BASEPTR_2)^ := PInt32  (JITMASK_BASEPTR_0)^ or PInt32  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbBOR_u32_LC:
  PUInt32 (JITMASK_BASEPTR_2)^ := PUInt32 (JITMASK_BASEPTR_0)^ or PUInt32 (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbBOR_i64_LC:
  PInt64  (JITMASK_BASEPTR_2)^ := PInt64  (JITMASK_BASEPTR_0)^ or PInt64  (JITMASK_CONSTPTR_1)^;
  Inc(pc);
lbBOR_u64_LC:
  PUInt64 (JITMASK_BASEPTR_2)^ := PUInt64 (JITMASK_BASEPTR_0)^ or PUInt64 (JITMASK_CONSTPTR_1)^;
  Inc(pc);

// --- SAR (Arithmetic Shift Right) ---
lbSAR_i32_LC:
  PInt32  (JITMASK_BASEPTR_2)^ := Sar(PInt32  (JITMASK_BASEPTR_0)^, PInt32  (JITMASK_CONSTPTR_1)^);
  Inc(pc);
lbSAR_u32_LC:
  PUInt32 (JITMASK_BASEPTR_2)^ := Sar(PUInt32 (JITMASK_BASEPTR_0)^, PUInt32 (JITMASK_CONSTPTR_1)^);
  Inc(pc);
lbSAR_i64_LC:
  PInt64  (JITMASK_BASEPTR_2)^ := Sar(PInt64  (JITMASK_BASEPTR_0)^, PInt64  (JITMASK_CONSTPTR_1)^);
  Inc(pc);
lbSAR_u64_LC:
  PUInt64 (JITMASK_BASEPTR_2)^ := Sar(PUInt64 (JITMASK_BASEPTR_0)^, PUInt64 (JITMASK_CONSTPTR_1)^);



// ----------------------------------------------------------
// --- ASSIGN OPERATIONS ------------------------------------
// ----------------------------------------------------------
lbMOV_LC: (* for copy reference*)

  // --- Same-Size or Truncating ---
  lbMOV8_LC:  PInt8 (JITMASK_BASEPTR_0)^ := PInt8 (JITMASK_CONSTPTR_1)^; Inc(pc);
  lbMOV16_LC: PInt16(JITMASK_BASEPTR_0)^ := PInt16(JITMASK_CONSTPTR_1)^; Inc(pc);
  lbMOV32_LC: PInt32(JITMASK_BASEPTR_0)^ := PInt32(JITMASK_CONSTPTR_1)^; Inc(pc);
  lbMOV64_LC: PInt64(JITMASK_BASEPTR_0)^ := PInt64(JITMASK_CONSTPTR_1)^; Inc(pc);

  // --- Widening Signed (Sign Extension) ---
  lbMOVSX16_8_LC:  PInt16(JITMASK_BASEPTR_0)^ := Int16(PInt8(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbMOVSX32_8_LC:  PInt32(JITMASK_BASEPTR_0)^ := Int32(PInt8(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbMOVSX32_16_LC: PInt32(JITMASK_BASEPTR_0)^ := Int32(PInt16(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbMOVSX64_8_LC:  PInt64(JITMASK_BASEPTR_0)^ := Int64(PInt8(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbMOVSX64_16_LC: PInt64(JITMASK_BASEPTR_0)^ := Int64(PInt16(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbMOVSX64_32_LC: PInt64(JITMASK_BASEPTR_0)^ := Int64(PInt32(JITMASK_CONSTPTR_1)^); Inc(pc);

  // --- Widening Unsigned (Zero Extension) ---
  lbMOVZX16_8_LC:  PUInt16(JITMASK_BASEPTR_0)^ := UInt16(PUInt8(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbMOVZX32_8_LC:  PUInt32(JITMASK_BASEPTR_0)^ := UInt32(PUInt8(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbMOVZX32_16_LC: PUInt32(JITMASK_BASEPTR_0)^ := UInt32(PUInt16(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbMOVZX64_8_LC:  PUInt64(JITMASK_BASEPTR_0)^ := UInt64(PUInt8(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbMOVZX64_16_LC: PUInt64(JITMASK_BASEPTR_0)^ := UInt64(PUInt16(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbMOVZX64_32_LC: PUInt64(JITMASK_BASEPTR_0)^ := UInt64(PUInt32(JITMASK_CONSTPTR_1)^); Inc(pc);

  // --- Float Conversion ---
  lbMOVF32_i32_LC: PFloat32(JITMASK_BASEPTR_0)^ := Float32(PInt32(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbMOVF32_u32_LC: PFloat32(JITMASK_BASEPTR_0)^ := Float32(PUInt32(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbMOVF32_i64_LC: PFloat32(JITMASK_BASEPTR_0)^ := Float32(PInt64(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbMOVF32_u64_LC: PFloat32(JITMASK_BASEPTR_0)^ := Float32(PUInt64(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbMOVF32_f32_LC: PFloat32(JITMASK_BASEPTR_0)^ := PFloat32(JITMASK_CONSTPTR_1)^; Inc(pc);
  lbMOVF32_f64_LC: PFloat32(JITMASK_BASEPTR_0)^ := Float32(PFloat64(JITMASK_CONSTPTR_1)^); Inc(pc);

  lbMOVF64_i32_LC: PFloat64(JITMASK_BASEPTR_0)^ := Float64(PInt32(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbMOVF64_u32_LC: PFloat64(JITMASK_BASEPTR_0)^ := Float64(PUInt32(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbMOVF64_i64_LC: PFloat64(JITMASK_BASEPTR_0)^ := Float64(PInt64(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbMOVF64_u64_LC: PFloat64(JITMASK_BASEPTR_0)^ := Float64(PUInt64(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbMOVF64_f32_LC: PFloat64(JITMASK_BASEPTR_0)^ := Float64(PFloat32(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbMOVF64_f64_LC: PFloat64(JITMASK_BASEPTR_0)^ := PFloat64(JITMASK_CONSTPTR_1)^; Inc(pc);


lbSTORE_LC: (* for copy reference*)

  // --- Same-Size or Truncating ---
  lbSTORE8_LC:  PInt8 (PPointer(JITMASK_BASEPTR_0)^)^ := PInt8 (JITMASK_CONSTPTR_1)^; Inc(pc);
  lbSTORE16_LC: PInt16(PPointer(JITMASK_BASEPTR_0)^)^ := PInt16(JITMASK_CONSTPTR_1)^; Inc(pc);
  lbSTORE32_LC: PInt32(PPointer(JITMASK_BASEPTR_0)^)^ := PInt32(JITMASK_CONSTPTR_1)^; Inc(pc);
  lbSTORE64_LC: PInt64(PPointer(JITMASK_BASEPTR_0)^)^ := PInt64(JITMASK_CONSTPTR_1)^; Inc(pc);

  // --- Widening Signed (Sign Extension) ---
  lbSTORESX16_8_LC:  PInt16(PPointer(JITMASK_BASEPTR_0)^)^ := Int16(PInt8(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbSTORESX32_8_LC:  PInt32(PPointer(JITMASK_BASEPTR_0)^)^ := Int32(PInt8(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbSTORESX32_16_LC: PInt32(PPointer(JITMASK_BASEPTR_0)^)^ := Int32(PInt16(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbSTORESX64_8_LC:  PInt64(PPointer(JITMASK_BASEPTR_0)^)^ := Int64(PInt8(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbSTORESX64_16_LC: PInt64(PPointer(JITMASK_BASEPTR_0)^)^ := Int64(PInt16(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbSTORESX64_32_LC: PInt64(PPointer(JITMASK_BASEPTR_0)^)^ := Int64(PInt32(JITMASK_CONSTPTR_1)^); Inc(pc);

  // --- Widening Unsigned (Zero Extension) ---
  lbSTOREZX16_8_LC:  PUInt16(PPointer(JITMASK_BASEPTR_0)^)^ := UInt16(PUInt8(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbSTOREZX32_8_LC:  PUInt32(PPointer(JITMASK_BASEPTR_0)^)^ := UInt32(PUInt8(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbSTOREZX32_16_LC: PUInt32(PPointer(JITMASK_BASEPTR_0)^)^ := UInt32(PUInt16(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbSTOREZX64_8_LC:  PUInt64(PPointer(JITMASK_BASEPTR_0)^)^ := UInt64(PUInt8(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbSTOREZX64_16_LC: PUInt64(PPointer(JITMASK_BASEPTR_0)^)^ := UInt64(PUInt16(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbSTOREZX64_32_LC: PUInt64(PPointer(JITMASK_BASEPTR_0)^)^ := UInt64(PUInt32(JITMASK_CONSTPTR_1)^); Inc(pc);

  // --- Float Conversion ---
  lbSTOREF32_i32_LC: PFloat32(PPointer(JITMASK_BASEPTR_0)^)^ := Float32(PInt32(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbSTOREF32_u32_LC: PFloat32(PPointer(JITMASK_BASEPTR_0)^)^ := Float32(PUInt32(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbSTOREF32_i64_LC: PFloat32(PPointer(JITMASK_BASEPTR_0)^)^ := Float32(PInt64(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbSTOREF32_u64_LC: PFloat32(PPointer(JITMASK_BASEPTR_0)^)^ := Float32(PUInt64(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbSTOREF32_f32_LC: PFloat32(PPointer(JITMASK_BASEPTR_0)^)^ := PFloat32(JITMASK_CONSTPTR_1)^; Inc(pc);
  lbSTOREF32_f64_LC: PFloat32(PPointer(JITMASK_BASEPTR_0)^)^ := Float32(PFloat64(JITMASK_CONSTPTR_1)^); Inc(pc);

  lbSTOREF64_i32_LC: PFloat64(PPointer(JITMASK_BASEPTR_0)^)^ := Float64(PInt32(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbSTOREF64_u32_LC: PFloat64(PPointer(JITMASK_BASEPTR_0)^)^ := Float64(PUInt32(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbSTOREF64_i64_LC: PFloat64(PPointer(JITMASK_BASEPTR_0)^)^ := Float64(PInt64(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbSTOREF64_u64_LC: PFloat64(PPointer(JITMASK_BASEPTR_0)^)^ := Float64(PUInt64(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbSTOREF64_f32_LC: PFloat64(PPointer(JITMASK_BASEPTR_0)^)^ := Float64(PFloat32(JITMASK_CONSTPTR_1)^); Inc(pc);
  lbSTOREF64_f64_LC: PFloat64(PPointer(JITMASK_BASEPTR_0)^)^ := PFloat64(JITMASK_CONSTPTR_1)^; Inc(pc);        

lbERROR_LC: (* for copy reference*)
