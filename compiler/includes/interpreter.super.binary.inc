lbADD_i32:
  if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := PInt32  (MEMBASE_0)^ + PInt32  (MEMBASE_1)^
  else                              PInt32  (MEMBASE_2)^ := PInt32  (MEMBASE_0)^ + PInt32  (MEMBASE_1)^;
  Inc(pc);
lbADD_u32:
  if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ + PUInt32 (MEMBASE_1)^
  else                              PUInt32 (MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ + PUInt32 (MEMBASE_1)^;
  Inc(pc);
lbADD_i64:
  if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := PInt64  (MEMBASE_0)^ + PInt64  (MEMBASE_1)^
  else                              PInt64  (MEMBASE_2)^ := PInt64  (MEMBASE_0)^ + PInt64  (MEMBASE_1)^;
  Inc(pc);
lbADD_u64:
  if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ + PUInt64 (MEMBASE_1)^
  else                              PUInt64 (MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ + PUInt64 (MEMBASE_1)^;
  Inc(pc);
lbADD_f32:
  if pc^.Args[2].Pos = mpLocal then PFloat32(BASEPTR_2)^ := PFloat32(MEMBASE_0)^ + PFloat32(MEMBASE_1)^
  else                              PFloat32(MEMBASE_2)^ := PFloat32(MEMBASE_0)^ + PFloat32(MEMBASE_1)^;
  Inc(pc);
lbADD_f64:
  if pc^.Args[2].Pos = mpLocal then PFloat64(BASEPTR_2)^ := PFloat64(MEMBASE_0)^ + PFloat64(MEMBASE_1)^
  else                              PFloat64(MEMBASE_2)^ := PFloat64(MEMBASE_0)^ + PFloat64(MEMBASE_1)^;
  Inc(pc);

// --- SUB (-) ---
lbSUB_i32:
  if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := PInt32  (MEMBASE_0)^ - PInt32  (MEMBASE_1)^
  else                              PInt32  (MEMBASE_2)^ := PInt32  (MEMBASE_0)^ - PInt32  (MEMBASE_1)^;
  Inc(pc);
lbSUB_u32:
  if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ - PUInt32 (MEMBASE_1)^
  else                              PUInt32 (MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ - PUInt32 (MEMBASE_1)^;
  Inc(pc);
lbSUB_i64:
  if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := PInt64  (MEMBASE_0)^ - PInt64  (MEMBASE_1)^
  else                              PInt64  (MEMBASE_2)^ := PInt64  (MEMBASE_0)^ - PInt64  (MEMBASE_1)^;
  Inc(pc);
lbSUB_u64:
  if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ - PUInt64 (MEMBASE_1)^
  else                              PUInt64 (MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ - PUInt64 (MEMBASE_1)^;
  Inc(pc);
lbSUB_f32:
  if pc^.Args[2].Pos = mpLocal then PFloat32(BASEPTR_2)^ := PFloat32(MEMBASE_0)^ - PFloat32(MEMBASE_1)^
  else                              PFloat32(MEMBASE_2)^ := PFloat32(MEMBASE_0)^ - PFloat32(MEMBASE_1)^;
  Inc(pc);
lbSUB_f64:
  if pc^.Args[2].Pos = mpLocal then PFloat64(BASEPTR_2)^ := PFloat64(MEMBASE_0)^ - PFloat64(MEMBASE_1)^
  else                              PFloat64(MEMBASE_2)^ := PFloat64(MEMBASE_0)^ - PFloat64(MEMBASE_1)^;
  Inc(pc);

// --- MUL (*) ---
lbMUL_i32:
  if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := PInt32  (MEMBASE_0)^ * PInt32  (MEMBASE_1)^
  else                              PInt32  (MEMBASE_2)^ := PInt32  (MEMBASE_0)^ * PInt32  (MEMBASE_1)^;
  Inc(pc);
lbMUL_u32:
  if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ * PUInt32 (MEMBASE_1)^
  else                              PUInt32 (MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ * PUInt32 (MEMBASE_1)^;
  Inc(pc);
lbMUL_i64:
  if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := PInt64  (MEMBASE_0)^ * PInt64  (MEMBASE_1)^
  else                              PInt64  (MEMBASE_2)^ := PInt64  (MEMBASE_0)^ * PInt64  (MEMBASE_1)^;
  Inc(pc);
lbMUL_u64:
  if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ * PUInt64 (MEMBASE_1)^
  else                              PUInt64 (MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ * PUInt64 (MEMBASE_1)^;
  Inc(pc);
lbMUL_f32:
  if pc^.Args[2].Pos = mpLocal then PFloat32(BASEPTR_2)^ := PFloat32(MEMBASE_0)^ * PFloat32(MEMBASE_1)^
  else                              PFloat32(MEMBASE_2)^ := PFloat32(MEMBASE_0)^ * PFloat32(MEMBASE_1)^;
  Inc(pc);
lbMUL_f64:
  if pc^.Args[2].Pos = mpLocal then PFloat64(BASEPTR_2)^ := PFloat64(MEMBASE_0)^ * PFloat64(MEMBASE_1)^
  else                              PFloat64(MEMBASE_2)^ := PFloat64(MEMBASE_0)^ * PFloat64(MEMBASE_1)^;
  Inc(pc);

// --- DIV (div / /) ---
lbDIV_i32:
  if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := PInt32  (MEMBASE_0)^ div PInt32  (MEMBASE_1)^
  else                              PInt32  (MEMBASE_2)^ := PInt32  (MEMBASE_0)^ div PInt32  (MEMBASE_1)^;
  Inc(pc);
lbDIV_u32:
  if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ div PUInt32 (MEMBASE_1)^
  else                              PUInt32 (MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ div PUInt32 (MEMBASE_1)^;
  Inc(pc);
lbDIV_i64:
  if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := PInt64  (MEMBASE_0)^ div PInt64  (MEMBASE_1)^
  else                              PInt64  (MEMBASE_2)^ := PInt64  (MEMBASE_0)^ div PInt64  (MEMBASE_1)^;
  Inc(pc);
lbDIV_u64:
  if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ div PUInt64 (MEMBASE_1)^
  else                              PUInt64 (MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ div PUInt64 (MEMBASE_1)^;
  Inc(pc);
lbDIV_f32:
  if pc^.Args[2].Pos = mpLocal then PFloat32(BASEPTR_2)^ := PFloat32(MEMBASE_0)^  /  PFloat32(MEMBASE_1)^
  else                              PFloat32(MEMBASE_2)^ := PFloat32(MEMBASE_0)^  /  PFloat32(MEMBASE_1)^;
  Inc(pc);
lbDIV_f64:
  if pc^.Args[2].Pos = mpLocal then PFloat64(BASEPTR_2)^ := PFloat64(MEMBASE_0)^  /  PFloat64(MEMBASE_1)^
  else                              PFloat64(MEMBASE_2)^ := PFloat64(MEMBASE_0)^  /  PFloat64(MEMBASE_1)^;
  Inc(pc);

// --- MOD (mod) ---
lbMOD_i32:
  if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := PInt32  (MEMBASE_0)^ mod PInt32  (MEMBASE_1)^
  else                              PInt32  (MEMBASE_2)^ := PInt32  (MEMBASE_0)^ mod PInt32  (MEMBASE_1)^;
  Inc(pc);
lbMOD_u32:
  if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ mod PUInt32 (MEMBASE_1)^
  else                              PUInt32 (MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ mod PUInt32 (MEMBASE_1)^;
  Inc(pc);
lbMOD_i64:
  if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := PInt64  (MEMBASE_0)^ mod PInt64  (MEMBASE_1)^
  else                              PInt64  (MEMBASE_2)^ := PInt64  (MEMBASE_0)^ mod PInt64  (MEMBASE_1)^;
  Inc(pc);
lbMOD_u64:
  if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ mod PUInt64 (MEMBASE_1)^
  else                              PUInt64 (MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ mod PUInt64 (MEMBASE_1)^;
  Inc(pc);
lbMOD_f32:
  if pc^.Args[2].Pos = mpLocal then PFloat32(BASEPTR_2)^ := PFloat32(MEMBASE_0)^ mod PFloat32(MEMBASE_1)^
  else                              PFloat32(MEMBASE_2)^ := PFloat32(MEMBASE_0)^ mod PFloat32(MEMBASE_1)^;
  Inc(pc);
lbMOD_f64:
  if pc^.Args[2].Pos = mpLocal then PFloat64(BASEPTR_2)^ := PFloat64(MEMBASE_0)^ mod PFloat64(MEMBASE_1)^
  else                              PFloat64(MEMBASE_2)^ := PFloat64(MEMBASE_0)^ mod PFloat64(MEMBASE_1)^;
  Inc(pc);

// --- POW (Power) ---
lbPOW_i32:
  if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := Power(PInt32  (MEMBASE_0)^, PInt32  (MEMBASE_1)^)
  else                              PInt32  (MEMBASE_2)^ := Power(PInt32  (MEMBASE_0)^, PInt32  (MEMBASE_1)^);
  Inc(pc);
lbPOW_u32:
  if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := Power(PUInt32 (MEMBASE_0)^, PUInt32 (MEMBASE_1)^)
  else                              PUInt32 (MEMBASE_2)^ := Power(PUInt32 (MEMBASE_0)^, PUInt32 (MEMBASE_1)^);
  Inc(pc);
lbPOW_i64:
  if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := Power(PInt64  (MEMBASE_0)^, PInt64  (MEMBASE_1)^)
  else                              PInt64  (MEMBASE_2)^ := Power(PInt64  (MEMBASE_0)^, PInt64  (MEMBASE_1)^);
  Inc(pc);
lbPOW_u64:
  if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := Power(PUInt64 (MEMBASE_0)^, PUInt64 (MEMBASE_1)^)
  else                              PUInt64 (MEMBASE_2)^ := Power(PUInt64 (MEMBASE_0)^, PUInt64 (MEMBASE_1)^);
  Inc(pc);
lbPOW_f32:
  if pc^.Args[2].Pos = mpLocal then PFloat32(BASEPTR_2)^ := Power(PFloat32(MEMBASE_0)^, PFloat32(MEMBASE_1)^)
  else                              PFloat32(MEMBASE_2)^ := Power(PFloat32(MEMBASE_0)^, PFloat32(MEMBASE_1)^);
  Inc(pc);
lbPOW_f64:
  if pc^.Args[2].Pos = mpLocal then PFloat64(BASEPTR_2)^ := Power(PFloat64(MEMBASE_0)^, PFloat64(MEMBASE_1)^)
  else                              PFloat64(MEMBASE_2)^ := Power(PFloat64(MEMBASE_0)^, PFloat64(MEMBASE_1)^);
  Inc(pc);

// -----------------------------------------------------------------------
// --- COMPARISON OPERATORS (Returns Boolean)                          ---
// -----------------------------------------------------------------------

// --- EQ (=) ---
lbEQ_i32:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt32  (MEMBASE_0)^ = PInt32  (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PInt32  (MEMBASE_0)^ = PInt32  (MEMBASE_1)^;
  Inc(pc);
lbEQ_u32:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ = PUInt32 (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ = PUInt32 (MEMBASE_1)^;
  Inc(pc);
lbEQ_i64:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt64  (MEMBASE_0)^ = PInt64  (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PInt64  (MEMBASE_0)^ = PInt64  (MEMBASE_1)^;
  Inc(pc);
lbEQ_u64:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ = PUInt64 (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ = PUInt64 (MEMBASE_1)^;
  Inc(pc);
lbEQ_f32:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat32(MEMBASE_0)^ = PFloat32(MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PFloat32(MEMBASE_0)^ = PFloat32(MEMBASE_1)^;
  Inc(pc);
lbEQ_f64:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat64(MEMBASE_0)^ = PFloat64(MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PFloat64(MEMBASE_0)^ = PFloat64(MEMBASE_1)^;
  Inc(pc);

// --- NE (<>) ---
lbNE_i32:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt32  (MEMBASE_0)^ <> PInt32  (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PInt32  (MEMBASE_0)^ <> PInt32  (MEMBASE_1)^;
  Inc(pc);
lbNE_u32:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ <> PUInt32 (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ <> PUInt32 (MEMBASE_1)^;
  Inc(pc);
lbNE_i64:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt64  (MEMBASE_0)^ <> PInt64  (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PInt64  (MEMBASE_0)^ <> PInt64  (MEMBASE_1)^;
  Inc(pc);
lbNE_u64:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ <> PUInt64 (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ <> PUInt64 (MEMBASE_1)^;
  Inc(pc);
lbNE_f32:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat32(MEMBASE_0)^ <> PFloat32(MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PFloat32(MEMBASE_0)^ <> PFloat32(MEMBASE_1)^;
  Inc(pc);
lbNE_f64:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat64(MEMBASE_0)^ <> PFloat64(MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PFloat64(MEMBASE_0)^ <> PFloat64(MEMBASE_1)^;
  Inc(pc);

// --- LT (<) ---
lbLT_i32:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt32  (MEMBASE_0)^ < PInt32  (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PInt32  (MEMBASE_0)^ < PInt32  (MEMBASE_1)^;
  Inc(pc);
lbLT_u32:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ < PUInt32 (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ < PUInt32 (MEMBASE_1)^;
  Inc(pc);
lbLT_i64:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt64  (MEMBASE_0)^ < PInt64  (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PInt64  (MEMBASE_0)^ < PInt64  (MEMBASE_1)^;
  Inc(pc);
lbLT_u64:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ < PUInt64 (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ < PUInt64 (MEMBASE_1)^;
  Inc(pc);
lbLT_f32:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat32(MEMBASE_0)^ < PFloat32(MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PFloat32(MEMBASE_0)^ < PFloat32(MEMBASE_1)^;
  Inc(pc);
lbLT_f64:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat64(MEMBASE_0)^ < PFloat64(MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PFloat64(MEMBASE_0)^ < PFloat64(MEMBASE_1)^;
  Inc(pc);

// --- GT (>) ---
lbGT_i32:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt32  (MEMBASE_0)^ > PInt32  (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PInt32  (MEMBASE_0)^ > PInt32  (MEMBASE_1)^;
  Inc(pc);
lbGT_u32:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ > PUInt32 (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ > PUInt32 (MEMBASE_1)^;
  Inc(pc);
lbGT_i64:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt64  (MEMBASE_0)^ > PInt64  (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PInt64  (MEMBASE_0)^ > PInt64  (MEMBASE_1)^;
  Inc(pc);
lbGT_u64:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ > PUInt64 (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ > PUInt64 (MEMBASE_1)^;
  Inc(pc);
lbGT_f32:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat32(MEMBASE_0)^ > PFloat32(MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PFloat32(MEMBASE_0)^ > PFloat32(MEMBASE_1)^;
  Inc(pc);
lbGT_f64:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat64(MEMBASE_0)^ > PFloat64(MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PFloat64(MEMBASE_0)^ > PFloat64(MEMBASE_1)^;
  Inc(pc);

// --- GTE (>=) ---
lbGTE_i32:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt32  (MEMBASE_0)^ >= PInt32  (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PInt32  (MEMBASE_0)^ >= PInt32  (MEMBASE_1)^;
  Inc(pc);
lbGTE_u32:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ >= PUInt32 (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ >= PUInt32 (MEMBASE_1)^;
  Inc(pc);
lbGTE_i64:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt64  (MEMBASE_0)^ >= PInt64  (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PInt64  (MEMBASE_0)^ >= PInt64  (MEMBASE_1)^;
  Inc(pc);
lbGTE_u64:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ >= PUInt64 (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ >= PUInt64 (MEMBASE_1)^;
  Inc(pc);
lbGTE_f32:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat32(MEMBASE_0)^ >= PFloat32(MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PFloat32(MEMBASE_0)^ >= PFloat32(MEMBASE_1)^;
  Inc(pc);
lbGTE_f64:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat64(MEMBASE_0)^ >= PFloat64(MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PFloat64(MEMBASE_0)^ >= PFloat64(MEMBASE_1)^;
  Inc(pc);

// --- LTE (<=) ---
lbLTE_i32:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt32  (MEMBASE_0)^ <= PInt32  (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PInt32  (MEMBASE_0)^ <= PInt32  (MEMBASE_1)^;
  Inc(pc);
lbLTE_u32:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ <= PUInt32 (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ <= PUInt32 (MEMBASE_1)^;
  Inc(pc);
lbLTE_i64:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PInt64  (MEMBASE_0)^ <= PInt64  (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PInt64  (MEMBASE_0)^ <= PInt64  (MEMBASE_1)^;
  Inc(pc);
lbLTE_u64:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ <= PUInt64 (MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ <= PUInt64 (MEMBASE_1)^;
  Inc(pc);
lbLTE_f32:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat32(MEMBASE_0)^ <= PFloat32(MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PFloat32(MEMBASE_0)^ <= PFloat32(MEMBASE_1)^;
  Inc(pc);
lbLTE_f64:
  if pc^.Args[2].Pos = mpLocal then PBoolean(BASEPTR_2)^ := PFloat64(MEMBASE_0)^ <= PFloat64(MEMBASE_1)^
  else                              PBoolean(MEMBASE_2)^ := PFloat64(MEMBASE_0)^ <= PFloat64(MEMBASE_1)^;
  Inc(pc);

// -----------------------------------------------------------------------
// --- BITWISE OPERATORS (Integers Only)                               ---
// -----------------------------------------------------------------------

// --- BND (and) ---
lbBND_i32:
  if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := PInt32  (MEMBASE_0)^ and PInt32  (MEMBASE_1)^
  else                              PInt32  (MEMBASE_2)^ := PInt32  (MEMBASE_0)^ and PInt32  (MEMBASE_1)^;
  Inc(pc);
lbBND_u32:
  if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ and PUInt32 (MEMBASE_1)^
  else                              PUInt32 (MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ and PUInt32 (MEMBASE_1)^;
  Inc(pc);
lbBND_i64:
  if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := PInt64  (MEMBASE_0)^ and PInt64  (MEMBASE_1)^
  else                              PInt64  (MEMBASE_2)^ := PInt64  (MEMBASE_0)^ and PInt64  (MEMBASE_1)^;
  Inc(pc);
lbBND_u64:
  if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ and PUInt64 (MEMBASE_1)^
  else                              PUInt64 (MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ and PUInt64 (MEMBASE_1)^;
  Inc(pc);

// --- SHL (shl) ---
lbSHL_i32:
  if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := PInt32  (MEMBASE_0)^ shl PInt32  (MEMBASE_1)^
  else                              PInt32  (MEMBASE_2)^ := PInt32  (MEMBASE_0)^ shl PInt32  (MEMBASE_1)^;
  Inc(pc);
lbSHL_u32:
  if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ shl PUInt32 (MEMBASE_1)^
  else                              PUInt32 (MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ shl PUInt32 (MEMBASE_1)^;
  Inc(pc);
lbSHL_i64:
  if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := PInt64  (MEMBASE_0)^ shl PInt64  (MEMBASE_1)^
  else                              PInt64  (MEMBASE_2)^ := PInt64  (MEMBASE_0)^ shl PInt64  (MEMBASE_1)^;
  Inc(pc);
lbSHL_u64:
  if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ shl PUInt64 (MEMBASE_1)^
  else                              PUInt64 (MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ shl PUInt64 (MEMBASE_1)^;
  Inc(pc);

// --- SHR (shr) ---
lbSHR_i32:
  if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := PInt32  (MEMBASE_0)^ shr PInt32  (MEMBASE_1)^
  else                              PInt32  (MEMBASE_2)^ := PInt32  (MEMBASE_0)^ shr PInt32  (MEMBASE_1)^;
  Inc(pc);
lbSHR_u32:
  if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ shr PUInt32 (MEMBASE_1)^
  else                              PUInt32 (MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ shr PUInt32 (MEMBASE_1)^;
  Inc(pc);
lbSHR_i64:
  if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := PInt64  (MEMBASE_0)^ shr PInt64  (MEMBASE_1)^
  else                              PInt64  (MEMBASE_2)^ := PInt64  (MEMBASE_0)^ shr PInt64  (MEMBASE_1)^;
  Inc(pc);
lbSHR_u64:
  if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ shr PUInt64 (MEMBASE_1)^
  else                              PUInt64 (MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ shr PUInt64 (MEMBASE_1)^;
  Inc(pc);

// --- XOR (xor) ---
lbXOR_i32:
  if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := PInt32  (MEMBASE_0)^ xor PInt32  (MEMBASE_1)^
  else                              PInt32  (MEMBASE_2)^ := PInt32  (MEMBASE_0)^ xor PInt32  (MEMBASE_1)^;
  Inc(pc);
lbXOR_u32:
  if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ xor PUInt32 (MEMBASE_1)^
  else                              PUInt32 (MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ xor PUInt32 (MEMBASE_1)^;
  Inc(pc);
lbXOR_i64:
  if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := PInt64  (MEMBASE_0)^ xor PInt64  (MEMBASE_1)^
  else                              PInt64  (MEMBASE_2)^ := PInt64  (MEMBASE_0)^ xor PInt64  (MEMBASE_1)^;
  Inc(pc);
lbXOR_u64:
  if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ xor PUInt64 (MEMBASE_1)^
  else                              PUInt64 (MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ xor PUInt64 (MEMBASE_1)^;
  Inc(pc);

// --- BOR (or) ---
lbBOR_i32:
  if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := PInt32  (MEMBASE_0)^ or PInt32  (MEMBASE_1)^
  else                              PInt32  (MEMBASE_2)^ := PInt32  (MEMBASE_0)^ or PInt32  (MEMBASE_1)^;
  Inc(pc);
lbBOR_u32:
  if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := PUInt32 (MEMBASE_0)^ or PUInt32 (MEMBASE_1)^
  else                              PUInt32 (MEMBASE_2)^ := PUInt32 (MEMBASE_0)^ or PUInt32 (MEMBASE_1)^;
  Inc(pc);
lbBOR_i64:
  if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := PInt64  (MEMBASE_0)^ or PInt64  (MEMBASE_1)^
  else                              PInt64  (MEMBASE_2)^ := PInt64  (MEMBASE_0)^ or PInt64  (MEMBASE_1)^;
  Inc(pc);
lbBOR_u64:
  if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := PUInt64 (MEMBASE_0)^ or PUInt64 (MEMBASE_1)^
  else                              PUInt64 (MEMBASE_2)^ := PUInt64 (MEMBASE_0)^ or PUInt64 (MEMBASE_1)^;
  Inc(pc);

// --- SAR (Arithmetic Shift Right) ---
lbSAR_i32:
  if pc^.Args[2].Pos = mpLocal then PInt32  (BASEPTR_2)^ := Sar(PInt32  (MEMBASE_0)^, PInt32  (MEMBASE_1)^)
  else                              PInt32  (MEMBASE_2)^ := Sar(PInt32  (MEMBASE_0)^, PInt32  (MEMBASE_1)^);
  Inc(pc);
lbSAR_u32:
  if pc^.Args[2].Pos = mpLocal then PUInt32 (BASEPTR_2)^ := Sar(PUInt32 (MEMBASE_0)^, PUInt32 (MEMBASE_1)^)
  else                              PUInt32 (MEMBASE_2)^ := Sar(PUInt32 (MEMBASE_0)^, PUInt32 (MEMBASE_1)^);
  Inc(pc);
lbSAR_i64:
  if pc^.Args[2].Pos = mpLocal then PInt64  (BASEPTR_2)^ := Sar(PInt64  (MEMBASE_0)^, PInt64  (MEMBASE_1)^)
  else                              PInt64  (MEMBASE_2)^ := Sar(PInt64  (MEMBASE_0)^, PInt64  (MEMBASE_1)^);
  Inc(pc);
lbSAR_u64:
  if pc^.Args[2].Pos = mpLocal then PUInt64 (BASEPTR_2)^ := Sar(PUInt64 (MEMBASE_0)^, PUInt64 (MEMBASE_1)^)
  else                              PUInt64 (MEMBASE_2)^ := Sar(PUInt64 (MEMBASE_0)^, PUInt64 (MEMBASE_1)^);
