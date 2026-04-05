label
  lbADDR,
  lbDREF, lbDREF32, lbDREF64,

  lbINC_i32, lbINC_u32, lbINC_i64, lbINC_u64,

  // specialized ops
  lbFMAD64_64, lbFMAD64_32, lbFMAD32_64, lbFMAD32_32,
  lbFMA_i8, lbFMA_u8, lbFMA_i16, lbFMA_u16, lbFMA_i32, lbFMA_u32, lbFMA_i64, lbFMA_u64,

  // reference point
  lbADD, 

  // Specialize for local dest
  lbADD_i32, lbADD_u32, lbADD_i64, lbADD_u64, lbADD_f32, lbADD_f64,
  lbSUB_i32, lbSUB_u32, lbSUB_i64, lbSUB_u64, lbSUB_f32, lbSUB_f64,
  lbMUL_i32, lbMUL_u32, lbMUL_i64, lbMUL_u64, lbMUL_f32, lbMUL_f64,
  lbDIV_i32, lbDIV_u32, lbDIV_i64, lbDIV_u64, lbDIV_f32, lbDIV_f64,
  lbMOD_i32, lbMOD_u32, lbMOD_i64, lbMOD_u64, lbMOD_f32, lbMOD_f64,
  lbPOW_i32, lbPOW_u32, lbPOW_i64, lbPOW_u64, lbPOW_f32, lbPOW_f64,

  lbEQ_i32, lbEQ_u32, lbEQ_i64, lbEQ_u64, lbEQ_f32, lbEQ_f64,
  lbNE_i32, lbNE_u32, lbNE_i64, lbNE_u64, lbNE_f32, lbNE_f64,
  lbLT_i32, lbLT_u32, lbLT_i64, lbLT_u64, lbLT_f32, lbLT_f64,
  lbGT_i32, lbGT_u32, lbGT_i64, lbGT_u64, lbGT_f32, lbGT_f64,
  lbGTE_i32, lbGTE_u32, lbGTE_i64, lbGTE_u64, lbGTE_f32, lbGTE_f64,
  lbLTE_i32, lbLTE_u32, lbLTE_i64, lbLTE_u64, lbLTE_f32, lbLTE_f64,

  lbBND_i32, lbBND_u32, lbBND_i64, lbBND_u64,
  lbSHL_i32, lbSHL_u32, lbSHL_i64, lbSHL_u64,
  lbSHR_i32, lbSHR_u32, lbSHR_i64, lbSHR_u64,
  lbXOR_i32, lbXOR_u32, lbXOR_i64, lbXOR_u64,
  lbBOR_i32, lbBOR_u32, lbBOR_i64, lbBOR_u64,
  lbSAR_i32, lbSAR_u32, lbSAR_i64, lbSAR_u64,

  // ---------------------------------------------
  // MOV Operations (Destination, Source)
  // ---------------------------------------------
  lbMOV,

  // --- Same-Size or Truncating (Narrowing)
  lbMOV8, lbMOV16, lbMOV32, lbMOV64,

  // --- Widening Signed (Sign Extension)
  lbMOVSX16_8,
  lbMOVSX32_8, lbMOVSX32_16,
  lbMOVSX64_8, lbMOVSX64_16, lbMOVSX64_32,

  // --- Widening Unsigned (Zero Extension)
  lbMOVZX16_8,
  lbMOVZX32_8, lbMOVZX32_16,
  lbMOVZX64_8, lbMOVZX64_16, lbMOVZX64_32,

  // --- Float Source Types (f32, f64) ---
  // --- Destination Types (i32, u32, i64, u64, f32, f64) ---
  lbMOVF32_i32, lbMOVF32_u32, lbMOVF32_i64, lbMOVF32_u64, lbMOVF32_f32, lbMOVF32_f64,
  lbMOVF64_i32, lbMOVF64_u32, lbMOVF64_i64, lbMOVF64_u64, lbMOVF64_f32, lbMOVF64_f64,

  // ---------------------------------------------
  // STORE Operations (Destination Address, Source)
  // Write from stack into memory
  // ---------------------------------------------
  lbSTORE,

  // --- Same-Size or Truncating (Narrowing) ---
  lbSTORE8, lbSTORE16, lbSTORE32, lbSTORE64,

  // --- Widening Signed (Sign Extension) ---
  lbSTORESX16_8,
  lbSTORESX32_8, lbSTORESX32_16,
  lbSTORESX64_8, lbSTORESX64_16, lbSTORESX64_32,

  // --- Widening Unsigned (Zero Extension) ---
  lbSTOREZX16_8,
  lbSTOREZX32_8, lbSTOREZX32_16,
  lbSTOREZX64_8, lbSTOREZX64_16, lbSTOREZX64_32,

  // --- Float Source Types (f32, f64) ---
  // --- Destination Types (i32, u32, i64, u64, f32, f64) ---
  lbSTOREF32_i32, lbSTOREF32_u32, lbSTOREF32_i64, lbSTOREF32_u64, lbSTOREF32_f32, lbSTOREF32_f64,
  lbSTOREF64_i32, lbSTOREF64_u32, lbSTOREF64_i64, lbSTOREF64_u64, lbSTOREF64_f32, lbSTOREF64_f64,

  lbERROR;
