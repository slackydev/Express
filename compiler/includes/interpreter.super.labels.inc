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




label
  lbADDR_CL,
  lbDREF_CL, lbDREF32_CL, lbDREF64_CL,

  lbINC_i32_CL, lbINC_u32_CL, lbINC_i64_CL, lbINC_u64_CL,

  // specialized ops
  lbFMAD64_64_CL, lbFMAD64_32_CL, lbFMAD32_64_CL, lbFMAD32_32_CL,
  lbFMA_i8_CL, lbFMA_u8_CL, lbFMA_i16_CL, lbFMA_u16_CL, lbFMA_i32_CL, lbFMA_u32_CL, lbFMA_i64_CL, lbFMA_u64_CL,

  // reference point
  lbADD_CL, 

  // Specialize for local dest
  lbADD_i32_CL, lbADD_u32_CL, lbADD_i64_CL, lbADD_u64_CL, lbADD_f32_CL, lbADD_f64_CL,
  lbSUB_i32_CL, lbSUB_u32_CL, lbSUB_i64_CL, lbSUB_u64_CL, lbSUB_f32_CL, lbSUB_f64_CL,
  lbMUL_i32_CL, lbMUL_u32_CL, lbMUL_i64_CL, lbMUL_u64_CL, lbMUL_f32_CL, lbMUL_f64_CL,
  lbDIV_i32_CL, lbDIV_u32_CL, lbDIV_i64_CL, lbDIV_u64_CL, lbDIV_f32_CL, lbDIV_f64_CL,
  lbMOD_i32_CL, lbMOD_u32_CL, lbMOD_i64_CL, lbMOD_u64_CL, lbMOD_f32_CL, lbMOD_f64_CL,
  lbPOW_i32_CL, lbPOW_u32_CL, lbPOW_i64_CL, lbPOW_u64_CL, lbPOW_f32_CL, lbPOW_f64_CL,

  lbEQ_i32_CL, lbEQ_u32_CL, lbEQ_i64_CL, lbEQ_u64_CL, lbEQ_f32_CL, lbEQ_f64_CL,
  lbNE_i32_CL, lbNE_u32_CL, lbNE_i64_CL, lbNE_u64_CL, lbNE_f32_CL, lbNE_f64_CL,
  lbLT_i32_CL, lbLT_u32_CL, lbLT_i64_CL, lbLT_u64_CL, lbLT_f32_CL, lbLT_f64_CL,
  lbGT_i32_CL, lbGT_u32_CL, lbGT_i64_CL, lbGT_u64_CL, lbGT_f32_CL, lbGT_f64_CL,
  lbGTE_i32_CL, lbGTE_u32_CL, lbGTE_i64_CL, lbGTE_u64_CL, lbGTE_f32_CL, lbGTE_f64_CL,
  lbLTE_i32_CL, lbLTE_u32_CL, lbLTE_i64_CL, lbLTE_u64_CL, lbLTE_f32_CL, lbLTE_f64_CL,

  lbBND_i32_CL, lbBND_u32_CL, lbBND_i64_CL, lbBND_u64_CL,
  lbSHL_i32_CL, lbSHL_u32_CL, lbSHL_i64_CL, lbSHL_u64_CL,
  lbSHR_i32_CL, lbSHR_u32_CL, lbSHR_i64_CL, lbSHR_u64_CL,
  lbXOR_i32_CL, lbXOR_u32_CL, lbXOR_i64_CL, lbXOR_u64_CL,
  lbBOR_i32_CL, lbBOR_u32_CL, lbBOR_i64_CL, lbBOR_u64_CL,
  lbSAR_i32_CL, lbSAR_u32_CL, lbSAR_i64_CL, lbSAR_u64_CL,

  // ---------------------------------------------
  // MOV Operations (Destination_CL, Source)
  // ---------------------------------------------
  lbMOV_CL,

  // --- Same-Size or Truncating (Narrowing)
  lbMOV8_CL, lbMOV16_CL, lbMOV32_CL, lbMOV64_CL,

  // --- Widening Signed (Sign Extension)
  lbMOVSX16_8_CL,
  lbMOVSX32_8_CL, lbMOVSX32_16_CL,
  lbMOVSX64_8_CL, lbMOVSX64_16_CL, lbMOVSX64_32_CL,

  // --- Widening Unsigned (Zero Extension)
  lbMOVZX16_8_CL,
  lbMOVZX32_8_CL, lbMOVZX32_16_CL,
  lbMOVZX64_8_CL, lbMOVZX64_16_CL, lbMOVZX64_32_CL,

  // --- Float Source Types (f32_CL, f64) ---
  // --- Destination Types (i32_CL, u32_CL, i64_CL, u64_CL, f32_CL, f64) ---
  lbMOVF32_i32_CL, lbMOVF32_u32_CL, lbMOVF32_i64_CL, lbMOVF32_u64_CL, lbMOVF32_f32_CL, lbMOVF32_f64_CL,
  lbMOVF64_i32_CL, lbMOVF64_u32_CL, lbMOVF64_i64_CL, lbMOVF64_u64_CL, lbMOVF64_f32_CL, lbMOVF64_f64_CL,

  // ---------------------------------------------
  // STORE Operations (Destination Address_CL, Source)
  // Write from stack into memory
  // ---------------------------------------------
  lbSTORE_CL,

  // --- Same-Size or Truncating (Narrowing) ---
  lbSTORE8_CL, lbSTORE16_CL, lbSTORE32_CL, lbSTORE64_CL,

  // --- Widening Signed (Sign Extension) ---
  lbSTORESX16_8_CL,
  lbSTORESX32_8_CL, lbSTORESX32_16_CL,
  lbSTORESX64_8_CL, lbSTORESX64_16_CL, lbSTORESX64_32_CL,

  // --- Widening Unsigned (Zero Extension) ---
  lbSTOREZX16_8_CL,
  lbSTOREZX32_8_CL, lbSTOREZX32_16_CL,
  lbSTOREZX64_8_CL, lbSTOREZX64_16_CL, lbSTOREZX64_32_CL,

  // --- Float Source Types (f32_CL, f64) ---
  // --- Destination Types (i32_CL, u32_CL, i64_CL, u64_CL, f32_CL, f64) ---
  lbSTOREF32_i32_CL, lbSTOREF32_u32_CL, lbSTOREF32_i64_CL, lbSTOREF32_u64_CL, lbSTOREF32_f32_CL, lbSTOREF32_f64_CL,
  lbSTOREF64_i32_CL, lbSTOREF64_u32_CL, lbSTOREF64_i64_CL, lbSTOREF64_u64_CL, lbSTOREF64_f32_CL, lbSTOREF64_f64_CL,

  lbERROR_CL;



label
  lbADDR_LC,
  lbDREF_LC, lbDREF32_LC, lbDREF64_LC,

  lbINC_i32_LC, lbINC_u32_LC, lbINC_i64_LC, lbINC_u64_LC,

  // specialized ops
  lbFMAD64_64_LC, lbFMAD64_32_LC, lbFMAD32_64_LC, lbFMAD32_32_LC,
  lbFMA_i8_LC, lbFMA_u8_LC, lbFMA_i16_LC, lbFMA_u16_LC, lbFMA_i32_LC, lbFMA_u32_LC, lbFMA_i64_LC, lbFMA_u64_LC,

  // reference point
  lbADD_LC, 

  // Specialize for local dest
  lbADD_i32_LC, lbADD_u32_LC, lbADD_i64_LC, lbADD_u64_LC, lbADD_f32_LC, lbADD_f64_LC,
  lbSUB_i32_LC, lbSUB_u32_LC, lbSUB_i64_LC, lbSUB_u64_LC, lbSUB_f32_LC, lbSUB_f64_LC,
  lbMUL_i32_LC, lbMUL_u32_LC, lbMUL_i64_LC, lbMUL_u64_LC, lbMUL_f32_LC, lbMUL_f64_LC,
  lbDIV_i32_LC, lbDIV_u32_LC, lbDIV_i64_LC, lbDIV_u64_LC, lbDIV_f32_LC, lbDIV_f64_LC,
  lbMOD_i32_LC, lbMOD_u32_LC, lbMOD_i64_LC, lbMOD_u64_LC, lbMOD_f32_LC, lbMOD_f64_LC,
  lbPOW_i32_LC, lbPOW_u32_LC, lbPOW_i64_LC, lbPOW_u64_LC, lbPOW_f32_LC, lbPOW_f64_LC,

  lbEQ_i32_LC, lbEQ_u32_LC, lbEQ_i64_LC, lbEQ_u64_LC, lbEQ_f32_LC, lbEQ_f64_LC,
  lbNE_i32_LC, lbNE_u32_LC, lbNE_i64_LC, lbNE_u64_LC, lbNE_f32_LC, lbNE_f64_LC,
  lbLT_i32_LC, lbLT_u32_LC, lbLT_i64_LC, lbLT_u64_LC, lbLT_f32_LC, lbLT_f64_LC,
  lbGT_i32_LC, lbGT_u32_LC, lbGT_i64_LC, lbGT_u64_LC, lbGT_f32_LC, lbGT_f64_LC,
  lbGTE_i32_LC, lbGTE_u32_LC, lbGTE_i64_LC, lbGTE_u64_LC, lbGTE_f32_LC, lbGTE_f64_LC,
  lbLTE_i32_LC, lbLTE_u32_LC, lbLTE_i64_LC, lbLTE_u64_LC, lbLTE_f32_LC, lbLTE_f64_LC,

  lbBND_i32_LC, lbBND_u32_LC, lbBND_i64_LC, lbBND_u64_LC,
  lbSHL_i32_LC, lbSHL_u32_LC, lbSHL_i64_LC, lbSHL_u64_LC,
  lbSHR_i32_LC, lbSHR_u32_LC, lbSHR_i64_LC, lbSHR_u64_LC,
  lbXOR_i32_LC, lbXOR_u32_LC, lbXOR_i64_LC, lbXOR_u64_LC,
  lbBOR_i32_LC, lbBOR_u32_LC, lbBOR_i64_LC, lbBOR_u64_LC,
  lbSAR_i32_LC, lbSAR_u32_LC, lbSAR_i64_LC, lbSAR_u64_LC,

  // ---------------------------------------------
  // MOV Operations (Destination_LC, Source)
  // ---------------------------------------------
  lbMOV_LC,

  // --- Same-Size or Truncating (Narrowing)
  lbMOV8_LC, lbMOV16_LC, lbMOV32_LC, lbMOV64_LC,

  // --- Widening Signed (Sign Extension)
  lbMOVSX16_8_LC,
  lbMOVSX32_8_LC, lbMOVSX32_16_LC,
  lbMOVSX64_8_LC, lbMOVSX64_16_LC, lbMOVSX64_32_LC,

  // --- Widening Unsigned (Zero Extension)
  lbMOVZX16_8_LC,
  lbMOVZX32_8_LC, lbMOVZX32_16_LC,
  lbMOVZX64_8_LC, lbMOVZX64_16_LC, lbMOVZX64_32_LC,

  // --- Float Source Types (f32_LC, f64) ---
  // --- Destination Types (i32_LC, u32_LC, i64_LC, u64_LC, f32_LC, f64) ---
  lbMOVF32_i32_LC, lbMOVF32_u32_LC, lbMOVF32_i64_LC, lbMOVF32_u64_LC, lbMOVF32_f32_LC, lbMOVF32_f64_LC,
  lbMOVF64_i32_LC, lbMOVF64_u32_LC, lbMOVF64_i64_LC, lbMOVF64_u64_LC, lbMOVF64_f32_LC, lbMOVF64_f64_LC,

  // ---------------------------------------------
  // STORE Operations (Destination Address_LC, Source)
  // Write from stack into memory
  // ---------------------------------------------
  lbSTORE_LC,

  // --- Same-Size or Truncating (Narrowing) ---
  lbSTORE8_LC, lbSTORE16_LC, lbSTORE32_LC, lbSTORE64_LC,

  // --- Widening Signed (Sign Extension) ---
  lbSTORESX16_8_LC,
  lbSTORESX32_8_LC, lbSTORESX32_16_LC,
  lbSTORESX64_8_LC, lbSTORESX64_16_LC, lbSTORESX64_32_LC,

  // --- Widening Unsigned (Zero Extension) ---
  lbSTOREZX16_8_LC,
  lbSTOREZX32_8_LC, lbSTOREZX32_16_LC,
  lbSTOREZX64_8_LC, lbSTOREZX64_16_LC, lbSTOREZX64_32_LC,

  // --- Float Source Types (f32_LC, f64) ---
  // --- Destination Types (i32_LC, u32_LC, i64_LC, u64_LC, f32_LC, f64) ---
  lbSTOREF32_i32_LC, lbSTOREF32_u32_LC, lbSTOREF32_i64_LC, lbSTOREF32_u64_LC, lbSTOREF32_f32_LC, lbSTOREF32_f64_LC,
  lbSTOREF64_i32_LC, lbSTOREF64_u32_LC, lbSTOREF64_i64_LC, lbSTOREF64_u64_LC, lbSTOREF64_f32_LC, lbSTOREF64_f64_LC,

  lbERROR_LC;
