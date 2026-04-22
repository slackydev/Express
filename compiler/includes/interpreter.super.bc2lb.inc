// xpri_directthreaded_bc2lb.inc
// This file populates the JumpTable with addresses of direct-threaded labels.
// It assumes 'JumpTable' is an array of PtrUInt indexed by EBytecode.
JumpTable[bcADDR] := PtrUInt(@lbADDR);

JumpTable[bcDREF] := PtrUInt(@lbDREF);
JumpTable[bcDREF32] := PtrUInt(@lbDREF32);
JumpTable[bcDREF64] := PtrUInt(@lbDREF64);


JumpTable[bcINC_i32] := PtrUInt(@lbINC_i32);
JumpTable[bcINC_u32] := PtrUInt(@lbINC_u32);
JumpTable[bcINC_i64] := PtrUInt(@lbINC_i64);
JumpTable[bcINC_u64] := PtrUInt(@lbINC_u64);


// specialized ops
JumpTable[bcFMAD64_64] := PtrUInt(@lbFMAD64_64);
JumpTable[bcFMAD64_32] := PtrUInt(@lbFMAD64_32);
JumpTable[bcFMAD32_64] := PtrUInt(@lbFMAD32_64);
JumpTable[bcFMAD32_32] := PtrUInt(@lbFMAD32_32);

JumpTable[bcFMA_i8] := PtrUInt(@lbFMA_i8);
JumpTable[bcFMA_u8] := PtrUInt(@lbFMA_u8);
JumpTable[bcFMA_i16] := PtrUInt(@lbFMA_i16);
JumpTable[bcFMA_u16] := PtrUInt(@lbFMA_u16);
JumpTable[bcFMA_i32] := PtrUInt(@lbFMA_i32);
JumpTable[bcFMA_u32] := PtrUInt(@lbFMA_u32);
JumpTable[bcFMA_i64] := PtrUInt(@lbFMA_i64);
JumpTable[bcFMA_u64] := PtrUInt(@lbFMA_u64);


// reference point
JumpTable[bcADD] := PtrUInt(@lbADD);


// Specialize for local dest
JumpTable[bcADD_i32] := PtrUInt(@lbADD_i32);
JumpTable[bcADD_u32] := PtrUInt(@lbADD_u32);
JumpTable[bcADD_i64] := PtrUInt(@lbADD_i64);
JumpTable[bcADD_u64] := PtrUInt(@lbADD_u64);
JumpTable[bcADD_f32] := PtrUInt(@lbADD_f32);
JumpTable[bcADD_f64] := PtrUInt(@lbADD_f64);

JumpTable[bcSUB_i32] := PtrUInt(@lbSUB_i32);
JumpTable[bcSUB_u32] := PtrUInt(@lbSUB_u32);
JumpTable[bcSUB_i64] := PtrUInt(@lbSUB_i64);
JumpTable[bcSUB_u64] := PtrUInt(@lbSUB_u64);
JumpTable[bcSUB_f32] := PtrUInt(@lbSUB_f32);
JumpTable[bcSUB_f64] := PtrUInt(@lbSUB_f64);

JumpTable[bcMUL_i32] := PtrUInt(@lbMUL_i32);
JumpTable[bcMUL_u32] := PtrUInt(@lbMUL_u32);
JumpTable[bcMUL_i64] := PtrUInt(@lbMUL_i64);
JumpTable[bcMUL_u64] := PtrUInt(@lbMUL_u64);
JumpTable[bcMUL_f32] := PtrUInt(@lbMUL_f32);
JumpTable[bcMUL_f64] := PtrUInt(@lbMUL_f64);

JumpTable[bcDIV_i32] := PtrUInt(@lbDIV_i32);
JumpTable[bcDIV_u32] := PtrUInt(@lbDIV_u32);
JumpTable[bcDIV_i64] := PtrUInt(@lbDIV_i64);
JumpTable[bcDIV_u64] := PtrUInt(@lbDIV_u64);
JumpTable[bcDIV_f32] := PtrUInt(@lbDIV_f32);
JumpTable[bcDIV_f64] := PtrUInt(@lbDIV_f64);

JumpTable[bcMOD_i32] := PtrUInt(@lbMOD_i32);
JumpTable[bcMOD_u32] := PtrUInt(@lbMOD_u32);
JumpTable[bcMOD_i64] := PtrUInt(@lbMOD_i64);
JumpTable[bcMOD_u64] := PtrUInt(@lbMOD_u64);
JumpTable[bcMOD_f32] := PtrUInt(@lbMOD_f32);
JumpTable[bcMOD_f64] := PtrUInt(@lbMOD_f64);

JumpTable[bcPOW_i32] := PtrUInt(@lbPOW_i32);
JumpTable[bcPOW_u32] := PtrUInt(@lbPOW_u32);
JumpTable[bcPOW_i64] := PtrUInt(@lbPOW_i64);
JumpTable[bcPOW_u64] := PtrUInt(@lbPOW_u64);
JumpTable[bcPOW_f32] := PtrUInt(@lbPOW_f32);
JumpTable[bcPOW_f64] := PtrUInt(@lbPOW_f64);


JumpTable[bcEQ_i32] := PtrUInt(@lbEQ_i32);
JumpTable[bcEQ_u32] := PtrUInt(@lbEQ_u32);
JumpTable[bcEQ_i64] := PtrUInt(@lbEQ_i64);
JumpTable[bcEQ_u64] := PtrUInt(@lbEQ_u64);
JumpTable[bcEQ_f32] := PtrUInt(@lbEQ_f32);
JumpTable[bcEQ_f64] := PtrUInt(@lbEQ_f64);

JumpTable[bcNE_i32] := PtrUInt(@lbNE_i32);
JumpTable[bcNE_u32] := PtrUInt(@lbNE_u32);
JumpTable[bcNE_i64] := PtrUInt(@lbNE_i64);
JumpTable[bcNE_u64] := PtrUInt(@lbNE_u64);
JumpTable[bcNE_f32] := PtrUInt(@lbNE_f32);
JumpTable[bcNE_f64] := PtrUInt(@lbNE_f64);

JumpTable[bcLT_i32] := PtrUInt(@lbLT_i32);
JumpTable[bcLT_u32] := PtrUInt(@lbLT_u32);
JumpTable[bcLT_i64] := PtrUInt(@lbLT_i64);
JumpTable[bcLT_u64] := PtrUInt(@lbLT_u64);
JumpTable[bcLT_f32] := PtrUInt(@lbLT_f32);
JumpTable[bcLT_f64] := PtrUInt(@lbLT_f64);

JumpTable[bcGT_i32] := PtrUInt(@lbGT_i32);
JumpTable[bcGT_u32] := PtrUInt(@lbGT_u32);
JumpTable[bcGT_i64] := PtrUInt(@lbGT_i64);
JumpTable[bcGT_u64] := PtrUInt(@lbGT_u64);
JumpTable[bcGT_f32] := PtrUInt(@lbGT_f32);
JumpTable[bcGT_f64] := PtrUInt(@lbGT_f64);

JumpTable[bcGTE_i32] := PtrUInt(@lbGTE_i32);
JumpTable[bcGTE_u32] := PtrUInt(@lbGTE_u32);
JumpTable[bcGTE_i64] := PtrUInt(@lbGTE_i64);
JumpTable[bcGTE_u64] := PtrUInt(@lbGTE_u64);
JumpTable[bcGTE_f32] := PtrUInt(@lbGTE_f32);
JumpTable[bcGTE_f64] := PtrUInt(@lbGTE_f64);

JumpTable[bcLTE_i32] := PtrUInt(@lbLTE_i32);
JumpTable[bcLTE_u32] := PtrUInt(@lbLTE_u32);
JumpTable[bcLTE_i64] := PtrUInt(@lbLTE_i64);
JumpTable[bcLTE_u64] := PtrUInt(@lbLTE_u64);
JumpTable[bcLTE_f32] := PtrUInt(@lbLTE_f32);
JumpTable[bcLTE_f64] := PtrUInt(@lbLTE_f64);


JumpTable[bcBND_i32] := PtrUInt(@lbBND_i32);
JumpTable[bcBND_u32] := PtrUInt(@lbBND_u32);
JumpTable[bcBND_i64] := PtrUInt(@lbBND_i64);
JumpTable[bcBND_u64] := PtrUInt(@lbBND_u64);

JumpTable[bcSHL_i32] := PtrUInt(@lbSHL_i32);
JumpTable[bcSHL_u32] := PtrUInt(@lbSHL_u32);
JumpTable[bcSHL_i64] := PtrUInt(@lbSHL_i64);
JumpTable[bcSHL_u64] := PtrUInt(@lbSHL_u64);

JumpTable[bcSHR_i32] := PtrUInt(@lbSHR_i32);
JumpTable[bcSHR_u32] := PtrUInt(@lbSHR_u32);
JumpTable[bcSHR_i64] := PtrUInt(@lbSHR_i64);
JumpTable[bcSHR_u64] := PtrUInt(@lbSHR_u64);

JumpTable[bcXOR_i32] := PtrUInt(@lbXOR_i32);
JumpTable[bcXOR_u32] := PtrUInt(@lbXOR_u32);
JumpTable[bcXOR_i64] := PtrUInt(@lbXOR_i64);
JumpTable[bcXOR_u64] := PtrUInt(@lbXOR_u64);

JumpTable[bcBOR_i32] := PtrUInt(@lbBOR_i32);
JumpTable[bcBOR_u32] := PtrUInt(@lbBOR_u32);
JumpTable[bcBOR_i64] := PtrUInt(@lbBOR_i64);
JumpTable[bcBOR_u64] := PtrUInt(@lbBOR_u64);

JumpTable[bcSAR_i32] := PtrUInt(@lbSAR_i32);
JumpTable[bcSAR_u32] := PtrUInt(@lbSAR_u32);
JumpTable[bcSAR_i64] := PtrUInt(@lbSAR_i64);
JumpTable[bcSAR_u64] := PtrUInt(@lbSAR_u64);


// ---------------------------------------------
// MOV Operations (Destination, Source)
// ---------------------------------------------
JumpTable[bcMOV] := PtrUInt(@lbMOV);


// --- Same-Size or Truncating (Narrowing)
JumpTable[bcMOV8] := PtrUInt(@lbMOV8);
JumpTable[bcMOV16] := PtrUInt(@lbMOV16);
JumpTable[bcMOV32] := PtrUInt(@lbMOV32);
JumpTable[bcMOV64] := PtrUInt(@lbMOV64);


// --- Widening Signed (Sign Extension)
JumpTable[bcMOVSX16_8] := PtrUInt(@lbMOVSX16_8);

JumpTable[bcMOVSX32_8] := PtrUInt(@lbMOVSX32_8);
JumpTable[bcMOVSX32_16] := PtrUInt(@lbMOVSX32_16);

JumpTable[bcMOVSX64_8] := PtrUInt(@lbMOVSX64_8);
JumpTable[bcMOVSX64_16] := PtrUInt(@lbMOVSX64_16);
JumpTable[bcMOVSX64_32] := PtrUInt(@lbMOVSX64_32);


// --- Widening Unsigned (Zero Extension)
JumpTable[bcMOVZX16_8] := PtrUInt(@lbMOVZX16_8);

JumpTable[bcMOVZX32_8] := PtrUInt(@lbMOVZX32_8);
JumpTable[bcMOVZX32_16] := PtrUInt(@lbMOVZX32_16);

JumpTable[bcMOVZX64_8] := PtrUInt(@lbMOVZX64_8);
JumpTable[bcMOVZX64_16] := PtrUInt(@lbMOVZX64_16);
JumpTable[bcMOVZX64_32] := PtrUInt(@lbMOVZX64_32);


// --- Float Source Types (f32, f64) ---
// --- Destination Types (i32, u32, i64, u64, f32, f64) ---
JumpTable[bcMOVF32_i32] := PtrUInt(@lbMOVF32_i32);
JumpTable[bcMOVF32_u32] := PtrUInt(@lbMOVF32_u32);
JumpTable[bcMOVF32_i64] := PtrUInt(@lbMOVF32_i64);
JumpTable[bcMOVF32_u64] := PtrUInt(@lbMOVF32_u64);
JumpTable[bcMOVF32_f32] := PtrUInt(@lbMOVF32_f32);
JumpTable[bcMOVF32_f64] := PtrUInt(@lbMOVF32_f64);

JumpTable[bcMOVF64_i32] := PtrUInt(@lbMOVF64_i32);
JumpTable[bcMOVF64_u32] := PtrUInt(@lbMOVF64_u32);
JumpTable[bcMOVF64_i64] := PtrUInt(@lbMOVF64_i64);
JumpTable[bcMOVF64_u64] := PtrUInt(@lbMOVF64_u64);
JumpTable[bcMOVF64_f32] := PtrUInt(@lbMOVF64_f32);
JumpTable[bcMOVF64_f64] := PtrUInt(@lbMOVF64_f64);


// ---------------------------------------------
// STORE Operations (Destination Address, Source)
// Write from stack into memory
// ---------------------------------------------
JumpTable[bcSTORE] := PtrUInt(@lbSTORE);


// --- Same-Size or Truncating (Narrowing) ---
JumpTable[bcSTORE8] := PtrUInt(@lbSTORE8);
JumpTable[bcSTORE16] := PtrUInt(@lbSTORE16);
JumpTable[bcSTORE32] := PtrUInt(@lbSTORE32);
JumpTable[bcSTORE64] := PtrUInt(@lbSTORE64);


// --- Widening Signed (Sign Extension) ---
JumpTable[bcSTORESX16_8] := PtrUInt(@lbSTORESX16_8);

JumpTable[bcSTORESX32_8] := PtrUInt(@lbSTORESX32_8);
JumpTable[bcSTORESX32_16] := PtrUInt(@lbSTORESX32_16);

JumpTable[bcSTORESX64_8] := PtrUInt(@lbSTORESX64_8);
JumpTable[bcSTORESX64_16] := PtrUInt(@lbSTORESX64_16);
JumpTable[bcSTORESX64_32] := PtrUInt(@lbSTORESX64_32);


// --- Widening Unsigned (Zero Extension) ---
JumpTable[bcSTOREZX16_8] := PtrUInt(@lbSTOREZX16_8);

JumpTable[bcSTOREZX32_8] := PtrUInt(@lbSTOREZX32_8);
JumpTable[bcSTOREZX32_16] := PtrUInt(@lbSTOREZX32_16);

JumpTable[bcSTOREZX64_8] := PtrUInt(@lbSTOREZX64_8);
JumpTable[bcSTOREZX64_16] := PtrUInt(@lbSTOREZX64_16);
JumpTable[bcSTOREZX64_32] := PtrUInt(@lbSTOREZX64_32);


// --- Float Source Types (f32, f64) ---
// --- Destination Types (i32, u32, i64, u64, f32, f64) ---
JumpTable[bcSTOREF32_i32] := PtrUInt(@lbSTOREF32_i32);
JumpTable[bcSTOREF32_u32] := PtrUInt(@lbSTOREF32_u32);
JumpTable[bcSTOREF32_i64] := PtrUInt(@lbSTOREF32_i64);
JumpTable[bcSTOREF32_u64] := PtrUInt(@lbSTOREF32_u64);
JumpTable[bcSTOREF32_f32] := PtrUInt(@lbSTOREF32_f32);
JumpTable[bcSTOREF32_f64] := PtrUInt(@lbSTOREF32_f64);

JumpTable[bcSTOREF64_i32] := PtrUInt(@lbSTOREF64_i32);
JumpTable[bcSTOREF64_u32] := PtrUInt(@lbSTOREF64_u32);
JumpTable[bcSTOREF64_i64] := PtrUInt(@lbSTOREF64_i64);
JumpTable[bcSTOREF64_u64] := PtrUInt(@lbSTOREF64_u64);
JumpTable[bcSTOREF64_f32] := PtrUInt(@lbSTOREF64_f32);
JumpTable[bcSTOREF64_f64] := PtrUInt(@lbSTOREF64_f64);

JumpTable[bcERROR] := PtrUInt(@lbERROR);


(*
  --- LC ----
*)
// xpri_directthreaded_bc2lb.inc
// This file populates the JumpTable_LC with addresses of direct-threaded labels.
// It assumes 'JumpTable_LC' is an array of PtrUInt indexed by EBytecode.
JumpTable_LC[bcADDR] := PtrUInt(@lbADDR_LC);

JumpTable_LC[bcDREF] := PtrUInt(@lbDREF_LC);
JumpTable_LC[bcDREF32] := PtrUInt(@lbDREF32_LC);
JumpTable_LC[bcDREF64] := PtrUInt(@lbDREF64_LC);


JumpTable_LC[bcINC_i32] := PtrUInt(@lbINC_i32_LC);
JumpTable_LC[bcINC_u32] := PtrUInt(@lbINC_u32_LC);
JumpTable_LC[bcINC_i64] := PtrUInt(@lbINC_i64_LC);
JumpTable_LC[bcINC_u64] := PtrUInt(@lbINC_u64_LC);


// specialized ops
JumpTable_LC[bcFMAD64_64] := PtrUInt(@lbFMAD64_64_LC);
JumpTable_LC[bcFMAD64_32] := PtrUInt(@lbFMAD64_32_LC);
JumpTable_LC[bcFMAD32_64] := PtrUInt(@lbFMAD32_64_LC);
JumpTable_LC[bcFMAD32_32] := PtrUInt(@lbFMAD32_32_LC);

JumpTable_LC[bcFMA_i8] := PtrUInt(@lbFMA_i8_LC);
JumpTable_LC[bcFMA_u8] := PtrUInt(@lbFMA_u8_LC);
JumpTable_LC[bcFMA_i16] := PtrUInt(@lbFMA_i16_LC);
JumpTable_LC[bcFMA_u16] := PtrUInt(@lbFMA_u16_LC);
JumpTable_LC[bcFMA_i32] := PtrUInt(@lbFMA_i32_LC);
JumpTable_LC[bcFMA_u32] := PtrUInt(@lbFMA_u32_LC);
JumpTable_LC[bcFMA_i64] := PtrUInt(@lbFMA_i64_LC);
JumpTable_LC[bcFMA_u64] := PtrUInt(@lbFMA_u64_LC);


// reference point
JumpTable_LC[bcADD] := PtrUInt(@lbADD_LC);


// Specialize for local dest
JumpTable_LC[bcADD_i32] := PtrUInt(@lbADD_i32_LC);
JumpTable_LC[bcADD_u32] := PtrUInt(@lbADD_u32_LC);
JumpTable_LC[bcADD_i64] := PtrUInt(@lbADD_i64_LC);
JumpTable_LC[bcADD_u64] := PtrUInt(@lbADD_u64_LC);
JumpTable_LC[bcADD_f32] := PtrUInt(@lbADD_f32_LC);
JumpTable_LC[bcADD_f64] := PtrUInt(@lbADD_f64_LC);

JumpTable_LC[bcSUB_i32] := PtrUInt(@lbSUB_i32_LC);
JumpTable_LC[bcSUB_u32] := PtrUInt(@lbSUB_u32_LC);
JumpTable_LC[bcSUB_i64] := PtrUInt(@lbSUB_i64_LC);
JumpTable_LC[bcSUB_u64] := PtrUInt(@lbSUB_u64_LC);
JumpTable_LC[bcSUB_f32] := PtrUInt(@lbSUB_f32_LC);
JumpTable_LC[bcSUB_f64] := PtrUInt(@lbSUB_f64_LC);

JumpTable_LC[bcMUL_i32] := PtrUInt(@lbMUL_i32_LC);
JumpTable_LC[bcMUL_u32] := PtrUInt(@lbMUL_u32_LC);
JumpTable_LC[bcMUL_i64] := PtrUInt(@lbMUL_i64_LC);
JumpTable_LC[bcMUL_u64] := PtrUInt(@lbMUL_u64_LC);
JumpTable_LC[bcMUL_f32] := PtrUInt(@lbMUL_f32_LC);
JumpTable_LC[bcMUL_f64] := PtrUInt(@lbMUL_f64_LC);

JumpTable_LC[bcDIV_i32] := PtrUInt(@lbDIV_i32_LC);
JumpTable_LC[bcDIV_u32] := PtrUInt(@lbDIV_u32_LC);
JumpTable_LC[bcDIV_i64] := PtrUInt(@lbDIV_i64_LC);
JumpTable_LC[bcDIV_u64] := PtrUInt(@lbDIV_u64_LC);
JumpTable_LC[bcDIV_f32] := PtrUInt(@lbDIV_f32_LC);
JumpTable_LC[bcDIV_f64] := PtrUInt(@lbDIV_f64_LC);

JumpTable_LC[bcMOD_i32] := PtrUInt(@lbMOD_i32_LC);
JumpTable_LC[bcMOD_u32] := PtrUInt(@lbMOD_u32_LC);
JumpTable_LC[bcMOD_i64] := PtrUInt(@lbMOD_i64_LC);
JumpTable_LC[bcMOD_u64] := PtrUInt(@lbMOD_u64_LC);
JumpTable_LC[bcMOD_f32] := PtrUInt(@lbMOD_f32_LC);
JumpTable_LC[bcMOD_f64] := PtrUInt(@lbMOD_f64_LC);

JumpTable_LC[bcPOW_i32] := PtrUInt(@lbPOW_i32_LC);
JumpTable_LC[bcPOW_u32] := PtrUInt(@lbPOW_u32_LC);
JumpTable_LC[bcPOW_i64] := PtrUInt(@lbPOW_i64_LC);
JumpTable_LC[bcPOW_u64] := PtrUInt(@lbPOW_u64_LC);
JumpTable_LC[bcPOW_f32] := PtrUInt(@lbPOW_f32_LC);
JumpTable_LC[bcPOW_f64] := PtrUInt(@lbPOW_f64_LC);


JumpTable_LC[bcEQ_i32] := PtrUInt(@lbEQ_i32_LC);
JumpTable_LC[bcEQ_u32] := PtrUInt(@lbEQ_u32_LC);
JumpTable_LC[bcEQ_i64] := PtrUInt(@lbEQ_i64_LC);
JumpTable_LC[bcEQ_u64] := PtrUInt(@lbEQ_u64_LC);
JumpTable_LC[bcEQ_f32] := PtrUInt(@lbEQ_f32_LC);
JumpTable_LC[bcEQ_f64] := PtrUInt(@lbEQ_f64_LC);

JumpTable_LC[bcNE_i32] := PtrUInt(@lbNE_i32_LC);
JumpTable_LC[bcNE_u32] := PtrUInt(@lbNE_u32_LC);
JumpTable_LC[bcNE_i64] := PtrUInt(@lbNE_i64_LC);
JumpTable_LC[bcNE_u64] := PtrUInt(@lbNE_u64_LC);
JumpTable_LC[bcNE_f32] := PtrUInt(@lbNE_f32_LC);
JumpTable_LC[bcNE_f64] := PtrUInt(@lbNE_f64_LC);

JumpTable_LC[bcLT_i32] := PtrUInt(@lbLT_i32_LC);
JumpTable_LC[bcLT_u32] := PtrUInt(@lbLT_u32_LC);
JumpTable_LC[bcLT_i64] := PtrUInt(@lbLT_i64_LC);
JumpTable_LC[bcLT_u64] := PtrUInt(@lbLT_u64_LC);
JumpTable_LC[bcLT_f32] := PtrUInt(@lbLT_f32_LC);
JumpTable_LC[bcLT_f64] := PtrUInt(@lbLT_f64_LC);

JumpTable_LC[bcGT_i32] := PtrUInt(@lbGT_i32_LC);
JumpTable_LC[bcGT_u32] := PtrUInt(@lbGT_u32_LC);
JumpTable_LC[bcGT_i64] := PtrUInt(@lbGT_i64_LC);
JumpTable_LC[bcGT_u64] := PtrUInt(@lbGT_u64_LC);
JumpTable_LC[bcGT_f32] := PtrUInt(@lbGT_f32_LC);
JumpTable_LC[bcGT_f64] := PtrUInt(@lbGT_f64_LC);

JumpTable_LC[bcGTE_i32] := PtrUInt(@lbGTE_i32_LC);
JumpTable_LC[bcGTE_u32] := PtrUInt(@lbGTE_u32_LC);
JumpTable_LC[bcGTE_i64] := PtrUInt(@lbGTE_i64_LC);
JumpTable_LC[bcGTE_u64] := PtrUInt(@lbGTE_u64_LC);
JumpTable_LC[bcGTE_f32] := PtrUInt(@lbGTE_f32_LC);
JumpTable_LC[bcGTE_f64] := PtrUInt(@lbGTE_f64_LC);

JumpTable_LC[bcLTE_i32] := PtrUInt(@lbLTE_i32_LC);
JumpTable_LC[bcLTE_u32] := PtrUInt(@lbLTE_u32_LC);
JumpTable_LC[bcLTE_i64] := PtrUInt(@lbLTE_i64_LC);
JumpTable_LC[bcLTE_u64] := PtrUInt(@lbLTE_u64_LC);
JumpTable_LC[bcLTE_f32] := PtrUInt(@lbLTE_f32_LC);
JumpTable_LC[bcLTE_f64] := PtrUInt(@lbLTE_f64_LC);


JumpTable_LC[bcBND_i32] := PtrUInt(@lbBND_i32_LC);
JumpTable_LC[bcBND_u32] := PtrUInt(@lbBND_u32_LC);
JumpTable_LC[bcBND_i64] := PtrUInt(@lbBND_i64_LC);
JumpTable_LC[bcBND_u64] := PtrUInt(@lbBND_u64_LC);

JumpTable_LC[bcSHL_i32] := PtrUInt(@lbSHL_i32_LC);
JumpTable_LC[bcSHL_u32] := PtrUInt(@lbSHL_u32_LC);
JumpTable_LC[bcSHL_i64] := PtrUInt(@lbSHL_i64_LC);
JumpTable_LC[bcSHL_u64] := PtrUInt(@lbSHL_u64_LC);

JumpTable_LC[bcSHR_i32] := PtrUInt(@lbSHR_i32_LC);
JumpTable_LC[bcSHR_u32] := PtrUInt(@lbSHR_u32_LC);
JumpTable_LC[bcSHR_i64] := PtrUInt(@lbSHR_i64_LC);
JumpTable_LC[bcSHR_u64] := PtrUInt(@lbSHR_u64_LC);

JumpTable_LC[bcXOR_i32] := PtrUInt(@lbXOR_i32_LC);
JumpTable_LC[bcXOR_u32] := PtrUInt(@lbXOR_u32_LC);
JumpTable_LC[bcXOR_i64] := PtrUInt(@lbXOR_i64_LC);
JumpTable_LC[bcXOR_u64] := PtrUInt(@lbXOR_u64_LC);

JumpTable_LC[bcBOR_i32] := PtrUInt(@lbBOR_i32_LC);
JumpTable_LC[bcBOR_u32] := PtrUInt(@lbBOR_u32_LC);
JumpTable_LC[bcBOR_i64] := PtrUInt(@lbBOR_i64_LC);
JumpTable_LC[bcBOR_u64] := PtrUInt(@lbBOR_u64_LC);

JumpTable_LC[bcSAR_i32] := PtrUInt(@lbSAR_i32_LC);
JumpTable_LC[bcSAR_u32] := PtrUInt(@lbSAR_u32_LC);
JumpTable_LC[bcSAR_i64] := PtrUInt(@lbSAR_i64_LC);
JumpTable_LC[bcSAR_u64] := PtrUInt(@lbSAR_u64_LC);


// ---------------------------------------------
// MOV Operations (Destination, Source)
// ---------------------------------------------
JumpTable_LC[bcMOV] := PtrUInt(@lbMOV_LC);


// --- Same-Size or Truncating (Narrowing)
JumpTable_LC[bcMOV8] := PtrUInt(@lbMOV8_LC);
JumpTable_LC[bcMOV16] := PtrUInt(@lbMOV16_LC);
JumpTable_LC[bcMOV32] := PtrUInt(@lbMOV32_LC);
JumpTable_LC[bcMOV64] := PtrUInt(@lbMOV64_LC);


// --- Widening Signed (Sign Extension)
JumpTable_LC[bcMOVSX16_8] := PtrUInt(@lbMOVSX16_8_LC);

JumpTable_LC[bcMOVSX32_8] := PtrUInt(@lbMOVSX32_8_LC);
JumpTable_LC[bcMOVSX32_16] := PtrUInt(@lbMOVSX32_16_LC);

JumpTable_LC[bcMOVSX64_8] := PtrUInt(@lbMOVSX64_8_LC);
JumpTable_LC[bcMOVSX64_16] := PtrUInt(@lbMOVSX64_16_LC);
JumpTable_LC[bcMOVSX64_32] := PtrUInt(@lbMOVSX64_32_LC);


// --- Widening Unsigned (Zero Extension)
JumpTable_LC[bcMOVZX16_8] := PtrUInt(@lbMOVZX16_8_LC);

JumpTable_LC[bcMOVZX32_8] := PtrUInt(@lbMOVZX32_8_LC);
JumpTable_LC[bcMOVZX32_16] := PtrUInt(@lbMOVZX32_16_LC);

JumpTable_LC[bcMOVZX64_8] := PtrUInt(@lbMOVZX64_8_LC);
JumpTable_LC[bcMOVZX64_16] := PtrUInt(@lbMOVZX64_16_LC);
JumpTable_LC[bcMOVZX64_32] := PtrUInt(@lbMOVZX64_32_LC);


// --- Float Source Types (f32, f64) ---
// --- Destination Types (i32, u32, i64, u64, f32, f64) ---
JumpTable_LC[bcMOVF32_i32] := PtrUInt(@lbMOVF32_i32_LC);
JumpTable_LC[bcMOVF32_u32] := PtrUInt(@lbMOVF32_u32_LC);
JumpTable_LC[bcMOVF32_i64] := PtrUInt(@lbMOVF32_i64_LC);
JumpTable_LC[bcMOVF32_u64] := PtrUInt(@lbMOVF32_u64_LC);
JumpTable_LC[bcMOVF32_f32] := PtrUInt(@lbMOVF32_f32_LC);
JumpTable_LC[bcMOVF32_f64] := PtrUInt(@lbMOVF32_f64_LC);

JumpTable_LC[bcMOVF64_i32] := PtrUInt(@lbMOVF64_i32_LC);
JumpTable_LC[bcMOVF64_u32] := PtrUInt(@lbMOVF64_u32_LC);
JumpTable_LC[bcMOVF64_i64] := PtrUInt(@lbMOVF64_i64_LC);
JumpTable_LC[bcMOVF64_u64] := PtrUInt(@lbMOVF64_u64_LC);
JumpTable_LC[bcMOVF64_f32] := PtrUInt(@lbMOVF64_f32_LC);
JumpTable_LC[bcMOVF64_f64] := PtrUInt(@lbMOVF64_f64_LC);


// ---------------------------------------------
// STORE Operations (Destination Address, Source)
// Write from stack into memory
// ---------------------------------------------
JumpTable_LC[bcSTORE] := PtrUInt(@lbSTORE_LC);


// --- Same-Size or Truncating (Narrowing) ---
JumpTable_LC[bcSTORE8] := PtrUInt(@lbSTORE8_LC);
JumpTable_LC[bcSTORE16] := PtrUInt(@lbSTORE16_LC);
JumpTable_LC[bcSTORE32] := PtrUInt(@lbSTORE32_LC);
JumpTable_LC[bcSTORE64] := PtrUInt(@lbSTORE64_LC);


// --- Widening Signed (Sign Extension) ---
JumpTable_LC[bcSTORESX16_8] := PtrUInt(@lbSTORESX16_8_LC);

JumpTable_LC[bcSTORESX32_8] := PtrUInt(@lbSTORESX32_8_LC);
JumpTable_LC[bcSTORESX32_16] := PtrUInt(@lbSTORESX32_16_LC);

JumpTable_LC[bcSTORESX64_8] := PtrUInt(@lbSTORESX64_8_LC);
JumpTable_LC[bcSTORESX64_16] := PtrUInt(@lbSTORESX64_16_LC);
JumpTable_LC[bcSTORESX64_32] := PtrUInt(@lbSTORESX64_32_LC);


// --- Widening Unsigned (Zero Extension) ---
JumpTable_LC[bcSTOREZX16_8] := PtrUInt(@lbSTOREZX16_8_LC);

JumpTable_LC[bcSTOREZX32_8] := PtrUInt(@lbSTOREZX32_8_LC);
JumpTable_LC[bcSTOREZX32_16] := PtrUInt(@lbSTOREZX32_16_LC);

JumpTable_LC[bcSTOREZX64_8] := PtrUInt(@lbSTOREZX64_8_LC);
JumpTable_LC[bcSTOREZX64_16] := PtrUInt(@lbSTOREZX64_16_LC);
JumpTable_LC[bcSTOREZX64_32] := PtrUInt(@lbSTOREZX64_32_LC);


// --- Float Source Types (f32, f64) ---
// --- Destination Types (i32, u32, i64, u64, f32, f64) ---
JumpTable_LC[bcSTOREF32_i32] := PtrUInt(@lbSTOREF32_i32_LC);
JumpTable_LC[bcSTOREF32_u32] := PtrUInt(@lbSTOREF32_u32_LC);
JumpTable_LC[bcSTOREF32_i64] := PtrUInt(@lbSTOREF32_i64_LC);
JumpTable_LC[bcSTOREF32_u64] := PtrUInt(@lbSTOREF32_u64_LC);
JumpTable_LC[bcSTOREF32_f32] := PtrUInt(@lbSTOREF32_f32_LC);
JumpTable_LC[bcSTOREF32_f64] := PtrUInt(@lbSTOREF32_f64_LC);

JumpTable_LC[bcSTOREF64_i32] := PtrUInt(@lbSTOREF64_i32_LC);
JumpTable_LC[bcSTOREF64_u32] := PtrUInt(@lbSTOREF64_u32_LC);
JumpTable_LC[bcSTOREF64_i64] := PtrUInt(@lbSTOREF64_i64_LC);
JumpTable_LC[bcSTOREF64_u64] := PtrUInt(@lbSTOREF64_u64_LC);
JumpTable_LC[bcSTOREF64_f32] := PtrUInt(@lbSTOREF64_f32_LC);
JumpTable_LC[bcSTOREF64_f64] := PtrUInt(@lbSTOREF64_f64_LC);

JumpTable_LC[bcERROR] := PtrUInt(@lbERROR_LC);

(*
 --- CL ---
*)
// xpri_directthreaded_bc2lb.inc
// This file populates the JumpTable_CL with addresses of direct-threaded labels.
// It assumes 'JumpTable_CL' is an array of PtrUInt indexed by EBytecode.
JumpTable_CL[bcADDR] := PtrUInt(@lbADDR_CL);

JumpTable_CL[bcDREF] := PtrUInt(@lbDREF_CL);
JumpTable_CL[bcDREF32] := PtrUInt(@lbDREF32_CL);
JumpTable_CL[bcDREF64] := PtrUInt(@lbDREF64_CL);


JumpTable_CL[bcINC_i32] := PtrUInt(@lbINC_i32_CL);
JumpTable_CL[bcINC_u32] := PtrUInt(@lbINC_u32_CL);
JumpTable_CL[bcINC_i64] := PtrUInt(@lbINC_i64_CL);
JumpTable_CL[bcINC_u64] := PtrUInt(@lbINC_u64_CL);


// specialized ops
JumpTable_CL[bcFMAD64_64] := PtrUInt(@lbFMAD64_64_CL);
JumpTable_CL[bcFMAD64_32] := PtrUInt(@lbFMAD64_32_CL);
JumpTable_CL[bcFMAD32_64] := PtrUInt(@lbFMAD32_64_CL);
JumpTable_CL[bcFMAD32_32] := PtrUInt(@lbFMAD32_32_CL);

JumpTable_CL[bcFMA_i8] := PtrUInt(@lbFMA_i8_CL);
JumpTable_CL[bcFMA_u8] := PtrUInt(@lbFMA_u8_CL);
JumpTable_CL[bcFMA_i16] := PtrUInt(@lbFMA_i16_CL);
JumpTable_CL[bcFMA_u16] := PtrUInt(@lbFMA_u16_CL);
JumpTable_CL[bcFMA_i32] := PtrUInt(@lbFMA_i32_CL);
JumpTable_CL[bcFMA_u32] := PtrUInt(@lbFMA_u32_CL);
JumpTable_CL[bcFMA_i64] := PtrUInt(@lbFMA_i64_CL);
JumpTable_CL[bcFMA_u64] := PtrUInt(@lbFMA_u64_CL);


// reference point
JumpTable_CL[bcADD] := PtrUInt(@lbADD_CL);


// Specialize for local dest
JumpTable_CL[bcADD_i32] := PtrUInt(@lbADD_i32_CL);
JumpTable_CL[bcADD_u32] := PtrUInt(@lbADD_u32_CL);
JumpTable_CL[bcADD_i64] := PtrUInt(@lbADD_i64_CL);
JumpTable_CL[bcADD_u64] := PtrUInt(@lbADD_u64_CL);
JumpTable_CL[bcADD_f32] := PtrUInt(@lbADD_f32_CL);
JumpTable_CL[bcADD_f64] := PtrUInt(@lbADD_f64_CL);

JumpTable_CL[bcSUB_i32] := PtrUInt(@lbSUB_i32_CL);
JumpTable_CL[bcSUB_u32] := PtrUInt(@lbSUB_u32_CL);
JumpTable_CL[bcSUB_i64] := PtrUInt(@lbSUB_i64_CL);
JumpTable_CL[bcSUB_u64] := PtrUInt(@lbSUB_u64_CL);
JumpTable_CL[bcSUB_f32] := PtrUInt(@lbSUB_f32_CL);
JumpTable_CL[bcSUB_f64] := PtrUInt(@lbSUB_f64_CL);

JumpTable_CL[bcMUL_i32] := PtrUInt(@lbMUL_i32_CL);
JumpTable_CL[bcMUL_u32] := PtrUInt(@lbMUL_u32_CL);
JumpTable_CL[bcMUL_i64] := PtrUInt(@lbMUL_i64_CL);
JumpTable_CL[bcMUL_u64] := PtrUInt(@lbMUL_u64_CL);
JumpTable_CL[bcMUL_f32] := PtrUInt(@lbMUL_f32_CL);
JumpTable_CL[bcMUL_f64] := PtrUInt(@lbMUL_f64_CL);

JumpTable_CL[bcDIV_i32] := PtrUInt(@lbDIV_i32_CL);
JumpTable_CL[bcDIV_u32] := PtrUInt(@lbDIV_u32_CL);
JumpTable_CL[bcDIV_i64] := PtrUInt(@lbDIV_i64_CL);
JumpTable_CL[bcDIV_u64] := PtrUInt(@lbDIV_u64_CL);
JumpTable_CL[bcDIV_f32] := PtrUInt(@lbDIV_f32_CL);
JumpTable_CL[bcDIV_f64] := PtrUInt(@lbDIV_f64_CL);

JumpTable_CL[bcMOD_i32] := PtrUInt(@lbMOD_i32_CL);
JumpTable_CL[bcMOD_u32] := PtrUInt(@lbMOD_u32_CL);
JumpTable_CL[bcMOD_i64] := PtrUInt(@lbMOD_i64_CL);
JumpTable_CL[bcMOD_u64] := PtrUInt(@lbMOD_u64_CL);
JumpTable_CL[bcMOD_f32] := PtrUInt(@lbMOD_f32_CL);
JumpTable_CL[bcMOD_f64] := PtrUInt(@lbMOD_f64_CL);

JumpTable_CL[bcPOW_i32] := PtrUInt(@lbPOW_i32_CL);
JumpTable_CL[bcPOW_u32] := PtrUInt(@lbPOW_u32_CL);
JumpTable_CL[bcPOW_i64] := PtrUInt(@lbPOW_i64_CL);
JumpTable_CL[bcPOW_u64] := PtrUInt(@lbPOW_u64_CL);
JumpTable_CL[bcPOW_f32] := PtrUInt(@lbPOW_f32_CL);
JumpTable_CL[bcPOW_f64] := PtrUInt(@lbPOW_f64_CL);


JumpTable_CL[bcEQ_i32] := PtrUInt(@lbEQ_i32_CL);
JumpTable_CL[bcEQ_u32] := PtrUInt(@lbEQ_u32_CL);
JumpTable_CL[bcEQ_i64] := PtrUInt(@lbEQ_i64_CL);
JumpTable_CL[bcEQ_u64] := PtrUInt(@lbEQ_u64_CL);
JumpTable_CL[bcEQ_f32] := PtrUInt(@lbEQ_f32_CL);
JumpTable_CL[bcEQ_f64] := PtrUInt(@lbEQ_f64_CL);

JumpTable_CL[bcNE_i32] := PtrUInt(@lbNE_i32_CL);
JumpTable_CL[bcNE_u32] := PtrUInt(@lbNE_u32_CL);
JumpTable_CL[bcNE_i64] := PtrUInt(@lbNE_i64_CL);
JumpTable_CL[bcNE_u64] := PtrUInt(@lbNE_u64_CL);
JumpTable_CL[bcNE_f32] := PtrUInt(@lbNE_f32_CL);
JumpTable_CL[bcNE_f64] := PtrUInt(@lbNE_f64_CL);

JumpTable_CL[bcLT_i32] := PtrUInt(@lbLT_i32_CL);
JumpTable_CL[bcLT_u32] := PtrUInt(@lbLT_u32_CL);
JumpTable_CL[bcLT_i64] := PtrUInt(@lbLT_i64_CL);
JumpTable_CL[bcLT_u64] := PtrUInt(@lbLT_u64_CL);
JumpTable_CL[bcLT_f32] := PtrUInt(@lbLT_f32_CL);
JumpTable_CL[bcLT_f64] := PtrUInt(@lbLT_f64_CL);

JumpTable_CL[bcGT_i32] := PtrUInt(@lbGT_i32_CL);
JumpTable_CL[bcGT_u32] := PtrUInt(@lbGT_u32_CL);
JumpTable_CL[bcGT_i64] := PtrUInt(@lbGT_i64_CL);
JumpTable_CL[bcGT_u64] := PtrUInt(@lbGT_u64_CL);
JumpTable_CL[bcGT_f32] := PtrUInt(@lbGT_f32_CL);
JumpTable_CL[bcGT_f64] := PtrUInt(@lbGT_f64_CL);

JumpTable_CL[bcGTE_i32] := PtrUInt(@lbGTE_i32_CL);
JumpTable_CL[bcGTE_u32] := PtrUInt(@lbGTE_u32_CL);
JumpTable_CL[bcGTE_i64] := PtrUInt(@lbGTE_i64_CL);
JumpTable_CL[bcGTE_u64] := PtrUInt(@lbGTE_u64_CL);
JumpTable_CL[bcGTE_f32] := PtrUInt(@lbGTE_f32_CL);
JumpTable_CL[bcGTE_f64] := PtrUInt(@lbGTE_f64_CL);

JumpTable_CL[bcLTE_i32] := PtrUInt(@lbLTE_i32_CL);
JumpTable_CL[bcLTE_u32] := PtrUInt(@lbLTE_u32_CL);
JumpTable_CL[bcLTE_i64] := PtrUInt(@lbLTE_i64_CL);
JumpTable_CL[bcLTE_u64] := PtrUInt(@lbLTE_u64_CL);
JumpTable_CL[bcLTE_f32] := PtrUInt(@lbLTE_f32_CL);
JumpTable_CL[bcLTE_f64] := PtrUInt(@lbLTE_f64_CL);


JumpTable_CL[bcBND_i32] := PtrUInt(@lbBND_i32_CL);
JumpTable_CL[bcBND_u32] := PtrUInt(@lbBND_u32_CL);
JumpTable_CL[bcBND_i64] := PtrUInt(@lbBND_i64_CL);
JumpTable_CL[bcBND_u64] := PtrUInt(@lbBND_u64_CL);

JumpTable_CL[bcSHL_i32] := PtrUInt(@lbSHL_i32_CL);
JumpTable_CL[bcSHL_u32] := PtrUInt(@lbSHL_u32_CL);
JumpTable_CL[bcSHL_i64] := PtrUInt(@lbSHL_i64_CL);
JumpTable_CL[bcSHL_u64] := PtrUInt(@lbSHL_u64_CL);

JumpTable_CL[bcSHR_i32] := PtrUInt(@lbSHR_i32_CL);
JumpTable_CL[bcSHR_u32] := PtrUInt(@lbSHR_u32_CL);
JumpTable_CL[bcSHR_i64] := PtrUInt(@lbSHR_i64_CL);
JumpTable_CL[bcSHR_u64] := PtrUInt(@lbSHR_u64_CL);

JumpTable_CL[bcXOR_i32] := PtrUInt(@lbXOR_i32_CL);
JumpTable_CL[bcXOR_u32] := PtrUInt(@lbXOR_u32_CL);
JumpTable_CL[bcXOR_i64] := PtrUInt(@lbXOR_i64_CL);
JumpTable_CL[bcXOR_u64] := PtrUInt(@lbXOR_u64_CL);

JumpTable_CL[bcBOR_i32] := PtrUInt(@lbBOR_i32_CL);
JumpTable_CL[bcBOR_u32] := PtrUInt(@lbBOR_u32_CL);
JumpTable_CL[bcBOR_i64] := PtrUInt(@lbBOR_i64_CL);
JumpTable_CL[bcBOR_u64] := PtrUInt(@lbBOR_u64_CL);

JumpTable_CL[bcSAR_i32] := PtrUInt(@lbSAR_i32_CL);
JumpTable_CL[bcSAR_u32] := PtrUInt(@lbSAR_u32_CL);
JumpTable_CL[bcSAR_i64] := PtrUInt(@lbSAR_i64_CL);
JumpTable_CL[bcSAR_u64] := PtrUInt(@lbSAR_u64_CL);


// ---------------------------------------------
// MOV Operations (Destination, Source)
// ---------------------------------------------
JumpTable_CL[bcMOV] := PtrUInt(@lbMOV_CL);


// --- Same-Size or Truncating (Narrowing)
JumpTable_CL[bcMOV8] := PtrUInt(@lbMOV8_CL);
JumpTable_CL[bcMOV16] := PtrUInt(@lbMOV16_CL);
JumpTable_CL[bcMOV32] := PtrUInt(@lbMOV32_CL);
JumpTable_CL[bcMOV64] := PtrUInt(@lbMOV64_CL);


// --- Widening Signed (Sign Extension)
JumpTable_CL[bcMOVSX16_8] := PtrUInt(@lbMOVSX16_8_CL);

JumpTable_CL[bcMOVSX32_8] := PtrUInt(@lbMOVSX32_8_CL);
JumpTable_CL[bcMOVSX32_16] := PtrUInt(@lbMOVSX32_16_CL);

JumpTable_CL[bcMOVSX64_8] := PtrUInt(@lbMOVSX64_8_CL);
JumpTable_CL[bcMOVSX64_16] := PtrUInt(@lbMOVSX64_16_CL);
JumpTable_CL[bcMOVSX64_32] := PtrUInt(@lbMOVSX64_32_CL);


// --- Widening Unsigned (Zero Extension)
JumpTable_CL[bcMOVZX16_8] := PtrUInt(@lbMOVZX16_8_CL);

JumpTable_CL[bcMOVZX32_8] := PtrUInt(@lbMOVZX32_8_CL);
JumpTable_CL[bcMOVZX32_16] := PtrUInt(@lbMOVZX32_16_CL);

JumpTable_CL[bcMOVZX64_8] := PtrUInt(@lbMOVZX64_8_CL);
JumpTable_CL[bcMOVZX64_16] := PtrUInt(@lbMOVZX64_16_CL);
JumpTable_CL[bcMOVZX64_32] := PtrUInt(@lbMOVZX64_32_CL);


// --- Float Source Types (f32, f64) ---
// --- Destination Types (i32, u32, i64, u64, f32, f64) ---
JumpTable_CL[bcMOVF32_i32] := PtrUInt(@lbMOVF32_i32_CL);
JumpTable_CL[bcMOVF32_u32] := PtrUInt(@lbMOVF32_u32_CL);
JumpTable_CL[bcMOVF32_i64] := PtrUInt(@lbMOVF32_i64_CL);
JumpTable_CL[bcMOVF32_u64] := PtrUInt(@lbMOVF32_u64_CL);
JumpTable_CL[bcMOVF32_f32] := PtrUInt(@lbMOVF32_f32_CL);
JumpTable_CL[bcMOVF32_f64] := PtrUInt(@lbMOVF32_f64_CL);

JumpTable_CL[bcMOVF64_i32] := PtrUInt(@lbMOVF64_i32_CL);
JumpTable_CL[bcMOVF64_u32] := PtrUInt(@lbMOVF64_u32_CL);
JumpTable_CL[bcMOVF64_i64] := PtrUInt(@lbMOVF64_i64_CL);
JumpTable_CL[bcMOVF64_u64] := PtrUInt(@lbMOVF64_u64_CL);
JumpTable_CL[bcMOVF64_f32] := PtrUInt(@lbMOVF64_f32_CL);
JumpTable_CL[bcMOVF64_f64] := PtrUInt(@lbMOVF64_f64_CL);


// ---------------------------------------------
// STORE Operations (Destination Address, Source)
// Write from stack into memory
// ---------------------------------------------
JumpTable_CL[bcSTORE] := PtrUInt(@lbSTORE_CL);


// --- Same-Size or Truncating (Narrowing) ---
JumpTable_CL[bcSTORE8] := PtrUInt(@lbSTORE8_CL);
JumpTable_CL[bcSTORE16] := PtrUInt(@lbSTORE16_CL);
JumpTable_CL[bcSTORE32] := PtrUInt(@lbSTORE32_CL);
JumpTable_CL[bcSTORE64] := PtrUInt(@lbSTORE64_CL);


// --- Widening Signed (Sign Extension) ---
JumpTable_CL[bcSTORESX16_8] := PtrUInt(@lbSTORESX16_8_CL);

JumpTable_CL[bcSTORESX32_8] := PtrUInt(@lbSTORESX32_8_CL);
JumpTable_CL[bcSTORESX32_16] := PtrUInt(@lbSTORESX32_16_CL);

JumpTable_CL[bcSTORESX64_8] := PtrUInt(@lbSTORESX64_8_CL);
JumpTable_CL[bcSTORESX64_16] := PtrUInt(@lbSTORESX64_16_CL);
JumpTable_CL[bcSTORESX64_32] := PtrUInt(@lbSTORESX64_32_CL);


// --- Widening Unsigned (Zero Extension) ---
JumpTable_CL[bcSTOREZX16_8] := PtrUInt(@lbSTOREZX16_8_CL);

JumpTable_CL[bcSTOREZX32_8] := PtrUInt(@lbSTOREZX32_8_CL);
JumpTable_CL[bcSTOREZX32_16] := PtrUInt(@lbSTOREZX32_16_CL);

JumpTable_CL[bcSTOREZX64_8] := PtrUInt(@lbSTOREZX64_8_CL);
JumpTable_CL[bcSTOREZX64_16] := PtrUInt(@lbSTOREZX64_16_CL);
JumpTable_CL[bcSTOREZX64_32] := PtrUInt(@lbSTOREZX64_32_CL);


// --- Float Source Types (f32, f64) ---
// --- Destination Types (i32, u32, i64, u64, f32, f64) ---
JumpTable_CL[bcSTOREF32_i32] := PtrUInt(@lbSTOREF32_i32_CL);
JumpTable_CL[bcSTOREF32_u32] := PtrUInt(@lbSTOREF32_u32_CL);
JumpTable_CL[bcSTOREF32_i64] := PtrUInt(@lbSTOREF32_i64_CL);
JumpTable_CL[bcSTOREF32_u64] := PtrUInt(@lbSTOREF32_u64_CL);
JumpTable_CL[bcSTOREF32_f32] := PtrUInt(@lbSTOREF32_f32_CL);
JumpTable_CL[bcSTOREF32_f64] := PtrUInt(@lbSTOREF32_f64_CL);

JumpTable_CL[bcSTOREF64_i32] := PtrUInt(@lbSTOREF64_i32_CL);
JumpTable_CL[bcSTOREF64_u32] := PtrUInt(@lbSTOREF64_u32_CL);
JumpTable_CL[bcSTOREF64_i64] := PtrUInt(@lbSTOREF64_i64_CL);
JumpTable_CL[bcSTOREF64_u64] := PtrUInt(@lbSTOREF64_u64_CL);
JumpTable_CL[bcSTOREF64_f32] := PtrUInt(@lbSTOREF64_f32_CL);
JumpTable_CL[bcSTOREF64_f64] := PtrUInt(@lbSTOREF64_f64_CL);

JumpTable_CL[bcERROR] := PtrUInt(@lbERROR_CL);
