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
