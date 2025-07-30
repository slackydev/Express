lbINC_i32,lbINC_u32,lbINC_i64,lbINC_u64,

lbFMA_i8, lbFMA_u8, lbFMA_i16, lbFMA_u16, lbFMA_i32, lbFMA_u32, lbFMA_i64, lbFMA_u64,
lbFMA_imm_i8, lbFMA_imm_u8, lbFMA_imm_i16, lbFMA_imm_u16, lbFMA_imm_i32, lbFMA_imm_u32, lbFMA_imm_i64, lbFMA_imm_u64,
lbDREF, lbDREF_32, lbDREF_64,
lbFMAD_d64_64, lbFMAD_d32_64, lbFMAD_d64_32, lbFMAD_d32_32,

lbADD, // for reference

// Specialize for local dest
lbADD_lll_i32, lbADD_lll_u32, lbADD_lll_i64, lbADD_lll_u64, lbADD_lll_f32, lbADD_lll_f64,
lbADD_lil_i32, lbADD_lil_u32, lbADD_lil_i64, lbADD_lil_u64, lbADD_lil_f32, lbADD_lil_f64,

lbADD_ill_i32, lbADD_ill_u32, lbADD_ill_i64, lbADD_ill_u64, lbADD_ill_f32, lbADD_ill_f64,
lbADD_iil_i32, lbADD_iil_u32, lbADD_iil_i64, lbADD_iil_u64, lbADD_iil_f32, lbADD_iil_f64,

lbSUB_lll_i32, lbSUB_lll_u32, lbSUB_lll_i64, lbSUB_lll_u64, lbSUB_lll_f32, lbSUB_lll_f64,
lbSUB_lil_i32, lbSUB_lil_u32, lbSUB_lil_i64, lbSUB_lil_u64, lbSUB_lil_f32, lbSUB_lil_f64,

lbSUB_ill_i32, lbSUB_ill_u32, lbSUB_ill_i64, lbSUB_ill_u64, lbSUB_ill_f32, lbSUB_ill_f64,
lbSUB_iil_i32, lbSUB_iil_u32, lbSUB_iil_i64, lbSUB_iil_u64, lbSUB_iil_f32, lbSUB_iil_f64,

lbMUL_lll_i32, lbMUL_lll_u32, lbMUL_lll_i64, lbMUL_lll_u64, lbMUL_lll_f32, lbMUL_lll_f64,
lbMUL_lil_i32, lbMUL_lil_u32, lbMUL_lil_i64, lbMUL_lil_u64, lbMUL_lil_f32, lbMUL_lil_f64,

lbMUL_ill_i32, lbMUL_ill_u32, lbMUL_ill_i64, lbMUL_ill_u64, lbMUL_ill_f32, lbMUL_ill_f64,
lbMUL_iil_i32, lbMUL_iil_u32, lbMUL_iil_i64, lbMUL_iil_u64, lbMUL_iil_f32, lbMUL_iil_f64,

lbDIV_lll_i32, lbDIV_lll_u32, lbDIV_lll_i64, lbDIV_lll_u64, lbDIV_lll_f32, lbDIV_lll_f64,
lbDIV_lil_i32, lbDIV_lil_u32, lbDIV_lil_i64, lbDIV_lil_u64, lbDIV_lil_f32, lbDIV_lil_f64,

lbDIV_ill_i32, lbDIV_ill_u32, lbDIV_ill_i64, lbDIV_ill_u64, lbDIV_ill_f32, lbDIV_ill_f64,
lbDIV_iil_i32, lbDIV_iil_u32, lbDIV_iil_i64, lbDIV_iil_u64, lbDIV_iil_f32, lbDIV_iil_f64,

lbMOD_lll_i32, lbMOD_lll_u32, lbMOD_lll_i64, lbMOD_lll_u64, lbMOD_lll_f32, lbMOD_lll_f64,
lbMOD_lil_i32, lbMOD_lil_u32, lbMOD_lil_i64, lbMOD_lil_u64, lbMOD_lil_f32, lbMOD_lil_f64,

lbMOD_ill_i32, lbMOD_ill_u32, lbMOD_ill_i64, lbMOD_ill_u64, lbMOD_ill_f32, lbMOD_ill_f64,
lbMOD_iil_i32, lbMOD_iil_u32, lbMOD_iil_i64, lbMOD_iil_u64, lbMOD_iil_f32, lbMOD_iil_f64,

lbPOW_lll_i32, lbPOW_lll_u32, lbPOW_lll_i64, lbPOW_lll_u64, lbPOW_lll_f32, lbPOW_lll_f64,
lbPOW_lil_i32, lbPOW_lil_u32, lbPOW_lil_i64, lbPOW_lil_u64, lbPOW_lil_f32, lbPOW_lil_f64,

lbPOW_ill_i32, lbPOW_ill_u32, lbPOW_ill_i64, lbPOW_ill_u64, lbPOW_ill_f32, lbPOW_ill_f64,
lbPOW_iil_i32, lbPOW_iil_u32, lbPOW_iil_i64, lbPOW_iil_u64, lbPOW_iil_f32, lbPOW_iil_f64,

lbEQ_lll_i32, lbEQ_lll_u32, lbEQ_lll_i64, lbEQ_lll_u64, lbEQ_lll_f32, lbEQ_lll_f64,
lbEQ_lil_i32, lbEQ_lil_u32, lbEQ_lil_i64, lbEQ_lil_u64, lbEQ_lil_f32, lbEQ_lil_f64,

lbEQ_ill_i32, lbEQ_ill_u32, lbEQ_ill_i64, lbEQ_ill_u64, lbEQ_ill_f32, lbEQ_ill_f64,
lbEQ_iil_i32, lbEQ_iil_u32, lbEQ_iil_i64, lbEQ_iil_u64, lbEQ_iil_f32, lbEQ_iil_f64,

lbNE_lll_i32, lbNE_lll_u32, lbNE_lll_i64, lbNE_lll_u64, lbNE_lll_f32, lbNE_lll_f64,
lbNE_lil_i32, lbNE_lil_u32, lbNE_lil_i64, lbNE_lil_u64, lbNE_lil_f32, lbNE_lil_f64,

lbNE_ill_i32, lbNE_ill_u32, lbNE_ill_i64, lbNE_ill_u64, lbNE_ill_f32, lbNE_ill_f64,
lbNE_iil_i32, lbNE_iil_u32, lbNE_iil_i64, lbNE_iil_u64, lbNE_iil_f32, lbNE_iil_f64,

lbLT_lll_i32, lbLT_lll_u32, lbLT_lll_i64, lbLT_lll_u64, lbLT_lll_f32, lbLT_lll_f64,
lbLT_lil_i32, lbLT_lil_u32, lbLT_lil_i64, lbLT_lil_u64, lbLT_lil_f32, lbLT_lil_f64,

lbLT_ill_i32, lbLT_ill_u32, lbLT_ill_i64, lbLT_ill_u64, lbLT_ill_f32, lbLT_ill_f64,
lbLT_iil_i32, lbLT_iil_u32, lbLT_iil_i64, lbLT_iil_u64, lbLT_iil_f32, lbLT_iil_f64,

lbGT_lll_i32, lbGT_lll_u32, lbGT_lll_i64, lbGT_lll_u64, lbGT_lll_f32, lbGT_lll_f64,
lbGT_lil_i32, lbGT_lil_u32, lbGT_lil_i64, lbGT_lil_u64, lbGT_lil_f32, lbGT_lil_f64,

lbGT_ill_i32, lbGT_ill_u32, lbGT_ill_i64, lbGT_ill_u64, lbGT_ill_f32, lbGT_ill_f64,
lbGT_iil_i32, lbGT_iil_u32, lbGT_iil_i64, lbGT_iil_u64, lbGT_iil_f32, lbGT_iil_f64,

lbGTE_lll_i32, lbGTE_lll_u32, lbGTE_lll_i64, lbGTE_lll_u64, lbGTE_lll_f32, lbGTE_lll_f64,
lbGTE_lil_i32, lbGTE_lil_u32, lbGTE_lil_i64, lbGTE_lil_u64, lbGTE_lil_f32, lbGTE_lil_f64,

lbGTE_ill_i32, lbGTE_ill_u32, lbGTE_ill_i64, lbGTE_ill_u64, lbGTE_ill_f32, lbGTE_ill_f64,
lbGTE_iil_i32, lbGTE_iil_u32, lbGTE_iil_i64, lbGTE_iil_u64, lbGTE_iil_f32, lbGTE_iil_f64,

lbLTE_lll_i32, lbLTE_lll_u32, lbLTE_lll_i64, lbLTE_lll_u64, lbLTE_lll_f32, lbLTE_lll_f64,
lbLTE_lil_i32, lbLTE_lil_u32, lbLTE_lil_i64, lbLTE_lil_u64, lbLTE_lil_f32, lbLTE_lil_f64,

lbLTE_ill_i32, lbLTE_ill_u32, lbLTE_ill_i64, lbLTE_ill_u64, lbLTE_ill_f32, lbLTE_ill_f64,
lbLTE_iil_i32, lbLTE_iil_u32, lbLTE_iil_i64, lbLTE_iil_u64, lbLTE_iil_f32, lbLTE_iil_f64,

lbBND_lll_i32, lbBND_lll_u32, lbBND_lll_i64, lbBND_lll_u64,
lbBND_lil_i32, lbBND_lil_u32, lbBND_lil_i64, lbBND_lil_u64,

lbBND_ill_i32, lbBND_ill_u32, lbBND_ill_i64, lbBND_ill_u64,
lbBND_iil_i32, lbBND_iil_u32, lbBND_iil_i64, lbBND_iil_u64,

lbSHL_lll_i32, lbSHL_lll_u32, lbSHL_lll_i64, lbSHL_lll_u64,
lbSHL_lil_i32, lbSHL_lil_u32, lbSHL_lil_i64, lbSHL_lil_u64,

lbSHL_ill_i32, lbSHL_ill_u32, lbSHL_ill_i64, lbSHL_ill_u64,
lbSHL_iil_i32, lbSHL_iil_u32, lbSHL_iil_i64, lbSHL_iil_u64,

lbSHR_lll_i32, lbSHR_lll_u32, lbSHR_lll_i64, lbSHR_lll_u64,
lbSHR_lil_i32, lbSHR_lil_u32, lbSHR_lil_i64, lbSHR_lil_u64,

lbSHR_ill_i32, lbSHR_ill_u32, lbSHR_ill_i64, lbSHR_ill_u64,
lbSHR_iil_i32, lbSHR_iil_u32, lbSHR_iil_i64, lbSHR_iil_u64,

lbXOR_lll_i32, lbXOR_lll_u32, lbXOR_lll_i64, lbXOR_lll_u64,
lbXOR_lil_i32, lbXOR_lil_u32, lbXOR_lil_i64, lbXOR_lil_u64,

lbXOR_ill_i32, lbXOR_ill_u32, lbXOR_ill_i64, lbXOR_ill_u64,
lbXOR_iil_i32, lbXOR_iil_u32, lbXOR_iil_i64, lbXOR_iil_u64,

lbBOR_lll_i32, lbBOR_lll_u32, lbBOR_lll_i64, lbBOR_lll_u64,
lbBOR_lil_i32, lbBOR_lil_u32, lbBOR_lil_i64, lbBOR_lil_u64,

lbBOR_ill_i32, lbBOR_ill_u32, lbBOR_ill_i64, lbBOR_ill_u64,
lbBOR_iil_i32, lbBOR_iil_u32, lbBOR_iil_i64, lbBOR_iil_u64,

lbSAR_lll_i32, lbSAR_lll_u32, lbSAR_lll_i64, lbSAR_lll_u64,
lbSAR_lil_i32, lbSAR_lil_u32, lbSAR_lil_i64, lbSAR_lil_u64,

lbSAR_ill_i32, lbSAR_ill_u32, lbSAR_ill_i64, lbSAR_ill_u64,
lbSAR_iil_i32, lbSAR_iil_u32, lbSAR_iil_i64, lbSAR_iil_u64,



// --- MOV Operations (Source, Destination, SourceType, DestType) ---
// Destination is ALWAYS Local ('l') for these specialized opcodes.
// Source can be Local ('l') or Immediate ('i')
// Format: lbMOV_SourceType_DestType_SourcePos_DestPos
// Example: lbMOV_i8_u16_li (Move Int8 Immediate to UInt16 Local)
lbMOV, //keep here for copy ref

// --- Integer Source Types (i8, u8, i16, u16, i32, u32, i64, u64) ---
// --- Destination Types (i8, u8, i16, u16, i32, u32, i64, u64) ---

// Source: i8
lbMOV_i8_i8_ll, lbMOV_i8_i8_li,
lbMOV_i8_u8_ll, lbMOV_i8_u8_li,
lbMOV_i8_i16_ll, lbMOV_i8_i16_li,
lbMOV_i8_u16_ll, lbMOV_i8_u16_li,
lbMOV_i8_i32_ll, lbMOV_i8_i32_li,
lbMOV_i8_u32_ll, lbMOV_i8_u32_li,
lbMOV_i8_i64_ll, lbMOV_i8_i64_li,
lbMOV_i8_u64_ll, lbMOV_i8_u64_li,

// Source: u8
lbMOV_u8_i8_ll, lbMOV_u8_i8_li,
lbMOV_u8_u8_ll, lbMOV_u8_u8_li,
lbMOV_u8_i16_ll, lbMOV_u8_i16_li,
lbMOV_u8_u16_ll, lbMOV_u8_u16_li,
lbMOV_u8_i32_ll, lbMOV_u8_i32_li,
lbMOV_u8_u32_ll, lbMOV_u8_u32_li,
lbMOV_u8_i64_ll, lbMOV_u8_i64_li,
lbMOV_u8_u64_ll, lbMOV_u8_u64_li,

// Source: i16
lbMOV_i16_i8_ll, lbMOV_i16_i8_li,
lbMOV_i16_u8_ll, lbMOV_i16_u8_li,
lbMOV_i16_i16_ll, lbMOV_i16_i16_li,
lbMOV_i16_u16_ll, lbMOV_i16_u16_li,
lbMOV_i16_i32_ll, lbMOV_i16_i32_li,
lbMOV_i16_u32_ll, lbMOV_i16_u32_li,
lbMOV_i16_i64_ll, lbMOV_i16_i64_li,
lbMOV_i16_u64_ll, lbMOV_i16_u64_li,

// Source: u16
lbMOV_u16_i8_ll, lbMOV_u16_i8_li,
lbMOV_u16_u8_ll, lbMOV_u16_u8_li,
lbMOV_u16_i16_ll, lbMOV_u16_i16_li,
lbMOV_u16_u16_ll, lbMOV_u16_u16_li,
lbMOV_u16_i32_ll, lbMOV_u16_i32_li,
lbMOV_u16_u32_ll, lbMOV_u16_u32_li,
lbMOV_u16_i64_ll, lbMOV_u16_i64_li,
lbMOV_u16_u64_ll, lbMOV_u16_u64_li,

// Source: i32
lbMOV_i32_i8_ll, lbMOV_i32_i8_li,
lbMOV_i32_u8_ll, lbMOV_i32_u8_li,
lbMOV_i32_i16_ll, lbMOV_i32_i16_li,
lbMOV_i32_u16_ll, lbMOV_i32_u16_li,
lbMOV_i32_i32_ll, lbMOV_i32_i32_li,
lbMOV_i32_u32_ll, lbMOV_i32_u32_li,
lbMOV_i32_i64_ll, lbMOV_i32_i64_li,
lbMOV_i32_u64_ll, lbMOV_i32_u64_li,

// Source: u32
lbMOV_u32_i8_ll, lbMOV_u32_i8_li,
lbMOV_u32_u8_ll, lbMOV_u32_u8_li,
lbMOV_u32_i16_ll, lbMOV_u32_i16_li,
lbMOV_u32_u16_ll, lbMOV_u32_u16_li,
lbMOV_u32_i32_ll, lbMOV_u32_i32_li,
lbMOV_u32_u32_ll, lbMOV_u32_u32_li,
lbMOV_u32_i64_ll, lbMOV_u32_i64_li,
lbMOV_u32_u64_ll, lbMOV_u32_u64_li,

// Source: i64
lbMOV_i64_i8_ll, lbMOV_i64_i8_li,
lbMOV_i64_u8_ll, lbMOV_i64_u8_li,
lbMOV_i64_i16_ll, lbMOV_i64_i16_li,
lbMOV_i64_u16_ll, lbMOV_i64_u16_li,
lbMOV_i64_i32_ll, lbMOV_i64_i32_li,
lbMOV_i64_u32_ll, lbMOV_i64_u32_li,
lbMOV_i64_i64_ll, lbMOV_i64_i64_li,
lbMOV_i64_u64_ll, lbMOV_i64_u64_li,

// Source: u64
lbMOV_u64_i8_ll, lbMOV_u64_i8_li,
lbMOV_u64_u8_ll, lbMOV_u64_u8_li,
lbMOV_u64_i16_ll, lbMOV_u64_i16_li,
lbMOV_u64_u16_ll, lbMOV_u64_u16_li,
lbMOV_u64_i32_ll, lbMOV_u64_i32_li,
lbMOV_u64_u32_ll, lbMOV_u64_u32_li,
lbMOV_u64_i64_ll, lbMOV_u64_i64_li,
lbMOV_u64_u64_ll, lbMOV_u64_u64_li,

// --- Float Source Types (f32, f64) ---
// --- Destination Types (i8, u8, i16, u16, i32, u32, i64, u64, f32, f64) ---

// Source: f32
lbMOV_f32_i8_ll, lbMOV_f32_i8_li,
lbMOV_f32_u8_ll, lbMOV_f32_u8_li,
lbMOV_f32_i16_ll, lbMOV_f32_i16_li,
lbMOV_f32_u16_ll, lbMOV_f32_u16_li,
lbMOV_f32_i32_ll, lbMOV_f32_i32_li,
lbMOV_f32_u32_ll, lbMOV_f32_u32_li,
lbMOV_f32_i64_ll, lbMOV_f32_i64_li,
lbMOV_f32_u64_ll, lbMOV_f32_u64_li,
lbMOV_f32_f32_ll, lbMOV_f32_f32_li,
lbMOV_f32_f64_ll, lbMOV_f32_f64_li,

// Source: f64
lbMOV_f64_i8_ll, lbMOV_f64_i8_li,
lbMOV_f64_u8_ll, lbMOV_f64_u8_li,
lbMOV_f64_i16_ll, lbMOV_f64_i16_li,
lbMOV_f64_u16_ll, lbMOV_f64_u16_li,
lbMOV_f64_i32_ll, lbMOV_f64_i32_li,
lbMOV_f64_u32_ll, lbMOV_f64_u32_li,
lbMOV_f64_i64_ll, lbMOV_f64_i64_li,
lbMOV_f64_u64_ll, lbMOV_f64_u64_li,
lbMOV_f64_f32_ll, lbMOV_f64_f32_li,
lbMOV_f64_f64_ll, lbMOV_f64_f64_li,

// --- MOVH Operations (Source, Destination Pointer, SourceType, DestType) ---
// Destination is ALWAYS Local reference ('l') for these specialized opcodes.
// Source can be Local ('l'), Immediate ('i'), or Global ('g').
// Format: lbMOVH_SourceType_DestType_SourcePos_DestPos
// Example: lbMOVH_i8_u16_li (move Int8 Immediate to PUInt16 Local)
lbMOVH,

// --- Integer Source Types (i8, u8, i16, u16, i32, u32, i64, u64) ---
// --- Destination Types Pointer(i8, u8, i16, u16, i32, u32, i64, u64) ---

// Source: i8
lbMOVH_i8_i8_ll, lbMOVH_i8_i8_li,
lbMOVH_i8_u8_ll, lbMOVH_i8_u8_li,
lbMOVH_i8_i16_ll, lbMOVH_i8_i16_li,
lbMOVH_i8_u16_ll, lbMOVH_i8_u16_li,
lbMOVH_i8_i32_ll, lbMOVH_i8_i32_li,
lbMOVH_i8_u32_ll, lbMOVH_i8_u32_li,
lbMOVH_i8_i64_ll, lbMOVH_i8_i64_li,
lbMOVH_i8_u64_ll, lbMOVH_i8_u64_li,

// Source: u8
lbMOVH_u8_i8_ll, lbMOVH_u8_i8_li,
lbMOVH_u8_u8_ll, lbMOVH_u8_u8_li,
lbMOVH_u8_i16_ll, lbMOVH_u8_i16_li,
lbMOVH_u8_u16_ll, lbMOVH_u8_u16_li,
lbMOVH_u8_i32_ll, lbMOVH_u8_i32_li,
lbMOVH_u8_u32_ll, lbMOVH_u8_u32_li,
lbMOVH_u8_i64_ll, lbMOVH_u8_i64_li,
lbMOVH_u8_u64_ll, lbMOVH_u8_u64_li,

// Source: i16
lbMOVH_i16_i8_ll, lbMOVH_i16_i8_li,
lbMOVH_i16_u8_ll, lbMOVH_i16_u8_li,
lbMOVH_i16_i16_ll, lbMOVH_i16_i16_li,
lbMOVH_i16_u16_ll, lbMOVH_i16_u16_li,
lbMOVH_i16_i32_ll, lbMOVH_i16_i32_li,
lbMOVH_i16_u32_ll, lbMOVH_i16_u32_li,
lbMOVH_i16_i64_ll, lbMOVH_i16_i64_li,
lbMOVH_i16_u64_ll, lbMOVH_i16_u64_li,

// Source: u16
lbMOVH_u16_i8_ll, lbMOVH_u16_i8_li,
lbMOVH_u16_u8_ll, lbMOVH_u16_u8_li,
lbMOVH_u16_i16_ll, lbMOVH_u16_i16_li,
lbMOVH_u16_u16_ll, lbMOVH_u16_u16_li,
lbMOVH_u16_i32_ll, lbMOVH_u16_i32_li,
lbMOVH_u16_u32_ll, lbMOVH_u16_u32_li,
lbMOVH_u16_i64_ll, lbMOVH_u16_i64_li,
lbMOVH_u16_u64_ll, lbMOVH_u16_u64_li,

// Source: i32
lbMOVH_i32_i8_ll, lbMOVH_i32_i8_li,
lbMOVH_i32_u8_ll, lbMOVH_i32_u8_li,
lbMOVH_i32_i16_ll, lbMOVH_i32_i16_li,
lbMOVH_i32_u16_ll, lbMOVH_i32_u16_li,
lbMOVH_i32_i32_ll, lbMOVH_i32_i32_li,
lbMOVH_i32_u32_ll, lbMOVH_i32_u32_li,
lbMOVH_i32_i64_ll, lbMOVH_i32_i64_li,
lbMOVH_i32_u64_ll, lbMOVH_i32_u64_li,

// Source: u32
lbMOVH_u32_i8_ll, lbMOVH_u32_i8_li,
lbMOVH_u32_u8_ll, lbMOVH_u32_u8_li,
lbMOVH_u32_i16_ll, lbMOVH_u32_i16_li,
lbMOVH_u32_u16_ll, lbMOVH_u32_u16_li,
lbMOVH_u32_i32_ll, lbMOVH_u32_i32_li,
lbMOVH_u32_u32_ll, lbMOVH_u32_u32_li,
lbMOVH_u32_i64_ll, lbMOVH_u32_i64_li,
lbMOVH_u32_u64_ll, lbMOVH_u32_u64_li,

// Source: i64
lbMOVH_i64_i8_ll, lbMOVH_i64_i8_li,
lbMOVH_i64_u8_ll, lbMOVH_i64_u8_li,
lbMOVH_i64_i16_ll, lbMOVH_i64_i16_li,
lbMOVH_i64_u16_ll, lbMOVH_i64_u16_li,
lbMOVH_i64_i32_ll, lbMOVH_i64_i32_li,
lbMOVH_i64_u32_ll, lbMOVH_i64_u32_li,
lbMOVH_i64_i64_ll, lbMOVH_i64_i64_li,
lbMOVH_i64_u64_ll, lbMOVH_i64_u64_li,

// Source: u64
lbMOVH_u64_i8_ll, lbMOVH_u64_i8_li,
lbMOVH_u64_u8_ll, lbMOVH_u64_u8_li,
lbMOVH_u64_i16_ll, lbMOVH_u64_i16_li,
lbMOVH_u64_u16_ll, lbMOVH_u64_u16_li,
lbMOVH_u64_i32_ll, lbMOVH_u64_i32_li,
lbMOVH_u64_u32_ll, lbMOVH_u64_u32_li,
lbMOVH_u64_i64_ll, lbMOVH_u64_i64_li,
lbMOVH_u64_u64_ll, lbMOVH_u64_u64_li,

// --- Float Source Types (f32, f64) ---
// --- Destination Types (i8, u8, i16, u16, i32, u32, i64, u64, f32, f64) ---

// Source: f32
lbMOVH_f32_i8_ll, lbMOVH_f32_i8_li,
lbMOVH_f32_u8_ll, lbMOVH_f32_u8_li,
lbMOVH_f32_i16_ll, lbMOVH_f32_i16_li,
lbMOVH_f32_u16_ll, lbMOVH_f32_u16_li,
lbMOVH_f32_i32_ll, lbMOVH_f32_i32_li,
lbMOVH_f32_u32_ll, lbMOVH_f32_u32_li,
lbMOVH_f32_i64_ll, lbMOVH_f32_i64_li,
lbMOVH_f32_u64_ll, lbMOVH_f32_u64_li,
lbMOVH_f32_f32_ll, lbMOVH_f32_f32_li,
lbMOVH_f32_f64_ll, lbMOVH_f32_f64_li,

// Source: f64
lbMOVH_f64_i8_ll, lbMOVH_f64_i8_li,
lbMOVH_f64_u8_ll, lbMOVH_f64_u8_li,
lbMOVH_f64_i16_ll, lbMOVH_f64_i16_li,
lbMOVH_f64_u16_ll, lbMOVH_f64_u16_li,
lbMOVH_f64_i32_ll, lbMOVH_f64_i32_li,
lbMOVH_f64_u32_ll, lbMOVH_f64_u32_li,
lbMOVH_f64_i64_ll, lbMOVH_f64_i64_li,
lbMOVH_f64_u64_ll, lbMOVH_f64_u64_li,
lbMOVH_f64_f32_ll, lbMOVH_f64_f32_li,
lbMOVH_f64_f64_ll, lbMOVH_f64_f64_li,

lbERROR;
