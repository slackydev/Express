unit xpr.Bytecode;
{
  Author: Jarl K. Holta  
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
{$I header.inc}

interface

uses 
  Classes, SysUtils,
  xpr.Types,
  xpr.Tokenizer,
  xpr.Intermediate;

type
  EBytecode = (
    bcNOOP,
    bcSUPER, bcHOTLOOP,

    // flow control
    bcJMP, bcRELJMP,
    bcJZ,  bcJNZ, bcJZ_i, bcJNZ_i,
    
    // dynamic objects
    bcNEW,
    bcRELEASE,
    bcDYNCAST,
    bcIS,

    // functions
    bcNEWFRAME,
    
    bcPUSH, bcPUSHREF,
    bcPUSH_FP,
    bcPOP, bcRPOP,
    bcPOPH, bcPOPPtr, {same op, both are pop no deref, merge plz}

    bcLOAD_GLOBAL, bcLOAD_NONLOCAL, bcCOPY_GLOBAL,

    bcPRT, bcPRTi, bcPRTf, bcPRTb,
    bcINVOKE, bcINVOKEX, bcINVOKE_VIRTUAL,
    bcRET,

    // fill byte
    bcFILL,

    // try-except
    bcSET_ERRHANDLER,
    bcRAISE,
    bcGET_EXCEPTION,
    bcIncTry,
    bcDecTry,

    // array managment
    bcREFCNT, bcREFCNT_imm,
    bcINCLOCK, bcDECLOCK,
    bcBCHK,

    // handling string table to var assignment, and basic string operators
    // bs is a byte string, us is a unicode string
    bcLOAD_STR, bcLOAD_USTR,
    bcADD_STR, bcADD_USTR,
    bcCh2Str,

    bcADDR,

    // ----- WARNING -------------------------------------------
    // ORDER BEYOND HERE WILL AFFECT INTERPRETER-CODE-INLINING
    // ---- DONT TOUCH -----------------------------------------
    bcINC_i32, bcINC_u32, bcINC_i64, bcINC_u64,

    // specialized ops
    bcFMA_i8, bcFMA_u8, bcFMA_i16, bcFMA_u16, bcFMA_i32, bcFMA_u32, bcFMA_i64, bcFMA_u64,
    bcFMA_imm_i8, bcFMA_imm_u8, bcFMA_imm_i16, bcFMA_imm_u16, bcFMA_imm_i32, bcFMA_imm_u32, bcFMA_imm_i64, bcFMA_imm_u64,

    bcDREF, bcDREF_32, bcDREF_64,

    // fusion
    bcFMAD_d64_64, bcFMAD_d64_32, bcFMAD_d32_64, bcFMAD_d32_32,

    // slowpath
    bcADD, bcSUB, bcMUL, bcDIV, bcMOD, bcPOW,
    bcEQ,  bcNE,  bcLT,  bcGT,  bcGTE, bcLTE,
    bcBND, bcSHL, bcSHR, bcXOR, bcBOR, bcSAR,

    // Specialize for local dest
    bcADD_lll_i32, bcADD_lll_u32, bcADD_lll_i64, bcADD_lll_u64, bcADD_lll_f32, bcADD_lll_f64,
    bcADD_lil_i32, bcADD_lil_u32, bcADD_lil_i64, bcADD_lil_u64, bcADD_lil_f32, bcADD_lil_f64,

    bcADD_ill_i32, bcADD_ill_u32, bcADD_ill_i64, bcADD_ill_u64, bcADD_ill_f32, bcADD_ill_f64,
    bcADD_iil_i32, bcADD_iil_u32, bcADD_iil_i64, bcADD_iil_u64, bcADD_iil_f32, bcADD_iil_f64,

    bcSUB_lll_i32, bcSUB_lll_u32, bcSUB_lll_i64, bcSUB_lll_u64, bcSUB_lll_f32, bcSUB_lll_f64,
    bcSUB_lil_i32, bcSUB_lil_u32, bcSUB_lil_i64, bcSUB_lil_u64, bcSUB_lil_f32, bcSUB_lil_f64,

    bcSUB_ill_i32, bcSUB_ill_u32, bcSUB_ill_i64, bcSUB_ill_u64, bcSUB_ill_f32, bcSUB_ill_f64,
    bcSUB_iil_i32, bcSUB_iil_u32, bcSUB_iil_i64, bcSUB_iil_u64, bcSUB_iil_f32, bcSUB_iil_f64,

    bcMUL_lll_i32, bcMUL_lll_u32, bcMUL_lll_i64, bcMUL_lll_u64, bcMUL_lll_f32, bcMUL_lll_f64,
    bcMUL_lil_i32, bcMUL_lil_u32, bcMUL_lil_i64, bcMUL_lil_u64, bcMUL_lil_f32, bcMUL_lil_f64,

    bcMUL_ill_i32, bcMUL_ill_u32, bcMUL_ill_i64, bcMUL_ill_u64, bcMUL_ill_f32, bcMUL_ill_f64,
    bcMUL_iil_i32, bcMUL_iil_u32, bcMUL_iil_i64, bcMUL_iil_u64, bcMUL_iil_f32, bcMUL_iil_f64,

    bcDIV_lll_i32, bcDIV_lll_u32, bcDIV_lll_i64, bcDIV_lll_u64, bcDIV_lll_f32, bcDIV_lll_f64,
    bcDIV_lil_i32, bcDIV_lil_u32, bcDIV_lil_i64, bcDIV_lil_u64, bcDIV_lil_f32, bcDIV_lil_f64,

    bcDIV_ill_i32, bcDIV_ill_u32, bcDIV_ill_i64, bcDIV_ill_u64, bcDIV_ill_f32, bcDIV_ill_f64,
    bcDIV_iil_i32, bcDIV_iil_u32, bcDIV_iil_i64, bcDIV_iil_u64, bcDIV_iil_f32, bcDIV_iil_f64,

    bcMOD_lll_i32, bcMOD_lll_u32, bcMOD_lll_i64, bcMOD_lll_u64, bcMOD_lll_f32, bcMOD_lll_f64,
    bcMOD_lil_i32, bcMOD_lil_u32, bcMOD_lil_i64, bcMOD_lil_u64, bcMOD_lil_f32, bcMOD_lil_f64,

    bcMOD_ill_i32, bcMOD_ill_u32, bcMOD_ill_i64, bcMOD_ill_u64, bcMOD_ill_f32, bcMOD_ill_f64,
    bcMOD_iil_i32, bcMOD_iil_u32, bcMOD_iil_i64, bcMOD_iil_u64, bcMOD_iil_f32, bcMOD_iil_f64,

    bcPOW_lll_i32, bcPOW_lll_u32, bcPOW_lll_i64, bcPOW_lll_u64, bcPOW_lll_f32, bcPOW_lll_f64,
    bcPOW_lil_i32, bcPOW_lil_u32, bcPOW_lil_i64, bcPOW_lil_u64, bcPOW_lil_f32, bcPOW_lil_f64,

    bcPOW_ill_i32, bcPOW_ill_u32, bcPOW_ill_i64, bcPOW_ill_u64, bcPOW_ill_f32, bcPOW_ill_f64,
    bcPOW_iil_i32, bcPOW_iil_u32, bcPOW_iil_i64, bcPOW_iil_u64, bcPOW_iil_f32, bcPOW_iil_f64,

    bcEQ_lll_i32, bcEQ_lll_u32, bcEQ_lll_i64, bcEQ_lll_u64, bcEQ_lll_f32, bcEQ_lll_f64,
    bcEQ_lil_i32, bcEQ_lil_u32, bcEQ_lil_i64, bcEQ_lil_u64, bcEQ_lil_f32, bcEQ_lil_f64,

    bcEQ_ill_i32, bcEQ_ill_u32, bcEQ_ill_i64, bcEQ_ill_u64, bcEQ_ill_f32, bcEQ_ill_f64,
    bcEQ_iil_i32, bcEQ_iil_u32, bcEQ_iil_i64, bcEQ_iil_u64, bcEQ_iil_f32, bcEQ_iil_f64,

    bcNE_lll_i32, bcNE_lll_u32, bcNE_lll_i64, bcNE_lll_u64, bcNE_lll_f32, bcNE_lll_f64,
    bcNE_lil_i32, bcNE_lil_u32, bcNE_lil_i64, bcNE_lil_u64, bcNE_lil_f32, bcNE_lil_f64,

    bcNE_ill_i32, bcNE_ill_u32, bcNE_ill_i64, bcNE_ill_u64, bcNE_ill_f32, bcNE_ill_f64,
    bcNE_iil_i32, bcNE_iil_u32, bcNE_iil_i64, bcNE_iil_u64, bcNE_iil_f32, bcNE_iil_f64,

    bcLT_lll_i32, bcLT_lll_u32, bcLT_lll_i64, bcLT_lll_u64, bcLT_lll_f32, bcLT_lll_f64,
    bcLT_lil_i32, bcLT_lil_u32, bcLT_lil_i64, bcLT_lil_u64, bcLT_lil_f32, bcLT_lil_f64,

    bcLT_ill_i32, bcLT_ill_u32, bcLT_ill_i64, bcLT_ill_u64, bcLT_ill_f32, bcLT_ill_f64,
    bcLT_iil_i32, bcLT_iil_u32, bcLT_iil_i64, bcLT_iil_u64, bcLT_iil_f32, bcLT_iil_f64,

    bcGT_lll_i32, bcGT_lll_u32, bcGT_lll_i64, bcGT_lll_u64, bcGT_lll_f32, bcGT_lll_f64,
    bcGT_lil_i32, bcGT_lil_u32, bcGT_lil_i64, bcGT_lil_u64, bcGT_lil_f32, bcGT_lil_f64,

    bcGT_ill_i32, bcGT_ill_u32, bcGT_ill_i64, bcGT_ill_u64, bcGT_ill_f32, bcGT_ill_f64,
    bcGT_iil_i32, bcGT_iil_u32, bcGT_iil_i64, bcGT_iil_u64, bcGT_iil_f32, bcGT_iil_f64,

    bcGTE_lll_i32, bcGTE_lll_u32, bcGTE_lll_i64, bcGTE_lll_u64, bcGTE_lll_f32, bcGTE_lll_f64,
    bcGTE_lil_i32, bcGTE_lil_u32, bcGTE_lil_i64, bcGTE_lil_u64, bcGTE_lil_f32, bcGTE_lil_f64,

    bcGTE_ill_i32, bcGTE_ill_u32, bcGTE_ill_i64, bcGTE_ill_u64, bcGTE_ill_f32, bcGTE_ill_f64,
    bcGTE_iil_i32, bcGTE_iil_u32, bcGTE_iil_i64, bcGTE_iil_u64, bcGTE_iil_f32, bcGTE_iil_f64,

    bcLTE_lll_i32, bcLTE_lll_u32, bcLTE_lll_i64, bcLTE_lll_u64, bcLTE_lll_f32, bcLTE_lll_f64,
    bcLTE_lil_i32, bcLTE_lil_u32, bcLTE_lil_i64, bcLTE_lil_u64, bcLTE_lil_f32, bcLTE_lil_f64,

    bcLTE_ill_i32, bcLTE_ill_u32, bcLTE_ill_i64, bcLTE_ill_u64, bcLTE_ill_f32, bcLTE_ill_f64,
    bcLTE_iil_i32, bcLTE_iil_u32, bcLTE_iil_i64, bcLTE_iil_u64, bcLTE_iil_f32, bcLTE_iil_f64,

    bcBND_lll_i32, bcBND_lll_u32, bcBND_lll_i64, bcBND_lll_u64,
    bcBND_lil_i32, bcBND_lil_u32, bcBND_lil_i64, bcBND_lil_u64,

    bcBND_ill_i32, bcBND_ill_u32, bcBND_ill_i64, bcBND_ill_u64,
    bcBND_iil_i32, bcBND_iil_u32, bcBND_iil_i64, bcBND_iil_u64,

    bcSHL_lll_i32, bcSHL_lll_u32, bcSHL_lll_i64, bcSHL_lll_u64,
    bcSHL_lil_i32, bcSHL_lil_u32, bcSHL_lil_i64, bcSHL_lil_u64,

    bcSHL_ill_i32, bcSHL_ill_u32, bcSHL_ill_i64, bcSHL_ill_u64,
    bcSHL_iil_i32, bcSHL_iil_u32, bcSHL_iil_i64, bcSHL_iil_u64,

    bcSHR_lll_i32, bcSHR_lll_u32, bcSHR_lll_i64, bcSHR_lll_u64,
    bcSHR_lil_i32, bcSHR_lil_u32, bcSHR_lil_i64, bcSHR_lil_u64,

    bcSHR_ill_i32, bcSHR_ill_u32, bcSHR_ill_i64, bcSHR_ill_u64,
    bcSHR_iil_i32, bcSHR_iil_u32, bcSHR_iil_i64, bcSHR_iil_u64,

    bcXOR_lll_i32, bcXOR_lll_u32, bcXOR_lll_i64, bcXOR_lll_u64,
    bcXOR_lil_i32, bcXOR_lil_u32, bcXOR_lil_i64, bcXOR_lil_u64,

    bcXOR_ill_i32, bcXOR_ill_u32, bcXOR_ill_i64, bcXOR_ill_u64,
    bcXOR_iil_i32, bcXOR_iil_u32, bcXOR_iil_i64, bcXOR_iil_u64,

    bcBOR_lll_i32, bcBOR_lll_u32, bcBOR_lll_i64, bcBOR_lll_u64,
    bcBOR_lil_i32, bcBOR_lil_u32, bcBOR_lil_i64, bcBOR_lil_u64,

    bcBOR_ill_i32, bcBOR_ill_u32, bcBOR_ill_i64, bcBOR_ill_u64,
    bcBOR_iil_i32, bcBOR_iil_u32, bcBOR_iil_i64, bcBOR_iil_u64,

    bcSAR_lll_i32, bcSAR_lll_u32, bcSAR_lll_i64, bcSAR_lll_u64,
    bcSAR_lil_i32, bcSAR_lil_u32, bcSAR_lil_i64, bcSAR_lil_u64,

    bcSAR_ill_i32, bcSAR_ill_u32, bcSAR_ill_i64, bcSAR_ill_u64,
    bcSAR_iil_i32, bcSAR_iil_u32, bcSAR_iil_i64, bcSAR_iil_u64,



    // --- MOV Operations (Source, Destination, SourceType, DestType) ---
    // Destination is ALWAYS Local ('l') for these specialized opcodes.
    // Source can be Local ('l') or Immediate ('i')
    // Format: bcMOV_SourceType_DestType_SourcePos_DestPos
    // Example: bcMOV_i8_u16_li (Move Int8 Immediate to UInt16 Local)
    bcMOV,

    // --- Integer Source Types (i8, u8, i16, u16, i32, u32, i64, u64) ---
    // --- Destination Types (i8, u8, i16, u16, i32, u32, i64, u64) ---

    // Source: i8
    bcMOV_i8_i8_ll, bcMOV_i8_i8_li,
    bcMOV_i8_u8_ll, bcMOV_i8_u8_li,
    bcMOV_i8_i16_ll, bcMOV_i8_i16_li,
    bcMOV_i8_u16_ll, bcMOV_i8_u16_li,
    bcMOV_i8_i32_ll, bcMOV_i8_i32_li,
    bcMOV_i8_u32_ll, bcMOV_i8_u32_li,
    bcMOV_i8_i64_ll, bcMOV_i8_i64_li,
    bcMOV_i8_u64_ll, bcMOV_i8_u64_li,

    // Source: u8
    bcMOV_u8_i8_ll, bcMOV_u8_i8_li,
    bcMOV_u8_u8_ll, bcMOV_u8_u8_li,
    bcMOV_u8_i16_ll, bcMOV_u8_i16_li,
    bcMOV_u8_u16_ll, bcMOV_u8_u16_li,
    bcMOV_u8_i32_ll, bcMOV_u8_i32_li,
    bcMOV_u8_u32_ll, bcMOV_u8_u32_li,
    bcMOV_u8_i64_ll, bcMOV_u8_i64_li,
    bcMOV_u8_u64_ll, bcMOV_u8_u64_li,

    // Source: i16
    bcMOV_i16_i8_ll, bcMOV_i16_i8_li,
    bcMOV_i16_u8_ll, bcMOV_i16_u8_li,
    bcMOV_i16_i16_ll, bcMOV_i16_i16_li,
    bcMOV_i16_u16_ll, bcMOV_i16_u16_li,
    bcMOV_i16_i32_ll, bcMOV_i16_i32_li,
    bcMOV_i16_u32_ll, bcMOV_i16_u32_li,
    bcMOV_i16_i64_ll, bcMOV_i16_i64_li,
    bcMOV_i16_u64_ll, bcMOV_i16_u64_li,

    // Source: u16
    bcMOV_u16_i8_ll, bcMOV_u16_i8_li,
    bcMOV_u16_u8_ll, bcMOV_u16_u8_li,
    bcMOV_u16_i16_ll, bcMOV_u16_i16_li,
    bcMOV_u16_u16_ll, bcMOV_u16_u16_li,
    bcMOV_u16_i32_ll, bcMOV_u16_i32_li,
    bcMOV_u16_u32_ll, bcMOV_u16_u32_li,
    bcMOV_u16_i64_ll, bcMOV_u16_i64_li,
    bcMOV_u16_u64_ll, bcMOV_u16_u64_li,

    // Source: i32
    bcMOV_i32_i8_ll, bcMOV_i32_i8_li,
    bcMOV_i32_u8_ll, bcMOV_i32_u8_li,
    bcMOV_i32_i16_ll, bcMOV_i32_i16_li,
    bcMOV_i32_u16_ll, bcMOV_i32_u16_li,
    bcMOV_i32_i32_ll, bcMOV_i32_i32_li,
    bcMOV_i32_u32_ll, bcMOV_i32_u32_li,
    bcMOV_i32_i64_ll, bcMOV_i32_i64_li,
    bcMOV_i32_u64_ll, bcMOV_i32_u64_li,

    // Source: u32
    bcMOV_u32_i8_ll, bcMOV_u32_i8_li,
    bcMOV_u32_u8_ll, bcMOV_u32_u8_li,
    bcMOV_u32_i16_ll, bcMOV_u32_i16_li,
    bcMOV_u32_u16_ll, bcMOV_u32_u16_li,
    bcMOV_u32_i32_ll, bcMOV_u32_i32_li,
    bcMOV_u32_u32_ll, bcMOV_u32_u32_li,
    bcMOV_u32_i64_ll, bcMOV_u32_i64_li,
    bcMOV_u32_u64_ll, bcMOV_u32_u64_li,

    // Source: i64
    bcMOV_i64_i8_ll, bcMOV_i64_i8_li,
    bcMOV_i64_u8_ll, bcMOV_i64_u8_li,
    bcMOV_i64_i16_ll, bcMOV_i64_i16_li,
    bcMOV_i64_u16_ll, bcMOV_i64_u16_li,
    bcMOV_i64_i32_ll, bcMOV_i64_i32_li,
    bcMOV_i64_u32_ll, bcMOV_i64_u32_li,
    bcMOV_i64_i64_ll, bcMOV_i64_i64_li,
    bcMOV_i64_u64_ll, bcMOV_i64_u64_li,

    // Source: u64
    bcMOV_u64_i8_ll, bcMOV_u64_i8_li,
    bcMOV_u64_u8_ll, bcMOV_u64_u8_li,
    bcMOV_u64_i16_ll, bcMOV_u64_i16_li,
    bcMOV_u64_u16_ll, bcMOV_u64_u16_li,
    bcMOV_u64_i32_ll, bcMOV_u64_i32_li,
    bcMOV_u64_u32_ll, bcMOV_u64_u32_li,
    bcMOV_u64_i64_ll, bcMOV_u64_i64_li,
    bcMOV_u64_u64_ll, bcMOV_u64_u64_li,

    // --- Float Source Types (f32, f64) ---
    // --- Destination Types (i8, u8, i16, u16, i32, u32, i64, u64, f32, f64) ---

    // Source: f32
    bcMOV_f32_i8_ll, bcMOV_f32_i8_li,
    bcMOV_f32_u8_ll, bcMOV_f32_u8_li,
    bcMOV_f32_i16_ll, bcMOV_f32_i16_li,
    bcMOV_f32_u16_ll, bcMOV_f32_u16_li,
    bcMOV_f32_i32_ll, bcMOV_f32_i32_li,
    bcMOV_f32_u32_ll, bcMOV_f32_u32_li,
    bcMOV_f32_i64_ll, bcMOV_f32_i64_li,
    bcMOV_f32_u64_ll, bcMOV_f32_u64_li,
    bcMOV_f32_f32_ll, bcMOV_f32_f32_li,
    bcMOV_f32_f64_ll, bcMOV_f32_f64_li,

    // Source: f64
    bcMOV_f64_i8_ll, bcMOV_f64_i8_li,
    bcMOV_f64_u8_ll, bcMOV_f64_u8_li,
    bcMOV_f64_i16_ll, bcMOV_f64_i16_li,
    bcMOV_f64_u16_ll, bcMOV_f64_u16_li,
    bcMOV_f64_i32_ll, bcMOV_f64_i32_li,
    bcMOV_f64_u32_ll, bcMOV_f64_u32_li,
    bcMOV_f64_i64_ll, bcMOV_f64_i64_li,
    bcMOV_f64_u64_ll, bcMOV_f64_u64_li,
    bcMOV_f64_f32_ll, bcMOV_f64_f32_li,
    bcMOV_f64_f64_ll, bcMOV_f64_f64_li,

    // --- MOVH Operations (Source, Destination Pointer, SourceType, DestType) ---
    // Destination is ALWAYS Local reference ('l') for these specialized opcodes.
    // Source can be Local ('l'), Immediate ('i'), or Global ('g').
    // Format: bcMOVH_SourceType_DestType_SourcePos_DestPos
    // Example: bcMOVH_i8_u16_li (move Int8 Immediate to PUInt16 Local)
    bcMOVH,

    // --- Integer Source Types (i8, u8, i16, u16, i32, u32, i64, u64) ---
    // --- Destination Types Pointer(i8, u8, i16, u16, i32, u32, i64, u64) ---

    // Source: i8
    bcMOVH_i8_i8_ll, bcMOVH_i8_i8_li,
    bcMOVH_i8_u8_ll, bcMOVH_i8_u8_li,
    bcMOVH_i8_i16_ll, bcMOVH_i8_i16_li,
    bcMOVH_i8_u16_ll, bcMOVH_i8_u16_li,
    bcMOVH_i8_i32_ll, bcMOVH_i8_i32_li,
    bcMOVH_i8_u32_ll, bcMOVH_i8_u32_li,
    bcMOVH_i8_i64_ll, bcMOVH_i8_i64_li,
    bcMOVH_i8_u64_ll, bcMOVH_i8_u64_li,

    // Source: u8
    bcMOVH_u8_i8_ll, bcMOVH_u8_i8_li,
    bcMOVH_u8_u8_ll, bcMOVH_u8_u8_li,
    bcMOVH_u8_i16_ll, bcMOVH_u8_i16_li,
    bcMOVH_u8_u16_ll, bcMOVH_u8_u16_li,
    bcMOVH_u8_i32_ll, bcMOVH_u8_i32_li,
    bcMOVH_u8_u32_ll, bcMOVH_u8_u32_li,
    bcMOVH_u8_i64_ll, bcMOVH_u8_i64_li,
    bcMOVH_u8_u64_ll, bcMOVH_u8_u64_li,

    // Source: i16
    bcMOVH_i16_i8_ll, bcMOVH_i16_i8_li,
    bcMOVH_i16_u8_ll, bcMOVH_i16_u8_li,
    bcMOVH_i16_i16_ll, bcMOVH_i16_i16_li,
    bcMOVH_i16_u16_ll, bcMOVH_i16_u16_li,
    bcMOVH_i16_i32_ll, bcMOVH_i16_i32_li,
    bcMOVH_i16_u32_ll, bcMOVH_i16_u32_li,
    bcMOVH_i16_i64_ll, bcMOVH_i16_i64_li,
    bcMOVH_i16_u64_ll, bcMOVH_i16_u64_li,

    // Source: u16
    bcMOVH_u16_i8_ll, bcMOVH_u16_i8_li,
    bcMOVH_u16_u8_ll, bcMOVH_u16_u8_li,
    bcMOVH_u16_i16_ll, bcMOVH_u16_i16_li,
    bcMOVH_u16_u16_ll, bcMOVH_u16_u16_li,
    bcMOVH_u16_i32_ll, bcMOVH_u16_i32_li,
    bcMOVH_u16_u32_ll, bcMOVH_u16_u32_li,
    bcMOVH_u16_i64_ll, bcMOVH_u16_i64_li,
    bcMOVH_u16_u64_ll, bcMOVH_u16_u64_li,

    // Source: i32
    bcMOVH_i32_i8_ll, bcMOVH_i32_i8_li,
    bcMOVH_i32_u8_ll, bcMOVH_i32_u8_li,
    bcMOVH_i32_i16_ll, bcMOVH_i32_i16_li,
    bcMOVH_i32_u16_ll, bcMOVH_i32_u16_li,
    bcMOVH_i32_i32_ll, bcMOVH_i32_i32_li,
    bcMOVH_i32_u32_ll, bcMOVH_i32_u32_li,
    bcMOVH_i32_i64_ll, bcMOVH_i32_i64_li,
    bcMOVH_i32_u64_ll, bcMOVH_i32_u64_li,

    // Source: u32
    bcMOVH_u32_i8_ll, bcMOVH_u32_i8_li,
    bcMOVH_u32_u8_ll, bcMOVH_u32_u8_li,
    bcMOVH_u32_i16_ll, bcMOVH_u32_i16_li,
    bcMOVH_u32_u16_ll, bcMOVH_u32_u16_li,
    bcMOVH_u32_i32_ll, bcMOVH_u32_i32_li,
    bcMOVH_u32_u32_ll, bcMOVH_u32_u32_li,
    bcMOVH_u32_i64_ll, bcMOVH_u32_i64_li,
    bcMOVH_u32_u64_ll, bcMOVH_u32_u64_li,

    // Source: i64
    bcMOVH_i64_i8_ll, bcMOVH_i64_i8_li,
    bcMOVH_i64_u8_ll, bcMOVH_i64_u8_li,
    bcMOVH_i64_i16_ll, bcMOVH_i64_i16_li,
    bcMOVH_i64_u16_ll, bcMOVH_i64_u16_li,
    bcMOVH_i64_i32_ll, bcMOVH_i64_i32_li,
    bcMOVH_i64_u32_ll, bcMOVH_i64_u32_li,
    bcMOVH_i64_i64_ll, bcMOVH_i64_i64_li,
    bcMOVH_i64_u64_ll, bcMOVH_i64_u64_li,

    // Source: u64
    bcMOVH_u64_i8_ll, bcMOVH_u64_i8_li,
    bcMOVH_u64_u8_ll, bcMOVH_u64_u8_li,
    bcMOVH_u64_i16_ll, bcMOVH_u64_i16_li,
    bcMOVH_u64_u16_ll, bcMOVH_u64_u16_li,
    bcMOVH_u64_i32_ll, bcMOVH_u64_i32_li,
    bcMOVH_u64_u32_ll, bcMOVH_u64_u32_li,
    bcMOVH_u64_i64_ll, bcMOVH_u64_i64_li,
    bcMOVH_u64_u64_ll, bcMOVH_u64_u64_li,

    // --- Float Source Types (f32, f64) ---
    // --- Destination Types (i8, u8, i16, u16, i32, u32, i64, u64, f32, f64) ---

    // Source: f32
    bcMOVH_f32_i8_ll, bcMOVH_f32_i8_li,
    bcMOVH_f32_u8_ll, bcMOVH_f32_u8_li,
    bcMOVH_f32_i16_ll, bcMOVH_f32_i16_li,
    bcMOVH_f32_u16_ll, bcMOVH_f32_u16_li,
    bcMOVH_f32_i32_ll, bcMOVH_f32_i32_li,
    bcMOVH_f32_u32_ll, bcMOVH_f32_u32_li,
    bcMOVH_f32_i64_ll, bcMOVH_f32_i64_li,
    bcMOVH_f32_u64_ll, bcMOVH_f32_u64_li,
    bcMOVH_f32_f32_ll, bcMOVH_f32_f32_li,
    bcMOVH_f32_f64_ll, bcMOVH_f32_f64_li,

    // Source: f64
    bcMOVH_f64_i8_ll, bcMOVH_f64_i8_li,
    bcMOVH_f64_u8_ll, bcMOVH_f64_u8_li,
    bcMOVH_f64_i16_ll, bcMOVH_f64_i16_li,
    bcMOVH_f64_u16_ll, bcMOVH_f64_u16_li,
    bcMOVH_f64_i32_ll, bcMOVH_f64_i32_li,
    bcMOVH_f64_u32_ll, bcMOVH_f64_u32_li,
    bcMOVH_f64_i64_ll, bcMOVH_f64_i64_li,
    bcMOVH_f64_u64_ll, bcMOVH_f64_u64_li,
    bcMOVH_f64_f32_ll, bcMOVH_f64_f32_li,
    bcMOVH_f64_f64_ll, bcMOVH_f64_f64_li,

    bcERROR
  );


  TOperandData = record
    case Byte of
      0: (Raw: array[0..7] of Byte);
      1: (Raw32: array[0..3] of Byte);
      2: (Arg: Int64);
      3: (Addr: PtrUInt);
      4: (i32: Int32);
      5: (u8: UInt8);
  end;

  // Instruction argument metadata
  TOperand = packed record
    Data: TOperandData;
    Pos: EMemPos;
    BaseType: EExpressBaseType;
  end;

  // Intermediate instruction record (fits in one cache line)
  TBytecodeInstruction = record
    Args: array[0..4] of TOperand;
    Code: EBytecode;
    nArgs: Byte;
  end;
  PBytecodeInstruction = ^TBytecodeInstruction;

  TProgramData = specialize TArrayList<TBytecodeInstruction>;

  TBytecode = record
    Code: TProgramData;
    Docpos: TDocPosList;
    FunctionTable: TFunctionTable;
    StringTable: TStringArray;
    ClassVMTs: TVMTList;

    procedure Init();
    procedure Free();

    function Add(OP: TBytecodeInstruction; Pos: TDocPos): Int32;
    function Delete(Index: Int32): TBytecodeInstruction;

    function ToString(Colorize: Boolean = False): string;
  end;



implementation

uses math, typinfo, xpr.Utils;


procedure TBytecode.Init();
begin
  Self.Code.Init([]);
  Self.Docpos.Init([]);
end;

procedure TBytecode.Free();
begin
  Self.Code.Free();
  Self.Docpos.Free();
end;

function TBytecode.Add(OP: TBytecodeInstruction; Pos: TDocPos): Int32;
begin
  Self.DocPos.Add(Pos);
  Result := Self.Code.Add(OP);
end;

function TBytecode.Delete(Index: Int32): TBytecodeInstruction;
begin
  Self.DocPos.Delete(Index);
  Result := Self.Code.Pop(Index);
end;


function TBytecode.ToString(Colorize: Boolean = False): string;
var
  i, j: Integer;
  this: TBytecodeInstruction;
  lineStr, idxStr, opStr, argStr, posColor, posName, typeStr, valStr: string;
  TSA: TStringArray;
  isFunction: Boolean;
begin
  Result := '';
  for i := 0 to Code.High do
  begin
    this := Code.Data[i];

    if (this.Code = bcNOOP) and (this.nArgs = 1) and (this.Args[0].BaseType = xtString) then
    begin
      Result += 'Function body: '+Self.StringTable[this.Args[0].Data.Addr] + LineEnding;
      continue;
    end;

    // Format line and index
    if Colorize then
    begin
      lineStr := _LWHITE_ + 'L' + DocPos.Data[i].Line.ToString + _WHITE_;
      idxStr  := _LWHITE_ + '#' + i.ToString + _WHITE_;

      // debugging purpose coloring
      if (this.Code = bcREFCNT) then
        opStr := _LYELLOW_ + GetEnumName(TypeInfo(EBytecode), Ord(this.Code)) + _WHITE_
      else if (this.Code = bcDECLOCK) then
        opStr := _LRED_ + GetEnumName(TypeInfo(EBytecode), Ord(this.Code)) + _WHITE_
      else if (this.Code = bcINCLOCK) then
        opStr := _LGREEN_ + GetEnumName(TypeInfo(EBytecode), Ord(this.Code)) + _WHITE_
      else
        opStr := _AQUA_ + GetEnumName(TypeInfo(EBytecode), Ord(this.Code)) + _WHITE_;
    end
    else
    begin
      lineStr := 'L' + DocPos.Data[i].Line.ToString;
      idxStr  := '#' + i.ToString;
      opStr   := GetEnumName(TypeInfo(EBytecode), Ord(this.Code));
    end;

    // Build aligned argument string: pos(type:value)
    argStr := '';
    for j := 0 to this.nArgs - 1 do
    begin
      with this.Args[j] do
      begin
        // Memory position label
        case Pos of
          mpImm:    begin posName := 'imm  '; if Colorize then posColor := _PURPLE_ else posColor := ''; end;
          mpLocal:  begin posName := 'loc  '; if Colorize then posColor := _YELLOW_ else posColor := ''; end;
          mpHeap:   begin posName := 'heap '; if Colorize then posColor := _GREEN_  else posColor := ''; end;
          mpConst:  begin posName := 'const'; if Colorize then posColor := _BLUE_   else posColor := ''; end;
          else      begin posName := 'unk  '; if Colorize then posColor := _RED_    else posColor := ''; end;
        end;

        // Type and value
        typeStr := BT2SM(BaseType);
        if (this.Code = bcINVOKE) and (j = 0) and (this.Args[2].BaseType = xtString) then
        begin
          valStr := Self.StringTable[this.Args[2].Data.Addr];
          if Length(valStr) > 8 then begin SetLength(ValStr, 6); valStr += '..'; end;
        end else  begin
          valStr := IntToStr(Data.Arg);
          if Length(valStr) > 8 then begin SetLength(ValStr, 6); valStr += '..'; end;
        end;
        // Compose formatted argument string
        argStr += Format('%s%s[%-3s:%s] %s|', [posColor, posName, typeStr, valStr, _WHITE_]);
      end;
    end;

    // Final line assembly
    lineStr := Format('%-15s %-12s %-28s', [lineStr, idxStr, opStr]);

    TSA := argStr.Split('|');
    argStr := '';
    for j:=0 to High(TSA)-1 do
      argStr += '%-29s';
    argStr += '%s';

    case Length(TSA) of
      1: argStr := Format(argStr, [TSA[0]]);
      2: argStr := Format(argStr, [TSA[0], TSA[1]]);
      3: argStr := Format(argStr, [TSA[0], TSA[1], TSA[2]]);
      4: argStr := Format(argStr, [TSA[0], TSA[1], TSA[2], TSA[3]]);
      5: argStr := Format(argStr, [TSA[0], TSA[1], TSA[2], TSA[3], TSA[4]]);
      6: argStr := Format(argStr, [TSA[0], TSA[1], TSA[2], TSA[3], TSA[4], TSA[5]]);
    else argStr := '';
    end;

    Result += lineStr + argStr + LineEnding;
  end;
end;


end.
