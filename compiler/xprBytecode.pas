unit xprBytecode;
{
  Author: Jarl K. Holta  
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
{$I header.inc}
interface

uses 
  SysUtils, xprTypes, xprTokenizer, xprDictionary, xprIntermediate;

type
  EBytecode = (
    bcNOOP,
    bcPASS,

    // flow control
    bcJMP, bcRELJMP,
    bcJZ,  bcJNZ, bcJZ_g, bcJNZ_g, bcJZ_i, bcJNZ_i,

    // functions
    bcNEWFRAME,

    bcPUSH,
    bcPOP, bcRPOP,
    bcPOPH, bcPOPPtr, {same op, both are pop no deref, merge plz}

    bcPRTi, bcPRTf, bcPRTb,
    bcINVOKE, bcINVOKEX,
    bcRET,

    // specialized ops
    bcFMA_lll_i8, bcFMA_lll_u8, bcFMA_lll_i16, bcFMA_lll_u16, bcFMA_lll_i32, bcFMA_lll_u32, bcFMA_lll_i64, bcFMA_lll_u64,
    bcFMA_llg_i8, bcFMA_llg_u8, bcFMA_llg_i16, bcFMA_llg_u16, bcFMA_llg_i32, bcFMA_llg_u32, bcFMA_llg_i64, bcFMA_llg_u64,
    bcFMA_lgl_i8, bcFMA_lgl_u8, bcFMA_lgl_i16, bcFMA_lgl_u16, bcFMA_lgl_i32, bcFMA_lgl_u32, bcFMA_lgl_i64, bcFMA_lgl_u64,
    bcFMA_lgg_i8, bcFMA_lgg_u8, bcFMA_lgg_i16, bcFMA_lgg_u16, bcFMA_lgg_i32, bcFMA_lgg_u32, bcFMA_lgg_i64, bcFMA_lgg_u64,
    bcFMA_gll_i8, bcFMA_gll_u8, bcFMA_gll_i16, bcFMA_gll_u16, bcFMA_gll_i32, bcFMA_gll_u32, bcFMA_gll_i64, bcFMA_gll_u64,
    bcFMA_glg_i8, bcFMA_glg_u8, bcFMA_glg_i16, bcFMA_glg_u16, bcFMA_glg_i32, bcFMA_glg_u32, bcFMA_glg_i64, bcFMA_glg_u64,
    bcFMA_ggl_i8, bcFMA_ggl_u8, bcFMA_ggl_i16, bcFMA_ggl_u16, bcFMA_ggl_i32, bcFMA_ggl_u32, bcFMA_ggl_i64, bcFMA_ggl_u64,
    bcFMA_ggg_i8, bcFMA_ggg_u8, bcFMA_ggg_i16, bcFMA_ggg_u16, bcFMA_ggg_i32, bcFMA_ggg_u32, bcFMA_ggg_i64, bcFMA_ggg_u64,

    bcDREF_ll,    bcDREF_lg,    bcDREF_gl,    bcDREF_gg,
    bcDREF_ll_32, bcDREF_lg_32, bcDREF_gl_32, bcDREF_gg_32,
    bcDREF_ll_64, bcDREF_lg_64, bcDREF_gl_64, bcDREF_gg_64,

    bcCMP, bcCMP_ll_i64, bcCMP_li_i64, bcCMP_il_i64,
           bcCMP_ll_i32, bcCMP_li_i32, bcCMP_il_i32,

    // slowpath
    bcADD, bcSUB, bcMUL, bcDIV, bcMOD, bcPOW,
    bcEQ,  bcNE,  bcLT,  bcGT,  bcGTE, bcLTE,
    bcBND, bcSHL, bcSHR, bcXOR, bcBOR, bcSAR,

    bcFADD, bcFSUB, bcFMUL, bcFDIV, bcFMOD, bcFPOW,
    bcFEQ, bcFNEQ, bcFLT, bcFGT, bcFLTE, bcFGTE,

    // Specialize for local dest
    // Ignore anything global
    bcADD_lll_i8, bcADD_lll_u8, bcADD_lll_i16, bcADD_lll_u16, bcADD_lll_i32, bcADD_lll_u32, bcADD_lll_i64, bcADD_lll_u64, bcADD_lll_f32, bcADD_lll_f64,
    bcADD_lil_i8, bcADD_lil_u8, bcADD_lil_i16, bcADD_lil_u16, bcADD_lil_i32, bcADD_lil_u32, bcADD_lil_i64, bcADD_lil_u64, bcADD_lil_f32, bcADD_lil_f64,

    bcADD_ill_i8, bcADD_ill_u8, bcADD_ill_i16, bcADD_ill_u16, bcADD_ill_i32, bcADD_ill_u32, bcADD_ill_i64, bcADD_ill_u64, bcADD_ill_f32, bcADD_ill_f64,
    bcADD_iil_i8, bcADD_iil_u8, bcADD_iil_i16, bcADD_iil_u16, bcADD_iil_i32, bcADD_iil_u32, bcADD_iil_i64, bcADD_iil_u64, bcADD_iil_f32, bcADD_iil_f64,

    bcSUB_lll_i8, bcSUB_lll_u8, bcSUB_lll_i16, bcSUB_lll_u16, bcSUB_lll_i32, bcSUB_lll_u32, bcSUB_lll_i64, bcSUB_lll_u64, bcSUB_lll_f32, bcSUB_lll_f64,
    bcSUB_lil_i8, bcSUB_lil_u8, bcSUB_lil_i16, bcSUB_lil_u16, bcSUB_lil_i32, bcSUB_lil_u32, bcSUB_lil_i64, bcSUB_lil_u64, bcSUB_lil_f32, bcSUB_lil_f64,

    bcSUB_ill_i8, bcSUB_ill_u8, bcSUB_ill_i16, bcSUB_ill_u16, bcSUB_ill_i32, bcSUB_ill_u32, bcSUB_ill_i64, bcSUB_ill_u64, bcSUB_ill_f32, bcSUB_ill_f64,
    bcSUB_iil_i8, bcSUB_iil_u8, bcSUB_iil_i16, bcSUB_iil_u16, bcSUB_iil_i32, bcSUB_iil_u32, bcSUB_iil_i64, bcSUB_iil_u64, bcSUB_iil_f32, bcSUB_iil_f64,

    bcMUL_lll_i8, bcMUL_lll_u8, bcMUL_lll_i16, bcMUL_lll_u16, bcMUL_lll_i32, bcMUL_lll_u32, bcMUL_lll_i64, bcMUL_lll_u64, bcMUL_lll_f32, bcMUL_lll_f64,
    bcMUL_lil_i8, bcMUL_lil_u8, bcMUL_lil_i16, bcMUL_lil_u16, bcMUL_lil_i32, bcMUL_lil_u32, bcMUL_lil_i64, bcMUL_lil_u64, bcMUL_lil_f32, bcMUL_lil_f64,

    bcMUL_ill_i8, bcMUL_ill_u8, bcMUL_ill_i16, bcMUL_ill_u16, bcMUL_ill_i32, bcMUL_ill_u32, bcMUL_ill_i64, bcMUL_ill_u64, bcMUL_ill_f32, bcMUL_ill_f64,
    bcMUL_iil_i8, bcMUL_iil_u8, bcMUL_iil_i16, bcMUL_iil_u16, bcMUL_iil_i32, bcMUL_iil_u32, bcMUL_iil_i64, bcMUL_iil_u64, bcMUL_iil_f32, bcMUL_iil_f64,

    bcDIV_lll_i8, bcDIV_lll_u8, bcDIV_lll_i16, bcDIV_lll_u16, bcDIV_lll_i32, bcDIV_lll_u32, bcDIV_lll_i64, bcDIV_lll_u64, bcDIV_lll_f32, bcDIV_lll_f64,
    bcDIV_lil_i8, bcDIV_lil_u8, bcDIV_lil_i16, bcDIV_lil_u16, bcDIV_lil_i32, bcDIV_lil_u32, bcDIV_lil_i64, bcDIV_lil_u64, bcDIV_lil_f32, bcDIV_lil_f64,

    bcDIV_ill_i8, bcDIV_ill_u8, bcDIV_ill_i16, bcDIV_ill_u16, bcDIV_ill_i32, bcDIV_ill_u32, bcDIV_ill_i64, bcDIV_ill_u64, bcDIV_ill_f32, bcDIV_ill_f64,
    bcDIV_iil_i8, bcDIV_iil_u8, bcDIV_iil_i16, bcDIV_iil_u16, bcDIV_iil_i32, bcDIV_iil_u32, bcDIV_iil_i64, bcDIV_iil_u64, bcDIV_iil_f32, bcDIV_iil_f64,

    bcMOD_lll_i8, bcMOD_lll_u8, bcMOD_lll_i16, bcMOD_lll_u16, bcMOD_lll_i32, bcMOD_lll_u32, bcMOD_lll_i64, bcMOD_lll_u64, bcMOD_lll_f32, bcMOD_lll_f64,
    bcMOD_lil_i8, bcMOD_lil_u8, bcMOD_lil_i16, bcMOD_lil_u16, bcMOD_lil_i32, bcMOD_lil_u32, bcMOD_lil_i64, bcMOD_lil_u64, bcMOD_lil_f32, bcMOD_lil_f64,

    bcMOD_ill_i8, bcMOD_ill_u8, bcMOD_ill_i16, bcMOD_ill_u16, bcMOD_ill_i32, bcMOD_ill_u32, bcMOD_ill_i64, bcMOD_ill_u64, bcMOD_ill_f32, bcMOD_ill_f64,
    bcMOD_iil_i8, bcMOD_iil_u8, bcMOD_iil_i16, bcMOD_iil_u16, bcMOD_iil_i32, bcMOD_iil_u32, bcMOD_iil_i64, bcMOD_iil_u64, bcMOD_iil_f32, bcMOD_iil_f64,

    bcPOW_lll_i8, bcPOW_lll_u8, bcPOW_lll_i16, bcPOW_lll_u16, bcPOW_lll_i32, bcPOW_lll_u32, bcPOW_lll_i64, bcPOW_lll_u64, bcPOW_lll_f32, bcPOW_lll_f64,
    bcPOW_lil_i8, bcPOW_lil_u8, bcPOW_lil_i16, bcPOW_lil_u16, bcPOW_lil_i32, bcPOW_lil_u32, bcPOW_lil_i64, bcPOW_lil_u64, bcPOW_lil_f32, bcPOW_lil_f64,

    bcPOW_ill_i8, bcPOW_ill_u8, bcPOW_ill_i16, bcPOW_ill_u16, bcPOW_ill_i32, bcPOW_ill_u32, bcPOW_ill_i64, bcPOW_ill_u64, bcPOW_ill_f32, bcPOW_ill_f64,
    bcPOW_iil_i8, bcPOW_iil_u8, bcPOW_iil_i16, bcPOW_iil_u16, bcPOW_iil_i32, bcPOW_iil_u32, bcPOW_iil_i64, bcPOW_iil_u64, bcPOW_iil_f32, bcPOW_iil_f64,

    bcEQ_lll_i8, bcEQ_lll_u8, bcEQ_lll_i16, bcEQ_lll_u16, bcEQ_lll_i32, bcEQ_lll_u32, bcEQ_lll_i64, bcEQ_lll_u64, bcEQ_lll_f32, bcEQ_lll_f64,
    bcEQ_lil_i8, bcEQ_lil_u8, bcEQ_lil_i16, bcEQ_lil_u16, bcEQ_lil_i32, bcEQ_lil_u32, bcEQ_lil_i64, bcEQ_lil_u64, bcEQ_lil_f32, bcEQ_lil_f64,

    bcEQ_ill_i8, bcEQ_ill_u8, bcEQ_ill_i16, bcEQ_ill_u16, bcEQ_ill_i32, bcEQ_ill_u32, bcEQ_ill_i64, bcEQ_ill_u64, bcEQ_ill_f32, bcEQ_ill_f64,
    bcEQ_iil_i8, bcEQ_iil_u8, bcEQ_iil_i16, bcEQ_iil_u16, bcEQ_iil_i32, bcEQ_iil_u32, bcEQ_iil_i64, bcEQ_iil_u64, bcEQ_iil_f32, bcEQ_iil_f64,

    bcNE_lll_i8, bcNE_lll_u8, bcNE_lll_i16, bcNE_lll_u16, bcNE_lll_i32, bcNE_lll_u32, bcNE_lll_i64, bcNE_lll_u64, bcNE_lll_f32, bcNE_lll_f64,
    bcNE_lil_i8, bcNE_lil_u8, bcNE_lil_i16, bcNE_lil_u16, bcNE_lil_i32, bcNE_lil_u32, bcNE_lil_i64, bcNE_lil_u64, bcNE_lil_f32, bcNE_lil_f64,

    bcNE_ill_i8, bcNE_ill_u8, bcNE_ill_i16, bcNE_ill_u16, bcNE_ill_i32, bcNE_ill_u32, bcNE_ill_i64, bcNE_ill_u64, bcNE_ill_f32, bcNE_ill_f64,
    bcNE_iil_i8, bcNE_iil_u8, bcNE_iil_i16, bcNE_iil_u16, bcNE_iil_i32, bcNE_iil_u32, bcNE_iil_i64, bcNE_iil_u64, bcNE_iil_f32, bcNE_iil_f64,

    bcLT_lll_i8, bcLT_lll_u8, bcLT_lll_i16, bcLT_lll_u16, bcLT_lll_i32, bcLT_lll_u32, bcLT_lll_i64, bcLT_lll_u64, bcLT_lll_f32, bcLT_lll_f64,
    bcLT_lil_i8, bcLT_lil_u8, bcLT_lil_i16, bcLT_lil_u16, bcLT_lil_i32, bcLT_lil_u32, bcLT_lil_i64, bcLT_lil_u64, bcLT_lil_f32, bcLT_lil_f64,

    bcLT_ill_i8, bcLT_ill_u8, bcLT_ill_i16, bcLT_ill_u16, bcLT_ill_i32, bcLT_ill_u32, bcLT_ill_i64, bcLT_ill_u64, bcLT_ill_f32, bcLT_ill_f64,
    bcLT_iil_i8, bcLT_iil_u8, bcLT_iil_i16, bcLT_iil_u16, bcLT_iil_i32, bcLT_iil_u32, bcLT_iil_i64, bcLT_iil_u64, bcLT_iil_f32, bcLT_iil_f64,

    bcGT_lll_i8, bcGT_lll_u8, bcGT_lll_i16, bcGT_lll_u16, bcGT_lll_i32, bcGT_lll_u32, bcGT_lll_i64, bcGT_lll_u64, bcGT_lll_f32, bcGT_lll_f64,
    bcGT_lil_i8, bcGT_lil_u8, bcGT_lil_i16, bcGT_lil_u16, bcGT_lil_i32, bcGT_lil_u32, bcGT_lil_i64, bcGT_lil_u64, bcGT_lil_f32, bcGT_lil_f64,

    bcGT_ill_i8, bcGT_ill_u8, bcGT_ill_i16, bcGT_ill_u16, bcGT_ill_i32, bcGT_ill_u32, bcGT_ill_i64, bcGT_ill_u64, bcGT_ill_f32, bcGT_ill_f64,
    bcGT_iil_i8, bcGT_iil_u8, bcGT_iil_i16, bcGT_iil_u16, bcGT_iil_i32, bcGT_iil_u32, bcGT_iil_i64, bcGT_iil_u64, bcGT_iil_f32, bcGT_iil_f64,

    bcGTE_lll_i8, bcGTE_lll_u8, bcGTE_lll_i16, bcGTE_lll_u16, bcGTE_lll_i32, bcGTE_lll_u32, bcGTE_lll_i64, bcGTE_lll_u64, bcGTE_lll_f32, bcGTE_lll_f64,
    bcGTE_lil_i8, bcGTE_lil_u8, bcGTE_lil_i16, bcGTE_lil_u16, bcGTE_lil_i32, bcGTE_lil_u32, bcGTE_lil_i64, bcGTE_lil_u64, bcGTE_lil_f32, bcGTE_lil_f64,

    bcGTE_ill_i8, bcGTE_ill_u8, bcGTE_ill_i16, bcGTE_ill_u16, bcGTE_ill_i32, bcGTE_ill_u32, bcGTE_ill_i64, bcGTE_ill_u64, bcGTE_ill_f32, bcGTE_ill_f64,
    bcGTE_iil_i8, bcGTE_iil_u8, bcGTE_iil_i16, bcGTE_iil_u16, bcGTE_iil_i32, bcGTE_iil_u32, bcGTE_iil_i64, bcGTE_iil_u64, bcGTE_iil_f32, bcGTE_iil_f64,

    bcLTE_lll_i8, bcLTE_lll_u8, bcLTE_lll_i16, bcLTE_lll_u16, bcLTE_lll_i32, bcLTE_lll_u32, bcLTE_lll_i64, bcLTE_lll_u64, bcLTE_lll_f32, bcLTE_lll_f64,
    bcLTE_lil_i8, bcLTE_lil_u8, bcLTE_lil_i16, bcLTE_lil_u16, bcLTE_lil_i32, bcLTE_lil_u32, bcLTE_lil_i64, bcLTE_lil_u64, bcLTE_lil_f32, bcLTE_lil_f64,

    bcLTE_ill_i8, bcLTE_ill_u8, bcLTE_ill_i16, bcLTE_ill_u16, bcLTE_ill_i32, bcLTE_ill_u32, bcLTE_ill_i64, bcLTE_ill_u64, bcLTE_ill_f32, bcLTE_ill_f64,
    bcLTE_iil_i8, bcLTE_iil_u8, bcLTE_iil_i16, bcLTE_iil_u16, bcLTE_iil_i32, bcLTE_iil_u32, bcLTE_iil_i64, bcLTE_iil_u64, bcLTE_iil_f32, bcLTE_iil_f64,

    bcBND_lll_i8, bcBND_lll_u8, bcBND_lll_i16, bcBND_lll_u16, bcBND_lll_i32, bcBND_lll_u32, bcBND_lll_i64, bcBND_lll_u64,
    bcBND_lil_i8, bcBND_lil_u8, bcBND_lil_i16, bcBND_lil_u16, bcBND_lil_i32, bcBND_lil_u32, bcBND_lil_i64, bcBND_lil_u64,

    bcBND_ill_i8, bcBND_ill_u8, bcBND_ill_i16, bcBND_ill_u16, bcBND_ill_i32, bcBND_ill_u32, bcBND_ill_i64, bcBND_ill_u64,
    bcBND_iil_i8, bcBND_iil_u8, bcBND_iil_i16, bcBND_iil_u16, bcBND_iil_i32, bcBND_iil_u32, bcBND_iil_i64, bcBND_iil_u64,

    bcSHL_lll_i8, bcSHL_lll_u8, bcSHL_lll_i16, bcSHL_lll_u16, bcSHL_lll_i32, bcSHL_lll_u32, bcSHL_lll_i64, bcSHL_lll_u64,
    bcSHL_lil_i8, bcSHL_lil_u8, bcSHL_lil_i16, bcSHL_lil_u16, bcSHL_lil_i32, bcSHL_lil_u32, bcSHL_lil_i64, bcSHL_lil_u64,

    bcSHL_ill_i8, bcSHL_ill_u8, bcSHL_ill_i16, bcSHL_ill_u16, bcSHL_ill_i32, bcSHL_ill_u32, bcSHL_ill_i64, bcSHL_ill_u64,
    bcSHL_iil_i8, bcSHL_iil_u8, bcSHL_iil_i16, bcSHL_iil_u16, bcSHL_iil_i32, bcSHL_iil_u32, bcSHL_iil_i64, bcSHL_iil_u64,

    bcSHR_lll_i8, bcSHR_lll_u8, bcSHR_lll_i16, bcSHR_lll_u16, bcSHR_lll_i32, bcSHR_lll_u32, bcSHR_lll_i64, bcSHR_lll_u64,
    bcSHR_lil_i8, bcSHR_lil_u8, bcSHR_lil_i16, bcSHR_lil_u16, bcSHR_lil_i32, bcSHR_lil_u32, bcSHR_lil_i64, bcSHR_lil_u64,

    bcSHR_ill_i8, bcSHR_ill_u8, bcSHR_ill_i16, bcSHR_ill_u16, bcSHR_ill_i32, bcSHR_ill_u32, bcSHR_ill_i64, bcSHR_ill_u64,
    bcSHR_iil_i8, bcSHR_iil_u8, bcSHR_iil_i16, bcSHR_iil_u16, bcSHR_iil_i32, bcSHR_iil_u32, bcSHR_iil_i64, bcSHR_iil_u64,

    bcXOR_lll_i8, bcXOR_lll_u8, bcXOR_lll_i16, bcXOR_lll_u16, bcXOR_lll_i32, bcXOR_lll_u32, bcXOR_lll_i64, bcXOR_lll_u64,
    bcXOR_lil_i8, bcXOR_lil_u8, bcXOR_lil_i16, bcXOR_lil_u16, bcXOR_lil_i32, bcXOR_lil_u32, bcXOR_lil_i64, bcXOR_lil_u64,

    bcXOR_ill_i8, bcXOR_ill_u8, bcXOR_ill_i16, bcXOR_ill_u16, bcXOR_ill_i32, bcXOR_ill_u32, bcXOR_ill_i64, bcXOR_ill_u64,
    bcXOR_iil_i8, bcXOR_iil_u8, bcXOR_iil_i16, bcXOR_iil_u16, bcXOR_iil_i32, bcXOR_iil_u32, bcXOR_iil_i64, bcXOR_iil_u64,

    bcBOR_lll_i8, bcBOR_lll_u8, bcBOR_lll_i16, bcBOR_lll_u16, bcBOR_lll_i32, bcBOR_lll_u32, bcBOR_lll_i64, bcBOR_lll_u64,
    bcBOR_lil_i8, bcBOR_lil_u8, bcBOR_lil_i16, bcBOR_lil_u16, bcBOR_lil_i32, bcBOR_lil_u32, bcBOR_lil_i64, bcBOR_lil_u64,

    bcBOR_ill_i8, bcBOR_ill_u8, bcBOR_ill_i16, bcBOR_ill_u16, bcBOR_ill_i32, bcBOR_ill_u32, bcBOR_ill_i64, bcBOR_ill_u64,
    bcBOR_iil_i8, bcBOR_iil_u8, bcBOR_iil_i16, bcBOR_iil_u16, bcBOR_iil_i32, bcBOR_iil_u32, bcBOR_iil_i64, bcBOR_iil_u64,

    bcSAR_lll_i8, bcSAR_lll_u8, bcSAR_lll_i16, bcSAR_lll_u16, bcSAR_lll_i32, bcSAR_lll_u32, bcSAR_lll_i64, bcSAR_lll_u64,
    bcSAR_lil_i8, bcSAR_lil_u8, bcSAR_lil_i16, bcSAR_lil_u16, bcSAR_lil_i32, bcSAR_lil_u32, bcSAR_lil_i64, bcSAR_lil_u64,

    bcSAR_ill_i8, bcSAR_ill_u8, bcSAR_ill_i16, bcSAR_ill_u16, bcSAR_ill_i32, bcSAR_ill_u32, bcSAR_ill_i64, bcSAR_ill_u64,
    bcSAR_iil_i8, bcSAR_iil_u8, bcSAR_iil_i16, bcSAR_iil_u16, bcSAR_iil_i32, bcSAR_iil_u32, bcSAR_iil_i64, bcSAR_iil_u64,


    // --- MOV Operations (Source, Destination, SourceType, DestType) ---
    // Destination is ALWAYS Local ('l') for these specialized opcodes.
    // Source can be Local ('l'), Immediate ('i'), or Global ('g').
    // Format: bcMOV_SourceType_DestType_SourcePos_DestPos
    // Example: bcMOV_i8_u16_li (Move Int8 Immediate to UInt16 Local)
    bcMOV,

    // --- Integer Source Types (i8, u8, i16, u16, i32, u32, i64, u64) ---
    // --- Destination Types (i8, u8, i16, u16, i32, u32, i64, u64) ---

    // Source: i8
    bcMOV_i8_i8_ll, bcMOV_i8_i8_li, bcMOV_i8_i8_lg,
    bcMOV_i8_u8_ll, bcMOV_i8_u8_li, bcMOV_i8_u8_lg,
    bcMOV_i8_i16_ll, bcMOV_i8_i16_li, bcMOV_i8_i16_lg,
    bcMOV_i8_u16_ll, bcMOV_i8_u16_li, bcMOV_i8_u16_lg,
    bcMOV_i8_i32_ll, bcMOV_i8_i32_li, bcMOV_i8_i32_lg,
    bcMOV_i8_u32_ll, bcMOV_i8_u32_li, bcMOV_i8_u32_lg,
    bcMOV_i8_i64_ll, bcMOV_i8_i64_li, bcMOV_i8_i64_lg,
    bcMOV_i8_u64_ll, bcMOV_i8_u64_li, bcMOV_i8_u64_lg,

    // Source: u8
    bcMOV_u8_i8_ll, bcMOV_u8_i8_li, bcMOV_u8_i8_lg,
    bcMOV_u8_u8_ll, bcMOV_u8_u8_li, bcMOV_u8_u8_lg,
    bcMOV_u8_i16_ll, bcMOV_u8_i16_li, bcMOV_u8_i16_lg,
    bcMOV_u8_u16_ll, bcMOV_u8_u16_li, bcMOV_u8_u16_lg,
    bcMOV_u8_i32_ll, bcMOV_u8_i32_li, bcMOV_u8_i32_lg,
    bcMOV_u8_u32_ll, bcMOV_u8_u32_li, bcMOV_u8_u32_lg,
    bcMOV_u8_i64_ll, bcMOV_u8_i64_li, bcMOV_u8_i64_lg,
    bcMOV_u8_u64_ll, bcMOV_u8_u64_li, bcMOV_u8_u64_lg,

    // Source: i16
    bcMOV_i16_i8_ll, bcMOV_i16_i8_li, bcMOV_i16_i8_lg,
    bcMOV_i16_u8_ll, bcMOV_i16_u8_li, bcMOV_i16_u8_lg,
    bcMOV_i16_i16_ll, bcMOV_i16_i16_li, bcMOV_i16_i16_lg,
    bcMOV_i16_u16_ll, bcMOV_i16_u16_li, bcMOV_i16_u16_lg,
    bcMOV_i16_i32_ll, bcMOV_i16_i32_li, bcMOV_i16_i32_lg,
    bcMOV_i16_u32_ll, bcMOV_i16_u32_li, bcMOV_i16_u32_lg,
    bcMOV_i16_i64_ll, bcMOV_i16_i64_li, bcMOV_i16_i64_lg,
    bcMOV_i16_u64_ll, bcMOV_i16_u64_li, bcMOV_i16_u64_lg,

    // Source: u16
    bcMOV_u16_i8_ll, bcMOV_u16_i8_li, bcMOV_u16_i8_lg,
    bcMOV_u16_u8_ll, bcMOV_u16_u8_li, bcMOV_u16_u8_lg,
    bcMOV_u16_i16_ll, bcMOV_u16_i16_li, bcMOV_u16_i16_lg,
    bcMOV_u16_u16_ll, bcMOV_u16_u16_li, bcMOV_u16_u16_lg,
    bcMOV_u16_i32_ll, bcMOV_u16_i32_li, bcMOV_u16_i32_lg,
    bcMOV_u16_u32_ll, bcMOV_u16_u32_li, bcMOV_u16_u32_lg,
    bcMOV_u16_i64_ll, bcMOV_u16_i64_li, bcMOV_u16_i64_lg,
    bcMOV_u16_u64_ll, bcMOV_u16_u64_li, bcMOV_u16_u64_lg,

    // Source: i32
    bcMOV_i32_i8_ll, bcMOV_i32_i8_li, bcMOV_i32_i8_lg,
    bcMOV_i32_u8_ll, bcMOV_i32_u8_li, bcMOV_i32_u8_lg,
    bcMOV_i32_i16_ll, bcMOV_i32_i16_li, bcMOV_i32_i16_lg,
    bcMOV_i32_u16_ll, bcMOV_i32_u16_li, bcMOV_i32_u16_lg,
    bcMOV_i32_i32_ll, bcMOV_i32_i32_li, bcMOV_i32_i32_lg,
    bcMOV_i32_u32_ll, bcMOV_i32_u32_li, bcMOV_i32_u32_lg,
    bcMOV_i32_i64_ll, bcMOV_i32_i64_li, bcMOV_i32_i64_lg,
    bcMOV_i32_u64_ll, bcMOV_i32_u64_li, bcMOV_i32_u64_lg,

    // Source: u32
    bcMOV_u32_i8_ll, bcMOV_u32_i8_li, bcMOV_u32_i8_lg,
    bcMOV_u32_u8_ll, bcMOV_u32_u8_li, bcMOV_u32_u8_lg,
    bcMOV_u32_i16_ll, bcMOV_u32_i16_li, bcMOV_u32_i16_lg,
    bcMOV_u32_u16_ll, bcMOV_u32_u16_li, bcMOV_u32_u16_lg,
    bcMOV_u32_i32_ll, bcMOV_u32_i32_li, bcMOV_u32_i32_lg,
    bcMOV_u32_u32_ll, bcMOV_u32_u32_li, bcMOV_u32_u32_lg,
    bcMOV_u32_i64_ll, bcMOV_u32_i64_li, bcMOV_u32_i64_lg,
    bcMOV_u32_u64_ll, bcMOV_u32_u64_li, bcMOV_u32_u64_lg,

    // Source: i64
    bcMOV_i64_i8_ll, bcMOV_i64_i8_li, bcMOV_i64_i8_lg,
    bcMOV_i64_u8_ll, bcMOV_i64_u8_li, bcMOV_i64_u8_lg,
    bcMOV_i64_i16_ll, bcMOV_i64_i16_li, bcMOV_i64_i16_lg,
    bcMOV_i64_u16_ll, bcMOV_i64_u16_li, bcMOV_i64_u16_lg,
    bcMOV_i64_i32_ll, bcMOV_i64_i32_li, bcMOV_i64_i32_lg,
    bcMOV_i64_u32_ll, bcMOV_i64_u32_li, bcMOV_i64_u32_lg,
    bcMOV_i64_i64_ll, bcMOV_i64_i64_li, bcMOV_i64_i64_lg,
    bcMOV_i64_u64_ll, bcMOV_i64_u64_li, bcMOV_i64_u64_lg,

    // Source: u64
    bcMOV_u64_i8_ll, bcMOV_u64_i8_li, bcMOV_u64_i8_lg,
    bcMOV_u64_u8_ll, bcMOV_u64_u8_li, bcMOV_u64_u8_lg,
    bcMOV_u64_i16_ll, bcMOV_u64_i16_li, bcMOV_u64_i16_lg,
    bcMOV_u64_u16_ll, bcMOV_u64_u16_li, bcMOV_u64_u16_lg,
    bcMOV_u64_i32_ll, bcMOV_u64_i32_li, bcMOV_u64_i32_lg,
    bcMOV_u64_u32_ll, bcMOV_u64_u32_li, bcMOV_u64_u32_lg,
    bcMOV_u64_i64_ll, bcMOV_u64_i64_li, bcMOV_u64_i64_lg,
    bcMOV_u64_u64_ll, bcMOV_u64_u64_li, bcMOV_u64_u64_lg,

    // --- Float Source Types (f32, f64) ---
    // --- Destination Types (i8, u8, i16, u16, i32, u32, i64, u64, f32, f64) ---

    // Source: f32
    bcMOV_f32_i8_ll, bcMOV_f32_i8_li, bcMOV_f32_i8_lg,
    bcMOV_f32_u8_ll, bcMOV_f32_u8_li, bcMOV_f32_u8_lg,
    bcMOV_f32_i16_ll, bcMOV_f32_i16_li, bcMOV_f32_i16_lg,
    bcMOV_f32_u16_ll, bcMOV_f32_u16_li, bcMOV_f32_u16_lg,
    bcMOV_f32_i32_ll, bcMOV_f32_i32_li, bcMOV_f32_i32_lg,
    bcMOV_f32_u32_ll, bcMOV_f32_u32_li, bcMOV_f32_u32_lg,
    bcMOV_f32_i64_ll, bcMOV_f32_i64_li, bcMOV_f32_i64_lg,
    bcMOV_f32_u64_ll, bcMOV_f32_u64_li, bcMOV_f32_u64_lg,
    bcMOV_f32_f32_ll, bcMOV_f32_f32_li, bcMOV_f32_f32_lg,
    bcMOV_f32_f64_ll, bcMOV_f32_f64_li, bcMOV_f32_f64_lg,

    // Source: f64
    bcMOV_f64_i8_ll, bcMOV_f64_i8_li, bcMOV_f64_i8_lg,
    bcMOV_f64_u8_ll, bcMOV_f64_u8_li, bcMOV_f64_u8_lg,
    bcMOV_f64_i16_ll, bcMOV_f64_i16_li, bcMOV_f64_i16_lg,
    bcMOV_f64_u16_ll, bcMOV_f64_u16_li, bcMOV_f64_u16_lg,
    bcMOV_f64_i32_ll, bcMOV_f64_i32_li, bcMOV_f64_i32_lg,
    bcMOV_f64_u32_ll, bcMOV_f64_u32_li, bcMOV_f64_u32_lg,
    bcMOV_f64_i64_ll, bcMOV_f64_i64_li, bcMOV_f64_i64_lg,
    bcMOV_f64_u64_ll, bcMOV_f64_u64_li, bcMOV_f64_u64_lg,
    bcMOV_f64_f32_ll, bcMOV_f64_f32_li, bcMOV_f64_f32_lg,
    bcMOV_f64_f64_ll, bcMOV_f64_f64_li, bcMOV_f64_f64_lg,

    // --- MOVH Operations (Source, Destination Pointer, SourceType, DestType) ---
    // Destination is ALWAYS Local reference ('l') for these specialized opcodes.
    // Source can be Local ('l'), Immediate ('i'), or Global ('g').
    // Format: bcMOVH_SourceType_DestType_SourcePos_DestPos
    // Example: bcMOVH_i8_u16_li (move Int8 Immediate to PUInt16 Local)
    bcMOVH,

    // --- Integer Source Types (i8, u8, i16, u16, i32, u32, i64, u64) ---
    // --- Destination Types Pointer(i8, u8, i16, u16, i32, u32, i64, u64) ---

    // Source: i8
    bcMOVH_i8_i8_ll, bcMOVH_i8_i8_li, bcMOVH_i8_i8_lg,
    bcMOVH_i8_u8_ll, bcMOVH_i8_u8_li, bcMOVH_i8_u8_lg,
    bcMOVH_i8_i16_ll, bcMOVH_i8_i16_li, bcMOVH_i8_i16_lg,
    bcMOVH_i8_u16_ll, bcMOVH_i8_u16_li, bcMOVH_i8_u16_lg,
    bcMOVH_i8_i32_ll, bcMOVH_i8_i32_li, bcMOVH_i8_i32_lg,
    bcMOVH_i8_u32_ll, bcMOVH_i8_u32_li, bcMOVH_i8_u32_lg,
    bcMOVH_i8_i64_ll, bcMOVH_i8_i64_li, bcMOVH_i8_i64_lg,
    bcMOVH_i8_u64_ll, bcMOVH_i8_u64_li, bcMOVH_i8_u64_lg,

    // Source: u8
    bcMOVH_u8_i8_ll, bcMOVH_u8_i8_li, bcMOVH_u8_i8_lg,
    bcMOVH_u8_u8_ll, bcMOVH_u8_u8_li, bcMOVH_u8_u8_lg,
    bcMOVH_u8_i16_ll, bcMOVH_u8_i16_li, bcMOVH_u8_i16_lg,
    bcMOVH_u8_u16_ll, bcMOVH_u8_u16_li, bcMOVH_u8_u16_lg,
    bcMOVH_u8_i32_ll, bcMOVH_u8_i32_li, bcMOVH_u8_i32_lg,
    bcMOVH_u8_u32_ll, bcMOVH_u8_u32_li, bcMOVH_u8_u32_lg,
    bcMOVH_u8_i64_ll, bcMOVH_u8_i64_li, bcMOVH_u8_i64_lg,
    bcMOVH_u8_u64_ll, bcMOVH_u8_u64_li, bcMOVH_u8_u64_lg,

    // Source: i16
    bcMOVH_i16_i8_ll, bcMOVH_i16_i8_li, bcMOVH_i16_i8_lg,
    bcMOVH_i16_u8_ll, bcMOVH_i16_u8_li, bcMOVH_i16_u8_lg,
    bcMOVH_i16_i16_ll, bcMOVH_i16_i16_li, bcMOVH_i16_i16_lg,
    bcMOVH_i16_u16_ll, bcMOVH_i16_u16_li, bcMOVH_i16_u16_lg,
    bcMOVH_i16_i32_ll, bcMOVH_i16_i32_li, bcMOVH_i16_i32_lg,
    bcMOVH_i16_u32_ll, bcMOVH_i16_u32_li, bcMOVH_i16_u32_lg,
    bcMOVH_i16_i64_ll, bcMOVH_i16_i64_li, bcMOVH_i16_i64_lg,
    bcMOVH_i16_u64_ll, bcMOVH_i16_u64_li, bcMOVH_i16_u64_lg,

    // Source: u16
    bcMOVH_u16_i8_ll, bcMOVH_u16_i8_li, bcMOVH_u16_i8_lg,
    bcMOVH_u16_u8_ll, bcMOVH_u16_u8_li, bcMOVH_u16_u8_lg,
    bcMOVH_u16_i16_ll, bcMOVH_u16_i16_li, bcMOVH_u16_i16_lg,
    bcMOVH_u16_u16_ll, bcMOVH_u16_u16_li, bcMOVH_u16_u16_lg,
    bcMOVH_u16_i32_ll, bcMOVH_u16_i32_li, bcMOVH_u16_i32_lg,
    bcMOVH_u16_u32_ll, bcMOVH_u16_u32_li, bcMOVH_u16_u32_lg,
    bcMOVH_u16_i64_ll, bcMOVH_u16_i64_li, bcMOVH_u16_i64_lg,
    bcMOVH_u16_u64_ll, bcMOVH_u16_u64_li, bcMOVH_u16_u64_lg,

    // Source: i32
    bcMOVH_i32_i8_ll, bcMOVH_i32_i8_li, bcMOVH_i32_i8_lg,
    bcMOVH_i32_u8_ll, bcMOVH_i32_u8_li, bcMOVH_i32_u8_lg,
    bcMOVH_i32_i16_ll, bcMOVH_i32_i16_li, bcMOVH_i32_i16_lg,
    bcMOVH_i32_u16_ll, bcMOVH_i32_u16_li, bcMOVH_i32_u16_lg,
    bcMOVH_i32_i32_ll, bcMOVH_i32_i32_li, bcMOVH_i32_i32_lg,
    bcMOVH_i32_u32_ll, bcMOVH_i32_u32_li, bcMOVH_i32_u32_lg,
    bcMOVH_i32_i64_ll, bcMOVH_i32_i64_li, bcMOVH_i32_i64_lg,
    bcMOVH_i32_u64_ll, bcMOVH_i32_u64_li, bcMOVH_i32_u64_lg,

    // Source: u32
    bcMOVH_u32_i8_ll, bcMOVH_u32_i8_li, bcMOVH_u32_i8_lg,
    bcMOVH_u32_u8_ll, bcMOVH_u32_u8_li, bcMOVH_u32_u8_lg,
    bcMOVH_u32_i16_ll, bcMOVH_u32_i16_li, bcMOVH_u32_i16_lg,
    bcMOVH_u32_u16_ll, bcMOVH_u32_u16_li, bcMOVH_u32_u16_lg,
    bcMOVH_u32_i32_ll, bcMOVH_u32_i32_li, bcMOVH_u32_i32_lg,
    bcMOVH_u32_u32_ll, bcMOVH_u32_u32_li, bcMOVH_u32_u32_lg,
    bcMOVH_u32_i64_ll, bcMOVH_u32_i64_li, bcMOVH_u32_i64_lg,
    bcMOVH_u32_u64_ll, bcMOVH_u32_u64_li, bcMOVH_u32_u64_lg,

    // Source: i64
    bcMOVH_i64_i8_ll, bcMOVH_i64_i8_li, bcMOVH_i64_i8_lg,
    bcMOVH_i64_u8_ll, bcMOVH_i64_u8_li, bcMOVH_i64_u8_lg,
    bcMOVH_i64_i16_ll, bcMOVH_i64_i16_li, bcMOVH_i64_i16_lg,
    bcMOVH_i64_u16_ll, bcMOVH_i64_u16_li, bcMOVH_i64_u16_lg,
    bcMOVH_i64_i32_ll, bcMOVH_i64_i32_li, bcMOVH_i64_i32_lg,
    bcMOVH_i64_u32_ll, bcMOVH_i64_u32_li, bcMOVH_i64_u32_lg,
    bcMOVH_i64_i64_ll, bcMOVH_i64_i64_li, bcMOVH_i64_i64_lg,
    bcMOVH_i64_u64_ll, bcMOVH_i64_u64_li, bcMOVH_i64_u64_lg,

    // Source: u64
    bcMOVH_u64_i8_ll, bcMOVH_u64_i8_li, bcMOVH_u64_i8_lg,
    bcMOVH_u64_u8_ll, bcMOVH_u64_u8_li, bcMOVH_u64_u8_lg,
    bcMOVH_u64_i16_ll, bcMOVH_u64_i16_li, bcMOVH_u64_i16_lg,
    bcMOVH_u64_u16_ll, bcMOVH_u64_u16_li, bcMOVH_u64_u16_lg,
    bcMOVH_u64_i32_ll, bcMOVH_u64_i32_li, bcMOVH_u64_i32_lg,
    bcMOVH_u64_u32_ll, bcMOVH_u64_u32_li, bcMOVH_u64_u32_lg,
    bcMOVH_u64_i64_ll, bcMOVH_u64_i64_li, bcMOVH_u64_i64_lg,
    bcMOVH_u64_u64_ll, bcMOVH_u64_u64_li, bcMOVH_u64_u64_lg,

    // --- Float Source Types (f32, f64) ---
    // --- Destination Types (i8, u8, i16, u16, i32, u32, i64, u64, f32, f64) ---

    // Source: f32
    bcMOVH_f32_i8_ll, bcMOVH_f32_i8_li, bcMOVH_f32_i8_lg,
    bcMOVH_f32_u8_ll, bcMOVH_f32_u8_li, bcMOVH_f32_u8_lg,
    bcMOVH_f32_i16_ll, bcMOVH_f32_i16_li, bcMOVH_f32_i16_lg,
    bcMOVH_f32_u16_ll, bcMOVH_f32_u16_li, bcMOVH_f32_u16_lg,
    bcMOVH_f32_i32_ll, bcMOVH_f32_i32_li, bcMOVH_f32_i32_lg,
    bcMOVH_f32_u32_ll, bcMOVH_f32_u32_li, bcMOVH_f32_u32_lg,
    bcMOVH_f32_i64_ll, bcMOVH_f32_i64_li, bcMOVH_f32_i64_lg,
    bcMOVH_f32_u64_ll, bcMOVH_f32_u64_li, bcMOVH_f32_u64_lg,
    bcMOVH_f32_f32_ll, bcMOVH_f32_f32_li, bcMOVH_f32_f32_lg,
    bcMOVH_f32_f64_ll, bcMOVH_f32_f64_li, bcMOVH_f32_f64_lg,

    // Source: f64
    bcMOVH_f64_i8_ll, bcMOVH_f64_i8_li, bcMOVH_f64_i8_lg,
    bcMOVH_f64_u8_ll, bcMOVH_f64_u8_li, bcMOVH_f64_u8_lg,
    bcMOVH_f64_i16_ll, bcMOVH_f64_i16_li, bcMOVH_f64_i16_lg,
    bcMOVH_f64_u16_ll, bcMOVH_f64_u16_li, bcMOVH_f64_u16_lg,
    bcMOVH_f64_i32_ll, bcMOVH_f64_i32_li, bcMOVH_f64_i32_lg,
    bcMOVH_f64_u32_ll, bcMOVH_f64_u32_li, bcMOVH_f64_u32_lg,
    bcMOVH_f64_i64_ll, bcMOVH_f64_i64_li, bcMOVH_f64_i64_lg,
    bcMOVH_f64_u64_ll, bcMOVH_f64_u64_li, bcMOVH_f64_u64_lg,
    bcMOVH_f64_f32_ll, bcMOVH_f64_f32_li, bcMOVH_f64_f32_lg,
    bcMOVH_f64_f64_ll, bcMOVH_f64_f64_li, bcMOVH_f64_f64_lg
  );



  // Instruction argument metadata
  TOperand = packed record
    Pos: EMemPos;
    Typ: EExpressBaseType;
    _pad: Byte;
    case Byte of
      0: (Arg: Int64);
      1: (Addr: PtrInt);
      2: (i32: Int32);
  end;

  // Intermediate instruction record (fits in one cache line)
  TBytecodeInstruction = packed record
    Args: array[0..4] of TOperand;
    Code: EBytecode;
    nArgs: Byte;
  end;

  TProgramData = specialize TArrayList<TBytecodeInstruction>;

  TBytecode = record
    Code: TProgramData;
    Docpos: TDocPosList;

    procedure Init();
    procedure Free();

    function Add(OP: TBytecodeInstruction; Pos: TDocPos): Int32;
    function Delete(Index: Int32): TBytecodeInstruction;

    function ToString(Colorize: Boolean = False): string;
  end;



implementation

uses math, typinfo, xprUtils;


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
begin
  Result := '';
  for i := 0 to Code.High do
  begin
    this := Code.Data[i];

    // Format line and index
    if Colorize then
    begin
      lineStr := _LWHITE_ + 'L' + DocPos.Data[i].Line.ToString + _WHITE_;
      idxStr  := _LWHITE_ + '#' + i.ToString + _WHITE_;
      opStr   := _AQUA_    + GetEnumName(TypeInfo(EBytecode), Ord(this.Code)) + _WHITE_;
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
          mpGlobal: begin posName := 'glob '; if Colorize then posColor := _AQUA_   else posColor := ''; end;
          mpLocal:  begin posName := 'loc  '; if Colorize then posColor := _YELLOW_ else posColor := ''; end;
          mpHeap:   begin posName := 'heap '; if Colorize then posColor := _GREEN_  else posColor := ''; end;
          mpConst:  begin posName := 'const'; if Colorize then posColor := _BLUE_  else posColor := ''; end;
          mpPointer:begin posName := 'ptr  '; if Colorize then posColor := _LYELLOW_  else posColor := ''; end;
        else        begin posName := 'unk  '; if Colorize then posColor := _RED_ else posColor := ''; end;
        end;

        // Type and value
        typeStr := BT2SM(Typ);
        valStr := IntToStr(Arg);
        if Length(valStr) > 8 then begin SetLength(ValStr, 6); valStr += '..'; end;

        // Compose formatted argument string
        argStr += Format('%s%s[%-3s:%s] %s|',
          [posColor, posName, typeStr, valStr, _WHITE_]);
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
