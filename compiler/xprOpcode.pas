unit xprBytecode;
{
  Author: Jarl K. Holta  
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)
}
{$I header.inc}
interface

uses 
  SysUtils, xprTypes, xprTokenizer, xprDictionary;

type
  // enum of opcodes for our interpreter
  // this enum can be large, and may contain a lot of specialized ops for hot patterns, and compounded opcodes
  // extracted in later peephole optimizations
  // types are inclued in opcode
  EOpCode = (
    NOP,
    JMP, RELJMP,
    JZ, JNZ,
    
    // push a reference to the top of the stack
    PUSH1, 
    PUSH2, 
    PUSH3,
    
    // pop reference from top of the stack
    POP,
    
    //
    STORE,
    STOREH,
    MOVE,
    MOVEH,
    
    //
    ADDR,
    DREF,

    // assign handles all recasts so binary operations are always type equal
    ASGN_UNDEF,
    
    ASGN_i8_i64, ASGN_i8_i32, ASGN_i8_i16, ASGN_i8_i8, ASGN_i8_u64, ASGN_i8_u32, ASGN_i8_u16, ASGN_i8_u8,
    ASGN_u8_i64, ASGN_u8_i32, ASGN_u8_i16, ASGN_u8_i8, ASGN_u8_u64, ASGN_u8_u32, ASGN_u8_u16, ASGN_u8_u8,
    ASGN_i16_i64, ASGN_i16_i32, ASGN_i16_i16, ASGN_i16_i8, ASGN_i16_u64, ASGN_i16_u32, ASGN_i16_u16, ASGN_i16_u8,
    ASGN_u16_i64, ASGN_u16_i32, ASGN_u16_i16, ASGN_u16_i8, ASGN_u16_u64, ASGN_u16_u32, ASGN_u16_u16, ASGN_u16_u8,
    ASGN_i32_i64, ASGN_i32_i32, ASGN_i32_i16, ASGN_i32_i8, ASGN_i32_u64, ASGN_i32_u32, ASGN_i32_u16, ASGN_i32_u8,
    ASGN_u32_i64, ASGN_u32_i32, ASGN_u32_i16, ASGN_u32_i8, ASGN_u32_u64, ASGN_u32_u32, ASGN_u32_u16, ASGN_u32_u8,
    ASGN_i64_i64, ASGN_i64_i32, ASGN_i64_i16, ASGN_i64_i8, ASGN_i64_u64, ASGN_i64_u32, ASGN_i64_u16, ASGN_i64_u8,
    ASGN_u64_i64, ASGN_u64_i32, ASGN_u64_i64, ASGN_u64_i8, ASGN_u64_u64, ASGN_u64_u32, ASGN_u64_u16, ASGN_u64_u8,

    ASGN_f32_f32, ASGN_f32_f64, ASGN_f32_i64, ASGN_f32_i32, ASGN_f32_i16, ASGN_f32_i8, ASGN_f32_u64, ASGN_f32_u32, ASGN_f32_u16, ASGN_f32_u8,
    ASGN_f64_f32, ASGN_f64_f64, ASGN_f64_i64, ASGN_f64_i32, ASGN_f64_i64, ASGN_f64_i8, ASGN_f64_u64, ASGN_f64_u32, ASGN_f64_u16, ASGN_f64_u8,


    // binary operations, left and right are always equal type, as well as result, no implicit upcasting of result
    USUB_i8, USUB_u8, USUB_i16, USUB_u16, USUB_i32, USUB_u32, USUB_i64, USUB_u64, USUB_f32, USUB_f64, 
    ADD_i8, ADD_u8, ADD_i16, ADD_u16, ADD_i32, ADD_u32, ADD_i64, ADD_u64, ADD_f32, ADD_f64, 
    SUB_i8, SUB_u8, SUB_i16, SUB_u16, SUB_i32, SUB_u32, SUB_i64, SUB_u64, SUB_f32, SUB_f64, 
    MUL_i8, MUL_u8, MUL_i16, MUL_u16, MUL_i32, MUL_u32, MUL_i64, MUL_u64, MUL_f32, MUL_f64, 
    DIV_i8, DIV_u8, DIV_i16, DIV_u16, DIV_i32, DIV_u32, DIV_i64, DIV_u64, DIV_f32, DIV_f64, 
    MOD_i8, MOD_u8, MOD_i16, MOD_u16, MOD_i32, MOD_u32, MOD_i64, MOD_u64, MOD_f32, MOD_f64, 
    POW_i8, POW_u8, POW_i16, POW_u16, POW_i32, POW_u32, POW_i64, POW_u64, POW_f32, POW_f64, 
    EQ_i8, EQ_u8, EQ_i16, EQ_u16, EQ_i32, EQ_u32, EQ_i64, EQ_u64, EQ_f32, EQ_f64, 
    GTE_i8, GTE_u8, GTE_i16, GTE_u16, GTE_i32, GTE_u32, GTE_i64, GTE_u64, GTE_f32, GTE_f64, 
    GT_i8, GT_u8, GT_i16, GT_u16, GT_i32, GT_u32, GT_i64, GT_u64, GT_f32, GT_f64, 
    LT_i8, LT_u8, LT_i16, LT_u16, LT_i32, LT_u32, LT_i64, LT_u64, LT_f32, LT_f64, 
    LTE_i8, LTE_u8, LTE_i16, LTE_u16, LTE_i32, LTE_u32, LTE_i64, LTE_u64, LTE_f32, LTE_f64, 
    NEQ_i8, NEQ_u8, NEQ_i16, NEQ_u16, NEQ_i32, NEQ_u32, NEQ_i64, NEQ_u64, NEQ_f32, NEQ_f64, 
    BND_i8, BND_u8, BND_i16, BND_u16, BND_i32, BND_u32, BND_i64, BND_u64, BND_f32, BND_f64, 
    BOR_i8, BOR_u8, BOR_i16, BOR_u16, BOR_i32, BOR_u32, BOR_i64, BOR_u64, BOR_f32, BOR_f64, 
    SHL_i8, SHL_u8, SHL_i16, SHL_u16, SHL_i32, SHL_u32, SHL_i64, SHL_u64, SHL_f32, SHL_f64, 
    SHR_i8, SHR_u8, SHR_i16, SHR_u16, SHR_i32, SHR_u32, SHR_i64, SHR_u64, SHR_f32, SHR_f64, 
    XOR_i8, XOR_u8, XOR_i16, XOR_u16, XOR_i32, XOR_u32, XOR_i64, XOR_u64, XOR_f32, XOR_f64, 
    SAR_i8, SAR_u8, SAR_i16, SAR_u16, SAR_i32, SAR_u32, SAR_i64, SAR_u64, SAR_f32, SAR_f64, 
    NOT_i8, NOT_u8, NOT_i16, NOT_u16, NOT_i32, NOT_u32, NOT_i64, NOT_u64, NOT_f32, NOT_f64, 
    INV_i8, INV_u8, INV_i16, INV_u16, INV_i32, INV_u32, INV_i64, INV_u64, INV_f32, INV_f64, 
    FMA_i8, FMA_u8, FMA_i16, FMA_u16, FMA_i32, FMA_u32, FMA_i64, FMA_u64, FMA_f32, FMA_f64, 
    
    // temporary
    PRTi, 
    PRTf, 
    PRTb,
    
    INVOKE, 
    INVOKEX,
    RET
  );
  
implementation

uses math, xprUtils, xprBytecode;



end.
