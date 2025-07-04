op2instruct[op_Asgn][xtInt8][xtInt8  ] := bcMOV_i8_i8_ll;
op2instruct[op_Asgn][xtInt8][xtUInt8 ] := bcMOV_i8_u8_ll;
op2instruct[op_Asgn][xtInt8][xtInt16 ] := bcMOV_i8_i16_ll;
op2instruct[op_Asgn][xtInt8][xtUInt16] := bcMOV_i8_u16_ll;
op2instruct[op_Asgn][xtInt8][xtInt32 ] := bcMOV_i8_i32_ll;
op2instruct[op_Asgn][xtInt8][xtUInt32] := bcMOV_i8_u32_ll;
op2instruct[op_Asgn][xtInt8][xtInt64 ] := bcMOV_i8_i64_ll;
op2instruct[op_Asgn][xtInt8][xtUInt64] := bcMOV_i8_u64_ll;

op2instruct[op_Asgn][xtUInt8][xtInt8  ] := bcMOV_u8_i8_ll;
op2instruct[op_Asgn][xtUInt8][xtUInt8 ] := bcMOV_u8_u8_ll;
op2instruct[op_Asgn][xtUInt8][xtInt16 ] := bcMOV_u8_i16_ll;
op2instruct[op_Asgn][xtUInt8][xtUInt16] := bcMOV_u8_u16_ll;
op2instruct[op_Asgn][xtUInt8][xtInt32 ] := bcMOV_u8_i32_ll;
op2instruct[op_Asgn][xtUInt8][xtUInt32] := bcMOV_u8_u32_ll;
op2instruct[op_Asgn][xtUInt8][xtInt64 ] := bcMOV_u8_i64_ll;
op2instruct[op_Asgn][xtUInt8][xtUInt64] := bcMOV_u8_u64_ll;

op2instruct[op_Asgn][xtInt16][xtInt8  ] := bcMOV_i16_i8_ll;
op2instruct[op_Asgn][xtInt16][xtUInt8 ] := bcMOV_i16_u8_ll;
op2instruct[op_Asgn][xtInt16][xtInt16 ] := bcMOV_i16_i16_ll;
op2instruct[op_Asgn][xtInt16][xtUInt16] := bcMOV_i16_u16_ll;
op2instruct[op_Asgn][xtInt16][xtInt32 ] := bcMOV_i16_i32_ll;
op2instruct[op_Asgn][xtInt16][xtUInt32] := bcMOV_i16_u32_ll;
op2instruct[op_Asgn][xtInt16][xtInt64 ] := bcMOV_i16_i64_ll;
op2instruct[op_Asgn][xtInt16][xtUInt64] := bcMOV_i16_u64_ll;

op2instruct[op_Asgn][xtUInt16][xtInt8  ] := bcMOV_u16_i8_ll;
op2instruct[op_Asgn][xtUInt16][xtUInt8 ] := bcMOV_u16_u8_ll;
op2instruct[op_Asgn][xtUInt16][xtInt16 ] := bcMOV_u16_i16_ll;
op2instruct[op_Asgn][xtUInt16][xtUInt16] := bcMOV_u16_u16_ll;
op2instruct[op_Asgn][xtUInt16][xtInt32 ] := bcMOV_u16_i32_ll;
op2instruct[op_Asgn][xtUInt16][xtUInt32] := bcMOV_u16_u32_ll;
op2instruct[op_Asgn][xtUInt16][xtInt64 ] := bcMOV_u16_i64_ll;
op2instruct[op_Asgn][xtUInt16][xtUInt64] := bcMOV_u16_u64_ll;

op2instruct[op_Asgn][xtInt32][xtInt8  ] := bcMOV_i32_i8_ll;
op2instruct[op_Asgn][xtInt32][xtUInt8 ] := bcMOV_i32_u8_ll;
op2instruct[op_Asgn][xtInt32][xtInt16 ] := bcMOV_i32_i16_ll;
op2instruct[op_Asgn][xtInt32][xtUInt16] := bcMOV_i32_u16_ll;
op2instruct[op_Asgn][xtInt32][xtInt32 ] := bcMOV_i32_i32_ll;
op2instruct[op_Asgn][xtInt32][xtUInt32] := bcMOV_i32_u32_ll;
op2instruct[op_Asgn][xtInt32][xtInt64 ] := bcMOV_i32_i64_ll;
op2instruct[op_Asgn][xtInt32][xtUInt64] := bcMOV_i32_u64_ll;

op2instruct[op_Asgn][xtUInt32][xtInt8  ] := bcMOV_u32_i8_ll;
op2instruct[op_Asgn][xtUInt32][xtUInt8 ] := bcMOV_u32_u8_ll;
op2instruct[op_Asgn][xtUInt32][xtInt16 ] := bcMOV_u32_i16_ll;
op2instruct[op_Asgn][xtUInt32][xtUInt16] := bcMOV_u32_u16_ll;
op2instruct[op_Asgn][xtUInt32][xtInt32 ] := bcMOV_u32_i32_ll;
op2instruct[op_Asgn][xtUInt32][xtUInt32] := bcMOV_u32_u32_ll;
op2instruct[op_Asgn][xtUInt32][xtInt64 ] := bcMOV_u32_i64_ll;
op2instruct[op_Asgn][xtUInt32][xtUInt64] := bcMOV_u32_u64_ll;

op2instruct[op_Asgn][xtInt64][xtInt8  ] := bcMOV_i64_i8_ll;
op2instruct[op_Asgn][xtInt64][xtUInt8 ] := bcMOV_i64_u8_ll;
op2instruct[op_Asgn][xtInt64][xtInt16 ] := bcMOV_i64_i16_ll;
op2instruct[op_Asgn][xtInt64][xtUInt16] := bcMOV_i64_u16_ll;
op2instruct[op_Asgn][xtInt64][xtInt32 ] := bcMOV_i64_i32_ll;
op2instruct[op_Asgn][xtInt64][xtUInt32] := bcMOV_i64_u32_ll;
op2instruct[op_Asgn][xtInt64][xtInt64 ] := bcMOV_i64_i64_ll;
op2instruct[op_Asgn][xtInt64][xtUInt64] := bcMOV_i64_u64_ll;

op2instruct[op_Asgn][xtUInt64][xtInt8  ] := bcMOV_u64_i8_ll;
op2instruct[op_Asgn][xtUInt64][xtUInt8 ] := bcMOV_u64_u8_ll;
op2instruct[op_Asgn][xtUInt64][xtInt16 ] := bcMOV_u64_i16_ll;
op2instruct[op_Asgn][xtUInt64][xtUInt16] := bcMOV_u64_u16_ll;
op2instruct[op_Asgn][xtUInt64][xtInt32 ] := bcMOV_u64_i32_ll;
op2instruct[op_Asgn][xtUInt64][xtUInt32] := bcMOV_u64_u32_ll;
op2instruct[op_Asgn][xtUInt64][xtInt64 ] := bcMOV_u64_i64_ll;
op2instruct[op_Asgn][xtUInt64][xtUInt64] := bcMOV_u64_u64_ll;

op2instruct[op_Asgn][xtSingle][xtInt8  ] := bcMOV_f32_i8_ll;
op2instruct[op_Asgn][xtSingle][xtUInt8 ] := bcMOV_f32_u8_ll;
op2instruct[op_Asgn][xtSingle][xtInt16 ] := bcMOV_f32_i16_ll;
op2instruct[op_Asgn][xtSingle][xtUInt16] := bcMOV_f32_u16_ll;
op2instruct[op_Asgn][xtSingle][xtInt32 ] := bcMOV_f32_i32_ll;
op2instruct[op_Asgn][xtSingle][xtUInt32] := bcMOV_f32_u32_ll;
op2instruct[op_Asgn][xtSingle][xtInt64 ] := bcMOV_f32_i64_ll;
op2instruct[op_Asgn][xtSingle][xtUInt64] := bcMOV_f32_u64_ll;
op2instruct[op_Asgn][xtSingle][xtSingle] := bcMOV_f32_f32_ll;
op2instruct[op_Asgn][xtSingle][xtDouble] := bcMOV_f32_f64_ll;

op2instruct[op_Asgn][xtDouble][xtInt8  ] := bcMOV_f64_i8_ll;
op2instruct[op_Asgn][xtDouble][xtUInt8 ] := bcMOV_f64_u8_ll;
op2instruct[op_Asgn][xtDouble][xtInt16 ] := bcMOV_f64_i16_ll;
op2instruct[op_Asgn][xtDouble][xtUInt16] := bcMOV_f64_u16_ll;
op2instruct[op_Asgn][xtDouble][xtInt32 ] := bcMOV_f64_i32_ll;
op2instruct[op_Asgn][xtDouble][xtUInt32] := bcMOV_f64_u32_ll;
op2instruct[op_Asgn][xtDouble][xtInt64 ] := bcMOV_f64_i64_ll;
op2instruct[op_Asgn][xtDouble][xtUInt64] := bcMOV_f64_u64_ll;
op2instruct[op_Asgn][xtDouble][xtSingle] := bcMOV_f64_f32_ll;
op2instruct[op_Asgn][xtDouble][xtDouble] := bcMOV_f64_f64_ll;
