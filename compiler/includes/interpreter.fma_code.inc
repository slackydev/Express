// lll
bcFMA_lll_i8:  PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PInt8(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lll_u8:  PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PUInt8(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lll_i16: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PInt16(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lll_u16: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PUInt16(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lll_i32: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PInt32(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lll_u32: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PUInt32(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lll_i64: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PInt64(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lll_u64: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PUInt64(Stack.Local(Args[0].Addr))^ * Args[1].Addr;

// llg
bcFMA_llg_i8:  PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PInt8(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_llg_u8:  PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PUInt8(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_llg_i16: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PInt16(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_llg_u16: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PUInt16(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_llg_i32: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PInt32(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_llg_u32: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PUInt32(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_llg_i64: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PInt64(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_llg_u64: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PUInt64(Stack.Global(Args[0].Addr))^ * Args[1].Addr;

// lgl
bcFMA_lgl_i8:  PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PInt8(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lgl_u8:  PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PUInt8(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lgl_i16: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PInt16(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lgl_u16: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PUInt16(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lgl_i32: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PInt32(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lgl_u32: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PUInt32(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lgl_i64: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PInt64(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lgl_u64: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PUInt64(Stack.Local(Args[0].Addr))^ * Args[1].Addr;

// lgg
bcFMA_lgg_i8:  PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PInt8(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lgg_u8:  PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PUInt8(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lgg_i16: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PInt16(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lgg_u16: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PUInt16(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lgg_i32: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PInt32(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lgg_u32: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PUInt32(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lgg_i64: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PInt64(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_lgg_u64: PPtrInt(Stack.Local(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PUInt64(Stack.Global(Args[0].Addr))^ * Args[1].Addr;

// gll
bcFMA_gll_i8:  PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PInt8(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_gll_u8:  PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PUInt8(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_gll_i16: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PInt16(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_gll_u16: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PUInt16(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_gll_i32: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PInt32(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_gll_u32: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PUInt32(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_gll_i64: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PInt64(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_gll_u64: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PUInt64(Stack.Local(Args[0].Addr))^ * Args[1].Addr;

// glg
bcFMA_glg_i8:  PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PInt8(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_glg_u8:  PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PUInt8(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_glg_i16: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PInt16(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_glg_u16: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PUInt16(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_glg_i32: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PInt32(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_glg_u32: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PUInt32(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_glg_i64: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PInt64(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_glg_u64: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Local(Args[2].Addr))^ + PUInt64(Stack.Global(Args[0].Addr))^ * Args[1].Addr;

// ggl
bcFMA_ggl_i8:  PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PInt8(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_ggl_u8:  PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PUInt8(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_ggl_i16: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PInt16(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_ggl_u16: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PUInt16(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_ggl_i32: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PInt32(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_ggl_u32: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PUInt32(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_ggl_i64: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PInt64(Stack.Local(Args[0].Addr))^ * Args[1].Addr;
bcFMA_ggl_u64: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PUInt64(Stack.Local(Args[0].Addr))^ * Args[1].Addr;

// ggg
bcFMA_ggg_i8:  PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PInt8(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_ggg_u8:  PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PUInt8(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_ggg_i16: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PInt16(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_ggg_u16: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PUInt16(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_ggg_i32: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PInt32(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_ggg_u32: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PUInt32(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_ggg_i64: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PInt64(Stack.Global(Args[0].Addr))^ * Args[1].Addr;
bcFMA_ggg_u64: PPtrInt(Stack.Global(Args[3].Addr))^ := PPtrInt(Stack.Global(Args[2].Addr))^ + PUInt64(Stack.Global(Args[0].Addr))^ * Args[1].Addr;