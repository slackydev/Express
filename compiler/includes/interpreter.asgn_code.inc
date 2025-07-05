// --- Local Source, Local Destination (bcMOV_SourceType_DestType_ll) ---
bcMOV_i8_i8_ll: PInt8(Stack.Local(Args[0].Addr))^ := PInt8(Stack.Local(Args[1].Addr))^;
bcMOV_i8_u8_ll: PInt8(Stack.Local(Args[0].Addr))^ := PUInt8(Stack.Local(Args[1].Addr))^;
bcMOV_i8_i16_ll: PInt8(Stack.Local(Args[0].Addr))^ := PInt16(Stack.Local(Args[1].Addr))^;
bcMOV_i8_u16_ll: PInt8(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Local(Args[1].Addr))^;
bcMOV_i8_i32_ll: PInt8(Stack.Local(Args[0].Addr))^ := PInt32(Stack.Local(Args[1].Addr))^;
bcMOV_i8_u32_ll: PInt8(Stack.Local(Args[0].Addr))^ := PUInt32(Stack.Local(Args[1].Addr))^;
bcMOV_i8_i64_ll: PInt8(Stack.Local(Args[0].Addr))^ := PInt64(Stack.Local(Args[1].Addr))^;
bcMOV_i8_u64_ll: PInt8(Stack.Local(Args[0].Addr))^ := PUInt64(Stack.Local(Args[1].Addr))^;

bcMOV_u8_i8_ll: PUInt8(Stack.Local(Args[0].Addr))^ := PInt8(Stack.Local(Args[1].Addr))^;
bcMOV_u8_u8_ll: PUInt8(Stack.Local(Args[0].Addr))^ := PUInt8(Stack.Local(Args[1].Addr))^;
bcMOV_u8_i16_ll: PUInt8(Stack.Local(Args[0].Addr))^ := PInt16(Stack.Local(Args[1].Addr))^;
bcMOV_u8_u16_ll: PUInt8(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Local(Args[1].Addr))^;
bcMOV_u8_i32_ll: PUInt8(Stack.Local(Args[0].Addr))^ := PInt32(Stack.Local(Args[1].Addr))^;
bcMOV_u8_u32_ll: PUInt8(Stack.Local(Args[0].Addr))^ := PUInt32(Stack.Local(Args[1].Addr))^;
bcMOV_u8_i64_ll: PUInt8(Stack.Local(Args[0].Addr))^ := PInt64(Stack.Local(Args[1].Addr))^;
bcMOV_u8_u64_ll: PUInt8(Stack.Local(Args[0].Addr))^ := PUInt64(Stack.Local(Args[1].Addr))^;

bcMOV_i16_i8_ll: PInt16(Stack.Local(Args[0].Addr))^ := PInt8(Stack.Local(Args[1].Addr))^;
bcMOV_i16_u8_ll: PInt16(Stack.Local(Args[0].Addr))^ := PUInt8(Stack.Local(Args[1].Addr))^;
bcMOV_i16_i16_ll: PInt16(Stack.Local(Args[0].Addr))^ := PInt16(Stack.Local(Args[1].Addr))^;
bcMOV_i16_u16_ll: PInt16(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Local(Args[1].Addr))^;
bcMOV_i16_i32_ll: PInt16(Stack.Local(Args[0].Addr))^ := PInt32(Stack.Local(Args[1].Addr))^;
bcMOV_i16_u32_ll: PInt16(Stack.Local(Args[0].Addr))^ := PUInt32(Stack.Local(Args[1].Addr))^;
bcMOV_i16_i64_ll: PInt16(Stack.Local(Args[0].Addr))^ := PInt64(Stack.Local(Args[1].Addr))^;
bcMOV_i16_u64_ll: PInt16(Stack.Local(Args[0].Addr))^ := PUInt64(Stack.Local(Args[1].Addr))^;

bcMOV_u16_i8_ll: PUInt16(Stack.Local(Args[0].Addr))^ := PInt8(Stack.Local(Args[1].Addr))^;
bcMOV_u16_u8_ll: PUInt16(Stack.Local(Args[0].Addr))^ := PUInt8(Stack.Local(Args[1].Addr))^;
bcMOV_u16_i16_ll: PUInt16(Stack.Local(Args[0].Addr))^ := PInt16(Stack.Local(Args[1].Addr))^;
bcMOV_u16_u16_ll: PUInt16(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Local(Args[1].Addr))^;
bcMOV_u16_i32_ll: PUInt16(Stack.Local(Args[0].Addr))^ := PInt32(Stack.Local(Args[1].Addr))^;
bcMOV_u16_u32_ll: PUInt16(Stack.Local(Args[0].Addr))^ := PUInt32(Stack.Local(Args[1].Addr))^;
bcMOV_u16_i64_ll: PUInt16(Stack.Local(Args[0].Addr))^ := PInt64(Stack.Local(Args[1].Addr))^;
bcMOV_u16_u64_ll: PUInt16(Stack.Local(Args[0].Addr))^ := PUInt64(Stack.Local(Args[1].Addr))^;

bcMOV_i32_i8_ll: PInt32(Stack.Local(Args[0].Addr))^ := PInt8(Stack.Local(Args[1].Addr))^;
bcMOV_i32_u8_ll: PInt32(Stack.Local(Args[0].Addr))^ := PUInt8(Stack.Local(Args[1].Addr))^;
bcMOV_i32_i16_ll: PInt32(Stack.Local(Args[0].Addr))^ := PInt16(Stack.Local(Args[1].Addr))^;
bcMOV_i32_u16_ll: PInt32(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Local(Args[1].Addr))^;
bcMOV_i32_i32_ll: PInt32(Stack.Local(Args[0].Addr))^ := PInt32(Stack.Local(Args[1].Addr))^;
bcMOV_i32_u32_ll: PInt32(Stack.Local(Args[0].Addr))^ := PUInt32(Stack.Local(Args[1].Addr))^;
bcMOV_i32_i64_ll: PInt32(Stack.Local(Args[0].Addr))^ := PInt64(Stack.Local(Args[1].Addr))^;
bcMOV_i32_u64_ll: PInt32(Stack.Local(Args[0].Addr))^ := PUInt64(Stack.Local(Args[1].Addr))^;

bcMOV_u32_i8_ll: PUInt32(Stack.Local(Args[0].Addr))^ := PInt8(Stack.Local(Args[1].Addr))^;
bcMOV_u32_u8_ll: PUInt32(Stack.Local(Args[0].Addr))^ := PUInt8(Stack.Local(Args[1].Addr))^;
bcMOV_u32_i16_ll: PUInt32(Stack.Local(Args[0].Addr))^ := PInt16(Stack.Local(Args[1].Addr))^;
bcMOV_u32_u16_ll: PUInt32(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Local(Args[1].Addr))^;
bcMOV_u32_i32_ll: PUInt32(Stack.Local(Args[0].Addr))^ := PInt32(Stack.Local(Args[1].Addr))^;
bcMOV_u32_u32_ll: PUInt32(Stack.Local(Args[0].Addr))^ := PUInt32(Stack.Local(Args[1].Addr))^;
bcMOV_u32_i64_ll: PUInt32(Stack.Local(Args[0].Addr))^ := PInt64(Stack.Local(Args[1].Addr))^;
bcMOV_u32_u64_ll: PUInt32(Stack.Local(Args[0].Addr))^ := PUInt64(Stack.Local(Args[1].Addr))^;

bcMOV_i64_i8_ll: PInt64(Stack.Local(Args[0].Addr))^ := PInt8(Stack.Local(Args[1].Addr))^;
bcMOV_i64_u8_ll: PInt64(Stack.Local(Args[0].Addr))^ := PUInt8(Stack.Local(Args[1].Addr))^;
bcMOV_i64_i16_ll: PInt64(Stack.Local(Args[0].Addr))^ := PInt16(Stack.Local(Args[1].Addr))^;
bcMOV_i64_u16_ll: PInt64(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Local(Args[1].Addr))^;
bcMOV_i64_i32_ll: PInt64(Stack.Local(Args[0].Addr))^ := PInt32(Stack.Local(Args[1].Addr))^;
bcMOV_i64_u32_ll: PInt64(Stack.Local(Args[0].Addr))^ := PUInt32(Stack.Local(Args[1].Addr))^;
bcMOV_i64_i64_ll: PInt64(Stack.Local(Args[0].Addr))^ := PInt64(Stack.Local(Args[1].Addr))^;
bcMOV_i64_u64_ll: PInt64(Stack.Local(Args[0].Addr))^ := PUInt64(Stack.Local(Args[1].Addr))^;

bcMOV_u64_i8_ll: PUInt64(Stack.Local(Args[0].Addr))^ := PInt8(Stack.Local(Args[1].Addr))^;
bcMOV_u64_u8_ll: PUInt64(Stack.Local(Args[0].Addr))^ := PUInt8(Stack.Local(Args[1].Addr))^;
bcMOV_u64_i16_ll: PUInt64(Stack.Local(Args[0].Addr))^ := PInt16(Stack.Local(Args[1].Addr))^;
bcMOV_u64_u16_ll: PUInt64(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Local(Args[1].Addr))^;
bcMOV_u64_i32_ll: PUInt64(Stack.Local(Args[0].Addr))^ := PInt32(Stack.Local(Args[1].Addr))^;
bcMOV_u64_u32_ll: PUInt64(Stack.Local(Args[0].Addr))^ := PUInt32(Stack.Local(Args[1].Addr))^;
bcMOV_u64_i64_ll: PUInt64(Stack.Local(Args[0].Addr))^ := PInt64(Stack.Local(Args[1].Addr))^;
bcMOV_u64_u64_ll: PUInt64(Stack.Local(Args[0].Addr))^ := PUInt64(Stack.Local(Args[1].Addr))^;

bcMOV_f32_i8_ll: PFloat32(Stack.Local(Args[0].Addr))^ := PInt8(Stack.Local(Args[1].Addr))^;
bcMOV_f32_u8_ll: PFloat32(Stack.Local(Args[0].Addr))^ := PUInt8(Stack.Local(Args[1].Addr))^;
bcMOV_f32_i16_ll: PFloat32(Stack.Local(Args[0].Addr))^ := PInt16(Stack.Local(Args[1].Addr))^;
bcMOV_f32_u16_ll: PFloat32(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Local(Args[1].Addr))^;
bcMOV_f32_i32_ll: PFloat32(Stack.Local(Args[0].Addr))^ := PInt32(Stack.Local(Args[1].Addr))^;
bcMOV_f32_u32_ll: PFloat32(Stack.Local(Args[0].Addr))^ := PUInt32(Stack.Local(Args[1].Addr))^;
bcMOV_f32_i64_ll: PFloat32(Stack.Local(Args[0].Addr))^ := PInt64(Stack.Local(Args[1].Addr))^;
bcMOV_f32_u64_ll: PFloat32(Stack.Local(Args[0].Addr))^ := PUInt64(Stack.Local(Args[1].Addr))^;
bcMOV_f32_f32_ll: PFloat32(Stack.Local(Args[0].Addr))^ := PFloat32(Stack.Local(Args[1].Addr))^;
bcMOV_f32_f64_ll: PFloat32(Stack.Local(Args[0].Addr))^ := PFloat64(Stack.Local(Args[1].Addr))^;

bcMOV_f64_i8_ll: PFloat64(Stack.Local(Args[0].Addr))^ := PInt8(Stack.Local(Args[1].Addr))^;
bcMOV_f64_u8_ll: PFloat64(Stack.Local(Args[0].Addr))^ := PUInt8(Stack.Local(Args[1].Addr))^;
bcMOV_f64_i16_ll: PFloat64(Stack.Local(Args[0].Addr))^ := PInt16(Stack.Local(Args[1].Addr))^;
bcMOV_f64_u16_ll: PFloat64(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Local(Args[1].Addr))^;
bcMOV_f64_i32_ll: PFloat64(Stack.Local(Args[0].Addr))^ := PInt32(Stack.Local(Args[1].Addr))^;
bcMOV_f64_u32_ll: PFloat64(Stack.Local(Args[0].Addr))^ := PUInt32(Stack.Local(Args[1].Addr))^;
bcMOV_f64_i64_ll: PFloat64(Stack.Local(Args[0].Addr))^ := PInt64(Stack.Local(Args[1].Addr))^;
bcMOV_f64_u64_ll: PFloat64(Stack.Local(Args[0].Addr))^ := PUInt64(Stack.Local(Args[1].Addr))^;
bcMOV_f64_f32_ll: PFloat64(Stack.Local(Args[0].Addr))^ := PFloat32(Stack.Local(Args[1].Addr))^;
bcMOV_f64_f64_ll: PFloat64(Stack.Local(Args[0].Addr))^ := PFloat64(Stack.Local(Args[1].Addr))^;

// --- Global Source, Local Destination (bcMOV_SourceType_DestType_lg) ---
bcMOV_i8_i8_lg: PInt8(Stack.Local(Args[0].Addr))^ := PInt8(Stack.Global(Args[1].Addr))^;
bcMOV_i8_u8_lg: PInt8(Stack.Local(Args[0].Addr))^ := PUInt8(Stack.Global(Args[1].Addr))^;
bcMOV_i8_i16_lg: PInt8(Stack.Local(Args[0].Addr))^ := PInt16(Stack.Global(Args[1].Addr))^;
bcMOV_i8_u16_lg: PInt8(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Global(Args[1].Addr))^;
bcMOV_i8_i32_lg: PInt8(Stack.Local(Args[0].Addr))^ := PInt32(Stack.Global(Args[1].Addr))^;
bcMOV_i8_u32_lg: PInt8(Stack.Local(Args[0].Addr))^ := PUInt32(Stack.Global(Args[1].Addr))^;
bcMOV_i8_i64_lg: PInt8(Stack.Local(Args[0].Addr))^ := PInt64(Stack.Global(Args[1].Addr))^;
bcMOV_i8_u64_lg: PInt8(Stack.Local(Args[0].Addr))^ := PUInt64(Stack.Global(Args[1].Addr))^;

bcMOV_u8_i8_lg: PUInt8(Stack.Local(Args[0].Addr))^ := PInt8(Stack.Global(Args[1].Addr))^;
bcMOV_u8_u8_lg: PUInt8(Stack.Local(Args[0].Addr))^ := PUInt8(Stack.Global(Args[1].Addr))^;
bcMOV_u8_i16_lg: PUInt8(Stack.Local(Args[0].Addr))^ := PInt16(Stack.Global(Args[1].Addr))^;
bcMOV_u8_u16_lg: PUInt8(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Global(Args[1].Addr))^;
bcMOV_u8_i32_lg: PUInt8(Stack.Local(Args[0].Addr))^ := PInt32(Stack.Global(Args[1].Addr))^;
bcMOV_u8_u32_lg: PUInt8(Stack.Local(Args[0].Addr))^ := PUInt32(Stack.Global(Args[1].Addr))^;
bcMOV_u8_i64_lg: PUInt8(Stack.Local(Args[0].Addr))^ := PInt64(Stack.Global(Args[1].Addr))^;
bcMOV_u8_u64_lg: PUInt8(Stack.Local(Args[0].Addr))^ := PUInt64(Stack.Global(Args[1].Addr))^;

bcMOV_i16_i8_lg: PInt16(Stack.Local(Args[0].Addr))^ := PInt8(Stack.Global(Args[1].Addr))^;
bcMOV_i16_u8_lg: PInt16(Stack.Local(Args[0].Addr))^ := PUInt8(Stack.Global(Args[1].Addr))^;
bcMOV_i16_i16_lg: PInt16(Stack.Local(Args[0].Addr))^ := PInt16(Stack.Global(Args[1].Addr))^;
bcMOV_i16_u16_lg: PInt16(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Global(Args[1].Addr))^;
bcMOV_i16_i32_lg: PInt16(Stack.Local(Args[0].Addr))^ := PInt32(Stack.Global(Args[1].Addr))^;
bcMOV_i16_u32_lg: PInt16(Stack.Local(Args[0].Addr))^ := PUInt32(Stack.Global(Args[1].Addr))^;
bcMOV_i16_i64_lg: PInt16(Stack.Local(Args[0].Addr))^ := PInt64(Stack.Global(Args[1].Addr))^;
bcMOV_i16_u64_lg: PInt16(Stack.Local(Args[0].Addr))^ := PUInt64(Stack.Global(Args[1].Addr))^;

bcMOV_u16_i8_lg: PUInt16(Stack.Local(Args[0].Addr))^ := PInt8(Stack.Global(Args[1].Addr))^;
bcMOV_u16_u8_lg: PUInt16(Stack.Local(Args[0].Addr))^ := PUInt8(Stack.Global(Args[1].Addr))^;
bcMOV_u16_i16_lg: PUInt16(Stack.Local(Args[0].Addr))^ := PInt16(Stack.Global(Args[1].Addr))^;
bcMOV_u16_u16_lg: PUInt16(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Global(Args[1].Addr))^;
bcMOV_u16_i32_lg: PUInt16(Stack.Local(Args[0].Addr))^ := PInt32(Stack.Global(Args[1].Addr))^;
bcMOV_u16_u32_lg: PUInt16(Stack.Local(Args[0].Addr))^ := PUInt32(Stack.Global(Args[1].Addr))^;
bcMOV_u16_i64_lg: PUInt16(Stack.Local(Args[0].Addr))^ := PInt64(Stack.Global(Args[1].Addr))^;
bcMOV_u16_u64_lg: PUInt16(Stack.Local(Args[0].Addr))^ := PUInt64(Stack.Global(Args[1].Addr))^;

bcMOV_i32_i8_lg: PInt32(Stack.Local(Args[0].Addr))^ := PInt8(Stack.Global(Args[1].Addr))^;
bcMOV_i32_u8_lg: PInt32(Stack.Local(Args[0].Addr))^ := PUInt8(Stack.Global(Args[1].Addr))^;
bcMOV_i32_i16_lg: PInt32(Stack.Local(Args[0].Addr))^ := PInt16(Stack.Global(Args[1].Addr))^;
bcMOV_i32_u16_lg: PInt32(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Global(Args[1].Addr))^;
bcMOV_i32_i32_lg: PInt32(Stack.Local(Args[0].Addr))^ := PInt32(Stack.Global(Args[1].Addr))^;
bcMOV_i32_u32_lg: PInt32(Stack.Local(Args[0].Addr))^ := PUInt32(Stack.Global(Args[1].Addr))^;
bcMOV_i32_i64_lg: PInt32(Stack.Local(Args[0].Addr))^ := PInt64(Stack.Global(Args[1].Addr))^;
bcMOV_i32_u64_lg: PInt32(Stack.Local(Args[0].Addr))^ := PUInt64(Stack.Global(Args[1].Addr))^;

bcMOV_u32_i8_lg: PUInt32(Stack.Local(Args[0].Addr))^ := PInt8(Stack.Global(Args[1].Addr))^;
bcMOV_u32_u8_lg: PUInt32(Stack.Local(Args[0].Addr))^ := PUInt8(Stack.Global(Args[1].Addr))^;
bcMOV_u32_i16_lg: PUInt32(Stack.Local(Args[0].Addr))^ := PInt16(Stack.Global(Args[1].Addr))^;
bcMOV_u32_u16_lg: PUInt32(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Global(Args[1].Addr))^;
bcMOV_u32_i32_lg: PUInt32(Stack.Local(Args[0].Addr))^ := PInt32(Stack.Global(Args[1].Addr))^;
bcMOV_u32_u32_lg: PUInt32(Stack.Local(Args[0].Addr))^ := PUInt32(Stack.Global(Args[1].Addr))^;
bcMOV_u32_i64_lg: PUInt32(Stack.Local(Args[0].Addr))^ := PInt64(Stack.Global(Args[1].Addr))^;
bcMOV_u32_u64_lg: PUInt32(Stack.Local(Args[0].Addr))^ := PUInt64(Stack.Global(Args[1].Addr))^;

bcMOV_i64_i8_lg: PInt64(Stack.Local(Args[0].Addr))^ := PInt8(Stack.Global(Args[1].Addr))^;
bcMOV_i64_u8_lg: PInt64(Stack.Local(Args[0].Addr))^ := PUInt8(Stack.Global(Args[1].Addr))^;
bcMOV_i64_i16_lg: PInt64(Stack.Local(Args[0].Addr))^ := PInt16(Stack.Global(Args[1].Addr))^;
bcMOV_i64_u16_lg: PInt64(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Global(Args[1].Addr))^;
bcMOV_i64_i32_lg: PInt64(Stack.Local(Args[0].Addr))^ := PInt32(Stack.Global(Args[1].Addr))^;
bcMOV_i64_u32_lg: PInt64(Stack.Local(Args[0].Addr))^ := PUInt32(Stack.Global(Args[1].Addr))^;
bcMOV_i64_i64_lg: PInt64(Stack.Local(Args[0].Addr))^ := PInt64(Stack.Global(Args[1].Addr))^;
bcMOV_i64_u64_lg: PInt64(Stack.Local(Args[0].Addr))^ := PUInt64(Stack.Global(Args[1].Addr))^;

bcMOV_u64_i8_lg: PUInt64(Stack.Local(Args[0].Addr))^ := PInt8(Stack.Global(Args[1].Addr))^;
bcMOV_u64_u8_lg: PUInt64(Stack.Local(Args[0].Addr))^ := PUInt8(Stack.Global(Args[1].Addr))^;
bcMOV_u64_i16_lg: PUInt64(Stack.Local(Args[0].Addr))^ := PInt16(Stack.Global(Args[1].Addr))^;
bcMOV_u64_u16_lg: PUInt64(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Global(Args[1].Addr))^;
bcMOV_u64_i32_lg: PUInt64(Stack.Local(Args[0].Addr))^ := PInt32(Stack.Global(Args[1].Addr))^;
bcMOV_u64_u32_lg: PUInt64(Stack.Local(Args[0].Addr))^ := PUInt32(Stack.Global(Args[1].Addr))^;
bcMOV_u64_i64_lg: PUInt64(Stack.Local(Args[0].Addr))^ := PInt64(Stack.Global(Args[1].Addr))^;
bcMOV_u64_u64_lg: PUInt64(Stack.Local(Args[0].Addr))^ := PUInt64(Stack.Global(Args[1].Addr))^;

bcMOV_f32_i8_lg: PFloat32(Stack.Local(Args[0].Addr))^ := PInt8(Stack.Global(Args[1].Addr))^;
bcMOV_f32_u8_lg: PFloat32(Stack.Local(Args[0].Addr))^ := PUInt8(Stack.Global(Args[1].Addr))^;
bcMOV_f32_i16_lg: PFloat32(Stack.Local(Args[0].Addr))^ := PInt16(Stack.Global(Args[1].Addr))^;
bcMOV_f32_u16_lg: PFloat32(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Global(Args[1].Addr))^;
bcMOV_f32_i32_lg: PFloat32(Stack.Local(Args[0].Addr))^ := PInt32(Stack.Global(Args[1].Addr))^;
bcMOV_f32_u32_lg: PFloat32(Stack.Local(Args[0].Addr))^ := PUInt32(Stack.Global(Args[1].Addr))^;
bcMOV_f32_i64_lg: PFloat32(Stack.Local(Args[0].Addr))^ := PInt64(Stack.Global(Args[1].Addr))^;
bcMOV_f32_u64_lg: PFloat32(Stack.Local(Args[0].Addr))^ := PUInt64(Stack.Global(Args[1].Addr))^;
bcMOV_f32_f32_lg: PFloat32(Stack.Local(Args[0].Addr))^ := PFloat32(Stack.Global(Args[1].Addr))^;
bcMOV_f32_f64_lg: PFloat32(Stack.Local(Args[0].Addr))^ := PFloat64(Stack.Global(Args[1].Addr))^;

bcMOV_f64_i8_lg: PFloat64(Stack.Local(Args[0].Addr))^ := PInt8(Stack.Global(Args[1].Addr))^;
bcMOV_f64_u8_lg: PFloat64(Stack.Local(Args[0].Addr))^ := PUInt8(Stack.Global(Args[1].Addr))^;
bcMOV_f64_i16_lg: PFloat64(Stack.Local(Args[0].Addr))^ := PInt16(Stack.Global(Args[1].Addr))^;
bcMOV_f64_u16_lg: PFloat64(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Global(Args[1].Addr))^;
bcMOV_f64_i32_lg: PFloat64(Stack.Local(Args[0].Addr))^ := PInt32(Stack.Global(Args[1].Addr))^;
bcMOV_f64_u32_lg: PFloat64(Stack.Local(Args[0].Addr))^ := PUInt32(Stack.Global(Args[1].Addr))^;
bcMOV_f64_i64_lg: PFloat64(Stack.Local(Args[0].Addr))^ := PInt64(Stack.Global(Args[1].Addr))^;
bcMOV_f64_u64_lg: PFloat64(Stack.Local(Args[0].Addr))^ := PUInt64(Stack.Global(Args[1].Addr))^;
bcMOV_f64_f32_lg: PFloat64(Stack.Local(Args[0].Addr))^ := PFloat32(Stack.Global(Args[1].Addr))^;
bcMOV_f64_f64_lg: PFloat64(Stack.Local(Args[0].Addr))^ := PFloat64(Stack.Global(Args[1].Addr))^;

// --- Immediate Source, Local Destination (bcMOV_SourceType_DestType_li) ---
bcMOV_i8_i8_li: PInt8(Stack.Local(Args[0].Addr))^ := Int8(Args[1].Arg);
bcMOV_i8_u8_li: PInt8(Stack.Local(Args[0].Addr))^ := UInt8(Args[1].Arg);
bcMOV_i8_i16_li: PInt8(Stack.Local(Args[0].Addr))^ := Int16(Args[1].Arg);
bcMOV_i8_u16_li: PInt8(Stack.Local(Args[0].Addr))^ := UInt16(Args[1].Arg);
bcMOV_i8_i32_li: PInt8(Stack.Local(Args[0].Addr))^ := Int32(Args[1].Arg);
bcMOV_i8_u32_li: PInt8(Stack.Local(Args[0].Addr))^ := UInt32(Args[1].Arg);
bcMOV_i8_i64_li: PInt8(Stack.Local(Args[0].Addr))^ := Int64(Args[1].Arg);
bcMOV_i8_u64_li: PInt8(Stack.Local(Args[0].Addr))^ := UInt64(Args[1].Arg);

bcMOV_u8_i8_li: PUInt8(Stack.Local(Args[0].Addr))^ := Int8(Args[1].Arg);
bcMOV_u8_u8_li: PUInt8(Stack.Local(Args[0].Addr))^ := UInt8(Args[1].Arg);
bcMOV_u8_i16_li: PUInt8(Stack.Local(Args[0].Addr))^ := Int16(Args[1].Arg);
bcMOV_u8_u16_li: PUInt8(Stack.Local(Args[0].Addr))^ := UInt16(Args[1].Arg);
bcMOV_u8_i32_li: PUInt8(Stack.Local(Args[0].Addr))^ := Int32(Args[1].Arg);
bcMOV_u8_u32_li: PUInt8(Stack.Local(Args[0].Addr))^ := UInt32(Args[1].Arg);
bcMOV_u8_i64_li: PUInt8(Stack.Local(Args[0].Addr))^ := Int64(Args[1].Arg);
bcMOV_u8_u64_li: PUInt8(Stack.Local(Args[0].Addr))^ := UInt64(Args[1].Arg);

bcMOV_i16_i8_li: PInt16(Stack.Local(Args[0].Addr))^ := Int8(Args[1].Arg);
bcMOV_i16_u8_li: PInt16(Stack.Local(Args[0].Addr))^ := UInt8(Args[1].Arg);
bcMOV_i16_i16_li: PInt16(Stack.Local(Args[0].Addr))^ := Int16(Args[1].Arg);
bcMOV_i16_u16_li: PInt16(Stack.Local(Args[0].Addr))^ := UInt16(Args[1].Arg);
bcMOV_i16_i32_li: PInt16(Stack.Local(Args[0].Addr))^ := Int32(Args[1].Arg);
bcMOV_i16_u32_li: PInt16(Stack.Local(Args[0].Addr))^ := UInt32(Args[1].Arg);
bcMOV_i16_i64_li: PInt16(Stack.Local(Args[0].Addr))^ := Int64(Args[1].Arg);
bcMOV_i16_u64_li: PInt16(Stack.Local(Args[0].Addr))^ := UInt64(Args[1].Arg);

bcMOV_u16_i8_li: PUInt16(Stack.Local(Args[0].Addr))^ := Int8(Args[1].Arg);
bcMOV_u16_u8_li: PUInt16(Stack.Local(Args[0].Addr))^ := UInt8(Args[1].Arg);
bcMOV_u16_i16_li: PUInt16(Stack.Local(Args[0].Addr))^ := Int16(Args[1].Arg);
bcMOV_u16_u16_li: PUInt16(Stack.Local(Args[0].Addr))^ := UInt16(Args[1].Arg);
bcMOV_u16_i32_li: PUInt16(Stack.Local(Args[0].Addr))^ := Int32(Args[1].Arg);
bcMOV_u16_u32_li: PUInt16(Stack.Local(Args[0].Addr))^ := UInt32(Args[1].Arg);
bcMOV_u16_i64_li: PUInt16(Stack.Local(Args[0].Addr))^ := Int64(Args[1].Arg);
bcMOV_u16_u64_li: PUInt16(Stack.Local(Args[0].Addr))^ := UInt64(Args[1].Arg);

bcMOV_i32_i8_li: PInt32(Stack.Local(Args[0].Addr))^ := Int8(Args[1].Arg);
bcMOV_i32_u8_li: PInt32(Stack.Local(Args[0].Addr))^ := UInt8(Args[1].Arg);
bcMOV_i32_i16_li: PInt32(Stack.Local(Args[0].Addr))^ := Int16(Args[1].Arg);
bcMOV_i32_u16_li: PInt32(Stack.Local(Args[0].Addr))^ := UInt16(Args[1].Arg);
bcMOV_i32_i32_li: PInt32(Stack.Local(Args[0].Addr))^ := Int32(Args[1].Arg);
bcMOV_i32_u32_li: PInt32(Stack.Local(Args[0].Addr))^ := UInt32(Args[1].Arg);
bcMOV_i32_i64_li: PInt32(Stack.Local(Args[0].Addr))^ := Int64(Args[1].Arg);
bcMOV_i32_u64_li: PInt32(Stack.Local(Args[0].Addr))^ := UInt64(Args[1].Arg);

bcMOV_u32_i8_li: PUInt32(Stack.Local(Args[0].Addr))^ := Int8(Args[1].Arg);
bcMOV_u32_u8_li: PUInt32(Stack.Local(Args[0].Addr))^ := UInt8(Args[1].Arg);
bcMOV_u32_i16_li: PUInt32(Stack.Local(Args[0].Addr))^ := Int16(Args[1].Arg);
bcMOV_u32_u16_li: PUInt32(Stack.Local(Args[0].Addr))^ := UInt16(Args[1].Arg);
bcMOV_u32_i32_li: PUInt32(Stack.Local(Args[0].Addr))^ := Int32(Args[1].Arg);
bcMOV_u32_u32_li: PUInt32(Stack.Local(Args[0].Addr))^ := UInt32(Args[1].Arg);
bcMOV_u32_i64_li: PUInt32(Stack.Local(Args[0].Addr))^ := Int64(Args[1].Arg);
bcMOV_u32_u64_li: PUInt32(Stack.Local(Args[0].Addr))^ := UInt64(Args[1].Arg);

bcMOV_i64_i8_li: PInt64(Stack.Local(Args[0].Addr))^ := Int8(Args[1].Arg);
bcMOV_i64_u8_li: PInt64(Stack.Local(Args[0].Addr))^ := UInt8(Args[1].Arg);
bcMOV_i64_i16_li: PInt64(Stack.Local(Args[0].Addr))^ := Int16(Args[1].Arg);
bcMOV_i64_u16_li: PInt64(Stack.Local(Args[0].Addr))^ := UInt16(Args[1].Arg);
bcMOV_i64_i32_li: PInt64(Stack.Local(Args[0].Addr))^ := Int32(Args[1].Arg);
bcMOV_i64_u32_li: PInt64(Stack.Local(Args[0].Addr))^ := UInt32(Args[1].Arg);
bcMOV_i64_i64_li: PInt64(Stack.Local(Args[0].Addr))^ := Int64(Args[1].Arg);
bcMOV_i64_u64_li: PInt64(Stack.Local(Args[0].Addr))^ := UInt64(Args[1].Arg);

bcMOV_u64_i8_li: PUInt64(Stack.Local(Args[0].Addr))^ := Int8(Args[1].Arg);
bcMOV_u64_u8_li: PUInt64(Stack.Local(Args[0].Addr))^ := UInt8(Args[1].Arg);
bcMOV_u64_i16_li: PUInt64(Stack.Local(Args[0].Addr))^ := Int16(Args[1].Arg);
bcMOV_u64_u16_li: PUInt64(Stack.Local(Args[0].Addr))^ := UInt16(Args[1].Arg);
bcMOV_u64_i32_li: PUInt64(Stack.Local(Args[0].Addr))^ := Int32(Args[1].Arg);
bcMOV_u64_u32_li: PUInt64(Stack.Local(Args[0].Addr))^ := UInt32(Args[1].Arg);
bcMOV_u64_i64_li: PUInt64(Stack.Local(Args[0].Addr))^ := Int64(Args[1].Arg);
bcMOV_u64_u64_li: PUInt64(Stack.Local(Args[0].Addr))^ := UInt64(Args[1].Arg);

bcMOV_f32_i8_li: PFloat32(Stack.Local(Args[0].Addr))^ := Int8(Args[1].Arg);
bcMOV_f32_u8_li: PFloat32(Stack.Local(Args[0].Addr))^ := UInt8(Args[1].Arg);
bcMOV_f32_i16_li: PFloat32(Stack.Local(Args[0].Addr))^ := Int16(Args[1].Arg);
bcMOV_f32_u16_li: PFloat32(Stack.Local(Args[0].Addr))^ := UInt16(Args[1].Arg);
bcMOV_f32_i32_li: PFloat32(Stack.Local(Args[0].Addr))^ := Int32(Args[1].Arg);
bcMOV_f32_u32_li: PFloat32(Stack.Local(Args[0].Addr))^ := UInt32(Args[1].Arg);
bcMOV_f32_i64_li: PFloat32(Stack.Local(Args[0].Addr))^ := Int64(Args[1].Arg);
bcMOV_f32_u64_li: PFloat32(Stack.Local(Args[0].Addr))^ := UInt64(Args[1].Arg);
bcMOV_f32_f32_li: PFloat32(Stack.Local(Args[0].Addr))^ := Float32(Args[1].Arg);
bcMOV_f32_f64_li: PFloat32(Stack.Local(Args[0].Addr))^ := Float64(Args[1].Arg);

bcMOV_f64_i8_li: PFloat64(Stack.Local(Args[0].Addr))^ := Int8(Args[1].Arg);
bcMOV_f64_u8_li: PFloat64(Stack.Local(Args[0].Addr))^ := UInt8(Args[1].Arg);
bcMOV_f64_i16_li: PFloat64(Stack.Local(Args[0].Addr))^ := Int16(Args[1].Arg);
bcMOV_f64_u16_li: PFloat64(Stack.Local(Args[0].Addr))^ := UInt16(Args[1].Arg);
bcMOV_f64_i32_li: PFloat64(Stack.Local(Args[0].Addr))^ := Int32(Args[1].Arg);
bcMOV_f64_u32_li: PFloat64(Stack.Local(Args[0].Addr))^ := UInt32(Args[1].Arg);
bcMOV_f64_i64_li: PFloat64(Stack.Local(Args[0].Addr))^ := Int64(Args[1].Arg);
bcMOV_f64_u64_li: PFloat64(Stack.Local(Args[0].Addr))^ := UInt64(Args[1].Arg);
bcMOV_f64_f32_li: PFloat64(Stack.Local(Args[0].Addr))^ := Float32(Args[1].Arg);
bcMOV_f64_f64_li: PFloat64(Stack.Local(Args[0].Addr))^ := Float64(Args[1].Arg);



 // --- Local Source, Local Destination (bcMOVH_SourceType_DestType_ll) ---
bcMOVH_i8_i8_ll: PInt8(Stack.Local(Args[0].Addr)^)^ := PInt8(Stack.Local(Args[1].Addr))^;
bcMOVH_i8_u8_ll: PInt8(Stack.Local(Args[0].Addr)^)^ := PUInt8(Stack.Local(Args[1].Addr))^;
bcMOVH_i8_i16_ll: PInt8(Stack.Local(Args[0].Addr)^)^ := PInt16(Stack.Local(Args[1].Addr))^;
bcMOVH_i8_u16_ll: PInt8(Stack.Local(Args[0].Addr)^)^ := PUInt16(Stack.Local(Args[1].Addr))^;
bcMOVH_i8_i32_ll: PInt8(Stack.Local(Args[0].Addr)^)^ := PInt32(Stack.Local(Args[1].Addr))^;
bcMOVH_i8_u32_ll: PInt8(Stack.Local(Args[0].Addr)^)^ := PUInt32(Stack.Local(Args[1].Addr))^;
bcMOVH_i8_i64_ll: PInt8(Stack.Local(Args[0].Addr)^)^ := PInt64(Stack.Local(Args[1].Addr))^;
bcMOVH_i8_u64_ll: PInt8(Stack.Local(Args[0].Addr)^)^ := PUInt64(Stack.Local(Args[1].Addr))^;

bcMOVH_u8_i8_ll: PUInt8(Stack.Local(Args[0].Addr)^)^ := PInt8(Stack.Local(Args[1].Addr))^;
bcMOVH_u8_u8_ll: PUInt8(Stack.Local(Args[0].Addr)^)^ := PUInt8(Stack.Local(Args[1].Addr))^;
bcMOVH_u8_i16_ll: PUInt8(Stack.Local(Args[0].Addr)^)^ := PInt16(Stack.Local(Args[1].Addr))^;
bcMOVH_u8_u16_ll: PUInt8(Stack.Local(Args[0].Addr)^)^ := PUInt16(Stack.Local(Args[1].Addr))^;
bcMOVH_u8_i32_ll: PUInt8(Stack.Local(Args[0].Addr)^)^ := PInt32(Stack.Local(Args[1].Addr))^;
bcMOVH_u8_u32_ll: PUInt8(Stack.Local(Args[0].Addr)^)^ := PUInt32(Stack.Local(Args[1].Addr))^;
bcMOVH_u8_i64_ll: PUInt8(Stack.Local(Args[0].Addr)^)^ := PInt64(Stack.Local(Args[1].Addr))^;
bcMOVH_u8_u64_ll: PUInt8(Stack.Local(Args[0].Addr)^)^ := PUInt64(Stack.Local(Args[1].Addr))^;

bcMOVH_i16_i8_ll: PInt16(Stack.Local(Args[0].Addr)^)^ := PInt8(Stack.Local(Args[1].Addr))^;
bcMOVH_i16_u8_ll: PInt16(Stack.Local(Args[0].Addr)^)^ := PUInt8(Stack.Local(Args[1].Addr))^;
bcMOVH_i16_i16_ll: PInt16(Stack.Local(Args[0].Addr)^)^ := PInt16(Stack.Local(Args[1].Addr))^;
bcMOVH_i16_u16_ll: PInt16(Stack.Local(Args[0].Addr)^)^ := PUInt16(Stack.Local(Args[1].Addr))^;
bcMOVH_i16_i32_ll: PInt16(Stack.Local(Args[0].Addr)^)^ := PInt32(Stack.Local(Args[1].Addr))^;
bcMOVH_i16_u32_ll: PInt16(Stack.Local(Args[0].Addr)^)^ := PUInt32(Stack.Local(Args[1].Addr))^;
bcMOVH_i16_i64_ll: PInt16(Stack.Local(Args[0].Addr)^)^ := PInt64(Stack.Local(Args[1].Addr))^;
bcMOVH_i16_u64_ll: PInt16(Stack.Local(Args[0].Addr)^)^ := PUInt64(Stack.Local(Args[1].Addr))^;

bcMOVH_u16_i8_ll: PUInt16(Stack.Local(Args[0].Addr)^)^ := PInt8(Stack.Local(Args[1].Addr))^;
bcMOVH_u16_u8_ll: PUInt16(Stack.Local(Args[0].Addr)^)^ := PUInt8(Stack.Local(Args[1].Addr))^;
bcMOVH_u16_i16_ll: PUInt16(Stack.Local(Args[0].Addr)^)^ := PInt16(Stack.Local(Args[1].Addr))^;
bcMOVH_u16_u16_ll: PUInt16(Stack.Local(Args[0].Addr)^)^ := PUInt16(Stack.Local(Args[1].Addr))^;
bcMOVH_u16_i32_ll: PUInt16(Stack.Local(Args[0].Addr)^)^ := PInt32(Stack.Local(Args[1].Addr))^;
bcMOVH_u16_u32_ll: PUInt16(Stack.Local(Args[0].Addr)^)^ := PUInt32(Stack.Local(Args[1].Addr))^;
bcMOVH_u16_i64_ll: PUInt16(Stack.Local(Args[0].Addr)^)^ := PInt64(Stack.Local(Args[1].Addr))^;
bcMOVH_u16_u64_ll: PUInt16(Stack.Local(Args[0].Addr)^)^ := PUInt64(Stack.Local(Args[1].Addr))^;

bcMOVH_i32_i8_ll: PInt32(Stack.Local(Args[0].Addr)^)^ := PInt8(Stack.Local(Args[1].Addr))^;
bcMOVH_i32_u8_ll: PInt32(Stack.Local(Args[0].Addr)^)^ := PUInt8(Stack.Local(Args[1].Addr))^;
bcMOVH_i32_i16_ll: PInt32(Stack.Local(Args[0].Addr)^)^ := PInt16(Stack.Local(Args[1].Addr))^;
bcMOVH_i32_u16_ll: PInt32(Stack.Local(Args[0].Addr)^)^ := PUInt16(Stack.Local(Args[1].Addr))^;
bcMOVH_i32_i32_ll: PInt32(Stack.Local(Args[0].Addr)^)^ := PInt32(Stack.Local(Args[1].Addr))^;
bcMOVH_i32_u32_ll: PInt32(Stack.Local(Args[0].Addr)^)^ := PUInt32(Stack.Local(Args[1].Addr))^;
bcMOVH_i32_i64_ll: PInt32(Stack.Local(Args[0].Addr)^)^ := PInt64(Stack.Local(Args[1].Addr))^;
bcMOVH_i32_u64_ll: PInt32(Stack.Local(Args[0].Addr)^)^ := PUInt64(Stack.Local(Args[1].Addr))^;

bcMOVH_u32_i8_ll: PUInt32(Stack.Local(Args[0].Addr)^)^ := PInt8(Stack.Local(Args[1].Addr))^;
bcMOVH_u32_u8_ll: PUInt32(Stack.Local(Args[0].Addr)^)^ := PUInt8(Stack.Local(Args[1].Addr))^;
bcMOVH_u32_i16_ll: PUInt32(Stack.Local(Args[0].Addr)^)^ := PInt16(Stack.Local(Args[1].Addr))^;
bcMOVH_u32_u16_ll: PUInt32(Stack.Local(Args[0].Addr)^)^ := PUInt16(Stack.Local(Args[1].Addr))^;
bcMOVH_u32_i32_ll: PUInt32(Stack.Local(Args[0].Addr)^)^ := PInt32(Stack.Local(Args[1].Addr))^;
bcMOVH_u32_u32_ll: PUInt32(Stack.Local(Args[0].Addr)^)^ := PUInt32(Stack.Local(Args[1].Addr))^;
bcMOVH_u32_i64_ll: PUInt32(Stack.Local(Args[0].Addr)^)^ := PInt64(Stack.Local(Args[1].Addr))^;
bcMOVH_u32_u64_ll: PUInt32(Stack.Local(Args[0].Addr)^)^ := PUInt64(Stack.Local(Args[1].Addr))^;

bcMOVH_i64_i8_ll: PInt64(Stack.Local(Args[0].Addr)^)^ := PInt8(Stack.Local(Args[1].Addr))^;
bcMOVH_i64_u8_ll: PInt64(Stack.Local(Args[0].Addr)^)^ := PUInt8(Stack.Local(Args[1].Addr))^;
bcMOVH_i64_i16_ll: PInt64(Stack.Local(Args[0].Addr)^)^ := PInt16(Stack.Local(Args[1].Addr))^;
bcMOVH_i64_u16_ll: PInt64(Stack.Local(Args[0].Addr)^)^ := PUInt16(Stack.Local(Args[1].Addr))^;
bcMOVH_i64_i32_ll: PInt64(Stack.Local(Args[0].Addr)^)^ := PInt32(Stack.Local(Args[1].Addr))^;
bcMOVH_i64_u32_ll: PInt64(Stack.Local(Args[0].Addr)^)^ := PUInt32(Stack.Local(Args[1].Addr))^;
bcMOVH_i64_i64_ll: PInt64(Stack.Local(Args[0].Addr)^)^ := PInt64(Stack.Local(Args[1].Addr))^;
bcMOVH_i64_u64_ll: PInt64(Stack.Local(Args[0].Addr)^)^ := PUInt64(Stack.Local(Args[1].Addr))^;

bcMOVH_u64_i8_ll: PUInt64(Stack.Local(Args[0].Addr)^)^ := PInt8(Stack.Local(Args[1].Addr))^;
bcMOVH_u64_u8_ll: PUInt64(Stack.Local(Args[0].Addr)^)^ := PUInt8(Stack.Local(Args[1].Addr))^;
bcMOVH_u64_i16_ll: PUInt64(Stack.Local(Args[0].Addr)^)^ := PInt16(Stack.Local(Args[1].Addr))^;
bcMOVH_u64_u16_ll: PUInt64(Stack.Local(Args[0].Addr)^)^ := PUInt16(Stack.Local(Args[1].Addr))^;
bcMOVH_u64_i32_ll: PUInt64(Stack.Local(Args[0].Addr)^)^ := PInt32(Stack.Local(Args[1].Addr))^;
bcMOVH_u64_u32_ll: PUInt64(Stack.Local(Args[0].Addr)^)^ := PUInt32(Stack.Local(Args[1].Addr))^;
bcMOVH_u64_i64_ll: PUInt64(Stack.Local(Args[0].Addr)^)^ := PInt64(Stack.Local(Args[1].Addr))^;
bcMOVH_u64_u64_ll: PUInt64(Stack.Local(Args[0].Addr)^)^ := PUInt64(Stack.Local(Args[1].Addr))^;

bcMOVH_f32_i8_ll: PFloat32(Stack.Local(Args[0].Addr)^)^ := PInt8(Stack.Local(Args[1].Addr))^;
bcMOVH_f32_u8_ll: PFloat32(Stack.Local(Args[0].Addr)^)^ := PUInt8(Stack.Local(Args[1].Addr))^;
bcMOVH_f32_i16_ll: PFloat32(Stack.Local(Args[0].Addr)^)^ := PInt16(Stack.Local(Args[1].Addr))^;
bcMOVH_f32_u16_ll: PFloat32(Stack.Local(Args[0].Addr)^)^ := PUInt16(Stack.Local(Args[1].Addr))^;
bcMOVH_f32_i32_ll: PFloat32(Stack.Local(Args[0].Addr)^)^ := PInt32(Stack.Local(Args[1].Addr))^;
bcMOVH_f32_u32_ll: PFloat32(Stack.Local(Args[0].Addr)^)^ := PUInt32(Stack.Local(Args[1].Addr))^;
bcMOVH_f32_i64_ll: PFloat32(Stack.Local(Args[0].Addr)^)^ := PInt64(Stack.Local(Args[1].Addr))^;
bcMOVH_f32_u64_ll: PFloat32(Stack.Local(Args[0].Addr)^)^ := PUInt64(Stack.Local(Args[1].Addr))^;
bcMOVH_f32_f32_ll: PFloat32(Stack.Local(Args[0].Addr)^)^ := PFloat32(Stack.Local(Args[1].Addr))^;
bcMOVH_f32_f64_ll: PFloat32(Stack.Local(Args[0].Addr)^)^ := PFloat64(Stack.Local(Args[1].Addr))^;

bcMOVH_f64_i8_ll: PFloat64(Stack.Local(Args[0].Addr)^)^ := PInt8(Stack.Local(Args[1].Addr))^;
bcMOVH_f64_u8_ll: PFloat64(Stack.Local(Args[0].Addr)^)^ := PUInt8(Stack.Local(Args[1].Addr))^;
bcMOVH_f64_i16_ll: PFloat64(Stack.Local(Args[0].Addr)^)^ := PInt16(Stack.Local(Args[1].Addr))^;
bcMOVH_f64_u16_ll: PFloat64(Stack.Local(Args[0].Addr)^)^ := PUInt16(Stack.Local(Args[1].Addr))^;
bcMOVH_f64_i32_ll: PFloat64(Stack.Local(Args[0].Addr)^)^ := PInt32(Stack.Local(Args[1].Addr))^;
bcMOVH_f64_u32_ll: PFloat64(Stack.Local(Args[0].Addr)^)^ := PUInt32(Stack.Local(Args[1].Addr))^;
bcMOVH_f64_i64_ll: PFloat64(Stack.Local(Args[0].Addr)^)^ := PInt64(Stack.Local(Args[1].Addr))^;
bcMOVH_f64_u64_ll: PFloat64(Stack.Local(Args[0].Addr)^)^ := PUInt64(Stack.Local(Args[1].Addr))^;
bcMOVH_f64_f32_ll: PFloat64(Stack.Local(Args[0].Addr)^)^ := PFloat32(Stack.Local(Args[1].Addr))^;
bcMOVH_f64_f64_ll: PFloat64(Stack.Local(Args[0].Addr)^)^ := PFloat64(Stack.Local(Args[1].Addr))^;

// --- Global Source, Local Destination (bcMOVH_SourceType_DestType_lg) ---
bcMOVH_i8_i8_lg: PInt8(Stack.Local(Args[0].Addr)^)^ := PInt8(Stack.Global(Args[1].Addr))^;
bcMOVH_i8_u8_lg: PInt8(Stack.Local(Args[0].Addr)^)^ := PUInt8(Stack.Global(Args[1].Addr))^;
bcMOVH_i8_i16_lg: PInt8(Stack.Local(Args[0].Addr)^)^ := PInt16(Stack.Global(Args[1].Addr))^;
bcMOVH_i8_u16_lg: PInt8(Stack.Local(Args[0].Addr)^)^ := PUInt16(Stack.Global(Args[1].Addr))^;
bcMOVH_i8_i32_lg: PInt8(Stack.Local(Args[0].Addr)^)^ := PInt32(Stack.Global(Args[1].Addr))^;
bcMOVH_i8_u32_lg: PInt8(Stack.Local(Args[0].Addr)^)^ := PUInt32(Stack.Global(Args[1].Addr))^;
bcMOVH_i8_i64_lg: PInt8(Stack.Local(Args[0].Addr)^)^ := PInt64(Stack.Global(Args[1].Addr))^;
bcMOVH_i8_u64_lg: PInt8(Stack.Local(Args[0].Addr)^)^ := PUInt64(Stack.Global(Args[1].Addr))^;

bcMOVH_u8_i8_lg: PUInt8(Stack.Local(Args[0].Addr)^)^ := PInt8(Stack.Global(Args[1].Addr))^;
bcMOVH_u8_u8_lg: PUInt8(Stack.Local(Args[0].Addr)^)^ := PUInt8(Stack.Global(Args[1].Addr))^;
bcMOVH_u8_i16_lg: PUInt8(Stack.Local(Args[0].Addr)^)^ := PInt16(Stack.Global(Args[1].Addr))^;
bcMOVH_u8_u16_lg: PUInt8(Stack.Local(Args[0].Addr)^)^ := PUInt16(Stack.Global(Args[1].Addr))^;
bcMOVH_u8_i32_lg: PUInt8(Stack.Local(Args[0].Addr)^)^ := PInt32(Stack.Global(Args[1].Addr))^;
bcMOVH_u8_u32_lg: PUInt8(Stack.Local(Args[0].Addr)^)^ := PUInt32(Stack.Global(Args[1].Addr))^;
bcMOVH_u8_i64_lg: PUInt8(Stack.Local(Args[0].Addr)^)^ := PInt64(Stack.Global(Args[1].Addr))^;
bcMOVH_u8_u64_lg: PUInt8(Stack.Local(Args[0].Addr)^)^ := PUInt64(Stack.Global(Args[1].Addr))^;

bcMOVH_i16_i8_lg: PInt16(Stack.Local(Args[0].Addr)^)^ := PInt8(Stack.Global(Args[1].Addr))^;
bcMOVH_i16_u8_lg: PInt16(Stack.Local(Args[0].Addr)^)^ := PUInt8(Stack.Global(Args[1].Addr))^;
bcMOVH_i16_i16_lg: PInt16(Stack.Local(Args[0].Addr)^)^ := PInt16(Stack.Global(Args[1].Addr))^;
bcMOVH_i16_u16_lg: PInt16(Stack.Local(Args[0].Addr)^)^ := PUInt16(Stack.Global(Args[1].Addr))^;
bcMOVH_i16_i32_lg: PInt16(Stack.Local(Args[0].Addr)^)^ := PInt32(Stack.Global(Args[1].Addr))^;
bcMOVH_i16_u32_lg: PInt16(Stack.Local(Args[0].Addr)^)^ := PUInt32(Stack.Global(Args[1].Addr))^;
bcMOVH_i16_i64_lg: PInt16(Stack.Local(Args[0].Addr)^)^ := PInt64(Stack.Global(Args[1].Addr))^;
bcMOVH_i16_u64_lg: PInt16(Stack.Local(Args[0].Addr)^)^ := PUInt64(Stack.Global(Args[1].Addr))^;

bcMOVH_u16_i8_lg: PUInt16(Stack.Local(Args[0].Addr)^)^ := PInt8(Stack.Global(Args[1].Addr))^;
bcMOVH_u16_u8_lg: PUInt16(Stack.Local(Args[0].Addr)^)^ := PUInt8(Stack.Global(Args[1].Addr))^;
bcMOVH_u16_i16_lg: PUInt16(Stack.Local(Args[0].Addr)^)^ := PInt16(Stack.Global(Args[1].Addr))^;
bcMOVH_u16_u16_lg: PUInt16(Stack.Local(Args[0].Addr))^ := PUInt16(Stack.Global(Args[1].Addr))^;
bcMOVH_u16_i32_lg: PUInt16(Stack.Local(Args[0].Addr)^)^ := PInt32(Stack.Global(Args[1].Addr))^;
bcMOVH_u16_u32_lg: PUInt16(Stack.Local(Args[0].Addr)^)^ := PUInt32(Stack.Global(Args[1].Addr))^;
bcMOVH_u16_i64_lg: PUInt16(Stack.Local(Args[0].Addr)^)^ := PInt64(Stack.Global(Args[1].Addr))^;
bcMOVH_u16_u64_lg: PUInt16(Stack.Local(Args[0].Addr)^)^ := PUInt64(Stack.Global(Args[1].Addr))^;

bcMOVH_i32_i8_lg: PInt32(Stack.Local(Args[0].Addr)^)^ := PInt8(Stack.Global(Args[1].Addr))^;
bcMOVH_i32_u8_lg: PInt32(Stack.Local(Args[0].Addr)^)^ := PUInt8(Stack.Global(Args[1].Addr))^;
bcMOVH_i32_i16_lg: PInt32(Stack.Local(Args[0].Addr)^)^ := PInt16(Stack.Global(Args[1].Addr))^;
bcMOVH_i32_u16_lg: PInt32(Stack.Local(Args[0].Addr)^)^ := PUInt16(Stack.Global(Args[1].Addr))^;
bcMOVH_i32_i32_lg: PInt32(Stack.Local(Args[0].Addr)^)^ := PInt32(Stack.Global(Args[1].Addr))^;
bcMOVH_i32_u32_lg: PInt32(Stack.Local(Args[0].Addr)^)^ := PUInt32(Stack.Global(Args[1].Addr))^;
bcMOVH_i32_i64_lg: PInt32(Stack.Local(Args[0].Addr)^)^ := PInt64(Stack.Global(Args[1].Addr))^;
bcMOVH_i32_u64_lg: PInt32(Stack.Local(Args[0].Addr)^)^ := PUInt64(Stack.Global(Args[1].Addr))^;

bcMOVH_u32_i8_lg: PUInt32(Stack.Local(Args[0].Addr)^)^ := PInt8(Stack.Global(Args[1].Addr))^;
bcMOVH_u32_u8_lg: PUInt32(Stack.Local(Args[0].Addr)^)^ := PUInt8(Stack.Global(Args[1].Addr))^;
bcMOVH_u32_i16_lg: PUInt32(Stack.Local(Args[0].Addr)^)^ := PInt16(Stack.Global(Args[1].Addr))^;
bcMOVH_u32_u16_lg: PUInt32(Stack.Local(Args[0].Addr)^)^ := PUInt16(Stack.Global(Args[1].Addr))^;
bcMOVH_u32_i32_lg: PUInt32(Stack.Local(Args[0].Addr)^)^ := PInt32(Stack.Global(Args[1].Addr))^;
bcMOVH_u32_u32_lg: PUInt32(Stack.Local(Args[0].Addr)^)^ := PUInt32(Stack.Global(Args[1].Addr))^;
bcMOVH_u32_i64_lg: PUInt32(Stack.Local(Args[0].Addr)^)^ := PInt64(Stack.Global(Args[1].Addr))^;
bcMOVH_u32_u64_lg: PUInt32(Stack.Local(Args[0].Addr)^)^ := PUInt64(Stack.Global(Args[1].Addr))^;

bcMOVH_i64_i8_lg: PInt64(Stack.Local(Args[0].Addr)^)^ := PInt8(Stack.Global(Args[1].Addr))^;
bcMOVH_i64_u8_lg: PInt64(Stack.Local(Args[0].Addr)^)^ := PUInt8(Stack.Global(Args[1].Addr))^;
bcMOVH_i64_i16_lg: PInt64(Stack.Local(Args[0].Addr)^)^ := PInt16(Stack.Global(Args[1].Addr))^;
bcMOVH_i64_u16_lg: PInt64(Stack.Local(Args[0].Addr)^)^ := PUInt16(Stack.Global(Args[1].Addr))^;
bcMOVH_i64_i32_lg: PInt64(Stack.Local(Args[0].Addr)^)^ := PInt32(Stack.Global(Args[1].Addr))^;
bcMOVH_i64_u32_lg: PInt64(Stack.Local(Args[0].Addr)^)^ := PUInt32(Stack.Global(Args[1].Addr))^;
bcMOVH_i64_i64_lg: PInt64(Stack.Local(Args[0].Addr)^)^ := PInt64(Stack.Global(Args[1].Addr))^;
bcMOVH_i64_u64_lg: PInt64(Stack.Local(Args[0].Addr)^)^ := PUInt64(Stack.Global(Args[1].Addr))^;

bcMOVH_u64_i8_lg: PUInt64(Stack.Local(Args[0].Addr)^)^ := PInt8(Stack.Global(Args[1].Addr))^;
bcMOVH_u64_u8_lg: PUInt64(Stack.Local(Args[0].Addr)^)^ := PUInt8(Stack.Global(Args[1].Addr))^;
bcMOVH_u64_i16_lg: PUInt64(Stack.Local(Args[0].Addr)^)^ := PInt16(Stack.Global(Args[1].Addr))^;
bcMOVH_u64_u16_lg: PUInt64(Stack.Local(Args[0].Addr)^)^ := PUInt16(Stack.Global(Args[1].Addr))^;
bcMOVH_u64_i32_lg: PUInt64(Stack.Local(Args[0].Addr)^)^ := PInt32(Stack.Global(Args[1].Addr))^;
bcMOVH_u64_u32_lg: PUInt64(Stack.Local(Args[0].Addr)^)^ := PUInt32(Stack.Global(Args[1].Addr))^;
bcMOVH_u64_i64_lg: PUInt64(Stack.Local(Args[0].Addr)^)^ := PInt64(Stack.Global(Args[1].Addr))^;
bcMOVH_u64_u64_lg: PUInt64(Stack.Local(Args[0].Addr)^)^ := PUInt64(Stack.Global(Args[1].Addr))^;

bcMOVH_f32_i8_lg: PFloat32(Stack.Local(Args[0].Addr)^)^ := PInt8(Stack.Global(Args[1].Addr))^;
bcMOVH_f32_u8_lg: PFloat32(Stack.Local(Args[0].Addr)^)^ := PUInt8(Stack.Global(Args[1].Addr))^;
bcMOVH_f32_i16_lg: PFloat32(Stack.Local(Args[0].Addr)^)^ := PInt16(Stack.Global(Args[1].Addr))^;
bcMOVH_f32_u16_lg: PFloat32(Stack.Local(Args[0].Addr)^)^ := PUInt16(Stack.Global(Args[1].Addr))^;
bcMOVH_f32_i32_lg: PFloat32(Stack.Local(Args[0].Addr)^)^ := PInt32(Stack.Global(Args[1].Addr))^;
bcMOVH_f32_u32_lg: PFloat32(Stack.Local(Args[0].Addr)^)^ := PUInt32(Stack.Global(Args[1].Addr))^;
bcMOVH_f32_i64_lg: PFloat32(Stack.Local(Args[0].Addr)^)^ := PInt64(Stack.Global(Args[1].Addr))^;
bcMOVH_f32_u64_lg: PFloat32(Stack.Local(Args[0].Addr)^)^ := PUInt64(Stack.Global(Args[1].Addr))^;
bcMOVH_f32_f32_lg: PFloat32(Stack.Local(Args[0].Addr)^)^ := PFloat32(Stack.Global(Args[1].Addr))^;
bcMOVH_f32_f64_lg: PFloat32(Stack.Local(Args[0].Addr)^)^ := PFloat64(Stack.Global(Args[1].Addr))^;

bcMOVH_f64_i8_lg: PFloat64(Stack.Local(Args[0].Addr)^)^ := PInt8(Stack.Global(Args[1].Addr))^;
bcMOVH_f64_u8_lg: PFloat64(Stack.Local(Args[0].Addr)^)^ := PUInt8(Stack.Global(Args[1].Addr))^;
bcMOVH_f64_i16_lg: PFloat64(Stack.Local(Args[0].Addr)^)^ := PInt16(Stack.Global(Args[1].Addr))^;
bcMOVH_f64_u16_lg: PFloat64(Stack.Local(Args[0].Addr)^)^ := PUInt16(Stack.Global(Args[1].Addr))^;
bcMOVH_f64_i32_lg: PFloat64(Stack.Local(Args[0].Addr)^)^ := PInt32(Stack.Global(Args[1].Addr))^;
bcMOVH_f64_u32_lg: PFloat64(Stack.Local(Args[0].Addr)^)^ := PUInt32(Stack.Global(Args[1].Addr))^;
bcMOVH_f64_i64_lg: PFloat64(Stack.Local(Args[0].Addr)^)^ := PInt64(Stack.Global(Args[1].Addr))^;
bcMOVH_f64_u64_lg: PFloat64(Stack.Local(Args[0].Addr)^)^ := PUInt64(Stack.Global(Args[1].Addr))^;
bcMOVH_f64_f32_lg: PFloat64(Stack.Local(Args[0].Addr)^)^ := PFloat32(Stack.Global(Args[1].Addr))^;
bcMOVH_f64_f64_lg: PFloat64(Stack.Local(Args[0].Addr)^)^ := PFloat64(Stack.Global(Args[1].Addr))^;

// --- Immediate Source, Local Destination (bcMOVH_SourceType_DestType_li) ---
bcMOVH_i8_i8_li: PInt8(Stack.Local(Args[0].Addr)^)^ := Int8(Args[1].Arg);
bcMOVH_i8_u8_li: PInt8(Stack.Local(Args[0].Addr)^)^ := UInt8(Args[1].Arg);
bcMOVH_i8_i16_li: PInt8(Stack.Local(Args[0].Addr)^)^ := Int16(Args[1].Arg);
bcMOVH_i8_u16_li: PInt8(Stack.Local(Args[0].Addr)^)^ := UInt16(Args[1].Arg);
bcMOVH_i8_i32_li: PInt8(Stack.Local(Args[0].Addr)^)^ := Int32(Args[1].Arg);
bcMOVH_i8_u32_li: PInt8(Stack.Local(Args[0].Addr)^)^ := UInt32(Args[1].Arg);
bcMOVH_i8_i64_li: PInt8(Stack.Local(Args[0].Addr)^)^ := Int64(Args[1].Arg);
bcMOVH_i8_u64_li: PInt8(Stack.Local(Args[0].Addr)^)^ := UInt64(Args[1].Arg);

bcMOVH_u8_i8_li: PUInt8(Stack.Local(Args[0].Addr)^)^ := Int8(Args[1].Arg);
bcMOVH_u8_u8_li: PUInt8(Stack.Local(Args[0].Addr)^)^ := UInt8(Args[1].Arg);
bcMOVH_u8_i16_li: PUInt8(Stack.Local(Args[0].Addr)^)^ := Int16(Args[1].Arg);
bcMOVH_u8_u16_li: PUInt8(Stack.Local(Args[0].Addr)^)^ := UInt16(Args[1].Arg);
bcMOVH_u8_i32_li: PUInt8(Stack.Local(Args[0].Addr)^)^ := Int32(Args[1].Arg);
bcMOVH_u8_u32_li: PUInt8(Stack.Local(Args[0].Addr)^)^ := UInt32(Args[1].Arg);
bcMOVH_u8_i64_li: PUInt8(Stack.Local(Args[0].Addr)^)^ := Int64(Args[1].Arg);
bcMOVH_u8_u64_li: PUInt8(Stack.Local(Args[0].Addr)^)^ := UInt64(Args[1].Arg);

bcMOVH_i16_i8_li: PInt16(Stack.Local(Args[0].Addr)^)^ := Int8(Args[1].Arg);
bcMOVH_i16_u8_li: PInt16(Stack.Local(Args[0].Addr)^)^ := UInt8(Args[1].Arg);
bcMOVH_i16_i16_li: PInt16(Stack.Local(Args[0].Addr)^)^ := Int16(Args[1].Arg);
bcMOVH_i16_u16_li: PInt16(Stack.Local(Args[0].Addr)^)^ := UInt16(Args[1].Arg);
bcMOVH_i16_i32_li: PInt16(Stack.Local(Args[0].Addr)^)^ := Int32(Args[1].Arg);
bcMOVH_i16_u32_li: PInt16(Stack.Local(Args[0].Addr)^)^ := UInt32(Args[1].Arg);
bcMOVH_i16_i64_li: PInt16(Stack.Local(Args[0].Addr)^)^ := Int64(Args[1].Arg);
bcMOVH_i16_u64_li: PInt16(Stack.Local(Args[0].Addr)^)^ := UInt64(Args[1].Arg);

bcMOVH_u16_i8_li: PUInt16(Stack.Local(Args[0].Addr)^)^ := Int8(Args[1].Arg);
bcMOVH_u16_u8_li: PUInt16(Stack.Local(Args[0].Addr)^)^ := UInt8(Args[1].Arg);
bcMOVH_u16_i16_li: PUInt16(Stack.Local(Args[0].Addr)^)^ := Int16(Args[1].Arg);
bcMOVH_u16_u16_li: PUInt16(Stack.Local(Args[0].Addr)^)^ := UInt16(Args[1].Arg);
bcMOVH_u16_i32_li: PUInt16(Stack.Local(Args[0].Addr)^)^ := Int32(Args[1].Arg);
bcMOVH_u16_u32_li: PUInt16(Stack.Local(Args[0].Addr)^)^ := UInt32(Args[1].Arg);
bcMOVH_u16_i64_li: PUInt16(Stack.Local(Args[0].Addr)^)^ := Int64(Args[1].Arg);
bcMOVH_u16_u64_li: PUInt16(Stack.Local(Args[0].Addr)^)^ := UInt64(Args[1].Arg);

bcMOVH_i32_i8_li: PInt32(Stack.Local(Args[0].Addr)^)^ := Int8(Args[1].Arg);
bcMOVH_i32_u8_li: PInt32(Stack.Local(Args[0].Addr)^)^ := UInt8(Args[1].Arg);
bcMOVH_i32_i16_li: PInt32(Stack.Local(Args[0].Addr)^)^ := Int16(Args[1].Arg);
bcMOVH_i32_u16_li: PInt32(Stack.Local(Args[0].Addr)^)^ := UInt16(Args[1].Arg);
bcMOVH_i32_i32_li: PInt32(Stack.Local(Args[0].Addr)^)^ := Int32(Args[1].Arg);
bcMOVH_i32_u32_li: PInt32(Stack.Local(Args[0].Addr)^)^ := UInt32(Args[1].Arg);
bcMOVH_i32_i64_li: PInt32(Stack.Local(Args[0].Addr)^)^ := Int64(Args[1].Arg);
bcMOVH_i32_u64_li: PInt32(Stack.Local(Args[0].Addr)^)^ := UInt64(Args[1].Arg);

bcMOVH_u32_i8_li: PUInt32(Stack.Local(Args[0].Addr)^)^ := Int8(Args[1].Arg);
bcMOVH_u32_u8_li: PUInt32(Stack.Local(Args[0].Addr)^)^ := UInt8(Args[1].Arg);
bcMOVH_u32_i16_li: PUInt32(Stack.Local(Args[0].Addr)^)^ := Int16(Args[1].Arg);
bcMOVH_u32_u16_li: PUInt32(Stack.Local(Args[0].Addr)^)^ := UInt16(Args[1].Arg);
bcMOVH_u32_i32_li: PUInt32(Stack.Local(Args[0].Addr)^)^ := Int32(Args[1].Arg);
bcMOVH_u32_u32_li: PUInt32(Stack.Local(Args[0].Addr)^)^ := UInt32(Args[1].Arg);
bcMOVH_u32_i64_li: PUInt32(Stack.Local(Args[0].Addr)^)^ := Int64(Args[1].Arg);
bcMOVH_u32_u64_li: PUInt32(Stack.Local(Args[0].Addr)^)^ := UInt64(Args[1].Arg);

bcMOVH_i64_i8_li: PInt64(Stack.Local(Args[0].Addr)^)^ := Int8(Args[1].Arg);
bcMOVH_i64_u8_li: PInt64(Stack.Local(Args[0].Addr)^)^ := UInt8(Args[1].Arg);
bcMOVH_i64_i16_li: PInt64(Stack.Local(Args[0].Addr)^)^ := Int16(Args[1].Arg);
bcMOVH_i64_u16_li: PInt64(Stack.Local(Args[0].Addr)^)^ := UInt16(Args[1].Arg);
bcMOVH_i64_i32_li: PInt64(Stack.Local(Args[0].Addr)^)^ := Int32(Args[1].Arg);
bcMOVH_i64_u32_li: PInt64(Stack.Local(Args[0].Addr)^)^ := UInt32(Args[1].Arg);
bcMOVH_i64_i64_li: PInt64(Stack.Local(Args[0].Addr)^)^ := Int64(Args[1].Arg);
bcMOVH_i64_u64_li: PInt64(Stack.Local(Args[0].Addr)^)^ := UInt64(Args[1].Arg);

bcMOVH_u64_i8_li: PUInt64(Stack.Local(Args[0].Addr)^)^ := Int8(Args[1].Arg);
bcMOVH_u64_u8_li: PUInt64(Stack.Local(Args[0].Addr)^)^ := UInt8(Args[1].Arg);
bcMOVH_u64_i16_li: PUInt64(Stack.Local(Args[0].Addr)^)^ := Int16(Args[1].Arg);
bcMOVH_u64_u16_li: PUInt64(Stack.Local(Args[0].Addr)^)^ := UInt16(Args[1].Arg);
bcMOVH_u64_i32_li: PUInt64(Stack.Local(Args[0].Addr)^)^ := Int32(Args[1].Arg);
bcMOVH_u64_u32_li: PUInt64(Stack.Local(Args[0].Addr)^)^ := UInt32(Args[1].Arg);
bcMOVH_u64_i64_li: PUInt64(Stack.Local(Args[0].Addr)^)^ := Int64(Args[1].Arg);
bcMOVH_u64_u64_li: PUInt64(Stack.Local(Args[0].Addr)^)^ := UInt64(Args[1].Arg);

bcMOVH_f32_i8_li: PFloat32(Stack.Local(Args[0].Addr)^)^ := Int8(Args[1].Arg);
bcMOVH_f32_u8_li: PFloat32(Stack.Local(Args[0].Addr)^)^ := UInt8(Args[1].Arg);
bcMOVH_f32_i16_li: PFloat32(Stack.Local(Args[0].Addr)^)^ := Int16(Args[1].Arg);
bcMOVH_f32_u16_li: PFloat32(Stack.Local(Args[0].Addr)^)^ := UInt16(Args[1].Arg);
bcMOVH_f32_i32_li: PFloat32(Stack.Local(Args[0].Addr)^)^ := Int32(Args[1].Arg);
bcMOVH_f32_u32_li: PFloat32(Stack.Local(Args[0].Addr)^)^ := UInt32(Args[1].Arg);
bcMOVH_f32_i64_li: PFloat32(Stack.Local(Args[0].Addr)^)^ := Int64(Args[1].Arg);
bcMOVH_f32_u64_li: PFloat32(Stack.Local(Args[0].Addr)^)^ := UInt64(Args[1].Arg);
bcMOVH_f32_f32_li: PFloat32(Stack.Local(Args[0].Addr)^)^ := Float32(Args[1].Arg);
bcMOVH_f32_f64_li: PFloat32(Stack.Local(Args[0].Addr)^)^ := Float64(Args[1].Arg);

bcMOVH_f64_i8_li: PFloat64(Stack.Local(Args[0].Addr)^)^ := Int8(Args[1].Arg);
bcMOVH_f64_u8_li: PFloat64(Stack.Local(Args[0].Addr)^)^ := UInt8(Args[1].Arg);
bcMOVH_f64_i16_li: PFloat64(Stack.Local(Args[0].Addr)^)^ := Int16(Args[1].Arg);
bcMOVH_f64_u16_li: PFloat64(Stack.Local(Args[0].Addr)^)^ := UInt16(Args[1].Arg);
bcMOVH_f64_i32_li: PFloat64(Stack.Local(Args[0].Addr)^)^ := Int32(Args[1].Arg);
bcMOVH_f64_u32_li: PFloat64(Stack.Local(Args[0].Addr)^)^ := UInt32(Args[1].Arg);
bcMOVH_f64_i64_li: PFloat64(Stack.Local(Args[0].Addr)^)^ := Int64(Args[1].Arg);
bcMOVH_f64_u64_li: PFloat64(Stack.Local(Args[0].Addr)^)^ := UInt64(Args[1].Arg);
bcMOVH_f64_f32_li: PFloat64(Stack.Local(Args[0].Addr)^)^ := Float32(Args[1].Arg);
bcMOVH_f64_f64_li: PFloat64(Stack.Local(Args[0].Addr)^)^ := Float64(Args[1].Arg);
