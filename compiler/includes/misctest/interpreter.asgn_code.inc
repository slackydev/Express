// --- Local Source, Local Destination (bcMOV_SourceType_DestType_ll) ---
bcMOV_i8_i8_ll: PInt8(Local(Args[0].Data.Addr))^ := PInt8(Local(Args[1].Data.Addr))^;
bcMOV_i8_u8_ll: PInt8(Local(Args[0].Data.Addr))^ := PUInt8(Local(Args[1].Data.Addr))^;
bcMOV_i8_i16_ll: PInt8(Local(Args[0].Data.Addr))^ := PInt16(Local(Args[1].Data.Addr))^;
bcMOV_i8_u16_ll: PInt8(Local(Args[0].Data.Addr))^ := PUInt16(Local(Args[1].Data.Addr))^;
bcMOV_i8_i32_ll: PInt8(Local(Args[0].Data.Addr))^ := PInt32(Local(Args[1].Data.Addr))^;
bcMOV_i8_u32_ll: PInt8(Local(Args[0].Data.Addr))^ := PUInt32(Local(Args[1].Data.Addr))^;
bcMOV_i8_i64_ll: PInt8(Local(Args[0].Data.Addr))^ := PInt64(Local(Args[1].Data.Addr))^;
bcMOV_i8_u64_ll: PInt8(Local(Args[0].Data.Addr))^ := PUInt64(Local(Args[1].Data.Addr))^;

bcMOV_u8_i8_ll: PUInt8(Local(Args[0].Data.Addr))^ := PInt8(Local(Args[1].Data.Addr))^;
bcMOV_u8_u8_ll: PUInt8(Local(Args[0].Data.Addr))^ := PUInt8(Local(Args[1].Data.Addr))^;
bcMOV_u8_i16_ll: PUInt8(Local(Args[0].Data.Addr))^ := PInt16(Local(Args[1].Data.Addr))^;
bcMOV_u8_u16_ll: PUInt8(Local(Args[0].Data.Addr))^ := PUInt16(Local(Args[1].Data.Addr))^;
bcMOV_u8_i32_ll: PUInt8(Local(Args[0].Data.Addr))^ := PInt32(Local(Args[1].Data.Addr))^;
bcMOV_u8_u32_ll: PUInt8(Local(Args[0].Data.Addr))^ := PUInt32(Local(Args[1].Data.Addr))^;
bcMOV_u8_i64_ll: PUInt8(Local(Args[0].Data.Addr))^ := PInt64(Local(Args[1].Data.Addr))^;
bcMOV_u8_u64_ll: PUInt8(Local(Args[0].Data.Addr))^ := PUInt64(Local(Args[1].Data.Addr))^;

bcMOV_i16_i8_ll: PInt16(Local(Args[0].Data.Addr))^ := PInt8(Local(Args[1].Data.Addr))^;
bcMOV_i16_u8_ll: PInt16(Local(Args[0].Data.Addr))^ := PUInt8(Local(Args[1].Data.Addr))^;
bcMOV_i16_i16_ll: PInt16(Local(Args[0].Data.Addr))^ := PInt16(Local(Args[1].Data.Addr))^;
bcMOV_i16_u16_ll: PInt16(Local(Args[0].Data.Addr))^ := PUInt16(Local(Args[1].Data.Addr))^;
bcMOV_i16_i32_ll: PInt16(Local(Args[0].Data.Addr))^ := PInt32(Local(Args[1].Data.Addr))^;
bcMOV_i16_u32_ll: PInt16(Local(Args[0].Data.Addr))^ := PUInt32(Local(Args[1].Data.Addr))^;
bcMOV_i16_i64_ll: PInt16(Local(Args[0].Data.Addr))^ := PInt64(Local(Args[1].Data.Addr))^;
bcMOV_i16_u64_ll: PInt16(Local(Args[0].Data.Addr))^ := PUInt64(Local(Args[1].Data.Addr))^;

bcMOV_u16_i8_ll: PUInt16(Local(Args[0].Data.Addr))^ := PInt8(Local(Args[1].Data.Addr))^;
bcMOV_u16_u8_ll: PUInt16(Local(Args[0].Data.Addr))^ := PUInt8(Local(Args[1].Data.Addr))^;
bcMOV_u16_i16_ll: PUInt16(Local(Args[0].Data.Addr))^ := PInt16(Local(Args[1].Data.Addr))^;
bcMOV_u16_u16_ll: PUInt16(Local(Args[0].Data.Addr))^ := PUInt16(Local(Args[1].Data.Addr))^;
bcMOV_u16_i32_ll: PUInt16(Local(Args[0].Data.Addr))^ := PInt32(Local(Args[1].Data.Addr))^;
bcMOV_u16_u32_ll: PUInt16(Local(Args[0].Data.Addr))^ := PUInt32(Local(Args[1].Data.Addr))^;
bcMOV_u16_i64_ll: PUInt16(Local(Args[0].Data.Addr))^ := PInt64(Local(Args[1].Data.Addr))^;
bcMOV_u16_u64_ll: PUInt16(Local(Args[0].Data.Addr))^ := PUInt64(Local(Args[1].Data.Addr))^;

bcMOV_i32_i8_ll: PInt32(Local(Args[0].Data.Addr))^ := PInt8(Local(Args[1].Data.Addr))^;
bcMOV_i32_u8_ll: PInt32(Local(Args[0].Data.Addr))^ := PUInt8(Local(Args[1].Data.Addr))^;
bcMOV_i32_i16_ll: PInt32(Local(Args[0].Data.Addr))^ := PInt16(Local(Args[1].Data.Addr))^;
bcMOV_i32_u16_ll: PInt32(Local(Args[0].Data.Addr))^ := PUInt16(Local(Args[1].Data.Addr))^;
bcMOV_i32_i32_ll: PInt32(Local(Args[0].Data.Addr))^ := PInt32(Local(Args[1].Data.Addr))^;
bcMOV_i32_u32_ll: PInt32(Local(Args[0].Data.Addr))^ := PUInt32(Local(Args[1].Data.Addr))^;
bcMOV_i32_i64_ll: PInt32(Local(Args[0].Data.Addr))^ := PInt64(Local(Args[1].Data.Addr))^;
bcMOV_i32_u64_ll: PInt32(Local(Args[0].Data.Addr))^ := PUInt64(Local(Args[1].Data.Addr))^;

bcMOV_u32_i8_ll: PUInt32(Local(Args[0].Data.Addr))^ := PInt8(Local(Args[1].Data.Addr))^;
bcMOV_u32_u8_ll: PUInt32(Local(Args[0].Data.Addr))^ := PUInt8(Local(Args[1].Data.Addr))^;
bcMOV_u32_i16_ll: PUInt32(Local(Args[0].Data.Addr))^ := PInt16(Local(Args[1].Data.Addr))^;
bcMOV_u32_u16_ll: PUInt32(Local(Args[0].Data.Addr))^ := PUInt16(Local(Args[1].Data.Addr))^;
bcMOV_u32_i32_ll: PUInt32(Local(Args[0].Data.Addr))^ := PInt32(Local(Args[1].Data.Addr))^;
bcMOV_u32_u32_ll: PUInt32(Local(Args[0].Data.Addr))^ := PUInt32(Local(Args[1].Data.Addr))^;
bcMOV_u32_i64_ll: PUInt32(Local(Args[0].Data.Addr))^ := PInt64(Local(Args[1].Data.Addr))^;
bcMOV_u32_u64_ll: PUInt32(Local(Args[0].Data.Addr))^ := PUInt64(Local(Args[1].Data.Addr))^;

bcMOV_i64_i8_ll: PInt64(Local(Args[0].Data.Addr))^ := PInt8(Local(Args[1].Data.Addr))^;
bcMOV_i64_u8_ll: PInt64(Local(Args[0].Data.Addr))^ := PUInt8(Local(Args[1].Data.Addr))^;
bcMOV_i64_i16_ll: PInt64(Local(Args[0].Data.Addr))^ := PInt16(Local(Args[1].Data.Addr))^;
bcMOV_i64_u16_ll: PInt64(Local(Args[0].Data.Addr))^ := PUInt16(Local(Args[1].Data.Addr))^;
bcMOV_i64_i32_ll: PInt64(Local(Args[0].Data.Addr))^ := PInt32(Local(Args[1].Data.Addr))^;
bcMOV_i64_u32_ll: PInt64(Local(Args[0].Data.Addr))^ := PUInt32(Local(Args[1].Data.Addr))^;
bcMOV_i64_i64_ll: PInt64(Local(Args[0].Data.Addr))^ := PInt64(Local(Args[1].Data.Addr))^;
bcMOV_i64_u64_ll: PInt64(Local(Args[0].Data.Addr))^ := PUInt64(Local(Args[1].Data.Addr))^;

bcMOV_u64_i8_ll: PUInt64(Local(Args[0].Data.Addr))^ := PInt8(Local(Args[1].Data.Addr))^;
bcMOV_u64_u8_ll: PUInt64(Local(Args[0].Data.Addr))^ := PUInt8(Local(Args[1].Data.Addr))^;
bcMOV_u64_i16_ll: PUInt64(Local(Args[0].Data.Addr))^ := PInt16(Local(Args[1].Data.Addr))^;
bcMOV_u64_u16_ll: PUInt64(Local(Args[0].Data.Addr))^ := PUInt16(Local(Args[1].Data.Addr))^;
bcMOV_u64_i32_ll: PUInt64(Local(Args[0].Data.Addr))^ := PInt32(Local(Args[1].Data.Addr))^;
bcMOV_u64_u32_ll: PUInt64(Local(Args[0].Data.Addr))^ := PUInt32(Local(Args[1].Data.Addr))^;
bcMOV_u64_i64_ll: PUInt64(Local(Args[0].Data.Addr))^ := PInt64(Local(Args[1].Data.Addr))^;
bcMOV_u64_u64_ll: PUInt64(Local(Args[0].Data.Addr))^ := PUInt64(Local(Args[1].Data.Addr))^;

bcMOV_f32_i8_ll: PFloat32(Local(Args[0].Data.Addr))^ := PInt8(Local(Args[1].Data.Addr))^;
bcMOV_f32_u8_ll: PFloat32(Local(Args[0].Data.Addr))^ := PUInt8(Local(Args[1].Data.Addr))^;
bcMOV_f32_i16_ll: PFloat32(Local(Args[0].Data.Addr))^ := PInt16(Local(Args[1].Data.Addr))^;
bcMOV_f32_u16_ll: PFloat32(Local(Args[0].Data.Addr))^ := PUInt16(Local(Args[1].Data.Addr))^;
bcMOV_f32_i32_ll: PFloat32(Local(Args[0].Data.Addr))^ := PInt32(Local(Args[1].Data.Addr))^;
bcMOV_f32_u32_ll: PFloat32(Local(Args[0].Data.Addr))^ := PUInt32(Local(Args[1].Data.Addr))^;
bcMOV_f32_i64_ll: PFloat32(Local(Args[0].Data.Addr))^ := PInt64(Local(Args[1].Data.Addr))^;
bcMOV_f32_u64_ll: PFloat32(Local(Args[0].Data.Addr))^ := PUInt64(Local(Args[1].Data.Addr))^;
bcMOV_f32_f32_ll: PFloat32(Local(Args[0].Data.Addr))^ := PFloat32(Local(Args[1].Data.Addr))^;
bcMOV_f32_f64_ll: PFloat32(Local(Args[0].Data.Addr))^ := PFloat64(Local(Args[1].Data.Addr))^;

bcMOV_f64_i8_ll: PFloat64(Local(Args[0].Data.Addr))^ := PInt8(Local(Args[1].Data.Addr))^;
bcMOV_f64_u8_ll: PFloat64(Local(Args[0].Data.Addr))^ := PUInt8(Local(Args[1].Data.Addr))^;
bcMOV_f64_i16_ll: PFloat64(Local(Args[0].Data.Addr))^ := PInt16(Local(Args[1].Data.Addr))^;
bcMOV_f64_u16_ll: PFloat64(Local(Args[0].Data.Addr))^ := PUInt16(Local(Args[1].Data.Addr))^;
bcMOV_f64_i32_ll: PFloat64(Local(Args[0].Data.Addr))^ := PInt32(Local(Args[1].Data.Addr))^;
bcMOV_f64_u32_ll: PFloat64(Local(Args[0].Data.Addr))^ := PUInt32(Local(Args[1].Data.Addr))^;
bcMOV_f64_i64_ll: PFloat64(Local(Args[0].Data.Addr))^ := PInt64(Local(Args[1].Data.Addr))^;
bcMOV_f64_u64_ll: PFloat64(Local(Args[0].Data.Addr))^ := PUInt64(Local(Args[1].Data.Addr))^;
bcMOV_f64_f32_ll: PFloat64(Local(Args[0].Data.Addr))^ := PFloat32(Local(Args[1].Data.Addr))^;
bcMOV_f64_f64_ll: PFloat64(Local(Args[0].Data.Addr))^ := PFloat64(Local(Args[1].Data.Addr))^;

// --- Immediate Source, Local Destination (bcMOV_SourceType_DestType_li) ---
bcMOV_i8_i8_li: PInt8(Local(Args[0].Data.Addr))^ := Int8(Args[1].Data.Arg);
bcMOV_i8_u8_li: PInt8(Local(Args[0].Data.Addr))^ := UInt8(Args[1].Data.Arg);
bcMOV_i8_i16_li: PInt8(Local(Args[0].Data.Addr))^ := Int16(Args[1].Data.Arg);
bcMOV_i8_u16_li: PInt8(Local(Args[0].Data.Addr))^ := UInt16(Args[1].Data.Arg);
bcMOV_i8_i32_li: PInt8(Local(Args[0].Data.Addr))^ := Int32(Args[1].Data.Arg);
bcMOV_i8_u32_li: PInt8(Local(Args[0].Data.Addr))^ := UInt32(Args[1].Data.Arg);
bcMOV_i8_i64_li: PInt8(Local(Args[0].Data.Addr))^ := Int64(Args[1].Data.Arg);
bcMOV_i8_u64_li: PInt8(Local(Args[0].Data.Addr))^ := UInt64(Args[1].Data.Arg);

bcMOV_u8_i8_li: PUInt8(Local(Args[0].Data.Addr))^ := Int8(Args[1].Data.Arg);
bcMOV_u8_u8_li: PUInt8(Local(Args[0].Data.Addr))^ := UInt8(Args[1].Data.Arg);
bcMOV_u8_i16_li: PUInt8(Local(Args[0].Data.Addr))^ := Int16(Args[1].Data.Arg);
bcMOV_u8_u16_li: PUInt8(Local(Args[0].Data.Addr))^ := UInt16(Args[1].Data.Arg);
bcMOV_u8_i32_li: PUInt8(Local(Args[0].Data.Addr))^ := Int32(Args[1].Data.Arg);
bcMOV_u8_u32_li: PUInt8(Local(Args[0].Data.Addr))^ := UInt32(Args[1].Data.Arg);
bcMOV_u8_i64_li: PUInt8(Local(Args[0].Data.Addr))^ := Int64(Args[1].Data.Arg);
bcMOV_u8_u64_li: PUInt8(Local(Args[0].Data.Addr))^ := UInt64(Args[1].Data.Arg);

bcMOV_i16_i8_li: PInt16(Local(Args[0].Data.Addr))^ := Int8(Args[1].Data.Arg);
bcMOV_i16_u8_li: PInt16(Local(Args[0].Data.Addr))^ := UInt8(Args[1].Data.Arg);
bcMOV_i16_i16_li: PInt16(Local(Args[0].Data.Addr))^ := Int16(Args[1].Data.Arg);
bcMOV_i16_u16_li: PInt16(Local(Args[0].Data.Addr))^ := UInt16(Args[1].Data.Arg);
bcMOV_i16_i32_li: PInt16(Local(Args[0].Data.Addr))^ := Int32(Args[1].Data.Arg);
bcMOV_i16_u32_li: PInt16(Local(Args[0].Data.Addr))^ := UInt32(Args[1].Data.Arg);
bcMOV_i16_i64_li: PInt16(Local(Args[0].Data.Addr))^ := Int64(Args[1].Data.Arg);
bcMOV_i16_u64_li: PInt16(Local(Args[0].Data.Addr))^ := UInt64(Args[1].Data.Arg);

bcMOV_u16_i8_li: PUInt16(Local(Args[0].Data.Addr))^ := Int8(Args[1].Data.Arg);
bcMOV_u16_u8_li: PUInt16(Local(Args[0].Data.Addr))^ := UInt8(Args[1].Data.Arg);
bcMOV_u16_i16_li: PUInt16(Local(Args[0].Data.Addr))^ := Int16(Args[1].Data.Arg);
bcMOV_u16_u16_li: PUInt16(Local(Args[0].Data.Addr))^ := UInt16(Args[1].Data.Arg);
bcMOV_u16_i32_li: PUInt16(Local(Args[0].Data.Addr))^ := Int32(Args[1].Data.Arg);
bcMOV_u16_u32_li: PUInt16(Local(Args[0].Data.Addr))^ := UInt32(Args[1].Data.Arg);
bcMOV_u16_i64_li: PUInt16(Local(Args[0].Data.Addr))^ := Int64(Args[1].Data.Arg);
bcMOV_u16_u64_li: PUInt16(Local(Args[0].Data.Addr))^ := UInt64(Args[1].Data.Arg);

bcMOV_i32_i8_li: PInt32(Local(Args[0].Data.Addr))^ := Int8(Args[1].Data.Arg);
bcMOV_i32_u8_li: PInt32(Local(Args[0].Data.Addr))^ := UInt8(Args[1].Data.Arg);
bcMOV_i32_i16_li: PInt32(Local(Args[0].Data.Addr))^ := Int16(Args[1].Data.Arg);
bcMOV_i32_u16_li: PInt32(Local(Args[0].Data.Addr))^ := UInt16(Args[1].Data.Arg);
bcMOV_i32_i32_li: PInt32(Local(Args[0].Data.Addr))^ := Int32(Args[1].Data.Arg);
bcMOV_i32_u32_li: PInt32(Local(Args[0].Data.Addr))^ := UInt32(Args[1].Data.Arg);
bcMOV_i32_i64_li: PInt32(Local(Args[0].Data.Addr))^ := Int64(Args[1].Data.Arg);
bcMOV_i32_u64_li: PInt32(Local(Args[0].Data.Addr))^ := UInt64(Args[1].Data.Arg);

bcMOV_u32_i8_li: PUInt32(Local(Args[0].Data.Addr))^ := Int8(Args[1].Data.Arg);
bcMOV_u32_u8_li: PUInt32(Local(Args[0].Data.Addr))^ := UInt8(Args[1].Data.Arg);
bcMOV_u32_i16_li: PUInt32(Local(Args[0].Data.Addr))^ := Int16(Args[1].Data.Arg);
bcMOV_u32_u16_li: PUInt32(Local(Args[0].Data.Addr))^ := UInt16(Args[1].Data.Arg);
bcMOV_u32_i32_li: PUInt32(Local(Args[0].Data.Addr))^ := Int32(Args[1].Data.Arg);
bcMOV_u32_u32_li: PUInt32(Local(Args[0].Data.Addr))^ := UInt32(Args[1].Data.Arg);
bcMOV_u32_i64_li: PUInt32(Local(Args[0].Data.Addr))^ := Int64(Args[1].Data.Arg);
bcMOV_u32_u64_li: PUInt32(Local(Args[0].Data.Addr))^ := UInt64(Args[1].Data.Arg);

bcMOV_i64_i8_li: PInt64(Local(Args[0].Data.Addr))^ := Int8(Args[1].Data.Arg);
bcMOV_i64_u8_li: PInt64(Local(Args[0].Data.Addr))^ := UInt8(Args[1].Data.Arg);
bcMOV_i64_i16_li: PInt64(Local(Args[0].Data.Addr))^ := Int16(Args[1].Data.Arg);
bcMOV_i64_u16_li: PInt64(Local(Args[0].Data.Addr))^ := UInt16(Args[1].Data.Arg);
bcMOV_i64_i32_li: PInt64(Local(Args[0].Data.Addr))^ := Int32(Args[1].Data.Arg);
bcMOV_i64_u32_li: PInt64(Local(Args[0].Data.Addr))^ := UInt32(Args[1].Data.Arg);
bcMOV_i64_i64_li: PInt64(Local(Args[0].Data.Addr))^ := Int64(Args[1].Data.Arg);
bcMOV_i64_u64_li: PInt64(Local(Args[0].Data.Addr))^ := UInt64(Args[1].Data.Arg);

bcMOV_u64_i8_li: PUInt64(Local(Args[0].Data.Addr))^ := Int8(Args[1].Data.Arg);
bcMOV_u64_u8_li: PUInt64(Local(Args[0].Data.Addr))^ := UInt8(Args[1].Data.Arg);
bcMOV_u64_i16_li: PUInt64(Local(Args[0].Data.Addr))^ := Int16(Args[1].Data.Arg);
bcMOV_u64_u16_li: PUInt64(Local(Args[0].Data.Addr))^ := UInt16(Args[1].Data.Arg);
bcMOV_u64_i32_li: PUInt64(Local(Args[0].Data.Addr))^ := Int32(Args[1].Data.Arg);
bcMOV_u64_u32_li: PUInt64(Local(Args[0].Data.Addr))^ := UInt32(Args[1].Data.Arg);
bcMOV_u64_i64_li: PUInt64(Local(Args[0].Data.Addr))^ := Int64(Args[1].Data.Arg);
bcMOV_u64_u64_li: PUInt64(Local(Args[0].Data.Addr))^ := UInt64(Args[1].Data.Arg);

bcMOV_f32_i8_li: PFloat32(Local(Args[0].Data.Addr))^ := Int8(Args[1].Data.Arg);
bcMOV_f32_u8_li: PFloat32(Local(Args[0].Data.Addr))^ := UInt8(Args[1].Data.Arg);
bcMOV_f32_i16_li: PFloat32(Local(Args[0].Data.Addr))^ := Int16(Args[1].Data.Arg);
bcMOV_f32_u16_li: PFloat32(Local(Args[0].Data.Addr))^ := UInt16(Args[1].Data.Arg);
bcMOV_f32_i32_li: PFloat32(Local(Args[0].Data.Addr))^ := Int32(Args[1].Data.Arg);
bcMOV_f32_u32_li: PFloat32(Local(Args[0].Data.Addr))^ := UInt32(Args[1].Data.Arg);
bcMOV_f32_i64_li: PFloat32(Local(Args[0].Data.Addr))^ := Int64(Args[1].Data.Arg);
bcMOV_f32_u64_li: PFloat32(Local(Args[0].Data.Addr))^ := UInt64(Args[1].Data.Arg);
bcMOV_f32_f32_li: PFloat32(Local(Args[0].Data.Addr))^ := Float32(Args[1].Data.Raw32);
bcMOV_f32_f64_li: PFloat32(Local(Args[0].Data.Addr))^ := Float64(Args[1].Data.Raw);

bcMOV_f64_i8_li: PFloat64(Local(Args[0].Data.Addr))^ := Int8(Args[1].Data.Arg);
bcMOV_f64_u8_li: PFloat64(Local(Args[0].Data.Addr))^ := UInt8(Args[1].Data.Arg);
bcMOV_f64_i16_li: PFloat64(Local(Args[0].Data.Addr))^ := Int16(Args[1].Data.Arg);
bcMOV_f64_u16_li: PFloat64(Local(Args[0].Data.Addr))^ := UInt16(Args[1].Data.Arg);
bcMOV_f64_i32_li: PFloat64(Local(Args[0].Data.Addr))^ := Int32(Args[1].Data.Arg);
bcMOV_f64_u32_li: PFloat64(Local(Args[0].Data.Addr))^ := UInt32(Args[1].Data.Arg);
bcMOV_f64_i64_li: PFloat64(Local(Args[0].Data.Addr))^ := Int64(Args[1].Data.Arg);
bcMOV_f64_u64_li: PFloat64(Local(Args[0].Data.Addr))^ := UInt64(Args[1].Data.Arg);
bcMOV_f64_f32_li: PFloat64(Local(Args[0].Data.Addr))^ := Float32(Args[1].Data.Raw32);
bcMOV_f64_f64_li: PFloat64(Local(Args[0].Data.Addr))^ := Float64(Args[1].Data.Raw);

// --- Local Source, Local Destination (bcMOVH_SourceType_DestType_ll) ---
bcMOVH_i8_i8_ll: PInt8(Local(Args[0].Data.Addr)^)^ := PInt8(Local(Args[1].Data.Addr))^;
bcMOVH_i8_u8_ll: PInt8(Local(Args[0].Data.Addr)^)^ := PUInt8(Local(Args[1].Data.Addr))^;
bcMOVH_i8_i16_ll: PInt8(Local(Args[0].Data.Addr)^)^ := PInt16(Local(Args[1].Data.Addr))^;
bcMOVH_i8_u16_ll: PInt8(Local(Args[0].Data.Addr)^)^ := PUInt16(Local(Args[1].Data.Addr))^;
bcMOVH_i8_i32_ll: PInt8(Local(Args[0].Data.Addr)^)^ := PInt32(Local(Args[1].Data.Addr))^;
bcMOVH_i8_u32_ll: PInt8(Local(Args[0].Data.Addr)^)^ := PUInt32(Local(Args[1].Data.Addr))^;
bcMOVH_i8_i64_ll: PInt8(Local(Args[0].Data.Addr)^)^ := PInt64(Local(Args[1].Data.Addr))^;
bcMOVH_i8_u64_ll: PInt8(Local(Args[0].Data.Addr)^)^ := PUInt64(Local(Args[1].Data.Addr))^;

bcMOVH_u8_i8_ll: PUInt8(Local(Args[0].Data.Addr)^)^ := PInt8(Local(Args[1].Data.Addr))^;
bcMOVH_u8_u8_ll: PUInt8(Local(Args[0].Data.Addr)^)^ := PUInt8(Local(Args[1].Data.Addr))^;
bcMOVH_u8_i16_ll: PUInt8(Local(Args[0].Data.Addr)^)^ := PInt16(Local(Args[1].Data.Addr))^;
bcMOVH_u8_u16_ll: PUInt8(Local(Args[0].Data.Addr)^)^ := PUInt16(Local(Args[1].Data.Addr))^;
bcMOVH_u8_i32_ll: PUInt8(Local(Args[0].Data.Addr)^)^ := PInt32(Local(Args[1].Data.Addr))^;
bcMOVH_u8_u32_ll: PUInt8(Local(Args[0].Data.Addr)^)^ := PUInt32(Local(Args[1].Data.Addr))^;
bcMOVH_u8_i64_ll: PUInt8(Local(Args[0].Data.Addr)^)^ := PInt64(Local(Args[1].Data.Addr))^;
bcMOVH_u8_u64_ll: PUInt8(Local(Args[0].Data.Addr)^)^ := PUInt64(Local(Args[1].Data.Addr))^;

bcMOVH_i16_i8_ll: PInt16(Local(Args[0].Data.Addr)^)^ := PInt8(Local(Args[1].Data.Addr))^;
bcMOVH_i16_u8_ll: PInt16(Local(Args[0].Data.Addr)^)^ := PUInt8(Local(Args[1].Data.Addr))^;
bcMOVH_i16_i16_ll: PInt16(Local(Args[0].Data.Addr)^)^ := PInt16(Local(Args[1].Data.Addr))^;
bcMOVH_i16_u16_ll: PInt16(Local(Args[0].Data.Addr)^)^ := PUInt16(Local(Args[1].Data.Addr))^;
bcMOVH_i16_i32_ll: PInt16(Local(Args[0].Data.Addr)^)^ := PInt32(Local(Args[1].Data.Addr))^;
bcMOVH_i16_u32_ll: PInt16(Local(Args[0].Data.Addr)^)^ := PUInt32(Local(Args[1].Data.Addr))^;
bcMOVH_i16_i64_ll: PInt16(Local(Args[0].Data.Addr)^)^ := PInt64(Local(Args[1].Data.Addr))^;
bcMOVH_i16_u64_ll: PInt16(Local(Args[0].Data.Addr)^)^ := PUInt64(Local(Args[1].Data.Addr))^;

bcMOVH_u16_i8_ll: PUInt16(Local(Args[0].Data.Addr)^)^ := PInt8(Local(Args[1].Data.Addr))^;
bcMOVH_u16_u8_ll: PUInt16(Local(Args[0].Data.Addr)^)^ := PUInt8(Local(Args[1].Data.Addr))^;
bcMOVH_u16_i16_ll: PUInt16(Local(Args[0].Data.Addr)^)^ := PInt16(Local(Args[1].Data.Addr))^;
bcMOVH_u16_u16_ll: PUInt16(Local(Args[0].Data.Addr)^)^ := PUInt16(Local(Args[1].Data.Addr))^;
bcMOVH_u16_i32_ll: PUInt16(Local(Args[0].Data.Addr)^)^ := PInt32(Local(Args[1].Data.Addr))^;
bcMOVH_u16_u32_ll: PUInt16(Local(Args[0].Data.Addr)^)^ := PUInt32(Local(Args[1].Data.Addr))^;
bcMOVH_u16_i64_ll: PUInt16(Local(Args[0].Data.Addr)^)^ := PInt64(Local(Args[1].Data.Addr))^;
bcMOVH_u16_u64_ll: PUInt16(Local(Args[0].Data.Addr)^)^ := PUInt64(Local(Args[1].Data.Addr))^;

bcMOVH_i32_i8_ll: PInt32(Local(Args[0].Data.Addr)^)^ := PInt8(Local(Args[1].Data.Addr))^;
bcMOVH_i32_u8_ll: PInt32(Local(Args[0].Data.Addr)^)^ := PUInt8(Local(Args[1].Data.Addr))^;
bcMOVH_i32_i16_ll: PInt32(Local(Args[0].Data.Addr)^)^ := PInt16(Local(Args[1].Data.Addr))^;
bcMOVH_i32_u16_ll: PInt32(Local(Args[0].Data.Addr)^)^ := PUInt16(Local(Args[1].Data.Addr))^;
bcMOVH_i32_i32_ll: PInt32(Local(Args[0].Data.Addr)^)^ := PInt32(Local(Args[1].Data.Addr))^;
bcMOVH_i32_u32_ll: PInt32(Local(Args[0].Data.Addr)^)^ := PUInt32(Local(Args[1].Data.Addr))^;
bcMOVH_i32_i64_ll: PInt32(Local(Args[0].Data.Addr)^)^ := PInt64(Local(Args[1].Data.Addr))^;
bcMOVH_i32_u64_ll: PInt32(Local(Args[0].Data.Addr)^)^ := PUInt64(Local(Args[1].Data.Addr))^;

bcMOVH_u32_i8_ll: PUInt32(Local(Args[0].Data.Addr)^)^ := PInt8(Local(Args[1].Data.Addr))^;
bcMOVH_u32_u8_ll: PUInt32(Local(Args[0].Data.Addr)^)^ := PUInt8(Local(Args[1].Data.Addr))^;
bcMOVH_u32_i16_ll: PUInt32(Local(Args[0].Data.Addr)^)^ := PInt16(Local(Args[1].Data.Addr))^;
bcMOVH_u32_u16_ll: PUInt32(Local(Args[0].Data.Addr)^)^ := PUInt16(Local(Args[1].Data.Addr))^;
bcMOVH_u32_i32_ll: PUInt32(Local(Args[0].Data.Addr)^)^ := PInt32(Local(Args[1].Data.Addr))^;
bcMOVH_u32_u32_ll: PUInt32(Local(Args[0].Data.Addr)^)^ := PUInt32(Local(Args[1].Data.Addr))^;
bcMOVH_u32_i64_ll: PUInt32(Local(Args[0].Data.Addr)^)^ := PInt64(Local(Args[1].Data.Addr))^;
bcMOVH_u32_u64_ll: PUInt32(Local(Args[0].Data.Addr)^)^ := PUInt64(Local(Args[1].Data.Addr))^;

bcMOVH_i64_i8_ll: PInt64(Local(Args[0].Data.Addr)^)^ := PInt8(Local(Args[1].Data.Addr))^;
bcMOVH_i64_u8_ll: PInt64(Local(Args[0].Data.Addr)^)^ := PUInt8(Local(Args[1].Data.Addr))^;
bcMOVH_i64_i16_ll: PInt64(Local(Args[0].Data.Addr)^)^ := PInt16(Local(Args[1].Data.Addr))^;
bcMOVH_i64_u16_ll: PInt64(Local(Args[0].Data.Addr)^)^ := PUInt16(Local(Args[1].Data.Addr))^;
bcMOVH_i64_i32_ll: PInt64(Local(Args[0].Data.Addr)^)^ := PInt32(Local(Args[1].Data.Addr))^;
bcMOVH_i64_u32_ll: PInt64(Local(Args[0].Data.Addr)^)^ := PUInt32(Local(Args[1].Data.Addr))^;
bcMOVH_i64_i64_ll: PInt64(Local(Args[0].Data.Addr)^)^ := PInt64(Local(Args[1].Data.Addr))^;
bcMOVH_i64_u64_ll: PInt64(Local(Args[0].Data.Addr)^)^ := PUInt64(Local(Args[1].Data.Addr))^;

bcMOVH_u64_i8_ll: PUInt64(Local(Args[0].Data.Addr)^)^ := PInt8(Local(Args[1].Data.Addr))^;
bcMOVH_u64_u8_ll: PUInt64(Local(Args[0].Data.Addr)^)^ := PUInt8(Local(Args[1].Data.Addr))^;
bcMOVH_u64_i16_ll: PUInt64(Local(Args[0].Data.Addr)^)^ := PInt16(Local(Args[1].Data.Addr))^;
bcMOVH_u64_u16_ll: PUInt64(Local(Args[0].Data.Addr)^)^ := PUInt16(Local(Args[1].Data.Addr))^;
bcMOVH_u64_i32_ll: PUInt64(Local(Args[0].Data.Addr)^)^ := PInt32(Local(Args[1].Data.Addr))^;
bcMOVH_u64_u32_ll: PUInt64(Local(Args[0].Data.Addr)^)^ := PUInt32(Local(Args[1].Data.Addr))^;
bcMOVH_u64_i64_ll: PUInt64(Local(Args[0].Data.Addr)^)^ := PInt64(Local(Args[1].Data.Addr))^;
bcMOVH_u64_u64_ll: PUInt64(Local(Args[0].Data.Addr)^)^ := PUInt64(Local(Args[1].Data.Addr))^;

bcMOVH_f32_i8_ll: PFloat32(Local(Args[0].Data.Addr)^)^ := PInt8(Local(Args[1].Data.Addr))^;
bcMOVH_f32_u8_ll: PFloat32(Local(Args[0].Data.Addr)^)^ := PUInt8(Local(Args[1].Data.Addr))^;
bcMOVH_f32_i16_ll: PFloat32(Local(Args[0].Data.Addr)^)^ := PInt16(Local(Args[1].Data.Addr))^;
bcMOVH_f32_u16_ll: PFloat32(Local(Args[0].Data.Addr)^)^ := PUInt16(Local(Args[1].Data.Addr))^;
bcMOVH_f32_i32_ll: PFloat32(Local(Args[0].Data.Addr)^)^ := PInt32(Local(Args[1].Data.Addr))^;
bcMOVH_f32_u32_ll: PFloat32(Local(Args[0].Data.Addr)^)^ := PUInt32(Local(Args[1].Data.Addr))^;
bcMOVH_f32_i64_ll: PFloat32(Local(Args[0].Data.Addr)^)^ := PInt64(Local(Args[1].Data.Addr))^;
bcMOVH_f32_u64_ll: PFloat32(Local(Args[0].Data.Addr)^)^ := PUInt64(Local(Args[1].Data.Addr))^;
bcMOVH_f32_f32_ll: PFloat32(Local(Args[0].Data.Addr)^)^ := PFloat32(Local(Args[1].Data.Addr))^;
bcMOVH_f32_f64_ll: PFloat32(Local(Args[0].Data.Addr)^)^ := PFloat64(Local(Args[1].Data.Addr))^;

bcMOVH_f64_i8_ll: PFloat64(Local(Args[0].Data.Addr)^)^ := PInt8(Local(Args[1].Data.Addr))^;
bcMOVH_f64_u8_ll: PFloat64(Local(Args[0].Data.Addr)^)^ := PUInt8(Local(Args[1].Data.Addr))^;
bcMOVH_f64_i16_ll: PFloat64(Local(Args[0].Data.Addr)^)^ := PInt16(Local(Args[1].Data.Addr))^;
bcMOVH_f64_u16_ll: PFloat64(Local(Args[0].Data.Addr)^)^ := PUInt16(Local(Args[1].Data.Addr))^;
bcMOVH_f64_i32_ll: PFloat64(Local(Args[0].Data.Addr)^)^ := PInt32(Local(Args[1].Data.Addr))^;
bcMOVH_f64_u32_ll: PFloat64(Local(Args[0].Data.Addr)^)^ := PUInt32(Local(Args[1].Data.Addr))^;
bcMOVH_f64_i64_ll: PFloat64(Local(Args[0].Data.Addr)^)^ := PInt64(Local(Args[1].Data.Addr))^;
bcMOVH_f64_u64_ll: PFloat64(Local(Args[0].Data.Addr)^)^ := PUInt64(Local(Args[1].Data.Addr))^;
bcMOVH_f64_f32_ll: PFloat64(Local(Args[0].Data.Addr)^)^ := PFloat32(Local(Args[1].Data.Addr))^;
bcMOVH_f64_f64_ll: PFloat64(Local(Args[0].Data.Addr)^)^ := PFloat64(Local(Args[1].Data.Addr))^;


// --- Immediate Source, Local Destination (bcMOVH_SourceType_DestType_li) ---
bcMOVH_i8_i8_li: PInt8(Local(Args[0].Data.Addr)^)^ := Int8(Args[1].Data.Arg);
bcMOVH_i8_u8_li: PInt8(Local(Args[0].Data.Addr)^)^ := UInt8(Args[1].Data.Arg);
bcMOVH_i8_i16_li: PInt8(Local(Args[0].Data.Addr)^)^ := Int16(Args[1].Data.Arg);
bcMOVH_i8_u16_li: PInt8(Local(Args[0].Data.Addr)^)^ := UInt16(Args[1].Data.Arg);
bcMOVH_i8_i32_li: PInt8(Local(Args[0].Data.Addr)^)^ := Int32(Args[1].Data.Arg);
bcMOVH_i8_u32_li: PInt8(Local(Args[0].Data.Addr)^)^ := UInt32(Args[1].Data.Arg);
bcMOVH_i8_i64_li: PInt8(Local(Args[0].Data.Addr)^)^ := Int64(Args[1].Data.Arg);
bcMOVH_i8_u64_li: PInt8(Local(Args[0].Data.Addr)^)^ := UInt64(Args[1].Data.Arg);

bcMOVH_u8_i8_li: PUInt8(Local(Args[0].Data.Addr)^)^ := Int8(Args[1].Data.Arg);
bcMOVH_u8_u8_li: PUInt8(Local(Args[0].Data.Addr)^)^ := UInt8(Args[1].Data.Arg);
bcMOVH_u8_i16_li: PUInt8(Local(Args[0].Data.Addr)^)^ := Int16(Args[1].Data.Arg);
bcMOVH_u8_u16_li: PUInt8(Local(Args[0].Data.Addr)^)^ := UInt16(Args[1].Data.Arg);
bcMOVH_u8_i32_li: PUInt8(Local(Args[0].Data.Addr)^)^ := Int32(Args[1].Data.Arg);
bcMOVH_u8_u32_li: PUInt8(Local(Args[0].Data.Addr)^)^ := UInt32(Args[1].Data.Arg);
bcMOVH_u8_i64_li: PUInt8(Local(Args[0].Data.Addr)^)^ := Int64(Args[1].Data.Arg);
bcMOVH_u8_u64_li: PUInt8(Local(Args[0].Data.Addr)^)^ := UInt64(Args[1].Data.Arg);

bcMOVH_i16_i8_li: PInt16(Local(Args[0].Data.Addr)^)^ := Int8(Args[1].Data.Arg);
bcMOVH_i16_u8_li: PInt16(Local(Args[0].Data.Addr)^)^ := UInt8(Args[1].Data.Arg);
bcMOVH_i16_i16_li: PInt16(Local(Args[0].Data.Addr)^)^ := Int16(Args[1].Data.Arg);
bcMOVH_i16_u16_li: PInt16(Local(Args[0].Data.Addr)^)^ := UInt16(Args[1].Data.Arg);
bcMOVH_i16_i32_li: PInt16(Local(Args[0].Data.Addr)^)^ := Int32(Args[1].Data.Arg);
bcMOVH_i16_u32_li: PInt16(Local(Args[0].Data.Addr)^)^ := UInt32(Args[1].Data.Arg);
bcMOVH_i16_i64_li: PInt16(Local(Args[0].Data.Addr)^)^ := Int64(Args[1].Data.Arg);
bcMOVH_i16_u64_li: PInt16(Local(Args[0].Data.Addr)^)^ := UInt64(Args[1].Data.Arg);

bcMOVH_u16_i8_li: PUInt16(Local(Args[0].Data.Addr)^)^ := Int8(Args[1].Data.Arg);
bcMOVH_u16_u8_li: PUInt16(Local(Args[0].Data.Addr)^)^ := UInt8(Args[1].Data.Arg);
bcMOVH_u16_i16_li: PUInt16(Local(Args[0].Data.Addr)^)^ := Int16(Args[1].Data.Arg);
bcMOVH_u16_u16_li: PUInt16(Local(Args[0].Data.Addr)^)^ := UInt16(Args[1].Data.Arg);
bcMOVH_u16_i32_li: PUInt16(Local(Args[0].Data.Addr)^)^ := Int32(Args[1].Data.Arg);
bcMOVH_u16_u32_li: PUInt16(Local(Args[0].Data.Addr)^)^ := UInt32(Args[1].Data.Arg);
bcMOVH_u16_i64_li: PUInt16(Local(Args[0].Data.Addr)^)^ := Int64(Args[1].Data.Arg);
bcMOVH_u16_u64_li: PUInt16(Local(Args[0].Data.Addr)^)^ := UInt64(Args[1].Data.Arg);

bcMOVH_i32_i8_li: PInt32(Local(Args[0].Data.Addr)^)^ := Int8(Args[1].Data.Arg);
bcMOVH_i32_u8_li: PInt32(Local(Args[0].Data.Addr)^)^ := UInt8(Args[1].Data.Arg);
bcMOVH_i32_i16_li: PInt32(Local(Args[0].Data.Addr)^)^ := Int16(Args[1].Data.Arg);
bcMOVH_i32_u16_li: PInt32(Local(Args[0].Data.Addr)^)^ := UInt16(Args[1].Data.Arg);
bcMOVH_i32_i32_li: PInt32(Local(Args[0].Data.Addr)^)^ := Int32(Args[1].Data.Arg);
bcMOVH_i32_u32_li: PInt32(Local(Args[0].Data.Addr)^)^ := UInt32(Args[1].Data.Arg);
bcMOVH_i32_i64_li: PInt32(Local(Args[0].Data.Addr)^)^ := Int64(Args[1].Data.Arg);
bcMOVH_i32_u64_li: PInt32(Local(Args[0].Data.Addr)^)^ := UInt64(Args[1].Data.Arg);

bcMOVH_u32_i8_li: PUInt32(Local(Args[0].Data.Addr)^)^ := Int8(Args[1].Data.Arg);
bcMOVH_u32_u8_li: PUInt32(Local(Args[0].Data.Addr)^)^ := UInt8(Args[1].Data.Arg);
bcMOVH_u32_i16_li: PUInt32(Local(Args[0].Data.Addr)^)^ := Int16(Args[1].Data.Arg);
bcMOVH_u32_u16_li: PUInt32(Local(Args[0].Data.Addr)^)^ := UInt16(Args[1].Data.Arg);
bcMOVH_u32_i32_li: PUInt32(Local(Args[0].Data.Addr)^)^ := Int32(Args[1].Data.Arg);
bcMOVH_u32_u32_li: PUInt32(Local(Args[0].Data.Addr)^)^ := UInt32(Args[1].Data.Arg);
bcMOVH_u32_i64_li: PUInt32(Local(Args[0].Data.Addr)^)^ := Int64(Args[1].Data.Arg);
bcMOVH_u32_u64_li: PUInt32(Local(Args[0].Data.Addr)^)^ := UInt64(Args[1].Data.Arg);

bcMOVH_i64_i8_li: PInt64(Local(Args[0].Data.Addr)^)^ := Int8(Args[1].Data.Arg);
bcMOVH_i64_u8_li: PInt64(Local(Args[0].Data.Addr)^)^ := UInt8(Args[1].Data.Arg);
bcMOVH_i64_i16_li: PInt64(Local(Args[0].Data.Addr)^)^ := Int16(Args[1].Data.Arg);
bcMOVH_i64_u16_li: PInt64(Local(Args[0].Data.Addr)^)^ := UInt16(Args[1].Data.Arg);
bcMOVH_i64_i32_li: PInt64(Local(Args[0].Data.Addr)^)^ := Int32(Args[1].Data.Arg);
bcMOVH_i64_u32_li: PInt64(Local(Args[0].Data.Addr)^)^ := UInt32(Args[1].Data.Arg);
bcMOVH_i64_i64_li: PInt64(Local(Args[0].Data.Addr)^)^ := Int64(Args[1].Data.Arg);
bcMOVH_i64_u64_li: PInt64(Local(Args[0].Data.Addr)^)^ := UInt64(Args[1].Data.Arg);

bcMOVH_u64_i8_li: PUInt64(Local(Args[0].Data.Addr)^)^ := Int8(Args[1].Data.Arg);
bcMOVH_u64_u8_li: PUInt64(Local(Args[0].Data.Addr)^)^ := UInt8(Args[1].Data.Arg);
bcMOVH_u64_i16_li: PUInt64(Local(Args[0].Data.Addr)^)^ := Int16(Args[1].Data.Arg);
bcMOVH_u64_u16_li: PUInt64(Local(Args[0].Data.Addr)^)^ := UInt16(Args[1].Data.Arg);
bcMOVH_u64_i32_li: PUInt64(Local(Args[0].Data.Addr)^)^ := Int32(Args[1].Data.Arg);
bcMOVH_u64_u32_li: PUInt64(Local(Args[0].Data.Addr)^)^ := UInt32(Args[1].Data.Arg);
bcMOVH_u64_i64_li: PUInt64(Local(Args[0].Data.Addr)^)^ := Int64(Args[1].Data.Arg);
bcMOVH_u64_u64_li: PUInt64(Local(Args[0].Data.Addr)^)^ := UInt64(Args[1].Data.Arg);

bcMOVH_f32_i8_li: PFloat32(Local(Args[0].Data.Addr)^)^ := Int8(Args[1].Data.Arg);
bcMOVH_f32_u8_li: PFloat32(Local(Args[0].Data.Addr)^)^ := UInt8(Args[1].Data.Arg);
bcMOVH_f32_i16_li: PFloat32(Local(Args[0].Data.Addr)^)^ := Int16(Args[1].Data.Arg);
bcMOVH_f32_u16_li: PFloat32(Local(Args[0].Data.Addr)^)^ := UInt16(Args[1].Data.Arg);
bcMOVH_f32_i32_li: PFloat32(Local(Args[0].Data.Addr)^)^ := Int32(Args[1].Data.Arg);
bcMOVH_f32_u32_li: PFloat32(Local(Args[0].Data.Addr)^)^ := UInt32(Args[1].Data.Arg);
bcMOVH_f32_i64_li: PFloat32(Local(Args[0].Data.Addr)^)^ := Int64(Args[1].Data.Arg);
bcMOVH_f32_u64_li: PFloat32(Local(Args[0].Data.Addr)^)^ := UInt64(Args[1].Data.Arg);
bcMOVH_f32_f32_li: PFloat32(Local(Args[0].Data.Addr)^)^ := Float32(Args[1].Data.Raw32);
bcMOVH_f32_f64_li: PFloat32(Local(Args[0].Data.Addr)^)^ := Float64(Args[1].Data.Raw);

bcMOVH_f64_i8_li: PFloat64(Local(Args[0].Data.Addr)^)^ := Int8(Args[1].Data.Arg);
bcMOVH_f64_u8_li: PFloat64(Local(Args[0].Data.Addr)^)^ := UInt8(Args[1].Data.Arg);
bcMOVH_f64_i16_li: PFloat64(Local(Args[0].Data.Addr)^)^ := Int16(Args[1].Data.Arg);
bcMOVH_f64_u16_li: PFloat64(Local(Args[0].Data.Addr)^)^ := UInt16(Args[1].Data.Arg);
bcMOVH_f64_i32_li: PFloat64(Local(Args[0].Data.Addr)^)^ := Int32(Args[1].Data.Arg);
bcMOVH_f64_u32_li: PFloat64(Local(Args[0].Data.Addr)^)^ := UInt32(Args[1].Data.Arg);
bcMOVH_f64_i64_li: PFloat64(Local(Args[0].Data.Addr)^)^ := Int64(Args[1].Data.Arg);
bcMOVH_f64_u64_li: PFloat64(Local(Args[0].Data.Addr)^)^ := UInt64(Args[1].Data.Arg);
bcMOVH_f64_f32_li: PFloat64(Local(Args[0].Data.Addr)^)^ := Float32(Args[1].Data.Raw32);
bcMOVH_f64_f64_li: PFloat64(Local(Args[0].Data.Addr)^)^ := Float64(Args[1].Data.Raw);
