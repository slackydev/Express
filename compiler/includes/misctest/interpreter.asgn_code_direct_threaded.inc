// --- Stack.Local Source, Stack.Local Destination (lbMOV_SourceType_DestType_ll) ---
lbMOV_i8_i8_ll: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i8_u8_ll: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i8_i16_ll: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i8_u16_ll: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i8_i32_ll: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i8_u32_ll: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i8_i64_ll: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i8_u64_ll: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH

lbMOV_u8_i8_ll: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u8_u8_ll: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u8_i16_ll: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u8_u16_ll: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u8_i32_ll: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u8_u32_ll: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u8_i64_ll: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u8_u64_ll: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH

lbMOV_i16_i8_ll: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i16_u8_ll: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i16_i16_ll: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i16_u16_ll: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i16_i32_ll: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i16_u32_ll: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i16_i64_ll: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i16_u64_ll: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH

lbMOV_u16_i8_ll: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u16_u8_ll: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u16_i16_ll: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u16_u16_ll: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u16_i32_ll: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u16_u32_ll: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u16_i64_ll: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u16_u64_ll: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH

lbMOV_i32_i8_ll: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i32_u8_ll: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i32_i16_ll: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i32_u16_ll: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i32_i32_ll: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i32_u32_ll: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i32_i64_ll: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i32_u64_ll: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH

lbMOV_u32_i8_ll: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u32_u8_ll: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u32_i16_ll: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u32_u16_ll: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u32_i32_ll: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u32_u32_ll: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u32_i64_ll: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u32_u64_ll: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH

lbMOV_i64_i8_ll: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i64_u8_ll: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i64_i16_ll: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i64_u16_ll: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i64_i32_ll: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i64_u32_ll: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i64_i64_ll: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_i64_u64_ll: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH

lbMOV_u64_i8_ll: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u64_u8_ll: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u64_i16_ll: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u64_u16_ll: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u64_i32_ll: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u64_u32_ll: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u64_i64_ll: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_u64_u64_ll: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH

lbMOV_f32_i8_ll: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_f32_u8_ll: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_f32_i16_ll: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_f32_u16_ll: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_f32_i32_ll: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_f32_u32_ll: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_f32_i64_ll: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_f32_u64_ll: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_f32_f32_ll: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_f32_f64_ll: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH

lbMOV_f64_i8_ll: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_f64_u8_ll: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_f64_i16_ll: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_f64_u16_ll: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_f64_i32_ll: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_f64_u32_ll: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_f64_i64_ll: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_f64_u64_ll: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_f64_f32_ll: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOV_f64_f64_ll: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH

// --- Immediate Source, Stack.Local Destination (lbMOV_SourceType_DestType_li) ---
lbMOV_i8_i8_li: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := Int8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i8_u8_li: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i8_i16_li: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := Int16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i8_u16_li: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i8_i32_li: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := Int32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i8_u32_li: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i8_i64_li: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := Int64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i8_u64_li: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt64(Data^.Args[1].Data.Arg);
  DISPATCH

lbMOV_u8_i8_li: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := Int8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u8_u8_li: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u8_i16_li: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := Int16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u8_u16_li: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u8_i32_li: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := Int32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u8_u32_li: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u8_i64_li: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := Int64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u8_u64_li: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt64(Data^.Args[1].Data.Arg);
  DISPATCH

lbMOV_i16_i8_li: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := Int8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i16_u8_li: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i16_i16_li: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := Int16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i16_u16_li: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i16_i32_li: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := Int32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i16_u32_li: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i16_i64_li: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := Int64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i16_u64_li: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt64(Data^.Args[1].Data.Arg);
  DISPATCH

lbMOV_u16_i8_li: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := Int8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u16_u8_li: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u16_i16_li: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := Int16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u16_u16_li: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u16_i32_li: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := Int32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u16_u32_li: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u16_i64_li: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := Int64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u16_u64_li: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt64(Data^.Args[1].Data.Arg);
  DISPATCH

lbMOV_i32_i8_li: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := Int8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i32_u8_li: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i32_i16_li: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := Int16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i32_u16_li: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i32_i32_li: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := Int32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i32_u32_li: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i32_i64_li: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := Int64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i32_u64_li: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt64(Data^.Args[1].Data.Arg);
  DISPATCH

lbMOV_u32_i8_li: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := Int8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u32_u8_li: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u32_i16_li: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := Int16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u32_u16_li: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u32_i32_li: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := Int32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u32_u32_li: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u32_i64_li: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := Int64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u32_u64_li: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt64(Data^.Args[1].Data.Arg);
  DISPATCH

lbMOV_i64_i8_li: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := Int8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i64_u8_li: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i64_i16_li: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := Int16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i64_u16_li: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i64_i32_li: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := Int32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i64_u32_li: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i64_i64_li: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := Int64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_i64_u64_li: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt64(Data^.Args[1].Data.Arg);
  DISPATCH

lbMOV_u64_i8_li: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := Int8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u64_u8_li: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u64_i16_li: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := Int16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u64_u16_li: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u64_i32_li: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := Int32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u64_u32_li: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u64_i64_li: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := Int64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_u64_u64_li: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt64(Data^.Args[1].Data.Arg);
  DISPATCH

lbMOV_f32_i8_li: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ := Int8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_f32_u8_li: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_f32_i16_li: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ := Int16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_f32_u16_li: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_f32_i32_li: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ := Int32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_f32_u32_li: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_f32_i64_li: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ := Int64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_f32_u64_li: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_f32_f32_li: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ := Float32(Data^.Args[1].Data.Raw32);
  DISPATCH
lbMOV_f32_f64_li: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ := Float64(Data^.Args[1].Data.Raw);
  DISPATCH

lbMOV_f64_i8_li: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ := Int8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_f64_u8_li: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_f64_i16_li: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ := Int16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_f64_u16_li: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_f64_i32_li: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ := Int32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_f64_u32_li: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_f64_i64_li: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ := Int64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_f64_u64_li: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ := UInt64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOV_f64_f32_li: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ := Float32(Data^.Args[1].Data.Raw32);
  DISPATCH
lbMOV_f64_f64_li: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ := Float64(Data^.Args[1].Data.Raw);
  DISPATCH

// --- Stack.Local Source, Stack.Local Destination (lbMOVH_SourceType_DestType_ll) ---
lbMOVH_i8_i8_ll: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i8_u8_ll: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i8_i16_ll: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i8_u16_ll: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i8_i32_ll: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i8_u32_ll: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i8_i64_ll: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i8_u64_ll: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH

lbMOVH_u8_i8_ll: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u8_u8_ll: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u8_i16_ll: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u8_u16_ll: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u8_i32_ll: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u8_u32_ll: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u8_i64_ll: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u8_u64_ll: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH

lbMOVH_i16_i8_ll: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i16_u8_ll: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i16_i16_ll: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i16_u16_ll: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i16_i32_ll: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i16_u32_ll: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i16_i64_ll: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i16_u64_ll: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH

lbMOVH_u16_i8_ll: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u16_u8_ll: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u16_i16_ll: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u16_u16_ll: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u16_i32_ll: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u16_u32_ll: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u16_i64_ll: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u16_u64_ll: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH

lbMOVH_i32_i8_ll: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i32_u8_ll: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i32_i16_ll: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i32_u16_ll: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i32_i32_ll: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i32_u32_ll: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i32_i64_ll: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i32_u64_ll: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH

lbMOVH_u32_i8_ll: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u32_u8_ll: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u32_i16_ll: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u32_u16_ll: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u32_i32_ll: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u32_u32_ll: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u32_i64_ll: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u32_u64_ll: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH

lbMOVH_i64_i8_ll: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i64_u8_ll: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i64_i16_ll: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i64_u16_ll: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i64_i32_ll: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i64_u32_ll: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i64_i64_ll: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_i64_u64_ll: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH

lbMOVH_u64_i8_ll: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u64_u8_ll: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u64_i16_ll: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u64_u16_ll: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u64_i32_ll: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u64_u32_ll: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u64_i64_ll: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_u64_u64_ll: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH

lbMOVH_f32_i8_ll: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_f32_u8_ll: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_f32_i16_ll: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_f32_u16_ll: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_f32_i32_ll: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_f32_u32_ll: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_f32_i64_ll: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_f32_u64_ll: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_f32_f32_ll: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_f32_f64_ll: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH

lbMOVH_f64_i8_ll: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_f64_u8_ll: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt8(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_f64_i16_ll: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_f64_u16_ll: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt16(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_f64_i32_ll: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_f64_u32_ll: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_f64_i64_ll: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_f64_u64_ll: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_f64_f32_ll: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
lbMOVH_f64_f64_ll: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH


// --- Immediate Source, Stack.Local Destination (lbMOVH_SourceType_DestType_li) ---
lbMOVH_i8_i8_li: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i8_u8_li: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i8_i16_li: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i8_u16_li: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i8_i32_li: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i8_u32_li: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i8_i64_li: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i8_u64_li: 
  PInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt64(Data^.Args[1].Data.Arg);
  DISPATCH

lbMOVH_u8_i8_li: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u8_u8_li: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u8_i16_li: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u8_u16_li: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u8_i32_li: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u8_u32_li: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u8_i64_li: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u8_u64_li: 
  PUInt8(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt64(Data^.Args[1].Data.Arg);
  DISPATCH

lbMOVH_i16_i8_li: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i16_u8_li: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i16_i16_li: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i16_u16_li: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i16_i32_li: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i16_u32_li: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i16_i64_li: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i16_u64_li: 
  PInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt64(Data^.Args[1].Data.Arg);
  DISPATCH

lbMOVH_u16_i8_li: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u16_u8_li: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u16_i16_li: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u16_u16_li: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u16_i32_li: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u16_u32_li: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u16_i64_li: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u16_u64_li: 
  PUInt16(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt64(Data^.Args[1].Data.Arg);
  DISPATCH

lbMOVH_i32_i8_li: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i32_u8_li: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i32_i16_li: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i32_u16_li: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i32_i32_li: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i32_u32_li: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i32_i64_li: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i32_u64_li: 
  PInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt64(Data^.Args[1].Data.Arg);
  DISPATCH

lbMOVH_u32_i8_li: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u32_u8_li: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u32_i16_li: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u32_u16_li: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u32_i32_li: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u32_u32_li: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u32_i64_li: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u32_u64_li: 
  PUInt32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt64(Data^.Args[1].Data.Arg);
  DISPATCH

lbMOVH_i64_i8_li: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i64_u8_li: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i64_i16_li: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i64_u16_li: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i64_i32_li: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i64_u32_li: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i64_i64_li: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_i64_u64_li: 
  PInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt64(Data^.Args[1].Data.Arg);
  DISPATCH

lbMOVH_u64_i8_li: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u64_u8_li: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u64_i16_li: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u64_u16_li: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u64_i32_li: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u64_u32_li: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u64_i64_li: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_u64_u64_li: 
  PUInt64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt64(Data^.Args[1].Data.Arg);
  DISPATCH

lbMOVH_f32_i8_li: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_f32_u8_li: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_f32_i16_li: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_f32_u16_li: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_f32_i32_li: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_f32_u32_li: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_f32_i64_li: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_f32_u64_li: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_f32_f32_li: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Float32(Data^.Args[1].Data.Raw32);
  DISPATCH
lbMOVH_f32_f64_li: 
  PFloat32(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Float64(Data^.Args[1].Data.Raw);
  DISPATCH

lbMOVH_f64_i8_li: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_f64_u8_li: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt8(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_f64_i16_li: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_f64_u16_li: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt16(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_f64_i32_li: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_f64_u32_li: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt32(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_f64_i64_li: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Int64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_f64_u64_li: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := UInt64(Data^.Args[1].Data.Arg);
  DISPATCH
lbMOVH_f64_f32_li: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Float32(Data^.Args[1].Data.Raw32);
  DISPATCH
lbMOVH_f64_f64_li: 
  PFloat64(Stack.Local(Data^.Args[0].Data.Addr)^)^ := Float64(Data^.Args[1].Data.Raw);
  DISPATCH
