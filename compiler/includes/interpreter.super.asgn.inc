lbMOV: (* for copy reference*)

  // --- Same-Size or Truncating ---
  lbMOV8:  PInt8 (MEMBASE_0)^ := PInt8 (MEMBASE_1)^; Inc(pc);
  lbMOV16: PInt16(MEMBASE_0)^ := PInt16(MEMBASE_1)^; Inc(pc);
  lbMOV32: PInt32(MEMBASE_0)^ := PInt32(MEMBASE_1)^; Inc(pc);
  lbMOV64: PInt64(MEMBASE_0)^ := PInt64(MEMBASE_1)^; Inc(pc);

  // --- Widening Signed (Sign Extension) ---
  lbMOVSX16_8:  PInt16(MEMBASE_0)^ := Int16(PInt8(MEMBASE_1)^); Inc(pc);
  lbMOVSX32_8:  PInt32(MEMBASE_0)^ := Int32(PInt8(MEMBASE_1)^); Inc(pc);
  lbMOVSX32_16: PInt32(MEMBASE_0)^ := Int32(PInt16(MEMBASE_1)^); Inc(pc);
  lbMOVSX64_8:  PInt64(MEMBASE_0)^ := Int64(PInt8(MEMBASE_1)^); Inc(pc);
  lbMOVSX64_16: PInt64(MEMBASE_0)^ := Int64(PInt16(MEMBASE_1)^); Inc(pc);
  lbMOVSX64_32: PInt64(MEMBASE_0)^ := Int64(PInt32(MEMBASE_1)^); Inc(pc);

  // --- Widening Unsigned (Zero Extension) ---
  lbMOVZX16_8:  PUInt16(MEMBASE_0)^ := UInt16(PUInt8(MEMBASE_1)^); Inc(pc);
  lbMOVZX32_8:  PUInt32(MEMBASE_0)^ := UInt32(PUInt8(MEMBASE_1)^); Inc(pc);
  lbMOVZX32_16: PUInt32(MEMBASE_0)^ := UInt32(PUInt16(MEMBASE_1)^); Inc(pc);
  lbMOVZX64_8:  PUInt64(MEMBASE_0)^ := UInt64(PUInt8(MEMBASE_1)^); Inc(pc);
  lbMOVZX64_16: PUInt64(MEMBASE_0)^ := UInt64(PUInt16(MEMBASE_1)^); Inc(pc);
  lbMOVZX64_32: PUInt64(MEMBASE_0)^ := UInt64(PUInt32(MEMBASE_1)^); Inc(pc);

  // --- Float Conversion ---
  lbMOVF32_i32: PFloat32(MEMBASE_0)^ := Float32(PInt32(MEMBASE_1)^); Inc(pc);
  lbMOVF32_u32: PFloat32(MEMBASE_0)^ := Float32(PUInt32(MEMBASE_1)^); Inc(pc);
  lbMOVF32_i64: PFloat32(MEMBASE_0)^ := Float32(PInt64(MEMBASE_1)^); Inc(pc);
  lbMOVF32_u64: PFloat32(MEMBASE_0)^ := Float32(PUInt64(MEMBASE_1)^); Inc(pc);
  lbMOVF32_f32: PFloat32(MEMBASE_0)^ := PFloat32(MEMBASE_1)^; Inc(pc);
  lbMOVF32_f64: PFloat32(MEMBASE_0)^ := Float32(PFloat64(MEMBASE_1)^); Inc(pc);

  lbMOVF64_i32: PFloat64(MEMBASE_0)^ := Float64(PInt32(MEMBASE_1)^); Inc(pc);
  lbMOVF64_u32: PFloat64(MEMBASE_0)^ := Float64(PUInt32(MEMBASE_1)^); Inc(pc);
  lbMOVF64_i64: PFloat64(MEMBASE_0)^ := Float64(PInt64(MEMBASE_1)^); Inc(pc);
  lbMOVF64_u64: PFloat64(MEMBASE_0)^ := Float64(PUInt64(MEMBASE_1)^); Inc(pc);
  lbMOVF64_f32: PFloat64(MEMBASE_0)^ := Float64(PFloat32(MEMBASE_1)^); Inc(pc);
  lbMOVF64_f64: PFloat64(MEMBASE_0)^ := PFloat64(MEMBASE_1)^; Inc(pc);


lbSTORE: (* for copy reference*)

  // --- Same-Size or Truncating ---
  lbSTORE8:  PInt8 (PPointer(MEMBASE_0)^)^ := PInt8 (MEMBASE_1)^; Inc(pc);
  lbSTORE16: PInt16(PPointer(MEMBASE_0)^)^ := PInt16(MEMBASE_1)^; Inc(pc);
  lbSTORE32: PInt32(PPointer(MEMBASE_0)^)^ := PInt32(MEMBASE_1)^; Inc(pc);
  lbSTORE64: PInt64(PPointer(MEMBASE_0)^)^ := PInt64(MEMBASE_1)^; Inc(pc);

  // --- Widening Signed (Sign Extension) ---
  lbSTORESX16_8:  PInt16(PPointer(MEMBASE_0)^)^ := Int16(PInt8(MEMBASE_1)^); Inc(pc);
  lbSTORESX32_8:  PInt32(PPointer(MEMBASE_0)^)^ := Int32(PInt8(MEMBASE_1)^); Inc(pc);
  lbSTORESX32_16: PInt32(PPointer(MEMBASE_0)^)^ := Int32(PInt16(MEMBASE_1)^); Inc(pc);
  lbSTORESX64_8:  PInt64(PPointer(MEMBASE_0)^)^ := Int64(PInt8(MEMBASE_1)^); Inc(pc);
  lbSTORESX64_16: PInt64(PPointer(MEMBASE_0)^)^ := Int64(PInt16(MEMBASE_1)^); Inc(pc);
  lbSTORESX64_32: PInt64(PPointer(MEMBASE_0)^)^ := Int64(PInt32(MEMBASE_1)^); Inc(pc);

  // --- Widening Unsigned (Zero Extension) ---
  lbSTOREZX16_8:  PUInt16(PPointer(MEMBASE_0)^)^ := UInt16(PUInt8(MEMBASE_1)^); Inc(pc);
  lbSTOREZX32_8:  PUInt32(PPointer(MEMBASE_0)^)^ := UInt32(PUInt8(MEMBASE_1)^); Inc(pc);
  lbSTOREZX32_16: PUInt32(PPointer(MEMBASE_0)^)^ := UInt32(PUInt16(MEMBASE_1)^); Inc(pc);
  lbSTOREZX64_8:  PUInt64(PPointer(MEMBASE_0)^)^ := UInt64(PUInt8(MEMBASE_1)^); Inc(pc);
  lbSTOREZX64_16: PUInt64(PPointer(MEMBASE_0)^)^ := UInt64(PUInt16(MEMBASE_1)^); Inc(pc);
  lbSTOREZX64_32: PUInt64(PPointer(MEMBASE_0)^)^ := UInt64(PUInt32(MEMBASE_1)^); Inc(pc);

  // --- Float Conversion ---
  lbSTOREF32_i32: PFloat32(PPointer(MEMBASE_0)^)^ := Float32(PInt32(MEMBASE_1)^); Inc(pc);
  lbSTOREF32_u32: PFloat32(PPointer(MEMBASE_0)^)^ := Float32(PUInt32(MEMBASE_1)^); Inc(pc);
  lbSTOREF32_i64: PFloat32(PPointer(MEMBASE_0)^)^ := Float32(PInt64(MEMBASE_1)^); Inc(pc);
  lbSTOREF32_u64: PFloat32(PPointer(MEMBASE_0)^)^ := Float32(PUInt64(MEMBASE_1)^); Inc(pc);
  lbSTOREF32_f32: PFloat32(PPointer(MEMBASE_0)^)^ := PFloat32(MEMBASE_1)^; Inc(pc);
  lbSTOREF32_f64: PFloat32(PPointer(MEMBASE_0)^)^ := Float32(PFloat64(MEMBASE_1)^); Inc(pc);

  lbSTOREF64_i32: PFloat64(PPointer(MEMBASE_0)^)^ := Float64(PInt32(MEMBASE_1)^); Inc(pc);
  lbSTOREF64_u32: PFloat64(PPointer(MEMBASE_0)^)^ := Float64(PUInt32(MEMBASE_1)^); Inc(pc);
  lbSTOREF64_i64: PFloat64(PPointer(MEMBASE_0)^)^ := Float64(PInt64(MEMBASE_1)^); Inc(pc);
  lbSTOREF64_u64: PFloat64(PPointer(MEMBASE_0)^)^ := Float64(PUInt64(MEMBASE_1)^); Inc(pc);
  lbSTOREF64_f32: PFloat64(PPointer(MEMBASE_0)^)^ := Float64(PFloat32(MEMBASE_1)^); Inc(pc);
  lbSTOREF64_f64: PFloat64(PPointer(MEMBASE_0)^)^ := PFloat64(MEMBASE_1)^; Inc(pc);        

lbERROR:
