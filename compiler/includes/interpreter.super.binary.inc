// ADD opcodes
lbADD_lll_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ + PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbADD_lll_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ + PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbADD_lll_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ + PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbADD_lll_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ + PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbADD_lll_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ + PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbADD_lll_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ + PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbADD_lil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ + pc^.Args[1].Data.i32;
  Inc(pc);

lbADD_lil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ + pc^.Args[1].Data.u32;
  Inc(pc);

lbADD_lil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ + Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbADD_lil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ + pc^.Args[1].Data.u64;
  Inc(pc);

lbADD_lil_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ + Float32(pc^.Args[1].Data.Raw32);
  Inc(pc);

lbADD_lil_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ + Float64(pc^.Args[1].Data.Raw);
  Inc(pc);


lbADD_ill_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 + PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbADD_ill_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 + PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbADD_ill_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) + PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbADD_ill_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 + PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbADD_ill_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float32(pc^.Args[0].Data.Raw32) + PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbADD_ill_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float64(pc^.Args[0].Data.Raw) + PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbADD_iil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 + pc^.Args[1].Data.i32;
  Inc(pc);

lbADD_iil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 + pc^.Args[1].Data.u32;
  Inc(pc);

lbADD_iil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) + Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbADD_iil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 + pc^.Args[1].Data.u64;
  Inc(pc);

lbADD_iil_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float32(pc^.Args[0].Data.Raw32) + Float32(pc^.Args[1].Data.Raw32);
  Inc(pc);

lbADD_iil_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float64(pc^.Args[0].Data.Raw) + Float64(pc^.Args[1].Data.Raw);
  Inc(pc);


 // SUB opcodes
lbSUB_lll_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ - PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSUB_lll_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ - PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSUB_lll_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ - PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSUB_lll_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ - PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSUB_lll_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ - PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSUB_lll_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ - PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbSUB_lil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ - pc^.Args[1].Data.i32;
  Inc(pc);

lbSUB_lil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ - pc^.Args[1].Data.u32;
  Inc(pc);

lbSUB_lil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ - Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbSUB_lil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ - pc^.Args[1].Data.u64;
  Inc(pc);

lbSUB_lil_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ - Float32(pc^.Args[1].Data.Raw32);
  Inc(pc);

lbSUB_lil_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ - Float64(pc^.Args[1].Data.Raw);
  Inc(pc);


lbSUB_ill_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 - PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSUB_ill_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 - PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSUB_ill_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) - PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSUB_ill_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 - PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSUB_ill_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float32(pc^.Args[0].Data.Raw32) - PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSUB_ill_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float64(pc^.Args[0].Data.Raw) - PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbSUB_iil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 - pc^.Args[1].Data.i32;
  Inc(pc);

lbSUB_iil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 - pc^.Args[1].Data.u32;
  Inc(pc);

lbSUB_iil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) - Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbSUB_iil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 - pc^.Args[1].Data.u64;
  Inc(pc);

lbSUB_iil_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float32(pc^.Args[0].Data.Raw32) - Float32(pc^.Args[1].Data.Raw32);
  Inc(pc);

lbSUB_iil_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float64(pc^.Args[0].Data.Raw) - Float64(pc^.Args[1].Data.Raw);
  Inc(pc);


 // MUL opcodes
lbMUL_lll_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbMUL_lll_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbMUL_lll_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbMUL_lll_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbMUL_lll_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbMUL_lll_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbMUL_lil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.i32;
  Inc(pc);

lbMUL_lil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.u32;
  Inc(pc);

lbMUL_lil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbMUL_lil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * pc^.Args[1].Data.u64;
  Inc(pc);

lbMUL_lil_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * Float32(pc^.Args[1].Data.Raw32);
  Inc(pc);

lbMUL_lil_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ * Float64(pc^.Args[1].Data.Raw);
  Inc(pc);


lbMUL_ill_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 * PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbMUL_ill_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 * PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbMUL_ill_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) * PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbMUL_ill_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 * PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbMUL_ill_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float32(pc^.Args[0].Data.Raw32) * PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbMUL_ill_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float64(pc^.Args[0].Data.Raw) * PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbMUL_iil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 * pc^.Args[1].Data.i32;
  Inc(pc);

lbMUL_iil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 * pc^.Args[1].Data.u32;
  Inc(pc);

lbMUL_iil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) * Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbMUL_iil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 * pc^.Args[1].Data.u64;
  Inc(pc);

lbMUL_iil_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float32(pc^.Args[0].Data.Raw32) * Float32(pc^.Args[1].Data.Raw32);
  Inc(pc);

lbMUL_iil_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float64(pc^.Args[0].Data.Raw) * Float64(pc^.Args[1].Data.Raw);
  Inc(pc);


 // DIV opcodes
lbDIV_lll_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ div PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbDIV_lll_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ div PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbDIV_lll_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ div PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbDIV_lll_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ div PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbDIV_lll_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ / PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbDIV_lll_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ / PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbDIV_lil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ div pc^.Args[1].Data.i32;
  Inc(pc);

lbDIV_lil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ div pc^.Args[1].Data.u32;
  Inc(pc);

lbDIV_lil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ div Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbDIV_lil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ div pc^.Args[1].Data.u64;
  Inc(pc);

lbDIV_lil_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ / Float32(pc^.Args[1].Data.Raw32);
  Inc(pc);

lbDIV_lil_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ / Float64(pc^.Args[1].Data.Raw);
  Inc(pc);


lbDIV_ill_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 div PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbDIV_ill_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 div PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbDIV_ill_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) div PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbDIV_ill_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 div PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbDIV_ill_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float32(pc^.Args[0].Data.Raw32) / PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbDIV_ill_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float64(pc^.Args[0].Data.Raw) / PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbDIV_iil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 div pc^.Args[1].Data.i32;
  Inc(pc);

lbDIV_iil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 div pc^.Args[1].Data.u32;
  Inc(pc);

lbDIV_iil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) div Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbDIV_iil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 div pc^.Args[1].Data.u64;
  Inc(pc);

lbDIV_iil_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float32(pc^.Args[0].Data.Raw32) / Float32(pc^.Args[1].Data.Raw32);
  Inc(pc);

lbDIV_iil_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float64(pc^.Args[0].Data.Raw) / Float64(pc^.Args[1].Data.Raw);
  Inc(pc);


 // --- MOD opcodes (Modulo) ---
 // lll (local, local, local)
lbMOD_lll_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbMOD_lll_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbMOD_lll_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbMOD_lll_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbMOD_lll_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbMOD_lll_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);


 // lil (local, immediate, local)
lbMOD_lil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, pc^.Args[1].Data.i32);
  Inc(pc);

lbMOD_lil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, pc^.Args[1].Data.u32);
  Inc(pc);

lbMOD_lil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, Int64(pc^.Args[1].Data.Arg));
  Inc(pc);

lbMOD_lil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, UInt64(pc^.Args[1].Data.Arg));
  Inc(pc);

lbMOD_lil_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, Float32(pc^.Args[1].Data.Raw32));
  Inc(pc);

lbMOD_lil_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, Float64(pc^.Args[1].Data.Raw));
  Inc(pc);


 // ill (immediate, local, local)
lbMOD_ill_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(pc^.Args[0].Data.i32, PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbMOD_ill_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(pc^.Args[0].Data.u32, PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbMOD_ill_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(Int64(pc^.Args[0].Data.Arg), PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbMOD_ill_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(pc^.Args[0].Data.u64, PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbMOD_ill_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(Float32(pc^.Args[0].Data.Raw32), PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbMOD_ill_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(Float64(pc^.Args[0].Data.Raw), PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);


 // iil (immediate, immediate, local)
lbMOD_iil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(pc^.Args[0].Data.i32, pc^.Args[1].Data.i32);
  Inc(pc);

lbMOD_iil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(pc^.Args[0].Data.u32, pc^.Args[1].Data.u32);
  Inc(pc);

lbMOD_iil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(Int64(pc^.Args[0].Data.Arg), Int64(pc^.Args[1].Data.Arg));
  Inc(pc);

lbMOD_iil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(pc^.Args[0].Data.u64, UInt64(pc^.Args[1].Data.Arg));
  Inc(pc);

lbMOD_iil_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(Float32(pc^.Args[0].Data.Raw32), Float32(pc^.Args[1].Data.Raw32));
  Inc(pc);

lbMOD_iil_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Modulo(Float64(pc^.Args[0].Data.Raw), Float64(pc^.Args[1].Data.Raw));
  Inc(pc);


 // POW opcodes (Assuming a Power function is available)
lbPOW_lll_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbPOW_lll_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbPOW_lll_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbPOW_lll_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbPOW_lll_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbPOW_lll_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);


lbPOW_lil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, pc^.Args[1].Data.i32);
  Inc(pc);

lbPOW_lil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, pc^.Args[1].Data.u32);
  Inc(pc);

lbPOW_lil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, Int64(pc^.Args[1].Data.Arg));
  Inc(pc);

lbPOW_lil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, UInt64(pc^.Args[1].Data.Arg));
  Inc(pc);

lbPOW_lil_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, Float32(pc^.Args[1].Data.Raw32));
  Inc(pc);

lbPOW_lil_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, Float64(pc^.Args[1].Data.Raw));
  Inc(pc);


lbPOW_ill_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(pc^.Args[0].Data.i32, PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbPOW_ill_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(pc^.Args[0].Data.u32, PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbPOW_ill_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(Int64(pc^.Args[0].Data.Arg), PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbPOW_ill_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(pc^.Args[0].Data.u64, PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbPOW_ill_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(Float32(pc^.Args[0].Data.Raw32), PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbPOW_ill_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(Float64(pc^.Args[0].Data.Raw), PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);


lbPOW_iil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(pc^.Args[0].Data.i32, pc^.Args[1].Data.i32);
  Inc(pc);

lbPOW_iil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(pc^.Args[0].Data.u32, pc^.Args[1].Data.u32);
  Inc(pc);

lbPOW_iil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(Int64(pc^.Args[0].Data.Arg), Int64(pc^.Args[1].Data.Arg));
  Inc(pc);

lbPOW_iil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(pc^.Args[0].Data.u64, UInt64(pc^.Args[1].Data.Arg));
  Inc(pc);

lbPOW_iil_f32:
  PFloat32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(Float32(pc^.Args[0].Data.Raw32), Float32(pc^.Args[1].Data.Raw32));
  Inc(pc);

lbPOW_iil_f64:
  PFloat64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Power(Float64(pc^.Args[0].Data.Raw), Float64(pc^.Args[1].Data.Raw));
  Inc(pc);


 // NE opcodes
lbNE_lll_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <> PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbNE_lll_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <> PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbNE_lll_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <> PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbNE_lll_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <> PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbNE_lll_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <> PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbNE_lll_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <> PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbNE_lil_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <> pc^.Args[1].Data.i32;
  Inc(pc);

lbNE_lil_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <> pc^.Args[1].Data.u32;
  Inc(pc);

lbNE_lil_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <> Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbNE_lil_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <> pc^.Args[1].Data.u64;
  Inc(pc);

lbNE_lil_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <> Float32(pc^.Args[1].Data.Raw32);
  Inc(pc);

lbNE_lil_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <> Float64(pc^.Args[1].Data.Raw);
  Inc(pc);


lbNE_ill_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 <> PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbNE_ill_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 <> PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbNE_ill_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) <> PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbNE_ill_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 <> PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbNE_ill_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float32(pc^.Args[0].Data.Raw32) <> PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbNE_ill_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float64(pc^.Args[0].Data.Raw) <> PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbNE_iil_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 <> pc^.Args[1].Data.i32;
  Inc(pc);

lbNE_iil_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 <> pc^.Args[1].Data.u32;
  Inc(pc);

lbNE_iil_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) <> Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbNE_iil_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 <> pc^.Args[1].Data.u64;
  Inc(pc);

lbNE_iil_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float32(pc^.Args[0].Data.Raw32) <> Float32(pc^.Args[1].Data.Raw32);
  Inc(pc);

lbNE_iil_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float64(pc^.Args[0].Data.Raw) <> Float64(pc^.Args[1].Data.Raw);
  Inc(pc);


 // EQ opcodes
lbEQ_lll_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ = PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbEQ_lll_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ = PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbEQ_lll_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ = PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbEQ_lll_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ = PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbEQ_lll_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ = PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbEQ_lll_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ = PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbEQ_lil_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ = pc^.Args[1].Data.i32;
  Inc(pc);

lbEQ_lil_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ = pc^.Args[1].Data.u32;
  Inc(pc);

lbEQ_lil_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ = Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbEQ_lil_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ = pc^.Args[1].Data.u64;
  Inc(pc);

lbEQ_lil_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ = Float32(pc^.Args[1].Data.Raw32);
  Inc(pc);

lbEQ_lil_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ = Float64(pc^.Args[1].Data.Raw);
  Inc(pc);


lbEQ_ill_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 = PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbEQ_ill_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 = PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbEQ_ill_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) = PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbEQ_ill_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 = PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbEQ_ill_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float32(pc^.Args[0].Data.Raw32) = PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbEQ_ill_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float64(pc^.Args[0].Data.Raw) = PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbEQ_iil_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 = pc^.Args[1].Data.i32;
  Inc(pc);

lbEQ_iil_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 = pc^.Args[1].Data.u32;
  Inc(pc);

lbEQ_iil_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) = Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbEQ_iil_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 = pc^.Args[1].Data.u64;
  Inc(pc);

lbEQ_iil_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float32(pc^.Args[0].Data.Raw32) = Float32(pc^.Args[1].Data.Raw32);
  Inc(pc);

lbEQ_iil_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float64(pc^.Args[0].Data.Raw) = Float64(pc^.Args[1].Data.Raw);
  Inc(pc);


 // LT opcodes
lbLT_lll_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ < PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbLT_lll_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ < PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbLT_lll_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ < PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbLT_lll_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ < PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbLT_lll_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ < PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbLT_lll_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ < PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbLT_lil_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ < pc^.Args[1].Data.i32;
  Inc(pc);

lbLT_lil_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ < pc^.Args[1].Data.u32;
  Inc(pc);

lbLT_lil_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ < Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbLT_lil_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ < pc^.Args[1].Data.u64;
  Inc(pc);

lbLT_lil_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ < Float32(pc^.Args[1].Data.Raw32);
  Inc(pc);

lbLT_lil_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ < Float64(pc^.Args[1].Data.Raw);
  Inc(pc);


lbLT_ill_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 < PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbLT_ill_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 < PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbLT_ill_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) < PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbLT_ill_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 < PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbLT_ill_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float32(pc^.Args[0].Data.Raw32) < PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbLT_ill_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float64(pc^.Args[0].Data.Raw) < PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbLT_iil_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 < pc^.Args[1].Data.i32;
  Inc(pc);

lbLT_iil_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 < pc^.Args[1].Data.u32;
  Inc(pc);

lbLT_iil_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) < Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbLT_iil_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 < pc^.Args[1].Data.u64;
  Inc(pc);

lbLT_iil_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float32(pc^.Args[0].Data.Raw32) < Float32(pc^.Args[1].Data.Raw32);
  Inc(pc);

lbLT_iil_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float64(pc^.Args[0].Data.Raw) < Float64(pc^.Args[1].Data.Raw);
  Inc(pc);



  // GT opcodes
lbGT_lll_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ > PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbGT_lll_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ > PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbGT_lll_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ > PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbGT_lll_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ > PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbGT_lll_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ > PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbGT_lll_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ > PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbGT_lil_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ > pc^.Args[1].Data.i32;
  Inc(pc);

lbGT_lil_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ > pc^.Args[1].Data.u32;
  Inc(pc);

lbGT_lil_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ > Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbGT_lil_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ > pc^.Args[1].Data.u64;
  Inc(pc);

lbGT_lil_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ > Float32(pc^.Args[1].Data.Raw32);
  Inc(pc);

lbGT_lil_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ > Float64(pc^.Args[1].Data.Raw);
  Inc(pc);


lbGT_ill_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 > PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbGT_ill_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 > PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbGT_ill_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) > PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbGT_ill_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 > PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbGT_ill_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float32(pc^.Args[0].Data.Raw32) > PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbGT_ill_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float64(pc^.Args[0].Data.Raw) > PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbGT_iil_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 > pc^.Args[1].Data.i32;
  Inc(pc);

lbGT_iil_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 > pc^.Args[1].Data.u32;
  Inc(pc);

lbGT_iil_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) > Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbGT_iil_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 > pc^.Args[1].Data.u64;
  Inc(pc);

lbGT_iil_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float32(pc^.Args[0].Data.Raw32) > Float32(pc^.Args[1].Data.Raw32);
  Inc(pc);

lbGT_iil_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float64(pc^.Args[0].Data.Raw) > Float64(pc^.Args[1].Data.Raw);
  Inc(pc);


  // GTE opcodes
lbGTE_lll_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ >= PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbGTE_lll_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ >= PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbGTE_lll_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ >= PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbGTE_lll_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ >= PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbGTE_lll_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ >= PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbGTE_lll_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ >= PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbGTE_lil_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ >= pc^.Args[1].Data.i32;
  Inc(pc);

lbGTE_lil_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ >= pc^.Args[1].Data.u32;
  Inc(pc);

lbGTE_lil_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ >= Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbGTE_lil_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ >= pc^.Args[1].Data.u64;
  Inc(pc);

lbGTE_lil_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ >= Float32(pc^.Args[1].Data.Raw32);
  Inc(pc);

lbGTE_lil_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ >= Float64(pc^.Args[1].Data.Raw);
  Inc(pc);


lbGTE_ill_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 >= PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbGTE_ill_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 >= PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbGTE_ill_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) >= PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbGTE_ill_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 >= PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbGTE_ill_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float32(pc^.Args[0].Data.Raw32) >= PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbGTE_ill_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float64(pc^.Args[0].Data.Raw) >= PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbGTE_iil_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 >= pc^.Args[1].Data.i32;
  Inc(pc);

lbGTE_iil_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 >= pc^.Args[1].Data.u32;
  Inc(pc);

lbGTE_iil_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) >= Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbGTE_iil_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 >= pc^.Args[1].Data.u64;
  Inc(pc);

lbGTE_iil_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float32(pc^.Args[0].Data.Raw32) >= Float32(pc^.Args[1].Data.Raw32);
  Inc(pc);

lbGTE_iil_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float64(pc^.Args[0].Data.Raw) >= Float64(pc^.Args[1].Data.Raw);
  Inc(pc);


  // LTE opcodes
lbLTE_lll_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <= PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbLTE_lll_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <= PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbLTE_lll_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <= PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbLTE_lll_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <= PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbLTE_lll_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <= PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbLTE_lll_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <= PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbLTE_lil_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <= pc^.Args[1].Data.i32;
  Inc(pc);

lbLTE_lil_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <= pc^.Args[1].Data.u32;
  Inc(pc);

lbLTE_lil_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <= Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbLTE_lil_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <= pc^.Args[1].Data.u64;
  Inc(pc);

lbLTE_lil_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <= Float32(pc^.Args[1].Data.Raw32);
  Inc(pc);

lbLTE_lil_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PFloat64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ <= Float64(pc^.Args[1].Data.Raw);
  Inc(pc);


lbLTE_ill_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 <= PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbLTE_ill_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 <= PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbLTE_ill_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) <= PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbLTE_ill_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 <= PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbLTE_ill_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float32(pc^.Args[0].Data.Raw32) <= PFloat32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbLTE_ill_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float64(pc^.Args[0].Data.Raw) <= PFloat64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbLTE_iil_i32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 <= pc^.Args[1].Data.i32;
  Inc(pc);

lbLTE_iil_u32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 <= pc^.Args[1].Data.u32;
  Inc(pc);

lbLTE_iil_i64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) <= Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbLTE_iil_u64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 <= pc^.Args[1].Data.u64;
  Inc(pc);

lbLTE_iil_f32:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float32(pc^.Args[0].Data.Raw32) <= Float32(pc^.Args[1].Data.Raw32);
  Inc(pc);

lbLTE_iil_f64:
  PBoolean(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Float64(pc^.Args[0].Data.Raw) <= Float64(pc^.Args[1].Data.Raw);
  Inc(pc);


  // BND opcodes (Bitwise AND)
lbBND_lll_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ and PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbBND_lll_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ and PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbBND_lll_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ and PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbBND_lll_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ and PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbBND_lil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ and pc^.Args[1].Data.i32;
  Inc(pc);

lbBND_lil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ and pc^.Args[1].Data.u32;
  Inc(pc);

lbBND_lil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ and Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbBND_lil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ and pc^.Args[1].Data.u64;
  Inc(pc);


lbBND_ill_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 and PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbBND_ill_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 and PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbBND_ill_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) and PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbBND_ill_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 and PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbBND_iil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 and pc^.Args[1].Data.i32;
  Inc(pc);

lbBND_iil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 and pc^.Args[1].Data.u32;
  Inc(pc);

lbBND_iil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) and Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbBND_iil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 and pc^.Args[1].Data.u64;
  Inc(pc);


  // SHL opcodes (Shift Left)
lbSHL_lll_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ shl PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSHL_lll_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ shl PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSHL_lll_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ shl PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSHL_lll_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ shl PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbSHL_lil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ shl pc^.Args[1].Data.i32;
  Inc(pc);

lbSHL_lil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ shl pc^.Args[1].Data.u32;
  Inc(pc);

lbSHL_lil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ shl Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbSHL_lil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ shl pc^.Args[1].Data.u64;
  Inc(pc);


lbSHL_ill_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 shl PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSHL_ill_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 shl PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSHL_ill_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) shl PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSHL_ill_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 shl PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbSHL_iil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 shl pc^.Args[1].Data.i32;
  Inc(pc);

lbSHL_iil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 shl pc^.Args[1].Data.u32;
  Inc(pc);

lbSHL_iil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) shl Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbSHL_iil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 shl pc^.Args[1].Data.u64;
  Inc(pc);


  // SHR opcodes (Shift Right)
lbSHR_lll_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ shr PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSHR_lll_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ shr PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSHR_lll_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ shr PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSHR_lll_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ shr PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbSHR_lil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ shr pc^.Args[1].Data.i32;
  Inc(pc);

lbSHR_lil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ shr pc^.Args[1].Data.u32;
  Inc(pc);

lbSHR_lil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ shr Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbSHR_lil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ shr pc^.Args[1].Data.u64;
  Inc(pc);


lbSHR_ill_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 shr PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSHR_ill_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 shr PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSHR_ill_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) shr PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbSHR_ill_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 shr PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbSHR_iil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 shr pc^.Args[1].Data.i32;
  Inc(pc);

lbSHR_iil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 shr pc^.Args[1].Data.u32;
  Inc(pc);

lbSHR_iil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) shr Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbSHR_iil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 shr pc^.Args[1].Data.u64;
  Inc(pc);


  // XOR opcodes (Bitwise XOR)
lbXOR_lll_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ xor PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbXOR_lll_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ xor PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbXOR_lll_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ xor PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbXOR_lll_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ xor PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbXOR_lil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ xor pc^.Args[1].Data.i32;
  Inc(pc);

lbXOR_lil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ xor pc^.Args[1].Data.u32;
  Inc(pc);

lbXOR_lil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ xor Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbXOR_lil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ xor pc^.Args[1].Data.u64;
  Inc(pc);


lbXOR_ill_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 xor PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbXOR_ill_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 xor PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbXOR_ill_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) xor PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbXOR_ill_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 xor PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbXOR_iil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 xor pc^.Args[1].Data.i32;
  Inc(pc);

lbXOR_iil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 xor pc^.Args[1].Data.u32;
  Inc(pc);

lbXOR_iil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) xor Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbXOR_iil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 xor pc^.Args[1].Data.u64;
  Inc(pc);


  // BOR opcodes (Bitwise OR)
lbBOR_lll_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ or PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbBOR_lll_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ or PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbBOR_lll_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ or PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbBOR_lll_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ or PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbBOR_lil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ or pc^.Args[1].Data.i32;
  Inc(pc);

lbBOR_lil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ or pc^.Args[1].Data.u32;
  Inc(pc);

lbBOR_lil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ or Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbBOR_lil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^ or pc^.Args[1].Data.u64;
  Inc(pc);


lbBOR_ill_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 or PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbBOR_ill_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 or PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbBOR_ill_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) or PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);

lbBOR_ill_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 or PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^;
  Inc(pc);


lbBOR_iil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.i32 or pc^.Args[1].Data.i32;
  Inc(pc);

lbBOR_iil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u32 or pc^.Args[1].Data.u32;
  Inc(pc);

lbBOR_iil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Int64(pc^.Args[0].Data.Arg) or Int64(pc^.Args[1].Data.Arg);
  Inc(pc);

lbBOR_iil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := pc^.Args[0].Data.u64 or pc^.Args[1].Data.u64;
  Inc(pc);


  // --- SAR opcodes (Shift Arithmetic Right) ---
  // lll (Local, Local, Local)
lbSAR_lll_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Sar(PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbSAR_lll_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Sar(PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbSAR_lll_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Sar(PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbSAR_lll_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Sar(PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);


  // lil (Local, immediate, Local)
lbSAR_lil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Sar(PInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, pc^.Args[1].Data.i32);
  Inc(pc);

lbSAR_lil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Sar(PUInt32(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, pc^.Args[1].Data.u32);
  Inc(pc);

lbSAR_lil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Sar(PInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, Int64(pc^.Args[1].Data.Arg));
  Inc(pc);

lbSAR_lil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Sar(PUInt64(Pointer(BasePtr + pc^.Args[0].Data.Addr))^, UInt64(pc^.Args[1].Data.Arg));
  Inc(pc);


  // ill (immediate, Local, Local)
lbSAR_ill_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Sar(pc^.Args[0].Data.i32, PInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbSAR_ill_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Sar(pc^.Args[0].Data.u32, PUInt32(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbSAR_ill_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Sar(Int64(pc^.Args[0].Data.Arg), PInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);

lbSAR_ill_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Sar(pc^.Args[0].Data.u64, PUInt64(Pointer(BasePtr + pc^.Args[1].Data.Addr))^);
  Inc(pc);


  // iil (immediate, immediate, Local)
lbSAR_iil_i32:
  PInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Sar(pc^.Args[0].Data.i32, pc^.Args[1].Data.i32);
  Inc(pc);

lbSAR_iil_u32:
  PUInt32(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Sar(pc^.Args[0].Data.u32, pc^.Args[1].Data.u32);
  Inc(pc);

lbSAR_iil_i64:
  PInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Sar(Int64(pc^.Args[0].Data.Arg), Int64(pc^.Args[1].Data.Arg));
  Inc(pc);

lbSAR_iil_u64:
  PUInt64(Pointer(BasePtr + pc^.Args[2].Data.Addr))^ := Sar(pc^.Args[0].Data.u64, UInt64(pc^.Args[1].Data.Arg));
  Inc(pc);

