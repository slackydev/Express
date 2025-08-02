  // --- Arithmetic Operations ---

  lbADD: lbSUB: lbMUL: lbDIV: lbMOD: lbPOW:
  lbBND: lbBOR: lbXOR: lbEQ: lbNE:
  lbGT: lbLT: lbGTE: lbLTE:
  lbSAR: lbSHL: lbSHR:
  begin
    (*
    // Point left operand
    if Data^.Args[0].Pos = mpLocal then
      left := Stack.Local(Data^.Args[0].Data.Addr)
    else
      left := @Data^.Args[0].Arg;   

    // Point right operand
    if Data^.Args[1].Pos = mpLocal then
      right := Stack.Local(Data^.Args[1].Data.Addr)
    else
      right := @Data^.Args[1].Arg; 

    // Type dispatch
    case Data^.Args[0].BaseType of
     xtInt8:
       case BC.Code.Data^.Code of
         bcADD: PInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt8(left)^ + PInt8(right)^;
         bcSUB: PInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt8(left)^ - PInt8(right)^;
         bcMUL: PInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt8(left)^ * PInt8(right)^;
         bcDIV: PInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt8(left)^ div PInt8(right)^;
         bcMOD: PInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(PInt8(left)^, PInt8(right)^);
         bcEQ:  PInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PInt8(left)^ = PInt8(right)^);
         bcNE:  PInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PInt8(left)^ <> PInt8(right)^);
         bcGT:  PInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PInt8(left)^ > PInt8(right)^);
         bcLT:  PInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PInt8(left)^ < PInt8(right)^);
         bcGTE: PInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PInt8(left)^ >= PInt8(right)^);
         bcLTE: PInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PInt8(left)^ <= PInt8(right)^);
         bcBND: PInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt8(left)^ and PInt8(right)^;
         bcBOR: PInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt8(left)^ or  PInt8(right)^;
         bcXOR: PInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt8(left)^ xor PInt8(right)^;
         bcSAR: PInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := Sar(PInt8(left)^, PInt8(right)^);
         bcSHR: PInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt8(left)^ shr PInt8(right)^;
         bcSHL: PInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt8(left)^ shl PInt8(right)^;
       end;

     xtUInt8:
       case BC.Code.Data^.Code of
         bcADD: PUInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt8(left)^ + PUInt8(right)^;
         bcSUB: PUInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt8(left)^ - PUInt8(right)^;
         bcMUL: PUInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt8(left)^ * PUInt8(right)^;
         bcDIV: PUInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt8(left)^ div PUInt8(right)^;
         bcMOD: PUInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(PUInt8(left)^, PUInt8(right)^);
         bcEQ:  PUInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PUInt8(left)^ = PUInt8(right)^);
         bcNE:  PUInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PUInt8(left)^ <> PUInt8(right)^);
         bcGT:  PUInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PUInt8(left)^ > PUInt8(right)^);
         bcLT:  PUInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PUInt8(left)^ < PUInt8(right)^);
         bcGTE: PUInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PUInt8(left)^ >= PUInt8(right)^);
         bcLTE: PUInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PUInt8(left)^ <= PUInt8(right)^);
         bcBND: PUInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt8(left)^ and PUInt8(right)^;
         bcBOR: PUInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt8(left)^ or  PUInt8(right)^;
         bcXOR: PUInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt8(left)^ xor PUInt8(right)^;
         bcSAR: PUInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := Sar(PUInt8(left)^, PUInt8(right)^);
         bcSHR: PUInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt8(left)^ shr PUInt8(right)^;
         bcSHL: PUInt8(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt8(left)^ shl PUInt8(right)^;
       end;

     xtInt16:
       case BC.Code.Data^.Code of
         bcADD: PInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt16(left)^ + PInt16(right)^;
         bcSUB: PInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt16(left)^ - PInt16(right)^;
         bcMUL: PInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt16(left)^ * PInt16(right)^;
         bcDIV: PInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt16(left)^ div PInt16(right)^;
         bcMOD: PInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(PInt16(left)^, PInt16(right)^);
         bcEQ:  PInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PInt16(left)^ = PInt16(right)^);
         bcNE:  PInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PInt16(left)^ <> PInt16(right)^);
         bcGT:  PInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PInt16(left)^ > PInt16(right)^);
         bcLT:  PInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PInt16(left)^ < PInt16(right)^);
         bcGTE: PInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PInt16(left)^ >= PInt16(right)^);
         bcLTE: PInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PInt16(left)^ <= PInt16(right)^);
         bcBND: PInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt16(left)^ and PInt16(right)^;
         bcBOR: PInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt16(left)^ or  PInt16(right)^;
         bcXOR: PInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt16(left)^ xor PInt16(right)^;
         bcSAR: PInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := Sar(PInt16(left)^, PInt16(right)^);
         bcSHR: PInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt16(left)^ shr PInt16(right)^;
         bcSHL: PInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt16(left)^ shl PInt16(right)^;
       end;

     xtUInt16:
       case BC.Code.Data^.Code of
         bcADD: PUInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt16(left)^ + PUInt16(right)^;
         bcSUB: PUInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt16(left)^ - PUInt16(right)^;
         bcMUL: PUInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt16(left)^ * PUInt16(right)^;
         bcDIV: PUInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt16(left)^ div PUInt16(right)^;
         bcMOD: PUInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(PUInt16(left)^, PUInt16(right)^);
         bcEQ:  PUInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PUInt16(left)^ = PUInt16(right)^);
         bcNE:  PUInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PUInt16(left)^ <> PUInt16(right)^);
         bcGT:  PUInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PUInt16(left)^ > PUInt16(right)^);
         bcLT:  PUInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PUInt16(left)^ < PUInt16(right)^);
         bcGTE: PUInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PUInt16(left)^ >= PUInt16(right)^);
         bcLTE: PUInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := Ord(PUInt16(left)^ <= PUInt16(right)^);
         bcBND: PUInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt16(left)^ and PUInt16(right)^;
         bcBOR: PUInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt16(left)^ or  PUInt16(right)^;
         bcXOR: PUInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt16(left)^ xor PUInt16(right)^;
         bcSAR: PUInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := Sar(PUInt16(left)^, PUInt16(right)^);
         bcSHR: PUInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt16(left)^ shr PUInt16(right)^;
         bcSHL: PUInt16(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt16(left)^ shl PUInt16(right)^;
       end;
    end;
    *)
    DISPATCH
  end;

// ADD opcodes
lbADD_lll_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ + PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
  
lbADD_lll_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ + PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH
  
lbADD_lll_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ + PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbADD_lll_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ + PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbADD_lll_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ + PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbADD_lll_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ + PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbADD_lil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ + Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbADD_lil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ + UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbADD_lil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ + Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbADD_lil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ + UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbADD_lil_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ + Float32(Data^.Args[1].Data.Raw32);
  DISPATCH 

lbADD_lil_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ + Float64(Data^.Args[1].Data.Raw);
  DISPATCH 


lbADD_ill_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) + PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbADD_ill_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) + PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbADD_ill_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) + PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbADD_ill_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) + PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbADD_ill_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := Float32(Data^.Args[0].Data.Raw32) + PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbADD_ill_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := Float64(Data^.Args[0].Data.Raw) + PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbADD_iil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) + Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbADD_iil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) + UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbADD_iil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) + Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbADD_iil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) + UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbADD_iil_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := Float32(Data^.Args[0].Data.Raw32) + Float32(Data^.Args[1].Data.Raw32);
  DISPATCH 

lbADD_iil_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := Float64(Data^.Args[0].Data.Raw) + Float64(Data^.Args[1].Data.Raw);
  DISPATCH 


 // SUB opcodes
lbSUB_lll_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ - PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSUB_lll_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ - PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSUB_lll_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ - PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSUB_lll_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ - PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSUB_lll_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ - PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSUB_lll_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ - PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbSUB_lil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ - Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbSUB_lil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ - UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbSUB_lil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ - Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbSUB_lil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ - UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbSUB_lil_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ - Float32(Data^.Args[1].Data.Raw32);
  DISPATCH 

lbSUB_lil_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ - Float64(Data^.Args[1].Data.Raw);
  DISPATCH 


lbSUB_ill_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) - PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSUB_ill_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) - PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSUB_ill_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) - PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSUB_ill_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) - PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSUB_ill_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := Float32(Data^.Args[0].Data.Raw32) - PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSUB_ill_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := Float64(Data^.Args[0].Data.Raw) - PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbSUB_iil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) - Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbSUB_iil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) - UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbSUB_iil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) - Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbSUB_iil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) - UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbSUB_iil_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := Float32(Data^.Args[0].Data.Raw32) - Float32(Data^.Args[1].Data.Raw32);
  DISPATCH 

lbSUB_iil_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := Float64(Data^.Args[0].Data.Raw) - Float64(Data^.Args[1].Data.Raw);
  DISPATCH 


 // MUL opcodes
lbMUL_lll_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ * PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbMUL_lll_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ * PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbMUL_lll_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ * PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbMUL_lll_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ * PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbMUL_lll_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ * PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbMUL_lll_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ * PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbMUL_lil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ * Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbMUL_lil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ * UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbMUL_lil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ * Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbMUL_lil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ * UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbMUL_lil_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ * Float32(Data^.Args[1].Data.Raw32);
  DISPATCH 

lbMUL_lil_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ * Float64(Data^.Args[1].Data.Raw);
  DISPATCH 


lbMUL_ill_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) * PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbMUL_ill_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) * PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbMUL_ill_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) * PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbMUL_ill_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) * PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbMUL_ill_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := Float32(Data^.Args[0].Data.Raw32) * PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbMUL_ill_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := Float64(Data^.Args[0].Data.Raw) * PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbMUL_iil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) * Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbMUL_iil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) * UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbMUL_iil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) * Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbMUL_iil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) * UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbMUL_iil_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := Float32(Data^.Args[0].Data.Raw32) * Float32(Data^.Args[1].Data.Raw32);
  DISPATCH 

lbMUL_iil_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := Float64(Data^.Args[0].Data.Raw) * Float64(Data^.Args[1].Data.Raw);
  DISPATCH 


 // DIV opcodes
lbDIV_lll_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ div PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbDIV_lll_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ div PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbDIV_lll_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ div PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbDIV_lll_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ div PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbDIV_lll_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ / PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbDIV_lll_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ / PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbDIV_lil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ div Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbDIV_lil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ div UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbDIV_lil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ div Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbDIV_lil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ div UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbDIV_lil_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ / Float32(Data^.Args[1].Data.Raw32);
  DISPATCH 

lbDIV_lil_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ / Float64(Data^.Args[1].Data.Raw);
  DISPATCH 


lbDIV_ill_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) div PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbDIV_ill_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) div PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbDIV_ill_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) div PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbDIV_ill_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) div PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbDIV_ill_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := Float32(Data^.Args[0].Data.Raw32) / PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbDIV_ill_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := Float64(Data^.Args[0].Data.Raw) / PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbDIV_iil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) div Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbDIV_iil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) div UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbDIV_iil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) div Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbDIV_iil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) div UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbDIV_iil_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := Float32(Data^.Args[0].Data.Raw32) / Float32(Data^.Args[1].Data.Raw32);
  DISPATCH 

lbDIV_iil_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := Float64(Data^.Args[0].Data.Raw) / Float64(Data^.Args[1].Data.Raw);
  DISPATCH 


 // --- MOD opcodes (Modulo) ---
 // lll (local, local, local)
lbMOD_lll_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(PInt32(Stack.Local(Data^.Args[0].Data.Addr))^, PInt32(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbMOD_lll_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^, PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbMOD_lll_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(PInt64(Stack.Local(Data^.Args[0].Data.Addr))^, PInt64(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbMOD_lll_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^, PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbMOD_lll_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^, PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbMOD_lll_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^, PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 


 // lil (local, immediate, local)
lbMOD_lil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(PInt32(Stack.Local(Data^.Args[0].Data.Addr))^, Int32(Data^.Args[1].Data.Arg));
  DISPATCH 

lbMOD_lil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^, UInt32(Data^.Args[1].Data.Arg));
  DISPATCH 

lbMOD_lil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(PInt64(Stack.Local(Data^.Args[0].Data.Addr))^, Int64(Data^.Args[1].Data.Arg));
  DISPATCH 

lbMOD_lil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^, UInt64(Data^.Args[1].Data.Arg));
  DISPATCH 

lbMOD_lil_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^, Float32(Data^.Args[1].Data.Raw32));
  DISPATCH 

lbMOD_lil_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^, Float64(Data^.Args[1].Data.Raw));
  DISPATCH 


 // ill (immediate, local, local)
lbMOD_ill_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(Int32(Data^.Args[0].Data.Arg), PInt32(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbMOD_ill_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(UInt32(Data^.Args[0].Data.Arg), PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbMOD_ill_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(Int64(Data^.Args[0].Data.Arg), PInt64(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbMOD_ill_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(UInt64(Data^.Args[0].Data.Arg), PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbMOD_ill_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(Float32(Data^.Args[0].Data.Raw32), PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbMOD_ill_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(Float64(Data^.Args[0].Data.Raw), PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 


 // iil (immediate, immediate, local)
lbMOD_iil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(Int32(Data^.Args[0].Data.Arg), Int32(Data^.Args[1].Data.Arg));
  DISPATCH 

lbMOD_iil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(UInt32(Data^.Args[0].Data.Arg), UInt32(Data^.Args[1].Data.Arg));
  DISPATCH 

lbMOD_iil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(Int64(Data^.Args[0].Data.Arg), Int64(Data^.Args[1].Data.Arg));
  DISPATCH 

lbMOD_iil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(UInt64(Data^.Args[0].Data.Arg), UInt64(Data^.Args[1].Data.Arg));
  DISPATCH 

lbMOD_iil_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(Float32(Data^.Args[0].Data.Raw32), Float32(Data^.Args[1].Data.Raw32));
  DISPATCH 

lbMOD_iil_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := Modulo(Float64(Data^.Args[0].Data.Raw), Float64(Data^.Args[1].Data.Raw));
  DISPATCH 


 // POW opcodes (Assuming a Power function is available)
lbPOW_lll_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(PInt32(Stack.Local(Data^.Args[0].Data.Addr))^, PInt32(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbPOW_lll_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^, PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbPOW_lll_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(PInt64(Stack.Local(Data^.Args[0].Data.Addr))^, PInt64(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbPOW_lll_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^, PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbPOW_lll_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^, PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbPOW_lll_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^, PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 


lbPOW_lil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(PInt32(Stack.Local(Data^.Args[0].Data.Addr))^, Int32(Data^.Args[1].Data.Arg));
  DISPATCH 

lbPOW_lil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^, UInt32(Data^.Args[1].Data.Arg));
  DISPATCH 

lbPOW_lil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(PInt64(Stack.Local(Data^.Args[0].Data.Addr))^, Int64(Data^.Args[1].Data.Arg));
  DISPATCH 

lbPOW_lil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^, UInt64(Data^.Args[1].Data.Arg));
  DISPATCH 

lbPOW_lil_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^, Float32(Data^.Args[1].Data.Raw32));
  DISPATCH 

lbPOW_lil_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^, Float64(Data^.Args[1].Data.Raw));
  DISPATCH 


lbPOW_ill_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(Int32(Data^.Args[0].Data.Arg), PInt32(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbPOW_ill_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(UInt32(Data^.Args[0].Data.Arg), PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbPOW_ill_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(Int64(Data^.Args[0].Data.Arg), PInt64(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbPOW_ill_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(UInt64(Data^.Args[0].Data.Arg), PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbPOW_ill_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(Float32(Data^.Args[0].Data.Raw32), PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbPOW_ill_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(Float64(Data^.Args[0].Data.Raw), PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 


lbPOW_iil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(Int32(Data^.Args[0].Data.Arg), Int32(Data^.Args[1].Data.Arg));
  DISPATCH 

lbPOW_iil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(UInt32(Data^.Args[0].Data.Arg), UInt32(Data^.Args[1].Data.Arg));
  DISPATCH 

lbPOW_iil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(Int64(Data^.Args[0].Data.Arg), Int64(Data^.Args[1].Data.Arg));
  DISPATCH 

lbPOW_iil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(UInt64(Data^.Args[0].Data.Arg), UInt64(Data^.Args[1].Data.Arg));
  DISPATCH 

lbPOW_iil_f32: 
  PFloat32(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(Float32(Data^.Args[0].Data.Raw32), Float32(Data^.Args[1].Data.Raw32));
  DISPATCH 

lbPOW_iil_f64: 
  PFloat64(Stack.Local(Data^.Args[2].Data.Addr))^ := Power(Float64(Data^.Args[0].Data.Raw), Float64(Data^.Args[1].Data.Raw));
  DISPATCH 


 // NE opcodes
lbNE_lll_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ <> PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbNE_lll_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ <> PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbNE_lll_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ <> PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbNE_lll_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ <> PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbNE_lll_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ <> PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbNE_lll_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ <> PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbNE_lil_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ <> Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbNE_lil_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ <> UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbNE_lil_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ <> Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbNE_lil_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ <> UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbNE_lil_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ <> Float32(Data^.Args[1].Data.Raw32);
  DISPATCH 

lbNE_lil_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ <> Float64(Data^.Args[1].Data.Raw);
  DISPATCH 


lbNE_ill_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) <> PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbNE_ill_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) <> PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbNE_ill_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) <> PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbNE_ill_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) <> PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbNE_ill_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float32(Data^.Args[0].Data.Raw32) <> PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbNE_ill_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float64(Data^.Args[0].Data.Raw) <> PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbNE_iil_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) <> Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbNE_iil_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) <> UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbNE_iil_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) <> Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbNE_iil_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) <> UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbNE_iil_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float32(Data^.Args[0].Data.Raw32) <> Float32(Data^.Args[1].Data.Raw32);
  DISPATCH 

lbNE_iil_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float64(Data^.Args[0].Data.Raw) <> Float64(Data^.Args[1].Data.Raw);
  DISPATCH 


 // EQ opcodes
lbEQ_lll_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ = PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbEQ_lll_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ = PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbEQ_lll_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ = PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbEQ_lll_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ = PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbEQ_lll_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ = PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbEQ_lll_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ = PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbEQ_lil_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ = Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbEQ_lil_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ = UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbEQ_lil_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ = Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbEQ_lil_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ = UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbEQ_lil_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ = Float32(Data^.Args[1].Data.Raw32);
  DISPATCH 

lbEQ_lil_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ = Float64(Data^.Args[1].Data.Raw);
  DISPATCH 


lbEQ_ill_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) = PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbEQ_ill_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) = PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbEQ_ill_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) = PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbEQ_ill_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) = PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbEQ_ill_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float32(Data^.Args[0].Data.Raw32) = PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbEQ_ill_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float64(Data^.Args[0].Data.Raw) = PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbEQ_iil_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) = Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbEQ_iil_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) = UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbEQ_iil_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) = Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbEQ_iil_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) = UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbEQ_iil_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float32(Data^.Args[0].Data.Raw32) = Float32(Data^.Args[1].Data.Raw32);
  DISPATCH 

lbEQ_iil_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float64(Data^.Args[0].Data.Raw) = Float64(Data^.Args[1].Data.Raw);
  DISPATCH 


 // LT opcodes
lbLT_lll_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ < PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbLT_lll_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ < PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbLT_lll_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ < PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbLT_lll_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ < PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbLT_lll_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ < PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbLT_lll_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ < PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbLT_lil_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ < Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbLT_lil_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ < UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbLT_lil_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ < Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbLT_lil_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ < UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbLT_lil_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ < Float32(Data^.Args[1].Data.Raw32);
  DISPATCH 

lbLT_lil_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ < Float64(Data^.Args[1].Data.Raw);
  DISPATCH 


lbLT_ill_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) < PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbLT_ill_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) < PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbLT_ill_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) < PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbLT_ill_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) < PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbLT_ill_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float32(Data^.Args[0].Data.Raw32) < PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbLT_ill_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float64(Data^.Args[0].Data.Raw) < PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbLT_iil_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) < Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbLT_iil_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) < UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbLT_iil_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) < Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbLT_iil_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) < UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbLT_iil_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float32(Data^.Args[0].Data.Raw32) < Float32(Data^.Args[1].Data.Raw32);
  DISPATCH 

lbLT_iil_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float64(Data^.Args[0].Data.Raw) < Float64(Data^.Args[1].Data.Raw);
  DISPATCH 



  // GT opcodes
lbGT_lll_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ > PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbGT_lll_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ > PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbGT_lll_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ > PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbGT_lll_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ > PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbGT_lll_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ > PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbGT_lll_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ > PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbGT_lil_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ > Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbGT_lil_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ > UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbGT_lil_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ > Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbGT_lil_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ > UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbGT_lil_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ > Float32(Data^.Args[1].Data.Raw32);
  DISPATCH 

lbGT_lil_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ > Float64(Data^.Args[1].Data.Raw);
  DISPATCH 


lbGT_ill_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) > PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbGT_ill_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) > PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbGT_ill_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) > PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbGT_ill_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) > PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbGT_ill_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float32(Data^.Args[0].Data.Raw32) > PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbGT_ill_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float64(Data^.Args[0].Data.Raw) > PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbGT_iil_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) > Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbGT_iil_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) > UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbGT_iil_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) > Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbGT_iil_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) > UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbGT_iil_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float32(Data^.Args[0].Data.Raw32) > Float32(Data^.Args[1].Data.Raw32);
  DISPATCH 

lbGT_iil_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float64(Data^.Args[0].Data.Raw) > Float64(Data^.Args[1].Data.Raw);
  DISPATCH 


  // GTE opcodes
lbGTE_lll_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ >= PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbGTE_lll_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ >= PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbGTE_lll_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ >= PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbGTE_lll_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ >= PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbGTE_lll_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ >= PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbGTE_lll_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ >= PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbGTE_lil_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ >= Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbGTE_lil_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ >= UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbGTE_lil_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ >= Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbGTE_lil_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ >= UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbGTE_lil_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ >= Float32(Data^.Args[1].Data.Raw32);
  DISPATCH 

lbGTE_lil_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ >= Float64(Data^.Args[1].Data.Raw);
  DISPATCH 


lbGTE_ill_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) >= PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbGTE_ill_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) >= PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbGTE_ill_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) >= PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbGTE_ill_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) >= PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbGTE_ill_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float32(Data^.Args[0].Data.Raw32) >= PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbGTE_ill_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float64(Data^.Args[0].Data.Raw) >= PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbGTE_iil_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) >= Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbGTE_iil_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) >= UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbGTE_iil_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) >= Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbGTE_iil_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) >= UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbGTE_iil_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float32(Data^.Args[0].Data.Raw32) >= Float32(Data^.Args[1].Data.Raw32);
  DISPATCH 

lbGTE_iil_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float64(Data^.Args[0].Data.Raw) >= Float64(Data^.Args[1].Data.Raw);
  DISPATCH 


  // LTE opcodes
lbLTE_lll_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ <= PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbLTE_lll_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ <= PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbLTE_lll_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ <= PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbLTE_lll_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ <= PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbLTE_lll_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ <= PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbLTE_lll_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ <= PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbLTE_lil_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ <= Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbLTE_lil_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ <= UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbLTE_lil_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ <= Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbLTE_lil_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ <= UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbLTE_lil_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat32(Stack.Local(Data^.Args[0].Data.Addr))^ <= Float32(Data^.Args[1].Data.Raw32);
  DISPATCH 

lbLTE_lil_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := PFloat64(Stack.Local(Data^.Args[0].Data.Addr))^ <= Float64(Data^.Args[1].Data.Raw);
  DISPATCH 


lbLTE_ill_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) <= PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbLTE_ill_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) <= PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbLTE_ill_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) <= PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbLTE_ill_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) <= PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbLTE_ill_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float32(Data^.Args[0].Data.Raw32) <= PFloat32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbLTE_ill_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float64(Data^.Args[0].Data.Raw) <= PFloat64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbLTE_iil_i32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) <= Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbLTE_iil_u32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) <= UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbLTE_iil_i64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) <= Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbLTE_iil_u64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) <= UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbLTE_iil_f32: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float32(Data^.Args[0].Data.Raw32) <= Float32(Data^.Args[1].Data.Raw32);
  DISPATCH 

lbLTE_iil_f64: 
  PBoolean(Stack.Local(Data^.Args[2].Data.Addr))^ := Float64(Data^.Args[0].Data.Raw) <= Float64(Data^.Args[1].Data.Raw);
  DISPATCH 


  // BND opcodes (Bitwise AND)
lbBND_lll_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ and PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbBND_lll_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ and PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbBND_lll_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ and PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbBND_lll_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ and PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbBND_lil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ and Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbBND_lil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ and UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbBND_lil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ and Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbBND_lil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ and UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 


lbBND_ill_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) and PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbBND_ill_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) and PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbBND_ill_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) and PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbBND_ill_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) and PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbBND_iil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) and Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbBND_iil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) and UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbBND_iil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) and Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbBND_iil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) and UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 


  // SHL opcodes (Shift Left)
lbSHL_lll_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ shl PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSHL_lll_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ shl PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSHL_lll_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ shl PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSHL_lll_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ shl PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbSHL_lil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ shl Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbSHL_lil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ shl UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbSHL_lil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ shl Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbSHL_lil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ shl UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 


lbSHL_ill_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) shl PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSHL_ill_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) shl PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSHL_ill_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) shl PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSHL_ill_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) shl PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbSHL_iil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) shl Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbSHL_iil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) shl UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbSHL_iil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) shl Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbSHL_iil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) shl UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 


  // SHR opcodes (Shift Right)
lbSHR_lll_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ shr PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSHR_lll_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ shr PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSHR_lll_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ shr PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSHR_lll_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ shr PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbSHR_lil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ shr Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbSHR_lil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ shr UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbSHR_lil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ shr Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbSHR_lil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ shr UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 


lbSHR_ill_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) shr PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSHR_ill_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) shr PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSHR_ill_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) shr PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbSHR_ill_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) shr PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbSHR_iil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) shr Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbSHR_iil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) shr UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbSHR_iil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) shr Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbSHR_iil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) shr UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 


  // XOR opcodes (Bitwise XOR)
lbXOR_lll_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ xor PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbXOR_lll_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ xor PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbXOR_lll_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ xor PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbXOR_lll_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ xor PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbXOR_lil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ xor Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbXOR_lil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ xor UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbXOR_lil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ xor Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbXOR_lil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ xor UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 


lbXOR_ill_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) xor PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbXOR_ill_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) xor PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbXOR_ill_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) xor PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbXOR_ill_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) xor PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbXOR_iil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) xor Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbXOR_iil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) xor UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbXOR_iil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) xor Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbXOR_iil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) xor UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 


  // BOR opcodes (Bitwise OR)
lbBOR_lll_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ or PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbBOR_lll_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ or PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbBOR_lll_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ or PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbBOR_lll_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ or PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbBOR_lil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt32(Stack.Local(Data^.Args[0].Data.Addr))^ or Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbBOR_lil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^ or UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbBOR_lil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PInt64(Stack.Local(Data^.Args[0].Data.Addr))^ or Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbBOR_lil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^ or UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 


lbBOR_ill_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) or PInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbBOR_ill_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) or PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbBOR_ill_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) or PInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 

lbBOR_ill_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) or PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^;
  DISPATCH 


lbBOR_iil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Int32(Data^.Args[0].Data.Arg) or Int32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbBOR_iil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt32(Data^.Args[0].Data.Arg) or UInt32(Data^.Args[1].Data.Arg);
  DISPATCH 

lbBOR_iil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Int64(Data^.Args[0].Data.Arg) or Int64(Data^.Args[1].Data.Arg);
  DISPATCH 

lbBOR_iil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := UInt64(Data^.Args[0].Data.Arg) or UInt64(Data^.Args[1].Data.Arg);
  DISPATCH 


  // --- SAR opcodes (Shift Arithmetic Right) ---
  // lll (Stack.Local, Stack.Local, Stack.Local)
lbSAR_lll_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Sar(PInt32(Stack.Local(Data^.Args[0].Data.Addr))^, PInt32(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbSAR_lll_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Sar(PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^, PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbSAR_lll_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Sar(PInt64(Stack.Local(Data^.Args[0].Data.Addr))^, PInt64(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbSAR_lll_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Sar(PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^, PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 


  // lil (Stack.Local, immediate, Stack.Local)
lbSAR_lil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Sar(PInt32(Stack.Local(Data^.Args[0].Data.Addr))^, Int32(Data^.Args[1].Data.Arg));
  DISPATCH 

lbSAR_lil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Sar(PUInt32(Stack.Local(Data^.Args[0].Data.Addr))^, UInt32(Data^.Args[1].Data.Arg));
  DISPATCH 

lbSAR_lil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Sar(PInt64(Stack.Local(Data^.Args[0].Data.Addr))^, Int64(Data^.Args[1].Data.Arg));
  DISPATCH 

lbSAR_lil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Sar(PUInt64(Stack.Local(Data^.Args[0].Data.Addr))^, UInt64(Data^.Args[1].Data.Arg));
  DISPATCH 


  // ill (immediate, Stack.Local, Stack.Local)
lbSAR_ill_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Sar(Int32(Data^.Args[0].Data.Arg), PInt32(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbSAR_ill_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Sar(UInt32(Data^.Args[0].Data.Arg), PUInt32(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbSAR_ill_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Sar(Int64(Data^.Args[0].Data.Arg), PInt64(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 

lbSAR_ill_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Sar(UInt64(Data^.Args[0].Data.Arg), PUInt64(Stack.Local(Data^.Args[1].Data.Addr))^);
  DISPATCH 


  // iil (immediate, immediate, Stack.Local)
lbSAR_iil_i32: 
  PInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Sar(Int32(Data^.Args[0].Data.Arg), Int32(Data^.Args[1].Data.Arg));
  DISPATCH 

lbSAR_iil_u32: 
  PUInt32(Stack.Local(Data^.Args[2].Data.Addr))^ := Sar(UInt32(Data^.Args[0].Data.Arg), UInt32(Data^.Args[1].Data.Arg));
  DISPATCH 

lbSAR_iil_i64: 
  PInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Sar(Int64(Data^.Args[0].Data.Arg), Int64(Data^.Args[1].Data.Arg));
  DISPATCH 

lbSAR_iil_u64: 
  PUInt64(Stack.Local(Data^.Args[2].Data.Addr))^ := Sar(UInt64(Data^.Args[0].Data.Arg), UInt64(Data^.Args[1].Data.Arg));
  DISPATCH 

