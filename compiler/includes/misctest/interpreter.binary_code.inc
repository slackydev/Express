  // --- Arithmetic Operations ---

  bcADD, bcSUB, bcMUL, bcDIV, bcMOD,
  bcBND, bcBOR, bcXOR, bcEQ, bcNE,
  bcGT, bcLT, bcGTE, bcLTE,
  bcSAR, bcSHL, bcSHR:
  begin
    // Point left operand
    if Args[0].Pos = mpLocal then
      left := Stack.Local(Args[0].Data.Addr)
    else
      left := @Args[0].Data.Arg;    

    // Point right operand
    if Args[1].Pos = mpLocal then
      right := Stack.Local(Args[1].Data.Addr)
    else
      right := @Args[1].Data.Arg;

    // Type dispatch
    case Args[0].BaseType of
      xtInt8:
        case Code of
          bcADD: PInt8(Local(Args[2].Data.Addr))^ := PInt8(left)^ + PInt8(right)^;
          bcSUB: PInt8(Local(Args[2].Data.Addr))^ := PInt8(left)^ - PInt8(right)^;
          bcMUL: PInt8(Local(Args[2].Data.Addr))^ := PInt8(left)^ * PInt8(right)^;
          bcDIV: PInt8(Local(Args[2].Data.Addr))^ := PInt8(left)^ div PInt8(right)^;
          bcMOD: PInt8(Local(Args[2].Data.Addr))^ := Modulo(PInt8(left)^, PInt8(right)^);
          bcEQ:  PInt8(Local(Args[2].Data.Addr))^ := Ord(PInt8(left)^ = PInt8(right)^);
          bcNE:  PInt8(Local(Args[2].Data.Addr))^ := Ord(PInt8(left)^ <> PInt8(right)^);
          bcGT:  PInt8(Local(Args[2].Data.Addr))^ := Ord(PInt8(left)^ > PInt8(right)^);
          bcLT:  PInt8(Local(Args[2].Data.Addr))^ := Ord(PInt8(left)^ < PInt8(right)^);
          bcGTE: PInt8(Local(Args[2].Data.Addr))^ := Ord(PInt8(left)^ >= PInt8(right)^);
          bcLTE: PInt8(Local(Args[2].Data.Addr))^ := Ord(PInt8(left)^ <= PInt8(right)^);
          bcBND: PInt8(Local(Args[2].Data.Addr))^ := PInt8(left)^ and PInt8(right)^;
          bcBOR: PInt8(Local(Args[2].Data.Addr))^ := PInt8(left)^ or  PInt8(right)^;
          bcXOR: PInt8(Local(Args[2].Data.Addr))^ := PInt8(left)^ xor PInt8(right)^;
          bcSAR: PInt8(Local(Args[2].Data.Addr))^ := Sar(PInt8(left)^, PInt8(right)^);
          bcSHR: PInt8(Local(Args[2].Data.Addr))^ := PInt8(left)^ shr PInt8(right)^;
          bcSHL: PInt8(Local(Args[2].Data.Addr))^ := PInt8(left)^ shl PInt8(right)^;
        end;

      xtUInt8:
        case Code of
          bcADD: PUInt8(Local(Args[2].Data.Addr))^ := PUInt8(left)^ + PUInt8(right)^;
          bcSUB: PUInt8(Local(Args[2].Data.Addr))^ := PUInt8(left)^ - PUInt8(right)^;
          bcMUL: PUInt8(Local(Args[2].Data.Addr))^ := PUInt8(left)^ * PUInt8(right)^;
          bcDIV: PUInt8(Local(Args[2].Data.Addr))^ := PUInt8(left)^ div PUInt8(right)^;
          bcMOD: PUInt8(Local(Args[2].Data.Addr))^ := Modulo(PUInt8(left)^, PUInt8(right)^);
          bcEQ:  PUInt8(Local(Args[2].Data.Addr))^ := Ord(PUInt8(left)^ = PUInt8(right)^);
          bcNE:  PUInt8(Local(Args[2].Data.Addr))^ := Ord(PUInt8(left)^ <> PUInt8(right)^);
          bcGT:  PUInt8(Local(Args[2].Data.Addr))^ := Ord(PUInt8(left)^ > PUInt8(right)^);
          bcLT:  PUInt8(Local(Args[2].Data.Addr))^ := Ord(PUInt8(left)^ < PUInt8(right)^);
          bcGTE: PUInt8(Local(Args[2].Data.Addr))^ := Ord(PUInt8(left)^ >= PUInt8(right)^);
          bcLTE: PUInt8(Local(Args[2].Data.Addr))^ := Ord(PUInt8(left)^ <= PUInt8(right)^);
          bcBND: PUInt8(Local(Args[2].Data.Addr))^ := PUInt8(left)^ and PUInt8(right)^;
          bcBOR: PUInt8(Local(Args[2].Data.Addr))^ := PUInt8(left)^ or  PUInt8(right)^;
          bcXOR: PUInt8(Local(Args[2].Data.Addr))^ := PUInt8(left)^ xor PUInt8(right)^;
          bcSAR: PUInt8(Local(Args[2].Data.Addr))^ := Sar(PUInt8(left)^, PUInt8(right)^);
          bcSHR: PUInt8(Local(Args[2].Data.Addr))^ := PUInt8(left)^ shr PUInt8(right)^;
          bcSHL: PUInt8(Local(Args[2].Data.Addr))^ := PUInt8(left)^ shl PUInt8(right)^;
        end;

      xtInt16:
        case Code of
          bcADD: PInt16(Local(Args[2].Data.Addr))^ := PInt16(left)^ + PInt16(right)^;
          bcSUB: PInt16(Local(Args[2].Data.Addr))^ := PInt16(left)^ - PInt16(right)^;
          bcMUL: PInt16(Local(Args[2].Data.Addr))^ := PInt16(left)^ * PInt16(right)^;
          bcDIV: PInt16(Local(Args[2].Data.Addr))^ := PInt16(left)^ div PInt16(right)^;
          bcMOD: PInt16(Local(Args[2].Data.Addr))^ := Modulo(PInt16(left)^, PInt16(right)^);
          bcEQ:  PInt16(Local(Args[2].Data.Addr))^ := Ord(PInt16(left)^ = PInt16(right)^);
          bcNE:  PInt16(Local(Args[2].Data.Addr))^ := Ord(PInt16(left)^ <> PInt16(right)^);
          bcGT:  PInt16(Local(Args[2].Data.Addr))^ := Ord(PInt16(left)^ > PInt16(right)^);
          bcLT:  PInt16(Local(Args[2].Data.Addr))^ := Ord(PInt16(left)^ < PInt16(right)^);
          bcGTE: PInt16(Local(Args[2].Data.Addr))^ := Ord(PInt16(left)^ >= PInt16(right)^);
          bcLTE: PInt16(Local(Args[2].Data.Addr))^ := Ord(PInt16(left)^ <= PInt16(right)^);
          bcBND: PInt16(Local(Args[2].Data.Addr))^ := PInt16(left)^ and PInt16(right)^;
          bcBOR: PInt16(Local(Args[2].Data.Addr))^ := PInt16(left)^ or  PInt16(right)^;
          bcXOR: PInt16(Local(Args[2].Data.Addr))^ := PInt16(left)^ xor PInt16(right)^;
          bcSAR: PInt16(Local(Args[2].Data.Addr))^ := Sar(PInt16(left)^, PInt16(right)^);
          bcSHR: PInt16(Local(Args[2].Data.Addr))^ := PInt16(left)^ shr PInt16(right)^;
          bcSHL: PInt16(Local(Args[2].Data.Addr))^ := PInt16(left)^ shl PInt16(right)^;
        end;

      xtUInt16:
        case Code of
          bcADD: PUInt16(Local(Args[2].Data.Addr))^ := PUInt16(left)^ + PUInt16(right)^;
          bcSUB: PUInt16(Local(Args[2].Data.Addr))^ := PUInt16(left)^ - PUInt16(right)^;
          bcMUL: PUInt16(Local(Args[2].Data.Addr))^ := PUInt16(left)^ * PUInt16(right)^;
          bcDIV: PUInt16(Local(Args[2].Data.Addr))^ := PUInt16(left)^ div PUInt16(right)^;
          bcMOD: PUInt16(Local(Args[2].Data.Addr))^ := Modulo(PUInt16(left)^, PUInt16(right)^);
          bcEQ:  PUInt16(Local(Args[2].Data.Addr))^ := Ord(PUInt16(left)^ = PUInt16(right)^);
          bcNE:  PUInt16(Local(Args[2].Data.Addr))^ := Ord(PUInt16(left)^ <> PUInt16(right)^);
          bcGT:  PUInt16(Local(Args[2].Data.Addr))^ := Ord(PUInt16(left)^ > PUInt16(right)^);
          bcLT:  PUInt16(Local(Args[2].Data.Addr))^ := Ord(PUInt16(left)^ < PUInt16(right)^);
          bcGTE: PUInt16(Local(Args[2].Data.Addr))^ := Ord(PUInt16(left)^ >= PUInt16(right)^);
          bcLTE: PUInt16(Local(Args[2].Data.Addr))^ := Ord(PUInt16(left)^ <= PUInt16(right)^);
          bcBND: PUInt16(Local(Args[2].Data.Addr))^ := PUInt16(left)^ and PUInt16(right)^;
          bcBOR: PUInt16(Local(Args[2].Data.Addr))^ := PUInt16(left)^ or  PUInt16(right)^;
          bcXOR: PUInt16(Local(Args[2].Data.Addr))^ := PUInt16(left)^ xor PUInt16(right)^;
          bcSAR: PUInt16(Local(Args[2].Data.Addr))^ := Sar(PUInt16(left)^, PUInt16(right)^);
          bcSHR: PUInt16(Local(Args[2].Data.Addr))^ := PUInt16(left)^ shr PUInt16(right)^;
          bcSHL: PUInt16(Local(Args[2].Data.Addr))^ := PUInt16(left)^ shl PUInt16(right)^;
        end;

    end;
  end;

  // ADD opcodes
  bcADD_lll_i32: PInt32(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ + PInt32(Local(Args[1].Data.Addr))^;
  bcADD_lll_u32: PUInt32(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ + PUInt32(Local(Args[1].Data.Addr))^;
  bcADD_lll_i64: PInt64(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ + PInt64(Local(Args[1].Data.Addr))^;
  bcADD_lll_u64: PUInt64(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ + PUInt64(Local(Args[1].Data.Addr))^;
  bcADD_lll_f32: PFloat32(Local(Args[2].Data.Addr))^ := PFloat32(Local(Args[0].Data.Addr))^ + PFloat32(Local(Args[1].Data.Addr))^;
  bcADD_lll_f64: PFloat64(Local(Args[2].Data.Addr))^ := PFloat64(Local(Args[0].Data.Addr))^ + PFloat64(Local(Args[1].Data.Addr))^;

  bcADD_lil_i32: PInt32(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ + Int32(Args[1].Data.Arg);
  bcADD_lil_u32: PUInt32(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ + UInt32(Args[1].Data.Arg);
  bcADD_lil_i64: PInt64(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ + Int64(Args[1].Data.Arg);
  bcADD_lil_u64: PUInt64(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ + UInt64(Args[1].Data.Arg);
  bcADD_lil_f32: PFloat32(Local(Args[2].Data.Addr))^ := PFloat32(Local(Args[0].Data.Addr))^ + Float32(Args[1].Data.Raw32);
  bcADD_lil_f64: PFloat64(Local(Args[2].Data.Addr))^ := PFloat64(Local(Args[0].Data.Addr))^ + Float64(Args[1].Data.Raw);

  bcADD_ill_i32: PInt32(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) + PInt32(Local(Args[1].Data.Addr))^;
  bcADD_ill_u32: PUInt32(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) + PUInt32(Local(Args[1].Data.Addr))^;
  bcADD_ill_i64: PInt64(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) + PInt64(Local(Args[1].Data.Addr))^;
  bcADD_ill_u64: PUInt64(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) + PUInt64(Local(Args[1].Data.Addr))^;
  bcADD_ill_f32: PFloat32(Local(Args[2].Data.Addr))^ := Float32(Args[0].Data.Raw32) + PFloat32(Local(Args[1].Data.Addr))^;
  bcADD_ill_f64: PFloat64(Local(Args[2].Data.Addr))^ := Float64(Args[0].Data.Raw) + PFloat64(Local(Args[1].Data.Addr))^;

  bcADD_iil_i32: PInt32(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) + Int32(Args[1].Data.Arg);
  bcADD_iil_u32: PUInt32(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) + UInt32(Args[1].Data.Arg);
  bcADD_iil_i64: PInt64(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) + Int64(Args[1].Data.Arg);
  bcADD_iil_u64: PUInt64(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) + UInt64(Args[1].Data.Arg);
  bcADD_iil_f32: PFloat32(Local(Args[2].Data.Addr))^ := Float32(Args[0].Data.Raw32) + Float32(Args[1].Data.Raw32);
  bcADD_iil_f64: PFloat64(Local(Args[2].Data.Addr))^ := Float64(Args[0].Data.Raw) + Float64(Args[1].Data.Raw);

  // SUB opcodes
  bcSUB_lll_i32: PInt32(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ - PInt32(Local(Args[1].Data.Addr))^;
  bcSUB_lll_u32: PUInt32(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ - PUInt32(Local(Args[1].Data.Addr))^;
  bcSUB_lll_i64: PInt64(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ - PInt64(Local(Args[1].Data.Addr))^;
  bcSUB_lll_u64: PUInt64(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ - PUInt64(Local(Args[1].Data.Addr))^;
  bcSUB_lll_f32: PFloat32(Local(Args[2].Data.Addr))^ := PFloat32(Local(Args[0].Data.Addr))^ - PFloat32(Local(Args[1].Data.Addr))^;
  bcSUB_lll_f64: PFloat64(Local(Args[2].Data.Addr))^ := PFloat64(Local(Args[0].Data.Addr))^ - PFloat64(Local(Args[1].Data.Addr))^;

  bcSUB_lil_i32: PInt32(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ - Int32(Args[1].Data.Arg);
  bcSUB_lil_u32: PUInt32(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ - UInt32(Args[1].Data.Arg);
  bcSUB_lil_i64: PInt64(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ - Int64(Args[1].Data.Arg);
  bcSUB_lil_u64: PUInt64(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ - UInt64(Args[1].Data.Arg);
  bcSUB_lil_f32: PFloat32(Local(Args[2].Data.Addr))^ := PFloat32(Local(Args[0].Data.Addr))^ - Float32(Args[1].Data.Raw32);
  bcSUB_lil_f64: PFloat64(Local(Args[2].Data.Addr))^ := PFloat64(Local(Args[0].Data.Addr))^ - Float64(Args[1].Data.Raw);

  bcSUB_ill_i32: PInt32(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) - PInt32(Local(Args[1].Data.Addr))^;
  bcSUB_ill_u32: PUInt32(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) - PUInt32(Local(Args[1].Data.Addr))^;
  bcSUB_ill_i64: PInt64(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) - PInt64(Local(Args[1].Data.Addr))^;
  bcSUB_ill_u64: PUInt64(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) - PUInt64(Local(Args[1].Data.Addr))^;
  bcSUB_ill_f32: PFloat32(Local(Args[2].Data.Addr))^ := Float32(Args[0].Data.Raw32) - PFloat32(Local(Args[1].Data.Addr))^;
  bcSUB_ill_f64: PFloat64(Local(Args[2].Data.Addr))^ := Float64(Args[0].Data.Raw) - PFloat64(Local(Args[1].Data.Addr))^;

  bcSUB_iil_i32: PInt32(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) - Int32(Args[1].Data.Arg);
  bcSUB_iil_u32: PUInt32(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) - UInt32(Args[1].Data.Arg);
  bcSUB_iil_i64: PInt64(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) - Int64(Args[1].Data.Arg);
  bcSUB_iil_u64: PUInt64(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) - UInt64(Args[1].Data.Arg);
  bcSUB_iil_f32: PFloat32(Local(Args[2].Data.Addr))^ := Float32(Args[0].Data.Raw32) - Float32(Args[1].Data.Raw32);
  bcSUB_iil_f64: PFloat64(Local(Args[2].Data.Addr))^ := Float64(Args[0].Data.Raw) - Float64(Args[1].Data.Raw);

  // MUL opcodes
  bcMUL_lll_i32: PInt32(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ * PInt32(Local(Args[1].Data.Addr))^;
  bcMUL_lll_u32: PUInt32(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ * PUInt32(Local(Args[1].Data.Addr))^;
  bcMUL_lll_i64: PInt64(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ * PInt64(Local(Args[1].Data.Addr))^;
  bcMUL_lll_u64: PUInt64(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ * PUInt64(Local(Args[1].Data.Addr))^;
  bcMUL_lll_f32: PFloat32(Local(Args[2].Data.Addr))^ := PFloat32(Local(Args[0].Data.Addr))^ * PFloat32(Local(Args[1].Data.Addr))^;
  bcMUL_lll_f64: PFloat64(Local(Args[2].Data.Addr))^ := PFloat64(Local(Args[0].Data.Addr))^ * PFloat64(Local(Args[1].Data.Addr))^;

  bcMUL_lil_i32: PInt32(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ * Int32(Args[1].Data.Arg);
  bcMUL_lil_u32: PUInt32(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ * UInt32(Args[1].Data.Arg);
  bcMUL_lil_i64: PInt64(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ * Int64(Args[1].Data.Arg);
  bcMUL_lil_u64: PUInt64(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ * UInt64(Args[1].Data.Arg);
  bcMUL_lil_f32: PFloat32(Local(Args[2].Data.Addr))^ := PFloat32(Local(Args[0].Data.Addr))^ * Float32(Args[1].Data.Raw32);
  bcMUL_lil_f64: PFloat64(Local(Args[2].Data.Addr))^ := PFloat64(Local(Args[0].Data.Addr))^ * Float64(Args[1].Data.Raw);

  bcMUL_ill_i32: PInt32(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) * PInt32(Local(Args[1].Data.Addr))^;
  bcMUL_ill_u32: PUInt32(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) * PUInt32(Local(Args[1].Data.Addr))^;
  bcMUL_ill_i64: PInt64(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) * PInt64(Local(Args[1].Data.Addr))^;
  bcMUL_ill_u64: PUInt64(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) * PUInt64(Local(Args[1].Data.Addr))^;
  bcMUL_ill_f32: PFloat32(Local(Args[2].Data.Addr))^ := Float32(Args[0].Data.Raw32) * PFloat32(Local(Args[1].Data.Addr))^;
  bcMUL_ill_f64: PFloat64(Local(Args[2].Data.Addr))^ := Float64(Args[0].Data.Raw) * PFloat64(Local(Args[1].Data.Addr))^;

  bcMUL_iil_i32: PInt32(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) * Int32(Args[1].Data.Arg);
  bcMUL_iil_u32: PUInt32(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) * UInt32(Args[1].Data.Arg);
  bcMUL_iil_i64: PInt64(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) * Int64(Args[1].Data.Arg);
  bcMUL_iil_u64: PUInt64(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) * UInt64(Args[1].Data.Arg);
  bcMUL_iil_f32: PFloat32(Local(Args[2].Data.Addr))^ := Float32(Args[0].Data.Raw32) * Float32(Args[1].Data.Raw32);
  bcMUL_iil_f64: PFloat64(Local(Args[2].Data.Addr))^ := Float64(Args[0].Data.Raw) * Float64(Args[1].Data.Raw);

  // DIV opcodes
  bcDIV_lll_i32: PInt32(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ div PInt32(Local(Args[1].Data.Addr))^;
  bcDIV_lll_u32: PUInt32(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ div PUInt32(Local(Args[1].Data.Addr))^;
  bcDIV_lll_i64: PInt64(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ div PInt64(Local(Args[1].Data.Addr))^;
  bcDIV_lll_u64: PUInt64(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ div PUInt64(Local(Args[1].Data.Addr))^;
  bcDIV_lll_f32: PFloat32(Local(Args[2].Data.Addr))^ := PFloat32(Local(Args[0].Data.Addr))^ / PFloat32(Local(Args[1].Data.Addr))^;
  bcDIV_lll_f64: PFloat64(Local(Args[2].Data.Addr))^ := PFloat64(Local(Args[0].Data.Addr))^ / PFloat64(Local(Args[1].Data.Addr))^;

  bcDIV_lil_i32: PInt32(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ div Int32(Args[1].Data.Arg);
  bcDIV_lil_u32: PUInt32(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ div UInt32(Args[1].Data.Arg);
  bcDIV_lil_i64: PInt64(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ div Int64(Args[1].Data.Arg);
  bcDIV_lil_u64: PUInt64(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ div UInt64(Args[1].Data.Arg);
  bcDIV_lil_f32: PFloat32(Local(Args[2].Data.Addr))^ := PFloat32(Local(Args[0].Data.Addr))^ / Float32(Args[1].Data.Raw32);
  bcDIV_lil_f64: PFloat64(Local(Args[2].Data.Addr))^ := PFloat64(Local(Args[0].Data.Addr))^ / Float64(Args[1].Data.Raw);

  bcDIV_ill_i32: PInt32(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) div PInt32(Local(Args[1].Data.Addr))^;
  bcDIV_ill_u32: PUInt32(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) div PUInt32(Local(Args[1].Data.Addr))^;
  bcDIV_ill_i64: PInt64(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) div PInt64(Local(Args[1].Data.Addr))^;
  bcDIV_ill_u64: PUInt64(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) div PUInt64(Local(Args[1].Data.Addr))^;
  bcDIV_ill_f32: PFloat32(Local(Args[2].Data.Addr))^ := Float32(Args[0].Data.Raw32) / PFloat32(Local(Args[1].Data.Addr))^;
  bcDIV_ill_f64: PFloat64(Local(Args[2].Data.Addr))^ := Float64(Args[0].Data.Raw) / PFloat64(Local(Args[1].Data.Addr))^;

  bcDIV_iil_i32: PInt32(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) div Int32(Args[1].Data.Arg);
  bcDIV_iil_u32: PUInt32(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) div UInt32(Args[1].Data.Arg);
  bcDIV_iil_i64: PInt64(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) div Int64(Args[1].Data.Arg);
  bcDIV_iil_u64: PUInt64(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) div UInt64(Args[1].Data.Arg);
  bcDIV_iil_f32: PFloat32(Local(Args[2].Data.Addr))^ := Float32(Args[0].Data.Raw32) / Float32(Args[1].Data.Raw32);
  bcDIV_iil_f64: PFloat64(Local(Args[2].Data.Addr))^ := Float64(Args[0].Data.Raw) / Float64(Args[1].Data.Raw);

  // --- MOD opcodes (Modulo) ---
  // lll (local, local, local)
  bcMOD_lll_i32: PInt32(Local(Args[2].Data.Addr))^ := Modulo(PInt32(Local(Args[0].Data.Addr))^, PInt32(Local(Args[1].Data.Addr))^);
  bcMOD_lll_u32: PUInt32(Local(Args[2].Data.Addr))^ := Modulo(PUInt32(Local(Args[0].Data.Addr))^, PUInt32(Local(Args[1].Data.Addr))^);
  bcMOD_lll_i64: PInt64(Local(Args[2].Data.Addr))^ := Modulo(PInt64(Local(Args[0].Data.Addr))^, PInt64(Local(Args[1].Data.Addr))^);
  bcMOD_lll_u64: PUInt64(Local(Args[2].Data.Addr))^ := Modulo(PUInt64(Local(Args[0].Data.Addr))^, PUInt64(Local(Args[1].Data.Addr))^);
  bcMOD_lll_f32: PFloat32(Local(Args[2].Data.Addr))^ := Modulo(PFloat32(Local(Args[0].Data.Addr))^, PFloat32(Local(Args[1].Data.Addr))^);
  bcMOD_lll_f64: PFloat64(Local(Args[2].Data.Addr))^ := Modulo(PFloat64(Local(Args[0].Data.Addr))^, PFloat64(Local(Args[1].Data.Addr))^);

  // lil (local, immediate, local)
  bcMOD_lil_i32: PInt32(Local(Args[2].Data.Addr))^ := Modulo(PInt32(Local(Args[0].Data.Addr))^, Int32(Args[1].Data.Arg));
  bcMOD_lil_u32: PUInt32(Local(Args[2].Data.Addr))^ := Modulo(PUInt32(Local(Args[0].Data.Addr))^, UInt32(Args[1].Data.Arg));
  bcMOD_lil_i64: PInt64(Local(Args[2].Data.Addr))^ := Modulo(PInt64(Local(Args[0].Data.Addr))^, Int64(Args[1].Data.Arg));
  bcMOD_lil_u64: PUInt64(Local(Args[2].Data.Addr))^ := Modulo(PUInt64(Local(Args[0].Data.Addr))^, UInt64(Args[1].Data.Arg));
  bcMOD_lil_f32: PFloat32(Local(Args[2].Data.Addr))^ := Modulo(PFloat32(Local(Args[0].Data.Addr))^, Float32(Args[1].Data.Raw32));
  bcMOD_lil_f64: PFloat64(Local(Args[2].Data.Addr))^ := Modulo(PFloat64(Local(Args[0].Data.Addr))^, Float64(Args[1].Data.Raw));

  // ill (immediate, local, local)
  bcMOD_ill_i32: PInt32(Local(Args[2].Data.Addr))^ := Modulo(Int32(Args[0].Data.Arg), PInt32(Local(Args[1].Data.Addr))^);
  bcMOD_ill_u32: PUInt32(Local(Args[2].Data.Addr))^ := Modulo(UInt32(Args[0].Data.Arg), PUInt32(Local(Args[1].Data.Addr))^);
  bcMOD_ill_i64: PInt64(Local(Args[2].Data.Addr))^ := Modulo(Int64(Args[0].Data.Arg), PInt64(Local(Args[1].Data.Addr))^);
  bcMOD_ill_u64: PUInt64(Local(Args[2].Data.Addr))^ := Modulo(UInt64(Args[0].Data.Arg), PUInt64(Local(Args[1].Data.Addr))^);
  bcMOD_ill_f32: PFloat32(Local(Args[2].Data.Addr))^ := Modulo(Float32(Args[0].Data.Raw32), PFloat32(Local(Args[1].Data.Addr))^);
  bcMOD_ill_f64: PFloat64(Local(Args[2].Data.Addr))^ := Modulo(Float64(Args[0].Data.Raw), PFloat64(Local(Args[1].Data.Addr))^);

  // iil (immediate, immediate, local)
  bcMOD_iil_i32: PInt32(Local(Args[2].Data.Addr))^ := Modulo(Int32(Args[0].Data.Arg), Int32(Args[1].Data.Arg));
  bcMOD_iil_u32: PUInt32(Local(Args[2].Data.Addr))^ := Modulo(UInt32(Args[0].Data.Arg), UInt32(Args[1].Data.Arg));
  bcMOD_iil_i64: PInt64(Local(Args[2].Data.Addr))^ := Modulo(Int64(Args[0].Data.Arg), Int64(Args[1].Data.Arg));
  bcMOD_iil_u64: PUInt64(Local(Args[2].Data.Addr))^ := Modulo(UInt64(Args[0].Data.Arg), UInt64(Args[1].Data.Arg));
  bcMOD_iil_f32: PFloat32(Local(Args[2].Data.Addr))^ := Modulo(Float32(Args[0].Data.Raw32), Float32(Args[1].Data.Raw32));
  bcMOD_iil_f64: PFloat64(Local(Args[2].Data.Addr))^ := Modulo(Float64(Args[0].Data.Raw), Float64(Args[1].Data.Raw));

  // POW opcodes (Assuming a Power function is available)
  bcPOW_lll_i32: PInt32(Local(Args[2].Data.Addr))^ := Power(PInt32(Local(Args[0].Data.Addr))^, PInt32(Local(Args[1].Data.Addr))^);
  bcPOW_lll_u32: PUInt32(Local(Args[2].Data.Addr))^ := Power(PUInt32(Local(Args[0].Data.Addr))^, PUInt32(Local(Args[1].Data.Addr))^);
  bcPOW_lll_i64: PInt64(Local(Args[2].Data.Addr))^ := Power(PInt64(Local(Args[0].Data.Addr))^, PInt64(Local(Args[1].Data.Addr))^);
  bcPOW_lll_u64: PUInt64(Local(Args[2].Data.Addr))^ := Power(PUInt64(Local(Args[0].Data.Addr))^, PUInt64(Local(Args[1].Data.Addr))^);
  bcPOW_lll_f32: PFloat32(Local(Args[2].Data.Addr))^ := Power(PFloat32(Local(Args[0].Data.Addr))^, PFloat32(Local(Args[1].Data.Addr))^);
  bcPOW_lll_f64: PFloat64(Local(Args[2].Data.Addr))^ := Power(PFloat64(Local(Args[0].Data.Addr))^, PFloat64(Local(Args[1].Data.Addr))^);

  bcPOW_lil_i32: PInt32(Local(Args[2].Data.Addr))^ := Power(PInt32(Local(Args[0].Data.Addr))^, Int32(Args[1].Data.Arg));
  bcPOW_lil_u32: PUInt32(Local(Args[2].Data.Addr))^ := Power(PUInt32(Local(Args[0].Data.Addr))^, UInt32(Args[1].Data.Arg));
  bcPOW_lil_i64: PInt64(Local(Args[2].Data.Addr))^ := Power(PInt64(Local(Args[0].Data.Addr))^, Int64(Args[1].Data.Arg));
  bcPOW_lil_u64: PUInt64(Local(Args[2].Data.Addr))^ := Power(PUInt64(Local(Args[0].Data.Addr))^, UInt64(Args[1].Data.Arg));
  bcPOW_lil_f32: PFloat32(Local(Args[2].Data.Addr))^ := Power(PFloat32(Local(Args[0].Data.Addr))^, Float32(Args[1].Data.Raw32));
  bcPOW_lil_f64: PFloat64(Local(Args[2].Data.Addr))^ := Power(PFloat64(Local(Args[0].Data.Addr))^, Float64(Args[1].Data.Raw));

  bcPOW_ill_i32: PInt32(Local(Args[2].Data.Addr))^ := Power(Int32(Args[0].Data.Arg), PInt32(Local(Args[1].Data.Addr))^);
  bcPOW_ill_u32: PUInt32(Local(Args[2].Data.Addr))^ := Power(UInt32(Args[0].Data.Arg), PUInt32(Local(Args[1].Data.Addr))^);
  bcPOW_ill_i64: PInt64(Local(Args[2].Data.Addr))^ := Power(Int64(Args[0].Data.Arg), PInt64(Local(Args[1].Data.Addr))^);
  bcPOW_ill_u64: PUInt64(Local(Args[2].Data.Addr))^ := Power(UInt64(Args[0].Data.Arg), PUInt64(Local(Args[1].Data.Addr))^);
  bcPOW_ill_f32: PFloat32(Local(Args[2].Data.Addr))^ := Power(Float32(Args[0].Data.Raw32), PFloat32(Local(Args[1].Data.Addr))^);
  bcPOW_ill_f64: PFloat64(Local(Args[2].Data.Addr))^ := Power(Float64(Args[0].Data.Raw), PFloat64(Local(Args[1].Data.Addr))^);

  bcPOW_iil_i32: PInt32(Local(Args[2].Data.Addr))^ := Power(Int32(Args[0].Data.Arg), Int32(Args[1].Data.Arg));
  bcPOW_iil_u32: PUInt32(Local(Args[2].Data.Addr))^ := Power(UInt32(Args[0].Data.Arg), UInt32(Args[1].Data.Arg));
  bcPOW_iil_i64: PInt64(Local(Args[2].Data.Addr))^ := Power(Int64(Args[0].Data.Arg), Int64(Args[1].Data.Arg));
  bcPOW_iil_u64: PUInt64(Local(Args[2].Data.Addr))^ := Power(UInt64(Args[0].Data.Arg), UInt64(Args[1].Data.Arg));
  bcPOW_iil_f32: PFloat32(Local(Args[2].Data.Addr))^ := Power(Float32(Args[0].Data.Raw32), Float32(Args[1].Data.Raw32));
  bcPOW_iil_f64: PFloat64(Local(Args[2].Data.Addr))^ := Power(Float64(Args[0].Data.Raw), Float64(Args[1].Data.Raw));

  // NE opcodes
  bcNE_lll_i32: PBoolean(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ <> PInt32(Local(Args[1].Data.Addr))^;
  bcNE_lll_u32: PBoolean(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ <> PUInt32(Local(Args[1].Data.Addr))^;
  bcNE_lll_i64: PBoolean(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ <> PInt64(Local(Args[1].Data.Addr))^;
  bcNE_lll_u64: PBoolean(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ <> PUInt64(Local(Args[1].Data.Addr))^;
  bcNE_lll_f32: PBoolean(Local(Args[2].Data.Addr))^ := PFloat32(Local(Args[0].Data.Addr))^ <> PFloat32(Local(Args[1].Data.Addr))^;
  bcNE_lll_f64: PBoolean(Local(Args[2].Data.Addr))^ := PFloat64(Local(Args[0].Data.Addr))^ <> PFloat64(Local(Args[1].Data.Addr))^;

  bcNE_lil_i32: PBoolean(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ <> Int32(Args[1].Data.Arg);
  bcNE_lil_u32: PBoolean(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ <> UInt32(Args[1].Data.Arg);
  bcNE_lil_i64: PBoolean(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ <> Int64(Args[1].Data.Arg);
  bcNE_lil_u64: PBoolean(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ <> UInt64(Args[1].Data.Arg);
  bcNE_lil_f32: PBoolean(Local(Args[2].Data.Addr))^ := PFloat32(Local(Args[0].Data.Addr))^ <> Float32(Args[1].Data.Raw32);
  bcNE_lil_f64: PBoolean(Local(Args[2].Data.Addr))^ := PFloat64(Local(Args[0].Data.Addr))^ <> Float64(Args[1].Data.Raw);

  bcNE_ill_i32: PBoolean(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) <> PInt32(Local(Args[1].Data.Addr))^;
  bcNE_ill_u32: PBoolean(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) <> PUInt32(Local(Args[1].Data.Addr))^;
  bcNE_ill_i64: PBoolean(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) <> PInt64(Local(Args[1].Data.Addr))^;
  bcNE_ill_u64: PBoolean(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) <> PUInt64(Local(Args[1].Data.Addr))^;
  bcNE_ill_f32: PBoolean(Local(Args[2].Data.Addr))^ := Float32(Args[0].Data.Raw32) <> PFloat32(Local(Args[1].Data.Addr))^;
  bcNE_ill_f64: PBoolean(Local(Args[2].Data.Addr))^ := Float64(Args[0].Data.Raw) <> PFloat64(Local(Args[1].Data.Addr))^;

  bcNE_iil_i32: PBoolean(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) <> Int32(Args[1].Data.Arg);
  bcNE_iil_u32: PBoolean(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) <> UInt32(Args[1].Data.Arg);
  bcNE_iil_i64: PBoolean(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) <> Int64(Args[1].Data.Arg);
  bcNE_iil_u64: PBoolean(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) <> UInt64(Args[1].Data.Arg);
  bcNE_iil_f32: PBoolean(Local(Args[2].Data.Addr))^ := Float32(Args[0].Data.Raw32) <> Float32(Args[1].Data.Raw32);
  bcNE_iil_f64: PBoolean(Local(Args[2].Data.Addr))^ := Float64(Args[0].Data.Raw) <> Float64(Args[1].Data.Raw);

  // EQ opcodes
  bcEQ_lll_i32: PBoolean(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ = PInt32(Local(Args[1].Data.Addr))^;
  bcEQ_lll_u32: PBoolean(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ = PUInt32(Local(Args[1].Data.Addr))^;
  bcEQ_lll_i64: PBoolean(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ = PInt64(Local(Args[1].Data.Addr))^;
  bcEQ_lll_u64: PBoolean(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ = PUInt64(Local(Args[1].Data.Addr))^;
  bcEQ_lll_f32: PBoolean(Local(Args[2].Data.Addr))^ := PFloat32(Local(Args[0].Data.Addr))^ = PFloat32(Local(Args[1].Data.Addr))^;
  bcEQ_lll_f64: PBoolean(Local(Args[2].Data.Addr))^ := PFloat64(Local(Args[0].Data.Addr))^ = PFloat64(Local(Args[1].Data.Addr))^;

  bcEQ_lil_i32: PBoolean(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ = Int32(Args[1].Data.Arg);
  bcEQ_lil_u32: PBoolean(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ = UInt32(Args[1].Data.Arg);
  bcEQ_lil_i64: PBoolean(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ = Int64(Args[1].Data.Arg);
  bcEQ_lil_u64: PBoolean(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ = UInt64(Args[1].Data.Arg);
  bcEQ_lil_f32: PBoolean(Local(Args[2].Data.Addr))^ := PFloat32(Local(Args[0].Data.Addr))^ = Float32(Args[1].Data.Raw32);
  bcEQ_lil_f64: PBoolean(Local(Args[2].Data.Addr))^ := PFloat64(Local(Args[0].Data.Addr))^ = Float64(Args[1].Data.Raw);

  bcEQ_ill_i32: PBoolean(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) = PInt32(Local(Args[1].Data.Addr))^;
  bcEQ_ill_u32: PBoolean(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) = PUInt32(Local(Args[1].Data.Addr))^;
  bcEQ_ill_i64: PBoolean(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) = PInt64(Local(Args[1].Data.Addr))^;
  bcEQ_ill_u64: PBoolean(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) = PUInt64(Local(Args[1].Data.Addr))^;
  bcEQ_ill_f32: PBoolean(Local(Args[2].Data.Addr))^ := Float32(Args[0].Data.Raw32) = PFloat32(Local(Args[1].Data.Addr))^;
  bcEQ_ill_f64: PBoolean(Local(Args[2].Data.Addr))^ := Float64(Args[0].Data.Raw) = PFloat64(Local(Args[1].Data.Addr))^;

  bcEQ_iil_i32: PBoolean(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) = Int32(Args[1].Data.Arg);
  bcEQ_iil_u32: PBoolean(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) = UInt32(Args[1].Data.Arg);
  bcEQ_iil_i64: PBoolean(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) = Int64(Args[1].Data.Arg);
  bcEQ_iil_u64: PBoolean(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) = UInt64(Args[1].Data.Arg);
  bcEQ_iil_f32: PBoolean(Local(Args[2].Data.Addr))^ := Float32(Args[0].Data.Raw32) = Float32(Args[1].Data.Raw32);
  bcEQ_iil_f64: PBoolean(Local(Args[2].Data.Addr))^ := Float64(Args[0].Data.Raw) = Float64(Args[1].Data.Raw);

  // LT opcodes
  bcLT_lll_i32: PBoolean(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ < PInt32(Local(Args[1].Data.Addr))^;
  bcLT_lll_u32: PBoolean(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ < PUInt32(Local(Args[1].Data.Addr))^;
  bcLT_lll_i64: PBoolean(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ < PInt64(Local(Args[1].Data.Addr))^;
  bcLT_lll_u64: PBoolean(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ < PUInt64(Local(Args[1].Data.Addr))^;
  bcLT_lll_f32: PBoolean(Local(Args[2].Data.Addr))^ := PFloat32(Local(Args[0].Data.Addr))^ < PFloat32(Local(Args[1].Data.Addr))^;
  bcLT_lll_f64: PBoolean(Local(Args[2].Data.Addr))^ := PFloat64(Local(Args[0].Data.Addr))^ < PFloat64(Local(Args[1].Data.Addr))^;

  bcLT_lil_i32: PBoolean(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ < Int32(Args[1].Data.Arg);
  bcLT_lil_u32: PBoolean(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ < UInt32(Args[1].Data.Arg);
  bcLT_lil_i64: PBoolean(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ < Int64(Args[1].Data.Arg);
  bcLT_lil_u64: PBoolean(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ < UInt64(Args[1].Data.Arg);
  bcLT_lil_f32: PBoolean(Local(Args[2].Data.Addr))^ := PFloat32(Local(Args[0].Data.Addr))^ < Float32(Args[1].Data.Raw32);
  bcLT_lil_f64: PBoolean(Local(Args[2].Data.Addr))^ := PFloat64(Local(Args[0].Data.Addr))^ < Float64(Args[1].Data.Raw);

  bcLT_ill_i32: PBoolean(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) < PInt32(Local(Args[1].Data.Addr))^;
  bcLT_ill_u32: PBoolean(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) < PUInt32(Local(Args[1].Data.Addr))^;
  bcLT_ill_i64: PBoolean(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) < PInt64(Local(Args[1].Data.Addr))^;
  bcLT_ill_u64: PBoolean(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) < PUInt64(Local(Args[1].Data.Addr))^;
  bcLT_ill_f32: PBoolean(Local(Args[2].Data.Addr))^ := Float32(Args[0].Data.Raw32) < PFloat32(Local(Args[1].Data.Addr))^;
  bcLT_ill_f64: PBoolean(Local(Args[2].Data.Addr))^ := Float64(Args[0].Data.Raw) < PFloat64(Local(Args[1].Data.Addr))^;

  bcLT_iil_i32: PBoolean(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) < Int32(Args[1].Data.Arg);
  bcLT_iil_u32: PBoolean(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) < UInt32(Args[1].Data.Arg);
  bcLT_iil_i64: PBoolean(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) < Int64(Args[1].Data.Arg);
  bcLT_iil_u64: PBoolean(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) < UInt64(Args[1].Data.Arg);
  bcLT_iil_f32: PBoolean(Local(Args[2].Data.Addr))^ := Float32(Args[0].Data.Raw32) < Float32(Args[1].Data.Raw32);
  bcLT_iil_f64: PBoolean(Local(Args[2].Data.Addr))^ := Float64(Args[0].Data.Raw) < Float64(Args[1].Data.Raw);

  // GT opcodes
  bcGT_lll_i32: PBoolean(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ > PInt32(Local(Args[1].Data.Addr))^;
  bcGT_lll_u32: PBoolean(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ > PUInt32(Local(Args[1].Data.Addr))^;
  bcGT_lll_i64: PBoolean(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ > PInt64(Local(Args[1].Data.Addr))^;
  bcGT_lll_u64: PBoolean(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ > PUInt64(Local(Args[1].Data.Addr))^;
  bcGT_lll_f32: PBoolean(Local(Args[2].Data.Addr))^ := PFloat32(Local(Args[0].Data.Addr))^ > PFloat32(Local(Args[1].Data.Addr))^;
  bcGT_lll_f64: PBoolean(Local(Args[2].Data.Addr))^ := PFloat64(Local(Args[0].Data.Addr))^ > PFloat64(Local(Args[1].Data.Addr))^;

  bcGT_lil_i32: PBoolean(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ > Int32(Args[1].Data.Arg);
  bcGT_lil_u32: PBoolean(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ > UInt32(Args[1].Data.Arg);
  bcGT_lil_i64: PBoolean(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ > Int64(Args[1].Data.Arg);
  bcGT_lil_u64: PBoolean(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ > UInt64(Args[1].Data.Arg);
  bcGT_lil_f32: PBoolean(Local(Args[2].Data.Addr))^ := PFloat32(Local(Args[0].Data.Addr))^ > Float32(Args[1].Data.Raw32);
  bcGT_lil_f64: PBoolean(Local(Args[2].Data.Addr))^ := PFloat64(Local(Args[0].Data.Addr))^ > Float64(Args[1].Data.Raw);

  bcGT_ill_i32: PBoolean(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) > PInt32(Local(Args[1].Data.Addr))^;
  bcGT_ill_u32: PBoolean(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) > PUInt32(Local(Args[1].Data.Addr))^;
  bcGT_ill_i64: PBoolean(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) > PInt64(Local(Args[1].Data.Addr))^;
  bcGT_ill_u64: PBoolean(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) > PUInt64(Local(Args[1].Data.Addr))^;
  bcGT_ill_f32: PBoolean(Local(Args[2].Data.Addr))^ := Float32(Args[0].Data.Raw32) > PFloat32(Local(Args[1].Data.Addr))^;
  bcGT_ill_f64: PBoolean(Local(Args[2].Data.Addr))^ := Float64(Args[0].Data.Raw) > PFloat64(Local(Args[1].Data.Addr))^;

  bcGT_iil_i32: PBoolean(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) > Int32(Args[1].Data.Arg);
  bcGT_iil_u32: PBoolean(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) > UInt32(Args[1].Data.Arg);
  bcGT_iil_i64: PBoolean(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) > Int64(Args[1].Data.Arg);
  bcGT_iil_u64: PBoolean(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) > UInt64(Args[1].Data.Arg);
  bcGT_iil_f32: PBoolean(Local(Args[2].Data.Addr))^ := Float32(Args[0].Data.Raw32) > Float32(Args[1].Data.Raw32);
  bcGT_iil_f64: PBoolean(Local(Args[2].Data.Addr))^ := Float64(Args[0].Data.Raw) > Float64(Args[1].Data.Raw);

  // GTE opcodes
  bcGTE_lll_i32: PBoolean(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ >= PInt32(Local(Args[1].Data.Addr))^;
  bcGTE_lll_u32: PBoolean(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ >= PUInt32(Local(Args[1].Data.Addr))^;
  bcGTE_lll_i64: PBoolean(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ >= PInt64(Local(Args[1].Data.Addr))^;
  bcGTE_lll_u64: PBoolean(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ >= PUInt64(Local(Args[1].Data.Addr))^;
  bcGTE_lll_f32: PBoolean(Local(Args[2].Data.Addr))^ := PFloat32(Local(Args[0].Data.Addr))^ >= PFloat32(Local(Args[1].Data.Addr))^;
  bcGTE_lll_f64: PBoolean(Local(Args[2].Data.Addr))^ := PFloat64(Local(Args[0].Data.Addr))^ >= PFloat64(Local(Args[1].Data.Addr))^;

  bcGTE_lil_i32: PBoolean(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ >= Int32(Args[1].Data.Arg);
  bcGTE_lil_u32: PBoolean(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ >= UInt32(Args[1].Data.Arg);
  bcGTE_lil_i64: PBoolean(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ >= Int64(Args[1].Data.Arg);
  bcGTE_lil_u64: PBoolean(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ >= UInt64(Args[1].Data.Arg);
  bcGTE_lil_f32: PBoolean(Local(Args[2].Data.Addr))^ := PFloat32(Local(Args[0].Data.Addr))^ >= Float32(Args[1].Data.Raw32);
  bcGTE_lil_f64: PBoolean(Local(Args[2].Data.Addr))^ := PFloat64(Local(Args[0].Data.Addr))^ >= Float64(Args[1].Data.Raw);

  bcGTE_ill_i32: PBoolean(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) >= PInt32(Local(Args[1].Data.Addr))^;
  bcGTE_ill_u32: PBoolean(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) >= PUInt32(Local(Args[1].Data.Addr))^;
  bcGTE_ill_i64: PBoolean(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) >= PInt64(Local(Args[1].Data.Addr))^;
  bcGTE_ill_u64: PBoolean(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) >= PUInt64(Local(Args[1].Data.Addr))^;
  bcGTE_ill_f32: PBoolean(Local(Args[2].Data.Addr))^ := Float32(Args[0].Data.Raw32) >= PFloat32(Local(Args[1].Data.Addr))^;
  bcGTE_ill_f64: PBoolean(Local(Args[2].Data.Addr))^ := Float64(Args[0].Data.Raw) >= PFloat64(Local(Args[1].Data.Addr))^;

  bcGTE_iil_i32: PBoolean(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) >= Int32(Args[1].Data.Arg);
  bcGTE_iil_u32: PBoolean(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) >= UInt32(Args[1].Data.Arg);
  bcGTE_iil_i64: PBoolean(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) >= Int64(Args[1].Data.Arg);
  bcGTE_iil_u64: PBoolean(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) >= UInt64(Args[1].Data.Arg);
  bcGTE_iil_f32: PBoolean(Local(Args[2].Data.Addr))^ := Float32(Args[0].Data.Raw32) >= Float32(Args[1].Data.Raw32);
  bcGTE_iil_f64: PBoolean(Local(Args[2].Data.Addr))^ := Float64(Args[0].Data.Raw) >= Float64(Args[1].Data.Raw);

  // LTE opcodes
  bcLTE_lll_i32: PBoolean(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ <= PInt32(Local(Args[1].Data.Addr))^;
  bcLTE_lll_u32: PBoolean(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ <= PUInt32(Local(Args[1].Data.Addr))^;
  bcLTE_lll_i64: PBoolean(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ <= PInt64(Local(Args[1].Data.Addr))^;
  bcLTE_lll_u64: PBoolean(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ <= PUInt64(Local(Args[1].Data.Addr))^;
  bcLTE_lll_f32: PBoolean(Local(Args[2].Data.Addr))^ := PFloat32(Local(Args[0].Data.Addr))^ <= PFloat32(Local(Args[1].Data.Addr))^;
  bcLTE_lll_f64: PBoolean(Local(Args[2].Data.Addr))^ := PFloat64(Local(Args[0].Data.Addr))^ <= PFloat64(Local(Args[1].Data.Addr))^;

  bcLTE_lil_i32: PBoolean(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ <= Int32(Args[1].Data.Arg);
  bcLTE_lil_u32: PBoolean(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ <= UInt32(Args[1].Data.Arg);
  bcLTE_lil_i64: PBoolean(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ <= Int64(Args[1].Data.Arg);
  bcLTE_lil_u64: PBoolean(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ <= UInt64(Args[1].Data.Arg);
  bcLTE_lil_f32: PBoolean(Local(Args[2].Data.Addr))^ := PFloat32(Local(Args[0].Data.Addr))^ <= Float32(Args[1].Data.Raw32);
  bcLTE_lil_f64: PBoolean(Local(Args[2].Data.Addr))^ := PFloat64(Local(Args[0].Data.Addr))^ <= Float64(Args[1].Data.Raw);

  bcLTE_ill_i32: PBoolean(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) <= PInt32(Local(Args[1].Data.Addr))^;
  bcLTE_ill_u32: PBoolean(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) <= PUInt32(Local(Args[1].Data.Addr))^;
  bcLTE_ill_i64: PBoolean(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) <= PInt64(Local(Args[1].Data.Addr))^;
  bcLTE_ill_u64: PBoolean(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) <= PUInt64(Local(Args[1].Data.Addr))^;
  bcLTE_ill_f32: PBoolean(Local(Args[2].Data.Addr))^ := Float32(Args[0].Data.Raw32) <= PFloat32(Local(Args[1].Data.Addr))^;
  bcLTE_ill_f64: PBoolean(Local(Args[2].Data.Addr))^ := Float64(Args[0].Data.Raw) <= PFloat64(Local(Args[1].Data.Addr))^;

  bcLTE_iil_i32: PBoolean(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) <= Int32(Args[1].Data.Arg);
  bcLTE_iil_u32: PBoolean(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) <= UInt32(Args[1].Data.Arg);
  bcLTE_iil_i64: PBoolean(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) <= Int64(Args[1].Data.Arg);
  bcLTE_iil_u64: PBoolean(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) <= UInt64(Args[1].Data.Arg);
  bcLTE_iil_f32: PBoolean(Local(Args[2].Data.Addr))^ := Float32(Args[0].Data.Raw32) <= Float32(Args[1].Data.Raw32);
  bcLTE_iil_f64: PBoolean(Local(Args[2].Data.Addr))^ := Float64(Args[0].Data.Raw) <= Float64(Args[1].Data.Raw);

  // BND opcodes (Bitwise AND)
  bcBND_lll_i32: PInt32(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ and PInt32(Local(Args[1].Data.Addr))^;
  bcBND_lll_u32: PUInt32(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ and PUInt32(Local(Args[1].Data.Addr))^;
  bcBND_lll_i64: PInt64(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ and PInt64(Local(Args[1].Data.Addr))^;
  bcBND_lll_u64: PUInt64(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ and PUInt64(Local(Args[1].Data.Addr))^;

  bcBND_lil_i32: PInt32(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ and Int32(Args[1].Data.Arg);
  bcBND_lil_u32: PUInt32(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ and UInt32(Args[1].Data.Arg);
  bcBND_lil_i64: PInt64(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ and Int64(Args[1].Data.Arg);
  bcBND_lil_u64: PUInt64(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ and UInt64(Args[1].Data.Arg);

  bcBND_ill_i32: PInt32(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) and PInt32(Local(Args[1].Data.Addr))^;
  bcBND_ill_u32: PUInt32(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) and PUInt32(Local(Args[1].Data.Addr))^;
  bcBND_ill_i64: PInt64(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) and PInt64(Local(Args[1].Data.Addr))^;
  bcBND_ill_u64: PUInt64(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) and PUInt64(Local(Args[1].Data.Addr))^;

  bcBND_iil_i32: PInt32(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) and Int32(Args[1].Data.Arg);
  bcBND_iil_u32: PUInt32(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) and UInt32(Args[1].Data.Arg);
  bcBND_iil_i64: PInt64(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) and Int64(Args[1].Data.Arg);
  bcBND_iil_u64: PUInt64(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) and UInt64(Args[1].Data.Arg);

  // SHL opcodes (Shift Left)
  bcSHL_lll_i32: PInt32(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ shl PInt32(Local(Args[1].Data.Addr))^;
  bcSHL_lll_u32: PUInt32(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ shl PUInt32(Local(Args[1].Data.Addr))^;
  bcSHL_lll_i64: PInt64(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ shl PInt64(Local(Args[1].Data.Addr))^;
  bcSHL_lll_u64: PUInt64(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ shl PUInt64(Local(Args[1].Data.Addr))^;

  bcSHL_lil_i32: PInt32(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ shl Int32(Args[1].Data.Arg);
  bcSHL_lil_u32: PUInt32(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ shl UInt32(Args[1].Data.Arg);
  bcSHL_lil_i64: PInt64(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ shl Int64(Args[1].Data.Arg);
  bcSHL_lil_u64: PUInt64(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ shl UInt64(Args[1].Data.Arg);

  bcSHL_ill_i32: PInt32(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) shl PInt32(Local(Args[1].Data.Addr))^;
  bcSHL_ill_u32: PUInt32(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) shl PUInt32(Local(Args[1].Data.Addr))^;
  bcSHL_ill_i64: PInt64(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) shl PInt64(Local(Args[1].Data.Addr))^;
  bcSHL_ill_u64: PUInt64(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) shl PUInt64(Local(Args[1].Data.Addr))^;

  bcSHL_iil_i32: PInt32(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) shl Int32(Args[1].Data.Arg);
  bcSHL_iil_u32: PUInt32(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) shl UInt32(Args[1].Data.Arg);
  bcSHL_iil_i64: PInt64(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) shl Int64(Args[1].Data.Arg);
  bcSHL_iil_u64: PUInt64(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) shl UInt64(Args[1].Data.Arg);

  // SHR opcodes (Shift Right)
  bcSHR_lll_i32: PInt32(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ shr PInt32(Local(Args[1].Data.Addr))^;
  bcSHR_lll_u32: PUInt32(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ shr PUInt32(Local(Args[1].Data.Addr))^;
  bcSHR_lll_i64: PInt64(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ shr PInt64(Local(Args[1].Data.Addr))^;
  bcSHR_lll_u64: PUInt64(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ shr PUInt64(Local(Args[1].Data.Addr))^;

  bcSHR_lil_i32: PInt32(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ shr Int32(Args[1].Data.Arg);
  bcSHR_lil_u32: PUInt32(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ shr UInt32(Args[1].Data.Arg);
  bcSHR_lil_i64: PInt64(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ shr Int64(Args[1].Data.Arg);
  bcSHR_lil_u64: PUInt64(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ shr UInt64(Args[1].Data.Arg);

  bcSHR_ill_i32: PInt32(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) shr PInt32(Local(Args[1].Data.Addr))^;
  bcSHR_ill_u32: PUInt32(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) shr PUInt32(Local(Args[1].Data.Addr))^;
  bcSHR_ill_i64: PInt64(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) shr PInt64(Local(Args[1].Data.Addr))^;
  bcSHR_ill_u64: PUInt64(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) shr PUInt64(Local(Args[1].Data.Addr))^;

  bcSHR_iil_i32: PInt32(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) shr Int32(Args[1].Data.Arg);
  bcSHR_iil_u32: PUInt32(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) shr UInt32(Args[1].Data.Arg);
  bcSHR_iil_i64: PInt64(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) shr Int64(Args[1].Data.Arg);
  bcSHR_iil_u64: PUInt64(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) shr UInt64(Args[1].Data.Arg);

  // XOR opcodes (Bitwise XOR)
  bcXOR_lll_i32: PInt32(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ xor PInt32(Local(Args[1].Data.Addr))^;
  bcXOR_lll_u32: PUInt32(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ xor PUInt32(Local(Args[1].Data.Addr))^;
  bcXOR_lll_i64: PInt64(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ xor PInt64(Local(Args[1].Data.Addr))^;
  bcXOR_lll_u64: PUInt64(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ xor PUInt64(Local(Args[1].Data.Addr))^;

  bcXOR_lil_i32: PInt32(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ xor Int32(Args[1].Data.Arg);
  bcXOR_lil_u32: PUInt32(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ xor UInt32(Args[1].Data.Arg);
  bcXOR_lil_i64: PInt64(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ xor Int64(Args[1].Data.Arg);
  bcXOR_lil_u64: PUInt64(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ xor UInt64(Args[1].Data.Arg);

  bcXOR_ill_i32: PInt32(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) xor PInt32(Local(Args[1].Data.Addr))^;
  bcXOR_ill_u32: PUInt32(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) xor PUInt32(Local(Args[1].Data.Addr))^;
  bcXOR_ill_i64: PInt64(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) xor PInt64(Local(Args[1].Data.Addr))^;
  bcXOR_ill_u64: PUInt64(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) xor PUInt64(Local(Args[1].Data.Addr))^;

  bcXOR_iil_i32: PInt32(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) xor Int32(Args[1].Data.Arg);
  bcXOR_iil_u32: PUInt32(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) xor UInt32(Args[1].Data.Arg);
  bcXOR_iil_i64: PInt64(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) xor Int64(Args[1].Data.Arg);
  bcXOR_iil_u64: PUInt64(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) xor UInt64(Args[1].Data.Arg);

  // BOR opcodes (Bitwise OR)
  bcBOR_lll_i32: PInt32(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ or PInt32(Local(Args[1].Data.Addr))^;
  bcBOR_lll_u32: PUInt32(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ or PUInt32(Local(Args[1].Data.Addr))^;
  bcBOR_lll_i64: PInt64(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ or PInt64(Local(Args[1].Data.Addr))^;
  bcBOR_lll_u64: PUInt64(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ or PUInt64(Local(Args[1].Data.Addr))^;

  bcBOR_lil_i32: PInt32(Local(Args[2].Data.Addr))^ := PInt32(Local(Args[0].Data.Addr))^ or Int32(Args[1].Data.Arg);
  bcBOR_lil_u32: PUInt32(Local(Args[2].Data.Addr))^ := PUInt32(Local(Args[0].Data.Addr))^ or UInt32(Args[1].Data.Arg);
  bcBOR_lil_i64: PInt64(Local(Args[2].Data.Addr))^ := PInt64(Local(Args[0].Data.Addr))^ or Int64(Args[1].Data.Arg);
  bcBOR_lil_u64: PUInt64(Local(Args[2].Data.Addr))^ := PUInt64(Local(Args[0].Data.Addr))^ or UInt64(Args[1].Data.Arg);

  bcBOR_ill_i32: PInt32(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) or PInt32(Local(Args[1].Data.Addr))^;
  bcBOR_ill_u32: PUInt32(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) or PUInt32(Local(Args[1].Data.Addr))^;
  bcBOR_ill_i64: PInt64(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) or PInt64(Local(Args[1].Data.Addr))^;
  bcBOR_ill_u64: PUInt64(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) or PUInt64(Local(Args[1].Data.Addr))^;

  bcBOR_iil_i32: PInt32(Local(Args[2].Data.Addr))^ := Int32(Args[0].Data.Arg) or Int32(Args[1].Data.Arg);
  bcBOR_iil_u32: PUInt32(Local(Args[2].Data.Addr))^ := UInt32(Args[0].Data.Arg) or UInt32(Args[1].Data.Arg);
  bcBOR_iil_i64: PInt64(Local(Args[2].Data.Addr))^ := Int64(Args[0].Data.Arg) or Int64(Args[1].Data.Arg);
  bcBOR_iil_u64: PUInt64(Local(Args[2].Data.Addr))^ := UInt64(Args[0].Data.Arg) or UInt64(Args[1].Data.Arg);

  // --- SAR opcodes (Shift Arithmetic Right) ---
  // lll (local, local, local)
  bcSAR_lll_i32: PInt32(Local(Args[2].Data.Addr))^ := Sar(PInt32(Local(Args[0].Data.Addr))^, PInt32(Local(Args[1].Data.Addr))^);
  bcSAR_lll_u32: PUInt32(Local(Args[2].Data.Addr))^ := Sar(PUInt32(Local(Args[0].Data.Addr))^, PUInt32(Local(Args[1].Data.Addr))^);
  bcSAR_lll_i64: PInt64(Local(Args[2].Data.Addr))^ := Sar(PInt64(Local(Args[0].Data.Addr))^, PInt64(Local(Args[1].Data.Addr))^);
  bcSAR_lll_u64: PUInt64(Local(Args[2].Data.Addr))^ := Sar(PUInt64(Local(Args[0].Data.Addr))^, PUInt64(Local(Args[1].Data.Addr))^);

  // lil (local, immediate, local)
  bcSAR_lil_i32: PInt32(Local(Args[2].Data.Addr))^ := Sar(PInt32(Local(Args[0].Data.Addr))^, Int32(Args[1].Data.Arg));
  bcSAR_lil_u32: PUInt32(Local(Args[2].Data.Addr))^ := Sar(PUInt32(Local(Args[0].Data.Addr))^, UInt32(Args[1].Data.Arg));
  bcSAR_lil_i64: PInt64(Local(Args[2].Data.Addr))^ := Sar(PInt64(Local(Args[0].Data.Addr))^, Int64(Args[1].Data.Arg));
  bcSAR_lil_u64: PUInt64(Local(Args[2].Data.Addr))^ := Sar(PUInt64(Local(Args[0].Data.Addr))^, UInt64(Args[1].Data.Arg));

  // ill (immediate, local, local)
  bcSAR_ill_i32: PInt32(Local(Args[2].Data.Addr))^ := Sar(Int32(Args[0].Data.Arg), PInt32(Local(Args[1].Data.Addr))^);
  bcSAR_ill_u32: PUInt32(Local(Args[2].Data.Addr))^ := Sar(UInt32(Args[0].Data.Arg), PUInt32(Local(Args[1].Data.Addr))^);
  bcSAR_ill_i64: PInt64(Local(Args[2].Data.Addr))^ := Sar(Int64(Args[0].Data.Arg), PInt64(Local(Args[1].Data.Addr))^);
  bcSAR_ill_u64: PUInt64(Local(Args[2].Data.Addr))^ := Sar(UInt64(Args[0].Data.Arg), PUInt64(Local(Args[1].Data.Addr))^);

  // iil (immediate, immediate, local)
  bcSAR_iil_i32: PInt32(Local(Args[2].Data.Addr))^ := Sar(Int32(Args[0].Data.Arg), Int32(Args[1].Data.Arg));
  bcSAR_iil_u32: PUInt32(Local(Args[2].Data.Addr))^ := Sar(UInt32(Args[0].Data.Arg), UInt32(Args[1].Data.Arg));
  bcSAR_iil_i64: PInt64(Local(Args[2].Data.Addr))^ := Sar(Int64(Args[0].Data.Arg), Int64(Args[1].Data.Arg));
  bcSAR_iil_u64: PUInt64(Local(Args[2].Data.Addr))^ := Sar(UInt64(Args[0].Data.Arg), UInt64(Args[1].Data.Arg));
