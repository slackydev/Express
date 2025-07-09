  // --- Arithmetic Operations ---

  bcADD, bcSUB, bcMUL, bcDIV, bcMOD,
  bcBND, bcBOR, bcXOR, bcEQ, bcNE,
  bcGT, bcLT, bcGTE, bcLTE,
  bcSAR, bcSHL, bcSHR:
  begin
    // Point left operand
    if Args[0].Pos = mpLocal then
      left := Stack.Local(Args[0].Addr)
    else
      left := @Args[0].Arg;    

    // Point right operand
    if Args[1].Pos = mpLocal then
      right := Stack.Local(Args[1].Addr)
    else
      right := @Args[1].Arg;

    // Type dispatch
    case Args[0].BaseType of

      xtInt8:
        case Code of
          bcADD: PInt8(Local(Args[2].Addr))^ := PInt8(left)^ + PInt8(right)^;
          bcSUB: PInt8(Local(Args[2].Addr))^ := PInt8(left)^ - PInt8(right)^;
          bcMUL: PInt8(Local(Args[2].Addr))^ := PInt8(left)^ * PInt8(right)^;
          bcDIV: PInt8(Local(Args[2].Addr))^ := PInt8(left)^ div PInt8(right)^;
          bcMOD: PInt8(Local(Args[2].Addr))^ := Modulo(PInt8(left)^, PInt8(right)^);
          bcEQ:  PInt8(Local(Args[2].Addr))^ := Ord(PInt8(left)^ = PInt8(right)^);
          bcNE:  PInt8(Local(Args[2].Addr))^ := Ord(PInt8(left)^ <> PInt8(right)^);
          bcGT:  PInt8(Local(Args[2].Addr))^ := Ord(PInt8(left)^ > PInt8(right)^);
          bcLT:  PInt8(Local(Args[2].Addr))^ := Ord(PInt8(left)^ < PInt8(right)^);
          bcGTE: PInt8(Local(Args[2].Addr))^ := Ord(PInt8(left)^ >= PInt8(right)^);
          bcLTE: PInt8(Local(Args[2].Addr))^ := Ord(PInt8(left)^ <= PInt8(right)^);
          bcBND: PInt8(Local(Args[2].Addr))^ := PInt8(left)^ and PInt8(right)^;
          bcBOR: PInt8(Local(Args[2].Addr))^ := PInt8(left)^ or  PInt8(right)^;
          bcXOR: PInt8(Local(Args[2].Addr))^ := PInt8(left)^ xor PInt8(right)^;
          bcSAR: PInt8(Local(Args[2].Addr))^ := Sar(PInt8(left)^, PInt8(right)^);
          bcSHR: PInt8(Local(Args[2].Addr))^ := PInt8(left)^ shr PInt8(right)^;
          bcSHL: PInt8(Local(Args[2].Addr))^ := PInt8(left)^ shl PInt8(right)^;
        end;

      xtUInt8:
        case Code of
          bcADD: PUInt8(Local(Args[2].Addr))^ := PUInt8(left)^ + PUInt8(right)^;
          bcSUB: PUInt8(Local(Args[2].Addr))^ := PUInt8(left)^ - PUInt8(right)^;
          bcMUL: PUInt8(Local(Args[2].Addr))^ := PUInt8(left)^ * PUInt8(right)^;
          bcDIV: PUInt8(Local(Args[2].Addr))^ := PUInt8(left)^ div PUInt8(right)^;
          bcMOD: PUInt8(Local(Args[2].Addr))^ := Modulo(PUInt8(left)^, PUInt8(right)^);
          bcEQ:  PUInt8(Local(Args[2].Addr))^ := Ord(PUInt8(left)^ = PUInt8(right)^);
          bcNE:  PUInt8(Local(Args[2].Addr))^ := Ord(PUInt8(left)^ <> PUInt8(right)^);
          bcGT:  PUInt8(Local(Args[2].Addr))^ := Ord(PUInt8(left)^ > PUInt8(right)^);
          bcLT:  PUInt8(Local(Args[2].Addr))^ := Ord(PUInt8(left)^ < PUInt8(right)^);
          bcGTE: PUInt8(Local(Args[2].Addr))^ := Ord(PUInt8(left)^ >= PUInt8(right)^);
          bcLTE: PUInt8(Local(Args[2].Addr))^ := Ord(PUInt8(left)^ <= PUInt8(right)^);
          bcBND: PUInt8(Local(Args[2].Addr))^ := PUInt8(left)^ and PUInt8(right)^;
          bcBOR: PUInt8(Local(Args[2].Addr))^ := PUInt8(left)^ or  PUInt8(right)^;
          bcXOR: PUInt8(Local(Args[2].Addr))^ := PUInt8(left)^ xor PUInt8(right)^;
          bcSAR: PUInt8(Local(Args[2].Addr))^ := Sar(PUInt8(left)^, PUInt8(right)^);
          bcSHR: PUInt8(Local(Args[2].Addr))^ := PUInt8(left)^ shr PUInt8(right)^;
          bcSHL: PUInt8(Local(Args[2].Addr))^ := PUInt8(left)^ shl PUInt8(right)^;
        end;

      xtInt16:
        case Code of
          bcADD: PInt16(Local(Args[2].Addr))^ := PInt16(left)^ + PInt16(right)^;
          bcSUB: PInt16(Local(Args[2].Addr))^ := PInt16(left)^ - PInt16(right)^;
          bcMUL: PInt16(Local(Args[2].Addr))^ := PInt16(left)^ * PInt16(right)^;
          bcDIV: PInt16(Local(Args[2].Addr))^ := PInt16(left)^ div PInt16(right)^;
          bcMOD: PInt16(Local(Args[2].Addr))^ := Modulo(PInt16(left)^, PInt16(right)^);
          bcEQ:  PInt16(Local(Args[2].Addr))^ := Ord(PInt16(left)^ = PInt16(right)^);
          bcNE:  PInt16(Local(Args[2].Addr))^ := Ord(PInt16(left)^ <> PInt16(right)^);
          bcGT:  PInt16(Local(Args[2].Addr))^ := Ord(PInt16(left)^ > PInt16(right)^);
          bcLT:  PInt16(Local(Args[2].Addr))^ := Ord(PInt16(left)^ < PInt16(right)^);
          bcGTE: PInt16(Local(Args[2].Addr))^ := Ord(PInt16(left)^ >= PInt16(right)^);
          bcLTE: PInt16(Local(Args[2].Addr))^ := Ord(PInt16(left)^ <= PInt16(right)^);
          bcBND: PInt16(Local(Args[2].Addr))^ := PInt16(left)^ and PInt16(right)^;
          bcBOR: PInt16(Local(Args[2].Addr))^ := PInt16(left)^ or  PInt16(right)^;
          bcXOR: PInt16(Local(Args[2].Addr))^ := PInt16(left)^ xor PInt16(right)^;
          bcSAR: PInt16(Local(Args[2].Addr))^ := Sar(PInt16(left)^, PInt16(right)^);
          bcSHR: PInt16(Local(Args[2].Addr))^ := PInt16(left)^ shr PInt16(right)^;
          bcSHL: PInt16(Local(Args[2].Addr))^ := PInt16(left)^ shl PInt16(right)^;
        end;

      xtUInt16:
        case Code of
          bcADD: PUInt16(Local(Args[2].Addr))^ := PUInt16(left)^ + PUInt16(right)^;
          bcSUB: PUInt16(Local(Args[2].Addr))^ := PUInt16(left)^ - PUInt16(right)^;
          bcMUL: PUInt16(Local(Args[2].Addr))^ := PUInt16(left)^ * PUInt16(right)^;
          bcDIV: PUInt16(Local(Args[2].Addr))^ := PUInt16(left)^ div PUInt16(right)^;
          bcMOD: PUInt16(Local(Args[2].Addr))^ := Modulo(PUInt16(left)^, PUInt16(right)^);
          bcEQ:  PUInt16(Local(Args[2].Addr))^ := Ord(PUInt16(left)^ = PUInt16(right)^);
          bcNE:  PUInt16(Local(Args[2].Addr))^ := Ord(PUInt16(left)^ <> PUInt16(right)^);
          bcGT:  PUInt16(Local(Args[2].Addr))^ := Ord(PUInt16(left)^ > PUInt16(right)^);
          bcLT:  PUInt16(Local(Args[2].Addr))^ := Ord(PUInt16(left)^ < PUInt16(right)^);
          bcGTE: PUInt16(Local(Args[2].Addr))^ := Ord(PUInt16(left)^ >= PUInt16(right)^);
          bcLTE: PUInt16(Local(Args[2].Addr))^ := Ord(PUInt16(left)^ <= PUInt16(right)^);
          bcBND: PUInt16(Local(Args[2].Addr))^ := PUInt16(left)^ and PUInt16(right)^;
          bcBOR: PUInt16(Local(Args[2].Addr))^ := PUInt16(left)^ or  PUInt16(right)^;
          bcXOR: PUInt16(Local(Args[2].Addr))^ := PUInt16(left)^ xor PUInt16(right)^;
          bcSAR: PUInt16(Local(Args[2].Addr))^ := Sar(PUInt16(left)^, PUInt16(right)^);
          bcSHR: PUInt16(Local(Args[2].Addr))^ := PUInt16(left)^ shr PUInt16(right)^;
          bcSHL: PUInt16(Local(Args[2].Addr))^ := PUInt16(left)^ shl PUInt16(right)^;
        end;

    end;
  end;

  // ADD opcodes
  bcADD_lll_i32: PInt32(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ + PInt32(Local(Args[1].Addr))^;
  bcADD_lll_u32: PUInt32(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ + PUInt32(Local(Args[1].Addr))^;
  bcADD_lll_i64: PInt64(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ + PInt64(Local(Args[1].Addr))^;
  bcADD_lll_u64: PUInt64(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ + PUInt64(Local(Args[1].Addr))^;
  bcADD_lll_f32: PFloat32(Local(Args[2].Addr))^ := PFloat32(Local(Args[0].Addr))^ + PFloat32(Local(Args[1].Addr))^;
  bcADD_lll_f64: PFloat64(Local(Args[2].Addr))^ := PFloat64(Local(Args[0].Addr))^ + PFloat64(Local(Args[1].Addr))^;

  bcADD_lil_i32: PInt32(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ + Int32(Args[1].Arg);
  bcADD_lil_u32: PUInt32(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ + UInt32(Args[1].Arg);
  bcADD_lil_i64: PInt64(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ + Int64(Args[1].Arg);
  bcADD_lil_u64: PUInt64(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ + UInt64(Args[1].Arg);
  bcADD_lil_f32: PFloat32(Local(Args[2].Addr))^ := PFloat32(Local(Args[0].Addr))^ + Float32(Args[1].Arg);
  bcADD_lil_f64: PFloat64(Local(Args[2].Addr))^ := PFloat64(Local(Args[0].Addr))^ + Float64(Args[1].Arg);

  bcADD_ill_i32: PInt32(Local(Args[2].Addr))^ := Int32(Args[0].Arg) + PInt32(Local(Args[1].Addr))^;
  bcADD_ill_u32: PUInt32(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) + PUInt32(Local(Args[1].Addr))^;
  bcADD_ill_i64: PInt64(Local(Args[2].Addr))^ := Int64(Args[0].Arg) + PInt64(Local(Args[1].Addr))^;
  bcADD_ill_u64: PUInt64(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) + PUInt64(Local(Args[1].Addr))^;
  bcADD_ill_f32: PFloat32(Local(Args[2].Addr))^ := Float32(Args[0].Arg) + PFloat32(Local(Args[1].Addr))^;
  bcADD_ill_f64: PFloat64(Local(Args[2].Addr))^ := Float64(Args[0].Arg) + PFloat64(Local(Args[1].Addr))^;

  bcADD_iil_i32: PInt32(Local(Args[2].Addr))^ := Int32(Args[0].Arg) + Int32(Args[1].Arg);
  bcADD_iil_u32: PUInt32(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) + UInt32(Args[1].Arg);
  bcADD_iil_i64: PInt64(Local(Args[2].Addr))^ := Int64(Args[0].Arg) + Int64(Args[1].Arg);
  bcADD_iil_u64: PUInt64(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) + UInt64(Args[1].Arg);
  bcADD_iil_f32: PFloat32(Local(Args[2].Addr))^ := Float32(Args[0].Arg) + Float32(Args[1].Arg);
  bcADD_iil_f64: PFloat64(Local(Args[2].Addr))^ := Float64(Args[0].Arg) + Float64(Args[1].Arg);

  // SUB opcodes
  bcSUB_lll_i32: PInt32(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ - PInt32(Local(Args[1].Addr))^;
  bcSUB_lll_u32: PUInt32(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ - PUInt32(Local(Args[1].Addr))^;
  bcSUB_lll_i64: PInt64(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ - PInt64(Local(Args[1].Addr))^;
  bcSUB_lll_u64: PUInt64(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ - PUInt64(Local(Args[1].Addr))^;
  bcSUB_lll_f32: PFloat32(Local(Args[2].Addr))^ := PFloat32(Local(Args[0].Addr))^ - PFloat32(Local(Args[1].Addr))^;
  bcSUB_lll_f64: PFloat64(Local(Args[2].Addr))^ := PFloat64(Local(Args[0].Addr))^ - PFloat64(Local(Args[1].Addr))^;

  bcSUB_lil_i32: PInt32(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ - Int32(Args[1].Arg);
  bcSUB_lil_u32: PUInt32(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ - UInt32(Args[1].Arg);
  bcSUB_lil_i64: PInt64(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ - Int64(Args[1].Arg);
  bcSUB_lil_u64: PUInt64(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ - UInt64(Args[1].Arg);
  bcSUB_lil_f32: PFloat32(Local(Args[2].Addr))^ := PFloat32(Local(Args[0].Addr))^ - Float32(Args[1].Arg);
  bcSUB_lil_f64: PFloat64(Local(Args[2].Addr))^ := PFloat64(Local(Args[0].Addr))^ - Float64(Args[1].Arg);

  bcSUB_ill_i32: PInt32(Local(Args[2].Addr))^ := Int32(Args[0].Arg) - PInt32(Local(Args[1].Addr))^;
  bcSUB_ill_u32: PUInt32(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) - PUInt32(Local(Args[1].Addr))^;
  bcSUB_ill_i64: PInt64(Local(Args[2].Addr))^ := Int64(Args[0].Arg) - PInt64(Local(Args[1].Addr))^;
  bcSUB_ill_u64: PUInt64(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) - PUInt64(Local(Args[1].Addr))^;
  bcSUB_ill_f32: PFloat32(Local(Args[2].Addr))^ := Float32(Args[0].Arg) - PFloat32(Local(Args[1].Addr))^;
  bcSUB_ill_f64: PFloat64(Local(Args[2].Addr))^ := Float64(Args[0].Arg) - PFloat64(Local(Args[1].Addr))^;

  bcSUB_iil_i32: PInt32(Local(Args[2].Addr))^ := Int32(Args[0].Arg) - Int32(Args[1].Arg);
  bcSUB_iil_u32: PUInt32(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) - UInt32(Args[1].Arg);
  bcSUB_iil_i64: PInt64(Local(Args[2].Addr))^ := Int64(Args[0].Arg) - Int64(Args[1].Arg);
  bcSUB_iil_u64: PUInt64(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) - UInt64(Args[1].Arg);
  bcSUB_iil_f32: PFloat32(Local(Args[2].Addr))^ := Float32(Args[0].Arg) - Float32(Args[1].Arg);
  bcSUB_iil_f64: PFloat64(Local(Args[2].Addr))^ := Float64(Args[0].Arg) - Float64(Args[1].Arg);

  // MUL opcodes
  bcMUL_lll_i32: PInt32(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ * PInt32(Local(Args[1].Addr))^;
  bcMUL_lll_u32: PUInt32(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ * PUInt32(Local(Args[1].Addr))^;
  bcMUL_lll_i64: PInt64(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ * PInt64(Local(Args[1].Addr))^;
  bcMUL_lll_u64: PUInt64(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ * PUInt64(Local(Args[1].Addr))^;
  bcMUL_lll_f32: PFloat32(Local(Args[2].Addr))^ := PFloat32(Local(Args[0].Addr))^ * PFloat32(Local(Args[1].Addr))^;
  bcMUL_lll_f64: PFloat64(Local(Args[2].Addr))^ := PFloat64(Local(Args[0].Addr))^ * PFloat64(Local(Args[1].Addr))^;

  bcMUL_lil_i32: PInt32(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ * Int32(Args[1].Arg);
  bcMUL_lil_u32: PUInt32(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ * UInt32(Args[1].Arg);
  bcMUL_lil_i64: PInt64(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ * Int64(Args[1].Arg);
  bcMUL_lil_u64: PUInt64(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ * UInt64(Args[1].Arg);
  bcMUL_lil_f32: PFloat32(Local(Args[2].Addr))^ := PFloat32(Local(Args[0].Addr))^ * Float32(Args[1].Arg);
  bcMUL_lil_f64: PFloat64(Local(Args[2].Addr))^ := PFloat64(Local(Args[0].Addr))^ * Float64(Args[1].Arg);

  bcMUL_ill_i32: PInt32(Local(Args[2].Addr))^ := Int32(Args[0].Arg) * PInt32(Local(Args[1].Addr))^;
  bcMUL_ill_u32: PUInt32(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) * PUInt32(Local(Args[1].Addr))^;
  bcMUL_ill_i64: PInt64(Local(Args[2].Addr))^ := Int64(Args[0].Arg) * PInt64(Local(Args[1].Addr))^;
  bcMUL_ill_u64: PUInt64(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) * PUInt64(Local(Args[1].Addr))^;
  bcMUL_ill_f32: PFloat32(Local(Args[2].Addr))^ := Float32(Args[0].Arg) * PFloat32(Local(Args[1].Addr))^;
  bcMUL_ill_f64: PFloat64(Local(Args[2].Addr))^ := Float64(Args[0].Arg) * PFloat64(Local(Args[1].Addr))^;

  bcMUL_iil_i32: PInt32(Local(Args[2].Addr))^ := Int32(Args[0].Arg) * Int32(Args[1].Arg);
  bcMUL_iil_u32: PUInt32(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) * UInt32(Args[1].Arg);
  bcMUL_iil_i64: PInt64(Local(Args[2].Addr))^ := Int64(Args[0].Arg) * Int64(Args[1].Arg);
  bcMUL_iil_u64: PUInt64(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) * UInt64(Args[1].Arg);
  bcMUL_iil_f32: PFloat32(Local(Args[2].Addr))^ := Float32(Args[0].Arg) * Float32(Args[1].Arg);
  bcMUL_iil_f64: PFloat64(Local(Args[2].Addr))^ := Float64(Args[0].Arg) * Float64(Args[1].Arg);

  // DIV opcodes
  bcDIV_lll_i32: PInt32(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ div PInt32(Local(Args[1].Addr))^;
  bcDIV_lll_u32: PUInt32(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ div PUInt32(Local(Args[1].Addr))^;
  bcDIV_lll_i64: PInt64(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ div PInt64(Local(Args[1].Addr))^;
  bcDIV_lll_u64: PUInt64(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ div PUInt64(Local(Args[1].Addr))^;
  bcDIV_lll_f32: PFloat32(Local(Args[2].Addr))^ := PFloat32(Local(Args[0].Addr))^ / PFloat32(Local(Args[1].Addr))^;
  bcDIV_lll_f64: PFloat64(Local(Args[2].Addr))^ := PFloat64(Local(Args[0].Addr))^ / PFloat64(Local(Args[1].Addr))^;

  bcDIV_lil_i32: PInt32(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ div Int32(Args[1].Arg);
  bcDIV_lil_u32: PUInt32(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ div UInt32(Args[1].Arg);
  bcDIV_lil_i64: PInt64(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ div Int64(Args[1].Arg);
  bcDIV_lil_u64: PUInt64(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ div UInt64(Args[1].Arg);
  bcDIV_lil_f32: PFloat32(Local(Args[2].Addr))^ := PFloat32(Local(Args[0].Addr))^ / Float32(Args[1].Arg);
  bcDIV_lil_f64: PFloat64(Local(Args[2].Addr))^ := PFloat64(Local(Args[0].Addr))^ / Float64(Args[1].Arg);

  bcDIV_ill_i32: PInt32(Local(Args[2].Addr))^ := Int32(Args[0].Arg) div PInt32(Local(Args[1].Addr))^;
  bcDIV_ill_u32: PUInt32(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) div PUInt32(Local(Args[1].Addr))^;
  bcDIV_ill_i64: PInt64(Local(Args[2].Addr))^ := Int64(Args[0].Arg) div PInt64(Local(Args[1].Addr))^;
  bcDIV_ill_u64: PUInt64(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) div PUInt64(Local(Args[1].Addr))^;
  bcDIV_ill_f32: PFloat32(Local(Args[2].Addr))^ := Float32(Args[0].Arg) / PFloat32(Local(Args[1].Addr))^;
  bcDIV_ill_f64: PFloat64(Local(Args[2].Addr))^ := Float64(Args[0].Arg) / PFloat64(Local(Args[1].Addr))^;

  bcDIV_iil_i32: PInt32(Local(Args[2].Addr))^ := Int32(Args[0].Arg) div Int32(Args[1].Arg);
  bcDIV_iil_u32: PUInt32(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) div UInt32(Args[1].Arg);
  bcDIV_iil_i64: PInt64(Local(Args[2].Addr))^ := Int64(Args[0].Arg) div Int64(Args[1].Arg);
  bcDIV_iil_u64: PUInt64(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) div UInt64(Args[1].Arg);
  bcDIV_iil_f32: PFloat32(Local(Args[2].Addr))^ := Float32(Args[0].Arg) / Float32(Args[1].Arg);
  bcDIV_iil_f64: PFloat64(Local(Args[2].Addr))^ := Float64(Args[0].Arg) / Float64(Args[1].Arg);

  // --- MOD opcodes (Modulo) ---
  // lll (local, local, local)
  bcMOD_lll_i32: PInt32(Local(Args[2].Addr))^ := Modulo(PInt32(Local(Args[0].Addr))^, PInt32(Local(Args[1].Addr))^);
  bcMOD_lll_u32: PUInt32(Local(Args[2].Addr))^ := Modulo(PUInt32(Local(Args[0].Addr))^, PUInt32(Local(Args[1].Addr))^);
  bcMOD_lll_i64: PInt64(Local(Args[2].Addr))^ := Modulo(PInt64(Local(Args[0].Addr))^, PInt64(Local(Args[1].Addr))^);
  bcMOD_lll_u64: PUInt64(Local(Args[2].Addr))^ := Modulo(PUInt64(Local(Args[0].Addr))^, PUInt64(Local(Args[1].Addr))^);
  bcMOD_lll_f32: PFloat32(Local(Args[2].Addr))^ := Modulo(PFloat32(Local(Args[0].Addr))^, PFloat32(Local(Args[1].Addr))^);
  bcMOD_lll_f64: PFloat64(Local(Args[2].Addr))^ := Modulo(PFloat64(Local(Args[0].Addr))^, PFloat64(Local(Args[1].Addr))^);

  // lil (local, immediate, local)
  bcMOD_lil_i32: PInt32(Local(Args[2].Addr))^ := Modulo(PInt32(Local(Args[0].Addr))^, Int32(Args[1].Arg));
  bcMOD_lil_u32: PUInt32(Local(Args[2].Addr))^ := Modulo(PUInt32(Local(Args[0].Addr))^, UInt32(Args[1].Arg));
  bcMOD_lil_i64: PInt64(Local(Args[2].Addr))^ := Modulo(PInt64(Local(Args[0].Addr))^, Int64(Args[1].Arg));
  bcMOD_lil_u64: PUInt64(Local(Args[2].Addr))^ := Modulo(PUInt64(Local(Args[0].Addr))^, UInt64(Args[1].Arg));
  bcMOD_lil_f32: PFloat32(Local(Args[2].Addr))^ := Modulo(PFloat32(Local(Args[0].Addr))^, Float32(Args[1].Arg));
  bcMOD_lil_f64: PFloat64(Local(Args[2].Addr))^ := Modulo(PFloat64(Local(Args[0].Addr))^, Float64(Args[1].Arg));

  // ill (immediate, local, local)
  bcMOD_ill_i32: PInt32(Local(Args[2].Addr))^ := Modulo(Int32(Args[0].Arg), PInt32(Local(Args[1].Addr))^);
  bcMOD_ill_u32: PUInt32(Local(Args[2].Addr))^ := Modulo(UInt32(Args[0].Arg), PUInt32(Local(Args[1].Addr))^);
  bcMOD_ill_i64: PInt64(Local(Args[2].Addr))^ := Modulo(Int64(Args[0].Arg), PInt64(Local(Args[1].Addr))^);
  bcMOD_ill_u64: PUInt64(Local(Args[2].Addr))^ := Modulo(UInt64(Args[0].Arg), PUInt64(Local(Args[1].Addr))^);
  bcMOD_ill_f32: PFloat32(Local(Args[2].Addr))^ := Modulo(Float32(Args[0].Arg), PFloat32(Local(Args[1].Addr))^);
  bcMOD_ill_f64: PFloat64(Local(Args[2].Addr))^ := Modulo(Float64(Args[0].Arg), PFloat64(Local(Args[1].Addr))^);

  // iil (immediate, immediate, local)
  bcMOD_iil_i32: PInt32(Local(Args[2].Addr))^ := Modulo(Int32(Args[0].Arg), Int32(Args[1].Arg));
  bcMOD_iil_u32: PUInt32(Local(Args[2].Addr))^ := Modulo(UInt32(Args[0].Arg), UInt32(Args[1].Arg));
  bcMOD_iil_i64: PInt64(Local(Args[2].Addr))^ := Modulo(Int64(Args[0].Arg), Int64(Args[1].Arg));
  bcMOD_iil_u64: PUInt64(Local(Args[2].Addr))^ := Modulo(UInt64(Args[0].Arg), UInt64(Args[1].Arg));
  bcMOD_iil_f32: PFloat32(Local(Args[2].Addr))^ := Modulo(Float32(Args[0].Arg), Float32(Args[1].Arg));
  bcMOD_iil_f64: PFloat64(Local(Args[2].Addr))^ := Modulo(Float64(Args[0].Arg), Float64(Args[1].Arg));

  // POW opcodes (Assuming a Power function is available)
  bcPOW_lll_i32: PInt32(Local(Args[2].Addr))^ := Power(PInt32(Local(Args[0].Addr))^, PInt32(Local(Args[1].Addr))^);
  bcPOW_lll_u32: PUInt32(Local(Args[2].Addr))^ := Power(PUInt32(Local(Args[0].Addr))^, PUInt32(Local(Args[1].Addr))^);
  bcPOW_lll_i64: PInt64(Local(Args[2].Addr))^ := Power(PInt64(Local(Args[0].Addr))^, PInt64(Local(Args[1].Addr))^);
  bcPOW_lll_u64: PUInt64(Local(Args[2].Addr))^ := Power(PUInt64(Local(Args[0].Addr))^, PUInt64(Local(Args[1].Addr))^);
  bcPOW_lll_f32: PFloat32(Local(Args[2].Addr))^ := Power(PFloat32(Local(Args[0].Addr))^, PFloat32(Local(Args[1].Addr))^);
  bcPOW_lll_f64: PFloat64(Local(Args[2].Addr))^ := Power(PFloat64(Local(Args[0].Addr))^, PFloat64(Local(Args[1].Addr))^);

  bcPOW_lil_i32: PInt32(Local(Args[2].Addr))^ := Power(PInt32(Local(Args[0].Addr))^, Int32(Args[1].Arg));
  bcPOW_lil_u32: PUInt32(Local(Args[2].Addr))^ := Power(PUInt32(Local(Args[0].Addr))^, UInt32(Args[1].Arg));
  bcPOW_lil_i64: PInt64(Local(Args[2].Addr))^ := Power(PInt64(Local(Args[0].Addr))^, Int64(Args[1].Arg));
  bcPOW_lil_u64: PUInt64(Local(Args[2].Addr))^ := Power(PUInt64(Local(Args[0].Addr))^, UInt64(Args[1].Arg));
  bcPOW_lil_f32: PFloat32(Local(Args[2].Addr))^ := Power(PFloat32(Local(Args[0].Addr))^, Float32(Args[1].Arg));
  bcPOW_lil_f64: PFloat64(Local(Args[2].Addr))^ := Power(PFloat64(Local(Args[0].Addr))^, Float64(Args[1].Arg));

  bcPOW_ill_i32: PInt32(Local(Args[2].Addr))^ := Power(Int32(Args[0].Arg), PInt32(Local(Args[1].Addr))^);
  bcPOW_ill_u32: PUInt32(Local(Args[2].Addr))^ := Power(UInt32(Args[0].Arg), PUInt32(Local(Args[1].Addr))^);
  bcPOW_ill_i64: PInt64(Local(Args[2].Addr))^ := Power(Int64(Args[0].Arg), PInt64(Local(Args[1].Addr))^);
  bcPOW_ill_u64: PUInt64(Local(Args[2].Addr))^ := Power(UInt64(Args[0].Arg), PUInt64(Local(Args[1].Addr))^);
  bcPOW_ill_f32: PFloat32(Local(Args[2].Addr))^ := Power(Float32(Args[0].Arg), PFloat32(Local(Args[1].Addr))^);
  bcPOW_ill_f64: PFloat64(Local(Args[2].Addr))^ := Power(Float64(Args[0].Arg), PFloat64(Local(Args[1].Addr))^);

  bcPOW_iil_i32: PInt32(Local(Args[2].Addr))^ := Power(Int32(Args[0].Arg), Int32(Args[1].Arg));
  bcPOW_iil_u32: PUInt32(Local(Args[2].Addr))^ := Power(UInt32(Args[0].Arg), UInt32(Args[1].Arg));
  bcPOW_iil_i64: PInt64(Local(Args[2].Addr))^ := Power(Int64(Args[0].Arg), Int64(Args[1].Arg));
  bcPOW_iil_u64: PUInt64(Local(Args[2].Addr))^ := Power(UInt64(Args[0].Arg), UInt64(Args[1].Arg));
  bcPOW_iil_f32: PFloat32(Local(Args[2].Addr))^ := Power(Float32(Args[0].Arg), Float32(Args[1].Arg));
  bcPOW_iil_f64: PFloat64(Local(Args[2].Addr))^ := Power(Float64(Args[0].Arg), Float64(Args[1].Arg));

  // NE opcodes
  bcNE_lll_i32: PBoolean(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ <> PInt32(Local(Args[1].Addr))^;
  bcNE_lll_u32: PBoolean(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ <> PUInt32(Local(Args[1].Addr))^;
  bcNE_lll_i64: PBoolean(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ <> PInt64(Local(Args[1].Addr))^;
  bcNE_lll_u64: PBoolean(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ <> PUInt64(Local(Args[1].Addr))^;
  bcNE_lll_f32: PBoolean(Local(Args[2].Addr))^ := PFloat32(Local(Args[0].Addr))^ <> PFloat32(Local(Args[1].Addr))^;
  bcNE_lll_f64: PBoolean(Local(Args[2].Addr))^ := PFloat64(Local(Args[0].Addr))^ <> PFloat64(Local(Args[1].Addr))^;

  bcNE_lil_i32: PBoolean(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ <> Int32(Args[1].Arg);
  bcNE_lil_u32: PBoolean(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ <> UInt32(Args[1].Arg);
  bcNE_lil_i64: PBoolean(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ <> Int64(Args[1].Arg);
  bcNE_lil_u64: PBoolean(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ <> UInt64(Args[1].Arg);
  bcNE_lil_f32: PBoolean(Local(Args[2].Addr))^ := PFloat32(Local(Args[0].Addr))^ <> Float32(Args[1].Arg);
  bcNE_lil_f64: PBoolean(Local(Args[2].Addr))^ := PFloat64(Local(Args[0].Addr))^ <> Float64(Args[1].Arg);

  bcNE_ill_i32: PBoolean(Local(Args[2].Addr))^ := Int32(Args[0].Arg) <> PInt32(Local(Args[1].Addr))^;
  bcNE_ill_u32: PBoolean(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) <> PUInt32(Local(Args[1].Addr))^;
  bcNE_ill_i64: PBoolean(Local(Args[2].Addr))^ := Int64(Args[0].Arg) <> PInt64(Local(Args[1].Addr))^;
  bcNE_ill_u64: PBoolean(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) <> PUInt64(Local(Args[1].Addr))^;
  bcNE_ill_f32: PBoolean(Local(Args[2].Addr))^ := Float32(Args[0].Arg) <> PFloat32(Local(Args[1].Addr))^;
  bcNE_ill_f64: PBoolean(Local(Args[2].Addr))^ := Float64(Args[0].Arg) <> PFloat64(Local(Args[1].Addr))^;

  bcNE_iil_i32: PBoolean(Local(Args[2].Addr))^ := Int32(Args[0].Arg) <> Int32(Args[1].Arg);
  bcNE_iil_u32: PBoolean(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) <> UInt32(Args[1].Arg);
  bcNE_iil_i64: PBoolean(Local(Args[2].Addr))^ := Int64(Args[0].Arg) <> Int64(Args[1].Arg);
  bcNE_iil_u64: PBoolean(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) <> UInt64(Args[1].Arg);
  bcNE_iil_f32: PBoolean(Local(Args[2].Addr))^ := Float32(Args[0].Arg) <> Float32(Args[1].Arg);
  bcNE_iil_f64: PBoolean(Local(Args[2].Addr))^ := Float64(Args[0].Arg) <> Float64(Args[1].Arg);

  // EQ opcodes
  bcEQ_lll_i32: PBoolean(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ = PInt32(Local(Args[1].Addr))^;
  bcEQ_lll_u32: PBoolean(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ = PUInt32(Local(Args[1].Addr))^;
  bcEQ_lll_i64: PBoolean(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ = PInt64(Local(Args[1].Addr))^;
  bcEQ_lll_u64: PBoolean(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ = PUInt64(Local(Args[1].Addr))^;
  bcEQ_lll_f32: PBoolean(Local(Args[2].Addr))^ := PFloat32(Local(Args[0].Addr))^ = PFloat32(Local(Args[1].Addr))^;
  bcEQ_lll_f64: PBoolean(Local(Args[2].Addr))^ := PFloat64(Local(Args[0].Addr))^ = PFloat64(Local(Args[1].Addr))^;

  bcEQ_lil_i32: PBoolean(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ = Int32(Args[1].Arg);
  bcEQ_lil_u32: PBoolean(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ = UInt32(Args[1].Arg);
  bcEQ_lil_i64: PBoolean(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ = Int64(Args[1].Arg);
  bcEQ_lil_u64: PBoolean(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ = UInt64(Args[1].Arg);
  bcEQ_lil_f32: PBoolean(Local(Args[2].Addr))^ := PFloat32(Local(Args[0].Addr))^ = Float32(Args[1].Arg);
  bcEQ_lil_f64: PBoolean(Local(Args[2].Addr))^ := PFloat64(Local(Args[0].Addr))^ = Float64(Args[1].Arg);

  bcEQ_ill_i32: PBoolean(Local(Args[2].Addr))^ := Int32(Args[0].Arg) = PInt32(Local(Args[1].Addr))^;
  bcEQ_ill_u32: PBoolean(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) = PUInt32(Local(Args[1].Addr))^;
  bcEQ_ill_i64: PBoolean(Local(Args[2].Addr))^ := Int64(Args[0].Arg) = PInt64(Local(Args[1].Addr))^;
  bcEQ_ill_u64: PBoolean(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) = PUInt64(Local(Args[1].Addr))^;
  bcEQ_ill_f32: PBoolean(Local(Args[2].Addr))^ := Float32(Args[0].Arg) = PFloat32(Local(Args[1].Addr))^;
  bcEQ_ill_f64: PBoolean(Local(Args[2].Addr))^ := Float64(Args[0].Arg) = PFloat64(Local(Args[1].Addr))^;

  bcEQ_iil_i32: PBoolean(Local(Args[2].Addr))^ := Int32(Args[0].Arg) = Int32(Args[1].Arg);
  bcEQ_iil_u32: PBoolean(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) = UInt32(Args[1].Arg);
  bcEQ_iil_i64: PBoolean(Local(Args[2].Addr))^ := Int64(Args[0].Arg) = Int64(Args[1].Arg);
  bcEQ_iil_u64: PBoolean(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) = UInt64(Args[1].Arg);
  bcEQ_iil_f32: PBoolean(Local(Args[2].Addr))^ := Float32(Args[0].Arg) = Float32(Args[1].Arg);
  bcEQ_iil_f64: PBoolean(Local(Args[2].Addr))^ := Float64(Args[0].Arg) = Float64(Args[1].Arg);

  // LT opcodes
  bcLT_lll_i32: PBoolean(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ < PInt32(Local(Args[1].Addr))^;
  bcLT_lll_u32: PBoolean(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ < PUInt32(Local(Args[1].Addr))^;
  bcLT_lll_i64: PBoolean(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ < PInt64(Local(Args[1].Addr))^;
  bcLT_lll_u64: PBoolean(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ < PUInt64(Local(Args[1].Addr))^;
  bcLT_lll_f32: PBoolean(Local(Args[2].Addr))^ := PFloat32(Local(Args[0].Addr))^ < PFloat32(Local(Args[1].Addr))^;
  bcLT_lll_f64: PBoolean(Local(Args[2].Addr))^ := PFloat64(Local(Args[0].Addr))^ < PFloat64(Local(Args[1].Addr))^;

  bcLT_lil_i32: PBoolean(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ < Int32(Args[1].Arg);
  bcLT_lil_u32: PBoolean(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ < UInt32(Args[1].Arg);
  bcLT_lil_i64: PBoolean(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ < Int64(Args[1].Arg);
  bcLT_lil_u64: PBoolean(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ < UInt64(Args[1].Arg);
  bcLT_lil_f32: PBoolean(Local(Args[2].Addr))^ := PFloat32(Local(Args[0].Addr))^ < Float32(Args[1].Arg);
  bcLT_lil_f64: PBoolean(Local(Args[2].Addr))^ := PFloat64(Local(Args[0].Addr))^ < Float64(Args[1].Arg);

  bcLT_ill_i32: PBoolean(Local(Args[2].Addr))^ := Int32(Args[0].Arg) < PInt32(Local(Args[1].Addr))^;
  bcLT_ill_u32: PBoolean(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) < PUInt32(Local(Args[1].Addr))^;
  bcLT_ill_i64: PBoolean(Local(Args[2].Addr))^ := Int64(Args[0].Arg) < PInt64(Local(Args[1].Addr))^;
  bcLT_ill_u64: PBoolean(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) < PUInt64(Local(Args[1].Addr))^;
  bcLT_ill_f32: PBoolean(Local(Args[2].Addr))^ := Float32(Args[0].Arg) < PFloat32(Local(Args[1].Addr))^;
  bcLT_ill_f64: PBoolean(Local(Args[2].Addr))^ := Float64(Args[0].Arg) < PFloat64(Local(Args[1].Addr))^;

  bcLT_iil_i32: PBoolean(Local(Args[2].Addr))^ := Int32(Args[0].Arg) < Int32(Args[1].Arg);
  bcLT_iil_u32: PBoolean(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) < UInt32(Args[1].Arg);
  bcLT_iil_i64: PBoolean(Local(Args[2].Addr))^ := Int64(Args[0].Arg) < Int64(Args[1].Arg);
  bcLT_iil_u64: PBoolean(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) < UInt64(Args[1].Arg);
  bcLT_iil_f32: PBoolean(Local(Args[2].Addr))^ := Float32(Args[0].Arg) < Float32(Args[1].Arg);
  bcLT_iil_f64: PBoolean(Local(Args[2].Addr))^ := Float64(Args[0].Arg) < Float64(Args[1].Arg);

  // GT opcodes
  bcGT_lll_i32: PBoolean(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ > PInt32(Local(Args[1].Addr))^;
  bcGT_lll_u32: PBoolean(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ > PUInt32(Local(Args[1].Addr))^;
  bcGT_lll_i64: PBoolean(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ > PInt64(Local(Args[1].Addr))^;
  bcGT_lll_u64: PBoolean(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ > PUInt64(Local(Args[1].Addr))^;
  bcGT_lll_f32: PBoolean(Local(Args[2].Addr))^ := PFloat32(Local(Args[0].Addr))^ > PFloat32(Local(Args[1].Addr))^;
  bcGT_lll_f64: PBoolean(Local(Args[2].Addr))^ := PFloat64(Local(Args[0].Addr))^ > PFloat64(Local(Args[1].Addr))^;

  bcGT_lil_i32: PBoolean(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ > Int32(Args[1].Arg);
  bcGT_lil_u32: PBoolean(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ > UInt32(Args[1].Arg);
  bcGT_lil_i64: PBoolean(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ > Int64(Args[1].Arg);
  bcGT_lil_u64: PBoolean(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ > UInt64(Args[1].Arg);
  bcGT_lil_f32: PBoolean(Local(Args[2].Addr))^ := PFloat32(Local(Args[0].Addr))^ > Float32(Args[1].Arg);
  bcGT_lil_f64: PBoolean(Local(Args[2].Addr))^ := PFloat64(Local(Args[0].Addr))^ > Float64(Args[1].Arg);

  bcGT_ill_i32: PBoolean(Local(Args[2].Addr))^ := Int32(Args[0].Arg) > PInt32(Local(Args[1].Addr))^;
  bcGT_ill_u32: PBoolean(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) > PUInt32(Local(Args[1].Addr))^;
  bcGT_ill_i64: PBoolean(Local(Args[2].Addr))^ := Int64(Args[0].Arg) > PInt64(Local(Args[1].Addr))^;
  bcGT_ill_u64: PBoolean(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) > PUInt64(Local(Args[1].Addr))^;
  bcGT_ill_f32: PBoolean(Local(Args[2].Addr))^ := Float32(Args[0].Arg) > PFloat32(Local(Args[1].Addr))^;
  bcGT_ill_f64: PBoolean(Local(Args[2].Addr))^ := Float64(Args[0].Arg) > PFloat64(Local(Args[1].Addr))^;

  bcGT_iil_i32: PBoolean(Local(Args[2].Addr))^ := Int32(Args[0].Arg) > Int32(Args[1].Arg);
  bcGT_iil_u32: PBoolean(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) > UInt32(Args[1].Arg);
  bcGT_iil_i64: PBoolean(Local(Args[2].Addr))^ := Int64(Args[0].Arg) > Int64(Args[1].Arg);
  bcGT_iil_u64: PBoolean(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) > UInt64(Args[1].Arg);
  bcGT_iil_f32: PBoolean(Local(Args[2].Addr))^ := Float32(Args[0].Arg) > Float32(Args[1].Arg);
  bcGT_iil_f64: PBoolean(Local(Args[2].Addr))^ := Float64(Args[0].Arg) > Float64(Args[1].Arg);

  // GTE opcodes
  bcGTE_lll_i32: PBoolean(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ >= PInt32(Local(Args[1].Addr))^;
  bcGTE_lll_u32: PBoolean(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ >= PUInt32(Local(Args[1].Addr))^;
  bcGTE_lll_i64: PBoolean(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ >= PInt64(Local(Args[1].Addr))^;
  bcGTE_lll_u64: PBoolean(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ >= PUInt64(Local(Args[1].Addr))^;
  bcGTE_lll_f32: PBoolean(Local(Args[2].Addr))^ := PFloat32(Local(Args[0].Addr))^ >= PFloat32(Local(Args[1].Addr))^;
  bcGTE_lll_f64: PBoolean(Local(Args[2].Addr))^ := PFloat64(Local(Args[0].Addr))^ >= PFloat64(Local(Args[1].Addr))^;

  bcGTE_lil_i32: PBoolean(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ >= Int32(Args[1].Arg);
  bcGTE_lil_u32: PBoolean(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ >= UInt32(Args[1].Arg);
  bcGTE_lil_i64: PBoolean(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ >= Int64(Args[1].Arg);
  bcGTE_lil_u64: PBoolean(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ >= UInt64(Args[1].Arg);
  bcGTE_lil_f32: PBoolean(Local(Args[2].Addr))^ := PFloat32(Local(Args[0].Addr))^ >= Float32(Args[1].Arg);
  bcGTE_lil_f64: PBoolean(Local(Args[2].Addr))^ := PFloat64(Local(Args[0].Addr))^ >= Float64(Args[1].Arg);

  bcGTE_ill_i32: PBoolean(Local(Args[2].Addr))^ := Int32(Args[0].Arg) >= PInt32(Local(Args[1].Addr))^;
  bcGTE_ill_u32: PBoolean(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) >= PUInt32(Local(Args[1].Addr))^;
  bcGTE_ill_i64: PBoolean(Local(Args[2].Addr))^ := Int64(Args[0].Arg) >= PInt64(Local(Args[1].Addr))^;
  bcGTE_ill_u64: PBoolean(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) >= PUInt64(Local(Args[1].Addr))^;
  bcGTE_ill_f32: PBoolean(Local(Args[2].Addr))^ := Float32(Args[0].Arg) >= PFloat32(Local(Args[1].Addr))^;
  bcGTE_ill_f64: PBoolean(Local(Args[2].Addr))^ := Float64(Args[0].Arg) >= PFloat64(Local(Args[1].Addr))^;

  bcGTE_iil_i32: PBoolean(Local(Args[2].Addr))^ := Int32(Args[0].Arg) >= Int32(Args[1].Arg);
  bcGTE_iil_u32: PBoolean(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) >= UInt32(Args[1].Arg);
  bcGTE_iil_i64: PBoolean(Local(Args[2].Addr))^ := Int64(Args[0].Arg) >= Int64(Args[1].Arg);
  bcGTE_iil_u64: PBoolean(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) >= UInt64(Args[1].Arg);
  bcGTE_iil_f32: PBoolean(Local(Args[2].Addr))^ := Float32(Args[0].Arg) >= Float32(Args[1].Arg);
  bcGTE_iil_f64: PBoolean(Local(Args[2].Addr))^ := Float64(Args[0].Arg) >= Float64(Args[1].Arg);

  // LTE opcodes
  bcLTE_lll_i32: PBoolean(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ <= PInt32(Local(Args[1].Addr))^;
  bcLTE_lll_u32: PBoolean(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ <= PUInt32(Local(Args[1].Addr))^;
  bcLTE_lll_i64: PBoolean(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ <= PInt64(Local(Args[1].Addr))^;
  bcLTE_lll_u64: PBoolean(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ <= PUInt64(Local(Args[1].Addr))^;
  bcLTE_lll_f32: PBoolean(Local(Args[2].Addr))^ := PFloat32(Local(Args[0].Addr))^ <= PFloat32(Local(Args[1].Addr))^;
  bcLTE_lll_f64: PBoolean(Local(Args[2].Addr))^ := PFloat64(Local(Args[0].Addr))^ <= PFloat64(Local(Args[1].Addr))^;

  bcLTE_lil_i32: PBoolean(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ <= Int32(Args[1].Arg);
  bcLTE_lil_u32: PBoolean(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ <= UInt32(Args[1].Arg);
  bcLTE_lil_i64: PBoolean(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ <= Int64(Args[1].Arg);
  bcLTE_lil_u64: PBoolean(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ <= UInt64(Args[1].Arg);
  bcLTE_lil_f32: PBoolean(Local(Args[2].Addr))^ := PFloat32(Local(Args[0].Addr))^ <= Float32(Args[1].Arg);
  bcLTE_lil_f64: PBoolean(Local(Args[2].Addr))^ := PFloat64(Local(Args[0].Addr))^ <= Float64(Args[1].Arg);

  bcLTE_ill_i32: PBoolean(Local(Args[2].Addr))^ := Int32(Args[0].Arg) <= PInt32(Local(Args[1].Addr))^;
  bcLTE_ill_u32: PBoolean(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) <= PUInt32(Local(Args[1].Addr))^;
  bcLTE_ill_i64: PBoolean(Local(Args[2].Addr))^ := Int64(Args[0].Arg) <= PInt64(Local(Args[1].Addr))^;
  bcLTE_ill_u64: PBoolean(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) <= PUInt64(Local(Args[1].Addr))^;
  bcLTE_ill_f32: PBoolean(Local(Args[2].Addr))^ := Float32(Args[0].Arg) <= PFloat32(Local(Args[1].Addr))^;
  bcLTE_ill_f64: PBoolean(Local(Args[2].Addr))^ := Float64(Args[0].Arg) <= PFloat64(Local(Args[1].Addr))^;

  bcLTE_iil_i32: PBoolean(Local(Args[2].Addr))^ := Int32(Args[0].Arg) <= Int32(Args[1].Arg);
  bcLTE_iil_u32: PBoolean(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) <= UInt32(Args[1].Arg);
  bcLTE_iil_i64: PBoolean(Local(Args[2].Addr))^ := Int64(Args[0].Arg) <= Int64(Args[1].Arg);
  bcLTE_iil_u64: PBoolean(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) <= UInt64(Args[1].Arg);
  bcLTE_iil_f32: PBoolean(Local(Args[2].Addr))^ := Float32(Args[0].Arg) <= Float32(Args[1].Arg);
  bcLTE_iil_f64: PBoolean(Local(Args[2].Addr))^ := Float64(Args[0].Arg) <= Float64(Args[1].Arg);

  // BND opcodes (Bitwise AND)
  bcBND_lll_i32: PInt32(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ and PInt32(Local(Args[1].Addr))^;
  bcBND_lll_u32: PUInt32(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ and PUInt32(Local(Args[1].Addr))^;
  bcBND_lll_i64: PInt64(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ and PInt64(Local(Args[1].Addr))^;
  bcBND_lll_u64: PUInt64(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ and PUInt64(Local(Args[1].Addr))^;

  bcBND_lil_i32: PInt32(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ and Int32(Args[1].Arg);
  bcBND_lil_u32: PUInt32(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ and UInt32(Args[1].Arg);
  bcBND_lil_i64: PInt64(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ and Int64(Args[1].Arg);
  bcBND_lil_u64: PUInt64(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ and UInt64(Args[1].Arg);

  bcBND_ill_i32: PInt32(Local(Args[2].Addr))^ := Int32(Args[0].Arg) and PInt32(Local(Args[1].Addr))^;
  bcBND_ill_u32: PUInt32(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) and PUInt32(Local(Args[1].Addr))^;
  bcBND_ill_i64: PInt64(Local(Args[2].Addr))^ := Int64(Args[0].Arg) and PInt64(Local(Args[1].Addr))^;
  bcBND_ill_u64: PUInt64(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) and PUInt64(Local(Args[1].Addr))^;

  bcBND_iil_i32: PInt32(Local(Args[2].Addr))^ := Int32(Args[0].Arg) and Int32(Args[1].Arg);
  bcBND_iil_u32: PUInt32(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) and UInt32(Args[1].Arg);
  bcBND_iil_i64: PInt64(Local(Args[2].Addr))^ := Int64(Args[0].Arg) and Int64(Args[1].Arg);
  bcBND_iil_u64: PUInt64(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) and UInt64(Args[1].Arg);

  // SHL opcodes (Shift Left)
  bcSHL_lll_i32: PInt32(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ shl PInt32(Local(Args[1].Addr))^;
  bcSHL_lll_u32: PUInt32(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ shl PUInt32(Local(Args[1].Addr))^;
  bcSHL_lll_i64: PInt64(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ shl PInt64(Local(Args[1].Addr))^;
  bcSHL_lll_u64: PUInt64(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ shl PUInt64(Local(Args[1].Addr))^;

  bcSHL_lil_i32: PInt32(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ shl Int32(Args[1].Arg);
  bcSHL_lil_u32: PUInt32(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ shl UInt32(Args[1].Arg);
  bcSHL_lil_i64: PInt64(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ shl Int64(Args[1].Arg);
  bcSHL_lil_u64: PUInt64(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ shl UInt64(Args[1].Arg);

  bcSHL_ill_i32: PInt32(Local(Args[2].Addr))^ := Int32(Args[0].Arg) shl PInt32(Local(Args[1].Addr))^;
  bcSHL_ill_u32: PUInt32(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) shl PUInt32(Local(Args[1].Addr))^;
  bcSHL_ill_i64: PInt64(Local(Args[2].Addr))^ := Int64(Args[0].Arg) shl PInt64(Local(Args[1].Addr))^;
  bcSHL_ill_u64: PUInt64(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) shl PUInt64(Local(Args[1].Addr))^;

  bcSHL_iil_i32: PInt32(Local(Args[2].Addr))^ := Int32(Args[0].Arg) shl Int32(Args[1].Arg);
  bcSHL_iil_u32: PUInt32(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) shl UInt32(Args[1].Arg);
  bcSHL_iil_i64: PInt64(Local(Args[2].Addr))^ := Int64(Args[0].Arg) shl Int64(Args[1].Arg);
  bcSHL_iil_u64: PUInt64(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) shl UInt64(Args[1].Arg);

  // SHR opcodes (Shift Right)
  bcSHR_lll_i32: PInt32(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ shr PInt32(Local(Args[1].Addr))^;
  bcSHR_lll_u32: PUInt32(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ shr PUInt32(Local(Args[1].Addr))^;
  bcSHR_lll_i64: PInt64(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ shr PInt64(Local(Args[1].Addr))^;
  bcSHR_lll_u64: PUInt64(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ shr PUInt64(Local(Args[1].Addr))^;

  bcSHR_lil_i32: PInt32(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ shr Int32(Args[1].Arg);
  bcSHR_lil_u32: PUInt32(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ shr UInt32(Args[1].Arg);
  bcSHR_lil_i64: PInt64(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ shr Int64(Args[1].Arg);
  bcSHR_lil_u64: PUInt64(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ shr UInt64(Args[1].Arg);

  bcSHR_ill_i32: PInt32(Local(Args[2].Addr))^ := Int32(Args[0].Arg) shr PInt32(Local(Args[1].Addr))^;
  bcSHR_ill_u32: PUInt32(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) shr PUInt32(Local(Args[1].Addr))^;
  bcSHR_ill_i64: PInt64(Local(Args[2].Addr))^ := Int64(Args[0].Arg) shr PInt64(Local(Args[1].Addr))^;
  bcSHR_ill_u64: PUInt64(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) shr PUInt64(Local(Args[1].Addr))^;

  bcSHR_iil_i32: PInt32(Local(Args[2].Addr))^ := Int32(Args[0].Arg) shr Int32(Args[1].Arg);
  bcSHR_iil_u32: PUInt32(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) shr UInt32(Args[1].Arg);
  bcSHR_iil_i64: PInt64(Local(Args[2].Addr))^ := Int64(Args[0].Arg) shr Int64(Args[1].Arg);
  bcSHR_iil_u64: PUInt64(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) shr UInt64(Args[1].Arg);

  // XOR opcodes (Bitwise XOR)
  bcXOR_lll_i32: PInt32(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ xor PInt32(Local(Args[1].Addr))^;
  bcXOR_lll_u32: PUInt32(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ xor PUInt32(Local(Args[1].Addr))^;
  bcXOR_lll_i64: PInt64(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ xor PInt64(Local(Args[1].Addr))^;
  bcXOR_lll_u64: PUInt64(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ xor PUInt64(Local(Args[1].Addr))^;

  bcXOR_lil_i32: PInt32(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ xor Int32(Args[1].Arg);
  bcXOR_lil_u32: PUInt32(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ xor UInt32(Args[1].Arg);
  bcXOR_lil_i64: PInt64(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ xor Int64(Args[1].Arg);
  bcXOR_lil_u64: PUInt64(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ xor UInt64(Args[1].Arg);

  bcXOR_ill_i32: PInt32(Local(Args[2].Addr))^ := Int32(Args[0].Arg) xor PInt32(Local(Args[1].Addr))^;
  bcXOR_ill_u32: PUInt32(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) xor PUInt32(Local(Args[1].Addr))^;
  bcXOR_ill_i64: PInt64(Local(Args[2].Addr))^ := Int64(Args[0].Arg) xor PInt64(Local(Args[1].Addr))^;
  bcXOR_ill_u64: PUInt64(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) xor PUInt64(Local(Args[1].Addr))^;

  bcXOR_iil_i32: PInt32(Local(Args[2].Addr))^ := Int32(Args[0].Arg) xor Int32(Args[1].Arg);
  bcXOR_iil_u32: PUInt32(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) xor UInt32(Args[1].Arg);
  bcXOR_iil_i64: PInt64(Local(Args[2].Addr))^ := Int64(Args[0].Arg) xor Int64(Args[1].Arg);
  bcXOR_iil_u64: PUInt64(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) xor UInt64(Args[1].Arg);

  // BOR opcodes (Bitwise OR)
  bcBOR_lll_i32: PInt32(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ or PInt32(Local(Args[1].Addr))^;
  bcBOR_lll_u32: PUInt32(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ or PUInt32(Local(Args[1].Addr))^;
  bcBOR_lll_i64: PInt64(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ or PInt64(Local(Args[1].Addr))^;
  bcBOR_lll_u64: PUInt64(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ or PUInt64(Local(Args[1].Addr))^;

  bcBOR_lil_i32: PInt32(Local(Args[2].Addr))^ := PInt32(Local(Args[0].Addr))^ or Int32(Args[1].Arg);
  bcBOR_lil_u32: PUInt32(Local(Args[2].Addr))^ := PUInt32(Local(Args[0].Addr))^ or UInt32(Args[1].Arg);
  bcBOR_lil_i64: PInt64(Local(Args[2].Addr))^ := PInt64(Local(Args[0].Addr))^ or Int64(Args[1].Arg);
  bcBOR_lil_u64: PUInt64(Local(Args[2].Addr))^ := PUInt64(Local(Args[0].Addr))^ or UInt64(Args[1].Arg);

  bcBOR_ill_i32: PInt32(Local(Args[2].Addr))^ := Int32(Args[0].Arg) or PInt32(Local(Args[1].Addr))^;
  bcBOR_ill_u32: PUInt32(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) or PUInt32(Local(Args[1].Addr))^;
  bcBOR_ill_i64: PInt64(Local(Args[2].Addr))^ := Int64(Args[0].Arg) or PInt64(Local(Args[1].Addr))^;
  bcBOR_ill_u64: PUInt64(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) or PUInt64(Local(Args[1].Addr))^;

  bcBOR_iil_i32: PInt32(Local(Args[2].Addr))^ := Int32(Args[0].Arg) or Int32(Args[1].Arg);
  bcBOR_iil_u32: PUInt32(Local(Args[2].Addr))^ := UInt32(Args[0].Arg) or UInt32(Args[1].Arg);
  bcBOR_iil_i64: PInt64(Local(Args[2].Addr))^ := Int64(Args[0].Arg) or Int64(Args[1].Arg);
  bcBOR_iil_u64: PUInt64(Local(Args[2].Addr))^ := UInt64(Args[0].Arg) or UInt64(Args[1].Arg);

  // --- SAR opcodes (Shift Arithmetic Right) ---
  // lll (local, local, local)
  bcSAR_lll_i32: PInt32(Local(Args[2].Addr))^ := Sar(PInt32(Local(Args[0].Addr))^, PInt32(Local(Args[1].Addr))^);
  bcSAR_lll_u32: PUInt32(Local(Args[2].Addr))^ := Sar(PUInt32(Local(Args[0].Addr))^, PUInt32(Local(Args[1].Addr))^);
  bcSAR_lll_i64: PInt64(Local(Args[2].Addr))^ := Sar(PInt64(Local(Args[0].Addr))^, PInt64(Local(Args[1].Addr))^);
  bcSAR_lll_u64: PUInt64(Local(Args[2].Addr))^ := Sar(PUInt64(Local(Args[0].Addr))^, PUInt64(Local(Args[1].Addr))^);

  // lil (local, immediate, local)
  bcSAR_lil_i32: PInt32(Local(Args[2].Addr))^ := Sar(PInt32(Local(Args[0].Addr))^, Int32(Args[1].Arg));
  bcSAR_lil_u32: PUInt32(Local(Args[2].Addr))^ := Sar(PUInt32(Local(Args[0].Addr))^, UInt32(Args[1].Arg));
  bcSAR_lil_i64: PInt64(Local(Args[2].Addr))^ := Sar(PInt64(Local(Args[0].Addr))^, Int64(Args[1].Arg));
  bcSAR_lil_u64: PUInt64(Local(Args[2].Addr))^ := Sar(PUInt64(Local(Args[0].Addr))^, UInt64(Args[1].Arg));

  // ill (immediate, local, local)
  bcSAR_ill_i32: PInt32(Local(Args[2].Addr))^ := Sar(Int32(Args[0].Arg), PInt32(Local(Args[1].Addr))^);
  bcSAR_ill_u32: PUInt32(Local(Args[2].Addr))^ := Sar(UInt32(Args[0].Arg), PUInt32(Local(Args[1].Addr))^);
  bcSAR_ill_i64: PInt64(Local(Args[2].Addr))^ := Sar(Int64(Args[0].Arg), PInt64(Local(Args[1].Addr))^);
  bcSAR_ill_u64: PUInt64(Local(Args[2].Addr))^ := Sar(UInt64(Args[0].Arg), PUInt64(Local(Args[1].Addr))^);

  // iil (immediate, immediate, local)
  bcSAR_iil_i32: PInt32(Local(Args[2].Addr))^ := Sar(Int32(Args[0].Arg), Int32(Args[1].Arg));
  bcSAR_iil_u32: PUInt32(Local(Args[2].Addr))^ := Sar(UInt32(Args[0].Arg), UInt32(Args[1].Arg));
  bcSAR_iil_i64: PInt64(Local(Args[2].Addr))^ := Sar(Int64(Args[0].Arg), Int64(Args[1].Arg));
  bcSAR_iil_u64: PUInt64(Local(Args[2].Addr))^ := Sar(UInt64(Args[0].Arg), UInt64(Args[1].Arg));
