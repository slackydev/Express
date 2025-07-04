
  // slow path opcodes:
  
  bcADD, bcSUB, bcMUL, bcDIV, bcMOD, bcBND, bcBOR, bcXOR,
  bcEQ, bcNE, bcGT, bcLT, bcGTE, bcLTE:
    begin
      case Ord(Args[0].Typ) shl 6 + Ord(Args[0].Pos) of
        Ord(xtInt8) shl 6 + Ord(mpLocal):  left_i64 := PInt8(Stack.Local(Args[0].Addr))^;
        Ord(xtInt16) shl 6 + Ord(mpLocal): left_i64 := PInt16(Stack.Local(Args[0].Addr))^;
        Ord(xtInt32) shl 6 + Ord(mpLocal): left_i64 := PInt32(Stack.Local(Args[0].Addr))^;
        Ord(xtInt64) shl 6 + Ord(mpLocal): left_i64 := PInt64(Stack.Local(Args[0].Addr))^;
        Ord(xtUInt8) shl 6 + Ord(mpLocal): left_i64 := PUInt8(Stack.Local(Args[0].Addr))^;
        Ord(xtUInt16) shl 6 + Ord(mpLocal):left_i64 := PUInt16(Stack.Local(Args[0].Addr))^;
        Ord(xtUInt32) shl 6 + Ord(mpLocal):left_i64 := PUInt32(Stack.Local(Args[0].Addr))^;
        Ord(xtUInt64) shl 6 + Ord(mpLocal):left_i64 := PUInt64(Stack.Local(Args[0].Addr))^;

        Ord(xtInt8) shl 6 + Ord(mpGlobal):  left_i64 := PInt8(Stack.Global(Args[0].Addr))^;
        Ord(xtInt16) shl 6 + Ord(mpGlobal): left_i64 := PInt16(Stack.Global(Args[0].Addr))^;
        Ord(xtInt32) shl 6 + Ord(mpGlobal): left_i64 := PInt32(Stack.Global(Args[0].Addr))^;
        Ord(xtInt64) shl 6 + Ord(mpGlobal): left_i64 := PInt64(Stack.Global(Args[0].Addr))^;
        Ord(xtUInt8) shl 6 + Ord(mpGlobal): left_i64 := PUInt8(Stack.Global(Args[0].Addr))^;
        Ord(xtUInt16) shl 6 + Ord(mpGlobal):left_i64 := PUInt16(Stack.Global(Args[0].Addr))^;
        Ord(xtUInt32) shl 6 + Ord(mpGlobal):left_i64 := PUInt32(Stack.Global(Args[0].Addr))^;
        Ord(xtUInt64) shl 6 + Ord(mpGlobal):left_i64 := PUInt64(Stack.Global(Args[0].Addr))^;

        else left_i64 := Args[0].Arg;
      end;

      case Ord(Args[1].Typ) shl 6 + Ord(Args[1].Pos) of
        Ord(xtInt8) shl 6 + Ord(mpLocal):  right_i64 := PInt8(Stack.Local(Args[1].Addr))^;
        Ord(xtInt16) shl 6 + Ord(mpLocal): right_i64 := PInt16(Stack.Local(Args[1].Addr))^;
        Ord(xtInt32) shl 6 + Ord(mpLocal): right_i64 := PInt32(Stack.Local(Args[1].Addr))^;
        Ord(xtInt64) shl 6 + Ord(mpLocal): right_i64 := PInt64(Stack.Local(Args[1].Addr))^;
        Ord(xtUInt8) shl 6 + Ord(mpLocal): right_i64 := PUInt8(Stack.Local(Args[1].Addr))^;
        Ord(xtUInt16) shl 6 + Ord(mpLocal):right_i64 := PUInt16(Stack.Local(Args[1].Addr))^;
        Ord(xtUInt32) shl 6 + Ord(mpLocal):right_i64 := PUInt32(Stack.Local(Args[1].Addr))^;
        Ord(xtUInt64) shl 6 + Ord(mpLocal):right_i64 := PUInt64(Stack.Local(Args[1].Addr))^;

        Ord(xtInt8) shl 6 + Ord(mpGlobal):  right_i64 := PInt8(Stack.Global(Args[1].Addr))^;
        Ord(xtInt16) shl 6 + Ord(mpGlobal): right_i64 := PInt16(Stack.Global(Args[1].Addr))^;
        Ord(xtInt32) shl 6 + Ord(mpGlobal): right_i64 := PInt32(Stack.Global(Args[1].Addr))^;
        Ord(xtInt64) shl 6 + Ord(mpGlobal): right_i64 := PInt64(Stack.Global(Args[1].Addr))^;
        Ord(xtUInt8) shl 6 + Ord(mpGlobal): right_i64 := PUInt8(Stack.Global(Args[1].Addr))^;
        Ord(xtUInt16) shl 6 + Ord(mpGlobal):right_i64 := PUInt16(Stack.Global(Args[1].Addr))^;
        Ord(xtUInt32) shl 6 + Ord(mpGlobal):right_i64 := PUInt32(Stack.Global(Args[1].Addr))^;
        Ord(xtUInt64) shl 6 + Ord(mpGlobal):right_i64 := PUInt64(Stack.Global(Args[1].Addr))^;

        else right_i64 := Args[1].Arg;
      end;

      // Perform operation
      case Code of
        bcADD: left_i64 := left_i64 + right_i64;
        bcSUB: left_i64 := left_i64 - right_i64;
        bcMUL: left_i64 := left_i64 * right_i64;
        bcDIV: left_i64 := left_i64 div right_i64;
        bcMOD: left_i64 := left_i64 mod right_i64;
        bcBND: left_i64 := left_i64 and right_i64;
        bcBOR: left_i64 := left_i64 or right_i64;
        bcXOR: left_i64 := left_i64 xor right_i64;
        bcEQ:  left_i64 := Ord(left_i64 = right_i64);
        bcNE:  left_i64 := Ord(left_i64 <> right_i64);
        bcGT:  left_i64 := Ord(left_i64 > right_i64);
        bcLT:  left_i64 := Ord(left_i64 < right_i64);
        bcGTE: left_i64 := Ord(left_i64 >= right_i64);
        bcLTE: left_i64 := Ord(left_i64 <= right_i64);
      end;

      // Store result
      case Args[2].Pos of
        mpLocal:  Move(left_i64, Stack.Local(Args[2].Addr)^, XprTypeSize[Args[2].Typ]);
        mpGlobal: Move(left_i64, Stack.Global(Args[2].Addr)^, XprTypeSize[Args[2].Typ]);
      end;
    end;  
		
  bcfADD, bcfSUB, bcfMUL, bcfDIV, bcfMOD,
  bcfEQ, bcfNEQ, bcfGT, bcfLT, bcfGTE, bcfLTE:
    begin	
      case Ord(Args[0].Typ) shl 2 + Ord(Args[0].Pos) of
        Ord(xtSingle) shl 2 + Ord(mpLocal): left_f64 := PFloat32(Stack.Local(Args[0].Addr))^;
        Ord(xtDouble) shl 2 + Ord(mpLocal): left_f64 := PFloat64(Stack.Local(Args[0].Addr))^;

        Ord(xtSingle) shl 2 + Ord(mpGlobal): left_f64 := PFloat32(Stack.Global(Args[0].Addr))^;
        Ord(xtDouble) shl 2 + Ord(mpGlobal): left_f64 := PFloat64(Stack.Global(Args[0].Addr))^;

        else left_f64 := Double(Args[0].Arg);
      end;

      case Ord(Args[1].Typ) shl 2 + Ord(Args[1].Pos) of
        Ord(xtSingle) shl 2 + Ord(mpLocal): right_f64 := PFloat32(Stack.Local(Args[1].Addr))^;
        Ord(xtDouble) shl 2 + Ord(mpLocal): right_f64 := PFloat64(Stack.Local(Args[1].Addr))^;

        Ord(xtSingle) shl 2 + Ord(mpGlobal): right_f64 := PFloat32(Stack.Global(Args[1].Addr))^;
        Ord(xtDouble) shl 2 + Ord(mpGlobal): right_f64 := PFloat64(Stack.Global(Args[1].Addr))^;

        else right_f64 := Double(Args[1].Arg);
      end;

      // Perform operation
      case Code of
        bcfADD: left_f64 := left_f64 + right_f64;
        bcfSUB: left_f64 := left_f64 - right_f64;
        bcfMUL: left_f64 := left_f64 * right_f64;
        bcfDIV: left_f64 := left_f64 / right_f64;
        (*
        bcfMOD: left_f64 := left_f64 mod right_f64;
        *)
        bcfEQ:  left_f64 := Ord(left_f64 = right_f64);
        bcfNEQ: left_f64 := Ord(left_f64 <> right_f64);
        bcfGT:  left_f64 := Ord(left_f64 > right_f64);
        bcfLT:  left_f64 := Ord(left_f64 < right_f64);
        bcfGTE: left_f64 := Ord(left_f64 >= right_f64);
        bcfLTE: left_f64 := Ord(left_f64 <= right_f64);
      end;



      // Store result
      case Args[2].Pos of
        mpLocal:  Move(left_f64, Stack.Local(Args[2].Addr)^,  XprTypeSize[Args[2].Typ]); //???
        mpGlobal: Move(left_f64, Stack.Global(Args[2].Addr)^, XprTypeSize[Args[2].Typ]);
      end;  		
    end;
