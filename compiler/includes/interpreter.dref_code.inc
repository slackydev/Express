bcDREF_ll: Move(Pointer(Stack.Local(Args[1].Arg)^)^, Stack.Local(Args[0].Arg)^, Args[2].Arg);
bcDREF_lg: Move(Pointer(Stack.Global(Args[1].Arg)^)^, Stack.Local(Args[0].Arg)^, Args[2].Arg);
bcDREF_gl: Move(Pointer(Stack.Local(Args[1].Arg)^)^, Stack.Global(Args[0].Arg)^, Args[2].Arg);
bcDREF_gg: Move(Pointer(Stack.Global(Args[1].Arg)^)^, Stack.Global(Args[0].Arg)^, Args[2].Arg);

bcDREF_ll_32: PInt32(Stack.Local(Args[0].Arg))^ := PInt32(Stack.Local(Args[1].Arg)^)^;
bcDREF_lg_32: PInt32(Stack.Local(Args[0].Arg))^ := PInt32(Stack.Global(Args[1].Arg)^)^;
bcDREF_gl_32: PInt32(Stack.Global(Args[0].Arg))^ := PInt32(Stack.Local(Args[1].Arg)^)^;
bcDREF_gg_32: PInt32(Stack.Global(Args[0].Arg))^ := PInt32(Stack.Global(Args[1].Arg)^)^;

bcDREF_ll_64: PInt64(Stack.Local(Args[0].Arg))^ := PInt64(Stack.Local(Args[1].Arg)^)^;
bcDREF_lg_64: PInt64(Stack.Local(Args[0].Arg))^ := PInt64(Stack.Global(Args[1].Arg)^)^;
bcDREF_gl_64: PInt64(Stack.Global(Args[0].Arg))^ := PInt64(Stack.Local(Args[1].Arg)^)^;
bcDREF_gg_64: PInt64(Stack.Global(Args[0].Arg))^ := PInt64(Stack.Global(Args[1].Arg)^)^;