
type
  TMyPointerList = specialize TArrayList<Pointer>;

var
  MyPointers: TMyPointerList;

procedure FreePointers(ptrs: TMyPointerList);
var i:Int32;
begin
  for i:=0 to ptrs.Top do Freemem(ptrs.data[i]);
end;

function ShutUpAndAlloc(const Value; Size:SizeInt; PtrList:TMyPointerList): Pointer;
begin
  Result := GetMem(Size);
  Move(Value, Result^, Size);
  PtrList.Add(Result);
end;

// a test
procedure Test_Iterate(count:Int64);
var
  BC: TBytecode;
  C1,C2, i, a, b, tmp, suc, lim: Pointer;
  _0, _1, _2, _1594: Int64;
  t:Double;
begin
  _0 := 0; _1 := 1; _2 := 2; _1594 := 1594;

  MyPointers.Init([]);
  BC.Init();

  tmp := ShutUpAndAlloc(_0,      SizeOf(Int64), MyPointers);
  i   := ShutUpAndAlloc(_0,      SizeOf(Int64), MyPointers);
  lim := ShutUpAndAlloc(count,   SizeOf(Int64), MyPointers);
  C1  := ShutUpAndAlloc(_1,      SizeOf(Int64), MyPointers);
  C2  := ShutUpAndAlloc(_2,      SizeOf(Int64), MyPointers);
  suc := ShutUpAndAlloc(_0,      SizeOf(Int64), MyPointers);
  a   := ShutUpAndAlloc(_1594,   SizeOf(Int64), MyPointers);
  b   := ShutUpAndAlloc(_0,      SizeOf(Int64), MyPointers);

  BC.AddInstruction( OpCode(MOV_I64,    b,    a             )); //0:  b   := a;
  BC.AddInstruction( OpCode(MUL_I64,    a,    C2,  tmp      )); //1:  tmp := a * 2;
  BC.AddInstruction( OpCode(LADD_I64,   b,    tmp           )); //2:  b   += tmp;
  BC.AddInstruction( OpCode(MUL_I64,    a,    C2,  tmp      )); //3:  tmp := a * 2;
  BC.AddInstruction( OpCode(LSUB_I64,   b,    tmp           )); //4:  b   -= tmp
  BC.AddInstruction( OpCode(JNE_I64,    a,    b,   OpVar(7) )); //5:  if a <> b GOTO(7)
  BC.AddInstruction( OpCode(LADD_I64,   suc,  C1            )); //6:  success += C1
  BC.AddInstruction( OpCode(LADD_I64,   i,    C1            )); //7:  i += 1
  BC.AddInstruction( OpCode(JLE_I64,    i,    lim, OpVar(0) )); //8:  if i < lim GOTO(0)
  BC.AddInstruction( OpCode(RETURN                          ));

  t := MarkTime();
  Interpret(BC);
  WriteLn('Result: ', PInt64(suc)^, ' computed in ', Format('%.3f', [MarkTime() - t]), 'ms');

  BC.Free();
  FreePointers(MyPointers);
  MyPointers.Free();
end;


//simple counting loop
procedure Test_SimpleLoop(count:Int64);
var
  BC: TBytecode;
  C1, i, lim: Pointer;
  _0, _1: Int64;
begin
  _0 := 0;  _1 := 1;

  MyPointers.Init([]);
  BC.Init();

  C1  := ShutUpAndAlloc(_1,    SizeOf(Int64), MyPointers);
  i   := ShutUpAndAlloc(_0,    SizeOf(Int64), MyPointers);
  lim := ShutUpAndAlloc(count, SizeOf(Int64), MyPointers);

  BC.AddInstruction( OpCode(LADD_I64,  i,    C1            )); //7:  i += 1
  BC.AddInstruction( OpCode(JLE_I64,   i,    lim, OpVar(0) )); //8:  if i < lim GOTO(0)
  BC.AddInstruction( OpCode(RETURN                         ));

  Interpret(BC);
  WriteLn('Result: ', PInt64(i)^);

  BC.Free();
  FreePointers(MyPointers);
  MyPointers.Free();
end; 
