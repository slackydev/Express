type
  TVector = record
    x, y: Double;
  end;

// Overloading the addition operator for TVector
operator + (v1, v2: TVector): TVector;
begin
  Result.x := v1.x + v2.x;
  Result.y := v1.y + v2.y;
end;

var 
  a,b: TVector; 
begin
  a := [10,10];
  b := [20,20];
  WriteLn(a+b);
end.