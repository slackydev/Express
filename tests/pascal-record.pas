type
  TVector = record
    y: Byte;
    x: Integer;
    z: Word;
  end;
  
  TVectorPacked = packed record
    y: Byte;
    x: Integer;
    z: Word;
  end;

var 
  a: TVector; 
  b: TVectorPacked; 
  l: Integer;
begin
  a.x := 100000;
  a.y := 99;
  a.z := 35000;
  
  WriteLn(SizeOf(a),'->',a);
  
  b.x := 100000;
  b.y := 99;
  b.z := 35000;

  WriteLn(SizeOf(b),'->',b);
end.