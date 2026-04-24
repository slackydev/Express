program Example;
// Small sample to showcase Pascal dialect hosted by express.
//
// The main difference from regular pascal is in promotion rules:
// Where in Express there are none, int32*int32 = int32
// Promotion is explicit: Int64(int32) * int32 = Int64
//
// Other than that, the language is fairly similar.
//
// The various methods you see are exported to Express language
// But as they share tree, we can use them here. Hence SetLen, High, Append, etc all existing.
//
// Refcounting, jit, generics.. A pascal interpter can have it.

type
  TIntegerArray = array of Int32;

  TPoint = record
    x,y: Int32;
  end;
  
procedure ShellSort(Arr: TIntegerArray); jit;
var
  Gap, i, j, H: Int32;
  tmp: Int32;
begin
  H := Arr.High();
  
  Gap := 0;
  while (Gap < (H+1) div 3) do Gap := Gap * 3 + 1;

  while Gap >= 1 do begin
    for i := Gap to H do begin
      j := i;
      while (j >= Gap) and (Arr[j] < Arr[j - Gap]) do
      begin
        tmp := arr[j] ;
        arr[j] := arr[j-gap] ;
        j := j - gap  ;
        arr[j] := tmp ;
      end;
    end;
    Gap := Gap div 3;
  end;
end; 

function GenerateRandom(n: Int32): TIntegerArray;
var i: Int32;
begin
  for i:=1 to n do
    Result.Append(Random(0,$FFFFFF));
end;

// Like Lape, we can attach to types.
function TIntegerArray.IsSorted(): Bool;
var i: Int32;
begin
  Result := True;
  for i:=1 to self.High() do
    if self[i] < self[i-1] then
      Exit(False);
end;




var
  i: Int32;
  arr: TIntegerArray;
  t: Double;
  pt: TPoint;
begin
  WriteLn('Lets do array sorting in Pascal!');
  arr := GenerateRandom(500000);
  WriteLn(arr.len());
  
  t := MarkTime();
  ShellSort(arr);
  WriteLn('Shellsort used:',MarkTime() - t,'ms');
  
  Writeln('Did we manage to sort?', IsSorted(arr));
  
  pt.x := 1000;
  pt.y := 99;
  Writeln('This is a point:', pt)
end;