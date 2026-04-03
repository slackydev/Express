program new;

type
  TIntegerArray = array of Int32;

procedure ShellSort(Arr: TIntegerArray);
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
    Result.Push(RandInt(0,$FFFFFF)); //push is inherited from express
end;

function IsSorted(arr: TIntegerArray): Bool;
var i: Int32;
begin
  Result := True;
  for i:=1 to arr.High() do
    if arr[i] < arr[i-1] then
      Exit(False);
end;


var
  i: Int32;
  arr: TIntegerArray;
  t: Double;
begin
  WriteLn('Lets do array sorting in Pascal!');
  arr := GenerateRandom(500000);
  WriteLn(arr.len());
  
  t := GetTickCount();
  ShellSort(arr);
  WriteLn('Shellsort used:',GetTickCount() - t,'ms');
  
  Writeln('Did we manage to sort?', IsSorted(arr));
end;