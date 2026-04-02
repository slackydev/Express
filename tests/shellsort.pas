program new;

type
  TIntegerArray = array of Int32;

procedure ShellSort(Arr: TIntegerArray); jit;
var
  gap, k, i, j, H: Int32;
  tmp: Int32;
  gaps: array of int32 := [39744,18298,8359,3785,1695,701,301,132,57,23,10,4,1];
begin
  H := Arr.High(); // inherited method for arrays

  for k:=0 to gaps.high() do
  begin
    gap := gaps[k];
    if gap < H then
    begin    
      for i:=gap to h do 
      begin
        tmp := arr[i];
        j := i;
        while (j >= gap) and (arr[j - gap] > tmp) do 
        begin
          arr[j] := arr[j - gap];
          j := j - gap;
        end;
        arr[j] := tmp;
      end;
    end;
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