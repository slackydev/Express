program new;

{$r-}

const
  Ciura_gaps: TIntegerArray   := [39744,18298,8359,3785,1695,     701, 301, 132, 57, 23, 10, 4, 1];
  unknown_gaps: TIntegerArray := [3735498, 1769455, 835387, 392925, 184011, 85764, 39744, 18298, 8359, 3785, 1695, 749, 326, 138, 57, 23, 9, 4, 1];
  mix_gaps: TIntegerArray     := [3735498, 1769455, 835387, 392925, 184011, 85764, 39744, 18298, 8359, 3785, 1695, 701, 301, 132, 57, 23, 10, 4, 1];
  new_gaps: TIntegerArray     := [2689522, 1032864, 396653, 152328, 58498, 22465, 8627, 3313, 1272, 488, 187, 72, 27, 10, 4, 1];

type
  TTestArray = array of int64;
  TTestType  = Int64;

// insertion
procedure InsertionSort(var arr: TTestArray); jit;
var
  i, j: Int;
  temp: TTestType;
begin
  for i:=1 to High(arr) do
  begin
    temp := arr[i];
    j := i-1;
    while (j >= 0) and (temp < arr[j]) do
    begin
      arr[j+1] := arr[j];
      Dec(j);
    end;
    arr[j+1] := temp;
  end;
end;

procedure InsertionSortBounded(var a: TTestArray; lo, hi: Int); jit;
var
  i, j: Int;
  tmp: TTestType;
begin
  for i := lo + 1 to hi do
  begin
    tmp := a[i];
    j := i - 1;
    while (j >= lo) and (a[j] > tmp) do
    begin
      a[j+1] := a[j];
      Dec(j);
    end;
    a[j + 1] := tmp;
  end;
end;

// selection sort
// pure O(n^2), by finding the min element repeatedly
procedure SelectionSort(var arr: TTestArray); jit;
var
  i, j, minIndex: Int;
  temp: TTestType;
begin
  for i := 0 to High(arr) - 1 do
  begin
    minIndex := i;
    for j := i + 1 to High(arr) do
    begin
      if arr[j] < arr[minIndex] then
        minIndex := j;
    end;

    if minIndex <> i then
    begin
      temp := arr[i];
      arr[i] := arr[minIndex];
      arr[minIndex] := temp;
    end;
  end;
end;


// Shell sort gapped
procedure GappyShellSort(var a: TTestArray; gaps: TIntegerArray); jit;
var
  i, j, gap, n, offset: Int;
  tmp: TTestType;
begin
  n := High(a);
  for gap in gaps do begin
    if gap > n then continue;
    for offset:=0 to gap-1 do begin
      i := offset;
      while i <= n do begin
        tmp := a[i];
        j := i;
        while (j >= gap) and (a[j - gap] > tmp) do begin
          a[j] := a[j-gap];
          j := j - gap;
        end;
        a[j] := tmp;
        i := i + gap;
      end;
    end;
  end;
end;

procedure GappyShellSort2(var a: TTestArray; gaps: TIntegerArray); jit;
var
  i, j, gap, n: Int;
  tmp: TTestType;
begin
  n := High(a);
  for gap in gaps do begin
    if gap > n then continue; // Skip gaps larger than array size
    for i:=gap to n do begin  // Start from `gap` to ensure bounds are maintained
      tmp := a[i];
      j := i;
      while (j >= gap) and (a[j - gap] > tmp) do begin
        a[j] := a[j - gap];
        j := j - gap;
      end;
      a[j] := tmp;
    end;
  end;
end;

procedure GappyShellSortPartial(var a: TTestArray; gaps: TIntegerArray; gapLim:Int=3); jit;
var
  i, j, gap, n, k: Int;
  tmp: TTestType;
begin
  n := High(a);
  for k:=0 to Min(gapLim - 1, High(gaps)) do begin
    gap := gaps[k];
    if gap > n then continue; // Skip gaps larger than array size
    for i:=gap to n do begin  // Start from `gap` to ensure bounds are maintained
      tmp := a[i];
      j := i;
      while (j >= gap) and (a[j - gap] > tmp) do begin
        a[j] := a[j - gap];
        j := j - gap;
      end;
      a[j] := tmp;
    end;
  end;
end;

// shellsort
procedure ShellSort(var Arr: TTestArray); jit;
var
  Gap, i, j, H: Int;
  tmp: TTestType;
begin
  H := High(Arr);
  Gap := 0;
  while (Gap < (H+1) div 3) do Gap := Gap * 3 + 1;

  while Gap >= 1 do begin
    for i := Gap to H do begin
      tmp := arr[i];
      j := i;
      while (j >= Gap) and (Arr[j] < Arr[j - Gap]) do
      begin
        arr[j] := Arr[j - Gap];
        arr[j - gap] := tmp;
        j := j - Gap;
      end;
    end;
    Gap := Gap div 3;
  end;
end;

// Heap sort


// Quicksort
procedure qsort(a: TTestArray; ilo, ihi:Int); jit;
var lo: Int;
var hi: Int;
var pivot, tmp: TTestType;
begin
  if (ihi - ilo <= 28) then
  begin
    InsertionSortBounded(a, ilo, ihi);
    Exit;
  end;
  
  lo := ilo;
  hi := ihi;
  pivot := a[(lo + hi) div 2];
  repeat
    while(a[lo] < pivot) do begin lo += 1; end;
    while(a[hi] > pivot) do begin hi -= 1; end;
    if lo <= hi then
    begin
      tmp := a[lo];
      a[lo] := a[hi];
      a[hi] := tmp;
      lo += 1;
      hi -= 1;
    end;
  until (lo > hi);

  if(hi > ilo) then qsort(a, ilo, hi);
  if(lo < ihi) then qsort(a, lo, ihi);
end;

procedure GappyShellSortBounded(var a: TTestArray; gaps: TIntegerArray; lo, hi: Int); jit;
var
  i, j, gap, logap: Int;
  tmp: TTestType;
begin
  for gap in gaps do begin
    if gap > (hi - lo + 1) then continue; // Skip gaps larger than the subsection size
    logap := lo + gap;
    for i := logap to hi do begin
      tmp := a[i];
      j := i;
      while (j >= logap) and (a[j - gap] > tmp) do begin
        a[j] := a[j - gap];
        Dec(j, gap);
      end;
      a[j] := tmp;
    end;
  end;
end;


procedure MergeSort(var arr: TTestArray); jit;
var
  buf: TTestArray;
  
  procedure Merge(L, M, R: Int); jit;
  var
    I, J, K: Int;
  begin
    I := L;
    J := M + 1;
    K := 0;

    while (I <= M) and (J <= R) do
    begin
      if arr[I] <= arr[J] then
      begin
        Buf[K] := arr[I];
        Inc(I);
      end else
      begin
        Buf[K] := arr[J];
        Inc(J);
      end;
      Inc(K);
    end;

    if I <= M then Move(@arr[I], @Buf[K], (M - I + 1) * SizeOf(arr[0]));
    if J <= R then Move(@arr[J], @Buf[K], (R - J + 1) * SizeOf(arr[0]));
    Move(@Buf[0], @arr[L], (R - L + 1) * SizeOf(arr[0]));
  end;

  procedure MSort(L, R: Int); jit;
  var
    M: Int;
  begin
    if R > L then begin
      M := (L + R) shr 1;
      MSort(L, M);
      MSort(M + 1, R);
      if arr[M] > arr[M + 1] then
        Merge(L, M, R);
    end;
  end;
begin
  if Length(arr) > 1 then begin
    SetLength(Buf, Length(arr));
    MSort(0, High(arr));
  end;
end;

(*
 =====================================================================
 =====================================================================
 =====================================================================
*)

function IsSorted(arr: TTestArray): Boolean;
var i: Int;
begin
  Result := True;
  for i:=1 to High(arr) do
    if arr[i] < arr[i-1] then
      Exit(False);
end;



function GenerateRandom(n: Int): TTestArray;
var i: Int;
begin
  for i:=0 to n do
    Result.Append(Random(0,$FFFFFF));
end;

function GenerateRandomEnd(n: Int): TTestArray;
begin
  Result := GenerateRandom(n);
  qsort(Result, 1, Min(High(Result), n-256));
end;

function GenerateNearlySorted(n: Int): TTestArray;
var
  i,r,step: Int;
begin
  Result := GenerateRandom(n);
  qsort(Result, 0, High(Result));

  step := High(Result) div 4;
  for i:=0 to High(Result) do
  begin
    r := Random(0,step);
    Swap(result[i],result[Min(High(Result),i+r)]);
    i += r;
  end;
end;

function GenerateRandomEnd_Reverse(n: Int): TTestArray;
begin
  Result := GenerateRandomEnd(n).Reversed();
end;

function GenerateNearlySorted_Reverse(n: Int): TTestArray;
begin
  Result := GenerateNearlySorted(n).Reversed();
end;





procedure DoTest(n: Int; Generator:function(n: Int): TTestArray);
var
  arr, orig: TTestArray;
  i: Int;
  t:Double;
begin
  orig := Generator(n);

  arr := Copy(orig);
  t := PerformanceTime();
  MergeSort(arr);
  WriteLn('MergeSort:       ', PerformanceTime() - t,'ms', ' | IsSorted: ', IsSorted(arr));
  
  arr := Copy(orig);
  t := PerformanceTime();
  GappyShellsort2(arr, new_gaps);
  WriteLn('ShellSortNew:    ', PerformanceTime() - t,'ms', ' | IsSorted: ', IsSorted(arr));

  arr := Copy(orig);
  t := PerformanceTime();
  ShellSort(arr);
  WriteLn('Basic->ShellSort:', PerformanceTime() - t,'ms', ' | IsSorted: ', IsSorted(arr));

  arr := Copy(orig);
  t := PerformanceTime();
  qsort(arr, Low(arr), High(arr));
  WriteLn('Quicksort:       ', PerformanceTime() - t,'ms', ' | IsSorted: ', IsSorted(arr));

  arr := Copy(orig);
  t := PerformanceTime();
  sort(arr);
  WriteLn('Internal:        ', PerformanceTime() - t,'ms', ' | IsSorted: ', IsSorted(arr));
end;


var
  i: Int32;
begin
  //i:=4;
  //while i < 10000000 do
  //begin
  //  WriteLn('----', i,'---------');
    DoTest(300000, GenerateRandomEnd);
  //  i *= 2;
  //end;
end.
