program new;

{$define JIT := jit}
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
procedure InsertionSort(var arr: TTestArray); {$macro JIT}
var
  i, j: Int64;
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

procedure InsertionSortBounded(var a: TTestArray; lo, hi: Int64); {$macro JIT}
var
  i, j: Int64;
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
procedure SelectionSort(var arr: TTestArray); {$macro JIT}
var
  i, j, minIndex: Int64;
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
procedure GappyShellSort(var a: TTestArray; gaps: TIntegerArray); {$macro JIT}
var
  i, j, gap, n, offset: Int64;
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

procedure GappyShellSort2(var a: TTestArray; gaps: TIntegerArray); {$macro JIT}
var
  i, j, gap, n: Int64;
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

procedure GappyShellSortPartial(var a: TTestArray; gaps: TIntegerArray; gapLim:Int64=3); {$macro JIT}
var
  i, j, gap, n, k: Int64;
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
procedure ShellSort(var Arr: TTestArray); {$macro JIT}
var
  Gap, i, j, H: Int64;
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
procedure qsort(a: TTestArray; left, right: NativeInt); {$macro JIT}
var 
  stack: array of NativeInt;
  top,i,j,_i,_j: NativeInt;
  pivot, tmp: TTestType;
begin
  stack.SetLen(right - left + 1);
  
  // push initial range
  stack[top] := left;  top += 1;
  stack[top] := right; top += 1;
  
  while (top > 0) do
  begin
    top -= 1; right := stack[top];
    top -= 1; left  := stack[top];
    
    // insertion sort pass
    if (right - left <= 22) then
    begin
      for _i := left + 1 to right do
      begin
        tmp := a[_i];
        _j := _i-1;
        while (_j >= left) and (a[_j] > tmp) do
        begin
          a[_j+1] := a[_j];
          _j -= 1;
        end;
        a[_j + 1] := tmp;
      end;
      continue;
    end;
    
    pivot := a[(left + right) shr 1];

    i := left;
    j := right;
    while (i <= j) do
    begin
      while (a[i] < pivot) do i += 1;
      while (a[j] > pivot) do j -= 1;

      if (i <= j) then
      begin
        tmp := a[i];
        a[i] := a[j];
        a[j] := tmp;
        i += 1;
        j -= 1;
      end;
    end;
    
    if (left < j) then
    begin
      stack[top] := left; top += 1;
      stack[top] := j;    top += 1;
    end;
    
    if (i < right) then
    begin
      stack[top] := i;     top += 1;
      stack[top] := right; top += 1;
    end;
  end;
end;

procedure GappyShellSortBounded(var a: TTestArray; gaps: TIntegerArray; lo, hi: Int64); {$macro JIT}
var
  i, j, gap, logap: Int64;
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


procedure MergeSort(var arr: TTestArray); {$macro JIT}
var
  buf: TTestArray;

  procedure Merge(L, M, R: Int64); {$macro JIT}
  var
    I, J, K: Int64;
  begin
    I := L;
    J := M + 1;
    K := L; 

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

    if I <= M then MemMove(@arr[I], @Buf[K], (M - I + 1) * SizeOf(arr[0]));
    if J <= R then MemMove(@arr[J], @Buf[K], (R - J + 1) * SizeOf(arr[0]));
    MemMove(@Buf[0], @arr[L], (R - L + 1) * SizeOf(arr[0]));
  end;

  procedure MSort(L, R: Int64); {$macro JIT}
  var
    M: Int64;
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


procedure MergeSort_Iterative(var arr: TTestArray); {$macro JIT}
var
  buf: TTestArray;
  width, L, M, R, N: Int64;

  procedure Merge(L, M, R: Int64); {$macro JIT}
  var
    I, J, K: Int64;
  begin
    I := L;
    J := M + 1;
    K := L;

    while (I <= M) and (J <= R) do
    begin
      if arr[I] <= arr[J] then
      begin
        buf[K] := arr[I];
        Inc(I);
      end
      else
      begin
        buf[K] := arr[J];
        Inc(J);
      end;
      Inc(K);
    end;

    if I <= M then
      MemMove(@arr[I], @buf[K], (M - I + 1) * SizeOf(arr[0]));
    if J <= R then
      MemMove(@arr[J], @buf[K], (R - J + 1) * SizeOf(arr[0]));

    MemMove(@buf[L], @arr[L], (R - L + 1) * SizeOf(arr[0]));
  end;

const
  RUN = 24; // Small chunks to be sorted by InsertionSort
begin
  N := Length(arr);
  if N <= 1 then Exit;

  // 1. PRE-SORT THE "LEAVES"
  L := 0;
  while L < N do
  begin
    InsertionSortBounded(arr, L, Min(L + RUN - 1, N - 1));
    Inc(L, RUN);
  end;

  SetLength(buf, N);

  // 2. START MERGING AT WIDTH 16
  width := RUN; 
  while width < N do
  begin
    L := 0;
    while L < N do
    begin
      M := L + width - 1;
      if M >= N - 1 then Break;

      R := L + 2 * width - 1;
      if R >= N then R := N - 1;

      if arr[M] > arr[M + 1] then
        Merge(L, M, R);

      Inc(L, 2 * width);
    end;
    width := width * 2;
  end;
end;


(*
 =====================================================================
 =====================================================================
 =====================================================================
*)

function IsSorted(arr: TTestArray): Boolean;
var i: Int64;
begin
  Result := True;
  for i:=1 to High(arr) do
    if arr[i] < arr[i-1] then
      Exit(False);
end;



function GenerateRandom(n: Int64): TTestArray;
var i: Int64;
begin
  for i:=0 to n do
    Result.Append(Random(0,$FFFFFF));
end;

function GenerateRandomEnd(n: Int64): TTestArray;
begin
  Result := GenerateRandom(n);
  qsort(Result, 1, Min(High(Result), n-256));
end;

function GenerateNearlySorted(n: Int64): TTestArray;
var
  i,r,step: Int64;
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

function GenerateRandomEnd_Reverse(n: Int64): TTestArray;
begin
  Result := GenerateRandomEnd(n).Reversed();
end;

function GenerateNearlySorted_Reverse(n: Int64): TTestArray;
begin
  Result := GenerateNearlySorted(n).Reversed();
end;





procedure DoTest(n: Int64; Generator:function(n: Int64): TTestArray);
var
  arr, orig: TTestArray;
  i: Int64;
  t:Double;
begin
  orig := Generator(n);

  arr := Copy(orig);
  t := PerformanceTime();
  MergeSort(arr);
  WriteLn('MergeSort:       ', PerformanceTime() - t,'ms', ' | IsSorted: ', IsSorted(arr));

  arr := Copy(orig);
  t := PerformanceTime();
  MergeSort_Iterative(arr);
  WriteLn('MergeSort [iter]:', PerformanceTime() - t,'ms', ' | IsSorted: ', IsSorted(arr));

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
    DoTest(300000, GenerateRandom);
  //  i *= 2;
  //end;
end.
