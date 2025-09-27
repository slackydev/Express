unit xpr.nativebench;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Test benchmarks (in FPC) to compare our speed to real native code.
}
{$I header.inc}

interface

uses
  Classes, SysUtils;

type
  XprNativeBenchmark = class
    class function DotProduct: Int64; static;
    class procedure LapeIsFast; static;
    class procedure ShellShort; static;
    class procedure Scimark; static;
    class procedure Pidigits; static;
    class procedure SplitTPA; static;
  end;

implementation

uses
  xpr.Utils;

class function XprNativeBenchmark.DotProduct: Int64; static;
var
  A, B: array of Int64;
  i: Int32;
  N,dot: Int64;
  t,tt: Double;
begin

  N := 10000000; // 10 million
  SetLength(A, N);
  SetLength(B, N);

  t := MarkTime();

  // Fill arrays as per original logic
  for i := 0 to N - 1 do
  begin
    A[i] := i mod 1000;
    B[i] := (i * 7) mod 1000;
  end;

  // Dot product
  tt := MarkTime();
  dot := 0;
  for i := 0 to N - 1 do
    dot := dot + A[i] * B[i];

  Result := dot;
  WriteLn(Format('All in %.4f, DotProd in %.4f ms', [MarkTime() - t, MarkTime() - tt])+#13#10);
end;

class procedure XprNativeBenchmark.LapeIsFast; static;
var
  a, b, hits, n: Int64;
  i: Int32;
begin
  a := 1594;
  hits := 0;

  n := 10000000;
  for i:=0 to n do
  begin
    b := a;
    b := b + a * (i mod 5);
    b := b - a * (i mod 10);
    if b+i = a then
      Inc(hits);
  end;
end;

class procedure XprNativeBenchmark.ShellShort(); static;
var
  arr: array of Int64;
  n, gap: Int64;
  i,j: int32;
  tmp: Int64;
  before, after: Double;
begin
  n := 1000000;
  SetLength(arr, n);

  for i := 0 to n - 1 do
    arr[i] := Random(1000000);

  before := MarkTime();

  // Compute initial gap
  gap := 1;
  while gap <= (n - 1) div 3 do
    gap := gap * 3 + 1;

  while gap >= 1 do
  begin
    for i := gap to n - 1 do
    begin
      j := i;
      while (j >= gap) and (arr[j] < arr[j - gap]) do
      begin
        tmp := arr[j];
        arr[j] := arr[j - gap];
        arr[j - gap] := tmp;
        j := j - gap;
      end;
    end;
    gap := gap div 3;
  end;

  after := MarkTime();

  WriteLn(Format('ShellSort in %.3f ms', [after-before])+#13#10);
end;

class procedure XprNativeBenchmark.Scimark; static;
const
  N = 1024*1024;
  // Large test sizes now wired into benchmark calls
  LG_FFT_SIZE = 1024 * 512;
  LG_SOR_SIZE = 300;
  LG_SPARSE_SIZE = 10000000;
  LG_LU_SIZE = 400;
var
  seed: Int64 = 123456;

  function TimeNow(): Double;
  begin
    Result := MarkTime() / 1000.0;
  end;

  function Rand: Double;
  begin
    seed := (16807 * (seed mod 127773)) - 2836 * (seed div 127773);
    if seed <= 0 then
      seed := seed + 2147483647;
    Result := seed / 2147483647.0;
  end;

  function KernelDot(n: Int64): Double;
  var
    A, B: array of Double;
    i: Int32;
    sum: Double;
  begin
    SetLength(A, N);
    SetLength(B, N);
    for i := 0 to N - 1 do
    begin
      A[i] := Rand;
      B[i] := Rand;
    end;

    sum := 0.0;
    for i := 0 to n - 1 do
      sum := sum + A[i] * B[i];
    Result := sum;
  end;

  function KernelSOR(nx, ny: Int64; omega: Double): Double;
  var
    grid, old: array of Double;
    x, y, idx, i, it, iterations: Int32;
  begin
    SetLength(grid, nx * ny);
    SetLength(old, nx * ny);
    for i := 0 to High(grid) do
      grid[i] := Rand;

    iterations := nx;
    for it := 0 to iterations - 1 do
      for y := 1 to ny - 2 do
        for x := 1 to nx - 2 do
        begin
          idx := y * nx + x;
          old[idx]  := grid[idx];
          grid[idx] := old[idx] + omega * (
            old[idx - 1] + old[idx + 1] +
            old[idx - nx] + old[idx + nx] -
            4.0 * old[idx]);
        end;

    Result := grid[(ny div 2) * nx + (nx div 2)];
  end;

  function KernelFFT(n: Int64): Double;
  var
    k: Int32;
    sum: Double;
  begin
    sum := 0.0;
    for k := 0 to n - 1 do
      sum := sum + Sin(2.0 * PI * k / n) + Cos(2.0 * PI * k / n);
    Result := sum;
  end;

  function KernelMonteCarlo(samples: Int64): Double;
  var
    i, count: Int32;
    x, y: Double;
  begin
    count := 0;
    for i := 0 to samples - 1 do
    begin
      x := Rand * 2.0 - 1.0;
      y := Rand * 2.0 - 1.0;
      if (x * x + y * y <= 1.0) then
        Inc(count);
    end;
    Result := 4.0 * count / samples;
  end;

  function KernelLU(n: Int64): Double;
  var
    mat: array of Double;
    i, j, k: Int32;
  begin
    SetLength(mat, n * n);
    for i := 0 to n * n - 1 do
      mat[i] := Rand;

    for k := 0 to n - 1 do
      for i := k + 1 to n - 1 do
      begin
        mat[i * n + k] := mat[i * n + k] / mat[k * n + k];
        for j := k + 1 to n - 1 do
          mat[i * n + j] := mat[i * n + j] - mat[i * n + k] * mat[k * n + j];
      end;

    Result := mat[(n - 1) * n + (n - 1)];
  end;

var
  dot, sor, fft, mc, lu: Double;
  dot_flops, sor_flops, fft_flops, mc_flops, lu_flops: Double;
  t0, t1, total: Double;
  iter, samples, fft_n, lu_n: Int64;
begin
  // Dot Product
  t0 := TimeNow();
  dot := KernelDot(N);
  t1 := TimeNow();
  dot_flops := (2.0 * N) / (t1 - t0) / 1000000.0;
  WriteLn(Format('DotProd:    %.3f', [dot_flops]));

  t0 := TimeNow();
  sor := KernelSOR(LG_SOR_SIZE, LG_SOR_SIZE, 1.25);
  t1 := TimeNow();
  iter := LG_SOR_SIZE;
  sor_flops := (6.0 * (LG_SOR_SIZE - 1) * (LG_SOR_SIZE - 1) * iter) / (t1 - t0) / 1000000.0;
  WriteLn(Format('SOR:        %.3f', [sor_flops]));

  fft_n := LG_FFT_SIZE;
  t0 := TimeNow();
  fft := KernelFFT(fft_n);
  t1 := TimeNow();
  fft_flops := (4.0 * fft_n) / (t1 - t0) / 1000000.0;
  WriteLn(Format('FFT init:   %.3f', [fft_flops]));

  samples := LG_SPARSE_SIZE;
  t0 := TimeNow();
  mc := KernelMonteCarlo(samples);
  t1 := TimeNow();
  mc_flops := (4.0 * samples) / (t1 - t0) / 1000000.0;
  WriteLn(Format('MonteCarlo: %.3f', [mc_flops]));

  lu_n := LG_LU_SIZE;
  t0 := TimeNow();
  lu := KernelLU(lu_n);
  t1 := TimeNow();
  lu_flops := ((2.0 / 3.0) * lu_n * lu_n * lu_n) / (t1 - t0) / 1000000.0;
  WriteLn(Format('LU:         %.3f', [lu_flops]));

  total := (dot_flops + sor_flops + fft_flops + mc_flops + lu_flops) / 5.0;
  WriteLn(Format('sum:        %.3f', [total]));
end;

class procedure XprNativeBenchmark.Pidigits; static;
var
  n, len, i, j, k, nines, predigit, q, x: Int64;
  a, res: array of Int64;
  resCount: Int64;
  tm, tma: Double;

begin
  a := [];
  res := [];

  n := 5000;
  len := (10 * n) div 3;
  SetLength(a, len + 1);
  SetLength(res, n + 1);
  resCount := 0;

  tm := MarkTime();

  nines := 0;
  predigit := 0;

  // Initialize array with 2s
  for j := 0 to len do
    a[j] := 2;

  for j := 0 to n - 1 do
  begin
    q := 0;

    // Backward loop
    for i := len downto 1 do
    begin
      x := 10 * a[i - 1] + q * i;
      a[i - 1] := x mod (2 * i - 1);
      q := x div (2 * i - 1);
    end;

    a[0] := q mod 10;
    q := q div 10;

    if q = 9 then
      Inc(nines)
    else if q = 10 then
    begin
      res[resCount] := predigit + 1;
      Inc(resCount);
      for k := 1 to nines do
      begin
        res[resCount] := 0;
        Inc(resCount);
      end;
      predigit := 0;
      nines := 0;
    end
    else
    begin
      res[resCount] := predigit;
      Inc(resCount);
      predigit := q;

      if nines <> 0 then
      begin
        for k := 1 to nines do
        begin
          res[resCount] := 9;
          Inc(resCount);
        end;
        nines := 0;
      end;
    end;
  end;

  res[resCount] := predigit;

  tma := MarkTime();
  writeln(Format('FPC Native Pidigits used: %.3f ms', [tma - tm]));
end;


class procedure XprNativeBenchmark.SplitTPA; static;
type
  TPoint = record X,Y: Int32; end;
  TPointArray = array of TPoint;
  T2DPointArray = array of TPointArray;

  function SplitPoints(var Points: TPointArray; DistX, DistY: Int32): T2DPointArray;
  var
    Clusters: T2DPointArray;
    Current: TPointArray;
    ClustSize: Int32;
    p1, p2: TPoint;
    xsq, ysq, xxyy: Int64; // Use Int64 to avoid overflow
    LastIndex, ProcCount, i, j, dx, dy: Int32;
  begin
    if High(Points) = -1 then
      Exit(nil);

    if High(Points) = 0 then
    begin
      SetLength(Clusters, 1);
      Clusters[0] := Points;
      Exit(Clusters);
    end;

    xsq := DistX * DistX;
    ysq := DistY * DistY;
    xxyy := xsq * ysq;

    LastIndex := High(Points);
    ProcCount := 0;
    SetLength(Clusters, 0);

    while (LastIndex - ProcCount >= 0) do
    begin
      SetLength(Current, 1);
      Current[0] := Points[0];
      Points[0] := Points[LastIndex - ProcCount];
      Inc(ProcCount);

      ClustSize := 1;
      i := 0;
      while (i < ClustSize) do
      begin
        j := 0;
        p1 := Current[i];
        while (j <= LastIndex - ProcCount) do
        begin
          p2 := Points[j];

          dx := p1.X - p2.X;
          dy := p1.Y - p2.Y;
          if ((dx * dx * ysq) + (dy * dy * xsq) <= xxyy) then
          begin
            SetLength(Current, ClustSize + 1);
            Current[ClustSize] := p2;
            Points[j] := Points[LastIndex - ProcCount];

            Inc(ProcCount);
            Inc(ClustSize);
            Dec(j);
          end;
          Inc(j);
        end;
        Inc(i);
      end;

      SetLength(Clusters, Length(Clusters) + 1);
      Clusters[High(Clusters)] := Current;
    end;

    Result := Clusters;
  end;

  function GeneratePoints(N: Integer): TPointArray;
  var
    i: Integer;
  begin
    SetLength(Result, N);
    for i := 0 to N - 1 do
    begin
      Result[i].X := Random(1001);
      Result[i].Y := Random(1001);
    end;
  end;

  procedure Benchmark;
  var
    P: TPointArray;
    t0, t1: Double;
    Clusters: T2DPointArray;
  begin
    P := GeneratePoints(10000);

    t0 := MarkTime();
    Clusters := SplitPoints(P, 2, 2);
    t1 := MarkTime();

    WriteLn(Length(Clusters));
    WriteLn(Format('Nayive SplitTPA used: %.3f ms', [t1 - t0]));
  end;

begin
 Randomize;
 Benchmark;
end;


end.

