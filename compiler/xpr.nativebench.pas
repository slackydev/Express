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

end.

