{.$R-}
const
  N = 1024*1024;

  // Large test sizes now wired into benchmark calls
  LG_FFT_SIZE = 1024 * 512;
  LG_SOR_SIZE = 300;
  LG_SPARSE_SIZE = 10000000;
  LG_LU_SIZE = 400;

var
  A, B: array of Double;
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

procedure InitArrays;
var
  i: Int64;
begin
  SetLength(A, N);
  SetLength(B, N);
  for i := 0 to N - 1 do
  begin
    A[i] := Rand;
    B[i] := Rand;
  end;
end;

function KernelDot(n: Int64): Double;
var
  i: Int64;
  sum: Double;
begin
  sum := 0.0;
  for i := 0 to n - 1 do
    sum := sum + A[i] * B[i];
  Result := sum;
end;

function KernelSOR(nx, ny: Int64; omega: Double): Double;
var
  grid, old: array of Double;
  x, y, idx, i, it, iterations: Int64;
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
  k: Int64;
  sum: Double;
begin
  sum := 0.0;
  for k := 0 to n - 1 do
    sum := sum + Sin(2.0 * PI * k / n) + Cos(2.0 * PI * k / n);
  Result := sum;
end;

function KernelMonteCarlo(samples: Int64): Double;
var
  i, count: Int64;
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
  i, j, k: Int64;
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

procedure RunScimark;
var
  dot, sor, fft, mc, lu: Double;
  dot_flops, sor_flops, fft_flops, mc_flops, lu_flops: Double;
  t0, t1, total: Double;
  iter, samples, fft_n, lu_n: Int64;
begin
  InitArrays;

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
