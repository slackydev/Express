import java.util.Random;
import java.util.Arrays;

public class SciMarkBenchmark {

    // --- large (out-of-cache) problem sizes (copied from your input) ---
    public static final int LG_FFT_SIZE = 1024 * 1024;
    public static final int LG_SOR_SIZE = 1000;
    public static final int LG_SPARSE_SIZE_M = 100000;
    public static final int LG_SPARSE_SIZE_NZ = 1000000;
    public static final int LG_LU_SIZE = 1000;

    // If you want faster/shorter runs for testing, uncomment & reduce sizes:
    /*
    public static final int LG_FFT_SIZE = 1024;
    public static final int LG_SOR_SIZE = 100;
    public static final int LG_SPARSE_SIZE_M = 1000;
    public static final int LG_SPARSE_SIZE_NZ = 5000;
    public static final int LG_LU_SIZE = 100;
    */

    public static final double PI = 3.14159265358979323846;

    private static final Random RNG = new Random();

    // -------------------------
    // Utility / timing helpers
    // -------------------------
    private static double seconds() {
        return System.nanoTime() / 1.0e9;
    }

    // -------------------------
    // Random vectors / matrices
    // -------------------------
    private static double[] randomVector(int n) {
        double[] r = new double[n];
        for (int i = 0; i < n; i++) r[i] = RNG.nextDouble();
        return r;
    }

    private static double[][] randomMatrix(int m, int n) {
        double[][] r = new double[m][];
        for (int i = 0; i < m; i++) r[i] = randomVector(n);
        return r;
    }

    // -------------------------
    // FFT routines
    // -------------------------
    private static int int_log2(int N) {
        int k = 1;
        int log = 0;
        while (k < N) {
            k *= 2;
            log += 1;
        }
        return log;
    }

    private static void FFT_bitreverse(int N, double[] data) {
        int n_val = N / 2;
        int nm1 = n_val - 1;
        int j = 0;
        for (int i = 0; i < nm1; i++) {
            int ii = i * 2;
            int jj = j * 2;
            int k = n_val / 2;
            if (i < j) {
                double t = data[ii];
                data[ii] = data[jj];
                data[jj] = t;

                t = data[ii + 1];
                data[ii + 1] = data[jj + 1];
                data[jj + 1] = t;
            }
            while (k <= j) {
                j -= k;
                k /= 2;
            }
            j += k;
        }
    }

    private static void FFT_transform_internal(int N, double[] data, int direction) {
        // ref PI (load global once conceptually)
        int n_val = N / 2;
        double wd_real, wd_imag;
        if (n_val == 1) return;

        int logn = int_log2(n_val);

        FFT_bitreverse(N, data);

        double dir = (double) direction;
        int dual = 1;
        for (int bit = 0; bit < logn; bit++) {
            double w_real = 1.0;
            double w_imag = 0.0;
            double theta = 2.0 * dir * PI / (2.0 * dual);
            double s = Math.sin(theta);
            double t = Math.sin(theta / 2.0);
            double s2 = 2.0 * t * t;

            for (int b = 0; b < n_val; b += 2 * dual) {
                int i = 2 * b;
                int j = 2 * (b + dual);
                wd_real = data[j];
                wd_imag = data[j + 1];
                data[j] = data[i] - wd_real;
                data[j + 1] = data[i + 1] - wd_imag;
                data[i] += wd_real;
                data[i + 1] += wd_imag;
            }

            // second loop
            for (int a = 1; a < dual; a++) {
                double tmp_real = w_real - s * w_imag - s2 * w_real;
                double tmp_imag = w_imag + s * w_real - s2 * w_imag;
                w_real = tmp_real;
                w_imag = tmp_imag;

                for (int b = 0; b < n_val; b += 2 * dual) {
                    int i = 2 * (b + a);
                    int j = 2 * (b + a + dual);
                    double z1_real = data[j];
                    double z1_imag = data[j + 1];
                    wd_real = w_real * z1_real - w_imag * z1_imag;
                    wd_imag = w_real * z1_imag + w_imag * z1_real;
                    data[j] = data[i] - wd_real;
                    data[j + 1] = data[i + 1] - wd_imag;
                    data[i] += wd_real;
                    data[i + 1] += wd_imag;
                }
            }
            dual *= 2;
        }
    }

    private static void FFT_transform(int N, double[] data) {
        FFT_transform_internal(N, data, -1);
    }

    private static void FFT_inverse(int N, double[] data) {
        int n_val = N / 2;
        FFT_transform_internal(N, data, 1);
        double norm = 1.0 / (double) n_val;
        for (int i = 0; i < N; i++) data[i] *= norm;
    }

    private static double FFT_num_flops(int N) {
        // Matches your Pascal formula.
        return (5.0 * N - 2.0) * int_log2(N) + 2.0 * (N + 1.0);
    }

    // -------------------------
    // SOR
    // -------------------------
    private static double SOR_num_flops(int m, int n, int num_iterations) {
        return (double) (m - 1) * (double) (n - 1) * (double) num_iterations * 6.0;
    }

    private static void SOR_execute(int m, int n, double omega, double[][] g, int num_iterations) {
        double omega_over_four = omega * 0.25;
        double one_minus_omega = 1.0 - omega;
        int mm1 = m - 1;
        int nm1 = n - 1;

        for (int p = 0; p < num_iterations; p++) {
            for (int i = 1; i < mm1; i++) {
                double[] gi = g[i];
                double[] gim1 = g[i - 1];
                double[] gip1 = g[i + 1];
                for (int j = 1; j < nm1; j++) {
                    // note: using temporaries as in original
                    double t1 = gim1[j];
                    double t2 = gip1[j];
                    double t3 = gi[j - 1];
                    double t4 = gi[j + 1];
                    double t5 = gi[j];
                    gi[j] = omega_over_four * (t1 + t2 + t3 + t4) + one_minus_omega * t5;
                }
            }
        }
    }

    // -------------------------
    // Monte Carlo
    // -------------------------
    private static double MonteCarlo_num_flops(int num_samples) {
        return num_samples * 4.0;
    }

    private static double MonteCarlo_integrate(int num_samples) {
        int under_curve = 0;
        for (int count = 0; count < num_samples; count++) {
            double x = RNG.nextDouble();
            double y = RNG.nextDouble();
            if (x * x + y * y <= 1.0) under_curve++;
        }
        return (double) under_curve / (double) num_samples * 4.0;
    }

    // -------------------------
    // Sparse Comp Row
    // -------------------------
    private static double SparseCompRow_num_flops(int n, int nz, int num_iterations) {
        int actual_nz = (nz / n) * n;
        return actual_nz * 2.0 * num_iterations;
    }

    private static void SparseCompRow_matmult(int m, double[] y, double[] val, double[] x, int[] row, int[] col, int num_iterations) {
        for (int reps = 0; reps < num_iterations; reps++) {
            for (int r = 0; r < m; r++) {
                int rowr = row[r];
                int rowrp1 = row[r + 1];
                double sum = 0.0;
                for (int i = rowr; i < rowrp1; i++) {
                    sum += x[col[i]] * val[i];
                }
                y[r] = sum;
            }
        }
    }

    // -------------------------
    // LU factorization
    // -------------------------
    private static double LU_num_flops(int N) {
        return 2.0 * (double) N * (double) N * (double) N / 3.0;
    }

    // returns 0 success, 1 failure
    private static int LU_factor(int M, int N, double[][] a, int[] pivot) {
        int minmn = Math.min(M, N);

        for (int j = 0; j < minmn; j++) {
            int jp = j;
            double t = Math.abs(a[j][j]);
            for (int i = j + 1; i < M; i++) {
                double ab = Math.abs(a[i][j]);
                if (ab > t) {
                    jp = i;
                    t = ab;
                }
            }

            pivot[j] = jp;
            if (a[jp][j] == 0.0) return 1; // factorization failed

            if (jp != j) {
                // swap row references
                double[] tmp = a[jp];
                a[jp] = a[j];
                a[j] = tmp;
            }

            if (j < M - 1) {
                double recp = 1.0 / a[j][j];
                for (int k = j + 1; k < M; k++) a[k][j] *= recp;
            }

            if (j < minmn - 1) {
                for (int ii = j + 1; ii < M; ii++) {
                    double[] aii = a[ii];
                    double[] aj = a[j];
                    double aiiJ = aii[j];
                    for (int jj = j + 1; jj < N; jj++) {
                        aii[jj] -= aiiJ * aj[jj];
                    }
                }
            }
        }

        return 0;
    }

    // -------------------------
    // Benchmark class (converted to static-like methods)
    // -------------------------
    static class TScimark {
        private final double[] flops = new double[6]; // 0 composite, 1-5 tests

        public TScimark() {}

        public double measureFFT(int p_n, double p_min_time) {
            int two_n = 2 * p_n;
            double[] x = randomVector(two_n);

            int cycles = 1;
            double t_sum, t;
            while (true) {
                double t0 = seconds();
                for (int i = 0; i < cycles; i++) {
                    FFT_transform(two_n, x);
                    FFT_inverse(two_n, x);
                }
                t_sum = seconds() - t0;
                if (t_sum >= p_min_time) break;
                cycles *= 2;
            }
            return FFT_num_flops(p_n) * cycles / t_sum * 0.000001;
        }

        public double measureSOR(int p_n, double p_min_time) {
            double[][] g = randomMatrix(p_n, p_n);
            int cycles = 1;
            double t_sum;
            while (true) {
                double t0 = seconds();
                SOR_execute(p_n, p_n, 1.25, g, cycles);
                t_sum = seconds() - t0;
                if (t_sum >= p_min_time) break;
                cycles *= 2;
            }
            return SOR_num_flops(p_n, p_n, cycles) / t_sum * 0.000001;
        }

        public double measureMonteCarlo(double p_min_time) {
            int cycles = 1;
            double t_sum;
            while (true) {
                double t0 = seconds();
                MonteCarlo_integrate(cycles);
                t_sum = seconds() - t0;
                if (t_sum >= p_min_time) break;
                cycles *= 2;
            }
            return MonteCarlo_num_flops(cycles) / t_sum * 0.000001;
        }

        public double measureSparseMatMult(int p_m, int p_nz, double p_min_time) {
            double[] x = randomVector(p_m);
            double[] y = new double[p_m];
            int nr = p_nz / p_m;
            int anz = nr * p_m;
            double[] val = randomVector(anz);
            int[] col = new int[anz];
            int[] row = new int[p_m + 1];
            row[0] = 0;
            for (int r_loop = 0; r_loop < p_m; r_loop++) {
                int rowr = row[r_loop];
                int step = r_loop / Math.max(1, nr);
                row[r_loop + 1] = rowr + nr;
                if (step < 1) step = 1;
                for (int i = 0; i < nr; i++) {
                    col[rowr + i] = (i * step);
                }
            }

            int cycles = 1;
            double t_sum;
            while (true) {
                double t0 = seconds();
                SparseCompRow_matmult(p_m, y, val, x, row, col, cycles);
                t_sum = seconds() - t0;
                if (t_sum >= p_min_time) break;
                cycles *= 2;
            }
            return SparseCompRow_num_flops(p_m, p_nz, cycles) / t_sum * 0.000001;
        }

        public double measureLU(int p_n, double p_min_time) {
            double[][] a = randomMatrix(p_n, p_n);
            double[][] lu = new double[p_n][p_n];
            int[] pivot = new int[p_n];

            int cycles = 1;
            double t_sum;
            while (true) {
                double t0 = seconds();
                for (int i = 0; i < cycles; i++) {
                    // copy a -> lu
                    for (int y = 0; y < p_n; y++) {
                        System.arraycopy(a[y], 0, lu[y], 0, p_n);
                    }
                    LU_factor(p_n, p_n, lu, pivot);
                }
                t_sum = seconds() - t0;
                if (t_sum >= p_min_time) break;
                cycles *= 2;
            }
            return LU_num_flops(p_n) * (double) cycles / t_sum * 0.000001;
        }

        public void Run() {
            final double min_time = 2.0;

            System.out.println("** ------------------------------------------------------------- **");
            System.out.println("**            SciMark2 Numeric Benchmark in Express              **");
            System.out.println("** ------------------------------------------------------------- **");
            System.out.println("Mininum running time per benchmark = " + min_time + " seconds");
            System.out.println();
            System.out.println("Running benchmarks...");
            this.flops[1] = this.measureFFT(LG_FFT_SIZE, min_time);
            System.out.println("FFT measured");
            this.flops[2] = this.measureSOR(LG_SOR_SIZE, min_time);
            System.out.println("SOR measured");
            this.flops[3] = this.measureMonteCarlo(min_time);
            System.out.println("MC measured");
            this.flops[4] = this.measureSparseMatMult(LG_SPARSE_SIZE_M, LG_SPARSE_SIZE_NZ, min_time);
            System.out.println("Sparse measured");
            this.flops[5] = this.measureLU(LG_LU_SIZE, min_time);
            System.out.println("LU measured");
            this.flops[0] = (this.flops[1] + this.flops[2] + this.flops[3] + this.flops[4] + this.flops[5]) / 5.0;

            System.out.println();
            System.out.println("/-----------------------------------------------------------------\\");
            System.out.printf("| Composite Score         MFlops: %.6f%n", this.flops[0]);
            System.out.printf("| Fast Fourier Transform  MFlops: %.6f  (N=%d)%n", this.flops[1], LG_FFT_SIZE);
            System.out.printf("| Succ. Over-Relaxation   MFlops: %.6f  (%dx%d)%n", this.flops[2], LG_SOR_SIZE, LG_SOR_SIZE);
            System.out.printf("| Monte Carlo             MFlops: %.6f%n", this.flops[3]);
            System.out.printf("| Sparse Matrix Mul.      MFlops: %.6f  (N=%d, nz=%d)%n", this.flops[4], LG_SPARSE_SIZE_M, LG_SPARSE_SIZE_NZ);
            System.out.printf("| LU Factorization        MFlops: %.6f  (N=%d)%n", this.flops[5], LG_LU_SIZE);
            System.out.println("\\-----------------------------------------------------------------/");
        }
    }

    // -------------------------
    // Main entry
    // -------------------------
    public static void main(String[] args) {
        TScimark bench = new TScimark();
        bench.Run();
    }
}