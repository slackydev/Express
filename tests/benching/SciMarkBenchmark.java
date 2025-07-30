// SciMark-style benchmark in Java
public class SciMarkBenchmark {

    // Configurable large sizes
    static final int LG_FFT_SIZE = 1024 * 512; // Mapped from Int32/Int64, fits int
    static final int LG_SOR_SIZE = 300;
    static final long LG_SPARSE_SIZE = 10000000L; // Use long for large numbers
    static final int LG_LU_SIZE = 400;

    static final long N = 1024 * 1024; // Use long for N

    // Global variables for Rand()
    private static long seed = 123456L;
    private static final double PI = 3.14159265358979;
    private static final double TWO_PI = 2.0 * PI;

    // Arrays for KernelDot
    private static double[] A;
    private static double[] B;

    // --- Helper Functions ---

    // Timer helper
    public static double timeNow() {
        // GetTickCount() returns milliseconds, so divide by 1000.0 for seconds.
        return System.currentTimeMillis() / 1000.0;
    }

    // log2 helper (not used in the provided benchmark, but included for completeness)
    public static double log2(double x) {
        return Math.log(x) / Math.log(2.0);
    }

    // Custom Random generator (Park-Miller LCG)
    public static double rand() {
        // This is a specific implementation of a Linear Congruential Generator (LCG)
        // known as MINSTD.
        // Modulus M = 2^31 - 1 = 2147483647
        // Multiplier A = 16807
        // The original Express code uses division and modulo operations that
        // are equivalent to the standard Park-Miller LCG.
        seed = (16807L * (seed % 127773L)) - 2836L * (seed / 127773L);
        if (seed <= 0) {
            seed = seed + 2147483647L;
        }
        return seed / 2147483647.0;
    }

    // Initialize arrays A and B with random values
    public static void initArrays() {
        A = new double[(int) N]; // Cast N to int for array size, assuming N fits int max
        B = new double[(int) N];
        for (int i = 0; i < N; i++) {
            A[i] = rand();
            B[i] = rand();
        }
    }

    // --- Kernels ---

    public static double kernelDot(double[] A, double[] B, long n) {
        double sum = 0.0;
        for (int i = 0; i < n; i++) {
            sum = sum + A[i] * B[i];
        }
        return sum;
    }

    public static double kernelSOR(int nx, int ny, double omega) {
        double[] grid = new double[nx * ny];
        double[] old = new double[nx * ny];

        for (int i = 0; i < nx * ny; i++) {
            grid[i] = rand();
        }

        int iterations = nx; // Express code uses nx as iterations
        for (int it = 0; it < iterations; it++) {
            for (int y = 1; y < ny - 1; y++) {
                for (int x = 1; x < nx - 1; x++) {
                    int idx = y * nx + x;
                    old[idx] = grid[idx];
                    grid[idx] = old[idx] + omega * (old[idx - 1] + old[idx + 1] + old[idx - nx] + old[idx + nx] - 4.0 * old[idx]);
                }
            }
        }
        return grid[(ny / 2) * nx + (nx / 2)]; // Integer division for /
    }

    public static double kernelFFT(int _n) {
        // TWO_PI is already defined as a static final field
        double n_double = _n; // Cast to double for calculations
        double sum = 0.0;
        for (double k = 0.0; k < n_double; k += 1.0) { // Use double for k
            sum = sum + Math.sin(TWO_PI * k / n_double) + Math.cos(TWO_PI * k / n_double);
        }
        return sum;
    }

    public static double kernelMonteCarlo(long samples) {
        long count = 0;
        for (int i = 0; i < samples; i++) { // Loop counter can be int if samples fits, or long
            double x = rand() * 2.0 - 1.0;
            double y = rand() * 2.0 - 1.0;
            if (x * x + y * y <= 1.0) {
                count = count + 1;
            }
        }
        return 4.0 * (double) count / (double) samples; // Explicit cast to double for division
    }

    public static double kernelLU(int n) { // n is Int64 in Express, but LG_LU_SIZE is 400, fits int
        double[] mat = new double[n * n];
        for (int i = 0; i < n * n; i++) {
            mat[i] = rand();
        }

        for (int k = 0; k < n; k++) {
            for (int i = k + 1; i < n; i++) {
                mat[i * n + k] = mat[i * n + k] / mat[k * n + k];
                for (int j = k + 1; j < n; j++) {
                    mat[i * n + j] = mat[i * n + j] - mat[i * n + k] * mat[k * n + j];
                }
            }
        }
        return mat[(n - 1) * n + (n - 1)];
    }

    // --- Benchmark Runner ---

    public static void runBenchmark() {
        double dot, sor, fft, mc, lu;
        double dot_flops, sor_flops, fft_flops, mc_flops, lu_flops;
        double t0, t1, total;

        initArrays(); // Initialize A and B once for Dot Product

        // Dot Product
        t0 = timeNow();
        dot = kernelDot(A, B, N);
        t1 = timeNow();
        dot_flops = (2.0 * N) / (t1 - t0) / 1000000.0;
        System.out.println("DotProd: " + dot_flops);

        // SOR
        t0 = timeNow();
        sor = kernelSOR(LG_SOR_SIZE, LG_SOR_SIZE, 1.25);
        t1 = timeNow();
        int sor_iter = LG_SOR_SIZE; // Express uses nx as iterations
        sor_flops = (6.0 * (LG_SOR_SIZE - 1) * (LG_SOR_SIZE - 1) * sor_iter) / (t1 - t0) / 1000000.0;
        System.out.println("SOR: " + sor_flops);

        // FFT
        int fft_n = LG_FFT_SIZE;
        t0 = timeNow();
        fft = kernelFFT(fft_n);
        t1 = timeNow();
        fft_flops = (4.0 * fft_n) / (t1 - t0) / 1000000.0;
        System.out.println("Init FFT: " + fft_flops);

        // Monte Carlo
        long samples = LG_SPARSE_SIZE;
        t0 = timeNow();
        mc = kernelMonteCarlo(samples);
        t1 = timeNow();
        mc_flops = (4.0 * samples) / (t1 - t0) / 1000000.0;
        System.out.println("MonteCarlo: " + mc_flops);

        // LU
        int lu_n = LG_LU_SIZE;
        t0 = timeNow();
        lu = kernelLU(lu_n);
        t1 = timeNow();
        lu_flops = ((2.0 / 3.0) * lu_n * lu_n * lu_n) / (t1 - t0) / 1000000.0;
        System.out.println("LU: " + lu_flops);

        total = (dot_flops + sor_flops + fft_flops + mc_flops + lu_flops) / 5.0;
        System.out.println("Summary: " + total);
        System.out.println("Executed in " + (t1 - t0) + " ms"); // This will only print the last kernel's time
                                                              // Let's calculate total time for all kernels
        // To get total time for all kernels, you need to sum up individual kernel times.
        // Or, measure the time for the entire runBenchmark() call.
        // For simplicity, I'll just print the last kernel's time as per your original code.
    }

    public static void main(String[] args) {
        // Measure total execution time of the benchmark suite
        double overallT0 = timeNow();
        runBenchmark();
        double overallT1 = timeNow();
        System.out.println("Total Benchmark Execution Time: " + (overallT1 - overallT0) + " seconds");
    }
}