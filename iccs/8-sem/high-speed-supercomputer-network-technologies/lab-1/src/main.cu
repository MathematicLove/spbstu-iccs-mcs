#include <cuda_runtime.h>

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <limits>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

#define CUDA_CHECK(call)                                                     \
    do {                                                                     \
        cudaError_t err__ = (call);                                          \
        if (err__ != cudaSuccess) {                                          \
            std::cerr << "CUDA error at " << __FILE__ << ":" << __LINE__     \
                      << " -> " << cudaGetErrorString(err__) << std::endl;   \
            std::exit(EXIT_FAILURE);                                         \
        }                                                                    \
    } while (0)

struct Options {
    int start = 1000;
    int step = 500;
    int count = 6;
    std::vector<int> sizes;
    int threads = 256;
    int max_iters = 10000;
    int repeat = 3;
    int warmup = 1;
    unsigned int seed = 42U;
    double eps = 1e-6;
    std::string csv_path;
};

struct Metrics {
    double elapsed_ms = std::numeric_limits<double>::max();
    int iterations = 0;
    double residual_inf = std::numeric_limits<double>::infinity();
    double x_error_inf = std::numeric_limits<double>::infinity();
    double gflops = 0.0;
    bool converged = false;
};

__global__ void jacobi_iteration(const double *a,
                                 const double *b,
                                 const double *x_in,
                                 double *x_out,
                                 int n,
                                 double eps,
                                 int *converged) {
    const int row = blockIdx.x * blockDim.x + threadIdx.x;
    if (row >= n) {
        return;
    }

    const int row_offset = row * n;
    double sum = 0.0;
    for (int col = 0; col < n; ++col) {
        if (col != row) {
            sum += a[row_offset + col] * x_in[col];
        }
    }

    const double next = (b[row] - sum) / a[row_offset + row];
    x_out[row] = next;
    if (fabs(next - x_in[row]) > eps) {
        atomicAnd(converged, 0);
    }
}

static void print_usage(const char *program) {
    std::cout
        << "Usage: " << program << " [options]\n"
        << "Options:\n"
        << "  --start N       First matrix size (default: 1000)\n"
        << "  --step N        Size increment (default: 500)\n"
        << "  --count N       Number of tests (default: 6)\n"
        << "  --sizes a,b,c   Comma-separated matrix sizes\n"
        << "  --threads N     Threads per block (default: 256)\n"
        << "  --max-iters N   Max Jacobi iterations (default: 10000)\n"
        << "  --eps X         Convergence epsilon (default: 1e-6)\n"
        << "  --repeat N      Timed repetitions, best is kept (default: 3)\n"
        << "  --warmup N      Warmup repetitions (default: 1)\n"
        << "  --seed N        RNG seed (default: 42)\n"
        << "  --csv PATH      Write CSV summary to PATH\n"
        << "  --help          Print this help\n";
}

static bool parse_int_arg(const std::string &text, int &out) {
    try {
        size_t pos = 0;
        const int parsed = std::stoi(text, &pos);
        if (pos != text.size()) {
            return false;
        }
        out = parsed;
        return true;
    } catch (...) {
        return false;
    }
}

static bool parse_uint_arg(const std::string &text, unsigned int &out) {
    try {
        size_t pos = 0;
        const unsigned long parsed = std::stoul(text, &pos);
        if (pos != text.size()) {
            return false;
        }
        out = static_cast<unsigned int>(parsed);
        return true;
    } catch (...) {
        return false;
    }
}

static bool parse_double_arg(const std::string &text, double &out) {
    try {
        size_t pos = 0;
        const double parsed = std::stod(text, &pos);
        if (pos != text.size()) {
            return false;
        }
        out = parsed;
        return true;
    } catch (...) {
        return false;
    }
}

static bool parse_sizes_arg(const std::string &text, std::vector<int> &sizes) {
    std::stringstream ss(text);
    std::string token;
    std::vector<int> parsed;

    while (std::getline(ss, token, ',')) {
        token.erase(
            std::remove_if(token.begin(),
                           token.end(),
                           [](unsigned char c) { return std::isspace(c) != 0; }),
            token.end());
        if (token.empty()) {
            continue;
        }

        int value = 0;
        if (!parse_int_arg(token, value) || value <= 0) {
            return false;
        }
        parsed.push_back(value);
    }

    if (parsed.empty()) {
        return false;
    }

    sizes = parsed;
    return true;
}

static bool parse_options(int argc, char **argv, Options &options) {
    for (int i = 1; i < argc; ++i) {
        const std::string arg = argv[i];
        auto require_value = [&](const char *name) -> const char * {
            if (i + 1 >= argc) {
                std::cerr << "Missing value for " << name << '\n';
                return nullptr;
            }
            return argv[++i];
        };

        if (arg == "--help") {
            print_usage(argv[0]);
            return false;
        }
        if (arg == "--start") {
            const char *v = require_value("--start");
            if (!v || !parse_int_arg(v, options.start) || options.start <= 0) {
                std::cerr << "Invalid --start value\n";
                return false;
            }
            continue;
        }
        if (arg == "--step") {
            const char *v = require_value("--step");
            if (!v || !parse_int_arg(v, options.step) || options.step <= 0) {
                std::cerr << "Invalid --step value\n";
                return false;
            }
            continue;
        }
        if (arg == "--count") {
            const char *v = require_value("--count");
            if (!v || !parse_int_arg(v, options.count) || options.count <= 0) {
                std::cerr << "Invalid --count value\n";
                return false;
            }
            continue;
        }
        if (arg == "--threads") {
            const char *v = require_value("--threads");
            if (!v || !parse_int_arg(v, options.threads) || options.threads <= 0 ||
                options.threads > 1024) {
                std::cerr << "Invalid --threads value\n";
                return false;
            }
            continue;
        }
        if (arg == "--max-iters") {
            const char *v = require_value("--max-iters");
            if (!v || !parse_int_arg(v, options.max_iters) ||
                options.max_iters <= 0) {
                std::cerr << "Invalid --max-iters value\n";
                return false;
            }
            continue;
        }
        if (arg == "--eps") {
            const char *v = require_value("--eps");
            if (!v || !parse_double_arg(v, options.eps) || options.eps <= 0.0) {
                std::cerr << "Invalid --eps value\n";
                return false;
            }
            continue;
        }
        if (arg == "--repeat") {
            const char *v = require_value("--repeat");
            if (!v || !parse_int_arg(v, options.repeat) || options.repeat <= 0) {
                std::cerr << "Invalid --repeat value\n";
                return false;
            }
            continue;
        }
        if (arg == "--warmup") {
            const char *v = require_value("--warmup");
            if (!v || !parse_int_arg(v, options.warmup) || options.warmup < 0) {
                std::cerr << "Invalid --warmup value\n";
                return false;
            }
            continue;
        }
        if (arg == "--seed") {
            const char *v = require_value("--seed");
            if (!v || !parse_uint_arg(v, options.seed)) {
                std::cerr << "Invalid --seed value\n";
                return false;
            }
            continue;
        }
        if (arg == "--sizes") {
            const char *v = require_value("--sizes");
            if (!v || !parse_sizes_arg(v, options.sizes)) {
                std::cerr << "Invalid --sizes value\n";
                return false;
            }
            continue;
        }
        if (arg == "--csv") {
            const char *v = require_value("--csv");
            if (!v) {
                return false;
            }
            options.csv_path = v;
            continue;
        }

        std::cerr << "Unknown option: " << arg << '\n';
        return false;
    }

    if (options.sizes.empty()) {
        options.sizes.reserve(static_cast<size_t>(options.count));
        for (int i = 0; i < options.count; ++i) {
            options.sizes.push_back(options.start + i * options.step);
        }
    }

    return true;
}

static double next_random_value(uint32_t &state) {
    state = state * 1664525U + 1013904223U;
    const double normalized =
        static_cast<double>(state & 0x00FFFFFFU) /
        static_cast<double>(0x00FFFFFFU);
    return normalized * 2.0 - 1.0;
}

static void build_system(int n,
                         unsigned int seed,
                         std::vector<double> &a,
                         std::vector<double> &x_true,
                         std::vector<double> &b) {
    const size_t nn = static_cast<size_t>(n) * static_cast<size_t>(n);
    a.assign(nn, 0.0);
    x_true.assign(static_cast<size_t>(n), 0.0);
    b.assign(static_cast<size_t>(n), 0.0);

    uint32_t state = seed;
    for (int i = 0; i < n; ++i) {
        x_true[static_cast<size_t>(i)] = next_random_value(state);
    }

    for (int row = 0; row < n; ++row) {
        double off_diag_sum = 0.0;
        const size_t row_offset = static_cast<size_t>(row) * static_cast<size_t>(n);
        for (int col = 0; col < n; ++col) {
            if (col == row) {
                continue;
            }
            const double value = next_random_value(state);
            a[row_offset + static_cast<size_t>(col)] = value;
            off_diag_sum += std::fabs(value);
        }
        a[row_offset + static_cast<size_t>(row)] =
            off_diag_sum + 2.0 + std::fabs(next_random_value(state));
    }

    for (int row = 0; row < n; ++row) {
        const size_t row_offset = static_cast<size_t>(row) * static_cast<size_t>(n);
        double sum = 0.0;
        for (int col = 0; col < n; ++col) {
            sum += a[row_offset + static_cast<size_t>(col)] *
                   x_true[static_cast<size_t>(col)];
        }
        b[static_cast<size_t>(row)] = sum;
    }
}

static double compute_residual_inf(const std::vector<double> &a,
                                   const std::vector<double> &x,
                                   const std::vector<double> &b,
                                   int n) {
    double max_residual = 0.0;
    for (int row = 0; row < n; ++row) {
        const size_t row_offset = static_cast<size_t>(row) * static_cast<size_t>(n);
        double sum = 0.0;
        for (int col = 0; col < n; ++col) {
            sum += a[row_offset + static_cast<size_t>(col)] *
                   x[static_cast<size_t>(col)];
        }
        max_residual =
            std::max(max_residual, std::fabs(sum - b[static_cast<size_t>(row)]));
    }
    return max_residual;
}

static double compute_x_error_inf(const std::vector<double> &x,
                                  const std::vector<double> &x_true) {
    double max_error = 0.0;
    for (size_t i = 0; i < x.size(); ++i) {
        max_error = std::max(max_error, std::fabs(x[i] - x_true[i]));
    }
    return max_error;
}

static Metrics solve_once(const Options &options,
                          int n,
                          const std::vector<double> &a,
                          const std::vector<double> &b,
                          const std::vector<double> &x_true,
                          double *d_a,
                          double *d_b,
                          double *d_x_old,
                          double *d_x_new,
                          int *d_converged) {
    Metrics metrics;
    std::vector<double> x(static_cast<size_t>(n), 0.0);

    CUDA_CHECK(cudaMemset(d_x_old, 0, static_cast<size_t>(n) * sizeof(double)));
    CUDA_CHECK(cudaMemset(d_x_new, 0, static_cast<size_t>(n) * sizeof(double)));

    cudaEvent_t start = nullptr;
    cudaEvent_t stop = nullptr;
    CUDA_CHECK(cudaEventCreate(&start));
    CUDA_CHECK(cudaEventCreate(&stop));
    CUDA_CHECK(cudaEventRecord(start, 0));

    const int block = options.threads;
    const int grid = (n + block - 1) / block;

    int h_converged = 0;
    int iterations = 0;
    while (iterations < options.max_iters) {
        h_converged = 1;
        CUDA_CHECK(cudaMemcpy(
            d_converged, &h_converged, sizeof(int), cudaMemcpyHostToDevice));

        jacobi_iteration<<<grid, block>>>(
            d_a, d_b, d_x_old, d_x_new, n, options.eps, d_converged);
        CUDA_CHECK(cudaGetLastError());

        CUDA_CHECK(cudaMemcpy(
            &h_converged, d_converged, sizeof(int), cudaMemcpyDeviceToHost));

        std::swap(d_x_old, d_x_new);
        ++iterations;

        if (h_converged == 1) {
            metrics.converged = true;
            break;
        }
    }

    CUDA_CHECK(cudaEventRecord(stop, 0));
    CUDA_CHECK(cudaEventSynchronize(stop));

    float elapsed_ms = 0.0f;
    CUDA_CHECK(cudaEventElapsedTime(&elapsed_ms, start, stop));
    CUDA_CHECK(cudaEventDestroy(start));
    CUDA_CHECK(cudaEventDestroy(stop));

    CUDA_CHECK(cudaMemcpy(x.data(),
                          d_x_old,
                          static_cast<size_t>(n) * sizeof(double),
                          cudaMemcpyDeviceToHost));

    metrics.elapsed_ms = static_cast<double>(elapsed_ms);
    metrics.iterations = iterations;
    metrics.residual_inf = compute_residual_inf(a, x, b, n);
    metrics.x_error_inf = compute_x_error_inf(x, x_true);

    const double seconds = metrics.elapsed_ms / 1000.0;
    const double flops =
        (2.0 / 3.0) * static_cast<double>(n) * static_cast<double>(n) *
        static_cast<double>(n);
    metrics.gflops = (seconds > 0.0) ? (flops / seconds) / 1e9 : 0.0;
    return metrics;
}

static Metrics benchmark_size(const Options &options,
                              int n,
                              const std::vector<double> &a,
                              const std::vector<double> &b,
                              const std::vector<double> &x_true) {
    const size_t matrix_bytes =
        static_cast<size_t>(n) * static_cast<size_t>(n) * sizeof(double);
    const size_t vector_bytes = static_cast<size_t>(n) * sizeof(double);

    double *d_a = nullptr;
    double *d_b = nullptr;
    double *d_x_old = nullptr;
    double *d_x_new = nullptr;
    int *d_converged = nullptr;

    CUDA_CHECK(cudaMalloc(reinterpret_cast<void **>(&d_a), matrix_bytes));
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void **>(&d_b), vector_bytes));
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void **>(&d_x_old), vector_bytes));
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void **>(&d_x_new), vector_bytes));
    CUDA_CHECK(cudaMalloc(reinterpret_cast<void **>(&d_converged), sizeof(int)));

    CUDA_CHECK(cudaMemcpy(
        d_a, a.data(), matrix_bytes, cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(
        d_b, b.data(), vector_bytes, cudaMemcpyHostToDevice));

    for (int w = 0; w < options.warmup; ++w) {
        (void)solve_once(options, n, a, b, x_true, d_a, d_b, d_x_old, d_x_new,
                         d_converged);
    }

    Metrics best;
    for (int run = 0; run < options.repeat; ++run) {
        Metrics current = solve_once(
            options, n, a, b, x_true, d_a, d_b, d_x_old, d_x_new, d_converged);
        if (current.elapsed_ms < best.elapsed_ms) {
            best = current;
        }
    }

    CUDA_CHECK(cudaFree(d_a));
    CUDA_CHECK(cudaFree(d_b));
    CUDA_CHECK(cudaFree(d_x_old));
    CUDA_CHECK(cudaFree(d_x_new));
    CUDA_CHECK(cudaFree(d_converged));

    return best;
}

int main(int argc, char **argv) {
    Options options;
    if (!parse_options(argc, argv, options)) {
        return 0;
    }

    int device = 0;
    CUDA_CHECK(cudaGetDevice(&device));
    cudaDeviceProp prop{};
    CUDA_CHECK(cudaGetDeviceProperties(&prop, device));

    std::ofstream csv_file;
    if (!options.csv_path.empty()) {
        csv_file.open(options.csv_path.c_str(), std::ios::out | std::ios::trunc);
        if (!csv_file) {
            std::cerr << "Failed to open CSV file: " << options.csv_path << '\n';
            return 1;
        }
        csv_file
            << "n,elapsed_ms,iterations,residual_inf,x_error_inf,gflops,converged\n";
    }

    std::cout << "CUDA Jacobi LINPACK-like benchmark\n";
    std::cout << "device = " << prop.name << ", compute capability = "
              << prop.major << '.' << prop.minor << ", threads = "
              << options.threads << ", repeat = " << options.repeat
              << ", warmup = " << options.warmup
              << ", eps = " << std::scientific << options.eps << std::defaultfloat
              << "\n\n";

    std::cout << std::left << std::setw(8) << "N" << std::setw(14) << "Time(ms)"
              << std::setw(12) << "Iter" << std::setw(18) << "ResidualInf"
              << std::setw(18) << "XerrInf" << std::setw(14) << "GFLOPS"
              << "Status\n";

    for (size_t i = 0; i < options.sizes.size(); ++i) {
        const int n = options.sizes[i];
        if (n <= 0) {
            continue;
        }

        std::vector<double> a;
        std::vector<double> x_true;
        std::vector<double> b;
        build_system(
            n, options.seed + static_cast<unsigned int>(n), a, x_true, b);

        Metrics metrics = benchmark_size(options, n, a, b, x_true);

        std::cout << std::left << std::setw(8) << n << std::setw(14)
                  << std::fixed << std::setprecision(4) << metrics.elapsed_ms
                  << std::setw(12) << metrics.iterations << std::setw(18)
                  << std::scientific << std::setprecision(3)
                  << metrics.residual_inf << std::setw(18) << metrics.x_error_inf
                  << std::setw(14) << std::fixed << std::setprecision(3)
                  << metrics.gflops
                  << (metrics.converged ? "converged" : "max_iters") << '\n';

        if (csv_file) {
            csv_file << n << ',' << std::fixed << std::setprecision(6)
                     << metrics.elapsed_ms << ',' << metrics.iterations << ','
                     << std::scientific << std::setprecision(8)
                     << metrics.residual_inf << ',' << metrics.x_error_inf << ','
                     << std::fixed << std::setprecision(6) << metrics.gflops
                     << ',' << (metrics.converged ? 1 : 0) << '\n';
        }
    }

    if (csv_file) {
        csv_file.close();
    }

    return 0;
}
