#include <cuda_runtime.h>
#include <device_launch_parameters.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Задача: нахождение матрицы алгебраических дополнений (CUDA).
 *
 * Дано:  матрица A размером n x n, элементы в [-10^5, 10^5], 2 <= n <= 100.
 * Найти: B[i][j] = (-1)^(i+j) * det(M_{ij}).
 *
 * Каждый CUDA-поток вычисляет один элемент матрицы B:
 *   1) извлекает минор (n-1) x (n-1) в рабочую область,
 *   2) вычисляет определитель через LU-разложение с выбором ведущего элемента,
 *   3) сохраняет результат с учётом знака.
 *
 * Для экономии памяти GPU элементы обрабатываются пакетами (batch).
 */

#define CUDA_CHECK(call) do {                                              \
    cudaError_t err = (call);                                              \
    if (err != cudaSuccess) {                                              \
        fprintf(stderr, "CUDA error at %s:%d: %s\n",                      \
                __FILE__, __LINE__, cudaGetErrorString(err));              \
        exit(EXIT_FAILURE);                                                \
    }                                                                      \
} while (0)

#define MAX_N       100
#define DEF_THREADS 256

__device__ double dev_determinant(double *mat, int n) {
    double det = 1.0;
    for (int k = 0; k < n; k++) {
        int pivot = k;
        double mx = fabs(mat[k * n + k]);
        for (int r = k + 1; r < n; r++) {
            double v = fabs(mat[r * n + k]);
            if (v > mx) { mx = v; pivot = r; }
        }
        if (pivot != k) {
            for (int c = 0; c < n; c++) {
                double tmp        = mat[k * n + c];
                mat[k * n + c]    = mat[pivot * n + c];
                mat[pivot * n + c] = tmp;
            }
            det = -det;
        }
        double diag = mat[k * n + k];
        if (fabs(diag) < 1e-15) return 0.0;
        det *= diag;
        for (int r = k + 1; r < n; r++) {
            double f = mat[r * n + k] / diag;
            for (int c = k + 1; c < n; c++)
                mat[r * n + c] -= f * mat[k * n + c];
        }
    }
    return det;
}

__global__ void cofactor_kernel(const double *A, double *B,
                                double *workspace, int n,
                                int batch_start, int batch_count) {
    int tid = blockIdx.x * blockDim.x + threadIdx.x;
    if (tid >= batch_count) return;

    int idx = batch_start + tid;
    int i   = idx / n;
    int j   = idx % n;
    int m   = n - 1;

    double *sub = workspace + (size_t)tid * m * m;

    int sr = 0;
    for (int r = 0; r < n; r++) {
        if (r == i) continue;
        int sc = 0;
        for (int c = 0; c < n; c++) {
            if (c == j) continue;
            sub[sr * m + sc] = A[r * n + c];
            sc++;
        }
        sr++;
    }

    double det  = dev_determinant(sub, m);
    double sign = ((i + j) % 2 == 0) ? 1.0 : -1.0;
    B[i * n + j] = sign * det;
}

static void generate_matrix(double *A, int n, unsigned int seed) {
    srand(seed);
    for (int i = 0; i < n * n; i++)
        A[i] = ((double)rand() / RAND_MAX) * 2e5 - 1e5;
}

static double host_determinant(double *mat, int n) {
    double det = 1.0;
    for (int k = 0; k < n; k++) {
        int pivot = k;
        double mx = fabs(mat[k * n + k]);
        for (int r = k + 1; r < n; r++) {
            double v = fabs(mat[r * n + k]);
            if (v > mx) { mx = v; pivot = r; }
        }
        if (pivot != k) {
            for (int c = 0; c < n; c++) {
                double tmp        = mat[k * n + c];
                mat[k * n + c]    = mat[pivot * n + c];
                mat[pivot * n + c] = tmp;
            }
            det = -det;
        }
        double diag = mat[k * n + k];
        if (fabs(diag) < 1e-15) return 0.0;
        det *= diag;
        for (int r = k + 1; r < n; r++) {
            double f = mat[r * n + k] / diag;
            for (int c = k + 1; c < n; c++)
                mat[r * n + c] -= f * mat[k * n + c];
        }
    }
    return det;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <n> [threads] [csv_file]\n", argv[0]);
        return 1;
    }

    int n       = atoi(argv[1]);
    int threads = (argc >= 3) ? atoi(argv[2]) : DEF_THREADS;
    const char *csv_path = (argc >= 4) ? argv[3] : NULL;

    if (n < 2 || n > MAX_N) {
        fprintf(stderr, "Error: n must be in [2, %d], got %d\n", MAX_N, n);
        return 1;
    }

    int total = n * n;
    int m     = n - 1;
    size_t sub_bytes = (size_t)m * m * sizeof(double);

    double *A = (double *)malloc((size_t)n * n * sizeof(double));
    double *B = (double *)malloc((size_t)n * n * sizeof(double));
    generate_matrix(A, n, 42);

    double *d_A, *d_B, *d_work;
    CUDA_CHECK(cudaMalloc(&d_A, (size_t)n * n * sizeof(double)));
    CUDA_CHECK(cudaMalloc(&d_B, (size_t)n * n * sizeof(double)));
    CUDA_CHECK(cudaMemcpy(d_A, A, (size_t)n * n * sizeof(double),
                          cudaMemcpyHostToDevice));

    size_t free_mem, total_mem;
    CUDA_CHECK(cudaMemGetInfo(&free_mem, &total_mem));
    size_t reserved = (size_t)n * n * sizeof(double) * 2 + (1 << 20);
    size_t avail = (free_mem > reserved) ? free_mem - reserved : sub_bytes;
    int batch_size = (int)(avail / sub_bytes);
    if (batch_size > total) batch_size = total;
    if (batch_size < 1) batch_size = 1;

    CUDA_CHECK(cudaMalloc(&d_work, (size_t)batch_size * sub_bytes));

    cudaEvent_t t0, t1;
    CUDA_CHECK(cudaEventCreate(&t0));
    CUDA_CHECK(cudaEventCreate(&t1));
    CUDA_CHECK(cudaEventRecord(t0));

    for (int start = 0; start < total; start += batch_size) {
        int count  = batch_size;
        if (start + count > total) count = total - start;
        int blocks = (count + threads - 1) / threads;
        cofactor_kernel<<<blocks, threads>>>(d_A, d_B, d_work,
                                             n, start, count);
        CUDA_CHECK(cudaGetLastError());
    }

    CUDA_CHECK(cudaEventRecord(t1));
    CUDA_CHECK(cudaEventSynchronize(t1));
    float elapsed_ms = 0;
    CUDA_CHECK(cudaEventElapsedTime(&elapsed_ms, t0, t1));

    CUDA_CHECK(cudaMemcpy(B, d_B, (size_t)n * n * sizeof(double),
                          cudaMemcpyDeviceToHost));

    /* ---------- verification: A * B^T == det(A) * I ---------- */
    double *A_copy = (double *)malloc((size_t)n * n * sizeof(double));
    memcpy(A_copy, A, (size_t)n * n * sizeof(double));
    double det_A = host_determinant(A_copy, n);
    free(A_copy);

    double max_err = 0.0;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            double sum = 0.0;
            for (int k = 0; k < n; k++)
                sum += A[i * n + k] * B[j * n + k];
            double expected = (i == j) ? det_A : 0.0;
            double err = fabs(sum - expected);
            if (err > max_err) max_err = err;
        }
    }
    double rel_err = (fabs(det_A) > 1e-15) ? max_err / fabs(det_A) : max_err;

    printf("n=%d  det(A)=%.6e  verify_err=%.6e  rel_err=%.6e  "
           "time=%.2f ms  batch=%d\n",
           n, det_A, max_err, rel_err, elapsed_ms, batch_size);

    if (n <= 5) {
        printf("\nMatrix A:\n");
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++)
                printf("%12.4f", A[i * n + j]);
            printf("\n");
        }
        printf("\nCofactor matrix B:\n");
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++)
                printf("%12.4f", B[i * n + j]);
            printf("\n");
        }
    }

    if (csv_path) {
        FILE *fp = fopen(csv_path, "a");
        if (fp) {
            fprintf(fp, "%d,cuda,%.4f,%.6e,%.6e\n",
                    n, elapsed_ms, max_err, rel_err);
            fclose(fp);
        }
    }

    free(A);
    free(B);
    CUDA_CHECK(cudaFree(d_A));
    CUDA_CHECK(cudaFree(d_B));
    CUDA_CHECK(cudaFree(d_work));
    CUDA_CHECK(cudaEventDestroy(t0));
    CUDA_CHECK(cudaEventDestroy(t1));
    return 0;
}
