#include <math.h>
#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Задача: нахождение матрицы алгебраических дополнений.
 *
 * Дано:  матрица A размером n x n, элементы в [-10^5, 10^5], 2 <= n <= 100.
 * Найти: матрицу B, где B[i][j] = (-1)^(i+j) * det(M_{ij}),
 *        M_{ij} — минор (матрица A без строки i и столбца j).
 *
 * Верификация: A * B^T = det(A) * I.
 *
 * MPI-параллелизация: строки матрицы B распределяются между процессами.
 */

static double determinant_lu(double *mat, int n) {
    double det = 1.0;
    for (int k = 0; k < n; k++) {
        int pivot = k;
        double max_val = fabs(mat[k * n + k]);
        for (int r = k + 1; r < n; r++) {
            double v = fabs(mat[r * n + k]);
            if (v > max_val) { max_val = v; pivot = r; }
        }
        if (pivot != k) {
            for (int c = 0; c < n; c++) {
                double tmp = mat[k * n + c];
                mat[k * n + c] = mat[pivot * n + c];
                mat[pivot * n + c] = tmp;
            }
            det = -det;
        }
        double diag = mat[k * n + k];
        if (fabs(diag) < 1e-15) return 0.0;
        det *= diag;
        for (int r = k + 1; r < n; r++) {
            double factor = mat[r * n + k] / diag;
            for (int c = k + 1; c < n; c++)
                mat[r * n + c] -= factor * mat[k * n + c];
        }
    }
    return det;
}

static void extract_minor(const double *A, double *sub, int n,
                           int skip_row, int skip_col) {
    int m = n - 1;
    int sr = 0;
    for (int r = 0; r < n; r++) {
        if (r == skip_row) continue;
        int sc = 0;
        for (int c = 0; c < n; c++) {
            if (c == skip_col) continue;
            sub[sr * m + sc] = A[r * n + c];
            sc++;
        }
        sr++;
    }
}

static void generate_matrix(double *A, int n, unsigned int seed) {
    srand(seed);
    for (int i = 0; i < n * n; i++)
        A[i] = ((double)rand() / RAND_MAX) * 2e5 - 1e5;
}

int main(int argc, char *argv[]) {
    MPI_Init(&argc, &argv);

    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (argc < 2) {
        if (rank == 0)
            fprintf(stderr,
                    "Usage: mpirun -np <P> %s <n> [csv_file]\n", argv[0]);
        MPI_Finalize();
        return 1;
    }

    int n = atoi(argv[1]);
    const char *csv_path = (argc >= 3) ? argv[2] : NULL;

    if (n < 2 || n > 100) {
        if (rank == 0)
            fprintf(stderr, "Error: n must be in [2, 100], got %d\n", n);
        MPI_Finalize();
        return 1;
    }

    double *A = (double *)malloc((size_t)n * n * sizeof(double));

    if (rank == 0)
        generate_matrix(A, n, 42);

    MPI_Bcast(A, n * n, MPI_DOUBLE, 0, MPI_COMM_WORLD);

    int base_rows = n / size;
    int remainder = n % size;
    int local_rows = base_rows + (rank < remainder ? 1 : 0);
    int start_row  = rank * base_rows + (rank < remainder ? rank : remainder);

    double *local_B = (double *)malloc((size_t)local_rows * n * sizeof(double));
    int m = n - 1;
    double *sub = (double *)malloc((size_t)m * m * sizeof(double));

    MPI_Barrier(MPI_COMM_WORLD);
    double t_start = MPI_Wtime();

    for (int li = 0; li < local_rows; li++) {
        int i = start_row + li;
        for (int j = 0; j < n; j++) {
            extract_minor(A, sub, n, i, j);
            double det = determinant_lu(sub, m);
            double sign = ((i + j) % 2 == 0) ? 1.0 : -1.0;
            local_B[li * n + j] = sign * det;
        }
    }

    double t_end = MPI_Wtime();
    double elapsed_ms = (t_end - t_start) * 1000.0;

    double *B = NULL;
    int *recvcounts = NULL, *displs = NULL;

    if (rank == 0) {
        B = (double *)malloc((size_t)n * n * sizeof(double));
        recvcounts = (int *)malloc((size_t)size * sizeof(int));
        displs     = (int *)malloc((size_t)size * sizeof(int));
        int off = 0;
        for (int r = 0; r < size; r++) {
            int rr = base_rows + (r < remainder ? 1 : 0);
            recvcounts[r] = rr * n;
            displs[r] = off;
            off += rr * n;
        }
    }

    MPI_Gatherv(local_B, local_rows * n, MPI_DOUBLE,
                B, recvcounts, displs, MPI_DOUBLE,
                0, MPI_COMM_WORLD);

    if (rank == 0) {
        double *A_copy = (double *)malloc((size_t)n * n * sizeof(double));
        memcpy(A_copy, A, (size_t)n * n * sizeof(double));
        double det_A = determinant_lu(A_copy, n);
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
        double rel_err = (fabs(det_A) > 1e-15)
                             ? max_err / fabs(det_A)
                             : max_err;

        printf("n=%d  det(A)=%.6e  verify_err=%.6e  rel_err=%.6e  "
               "time=%.2f ms  procs=%d\n",
               n, det_A, max_err, rel_err, elapsed_ms, size);

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
                fprintf(fp, "%d,%d,%.4f,%.6e,%.6e\n",
                        n, size, elapsed_ms, max_err, rel_err);
                fclose(fp);
            }
        }

        free(B);
        free(recvcounts);
        free(displs);
    }

    free(A);
    free(local_B);
    free(sub);

    MPI_Finalize();
    return 0;
}
