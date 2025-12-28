// src/cuda_ops.cu
#include <cuda_runtime.h>
#include <cstdio>
#include <cmath>
#include <cstdint>

extern "C" {

// kernel: compute flags and bin indices for items
__global__ void filter_and_bin_kernel(const double *d_v1, const double *d_v7, const double *d_v11, const double *d_amount,
                                      long long *d_bins, unsigned char *d_flags, int n) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx >= n) return;
    double a = d_amount[idx];
    if (a >= 9000.0) {
        double v1 = d_v1[idx];
        double ratio = v1 / 0.1;
        long long bin = (long long) floor(ratio);
        d_bins[idx] = bin;
        d_flags[idx] = 1;
    } else {
        d_flags[idx] = 0;
    }
}

// host wrapper
int cuda_filter_and_bin(const double *h_v1, const double *h_v7, const double *h_v11, const double *h_amount,
                        long long *h_bins_out, unsigned char *h_flags_out, int n) {
    if (n <= 0) return 0;
    cudaError_t err;
    double *d_v1=nullptr, *d_v7=nullptr, *d_v11=nullptr, *d_amount=nullptr;
    long long *d_bins=nullptr;
    unsigned char *d_flags=nullptr;

    size_t szd = n * sizeof(double);
    size_t szll = n * sizeof(long long);
    size_t szk = n * sizeof(unsigned char);

    err = cudaMalloc((void**)&d_v1, szd); if (err!=cudaSuccess) goto error;
    err = cudaMalloc((void**)&d_v7, szd); if (err!=cudaSuccess) goto error;
    err = cudaMalloc((void**)&d_v11, szd); if (err!=cudaSuccess) goto error;
    err = cudaMalloc((void**)&d_amount, szd); if (err!=cudaSuccess) goto error;
    err = cudaMalloc((void**)&d_bins, szll); if (err!=cudaSuccess) goto error;
    err = cudaMalloc((void**)&d_flags, szk); if (err!=cudaSuccess) goto error;

    err = cudaMemcpy(d_v1, h_v1, szd, cudaMemcpyHostToDevice); if (err!=cudaSuccess) goto error;
    err = cudaMemcpy(d_v7, h_v7, szd, cudaMemcpyHostToDevice); if (err!=cudaSuccess) goto error;
    err = cudaMemcpy(d_v11, h_v11, szd, cudaMemcpyHostToDevice); if (err!=cudaSuccess) goto error;
    err = cudaMemcpy(d_amount, h_amount, szd, cudaMemcpyHostToDevice); if (err!=cudaSuccess) goto error;

    int threads = 256;
    int blocks = (n + threads - 1) / threads;
    filter_and_bin_kernel<<<blocks, threads>>>(d_v1, d_v7, d_v11, d_amount, d_bins, d_flags, n);
    err = cudaDeviceSynchronize(); if (err!=cudaSuccess) goto error;

    err = cudaMemcpy(h_bins_out, d_bins, szll, cudaMemcpyDeviceToHost); if (err!=cudaSuccess) goto error;
    err = cudaMemcpy(h_flags_out, d_flags, szk, cudaMemcpyDeviceToHost); if (err!=cudaSuccess) goto error;

    // free
    cudaFree(d_v1); cudaFree(d_v7); cudaFree(d_v11); cudaFree(d_amount); cudaFree(d_bins); cudaFree(d_flags);
    return 0;
error:
    fprintf(stderr, "CUDA error: %s\n", cudaGetErrorString(err));
    if (d_v1) cudaFree(d_v1); if (d_v7) cudaFree(d_v7); if (d_v11) cudaFree(d_v11);
    if (d_amount) cudaFree(d_amount); if (d_bins) cudaFree(d_bins); if (d_flags) cudaFree(d_flags);
    return -1;
}
} // extern "C"
