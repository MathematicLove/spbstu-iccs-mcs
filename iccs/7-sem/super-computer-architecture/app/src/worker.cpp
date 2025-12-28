// src/worker.cpp
#include "worker.h"
#include "include/csv_utils.h"
#include <omp.h>
#include <iostream>
#include <sstream>
#include <cstdlib>
#include <cstring>

// declare cuda wrapper
extern "C" int cuda_filter_and_bin(const double*, const double*, const double*, const double*, long long*, unsigned char*, int);

Worker::Worker(const WorkerConfig &cfg)
    : cfg_(cfg)
{
    logger_.open(cfg_.log_dir + "/timings_rank_" + std::to_string(cfg.rank) + ".log");
    omp_set_num_threads(cfg_.omp_threads);
    t_global_.reset();
    log_timing("startup", 0.0);
}

Worker::~Worker() {}

void Worker::log_timing(const std::string &k, double sec) {
    logger_.log(k, sec);
}

void Worker::process_batch_lines(const std::vector<std::string> &lines) {
    Timer t;
    if (cfg_.has_gpu && cfg_.gpu_fraction > 0.0 && (int)lines.size() > 2) {
        // split
        size_t n = lines.size();
        size_t ngpu = std::max<size_t>(1, (size_t)(cfg_.gpu_fraction * (double)n));
        if (ngpu >= n) ngpu = n;
        std::vector<std::string> gpu_lines(lines.begin(), lines.begin() + ngpu);
        std::vector<std::string> cpu_lines(lines.begin() + ngpu, lines.end());
        if (!gpu_lines.empty()) process_gpu(gpu_lines);
        if (!cpu_lines.empty()) process_cpu(cpu_lines);
    } else {
        process_cpu(lines);
    }
    double sec = t.elapsed();
    log_timing("batch_total", sec);
}

void Worker::process_cpu(const std::vector<std::string> &lines) {
    Timer t;
    // parse lines in parallel
    #pragma omp parallel for schedule(dynamic,256)
    for (size_t i = 0; i < lines.size(); ++i) {
        auto fields = split_csv_line(lines[i]);
        // check indices
        int idx_v1 = cfg_.idx_v1;
        int idx_v7 = cfg_.idx_v7;
        int idx_v11 = cfg_.idx_v11;
        int idx_amount = cfg_.idx_amount;
        if (idx_v1 < 0 || idx_v7 < 0 || idx_v11 < 0 || idx_amount < 0) continue;
        if ((int)fields.size() <= std::max(std::max(idx_v1, idx_v7), std::max(idx_v11, idx_amount))) continue;
        try {
            double v1 = std::stod(trim(fields[idx_v1]));
            double v7 = std::stod(trim(fields[idx_v7]));
            double v11 = std::stod(trim(fields[idx_v11]));
            double amount = std::stod(trim(fields[idx_amount]));
            if (!(amount >= 9000.0)) continue;
            long long bin = (long long) floor(v1 / 0.1);
            bool neg = (v7 < 0.0);
            localAgg_.add(bin, (long double)v11, (long double)amount, neg);
        } catch (...) { continue; }
    }
    double sec = t.elapsed();
    log_timing("cpu_parse_agg", sec);
}

void Worker::process_gpu(const std::vector<std::string> &lines) {
    Timer t_all;
    size_t n = lines.size();
    std::vector<double> v1(n), v7(n), v11(n), amount(n);
    // parse to arrays
    Timer t_parse;
    #pragma omp parallel for
    for (size_t i = 0; i < n; ++i) {
        auto fields = split_csv_line(lines[i]);
        int idx_v1 = cfg_.idx_v1;
        int idx_v7 = cfg_.idx_v7;
        int idx_v11 = cfg_.idx_v11;
        int idx_amount = cfg_.idx_amount;
        try {
            v1[i] = std::stod(trim(fields[idx_v1]));
            v7[i] = std::stod(trim(fields[idx_v7]));
            v11[i] = std::stod(trim(fields[idx_v11]));
            amount[i] = std::stod(trim(fields[idx_amount]));
        } catch (...) {
            v1[i]=0; v7[i]=0; v11[i]=0; amount[i]=0;
        }
    }
    double t_parse_sec = t_parse.elapsed();
    log_timing("gpu_parse", t_parse_sec);

    // allocate outputs
    std::vector<long long> bins(n);
    std::vector<unsigned char> flags(n);

    // call cuda wrapper
    Timer t_cuda;
    int rc = cuda_filter_and_bin(v1.data(), v7.data(), v11.data(), amount.data(), bins.data(), flags.data(), (int)n);
    double t_cuda_sec = t_cuda.elapsed();
    log_timing("gpu_kernel", t_cuda_sec);
    if (rc != 0) {
        log_timing("gpu_kernel_failed", (double)rc);
        // fallback to CPU processing for the batch
        process_cpu(lines);
        return;
    }

    // reduce results on CPU (parallel)
    Timer t_reduce;
    #pragma omp parallel for schedule(dynamic,256)
    for (size_t i = 0; i < n; ++i) {
        if (!flags[i]) continue;
        long long bin = bins[i];
        bool neg = (v7[i] < 0.0);
        localAgg_.add(bin, (long double)v11[i], (long double)amount[i], neg);
    }
    double t_reduce_sec = t_reduce.elapsed();
    log_timing("gpu_postreduce_cpu", t_reduce_sec);

    double t_all_sec = t_all.elapsed();
    log_timing("gpu_total", t_all_sec);
}

std::vector<AggregateRecord> Worker::export_aggregates_and_clear() {
    return localAgg_.export_and_clear();
}
