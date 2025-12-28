// src/worker.h
#pragma once
#include <string>
#include <vector>
#include <unordered_map>
#include <cstdint>
#include "timers.h"
#include "aggregator_local.h" // we'll add this small header inline below

struct WorkerConfig {
    int rank;
    int world_size;
    int batch_size;
    double gpu_fraction;
    int omp_threads;
    std::string log_dir;
    bool has_gpu;
    // header mapping
    int idx_v1=-1, idx_v7=-1, idx_v11=-1, idx_amount=-1;
};

class Worker {
public:
    Worker(const WorkerConfig &cfg);
    ~Worker();
    // process a batch of CSV lines
    void process_batch_lines(const std::vector<std::string> &lines);
    // export local aggregates to vector of AggregateRecord (for sending)
    std::vector<AggregateRecord> export_aggregates_and_clear();
    // logging
    void log_timing(const std::string &k, double sec);
private:
    WorkerConfig cfg_;
    TimingsLogger logger_;
    LocalAggregator localAgg_; // small class defined below
    Timer t_global_;
    // helpers
    void process_cpu(const std::vector<std::string> &lines);
    void process_gpu(const std::vector<std::string> &lines);
};
