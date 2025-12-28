// src/aggregator_local.h
#pragma once
#include <unordered_map>
#include <cstdint>
#include <mutex>
#include <vector>
#include "include/mpi_utils.h"

// simple thread-safe local aggregator
struct LocalAggEntry {
    int64_t count = 0;
    int64_t count_neg_v7 = 0;
    long double sum_v11 = 0.0L;
    long double total_amount = 0.0L;
};

class LocalAggregator {
public:
    void add(int64_t bin, long double v11, long double amount, bool v7_neg) {
        std::lock_guard<std::mutex> lg(mut_);
        auto &e = map_[bin];
        e.count += 1;
        if (v7_neg) e.count_neg_v7 += 1;
        e.sum_v11 += v11;
        e.total_amount += amount;
    }
    // export to AggregateRecord vector and clear
    std::vector<AggregateRecord> export_and_clear() {
        std::lock_guard<std::mutex> lg(mut_);
        std::vector<AggregateRecord> out;
        out.reserve(map_.size());
        for (auto &p : map_) {
            AggregateRecord r;
            r.bin = p.first;
            r.count = p.second.count;
            r.count_neg_v7 = p.second.count_neg_v7;
            r.sum_v11 = (double)p.second.sum_v11;
            r.total_amount = (double)p.second.total_amount;
            out.push_back(r);
        }
        map_.clear();
        return out;
    }
    size_t size() {
        std::lock_guard<std::mutex> lg(mut_);
        return map_.size();
    }
private:
    std::unordered_map<int64_t, LocalAggEntry> map_;
    std::mutex mut_;
};
