// src/aggregator.h
#pragma once
#include <unordered_map>
#include <cstdint>
#include <vector>
#include <mutex>

struct LocalAgg {
    int64_t count = 0;
    int64_t count_neg_v7 = 0;
    long double sum_v11 = 0.0L;
    long double total_amount = 0.0L;
};

class Aggregator {
public:
    void add(int64_t bin, long double v11, long double amount, bool v7_neg);
    std::vector<std::pair<int64_t, LocalAgg>> extract_all(); // extract and clear
    void merge_from_records(const std::vector<std::pair<int64_t, LocalAgg>> &other);
private:
    std::unordered_map<int64_t, LocalAgg> map_;
    std::mutex mut_;
};
