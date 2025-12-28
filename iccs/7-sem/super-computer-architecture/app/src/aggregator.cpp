// src/aggregator.cpp
#include "aggregator.h"

void Aggregator::add(int64_t bin, long double v11, long double amount, bool v7_neg) {
    std::lock_guard<std::mutex> lg(mut_);
    auto &e = map_[bin];
    e.count += 1;
    e.sum_v11 += v11;
    e.total_amount += amount;
    if (v7_neg) e.count_neg_v7 += 1;
}

std::vector<std::pair<int64_t, LocalAgg>> Aggregator::extract_all() {
    std::lock_guard<std::mutex> lg(mut_);
    std::vector<std::pair<int64_t, LocalAgg>> out;
    out.reserve(map_.size());
    for (auto &p : map_) out.emplace_back(p.first, p.second);
    map_.clear();
    return out;
}

void Aggregator::merge_from_records(const std::vector<std::pair<int64_t, LocalAgg>> &other) {
    std::lock_guard<std::mutex> lg(mut_);
    for (auto &p : other) {
        auto &e = map_[p.first];
        e.count += p.second.count;
        e.count_neg_v7 += p.second.count_neg_v7;
        e.sum_v11 += p.second.sum_v11;
        e.total_amount += p.second.total_amount;
    }
}
