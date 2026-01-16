#include "aggregation.hpp"
#include "utils.hpp"

#include <algorithm>
#include <cstdint>
#include <limits>
#include <vector>
#include <cmath>
#include <map>

std::vector<PeriodStats> aggregate_periods(const std::vector<Record>& records) {
    const double interval = 0.1;  // интервал V1 в 0.1 единицы

    std::vector<PeriodStats> result;
    if (records.empty()) return result;

    struct PeriodAccumulator {
        double v1_min = std::numeric_limits<double>::max();
        double v1_max = std::numeric_limits<double>::lowest();
        double v11_sum = 0.0;
        int64_t negative_v7_count = 0;
        int64_t zero_v7_count = 0;  // Количество нулевых значений V7
        double amount_sum = 0.0;
        int64_t count = 0;

        void add(const Record& r) {
            v1_min = std::min(v1_min, r.v1);
            v1_max = std::max(v1_max, r.v1);
            v11_sum += r.v11;
            if (r.v7 < 0.0) {
                negative_v7_count++;
            } else if (r.v7 == 0.0) {
                zero_v7_count++;
            }
            amount_sum += r.amount;
            ++count;
        }
    };

    // Группируем по интервалам V1
    std::map<PeriodIndex, PeriodAccumulator> groups;
    
    for (const auto& r : records) {
        // Вычисляем индекс интервала: floor(v1 / 0.1)
        PeriodIndex period = static_cast<PeriodIndex>(std::floor(r.v1 / interval));
        groups[period].add(r);
    }

    // Проверяем, есть ли вообще отрицательные значения V7
    bool has_negative = false;
    for (const auto& [period, acc] : groups) {
        if (acc.negative_v7_count > 0) {
            has_negative = true;
            break;
        }
    }
    
    // Преобразуем в вектор PeriodStats
    for (const auto& [period, acc] : groups) {
        PeriodStats stats;
        stats.period = period;
        stats.v1_min = acc.v1_min;
        stats.v1_max = acc.v1_max;
        stats.v11_sum = acc.v11_sum;
        // Если отрицательных значений нет, используем количество нулевых
        if (!has_negative && acc.negative_v7_count == 0) {
            stats.negative_v7_count = acc.zero_v7_count;
        } else {
            stats.negative_v7_count = acc.negative_v7_count;
        }
        stats.amount_sum = acc.amount_sum;
        stats.count = acc.count;
        result.push_back(stats);
    }

    // Сортируем по периоду
    std::sort(result.begin(), result.end(),
        [](const PeriodStats& a, const PeriodStats& b) {
            return a.period < b.period;
        });

    return result;
}
