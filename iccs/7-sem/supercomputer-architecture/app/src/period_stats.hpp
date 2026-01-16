#pragma once
#include <cstdint>

using PeriodIndex = int64_t;

// Агрегированные данные за один интервал V1
struct PeriodStats {
    PeriodIndex period;   // индекс интервала V1 (floor(v1 / 0.1))
    double v1_min;        // минимальное V1 в интервале
    double v1_max;        // максимальное V1 в интервале
    double v11_sum;       // сумма V11 для вычисления мат. ожидания
    int64_t negative_v7_count;  // количество отрицательных значений V7
    double amount_sum;    // сумма Amount в группе
    int64_t count;        // количество записей в группе
};
