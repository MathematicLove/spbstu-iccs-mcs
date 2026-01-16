#pragma once

#include "period_stats.hpp"
#include <vector>
#include <string>

// Группа с статистикой
struct Interval {
    PeriodIndex period;        // индекс интервала V1
    double v1_min;             // минимальное V1 в интервале
    double v1_max;             // максимальное V1 в интервале
    double v11_expectation;    // мат. ожидание V11 (v11_sum / count)
    int64_t negative_v7_count; // количество отрицательных значений V7
    double amount_sum;         // сумма Amount в группе
    int64_t count;             // количество записей в группе
};

// Результат параллельного построения интервалов
struct IntervalResult {
    std::vector<Interval> intervals;
    double compute_time;  // время вычислений
    double wait_time;     // время ожидания данных от предыдущего ранка
};

// Параллельное построение интервалов с использованием MPI
IntervalResult find_intervals_parallel(
    const std::vector<PeriodStats>& periods,
    int rank, int size
);

// Сбор интервалов со всех ранков на ранк 0
double collect_intervals(
    std::vector<Interval>& local_intervals,
    int rank, int size,
    size_t top_n = 50
);

// Вывод интервалов в файл
void write_intervals(const std::string& filename, const std::vector<Interval>& intervals);
