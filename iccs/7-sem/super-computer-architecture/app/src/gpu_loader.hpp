#pragma once
#include "period_stats.hpp"
#include "record.hpp"
#include <vector>

// Проверка доступности CUDA
bool gpu_is_available();

// Агрегация периодов на GPU
// Возвращает true если успешно, false если GPU недоступен или ошибка
bool aggregate_periods_gpu(
    const std::vector<Record>& records,
    int64_t aggregation_interval,
    std::vector<PeriodStats>& out_stats
);
