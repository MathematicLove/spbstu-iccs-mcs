#pragma once

#include "record.hpp"
#include "period_stats.hpp"
#include <vector>

// Агрегация записей по периодам на одном узле
std::vector<PeriodStats> aggregate_periods(const std::vector<Record>& records);
