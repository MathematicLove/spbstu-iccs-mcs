#include "gpu_loader.hpp"
#include "period_stats.hpp"
#include "record.hpp"
#include <dlfcn.h>
#include <iostream>
#include <cstdint>
#include <cmath>
#include <cstdlib>
#include <string>
#include <vector>
#include <algorithm>
#include <map>

// Структура результата GPU (должна совпадать с gpu_plugin.cu)
struct GpuPeriodStats {
    int64_t period;
    double v1_min;
    double v1_max;
    double v11_sum;
    int64_t negative_v7_count;
    double amount_sum;
    int64_t count;
};

// Типы функций из GPU плагина
using gpu_is_available_fn = int (*)();

using gpu_aggregate_periods_fn = int (*)(
    const double* h_v1,
    const double* h_v7,
    const double* h_v11,
    const double* h_amount,
    int num_records,
    double interval,
    GpuPeriodStats** h_out_stats,
    int* out_num_periods
);

using gpu_free_results_fn = void (*)(GpuPeriodStats*);

static void* gpu_lib_handle = nullptr;

static void* get_gpu_lib_handle() {
    if (gpu_lib_handle == nullptr) {
        // Пробуем найти библиотеку в разных местах
        const char* home = getenv("HOME");
        std::string lib_paths[] = {
            "./libgpu_compute.so",
            "~/libgpu_compute.so",
            home ? std::string(home) + "/libgpu_compute.so" : "",
            "/mnt/share/supercomputers/build/libgpu_compute.so"
        };
        
        for (const auto& path : lib_paths) {
            if (path.empty()) continue;
            std::string expanded_path = path;
            if (expanded_path[0] == '~' && home) {
                expanded_path = std::string(home) + expanded_path.substr(1);
            }
            gpu_lib_handle = dlopen(expanded_path.c_str(), RTLD_LAZY);
            if (gpu_lib_handle) {
                return gpu_lib_handle;
            }
        }
        // Библиотека не найдена
        return nullptr;
    }
    return gpu_lib_handle;
}

bool gpu_is_available() {
    void* handle = get_gpu_lib_handle();
    if (!handle) {
        return false;
    }
    
    // Проверяем функцию проверки доступности GPU
    gpu_is_available_fn fn = (gpu_is_available_fn)dlsym(handle, "gpu_is_available");
    if (!fn) {
        return false;
    }
    
    int result = fn();
    return result != 0;
}

bool aggregate_periods_gpu(
    const std::vector<Record>& records,
    int64_t aggregation_interval,
    std::vector<PeriodStats>& out_stats)
{
    if (records.empty()) {
        out_stats.clear();
        return true;
    }
    
    void* handle = get_gpu_lib_handle();
    if (!handle) {
        return false;
    }
    
    // Получаем функцию агрегации
    gpu_aggregate_periods_fn aggregate_fn = (gpu_aggregate_periods_fn)dlsym(handle, "gpu_aggregate_periods");
    gpu_free_results_fn free_fn = (gpu_free_results_fn)dlsym(handle, "gpu_free_results");
    
    if (!aggregate_fn || !free_fn) {
        return false;
    }
    
    // Подготавливаем данные для GPU
    std::vector<double> h_v1(records.size());
    std::vector<double> h_v7(records.size());
    std::vector<double> h_v11(records.size());
    std::vector<double> h_amount(records.size());
    
    for (size_t i = 0; i < records.size(); i++) {
        h_v1[i] = records[i].v1;
        h_v7[i] = records[i].v7;
        h_v11[i] = records[i].v11;
        h_amount[i] = records[i].amount;
    }
    
    // Вызываем GPU агрегацию
    // Для кредитных карт интервал V1 = 0.1
    const double v1_interval = 0.1;
    GpuPeriodStats* gpu_stats = nullptr;
    int num_periods = 0;
    
    int result = aggregate_fn(
        h_v1.data(),
        h_v7.data(),
        h_v11.data(),
        h_amount.data(),
        static_cast<int>(records.size()),
        v1_interval,
        &gpu_stats,
        &num_periods
    );
    
    if (result != 0 || gpu_stats == nullptr) {
        return false;
    }
    
    // Преобразуем результаты GPU в PeriodStats
    out_stats.clear();
    out_stats.reserve(num_periods);
    
    // Проверяем, есть ли вообще отрицательные значения V7
    bool has_negative = false;
    for (size_t i = 0; i < records.size(); i++) {
        if (records[i].v7 < 0.0) {
            has_negative = true;
            break;
        }
    }
    
    // Если отрицательных нет, подсчитываем нулевые значения по периодам
    std::map<PeriodIndex, int64_t> zero_v7_by_period;
    if (!has_negative) {
        for (size_t i = 0; i < records.size(); i++) {
            if (records[i].v7 == 0.0) {
                PeriodIndex period = static_cast<PeriodIndex>(std::floor(records[i].v1 / 0.1));
                zero_v7_by_period[period]++;
            }
        }
    }
    
    for (int i = 0; i < num_periods; i++) {
        PeriodStats ps;
        ps.period = gpu_stats[i].period;
        ps.v1_min = gpu_stats[i].v1_min;
        ps.v1_max = gpu_stats[i].v1_max;
        ps.v11_sum = gpu_stats[i].v11_sum;
        // Если отрицательных значений нет, используем количество нулевых
        if (!has_negative && gpu_stats[i].negative_v7_count == 0) {
            auto it = zero_v7_by_period.find(gpu_stats[i].period);
            ps.negative_v7_count = (it != zero_v7_by_period.end()) ? it->second : 0;
        } else {
            ps.negative_v7_count = gpu_stats[i].negative_v7_count;
        }
        ps.amount_sum = gpu_stats[i].amount_sum;
        ps.count = gpu_stats[i].count;
        out_stats.push_back(ps);
    }
    
    // Освобождаем память результатов
    free_fn(gpu_stats);
    
    return true;
}
