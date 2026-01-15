#pragma once

#include "record.hpp"
#include "period_stats.hpp"
#include <vector>
#include <string>
#include <cstdlib>
#include <cstdint>

// Чтение переменных окружения
int get_num_cpu_threads();
std::string get_data_path();
std::vector<int> get_data_read_shares();
int64_t get_read_overlap_bytes();
int64_t get_aggregation_interval();
bool get_use_cuda();

// Автоматическое вычисление долей на основе узлов
// Возвращает true, если узел является GPU узлом (по имени)
bool is_gpu_node(const std::string& node_name);
// Автоматически вычисляет доли на основе количества узлов и их типов
std::vector<int> calculate_data_shares_auto(int num_nodes, const std::vector<std::string>& node_names);

// Весовая балансировка процессов
// Получает веса GPU и CPU процессов из переменных окружения
double get_gpu_process_weight();
double get_cpu_process_weight();
// Вычисляет доли данных на основе весов процессов (GPU/CPU)
// process_has_gpu[i] = true если процесс i имеет доступный GPU
std::vector<double> calculate_process_weights(const std::vector<bool>& process_has_gpu);
// Преобразует веса в целочисленные доли для распределения данных
std::vector<int> weights_to_shares(const std::vector<double>& weights, int base_share = 1000);

// Структура для хранения диапазона байт для чтения
struct ByteRange {
    int64_t start;
    int64_t end;  // exclusive
};

// Вычисляет диапазон байт для конкретного ранка
ByteRange calculate_byte_range(int rank, int size, int64_t file_size,
                               const std::vector<int>& shares, int64_t overlap_bytes);

// Получение размера файла
int64_t get_file_size(const std::string& path);

// Удаляет крайние периоды, которые могут быть неполными из-за параллельного чтения
void trim_edge_periods(std::vector<PeriodStats>& periods, int rank, int size);
