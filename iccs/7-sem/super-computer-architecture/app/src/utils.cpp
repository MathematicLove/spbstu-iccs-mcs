#include "utils.hpp"
#include <fstream>
#include <sstream>
#include <stdexcept>
#include <numeric>
#include <algorithm>
#include <cstring>

int get_num_cpu_threads() {
    const char* env_threads = std::getenv("NUM_CPU_THREADS");
    int num_cpu_threads = 1;
    if (env_threads) {
        num_cpu_threads = std::atoi(env_threads);
        if (num_cpu_threads < 1) num_cpu_threads = 1;
    }
    return num_cpu_threads;
}

std::string get_env(const char* name) {
    const char* env = std::getenv(name);
    if (!env) {
        throw std::runtime_error(std::string("Environment variable not set: ") + name);
    }
    return std::string(env);
}

std::string get_data_path() {
    return get_env("DATA_PATH");
}

bool is_gpu_node(const std::string& node_name) {
    // Проверяем, содержит ли имя узла "gpu" (регистронезависимо)
    std::string lower_name = node_name;
    std::transform(lower_name.begin(), lower_name.end(), lower_name.begin(), ::tolower);
    return lower_name.find("gpu") != std::string::npos;
}

double get_gpu_process_weight() {
    const char* env = std::getenv("GPU_PROCESS_WEIGHT");
    if (env) {
        try {
            double weight = std::stod(env);
            if (weight > 0.0) {
                return weight;
            }
        } catch (...) {
            // Игнорируем ошибки парсинга
        }
    }
    // Значение по умолчанию: 1.28 (GPU получает на 28% больше данных)
    return 1.28;
}

double get_cpu_process_weight() {
    const char* env = std::getenv("CPU_PROCESS_WEIGHT");
    if (env) {
        try {
            double weight = std::stod(env);
            if (weight > 0.0) {
                return weight;
            }
        } catch (...) {
            // Игнорируем ошибки парсинга
        }
    }
    // Значение по умолчанию: 1.0 (базовый вес для CPU)
    return 1.0;
}

std::vector<double> calculate_process_weights(const std::vector<bool>& process_has_gpu) {
    double gpu_weight = get_gpu_process_weight();
    double cpu_weight = get_cpu_process_weight();
    
    std::vector<double> weights;
    weights.reserve(process_has_gpu.size());
    
    for (bool has_gpu : process_has_gpu) {
        weights.push_back(has_gpu ? gpu_weight : cpu_weight);
    }
    
    return weights;
}

std::vector<int> weights_to_shares(const std::vector<double>& weights, int base_share) {
    if (weights.empty()) {
        return std::vector<int>();
    }
    
    // Находим минимальный вес для нормализации
    double min_weight = *std::min_element(weights.begin(), weights.end());
    if (min_weight <= 0.0) {
        min_weight = 1.0;
    }
    
    // Вычисляем суммарный вес
    double total_weight = 0.0;
    for (double w : weights) {
        total_weight += w;
    }
    
    if (total_weight <= 0.0) {
        // Если суммарный вес некорректный, используем равномерное распределение
        return std::vector<int>(weights.size(), base_share);
    }
    
    // Преобразуем веса в доли, сохраняя пропорции
    // Используем base_share как базовое значение для нормализации
    std::vector<int> shares;
    shares.reserve(weights.size());
    
    for (double w : weights) {
        // Нормализуем вес относительно минимального и умножаем на base_share
        double normalized = (w / min_weight) * base_share;
        shares.push_back(static_cast<int>(normalized + 0.5)); // Округляем
    }
    
    return shares;
}

std::vector<int> calculate_data_shares_auto(int num_nodes, const std::vector<std::string>& node_names) {
    std::vector<int> shares;
    
    // Если имена узлов не предоставлены, используем равномерное распределение
    if (node_names.empty() || node_names.size() != static_cast<size_t>(num_nodes)) {
        shares.assign(num_nodes, 10);
        return shares;
    }
    
    // Подсчитываем количество GPU и CPU узлов и их индексы
    int num_gpu = 0;
    int num_cpu = 0;
    std::vector<bool> is_gpu(num_nodes, false);
    std::vector<int> gpu_indices;
    std::vector<int> cpu_indices;
    
    for (size_t i = 0; i < node_names.size(); i++) {
        if (is_gpu_node(node_names[i])) {
            is_gpu[i] = true;
            num_gpu++;
            gpu_indices.push_back(static_cast<int>(i));
        } else {
            num_cpu++;
            cpu_indices.push_back(static_cast<int>(i));
        }
    }
    

    // С GPU (4 узла, 2 CPU + 2 GPU): 
    // CPU среднее ~10.5, GPU среднее ~13.5
    // Соотношение GPU/CPU: 13.5/10.5 ≈ 1.286 (GPU получает ~28.6% больше данных)

    const int base_cpu_share = 10;
    const double gpu_performance_ratio = 1.286;  // Основано на реальных данных
    const int base_gpu_share = static_cast<int>(base_cpu_share * gpu_performance_ratio + 0.5);
    
    // Если есть и GPU, и CPU узлы
    if (num_gpu > 0 && num_cpu > 0) {
        shares.resize(num_nodes);
        
        // Распределяем доли для CPU узлов (варьируем немного для балансировки)
        if (num_cpu == 1) {
            shares[cpu_indices[0]] = base_cpu_share;
        } else if (num_cpu == 2) {
            shares[cpu_indices[0]] = base_cpu_share;      // 10
            shares[cpu_indices[1]] = base_cpu_share + 1;   // 11
        } else {
            // Для 3+ CPU узлов: равномерно с небольшими вариациями
            for (size_t i = 0; i < cpu_indices.size(); i++) {
                shares[cpu_indices[i]] = base_cpu_share + (i % 2);  // 10 или 11
            }
        }
        
        // Распределяем доли для GPU узлов (варьируем немного для балансировки)
        if (num_gpu == 1) {
            shares[gpu_indices[0]] = base_gpu_share;
        } else if (num_gpu == 2) {
            shares[gpu_indices[0]] = base_gpu_share;      // 13
            shares[gpu_indices[1]] = base_gpu_share + 1;   // 14
        } else {
            // Для 3+ GPU узлов: равномерно с небольшими вариациями
            for (size_t i = 0; i < gpu_indices.size(); i++) {
                shares[gpu_indices[i]] = base_gpu_share + (i % 2);  // 13 или 14
            }
        }
    } else if (num_gpu > 0) {
        // Только GPU узлы - равномерное распределение
        shares.assign(num_nodes, base_gpu_share);
    } else {
        // Только CPU узлы - равномерное распределение
        shares.assign(num_nodes, base_cpu_share);
    }
    
    return shares;
}

std::vector<int> get_data_read_shares() {
    // Сначала проверяем, задана ли переменная DATA_READ_SHARES вручную
    const char* env_shares = std::getenv("DATA_READ_SHARES");
    if (env_shares && strlen(env_shares) > 0) {
        std::vector<int> shares;
        std::stringstream ss(env_shares);
        std::string item;
        while (std::getline(ss, item, ',')) {
            shares.push_back(std::stoi(item));
        }
        return shares;
    }
    
    // Если не задана, пытаемся автоматически вычислить из SLURM
    const char* nodelist = std::getenv("SLURM_JOB_NODELIST");
    const char* ntasks = std::getenv("SLURM_NTASKS");
    const char* nnodes = std::getenv("SLURM_NNODES");
    
    // Определяем количество задач (приоритет: SLURM_NTASKS > SLURM_NNODES)
    int num_tasks = 0;
    if (ntasks) {
        num_tasks = std::atoi(ntasks);
    } else if (nnodes) {
        num_tasks = std::atoi(nnodes);
    }
    
    if (num_tasks > 0) {
        std::vector<std::string> node_names;
        
        // Парсим список узлов, если доступен
        if (nodelist) {
            std::string nodelist_str(nodelist);
            
            // Простой парсинг: разделяем по запятым
            std::stringstream ss(nodelist_str);
            std::string node;
            while (std::getline(ss, node, ',')) {
                // Убираем пробелы
                node.erase(std::remove_if(node.begin(), node.end(), ::isspace), node.end());
                if (!node.empty()) {
                    // Если формат node[1-4], извлекаем базовое имя
                    size_t bracket = node.find('[');
                    if (bracket != std::string::npos) {
                        node = node.substr(0, bracket);
                    }
                    node_names.push_back(node);
                }
            }
        }
        
        // Если получили имена узлов и их количество совпадает с количеством задач
        if (!node_names.empty() && node_names.size() == static_cast<size_t>(num_tasks)) {
            return calculate_data_shares_auto(num_tasks, node_names);
        } else if (!node_names.empty()) {
            // Если имена узлов есть, но их меньше чем задач (несколько задач на одном узле)
            // Повторяем имена узлов для всех задач
            std::vector<std::string> expanded_names;
            for (int i = 0; i < num_tasks; i++) {
                expanded_names.push_back(node_names[i % node_names.size()]);
            }
            return calculate_data_shares_auto(num_tasks, expanded_names);
        } else {
            // Если имен узлов нет, используем равномерное распределение
            return std::vector<int>(num_tasks, 10);
        }
    }
    
    // Если ничего не получилось, используем равномерное распределение (по умолчанию 4)
    return std::vector<int>(4, 10);
}

int64_t get_read_overlap_bytes() {
    return std::stoll(get_env("READ_OVERLAP_BYTES"));
}

int64_t get_aggregation_interval() {
    return std::stoll(get_env("AGGREGATION_INTERVAL"));
}

bool get_use_cuda() {
    const char* env = std::getenv("USE_CUDA");
    if (!env) {
        return false; // По умолчанию отключено, если переменная не установлена
    }
    try {
        return std::stoi(env) != 0;
    } catch (...) {
        return false;
    }
}

int64_t get_file_size(const std::string& path) {
    std::ifstream file(path, std::ios::binary | std::ios::ate);
    if (!file.is_open()) {
        throw std::runtime_error("Cannot open file: " + path);
    }
    return static_cast<int64_t>(file.tellg());
}

ByteRange calculate_byte_range(int rank, int size, int64_t file_size,
                               const std::vector<int>& shares, int64_t overlap_bytes) {
    std::vector<int> effective_shares;
    if (shares.size() == static_cast<size_t>(size)) {
        effective_shares = shares;
    } else {
        effective_shares.assign(size, 1);
    }
    
    int total_shares = std::accumulate(effective_shares.begin(), effective_shares.end(), 0);
    int64_t bytes_per_share = file_size / total_shares;
    
    int64_t base_start = 0;
    for (int i = 0; i < rank; i++) {
        base_start += bytes_per_share * effective_shares[i];
    }
    
    int64_t base_end = base_start + bytes_per_share * effective_shares[rank];
    
    ByteRange range;
    
    if (rank == 0) {
        range.start = 0;
        range.end = std::min(base_end + overlap_bytes, file_size);
    } else if (rank == size - 1) {
        range.start = std::max(base_start - overlap_bytes, static_cast<int64_t>(0));
        range.end = file_size;
    } else {
        range.start = std::max(base_start - overlap_bytes, static_cast<int64_t>(0));
        range.end = std::min(base_end + overlap_bytes, file_size);
    }
    
    return range;
}

void trim_edge_periods(std::vector<PeriodStats>& periods, int rank, int size) {
    if (periods.empty()) return;
    
    if (rank == 0) {
        periods.pop_back();
    } else if (rank == size - 1) {
        periods.erase(periods.begin());
    } else {
        periods.pop_back();
        periods.erase(periods.begin());
    }
}
