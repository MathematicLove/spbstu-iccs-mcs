#include <mpi.h>
#include <iostream>
#include <vector>
#include <iomanip>
#include <cstdlib>
#include <string>
#include <sstream>
#include <climits>
#include <algorithm>
#include <numeric>

#include "csv_loader.hpp"
#include "record.hpp"
#include "period_stats.hpp"
#include "aggregation.hpp"
#include "intervals.hpp"
#include "utils.hpp"
#include "gpu_loader.hpp"

// Функция для парсинга аргументов командной строки
size_t parse_top_n(int argc, char** argv, int rank) {
    size_t top_n = 50; // значение по умолчанию
    
    for (int i = 1; i < argc; i++) {
        std::string arg = argv[i];
        if (arg == "-n" || arg == "--top-n") {
            if (i + 1 < argc) {
                try {
                    int value = std::stoi(argv[i + 1]);
                    if (value > 0) {
                        top_n = static_cast<size_t>(value);
                        if (rank == 0) {
                            std::cout << "Using top_n = " << top_n << std::endl;
                        }
                    } else {
                        if (rank == 0) {
                            std::cerr << "ERROR: top_n must be positive. Using default: 50" << std::endl;
                        }
                    }
                } catch (const std::exception&) {
                    if (rank == 0) {
                        std::cerr << "ERROR: Invalid value for top_n. Using default: 50" << std::endl;
                    }
                }
                i++; // пропускаем следующий аргумент
            }
        } else if (arg == "-h" || arg == "--help") {
            if (rank == 0) {
                std::cout << "Usage: " << argv[0] << " [-n|--top-n N]" << std::endl;
                std::cout << "  -n, --top-n N    Number of top groups to output (default: 50)" << std::endl;
                std::cout << "  -h, --help       Show this help message" << std::endl;
            }
            MPI_Finalize();
            return 0;
        }
    }
    
    return top_n;
}

// Функция валидации данных
bool validate_data(const std::vector<Record>& records, int rank, int size) {
    if (rank == 0) {
        // Проверка на пустые данные
        size_t total_records = records.size();
        
        // Собираем информацию со всех ранков
        for (int r = 1; r < size; r++) {
            int count;
            MPI_Recv(&count, 1, MPI_INT, r, 100, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            total_records += static_cast<size_t>(count);
        }
        
        if (total_records == 0) {
            std::cerr << std::endl;
            std::cerr << "========================================" << std::endl;
            std::cerr << "ERROR: No valid records found!" << std::endl;
            std::cerr << "========================================" << std::endl;
            std::cerr << "Possible reasons:" << std::endl;
            std::cerr << "  1. File path is incorrect" << std::endl;
            std::cerr << "  2. All records have Amount < 9000.00" << std::endl;
            std::cerr << "  3. File format is incorrect" << std::endl;
            std::cerr << std::endl;
            std::cerr << "Please check:" << std::endl;
            std::cerr << "  - DATA_PATH environment variable" << std::endl;
            std::cerr << "  - File exists and is readable" << std::endl;
            std::cerr << "  - File contains valid CSV data" << std::endl;
            std::cerr << std::endl;
            std::cerr << "Please fix the issue and run again." << std::endl;
            std::cerr << "========================================" << std::endl;
            return false;
        }
        
        std::cout << "Rank 0: Total valid records across all ranks: " << total_records << std::endl;
        return true;
    } else {
        // Отправляем количество записей на Rank 0
        // Ограничиваем размер до INT_MAX для совместимости с MPI_INT
        int count = static_cast<int>(std::min(records.size(), static_cast<size_t>(INT_MAX)));
        MPI_Send(&count, 1, MPI_INT, 0, 100, MPI_COMM_WORLD);
        return true; // остальные ранки всегда возвращают true
    }
}

int main(int argc, char** argv) {
    MPI_Init(&argc, &argv);
    double total_start = MPI_Wtime();

    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    
    // Парсим аргументы командной строки
    size_t top_n = parse_top_n(argc, argv, rank);
    if (top_n == 0) {
        // help был вызван
        return 0;
    }
    
    // Отладочный вывод для проверки параметра
    if (rank == 0) {
        std::cout << "Rank 0: Parsed top_n = " << top_n << " from command line" << std::endl;
        std::cout << "Rank 0: *** Using NEW weighted balancing system ***" << std::endl;
    }

    // Проверяем доступность GPU
    bool use_cuda = get_use_cuda();
    bool have_gpu = gpu_is_available();
    bool use_gpu = use_cuda && have_gpu;
    
    std::cout << "Rank " << rank 
              << ": USE_CUDA=" << use_cuda 
              << ", GPU available=" << have_gpu 
              << ", using " << (use_gpu ? "GPU" : "CPU")
              << std::endl;

    // Собираем информацию о GPU доступности всех процессов для весовой балансировки
    int my_gpu_flag = use_gpu ? 1 : 0;
    std::vector<int> gpu_flags(size, 0);
    
    // Собираем информацию со всех процессов
    MPI_Allgather(&my_gpu_flag, 1, MPI_INT, 
                  gpu_flags.data(), 1, MPI_INT, MPI_COMM_WORLD);
    
    std::vector<bool> process_has_gpu(size, false);
    for (int i = 0; i < size; i++) {
        process_has_gpu[i] = (gpu_flags[i] != 0);
    }
    
    // Вычисляем веса процессов на основе GPU доступности
    std::vector<double> process_weights = calculate_process_weights(process_has_gpu);
    std::vector<int> data_shares = weights_to_shares(process_weights, 1000);
    
    // Выводим информацию о распределении (только на Rank 0)
    if (rank == 0) {
        double gpu_weight = get_gpu_process_weight();
        double cpu_weight = get_cpu_process_weight();
        std::cout << "Rank 0: GPU weight = " << gpu_weight << ", CPU weight = " << cpu_weight << std::endl;
        std::cout << "Rank 0: Process weights: ";
        for (int i = 0; i < size; i++) {
            std::cout << "P" << i << "=" << process_weights[i] << (process_has_gpu[i] ? "(GPU)" : "(CPU)");
            if (i < size - 1) std::cout << ", ";
        }
        std::cout << std::endl;
        std::cout << "Rank 0: Data shares: ";
        for (size_t i = 0; i < data_shares.size(); i++) {
            std::cout << data_shares[i];
            if (i < data_shares.size() - 1) std::cout << ",";
        }
        std::cout << " (total: " << std::accumulate(data_shares.begin(), data_shares.end(), 0) << ")" << std::endl;
    }

    // Параллельное чтение данных с использованием вычисленных долей
    double read_start = MPI_Wtime();
    std::vector<Record> records;
    try {
        records = load_csv_parallel(rank, size, data_shares);
    } catch (const std::exception& e) {
        if (rank == 0) {
            std::cerr << std::endl;
            std::cerr << "========================================" << std::endl;
            std::cerr << "ERROR: Failed to read data file!" << std::endl;
            std::cerr << "========================================" << std::endl;
            std::cerr << "Error message: " << e.what() << std::endl;
            std::cerr << std::endl;
            std::cerr << "Please check:" << std::endl;
            std::cerr << "  - DATA_PATH environment variable is set correctly" << std::endl;
            std::cerr << "  - File exists and is readable on all nodes" << std::endl;
            std::cerr << "  - File path is accessible from all nodes" << std::endl;
            std::cerr << std::endl;
            std::cerr << "Please fix the issue and run again." << std::endl;
            std::cerr << "========================================" << std::endl;
        }
        MPI_Finalize();
        return 1;
    }
    double read_time = MPI_Wtime() - read_start;

    std::cout << "Rank " << rank 
              << ": read " << records.size() << " records"
              << " in " << std::fixed << std::setprecision(3) << read_time << " sec"
              << std::endl;
    
    // Валидация данных
    if (!validate_data(records, rank, size)) {
        MPI_Finalize();
        return 1;
    }

    // Агрегация по интервалам V1
    double agg_start = MPI_Wtime();
    std::vector<PeriodStats> periods;
    
    // Пытаемся использовать GPU если доступно, иначе CPU
    if (use_gpu) {
        if (!aggregate_periods_gpu(records, 0, periods)) {
            // Если GPU агрегация не удалась, используем CPU
            periods = aggregate_periods(records);
        }
    } else {
        periods = aggregate_periods(records);
    }
    
    double agg_time = MPI_Wtime() - agg_start;

    std::cout << "Rank " << rank 
              << ": aggregated " << periods.size() << " groups"
              << " [" << (periods.empty() ? 0 : periods.front().period) 
              << ".." << (periods.empty() ? 0 : periods.back().period) << "]"
              << " in " << std::fixed << std::setprecision(3) << agg_time << " sec"
              << std::endl;

    // Параллельное построение интервалов
    IntervalResult iv_result = find_intervals_parallel(periods, rank, size);

    std::cout << "Rank " << rank 
              << ": found " << iv_result.intervals.size() << " intervals"
              << ", compute " << std::fixed << std::setprecision(6) << iv_result.compute_time << " sec"
              << ", wait " << iv_result.wait_time << " sec"
              << std::endl;

    // Сбор интервалов на ранке 0
    double collect_wait = collect_intervals(iv_result.intervals, rank, size, top_n);

    if (rank == 0) {
        std::cout << "Rank 0: collected " << iv_result.intervals.size() << " total intervals"
                  << ", wait " << std::fixed << std::setprecision(3) << collect_wait << " sec"
                  << std::endl;
    }

    // Запись результатов в файл (только ранк 0)
    const char* home = std::getenv("HOME");
    std::string result_path = home ? std::string(home) + "/result.csv" : "result.csv";
    
    if (rank == 0) {
        double write_start = MPI_Wtime();
        write_intervals(result_path, iv_result.intervals);
        double write_time = MPI_Wtime() - write_start;

        std::cout << "Rank 0: wrote " << result_path << " with " << iv_result.intervals.size() << " groups"
                  << " in " << std::fixed << std::setprecision(3) << write_time << " sec"
                  << std::endl;
    }

    // Вывод общего времени выполнения (без барьера - каждый процесс работает независимо)
    double total_time = MPI_Wtime() - total_start;
    if (rank == 0) {
        std::cout << "Total execution time: "
                  << std::fixed << std::setprecision(3)
                  << total_time << " sec" << std::endl;
    }

    MPI_Finalize();
    return 0;
}
