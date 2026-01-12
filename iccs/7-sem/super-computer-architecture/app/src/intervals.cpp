#include "intervals.hpp"
#include "utils.hpp"
#include <mpi.h>
#include <algorithm>
#include <cmath>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <ctime>
#include <limits>
#include <iostream>

IntervalResult find_intervals_parallel(
    const std::vector<PeriodStats>& periods,
    int rank, int size)
{
    (void)rank;  // Не используется для кредитных карт
    (void)size;  // Не используется для кредитных карт
    
    IntervalResult result;
    result.compute_time = 0.0;
    result.wait_time = 0.0;
    
    if (periods.empty()) {
        return result;
    }
    
    double compute_start = MPI_Wtime();
    
    // Преобразуем PeriodStats в Interval
    for (const auto& ps : periods) {
        Interval iv;
        iv.period = ps.period;
        iv.v1_min = ps.v1_min;
        iv.v1_max = ps.v1_max;
        iv.v11_expectation = (ps.count > 0) ? (ps.v11_sum / static_cast<double>(ps.count)) : 0.0;
        iv.negative_v7_count = ps.negative_v7_count;
        iv.amount_sum = ps.amount_sum;
        iv.count = ps.count;
        result.intervals.push_back(iv);
    }
    
    result.compute_time = MPI_Wtime() - compute_start;
    
    return result;
}

double collect_intervals(
    std::vector<Interval>& local_intervals,
    int rank, int size,
    size_t top_n)
{
    double wait_time = 0.0;
    
    if (rank == 0) {
        for (int r = 1; r < size; r++) {
            double wait_start = MPI_Wtime();
            
            int count;
            MPI_Recv(&count, 1, MPI_INT, r, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            
            if (count > 0) {
                std::vector<double> buffer(count * 7);
                MPI_Recv(buffer.data(), count * 7, MPI_DOUBLE, r, 2, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
                
                for (int i = 0; i < count; i++) {
                    Interval iv;
                    iv.period = static_cast<PeriodIndex>(buffer[i * 7 + 0]);
                    iv.v1_min = buffer[i * 7 + 1];
                    iv.v1_max = buffer[i * 7 + 2];
                    iv.v11_expectation = buffer[i * 7 + 3];
                    iv.negative_v7_count = static_cast<int64_t>(buffer[i * 7 + 4]);
                    iv.amount_sum = buffer[i * 7 + 5];
                    iv.count = static_cast<int64_t>(buffer[i * 7 + 6]);
                    local_intervals.push_back(iv);
                }
            }
            
            wait_time += MPI_Wtime() - wait_start;
        }
        
        // Отладочный вывод: сколько групп собрано
        std::cout << "Rank 0: collected " << local_intervals.size() << " groups from all ranks" << std::endl;
        
        // Сортируем все группы по убыванию количества отрицательных V7
        std::sort(local_intervals.begin(), local_intervals.end(),
            [](const Interval& a, const Interval& b) {
                return a.negative_v7_count > b.negative_v7_count;
            });
        
        // Находим максимальное количество отрицательных V7
        int64_t max_negative_v7 = local_intervals.empty() ? 0 : local_intervals[0].negative_v7_count;
        
        std::cout << "Rank 0: max_negative_v7_count = " << max_negative_v7 << std::endl;
        
        // Проверка: если нет отрицательных значений V7, проверяем нулевые
        if (max_negative_v7 == 0) {
            // Проверяем, есть ли вообще какие-то значения (отрицательные или нулевые)
            bool has_any_special = false;
            for (const auto& iv : local_intervals) {
                if (iv.negative_v7_count > 0) {
                    has_any_special = true;
                    break;
                }
            }
            
            if (!has_any_special) {
                std::cout << "Rank 0: WARNING: No negative or zero values found in V7 column (-)" << std::endl;
                // Продолжаем с текущими данными, но выводим предупреждение
            } else {
                std::cout << "Rank 0: No negative V7 values found, using zero values instead" << std::endl;
            }
        }
        
        // Находим все группы с максимальным количеством отрицательных V7 (или нулевых, если отрицательных нет)
        // (может быть несколько групп с одинаковым максимальным значением)
        std::vector<Interval> filtered;
        for (const auto& iv : local_intervals) {
            if (iv.negative_v7_count == max_negative_v7) {
                filtered.push_back(iv);
            } else {
                // Если количество отрицательных V7 меньше максимального, прекращаем
                break;
            }
        }
        
        std::cout << "Rank 0: found " << filtered.size() << " groups with max negative_v7_count" << std::endl;
        
        // Если групп с максимальным количеством меньше top_n, берем топ top_n по количеству отрицательных V7
        if (filtered.size() < top_n) {
            // Берем топ top_n по количеству отрицательных V7
            size_t top_by_negative_v7 = std::min(top_n, local_intervals.size());
            filtered.clear();
            for (size_t i = 0; i < top_by_negative_v7; i++) {
                filtered.push_back(local_intervals[i]);
            }
            std::cout << "Rank 0: expanded to top " << top_n << " by negative_v7_count: " << filtered.size() << " groups" << std::endl;
        }
        
        // Сортируем по убыванию мат. ожидания V11, затем по убыванию суммы Amount
        std::sort(filtered.begin(), filtered.end(),
            [](const Interval& a, const Interval& b) {
                if (std::abs(a.v11_expectation - b.v11_expectation) > 1e-9) {
                    return a.v11_expectation > b.v11_expectation;
                }
                return a.amount_sum > b.amount_sum;
            });
        
        // Берем топ top_n по мат. ожиданию V11
        local_intervals.clear();
        size_t final_top_n = std::min(top_n, filtered.size());
        for (size_t i = 0; i < final_top_n; i++) {
            local_intervals.push_back(filtered[i]);
        }
        
        // Финальная сортировка по убыванию суммы Amount
        std::sort(local_intervals.begin(), local_intervals.end(),
            [](const Interval& a, const Interval& b) {
                return a.amount_sum > b.amount_sum;
            });
    } else {
        int count = static_cast<int>(local_intervals.size());
        MPI_Send(&count, 1, MPI_INT, 0, 1, MPI_COMM_WORLD);
        
        if (count > 0) {
            std::vector<double> buffer(count * 7);
            for (int i = 0; i < count; i++) {
                const auto& iv = local_intervals[i];
                buffer[i * 7 + 0] = static_cast<double>(iv.period);
                buffer[i * 7 + 1] = iv.v1_min;
                buffer[i * 7 + 2] = iv.v1_max;
                buffer[i * 7 + 3] = iv.v11_expectation;
                buffer[i * 7 + 4] = static_cast<double>(iv.negative_v7_count);
                buffer[i * 7 + 5] = iv.amount_sum;
                buffer[i * 7 + 6] = static_cast<double>(iv.count);
            }
            MPI_Send(buffer.data(), count * 7, MPI_DOUBLE, 0, 2, MPI_COMM_WORLD);
        }
    }
    
    return wait_time;
}

void write_intervals(const std::string& filename, const std::vector<Interval>& intervals) {
    std::ofstream out(filename);
    
    out << std::fixed << std::setprecision(6);
    out << "v1_interval,v1_min,v1_max,v11_expectation,negative_v7_count,amount_sum,count\n";
    
    // Проверяем, есть ли вообще отрицательные или нулевые значения
    bool has_negative_or_zero = false;
    for (const auto& iv : intervals) {
        if (iv.negative_v7_count > 0) {
            has_negative_or_zero = true;
            break;
        }
    }
    
    for (const auto& iv : intervals) {
        double v1_interval_start = static_cast<double>(iv.period) * 0.1;
        out << v1_interval_start << ","
            << iv.v1_min << ","
            << iv.v1_max << ","
            << iv.v11_expectation << ",";
        
        // Если нет отрицательных и нулевых значений, выводим "-"
        if (!has_negative_or_zero && iv.negative_v7_count == 0) {
            out << "-,";
        } else {
            out << iv.negative_v7_count << ",";
        }
        
        out << std::setprecision(2) << iv.amount_sum << ","
            << std::setprecision(0) << iv.count << "\n";
    }
}
