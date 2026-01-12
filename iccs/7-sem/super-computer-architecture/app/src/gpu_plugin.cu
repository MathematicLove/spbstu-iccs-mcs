#include <cuda_runtime.h>
#include <cstdint>
#include <cfloat>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <cmath>

// ============================================================================
// Структуры данных
// ============================================================================

// Результат агрегации одного периода для кредитных карт
struct GpuPeriodStats {
    int64_t period;              // индекс интервала V1 (floor(v1 / 0.1))
    double v1_min;                // минимальное V1 в интервале
    double v1_max;                // максимальное V1 в интервале
    double v11_sum;               // сумма V11 для вычисления мат. ожидания
    int64_t negative_v7_count;    // количество отрицательных значений V7
    double amount_sum;            // сумма Amount в группе
    int64_t count;                // количество записей в группе
};

// ============================================================================
// Вспомогательные функции
// ============================================================================

static double get_time_ms() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000.0 + ts.tv_nsec / 1000000.0;
}

#define CUDA_CHECK(call) do { \
    cudaError_t err = call; \
    if (err != cudaSuccess) { \
        printf("CUDA error at %s:%d: %s\n", __FILE__, __LINE__, cudaGetErrorString(err)); \
        return -1; \
    } \
} while(0)

// ============================================================================
// Kernel: вычисление period_id для каждого тика
// Для кредитных карт: period = floor(v1 / 0.1)
// ============================================================================

__global__ void compute_period_ids_kernel(
    const double* __restrict__ v1,
    int64_t* __restrict__ period_ids,
    int n,
    double interval)
{
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx < n) {
        period_ids[idx] = static_cast<int64_t>(floor(v1[idx] / interval));
    }
}

// ============================================================================
// Kernel: нахождение границ периодов (RLE без CUB)
// Находит позиции, где period_id меняется
// ============================================================================

__global__ void find_period_boundaries_kernel(
    const int64_t* __restrict__ period_ids,
    int* __restrict__ boundaries,
    int n)
{
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx == 0) {
        boundaries[0] = 0;  // Первая граница всегда 0
    }
    if (idx < n - 1) {
        if (period_ids[idx] != period_ids[idx + 1]) {
            boundaries[idx + 1] = 1;  // Граница найдена
        } else {
            boundaries[idx + 1] = 0;
        }
    }
}

// ============================================================================
// Kernel: подсчет уникальных периодов и их индексов
// ============================================================================


// ============================================================================
// Kernel: агрегация одного периода (один блок на период)
// ============================================================================

__global__ void aggregate_periods_kernel(
    const double* __restrict__ v1,
    const double* __restrict__ v7,
    const double* __restrict__ v11,
    const double* __restrict__ amount,
    const int64_t* __restrict__ unique_periods,
    const int* __restrict__ offsets,
    const int* __restrict__ counts,
    int num_periods,
    GpuPeriodStats* __restrict__ out_stats)
{
    int period_idx = blockIdx.x;
    if (period_idx >= num_periods) return;
    
    int offset = offsets[period_idx];
    int count = counts[period_idx];
    
    // Используем shared memory для редукции внутри блока
    __shared__ double s_v1_min;
    __shared__ double s_v1_max;
    __shared__ double s_v11_sum;
    __shared__ int64_t s_negative_v7_count;
    __shared__ double s_amount_sum;
    
    // Инициализация shared memory первым потоком
    if (threadIdx.x == 0) {
        s_v1_min = DBL_MAX;
        s_v1_max = -DBL_MAX;
        s_v11_sum = 0.0;
        s_negative_v7_count = 0;
        s_amount_sum = 0.0;
    }
    __syncthreads();
    
    // Локальные аккумуляторы для каждого потока
    double local_v1_min = DBL_MAX;
    double local_v1_max = -DBL_MAX;
    double local_v11_sum = 0.0;
    int64_t local_negative_v7_count = 0;
    double local_amount_sum = 0.0;
    
    // Каждый поток обрабатывает свою часть записей
    for (int i = threadIdx.x; i < count; i += blockDim.x) {
        int record_idx = offset + i;
        double v1_val = v1[record_idx];
        double v7_val = v7[record_idx];
        double v11_val = v11[record_idx];
        double amount_val = amount[record_idx];
        
        local_v1_min = fmin(local_v1_min, v1_val);
        local_v1_max = fmax(local_v1_max, v1_val);
        local_v11_sum += v11_val;
        if (v7_val < 0.0) {
            local_negative_v7_count++;
        }
        local_amount_sum += amount_val;
    }
    
    // Редукция с использованием атомарных операций
    atomicMin(reinterpret_cast<unsigned long long*>(&s_v1_min), 
              __double_as_longlong(local_v1_min));
    atomicMax(reinterpret_cast<unsigned long long*>(&s_v1_max),
              __double_as_longlong(local_v1_max));
    atomicAdd(&s_v11_sum, local_v11_sum);
    atomicAdd(reinterpret_cast<unsigned long long*>(&s_negative_v7_count),
              static_cast<unsigned long long>(local_negative_v7_count));
    atomicAdd(&s_amount_sum, local_amount_sum);
    
    __syncthreads();
    
    // Первый поток записывает результат
    if (threadIdx.x == 0) {
        GpuPeriodStats stats;
        stats.period = unique_periods[period_idx];
        stats.v1_min = s_v1_min;
        stats.v1_max = s_v1_max;
        stats.v11_sum = s_v11_sum;
        stats.negative_v7_count = s_negative_v7_count;
        stats.amount_sum = s_amount_sum;
        stats.count = count;
        out_stats[period_idx] = stats;
    }
}

// ============================================================================
// Простой kernel для агрегации (один поток на период)
// Используется когда периодов много и записей в каждом мало
// ============================================================================

__global__ void aggregate_periods_simple_kernel(
    const double* __restrict__ v1,
    const double* __restrict__ v7,
    const double* __restrict__ v11,
    const double* __restrict__ amount,
    const int64_t* __restrict__ unique_periods,
    const int* __restrict__ offsets,
    const int* __restrict__ counts,
    int num_periods,
    GpuPeriodStats* __restrict__ out_stats)
{
    int period_idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (period_idx >= num_periods) return;
    
    int offset = offsets[period_idx];
    int count = counts[period_idx];
    
    double v1_min = DBL_MAX;
    double v1_max = -DBL_MAX;
    double v11_sum = 0.0;
    int64_t negative_v7_count = 0;
    double amount_sum = 0.0;
    
    for (int i = 0; i < count; i++) {
        int record_idx = offset + i;
        double v1_val = v1[record_idx];
        double v7_val = v7[record_idx];
        double v11_val = v11[record_idx];
        double amount_val = amount[record_idx];
        
        v1_min = fmin(v1_min, v1_val);
        v1_max = fmax(v1_max, v1_val);
        v11_sum += v11_val;
        if (v7_val < 0.0) {
            negative_v7_count++;
        }
        amount_sum += amount_val;
    }
    
    GpuPeriodStats stats;
    stats.period = unique_periods[period_idx];
    stats.v1_min = v1_min;
    stats.v1_max = v1_max;
    stats.v11_sum = v11_sum;
    stats.negative_v7_count = negative_v7_count;
    stats.amount_sum = amount_sum;
    stats.count = count;
    out_stats[period_idx] = stats;
}

// ============================================================================
// Kernel: переупорядочивание данных по индексам
// ============================================================================

__global__ void reorder_data_kernel(
    const double* __restrict__ src_v1, const double* __restrict__ src_v7,
    const double* __restrict__ src_v11, const double* __restrict__ src_amount,
    const int* __restrict__ indices,
    double* __restrict__ dst_v1, double* __restrict__ dst_v7,
    double* __restrict__ dst_v11, double* __restrict__ dst_amount,
    int n)
{
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx < n) {
        int src_idx = indices[idx];
        dst_v1[idx] = src_v1[src_idx];
        dst_v7[idx] = src_v7[src_idx];
        dst_v11[idx] = src_v11[src_idx];
        dst_amount[idx] = src_amount[src_idx];
    }
}

// ============================================================================
// Проверка доступности GPU
// ============================================================================

extern "C" int gpu_is_available() {
    int n = 0;
    cudaError_t err = cudaGetDeviceCount(&n);
    if (err != cudaSuccess) return 0;
    return (n > 0) ? 1 : 0;
}

// ============================================================================
// Главная функция агрегации на GPU для кредитных карт (без CUB)
// ============================================================================

extern "C" int gpu_aggregate_periods(
    const double* h_v1,
    const double* h_v7,
    const double* h_v11,
    const double* h_amount,
    int num_records,
    double interval,
    GpuPeriodStats** h_out_stats,
    int* out_num_periods)
{
    if (num_records == 0) {
        *h_out_stats = nullptr;
        *out_num_periods = 0;
        return 0;
    }
    
    double total_start = get_time_ms();
    
    // ========================================================================
    // Шаг 1: Выделение памяти и копирование данных на GPU
    // ========================================================================
    double step1_start = get_time_ms();
    
    double* d_v1 = nullptr;
    double* d_v7 = nullptr;
    double* d_v11 = nullptr;
    double* d_amount = nullptr;
    int64_t* d_period_ids = nullptr;
    
    size_t records_bytes = num_records * sizeof(double);
    
    CUDA_CHECK(cudaMalloc(&d_v1, records_bytes));
    CUDA_CHECK(cudaMalloc(&d_v7, records_bytes));
    CUDA_CHECK(cudaMalloc(&d_v11, records_bytes));
    CUDA_CHECK(cudaMalloc(&d_amount, records_bytes));
    CUDA_CHECK(cudaMalloc(&d_period_ids, num_records * sizeof(int64_t)));
    
    CUDA_CHECK(cudaMemcpy(d_v1, h_v1, records_bytes, cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_v7, h_v7, records_bytes, cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_v11, h_v11, records_bytes, cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_amount, h_amount, records_bytes, cudaMemcpyHostToDevice));
    
    double step1_ms = get_time_ms() - step1_start;
    
    // ========================================================================
    // Шаг 2: Вычисление period_id для каждой записи
    // ========================================================================
    double step2_start = get_time_ms();
    
    const int BLOCK_SIZE = 256;
    int num_blocks = (num_records + BLOCK_SIZE - 1) / BLOCK_SIZE;
    
    compute_period_ids_kernel<<<num_blocks, BLOCK_SIZE>>>(
        d_v1, d_period_ids, num_records, interval);
    CUDA_CHECK(cudaGetLastError());
    CUDA_CHECK(cudaDeviceSynchronize());
    
    double step2_ms = get_time_ms() - step2_start;
    
    // ========================================================================
    // Шаг 3: Сортировка period_ids для группировки (простая сортировка на CPU)
    // ========================================================================
    double step3_start = get_time_ms();
    
    // Копируем period_ids на CPU для сортировки
    int64_t* h_period_ids = new int64_t[num_records];
    CUDA_CHECK(cudaMemcpy(h_period_ids, d_period_ids, num_records * sizeof(int64_t), 
                          cudaMemcpyDeviceToHost));
    
    // Создаем массив индексов для сортировки
    int* h_indices = new int[num_records];
    for (int i = 0; i < num_records; i++) {
        h_indices[i] = i;
    }
    
    // Сортируем индексы по period_ids
    for (int i = 0; i < num_records - 1; i++) {
        for (int j = i + 1; j < num_records; j++) {
            if (h_period_ids[h_indices[i]] > h_period_ids[h_indices[j]]) {
                int tmp = h_indices[i];
                h_indices[i] = h_indices[j];
                h_indices[j] = tmp;
            }
        }
    }
    
    // Находим уникальные периоды и их границы
    int num_periods = 0;
    int64_t* h_unique_periods = new int64_t[num_records];
    int* h_counts = new int[num_records];
    int* h_offsets = new int[num_records];
    
    if (num_records > 0) {
        h_unique_periods[0] = h_period_ids[h_indices[0]];
        h_offsets[0] = h_indices[0];
        h_counts[0] = 1;
        num_periods = 1;
        
        for (int i = 1; i < num_records; i++) {
            int idx = h_indices[i];
            if (h_period_ids[idx] == h_unique_periods[num_periods - 1]) {
                h_counts[num_periods - 1]++;
            } else {
                h_unique_periods[num_periods] = h_period_ids[idx];
                h_offsets[num_periods] = idx;
                h_counts[num_periods] = 1;
                num_periods++;
            }
        }
        
        // Вычисляем offsets (позиции в отсортированном массиве)
        int current_offset = 0;
        for (int i = 0; i < num_periods; i++) {
            h_offsets[i] = current_offset;
            current_offset += h_counts[i];
        }
    }
    
    // Копируем отсортированные данные обратно на GPU
    int64_t* d_sorted_period_ids = nullptr;
    int* d_sorted_indices = nullptr;
    CUDA_CHECK(cudaMalloc(&d_sorted_period_ids, num_records * sizeof(int64_t)));
    CUDA_CHECK(cudaMalloc(&d_sorted_indices, num_records * sizeof(int)));
    
    int64_t* h_sorted_period_ids = new int64_t[num_records];
    for (int i = 0; i < num_records; i++) {
        h_sorted_period_ids[i] = h_period_ids[h_indices[i]];
    }
    CUDA_CHECK(cudaMemcpy(d_sorted_period_ids, h_sorted_period_ids, 
                          num_records * sizeof(int64_t), cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_sorted_indices, h_indices, 
                          num_records * sizeof(int), cudaMemcpyHostToDevice));
    
    // Копируем уникальные периоды, counts и offsets на GPU
    int64_t* d_unique_periods = nullptr;
    int* d_counts = nullptr;
    int* d_offsets = nullptr;
    CUDA_CHECK(cudaMalloc(&d_unique_periods, num_periods * sizeof(int64_t)));
    CUDA_CHECK(cudaMalloc(&d_counts, num_periods * sizeof(int)));
    CUDA_CHECK(cudaMalloc(&d_offsets, num_periods * sizeof(int)));
    
    CUDA_CHECK(cudaMemcpy(d_unique_periods, h_unique_periods, 
                          num_periods * sizeof(int64_t), cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_counts, h_counts, 
                          num_periods * sizeof(int), cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_offsets, h_offsets, 
                          num_periods * sizeof(int), cudaMemcpyHostToDevice));
    
    // Переупорядочиваем данные v1, v7, v11, amount по отсортированным индексам
    double* d_v1_sorted = nullptr;
    double* d_v7_sorted = nullptr;
    double* d_v11_sorted = nullptr;
    double* d_amount_sorted = nullptr;
    CUDA_CHECK(cudaMalloc(&d_v1_sorted, records_bytes));
    CUDA_CHECK(cudaMalloc(&d_v7_sorted, records_bytes));
    CUDA_CHECK(cudaMalloc(&d_v11_sorted, records_bytes));
    CUDA_CHECK(cudaMalloc(&d_amount_sorted, records_bytes));
    
    reorder_data_kernel<<<num_blocks, BLOCK_SIZE>>>(
        d_v1, d_v7, d_v11, d_amount,
        d_sorted_indices,
        d_v1_sorted, d_v7_sorted, d_v11_sorted, d_amount_sorted,
        num_records);
    CUDA_CHECK(cudaGetLastError());
    CUDA_CHECK(cudaDeviceSynchronize());
    
    delete[] h_period_ids;
    delete[] h_indices;
    delete[] h_unique_periods;
    delete[] h_counts;
    delete[] h_offsets;
    delete[] h_sorted_period_ids;
    
    cudaFree(d_sorted_period_ids);
    cudaFree(d_sorted_indices);
    
    double step3_ms = get_time_ms() - step3_start;
    
    // ========================================================================
    // Шаг 4: Агрегация периодов
    // ========================================================================
    double step4_start = get_time_ms();
    
    GpuPeriodStats* d_out_stats = nullptr;
    CUDA_CHECK(cudaMalloc(&d_out_stats, num_periods * sizeof(GpuPeriodStats)));
    
    // Выбор ядра через переменную окружения USE_BLOCK_KERNEL
    const char* env_block_kernel = getenv("USE_BLOCK_KERNEL");
    if (env_block_kernel == nullptr) {
        printf("Error: Environment variable USE_BLOCK_KERNEL is not set\n");
        return -1;
    }
    bool use_block_kernel = (atoi(env_block_kernel) != 0);
    
    if (use_block_kernel) {
        // Блочное ядро: один блок на период, потоки параллельно обрабатывают записи
        aggregate_periods_kernel<<<num_periods, BLOCK_SIZE>>>(
            d_v1_sorted, d_v7_sorted, d_v11_sorted, d_amount_sorted,
            d_unique_periods, d_offsets, d_counts,
            num_periods, d_out_stats);
    } else {
        // Простое ядро: один поток на период
        int agg_blocks = (num_periods + BLOCK_SIZE - 1) / BLOCK_SIZE;
        aggregate_periods_simple_kernel<<<agg_blocks, BLOCK_SIZE>>>(
            d_v1_sorted, d_v7_sorted, d_v11_sorted, d_amount_sorted,
            d_unique_periods, d_offsets, d_counts,
            num_periods, d_out_stats);
    }

    CUDA_CHECK(cudaGetLastError());
    CUDA_CHECK(cudaDeviceSynchronize());
    
    double step4_ms = get_time_ms() - step4_start;
    
    // ========================================================================
    // Шаг 5: Копирование результатов на CPU
    // ========================================================================
    double step5_start = get_time_ms();
    
    GpuPeriodStats* h_stats = new GpuPeriodStats[num_periods];
    CUDA_CHECK(cudaMemcpy(h_stats, d_out_stats, num_periods * sizeof(GpuPeriodStats), 
                          cudaMemcpyDeviceToHost));
    
    double step5_ms = get_time_ms() - step5_start;
    
    // ========================================================================
    // Шаг 6: Освобождение GPU памяти
    // ========================================================================
    double step6_start = get_time_ms();
    
    cudaFree(d_v1);
    cudaFree(d_v7);
    cudaFree(d_v11);
    cudaFree(d_amount);
    cudaFree(d_period_ids);
    cudaFree(d_unique_periods);
    cudaFree(d_counts);
    cudaFree(d_offsets);
    cudaFree(d_v1_sorted);
    cudaFree(d_v7_sorted);
    cudaFree(d_v11_sorted);
    cudaFree(d_amount_sorted);
    cudaFree(d_out_stats);
    
    double step6_ms = get_time_ms() - step6_start;
    
    // ========================================================================
    // Итого
    // ========================================================================
    double total_ms = get_time_ms() - total_start;
    
    // Выводим информацию о GPU агрегации
    printf("  GPU aggregation (%d records, interval=%.1f, kernel=%s):\n",
           num_records, interval, use_block_kernel ? "block" : "simple");
    printf("    1. Malloc + H->D copy:  %7.3f ms\n", step1_ms);
    printf("    2. Compute period_ids:  %7.3f ms\n", step2_ms);
    printf("    3. Sort + group (CPU):  %7.3f ms (%d periods)\n", step3_ms, num_periods);
    printf("    4. Aggregation kernel:  %7.3f ms (%s)\n", step4_ms, use_block_kernel ? "block" : "simple");
    printf("    5. D->H copy:           %7.3f ms\n", step5_ms);
    printf("    6. Free GPU memory:     %7.3f ms\n", step6_ms);
    printf("    GPU TOTAL:              %7.3f ms\n", total_ms);
    fflush(stdout);
    
    *h_out_stats = h_stats;
    *out_num_periods = num_periods;
    
    return 0;
}

// ============================================================================
// Освобождение памяти результатов
// ============================================================================

extern "C" void gpu_free_results(GpuPeriodStats* stats) {
    delete[] stats;
}
