#!/bin/bash
#SBATCH --job-name=creditcard
#SBATCH --nodes=4
#SBATCH --ntasks=4
#SBATCH --cpus-per-task=2
#SBATCH --nodelist=ayzekcpu1,ayzekcpu2,ayzekgpu1,ayzekgpu2
#SBATCH --output=out.txt
#SBATCH --export=ALL

# Путь к файлу данных (должен существовать на всех узлах)
export DATA_PATH="/home/ayzek/creditcard_2023.csv"

# Веса процессов для балансировки нагрузки
# GPU процессы получают больше данных пропорционально весу
# По умолчанию: GPU_PROCESS_WEIGHT=1.28, CPU_PROCESS_WEIGHT=1.0
# Измените значения ниже для настройки балансировки
export GPU_PROCESS_WEIGHT=${GPU_PROCESS_WEIGHT:-1.28}   
export CPU_PROCESS_WEIGHT=${CPU_PROCESS_WEIGHT:-1.0}    
# 
# Доли данных для каждого ранка (автоматически вычисляются на основе весов процессов)
# Формат: "share1,share2,share3,share4" или оставьте пустым для автоматического вычисления
# export DATA_READ_SHARES="10,11,13,14"  # Раскомментируйте для ручной настройки (не рекомендуется)

# Размер перекрытия в байтах для обработки границ строк
export READ_OVERLAP_BYTES=131072

# Интервал агрегации (не используется для кредитных карт, но оставляем для совместимости)
export AGGREGATION_INTERVAL=60

# Использовать ли CUDA для агрегации (0 = нет, 1 = да)
# GPU будет использоваться только на узлах с доступным GPU
export USE_CUDA=1

# Использовать ли блочное ядро (быстрее для больших интервалов, 0 = нет, 1 = да)
export USE_BLOCK_KERNEL=0

# Загружаем модуль CUDA на GPU узлах (если доступен)
module load cuda 2>/dev/null || true

# Если CUDA установлена в домашней директории, добавляем в PATH
if [ -d "$HOME/cuda/bin" ]; then
    export PATH=$HOME/cuda/bin:$PATH
    export LD_LIBRARY_PATH=$HOME/cuda/lib64:$LD_LIBRARY_PATH
fi

# Переходим в директорию проекта (NFS - доступна на всех узлах)
cd /mnt/share/supercomputers

# Количество выводимых групп (по умолчанию 50)
# Можно переопределить перед sbatch: export TOP_N=100
TOP_N=${TOP_N:-50}
echo "Using TOP_N = $TOP_N"

# Путь к программе (NFS - доступна на всех узлах)
APP_PATH="/mnt/share/supercomputers/build/bitcoin_app"

# Явно передаем переменные окружения через mpirun
# GPU_PROCESS_WEIGHT и CPU_PROCESS_WEIGHT передаются автоматически через --export=ALL (если заданы)
# DATA_READ_SHARES не передаем, так как она опциональна и вычисляется автоматически
mpirun -np $SLURM_NTASKS -x USE_CUDA -x DATA_PATH -x READ_OVERLAP_BYTES -x AGGREGATION_INTERVAL -x USE_BLOCK_KERNEL -x GPU_PROCESS_WEIGHT -x CPU_PROCESS_WEIGHT $APP_PATH -n $TOP_N
