#!/bin/bash
#SBATCH --job-name=creditcard
#SBATCH --nodes=3
#SBATCH --ntasks=3
#SBATCH --cpus-per-task=2
#SBATCH --nodelist=ayzekcpu1,ayzekcpu2,ayzekgpu1
#SBATCH --output=out.txt
#SBATCH --export=ALL

# Путь к файлу данных (должен существовать на всех узлах)
export DATA_PATH="/home/ayzek/creditcard_2023.csv"

# Доли данных для каждого ранка (автоматически вычисляются, если не заданы)
# Формат: "share1,share2,share3,share4" или оставьте пустым для автоматического вычисления
# Автоматическое вычисление: GPU узлы получают в 2 раза больше данных
# export DATA_READ_SHARES="10,11,13,14"  # Раскомментируйте для ручной настройки

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

cd ~
# Количество выводимых групп (по умолчанию 50)
# Можно переопределить перед sbatch: export TOP_N=100
TOP_N=${TOP_N:-50}
echo "Using TOP_N = $TOP_N"
# Явно передаем переменные окружения через mpirun
# DATA_READ_SHARES не передаем, так как она опциональна и вычисляется автоматически
mpirun -np $SLURM_NTASKS -x USE_CUDA -x DATA_PATH -x READ_OVERLAP_BYTES -x AGGREGATION_INTERVAL -x USE_BLOCK_KERNEL ~/bitcoin_app -n $TOP_N
