CXX = mpic++
CXXFLAGS = -std=c++17 -O2 -Wall -Wextra -Wno-cast-function-type -fopenmp

NVCC = nvcc
# Используем минимальные флаги для обхода проблем совместимости nvcc
# Убираем все стандарты C++, используем дефолтные настройки nvcc
# RTX 3060 имеет compute capability 8.6, используем sm_86
NVCCFLAGS = -O3 -arch=sm_86 -Xcompiler -fPIC --expt-relaxed-constexpr -Xcompiler -Wno-deprecated-declarations

SRC_DIR = src
BUILD_DIR = build

SRCS = $(wildcard $(SRC_DIR)/*.cpp)
OBJS = $(patsubst $(SRC_DIR)/%.cpp,$(BUILD_DIR)/%.o,$(SRCS))

TARGET = $(BUILD_DIR)/bitcoin_app

PLUGIN_SRC = $(SRC_DIR)/gpu_plugin.cu
PLUGIN = $(BUILD_DIR)/libgpu_compute.so

all: $(TARGET) $(PLUGIN)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.cpp | $(BUILD_DIR)
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(TARGET): $(OBJS) | $(BUILD_DIR)
	$(CXX) $(CXXFLAGS) $^ -o $@ -ldl

# GPU плагин - собираем на главном узле (где есть nvcc)
# nvcc 11.5 имеет проблемы с C++ стандартной библиотекой
# Пробуем разные варианты компиляции
$(PLUGIN): $(PLUGIN_SRC) | $(BUILD_DIR)
	@echo "Building GPU plugin on main node..."
	@echo "nvcc 11.5 has a known bug with C++ stdlib. Trying workarounds..."
	@if $(NVCC) -O3 -arch=sm_86 -shared \
		-Xcompiler -fPIC \
		-Xcompiler -std=c++11 \
		--compiler-options -fno-strict-aliasing \
		--compiler-options -Wno-deprecated-declarations \
		--expt-relaxed-constexpr \
		-D__GNUC__=7 \
		-D__GNUC_MINOR__=5 \
		$< -o $(PLUGIN) 2>&1; then \
		echo "GPU plugin built successfully with workaround flags"; \
	elif [ -f /usr/bin/gcc-9 ] && $(NVCC) -O3 -arch=sm_86 -shared \
		-Xcompiler -fPIC \
		-Xcompiler -std=c++11 \
		--compiler-bindir=/usr/bin/gcc-9 \
		--expt-relaxed-constexpr \
		$< -o $(PLUGIN) 2>&1; then \
		echo "GPU plugin built successfully with gcc-9"; \
	elif [ -f /usr/bin/gcc-8 ] && $(NVCC) -O3 -arch=sm_86 -shared \
		-Xcompiler -fPIC \
		-Xcompiler -std=c++11 \
		--compiler-bindir=/usr/bin/gcc-8 \
		--expt-relaxed-constexpr \
		$< -o $(PLUGIN) 2>&1; then \
		echo "GPU plugin built successfully with gcc-8"; \
	else \
		echo "GPU plugin build failed (nvcc 11.5 stdlib bug). Program will use CPU."; \
		touch $(PLUGIN); \
	fi

# Собирается только по явному запросу: make gpu-plugin
gpu-plugin: $(PLUGIN)


clean:
	rm -rf $(BUILD_DIR)

run: all
	sbatch run.slurm

.PHONY: all clean run gpu-plugin
