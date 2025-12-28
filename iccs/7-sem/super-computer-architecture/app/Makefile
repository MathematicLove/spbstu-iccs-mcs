# Makefile: mpicxx + nvcc, OpenMP enabled
CXX := mpicxx
NVCC := nvcc
CXXFLAGS := -std=c++17 -O3 -fopenmp -Iinclude
LDFLAGS := -fopenmp

SRCDIR := src
BUILDDIR := build
TARGET := $(BUILDDIR)/process

CUDA_AVAILABLE := $(shell command -v $(NVCC) >/dev/null 2>&1 && echo yes || echo no)

SRCS_CPP := $(SRCDIR)/main.cpp $(SRCDIR)/worker.cpp
OBJS := $(patsubst $(SRCDIR)/%.cpp,$(BUILDDIR)/%.o,$(SRCS_CPP))

.PHONY: all clean

all: prepare $(TARGET)

prepare:
	@mkdir -p $(BUILDDIR)

$(BUILDDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

ifneq ($(CUDA_AVAILABLE),no)
CUDA_OBJ := $(BUILDDIR)/cuda_ops.o
CUDA_FLAGS := -Xcompiler -fopenmp -O3
NVCC_COMPILE := $(NVCC) -c $(SRCDIR)/cuda_ops.cu -o $(CUDA_OBJ) $(CUDA_FLAGS) -Iinclude
OBJS += $(CUDA_OBJ)
LDFLAGS += -lcudart
endif

$(TARGET): $(OBJS)
ifeq ($(CUDA_AVAILABLE),yes)
	@echo "CUDA found: compiling CUDA object."
	$(NVCC_COMPILE)
endif
	$(CXX) $(CXXFLAGS) $(OBJS) -o $(TARGET) $(LDFLAGS)

clean:
	rm -rf $(BUILDDIR) $(TARGET)
