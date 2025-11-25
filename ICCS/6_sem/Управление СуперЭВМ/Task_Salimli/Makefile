CXX = g++
CXXFLAGS = -std=c++11 -Wall -O2

TARGET = matrix_program
SRCS = main.cpp matrix_operations.cpp
OBJS = $(SRCS:.cpp=.o)

$(TARGET): $(OBJS)
	$(CXX) $(OBJS) -o $(TARGET)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) $(TARGET)

.PHONY: clean 