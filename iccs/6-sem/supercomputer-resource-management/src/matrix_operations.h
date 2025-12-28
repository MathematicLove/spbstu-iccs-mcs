#ifndef MATRIX_OPERATIONS_H
#define MATRIX_OPERATIONS_H

#include <vector>

// Функция для вычисления определителя матрицы
double determinant(const std::vector<std::vector<double>>& matrix);

// Функция для получения минора матрицы
std::vector<std::vector<double>> getMinor(const std::vector<std::vector<double>>& matrix, int row, int col);

// Функция для вычисления алгебраического дополнения
double algebraicComplement(const std::vector<std::vector<double>>& matrix, int row, int col);

// Функция для создания матрицы алгебраических дополнений
std::vector<std::vector<double>> algebraicComplementMatrix(const std::vector<std::vector<double>>& matrix);

#endif  