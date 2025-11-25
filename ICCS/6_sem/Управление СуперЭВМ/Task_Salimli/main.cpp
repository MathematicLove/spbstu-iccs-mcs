#include "matrix_operations.h"
#include <iostream>
#include <iomanip>

void printMatrix(const std::vector<std::vector<double>>& matrix) {
    for (const auto& row : matrix) {
        for (double val : row) {
            std::cout << std::setw(10) << std::fixed << std::setprecision(2) << val << " ";
        }
        std::cout << std::endl;
    }
}

int main() {
    int n;
    std::cout << "Введите размерность матрицы: ";
    std::cin >> n;

    std::vector<std::vector<double>> matrix(n, std::vector<double>(n));
    
    std::cout << "Введите элементы матрицы:" << std::endl;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            std::cin >> matrix[i][j];
        }
    }

    std::cout << "\nИсходная матрица:" << std::endl;
    printMatrix(matrix);

    std::vector<std::vector<double>> result = algebraicComplementMatrix(matrix);
    
    std::cout << "\nМатрица алгебраических дополнений:" << std::endl;
    printMatrix(result);

    return 0;
} 