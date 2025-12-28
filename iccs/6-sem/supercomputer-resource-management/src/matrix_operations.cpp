#include "matrix_operations.h"
#include <cmath>

double determinant(const std::vector<std::vector<double>>& matrix) {
    int n = matrix.size();
    if (n == 1) {
        return matrix[0][0];
    }
    
    double det = 0;
    for (int j = 0; j < n; j++) {
        std::vector<std::vector<double>> minor = getMinor(matrix, 0, j);
        det += matrix[0][j] * pow(-1, j) * determinant(minor);
    }
    return det;
}

std::vector<std::vector<double>> getMinor(const std::vector<std::vector<double>>& matrix, int row, int col) {
    int n = matrix.size();
    std::vector<std::vector<double>> minor(n-1, std::vector<double>(n-1));
    
    int minorRow = 0;
    for (int i = 0; i < n; i++) {
        if (i == row) continue;
        
        int minorCol = 0;
        for (int j = 0; j < n; j++) {
            if (j == col) continue;
            minor[minorRow][minorCol] = matrix[i][j];
            minorCol++;
        }
        minorRow++;
    }
    return minor;
}

double algebraicComplement(const std::vector<std::vector<double>>& matrix, int row, int col) {
    std::vector<std::vector<double>> minor = getMinor(matrix, row, col);
    return pow(-1, row + col) * determinant(minor);
}

std::vector<std::vector<double>> algebraicComplementMatrix(const std::vector<std::vector<double>>& matrix) {
    int n = matrix.size();
    std::vector<std::vector<double>> result(n, std::vector<double>(n));
    
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            result[i][j] = algebraicComplement(matrix, i, j);
        }
    }
    return result;
} 