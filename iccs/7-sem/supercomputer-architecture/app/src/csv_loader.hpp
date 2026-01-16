#pragma once
#include <string>
#include <vector>
#include "record.hpp"
#include "utils.hpp"

// Параллельное чтение CSV файла для MPI
// rank - номер текущего ранка
// size - общее количество ранков
// shares - доли данных для каждого процесса (если пуст, вычисляется автоматически)
// Возвращает вектор записей, прочитанных этим ранком
std::vector<Record> load_csv_parallel(int rank, int size, const std::vector<int>& shares = std::vector<int>());

// Парсинг одной строки CSV в Record
// Возвращает true если парсинг успешен
bool parse_csv_line(const std::string& line, Record& record);
