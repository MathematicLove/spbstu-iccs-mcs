#include "csv_loader.hpp"
#include <fstream>
#include <sstream>
#include <iostream>
#include <stdexcept>
#include <numeric>

bool parse_csv_line(const std::string& line, Record& record) {
    if (line.empty()) {
        return false;
    }
    
    std::stringstream ss(line);
    std::string item;
    
    try {
        // id (skip)
        if (!std::getline(ss, item, ',')) return false;
        
        // V1 (column 2)
        if (!std::getline(ss, item, ',')) return false;
        record.v1 = std::stod(item);
        
        // V2-V6 (skip)
        for (int i = 0; i < 5; i++) {
            if (!std::getline(ss, item, ',')) return false;
        }
        
        // V7 (column 8)
        if (!std::getline(ss, item, ',')) return false;
        record.v7 = std::stod(item);
        
        // V8-V10 (skip)
        for (int i = 0; i < 3; i++) {
            if (!std::getline(ss, item, ',')) return false;
        }
        
        // V11 (column 12)
        if (!std::getline(ss, item, ',')) return false;
        record.v11 = std::stod(item);
        
        // V12-V28 (skip)
        for (int i = 0; i < 17; i++) {
            if (!std::getline(ss, item, ',')) return false;
        }
        
        // Amount (column 30)
        if (!std::getline(ss, item, ',')) return false;
        record.amount = std::stod(item);
        
        // Filter: only Amount >= 9000.00
        if (record.amount < 9000.00) {
            return false;
        }
        
        return true;
    } catch (const std::exception&) {
        return false;
    }
}

std::vector<Record> load_csv_parallel(int rank, int size) {
    std::vector<Record> data;
    
    // Читаем настройки из переменных окружения
    std::string data_path = get_data_path();
    std::vector<int> shares = get_data_read_shares();
    int64_t overlap_bytes = get_read_overlap_bytes();
    
    // Выводим информацию о распределении (только на Rank 0)
    if (rank == 0) {
        std::cout << "Rank 0: Data distribution shares: ";
        for (size_t i = 0; i < shares.size(); i++) {
            std::cout << shares[i];
            if (i < shares.size() - 1) std::cout << ",";
        }
        std::cout << " (total: " << std::accumulate(shares.begin(), shares.end(), 0) << ")" << std::endl;
    }
    
    // Получаем размер файла
    int64_t file_size = get_file_size(data_path);
    
    // Вычисляем диапазон байт для этого ранка
    ByteRange range = calculate_byte_range(rank, size, file_size, shares, overlap_bytes);
    
    // Открываем файл и читаем нужный диапазон
    std::ifstream file(data_path, std::ios::binary);
    if (!file.is_open()) {
        throw std::runtime_error("Cannot open file: " + data_path);
    }
    
    // Переходим к началу диапазона
    file.seekg(range.start);
    
    // Читаем данные в буфер
    int64_t bytes_to_read = range.end - range.start;
    std::vector<char> buffer(bytes_to_read);
    file.read(buffer.data(), bytes_to_read);
    int64_t bytes_read = file.gcount();
    
    file.close();
    
    // Преобразуем в строку для удобства парсинга
    std::string content(buffer.data(), bytes_read);
    
    // Находим позицию начала первой полной строки
    size_t parse_start = 0;
    if (rank == 0) {
        // Первый ранк: пропускаем заголовок (первую строку)
        size_t header_end = content.find('\n');
        if (header_end != std::string::npos) {
            parse_start = header_end + 1;
        }
    } else {
        // Остальные ранки: начинаем с первого \n (пропускаем неполную строку)
        size_t first_newline = content.find('\n');
        if (first_newline != std::string::npos) {
            parse_start = first_newline + 1;
        }
    }
    
    // Находим позицию конца последней полной строки
    size_t parse_end = content.size();
    if (rank != size - 1) {
        // Не последний ранк: ищем последний \n
        size_t last_newline = content.rfind('\n');
        if (last_newline != std::string::npos && last_newline > parse_start) {
            parse_end = last_newline;
        }
    }
    
    // Парсим строки
    size_t pos = parse_start;
    while (pos < parse_end) {
        size_t line_end = content.find('\n', pos);
        if (line_end == std::string::npos || line_end > parse_end) {
            line_end = parse_end;
        }
        
        std::string line = content.substr(pos, line_end - pos);
        
        // Убираем \r если есть (Windows line endings)
        if (!line.empty() && line.back() == '\r') {
            line.pop_back();
        }
        
        Record record;
        if (parse_csv_line(line, record)) {
            data.push_back(record);
        }
        // Если парсинг не удался, просто пропускаем строку (не критичная ошибка)
        
        pos = line_end + 1;
    }
    
    return data;
}
