#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <cmath>
#include <algorithm>
#include <cctype>

using namespace std;

struct GroupStats {
    long long count = 0;
    long long count_neg_v7 = 0;
    long double sum_v11 = 0.0L;
    long double total_amount = 0.0L;
};

static string trim(const string &s) {
    size_t a = 0, b = s.size();
    while (a < b && isspace((unsigned char)s[a])) ++a;
    while (b > a && isspace((unsigned char)s[b-1])) --b;
    return s.substr(a, b - a);
}

// CSV parser for standard quoted CSV
static vector<string> split_csv_line(const string &line) {
    vector<string> res;
    string cur;
    bool in_quotes = false;

    for (size_t i = 0; i < line.size(); ++i) {
        char c = line[i];

        if (in_quotes) {
            if (c == '"') {
                if (i + 1 < line.size() && line[i + 1] == '"') {
                    cur.push_back('"');
                    ++i;
                } else {
                    in_quotes = false;
                }
            } else {
                cur.push_back(c);
            }
        } else {
            if (c == '"') {
                in_quotes = true;
            } else if (c == ',') {
                res.push_back(cur);
                cur.clear();
            } else {
                cur.push_back(c);
            }
        }
    }

    res.push_back(cur);
    return res;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    const string input_path = "./creditcard_2023.csv";
    const string output_path = "./top50_groups.csv";

    ifstream fin(input_path);
    if (!fin.is_open()) {
        cerr << "Ошибка: не могу открыть файл " << input_path << "\n";
        return 1;
    }

    string header_line;
    if (!getline(fin, header_line)) {
        cerr << "Ошибка: пустой файл или проблемы с заголовком\n";
        return 1;
    }

    vector<string> headers = split_csv_line(header_line);
    unordered_map<string, size_t> idx;

    for (size_t i = 0; i < headers.size(); ++i)
        idx[trim(headers[i])] = i;

    const vector<string> needed = {"V1", "V7", "V11", "Amount"};
    for (const auto &n : needed) {
        if (idx.find(n) == idx.end()) {
            cerr << "Ошибка: колонка " << n << " отсутствует в файле\n";
            return 1;
        }
    }

    size_t col_v1 = idx["V1"];
    size_t col_v7 = idx["V7"];
    size_t col_v11 = idx["V11"];
    size_t col_amount = idx["Amount"];

    unordered_map<long long, GroupStats> groups;

    string line;
    long long line_no = 1;

    while (getline(fin, line)) {
        ++line_no;
        if (line.empty()) continue;

        vector<string> f = split_csv_line(line);
        if (f.size() < headers.size()) {
            cerr << "Строка " << line_no << " — мало полей, пропуск.\n";
            continue;
        }

        try {
            long double amount = stold(trim(f[col_amount]));
            if (amount < 9000.0L) continue;

            long double v1 = stold(trim(f[col_v1]));
            long double v7 = stold(trim(f[col_v7]));
            long double v11 = stold(trim(f[col_v11]));

            long long bin_index = (long long)floor(v1 / 0.1L);

            GroupStats &g = groups[bin_index];
            g.count++;
            if (v7 < 0.0) g.count_neg_v7++;
            g.sum_v11 += v11;
            g.total_amount += amount;

        } catch (...) {
            cerr << "Парсинг ошибки на строке " << line_no << ", пропуск\n";
            continue;
        }
    }

    fin.close();

    if (groups.empty()) {
        cerr << "Нет данных после фильтра Amount >= 9000\n";
        return 0;
    }

    struct GroupOut {
        long long bin;
        long double start;
        long double end;
        long long count;
        long long neg;
        long double mean_v11;
        long double total_amount;
    };

    vector<GroupOut> vec;
    vec.reserve(groups.size());

    for (const auto &p : groups) {
        const auto &s = p.second;
        if (s.count == 0) continue;

        GroupOut o;
        o.bin = p.first;
        o.start = p.first * 0.1L;
        o.end = o.start + 0.1L;
        o.count = s.count;
        o.neg = s.count_neg_v7;
        o.mean_v11 = s.sum_v11 / (long double)s.count;
        o.total_amount = s.total_amount;

        vec.push_back(o);
    }

    // 1) сортировка по количеству отрицательных V7 (спускаемся вниз)
    sort(vec.begin(), vec.end(), [](auto &a, auto &b) {
        if (a.neg != b.neg) return a.neg > b.neg;
        return a.mean_v11 > b.mean_v11;
    });

    // Выбираем группы по принципу накопления уровней neg
    vector<GroupOut> candidates;
    candidates.reserve(vec.size());

    long long need = 50;
    size_t i = 0;

    while (i < vec.size() && candidates.size() < need) {
        long long current_neg = vec[i].neg;
        size_t j = i;
        while (j < vec.size() && vec[j].neg == current_neg) {
            candidates.push_back(vec[j]);
            j++;
        }
        i = j;
    }

    // 2) Из кандидатов берем топ-50 по mean_V11
    sort(candidates.begin(), candidates.end(), [](auto &a, auto &b) {
        if (a.mean_v11 != b.mean_v11) return a.mean_v11 > b.mean_v11;
        return a.total_amount > b.total_amount;
    });

    if (candidates.size() > need) candidates.resize(need);

    // 3) Финальный вывод — сортировка по total_amount
    sort(candidates.begin(), candidates.end(), [](auto &a, auto &b) {
        return a.total_amount > b.total_amount;
    });

    ofstream fout(output_path);
    if (!fout.is_open()) {
        cerr << "Ошибка создания output файла\n";
        return 1;
    }

    fout << "bin_start,bin_end,bin_index,count,count_neg_v7,mean_V11,total_amount\n";
    fout.setf(ios::fixed);
    fout.precision(10);

    for (auto &g : candidates) {
        fout << (double)g.start << ","
             << (double)g.end << ","
             << g.bin << ","
             << g.count << ","
             << g.neg << ","
             << (double)g.mean_v11 << ","
             << (double)g.total_amount << "\n";
    }

    fout.close();

    cout << "Готово! Результат: ./top50_groups.csv\n";

    return 0;
}
