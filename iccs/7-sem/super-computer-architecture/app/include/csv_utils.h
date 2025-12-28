// include/csv_utils.h
#pragma once
#include <string>
#include <vector>
#include <cctype>

inline static std::string trim(const std::string &s) {
    size_t a = 0, b = s.size();
    while (a < b && std::isspace((unsigned char)s[a])) ++a;
    while (b > a && std::isspace((unsigned char)s[b-1])) --b;
    return s.substr(a, b - a);
}

inline static std::vector<std::string> split_csv_line(const std::string &line) {
    std::vector<std::string> res;
    std::string cur;
    bool in_quotes = false;
    for (size_t i = 0; i < line.size(); ++i) {
        char c = line[i];
        if (in_quotes) {
            if (c == '"') {
                if (i + 1 < line.size() && line[i+1] == '"') {
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
