// include/timers.h
#pragma once
#include <chrono>
#include <string>
#include <fstream>
#include <iomanip>
#include <mutex>

class Timer {
public:
    using clock_t = std::chrono::high_resolution_clock;
    Timer() { reset(); }
    void reset() { start_ = clock_t::now(); }
    double elapsed() const {
        auto now = clock_t::now();
        return std::chrono::duration<double>(now - start_).count();
    }
private:
    clock_t::time_point start_;
};

class TimingsLogger {
public:
    TimingsLogger() : out_open(false) {}
    void open(const std::string &path) {
        std::lock_guard<std::mutex> lg(mut);
        out.open(path, std::ios::out);
        out_open = out.is_open();
    }
    void log(const std::string &k, double seconds) {
        std::lock_guard<std::mutex> lg(mut);
        if (!out_open) return;
        out << k << "," << std::fixed << std::setprecision(6) << seconds << std::endl;
    }
    ~TimingsLogger() {
        std::lock_guard<std::mutex> lg(mut);
        if (out_open) out.close();
    }
private:
    std::ofstream out;
    bool out_open;
    std::mutex mut;
};
