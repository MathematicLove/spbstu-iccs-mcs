#pragma once
#include <algorithm>
#include <random>
#pragma warning (disable: 4786)
#include <functional>
#include <vector>
#include <stack>
#include <algorithm>
#include <cstdlib>
#include <ctime>

using std::vector;
//using std::shuffle;

struct NotShuffled {};
struct ShufflerIsEmpty {};

template<typename T>
class Shuffler {
private:
    bool isShuffled;
    vector<T> vec;
public:
    Shuffler();
    void Shuffle();
    void Push(T item);
    T Pop();
    T Peek();
    void Clear();
    vector<T> GetVector();
};

template<typename T>
Shuffler<T>::Shuffler() : isShuffled(false), vec() {}

template<typename T>
void Shuffler<T>::Shuffle() {
    if (vec.size() != 0) {
        //shuffle(vec.begin(), vec.end());
        isShuffled = true;
    }
    else {
        throw ShufflerIsEmpty();
    }
}

template<typename T>
void Shuffler<T>::Push(T item) {
    vec.push_back(item);
}

template<typename T>
T Shuffler<T>::Pop() {
    if (isShuffled) {
        if (vec.size() != 0) {
            int tmp = vec[vec.size() - 1];
            vec.erase(vec.end() - 1);
            return tmp;
        }
        else {
            throw ShufflerIsEmpty();
        }
    }
    else {
        throw NotShuffled();
    }
}

template<typename T>
T Shuffler<T>::Peek() {
    if (isShuffled) {
        if (vec.size() != 0) {
            int tmp = vec[vec.size() - 1];
            //vec.erase(vec.end() - 1);
            return tmp;
        }
        else {
            throw ShufflerIsEmpty();
        }
    }
    else {
        throw NotShuffled();
    }
}

template<typename T>
void Shuffler<T>::Clear() {
    isShuffled = false;
    vec.clear();
}

template<typename T>
vector<T> Shuffler<T>::GetVector() {
    return vec;
}
