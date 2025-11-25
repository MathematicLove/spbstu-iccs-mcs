#include <iostream>
#include <vector>
#include <fstream>


using namespace std;

struct element {
    string data;
    int index = 1;
    element* ref;
    element(string d = "", element* r = NULL) {
        data = d;
        ref = r;
    }
};



class HashTable {
public:
    int size;
    element* data;
    ~HashTable();
    HashTable();
    uint32_t FNV1aHash(const std::string &key);
    uint32_t SecondHash(const std::string& key);
    int HashFunc(string el);
    void insert(string k);
    void insertFromFile(string file);
    element* find(string k);
    std::pair<element*, int> find2(std::string k);
    void remove(string k);
    void clear();
    void clearFromFile();
    void print();
};
