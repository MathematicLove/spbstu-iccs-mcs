#include "Hash-Table.h"


HashTable::HashTable() {
    data = new element[100];
    size = 100;
}

HashTable::~HashTable() {
    for (int i = 0; i < size; i++) {
        std::vector<element*> deleted;
        while (data[i].ref != NULL) {
            element* current = &data[i];
            if (current->ref) {
                while (current->ref->ref) {
                    current = current->ref;
                }
            }
            delete current->ref;
            current->ref = NULL;
        }
    }
}

// 1
uint32_t HashTable::FNV1aHash(const std::string& key) {
    const uint32_t prime = 0x01000193; // FNV-1a 1
    uint32_t hash = 0x811c9dc5; // FNV-1a потом

    for (char c : key) {
        hash ^= static_cast<uint8_t>(c);
        hash *= prime;
    }
    return hash;
}

// 2
uint32_t HashTable::SecondHash(const std::string& key) {
    const uint32_t prime = 0x01000193;
    uint32_t hash = 0x811c9dc5;
    for (char c : key) {
        hash ^= static_cast<uint8_t>(c);
        hash += prime;
    }
    return hash;
}

int HashTable::HashFunc(std::string el) {
    uint32_t hashValue = FNV1aHash(el);
    int index = hashValue % size;
    uint32_t secondHashValue = hashValue | 1;
    int attempt = 0;
    int finalIndex = index;
    while (data[finalIndex].data != "" && data[finalIndex].data != el) {
        //новый индекс с второым хэшированием что бы было все гуд
        finalIndex = (index + attempt * secondHashValue) % size;
        attempt++;
    }

    return finalIndex;
}

void HashTable::insert(string k) {
    if (this->find(k) == NULL) {
        int id = HashFunc(k);
        element d;
        d.data = k;
        d.ref = NULL;
        if (data[id].data == "") {
            data[id] = d;
        }
        else {
            element* current = &data[id];
            while (current->ref) {
                current = current->ref;
            }
            current->ref = new element;
            current->ref->data = k;
            current->ref->ref = NULL;
        }
    }
    else {
        if (this->find(k)->data == k) {
            this->find(k)->index++;
        }
        else {
            this->find(k)->ref->index++;
        }
    }
}

void HashTable::insertFromFile(std::string file) {
    
    std::locale::global(std::locale(""));

   
    std::ifstream myfile(file);


    if (myfile.is_open()) {
        myfile.imbue(std::locale(""));
        std::string word;

        // Считываем весь файл слово за словом
        while (myfile >> word) {
            this->insert(word);
        }

        myfile.close();
    } else {
        std::cerr << "Не удалось открыть файл: " << file << std::endl;
    }
}
element* HashTable::find(string k) {
    int id = HashFunc(k);
    if (data[id].data == k) {
        return &data[id];
    }
    else {
        element* current = &data[id];
        while (current->ref != nullptr) {
            if (current->ref->data == k) {
                return current;
            }
            current = current->ref;
        }
    }
    return NULL;
}
std::pair<element*, int> HashTable::find2(std::string k) {
    int id = HashFunc(k);
    if (data[id].data == k) {
        return {&data[id], id};
    } else {
        element* current = &data[id];
        int index = id;
        while (current->ref != nullptr) {
            index++;
            if (current->ref->data == k) {
                return {current->ref, index};
            }
            current = current->ref;
        }
    }
    return {nullptr, -1}; // если не найдено, возвращаем nullptr и -1 в качестве индекса
}

void HashTable::clear() {
    for (int i = 0; i < size; ++i) {
        while (data[i].ref != nullptr) {
            element* temp = data[i].ref;
            data[i].ref = temp->ref;
            delete temp;
        }
        data[i].data = "";
        data[i].index = 0;
    }
}

void HashTable::clearFromFile() {
    for (int i = 0; i < size; ++i) {
        if (data[i].data != "") {
            element* current = &data[i];
            while (current->ref != nullptr) {
                element* temp = current->ref;
                current->ref = temp->ref;
                delete temp;
            }
            data[i].data = "";
            data[i].index = 0;
        }
    }
}


void HashTable::remove(string k) {
    element* removing = find(k);
    if (removing != NULL) {
        if (removing->index > 1 && removing->data == k) {
            removing->index--;
            return;
        }
        if (removing->ref != NULL) {
            if (removing->data != k) {
                if (removing->ref->index > 1) {
                    removing->ref->index--;
                    return;
                }
                if (removing->ref->data == k) {
                    if (!removing->ref->ref) {
                        delete removing->ref;
                        removing->ref = NULL;
                    }
                    else {
                        element* del = removing->ref;
                        removing->ref->data = removing->ref->ref->data;
                        removing->ref->index = removing->ref->ref->index;
                        removing->ref->ref = removing->ref->ref->ref;
                        delete del;
                    }
                }
            }
            else {
                element* del = removing->ref;
                removing->data = removing->ref->data;
                removing->index = removing->ref->index;
                removing->ref = removing->ref->ref;
                delete del;
            }
        }
        else {
            removing->data = "";
            removing->ref = NULL;
        }
    }
}


void HashTable::print() {
    bool empty = true;
    for (int i = 0; i < size; i++) {
        if (data[i].data != "") {
            empty = false;
            
            std::cout <<i<<")"<< data[i].data << " (" << data[i].index << ")" <<std::endl;
            element current = data[i];
            while (current.ref != NULL) {
                current = *current.ref;
                std::cout << i << ")" << current.data << " (" << current.index << ")" << std::endl;
            }
        }
    }
    if (empty) {
        std::cout << "Словарь пуст!(." << std::endl;
    }
}
