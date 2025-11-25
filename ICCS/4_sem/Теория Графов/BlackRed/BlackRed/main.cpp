#include "Hash-Table.h"
#include "RBTree.h"
#include "BTree.h"

using namespace std;

void MenuTree() {
    cout << "В меню К-Ч дерева" << endl;
    cout << "1) Записать элемент." << endl;
    cout << "2) Запись из файла." << endl;
    cout << "3) Удалить элемент из дерева." << endl;
    cout << "4) Удалить К-Ч дерево." << endl;
    cout << "5) Вывести К-Ч дерево." << endl;
    cout << "6) Выйти." << endl;
}

void MenuHash() {
    cout << "В Меню Хэш-словаря" << endl;
    cout << "1) Записать элемент." << endl;
    cout << "2) Запись из файла." << endl;
    cout << "3) Удалить элемент из словаря." << endl;
    cout << "4) Вывести словарь." << endl;
    cout << "5) Поиск." << endl;
    cout << "6) Очистить весь словарь." << endl;
    cout << "7) Выйти." << endl;
}

void MenuBPlusTree() {
    cout << "В меню B+ дерева" << endl;
    cout << "1) Записать элемент." << endl;
    cout << "2) Запись из файла." << endl;
    cout << "3) Удалить элемент из дерева." << endl;
    cout << "4) Вывести дерево." << endl;
    cout << "5) Поиск." << endl;
    cout << "6) Удалить все дерево." << endl;
    cout << "7) Выйти." << endl;
}

int main() {
    setlocale(LC_ALL, "ru");
    int x;
    cout << "Что Вы хотите выбрать ? 0 - К-Ч дерево, 1 - ХЭШ-словарь, 2 - B+ дерево" << endl;
    cin >> x;
    if(x==0){
        {
            RBTree tree;
            MenuTree();
            string command;
            while (1) {
                cout << "Введите цифру меню: ";
                cin >> command;
                cout << endl;
                if (command == "1") {
                    string e;
                    cout << "Введите элемент: ";
                    cin >> e;
                    tree.insert(e);
                    cout << "Готово!" << endl;
                }
                else if (command == "2") {
                    tree.insertFromFile("text2.txt");
                    cout << "Готово!" << endl;
                }
                else if (command == "3") {
                    if (tree.isEmpty()) {
                        cout << "Дерево пустое(\n";
                    } else {
                        string e;
                        cout << "Введите элемент: ";
                        cin >> e;
                        if (tree.search(e)) {
                            tree.remove(e);
                            cout << "Элемент " << e << " удален!" << endl;
                        } else {
                            cout << "Такого элемента нет(" << endl;
                        }
                    }
                }
                else if (command == "4") {
                    if(tree.isEmpty()){
                        cout << "Дерева и так нет" << endl;
                    }
                    else{
                        tree.Clear();
                        cout << "Удалено дерево!" << endl;
                    }
                }
                else if (command == "5") {
                    tree.print();
                }
                else if (command == "6") break;
                else {
                    cout << "Неверная цифра!" << endl;
                }
            }
        }
    }
    
    else if (x == 1){
        HashTable h;
        string command;
        while (true) {
            MenuHash();
            cout << "Введите цифру меню: ";
            cin >> command;
            cout << endl;
            
            if (command == "1") {
                string e;
                cout << "Введите элемент: ";
                cin >> e;
                h.insert(e);
                cout << "Готово!" << endl;
            } else if (command == "2") {
                h.insertFromFile("text2.txt");
                cout << "Готово!" << endl;
            } else if (command == "3") {
                bool isDictionaryEmpty = true;
                for (int i = 0; i < h.size; i++) {
                    if (!h.data[i].data.empty()) {
                        isDictionaryEmpty = false;
                        break;
                    }
                }
                
                if (isDictionaryEmpty) {
                    cout << "Словарь и так пуст!(" << endl;
                } else {
                    string e;
                    cout << "Введите элемент: ";
                    cin >> e;
                    if (h.find(e)) {
                        h.remove(e);
                        cout << "Готово!" << endl;
                    } else {
                        cout << "Нет такого элемента!" << endl;
                    }
                }
            } else if (command == "4") {
                h.print();
            } else if (command == "7") {
                break;
            } else if (command == "5") {
                if (command == "5") {
                    bool isDictionaryEmpty = true;
                    for (int i = 0; i < h.size; i++) {
                        if (!h.data[i].data.empty()) {
                            isDictionaryEmpty = false;
                            break;
                        }
                    }
                    if (isDictionaryEmpty) {
                        cout << "Словарь, пока что пуст(" << endl;
                    } else {
                        std::string e;
                        cout << "Введите элемент для поиска: ";
                        cin >> e;
                        auto result = h.find2(e);
                        element* foundElement = result.first;
                        int index = result.second;
                        if (foundElement != nullptr) {
                            cout << "Слово: " << foundElement->data
                            << ", Индекс: " << index
                            << ", Счетчик: " << foundElement->index << endl;
                        } else {
                            cout << "Элемент не найден." << endl;
                        }
                    }
                }
            }
            else if(command == "6"){
                h.clearFromFile();
                cout << "Словарь удален" << endl;
                continue;
            }
            else {
                cout << "Неверная цифра!" << endl;
            }
        }
    }
    else if (x == 2) {
        
        int choice;
        string word;
        Bplus* bplus = new Bplus;
        vector<pair<string, string>> key_words;
        do {
            MenuBPlusTree();
            cout << "Введите пункт меню\n";
            cin >> choice;
            switch (choice) {
                case 1:
                    cout << "Введите слово: ";
                    cin >> word;
                    if (Add(bplus, word, key_words)) cout<<"Оке!\n";
                    else cout << "Уже есть!\n";
                    break;
                case 5:
                    if(bplus->Root == nullptr)
                    {
                        cout << "Дерево пустое!\n";
                    }
                    else{
                        cout << "Введите слово: ";
                        cin >> word;
                        Search(bplus, word);
                    }
                    break;
                case 3:
                    if(bplus->Root == nullptr){
                        cout << "Дерево и так пустое!\n";
                    }
                    else{
                        cout << "Введите слово: ";
                        cin >> word;
                        Del(bplus, word);
                    }
                    break;
                case 2:
                    From_txt(bplus, key_words);
                    break;
                case 4:
                    Print_tree(bplus, key_words);
                    break;
                case 6:
                    if(bplus->Root == nullptr){
                        cout << "Дерева и так нет(\n";
                    }
                    else{
                        delete bplus;
                        bplus = new Bplus;
                        cout << "Удалено!\n";
                    }
                    break;
                case 7:
                    break;
                default:
                    cout << "Неверная цифра!\n" << endl;
            }
        } while (choice != 7);
    }
    else cout << "Неверная цифра!" << endl;
    return 0;
}
