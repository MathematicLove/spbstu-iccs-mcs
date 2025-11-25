#include "globals.h"
#include "Calculator.h"

int main() {
    setlocale(LC_ALL, "Russian");
    cout << "Порядок по возрастанию(от 0 до 7): a b e c g h d f " << endl;
    cout << "Вводите до 8 значений" << endl;
    PrintTables();

    string inp1, inp2;
    char op, qt;
    vector<char> fm, sm;
    vector<char> rez = { 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a' };
    vector<char> ost = { 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a' };
    do {
        while (!GetNum(inp1)) {}
        while (!GetOp(op)) {}
        while (!GetNum(inp2)) {}
        switch (op)
        {
        case '+':
            DoPlus(inp1, inp2, fm, sm, rez);
            break;
        case '-':
            DoSub(inp1, inp2, fm, sm, rez);
            break;
        case '*':
            DoMult(inp1, inp2, fm, sm, rez);
            break;
        case '/':
            DoDiv(inp1, inp2, fm, sm, rez, ost);
            break;
        }
        fm.erase(fm.begin(), fm.end());
        sm.erase(sm.begin(), sm.end());
        rez = { 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a' };
        ost = { 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a' };
        cout << endl;
    } while (GetQuit(qt) != true);
    return 0;
}

