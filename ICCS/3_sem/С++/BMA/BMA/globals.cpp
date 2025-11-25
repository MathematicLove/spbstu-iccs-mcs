#include "globals.h"
#include "Calculator.h"
void Delay(int i) {
    while (i--) {};
}
void STRtovec(vector<char>& fm, vector<char>& sm, string inpF, string inpS) {
    char ta;
    for (int i = 7; i >= inpF.length(); i--) {
        fm.push_back('a');
    }
    for (int j = 0; j < inpF.length(); j++) {
        ta = inpF[j];
        fm.push_back(ta);
    }
    ta = 0;
    for (int i = 7; i >= inpS.length(); i--) {
        sm.push_back('a');
    }
    for (int j = 0; j < inpS.length(); j++) {
        ta = inpS[j];
        sm.push_back(ta);
    }
}
void Neg(vector<char>& vec) {
    for (int i = 0; i < TAB_SZ; i++) {
        if (vec[i] != 0) {
            vec[i] = vec[i] * (-1);
            break;
        }
    }
};
int CountZero(vector<char> f) {
    int count0 = 0;
    for (int i = 0; i < TAB_SZ; i++) {
        if (f[i] == 'a')
            count0++;
        else if (f[i] != 'a')
            return count0;
    }
    return count0++;
}
void PrintVec(vector<char>& vec) {
    bool flagP = true;
    for (int i = 0; i < TAB_SZ; i++) {
        if (!flagP)
            cout << vec[i];
        else if (vec[i] != 'a' && flagP || i == 7) {
            if (vec[i] < 0) {
                cout << '-';
                cout << static_cast<char>(vec[i] * -1);
                flagP = false;
            }
            else {
                cout << vec[i];
                flagP = false;
            }
        }
    }
    cout << endl;
}


void BinGray()
{
    int nBinG, zam, turnit;
    cin >> nBinG;
    cout << "Bin value of entering num is : " << endl;
    for (int i = (sizeof(int)* 8) - 1; i >= 0; i--) {
        zam = nBinG >> i;
        cout << (nBinG & 1);
        turnit = (nBinG & 1);
    }
    pow(turnit, 2);
    cout << " ";
    cout << " " << endl;
    
}

bool GetNum(string& n) {
    cout << "Введите буквы: ";
    string strInp;
    getline(cin, strInp);
    if (regex_match(strInp, RXChar)) {
        n = strInp;
        cout << " Окей !" << endl;
        return true;
    }
    else {
        cout << "Ошибка !" << endl;
        return false;
    }
}

bool GetOp(char& o) {
    cout << "Выбирите опирацию (+,-,*,/) : ";
    string strInp;
    getline(cin, strInp);
    if (regex_match(strInp, RXOp)) {
        o = strInp[0];
        cout << "Окей !" << endl;
        return true;
    }
    else {
        cout << "Ошибка !" << endl;
        return false;
    }
}

bool GetQuit(char& quit) {
    while (1) {
        cout << "Продолжить ? Y/N : ";
        string strInp;
        getline(cin, strInp);
        if (regex_match(strInp, RXQuit)) {
            quit = strInp[0];
            switch (quit)
            {
            case 'y':
                return false;
                break;
            case 'Y':
                return false;
                break;
            case 'n':
                cout << " Пока !" << endl;
                return true;
                break;
            case 'N':
                cout << " Пока !" << endl;
                return true;
                break;
            }
        }
        else
            cout << "Ошибка!" << endl;
    }
}

