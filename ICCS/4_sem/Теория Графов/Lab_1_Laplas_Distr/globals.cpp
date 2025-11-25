#include "globals.h"


// 1/2 * a * exp(-1 * a * abs(x - b));


// по лапласу
int LaplaceD() {
    random_device rd;
       mt19937 mersenne(rd());

        const double a = 10; // Параметр a для распределения Лапласа масштаб ширина заполнения
        const double b = 20; // Параметр b для распределения Лапласа сдвиг увеличивается вниз но если а большое нет

        std::uniform_real_distribution<double> dist(0.0001, 0.9999);
        double u = dist(mersenne); // Генерируем u из равномерного распределения на [0, 1]
        double x;
    
        if (u < 0.5)
            x = b + (a * log(2 * u));
        else
            x = b - (a * log(2 * (1 - u)));
    
        return static_cast<int>(round(x)); // Возвращаем целую часть
}

// Общее для матрицы :
void PrintMatrix(const vector<vector<int> > mx) {
    int n = mx.size();
    int m = mx[0].size();
    cout << "   |";
    for (int i = 0; i < n; i++) {
        cout << setw(3) << i + 1 << "|";
    }
    cout << "\n";
    for (int i = 0; i < n; i++) {
        cout << setw(3) << i+1 << "|";
        for (int j = 0; j < m; j++) {
            if(mx[i][j] != INF)
                cout << setw(3) << mx[i][j] << " ";
            else
                cout << setw(3) << "inf" << " ";
        }
        cout << '\n';
    }
}
//булевая для ^ и дизъюнк
vector<vector<int> > BMultMatrixs(const vector<vector<int> > fMx, const vector<vector<int> > sMx) {
    vector<vector<int> > res(fMx.size(), vector<int>(fMx.size(), 0));
    vector<int> buf;
    for (int i = 0; i < fMx.size(); i++) {
        for (int j = 0; j < fMx.size(); j++) {
            buf.clear();
            for (int k = 0; k < fMx.size(); k++) {
                buf.push_back(fMx[i][k] && sMx[k][j]);
            }
            res[i][j] = *std::max_element(buf.begin(), buf.end());
        }
    }
    return res;
}
// сумма как алг уоршалла
vector<vector<int> > AddMatrixs(const vector<vector<int> > fMx, const vector<vector<int> > sMx) {
    vector<vector<int> > res(fMx.size(), vector<int>(fMx.size(), 0));
    for (int i = 0; i < fMx.size(); i++) {
        for (int j = 0; j < fMx.size(); j++) {
            res[i][j] = fMx[i][j] + sMx[i][j];
        }
    }
    return res;
}

// Main:
int InputVertexCount() {
    string strInp;
    bool flagInp;
    int res;
    do {
        cout << "Введите число вершин графа от " << MIN_NUM_of_VERT << " до " << MAX_NUM_of_VERT << ":\n";
        getline(cin, strInp);
        if (regex_match(strInp, kRxNumber)) {
            res = stoi(strInp);
            if ( res > MAX_NUM_of_VERT){
                cout << "неверное кол-во вершин!" << endl;
                flagInp = false;
            }
            if (res >= MIN_NUM_of_VERT && res <= MAX_NUM_of_VERT) {
                flagInp = true;
            }
            else {
                cout << "Ошибочка!" << endl;
                flagInp = false;
            }
        }
        else {
            cout << "Ошибочка!" << endl;
            flagInp = false;
        }
    } while (!flagInp);
    cout << '\n';
    return res;
}
// меню
void PrintMenu() {
    for (int i = 0; i < 18; i++) {
        cout << MenuItems[i] << " [" << i << "]" << endl;
    }
}
// меню
char InputMenuItem() {
    string strInp;
    bool flagInp;
    char res;
    do {
        cout << "Выберите пункт меню нажав нужную цифру:\n";
        getline(cin, strInp);
        if (regex_match(strInp, kRxNumber)) {
            res = stoi(strInp);
            if (res >= 0 && res < 18 /*Если будет ошибка ввода в меню !!!!!!!!!!!!*/ ) {
                flagInp = true;
            }
            else {
                cout << "Ошибочка! Введите цифру из того что есть в меню" << endl;
                flagInp = false;
            }
        }
        else {
            cout << "Ошибочка!" << endl;
            flagInp = false;
        }
    } while (!flagInp);
    cout << '\n';
    return res;
}
// продолжить ?
bool IsContinue(char& d) {
    string strInp;
    bool isOk = false;
    do {
        cout << "Продолжаем? (Y/N): ";
        getline(cin, strInp);
        if (regex_match(strInp, kRxYn)) {
            isOk = true;
            d = strInp[0];
            switch (d) {
            case 'Y':
            case 'y':
                return true;
            case 'N':
            case 'n':
                return false;
            }
        }
        else {
            isOk = false;
            cout << "Ошибочка! Вводите y/Y либо n/N! \n";
        }
    } while (!isOk);
    return d;
}


