#pragma once
#include <iostream>
#include <random>
#include <string>
#include <vector>
#include <map>
#include <queue>
#include <list>
#include <algorithm>
#include <iomanip>
#include <regex>

#include <fstream>
#include <iostream>

// для распределения
#define MAX 10000
#define M 4
#define P 0.6

//Свойства графа и вес
#define MAX_POS_WEIGHT 15
#define MAX_NUM_of_VERT 51
#define MIN_NUM_of_VERT 2
#define INF 999
// 1 степени вершин затем строки 
using namespace std;
using std::min;

int LaplaceD(); //распределение Лапласа и -u для выбора т.к если не будет = 1 => вероятность 100% u по вероятности
void PrintMatrix(const vector<vector<int> > vec); //печать матриц
vector<vector<int> > BMultMatrixs(const vector<vector<int> > fMx, const vector<vector<int> > sMx); // матрицы как бул.
vector<vector<int> > AddMatrixs(const vector<vector<int> > fMx, const vector<vector<int> > sMx); // матриц по принципу &

const regex kRxYn("[YNyn]$"); // что б не дурачились
const regex kRxNumber("^(([1-9])([0-9])*)|(0)$");


static const char* MenuItems[] = { // сегодня в меню
"Метод Шимбелла _ _   _   _   _   _   _   _ ",
"Построить маршрут с n до m вершины_   _   _",
"Алгоритм Дейкстры_   _   _   _   _   _   _ ",
"Алгоритм Беллмана-Форда _   _   _   _   _  ",
"Алгоритм Флойда-Уоршалла _   _   _   _   _ ",
"Алгоритм поиска в глубину (DFS)_   _   _  _",
"Алгоритм поиска в ширину (BFS)_   _   _   _",
"Алгоритм Краскала либо Примы_  _  _  _  _ _", // если со смешанной то Прима если нет то Краскал
"Код Прюфера (Заранее создайте Мин.Ост.Гр!!)",
"Алгоритм Мин.стоим. через Эдмондса-Карпа_ _",
"Алгоритм минимального разреза графа_   _  _",
"Алгормтм А*_   _   _   _   _   _   _   _  _",
"Создать новый граф?_   _   _   _   _   _  _",
"Алгоритм Луна_   _   _   _   _   _   _    _",
"Гамельтонов цикл. мин_   _   _   _    _   _",
"Эйлеровый цикл_  _   _   _   _   _   _    _",
"Алгоритм Магу + Раскраска_   _   _   _    _",
"Выход_   _   _   _   _   _   _   _   _    _"
};

//Main:
int InputVertexCount(); //ввод вершины
void PrintMenu(); //вывод меню
char InputMenuItem(); // ввод цифры для меню
bool IsContinue(char& d); // хотите континью?


