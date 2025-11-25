#include "MyGraph.h"
#include "globals.h"
#include <stack>
#include <algorithm>
#include <limits>
#include <ctime>
#include <random>
#include <unordered_set>
#include <set>

using namespace std;




// Конструктор
MyGraph::MyGraph(int n) : vertexCnt(n), MatrixSmejn(n, vector<int>(n, 0)),
posWeightsMatrix(n, vector<int>(n, 0)), MatrixReach(),torrent(n , vector<int>(n, 0))//,Инициализирует переменные vertexCnt, MatrixSmejn, posWeightsMatrix и MatrixReach.

{
    unsigned int start_time = clock();
    int tmp;
    random_device rd;
    mt19937 mersenne(rd()); // случайные числа
    vector<int> vertDegrees = VertPrepare();
    // если достиг до сетиа для FodrFalk алгоритма
    bool flag = true;   //избавляемся от тупиковых вершин
    for (int i = 0; i < vertexCnt-1; i++){
        flag = true;
        for (int j = 0; j < vertexCnt; j++) {
            if (MatrixSmejn[i][j] != 0) { //проверка, есть ли путь между вершинами i и j.
                flag = false; //роверка, является ли вершина i тупиковой.
                break;
            }
        }
        if (flag == true) {
            MatrixSmejn[i][i+1] = 1; //установка значения в матрице смежности между вершинами i и i + 1 равным 1.
        }
    }
    vector<int> sources; //инициализация вектора sources
    //избавление от дополнительных истоков
    flag = true;
    for (int i = 1; i < vertexCnt - 1; i++) { //цикл, который проходит по всем вершинам, кроме первой и последней
        flag = true;
        if (MatrixSmejn[0][i] == 0) { //проверка, есть ли путь между вершинами 0 и i.
            for (int j = 1; j < vertexCnt; j++) {
                if (MatrixSmejn[j][i] != 0) { //проверка, есть ли путь между вершинами j и i.
                    flag = false;
                    break;
                }
            }
            if (flag) { // тупикова вкршина?
                sources.push_back(i); //добавление вершины i в вектор sources
            }
        }
    }
    if (sources.size() != 0) { //проверка, является ли вектор sources пустым.
        for (int i = 0; i < sources.size(); i++) { //цикл, который проходит по всем вершинам в векторе sources.
            MatrixSmejn[0][sources[i]] = 1; //установка значения в матрице
        }
    }
    
     vertDegrees = VertPrepare(); // вызов функции VertPrepare для инициализации вектора vertDegrees
       for (int i = 0; i < vertexCnt - 1; ++i) {
           for (int j = i + 1; j < vertexCnt; ++j) {
               // Генерация степени вершины для связи от вершины i к вершине j
               int degree = LaplaceD();
               // Если степень больше 0, устанавливаем связь
               if (degree > 0) {
                   MatrixSmejn[i][j] = 1;
               }
           }
       }
    //MatrixSmejn[0][(vertexCnt / 2) + 1] = 0;
    //if(vertexCnt != 2)
    //MatrixSmejn[0][vertexCnt-1] = 0;
    
    AssignWeights(); //весы
    MixWeights(); //смкшанные весы
    ModifyWeights(posWeightsMatrix, modPosWeightsMx); //вызов функции ModifyWeights для модификации весов ребер в матрице posWeightsMatrix.
    ModifyWeights(mixedWeightsMatrix, modMixedWeightsMx); //вызов функции ModifyWeights для модификации весов ребер в матрицк
    ReachMatrixGenerator(); //достежимости
    AssignTorrent(); // Стоимость пропуск
    unsigned int start_end = clock();
    cout<< start_end - start_time << endl;
}
void MyGraph::printPathDFS(int startVertex, int endVertex, const vector<int>& parent, const vector<vector<int>>& weights) const {
    unsigned int start_time = clock();
    if (startVertex == endVertex) {
           cout << startVertex + 1 << " ";
           return;
       }
       if (parent[endVertex] == -1) {
           cout << "Нет пути между вершинами " << startVertex + 1 << " и " << endVertex + 1;
           return;
       }
       printPathDFS(startVertex, parent[endVertex], parent, weights);
       cout << "-> " << endVertex + 1 << " (вес: " << weights[parent[endVertex]][endVertex] << ") ";
    unsigned int start_end = clock();
    cout<< start_end - start_time << endl;
}

// Рекурсивная вспомогательная функция для обхода в глубину с поиском минимального пути
void MyGraph::DFSUtil(int v, int endVertex, vector<bool>& visited, vector<int>& visitedNodes, vector<int>& parent, int& totalDistance) {
    unsigned int start_time = clock();
    // Помечаем текущую вершину как посещенную
    visited[v] = true;
    // Добавляем вершину в список посещенных вершин
    visitedNodes.push_back(v);

    // Если достигли конечной вершины, завершаем обход
    if (v == endVertex) {
        return;
    }

    // Обходим все смежные вершины текущей вершины
    for (int i = 0; i < vertexCnt; ++i) {
        if (MatrixSmejn[v][i] && !visited[i]) {
            parent[i] = v; // Записываем предыдущую вершину для вершины i
            DFSUtil(i, endVertex, visited, visitedNodes, parent, totalDistance); // Рекурсивно вызываем DFS
        }
    }
    unsigned int start_end = clock();
    cout<< start_end - start_time << endl;
}


// Метод для печати кратчайшего пути от начальной вершины до конечной
void MyGraph::printPath(int startVertex, int endVertex, const vector<int>& parent, const vector<vector<int>>& weights) const {
    if (startVertex == endVertex) {
        cout << startVertex << " ";
        return;
    }
    if (parent[endVertex] == -1) {
        cout << "Нет пути между вершинами " << startVertex << " и " << endVertex;
        return;
    }
    printPath(startVertex, parent[endVertex], parent, weights);
    cout << "-> " << endVertex << " (вес: " << weights[parent[endVertex]][endVertex] << ") ";
   
}


// DFS с учетом весов ребер
void MyGraph::DFS(int startVertex, int endVertex) {
    vector<bool> visited(vertexCnt, false);
    vector<int> parent(vertexCnt, -1);
    stack<pair<int, int>> pathStack; // хранение вершины и веса пути от стартовой до нее

    visited[startVertex - 1] = true;
    pathStack.push({startVertex - 1, 0});

    int totalWeight = 0; // суммарный вес пути

    while (!pathStack.empty()) {
        int currentVertex = pathStack.top().first;
        int pathWeight = pathStack.top().second;
        pathStack.pop();

        if (currentVertex == endVertex - 1) { // если достигли конечной вершины, выходим из цикла
            totalWeight = pathWeight;
            break;
        }

        int shortestEdge = INT_MAX; // храним длину кратчайшего ребра
        int nextVertex = -1; // индекс следующей вершины для исследования

        // Ищем смежные вершины с наименьшим весом
        for (int i = 0; i < vertexCnt; ++i) {
            if (posWeightsMatrix[currentVertex][i] > 0 && !visited[i]) { // если есть ребро и вершина не посещена
                if (posWeightsMatrix[currentVertex][i] < shortestEdge) { // если ребро короче текущего минимального
                    shortestEdge = posWeightsMatrix[currentVertex][i];
                    nextVertex = i;
                }
            }
        }

        if (nextVertex != -1) { // если найдена следующая вершина для исследования
            visited[nextVertex] = true;
            parent[nextVertex] = currentVertex;
            // Добавляем в стек с учетом веса ребра
            pathStack.push({nextVertex, pathWeight + shortestEdge});
        }
    }

    if (!visited[endVertex - 1]) {
        cout << "Нет пути между вершинами " << startVertex << " и " << endVertex << endl;
        return;
    }

    // Восстановление кратчайшего пути
    stack<int> shortestPath;
    int currentVertex = endVertex - 1;
    while (currentVertex != -1) {
        shortestPath.push(currentVertex + 1); // добавляем 1, чтобы учесть смещение индексов
        currentVertex = parent[currentVertex];
    }

    cout << "Кратчайший путь из " << startVertex << " в " << endVertex << ": ";
    while (!shortestPath.empty()) {
        cout << shortestPath.top();
        shortestPath.pop();
        if (!shortestPath.empty()) cout << " -> ";
    }
    cout << " (суммарный вес пути: " << totalWeight << ")" << endl;

    cout << "Обход в глубину завершен." << endl;

    // Сброс состояния массивов
    visited.assign(vertexCnt, false);
    parent.assign(vertexCnt, -1);
}
void MyGraph::DFS1(int currentVertex, int endVertex, vector<int>& path, vector<bool>& visited, vector<int>& shortestPath, int& shortestWeight) {
    
    // Добавляем текущую вершину в путь
    path.push_back(currentVertex);
    visited[currentVertex] = true;

    // Если достигли конечной вершины
    if (currentVertex == endVertex - 1) {
        int totalWeight = 0;
        // Вычисляем вес пути
        for (int i = 0; i < path.size() - 1; ++i) {
            totalWeight += posWeightsMatrix[path[i]][path[i + 1]];
        }
        // Если это кратчайший путь, обновляем значения
        if (totalWeight < shortestWeight) {
            shortestWeight = totalWeight;
            shortestPath = path;
        }
    } else {
        // Продолжаем обход в глубину для всех смежных вершин
        for (int i = 0; i < vertexCnt; ++i) {
            if (posWeightsMatrix[currentVertex][i] > 0 && !visited[i]) {
                DFS1(i, endVertex, path, visited, shortestPath, shortestWeight);
            }
        }
    }

    // Возвращаемся назад и отмечаем вершину как непосещенную
    path.pop_back();
    visited[currentVertex] = false;
}
int MyGraph::DFS2(int currentVertex, int endVertex, vector<int>& path, vector<bool>& visited, vector<int>& shortestPath, int& shortestWeight) {
    // Добавляем текущую вершину в путь
    path.push_back(currentVertex);
    visited[currentVertex] = true;

    // Если достигли конечной вершины
    if (currentVertex == endVertex - 1) {
        int totalWeight = 0;
        // Вычисляем вес пути
        for (int i = 0; i < path.size() - 1; ++i) {
            totalWeight += posWeightsMatrix[path[i]][path[i + 1]];
        }
        // Если это кратчайший путь, обновляем значения
        if (totalWeight < shortestWeight) {
            shortestWeight = totalWeight;
            shortestPath = path;
        }
        return totalWeight; // Возвращаем вес пути
    } else {
        int minWeight = INT_MAX; // Инициализируем минимальный вес как максимальное значение
        // Продолжаем обход в глубину для всех смежных вершин
        for (int i = 0; i < vertexCnt; ++i) {
            if (posWeightsMatrix[currentVertex][i] > 0 && !visited[i]) {
                int weight = DFS2(i, endVertex, path, visited, shortestPath, shortestWeight);
                minWeight = min(minWeight, weight); // Обновляем минимальный вес
            }
        }
        return minWeight; // Возвращаем минимальный вес пути
    }

    // Возвращаемся назад и отмечаем вершину как непосещенную
    path.pop_back();
    visited[currentVertex] = false;
}
void MyGraph::findShortestPathDFS1(int startVertex, int endVertex) {
    unsigned int start_time = clock();
    vector<bool> visited(vertexCnt, false);
    vector<int> path;
    vector<int> shortestPath;
    int shortestWeight = INT_MAX;
    DFS1(startVertex - 1, endVertex, path, visited, shortestPath, shortestWeight);

    if (shortestWeight == INT_MAX) {
        cout << "Нет пути между вершинами " << startVertex << " и " << endVertex << endl;
        return;
    }

    cout << "Кратчайший путь из " << startVertex << " в " << endVertex << ": ";
    for (int i = 0; i < shortestPath.size(); ++i) {
        cout << shortestPath[i] + 1; // добавляем 1, чтобы учесть смещение индексов
        if (i < shortestPath.size() - 1) cout << " -> ";
    }
    cout << " (суммарный вес пути: " << shortestWeight << ")" << endl;
    cout << "Время выполнения: " << endl;
    unsigned int start_end = clock();
    cout<< start_end - start_time << " " << "мс";
    cout << " " << endl;
}
// BFS с учетом весов ребер
void MyGraph::BFS(int startVertex, int endVertex) {
    unsigned int start_time = clock();
    // Инициализация
           vector<vector<int>> allPaths; // для хранения всех найденных путей
           queue<vector<int>> q; // хранение пути

           // Начальная вершина и путь до неё
           vector<int> initialPath = {startVertex - 1};
           q.push(initialPath);

           // Обход в ширину
           while (!q.empty()) {
               auto currentPath = q.front();
               q.pop();

               int currentVertex = currentPath.back();

               // Если достигли конечной вершины, добавляем путь в список найденных путей
               if (currentVertex == endVertex - 1) {
                   allPaths.push_back(currentPath);
                   continue; // пропускаем добавление смежных вершин и продолжаем обход
               }

               // Проходим по всем смежным вершинам
               for (int i = 0; i < vertexCnt; ++i) {
                   if (posWeightsMatrix[currentVertex][i] > 0) {
                       vector<int> newPath = currentPath;
                       newPath.push_back(i);
                       q.push(newPath);
                   }
               }
           }

           // Поиск кратчайшего пути
           int shortestWeight = INT_MAX;
           vector<int> shortestPath;
           for (const auto& path : allPaths) {
               int totalWeight = 0;
               for (int i = 0; i < path.size() - 1; ++i) {
                   totalWeight += posWeightsMatrix[path[i]][path[i + 1]];
               }
               if (totalWeight < shortestWeight) {
                   shortestWeight = totalWeight;
                   shortestPath = path;
               }
           }

           // Вывод кратчайшего пути
           if (shortestWeight == INT_MAX) {
               cout << "Нет пути между вершинами " << startVertex << " и " << endVertex << endl;
               return;
           }

           cout << "Кратчайший путь из " << startVertex << " в " << endVertex << ": ";
           for (int i = 0; i < shortestPath.size(); ++i) {
               cout << shortestPath[i] + 1; // добавляем 1, чтобы учесть смещение индексов
               if (i < shortestPath.size() - 1) cout << " -> ";
           }
           cout << " (это минимальный путь с весом: " << shortestWeight << ")" << endl;
    
    cout << "Время выполнения: " << endl;
    unsigned int start_end = clock();
    cout<< start_end - start_time << " " << "мс";
    cout << " " << endl;
   }

int MyGraph::BFS1(int startVertex, int endVertex) {
    // Инициализация
           vector<vector<int>> allPaths; // для хранения всех найденных путей
           queue<vector<int>> q; // хранение пути

           // Начальная вершина и путь до неё
           vector<int> initialPath = {startVertex - 1};
           q.push(initialPath);

           // Обход в ширину
           while (!q.empty()) {
               auto currentPath = q.front();
               q.pop();

               int currentVertex = currentPath.back();

               // Если достигли конечной вершины, добавляем путь в список найденных путей
               if (currentVertex == endVertex - 1) {
                   allPaths.push_back(currentPath);
                   continue; // пропускаем добавление смежных вершин и продолжаем обход
               }

               // Проходим по всем смежным вершинам
               for (int i = 0; i < vertexCnt; ++i) {
                   if (posWeightsMatrix[currentVertex][i] > 0) {
                       vector<int> newPath = currentPath;
                       newPath.push_back(i);
                       q.push(newPath);
                   }
               }
           }

           // Поиск кратчайшего пути
           int shortestWeight = INT_MAX;
           vector<int> shortestPath;
           for (const auto& path : allPaths) {
               int totalWeight = 0;
               for (int i = 0; i < path.size() - 1; ++i) {
                   totalWeight += posWeightsMatrix[path[i]][path[i + 1]];
               }
               if (totalWeight < shortestWeight) {
                   shortestWeight = totalWeight;
                   shortestPath = path;
               }
           }

           // Вывод кратчайшего пути
           if (shortestWeight == INT_MAX) {
               cout << "Нет пути между вершинами " << startVertex << " и " << endVertex << endl;
               return 1;
           }

           cout << "Кратчайший путь из " << startVertex << " в " << endVertex << ": ";
           for (int i = 0; i < shortestPath.size(); ++i) {
               cout << shortestPath[i] + 1; // добавляем 1, чтобы учесть смещение индексов
               if (i < shortestPath.size() - 1) cout << " -> ";
           }
           cout << " (суммарный вес пути: " << shortestWeight << ")" << endl;
    return 0;
   }

    //генерация вектора (отсортированного) с размером n-2 степеней вершин
    vector<int> MyGraph::VertPrepare() const { // заполняем
        vector<int> vertexDegrees;
        int tmpVertDeg;
        for (int i = 0; i < vertexCnt-1; i++) {
             //цикл -2 потому что последняя вершина это сток и связь предпоследнего с последним в ручную в конструкторе
            do {
                tmpVertDeg = LaplaceD();
            } while (tmpVertDeg >= vertexCnt); 
            // tmpVertDeg = LaplaceD()%(vertexCnt-i-1);// + откл цикл если хотим b>a
            vertexDegrees.push_back(tmpVertDeg);
        }
        std::sort(vertexDegrees.begin(), vertexDegrees.end(), std::greater<int>());
        return vertexDegrees;
    }
    // Задаем весы случайным оброзом
    void MyGraph::AssignWeights() {
        std::random_device rd;
        std::mt19937 mersenne(rd());
        for (int i = 0; i < vertexCnt; i++) {
            for (int j = i + 1; j < vertexCnt; j++) {
                if (MatrixSmejn[i][j] != 0)
                   //posWeightsMatrix[i][j] = LaplaceD();
                    posWeightsMatrix[i][j] = mersenne() % (MAX_POS_WEIGHT - 1) + 1;
            }
        }
    }

void MyGraph::MixWeights() {
    mixedWeightsMatrix = posWeightsMatrix;
    int posOrNeg;
    std::random_device rd;
    std::mt19937 mersenne(rd());
    for (int i = 0; i < vertexCnt; i++) {
        for (int j = i + 1; j < vertexCnt; j++) {
            if (mixedWeightsMatrix[i][j] != 0) {
                posOrNeg = mersenne() % 2;
                if (posOrNeg == 1) {
                    mixedWeightsMatrix[i][j] *= -1;
                }
            }
        }
    }
}

    void MyGraph::ModifyWeights(vector<vector<int> >& WeightsMatrix, vector<vector<int> >& modified) { // для Флойда-Уоршелла и др где 0 = inf
        modified = WeightsMatrix;
        for (int i = 0; i < vertexCnt; i++) {
            for (int j = 0; j < vertexCnt; j++) {
                if (modified[i][j] == 0) {
                    modified[i][j] = INF;
                }
            }
        }
    }

    int MyGraph::GetVertexCount() const {
        return vertexCnt;
    }

    vector<vector<int> > MyGraph::GetAdjMatrix() const {
        return MatrixSmejn;
    }

    vector<vector<int> > MyGraph::GetWeightsMatrix(int type) const {
        switch (type) {
        case 0:
            return posWeightsMatrix; // что будет приниматся позитив веса или смешанные или дальнейшее модифицированные с inf 
            break;
        case 1:
            return mixedWeightsMatrix;
            break;
        case 2:
            return modPosWeightsMx;
            break;
        case 3:
            return modMixedWeightsMx;
            break;
        default:
            break;
        }
        return GetWeightsMatrix(3);
    }

    vector<vector<int>> MyGraph::GetTorrentMatrix() const{
        return torrent;
    }

//Метод Шимбелла-------------------------------------------
// тутт для умножения двух матриц с использованием алгоритма Шимбелла. Он принимает две матрицы firstM и secM и режим mode, который может быть ShimbellMode::Short или ShimbellMode::Long. Если mode установлен в ShimbellMode::Short, то метод возвращает минимальное значение в каждой ячейке результирующей матрицы. Если mode установлен в ShimbellMode::Long, то метод возвращает максимальное значение в каждой ячейке результирующей матрицы.
//Метод CalcShimbell используется для вычисления матрицы кратчайших путей. Он принимает количество ребер в графе и режим mode. Метод проходит по всем ребрам графа и умножает матрицу весов на себя edgeCnt - 1 раз с использованием метода MultByShimbell.
vector<vector<int> > MyGraph::MultByShimbell(const vector<vector<int> > firstM, const vector<vector<int> > secM, ShimbellMode mode) const {
    vector<vector<int> > resM(vertexCnt, vector<int>(vertexCnt, 0));
    vector<int> tmp;
    bool isNotZero;
    for (int i = 0; i < vertexCnt; i++) {
        for (int j = 0; j < vertexCnt; j++) {
            tmp.clear();
            isNotZero = false;
            for (int k = 0; k < vertexCnt; k++) {
                // элемент строки + элемент столбца как при сложение эл матриц
                if ((firstM[i][k] != 0) && (secM[k][j] != 0)) {
                    tmp.push_back(firstM[i][k] + secM[k][j]);
                    isNotZero = true;
                }
            }
            if (isNotZero) {
                if (mode == ShimbellMode::Short) {
                    resM[i][j] = *std::min_element(tmp.begin(), tmp.end());
                }
                else {    //mode == ShimbellMode::Long
                    resM[i][j] = *std::max_element(tmp.begin(), tmp.end());
                }
            }
            else {
                resM[i][j] = 0;
            }
        }
    }
    return resM;
}
// a * b = b * a = a + b => max\min(a+b)i
// a + 0 = 0 + a = 0
vector<vector<int> > MyGraph::CalcShimbell(int edgeCnt, ShimbellMode mode) const {
    vector<vector<int> > resM = posWeightsMatrix;
    for (int i = 0; i < edgeCnt - 1; i++) {
        resM = MultByShimbell(resM, posWeightsMatrix, mode);
    }
    return resM;
}

//Матрица достижимости с кол-вом путей---------------------
void MyGraph::ReachMatrixGenerator() { // используем Уоршелла
    vector<vector<int> > boolExpMatrix = MatrixSmejn;
    MatrixReach = vector<vector<int> >(vertexCnt, vector<int>(vertexCnt, 0));
    for (int i = 0; i < vertexCnt - 1; i++) {
        boolExpMatrix = BMultMatrixs(boolExpMatrix, MatrixSmejn);
        MatrixReach = AddMatrixs(MatrixReach, boolExpMatrix);
    }
    for (int i = 0; i < vertexCnt; i++) {
        MatrixReach[i][i] = 1;
    }
}
vector<vector<int> > MyGraph::GetReachMatrix() const {
    return MatrixReach;
}

vector<vector<int>> CreateUndirectedMatrix(const vector<vector<int>>& weightsMatrix) {
    int vertexCnt = weightsMatrix.size();
    vector<vector<int>> undirectedMatrix(vertexCnt, vector<int>(vertexCnt, 0));
    for (int i = 0; i < vertexCnt; ++i) {
        for (int j = i + 1; j < vertexCnt; ++j) {
            if (weightsMatrix[i][j] != 0) {
                undirectedMatrix[i][j] = weightsMatrix[i][j]; // Ориентированное ребро
                undirectedMatrix[j][i] = weightsMatrix[i][j]; // Обратное ребро для создания неориентированного графа
            }
        }
    }
    return undirectedMatrix;
}

//Инициализация: Для каждой вершины v в графе, установите d(v) = ∞, где d(v) - расстояние от начальной вершины до вершины v. Установите d(s) = 0, где s - начальная вершина.
//Рассмотрение вершин: Рассмотрите каждую вершину v по очереди. Если вершина v уже была рассмотрена, пропустите ее.
//Обновление расстояний: Если d(v) < d(u) + w(u, v), где w(u, v) - вес ребра между вершинами u и v, обновите d(u) = d(v) + w(u, v) для всех вершин u, связанных с v.
//Повторение: Повторяйте шаги 2 и 3, пока все расстояния не будут обновлены.
//Завершение: После завершения алгоритма, кратчайшие пути до всех вершин будут известны.

vector<int> MyGraph::Dijkstra(int vertNum, int& counter) const { //O(V3)/ O(V*E*log(V))
    vector<int> distances(modPosWeightsMx[vertNum]); //инициализация массива расстояний.
    distances[vertNum] = 0; // начальное = 0

    vector<bool> isVisitedVector(vertexCnt, false); // инициализация массива посещенных вершин.
    isVisitedVector[vertNum] = true; //зашли в вершину

    int min, curVert = vertexCnt - 1; //нициализация переменной min и переменной curVert.

    for (int i = 0; i < vertexCnt; i++) { //цикл, который проходит по всем вершинам.
        min = INF; //инициализация переменной min значением INF
        // Поиск минимальной вершины
        for (int j = 0; j < vertexCnt; j++, counter++) {
            if (!isVisitedVector[j] && (distances[j] < min)) { //проверка, является ли вершина j не посещенной и имеет ли она меньшее расстояние, чем переменная min.
                min = distances[j]; //обновление переменной min на значение расстояния до вершины j.
                curVert = j; //обновление переменной curVert на значение индекса вершины j.
            }
        }
        // Соседи этой вершины
        isVisitedVector[curVert] = true; // установка флага посещенной для вершины curVert.
        //проверка, является ли вершина j не посещенной, есть ли путь между вершинами curVert и j, не является ли текущее расстояние до вершины curVert бесконечным и является ли сумма расстояния до вершины curVert и веса ребра между ними меньше текущего расстояния до вершины j.
        for (int j = 0; j < vertexCnt; j++, counter++) {
            if (!isVisitedVector[j] && (modPosWeightsMx[curVert][j] != INF)
                && (distances[curVert] != INF) && ((distances[curVert] + modPosWeightsMx[curVert][j]) < distances[j]))
            {
                distances[j] = distances[curVert] + modPosWeightsMx[curVert][j];//обновление расстояния до вершины j на сумму расстояния до вершины curVert и веса ребра между ними.
            }
        }
    }
    return distances; //возврат расстояний.
}


//Инициализируется вектор paths, который будет содержать пути от начальной вершины до всех остальных вершин.
//Для каждой вершины i в графе, если расстояние до этой вершины не равно нулю (то есть, это не исходная вершина), то выполняются следующие шаги:1. Если путь до вершины i существует (то есть, расстояние до нее не равно INF), то текущая вершина curVert устанавливается в i, и путь до этой вершины добавляется в вектор путей.2. Пока текущая вершина curVert не равна исходной вершине, выполняется следующий шаг:   i. Для каждой смежной вершины j текущей вершины curVert, если вес ребра между ними не равен INF, и если разность расстояний до текущей вершины и веса ребра равна расстоянию до смежной вершины, то текущая вершина curVert устанавливается в j, и путь до этой вершины добавляется в вектор путей.
//3.Если путь до вершины i не существует (то есть, расстояние до нее равно INF), то путь до этой вершины добавляется в вектор путей.
//После прохождения по всем вершинам, вектор путей возвращается.
vector<vector<int> > MyGraph::RestorePaths(int inpVert, const vector<int>& distances, const vector<vector<int> > weightMx) const {
    vector<vector<int> > paths(vertexCnt, vector<int>());
    int tmp, curVert;
    for (int i = 0; i < vertexCnt; i++) {
        if (distances[i] != 0) {        //== 0 -- исходная вершина
            if (distances[i] != INF) {    //Если есть путь
                curVert = i;
                paths[i].push_back(curVert);
                while (curVert != inpVert) {    //Пока не дошли до исходной вершины
                    for (int j = 0; j < vertexCnt; j++) {    //Ищем такую смежную вершину с нынешней что бы
                                                            //значения ее метки было = разности значений
                                                            //метки нынешней и веса ребра между ними
                        if (weightMx[j][curVert] != INF) { // если есть ребра
                            if ((distances[curVert] - weightMx[j][curVert]) == distances[j]) { // вычисляем
                                curVert = j;
                                paths[i].push_back(j);
                                break;
                            }
                        }
                    }
                }
            }
            else {
                paths[i].push_back(INF);
            }
        }
        else {
            paths[i].push_back(INF);
        }
    }
    return paths;
}



vector<int> MyGraph::BellmanFord(int inpVert, int& counter, vector<vector<int> > mx) const {
    counter = 0;
    vector<int> distances(vertexCnt, INF);
    distances[inpVert] = 0;

    int curVert, newDistance;

    deque<int> dq;
    dq.push_back(inpVert);
    while (!dq.empty()) {
        curVert = dq.front();
        dq.pop_front();
        for (int i = curVert + 1; i < vertexCnt; i++) {
            if (mx[curVert][i] != INF) {
                newDistance = distances[curVert] + mx[curVert][i];
                if (newDistance < distances[i]) {
                    distances[i] = newDistance;
                    if (std::find(dq.begin(), dq.end(), i) == dq.end()) {
                        dq.push_back(i);
                    }
                    else {
                        std::remove(dq.begin(), dq.end(), i);
                        dq.push_front(i);
                    }
                    // Увеличиваем счетчик только при обновлении расстояния
                    counter++;
                }
            }
            // Увеличиваем счетчик даже если ребро отсутствует,
            // чтобы отразить посещение каждой вершины
            counter++;
        }
    }
    return distances;
}

//Инициализируется вектор distances, который будет содержать расстояния от начальной вершины до всех остальных вершин. Начальное расстояние до начальной вершины устанавливается равным 0, а для всех остальных вершин - равным бесконечности.
//Создается очередь dq, в которую добавляется начальная вершина.
//Пока очередь dq не пуста, выполняются следующие шаги:a. Из начала очереди dq извлекается текущая вершина curVert.b. Для каждой вершины i, смежной с текущей вершиной curVert, если существует путь между ними, то вычисляется новое расстояние newDistance от начальной вершины до вершины i.c. Если новое расстояние меньше текущего расстояния до вершины i, то текущее расстояние обновляется. Если вершина i еще не была добавлена в очередь dq, то она добавляется в конец очереди. Если вершина i уже была добавлена в очередь dq, то она удаляется из очереди и добавляется в начало очереди.
//После прохождения по всем вершинам, вектор distances возвращается.


vector<int> MyGraph::BellmanFord(int inpVert, vector<vector<int> > mx) const { //для весов сложность O(|V||E|) против O(|E| + |V|\ln(|V|))
    vector<int> distances(vertexCnt, INF);
    distances[inpVert] = 0;
    int counter = 0;
    int curVert, newDistance;

    deque<int> dq;
    dq.push_back(inpVert);

    while (!dq.empty()) {
        curVert = dq.front();
        dq.pop_front();
        for (int i = curVert + 1; i < vertexCnt; i++, counter++) {
            if (mx[curVert][i] != INF) {
                newDistance = distances[curVert] + mx[curVert][i];
                if (newDistance < distances[i]) {
                    distances[i] = newDistance;
                    if (std::find(dq.begin(), dq.end(), i) == dq.end()) {        //этой вершины нет в очереди
                        dq.push_back(i);                                        //добавили в конец
                    }
                    else {                                                        //вершина есть в очереди
                        std::remove(dq.begin(), dq.end(), i);                    //удалили ее из очереди
                        dq.push_front(i);                                        //добавили в начало очереди
                    }
                }
            }
        }
    }
    return distances;
}

vector<vector<int> > MyGraph::FloydWarshall(int& counter, vector<vector<int> > mx) const { //O(nˆ3) time O(nˆ2) mem берем только ребра => матрицу расстояний и перебераем затем присваеваем вершины k  а ребра i - j
    vector<vector<int> > distancesMx = mx; //инициализация матрицы расстояний.
    for (int i = 0; i < vertexCnt; i++) { //цикл, который проходит по всем вершинам.
        for (int j = 0; j < vertexCnt; j++) {
            for (int k = 0; k < vertexCnt; k++, counter++) {
                if (distancesMx[i][k] != INF && distancesMx[k][j] != INF) { //проверка, есть ли путь между вершинами i, k и k, j.
                    distancesMx[i][j] = min(distancesMx[i][j], (distancesMx[i][k] + distancesMx[k][j]));  //обновление расстояния между вершинами
                }
            }
        }
    }

    for (int i = 0; i < vertexCnt; i++) {
        distancesMx[i][i] = 0; //становка расстояния от вершины до самой себя равным 0. (так как граф без узлов)
    }

    return distancesMx;
}

// Алгоритм Прима что бы найти остовый граф (положительная матрица)
//Инициализируется вектор key, который будет содержать информацию о весе ключа для каждой вершины и индексе ребра. Также инициализируется вектор inMST, который будет содержать информацию о включении вершин в MST (минимальное остовное дерево).
//Создается приоритетная очередь pq, которая будет содержать пары (вес, вершина).
//Начинается с первой вершины и добавляется в приоритетную очередь.
//Пока приоритетная очередь не пуста, выполняются следующие шаги:a. Извлекается вершина с наименьшим весом из приоритетной очереди.b. Вершина включается в MST.c. Для каждой смежной вершины, если она еще не включена в MST и вес ребра между ними меньше текущего веса ключа, то ключ обновляется и вершина добавляется в приоритетную очередь.
//После прохождения по всем вершинам, создается и возвращается список рёбер MST.
bool MyGraph::isAchievable(int vertexOne, int vertexTwo, vector<vector<int>>& graph, int lastVertex) const {
    for (int i = 0; i < vertexCnt; i++) {
        if ((graph[i][vertexOne] != 0 || graph[vertexOne][i] != 0) && i != lastVertex) {
            if (i == vertexTwo) return true;
            if (isAchievable(i, vertexTwo, graph, vertexOne)) return true;
        }
    }
    return false;
}

//Алгоритм Краскала работает следующим образом: он сортирует все рёбра графа по весу и затем добавляет их в минимальное остовное дерево, если они не создают цикл. Алгоритм Прима начинается с произвольной вершины и добавляет рёбра с наименьшим весом, которые не создают цикл.
//Код Прюфера представляет собой список пар вершин, который может быть использован для восстановления минимального остовного дерева. Код Прюфера генерируется путём выбора рёбер, которые ещё не были добавлены в МОД, и добавления их в список, если они не создают цикл.
//Для декодирования кода Прюфера, каждое ребро в списке добавляется в МОД, если оно ещё не было добавлено. Затем, МОД проверяется на соответствие сгенерированному коду Прюфера.
//Код Прюфера - это метод представления минимального остовного дерева (МОД) в виде списка пар вершин. В данном коде, функция PruferCode генерирует код Прюфера, а функция PruferDecode декодирует его обратно в МОД.
//Функция PruferCode работает следующим образом:
//Она инициализирует пустой список PruferTree.
//Затем она проходит по каждой вершине в МОД и проверяет, является ли она листом (то есть имеет ли она только одно ребро).
//Если вершина является листом, она ищет ближайшее к ней ребро, которое еще не было добавлено в код Прюфера.
//Если такое ребро найдено, оно добавляется в список PruferTree вместе с весом этого ребра.
//После добавления ребра, оно удаляется из МОД.
//Этот процесс продолжается до тех пор, пока не будут добавлены все листовые вершины.
//Функция PruferDecode работает следующим образом:
//Она инициализирует вектор isUsed, который отслеживает, была ли вершина уже использована для кодирования.
//Затем она создает вектор decodeTree, который будет использоваться для восстановления МОД.
//Она проходит по списку PruferTree и для каждой пары вершин и веса, она проверяет, не была ли вершина уже использована.
//Если вершина еще не использована, она добавляет ребро в decodeTree.
//После добавления всех рёбер, она проверяет, соответствует ли decodeTree сгенерированному коду Прюфера.
//В конце она выводит результат декодирования и МОД для проверки.

void MyGraph::Kraskal(vector<vector<int>> weightMx, int &iterationCounter){
    minTree.clear();
    minTree = vector<vector<int>>(vertexCnt, vector<int>(vertexCnt, 0));
    list<std::pair<int, int>> sortedEdges; // для сортировки
    
    //Заполнить список вершин в порядке возрастания
    for (int i = 0; i < vertexCnt; i++) {
        for (int j = 0; j < vertexCnt; j++) {
            if (weightMx[i][j] != 0) {
                bool isEmplaced = false;
                for (auto iter = sortedEdges.begin(); iter != sortedEdges.end(); iter++) {
                    iterationCounter++;
                    if (weightMx[(*iter).first][(*iter).second] >= weightMx[i][j]) {
                        sortedEdges.emplace(iter, std::make_pair(i, j));
                        isEmplaced = true;
                        break;
                    }
                }
                if (!isEmplaced)
                    sortedEdges.push_back(std::make_pair(i, j));
            }
        }
    }
    
    // Заполняем кратчайший остов
    while (!sortedEdges.empty()) {
        auto curEdge = *(sortedEdges.begin());
        sortedEdges.pop_front();
        if (!isAchievable(curEdge.first, curEdge.second, minTree, -1)) {
            minTree[curEdge.first][curEdge.second] = weightMx[curEdge.first][curEdge.second];
            minTree[curEdge.second][curEdge.first] = weightMx[curEdge.first][curEdge.second];
        }
        if (isAchievable(curEdge.first, curEdge.first, minTree, -1)) {
            minTree[curEdge.first][curEdge.second] = 0;
            minTree[curEdge.second][curEdge.first] = 0;
        }
    }
    int resultSum = 0;
    for (int i = 0; i < vertexCnt; i++)
        for (int j = i + 1; j < vertexCnt; j++)
            if (minTree[i][j] != 0)
                resultSum += minTree[i][j];
    
    cout << "Кратчайший остов : " << endl;
    PrintMatrix(minTree);
    cout << "\nВес кратчайшего остова : " << resultSum << '\n';
}

void MyGraph::Prim(vector<vector<int>> weightMx, int& iterationCounter) {
    random_device rd;
    mt19937 mersenne(rd());
    minTree.clear();
    minTree = vector<vector<int>>(vertexCnt, vector<int>(vertexCnt, 0));
    
    int startingVertex = mersenne() % vertexCnt;
    std::vector<bool> isInSpanningTree(vertexCnt, false);
    isInSpanningTree[startingVertex] = true;
    bool isOver = false;
    
    auto weightMxCopy = weightMx;
    while (!isOver) {
        //Найти минимальное ребро, связанное с текущим текущим подграфом
        pair<int, int> minimumEdge;
        bool isFound = false;
        for (int i = 0; i < vertexCnt; i++) {
            // Проверка ребер в минимальном остовном дереве
            if (isInSpanningTree[i] == true) {
                for (int j = 0; j < vertexCnt; j++) {
                    iterationCounter++;
                    // Check for not internal edges
                    if (isInSpanningTree[j] == false) {
                        // Проверьте оба возможных соединения
                        if (weightMxCopy[i][j] != 0) {
                            if (!isFound) {
                                isFound = true;
                                minimumEdge = std::make_pair(i, j);
                            }
                            else {
                                if (weightMxCopy[i][j] < weightMxCopy[minimumEdge.first][minimumEdge.second]) {
                                    minimumEdge = std::make_pair(i, j);
                                }
                            }
                        }
                        if (weightMxCopy[j][i] != 0) {
                            if (!isFound) {
                                isFound = true;
                                minimumEdge = std::make_pair(j, i);
                            }
                            else {
                                if (weightMxCopy[j][i] < weightMxCopy[minimumEdge.first][minimumEdge.second]) {
                                    minimumEdge = std::make_pair(j, i);
                                }
                            }
                        }
                    }
                }
            }
        }
        
        //Поместить ребро в минимальное остовное дерево или нет
        if (!isAchievable(minimumEdge.first, minimumEdge.second, minTree, -1)) {
            minTree[minimumEdge.first][minimumEdge.second] = weightMx[minimumEdge.first][minimumEdge.second];
            minTree[minimumEdge.second][minimumEdge.first] = weightMx[minimumEdge.first][minimumEdge.second];
        }
        if (isAchievable(minimumEdge.first, minimumEdge.first, minTree, -1)) {
            minTree[minimumEdge.first][minimumEdge.second] = 0;
            minTree[minimumEdge.second][minimumEdge.first] = 0;
            weightMxCopy[minimumEdge.first][minimumEdge.second] = 0;
        }
        else {
            isInSpanningTree[minimumEdge.first] = true;
            isInSpanningTree[minimumEdge.second] = true;
        }
        
        isOver = true;
        for (int i = 0; i < vertexCnt; i++)
            if (isInSpanningTree[i] == false) {
                isOver = false;
                break;
            }
    }
    
    int resultSum = 0;
    for (int i = 0; i < vertexCnt; i++)
        for (int j = i + 1; j < vertexCnt; j++)
            if (minTree[i][j] != 0)
                resultSum += minTree[i][j];
    
    cout << "Кратчайший остов : " << endl;
    cout << "РАБОТА С НЕОРИЕНТИРОВАННЫЙ ГРАФ!" << endl;
    PrintMatrix(minTree);
    cout << "Вес кратчайшего остова : " << resultSum << '\n';
}

void MyGraph::PruferCode(){
    PruferTree.clear();
    if (minTree.size() == 0) {
        cout << "Остов не был сгенерирован !!!\n";
        return;
    }
    
    auto copySpanTree = minTree;
    
    for (int i = 0; i < vertexCnt; i++) {
        bool isLeaf = true;
        int count = 0;
        
        for (int j = 0; j < vertexCnt; j++) {
            if (copySpanTree[i][j] != 0) {
                count++;
                if (count > 1) {
                    isLeaf = false;
                    break;
                }
            }
        }
        
        if (isLeaf && count != 0) {
            bool isAppripriate = true;
            if (!PruferTree.empty())
                for (auto iter = PruferTree.begin(); iter != PruferTree.begin(); iter++)
                    if ((*iter).first == i) { isAppripriate = false; break; }
            
            int nearVertex = 0;
            for (int k = 0; k < vertexCnt; k++)
                if (copySpanTree[i][k] != 0)
                    nearVertex = k;
            
            if (isAppripriate) {
                PruferTree.push_back(make_pair(nearVertex, copySpanTree[i][nearVertex]));
                copySpanTree[i][nearVertex] = 0;
                copySpanTree[nearVertex][i] = 0;
                i = 0;
            }
        }
    }
    
    cout << "Код Прюфера с весом: ";
    for (int i = 0; i < PruferTree.size(); i++)
        cout << PruferTree[i].first + 1 << "(" << PruferTree[i].second << ")" << '\t';
    
    cout << "\n\n";
}

void MyGraph::PruferDecode(){
    vector<bool> isUsed(vertexCnt, false);
    vector<vector<int>> decodeTree(vertexCnt, vector<int>(vertexCnt, 0));
    
    for (auto iter = PruferTree.begin(); iter != PruferTree.end(); iter++) {
        vector<bool> isNotInCode(vertexCnt, true);
        for (auto jter = iter; jter != PruferTree.end(); jter++) {
            isNotInCode[(*jter).first] = false;
        }
        for (int j = 0; j < vertexCnt; j++) {
            if (isNotInCode[j] && !(isUsed[j])) {
                decodeTree[j][(*iter).first] = (*iter).second;
                decodeTree[(*iter).first][j] = (*iter).second;
                isUsed[j] = true;
                break;
            }
        }
    }
    
    cout << "Результат декодирования :" << endl;
    PrintMatrix(decodeTree);
    cout << "Сам Мин.Ост.Граф :" << endl;
    PrintMatrix(minTree);
    
    if(minTree == decodeTree){
        cout << "Успешно!" << endl;
    }
    else cout << "Ошибка(" << endl;
    
    cout << endl;
}

void MyGraph::BuildFakeSource() {
    vector<int> sources;
    bool flag = true;
    // Поиск подозрительных вершин для проверки
    for (int i = 1; i < vertexCnt-1; i++) {
        if (MatrixSmejn[0][i] == 0) {
            for (int j = 1; j < vertexCnt; j++) {
                if (MatrixSmejn[j][i] != 0) {
                    flag = false;
                    break;
                }
            }
            if (flag) { //Столбик нулевой, для веришны нужно строить фиктивный исток
                sources.push_back(i);
            }
        }
    }
    if (sources.size() == 0) {
        MatrixFakeSource = MatrixSmejn;
    }
    else {
        for (int i = 0; i < vertexCnt + 1; i++) {
            if (i == 0 || (i != 0 && find(sources.begin(), sources.end(), (i)) != sources.end())) {
                MatrixFakeSource[0][i + 1] = 1;
            }
        }
        for (int j = 1; j < vertexCnt + 1; j++) {
            for (int k = 1; k < vertexCnt + 1; k++) {
                MatrixFakeSource[j][k] = MatrixSmejn[j - 1][k - 1];
            }
        }
    }
}

void MyGraph::AssignTorrent() {
    std::random_device rd;
    std::mt19937 mersenne(rd());
    for (int i = 0; i < vertexCnt; i++) {
        for (int j = i + 1; j < vertexCnt; j++) {
            if (MatrixSmejn[i][j] != 0)
                torrent[i][j] = mersenne() % (MAX_POS_WEIGHT - 1) + 1;
        }
    }
};

bool MyGraph::bfs_FordFulkerson(vector<vector<int>> matrix, int source, int sink, vector<int>& path) const {
    vector<bool> isVisitedArr(matrix.size(), false);
    queue<int> q;
    int curVert;
    
    q.push(source);
    isVisitedArr[source] = true;
    path[source] = -1;
    
    while (!q.empty()) {
        curVert = q.front();
        q.pop();
        
        for (int i = 0; i < matrix.size(); i++) {
            if ((isVisitedArr[i] == false) && (matrix[curVert][i] > 0)) {
                path[i] = curVert;
                if (i == sink) {
                    return true;
                }
                q.push(i);
                isVisitedArr[i] = true;
            }
        }
    }
    return false;
}

int MyGraph::fordFulkerson(int source, int sink) const {
    int tmpSink = sink;
    vector<vector<int>> tmptorrent = torrent;
    //tmpSink++;
    vector<int> path(tmptorrent.size(), 0);
    int maxFlow = 0;
    int curFlow;
    while (bfs_FordFulkerson(tmptorrent, source, tmpSink, path)) {
        curFlow = INF;
        for (int i = tmpSink; i != source; i = path[i]) {
            curFlow = std::min(curFlow, tmptorrent[path[i]][i]);
        }
        for (int i = tmpSink; i != source; i = path[i]) {
            tmptorrent[path[i]][i] -= curFlow;
            tmptorrent[i][path[i]] += curFlow;
        }
        maxFlow += curFlow;
    }
    return maxFlow;
}

int MyGraph::calcMinCostFlow(int s, int t) const {
    int flow = fordFulkerson(s, t);
    int minCostFlow = 0;
    
    cout << "Максимальный поток : " << flow << ". ";
    flow = flow * 2 / 3;
    cout << "Используемый поток : " << flow << '\n';
    
    vector<vector<int>> tmpmodMixedWeightsMx = modPosWeightsMx;
    vector<vector<int>> tmpTorrent = torrent; // Матрица пропуск. способностей
    
    cout << "Матрица стоимости :" << endl;
    PrintMatrix(modPosWeightsMx);
    cout << "Матрица пропускных способностей :" << endl;
    PrintMatrix(torrent);
    
    while (flow != 0) {
        vector<int> route = RestorePaths(s, BellmanFord(s, tmpmodMixedWeightsMx), tmpmodMixedWeightsMx)[t];
        sort(route.begin(), route.end());
        
        int bottleNeck = INF;
        int cost = 0;
        vector<pair<int, int>> edgesToDelete;
        
        for (vector<int>::iterator it = route.begin(); it != route.end() - 1; it++) {
            if (tmpTorrent[*it][*(it + 1)] < bottleNeck)
                bottleNeck = tmpTorrent[*it][*(it + 1)];
            cout <<* it + 1 << "->";
        }
        bottleNeck = min(bottleNeck, flow);
        cout << t + 1 <<" Пускаем поток " << bottleNeck;
        
        for (vector<int>::iterator it = route.begin(); it != route.end() - 1; it++) {
            tmpTorrent[*it][*(it + 1)] -= bottleNeck;
            cost += tmpmodMixedWeightsMx[*it][*(it + 1)];
            if (!tmpTorrent[*it][*(it + 1)])
                edgesToDelete.push_back(pair<int, int>(*it, *(it + 1)));
        }
        
        cout << " со стоимостью за единицу потока " << cost;
        
        minCostFlow += cost * bottleNeck;
        
        cout << ". Итоговая стоимость " << cost * bottleNeck << "\n";
        
        for (vector<pair<int,int>>::iterator it = edgesToDelete.begin(); it != edgesToDelete.end(); it++)
            tmpmodMixedWeightsMx[it->first][it->second] = INF;
        
        edgesToDelete.clear();
        
        flow -= bottleNeck;
        
//        if (minCostFlow == 0){
//            minCostFlow++;
//        }
        
    }
    cout << endl;
    return minCostFlow;
}


void MyGraph::AssignTorrent1() {
    std::random_device rd;
    std::mt19937 mersenne(rd());
    for (int i = 0; i < vertexCnt; i++) {
        for (int j = i + 1; j < vertexCnt; j++) {
            if (MatrixSmejn[i][j] != 0) {
                // Присваиваем значение в верхнем треугольнике
                torrent[i][j] = mersenne() % (MAX_POS_WEIGHT - 1) + 1;
                // Копируем значение в нижний треугольник
                torrent[j][i] = torrent[i][j];
            }
        }
    }
}

int MyGraph::minCut(vector<vector<int>>& torrent) {
    int n = torrent.size();
    
    vector<int> label(n + 1, 0); // для вершин
    for (int i = 1; i <= n; ++i) {
        label[i] = i;
    }

    int min_cut = INF;

    
    srand(time(0));

    
    for (int phase = 0; phase < n; ++phase) {
        vector<int> value(n + 1, 0);

        //случайным образом две вершины, u и v
        int u = -1, v = -1;
        while (u == v) {
            u = rand() % n + 1;
            v = rand() % n + 1;
        }
        // обновлик вершин
        for (int w = 0; w <= n; ++w) {
            //value[w] = torrent[label[u]][label[w]];
        }
        // Объединение вершины u и v ???????????????
        //torrent[label[u]][label[v]] += torrent[label[v]][label[u]];
        //torrent[label[v]][label[u]] = torrent[label[u]][label[v]];
        //  по всем вершинам кроме v
        for (int w = 0; w <= n; ++w) {
            if (w != v) {
                torrent[label[u]][label[w]] += torrent[label[v]][label[w]];
                //torrent[label[w]][label[u]] += torrent[label[u]][label[w]];
            }
        }
        // замен v вершиной u
        for (int i = 0; i <= n; ++i) {
            if (label[i] == label[v]) {
                label[i] = label[u];
            }
        }
        min_cut = min(min_cut, torrent[label[u]][label[u]]);
    }
    return min_cut;
}

void MyGraph::mergeVertices(vector<vector<int>>& matrix, vector<bool>& cutSet, int u, int v) {
    int V = matrix.size();
    
    
    for (int i = 1; i <= V; ++i) {
        if (i != v && !cutSet[i] && matrix[v][i] > 0) {
            matrix[u][i] += matrix[v][i];
            matrix[i][u] += matrix[v][i];
            matrix[i][v] = matrix[v][i] = 0;
        }
    }
}

bool MyGraph::bfs_FordFulkerson1(vector<vector<int>> matrix, int source, int sink, vector<int>& path) const {
        vector<bool> isVisitedArr(matrix.size(), false);
        queue<int> q;
        int curVert;

        q.push(source);
        isVisitedArr[source] = true;
        path[source] = -1;

        while (!q.empty()) {
            curVert = q.front();
            q.pop();

            for (int i = 0; i < matrix.size(); i++) {
                if ((isVisitedArr[i] == false) && (matrix[curVert][i] > 0)) {
                    path[i] = curVert;
                    if (i == sink) {
                        return true;
                    }
                    q.push(i);
                    isVisitedArr[i] = true;
                }
            }
        }
        return false;
    }


void MyGraph::addEdge(int u, int v, int capacity) {
        torrent[u][v] = capacity;
        torrent[v][u] = capacity; // для неориентированного графа
    }



void MyGraph::fordFulkerson1(int source, int sink) const {
        int tmpSink = sink;
    random_device rd;
    mt19937 mersenne(rd()); // случайные числа
    vector<int> vertDegrees = VertPrepare();
    for (int i = 0; i < vertexCnt; i++) {
        for (int j = i + 1; j < vertexCnt; j++) {
            if (MatrixSmejn[i][j] != 0)
               //posWeightsMatrix[i][j] = LaplaceD();
                posWeightsMatrix[i][j] == mersenne() % (MAX_POS_WEIGHT - 1) + 1;
        }
    }
        vector<vector<int>> tmptorrent = torrent;
        vector<int> path(tmptorrent.size(), 0);
        int maxFlow = 0;
        int curFlow;

        while (bfs_FordFulkerson(tmptorrent, source, tmpSink, path)) {
            curFlow = INF;
            for (int i = tmpSink; i != source; i = path[i]) {
                curFlow = min(curFlow, tmptorrent[path[i]][i]);
            }
            for (int i = tmpSink; i != source; i = path[i]) {
                tmptorrent[path[i]][i] -= curFlow;
                tmptorrent[i][path[i]] += curFlow;
            }
            maxFlow += curFlow;
        }
    
    //maxFlow = calcMinCostFlow(source, sink); // нужно ли ?

        // Выводим вершины рёбер минимального разреза
        cout << "Вершины рёбер минимального разреза:" << endl;
        vector<bool> visited(vertexCnt, false);
        queue<int> q;
        q.push(source);
        visited[source] = true;
        while (!q.empty()) {
            int u = q.front();
            q.pop();
            for (int v = 1; v < vertexCnt; ++v) {
                if (!visited[v] && tmptorrent[u][v] > 0) {
                    cout << u << "-" << v << endl;
                    visited[v] = true;
                    q.push(v);
                }
            }
        }
}

int MyGraph::DijkstraAS(int startVertex, int endVertex) const {
    vector<int> distances(vertexCnt, INF); // инициализация массива расстояний
    distances[startVertex] = 0; // начальное расстояние равно 0

    vector<bool> visited(vertexCnt, false); // инициализация массива посещенных вершин

    for (int i = 1; i < vertexCnt; ++i) {
        int minDistance = INF;
        int minIndex = -1;

        // Находим вершину с наименьшим расстоянием от начальной вершины
        for (int j = 1; j < vertexCnt; ++j) {
            if (!visited[j] && distances[j] < minDistance) {
                minDistance = distances[j];
                minIndex = j;
            }
        }

        // Если не нашли такую вершину, выходим из цикла
        if (minIndex == -1) break;

        visited[minIndex] = true; // отмечаем вершину как посещенную

        // Обновляем расстояния до соседних вершин от текущей вершины
        for (int j = 1; j < vertexCnt; ++j) {
            if (!visited[j] && MatrixSmejn[minIndex][j] != 0 && distances[minIndex] + MatrixSmejn[minIndex][j] < distances[j]) {
                distances[j] = distances[minIndex] + MatrixSmejn[minIndex][j];
            }
        }
    }

    // Возвращаем расстояние от начальной вершины до конечной вершины
    return distances[endVertex];
}

int MyGraph::heuristic(int start, int end, const vector<vector<int>>& graph) {
    int heuristic_cost = 0;
       int u = start;
       while (u != end) {
           int v = -1;
           int min_weight = numeric_limits<int>::max();
           for (int i = 0; i < graph.size(); ++i) {
               if (graph[u][i] != 0 && graph[u][i] < min_weight && i != u && i != previous[u].first) { // Заменяем prev на previous
                   v = i;
                   min_weight = graph[u][i];
               }
           }
           if (v == -1) return heuristic_cost; // Нет пути до конечной вершины
           heuristic_cost += min_weight;
           u = v;
       }
       return heuristic_cost;
}

// Функция для поиска пути с помощью алгоритма A*
vector<int> MyGraph::aStarSearch(const vector<vector<int>>& graph, int start, int end) {
    BFS(start,end);
    int n = graph.size();
           if (n == 0 || n != graph[0].size()) return {};
    
           vector<pair<int, int>> dist(n, {numeric_limits<int>::max(), numeric_limits<int>::max()});
           previous.assign(n, {-1, -1}); // Заполняем массив предыдущих вершин

           dist[start] = {0, heuristic(start, end, graph)};
           priority_queue<pair<int, int>, vector<pair<int, int>>, greater<pair<int, int>>> pq;
           pq.push({0, start});

           while (!pq.empty()) {
               auto [cost, u] = pq.top();
               pq.pop();

               if (u == end) {
                   vector<int> path;
                   while (u != -1) {
                       path.push_back(u);
                       u = previous[u].first;
                   }
                   reverse(path.begin(), path.end());
                   return path;
               }

               for (int v = 1; v < n; ++v) {
                   if (graph[u][v] != 0) {
                       int new_cost = dist[u].first + graph[u][v];
                       if (new_cost < dist[v].first) {
                           dist[v].first = new_cost;
                           dist[v].second = new_cost + heuristic(v, end, graph); // Здесь не нужно передавать previous
                           previous[v] = {u, graph[u][v]};
                           pq.push({dist[v].second, v});
                       }
                   }
               }
           }
           return {}; // Путь не найден
 }

void MyGraph::Hamilton(vector<vector<int>> weightMx, int weightMode) const {
    ofstream fout("ListOfHamiltonianCycles.txt");
    if (vertexCnt == 2) {
        cout << "Нельзя составить цикл из 2-ух вершин(\n\n";
        fout.close();
        return;
    }
        else if (vertexCnt > 15){
            cout << "Слишком большое кол-во вершин, используйте эвристику!"<< endl;
            fout.close();
            return;
        }
      
    
    
    auto modWeightMatrix = weightMx;
    for (int i = 0; i < vertexCnt; i++) {
        for (int j = 0; j < i; j++) {
            modWeightMatrix[i][j] = weightMx[j][i];
        }
    }
    
    // (Дирак при V>3 & степени не больше чем V/2) (если степени > Vert/2 => Гамельтонов
    // Оре?? V>2 & deg(x) + deg(y) >= V :: x,y пренадлежит несмежные вершины пары
    //по сути Бонди-Хватала но нет Дирак+Оре при этом sort(deg) с импликацией dk⩽k<n/2⇒dn−k⩾n−k,(∗)
    if (vertexCnt == 3) {
        bool isHamiltonian = true;
        random_device rd;
        mt19937 mersenne(rd());
        for (int i = 0; i < vertexCnt; i++) {
            for (int j = 0; j < vertexCnt; j++) {
                if (i != j && modWeightMatrix[i][j] == 0) {
                    isHamiltonian = false;
                    modWeightMatrix[i][j] = (mersenne() % 40) + 1;
                    if (weightMode == 1)
                        modWeightMatrix[i][j] *= std::pow(-1, mersenne() % 2);
                    modWeightMatrix[j][i] = modWeightMatrix[i][j];
                }
            }
        }
        if (!isHamiltonian) {
            PrintMatrix(GetWeightsMatrix(0));
            cout << "Граф не гамильтонов. Гамильтонова модифицированная матрица: \n";
            PrintMatrix(modWeightMatrix);
            cout << '\n';
        }
        else {
            cout << "Граф гамильтонов.\n";
            PrintMatrix(modWeightMatrix);
        }
    }
    else {
        bool isHamiltonian = true;
        vector<int> degrees(vertexCnt, 0);
        random_device rd;
        mt19937 mersenne(rd());
        
        // степени ++ для вершин если не 0
        for (int i = 0; i < vertexCnt; i++) {
            for (int j = 0; j < vertexCnt; j++) {
                if (modWeightMatrix[i][j] != 0) degrees[i]++;
            }
            if (degrees[i] < (vertexCnt / 2)) {
                isHamiltonian = false;
            }
        }
        
        // Если не гамельтонов то добавим ребро
        
        if (isHamiltonian) cout << "Граф гамильтонов\n";
        else {
            cout << "Граф не гамильтонов\n";
            
            while (!isHamiltonian) {
                bool isChanged = false;
                
                //с учетом теоремы Дирака так как должно быть N>V/2
                for (int i = 0; i < vertexCnt; i++) {
                    if (degrees[i] < (vertexCnt / 2)) {
                        // Добавляем ребра для deg > V/2
                        int appropriateVertex = -1;
                        
                        for (int j = 0; j < vertexCnt; j++) {
                            if (modWeightMatrix[i][j] == 0 && i != j) {
                                if (appropriateVertex == -1) appropriateVertex = j;
                                
                                if (degrees[j] < (vertexCnt / 2)) {
                                    isChanged = true;
                                    degrees[i]++;
                                    degrees[j]++;
                                    modWeightMatrix[i][j] = (mersenne() % 100) + 1;
                                    if (weightMode == 1)
                                        modWeightMatrix[i][j] *= std::pow(-1, mersenne() % 2);
                                    modWeightMatrix[j][i] = modWeightMatrix[i][j];
                                    break;
                                }
                            }
                        }
                        
                        if (!isChanged && appropriateVertex != -1) {
                            isChanged = true;
                            degrees[i]++;
                            degrees[appropriateVertex]++;
                            modWeightMatrix[i][appropriateVertex] = (mersenne() % 100) + 1;
                            if (weightMode == 1)
                                modWeightMatrix[i][appropriateVertex] *= std::pow(-1, mersenne() % 2);
                            modWeightMatrix[appropriateVertex][i] = modWeightMatrix[i][appropriateVertex];
                        }
                    }
                }
                
                if (!isChanged) isHamiltonian = true;
            }
            
            cout << "Гамильтонова модифицированная матрица: \n";
            PrintMatrix(modWeightMatrix);
            std::cout << '\n';
        }
    }
    
    // Находим все гамильтоновы циклы
    vector<int> path;
    path.push_back(0);
    vector<int> minimumPath;
    int length = 0;
    int minimumLength = INT_MAX;
    
    findHamiltonCycleACD(fout, modWeightMatrix, path, length, minimumPath, minimumLength);
    
    cout << "\nМинимальный цикл: ";
    int i = 0;
    for (i; i < minimumPath.size()-1; i++)
        cout << minimumPath[i]+1 << "->";
    cout << minimumPath[i]+1 << endl;
    cout << "Вес: " << minimumLength << "\n\n";
    PrintMatrix(modWeightMatrix);
    fout.close();
}

//void MyGraph::findHamiltonCycleACD(ofstream& fout, vector<vector<int>>& graph, vector<int>& path, int length, vector<int>& minimumPath, int& minimumLength) const
//{
//    if (path.size() == vertexCnt)
//    {
//        if (graph[path[path.size() - 1]][0] != 0)
//        {
//            length += graph[path[path.size() - 1]][0];
//            path.push_back(0);
//            
//            if (minimumLength > length)
//            {
//                minimumPath = path;
//                minimumLength = length;
//            }
//            
//            fout << "Цикл: ";
//            int i = 0;
//            for (i ; i < path.size()-1; i++) {
//                fout << path[i]+1 << "->";
//            }
//            fout << path[i]+1 << endl;
//            fout << "Вес:" << length << '\n';
//            
//            path.pop_back();
//        }
//        return;
//    }
//    else {
//        for (int i = 0; i < vertexCnt; i++) {
//            if (graph[path[path.size() - 1]][i] != 0){
//                bool isInPath = false;
//                
//                for (int j = 0; j < path.size(); j++) {
//                    if (path[j] == i) {
//                        isInPath = true;
//                        break;
//                    }
//                }
//                
//                if (!isInPath)
//                {
//                    int newLength = length + graph[path[path.size() - 1]][i];
//                    path.push_back(i);
//                    findHamiltonCycleACD(fout, graph, path, newLength, minimumPath, minimumLength);
//                    path.pop_back();
//                }
//            }
//        }
//        
//        return;
//    }
//}

void MyGraph::AntColonyOptimization(vector<vector<int>> weightMx, int weightMode) const {
    ofstream fout("ListOfHamiltonianCycles.txt");
    if (vertexCnt == 2) {
        cout << "Нельзя составить цикл из 2-ух вершин(\n\n";
        fout.close();
        return;
    }
    

    auto modWeightMatrix = weightMx;
    for (int i = 0; i < vertexCnt; i++) {
        for (int j = 0; j < i; j++) {
            modWeightMatrix[i][j] = weightMx[j][i];
        }
    }

    if (vertexCnt == 3) {
        bool isHamiltonian = true;
        random_device rd;
        mt19937 mersenne(rd());
        for (int i = 0; i < vertexCnt; i++) {
            for (int j = 0; j < vertexCnt; j++) {
                if (i != j && modWeightMatrix[i][j] == 0) {
                    isHamiltonian = false;
                    modWeightMatrix[i][j] = (mersenne() % 40) + 1;
                    modWeightMatrix[j][i] = modWeightMatrix[i][j];
                }
            }
        }
        if (!isHamiltonian) {
            cout << "Граф не гамильтонов. Гамильтонова модифицированная матрица: \n";
            PrintMatrix(modWeightMatrix);
        }
        else {
            cout << "Граф гамильтонов.\n";
            PrintMatrix(modWeightMatrix);
        }
    }
    else {
        bool isHamiltonian = true;
        vector<int> degrees(vertexCnt, 0);
        random_device rd;
        mt19937 mersenne(rd());

        for (int i = 0; i < vertexCnt; i++) {
            for (int j = 0; j < vertexCnt; j++) {
                if (modWeightMatrix[i][j] != 0) degrees[i]++;
            }
            if (degrees[i] < (vertexCnt / 2)) {
                isHamiltonian = false;
            }
        }

        if (isHamiltonian) {
            cout << "Граф гамильтонов.\n";
            PrintMatrix(modWeightMatrix);
        } else {
            cout << "Граф не гамильтонов\n";
            return;
        }

        vector<vector<double>> pheromoneMatrix(vertexCnt, vector<double>(vertexCnt, 1.0));

        int n = 10; // количество муравьев
        int t = 100; // количество итераций
        double rho = 0.5; // вероятность испарения феромона
        double alpha = 10.0; // коэффициент притяжения феромона
        double beta = 5.0; // коэффициент притяжения геометрии пространства

        vector<int> minimumPath;
        int minimumLength = INT_MAX;

        for (int iter = 0; iter < t; ++iter) {
            vector<int> path;
            path.push_back(0);
            int length = 0;

            findHamiltonCycleACD(fout, modWeightMatrix, path, length, minimumPath, minimumLength);

            // Обновление феромонов
            for (int i = 0; i < vertexCnt; ++i) {
                for (int j = 0; j < vertexCnt; ++j) {
                    if (i != j && modWeightMatrix[i][j] != 0) {
                        pheromoneMatrix[i][j] *= (1.0 - rho);
                    }
                }
            }
        }

        // После завершения поиска, вывод минимального цикла и его длины
        cout << "\nМинимальный цикл: ";
        for (int i = 0; i < minimumPath.size() - 1; i++) {
            cout << minimumPath[i] + 1 << "->";
        }
        cout << minimumPath.back() + 1 << endl;
        cout << "Вес: " << minimumLength << "\n\n";

        fout.close();
    }
}
void MyGraph::findHamiltonCycleACD(ofstream& fout, vector<vector<int>>& graph, vector<int>& path, int length, vector<int>& minimumPath, int& minimumLength) const {
    float ALPHA = 10.0;
    float BETA = 5.0;
    // Инициализация феромонов
    static vector<vector<double>> pheromoneMatrix(vertexCnt, vector<double>(vertexCnt, 1.0));
    if (path.size() == vertexCnt) {
        // Проверка завершения цикла
        if (graph[path.back()][path.front()] != 0) {
            length += graph[path.back()][path.front()];  // Добавляем вес последнего ребра
            path.push_back(path.front());  // Добавляем начальную вершину для замыкания цикла

            // Проверка на минимальный цикл
            if (length < minimumLength) {
                minimumLength = length;
                minimumPath = path;
            }
            
            // Вывод найденного цикла и его длины в файл
            fout << "Найден цикл: ";
            for (int i = 0; i < path.size() - 1; i++) {
                fout << path[i] + 1 << " -> ";
            }
            fout << path.back() + 1 << endl;  // Вывод последней вершины
            fout << "Длина цикла: " << length << endl;

            // Обратно отменяем изменения для продолжения поиска
            path.pop_back();
            length -= graph[path.back()][path.front()];
        }
        return;
    }

    // Расчет вероятностей и выбор следующей вершины
    int currentVertex = path.back();
    vector<double> probabilities(vertexCnt, 0.0);
    double sumProbabilities = 0.0;

    for (int i = 0; i < vertexCnt; i++) {
        if (graph[currentVertex][i] != 0 && std::find(path.begin(), path.end(), i) == path.end()) {
            probabilities[i] = pow(pheromoneMatrix[currentVertex][i], ALPHA) * pow(1.0 / graph[currentVertex][i], BETA);
            sumProbabilities += probabilities[i];
        }
    }

    // Нормализация вероятностей
    for (int i = 0; i < vertexCnt; i++) {
        if (probabilities[i] > 0) {
            probabilities[i] /= sumProbabilities;
        }
    }

    // Выбор следующей вершины
    random_device rd;
    mt19937 mersenne(rd());
    uniform_real_distribution<double> dist(0.0, 1.0);
    double randValue = dist(mersenne);
    double cumulativeProbability = 0.0;

    for (int i = 0; i < vertexCnt; i++) {
        cumulativeProbability += probabilities[i];
        if (randValue <= cumulativeProbability) {
            path.push_back(i);
            findHamiltonCycleACD(fout, graph, path, length + graph[currentVertex][i], minimumPath, minimumLength);
            path.pop_back();
            break;
        }
    }
}
//void MyGraph::Otjig(ofstream &fout, vector<vector<int>> &graph, vector<int> &path, int lenght, vector<int> &minimumPath, int &minimumLenght) const
//{
//    
//}
void MyGraph::Euler(vector<vector<int>> weightMx, int weightMode) const {  // 0 если положительную, 1 если смешанную
 if (vertexCnt == 2) {
  std::cout << "\nГраф из двух вершин не может иметь эйлеров цикл !!!\n\n";
  return;
 }
 
 auto modWeightMatrix = weightMx;
 for (int i = 0; i < modWeightMatrix[0].size(); i++) {
  for (int j = 0; j < i; j++) {
   modWeightMatrix[i][j] = weightMx[j][i];
  }
 }
 //Проверка : является ли граф эйлеровым ?
 bool isEulerian = true;
 bool modByEdges = true;
 vector<int> degrees(modWeightMatrix[0].size(), 0);
 random_device rd;
 mt19937 mersenne(rd());
 
    for (int i = 0; i < modWeightMatrix[0].size(); i++) {
        for (int j = 0; j < modWeightMatrix[0].size(); j++) {
            if (modWeightMatrix[i][j] != 0)
                degrees[i]++;
        }
        if (degrees[i] % 2 == 1) {
            if (degrees[i] == modWeightMatrix[0].size() - 1 && modByEdges) {
                cout << "Граф не является эйлеровым! Его нельзя сделать эйлеровым при помощи добавления ребер !!\n";
                modByEdges = false;
            }
            isEulerian = false;
        }
    }
 auto tmpDeg = degrees;
    if (modByEdges) {
        // Если граф не эйлеров, то добавить ребра чтобы сделать эйлеровым
        if (isEulerian) cout << "Граф является эйлеровым\n";
        else {
            //cout << "\nGraph is not Eulerian\n";
            while (!isEulerian) {
                bool isChanged = false;
                for (int i = 0; i < modWeightMatrix[0].size(); i++) {
                    if (degrees[i] % 2 == 1) {
                        // Добавляем ребра чтобы сделать степень четной
                        int appropriateVertex = -1;
                        for (int j = 0; j < modWeightMatrix[0].size(); j++) {
                            if (modWeightMatrix[i][j] == 0 && i != j) {
                                if (appropriateVertex == -1) {
                                    if (modWeightMatrix[0].size() % 2 == 0) {
                                        if (degrees[j] != modWeightMatrix[0].size() - 1)
                                            appropriateVertex = j;
                                    }
                                    else
                                        appropriateVertex = j;
                                }
                                if (degrees[j] % 2 == 1) {
                                    isChanged = true;
                                    degrees[i]++;
                                    degrees[j]++;
                                    modWeightMatrix[i][j] = (mersenne() % 40) + 1;
                                    if (weightMode == 1)
                                        modWeightMatrix[i][j] *= std::pow(-1, mersenne() % 2);
                                    modWeightMatrix[j][i] = modWeightMatrix[i][j];
                                    break;
                                }
                            }
                        }
                        
                        if (!isChanged && appropriateVertex != -1) {
                            isChanged = true;
                            degrees[i]++;
                            degrees[appropriateVertex]++;
                            modWeightMatrix[i][appropriateVertex] = (mersenne() % 100) + 1;
                            if (weightMode == 1)
                                modWeightMatrix[i][appropriateVertex] *= std::pow(-1, mersenne() % 2);
                            modWeightMatrix[appropriateVertex][i] = modWeightMatrix[i][appropriateVertex];
                        }
                        
                        if (appropriateVertex == -1) {
                            cout << "Граф не является эйлеровым!\n";
                            modByEdges = false;
                            break;
                            //return;
                        }
                    }
                }
                
                if (!isChanged) isEulerian = true;
                if (!modByEdges) break;
            }
            if (modByEdges) {
                cout << "\nВесовая матрица эйлерова графа: \n";
                PrintMatrix(modWeightMatrix);
                cout << '\n';
            }
        }
    }
 // Если произошел амогус, то удалить ребро
 if (!modByEdges) {
  int k = 0;
     for (k; k < tmpDeg.size(); k++) {
         if (tmpDeg[k] % 2 == 1) {
             break;
         }
     }
  //cout << "ДО\n";
  //PrintMatrix(modWeightMatrix);
     for (int i = k; i < tmpDeg.size(); i++) {
         if (tmpDeg[i] % 2 == 1 && modWeightMatrix[k][i] != 0) {
             modWeightMatrix[k][i] = 0;
             modWeightMatrix[i][k] = 0;
             tmpDeg[k]--;
             tmpDeg[i]--;
             cout << "Удалено ребро\n";
             break;
         }
     }
  //cout << "ПОСЛЕ\n";
  //PrintMatrix(modWeightMatrix);
 
  //cout << "Степени вершин:" << endl;
  //for (int i = 0; i < tmpDeg.size(); i++) {
  // cout << i << ": " << tmpDeg[i] << "; " << endl;
  //}
  //cout << endl;
  //k = 0;
  //for (k; k < tmpDeg.size(); k++)
  // if (tmpDeg[k] % 2 == 1)
  //  break;
  //for (int i = k; i < tmpDeg.size(); i++)
  // if (tmpDeg[i] % 2 == 1 && modWeightMatrix[k][i] != 0) {
  //  modWeightMatrix[k][i] =

     //0;
     //  modWeightMatrix[i][k] = 0;
  //  tmpDeg[k]--;
  //  tmpDeg[i]--;
  //  cout << "Удалено ребро\n";
  //  break;
  // }
 }
 EulerCycles(modWeightMatrix, tmpDeg);
}

void MyGraph::EulerCycles(vector<vector<int>> weightMx, vector<int> degrees) const {  // 0 если положительную, 1 если смешанну
    // Поиск эйлеровых циклов
 vector<int> eulerianCycle;
 stack<int> vertexes;
 auto wMx = weightMx;
 auto degs = degrees;
 vertexes.push(0);
 cout << "Матрица эйлерова графа" << endl;
 PrintMatrix(wMx);
    while (!vertexes.empty()) {
        int currentVertex = vertexes.top();
        if (degs[currentVertex] == 0) {
            vertexes.pop();
            eulerianCycle.push_back(currentVertex + 1);
        }
        else {
            for (int i = 0; i < wMx[0].size(); i++) {
                if (wMx[currentVertex][i] != 0) {
                    vertexes.push(i);
                    degs[i]--;
                    degs[currentVertex]--;
                    wMx[currentVertex][i] = 0;
                    wMx[i][currentVertex] = 0;
                    break;
                }
            }
        }
    }
 
 // Вывод эйлерового цикла
 std::cout << "\nВершины эйлерового цикла  графе: ";
 int i = 0;
 for (i; i < eulerianCycle.size() - 1; i++) cout << eulerianCycle[i] << "->";
 cout << eulerianCycle[i] << endl << endl;
}

void MyGraph::BronKerboschIterative(set<int>& r, set<int>& p, set<int>& x, vector<vector<int>>& maximalIndependentSets, vector<vector<int>>& tempMatrix) {
    // Создаем стек для имитации рекурсивного вызова
    stack<tuple<set<int>, set<int>, set<int>>> stack;
    stack.push(make_tuple(r, p, x));

    while (!stack.empty()) {
        auto [current_r, current_p, current_x] = stack.top();
        stack.pop();

        // Если множества `p` и `x` пусты, добавляем текущее независимое множество `r` в список максимальных независимых множеств
        if (current_p.empty() && current_x.empty()) {
            maximalIndependentSets.push_back(vector<int>(current_r.begin(), current_r.end()));
            continue;
        }

        // Выбор вершины `pivot` из `p` или `x`
        int pivot;
        if (!current_p.empty()) {
            pivot = *current_p.begin();
        } else {
            pivot = *current_x.begin();
        }

        // Копируем `current_p` и перебираем все вершины из копии `current_p`
        set<int> current_p_copy = current_p;
        for (int v : current_p_copy) {
            // Проверяем смежность с `pivot`
            if (tempMatrix[pivot][v] != 0) {
                continue; // Пропускаем вершины, смежные с `pivot`
            }

            // Добавляем вершину `v` в независимое множество `r`
            current_r.insert(v);

            // Создаем новые множества `p_new` и `x_new`
            set<int> p_new, x_new;

            // Убираем вершины из `current_p`, которые смежные с `v`
            for (int u : current_p) {
                if (tempMatrix[v][u] == 0) {
                    p_new.insert(u);
                }
            }

            // Убираем вершины из `current_x`, которые смежные с `v`
            for (int u : current_x) {
                if (tempMatrix[v][u] == 0) {
                    x_new.insert(u);
                }
            }

            // Добавляем текущее состояние в стек для последующего выполнения
            stack.push(make_tuple(current_r, p_new, x_new));

            // Возвращаем начальные множества `current_r`, `current_p`, и `current_x`
            current_r.erase(v);
            current_p.erase(v);
            current_x.insert(v);
        }
    }
}

vector<vector<int>> MyGraph::FindMaximalIndependentSets() {
    vector<vector<int>> maximalIndependentSets;
    std::vector<std::vector<int>> Square = {
        {0, 1, 1, 0},
        {0, 0, 0, 1},
        {0, 0, 0, 1},
        {0, 0, 0, 0}
    };
    vector<vector<int>> tempMatrix = CreateUndirectedMatrix(GetWeightsMatrix(0));
    
    int vertexCount = vertexCnt;
    //  (от 0 до 2^v - 1)
    for (int mask = 1; mask < (1 << vertexCount); mask++) {
        vector<int> independentSet;
        bool isIndependent = true;
        for (int i = 0; i < vertexCount && isIndependent; i++) {
            if (mask & (1 << i)) {
                // вершина i включена в подмножество? проверить смежность с другими включенными
                for (int j = 0; j < vertexCount; j++) {
                    if (i != j && (mask & (1 << j)) && tempMatrix[i][j] != 0) {
                        isIndependent = false;
                        break;
                    }
                }
                // если независимое + вершину i в независимое множество
                if (isIndependent) {
                    independentSet.push_back(i + 1);
                }
            }
        }
        if (isIndependent) {
            bool isMaximal = true;
            // Проверяем, не является ли текущее независимое множество подмножеством другого уже найденного максимального независимого множества
            for (const auto& maxSet : maximalIndependentSets) {
                if (includes(maxSet.begin(), maxSet.end(), independentSet.begin(), independentSet.end())) {
                    isMaximal = false;
                    break;
                }
            }
            
            if (isMaximal) {
                maximalIndependentSets.push_back(independentSet);
            }
        }
    }
    
    return maximalIndependentSets;
}



void MyGraph::NewGraph() {
    // Очистка текущего графа
    vertexCnt = 0;
    MatrixSmejn.clear();
    posWeightsMatrix.clear();
    mixedWeightsMatrix.clear();
    MatrixReach.clear();
    minTree.clear();
    torrent.clear();

    
    cout << "Введите количество вершин графа: ";
    cin >> vertexCnt;
    cin.ignore();

    
    MatrixSmejn.resize(vertexCnt, vector<int>(vertexCnt, 0));
    posWeightsMatrix.resize(vertexCnt, vector<int>(vertexCnt, 0));
    mixedWeightsMatrix.resize(vertexCnt, vector<int>(vertexCnt, 0));
    MatrixReach.resize(vertexCnt, vector<int>(vertexCnt, 0));
    torrent.resize(vertexCnt, vector<int>(vertexCnt, 0));

    
    int tmp;
    random_device rd;
    mt19937 mersenne(rd());
    vector<int> vertDegrees = VertPrepare();

    
    bool flag = true;
    for (int i = 0; i < vertexCnt-1; i++){
        flag = true;
        for (int j = 0; j < vertexCnt; j++) {
            if (MatrixSmejn[i][j] != 0) { //проверка, есть ли путь между вершинами i и j.
                flag = false; //роверка, является ли вершина i тупиковой.
                break;
            }
        }
        if (flag == true) {
            MatrixSmejn[i][i+1] = 1; //установка значения в матрице смежности между вершинами i и i + 1 равным 1.
        }
    }
    vector<int> sources; //инициализация вектора sources
    //избавление от дополнительных истоков
    flag = true;
    for (int i = 1; i < vertexCnt - 1; i++) { //цикл, который проходит по всем вершинам, кроме первой и последней
        flag = true;
        if (MatrixSmejn[0][i] == 0) { //проверка, есть ли путь между вершинами 0 и i.
            for (int j = 1; j < vertexCnt; j++) {
                if (MatrixSmejn[j][i] != 0) { //проверка, есть ли путь между вершинами j и i.
                    flag = false;
                    break;
                }
            }
            if (flag) { // тупикова вкршина?
                sources.push_back(i); //добавление вершины i в вектор sources
            }
        }
    }
    if (sources.size() != 0) { //проверка, является ли вектор sources пустым.
        for (int i = 0; i < sources.size(); i++) { //цикл, который проходит по всем вершинам в векторе sources.
            MatrixSmejn[0][sources[i]] = 1; //установка значения в матрице
        }
    }
    
     vertDegrees = VertPrepare(); // вызов функции VertPrepare для инициализации вектора vertDegrees
       for (int i = 0; i < vertexCnt - 1; ++i) {
           for (int j = i + 1; j < vertexCnt; ++j) {
               // Генерация степени вершины для связи от вершины i к вершине j
               int degree = LaplaceD();
               // Если степень больше 0, устанавливаем связь
               if (degree > 0) {
                   MatrixSmejn[i][j] = 1;
               }
           }
       }
    //MatrixSmejn[0][(vertexCnt / 2) + 1] = 0;
    //if(vertexCnt != 2)
    //MatrixSmejn[0][vertexCnt-1] = 0;
    
    AssignWeights(); //весы
    MixWeights(); //смкшанные весы
    ModifyWeights(posWeightsMatrix, modPosWeightsMx); //вызов функции ModifyWeights для модификации весов ребер в матрице posWeightsMatrix.
    ModifyWeights(mixedWeightsMatrix, modMixedWeightsMx); //вызов функции ModifyWeights для модификации весов ребер в матрицк
    ReachMatrixGenerator(); //достежимости
    AssignTorrent(); // Стоимость пропуск
    cout << "После введения вершин нажмите Y/y что бы работать с новой матрицой или N/n что бы закончить)" << endl;
}
 









