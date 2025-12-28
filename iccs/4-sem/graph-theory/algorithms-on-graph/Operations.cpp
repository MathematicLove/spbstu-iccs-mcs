#include "Operations.h"
int InputEdgeCount(const MyGraph& graph) {
    string strInp;
    bool flagInp;
    int res;
    do {
        cout << "Введите кол-во ребер графа- \n";
        getline(cin, strInp);
        if (regex_match(strInp, kRxNumber)) {
            res = stoi(strInp);
            if (res >= 1 && res <= graph.GetVertexCount() - 1) {
                flagInp = true;
            }
            else {
                cout << "Ошибочка( введите правильно!" << endl;
                flagInp = false;
            }
        }
        else {
            cout << "Ошибочка( введите правильно!!!" << endl;
            flagInp = false;
        }
    } while (!flagInp);
    cout << '\n';
    return res;
};
int InputShimbelMode() {
    string strInp;
    bool flagInp;
    int res;
    do {
        cout << "Желаемая матрица: матрица кратчайших путей [0], матрица длиннейших путей [1]\n";
        getline(cin, strInp);
        if (regex_match(strInp, kRxNumber)) {
            res = stoi(strInp);
            if (res >= 0 && res <= 1) {
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
};
int InputNumberOfVert(const MyGraph& graph){
    string strInp;
    bool flagInp;
    int res;
    do {
        cout << "Введите номер вершины:\n";
        getline(cin, strInp);
        if (regex_match(strInp, kRxNumber)) {
            res = stoi(strInp);
            if (res >= 1 && res <= graph.GetVertexCount()) {
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
int InputStartVert(const MyGraph& graph) {
    string strInp;
    bool flagInp;
    int res;
    do {
        cout << "Введите исходную вершину:\n";
        getline(cin, strInp);
        if (regex_match(strInp, kRxNumber)) {
            res = stoi(strInp);
            if (res >= 1 && res <= graph.GetVertexCount()) {
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

int InputAlgorithmMode1() {
    string strInp;
    bool flagInp;
    int res;
    do {
        cout << "Какой алгоритм используем? [0] - Прима. [1] - Краскал.\n";
        getline(cin, strInp);
        if (regex_match(strInp, kRxNumber)) {
            res = stoi(strInp);
            if (res >= 0 && res <= 1) {
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
};

int InputAlgorithmMode() {
    string strInp;
    bool flagInp;
    int res;
    do {
        cout << "С какой матрицей работать (весов) ? [0] - с положительной. [1] - со смешанной.\n";
        getline(cin, strInp);
        if (regex_match(strInp, kRxNumber)) {
            res = stoi(strInp);
            if (res >= 0 && res <= 1) {
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
};
int InputAlgorithmModeACD() {
    string strInp;
    bool flagInp;
    int res;
    do {
        cout << "Какой алгоритм хотите ? [0] - NP-полный алгоритм. [1] - Муравьинный алгоритм.\n";
        getline(cin, strInp);
        if (regex_match(strInp, kRxNumber)) {
            res = stoi(strInp);
            if (res >= 0 && res <= 1) {
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
};
int InputVertex(int f, int s) {
    string strInp;
    bool flagInp;
    int res;
    do {
        cout << "Введите число от " << f << " до " << s << ":\n";
        getline(cin, strInp);
        if (regex_match(strInp, kRxNumber)) {
            res = stoi(strInp);
            if (res >= f && res <= s) {
                flagInp = true;
            }
            else {
                cout << "Ошибочка" << endl;
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

int InputVertex1(int f, int s) {
    string strInp;
    bool flagInp;
    int res;
    do {
        cout << "Введите число от " << f+1 << " до " << s << ":\n";
        getline(cin, strInp);
        if (regex_match(strInp, kRxNumber)) {
            res = stoi(strInp);
            if (res >= f && res <= s) {
                flagInp = true;
            }
            else {
                cout << "Ошибочка" << endl;
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

void ShimbellMethod(const MyGraph& graph) {
    unsigned int start_time = clock();
    int edgesCnt = InputEdgeCount(graph);
    int ShimMode = InputShimbelMode();

    cout << "Матрица весов:\n";
    PrintMatrix(graph.GetWeightsMatrix(0));
    cout << '\n';
    cout << "Матрица Шиммбелла:\n";
    PrintMatrix(graph.CalcShimbell(edgesCnt, static_cast<ShimbellMode>(ShimMode)));
    cout << '\n';
    cout << "Время выполнения: " << endl;
    unsigned int start_end = clock();
    cout<< start_end - start_time << " " << "мс";
    cout << " " << endl;
}

void ReachabilityOperation(const MyGraph& graph) {
    unsigned int start_time = clock();
    // Нумерация с 1
    cout << "Вершина 1: ";
    int vert1 = InputNumberOfVert(graph);

    cout << "Вершина 2: ";
    int vert2 = InputNumberOfVert(graph);
    vector<vector<int> > MatrixReach = graph.GetReachMatrix();
    vert1--; // спасибо С++
    vert2--;

    if (vert1 != vert2) {
        cout << "Матрица достижимости:\n";
        PrintMatrix(MatrixReach);
        cout << '\n';
        if (MatrixReach[vert1][vert2] != 0) {
            cout << "Есть " << MatrixReach[vert1][vert2] << " путей между этими вершинами!\n";
        }
        else {
            cout << "У этих вершин путей не существуеn!\n";
        }
    }
    else
        cout << "Мы уже находимся в вершине " << vert1 + 1 << endl;
    cout << '\n';
    cout << "Время выполнения: " << endl;
    unsigned int start_end = clock();
    cout<< start_end - start_time << " " << "мс";
    cout << " " << endl;
}


void DijkstraAlgorithm(const MyGraph& graph) {
    unsigned int start_time = clock();
    int decision = InputAlgorithmMode();
    if (decision == 0) {
        int inpVert = InputStartVert(graph), counter = 0;
        cout << '\n';
        inpVert--;

        cout << "Матрица весов:\n";
        PrintMatrix(graph.GetWeightsMatrix(0));
        cout << '\n';

        vector<int> distances = graph.Dijkstra(inpVert, counter); //вызов функции Dijkstra для поиска кратчайших расстояний от начальной вершины до всех остальных вершин.
        
        //сейчас восстановить пути, чтобы увидеть не только кратчайшее расстояние до каждой вершины, но и сам путь, который проходит через эти вершины.
       //кратчайшее расстояние до каждой вершины вычисляется с помощью очереди приоритетов. Однако, эта очередь не хранит информацию о пути, по которому было найдено кратчайшее расстояние. Поэтому, после нахождения кратчайших расстояний, необходимо восстановить пути, по которым эти расстояния были найдены. путь затем сохраняется в векторе путей.
        vector<vector<int> > paths = graph.RestorePaths(inpVert, distances, graph.GetWeightsMatrix(2));
//цикл, который проходит по всем вершинам графа, кроме начальной.
        for (int i = 0; i < graph.GetVertexCount(); i++) {
            if (i != inpVert) { //проверка, что текущая вершина не является начальной.
                if (paths[i][0] != INF) { //проверка, что путь до текущей вершины существует.
                    //цикл, который проходит по элементам пути от текущей вершины до начальной.
                    for (int j = paths[i].size() - 1; j > 0; j--) {
                        cout << std::setw(2) << paths[i][j] + 1 << "--> "; //вывод элемента пути
                    }
                    // setw для прикола что б читалось лучше
                    cout << std::setw(2) << paths[i][0] + 1; //вывод начальной вершины пути.
                    cout << " это минимальный путь до вершины " << std::setw(2) << i + 1 << " длинною в " << std::setw(2) << distances[i];
                    cout << endl;
                }
                else {
                    cout << "До вершины " << std::setw(2) << i + 1 << " путя нет!\n";
                }
            }
        }
        cout << '\n';
        cout << " Кол-во итераций: " << counter << "\n\n";
        cout << "Время выполнения: " << endl;
        unsigned int start_end = clock();
        cout<< start_end - start_time << " " << "мс";
        cout << " " << endl;
    }
    else 
        cout << "Алгоритм Дейкстры только с матрицей с положительными значениями!\n\n";
}

void BellmanFordAlgorithm(const MyGraph& graph) { 
    unsigned int start_time = clock();
    int decision = InputAlgorithmMode();
    if (decision == 1) {
        int inpVert = InputStartVert(graph), counter = 0;

        cout << '\n';
        inpVert--;

        cout << "Матрица весов:\n";
        PrintMatrix(graph.GetWeightsMatrix(1));
        cout << '\n';

        vector<int> distances = graph.BellmanFord(inpVert, counter, graph.GetWeightsMatrix(3));
        vector<vector<int> > paths = graph.RestorePaths(inpVert, distances, graph.GetWeightsMatrix(3));

        for (int i = 0; i < graph.GetVertexCount(); i++) {
            if (i != inpVert) {
                if (paths[i][0] != INF) {
                    for (int j = paths[i].size() - 1; j > 0; j--) {
                        cout << std::setw(2) << paths[i][j] + 1 << "--> ";
                    }
                    cout << std::setw(2) << paths[i][0] + 1;
                    cout << " Минимальный путь до вершины " << std::setw(2) << i + 1 << " длиною в " << std::setw(2) << distances[i];
                    cout << endl;
                }
                else {
                    cout << "До вершины" << std::setw(2) << i + 1 << " путя нет!\n";
                }
            }
        }
        cout << '\n';
        cout << " Количество итераций: " << counter << '\n';
        cout << '\n';
        cout << "Время выполнения: " << endl;
        unsigned int start_end = clock();
        cout<< start_end - start_time << " " << "мс";
        cout << " " << endl;
    }
    else {
        int inpVert = InputStartVert(graph), counter = 0;

        cout << '\n';
        inpVert--;

        cout << "Матрица весов:\n";
        PrintMatrix(graph.GetWeightsMatrix(0));
        cout << '\n';

        vector<int> distances = graph.BellmanFord(inpVert, counter, graph.GetWeightsMatrix(2));
        vector<vector<int> > paths = graph.RestorePaths(inpVert, distances, graph.GetWeightsMatrix(2));

        for (int i = 0; i < graph.GetVertexCount(); i++) {
            if (i != inpVert) {
                if (paths[i][0] != INF) {
                    for (int j = paths[i].size() - 1; j > 0; j--) {
                        cout << std::setw(2) << paths[i][j] + 1 << "--> ";
                    }
                    cout << std::setw(2) << paths[i][0] + 1;
                    cout << " это минимальный путь до вершины " << std::setw(2) << i + 1 << " длиною в " << std::setw(2) << distances[i];
                    cout << endl;
                }
                else {
                    cout << "До вершины " << std::setw(2) << i + 1 << " путя нет !\n";
                }
            }
        }
        cout << '\n';
        cout << " Количество итераций: " << counter << '\n';
        cout << '\n';
        cout << "Время выполнения: " << endl;
        unsigned int start_end = clock();
        cout<< start_end - start_time << " " << "мс";
        cout << " " << endl;
    }
}

//что бы восстонавить пути Флойда-Уоршалла улучшить алгоритм restore
void FloydWarshallAlgorithm(const MyGraph& graph) {
    unsigned int start_time = clock();
    int decision = InputAlgorithmMode();
    if (decision == 1) {
        int counter = 0;

        cout << "Матрица весов:\n";
        PrintMatrix(graph.GetWeightsMatrix(1));
        cout << '\n';

        vector<vector<int> > distancesMx = graph.FloydWarshall(counter, graph.GetWeightsMatrix(3));

        cout << "Матрица расстояний:\n";
        cout << "   |";
        for (int i = 0; i < graph.GetVertexCount(); i++) {
            cout << setw(3) << i + 1 << "|";
        }
        cout << endl;
        for (int i = 0; i < graph.GetVertexCount(); i++) {
            cout << setw(3) << i + 1 << "|";
            for (int j = 0; j < graph.GetVertexCount(); j++) {

                if (distancesMx[i][j] != INF) {
                    cout << std::setw(3) << distancesMx[i][j] << " ";
                }
                else {
                    cout << std::setw(3) << "inf" << " ";
                }
            }
            cout << '\n';
        }
        cout << '\n';

        cout << " Кол-во итераций: " << counter << '\n';
        cout << '\n';
        cout << "Время выполнения: " << endl;
        unsigned int start_end = clock();
        cout<< start_end - start_time << " " << "мс";
        cout << " " << endl;
    }
    else {
        int counter = 0;

        cout << "Матрица весов:\n";
        PrintMatrix(graph.GetWeightsMatrix(0));
        cout << '\n';

        vector<vector<int> > distancesMx = graph.FloydWarshall(counter, graph.GetWeightsMatrix(2));

        cout << "Матрица расстояний:\n";
        cout << "   |";
        for (int i = 0; i < graph.GetVertexCount(); i++) {
            cout << setw(3) << i + 1 << "|";
        }
        cout << endl;
        for (int i = 0; i < graph.GetVertexCount(); i++) {
            cout << setw(3) << i + 1 << "|";
            for (int j = 0; j < graph.GetVertexCount(); j++) {

                if (distancesMx[i][j] != INF) {
                    cout << std::setw(3) << distancesMx[i][j] << " ";
                }
                else {
                    cout << std::setw(3) << "inf" << " ";
                }
            }
            cout << '\n';
        }
        cout << '\n';

        cout << " Кол-во итераций: " << counter << "\n\n";
        cout << "Время выполнения: " << endl;
        unsigned int start_end = clock();
        cout<< start_end - start_time << " " << "мс";
        cout << " " << endl;
    }
}

void MinCostFlow(const MyGraph& graph) {
    unsigned int start_time = clock();
    bool flag = false;
    int firstV, lastV;
    do {
        cout << "Введите номер первой вершины : " << endl;
        firstV = InputVertex(1, graph.GetVertexCount()-1);
        cout << "Введите номер второй вершины : " << endl;
        lastV = InputVertex(firstV+1, graph.GetVertexCount());
        if (firstV == lastV)
            cout << "Нельзя искать поток минимальной стоимости из вершины в нее саму !!!" << endl;
    } while (firstV == lastV);
        firstV--; lastV--;
        int schetMin = graph.calcMinCostFlow(firstV, lastV);
        cout << "Поток минимальной стоимости : " << schetMin << endl << endl;
    cout << "Время выполнения: " << endl;
    unsigned int start_end = clock();
    cout<< start_end - start_time << " " << "мс";
    cout << " " << endl;
}

void Hamilton(const MyGraph& graph) {
    unsigned int start_time = clock();
    int mode = InputAlgorithmModeACD();
    // Если работаем с положительной:
    if (mode == 0) {
        graph.Hamilton(graph.GetWeightsMatrix(0), mode); }
    //Эвристика :
    else {
        graph.AntColonyOptimization(graph.GetWeightsMatrix(0), mode);
    }
    cout << "Время выполнения: " << endl;
    unsigned int start_end = clock();
    cout<< start_end - start_time << " " << "мс";
    cout << " " << endl;
}

void Euler(const MyGraph& graph) { int mode = InputAlgorithmMode();
    unsigned int start_time = clock();
 // Если работаем с положительной:
    if (mode == 0) {
        graph.Euler(graph.GetWeightsMatrix(0), mode);
    }
 //Если с отрицательной :
    else {
        graph.Euler(graph.GetWeightsMatrix(1), mode);
    }
    cout << "Время выполнения: " << endl;
    unsigned int start_end = clock();
    cout<< start_end - start_time << " " << "мс";
    cout << " " << endl;
 }

