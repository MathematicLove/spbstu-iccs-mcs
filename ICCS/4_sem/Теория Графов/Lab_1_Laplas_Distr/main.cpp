#include "globals.h"
#include "MyGraph.h"
#include "Operations.h"
#include "FSM.h"
#include <string>

using namespace std;
vector<vector<int>> CreateUndirectedMatrix(const vector<vector<int>>& weightsMatrix);
bool forPrufer;
void ColorAndPrintIndependentSets(MyGraph& graph) {
    vector<vector<int>> independentSets = graph.FindMaximalIndependentSets();
    unordered_map<int, string> vertexColorMap;
    vector<string> colors = {"Красный", "Зеленый", "Синий", "Желтый"};
    
    int currentColorIndex = 0;
    for (const auto& set : independentSets) {
        string currentColor = colors[currentColorIndex % colors.size()];
        cout << "{";
        for (size_t i = 0; i < set.size(); i++) {
            vertexColorMap[set[i]] = currentColor;
            cout << set[i];
            if (i < set.size() - 1) {
                cout << ", ";
            }
        }
        cout << "} - " << currentColor << endl;
        currentColorIndex++;
    }
}

int luna(string const & input)
{
    int check_digit = 0;
    bool odd = false;
    for (auto it = input.rbegin(); it != input.rend(); ++it)
    {
        auto digit = *it - '0';
        if ((odd = !odd))
        {
            digit *= 2;
            if (digit > 9)
                digit -= 9;
        }
        check_digit += digit;
    }
    return (check_digit * 9) % 10;
}

int main()
{
    setlocale(LC_ALL, "Russian");
    int VertexCNT = InputVertexCount();
    MyGraph graph(VertexCNT);
    cout << "Матрица смежности вершин:" << endl;
    vector<vector<int>> MatAdj1;
    PrintMatrix(graph.GetAdjMatrix());
    cout << endl;
    cout << "Весовая положительная матрица:" << endl;
    PrintMatrix(graph.GetWeightsMatrix(0));
    cout << endl;
    cout << "Весовая смешанная матрица:" << endl;
    PrintMatrix(graph.GetWeightsMatrix(1));
    cout << endl;
    MatAdj1= graph.GetAdjMatrix();
    int countofE=0;
    for(int ie = 0; ie < VertexCNT; ie++){
        for(int je = 0; je< VertexCNT; je++){
            countofE += MatAdj1[ie][je];
        }
    }
    cout << endl;
    cout << "Количество вершин: " << VertexCNT << endl;
    cout <<"Количество ребер: " << countofE << endl;
    cout << endl;
    cout << "Рекомендации: " << endl;
    cout << endl;
    if(VertexCNT <= 15 && countofE <= 85){
        cout <<"Для быстрого поиска кратчайших путей можете выбрать: " << endl;
        cout << "Алгоритм Дейкстры, Алгоритмы DFS и BFS, Алгоритм Флойда-Уоршелла" << endl;
        cout << "Для минимального остовного дерева можно выбрать алгоритм по желанию" << endl;
    }
    else
    {
        cout << "Для быстрого поиска кратчайших путей можете выбрать: " << endl;
        cout << "Алгоритм Беллмана-Форда, Алгоритм Дейкстры" << endl;
        cout << "Для минимального остовного дерева лучше выбрать Краскала" << endl;
    }

    char YesNo;  
    vector<vector<pair<int, int>>> mstTree;
    int MenuItem, decision, i;
    do {
        cout << endl;
        cout << endl;
        PrintMenu();
        cout << '\n';
        MenuItem = InputMenuItem();
        switch (MenuItem) {
            case 0: //Метод Шимбела
                ShimbellMethod(graph);
                break;
            case 1: //Матрица достижимости
                ReachabilityOperation(graph);
                break;
            case 2: //Алгоритм дейкстры
                DijkstraAlgorithm(graph);
                break;
            case 3: //Алгоритм Беллмана-Форда
                BellmanFordAlgorithm(graph);
                break;
            case 4: //Алгоритм Флойда-Уоршала
                FloydWarshallAlgorithm(graph);
                break;
            case 5: //Поиск в глубину
                int startVertexDFS, endVertexDFS;
                cout << "Введите начальную вершину для поиска в глубину: ";
                cin >> startVertexDFS;
                cout << "Введите конечную вершину для поиска в глубину: ";
                cin >> endVertexDFS;
                if(startVertexDFS == endVertexDFS){
                    cout << "мы уже в этой вершине"<< endl;
                    cin.ignore();
                    break;
                }
                if(startVertexDFS == VertexCNT){
                    cout << "Введите хотя бы на 1 меньше" << endl;
                    cin.ignore();
                    break;
                }
                if(startVertexDFS > VertexCNT ){
                    cout << " Больше чем кол-во вершин " << endl;
                    cin.ignore();
                    break;
                }
                graph.findShortestPathDFS1(startVertexDFS, endVertexDFS);
                cin.ignore();
                break;
            case 6: //Поиск в ширину
                int startVertexBFS, endVertexBFS;
                cout << "Введите начальную вершину для поиска в ширину: ";
                cin >> startVertexBFS;
                cout << "Введите конечную вершину для поиска в ширину: ";
                cin >> endVertexBFS;
                if(startVertexBFS == endVertexBFS){
                    cout << "мы уже в этой вершине"<< endl;
                    cin.ignore();
                    break;
                }
                if(startVertexBFS == VertexCNT){
                    cout << "Введите хотя бы на 1 меньше" << endl;
                    cin.ignore();
                    break;
                }
                if(startVertexBFS > VertexCNT ){
                    cout << " Больше чем кол-во вершин " << endl;
                    cin.ignore();
                    break;
                }
                graph.BFS(startVertexBFS, endVertexBFS);
                cin.ignore();
                break;
            case 7: // Алгоритм Прима и Краскала
            {
                unsigned int start_time = clock();
                i = 0; decision = InputAlgorithmMode();
                if (decision == 0) {
                    graph.Prim(graph.GetWeightsMatrix(0), i);
                    cout << "Количество итераций : " << i << endl << endl;
                }
                else {
                    graph.Kraskal(graph.GetWeightsMatrix(0), i);
                    cout << "Количество итераций : " << i << endl << endl;
                }
                forPrufer = 1;
                cout << "Время выполнения: " << endl;
                unsigned int start_end = clock();
                cout<< start_end - start_time << " " << "мс";
                cout << " " << endl;
        }
                break;
                
            case 8: // Код Прюфера
            {
                if(forPrufer !=1){
                    cout << "Вы не создали минимальный остов!!! перейдите в пункт 7!" << endl;
                }
                else{
                    graph.PruferCode();
                    graph.PruferDecode();
                }
            }
                break;
            case 9: // ЭТО ЭДМОНДС-КАРП! BFS
                MinCostFlow(graph); //i1 j1 = 0 ??
                
                break;
            case 10: // Минимальный разрез? Алгоритм Каргера + модифицированный Эдмондс
            {
                int min_cut = graph.minCut(graph.torrent);
                //cout << graph.minCut(graph.torrent) << endl;
                cout << "Минимальный разрез: " << min_cut << endl;
                cout << " ";
            }
                cin.ignore();
                break;
            case 11: // A*
            {
                int startVertex, endVertex;
                cout << "Введите начальную вершину: ";
                cin >> startVertex;
                cout << "Введите конечную вершину: ";
                cin >> endVertex;
                // Поиск пути с использованием алгоритма A*
                graph.aStarSearch(graph.GetWeightsMatrix(0), startVertex, endVertex);
                cin.ignore();
            }
            break;
            case 12:
                // новый граф?
            {
                graph.NewGraph();
                MatAdj1.clear();
                cout << "Матрица смежности вершин:" << endl;
                PrintMatrix(graph.GetAdjMatrix());
                cout << endl;
                cout << "Весовая положительная матрица:" << endl;
                PrintMatrix(graph.GetWeightsMatrix(0));
                cout << endl;
                cout << "Весовая смешанная матрица:" << endl;
                PrintMatrix(graph.GetWeightsMatrix(1));
                cout << endl;
        }
                break;
            case 13: // Алгоритм Луна по приколу :)
            {
                cout << "Алгоритм Луна - Если проверочная цифра совподет с конечной, то номер коррекнтый" << endl;
                string input;
                cout << "Введите строку цифр: ";
                getline(cin, input);
                int check_digit = luna(input);
                cout << "Проверочная цифра: " << check_digit << endl;
                int lastDigit = input[input.length() - 1] - '0';
                if (lastDigit == check_digit)
                    cout << "Номер правильный" << endl;
                else
                    cout << "Номер неверный" << endl;
            }
                break;
            case 14: // Гамильтон ура
            {
                Hamilton(graph);
                break;
            }
            case 15: // Эйлер??
                Euler(graph);
                break;
            case 16: // Раскраска + МНМ
                ColorAndPrintIndependentSets(graph);
                break;
            case 17:
                return 0;
        default:
            break;
        }
    } while (IsContinue(YesNo));
}

