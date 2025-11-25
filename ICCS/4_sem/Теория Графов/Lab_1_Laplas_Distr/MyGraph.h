#pragma once
#include "globals.h"
#include <set>
#include <iostream>
#include <stdio.h>
#include <fstream>
using namespace std;

enum class ShimbellMode {
    Short = 0,
    Long = 1
};

struct Node {
    int vertex;
    int g_cost; // стоимость пути от начальной вершины до текущей
    int h_cost; // эвристическая оценка стоимости от текущей вершины до конечной
    Node(int v, int g, int h) : vertex(v), g_cost(g), h_cost(h) {}
};

struct CompareNodes {
    bool operator()(const Node& n1, const Node& n2) {
        // сравниваем суммарную стоимость f = g + h
        return n1.g_cost + n1.h_cost > n2.g_cost + n2.h_cost;
    }
};

struct edge {
    int a, b, cost;
    edge(int a2, int b3, int cost4) {
        a = a2;
        b = b3;
        cost = cost4;
    }
};

class MyGraph {
    
private:
    int vertexCnt; // переменная, которая хранит количество вершин в графе.
    vector<vector<int> > MatrixSmejn; // двумерный вектор, который представляет матрицу смежности графа.
    vector<vector<int> > posWeightsMatrix; // двумерный вектор, который представляет матрицу весов ребер графа.
    vector<vector<int> > mixedWeightsMatrix; // двумерный вектор, который представляет смешанную матрицу весов ребер графа.
    vector<vector<int> > modPosWeightsMx; // двумерный вектор, который представляет модифицированную матрицу весов ребер графа.
    vector<pair<int, int>> previous; // A*
    vector<vector<double>> pheromoneMatrix;
    vector<vector<int> > modMixedWeightsMx; //двумерный вектор, который представляет модифицированную смкшанную матрицу весов ребер графа.
    vector<vector<int> > MatrixReach;  // двумерный вектор, который представляет матрицу достижимости графа.
    vector<int> VertPrepare() const; //функция, которая подготавливает вершины графа для матрицы смежности.
    void AssignWeights(); // функция, которая присваивает веса ребрам графа.
    void MixWeights(); // смешанные веса
    void ModifyWeights(vector<vector<int> >& WeightsMatrix, vector<vector<int> >& modified); // функция, которая модифицирует веса ребер в матрице.
    vector<vector<int>> MultByShimbell(const vector<vector<int> > fMx, // функция, которая умножает две матрицы с использованием алгоритма Шимбелла
                                       const vector<vector<int> > sMx, ShimbellMode mode) const;
    vector<vector<int>> throughPutMx; // двумерный вектор, который представляет матрицу пропускной способности ребер графа.
    //vector<vector<int>> torrent;// матрица стоимости
    vector<vector<int>> torrentMatForFord;
    vector<vector<int>> MatrixFakeSource; //двумерный вектор, который представляет матрицу фиктивного источника графа. потом если не нужен то станет копией смежности
    vector<vector<int> > UnOrMxSmejn; //двумерный вектор, который представляет матрицу смежности графа без отрицательных весов ребер.
    vector<vector<int> > UnOrPosWeightMx; // веса >0
    vector<vector<int> >minTree; // потом
    vector<pair<int, int> > PruferTree; // потом ???
public:
   
    MyGraph(int n); //конструктор класса MyGraph, который инициализирует переменные класса.
    vector<vector<int>> torrent; // матрица стоимости
    int GetVertexCount() const; //функция, которая возвращает количество вершин в графе.
    vector<vector<int> > GetAdjMatrix() const; //возвращает матрицу смежности графа.
    vector<vector<int> > GetWeightsMatrix(int type) const; //весов
    vector<vector<int> > GetReachMatrix() const; // достижимости
    vector<vector<int> > GetTorrentMatrix() const; // матрица стоимости
    vector<vector<int>> CalcShimbell(int edgeCnt, ShimbellMode mode) const;
    void MakeUndirected(); // сделать неориентированным
    void CreateUnderictedMatrix(const vector<vector<int>>& weightsMatrix);
    void DFSUtil(int v, int endVertex, vector<bool>& visited, vector<int>& visitedNodes, vector<int>& parent, int& totalDistance);
    void printPath(int startVertex, int endVertex, const vector<int>& parent, const vector<vector<int>>& weights) const;
    void printPathDFS(int startVertex, int endVertex, const vector<int>& parent, const vector<vector<int>>& weights) const;
    void DFS(int startVertex, int endVertex); // обход в глубину для графа. выводы и т.д.
    void DFS1(int currentVertex, int endVertex, vector<int>& path, vector<bool>& visited, vector<int>& shortestPath, int& shortestWeight);
    int DFS2(int currentVertex, int endVertex, vector<int>& path, vector<bool>& visited, vector<int>& shortestPath, int& shortestWeight);
    void findShortestPathDFS1(int startVertex, int endVertex);
    void BFS(int startVertex, int endVertex); //обход в ширину для графа, начиная с указанной вершины
    int BFS1(int startVertex,int endVertex);
    void ReachMatrixGenerator(); // матрица достижимости графа.
    void BuildFakeSource();
    vector<int> Dijkstra(int inpVert, int& counter) const; //алгоритм Дейкстры для нахождения кратчайшего пути в графе
    vector<vector<int> > RestorePaths(int inpVert, const vector<int>& distances, const vector<vector<int> > weightMx) const;//восстанавливает пути в графе. потом
    vector<int> BellmanFord(int inpVert, int& counter, vector<vector<int> > mx) const; //алгоритм Беллмана-Форда для нахождения кратчайшего пути в графе.
    vector<int> BellmanFord(int inpVert, vector<vector<int> > mx) const; //алгоритм Беллмана-Форда для нахождения кратчайшего пути в графе но для весовой.
    vector<vector<int> > FloydWarshall(int& counter, vector<vector<int> > mx) const; //алгоритм Флойда-Уоршала с INF нахождения кратчайшего пути в графе.
    vector<int> encodePruefer(const vector<vector<pair<int, int>>>& tree);
    vector<vector<pair<int, int>>> PrimMST(); // алгоритм Прима для минимального остовного графа
    bool isAchievable(int vertexOne, int vertexTwo, vector<vector<int>>& graph, int lastVertex) const;
    void Kraskal(vector<vector<int>> weightMx, int &iterationCounter);
    void Prim(vector<vector<int>> weightMx, int& iterationCounter);
    void PruferCode();
    void PruferDecode();
    void AssignTorrent();
    bool bfs_FordFulkerson(vector<vector<int>> residualG, int source, int sink, vector<int>&path) const;
    int fordFulkerson(int source,int sink) const;
    int calcMinCostFlow(int s, int t) const;
    void AssignTorrent1();
    void MakeUndirected1();
    bool bfs_FordFulkerson1(vector<vector<int>> matrix, int source, int sink, vector<int>& path) const;
    void addEdge(int u, int v, int capacity);
    void fordFulkerson1(int source, int sink) const;
    int minCut(vector<vector<int>>& torrent2); /////////////
    int countCrossingEdges(vector<vector<int>>& matrix, vector<bool>& cutSet);
    void mergeVertices(vector<vector<int>>& matrix,vector<bool>& cutSet, int u,int v);
    int DijkstraAS(int startVertex, int endVertex) const;
    int heuristic(int start, int end, const vector<vector<int>>& graph);
    vector<int> aStarSearch(const vector<vector<int>>& graph, int start, int end);
    void Hamilton(vector<vector<int>> weightMx, int weightMode) const;
    //void (ofstream& fout, vector<vector<int>>& graph, vector<int>& path, int length, vector<int>& minimumPath, int& minimumLength) const;
    void AntColonyOptimization(vector<vector<int>> weightMx, int weightMode) const;
    void findHamiltonCycleACD(ofstream& fout, vector<vector<int>>& graph, vector<int>& path, int length, vector<int>& minimumPath, int& minimumLength) const;
    void Euler(vector<vector<int>> weightMx, int weightMode) const;
    void EulerCycles(vector<vector<int>> weightMx, vector<int> degrees) const;
    vector<vector<int>> FindMaximalIndependentSets();
    void BronKerboschIterative(set<int>& r, set<int>& p, set<int>& x, vector<vector<int>>& maximalIndependentSets, vector<vector<int>>& tempMatrix);
   // void Otjig(ofstream& fout, vector<vector<int>>& graph, vector<int>& path, int lenght, vector<int>& minimumPath, int& minimumlenght) const;
    //void FGG(ofstream& fout, vector<vector<int>>& graph, vector<int>&path, int lenght, vector<int>&minpath, int minlenght) const;
    void NewGraph();
};
