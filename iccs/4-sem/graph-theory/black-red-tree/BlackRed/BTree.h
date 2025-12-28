#pragma once
#include <time.h>
#include <iostream>
#include <string>
#include <fstream>
#include <queue>
#include<algorithm>

using namespace std;
#define M 3

class Inter_Node;

class Node
{
public:
    
    Node();
    virtual ~Node();
    Node* GetBrother(int& flag);
    Inter_Node* Parent;
    string key[M * 2];
    int count;
    int isLeaf;
    void Print(vector<pair<string, string>> key_words);
};

class Inter_Node : public Node
{
public:
    Inter_Node();
    virtual ~Inter_Node();
    bool Insert(string value, Node* pNode);
    bool Delete(string value);
    string Split(Inter_Node* pNode, string key);
    bool Merge(Inter_Node* pNode);
    bool Slib(Inter_Node* pNode);
    Node* Child[M * 2 + 1];
};

class Leaf_Node : public Node
{
public:
    Leaf_Node();
    virtual ~Leaf_Node();
    bool Insert(string value);
    bool Delete(string value);
    string Split(Leaf_Node* pNode);
    bool Merge(Leaf_Node* pNode);
    Leaf_Node* Pre_Node;
    Leaf_Node* Next_Node;
};


class Bplus
{
public:
    Bplus();
    virtual ~Bplus();
    bool Search(string data);
    bool Insert(string data);
    bool Delete(string data);
    void Print(vector<pair<string, string>> key_words);
    
    Leaf_Node* Find(string data);
    bool Add_Node(Inter_Node* pNode, string key, Node* New_Node);
    bool Remove_Node(Inter_Node* pNode, string key);
    Node* Root;
};

void Search(Bplus* bplus, string s);
bool Add(Bplus* bplus, string s, vector<pair<string, string>>& key_words);
void Del(Bplus* bplus, string s);
void Print_tree(Bplus* bplus, vector<pair<string, string>> key_words);
void From_txt(Bplus* bplus, vector<pair<string, string>> &key_words);
string STRToSTR(string str, unsigned int len);
