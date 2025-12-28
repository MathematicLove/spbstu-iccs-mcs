#pragma once
#include <iostream>
#include <vector>
#include <fstream>


using namespace std;

enum RBTColor { Black, Red };

struct  RBTNode
{
    string key;
    int index = 0;
    RBTColor color;
    RBTNode* left;
    RBTNode* right;
    RBTNode* parent;
    RBTNode(string k, RBTColor c, RBTNode* p, RBTNode* l, RBTNode* r) :
        key(k), color(c), parent(p), left(l), right(r) { };
};


class  RBTree
{
public:
    RBTree();
    ~RBTree();

    void insert(string key);
    void insertFromFile(const char* file);
    bool isEmpty() const;
    void remove(string key);
    RBTNode* search(string key);
    void print();
    void Clear();



private:
    void leftRotate(RBTNode*& root, RBTNode* x);// Левая
    void rightRotate(RBTNode*& root, RBTNode* y);// Правая

//    void insert(RBTNode*& root, RBTNode* node);// потом

    void destory(RBTNode*& node);

//    void remove(RBTNode*& root, RBTNode* node);// удалить как KEY
    void fixDelete(RBTNode* child, RBTNode* parent);


    RBTNode* search(RBTNode* node, string key) const;
    void print(RBTNode* node)const;
private:
    RBTNode* root;
};

