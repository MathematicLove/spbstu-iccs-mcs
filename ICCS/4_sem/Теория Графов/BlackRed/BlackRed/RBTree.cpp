#include "RBTree.h"

using namespace std;


RBTree::RBTree() : root(NULL) {
    root = nullptr;
}


RBTree::~RBTree() {
    destory(root);
}

// Поворачивает дерево влево вокруг узла x.
void RBTree::leftRotate(RBTNode*& root, RBTNode* x) {
    RBTNode* y = x->right;
    x->right = y->left; //
    if (y->left != NULL)
        y->left->parent = x;

    y->parent = x->parent;
    if (x->parent == NULL)
        root = y;
    else {
        if (x == x->parent->left)
            x->parent->left = y;
        else
            x->parent->right = y;
    }
    y->left = x; // x левым ребенком y.
    x->parent = y;
}


void RBTree::rightRotate(RBTNode*& root, RBTNode* y) {
    RBTNode* x = y->left;
    y->left = x->right;
    if (x->right != NULL)
        x->right->parent = y;

    x->parent = y->parent; // x наследует родителя y.
    if (y->parent == NULL)
        root = x; // Если y был корнем, теперь корень - x.
    else {
        if (y == y->parent->right)
            y->parent->right = x;
        else
            y->parent->left = x;
    }
    x->right = y; // Делает y правым ребенком x.
    y->parent = x;
}

void RBTree::insert(string key)
{
    RBTNode* k = this->search(key);
    if (k) {
        k->index++;
        return;
    }
    RBTNode* z = new RBTNode(key, Red, NULL, NULL, NULL);
    RBTNode* x = root;
    RBTNode* y = NULL;
    while (x != NULL)
    {
        y = x;
        if (z->key > x->key)
            x = x->right;
        else
            x = x->left;
    }
    z->parent = y;
    if (y != NULL)
    {
        if (z->key > y->key)
            y->right = z;
        else
            y->left = z;
    }
    else
        root = z;
    z->color = Red;
    RBTNode* parent;
    parent = z->parent;
    while (z != root && parent->color == Red)
    {
        RBTNode* gparent = parent->parent;
        if (gparent->left == parent)
        {
            RBTNode* uncle = gparent->right;
            if (uncle != NULL && uncle->color == Red)
            {
                parent->color = Black;
                uncle->color = Black;
                gparent->color = Red;
                z = gparent;
                parent = z->parent;
            }
            else
            {
                if (parent->right == z)
                {
                    leftRotate(root, parent);
                    swap(z, parent);
                }
                rightRotate(root, gparent);
                gparent->color = Red;
                parent->color = Black;
                break;
            }
        }
        else
        {
            RBTNode* uncle = gparent->left;
            if (uncle != NULL && uncle->color == Red)
            {
                gparent->color = Red;
                parent->color = Black;
                uncle->color = Black;

                z = gparent;
                parent = z->parent;
            }
            else
            {
                if (parent->left == z)
                {
                    rightRotate(root, parent);
                    swap(parent, z);
                }
                leftRotate(root, gparent);
                parent->color = Black;
                gparent->color = Red;
                break;
            }
        }
    }
    root->color = Black;
};


void RBTree::insertFromFile(const char* file) {
    std::locale::global(std::locale(""));
    std::ifstream myfile(file);
    if (myfile.is_open()) {
        myfile.imbue(std::locale(""));
        std::string word;
        while (myfile >> word) {
            this->insert(word);
        }
        myfile.close();
    } else {
        std::cerr << "Не удалось открыть файл: " << file << std::endl;
    }
}
bool RBTree::isEmpty() const {
    return root == NULL; 
}
void RBTree::destory(RBTNode*& node)
{
    if (node == NULL)
        return;
    destory(node->left);
    destory(node->right);
    delete node;
    node = nullptr;
}


void RBTree::remove(string key)
{
    RBTNode* deletenode = search(root, key);
    if (deletenode == NULL) {
        return;
    }
    
    RBTNode* child;
    RBTNode* parent;
    RBTColor color;

    if (deletenode->left != NULL && deletenode->right != NULL) {
        //--------------------------------если у корня есть оба поддерева--------------------------------
        // минимальный узел в правом поддереве (преемник)
        RBTNode* replace = deletenode->right;
        while (replace->left != NULL) {
            replace = replace->left;
        }
        
        deletenode->key = replace->key;
        deletenode = replace;
    }

    // deletenode < 1 kid (или бЫтЬ бЕсПлОдНыМ :-))))
    child = (deletenode->left != NULL) ? deletenode->left : deletenode->right;
    parent = deletenode->parent;
    color = deletenode->color;

    // Если у deletenode есть ребенок, обновляем его родителя
    if (child != NULL) {
        child->parent = parent;
    }

    if (parent != NULL) {
        if (deletenode == parent->left) {
            parent->left = child;
        } else {
            parent->right = child;
        }
    } else {
        root = child;
    }

    if (color == Black) {
        fixDelete(child, parent);
    }
    delete deletenode;
}

// исправления дерева если удалили черноый / корнень + случаи с сиблингом - если один папаня
void RBTree::fixDelete(RBTNode* child, RBTNode* parent)
{
    while (child != root && (child == NULL || child->color == Black)) {
        RBTNode* sibling;
        
        if (child == parent->left) {
            sibling = parent->right;

            if (sibling != NULL && sibling->color == Red) {
                // Сиблинг красный
                sibling->color = Black;
                parent->color = Red;
                leftRotate(root, parent);
                sibling = parent->right;
            }
            
            if (sibling != NULL && (sibling->left == NULL || sibling->left->color == Black) &&
                (sibling->right == NULL || sibling->right->color == Black)) {
                // Оба ребенка сиблинга черные
                sibling->color = Red;
                child = parent;
                parent = child->parent;
            } else {
                if (sibling != NULL && (sibling->right == NULL || sibling->right->color == Black)) {
                    // Случай сиблинга и его правого ребенка
                    sibling->left->color = Black;
                    sibling->color = Red;
                    rightRotate(root, sibling);
                    sibling = parent->right;
                }
                
                if (sibling != NULL) {
                    // Балансировка после удаления
                    sibling->color = parent->color;
                    parent->color = Black;
                    sibling->right->color = Black;
                    leftRotate(root, parent);
                    child = root;
                    break;
                }
            }
        } else {
            // зеркальн(child == parent->right)
            sibling = parent->left;

            if (sibling != NULL && sibling->color == Red) {
                // Сиблинг красный
                sibling->color = Black;
                parent->color = Red;
                rightRotate(root, parent);
                sibling = parent->left;
            }

            if (sibling != NULL && (sibling->left == NULL || sibling->left->color == Black) &&
                (sibling->right == NULL || sibling->right->color == Black)) {
                //Оба ребенка сиблинга черные
                sibling->color = Red;
                child = parent;
                parent = child->parent;
            } else {
                if (sibling != NULL && (sibling->left == NULL || sibling->left->color == Black)) {
                    // Случай сиблинга и его левого ребенка
                    sibling->right->color = Black;
                    sibling->color = Red;
                    leftRotate(root, sibling);
                    sibling = parent->left;
                }

                if (sibling != NULL) {
                    // Балансировка после удаления
                    sibling->color = parent->color;
                    parent->color = Black;
                    sibling->left->color = Black;
                    rightRotate(root, parent);
                    child = root;
                    break;
                }
            }
        }
    }
    
    if (child != NULL) {
        child->color = Black;
    }
}



RBTNode* RBTree::search(string key)
{
    return search(root, key);
}

RBTNode* RBTree::search(RBTNode* node, string key) const
{
    if (node == NULL || node->key == key)
        return node;
    else
        if (key > node->key)
            return search(node->right, key);
        else
            return search(node->left, key);
}
// выВОДЫ
void RBTree::print() {
    if (root == NULL)
        cout << "Дерево пустое(\n";
    else
        print(root);
}

void RBTree::print(RBTNode* node)const {
    std::string color_str;
    if (node == NULL)
        return;
    if (node->color == Red)
        color_str = "Красный";
    else color_str = "Черный";
    if (node->parent == NULL) {
        cout << node->key << "(" << color_str << ") - Это КОРЕНЬ" << endl;
    }
    else if (node->parent->left == node)
    {
        cout << node->key << "(" << color_str << ") - " << "Левый " << node->parent->key <<  endl;
    }
    else
    {
        cout << node->key << "(" << color_str << ") - " << "Правый " << node->parent->key << endl;
    }
    print(node->left);
    print(node->right);
}

void RBTree::Clear() {
    destory(root);
}
