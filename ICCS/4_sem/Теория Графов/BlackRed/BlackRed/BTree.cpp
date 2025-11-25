#include "BTree.h"

Inter_Node::~Inter_Node()
{
    for (int i = 0; i < M * 2 + 1; i++)
        Child[i] = NULL;
}

Node::~Node() {}
Leaf_Node::~Leaf_Node() {}
Bplus::~Bplus() {}

Node::Node()
{
    isLeaf = true;
    count = 0;
    Parent = NULL;
}

Leaf_Node::Leaf_Node()
{
    isLeaf = true;
    Pre_Node = NULL;
    Next_Node = NULL;
}

Inter_Node::Inter_Node()
{
    isLeaf = false;
    for (int i = 0; i < M * 2 + 1; i++)
        Child[i] = NULL;
}

Bplus::Bplus()
{
    Root = NULL;
}


Node* Node::GetBrother(int& flag)
{
    if (NULL == Parent)
        return NULL;

    Node* p = NULL;
    for (int i = 0; i <= Parent->count; i++)
    {
        if (Parent->Child[i] == this)
        {
            if (i == Parent->count)
            {
                p = Parent->Child[i - 1];
                flag = 1;
            }
            else
            {
                p = Parent->Child[i + 1];
                flag = 2;
            }
        }
    }
    return p;
}


void Node::Print(vector<pair<string, string>> key_words)
{
    
    for (int i = 0; i < this->count; i++)
    {
        string s;
        for (auto x: key_words)
        {
            if (x.first == key[i])
            {
                s = x.second;
                break;
            }
        }
        cout << s << " ";
        if (i >= count - 1)
            cout <<  "|> ";
    }
}


string Leaf_Node::Split(Leaf_Node* p)
{
    int j = 0;
    for (int i = M; i < M * 2; i++, j++)
        p->key[j] = this->key[i];
    this->count = this->count - j;
    p->count = j;
    return p->key[0];
}

bool Leaf_Node::Delete(string value)
{
    bool found = false;
    int i = 0;
    for (; i < count; i++)
    {
        if (value == key[i])
        {
            found = true;
            break;
        }
    }
    if (false == found)
        return false;
    int j = i;
    for (; j < count - 1; j++)
        key[j] = key[j + 1];
    key[j] = string();
    count--;
    return true;
}


bool Leaf_Node::Insert(string value)
{
    int i = 0;
    for (; (value > key[i]) && (i < count); i++)
    {
    }
    for (int j = count; j > i; j--)
        key[j] = key[j - 1];
    key[i] = value;
    count++;
    return true;
}


Leaf_Node* Bplus::Find(string data)
{
    int i = 0;
    Node* p = Root;
    Inter_Node* q;
    while (NULL != p)
    {
        if (p->isLeaf)
            break;
        for (i = 0; i < p->count; i++)
        {
            if (data < p->key[i])
                break;
        }
        q = (Inter_Node*)p;
        p = q->Child[i];
    }

    return (Leaf_Node*)p;
}


bool Leaf_Node::Merge(Leaf_Node* p)
{
    if (this->count + p->count > M * 2)
        return false;
    for (int i = 0; i < p->count; i++)
        this->Insert(p->key[i]);
    return true;
}

 
bool Inter_Node::Merge(Inter_Node* p)
{
    key[count] = p->Child[0]->key[0];
    count++;
    Child[count] = p->Child[0];
    for (int i = 0; i < p->count; i++)
    {
        key[count] = p->key[i];
        count++;
        Child[count] = p->Child[i + 1];
    }
    return true;
}

 
bool Inter_Node::Insert(string value, Node* New)
{
    int i = 0;
    for (; (i < count) && (value > key[i]); i++)
    {
    }
    for (int j = count; j > i; j--)
        key[j] = key[j - 1];
    for (int j = count + 1; j > i + 1; j--)
        Child[j] = Child[j - 1];
    key[i] = value;
    Child[i + 1] = New;
    New->Parent = this;
    count++;
    return true;
}


string Inter_Node::Split(Inter_Node* p, string k)
{
    int i = 0, j = 0;
    if ((k > this->key[M - 1]) && (k < this->key[M]))
    {
        for (i = M; i < M * 2; i++, j++)
            p->key[j] = this->key[i];
        j = 1;
        for (i = M + 1; i <= M * 2; i++, j++)
        {
            this->Child[i]->Parent = p;
            p->Child[j] = this->Child[i];
        }
        this->count = M;
        p->count = M;
        return k;
    }
    int pos = k < this->key[M - 1] ? (M - 1) : M;
    k = this->key[pos];
    j = 0;
    for (i = pos + 1; i < M * 2; i++, j++)
        p->key[j] = this->key[i];
    j = 0;
    for (i = pos + 1; i <= M * 2; i++, j++)
    {
        this->Child[i]->Parent = p;
        p->Child[j] = this->Child[i];
    }
    this->count = pos;
    p->count = M * 2 - pos - 1;
    return k;
}

bool Inter_Node::Delete(string k)
{
    int i = 0;
    for (; (k >= key[i]) && (i < count); i++)
    {
    }
    for (int j = i - 1; j < count - 1; j++)
        key[j] = key[j + 1];
    int l = i;
    for (; l < count; l++)
    {
        Child[l] = Child[l + 1];
    }
    Child[l] = NULL;
    count--;
    return true;
}

bool Inter_Node::Slib(Inter_Node* p)
{
    int i, j;
    if (p->key[0] < this->key[0])
    {
        for (i = count; i > 0; i--)
            key[i] = key[i - 1];
        for (j = count + 1; j > 0; j--)
            Child[j] = Child[j - 1];
        key[0] = Child[0]->key[0];
        Child[0] = p->Child[p->count];
    }
    else
    {
        key[count] = p->Child[0]->key[0];
        Child[count + 1] = p->Child[0];
        for (i = 1; i < p->count - 1; i++)
            p->key[i - 1] = p->key[i];
        for (j = 0; j < p->count - 1; j++)
            p->Child[j] = p->Child[j + 1];
    }
    this->count++;
    p->count--;
    return true;
}


bool Bplus::Add_Node(Inter_Node* p, string k, Node* New_Node)
{
    if (NULL == p || p->isLeaf)
        return false;
    if (p->count < M * 2)
        return p->Insert(k, New_Node);
    Inter_Node* Brother = new Inter_Node;
    string NewKey = p->Split(Brother, k);
    if (p->count < Brother->count)
    {
        p->Insert(k, New_Node);
    }
    else if (p->count > Brother->count)
    {
        Brother->Insert(k, New_Node);
    }
    else
    {
        Brother->Child[0] = New_Node;
        New_Node->Parent = Brother;
    }
    Inter_Node* parent = (Inter_Node*)(p->Parent);
    if (NULL == parent)
    {
        parent = new Inter_Node();
        parent->Child[0] = p;
        parent->key[0] = NewKey; 
        parent->Child[1] = Brother;
        p->Parent = parent;
        Brother->Parent = parent;
        parent->count = 1;
        Root = parent;
        return true;
    }
    return Add_Node(parent, NewKey, Brother);
}

bool Bplus::Search(string data)
{
    int i = 0;

    Node* p = Root;
    if (NULL == p)
        return false;
    Inter_Node* q;
    while (NULL != p)
    {
        if (p->isLeaf)
            break;
        for (i = 0; (i < p->count) && (data >= p->key[i]); i++)
        {
        }
        int k = i > 0 ? i - 1 : i;

        q = (Inter_Node*)p;
        p = q->Child[i];
    }
    if (NULL == p)
        return false;

    bool found = false;
    for (i = 0; i < p->count; i++)
    {
        if (data == p->key[i])
            found = true;
        
    }
    
    return found;
}

bool Bplus::Insert(string data)
{

    string a;
    if (true == Search(data))
        return false;

    Leaf_Node* Old_Node = Find(data);

    if (NULL == Old_Node)
    {
        Old_Node = new Leaf_Node;
        Root = Old_Node;
    }

    if (Old_Node->count < M * 2) {
        return Old_Node->Insert(data);

    }

    Leaf_Node* New_Node = new Leaf_Node;

    string k = Old_Node->Split(New_Node);

    Leaf_Node* OldNext = Old_Node->Next_Node;
    Old_Node->Next_Node = New_Node;
    New_Node->Next_Node = OldNext;
    New_Node->Pre_Node = Old_Node;

    if (NULL != OldNext)
        OldNext->Pre_Node = New_Node;

    if (data < k)
    {
        Old_Node->Insert(data);
    }
    else
    {
        New_Node->Insert(data);
    }
    Inter_Node* parent = (Inter_Node*)(Old_Node->Parent);

    if (NULL == parent)
    {
        Inter_Node* New_Root = new Inter_Node;
        New_Root->Child[0] = Old_Node;
        New_Root->key[0] = k;
        New_Root->Child[1] = New_Node;
        Old_Node->Parent = New_Root;
        New_Node->Parent = New_Root;
        New_Root->count = 1;
        Root = New_Root;
        return true;
    }

    return Add_Node(parent, k, New_Node);
}

bool Bplus::Delete(string data)
{
    Leaf_Node* Old_Node = Find(data);
    if (NULL == Old_Node)
        return false;
    if (false == Old_Node->Delete(data))
        return false;
    Inter_Node* parent = (Inter_Node*)(Old_Node->Parent);
    if (NULL == parent)
    {
        if (0 == Old_Node->count)
        {
            delete Old_Node;
            Root = NULL;
        }
        return true;
    }
    if (Old_Node->count >= M)
    {
        for (int i = 0; (i < parent->count) && (data >= parent->key[i]); i++)
        {
            if (parent->key[i] == data)
                parent->key[i] = Old_Node->key[0];
        }
        return true;
    }
    
    int flag = 1;
    Leaf_Node* Brother = (Leaf_Node*)(Old_Node->GetBrother(flag));
    string NewData;
    if (Brother->count > M)
    {
        if (1 == flag)
        {
            NewData = Brother->key[Brother->count - 1];
        }
        else
        {
            NewData = Brother->key[0];
        }
        Old_Node->Insert(NewData);
        Brother->Delete(NewData);
        if (1 == flag)
        {
            for (int i = 0; i <= parent->count; i++)
            {
                if (parent->Child[i] == Old_Node && i > 0)
                    parent->key[i - 1] = Old_Node->key[0];
            }
        }
        else
        {
            for (int i = 0; i <= parent->count; i++)
            {
                if (parent->Child[i] == Old_Node && i > 0)
                    parent->key[i - 1] = Old_Node->key[0];
                if (parent->Child[i] == Brother && i > 1)
                    parent->key[i - 1] = Brother->key[0];
            }
        }
        return true;
    }
    string NewKey;
    if (1 == flag)
    {
        Brother->Merge(Old_Node);
        NewKey = Old_Node->key[0];
        Leaf_Node* OldNext = Old_Node->Next_Node;
        Brother->Next_Node = OldNext;
        if (NULL != OldNext)
            OldNext->Pre_Node = Brother;
        delete Old_Node;
    }
    else
    {
        Old_Node->Merge(Brother);
        NewKey = Brother->key[0];
        Leaf_Node* OldNext = Brother->Next_Node;
        Old_Node->Next_Node = OldNext;
        if (NULL != OldNext)
            OldNext->Pre_Node = Old_Node;
        delete Brother;
    }
    return Remove_Node(parent, NewKey);
}

bool Bplus::Remove_Node(Inter_Node* p, string k)
{
    if (false == p->Delete(k))
    {
        return false;
    }
    Inter_Node* parent = (Inter_Node*)(p->Parent);
    if (NULL == parent)
    {
        if (0 == p->count)
        {
            Root = p->Child[0];
            delete p;
        }
        return true;
    }
    if (p->count >= M)
    {
        for (int i = 0; (i < parent->count) && (k >= parent->key[i]); i++)
        {
            if (parent->key[i] == k)
                parent->key[i] = p->key[0];
        }
        return true;
    }
    
    int flag = 1;
    Inter_Node* Brother = (Inter_Node*)(p->GetBrother(flag));
    if (Brother->count > M)
    {
        p->Slib(Brother);
        if (1 == flag)
        {
            for (int i = 0; i <= parent->count; i++)
            {
                if (parent->Child[i] == p && i > 0)
                    parent->key[i - 1] = p->key[0];
            }
        }
        else
        {
            for (int i = 0; i <= parent->count; i++)
            {
                if (parent->Child[i] == p && i > 0)
                    parent->key[i - 1] = p->key[0];
                if (parent->Child[i] == Brother && i > 0)
                    parent->key[i - 1] = Brother->key[0];
            }
        }
        return true;
    }
    
    string NewKey = 0;
    if (1 == flag)
    {
        Brother->Merge(p);
        NewKey = p->key[0];
        delete p;
    }
    else
    {
        p->Merge(Brother);
        NewKey = Brother->key[0];
        delete Brother;
    }
    return Remove_Node(parent, NewKey);
}


void Bplus::Print(vector<pair<string, string>> key_words)
{
    Node* p = Root;
    if (NULL == p)
    {
        cout << "Пока тут пусто(" << '\n';
        return;
    }
    Inter_Node* a;
    int H = 0;
    queue<Node*> q;
    queue<int> h;
    q.push(p);
    h.push(1);
    while (!q.empty())
    {
        p = q.front();
        if (H != h.front())
        {
            cout << endl;
            cout << H << "-уровень - ";
            H = h.front();
        }
        q.pop();
        h.pop();
        p->Print(key_words);
        if (NULL != p && !p->isLeaf)
        {
            a = (Inter_Node*)p;
            for (int i = 0; i <= p->count; i++)
            {
                q.push(a->Child[i]);
                h.push(H + 1);
            }
        }
    }
}


void Search(Bplus* bplus, string s)
{
    string data = STRToSTR(s, s.size());
    

    if (bplus->Search(data))
    {
        cout << "Нашлось!" << endl;
    }
    else
    {
        cout << "Не нашлось(" << endl;
    }

}


bool Add(Bplus* bplus, string s, vector<pair<string, string>>& key_words)
{
    
    string data = STRToSTR(s, s.size());
    bool success = bplus->Insert(data);
    key_words.push_back(pair<string, string>(data, s));
    
  //  if (true != success)
   // {
        
   // }
    return success;
}

void Del(Bplus* bplus, string s)
{
    string data = STRToSTR(s, s.size());
    bool success = bplus->Delete(data);
    if (true == success)
    {
        cout << "Оно удалилось!! " << endl;
    }
    else
    {
        cout << "Не найдено(" << endl;
    }
    cout << endl;
}

void Print_tree(Bplus* bplus, vector<pair<string, string>> key_words)
{
    bplus->Print(key_words);
    cout << endl;
}

void From_txt(Bplus* bplus, vector<pair<string, string>> &key_words)
{
    ifstream in;
    string line;
    in.open("text2.txt");
    if (in.is_open())
    {
        while (getline(in, line))
        {
            string tmp = "";
            int i = 0;

            while (i <= line.size())
            {

                if ((line[i] != ' ' and line[i] != ',' and line[i] != '.'
                    and line[i] != '!' and line[i] != '?' and line[i] != ';' and
                    line[i] != ':' and line[i] != '\n' and line[i] != '\0' and
                    line[i] != '-' and line[i] != '\"'))
                {
                    tmp += line[i];
                }
                else
                {
                    if (tmp.size() == 1 and tmp[0] != '-' and tmp[0] != ' ')
                    {
                        for (int j = 0; j < tmp.size(); j++)
                        {
                            tmp[j] = tolower(tmp[j]);
                        }
                        
                        Add(bplus, tmp, key_words);
                        tmp.clear();
                    }
                    else if (tmp.size() > 1)
                    {
                        for (int j = 0; j < tmp.size(); j++)
                        {
                            tmp[j] = tolower(tmp[j]);
                        }
                        
                        Add(bplus, tmp, key_words);
                        tmp.clear();
                    }

                }
                i++;
            }
        }
    }
    in.close();
}

string STRToSTR(string str, unsigned int len) {

    long long hash_value = 0;
    long long p_pow = 1;

    for (unsigned char c : str) {
        if ((c >= 0xD0 && c <= 0xDF) || (c >= 0xD0 && c <= 0xEF)) {
            int c_value = c - 0xC0;
            hash_value = (hash_value + c_value * p_pow) % (int)(1e9 + 9);
            p_pow = (p_pow * 31) % (int)(1e9 + 9);
        }
    }

    return str;
}
