#include <iostream>
#include <vector>
#include <algorithm>
#include <map>

using namespace std;

struct Node {
    char data;
    int frequency;
    Node* left;
    Node* right;

    Node(char d, int freq) : data(d), frequency(freq), left(nullptr), right(nullptr) {}
    ~Node() {
        delete left;
        delete right;
    }
};

bool compareNodes(const Node* a, const Node* b) {
    return a->frequency > b->frequency;
}

Node* buildHuffmanTree(vector<Node*>& nodes) {
    while (nodes.size() > 1) {
        sort(nodes.begin(), nodes.end(), compareNodes);

        Node* mergedNode = new Node('#', nodes[nodes.size() - 2]->frequency + nodes[nodes.size() - 1]->frequency);
        mergedNode->left = nodes[nodes.size() - 2];
        mergedNode->right = nodes[nodes.size() - 1];

        nodes.pop_back();
        nodes.pop_back();

        nodes.push_back(mergedNode);
    }

    return nodes[0];
}

void generateCodes(Node* root, string code, map<char, string>& codes) {
    if (root == nullptr) {
        return;
    }

    if (root->data != '#') {
        codes[root->data] = code;
    }

    generateCodes(root->left, code + "0", codes);
    generateCodes(root->right, code + "1", codes);
}

void printCodeTable(const map<char, string>& codes) {
    cout << "Табличка кодов:" << endl;
    for (const auto& pair : codes) {
        cout << pair.first << ": " << pair.second << endl;
    }
}

string encodeText(const string& text, const map<char, string>& codes) {
    string encodedText = "";

    for (char c : text) {
        encodedText += codes.at(c);
    }

    return encodedText;
}

string decodeText(Node* root, const string& encodedText) {
    string decodedText = "";

    Node* current = root;

    for (char bit : encodedText) {
        if (bit == '0') {
            current = current->left;
        }
        else {
            current = current->right;
        }

        if (current->data != '#') {
            decodedText += current->data;
            current = root;
        }
    }

    return decodedText;
}

void decToBin(int x) {
    for (int i = (sizeof(int) * 8) - 1; i >= 0; i--) {
        int temp = x << i;
        cout << (temp & 1);
        cout << " ";
    }
    cout << endl;
}

int main() {
    setlocale(LC_ALL, "ru");
    string alphabet = "kemsyarKEMSYAR 123456789[.";
    string text;
    
    srand(time(NULL));
    for (int i = 0; i < 10000; ++i) {
        text += alphabet[rand() % alphabet.size()];
    }
    
    map<char, int> frequencies;
    for (char c : text) {
        frequencies[c]++;
    }
    
    vector<Node*> nodes;
    for (const auto& pair : frequencies) {
        nodes.push_back(new Node(pair.first, pair.second));
    }
    
    Node* root = buildHuffmanTree(nodes);
    
    map<char, string> codes;
    generateCodes(root, "", codes);
    
    printCodeTable(codes);
    
    string encodedText = encodeText(text, codes);
    string decodedText = decodeText(root, encodedText);
    
    cout << "\n Сам текст: " << text << endl;
    cout << "Закодированный текст: " << encodedText << endl;
    cout << "Декодированный текст: " << decodedText << endl;
    
    double encodingPrice = static_cast<double>(encodedText.size()) / text.size();
    cout << "Цена кодированмя: " << encodingPrice << endl;
    
    double compressionRatio = static_cast<double>(text.size()) / encodedText.size();
    cout << "Коэфф. сжатия: " << compressionRatio << endl;
    
    if (text == decodedText) {
        cout << "Оно работает!" << endl;
    }
    else {
        cout << "Эх.. что-то пошло не так(" << endl;
    }
    decToBin(5);
    delete root;
    return 0;
}
