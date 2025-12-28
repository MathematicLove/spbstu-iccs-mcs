#include <iostream>
#include <string>

using namespace std;

std::string runLengthEncode(const std::string& input) {
    std::string encoded;
    int count = 1;

    for (int i = 1; i <= input.length(); ++i) {
        if (i == input.length() || input[i] != input[i - 1]) {
            encoded += input[i - 1] + std::to_string(count);
            count = 1;
        }
        else {
            ++count;
        }
    }

    return encoded;
}


std::string runLengthDecode(const std::string& input) {
    std::string decoded;

    for (int i = 0; i < input.length(); i += 2) {
        char character = input[i];
        int count = input[i + 1] - '0';

        for (int j = 0; j < count; ++j) {
            decoded += character;
        }
    }

    return decoded;
}

int main() {
    setlocale(LC_ALL, "ru");
    string alphabet = "9.";
    string text;
    
    srand(time(NULL));
    for (int i = 0; i < 10000; ++i) {
        text += alphabet[rand() % alphabet.size()];
    }
    
    std::string originalText = text;
    std::string encodedText = runLengthEncode(originalText);

    std::cout << "Original Text: " << originalText << std::endl;
    cout <<" _________________________________________________________________________________ " << endl;
    std::cout << "Encoded Text: " << encodedText << std::endl;
    cout <<" _________________________________________________________________________________ " << endl;
    std::string decodedText = runLengthDecode(encodedText);
    cout <<" _________________________________________________________________________________ " << endl;
    std::cout << "Decoded Text: " << decodedText << std::endl;
    cout <<" _________________________________________________________________________________ " << endl;
    float ksjt = decodedText.size()/encodedText.size();
    cout << "Коэффициент сжатия: " <<  ksjt << endl;

    return 0;
}

