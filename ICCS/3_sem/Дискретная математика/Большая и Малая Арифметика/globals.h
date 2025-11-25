#pragma once
#include <iostream>
#include <vector>
#include <string>
#include <math.h>
#include <regex>
using namespace std;

const regex RXChar("^((-?)([bcdefgh])([abcdefgh]){0,7})|(a)$"); // Num(letters) 
const regex RXOp("^[\\+\\-\\*\\/]$"); // Symbols of operation
const regex RXQuit("^Y|N|y|n$"); // Yes or No 

void BinGray();
bool GetNum(string& n);
bool GetOp(char& o);
bool GetQuit(char& quit);
void Delay(int i);
void STRtovec(vector<char>& fm, vector<char>& sm, string inpF, string inpS);
void Neg(vector<char>& vec); //Функция смены знака (+ -> - )
void PrintVec(vector<char>& vec);
int CountZero(vector<char> f);