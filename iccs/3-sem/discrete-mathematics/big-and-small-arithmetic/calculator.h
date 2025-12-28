#pragma once
#include "globals.h"
#define TAB_SZ 8
static char TableAdd[TAB_SZ][TAB_SZ] = {    // Creating Massiv TABLE 8x8 like matrix 8x8 then R^-1
	//  a    b    c    d    e    f    g    h
	  {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'},
	  {'b', 'e', 'g', 'f', 'c', 'a', 'h', 'd'},
	  {'c', 'g', 'd', 'b', 'h', 'e', 'f', 'a'},
	  {'d', 'f', 'b', 'g', 'a', 'h', 'e', 'c'},
	  {'e', 'c', 'h', 'a', 'g', 'b', 'd', 'f'},
	  {'f', 'a', 'e', 'h', 'b', 'd', 'c', 'g'},
	  {'g', 'h', 'f', 'e', 'd', 'c', 'a', 'b'},
	  {'h', 'd', 'a', 'c', 'f', 'g', 'b', 'e'}
};
static char TableAddSdvig[TAB_SZ][TAB_SZ] = {
	//  a    b    c    d    e    f    g    h
	  {'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'},
	  {'a', 'a', 'a', 'a', 'a', 'b', 'a', 'a'},
	  {'a', 'a', 'a', 'b', 'a', 'b', 'a', 'b'},
	  {'a', 'a', 'b', 'b', 'b', 'b', 'b', 'b'},
	  {'a', 'a', 'a', 'b', 'a', 'b', 'a', 'a'},
	  {'a', 'b', 'b', 'b', 'b', 'b', 'b', 'b'},
	  {'a', 'a', 'a', 'b', 'a', 'b', 'b', 'b'},
	  {'a', 'a', 'b', 'b', 'a', 'b', 'b', 'b'}
};
static char TableMulti[TAB_SZ][TAB_SZ] = {
	//  a    b    c    d    e    f    g    h
	  {'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'},
	  {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'},
	  {'a', 'c', 'b', 'e', 'd', 'h', 'g', 'f'},
	  {'a', 'd', 'e', 'g', 'g', 'e', 'a', 'd'},
	  {'a', 'e', 'd', 'g', 'g', 'd', 'a', 'e'},
	  {'a', 'f', 'h', 'e', 'd', 'b', 'g', 'c'},
	  {'a', 'g', 'g', 'a', 'a', 'g', 'a', 'g'},
	  {'a', 'h', 'f', 'd', 'e', 'c', 'g', 'b'}
};
static char TableMultiSvig[TAB_SZ][TAB_SZ] = { // sdvigaet po umnojeniu - zapolnenie null = a
	{'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'},
	{'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'},
	{'a', 'a', 'b', 'e', 'a', 'e', 'b', 'b'},
	{'a', 'a', 'e', 'g', 'b', 'h', 'c', 'c'},
	{'a', 'a', 'a', 'b', 'a', 'b', 'b', 'b'},
	{'a', 'a', 'e', 'h', 'b', 'd', 'c', 'g'},
	{'a', 'a', 'b', 'c', 'b', 'c', 'e', 'e'},
	{'a', 'a', 'b', 'c', 'b', 'g', 'e', 'c'},
};
void PrintTables();
void Show2Tables(char tbl1[][8], char tbl2[][8]);
int cmp(char a, char b);

bool cmpVec(vector<char>& f, vector<char>& s);
void VecSwap(vector<char>& f, vector<char>& s, bool& b);
void VecSwap(vector<char>& f, vector<char>& s);
char FindSub(char a, char b);

void Plus(vector<char> f, vector<char> s, vector<char>& rez, bool pechat);
void Plus(bool& overf, vector<char> f, vector<char> s, vector<char>& rez);
void Sub(vector<char> f, vector<char> s, vector<char>& rez, bool pechat);
void Mult(vector<char>& f, vector<char>& s, vector<char>& rez, bool pechat);
void Div(vector<char>& f, vector<char>& s, vector<char>& rez, vector<char>& ost, bool pechat);
void DoPlus(string a, string b, vector<char>& f, vector<char>& s, vector<char>& rez);
void DoSub(string a, string b, vector<char>& f, vector<char>& s, vector<char>& rez);
void DoMult(string a, string b, vector<char>& f, vector<char>& s, vector<char>& rez);
void DoDiv(string a, string b, vector<char>& f, vector<char>& s, vector<char>& rez, vector<char>& ost);