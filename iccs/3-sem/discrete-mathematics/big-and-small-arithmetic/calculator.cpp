#include "Calculator.h"
void PrintTables() {
	cout << "Таблица сложения:                          Таблица умножения:" << endl;
	Show2Tables(TableAdd, TableMulti);
	cout << "Переполнение по сложению:                   Переполнение по умножению:" << endl;
	Show2Tables(TableAddSdvig, TableMultiSvig);
}
void Show2Tables(char tbl1[][8], char tbl2[][8]) {
	cout << "    ";
	for (char i = 'a'; i < 'i'; i++) {
		cout << i << " ";
		Delay(800000);
	}
	cout << "                            ";
	for (char i = 'a'; i < 'i'; i++) {
		cout << i << " ";
		Delay(800000);
	}

	cout << endl << "   ";
	for (int i = 0; i < 16; i++) {
		cout << "-";
		Delay(400000);
	}
	cout << "                            ";
	for (int i = 0; i < 16; i++) {
		cout << "-";
		Delay(400000);
	}

	cout << endl;
	for (int i = 0; i < 8; i++) {
		char tmp = i + 'a';
		cout << tmp << " | ";
		for (int j = 0; j < 8; j++) {
			cout << tbl1[i][j] << " ";
			Delay(800000);
		}
		cout << "                        " << tmp << " | ";
		for (int j = 0; j < 8; j++) {
			cout << tbl2[i][j] << " ";
			Delay(800000);
		}
		cout << endl;
	}
	cout << endl;
}


int cmp(char a, char b) {
	char i = 'a'; // задаем 0
	char ifa = 'a', ifb = 'a'; // задание значение если А = 0 и если В = 0
	int countA = 0, countB = 0; // счет А и В
	while (ifa != a) { // пока (если а != 0)
		if (TableAdd[i - 0x61][1] != a) { // [0x97 - 0x61][1] != 0;  [36][1] != 0;
			ifa = TableAdd[i - 0x61][1]; // ifa принимает значение по массиву букв [i-0x61][1]
			countA += 1; // счет А
			i = TableAdd[i - 0x61][1]; // i принимает символ по значению [0x97-0x61][1] из массива Add
		}
		else {
			ifa = TableAdd[i - 0x61][1]; // ifa = [0x97 - 0x61][1];
			countA += 1; // счет А
		}
	}
	i = 'a'; // i = 0x97 по дефолту 
	while (ifb != b) {  // так же для b
		if (TableAdd[i - 0x61][1] != b) {
			ifb = TableAdd[i - 0x61][1];
			countB += 1;
			i = TableAdd[i - 0x61][1];
		}
		else {
			ifb = TableAdd[i - 0x61][1];
			countB += 1;
		}
	}
	if (countA > countB)
		return 1;
	else if (countA == countB)
		return 0;
	else if (countA < countB)
		return -1;
}
bool cmpVec(vector<char>& f, vector<char>& s) { // сравнение векторов f & s
	for (int i = 0; i < TAB_SZ; i++) {
		if (f[i] != 'a' || s[i] != 'a') {
			if (cmp(f[i], s[i]) == 1)
				break;
			else if (cmp(f[i], s[i]) == -1) {
				return false;  // s > f !
				break;
			}
		}
	}
	return true; // f >= s
};

void VecSwap(vector<char>& f, vector<char>& s, bool& b) { // перестановка векторов f & s 
	bool flagC = true; // f > s по умолчанию
	for (int i = 0; i < TAB_SZ; i++) {
		if (f[i] != 'a' || s[i] != 'a') { 
			if (cmp(f[i], s[i]) == 1)
				break;
			else if (cmp(f[i], s[i]) == -1) {
				flagC = false;  // s > f !
				break;
			}
		}
	}
	if (!flagC) { // flagC false 
		b = true;
		vector<char> tmp = f;
		f = s;
		s = tmp;
	}
}
void VecSwap(vector<char>& f, vector<char>& s) {
	bool flagC = true; // f > s по умолчанию
	for (int i = 0; i < TAB_SZ; i++) {
		if (f[i] != 'a' || s[i] != 'a') {
			if (cmp(f[i], s[i]) == 1)
				break;
			else if (cmp(f[i], s[i]) == -1) {
				flagC = false;  // s > f !
				break;
			}
		}
	}
	if (!flagC) {
		vector<char> tmp = f;
		f = s;
		s = tmp;
	}
}

char FindSub(char a, char b) {
	for (int i = 0; i < TAB_SZ; i++) {
		if (TableAdd[b - 0x61][i] == a)
			return static_cast<char>(i + 0x61); // a + 0x61 -> 0x97 - 0x61
	}
};

void Plus(vector<char> f, vector<char> s, vector<char>& rez, bool pechat) {
	bool flagP = true; char sdvig = 'a'; // сдвиг на 'a' - 0x97
	VecSwap(f, s); // замена f,s
	int c = CountZero(f); // счет 0 по f vector
	if (c != 0) c--; 
	for (int i = TAB_SZ - 1; i >= c; i--) {
		rez[i] = TableAdd[f[i] - 0x61][s[i] - 0x61]; 
		rez[i] = TableAdd[sdvig - 0x61][rez[i] - 0x61];
		if (TableAdd[f[i] - 0x61][s[i] - 0x61] == 'f' && sdvig == 'b')
			sdvig = 'b';
		else if (s[i] != 'a' && f[i] != 'a')
			sdvig = TableAddSdvig[f[i] - 0x61][s[i] - 0x61];
		else if (f[i] != 'a')
			sdvig = TableAddSdvig[f[i] - 0x61][sdvig - 0x61];
		else if (s[i] != 0)
			sdvig = TableAddSdvig[sdvig - 0x61][s[i] - 0x61];
		else
			sdvig = 'a';
		if ((TableAddSdvig[f[i] - 0x61][s[i] - 0x61] || TableAddSdvig[sdvig - 0x61][TableAdd[f[i] - 0x61][s[i] - 0x61] - 0x61]) && i == 0) {
			cout << "Результат не находится в интервале [-ffffffff; ffffffff]" << endl;
			flagP = false;
		}
	}
	if (flagP && pechat) PrintVec(rez);
}
void Plus(bool& overf, vector<char> f, vector<char> s, vector<char>& rez) {
	char sdvig = 'a'; VecSwap(f, s);
	int c = CountZero(f);
	if (c != 0) c--;
	for (int i = TAB_SZ - 1; i >= c; i--) {
		rez[i] = TableAdd[f[i] - 0x61][s[i] - 0x61];
		rez[i] = TableAdd[sdvig - 0x61][rez[i] - 0x61];
		if (TableAdd[f[i] - 0x61][s[i] - 0x61] == 'f' && sdvig == 'b')
			sdvig = 'b';
		else if (s[i] != 'a' && f[i] != 'a')
			sdvig = TableAddSdvig[f[i] - 0x61][s[i] - 0x61];
		else if (f[i] != 'a')
			sdvig = TableAddSdvig[f[i] - 0x61][sdvig - 0x61];
		else if (s[i] != 'a')
			sdvig = TableAddSdvig[sdvig - 0x61][s[i] - 0x61];
		else
			sdvig = 'a';

		if ((TableAddSdvig[f[i] - 0x61][s[i] - 0x61] || TableAddSdvig[sdvig - 0x61][TableAdd[f[i] - 0x61][s[i] - 0x61] - 0x61]) && i == 0) {
			overf = true;
		}
	}
}
void Sub(vector<char> f, vector<char> s, vector<char>& rez, bool pechat) {
	char sdvig = 'a'; bool flSwap = false;
	VecSwap(f, s, flSwap); //Если число s > f, тогда они меняются местами
	for (int i = TAB_SZ - 1; i >= 0; i--) {
		f[i] = FindSub(f[i], sdvig);
		rez[i] = FindSub(f[i], s[i]);
		if (f[i] == 'f' && sdvig == 'b')
			sdvig = 'b';
		else
			sdvig = TableAddSdvig[s[i] - 0x61][rez[i] - 0x61];
	}
	if (!flSwap && pechat) {
		PrintVec(rez);
	}
	else if (pechat) {
		for (int i = 0; i < TAB_SZ; i++) {
			if (rez[i] != 0) {
				rez[i] = rez[i] * (-1);
				break;
			}
		}
		PrintVec(rez);
	}
}
void Mult(vector<char>& f, vector<char>& s, vector<char>& rez, bool pechat) { //функция перегруж. умножения
	bool flagP = false; int k = 0; char sdvig = 'a';
	vector<char> tmpR;
	VecSwap(f, s); // Switching the vectors f and s
	int c = CountZero(s); 
	for (int i = TAB_SZ - 1; i >= c; i--) {
		tmpR = { 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a' };
		k = i;
		for (int j = TAB_SZ - 1; j >= 0; j--) {
			tmpR[k] = TableMulti[f[j] - 0x61][s[i] - 0x61]; //from TAB.Mult - 0x61 masiv 2x2
			tmpR[k] = TableAdd[tmpR[k] - 0x61][sdvig - 0x61]; //from TAB.Add - 0x61 making "sdvig"

			char tmpSdvig = sdvig;  // changing tmpSdvig to new sdvig
			sdvig = TableMultiSvig[f[j] - 0x61][s[i] - 0x61]; // new sdvig initiation 
			sdvig = TableAdd[sdvig - 0x61][TableAddSdvig[TableMulti[f[j] - 0x61][s[i] - 0x61] - 0x61][tmpSdvig - 0x61] - 0x61];
			if (j == 0 && sdvig != 'a')
				flagP = true;
			if (k != 0)
				k--;
			else
				break;
		}
		Plus(flagP, rez, tmpR, rez);
	}
	if (!flagP && pechat) {
		PrintVec(rez);
	}
	else if (flagP) {
		cout << "Результат не в интервале [-ffffffff; ffffffff]" << endl;
	}
}
void Div(vector<char>& f, vector<char>& s, vector<char>& rez, vector<char>& ost, bool pechat) {
	bool flagOver = false;
	vector<char> edinich = { 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'b' }, tmpR = { 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a' };
	ost = { 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a' }; rez = { 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a' };
	vector<char> chetchik = { 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a' }, tmpChet;
	if (cmpVec(f, s)) { //f >= s         f|_s
		if (s != edinich) {
			while (cmpVec(f, chetchik) != false) {
				tmpChet = chetchik;    // Делим 54 на 11.  Считаем склолько раз прибавим 11 чтобы достичь 54 или рядом (0+11+11 = 55)  
				Plus(flagOver, s, chetchik, chetchik);		// 0 + 11 + 11 в chetchik
				if (flagOver)
					break;
				Plus(tmpR, edinich, tmpR, false);			// Количество сложений в tmpR (+1 от 0 по нашему правилу + )
			};
			if (!flagOver) { //Переполнения не было
				Sub(tmpR, edinich, rez, false);
				Sub(f, tmpChet, ost, false);
			}
			else { // Переполнение было
				Sub(f, tmpChet, ost, false); //Chetchick result tmpR
				rez = tmpR;
			}
			if (pechat) {
				cout << "Результат деления : ";
				PrintVec(rez);
				cout << "Остаток : ";
				PrintVec(ost);
			}
		}
		else {
			if (pechat) {
				cout << "Результат деления : ";
				PrintVec(f);
				cout << "Остаток : " << 0;
			}
			else {
				rez = f;
			}
		}
	}
	else {  // f < s  
		ost = f;
		if (pechat) {
			cout << "Результат деления: a; Остаток:";
			PrintVec(f);
		}
	}
}

void DoPlus(string a1, string b2, vector<char>& f, vector<char>& s, vector<char>& rez) {
	if (a1.size() && b2.size()) {
		if (a1[0] != '-' && b2[0] != '-') {
			STRtovec(f, s, a1, b2);
			Plus(f, s, rez, true);
		}
		else if (a1[0] != '-' && b2[0] == '-') {
			b2.erase(0, 1);
			STRtovec(f, s, a1, b2);
			if (cmpVec(f, s)) {
				Sub(f, s, rez, true);
			}
			else {
				Sub(f, s, rez, false);
				Neg(rez);
				PrintVec(rez);
			}
		}
		else if (a1[0] == '-' && b2[0] != '-') {
			swap(a1, b2);
			b2.erase(0, 1);
			STRtovec(f, s, a1, b2);
			if (cmpVec(f, s)) {
				Sub(f, s, rez, true);
			}
			else {
				Sub(f, s, rez, false);
				Neg(rez);
				PrintVec(rez);
			}
		}
		else if (a1[0] == '-' && b2[0] == '-') {
			a1.erase(0, 1); b2.erase(0, 1);
			STRtovec(f, s, a1, b2);
			Plus(f, s, rez, false);
			Neg(rez);
			PrintVec(rez);
		}
	}
	else
		cout << 0 << endl;
};
void DoSub(string a1, string b2, vector<char>& f, vector<char>& s, vector<char>& rez) {
	if (a1.size() && b2.size()) {
		if (a1[0] != '-' && b2[0] != '-') {
			STRtovec(f, s, a1, b2);
			Sub(f, s, rez, true);
		}
		else if (a1[0] != '-' && b2[0] == '-') {
			b2.erase(0, 1);
			STRtovec(f, s, a1, b2);
			Plus(f, s, rez, true);
		}
		else if (a1[0] == '-' && b2[0] != '-') {
			a1.erase(0, 1);
			STRtovec(f, s, a1, b2);
			bool flag = false;
			Plus(flag, f, s, rez);
			if (!flag) {
				Neg(rez);
				PrintVec(rez);
			}
			else
				cout << "Результат не находится в интервале [-ffffffff; ffffffff]" << endl;
		}
		else if (a1[0] == '-' && b2[0] == '-') {
			a1.erase(0, 1); b2.erase(0, 1);
			STRtovec(f, s, a1, b2);
			if (cmpVec(f, s)) {
				Sub(f, s, rez, false);
				Neg(rez);
				PrintVec(rez);
			}
			else {
				Sub(s, f, rez, true);
			}
		}
	}
	else
		cout << 0 << endl;
};
void DoMult(string a1, string b2, vector<char>& f, vector<char>& s, vector<char>& rez) {
	if (a1[0] != 'a' && b2[0] != 'a') {
		if (a1[0] != '-' && b2[0] != '-') {
			STRtovec(f, s, a1, b2);
			Mult(f, s, rez, true);
		}
		else if (a1[0] == '-' && b2[0] == '-') {
			a1.erase(0, 1); b2.erase(0, 1);
			STRtovec(f, s, a1, b2);
			Mult(f, s, rez, true);
		}
		else if (a1[0] != '-' && b2[0] == '-') {
			b2.erase(0, 1);
			STRtovec(f, s, a1, b2);
			Mult(f, s, rez, false);
			Neg(rez);
			PrintVec(rez);
		}
		else if (a1[0] == '-' && b2[0] != '-') {
			a1.erase(0, 1);
			STRtovec(f, s, a1, b2);
			Mult(f, s, rez, false);
			Neg(rez);
			PrintVec(rez);
		}
	}
	else
		cout << 'a' << endl;
};
void DoDiv(string a1, string b2, vector<char>& f, vector<char>& s, vector<char>& rez, vector<char>& ost) {
	if (a1[0] != 'a' && b2[0] != 'a') {
		if (a1[0] != '-' && b2[0] != '-') {
			STRtovec(f, s, a1, b2);
			Div(f, s, rez, ost, false);
			cout << "Результат деления : ";
			PrintVec(rez);
			cout << "Остаток : ";
			PrintVec(ost);
		}
		else if (a1[0] == '-' && b2[0] == '-') {
			a1.erase(0, 1); b2.erase(0, 1);
			STRtovec(f, s, a1, b2);
			Div(f, s, rez, ost, false);
			cout << "Результат деления : ";
			PrintVec(rez);
			cout << "Остаток : ";
			PrintVec(ost);
		}
		else if (a1[0] != '-' && b2[0] == '-') {
			b2.erase(0, 1);
			STRtovec(f, s, a1, b2);
			Div(f, s, rez, ost, false);
			Neg(rez);
			cout << "Результат деления : ";
			PrintVec(rez);
			cout << "Остаток : ";
			PrintVec(ost);
		}
		else if (a1[0] == '-' && b2[0] != '-') {
			a1.erase(0, 1);
			STRtovec(f, s, a1, b2);
			vector<char> fff = f;
			Div(f, s, rez, ost, false);
			vector<char> edin = { 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'b' };
			Plus(rez, edin, rez, false);
			vector<char> tmp;
			Mult(s, rez, tmp, false);
			Sub(tmp, fff, ost, false);
			Neg(rez);
			cout << "Результат деления : ";
			PrintVec(rez);
			cout << "Остаток : ";
			PrintVec(ost);
		}
	}
	else if (a1[0] == 'a' && b2[0] != 'a') { // 0/b
		cout << 'a' << endl;
	}
	else if (a1[0] != 'a' && b2[0] == 'a') { // a/0
		cout << "Неопределенность a/a;" << endl;
	}
	else if (a1[0] == 'a' && b2[0] == 'a') { // если оба нули
		cout << "[-ffffffff; ffffffff]" << endl;
	}
}