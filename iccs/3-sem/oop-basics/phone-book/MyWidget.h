#ifndef MYWIDGET_H
#define MYWIDGET_H

#include <QApplication>
#include <QWidget>
#include <QPushButton>
#include <QGridLayout>
#include <QFile>
#include <QTableWidget>
#include <QHeaderView>
#include <QDate>
#include <QLineEdit>
#include <QList>
#include <QTextStream>
#include <algorithm>
#include "MyTerran.h"
#include "AddNew.h"
#include "EditLine.h"
#include "GlobVar.h"

class MyWidget : public QWidget {
    Q_OBJECT
private:
    QGridLayout* grid;
    QTableWidget* table;
    QPushButton* btnAdd;
    QPushButton* btnEdit;
    QPushButton* btnDelete;
    QPushButton* btnLoad;
    QPushButton* btnSave;
    QPushButton* btnExit;
    QLineEdit* LiEdSearch;
    QPushButton* btnSearch;
    QPushButton* btnNext;
    QPushButton* btnPrev;
    QList<MyTerran> terran;
    QList<QTableWidgetItem*> found;
    QList<QTableWidgetItem*>::iterator iterFoun;
    void FillRow(int index);
public:
    MyWidget(QWidget* parent = Q_NULLPTR);
    void AddPerson(const MyTerran &person);
public slots:
    void AddClicked();
    void EditClicked();
    void LoadClicked();
    void SaveClicked();
    void DelClicked();
    void UpdAfterSort(int, Qt::SortOrder order);
    void LEUpdate(const QString& text);
    void SearchClicked();
    void NextClicked();
    void PrevClicked();
};
class SortFunctor {
private:
    int col;
    Qt::SortOrder order;
public:
    SortFunctor(int c, Qt::SortOrder o);
    bool operator()(const MyTerran &p1, const MyTerran &p2) const;
};

#endif // MYWIDGET_H
