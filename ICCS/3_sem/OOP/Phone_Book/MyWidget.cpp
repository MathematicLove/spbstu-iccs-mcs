// MyWidget.cpp

#include "MyWidget.h"
#include "AddNew.h"
#include "EditLine.h"
#include "MyTerran.h"
#include "GlobVar.h"

#include <QGridLayout>
#include <QTableWidget>
#include <QPushButton>
#include <QLineEdit>
#include <QHeaderView>
#include <QFile>
#include <QTextStream>
#include <QDate>
#include <QApplication>
#include <algorithm>

// Конструктор
MyWidget::MyWidget(QWidget* parent) : QWidget(parent)
{
    this->setWindowTitle("--Phone_Book--");

    grid = new QGridLayout(this);
    table = new QTableWidget(this);
    btnAdd = new QPushButton("Add", this);
    btnEdit = new QPushButton("Edit", this);
    btnDelete = new QPushButton("Delete", this);
    btnLoad = new QPushButton("Load from file", this);
    btnSave = new QPushButton("Save to file", this);
    btnExit = new QPushButton("Exit", this);

    // Размещаем виджеты на сетке
    grid->addWidget(table, 0, 0, 20, 10);
    grid->addWidget(btnAdd, 0, 21, 1, 1);
    grid->addWidget(btnEdit, 1, 21, 1, 1);
    grid->addWidget(btnDelete, 2, 21, 1, 1);
    grid->addWidget(btnLoad, 3, 21, 1, 1);
    grid->addWidget(btnSave, 4, 21, 1, 1);
    grid->addWidget(btnExit, 21, 21, 1, 1);

    table->setSortingEnabled(true);
    table->setEditTriggers(QAbstractItemView::NoEditTriggers);
    table->setSelectionBehavior(QAbstractItemView::SelectRows);
    table->setSelectionMode(QAbstractItemView::SingleSelection);
    table->setColumnCount(ColumnCount);
    table->setHorizontalHeaderLabels(QStringList() << "Last name"
                                                   << "First name"
                                                   << "Second name"
                                                   << "Date of birth"
                                                   << "Address"
                                                   << "E-mail"
                                                   << "Phone");
    table->setColumnWidth(0, 165);
    table->setColumnWidth(1, 165);
    table->setColumnWidth(2, 165);
    table->setColumnWidth(3, 165);
    table->setColumnWidth(4, 250);
    table->setColumnWidth(5, 250);
    table->setColumnWidth(6, 250);

    connect(table->horizontalHeader(),
            SIGNAL(sortIndicatorChanged(int, Qt::SortOrder)),
            this,
            SLOT(UpdAfterSort(int, Qt::SortOrder)));
    connect(btnAdd, SIGNAL(clicked()),
            this,
            SLOT(AddClicked()));
    connect(btnEdit, SIGNAL(clicked()),
            this,
            SLOT(EditClicked()));
    connect(btnDelete, SIGNAL(clicked()),
            this,
            SLOT(DelClicked()));
    connect(btnLoad, SIGNAL(clicked()),
            this,
            SLOT(LoadClicked()));
    connect(btnSave, SIGNAL(clicked()),
            this,
            SLOT(SaveClicked()));
    connect(btnExit, SIGNAL(clicked()),
            qApp,
            SLOT(quit()));

    LiEdSearch = new QLineEdit(this);
    btnSearch = new QPushButton("Search", this);
    btnNext = new QPushButton("Next", this);
    btnPrev = new QPushButton("Prev", this);

    grid->addWidget(LiEdSearch, 20, 0, 3, 1);
    grid->addWidget(btnSearch, 21, 1, 1, 1);
    grid->addWidget(btnNext, 21, 2, 1, 1);
    grid->addWidget(btnPrev, 21, 3, 1, 1);

    LiEdSearch->setMaximumWidth(200);
    btnSearch->setEnabled(false);
    btnNext->setEnabled(false);
    btnPrev->setEnabled(false);

    connect(LiEdSearch, SIGNAL(textChanged(const QString&)),
            this, SLOT(LEUpdate(const QString&)));
    connect(btnSearch, SIGNAL(clicked()),
            this, SLOT(SearchClicked()));
    connect(btnNext, SIGNAL(clicked()),
            this, SLOT(NextClicked()));
    connect(btnPrev, SIGNAL(clicked()),
            this, SLOT(PrevClicked()));
}

// Обновление строки таблицы
void MyWidget::FillRow(int index)
{
    for (int i = 0; i < ColumnCount; i++) {
        QTableWidgetItem* item = table->item(index, i);
        if (item)
            delete item;
        item = new QTableWidgetItem(terran[index].GetAll()[i]);
        table->setItem(index, i, item);
    }
}

// Добавление новой персоны (принимаем объект по константной ссылке)
void MyWidget::AddPerson(const MyTerran &person)
{
    table->setSortingEnabled(false);
    terran.append(person);
    table->insertRow(terran.size() - 1);
    FillRow(terran.size() - 1);
    table->setSortingEnabled(true);
}

// Обработчик нажатия кнопки Add
void MyWidget::AddClicked()
{
    table->setSortingEnabled(false);
    AddNew* add = new AddNew("Add", this);
    if (add->exec() == QDialog::Accepted) {
        if (add->GetOutputPerson() != nullptr) {
            AddPerson(*add->GetOutputPerson());
        }
    }
    delete add;
    int column = table->horizontalHeader()->sortIndicatorSection();
    Qt::SortOrder ord = table->horizontalHeader()->sortIndicatorOrder();
    if (column != 7) {
        table->sortItems(column, Qt::DescendingOrder);
        table->sortItems(column, Qt::AscendingOrder);
        table->sortItems(column, ord);
    }
}

// Обработчик нажатия кнопки Edit
void MyWidget::EditClicked()
{
    table->setSortingEnabled(false);
    QItemSelectionModel* select = table->selectionModel();
    if (select->hasSelection()) {
        int row = select->selectedRows().first().row();
        EditLine* edit = new EditLine("Edit", &terran[row], this);
        if (edit->exec() == QDialog::Accepted) {
            if (edit->GetOutputPerson() != nullptr) {
                terran[row] = *edit->GetOutputPerson();
                FillRow(row);
            }
        }
        delete edit;
    }
    table->setSortingEnabled(true);
    int column = table->horizontalHeader()->sortIndicatorSection();
    Qt::SortOrder ord = table->horizontalHeader()->sortIndicatorOrder();
    if (column != 7) {
        table->sortItems(column, Qt::DescendingOrder);
        table->sortItems(column, Qt::AscendingOrder);
        table->sortItems(column, ord);
    }
}

// Обработчик загрузки из файла
void MyWidget::LoadClicked()
{
    QFile file("input.txt");
    if (file.open(QIODevice::ReadOnly)) {
        QTextStream inp(&file);
        while (!inp.atEnd()) {
            QString line = inp.readLine();
            QStringList fields = line.split(",");
            // Используем статический метод FromStringList из MyTerran
            MyTerran person = MyTerran::FromStringList(fields);
            AddPerson(person);
        }
        file.close();
    }
    int column = table->horizontalHeader()->sortIndicatorSection();
    Qt::SortOrder ord = table->horizontalHeader()->sortIndicatorOrder();
    if (column != 7) {
        table->sortItems(column, Qt::DescendingOrder);
        table->sortItems(column, Qt::AscendingOrder);
        table->sortItems(column, ord);
    }
}

// Обработчик сохранения в файл
void MyWidget::SaveClicked()
{
    if (!terran.isEmpty()) {
        QFile file("Data.txt");
        if (file.open(QIODevice::Truncate | QIODevice::ReadWrite)) {
            QTextStream out(&file);
            for (int i = 0; i < terran.size(); i++) {
                out << terran[i].ToString() << "\n";
            }
            file.close();
        }
    }
}

// Обработчик удаления выбранной строки
void MyWidget::DelClicked()
{
    QItemSelectionModel* select = table->selectionModel();
    if (select->hasSelection()) {
        int row = select->selectedRows().first().row();
        terran.removeAt(row);
        table->removeRow(row);
    }
}

// Слот обновления сортировки после изменения столбца
void MyWidget::UpdAfterSort(int col, Qt::SortOrder order)
{
    std::sort(terran.begin(), terran.end(), SortFunctor(col, order));
}

// Обработчик изменения текста поиска
void MyWidget::LEUpdate(const QString& text)
{
    if (!text.isEmpty()) {
        btnSearch->setEnabled(true);
    } else {
        btnSearch->setEnabled(false);
        btnNext->setEnabled(false);
        btnPrev->setEnabled(false);
    }
}

// Обработчик нажатия кнопки Search
void MyWidget::SearchClicked()
{
    found = table->findItems(LiEdSearch->text(), Qt::MatchExactly);
    iterFoun = found.begin();
    if (iterFoun != found.end()) {
        table->selectRow((*iterFoun)->row());
        if ((iterFoun + 1) != found.end()) {
            btnNext->setEnabled(true);
        }
    }
}

// Обработчик нажатия кнопки Next
void MyWidget::NextClicked()
{
    ++iterFoun;
    if (iterFoun != found.end()) {
        table->selectRow((*iterFoun)->row());
        btnPrev->setEnabled(true);
        if ((iterFoun + 1) != found.end()) {
            btnNext->setEnabled(true);
        } else {
            btnNext->setEnabled(false);
        }
    }
}

// Обработчик нажатия кнопки Prev
void MyWidget::PrevClicked()
{
    if (iterFoun != found.begin()) {
        --iterFoun;
        table->selectRow((*iterFoun)->row());
        btnNext->setEnabled(true);
        if (iterFoun != found.begin()) {
            btnPrev->setEnabled(true);
        } else {
            btnPrev->setEnabled(false);
        }
    }
}

// Реализация сортировочного функционального объекта
SortFunctor::SortFunctor(int c, Qt::SortOrder o) : col(c), order(o)
{
}

bool SortFunctor::operator()(const MyTerran &p1, const MyTerran &p2) const
{
    if (col != 3) { // Если не столбец с датой рождения
        if (order == Qt::AscendingOrder) {
            return p1.GetAll()[col] < p2.GetAll()[col];
        } else {
            return p1.GetAll()[col] > p2.GetAll()[col];
        }
    } else {
        QDate d1 = QDate::fromString(p1.GetBirth(), ::DateMask);
        QDate d2 = QDate::fromString(p2.GetBirth(), ::DateMask);
        if (order == Qt::AscendingOrder) {
            return d1 < d2;
        } else {
            return d1 > d2;
        }
    }
}
