// AddNew.cpp

#include "AddNew.h"
#include "GlobVar.h"    // Здесь должны быть определения ::DateMask, ::fslname_regex, ::email_regex, ::phone_regex
#include "MyTerran.h"

#include <QGridLayout>
#include <QLabel>
#include <QLineEdit>
#include <QDateEdit>
#include <QPushButton>
#include <QMessageBox>
#include <QRegularExpression>
#include <QRegularExpressionValidator>
#include <QDate>
#include <QList>

// Конструктор
AddNew::AddNew(QString title, QWidget* parent)
    : QDialog(parent),
    // Инициализируем регулярные выражения через QRegularExpression
    regexName(QRegularExpression(QString::fromUtf8(::fslname_regex))),
    regexEmail(QRegularExpression(QString::fromUtf8(::email_regex))),
    regexPhone(QRegularExpression(QString::fromUtf8(::phone_regex))),
    inpPerson(MyTerran()),  // создаём временный объект по умолчанию
    outPerson(nullptr)
{
    this->setWindowTitle(title);
    this->resize(600, 600);
    this->setFixedSize(600, 600);

    grid = new QGridLayout(this);

    // Создание меток
    lblLN = new QLabel("Last name :", this);
    lblFN = new QLabel("First name :", this);
    lblSN = new QLabel("Second name :", this);
    lblBirth = new QLabel("Date of birth :", this);
    lblAddress = new QLabel("Address :", this);
    lblEmail = new QLabel("E-mail :", this);
    lblPhone = new QLabel("Phone :", this);
    temaplatePhone = new QLabel("+X(XXX)XXX-XX-XX", this);

    // Создание полей ввода с начальными значениями из inpPerson
    LiEdLN = new QLineEdit(inpPerson.GetLastName(), this);
    LiEdFN = new QLineEdit(inpPerson.GetFirstName(), this);
    LiEdSN = new QLineEdit(inpPerson.GetSecName(), this);
    DaEdBirth = new QDateEdit(QDate::fromString(inpPerson.GetBirth(), ::DateMask), this);
    DaEdBirth->setMinimumDate(QDate::fromString("02.01.1903", ::DateMask));
    DaEdBirth->setMaximumDate(QDate::currentDate());
    LiEdAddress = new QLineEdit(inpPerson.GetAddress(), this);
    LiEdEmail = new QLineEdit(inpPerson.GetEmail(), this);
    LiEdPhone = new QLineEdit(inpPerson.GetPhone(), this);

    // Кнопки
    btnOk = new QPushButton("OK", this);
    btnCancel = new QPushButton("Cancel", this);

    // Размещение виджетов на сетке
    grid->addWidget(lblLN, 0, 0, 1, 5);
    grid->addWidget(LiEdLN, 0, 1, 1, 5);

    grid->addWidget(lblFN, 1, 0, 1, 5);
    grid->addWidget(LiEdFN, 1, 1, 1, 5);

    grid->addWidget(lblSN, 2, 0, 1, 5);
    grid->addWidget(LiEdSN, 2, 1, 1, 5);

    grid->addWidget(lblBirth, 3, 0, 1, 5);
    grid->addWidget(DaEdBirth, 3, 1, 1, 5);

    grid->addWidget(lblAddress, 4, 0, 1, 5);
    grid->addWidget(LiEdAddress, 4, 1, 1, 5);

    grid->addWidget(lblEmail, 5, 0, 1, 5);
    grid->addWidget(LiEdEmail, 5, 1, 1, 5);

    grid->addWidget(lblPhone, 6, 0, 1, 5);
    grid->addWidget(LiEdPhone, 6, 1, 1, 5);

    grid->addWidget(temaplatePhone, 7, 1, 1, 5);

    grid->addWidget(btnOk, 8, 4, 1, 1);
    grid->addWidget(btnCancel, 8, 5, 1, 1);

    // Подключение сигналов и слотов
    connect(btnOk, SIGNAL(clicked()), this, SLOT(accept()));
    connect(btnCancel, SIGNAL(clicked()), this, SLOT(reject()));

    // Установка валидаторов для полей ввода
    QRegularExpressionValidator* validatorName = new QRegularExpressionValidator(regexName, this);
    LiEdLN->setValidator(validatorName);
    LiEdFN->setValidator(validatorName);
    LiEdSN->setValidator(validatorName);

    QRegularExpressionValidator* validatorEmail = new QRegularExpressionValidator(regexEmail, this);
    LiEdEmail->setValidator(validatorEmail);

    QRegularExpressionValidator* validatorPhone = new QRegularExpressionValidator(regexPhone, this);
    LiEdPhone->setValidator(validatorPhone);
}

// Деструктор
AddNew::~AddNew() {
    delete outPerson;
}

// Возвращает указатель на созданного объекта MyTerran
MyTerran* AddNew::GetOutputPerson() {
    return outPerson;
}

// Проверка заполненности всех полей
bool AddNew::IsAllFilled() {
    // Создаем список полей QLineEdit
    QList<QLineEdit*> widgets = { LiEdLN, LiEdFN, LiEdSN,
                                  LiEdAddress, LiEdEmail, LiEdPhone };

    bool allFilled = true;
    for (int i = 0; i < widgets.size(); i++) {
        if (widgets[i]->text().isEmpty()) {
            allFilled = false;
            if (i == 0)
                notFilled << "Last name";
            else if (i == 1)
                notFilled << "First name";
            else if (i == 2)
                notFilled << "Second name";
            else if (i == 3)
                notFilled << "Address";
            else if (i == 4)
                notFilled << "E-mail";
            else if (i == 5)
                notFilled << "Phone";
        }
    }
    // Проверка поля даты рождения
    if (DaEdBirth->text() == "02.01.1903") {
        allFilled = false;
        notFilled << "Date of birth";
    }
    return allFilled;
}

// Валидация вводимых данных с использованием регулярных выражений
bool AddNew::ValidateInput() {
    notValidated.clear();
    if (!regexName.match(LiEdLN->text()).hasMatch())
        notValidated << "Last name";
    if (!regexName.match(LiEdFN->text()).hasMatch())
        notValidated << "First name";
    if (!regexName.match(LiEdSN->text()).hasMatch())
        notValidated << "Second name";
    if (!regexEmail.match(LiEdEmail->text()).hasMatch())
        notValidated << "E-mail";
    if (!regexPhone.match(LiEdPhone->text()).hasMatch())
        notValidated << "Phone";
    return notValidated.isEmpty();
}

// Вывод предупреждения с перечислением полей, где допущены ошибки
void AddNew::ThrowWarning(QString msg, QStringList& list) {
    if (!list.isEmpty()) {
        msg += list.join(", ");
        QMessageBox msgBox(this);
        msgBox.setWindowTitle("Warning");
        msgBox.setIcon(QMessageBox::Warning);
        msgBox.setText(msg);
        msgBox.exec();
        list.clear();
    }
}

// Переопределение слота accept() для проверки заполненности и валидации
void AddNew::accept() {
    notFilled.clear();
    notValidated.clear();

    if (IsAllFilled()) {
        if (ValidateInput()) {
            // Создаем новый объект MyTerran с данными из полей ввода
            outPerson = new MyTerran(LiEdLN->text(),
                                     LiEdFN->text(),
                                     LiEdSN->text(),
                                     DaEdBirth->text(),
                                     LiEdAddress->text(),
                                     LiEdEmail->text(),
                                     LiEdPhone->text());
            QDialog::accept();
            this->close();
        } else {
            ThrowWarning("The strings are filled incorrectly:\n", notValidated);
        }
    } else {
        ThrowWarning("Please fill the following:\n", notFilled);
    }
}
