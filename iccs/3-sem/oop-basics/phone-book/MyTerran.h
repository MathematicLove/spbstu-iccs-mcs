#ifndef MYTERRAN_H
#define MYTERRAN_H

#include <QTextStream>
#include <QDate>
#include <QString>
#include <QStringList>
#include "GlobVar.h"

class MyTerran {
private:
    QString TerLaN;
    QString TerFiN;
    QString TerSeN;
    QString TerBirth;
    QString TerAddres;
    QString TerEmail;
    QString TerPhone;
public:
    // Конструктор с параметрами по умолчанию
    MyTerran(QString lastName = "",
             QString firstName = "",
             QString secName = "",
             QString birth = "02.01.1903",
             QString address = "",
             QString email = "",
             QString phone = "");

    // Конструктор, принимающий список строк
    MyTerran(const QStringList &list);

    // Методы доступа, объявленные как const
    QString GetLastName() const;
    QString GetFirstName() const;
    QString GetSecName() const;
    QString GetBirth() const;
    QString GetAddress() const;
    QString GetEmail() const;
    QString GetPhone() const;
    QStringList GetAll() const;

    // Оператор сравнения, объявленный как const
    bool operator==(const MyTerran& p) const;

    // Статический метод создания объекта по списку строк
    static MyTerran FromStringList(const QStringList &fields);

    // Метод преобразования объекта в строку (например, для записи в файл)
    QString ToString() const;

    friend QTextStream& operator<<(QTextStream& s, const MyTerran& p);
};

QTextStream& operator<<(QTextStream& stream, const MyTerran& p);

#endif // MYTERRAN_H
