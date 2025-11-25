#include "MyTerran.h"

MyTerran::MyTerran(QString lastName, QString firstName, QString secName,
                   QString birth, QString address, QString email, QString phone)
    : TerLaN(lastName),
    TerFiN(firstName),
    TerSeN(secName),
    TerBirth(birth),
    TerAddres(address),
    TerEmail(email),
    TerPhone(phone)
{}

MyTerran::MyTerran(const QStringList &list)
    : TerLaN(list.value(0)),
    TerFiN(list.value(1)),
    TerSeN(list.value(2)),
    TerBirth(list.value(3)),
    TerAddres(list.value(4)),
    TerEmail(list.value(5)),
    TerPhone(list.value(6))
{}

QString MyTerran::GetLastName() const {
    return TerLaN;
}

QString MyTerran::GetFirstName() const {
    return TerFiN;
}

QString MyTerran::GetSecName() const {
    return TerSeN;
}

QString MyTerran::GetBirth() const {
    return TerBirth;
}

QString MyTerran::GetAddress() const {
    return TerAddres;
}

QString MyTerran::GetEmail() const {
    return TerEmail;
}

QString MyTerran::GetPhone() const {
    return TerPhone;
}

QStringList MyTerran::GetAll() const {
    return QStringList() << TerLaN
                         << TerFiN
                         << TerSeN
                         << TerBirth
                         << TerAddres
                         << TerEmail
                         << TerPhone;
}

bool MyTerran::operator==(const MyTerran& p) const {
    return (TerLaN == p.TerLaN) && (TerFiN == p.TerFiN) &&
           (TerSeN == p.TerSeN) && (TerBirth == p.TerBirth) &&
           (TerAddres == p.TerAddres) && (TerEmail == p.TerEmail) &&
           (TerPhone == p.TerPhone);
}

QString MyTerran::ToString() const {
    return GetAll().join(",");
}

QTextStream& operator<<(QTextStream& stream, const MyTerran& p) {
    stream << p.TerLaN << ","
           << p.TerFiN << ","
           << p.TerSeN << ","
           << p.TerBirth << ","
           << p.TerAddres << ","
           << p.TerEmail << ","
           << p.TerPhone;
    return stream;
}

// Реализация статического метода FromStringList:
MyTerran MyTerran::FromStringList(const QStringList &fields) {
    // Если элементов меньше 7, можно вернуть объект по умолчанию
    if (fields.size() < 7)
        return MyTerran();
    return MyTerran(fields[0], fields[1], fields[2],
                    fields[3], fields[4], fields[5], fields[6]);
}
