#ifndef ADDNEW_H
#define ADDNEW_H

#include <QGridLayout>
#include <QLabel>
#include <QPushButton>
#include <QString>
#include <QStringList>
#include <QDialog>
#include <QLineEdit>
#include <QRegularExpression>
#include <QRegularExpressionValidator>
#include <QDateEdit>
#include <QMessageBox>
#include "GlobVar.h"
#include "MyTerran.h"

class AddNew : public QDialog {
    Q_OBJECT
private:
    QGridLayout* grid;
    QLabel* lblLN;
    QLabel* lblFN;
    QLabel* lblSN;
    QLabel* lblBirth;
    QLabel* lblAddress;
    QLabel* lblEmail;
    QLabel* lblPhone;
    QLabel* temaplatePhone;
    QLineEdit* LiEdLN;
    QLineEdit* LiEdFN;
    QLineEdit* LiEdSN;
    QLineEdit* LiEdAddress;
    QLineEdit* LiEdEmail;
    QLineEdit* LiEdPhone;
    QDateEdit* DaEdBirth;
    QPushButton* btnOk;
    QPushButton* btnCancel;
    QRegularExpression regexName;
    QRegularExpression regexEmail;
    QRegularExpression regexPhone;
    MyTerran inpPerson;
    MyTerran* outPerson;
    QStringList notFilled;
    QStringList notValidated;
    bool IsAllFilled();
    bool ValidateInput();
    void ThrowWarning(QString msg, QStringList& list);
public:
    AddNew(QString title, QWidget* parent = Q_NULLPTR);
    ~AddNew();
    MyTerran* GetOutputPerson();
public slots:
    void accept() override;
};
#endif // ADDNEW_H
