#include "mainwindow.h"

#include <QApplication>
#include <QtWidgets/QApplication>
#include "MyWidget.h"

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    MyWidget mw;
    mw.showFullScreen();
    return a.exec();
}
