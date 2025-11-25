#include "mainwindow.h"
#include "./ui_mainwindow.h"
#include <QAction>
#include <QMenuBar>
#include <QSizePolicy>
#include <QWidget>
#include <QtGlobal> // Для QWIDGETSIZE_MAX

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    this->resize(800, 600);
    this->setMinimumSize(800, 600);
    this->setMaximumSize(QWIDGETSIZE_MAX, QWIDGETSIZE_MAX);
    this->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

    // Добавляем действие в меню для переключения режима полного экрана
    QAction *toggleFullScreen = new QAction("Toggle Full Screen", this);
    connect(toggleFullScreen, &QAction::triggered, this, [this]() {
        if (this->isFullScreen())
            this->showNormal();
        else
            this->showFullScreen();
    });
    menuBar()->addAction(toggleFullScreen);
}

MainWindow::~MainWindow()
{
    delete ui;
}
