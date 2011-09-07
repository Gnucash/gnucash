#include "dashboard.hpp"
#include "ui_dashboard.h"

Dashboard::Dashboard(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::Dashboard)
{
    ui->setupUi(this);

    setCentralWidget(ui->firstPersonOverview);

    this->tabifyDockWidget(ui->txnEntryBasic, ui->txnEntrySplit);
    ui->txnEntryBasic->raise();
}

Dashboard::~Dashboard()
{
    delete ui;
}
