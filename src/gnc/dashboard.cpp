/*
 * Copyright (C) 2011 Free Software Foundation, Inc
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
 */
/**
 * @file
 * Dashboard for embedding various dock widgets.
 *
 * Dock Widgets:
 * 1) Transfer Funds
 * 2) First Person Overview
 */

#include "dashboard.hpp"
#include "ui_dashboard.h"
#include "engine/gnc-event.h" // for GNC_EVENT_ITEM_ADDED

#include <QtGui>
#include <QAbstractItemModel>
#include <QDate>

namespace gnc
{
Dashboard::Dashboard(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::Dashboard),
    m_eventWrapper(*this, &Dashboard::transactionEvent),
    m_eventWrapperAccount(*this, &Dashboard::accountEvent)
{
    QSettings settings;
    restoreGeometry(settings.value("dashboardGeometry").toByteArray());

    ui->setupUi(this);

    // Generate UI
    setUiWidgets();
    setBasicTxnEntryFormLayout();
    setCentralWidget(ui->dockwFPO);
    setFPO();
    //this->tabifyDockWidget(ui->dockwBasicTxn, ui->dockwSplitTxn);
    //ui->dockwBasicTxn->raise();
    ui->dockwSplitTxn->hide();
    ui->dockwLogViewer->hide();

    /* Set stylesheet, so that some styling to viewlets can be applied */
    /*QFile styleSheetFile(":/qss-default");
    styleSheetFile.open(QFile::ReadOnly);
    QString styleSheetName = QLatin1String(styleSheetFile.readAll());
    this->setStyleSheet(styleSheetName);*/

    restoreState(settings.value("dashboardState").toByteArray());
    bool isBasicTxnDockVisible = settings.value("basic-txn-dockwidget-visible").toBool();
    ui->dockwBasicTxn->setVisible(isBasicTxnDockVisible);

    /*
    // Trying to restore pos and size of dockwidget manually
    QSettings settings;
    QPoint bTxnWdgPos = settings.value("basic-txn-entry-dockwidget-pos").toPoint();
    ui->dockwBasicTxn->move(bTxnWdgPos);
    QSize bTxnWdgSize = settings.value("basic-txn-entry-dockwidget-size").toSize();
    ui->dockwBasicTxn->resize(bTxnWdgSize);
    */

    connect(btnCreateBasicTxn, SIGNAL(clicked()),
            this, SLOT(on_btnCreateBasicTxn_clicked()));
}

Dashboard::~Dashboard()
{
    /*
    QSettings settings;
    settings.setValue("basic-txn-entry-dockwidget-pos", ui->dockwBasicTxn->pos());
    settings.setValue("basic-txn-entry-dockwidget-size", ui->dockwBasicTxn->size());
    */
    delete ui;
}

void
Dashboard::mainWindowCloseEvent()
{
    QSettings settings;
    settings.setValue("dashboardGeometry", saveGeometry());
    settings.setValue("dashboardState", saveState());
    settings.setValue("basic-txn-dockwidget-visible", ui->dockwBasicTxn->isVisible());
    //qDebug()<<"while writing dockwdg visibility is "<<settings.value("basic-txn-dockwidget-visible").toBool();
}

void
Dashboard::showDashboardWidgets()
{
    QSettings settings;
    ui->dockwBasicTxn->setVisible(settings.value("basic-txn-dockwidget-visible").toBool());
    ui->dockwFPO->show();
}

/** Initialise widgets to startup defaults */
void
Dashboard::setUiWidgets()
{
    numer = 0.0;

    lblDescription  = new QLabel(tr("Description:"));
    lblDate         = new QLabel(tr("Date:"));
    lblTransferFrom = new QLabel(tr("Transfer From:"));
    lblTransferTo   = new QLabel(tr("TransferTo:"));
    lblAmount       = new QLabel(tr("Amount:"));
    lblMemo         = new QLabel(tr("Memo:"));
    lblNum          = new QLabel(tr("Num:"));
    comboTransferFrom = new QComboBox();
    comboTransferFrom->addItem(tr("- NA -"));
    comboTransferTo   = new QComboBox();
    comboTransferTo->addItem(tr("- NA -"));
    lineDescription = new QLineEdit();
    lineAmount      = new QLineEdit();
    lineAmount->setText(tr("0.00"));
    lineAmount->setValidator(new QDoubleValidator(0.0, 1e100, 2, this));
    lineMemo        = new QLineEdit();
    lineNum         = new QLineEdit();
    dateTxnDate = new QDateEdit();
    dateVal = QDate::currentDate();
    dateTxnDate->setDate(dateVal);
    btnCreateBasicTxn = new QPushButton(tr("Create Transaction"));
}

/** Layout for data entry. Type: Form Based
 *
 * Set this as default to make this layout as default
 * for all data entry widgets. */
void
Dashboard::setBasicTxnEntryFormLayout()
{
    gridBasicTxnEntry = new QGridLayout(ui->dockcBasicTxn);

    gridBasicTxnEntry->addWidget(lblDescription, 0, 0);
    gridBasicTxnEntry->addWidget(lineDescription, 0, 1);

    gridBasicTxnEntry->addWidget(lblDate, 1, 0);
    gridBasicTxnEntry->addWidget(dateTxnDate, 1, 1);

    gridBasicTxnEntry->addWidget(lblTransferFrom, 2, 0);
    gridBasicTxnEntry->addWidget(comboTransferFrom, 2, 1);

    gridBasicTxnEntry->addWidget(lblTransferTo, 3, 0);
    gridBasicTxnEntry->addWidget(comboTransferTo, 3, 1);

    gridBasicTxnEntry->addWidget(lblAmount, 4, 0);
    gridBasicTxnEntry->addWidget(lineAmount, 4, 1);

    gridBasicTxnEntry->addWidget(lblMemo, 5, 0);
    gridBasicTxnEntry->addWidget(lineMemo, 5, 1);

    gridBasicTxnEntry->addWidget(lblNum, 6, 0);
    gridBasicTxnEntry->addWidget(lineNum, 6, 1);

    gridBasicTxnEntry->addWidget(btnCreateBasicTxn, 7, 1);
}

void
Dashboard::setFPO()
{
    QHBoxLayout *FPOLayout = new QHBoxLayout;
    ui->dockcFPO->setLayout(FPOLayout);
    fpoWidget = new FPO(ui->dockcFPO, FPOLayout);
}

void
Dashboard::clearFields()
{
    lineDescription->clear();
    lineAmount->clear();
    lineMemo->clear();
    lineNum->clear();
}

void
Dashboard::transactionEvent( ::Transaction* trans, QofEventId event_type)
{
    //qDebug() << "Dashboard::transactionEvent, id=" << qofEventToString(event_type);
    switch (event_type)
    {
    case QOF_EVENT_MODIFY:
        fpoWidget->leftViewlet->leftVUpdate();
        fpoWidget->rightViewlet->rightVUpdate();
        fpoWidget->defaultViewlet->defaultVUpdate();
        break;
    case GNC_EVENT_ITEM_REMOVED:
    case GNC_EVENT_ITEM_ADDED:
        // This event is triggered by a split being added (or removed)
        // to a transaction. Ignored because we already reacted upon
        // the MODIFY event.
        break;
    case QOF_EVENT_CREATE:
        // This event is triggered by a newly created transaction, but
        // we reacted on this in the accountEvent handler already.
        break;
    default:
        qDebug() << "Dashboard::transactionEvent, ignored event id=" << qofEventToString(event_type);
        break;
    }
}

void
Dashboard::accountEvent( ::Account* acc, QofEventId event_type)
{
    //qDebug() << "Dashboard::accountEvent, id=" << qofEventToString(event_type);

    switch (event_type)
    {
    case GNC_EVENT_ITEM_REMOVED:
        fpoWidget->leftViewlet->leftVUpdate();
        fpoWidget->rightViewlet->rightVUpdate();
        fpoWidget->defaultViewlet->defaultVUpdate();
        break;
    case GNC_EVENT_ITEM_CHANGED:
    case GNC_EVENT_ITEM_ADDED:
    case QOF_EVENT_MODIFY:
        // These events are also triggered e.g. by a newly added
        // transaction/split in this account. However, we already
        // reacted on this by the above events, so we can ignore them
        // here.
        break;
    default:
        qDebug() << "Dashboard::accountEvent, ignored event id=" << qofEventToString(event_type);
        break;
    }
}


void
Dashboard::loadAccountsTreeComboBox(AccountListModel * const m_accountsListModel)
{
    accountsList = m_accountsListModel;
    comboTransferFrom->setModel(accountsList);
    comboTransferTo->setModel(accountsList);
}

/***** Slots *****/

/** "Create Transaction" button for Basic Transaction Entry dock widget */
void
Dashboard::on_btnCreateBasicTxn_clicked()
{
    if (lineDescription->text().isEmpty())
    {
        QMessageBox::StandardButton warnEmptyDesc;
        warnEmptyDesc = QMessageBox::warning(this, tr("Empty Description"), "You have not set a description. Are you sure you want to create a transaction with no description?", QMessageBox::Yes | QMessageBox::No);
        if (warnEmptyDesc == QMessageBox::No)
            return;
    }

    // Allocate memory and start editing a new transaction
    int index = comboTransferFrom->currentIndex();
    account = accountsList->at(index);
    book = gnc_account_get_book(account);
    transaction = ::xaccMallocTransaction(book);
    ::xaccTransBeginEdit(transaction);

    // Populate transaction details
    dateVal = dateTxnDate->date();
    ::xaccTransSetDate(transaction, dateVal.day(), dateVal.month(),
                       dateVal.year());

    ::xaccTransSetNum(transaction, lineNum->text().toUtf8());
    ::xaccTransSetDescription(transaction, lineDescription->text().toUtf8());

    currency = xaccAccountGetCommodity(account);
    ::xaccTransSetCurrency(transaction, currency);

    denom = ::gnc_commodity_get_fraction(currency);

    // Populate split 1
    // Check whether the account for this split is a placeholder
    qDebug() << ::xaccAccountGetPlaceholder(account);
    // ^^ does not work as expected, returns false for placeholder accounts too. Why?

    split = xaccMallocSplit(book);
    ::xaccTransAppendSplit(transaction, split);
    ::xaccAccountInsertSplit(account, split);

    numer = lineAmount->text().toDouble();
    amount = ::double_to_gnc_numeric(numer, denom,
                                     GNC_HOW_DENOM_REDUCE | GNC_HOW_RND_NEVER);
    ::xaccSplitSetValue(split, amount);
    ::xaccSplitSetAmount(split, amount);

    // Populate split 2
    split2 = ::xaccMallocSplit(book);
    ::xaccTransAppendSplit(transaction, split2);
    int index2 = comboTransferTo->currentIndex();
    account2 = accountsList->at(index2);
    ::xaccAccountInsertSplit(account2, split2);

    amount2 = ::gnc_numeric_neg(amount);
    ::xaccSplitSetValue(split2, amount2);
    ::xaccSplitSetAmount(split2, amount2);

    // Finally commit the transaction to storage backend.
    ::xaccTransCommitEdit(transaction);

    statusBar()->showMessage(tr("Transaction has been created"), 2000);
    clearFields();
}

void
Dashboard::on_dockwBasicTxn_visibilityChanged(bool visible)
{
    ((MainWindow *)parentWidget()->parentWidget()->parentWidget()->parentWidget())->dockWidgetsVisibilityChanged(0, visible);
}

void
Dashboard::transferFundsWidgetButtonToggled(bool checked)
{
    if(checked)
    {
        ui->dockwBasicTxn->show();
    }
    else
    {
        ui->dockwBasicTxn->hide();
    }
}

} // END namespace gnc
