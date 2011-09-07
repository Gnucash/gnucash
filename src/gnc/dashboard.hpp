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
 */

#ifndef DASHBOARD_HPP
#define DASHBOARD_HPP

#include "config.h"

extern "C"
{
#include "qof.h"
#include "engine/Account.h"
#include "engine/Transaction.h"
}

#include "AccountItemModel.hpp"
#include "fpo/FPO.hpp"
#include "gnc/QofEventWrapper.hpp"

#include <QMainWindow>
#include <QAbstractItemModel>
#include <QtGui>

namespace Ui {
    class Dashboard;
}

namespace gnc
{

class FPO;

class Dashboard : public QMainWindow
{
    Q_OBJECT

public:
    explicit Dashboard(QWidget *parent = 0);
    ~Dashboard();

    FPO *fpoWidget;
    AccountListModel *accountsList;
    void loadAccountsTreeComboBox(AccountListModel * const m_accountListModel);
    void showDashboardWidgets();
    void mainWindowCloseEvent();

public slots:
    void transferFundsWidgetButtonToggled(bool checked);
    void transactionEvent( ::Transaction* trans, QofEventId event_type);
    void accountEvent( ::Account* acc, QofEventId event_type);

private:
    Ui::Dashboard *ui;

    /* UI widgets */
    QGridLayout *gridBasicTxnEntry;
    QHBoxLayout *hbox;
    QVBoxLayout *vbox;
    QLabel *lblDescription;
    QLabel *lblDate;
    QLabel *lblTransferFrom;
    QLabel *lblTransferTo;
    QLabel *lblAmount;
    QLabel *lblMemo;
    QLabel *lblNum;
    QComboBox *comboTransferFrom;
    QComboBox *comboTransferTo;
    QLineEdit *lineDescription;
    QLineEdit *lineAmount;
    QLineEdit *lineMemo;
    QLineEdit *lineNum;
    QDateEdit *dateTxnDate;
    QDate dateVal;
    QPushButton *btnCreateBasicTxn;

    /* Transaction related data types */
    ::QofBook *book;
    ::Transaction *transaction;
    ::gnc_commodity *currency;
    int denom;

    ::Account *account;
    ::Split *split;
    double numer;
    ::gnc_numeric amount;

    ::Account *account2;
    ::Split *split2;
    ::gnc_numeric amount2;

    void setUiWidgets();
    void setBasicTxnEntryFormLayout();
    void setFPO();
    void clearFields();
    QofEventWrapper<Dashboard, ::Transaction*> m_eventWrapper;
    QofEventWrapper<Dashboard, ::Account*> m_eventWrapperAccount;

private slots:
    void on_btnCreateBasicTxn_clicked();
    void on_dockwBasicTxn_visibilityChanged(bool visible);
};

} // END namespace gnc

#endif // DASHBOARD_HPP
