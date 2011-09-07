#include "ViewletView.hpp"

namespace gnc
{

ViewletView::ViewletView(QWidget * parent, QHBoxLayout * FPOlayout) :
    QWidget(parent)
{
    viewletModel = new ViewletModel();
}

/***** Public *****/

void
ViewletView::defaultVSet(QWidget *parent, QHBoxLayout *FPOLayout)
{
    /* For default viewlet */
    comboAccountsList = new QComboBox();
    comboAccountsList->addItem(tr("-NA-"));

    connect(comboAccountsList, SIGNAL(currentIndexChanged(int)),
            this, SLOT(defaultVUpdate()));

    /* Add a new QWidget (acts as a container for this viewlet) to the
       layout of QWidget (QDockWidget>QWidget, i.e, dockwFPO>dockcFPO)
       in dashboard QMainWindow.
    */
    QWidget *viewletContainer = new QWidget;
    FPOLayout->addWidget(viewletContainer);

    /* Set a layout for the container QWidget */
    QVBoxLayout *vLay = new QVBoxLayout;
    viewletContainer->setLayout(vLay);

    /***** Start of viewlet specific implementations *****/
    /* Specification:
       This default viewlet contains two widgets, 1) An account
       selection widget, and 2) A scroll area which wraps a QWidget
       to show the entries.*/
    /** @bugid_1 1) Account selection feature of the viewlet  */
    /*
    comboAccountsList = new QComboBox();
    comboAccountsList->addItem(tr("-NA-"));
    */

    vLay->addWidget(comboAccountsList);

    /* 2) The actual viewlet display of account selected in 1) */
    QWidget *defaultViewletWidget = new QWidget();
    defaultVLayout = new QVBoxLayout();
    QScrollArea *viewletScrollArea = new QScrollArea();

    viewletScrollArea->setWidget(defaultViewletWidget);
    viewletScrollArea->setAlignment(Qt::AlignLeft);
    viewletScrollArea->setWidgetResizable(true);
    defaultViewletWidget->setLayout(defaultVLayout);
    vLay->addWidget(viewletScrollArea);

    //create viewlet
    if(comboAccountsList->currentIndex())
    {
        selectedAccountIndex = comboAccountsList->currentIndex();
        selectedAccount = accountsList->at(selectedAccountIndex);

        viewletModel->defaultVGenerate(selectedAccount);
        defaultVDraw();
    }
}

void
ViewletView::leftVSet(QWidget *parent, QHBoxLayout *FPOLayout)
{
    connect(this, SIGNAL(fileLoaded()),
            this, SLOT(leftVLoad()));

    //connect(comboAccountsList, SIGNAL(currentIndexChanged(int)),
          //  this, SLOT(leftVUpdate()));

    //not required. remove after cleaning loadAccountsTreeComboBox()
    comboAccountsList = new QComboBox();
    comboAccountsList->addItem(tr("-NA-"));


    QWidget *viewletContainer = new QWidget;
    FPOLayout->addWidget(viewletContainer);

    QVBoxLayout *vLay = new QVBoxLayout;
    viewletContainer->setLayout(vLay);

    QLabel *title = new QLabel(tr("Expense"));
    vLay->addWidget(title);

    /* The actual viewlet display of account(s) chosen*/
    QWidget *defaultViewletWidget = new QWidget();
    defaultVLayout = new QVBoxLayout();
    QScrollArea *viewletScrollArea = new QScrollArea();

    viewletScrollArea->setWidget(defaultViewletWidget);
    viewletScrollArea->setAlignment(Qt::AlignLeft);
    viewletScrollArea->setWidgetResizable(true);
    defaultViewletWidget->setLayout(defaultVLayout);
    vLay->addWidget(viewletScrollArea);

    /*//create viewlet
    if(comboAccountsList->currentIndex())
    {
        selectedAccountIndex = comboAccountsList->currentIndex();
        selectedAccount = accountsList->at(selectedAccountIndex);

        defaultVDraw();
    }*/

}

void
ViewletView::rightVSet(QWidget *parent, QHBoxLayout *FPOLayout)
{
    connect(this, SIGNAL(fileLoaded()),
            this, SLOT(rightVLoad()));

    //connect(comboAccountsList, SIGNAL(currentIndexChanged(int)),
          //  this, SLOT(leftVUpdate()));

    //not required. remove after cleaning loadAccountsTreeComboBox()
    comboAccountsList = new QComboBox();
    comboAccountsList->addItem(tr("-NA-"));


    QWidget *viewletContainer = new QWidget;
    FPOLayout->addWidget(viewletContainer);

    QVBoxLayout *vLay = new QVBoxLayout;
    viewletContainer->setLayout(vLay);

    QLabel *title = new QLabel(tr("Income"));
    vLay->addWidget(title);

    /* The actual viewlet display of account(s) chosen*/
    QWidget *defaultViewletWidget = new QWidget();
    defaultVLayout = new QVBoxLayout();
    QScrollArea *viewletScrollArea = new QScrollArea();

    viewletScrollArea->setWidget(defaultViewletWidget);
    viewletScrollArea->setAlignment(Qt::AlignLeft);
    viewletScrollArea->setWidgetResizable(true);
    defaultViewletWidget->setLayout(defaultVLayout);
    vLay->addWidget(viewletScrollArea);

    /*//create viewlet
    if(comboAccountsList->currentIndex())
    {
        selectedAccountIndex = comboAccountsList->currentIndex();
        selectedAccount = accountsList->at(selectedAccountIndex);

        defaultVDraw();
    }*/

}

/***** Private *****/

/** Create the widgets for the viewlet entries

    Passes the selected account to the model. The updated textual
    data in the struct of the model is used in the newly created
    widgets.
*/
void
ViewletView::defaultVDraw()
{
    /* Update the struct in ViewletModel with data from the selected
       account
    */


    int numOfTransactions = viewletModel->queueEntries.count();
    for (int i = 0; i < numOfTransactions; i++)
    {
        viewletModel->tempEntry = viewletModel->queueEntries.at(i);

        //1 & 2
        if((!viewletModel->tempEntry.isDateEqual && !viewletModel->tempEntry.isSplitAccountEqual)
            || (!viewletModel->tempEntry.isDateEqual && viewletModel->tempEntry.isSplitAccountEqual))
        {
            dateCheckOutput();
            accountCheckOutput();
            descriptionAmountOutput();
        }

        //3
        if(viewletModel->tempEntry.isDateEqual && !viewletModel->tempEntry.isSplitAccountEqual)
        {
            accountCheckOutput();
            descriptionAmountOutput();
        }

        //4
        if(viewletModel->tempEntry.isDateEqual && viewletModel->tempEntry.isSplitAccountEqual)
        {
            descriptionAmountOutput();
        }
    }
}

void
ViewletView::dateCheckOutput()
{
    QWidget *dateLevelContainer = new QWidget();
    QVBoxLayout *dateLayout = new QVBoxLayout;
    dateLevelContainer->setLayout(dateLayout);

    /* Append the pointer of this top level widget
      of the viewlet for later removal during update */
    viewletWidgetContainersList.append(dateLevelContainer);

    defaultVLayout->addWidget(dateLevelContainer, 10, Qt::AlignTop);

    txnDate = viewletModel->tempEntry.txnDate;
    setLabel(txnDate, "dateWidget", dateLayout);

    QWidget *accountLevelContainer = new QWidget();
    accountLayout = new QVBoxLayout;
    accountLevelContainer->setLayout(accountLayout);
    dateLayout->addWidget(accountLevelContainer);
}

void
ViewletView::accountCheckOutput()
{
    QWidget *singleAccountLevelContainer = new QWidget();
    QVBoxLayout *singleAccountLayout = new QVBoxLayout();
    singleAccountLevelContainer->setLayout(singleAccountLayout);

    accountLayout->addWidget(singleAccountLevelContainer);

    // 1
    splitAccount = viewletModel->tempEntry.splitAccount;
    setLabel(splitAccount, "accountWidget", singleAccountLayout);

    QWidget *descriptionAmountLevelContainer = new QWidget();
    descriptionAmountLayout = new QVBoxLayout();
    descriptionAmountLevelContainer->setLayout(descriptionAmountLayout);

    // 2
    singleAccountLayout->addWidget(descriptionAmountLevelContainer);
}

void
ViewletView::descriptionAmountOutput()
{
    txnDescription = viewletModel->tempEntry.txnDescription;
    setLabel(txnDescription, "descWidget", descriptionAmountLayout);

    splitAmount = viewletModel->tempEntry.splitAmount;
    setLabel(splitAmount, "amountWidget", descriptionAmountLayout);
}

void
ViewletView::defaultVRemoveWidgets()
{
    /* Remove old widgets. */
    int numOfContainers = viewletWidgetContainersList.count();
    for (int i=0; i<numOfContainers; i++)
    {
        delete viewletWidgetContainersList.at(i);
    }

    /* Empty the data structures */
    viewletModel->queueEntries.clear();
    viewletWidgetContainersList.clear();
}

/**********/

void
ViewletView::setLabel(QString data, QString objectName, QVBoxLayout *layout)
{
    QLabel *lbl = new QLabel();
    lbl->setText(data);
    layout->addWidget(lbl);
    /* Used as CSS ID by QStyleSheet */
    lbl->setObjectName(objectName);
    viewletWidgetsList.append(lbl);
}

void
ViewletView::loadAccountsTreeComboBox(AccountListModel * const m_accountsListModel)
{
    accountsList = m_accountsListModel;
    comboAccountsList->setModel(accountsList);
    emit fileLoaded();
}

/***** Slots *****/

void
ViewletView::defaultVUpdate()
{
    selectedAccountIndex = comboAccountsList->currentIndex();
    selectedAccount = accountsList->at(selectedAccountIndex);

    defaultVRemoveWidgets();
    viewletModel->defaultVGenerate(selectedAccount);
    defaultVDraw();
}


void
ViewletView::leftVUpdate()
{
    selectedAccountIndex = comboAccountsList->currentIndex();
    selectedAccount = accountsList->at(selectedAccountIndex);

    //Call this in dboard gnc event switch
    defaultVRemoveWidgets();
    viewletModel->leftVGenerate(selectedAccount);
    defaultVDraw();
}

void
ViewletView::leftVLoad()
{
    selectedAccount = accountsList->at(1);
    defaultVRemoveWidgets();
    viewletModel->leftVGenerate(selectedAccount);
    defaultVDraw();
}

void
ViewletView::rightVUpdate()
{
    selectedAccountIndex = comboAccountsList->currentIndex();
    selectedAccount = accountsList->at(selectedAccountIndex);

    //Call this in dboard gnc event switch
    defaultVRemoveWidgets();
    viewletModel->rightVGenerate(selectedAccount);
    defaultVDraw();
}

void
ViewletView::rightVLoad()
{
    selectedAccount = accountsList->at(1);
    defaultVRemoveWidgets();
    viewletModel->rightVGenerate(selectedAccount);
    defaultVDraw();
}

} // END namespace gnc

