#ifndef VIEWLETMODEL_HPP
#define VIEWLETMODEL_HPP

#include <QtCore>
#include <QtGui>

#include "config.h"

extern "C"
{
#include "qof.h"
#include "engine/Account.h"
#include "engine/Transaction.h"
#include "engine/Split.h"
}

#include "gnc/Split.hpp"
#include "gnc/SplitListModel.hpp"

namespace gnc
{

class ViewletModel
{
public:
    ViewletModel();
    void defaultVGenerate(::Account * selectedAccount);
    void leftVGenerate(::Account * selectedAccount);
    void rightVGenerate(::Account *selectedAccount);

    struct structViewletEntries
    {
        QString txnDate;
        bool isDateEqual;
        QString splitAccount;
        bool isSplitAccountEqual;
        QString txnDescription;
        QString splitAmount;
    };
    structViewletEntries tempEntry;
    QQueue<structViewletEntries> queueEntries;

private:
    SplitQList buildSplitListDateSort(::Account *selectedAccount);
    SplitQList buildSplitListDateSort(AccountQList accountsList);
    bool static lessThanByDate(::Split* a, ::Split* b);
    void buildMiniJournalStruct(SplitQList splitList);

};

} // END namespace gnc

#endif // VIEWLETMODEL_HPP
