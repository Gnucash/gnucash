#include "ViewletModel.hpp"
#include "gncmm/Transaction.hpp"
#include "gncmm/Account.hpp"

#include "gncmm/Numeric.hpp"

namespace gnc
{

ViewletModel::ViewletModel()
{
}

void
ViewletModel::defaultVGenerate(::Account *selectedAccount)
{
    SplitQList splitList = buildSplitListDateSort(selectedAccount);
    buildMiniJournalStruct(splitList);
}

void
ViewletModel::leftVGenerate(::Account *selectedAccount)
{
    ::QofBook *book = gnc_account_get_book(selectedAccount);
    ::Account *rootAccount = gnc_book_get_root_account(book);

    GList *accountsGList = gnc_account_get_descendants(rootAccount);
    AccountQList accountsList = accountFromGList(accountsGList);

    int numOfAccounts = accountsList.count();
    qDebug()<<"Total num of accounts: "<<numOfAccounts;

    AccountQList expenseAccountsList;
    for(int i = 0; i < numOfAccounts; i++)
    {
        if(xaccAccountGetType(accountsList.at(i)) == 9)
        {
            expenseAccountsList.append(accountsList.at(i));
        }
    }

    SplitQList splitsList = buildSplitListDateSort(expenseAccountsList);
    buildMiniJournalStruct(splitsList);
}

void
ViewletModel::rightVGenerate(::Account *selectedAccount)
{
    ::QofBook *book = gnc_account_get_book(selectedAccount);
    ::Account *rootAccount = gnc_book_get_root_account(book);

    GList *accountsGList = gnc_account_get_descendants(rootAccount);
    AccountQList accountsList = accountFromGList(accountsGList);

    int numOfAccounts = accountsList.count();
    qDebug()<<"Total num of accounts: "<<numOfAccounts;

    AccountQList expenseAccountsList;
    for(int i = 0; i < numOfAccounts; i++)
    {
        if(xaccAccountGetType(accountsList.at(i)) == 8)
        {
            expenseAccountsList.append(accountsList.at(i));
        }
    }

    SplitQList splitsList = buildSplitListDateSort(expenseAccountsList);
    buildMiniJournalStruct(splitsList);
}

#if 0

    /* get all transactions earlier than the specified date */
    QofQuery *qr =  qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book(qr, ::gnc_account_get_book(selectedAccount));
    // To look for dates, you need to create a "PredData" object
    Timespec calve_date;

    //calve_date =  gdate_to_timespec(trans.getGDatePosted());
    QofQueryPredData *pred_data = qof_query_date_predicate (QOF_COMPARE_LTE,
                                          QOF_DATE_MATCH_NORMAL,
                                          calve_date);
    // and additionally a "query parameter" object
    GSList *param_list = qof_query_build_param_list (TRANS_DATE_POSTED, NULL);
    // The "PredData" and the "query parameter" object are added to this query
    qof_query_add_term (qr, param_list, pred_data,
                        QOF_QUERY_FIRST_TERM);

    // Query is run; result is returned
    GList *result =  qof_query_run (qr);

    SplitQList querySplitList = Split::fromGList(result);
    Split qSplit;
    int numOfQuerSplits = querySplitList.count();

    for(i=0; i < numOfQuerSplits; ++i)
    {
        qSplit = querySplitList.at(i);
        //qDebug()<<qSplit.getCorrAccountName();
        qDebug()<<qSplit.getParent().getDatePosted().toString();

    }

    // "result" is now a GList of "Transaction*" because at
    //qof_query_create_for, we've asked for transactions.

    // When finished, delete the QofQuery object but this will
    // also delete the "result" list.
    qof_query_destroy (qr);
#endif

SplitQList
ViewletModel::buildSplitListDateSort(::Account *selectedAccount)
{
    ::SplitList * splitL = ::xaccAccountGetSplitList(selectedAccount);
    return Split::from_glist(splitL);
}

static bool greaterThanByDate(const ::Split* a, const ::Split* b)
{
    const ::Transaction* tx_a = xaccSplitGetParent(a);
    const ::Transaction* tx_b = xaccSplitGetParent(b);
    return xaccTransGetDate(tx_a) > xaccTransGetDate(tx_b);
}

SplitQList
ViewletModel::buildSplitListDateSort(AccountQList accountsList)
{
    int numOfAccounts = accountsList.count();
    qDebug() <<"Num of accounts of X TYPE: "<<numOfAccounts;

    SplitQList allSplitsList;
    for(int i=0; i< numOfAccounts; i++)
    {
        ::Account *C_acct = static_cast< ::Account *>(accountsList.at(i));

        SplitQList tempList = Split::from_glist(::xaccAccountGetSplitList(C_acct));

        int numOfSplits = tempList.size();
        for(int i=0; i<numOfSplits; i++)
        {
            allSplitsList.push_back(tempList.at(i));
        }
    }

    qSort(allSplitsList.begin(), allSplitsList.end(), &greaterThanByDate);
    return allSplitsList;
}

bool
ViewletModel::lessThanByDate(::Split* a, ::Split* b)
{
    ::Transaction* tx_a = xaccSplitGetParent(a);
    ::Transaction* tx_b = xaccSplitGetParent(b);
    return xaccTransGetDate(tx_a) < xaccTransGetDate(tx_b);
}

void
ViewletModel::buildMiniJournalStruct(SplitQList splitList)
{
    int numOfSplits = splitList.size();
    Glib::Date tempDate;
    Glib::ustring tempAccount;

    for (int i = 0; i < numOfSplits; i++)
    {
        Glib::RefPtr<Split> split = Glib::wrap(splitList.at(i));
        Glib::RefPtr<Transaction> txn = split->get_parent();

        structViewletEntries entry;

        if(i == 0)
        {
            tempDate = txn->get_date_posted();
            entry.isDateEqual = false;
            tempAccount = split->get_corr_account_name();
            entry.isSplitAccountEqual = false;
        }
        else
        {
            if(txn->get_date_posted() == tempDate)
            {
                entry.isDateEqual = true;
            }
            else
            {
                entry.isDateEqual = false;
                tempDate = txn->get_date_posted();
            }

            if(split->get_corr_account_name() == tempAccount)
            {
                entry.isSplitAccountEqual = true;
            }
            else
            {
                entry.isSplitAccountEqual = false;
                tempAccount = split->get_corr_account_name();
            }
        }

        entry.txnDate = g2q(txn->get_date_posted()).toString();
        entry.splitAccount = g2q(split->get_corr_account_name());
        entry.txnDescription = g2q(txn->get_description());

        Numeric splitAmount = split->get_amount();
        PrintAmountInfo printInfo(split, true);
        entry.splitAmount = g2q(splitAmount.printAmount(printInfo));

        //qDebug()<<entry.isDateEqual;
        //qDebug()<<entry.isSplitAccountEqual;

        queueEntries.enqueue(entry);
    }
}
} // END namespace gnc
