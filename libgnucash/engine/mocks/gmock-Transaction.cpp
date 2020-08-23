#include <config.h>

#include "gmock-Transaction.h"
#include "gmock-Account.h"


struct _MockTransactionClass
{
    QofInstanceClass parent_class;
};
typedef struct _MockTransactionClass MockTransactionClass;

G_DEFINE_TYPE(MockTransaction, gnc_mock_transaction, QOF_TYPE_INSTANCE);

static void
gnc_mock_transaction_init (MockTransaction *inst)
{
    // function is unused, initialization is done in the MockTransaction's constructor
}

static void
gnc_mock_transaction_class_init(MockTransactionClass *klass)
{
    // function is unused, class functions are defined in C++ code
}


void
xaccTransBeginEdit (Transaction *trans)
{
    g_return_if_fail(GNC_IS_MOCK_TRANSACTION(trans));
    ((MockTransaction*)trans)->beginEdit();
}

void
xaccTransCommitEdit (Transaction *trans)
{
    g_return_if_fail(GNC_IS_MOCK_TRANSACTION(trans));
    ((MockTransaction*)trans)->commitEdit();
}

Split *
xaccTransGetSplit (const Transaction *trans, int i)
{
    g_return_val_if_fail(GNC_IS_MOCK_TRANSACTION(trans), NULL);
    return ((MockTransaction*)trans)->getSplit(i);
}

SplitList *
xaccTransGetSplitList (const Transaction *trans)
{
    g_return_val_if_fail(GNC_IS_MOCK_TRANSACTION(trans), NULL);
    return trans ? ((MockTransaction*)trans)->getSplitList() : NULL;
}

Split *
xaccTransFindSplitByAccount(const Transaction *trans, const Account *acc)
{
    g_return_val_if_fail(GNC_IS_MOCK_TRANSACTION(trans), NULL);
    g_return_val_if_fail(GNC_IS_MOCK_ACCOUNT(acc), NULL);
    return ((MockTransaction*)trans)->findSplitByAccount(acc);
}

time64
xaccTransGetDate (const Transaction *trans)
{
    g_return_val_if_fail(GNC_IS_MOCK_TRANSACTION(trans), 0);
    return ((MockTransaction*)trans)->getDate();
}

void
xaccTransSetDatePostedSecsNormalized (Transaction *trans, time64 time)
{
    g_return_if_fail(GNC_IS_MOCK_TRANSACTION(trans));
    ((MockTransaction*)trans)->setDatePostedSecsNormalized(time);
}

const char *
xaccTransGetDescription (const Transaction *trans)
{
    g_return_val_if_fail(GNC_IS_MOCK_TRANSACTION(trans), NULL);
    return ((MockTransaction*)trans)->getDescription();
}

void
xaccTransSetDescription (Transaction *trans, const char *desc)
{
    g_return_if_fail(GNC_IS_MOCK_TRANSACTION(trans));
    ((MockTransaction*)trans)->setDescription(desc);
}

const char *
xaccTransGetNotes (const Transaction *trans)
{
    g_return_val_if_fail(GNC_IS_MOCK_TRANSACTION(trans), NULL);
    return ((MockTransaction*)trans)->getNotes();
}

void
xaccTransSetNotes (Transaction *trans, const char *notes)
{
    g_return_if_fail(GNC_IS_MOCK_TRANSACTION(trans));
    ((MockTransaction*)trans)->setDescription(notes);
}

gnc_numeric
xaccTransGetImbalanceValue (const Transaction * trans)
{
    g_return_val_if_fail(GNC_IS_MOCK_TRANSACTION(trans), gnc_numeric_zero());
    return ((MockTransaction*)trans)->getImbalanceValue();
}

const char *
xaccTransGetNum (const Transaction *trans)
{
    g_return_val_if_fail(GNC_IS_MOCK_TRANSACTION(trans), NULL);
    return ((MockTransaction*)trans)->getNum();
}

gboolean
xaccTransIsOpen (const Transaction *trans)
{
    g_return_val_if_fail(GNC_IS_MOCK_TRANSACTION(trans), FALSE);
    return ((MockTransaction*)trans)->isOpen();
}

void
xaccTransDestroy (Transaction *trans)
{
    g_return_if_fail(GNC_IS_MOCK_TRANSACTION(trans));
    ((MockTransaction*)trans)->destroy();
}
