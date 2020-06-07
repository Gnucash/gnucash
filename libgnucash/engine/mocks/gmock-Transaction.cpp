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
    // function is unused, initialization is done in the MockTransaction's C++ constructor
}

static void
gnc_mock_transaction_class_init(MockTransactionClass *klass)
{
    // function is unused, class functions are defined in C++ code
}


void
xaccTransBeginEdit (Transaction *trans)
{
    ASSERT_TRUE(GNC_IS_MOCK_TRANSACTION(trans));
    ((MockTransaction*)trans)->beginEdit();
}

void
xaccTransCommitEdit (Transaction *trans)
{
    ASSERT_TRUE(GNC_IS_MOCK_TRANSACTION(trans));
    ((MockTransaction*)trans)->commitEdit();
}

Split *
xaccTransGetSplit (const Transaction *trans, int i)
{
    EXPECT_TRUE(GNC_IS_MOCK_TRANSACTION(trans));
    if (GNC_IS_MOCK_TRANSACTION(trans))
        return ((MockTransaction*)trans)->getSplit(i);
    else
        return nullptr;
}

Split *
xaccTransFindSplitByAccount(const Transaction *trans, const Account *acc)
{
    EXPECT_TRUE(GNC_IS_MOCK_TRANSACTION(trans));
    EXPECT_TRUE(GNC_IS_MOCK_ACCOUNT(acc));
    if (GNC_IS_MOCK_TRANSACTION(trans))
        return ((MockTransaction*)trans)->findSplitByAccount(acc);
    else
        return nullptr;
}

time64
xaccTransGetDate (const Transaction *trans)
{
    EXPECT_TRUE(GNC_IS_MOCK_TRANSACTION(trans));
    if (GNC_IS_MOCK_TRANSACTION(trans))
        return ((MockTransaction*)trans)->getDate();
    else
        return 0;
}

void
xaccTransSetDatePostedSecsNormalized (Transaction *trans, time64 time)
{
    ASSERT_TRUE(GNC_IS_MOCK_TRANSACTION(trans));
    ((MockTransaction*)trans)->setDatePostedSecsNormalized(time);
}

const char *
xaccTransGetDescription (const Transaction *trans)
{
    EXPECT_TRUE(GNC_IS_MOCK_TRANSACTION(trans));
    if (GNC_IS_MOCK_TRANSACTION(trans))
        return ((MockTransaction*)trans)->getDescription();
    else
        return nullptr;
}

void
xaccTransSetDescription (Transaction *trans, const char *desc)
{
    ASSERT_TRUE(GNC_IS_MOCK_TRANSACTION(trans));
    ((MockTransaction*)trans)->setDescription(desc);
}

const char *
xaccTransGetNotes (const Transaction *trans)
{
    EXPECT_TRUE(GNC_IS_MOCK_TRANSACTION(trans));
    if (GNC_IS_MOCK_TRANSACTION(trans))
        return ((MockTransaction*)trans)->getNotes();
    else
        return nullptr;
}

void
xaccTransSetNotes (Transaction *trans, const char *notes)
{
    ASSERT_TRUE(GNC_IS_MOCK_TRANSACTION(trans));
    ((MockTransaction*)trans)->setDescription(notes);
}

gnc_numeric
xaccTransGetImbalanceValue (const Transaction * trans)
{
    EXPECT_TRUE(GNC_IS_MOCK_TRANSACTION(trans));
    if (GNC_IS_MOCK_TRANSACTION(trans))
        return ((MockTransaction*)trans)->getImbalanceValue();
    else
        return gnc_numeric_zero();
}

const char *
xaccTransGetNum (const Transaction *trans)
{
    EXPECT_TRUE(GNC_IS_MOCK_TRANSACTION(trans));
    if (GNC_IS_MOCK_TRANSACTION(trans))
        return ((MockTransaction*)trans)->getNum();
    else
        return nullptr;
}

gboolean
xaccTransIsOpen (const Transaction *trans)
{
    EXPECT_TRUE(GNC_IS_MOCK_TRANSACTION(trans));
    if (GNC_IS_MOCK_TRANSACTION(trans))
        return ((MockTransaction*)trans)->isOpen();
    else
        return FALSE;
}

void
xaccTransDestroy (Transaction *trans)
{
    ASSERT_TRUE(GNC_IS_MOCK_TRANSACTION(trans));
    ((MockTransaction*)trans)->destroy();
}
