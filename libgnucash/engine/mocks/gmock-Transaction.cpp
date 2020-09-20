#include <config.h>

#include "gmock-Transaction.h"
#include "gmock-Account.h"


struct _MockTransactionClass
{
    QofInstanceClass parent_class;
};
typedef struct _MockTransactionClass MockTransactionClass;

G_DEFINE_TYPE(MockTransaction, gnc_mocktransaction, QOF_TYPE_INSTANCE);

static void
gnc_mocktransaction_init (MockTransaction *inst)
{
    // function is unused, initialization is done in the MockTransaction's C++ constructor
}

static void
gnc_mocktransaction_class_init(MockTransactionClass *klass)
{
    // function is unused, class functions are defined in C++ code
}


void
xaccTransBeginEdit (Transaction *trans)
{
    ASSERT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    ((MockTransaction*)trans)->begin_edit();
}

void
xaccTransCommitEdit (Transaction *trans)
{
    ASSERT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    ((MockTransaction*)trans)->commit_edit();
}

Split *
xaccTransGetSplit (const Transaction *trans, int i)
{
    EXPECT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    if (GNC_IS_MOCKTRANSACTION(trans))
        return ((MockTransaction*)trans)->get_split(i);
    else
        return nullptr;
}

Split *
xaccTransFindSplitByAccount(const Transaction *trans, const Account *acc)
{
    EXPECT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    EXPECT_TRUE(GNC_IS_MOCKACCOUNT(acc));
    if (GNC_IS_MOCKTRANSACTION(trans))
        return ((MockTransaction*)trans)->find_split_by_account(acc);
    else
        return nullptr;
}

time64
xaccTransGetDate (const Transaction *trans)
{
    EXPECT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    if (GNC_IS_MOCKTRANSACTION(trans))
        return ((MockTransaction*)trans)->get_date();
    else
        return 0;
}

void
xaccTransSetDatePostedSecsNormalized (Transaction *trans, time64 time)
{
    ASSERT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    ((MockTransaction*)trans)->set_date_posted_secs_normalized(time);
}

const char *
xaccTransGetDescription (const Transaction *trans)
{
    EXPECT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    if (GNC_IS_MOCKTRANSACTION(trans))
        return ((MockTransaction*)trans)->get_description();
    else
        return nullptr;
}

void
xaccTransSetDescription (Transaction *trans, const char *desc)
{
    ASSERT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    ((MockTransaction*)trans)->set_description(desc);
}

const char *
xaccTransGetNotes (const Transaction *trans)
{
    EXPECT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    if (GNC_IS_MOCKTRANSACTION(trans))
        return ((MockTransaction*)trans)->get_notes();
    else
        return nullptr;
}

void
xaccTransSetNotes (Transaction *trans, const char *notes)
{
    ASSERT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    ((MockTransaction*)trans)->set_description(notes);
}

gnc_numeric
xaccTransGetImbalanceValue (const Transaction * trans)
{
    EXPECT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    if (GNC_IS_MOCKTRANSACTION(trans))
        return ((MockTransaction*)trans)->get_imbalance_value();
    else
        return gnc_numeric_zero();
}

const char *
xaccTransGetNum (const Transaction *trans)
{
    EXPECT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    if (GNC_IS_MOCKTRANSACTION(trans))
        return ((MockTransaction*)trans)->get_num();
    else
        return nullptr;
}

gboolean
xaccTransIsOpen (const Transaction *trans)
{
    EXPECT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    if (GNC_IS_MOCKTRANSACTION(trans))
        return ((MockTransaction*)trans)->is_open();
    else
        return FALSE;
}

void
xaccTransDestroy (Transaction *trans)
{
    ASSERT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    ((MockTransaction*)trans)->destroy();
}
