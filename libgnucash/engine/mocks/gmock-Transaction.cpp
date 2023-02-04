#include <config.h>

#include <Account.h>

#include "gmock-Transaction.h"


struct _MockTransactionClass
{
    QofInstanceClass parent_class;
};
typedef struct _MockTransactionClass MockTransactionClass;

G_DEFINE_TYPE(MockTransaction, gnc_mocktransaction, QOF_TYPE_INSTANCE)

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


GType gnc_transaction_get_type(void)
{
    return gnc_mocktransaction_get_type();
}

void
xaccTransBeginEdit (Transaction *trans)
{
    ASSERT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    gnc_mocktransaction(trans)->begin_edit();
}

void
xaccTransCommitEdit (Transaction *trans)
{
    ASSERT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    gnc_mocktransaction(trans)->commit_edit();
}

Split *
xaccTransGetSplit (const Transaction *trans, int i)
{
    SCOPED_TRACE("");
    auto mocktrans = gnc_mocktransaction(trans);
    return mocktrans ? mocktrans->get_split(i) : nullptr;
}

SplitList *
xaccTransGetSplitList (const Transaction *trans)
{
    g_return_val_if_fail(GNC_IS_MOCKTRANSACTION(trans), NULL);
    return trans ? ((MockTransaction*)trans)->get_split_list() : NULL;
}

Split *
xaccTransFindSplitByAccount(const Transaction *trans, const Account *acc)
{
    SCOPED_TRACE("");
    auto mocktrans = gnc_mocktransaction(trans);
    EXPECT_TRUE(GNC_IS_ACCOUNT(acc));
    return mocktrans ? mocktrans->find_split_by_account(acc) : nullptr;
}

time64
xaccTransGetDate (const Transaction *trans)
{
    SCOPED_TRACE("");
    auto mocktrans = gnc_mocktransaction(trans);
    return mocktrans ? mocktrans->get_date() : 0;
}

void
xaccTransSetDatePostedSecsNormalized (Transaction *trans, time64 time)
{
    ASSERT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    gnc_mocktransaction(trans)->set_date_posted_secs_normalized(time);
}

const char *
xaccTransGetDescription (const Transaction *trans)
{
    SCOPED_TRACE("");
    auto mocktrans = gnc_mocktransaction(trans);
    return mocktrans ? mocktrans->get_description() : "";
}

void
xaccTransSetDescription (Transaction *trans, const char *desc)
{
    ASSERT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    gnc_mocktransaction(trans)->set_description(desc);
}

const char *
xaccTransGetNotes (const Transaction *trans)
{
    SCOPED_TRACE("");
    auto mocktrans = gnc_mocktransaction(trans);
    return mocktrans ? mocktrans->get_notes() : "";
}

void
xaccTransSetNotes (Transaction *trans, const char *notes)
{
    ASSERT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    gnc_mocktransaction(trans)->set_notes(notes);
}

gnc_numeric
xaccTransGetImbalanceValue (const Transaction * trans)
{
    SCOPED_TRACE("");
    auto mocktrans = gnc_mocktransaction(trans);
    return mocktrans ? mocktrans->get_imbalance_value() : gnc_numeric_zero();
}

const char *
xaccTransGetNum (const Transaction *trans)
{
    SCOPED_TRACE("");
    auto mocktrans = gnc_mocktransaction(trans);
    return mocktrans ? mocktrans->get_num() : "";
}

gnc_commodity *
xaccTransGetCurrency (const Transaction *trans)
{
    SCOPED_TRACE("");
    auto mocktrans = gnc_mocktransaction(trans);
    return mocktrans ? mocktrans->get_currency() : nullptr;
}

gboolean
xaccTransIsOpen (const Transaction *trans)
{
    SCOPED_TRACE("");
    auto mocktrans = gnc_mocktransaction(trans);
    return mocktrans ? mocktrans->is_open() : FALSE;
}

void
xaccTransDestroy (Transaction *trans)
{
    ASSERT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    gnc_mocktransaction(trans)->destroy();
}

void
xaccTransRecordPrice (Transaction *trans, PriceSource source)
{
    g_return_if_fail(GNC_IS_MOCKTRANSACTION(trans));
    ((MockTransaction*)trans)->recordPrice();
}
