#include <config.h>

#include <Transaction.h>
#include <Account.h>

#include "gmock-Split.h"
#include "gmock-qofbook.h"


struct _MockSplitClass
{
    QofInstanceClass parent_class;
};
typedef struct _MockSplitClass MockSplitClass;

G_DEFINE_TYPE(MockSplit, gnc_mocksplit, QOF_TYPE_INSTANCE)

static void
gnc_mocksplit_init (MockSplit *inst)
{
    // function is unused, initialization is done in the MockSplit's C++ constructor
}

static void
gnc_mocksplit_class_init (MockSplitClass *klass)
{
    // function is unused, class functions are defined in C++ code
}


GType gnc_split_get_type(void)
{
    return gnc_mocksplit_get_type();
}

Split *
xaccMallocSplit (QofBook *book)
{
    SCOPED_TRACE("");
    QofMockBook* mockbook = qof_mockbook(book);
    return mockbook ? mockbook->malloc_split() : nullptr;
}

QofBook *
xaccSplitGetBook (const Split *split)
{
    SCOPED_TRACE("");
    auto mocksplit = gnc_mocksplit(split);
    return mocksplit ? mocksplit->get_book() : nullptr;
}

Account *
xaccSplitGetAccount (const Split *split)
{
    SCOPED_TRACE("");
    auto mocksplit = gnc_mocksplit(split);
    return mocksplit ? mocksplit->get_account() : nullptr;
}

void
xaccSplitSetAccount (Split *split, Account *acc)
{
    ASSERT_TRUE(GNC_IS_MOCKSPLIT(split));
    ASSERT_TRUE(GNC_IS_ACCOUNT(acc));
    gnc_mocksplit(split)->set_account(acc);
}

gnc_numeric
xaccSplitGetAmount (const Split *split)
{
    SCOPED_TRACE("");
    auto mocksplit = gnc_mocksplit(split);
    return mocksplit ? mocksplit->get_amount() : gnc_numeric_zero();
}

void
xaccSplitSetAmount (Split *split, gnc_numeric amt)
{
    ASSERT_TRUE(GNC_IS_MOCKSPLIT(split));
    gnc_mocksplit(split)->set_amount(amt);
}

gnc_numeric
xaccSplitGetValue (const Split *split)
{
    SCOPED_TRACE("");
    auto mocksplit = gnc_mocksplit(split);
    return mocksplit ? mocksplit->get_value() : gnc_numeric_zero();
}

void
xaccSplitSetValue (Split *split, gnc_numeric val)
{
    ASSERT_TRUE(GNC_IS_MOCKSPLIT(split));
    gnc_mocksplit(split)->set_value(val);
}

const char *
xaccSplitGetMemo (const Split *split)
{
    SCOPED_TRACE("");
    auto mocksplit = gnc_mocksplit(split);
    return mocksplit ? mocksplit->get_memo() : "";
}

void
xaccSplitSetMemo (Split *split, const char *memo)
{
    ASSERT_TRUE(GNC_IS_MOCKSPLIT(split));
    gnc_mocksplit(split)->set_memo(memo);
}

char
xaccSplitGetReconcile (const Split *split)
{
    SCOPED_TRACE("");
    auto mocksplit = gnc_mocksplit(split);
    return mocksplit ? mocksplit->get_reconcile() : VREC;
}

void
xaccSplitSetReconcile (Split *split, char recn)
{
    ASSERT_TRUE(GNC_IS_MOCKSPLIT(split));
    gnc_mocksplit(split)->set_reconcile(recn);
}

void
xaccSplitSetDateReconciledSecs (Split *split, time64 secs)
{
    ASSERT_TRUE(GNC_IS_MOCKSPLIT(split));
    gnc_mocksplit(split)->set_date_reconciled_secs(secs);
}

const char *
xaccSplitGetAction (const Split *split)
{
    SCOPED_TRACE("");
    auto mocksplit = gnc_mocksplit(split);
    return mocksplit ? mocksplit->get_action() : "";
}

Split *
xaccSplitGetOtherSplit (const Split *split)
{
    SCOPED_TRACE("");
    auto mocksplit = gnc_mocksplit(split);
    return mocksplit ? mocksplit->get_other_split() : nullptr;
}

Transaction *
xaccSplitGetParent (const Split *split)
{
    SCOPED_TRACE("");
    auto mocksplit = gnc_mocksplit(split);
    return mocksplit ? mocksplit->get_parent() : nullptr;
}

void
xaccSplitSetParent(Split *split, Transaction *trans)
{
    ASSERT_TRUE(GNC_IS_MOCKSPLIT(split));
    ASSERT_TRUE(GNC_IS_TRANSACTION(trans));
    gnc_mocksplit(split)->set_parent(trans);
}
