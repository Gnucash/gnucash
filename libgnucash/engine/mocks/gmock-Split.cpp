#include <config.h>

#include "gmock-Split.h"
#include "gmock-qofbook.h"
#include "gmock-Account.h"
#include "gmock-Transaction.h"


struct _MockSplitClass
{
    QofInstanceClass parent_class;
};
typedef struct _MockSplitClass MockSplitClass;

G_DEFINE_TYPE(MockSplit, gnc_mocksplit, QOF_TYPE_INSTANCE);

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


Split *
xaccMallocSplit (QofBook *book)
{
    EXPECT_TRUE(QOF_IS_MOCKBOOK(book));
    if (QOF_IS_MOCKBOOK(book))
        return ((QofMockBook*)book)->malloc_split();
    else
        return nullptr;
}

QofBook *
xaccSplitGetBook (const Split *split)
{
    EXPECT_TRUE(GNC_IS_MOCKSPLIT(split));
    if (GNC_IS_MOCKSPLIT(split))
        return ((MockSplit*)split)->get_book();
    else
        return nullptr;
}

Account *
xaccSplitGetAccount (const Split *split)
{
    EXPECT_TRUE(GNC_IS_MOCKSPLIT(split));
    if (GNC_IS_MOCKSPLIT(split))
        return ((MockSplit*)split)->get_account();
    else
        return nullptr;
}

void
xaccSplitSetAccount (Split *split, Account *acc)
{
    ASSERT_TRUE(GNC_IS_MOCKSPLIT(split));
    ASSERT_TRUE(GNC_IS_MOCKACCOUNT(acc));
    ((MockSplit*)split)->set_account(acc);
}

gnc_numeric
xaccSplitGetAmount (const Split *split)
{
    EXPECT_TRUE(GNC_IS_MOCKSPLIT(split));
    if (GNC_IS_MOCKSPLIT(split))
        return ((MockSplit*)split)->get_amount();
    else
        return gnc_numeric_zero();
}

void
xaccSplitSetAmount (Split *split, gnc_numeric amt)
{
    ASSERT_TRUE(GNC_IS_MOCKSPLIT(split));
    ((MockSplit*)split)->set_amount(amt);
}

gnc_numeric
xaccSplitGetValue (const Split *split)
{
    EXPECT_TRUE(GNC_IS_MOCKSPLIT(split));
    if (GNC_IS_MOCKSPLIT(split))
        return ((MockSplit*)split)->get_value();
    else
        return gnc_numeric_zero();
}

void
xaccSplitSetValue (Split *split, gnc_numeric val)
{
    ASSERT_TRUE(GNC_IS_MOCKSPLIT(split));
    ((MockSplit*)split)->set_value(val);
}

const char *
xaccSplitGetMemo (const Split *split)
{
    EXPECT_TRUE(GNC_IS_MOCKSPLIT(split));
    if (GNC_IS_MOCKSPLIT(split))
        return ((MockSplit*)split)->get_memo();
    else
        return nullptr;
}

char
xaccSplitGetReconcile (const Split *split)
{
    EXPECT_TRUE(GNC_IS_MOCKSPLIT(split));
    if (GNC_IS_MOCKSPLIT(split))
        return ((MockSplit*)split)->get_reconcile();
    else
        return VREC;
}

void
xaccSplitSetReconcile (Split *split, char recn)
{
    ASSERT_TRUE(GNC_IS_MOCKSPLIT(split));
    ((MockSplit*)split)->set_reconcile(recn);
}

void
xaccSplitSetDateReconciledSecs (Split *split, time64 secs)
{
    ASSERT_TRUE(GNC_IS_MOCKSPLIT(split));
    ((MockSplit*)split)->set_date_reconciled_secs(secs);
}

const char *
xaccSplitGetAction (const Split *split)
{
    EXPECT_TRUE(GNC_IS_MOCKSPLIT(split));
    if (GNC_IS_MOCKSPLIT(split))
        return ((MockSplit*)split)->get_action();
    else
        return nullptr;
}

Split *
xaccSplitGetOtherSplit (const Split *split)
{
    EXPECT_TRUE(GNC_IS_MOCKSPLIT(split));
    if (GNC_IS_MOCKSPLIT(split))
        return ((MockSplit*)split)->get_other_split();
    else
        return nullptr;
}

Transaction *
xaccSplitGetParent (const Split *split)
{
    EXPECT_TRUE(GNC_IS_MOCKSPLIT(split));
    if (GNC_IS_MOCKSPLIT(split))
        return ((MockSplit*)split)->get_parent();
    else
        return nullptr;
}

void
xaccSplitSetParent(Split *split, Transaction *trans)
{
    ASSERT_TRUE(GNC_IS_MOCKSPLIT(split));
    ASSERT_TRUE(GNC_IS_MOCKTRANSACTION(trans));
    ((MockSplit*)split)->set_parent(trans);
}
