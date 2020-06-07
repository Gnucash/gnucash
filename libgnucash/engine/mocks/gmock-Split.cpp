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

G_DEFINE_TYPE(MockSplit, gnc_mock_split, QOF_TYPE_INSTANCE);

static void
gnc_mock_split_init (MockSplit *inst)
{
    // function is unused, initialization is done in the MockSplit's C++ constructor
}

static void
gnc_mock_split_class_init (MockSplitClass *klass)
{
    // function is unused, class functions are defined in C++ code
}


Split *
xaccMallocSplit (QofBook *book)
{
    EXPECT_TRUE(QOF_IS_MOCK_BOOK(book));
    if (QOF_IS_MOCK_BOOK(book))
        return ((QofMockBook*)book)->mallocSplit();
    else
        return nullptr;
}

QofBook *
xaccSplitGetBook (const Split *split)
{
    EXPECT_TRUE(GNC_IS_MOCK_SPLIT(split));
    if (GNC_IS_MOCK_SPLIT(split))
        return ((MockSplit*)split)->getBook();
    else
        return nullptr;
}

Account *
xaccSplitGetAccount (const Split *split)
{
    EXPECT_TRUE(GNC_IS_MOCK_SPLIT(split));
    if (GNC_IS_MOCK_SPLIT(split))
        return ((MockSplit*)split)->getAccount();
    else
        return nullptr;
}

void
xaccSplitSetAccount (Split *split, Account *acc)
{
    ASSERT_TRUE(GNC_IS_MOCK_SPLIT(split));
    ASSERT_TRUE(GNC_IS_MOCK_ACCOUNT(acc));
    ((MockSplit*)split)->setAccount(acc);
}

gnc_numeric
xaccSplitGetAmount (const Split *split)
{
    EXPECT_TRUE(GNC_IS_MOCK_SPLIT(split));
    if (GNC_IS_MOCK_SPLIT(split))
        return ((MockSplit*)split)->getAmount();
    else
        return gnc_numeric_zero();
}

void
xaccSplitSetAmount (Split *split, gnc_numeric amt)
{
    ASSERT_TRUE(GNC_IS_MOCK_SPLIT(split));
    ((MockSplit*)split)->setAmount(amt);
}

gnc_numeric
xaccSplitGetValue (const Split *split)
{
    EXPECT_TRUE(GNC_IS_MOCK_SPLIT(split));
    if (GNC_IS_MOCK_SPLIT(split))
        return ((MockSplit*)split)->getValue();
    else
        return gnc_numeric_zero();
}

void
xaccSplitSetValue (Split *split, gnc_numeric val)
{
    ASSERT_TRUE(GNC_IS_MOCK_SPLIT(split));
    ((MockSplit*)split)->setValue(val);
}

const char *
xaccSplitGetMemo (const Split *split)
{
    EXPECT_TRUE(GNC_IS_MOCK_SPLIT(split));
    if (GNC_IS_MOCK_SPLIT(split))
        return ((MockSplit*)split)->getMemo();
    else
        return nullptr;
}

char
xaccSplitGetReconcile (const Split *split)
{
    EXPECT_TRUE(GNC_IS_MOCK_SPLIT(split));
    if (GNC_IS_MOCK_SPLIT(split))
        return ((MockSplit*)split)->getReconcile();
    else
        return VREC;
}

void
xaccSplitSetReconcile (Split *split, char recn)
{
    ASSERT_TRUE(GNC_IS_MOCK_SPLIT(split));
    ((MockSplit*)split)->setReconcile(recn);
}

void
xaccSplitSetDateReconciledSecs (Split *split, time64 secs)
{
    ASSERT_TRUE(GNC_IS_MOCK_SPLIT(split));
    ((MockSplit*)split)->setDateReconciledSecs(secs);
}

const char *
xaccSplitGetAction (const Split *split)
{
    EXPECT_TRUE(GNC_IS_MOCK_SPLIT(split));
    if (GNC_IS_MOCK_SPLIT(split))
        return ((MockSplit*)split)->getAction();
    else
        return nullptr;
}

Split *
xaccSplitGetOtherSplit (const Split *split)
{
    EXPECT_TRUE(GNC_IS_MOCK_SPLIT(split));
    if (GNC_IS_MOCK_SPLIT(split))
        return ((MockSplit*)split)->getOtherSplit();
    else
        return nullptr;
}

Transaction *
xaccSplitGetParent (const Split *split)
{
    EXPECT_TRUE(GNC_IS_MOCK_SPLIT(split));
    if (GNC_IS_MOCK_SPLIT(split))
        return ((MockSplit*)split)->getParent();
    else
        return nullptr;
}

void
xaccSplitSetParent(Split *split, Transaction *trans)
{
    ASSERT_TRUE(GNC_IS_MOCK_SPLIT(split));
    ASSERT_TRUE(GNC_IS_MOCK_TRANSACTION(trans));
    ((MockSplit*)split)->setParent(trans);
}
