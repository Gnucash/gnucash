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
    // function is unused since it's overwritten by MockSplit's constructor anyway
}

static void
gnc_mock_split_class_init (MockSplitClass *klass)
{
    // function is unused, class functions are defined in C++ code
}


Split *
xaccMallocSplit (QofBook *book)
{
    g_return_val_if_fail(QOF_IS_MOCK_BOOK(book), NULL);
    return ((QofMockBook*)book)->mallocSplit();
}

QofBook *
xaccSplitGetBook (const Split *split)
{
    g_return_val_if_fail(GNC_IS_MOCK_SPLIT(split), NULL);
    return ((MockSplit*)split)->getBook();
}

Account *
xaccSplitGetAccount (const Split *split)
{
    g_return_val_if_fail(GNC_IS_MOCK_SPLIT(split), NULL);
    return ((MockSplit*)split)->getAccount();
}

void
xaccSplitSetAccount (Split *split, Account *acc)
{
    g_return_if_fail(GNC_IS_MOCK_SPLIT(split));
    g_return_if_fail(GNC_IS_MOCK_ACCOUNT(acc));
    ((MockSplit*)split)->setAccount(acc);
}

gnc_numeric
xaccSplitGetAmount (const Split *split)
{
    g_return_val_if_fail(GNC_IS_MOCK_SPLIT(split), gnc_numeric_zero());
    return ((MockSplit*)split)->getAmount();
}

void
xaccSplitSetAmount (Split *split, gnc_numeric amt)
{
    g_return_if_fail(GNC_IS_MOCK_SPLIT(split));
    ((MockSplit*)split)->setAmount(amt);
}

gnc_numeric
xaccSplitGetValue (const Split *split)
{
    g_return_val_if_fail(GNC_IS_MOCK_SPLIT(split), gnc_numeric_zero());
    return ((MockSplit*)split)->getValue();
}

void
xaccSplitSetValue (Split *split, gnc_numeric val)
{
    g_return_if_fail(GNC_IS_MOCK_SPLIT(split));
    ((MockSplit*)split)->setValue(val);
}

const char *
xaccSplitGetMemo (const Split *split)
{
    g_return_val_if_fail(GNC_IS_MOCK_SPLIT(split), NULL);
    return ((MockSplit*)split)->getMemo();
}

char
xaccSplitGetReconcile (const Split *split)
{
    g_return_val_if_fail(GNC_IS_MOCK_SPLIT(split), VREC);
    return ((MockSplit*)split)->getReconcile();
}

void
xaccSplitSetReconcile (Split *split, char recn)
{
    g_return_if_fail(GNC_IS_MOCK_SPLIT(split));
    ((MockSplit*)split)->setReconcile(recn);
}

void
xaccSplitSetDateReconciledSecs (Split *split, time64 secs)
{
    g_return_if_fail(GNC_IS_MOCK_SPLIT(split));
    ((MockSplit*)split)->setDateReconciledSecs(secs);
}

const char *
xaccSplitGetAction (const Split *split)
{
    g_return_val_if_fail(GNC_IS_MOCK_SPLIT(split), NULL);
    return ((MockSplit*)split)->getAction();
}

Split *
xaccSplitGetOtherSplit (const Split *split)
{
    g_return_val_if_fail(GNC_IS_MOCK_SPLIT(split), NULL);
    return ((MockSplit*)split)->getOtherSplit();
}

Transaction *
xaccSplitGetParent (const Split *split)
{
    g_return_val_if_fail(GNC_IS_MOCK_SPLIT(split), NULL);
    return ((MockSplit*)split)->getParent();
}

void
xaccSplitSetParent(Split *split, Transaction *trans)
{
    g_return_if_fail(GNC_IS_MOCK_SPLIT(split));
    g_return_if_fail(GNC_IS_MOCK_TRANSACTION(trans));
    ((MockSplit*)split)->setParent(trans);
}
