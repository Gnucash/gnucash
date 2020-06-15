#include <config.h>

#include "gmock-Account.h"


struct _MockAccountClass
{
    QofInstanceClass parent_class;
};
typedef struct _MockAccountClass MockAccountClass;

G_DEFINE_TYPE(MockAccount, gnc_mock_account, QOF_TYPE_INSTANCE);

static void
gnc_mock_account_init (MockAccount *inst)
{
    // function is unused, initialization is done in the MockAccount's C++ constructor
}

static void
gnc_mock_account_class_init(MockAccountClass *klass)
{
    // function is unused, class functions are defined in C++ code
}

void
xaccAccountBeginEdit (Account *account)
{
    ASSERT_TRUE(GNC_IS_MOCK_ACCOUNT(account));
    ((MockAccount*)account)->beginEdit();
}

void
xaccAccountCommitEdit (Account *account)
{
    ASSERT_TRUE(GNC_IS_MOCK_ACCOUNT(account));
    ((MockAccount*)account)->commitEdit();
}

QofBook *
gnc_account_get_book(const Account *account)
{
    EXPECT_TRUE(GNC_IS_MOCK_ACCOUNT(account));
    if (GNC_IS_MOCK_ACCOUNT(account))
        return ((MockAccount*)account)->getBook();
    else
        return nullptr;
}

gint
xaccAccountForEachTransaction(const Account *acc, TransactionCallback proc,
                              void *data)
{
    EXPECT_TRUE(GNC_IS_MOCK_ACCOUNT(acc));
    if (GNC_IS_MOCK_ACCOUNT(acc))
        return ((MockAccount*)acc)->forEachTransaction(proc, data);
    else
        return 0;
}

GncImportMatchMap *
gnc_account_imap_create_imap (Account *acc)
{
    EXPECT_TRUE(GNC_IS_MOCK_ACCOUNT(acc));
    if (GNC_IS_MOCK_ACCOUNT(acc))
        return ((MockAccount*)acc)->imapCreateImap();
    else
        return nullptr;
}

Account*
gnc_account_imap_find_account (
        GncImportMatchMap *imap,
        const char* category,
        const char *key)
{
    return ((GncMockImportMatchMap*)imap)->findAccount(category, key);
}

void
gnc_account_imap_add_account (
        GncImportMatchMap *imap,
        const char *category,
        const char *key,
        Account *acc)
{
    ((GncMockImportMatchMap*)imap)->addAccount(category, key, acc);
}

Account*
gnc_account_imap_find_account_bayes (
        GncImportMatchMap *imap,
        GList *tokens)
{
    std::vector<const char*> tokenVec;

    for (auto token = tokens; token; token = token->next)
    {
        tokenVec.push_back(static_cast <char const *> (token->data));
    }

    return ((GncMockImportMatchMap*)imap)->findAccountBayes(tokenVec);
}

void
gnc_account_imap_add_account_bayes (
        GncImportMatchMap *imap,
        GList *tokens,
        Account *acc)
{
    std::vector<const char*> tokenVec;

    for (auto token = tokens; token; token = token->next)
    {
        tokenVec.push_back(static_cast <char const *> (token->data));
    }

    ((GncMockImportMatchMap*)imap)->addAccountBayes(tokenVec, acc);
}

