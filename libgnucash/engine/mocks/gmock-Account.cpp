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
    // function is unused, initialization is done in the MockAccount's constructor
}

static void
gnc_mock_account_class_init(MockAccountClass *klass)
{
    // function is unused, class functions are defined in C++ code
}

void
xaccAccountBeginEdit (Account *account)
{
    g_return_if_fail(GNC_IS_MOCK_ACCOUNT(account));
    ((MockAccount*)account)->beginEdit();
}

void
xaccAccountCommitEdit (Account *account)
{
    g_return_if_fail(GNC_IS_MOCK_ACCOUNT(account));
    ((MockAccount*)account)->commitEdit();
}

QofBook *
gnc_account_get_book(const Account *account)
{
    g_return_val_if_fail(GNC_IS_MOCK_ACCOUNT(account), NULL);
    return ((MockAccount*)account)->getBook();
}

gint
xaccAccountForEachTransaction(const Account *acc, TransactionCallback proc,
                              void *data)
{
    g_return_val_if_fail(GNC_IS_MOCK_ACCOUNT(acc), 0);
    return ((MockAccount*)acc)->forEachTransaction(proc, data);
}

GncImportMatchMap *
gnc_account_imap_create_imap (Account *acc)
{
    g_return_val_if_fail(GNC_IS_MOCK_ACCOUNT(acc), NULL);
    return ((MockAccount*)acc)->imapCreateImap();
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
    // not used at the moment
    ((GncMockImportMatchMap*)imap)->addAccount(category, key, acc);
}

Account*
gnc_account_imap_find_account_bayes (
        GncImportMatchMap *imap,
        GList *tokens)
{
    // \todo use std::list instead of std::vector, since GList is a double-linked list like std::list
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
    // \todo use std::list instead of std::vector, since GList is a double-linked list like std::list
    std::vector<const char*> tokenVec;

    for (auto token = tokens; token; token = token->next)
    {
        tokenVec.push_back(static_cast <char const *> (token->data));
    }

    ((GncMockImportMatchMap*)imap)->addAccountBayes(tokenVec, acc);
}

