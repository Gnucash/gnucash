#include <config.h>

#include "gmock-Account.h"


struct _MockAccountClass
{
    QofInstanceClass parent_class;
};
typedef struct _MockAccountClass MockAccountClass;

G_DEFINE_TYPE(MockAccount, gnc_mockaccount, QOF_TYPE_INSTANCE);

static void
gnc_mockaccount_init (MockAccount *inst)
{
    // function is unused, initialization is done in the MockAccount's C++ constructor
}

static void
gnc_mockaccount_class_init(MockAccountClass *klass)
{
    // function is unused, class functions are defined in C++ code
}


GType gnc_account_get_type(void)
{
    return gnc_mockaccount_get_type();
}

void
xaccAccountBeginEdit (Account *account)
{
    ASSERT_TRUE(GNC_IS_MOCKACCOUNT(account));
    gnc_mockaccount(account)->begin_edit();
}

void
xaccAccountCommitEdit (Account *account)
{
    ASSERT_TRUE(GNC_IS_MOCKACCOUNT(account));
    gnc_mockaccount(account)->commit_edit();
}

QofBook *
gnc_account_get_book(const Account *account)
{
    SCOPED_TRACE("");
    auto mockaccount = gnc_mockaccount(account);
    return mockaccount ? mockaccount->get_book() : nullptr;
}

gint
xaccAccountForEachTransaction(const Account *acc, TransactionCallback proc,
                              void *data)
{
    SCOPED_TRACE("");
    auto mockaccount = gnc_mockaccount(acc);
    return mockaccount ? mockaccount->for_each_transaction(proc, data) : 0;
}

GncImportMatchMap *
gnc_account_imap_create_imap (Account *acc)
{
    SCOPED_TRACE("");
    auto mockaccount = gnc_mockaccount(acc);
    return mockaccount ? mockaccount->create_imap() : nullptr;
}

Account*
gnc_account_imap_find_account (
        GncImportMatchMap *imap,
        const char* category,
        const char *key)
{
    return ((GncMockImportMatchMap*)imap)->find_account(category, key);
}

void
gnc_account_imap_add_account (
        GncImportMatchMap *imap,
        const char *category,
        const char *key,
        Account *acc)
{
    ((GncMockImportMatchMap*)imap)->add_account(category, key, acc);
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

    return ((GncMockImportMatchMap*)imap)->find_account_bayes(tokenVec);
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

    ((GncMockImportMatchMap*)imap)->add_account_bayes(tokenVec, acc);
}

