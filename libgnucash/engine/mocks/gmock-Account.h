#ifndef GMOCK_ACCOUNT_H
#define GMOCK_ACCOUNT_H

#include <gmock/gmock.h>

#include <Account.h>
#include <AccountP.h>

#include "gmock-qofbook.h"
#include "gmock-gobject.h"


GType gnc_mock_account_get_type(void);

#define GNC_TYPE_MOCK_ACCOUNT   (gnc_mock_account_get_type ())
#define GNC_IS_MOCK_ACCOUNT(o)  (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_MOCK_ACCOUNT))


// mock up for Account
class MockAccount : public Account
{
public:
    MockAccount() {}
    void* operator new(size_t size)
    {
        return mock_g_object_new (GNC_TYPE_MOCK_ACCOUNT, NULL, size);
    }

    // define separate free() function since destructor is protected
    void free()
    {
        delete this;
    }
    void operator delete(void* acc, size_t size)
    {
        mock_g_object_unref(acc, size);
    }

    MOCK_METHOD0(beginEdit, void());
    MOCK_METHOD0(commitEdit, void());
    MOCK_METHOD0(getBook, QofMockBook*());
    MOCK_METHOD2(forEachTransaction, gint(TransactionCallback, void*));
    MOCK_METHOD0(imapCreateImap, GncImportMatchMap*());

protected:
    // Protect destructor to avoid MockAccount objects to be created on stack. MockAccount
    // objects can only be dynamically created, since they are derived from GObject.
    ~MockAccount() {}
};


class GncMockImportMatchMap : public GncImportMatchMap
{
public:
    GncMockImportMatchMap(MockAccount* account)
    {
        g_return_if_fail(GNC_IS_MOCK_ACCOUNT(account));

        acc  = account;
        book = account->getBook();
    };

    MOCK_METHOD2(findAccount, Account *(const char*, const char*));
    MOCK_METHOD3(addAccount, void(const char*, const char*, Account*));
    MOCK_METHOD1(findAccountBayes, Account *(std::vector<const char*>&));
    MOCK_METHOD2(addAccountBayes, void(std::vector<const char*>&, Account*));
};

#endif
