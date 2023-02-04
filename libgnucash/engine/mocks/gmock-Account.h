#ifndef GMOCK_ACCOUNT_H
#define GMOCK_ACCOUNT_H

#include <gmock/gmock.h>

#include <Account.h>
#include <AccountP.h>
#include <qofbook.h>

#include "gmock-gobject.h"


GType gnc_mockaccount_get_type(void);

#define GNC_TYPE_MOCKACCOUNT   (gnc_mockaccount_get_type ())
#define GNC_IS_MOCKACCOUNT(o)  (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_MOCKACCOUNT))


// mock up for Account
class MockAccount : public Account
{
public:
    /* note: don't use default constructor instead of empty constructor, since
     * it does zero initialization, which would overwrite GObject
     * initialization, which is already done in the new operator. */
    MockAccount() {}
    void* operator new(size_t size)
    {
        return mock_g_object_new (GNC_TYPE_MOCKACCOUNT, NULL, size);
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

    MOCK_METHOD0(begin_edit, void());
    MOCK_METHOD0(commit_edit, void());
    MOCK_CONST_METHOD0(get_book, QofBook*());
    MOCK_CONST_METHOD0(get_commodity, gnc_commodity*());
    MOCK_CONST_METHOD2(for_each_transaction, gint(TransactionCallback, void*));
    MOCK_CONST_METHOD0(xaccAccountGetSplitList, SplitList*());
    MOCK_METHOD2(find_account, Account *(const char*, const char*));
    MOCK_METHOD3(add_account, void(const char*, const char*, Account*));
    MOCK_METHOD1(find_account_bayes, Account *(std::vector<const char*>&));
    MOCK_METHOD2(add_account_bayes, void(std::vector<const char*>&, Account*));

protected:
    /* Protect destructor to avoid MockAccount objects to be created on stack. MockAccount
     * objects can only be dynamically created, since they are derived from GObject. */
    ~MockAccount() {}
};


// type conversion functions
static inline MockAccount*
gnc_mockaccount (Account *account)
{
    if (GNC_IS_MOCKACCOUNT(account))
        return static_cast<MockAccount*>(account);
    ADD_FAILURE() << "Expected 'account' to be of type 'MockAccount'";
    return nullptr;
}

static inline const MockAccount*
gnc_mockaccount (const Account *account)
{
    if (GNC_IS_MOCKACCOUNT(account))
        return static_cast<const MockAccount*>(account);
    ADD_FAILURE() << "Expected 'account' to be of type 'MockAccount'";
    return nullptr;
}

#endif
