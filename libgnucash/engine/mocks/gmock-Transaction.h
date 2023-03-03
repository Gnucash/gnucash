#ifndef GMOCK_TRANSACTION_H
#define GMOCK_TRANSACTION_H

#include <gmock/gmock.h>

#include <Transaction.h>
#include <TransactionP.h>

#include "gmock-gobject.h"


GType gnc_mocktransaction_get_type(void);

#define GNC_TYPE_MOCKTRANSACTION   (gnc_mocktransaction_get_type ())
#define GNC_IS_MOCKTRANSACTION(o)  (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_MOCKTRANSACTION))


// mock up for Transaction
class MockTransaction : public Transaction
{
public:
    MockTransaction()
    {
        num                 = nullptr;
        description         = nullptr;
        common_currency     = nullptr;
        splits              = nullptr;
        date_entered        = 0;
        date_posted         = 0;
        marker              = 0;
        orig                = nullptr;
    }
    void* operator new(size_t size)
    {
        return mock_g_object_new (GNC_TYPE_MOCKTRANSACTION, NULL, size);
    }

    // define separate free() function since destructor is protected
    void free()
    {
        delete this;
    }
    void operator delete(void* trans, size_t size)
    {
        mock_g_object_unref(trans, size);
    }

    MOCK_METHOD0(begin_edit, void());
    MOCK_METHOD0(commit_edit, void());
    MOCK_CONST_METHOD1(get_split, Split *(int));
    MOCK_CONST_METHOD0(get_split_list, GList*());
    MOCK_CONST_METHOD1(find_split_by_account, Split *(const Account*));
    MOCK_CONST_METHOD0(get_date, time64());
    MOCK_METHOD1(set_date_posted_secs_normalized, void(time64));
    MOCK_CONST_METHOD0(get_description, const char *());
    MOCK_METHOD1(set_description, void(const char*));
    MOCK_CONST_METHOD0(get_currency, gnc_commodity *());
    MOCK_CONST_METHOD0(get_notes, const char *());
    MOCK_METHOD1(set_notes, void(const char*));
    MOCK_CONST_METHOD0(get_imbalance_value, gnc_numeric());
    MOCK_CONST_METHOD0(get_num, const char *());
    MOCK_CONST_METHOD0(is_open, gboolean());
    MOCK_METHOD0(destroy, void());
    MOCK_METHOD0(recordPrice, void());

protected:
    // Protect destructor to avoid MockTransaction objects to be created on stack. MockTransaction
    // objects can only be dynamically created, since they are derived from GObject.
    ~MockTransaction() {}
};


// type conversion functions
static inline MockTransaction*
gnc_mocktransaction (Transaction *trans)
{
    if (GNC_IS_MOCKTRANSACTION(trans))
        return static_cast<MockTransaction*>(trans);
    ADD_FAILURE() << "Expected 'trans' to be of type 'MockTransaction'";
    return nullptr;
}

static inline const MockTransaction*
gnc_mocktransaction (const Transaction *trans)
{
    if (GNC_IS_MOCKTRANSACTION(trans))
        return static_cast<const MockTransaction*>(trans);
    ADD_FAILURE() << "Expected 'trans' to be of type 'MockTransaction'";
    return nullptr;
}

#endif
