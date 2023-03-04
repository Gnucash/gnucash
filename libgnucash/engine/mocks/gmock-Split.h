#ifndef GMOCK_SPLIT_H
#define GMOCK_SPLIT_H

#include <gmock/gmock.h>

#include <Split.h>
#include <SplitP.h>

#include "gmock-qofbook.h"
#include "gmock-gobject.h"


GType gnc_mocksplit_get_type(void);

#define GNC_TYPE_MOCKSPLIT   (gnc_mocksplit_get_type ())
#define GNC_IS_MOCKSPLIT(o)  (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_MOCKSPLIT))


// mock up for Split
class MockSplit : public Split
{
public:
    MockSplit()
    {
        acc         = nullptr;
        orig_acc    = nullptr;
        parent      = nullptr;
        orig_parent = nullptr;
        lot         = nullptr;

        action      = nullptr;
        memo        = nullptr;
        reconciled  = VREC;
        amount      = gnc_numeric_zero();
        value       = gnc_numeric_zero();

        date_reconciled    = 0;

        balance            = gnc_numeric_zero();
        cleared_balance    = gnc_numeric_zero();
        reconciled_balance = gnc_numeric_zero();
        noclosing_balance  = gnc_numeric_zero();

        gains        = GAINS_STATUS_UNKNOWN;
        gains_split  = nullptr;
    }
    void* operator new(size_t size)
    {
        return mock_g_object_new (GNC_TYPE_MOCKSPLIT, NULL, size);
    }

    // define separate free() function since destructor is protected
    void free()
    {
        delete this;
    }
    void operator delete(void* split, size_t size)
    {
        mock_g_object_unref(split, size);
    }

    MOCK_METHOD0(init, void());
    MOCK_CONST_METHOD0(get_book, QofBook *());
    MOCK_CONST_METHOD0(get_account, Account *());
    MOCK_METHOD1(set_account, void(Account*));
    MOCK_CONST_METHOD0(get_amount, gnc_numeric());
    MOCK_METHOD1(set_amount, void(gnc_numeric));
    MOCK_CONST_METHOD0(get_value, gnc_numeric());
    MOCK_METHOD1(set_value, void(gnc_numeric));
    MOCK_CONST_METHOD0(get_memo, const char *());
    MOCK_METHOD1(set_memo, void(const char *));
    MOCK_CONST_METHOD0(get_reconcile, char());
    MOCK_METHOD1(set_reconcile, void(char));
    MOCK_METHOD1(set_date_reconciled_secs, void(time64));
    MOCK_CONST_METHOD0(get_action, const char *());
    MOCK_CONST_METHOD0(get_other_split, Split *());
    MOCK_CONST_METHOD0(get_parent, Transaction *());
    MOCK_METHOD1(set_parent, void(Transaction*));

protected:
    // Protect destructor to avoid MockSplit objects to be created on stack. MockSplit
    // objects can only be dynamically created, since they are derived from GObject.
    ~MockSplit() {}
};


// type conversion functions
static inline MockSplit*
gnc_mocksplit (Split *split)
{
    if (GNC_IS_MOCKSPLIT(split))
        return static_cast<MockSplit*>(split);
    ADD_FAILURE() << "Expected 'split' to be of type 'MockSplit'";
    return nullptr;
}

static inline const MockSplit*
gnc_mocksplit (const Split *split)
{
    if (GNC_IS_MOCKSPLIT(split))
        return static_cast<const MockSplit*>(split);
    ADD_FAILURE() << "Expected 'split' to be of type 'MockSplit'";
    return nullptr;
}

#endif
