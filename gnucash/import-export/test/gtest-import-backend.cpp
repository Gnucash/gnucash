#include <gtk/gtk.h>
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <config.h>

extern "C"
{
#include <import-backend.h>
#include <engine-helpers.h>
#include <gnc-ui-util.h>
}

#include "gmock-gnc-prefs.h"
#include "gmock-qofbook.h"
#include "gmock-Account.h"
#include "gmock-Transaction.h"
#include "gmock-Split.h"



/* Global test environment */

class TestEnvironment : public testing::Environment
{
public:
    void SetUp()
    {
        m_book = new QofMockBook;
    };

    void TearDown()
    {
        m_book->free();
    };

    QofMockBook* m_book;
};

testing::Environment* const env = testing::AddGlobalTestEnvironment(new TestEnvironment);



/* mock functions, which can not be mocked by mock classes */

gint
safe_strcasecmp (const gchar * da, const gchar * db)
{
    // use simplified case-sensitive string comparison as mock up
    return g_strcmp0(da, db);
}

const char *
qof_log_prettify (const char *name)
{
    // do nothing
    return name;
}

// this is a slightly modified version of the function from engine-helpers.c
const char *
gnc_get_num_action (const Transaction *trans, const Split *split)
{
    gboolean num_action = qof_book_use_split_action_for_num_field(gnc_get_current_book());

    if (trans && !split)
        return xaccTransGetNum(trans);
    if (split && !trans)
        return xaccSplitGetAction(split);
    if (trans && split)
    {
        if (num_action)
            return xaccSplitGetAction(split);
        else
            return xaccTransGetNum(trans);
    }
    else return NULL;
}

QofBook *
gnc_get_current_book (void)
{
    return ((TestEnvironment*)env)->m_book;
}



class ImportBackendTest : public testing::Test
{
protected:
    void SetUp()
    {
        m_prefs = MockPrefsBackend::getInstance();
        ASSERT_NE(m_prefs, nullptr);
        m_import_acc = new MockAccount();
        m_dest_acc   = new MockAccount();
        m_trans      = new MockTransaction();
        m_split      = new MockSplit();

        using namespace testing;

        // define behaviour of m_import_acc
        ON_CALL(*m_import_acc, getBook())
            .WillByDefault(Return(((TestEnvironment*)env)->m_book));
    }

    void TearDown()
    {
        m_import_acc->free();
        m_dest_acc->free();
        m_trans->free();
        m_split->free();
    }

    MockPrefsBackend* m_prefs;
    MockAccount*      m_import_acc;
    MockAccount*      m_dest_acc;
    MockTransaction*  m_trans;
    MockSplit*        m_split;
};

