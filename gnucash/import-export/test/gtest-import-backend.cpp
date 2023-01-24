#include <gtk/gtk.h>
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <config.h>

#include <gnc-datetime.hpp>

#include <import-backend.h>
#include <engine-helpers.h>
#include <gnc-ui-util.h>

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



/* required fake functions from engine sources, which should not be linked to the test application */

// fake function from qofutil.cpp
gint
safe_strcasecmp (const gchar * da, const gchar * db)
{
    // use simplified case-sensitive string comparison as mock up
    return g_strcmp0(da, db);
}

// fake function from qoflog.cpp
const char *
qof_log_prettify (const char *name)
{
    // do nothing
    return name;
}

// Fake qof_log_check suppresses logging.
gboolean
qof_log_check(QofLogModule log_module, QofLogLevel log_level)
{
    return FALSE;
}

// fake function from engine-helpers.c
// this is a slightly modified version of the original function
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

// fake function from gnc-commodity.c
// this is a simplified version of the original function
gboolean
gnc_commodity_equiv(const gnc_commodity * a, const gnc_commodity * b)
{
    if (a == b) return TRUE;
    if (!a || !b) return FALSE;

    return TRUE;
}


/* required fake functions from app-utils sources, which should not be linked to the test application */

// fake function from gnc-ui-util.c
QofBook *
gnc_get_current_book (void)
{
    return ((TestEnvironment*)env)->m_book;
}



/* GMock MATCHERS */

// GMock MATCHER to check for duplicates in containers
MATCHER(HasDuplicates, std::string("has ") + std::string(negation ? "no " : "") + std::string("duplicated elements"))
{
    bool ret = false;

    for (auto e : arg)
    {
        if (std::count(arg.begin(), arg.end(), e) > 1)
        {
            ret = true;
            break;
        }
    }

    return ret;
}



// Test fixture for tests without bayesian matching
class ImportBackendTest : public testing::Test
{
protected:
    void SetUp()
    {
        gmock_gnc_prefs_set_backend(&m_prefs);
        m_import_acc = new MockAccount();
        m_dest_acc   = new MockAccount();
        m_trans      = new MockTransaction();
        m_split      = new MockSplit();
        m_splitList  = NULL;
        m_splitList  = g_list_prepend (m_splitList, m_split);

        using namespace testing;

        // define behaviour of m_import_acc
        ON_CALL(*m_import_acc, get_book())
            .WillByDefault(Return(((TestEnvironment*)env)->m_book));
    }

    void TearDown()
    {
        m_import_acc->free();
        m_dest_acc->free();
        m_trans->free();
        m_split->free();
    }

    MockPrefsBackend  m_prefs;
    MockAccount*      m_import_acc;
    MockAccount*      m_dest_acc;
    MockTransaction*  m_trans;
    MockSplit*        m_split;
    GList*            m_splitList;
};



/* Tests using fixture ImportBackendTest */

//! Test for function gnc_import_TransInfo_new()
TEST_F(ImportBackendTest, CreateTransInfo)
{

    using namespace testing;

    //qof_instance_get (QOF_INSTANCE (split), "online-id", &online_id, NULL);

    // Define first split
    ON_CALL(*m_trans, get_split(0))
        .WillByDefault(Return(m_split));
    ON_CALL(*m_trans, get_split_list())
        .WillByDefault(Return(m_splitList));
    // define description of the transaction
    ON_CALL(*m_trans, get_description())
        .WillByDefault(Return("This is the description"));

    // function gnc_import_TransInfo_new() should try to find account using the description from the transaction
    EXPECT_CALL(*m_import_acc, find_account(_, StrEq("This is the description")))
        .WillOnce(Return(m_dest_acc));

    // call function to be tested
    GNCImportTransInfo *trans_info = gnc_import_TransInfo_new(m_trans, m_import_acc);

    // check 'trans_info'
    EXPECT_EQ(gnc_import_TransInfo_get_fsplit(trans_info),  m_split);
    EXPECT_EQ(gnc_import_TransInfo_get_destacc(trans_info), m_dest_acc);

    // transaction is not open anymore
    ON_CALL(*m_trans, is_open())
        .WillByDefault(Return(false));

    // delete transaction info
    gnc_import_TransInfo_delete(trans_info);
};



// Test fixture for tests with bayesian matching
class ImportBackendBayesTest : public ImportBackendTest
{
protected:
    void SetUp()
    {
        ImportBackendTest::SetUp();

        using namespace testing;

        // set bayesian import matching in preferences
        ON_CALL(m_prefs, get_bool(StrEq(GNC_PREFS_GROUP_IMPORT), StrEq(GNC_PREF_USE_BAYES)))
            .WillByDefault(Return(true));
    }

    void TearDown()
    {
        ImportBackendTest::TearDown();
    };
};



/* Tests using fixture ImportBackendBayesTest */

//! Test for function gnc_import_TransInfo_new()
TEST_F(ImportBackendBayesTest, CreateTransInfo)
{
    using namespace testing;

    time64 date(GncDateTime(GncDate(2020, 3, 18)));
    struct tm *tm_struct;
    char local_day_of_week[16];

    // get local day of week
    tm_struct = gnc_gmtime(&date);
    qof_strftime(local_day_of_week, sizeof(local_day_of_week), "%A", tm_struct);
    gnc_tm_free(tm_struct);

    // Define first split
    ON_CALL(*m_trans, get_split(0))
        .WillByDefault(Return(m_split));
    ON_CALL(*m_trans, get_split_list())
        .WillByDefault(Return(m_splitList));
    // Transaction has no further splits
    ON_CALL(*m_trans, get_split(Gt(0)))
        .WillByDefault(Return(nullptr));
    // Define description and memo of first split
    // This transaction is used for testing tokenization of its content.
    // Therefore the description text and the memo should contain
    //   * consecutive separators
    //   * separators at the beginning and end of string
    //   * duplicated tokens within and between description text end memo
    // The token separator is space.
    ON_CALL(*m_trans, get_description())
        .WillByDefault(Return(" test  tokens within   description  tokens  "));
    ON_CALL(*m_split, get_memo())
        .WillByDefault(Return("  test   the memo test "));
    // Define transaction date
    ON_CALL(*m_trans, get_date())
        .WillByDefault(Return(date));

    // check tokens created from transaction
    EXPECT_CALL(*m_import_acc, find_account_bayes(AllOf(
            Each(Not(StrEq(""))),                // tokens must not be empty strings
            Each(Not(HasSubstr(" "))),           // tokens must not contain separator
            Not(HasDuplicates()),                // tokens must be unique
            Contains(StrEq(local_day_of_week)),  // tokens must contain local day of week
            Contains(StrEq("description")),      // spot sample
            Contains(StrEq("memo"))              // spot sample
            )))
        .WillOnce(Return(m_dest_acc));

    // call function to be tested
    GNCImportTransInfo *trans_info = gnc_import_TransInfo_new(m_trans, m_import_acc);

    // check 'trans_info'
    EXPECT_EQ(gnc_import_TransInfo_get_fsplit(trans_info),  m_split);
    EXPECT_EQ(gnc_import_TransInfo_get_destacc(trans_info), m_dest_acc);

    // transaction is not open anymore
    ON_CALL(*m_trans, is_open())
        .WillByDefault(Return(false));

    // delete transaction info
    gnc_import_TransInfo_delete(trans_info);
};
