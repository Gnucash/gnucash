#include <gtk/gtk.h>
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <config.h>

#include <gnc-datetime.hpp>

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



/* Tests using fixture ImportBackendTest */

//! Test for function gnc_import_TransInfo_new()
TEST_F(ImportBackendTest, CreateTransInfo)
{
    GncMockImportMatchMap imap(m_import_acc);
    gchar* online_id;

    using namespace testing;

    //qof_instance_get (QOF_INSTANCE (split), "online-id", &online_id, NULL);

    // Define first split
    ON_CALL(*m_trans, getSplit(0))
        .WillByDefault(Return(m_split));
    // define description of the transaction
    ON_CALL(*m_trans, getDescription())
        .WillByDefault(Return("This is the description"));

    // function gnc_import_TransInfo_new() should try to find account using the description from the transaction
    EXPECT_CALL(imap, findAccount(_, StrEq("This is the description")))
        .WillOnce(Return(m_dest_acc));

    // call function to be tested
    GNCImportTransInfo *trans_info = gnc_import_TransInfo_new(m_trans, &imap);

    // check 'trans_info'
    EXPECT_EQ(gnc_import_TransInfo_get_fsplit(trans_info),  m_split);
    EXPECT_EQ(gnc_import_TransInfo_get_destacc(trans_info), m_dest_acc);

    // transaction is not open anymore
    ON_CALL(*m_trans, isOpen())
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
        ON_CALL(*m_prefs, getBool(StrEq(GNC_PREFS_GROUP_IMPORT), StrEq(GNC_PREF_USE_BAYES)))
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

    GncMockImportMatchMap imap(m_import_acc);
    time64 date(GncDateTime(GncDate(2020, 3, 18)));
    struct tm *tm_struct;
    char local_day_of_week[16];

    // get local day of week
    tm_struct = gnc_gmtime(&date);
    qof_strftime(local_day_of_week, sizeof(local_day_of_week), "%A", tm_struct);
    gnc_tm_free(tm_struct);

    // Define first split
    ON_CALL(*m_trans, getSplit(0))
        .WillByDefault(Return(m_split));
    // Transaction has no further splits
    ON_CALL(*m_trans, getSplit(Gt(0)))
        .WillByDefault(Return(nullptr));
    // Define description and memo of first split
    // This transaction is used for testing tokenization of its content.
    // Therefore the description text and the memo should contain
    //   * consecutive separators
    //   * separators at the beginning and end of string
    //   * duplicated tokens within and between description text end memo
    // The token separator is space.
    ON_CALL(*m_trans, getDescription())
        .WillByDefault(Return(" test  tokens within   description  tokens  "));
    ON_CALL(*m_split, getMemo())
        .WillByDefault(Return("  test   the memo test "));
    // Define transaction date
    ON_CALL(*m_trans, getDate())
        .WillByDefault(Return(date));

    // check tokens created from transaction
    EXPECT_CALL(imap, findAccountBayes(AllOf(
            Each(Not(IsEmpty())),                // tokens must not be empty strings
            Each(Not(HasSubstr(" "))),           // tokens must not contain separator
            Not(HasDuplicates()),                // tokens must be unique
            Contains(StrEq(local_day_of_week)),  // tokens must contain local day of week
            Contains(StrEq("description")),      // spot sample
            Contains(StrEq("memo"))              // spot sample
            )))
        .WillOnce(Return(m_dest_acc));

    // call function to be tested
    GNCImportTransInfo *trans_info = gnc_import_TransInfo_new(m_trans, &imap);

    // check 'trans_info'
    EXPECT_EQ(gnc_import_TransInfo_get_fsplit(trans_info),  m_split);
    EXPECT_EQ(gnc_import_TransInfo_get_destacc(trans_info), m_dest_acc);

    // transaction is not open anymore
    ON_CALL(*m_trans, isOpen())
        .WillByDefault(Return(false));

    // delete transaction info
    gnc_import_TransInfo_delete(trans_info);
};
