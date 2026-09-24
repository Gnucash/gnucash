/********************************************************************\
 * gtest-import-backend.cpp - Tests for import-backend              *
 *                                                                  *
 * Copyright (c) 2020 Christian Gruber                              *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#include <gtk/gtk.h>
#include <cstring>
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop

#include <gmock/gmock.h>

#include <config.h>

#include <gnc-datetime.hpp>

#include <import-backend.h>
#include <engine-helpers.h>
#include <gnc-ui-util.h>

#include "gmock-gnc-prefs.h"
#include "gmock-qofbook.hpp"
#include "gmock-Account.h"
#include "gmock-Transaction.h"
#include "gmock-Split.hpp"



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

// Fakes from qofinstance.cpp and guid.cpp. The isolated backend test doesn't
// link the engine GUID implementation, but conflict resolution deliberately
// retains the product contract of comparing matched transaction GUIDs.
const GncGUID *
qof_entity_get_guid (gconstpointer entity)
{
    if (!entity)
        return nullptr;
    auto object = G_OBJECT (entity);
    auto guid = static_cast<GncGUID*> (
        g_object_get_data (object, "test-guid"));
    if (!guid)
    {
        guid = g_new0 (GncGUID, 1);
        g_object_set_data_full (object, "test-guid", guid, g_free);
    }
    return guid;
}

gboolean
guid_equal (const GncGUID *first, const GncGUID *second)
{
    return first && second &&
           std::memcmp (first, second, sizeof (GncGUID)) == 0;
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
        g_list_free (m_splitList);
        m_split->free();
    }

    MockPrefsBackend  m_prefs;
    MockAccount*      m_import_acc;
    MockAccount*      m_dest_acc;
    MockTransaction*  m_trans;
    MockSplit*        m_split;
    GList*            m_splitList;
};

static void
set_test_guid (GObject *object, guint8 value)
{
    auto guid = const_cast<GncGUID*> (qof_entity_get_guid (object));
    std::memset (guid, 0, sizeof (GncGUID));
    guid->reserved[0] = value;
}

class MockMatchTarget
{
public:
    MockMatchTarget (guint8 guid, const char *description) :
        trans {new MockTransaction()}, split {new MockSplit()}
    {
        using namespace testing;
        const auto amount = gnc_numeric_create (100, 1);
        ON_CALL(*split, get_amount()).WillByDefault(Return(amount));
        ON_CALL(*split, get_memo()).WillByDefault(Return(nullptr));
        ON_CALL(*split, get_parent()).WillByDefault(Return(trans));
        ON_CALL(*trans, get_date()).WillByDefault(Return(1000));
        ON_CALL(*trans, get_num()).WillByDefault(Return(nullptr));
        ON_CALL(*trans, get_description()).WillByDefault(Return(description));
        set_test_guid (G_OBJECT (trans), guid);
    }

    ~MockMatchTarget ()
    {
        split->free();
        trans->free();
    }

    MockTransaction *trans;
    MockSplit *split;
};

class MockImportedTransaction
{
public:
    MockImportedTransaction (Account *base_account, const char *description) :
        trans {new MockTransaction()}, split {new MockSplit()}
    {
        using namespace testing;
        const auto amount = gnc_numeric_create (100, 1);
        ON_CALL(*trans, get_split(0)).WillByDefault(Return(split));
        ON_CALL(*trans, get_description()).WillByDefault(Return(description));
        ON_CALL(*trans, get_date()).WillByDefault(Return(1000));
        ON_CALL(*trans, get_num()).WillByDefault(Return(nullptr));
        ON_CALL(*trans, is_open()).WillByDefault(Return(false));
        ON_CALL(*split, get_amount()).WillByDefault(Return(amount));
        ON_CALL(*split, get_memo()).WillByDefault(Return(nullptr));
        info = gnc_import_TransInfo_new (trans, base_account);
    }

    ~MockImportedTransaction ()
    {
        gnc_import_TransInfo_delete (info);
        split->free();
        trans->free();
    }

    void add_match (const MockMatchTarget& target)
    {
        split_find_match (info, target.split, 0, 4, 14, 0.0);
    }

    GNCImportTransInfo *info;
    MockTransaction *trans;
    MockSplit *split;
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

TEST_F(ImportBackendTest, DiscardTransInfoDoesNotInspectTransaction)
{
    using namespace testing;

    ON_CALL(*m_trans, get_split(0))
        .WillByDefault(Return(m_split));
    ON_CALL(*m_trans, get_split_list())
        .WillByDefault(Return(m_splitList));
    ON_CALL(*m_trans, get_description())
        .WillByDefault(Return("This is the description"));
    EXPECT_CALL(*m_import_acc, find_account(_, StrEq("This is the description")))
        .WillOnce(Return(m_dest_acc));

    auto trans_info = gnc_import_TransInfo_new (m_trans, m_import_acc);
    EXPECT_CALL(*m_trans, is_open()).Times(0);
    gnc_import_TransInfo_discard (trans_info);
}

TEST_F(ImportBackendTest, RemoveTopMatchWithEmptyListIsSafe)
{
    using namespace testing;

    ON_CALL(*m_trans, get_split(0))
        .WillByDefault(Return(m_split));
    ON_CALL(*m_trans, get_description())
        .WillByDefault(Return("No match"));
    ON_CALL(*m_trans, is_open())
        .WillByDefault(Return(false));

    auto trans_info = gnc_import_TransInfo_new (m_trans, m_import_acc);
    ASSERT_EQ (gnc_import_TransInfo_get_match_list (trans_info), nullptr);

    gnc_import_TransInfo_remove_top_match (trans_info);

    EXPECT_EQ (gnc_import_TransInfo_get_match_list (trans_info), nullptr);
    gnc_import_TransInfo_delete (trans_info);
}

TEST_F(ImportBackendTest, ResolveConflictsKeepsSingleCandidate)
{
    using namespace testing;

    auto existing_trans = new MockTransaction();
    auto existing_split = new MockSplit();
    const auto amount = gnc_numeric_create (100, 1);

    ON_CALL(*m_trans, get_split(0))
        .WillByDefault(Return(m_split));
    ON_CALL(*m_trans, get_description())
        .WillByDefault(Return("Imported"));
    ON_CALL(*m_trans, get_date())
        .WillByDefault(Return(1000));
    ON_CALL(*m_trans, get_num())
        .WillByDefault(Return(nullptr));
    ON_CALL(*m_trans, is_open())
        .WillByDefault(Return(false));
    ON_CALL(*m_split, get_amount())
        .WillByDefault(Return(amount));
    ON_CALL(*m_split, get_memo())
        .WillByDefault(Return(nullptr));
    ON_CALL(*existing_split, get_amount())
        .WillByDefault(Return(amount));
    ON_CALL(*existing_split, get_memo())
        .WillByDefault(Return(nullptr));
    ON_CALL(*existing_split, get_parent())
        .WillByDefault(Return(existing_trans));
    ON_CALL(*existing_trans, get_date())
        .WillByDefault(Return(1000));
    ON_CALL(*existing_trans, get_num())
        .WillByDefault(Return(nullptr));
    ON_CALL(*existing_trans, get_description())
        .WillByDefault(Return("Existing"));

    auto trans_info = gnc_import_TransInfo_new (m_trans, m_import_acc);
    split_find_match (trans_info, existing_split, 0, 4, 14, 0.0);
    ASSERT_EQ (g_list_length (gnc_import_TransInfo_get_match_list (trans_info)), 1u);
    GList *imports = g_list_append (nullptr, trans_info);

    gnc_import_TransInfo_resolve_conflicts (imports);

    EXPECT_EQ (g_list_length (gnc_import_TransInfo_get_match_list (trans_info)), 1u);
    g_list_free (imports);
    gnc_import_TransInfo_delete (trans_info);
    existing_split->free();
    existing_trans->free();
}

TEST_F(ImportBackendTest, ResolveConflictsKeepsLaterHigherScoringImport)
{
    using namespace testing;

    auto better_trans = new MockTransaction();
    auto better_split = new MockSplit();
    auto existing_trans = new MockTransaction();
    auto existing_split = new MockSplit();
    const auto amount = gnc_numeric_create (100, 1);

    ON_CALL(*m_trans, get_split(0))
        .WillByDefault(Return(m_split));
    ON_CALL(*m_trans, get_description())
        .WillByDefault(Return("Lower score"));
    ON_CALL(*m_trans, get_date())
        .WillByDefault(Return(1000));
    ON_CALL(*m_trans, get_num())
        .WillByDefault(Return(nullptr));
    ON_CALL(*m_trans, is_open())
        .WillByDefault(Return(false));
    ON_CALL(*m_split, get_amount())
        .WillByDefault(Return(amount));
    ON_CALL(*m_split, get_memo())
        .WillByDefault(Return(nullptr));

    ON_CALL(*better_trans, get_split(0))
        .WillByDefault(Return(better_split));
    ON_CALL(*better_trans, get_description())
        .WillByDefault(Return("Existing"));
    ON_CALL(*better_trans, get_date())
        .WillByDefault(Return(1000));
    ON_CALL(*better_trans, get_num())
        .WillByDefault(Return(nullptr));
    ON_CALL(*better_trans, is_open())
        .WillByDefault(Return(false));
    ON_CALL(*better_split, get_amount())
        .WillByDefault(Return(amount));
    ON_CALL(*better_split, get_memo())
        .WillByDefault(Return(nullptr));

    ON_CALL(*existing_split, get_amount())
        .WillByDefault(Return(amount));
    ON_CALL(*existing_split, get_memo())
        .WillByDefault(Return(nullptr));
    ON_CALL(*existing_split, get_parent())
        .WillByDefault(Return(existing_trans));
    ON_CALL(*existing_trans, get_date())
        .WillByDefault(Return(1000));
    ON_CALL(*existing_trans, get_num())
        .WillByDefault(Return(nullptr));
    ON_CALL(*existing_trans, get_description())
        .WillByDefault(Return("Existing"));

    auto lower = gnc_import_TransInfo_new (m_trans, m_import_acc);
    auto higher = gnc_import_TransInfo_new (better_trans, m_import_acc);
    split_find_match (lower, existing_split, 0, 4, 14, 0.0);
    split_find_match (higher, existing_split, 0, 4, 14, 0.0);
    ASSERT_LT (gnc_import_MatchInfo_get_probability (
                   static_cast<GNCImportMatchInfo*> (
                       gnc_import_TransInfo_get_match_list (lower)->data)),
               gnc_import_MatchInfo_get_probability (
                   static_cast<GNCImportMatchInfo*> (
                       gnc_import_TransInfo_get_match_list (higher)->data)));
    GList *imports = nullptr;
    imports = g_list_append (imports, lower);
    imports = g_list_append (imports, higher);

    gnc_import_TransInfo_resolve_conflicts (imports);

    EXPECT_EQ (gnc_import_TransInfo_get_match_list (lower), nullptr);
    EXPECT_EQ (g_list_length (gnc_import_TransInfo_get_match_list (higher)), 1u);
    g_list_free (imports);
    gnc_import_TransInfo_delete (lower);
    gnc_import_TransInfo_delete (higher);
    existing_split->free();
    existing_trans->free();
    better_split->free();
    better_trans->free();
}

TEST_F(ImportBackendTest, ResolveConflictsKeepsEarlierHigherScoringImport)
{
    MockMatchTarget existing {1, "Existing"};
    MockImportedTransaction higher {m_import_acc, "Existing"};
    MockImportedTransaction lower {m_import_acc, "Unrelated"};
    higher.add_match (existing);
    lower.add_match (existing);
    GList *imports = nullptr;
    imports = g_list_append (imports, higher.info);
    imports = g_list_append (imports, lower.info);

    gnc_import_TransInfo_resolve_conflicts (imports);

    EXPECT_EQ (g_list_length (gnc_import_TransInfo_get_match_list (higher.info)), 1u);
    EXPECT_EQ (gnc_import_TransInfo_get_match_list (lower.info), nullptr);
    g_list_free (imports);
}

TEST_F(ImportBackendTest, ResolveConflictsKeepsEarlierImportOnTie)
{
    MockMatchTarget existing {2, "Existing"};
    MockImportedTransaction first {m_import_acc, "First unrelated"};
    MockImportedTransaction second {m_import_acc, "Second unrelated"};
    first.add_match (existing);
    second.add_match (existing);
    ASSERT_EQ (gnc_import_MatchInfo_get_probability (
                   static_cast<GNCImportMatchInfo*> (
                       gnc_import_TransInfo_get_match_list (first.info)->data)),
               gnc_import_MatchInfo_get_probability (
                   static_cast<GNCImportMatchInfo*> (
                       gnc_import_TransInfo_get_match_list (second.info)->data)));
    GList *imports = nullptr;
    imports = g_list_append (imports, first.info);
    imports = g_list_append (imports, second.info);

    gnc_import_TransInfo_resolve_conflicts (imports);

    EXPECT_EQ (g_list_length (gnc_import_TransInfo_get_match_list (first.info)), 1u);
    EXPECT_EQ (gnc_import_TransInfo_get_match_list (second.info), nullptr);
    g_list_free (imports);
}

TEST_F(ImportBackendTest, ResolveConflictsRestartsForExposedNextMatch)
{
    MockMatchTarget first_existing {3, "First existing"};
    MockMatchTarget second_existing {4, "Second existing"};
    MockImportedTransaction two_matches {m_import_acc, "Unrelated"};
    MockImportedTransaction first_winner {m_import_acc, "First existing"};
    MockImportedTransaction second_winner {m_import_acc, "Second existing"};

    /* Match insertion prepends. Add the eventual fallback first so that the
     * first-existing transaction is initially selected. */
    two_matches.add_match (second_existing);
    two_matches.add_match (first_existing);
    first_winner.add_match (first_existing);
    second_winner.add_match (second_existing);
    ASSERT_EQ (g_list_length (
                   gnc_import_TransInfo_get_match_list (two_matches.info)), 2u);
    GList *imports = nullptr;
    imports = g_list_append (imports, two_matches.info);
    imports = g_list_append (imports, first_winner.info);
    imports = g_list_append (imports, second_winner.info);

    gnc_import_TransInfo_resolve_conflicts (imports);

    EXPECT_EQ (gnc_import_TransInfo_get_match_list (two_matches.info), nullptr);
    EXPECT_EQ (g_list_length (
                   gnc_import_TransInfo_get_match_list (first_winner.info)), 1u);
    EXPECT_EQ (g_list_length (
                   gnc_import_TransInfo_get_match_list (second_winner.info)), 1u);
    g_list_free (imports);
}



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

TEST_F(ImportBackendBayesTest, gen_probability_pixbuf_test)
{
    auto settings = gnc_import_Settings_new();
    GObject parent;
    auto pixbuf = gen_probability_pixbuf(8, settings, (GtkWidget*)&parent);
    EXPECT_NE(pixbuf, nullptr);
    g_object_unref(pixbuf);
    g_free(settings);
}
