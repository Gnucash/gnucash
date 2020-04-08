#include <gtk/gtk.h>
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <config.h>

extern "C"
{
#include <import-backend.h>
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

