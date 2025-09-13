/********************************************************************\
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
 *                                                                  *
\********************************************************************/
#include <glib.h>

#include <config.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <cashobjects.h>
#include <TransLog.h>
#include <gnc-engine.h>
#include <gnc-lot.h>
#include <gnc-prefs.h>
#include <Account.hpp>
#include <gnc-datetime.hpp>
#include <gnc-uri-utils.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop
#include <unittest-support.h>

#include "../gnc-backend-xml.h"
#include "../io-gncxml-v2.h"

#define GNC_LIB_NAME "gncmod-backend-xml"
#define GNC_LIB_REL_PATH "xml"

class LoadFile : public testing::Test
{
public:
    static void SetUpTestSuite ()
    {
        g_setenv ("GNC_UNINSTALLED", "1", TRUE);
        qof_init ();
        cashobjects_register ();
        ASSERT_TRUE(qof_load_backend_library (GNC_LIB_REL_PATH, GNC_LIB_NAME)) << "loading gnc-backend-xml GModule failed";
        xaccLogDisable ();
    }

    static void TearDownTestSuite () { qof_close (); }
};

static QofBook*
session_load (QofSession* session, const char* filename)
{
    if (!session || !filename) return nullptr;
    auto url = gnc_uri_normalize_uri (filename, FALSE);

    qof_session_begin (session, url, SESSION_READ_ONLY);
    g_free (url);

    if (qof_session_get_error(session) != 0)
    {
        std::cerr << "Session begin failed: " << qof_session_get_error_message(session);
        return nullptr;
    }

    qof_session_load(session, nullptr);
    if (qof_session_get_error(session) != 0)
    {
        std::cerr << "Session begin failed: " << qof_session_get_error_message(session);
        return nullptr;
    }

    return qof_session_get_book(session);
}

TEST_F(LoadFile, LoadAndVerifyKVP)
{
    auto location{g_getenv ("GNC_TEST_FILES")};
    if (!location)
        location = "test-files/load-save";
    auto filename{"xml-datafile.gnucash"};
    std::shared_ptr<gchar> to_open{g_build_filename (location, filename, (gchar*)nullptr), g_free};
    std::shared_ptr<QofSession> session{qof_session_new(qof_book_new()), qof_session_destroy};
    auto book{session_load (session.get(), to_open.get())};
    ASSERT_NE(book, nullptr);

    auto bank_acct = gnc_account_lookup_by_name (gnc_book_get_root_account (book), "Bank");
    ASSERT_TRUE (bank_acct != nullptr);

    // THE FOLLOWING TESTS BANK ACCOUNT
    const auto& bank_splitlist = xaccAccountGetSplits (bank_acct);
    ASSERT_EQ (bank_splitlist.size(), static_cast<uint32_t>(4));

    // first split is from a regular transaction
    auto bank_reg_split{bank_splitlist[0]};
    auto bank_reg_txn{xaccSplitGetParent(bank_reg_split)};
    EXPECT_STREQ (xaccTransGetDescription (bank_reg_txn), "income");
    EXPECT_STREQ (xaccTransGetNotes (bank_reg_txn), "notes");
    EXPECT_STREQ (xaccTransGetNum (bank_reg_txn), "num");
    EXPECT_STREQ (xaccTransGetDocLink (bank_reg_txn), "https://www.gnucash.org/");
    EXPECT_TRUE (gnc_numeric_equal (xaccSplitGetAmount (bank_reg_split), gnc_numeric_create (200, 1)));
    EXPECT_TRUE (gnc_numeric_equal (xaccSplitGetValue (bank_reg_split), gnc_numeric_create (200, 1)));
    auto gdate = xaccTransGetDatePostedGDate (bank_reg_txn);
    EXPECT_EQ (g_date_get_day (&gdate), static_cast<unsigned>(1));
    EXPECT_EQ (g_date_get_month (&gdate), static_cast<unsigned>(1));
    EXPECT_EQ (g_date_get_year (&gdate), static_cast<unsigned>(2025));
    EXPECT_EQ (GncDateTime(xaccTransGetDate(bank_reg_txn)).format_iso8601(), "2025-01-01 10:59:00");
    EXPECT_EQ (xaccTransGetTxnType (bank_reg_txn), TXN_TYPE_NONE);
    EXPECT_FALSE (xaccTransGetIsClosingTxn (bank_reg_txn));
    EXPECT_STREQ (xaccTransGetReadOnly (bank_reg_txn), nullptr);

    // 2nd split is a payment split
    auto bank_pmt_split{bank_splitlist[1]};
    auto bank_pmt_txn{xaccSplitGetParent(bank_pmt_split)};
    EXPECT_STREQ (xaccTransGetDescription (bank_pmt_txn), "customer-name");
    EXPECT_STREQ (xaccTransGetNotes (bank_pmt_txn), NULL);
    EXPECT_STREQ (xaccTransGetNum (bank_pmt_txn), "pmt-num");
    EXPECT_STREQ (xaccTransGetDocLink (bank_pmt_txn), nullptr);
    EXPECT_EQ (GncDateTime(xaccTransGetDate(bank_pmt_txn)).format_iso8601(), "2025-02-12 10:59:00");
    gdate = xaccTransGetDatePostedGDate (bank_pmt_txn);
    EXPECT_EQ (g_date_get_day (&gdate), static_cast<unsigned>(12));
    EXPECT_EQ (g_date_get_month (&gdate), static_cast<unsigned>(2));
    EXPECT_EQ (g_date_get_year (&gdate), static_cast<unsigned>(2025));
    EXPECT_EQ (xaccTransGetTxnType (bank_pmt_txn), TXN_TYPE_PAYMENT);
    EXPECT_TRUE (gnc_numeric_equal (xaccSplitGetAmount (bank_pmt_split), gnc_numeric_create (194, 100)));
    EXPECT_TRUE (gnc_numeric_equal (xaccSplitGetValue (bank_pmt_split), gnc_numeric_create (194, 100)));
    EXPECT_STREQ (xaccTransGetReadOnly (bank_pmt_txn), nullptr);
    EXPECT_STREQ (xaccTransGetVoidReason (bank_pmt_txn), nullptr);

    // 3nd split is a voided split
    auto bank_voided_split{bank_splitlist[2]};
    auto bank_voided_txn{xaccSplitGetParent(bank_voided_split)};
    EXPECT_STREQ (xaccTransGetDescription (bank_voided_txn), "another income voided");
    EXPECT_STREQ (xaccTransGetNotes (bank_voided_txn), "Voided transaction");
    EXPECT_STREQ (xaccTransGetNum (bank_voided_txn), "");
    EXPECT_STREQ (xaccTransGetDocLink (bank_voided_txn), nullptr);
    EXPECT_EQ (GncDateTime(xaccTransGetDate(bank_voided_txn)).format_iso8601(), "2025-03-01 10:59:00");
    EXPECT_EQ (xaccTransGetTxnType (bank_voided_txn), TXN_TYPE_NONE);
    EXPECT_TRUE (gnc_numeric_equal (xaccSplitGetAmount (bank_voided_split), gnc_numeric_create (0, 100)));
    EXPECT_TRUE (gnc_numeric_equal (xaccSplitGetValue (bank_voided_split), gnc_numeric_create (0, 100)));
    EXPECT_STREQ (xaccTransGetReadOnly (bank_voided_txn), "Transaction Voided");
    EXPECT_STREQ (xaccTransGetVoidReason (bank_voided_txn), "cancelled");

    // 4th split is the 2nd payment reversal txn, reversed
    auto bank_pmt_rev_split{bank_splitlist[3]};
    auto bank_pmt_rev_txn{xaccSplitGetParent(bank_pmt_rev_split)};
    EXPECT_STREQ (xaccTransGetDescription (bank_pmt_rev_txn), "customer-name");
    EXPECT_STREQ (xaccTransGetNotes (bank_pmt_rev_txn), nullptr);
    EXPECT_STREQ (xaccTransGetNum (bank_pmt_rev_txn), "pmt-num");
    EXPECT_STREQ (xaccTransGetDocLink (bank_pmt_rev_txn), nullptr);
    EXPECT_EQ (GncDateTime(xaccTransGetDate(bank_pmt_rev_txn)).format_iso8601(), "2025-04-01 10:59:00");
    EXPECT_EQ (xaccTransGetTxnType (bank_pmt_rev_txn), TXN_TYPE_PAYMENT);
    EXPECT_EQ (xaccTransGetReversedBy (bank_pmt_txn), bank_pmt_rev_txn);
    EXPECT_TRUE (gnc_numeric_equal (xaccSplitGetAmount (bank_pmt_rev_split), gnc_numeric_create (-194, 100)));
    EXPECT_TRUE (gnc_numeric_equal (xaccSplitGetValue (bank_pmt_rev_split), gnc_numeric_create (-194, 100)));
    EXPECT_STREQ (xaccTransGetReadOnly (bank_pmt_rev_txn), nullptr);
    EXPECT_STREQ (xaccTransGetVoidReason (bank_pmt_rev_txn), nullptr);

    // THE FOLLOWING TESTS AR ACCOUNT
    auto AR_acct{gnc_account_lookup_by_name (gnc_book_get_root_account (book), "AReceivable")};
    ASSERT_TRUE (AR_acct != nullptr);

    const auto& AR_splitlist{xaccAccountGetSplits (AR_acct)};
    ASSERT_EQ (AR_splitlist.size(), static_cast<uint32_t>(3));

    // 1st split is invoice posting txn
    auto inv_post_txn{xaccSplitGetParent(AR_splitlist[0])};
    EXPECT_STREQ (xaccTransGetDescription (inv_post_txn), "customer-name");
    EXPECT_STREQ (xaccTransGetNotes (inv_post_txn), nullptr);
    EXPECT_STREQ (xaccTransGetNum (inv_post_txn), "000001");
    EXPECT_EQ (GncDateTime(xaccTransGetDate(inv_post_txn)).format_iso8601(), "2025-02-01 10:59:00");
    EXPECT_EQ (xaccTransGetTxnType (inv_post_txn), TXN_TYPE_INVOICE);
    EXPECT_EQ (GncDateTime(xaccTransRetDateDue(inv_post_txn)).format_iso8601(), "2025-02-14 10:59:00");
    EXPECT_FALSE (xaccTransGetIsClosingTxn (inv_post_txn));
    EXPECT_STREQ (xaccTransGetReadOnly (inv_post_txn), "Generated from an invoice. Try unposting the invoice.");

    // test invoice lot properties
    auto inv_lot{xaccSplitGetLot (AR_splitlist[0])};
    EXPECT_STREQ (gnc_lot_get_title (inv_lot), "Invoice 000001");
    EXPECT_STREQ (gnc_lot_get_notes (inv_lot), nullptr);

    // 2nd split is from payment txn, should be identical to bank payment txn
    auto inv_pmt_txn{xaccSplitGetParent(AR_splitlist[1])};
    EXPECT_EQ (inv_pmt_txn, bank_pmt_txn);

    // THE FOLLOWING TESTS CLOSING ACCOUNT
    auto close_acct{gnc_account_lookup_by_name (gnc_book_get_root_account (book), "Closing")};
    ASSERT_TRUE (close_acct != nullptr);

    const auto& close_splitlist{xaccAccountGetSplits (close_acct)};
    ASSERT_EQ (close_splitlist.size(), static_cast<uint32_t>(1));

    // 3rd split is a closing txn
    auto closing_txn{xaccSplitGetParent(close_splitlist[0])};
    EXPECT_STREQ (xaccTransGetDescription (closing_txn), "Closing Txn");
    EXPECT_STREQ (xaccTransGetNotes (closing_txn), nullptr);
    EXPECT_STREQ (xaccTransGetNum (closing_txn), "");
    EXPECT_EQ (xaccTransGetTxnType (closing_txn), TXN_TYPE_NONE);
    EXPECT_EQ (GncDateTime(xaccTransGetDate(closing_txn)).format_iso8601(), "2025-03-01 10:59:00");
    EXPECT_EQ (GncDateTime(xaccTransRetDateDue(closing_txn)).format_iso8601(), "2025-03-01 10:59:00");
    EXPECT_TRUE (xaccTransGetIsClosingTxn (closing_txn));
    EXPECT_STREQ (xaccTransGetReadOnly (closing_txn), nullptr);

    qof_session_end(session.get());
}
