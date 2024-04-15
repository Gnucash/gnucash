/********************************************************************
 * test_qofbook.c: GLib g_test test suite for qofbook.		    *
 * Copyright 2012 Christian Stimming <christian@cstimming.de>       *
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
#include <config.h>
#include <string.h>
#include <glib.h>
#include <qof.h>
#include <unittest-support.h>
#include "../gncInvoice.h"
#include "../Transaction.h"

static const gchar *suitename = "/engine/gncInvoice";
void test_suite_gncInvoice ( void );

typedef struct
{
    gboolean is_cn;
    gboolean is_cust_doc;
    gnc_numeric quantity;
    gnc_numeric price;
} InvoiceData;

typedef struct
{
    QofBook *book;
    Account *account;
    Account *account2;
    GncOwner owner;
    GncCustomer *customer;
    GncVendor *vendor;
    gnc_commodity *commodity;

    GncInvoice* invoice;
    GncInvoice* invoice2;
    Transaction *trans;
    Transaction *trans2;
} Fixture;

static void
setup( Fixture *fixture, gconstpointer pData )
{
    const InvoiceData *data = (InvoiceData*) pData;

    fixture->book = qof_book_new();

    fixture->account = xaccMallocAccount(fixture->book);
    fixture->account2 = xaccMallocAccount(fixture->book);
    fixture->commodity = gnc_commodity_new(fixture->book, "foo", "bar", "xy", "xy", 100);
    xaccAccountSetCommodity(fixture->account, fixture->commodity);
    xaccAccountSetCommodity(fixture->account2, fixture->commodity);

    if (data->is_cust_doc)
    {
        xaccAccountSetType (fixture->account2, ACCT_TYPE_RECEIVABLE);
        fixture->customer = gncCustomerCreate(fixture->book);
        gncOwnerInitCustomer(&fixture->owner, fixture->customer);
    }
    else
    {
        xaccAccountSetType (fixture->account2, ACCT_TYPE_PAYABLE);
        fixture->vendor = gncVendorCreate(fixture->book);
        gncOwnerInitVendor(&fixture->owner, fixture->vendor);
    }

    fixture->invoice = gncInvoiceCreate(fixture->book);
    fixture->invoice2 = NULL;
    fixture->trans2 = NULL;
}

static void
teardown( Fixture *fixture, gconstpointer pData )
{
    const InvoiceData *data = (InvoiceData*) pData;

    gncInvoiceBeginEdit(fixture->invoice);
    gncInvoiceDestroy(fixture->invoice);

    xaccAccountBeginEdit(fixture->account);
    xaccAccountDestroy(fixture->account);
    xaccAccountBeginEdit(fixture->account2);
    xaccAccountDestroy(fixture->account2);

    if (data->is_cust_doc)
    {
        gncCustomerBeginEdit(fixture->customer);
        gncCustomerDestroy(fixture->customer);
    }
    else
    {
        gncVendorBeginEdit(fixture->vendor);
        gncVendorDestroy(fixture->vendor);
    }

    gnc_commodity_destroy(fixture->commodity);

    qof_book_destroy( fixture->book );
};

static void
setup_with_invoice( Fixture *fixture, gconstpointer pData )
{
    const InvoiceData *data = (InvoiceData*) pData;

    time64 ts1 = gnc_time(NULL);
    time64 ts2 = ts1;
    const char *desc = "Test description";
    GncEntry *entry = NULL;

    setup(fixture, pData);

    fixture->invoice = gncInvoiceCreate(fixture->book);
    gncInvoiceSetCurrency(fixture->invoice, fixture->commodity);
    gncInvoiceSetOwner(fixture->invoice, &fixture->owner);

    entry = gncEntryCreate(fixture->book);
    gncEntrySetDate (entry, ts1);
    gncEntrySetDateEntered (entry, ts1);
    gncEntrySetDescription (entry, desc);
    gncEntrySetDocQuantity (entry, data->quantity, data->is_cn);

    if (data->is_cust_doc)
    {
        gncEntrySetInvAccount(entry, fixture->account);
        gncInvoiceAddEntry (fixture->invoice, entry);
    }
    else
    {
        gncEntrySetBillAccount(entry, fixture->account);
        gncBillAddEntry(fixture->invoice, entry);
    }

    fixture->trans = gncInvoicePostToAccount(fixture->invoice, fixture->account2, ts1, ts2, "memo", TRUE, FALSE);
}


static void
setup_with_invoice_and_payment (Fixture *fixture, gconstpointer pData)
{
    Split *split;
    GNCLot *lot;
    gnc_numeric amt = gnc_numeric_create (1000, 100);

    /* 1. create invoice */
    setup_with_invoice (fixture, pData);

    /* 2. create payment */
    fixture->trans2 = xaccMallocTransaction (fixture->book);
    lot = gncInvoiceGetPostedLot (fixture->invoice);

    xaccTransBeginEdit (fixture->trans2);
    xaccTransSetCurrency (fixture->trans2, fixture->commodity);

    /* This split will balance the invoice lot */
    split = xaccMallocSplit (fixture->book);
    xaccSplitSetParent (split, fixture->trans2);
    xaccAccountBeginEdit (fixture->account2);
    xaccSplitSetAccount (split, fixture->account2);
    xaccSplitSetValue (split, gnc_numeric_neg (amt));
    xaccSplitSetAmount (split, gnc_numeric_neg (amt));
    xaccSplitSetLot (split, lot);

    /* bank split will balance the transaction */
    split = xaccMallocSplit (fixture->book);
    xaccSplitSetParent (split, fixture->trans2);
    xaccAccountBeginEdit (fixture->account);
    xaccSplitSetAccount (split, fixture->account);
    xaccSplitSetValue (split, amt);
    xaccSplitSetAmount (split, amt);

    xaccTransCommitEdit (fixture->trans2);
    xaccAccountCommitEdit (fixture->account);
    xaccAccountCommitEdit (fixture->account2);

    gncInvoiceAutoApplyPayments (fixture->invoice);
}

static void
setup_with_invoice_and_CN (Fixture *fixture, gconstpointer pData)
{
    time64 ts1 = gnc_time(NULL);
    time64 ts2 = ts1;
    const char *desc = "Test description";
    GncEntry *entry = NULL;
    Split *split;
    GNCLot *lot1, *lot2;
    gnc_numeric amt = gnc_numeric_create (1000, 100);

    setup (fixture, pData);

    /* 1. invoice */
    fixture->invoice = gncInvoiceCreate (fixture->book);
    gncInvoiceSetCurrency (fixture->invoice, fixture->commodity);
    gncInvoiceSetOwner (fixture->invoice, &fixture->owner);

    entry = gncEntryCreate(fixture->book);
    gncEntrySetDate (entry, ts1);
    gncEntrySetDateEntered (entry, ts1);
    gncEntrySetDescription (entry, desc);
    gncEntrySetDocQuantity (entry, amt, FALSE);
    gncEntrySetBillAccount (entry, fixture->account);
    gncBillAddEntry (fixture->invoice, entry);

    gncInvoicePostToAccount (fixture->invoice, fixture->account2, ts1, ts2, "memo", TRUE, FALSE);

    /* 2. CN */
    fixture->invoice2 = gncInvoiceCreate (fixture->book);
    gncInvoiceSetCurrency (fixture->invoice2, fixture->commodity);
    gncInvoiceSetOwner (fixture->invoice2, &fixture->owner);

    entry = gncEntryCreate(fixture->book);
    gncEntrySetDate (entry, ts1);
    gncEntrySetDateEntered (entry, ts1);
    gncEntrySetDescription (entry, desc);
    gncEntrySetDocQuantity (entry, amt, TRUE);
    gncEntrySetInvAccount(entry, fixture->account);
    gncInvoiceAddEntry (fixture->invoice2, entry);

    gncInvoicePostToAccount (fixture->invoice2, fixture->account2, ts1, ts2, "memo", TRUE, FALSE);

    /* 3. now create the LL txn linking Invoice and CN */
    lot1 = gncInvoiceGetPostedLot (fixture->invoice);
    lot2 = gncInvoiceGetPostedLot (fixture->invoice2);
    fixture->trans2 = xaccMallocTransaction (fixture->book);

    xaccTransBeginEdit (fixture->trans2);
    xaccTransSetCurrency (fixture->trans2, fixture->commodity);
    xaccAccountBeginEdit (fixture->account2);

    /* This split will balance the invoice */
    split = xaccMallocSplit (fixture->book);
    xaccSplitSetParent (split, fixture->trans2);
    xaccSplitSetAccount (split, fixture->account2);
    xaccSplitSetValue (split, gnc_numeric_neg (amt));
    xaccSplitSetAmount (split, gnc_numeric_neg (amt));
    xaccSplitSetLot (split, lot1);

    /* This split will balance the CN*/
    split = xaccMallocSplit (fixture->book);
    xaccSplitSetParent (split, fixture->trans2);
    xaccSplitSetAccount (split, fixture->account2);
    xaccSplitSetValue (split, amt);
    xaccSplitSetAmount (split, amt);
    xaccSplitSetLot (split, lot2);

    xaccTransCommitEdit (fixture->trans2);
    xaccAccountCommitEdit (fixture->account2);
}

static void
teardown_with_invoice( Fixture *fixture, gconstpointer pData )
{
    if (fixture->trans2)
        xaccTransDestroy (fixture->trans2);

    gncInvoiceUnpost(fixture->invoice, TRUE);
    gncInvoiceRemoveEntries (fixture->invoice);

    if (fixture->invoice2)
    {
        gncInvoiceUnpost(fixture->invoice2, TRUE);
        gncInvoiceRemoveEntries (fixture->invoice2);
    }

    teardown(fixture, pData);
}


static void
test_invoice_post ( Fixture *fixture, gconstpointer pData )
{
    time64 ts1 = gnc_time(NULL);
    time64 ts2 = ts1;
    g_assert_true(fixture->invoice);
    g_assert_true(!gncInvoiceGetIsCreditNote(fixture->invoice));
    g_assert_true(gncInvoiceGetActive(fixture->invoice));
    g_assert_true(gncInvoiceGetPostedAcc(fixture->invoice) == NULL);

    gncInvoiceSetCurrency(fixture->invoice, fixture->commodity);

    gncInvoiceSetOwner(fixture->invoice, &fixture->owner);

    g_test_message( "Will now post the invoice" );
    g_assert_true(!gncInvoiceIsPosted(fixture->invoice));
    gncInvoicePostToAccount(fixture->invoice, fixture->account, ts1, ts2, "memo", TRUE, FALSE);
    g_assert_true(gncInvoiceIsPosted(fixture->invoice));

    gncInvoiceUnpost(fixture->invoice, TRUE);
    g_assert_true(!gncInvoiceIsPosted(fixture->invoice));
}


static void
test_invoice_doclink ( Fixture *fixture, gconstpointer pData )
{
    GncInvoice* inv = fixture->invoice;

    g_assert_cmpstr (gncInvoiceGetDocLink (inv), ==, NULL);

    gncInvoiceSetDocLink (inv, "doc");
    g_assert_cmpstr (gncInvoiceGetDocLink (inv), ==, "doc");

    gncInvoiceSetDocLink (inv, "unset");
    g_assert_cmpstr (gncInvoiceGetDocLink (inv), ==, "unset");

    gncInvoiceSetDocLink (inv, "");
    g_assert_cmpstr (gncInvoiceGetDocLink (inv), ==, NULL);

    gncInvoiceSetDocLink (inv, NULL);
    g_assert_cmpstr (gncInvoiceGetDocLink (inv), ==, NULL);
}

static gboolean account_has_one_split (const Account *acc)
{
    return (xaccAccountGetSplitsSize (acc) == 1);
}

static void
test_invoice_posted_trans ( Fixture *fixture, gconstpointer pData )
{
    const InvoiceData *data = (InvoiceData*) pData;

    gnc_numeric total = gncInvoiceGetTotal(fixture->invoice);
    gnc_numeric acct_balance, acct2_balance;

    g_assert_true (account_has_one_split (fixture->account));
    g_assert_true (account_has_one_split (fixture->account2));

    acct_balance = xaccAccountGetBalance(fixture->account);
    acct2_balance = xaccAccountGetBalance(fixture->account2);

    // Handle sign reversals (document values vs balance values)
    if (data->is_cn != !data->is_cust_doc)
    {
        g_assert_true (gnc_numeric_equal (gnc_numeric_neg(acct_balance), total));
        g_assert_true (gnc_numeric_equal (acct2_balance, total));
    }
    else
    {
        g_assert_true (gnc_numeric_equal (acct_balance, total));
        g_assert_true (gnc_numeric_equal (gnc_numeric_neg(acct2_balance), total));
    }
}

// Testing for TXN_TYPE_INVOICE TXN_TYPE_PAYMENT are strictly testing functions
// in Transaction.c, but require creating invoices, so, they are tested in
// this file instead.
static void
test_xaccTransGetTxnTypeInvoice (Fixture *fixture, gconstpointer pData)
{
    g_assert_cmpint (TXN_TYPE_INVOICE, ==, xaccTransGetTxnType (fixture->trans));

    g_assert_cmpint (TXN_TYPE_PAYMENT, ==, xaccTransGetTxnType (fixture->trans2));

    xaccTransVoid (fixture->trans2, "Cancel payment");

    g_assert_cmpint (TXN_TYPE_PAYMENT, ==, xaccTransGetTxnType (fixture->trans2));

    xaccTransUnvoid (fixture->trans2);
}


static void
test_xaccTransGetTxnTypeLink (Fixture *fixture, gconstpointer pData)
{
    g_assert_cmpint (TXN_TYPE_LINK, ==, xaccTransGetTxnType (fixture->trans2));
}


void
test_suite_gncInvoice ( void )
{
    static InvoiceData pData = { FALSE, FALSE, { 1000, 100 }, { 2000, 100 } };  // Vendor bill
    GNC_TEST_ADD( suitename, "post/unpost", Fixture, &pData, setup, test_invoice_post, teardown );

    GNC_TEST_ADD( suitename, "doclink", Fixture, &pData, setup, test_invoice_doclink, teardown );
    GNC_TEST_ADD( suitename, "post trans - vendor bill", Fixture, &pData, setup_with_invoice, test_invoice_posted_trans, teardown_with_invoice );
    pData.is_cn = TRUE;   // Vendor credit note
    GNC_TEST_ADD( suitename, "post trans - vendor credit note", Fixture, &pData, setup_with_invoice, test_invoice_posted_trans, teardown_with_invoice );
    pData.is_cust_doc = TRUE;   // Customer credit note
    GNC_TEST_ADD( suitename, "post trans - customer creditnote", Fixture, &pData, setup_with_invoice, test_invoice_posted_trans, teardown_with_invoice );
    pData.is_cn = FALSE;   // Customer invoice
    GNC_TEST_ADD( suitename, "post trans - customer invoice", Fixture, &pData, setup_with_invoice, test_invoice_posted_trans, teardown_with_invoice );

    /* test txn type heuristics */
    GNC_TEST_ADD( suitename, "tests txntype I & P", Fixture, &pData, setup_with_invoice_and_payment, test_xaccTransGetTxnTypeInvoice, teardown_with_invoice);
    GNC_TEST_ADD( suitename, "tests txntype L", Fixture, &pData, setup_with_invoice_and_CN, test_xaccTransGetTxnTypeLink, teardown_with_invoice);
}
