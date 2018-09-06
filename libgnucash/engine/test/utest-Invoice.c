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
    Transaction *trans;
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
        fixture->customer = gncCustomerCreate(fixture->book);
        gncOwnerInitCustomer(&fixture->owner, fixture->customer);
    }
    else
    {
        fixture->vendor = gncVendorCreate(fixture->book);
        gncOwnerInitVendor(&fixture->owner, fixture->vendor);
    }

    fixture->invoice = gncInvoiceCreate(fixture->book);
}

static void
teardown( Fixture *fixture, gconstpointer pData )
{
    const InvoiceData *data = (InvoiceData*) pData;

    gncInvoiceBeginEdit(fixture->invoice);
    gncInvoiceDestroy(fixture->invoice);

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

    xaccAccountBeginEdit(fixture->account);
    xaccAccountDestroy(fixture->account);
    xaccAccountBeginEdit(fixture->account2);
    xaccAccountDestroy(fixture->account2);
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
teardown_with_invoice( Fixture *fixture, gconstpointer pData )
{
    gncInvoiceUnpost(fixture->invoice, TRUE);
    gncInvoiceRemoveEntries (fixture->invoice);

    teardown(fixture, pData);
}


static void
test_invoice_post ( Fixture *fixture, gconstpointer pData )
{
    time64 ts1 = gnc_time(NULL);
    time64 ts2 = ts1;
    g_assert(fixture->invoice);
    g_assert(!gncInvoiceGetIsCreditNote(fixture->invoice));
    g_assert(gncInvoiceGetActive(fixture->invoice));
    g_assert(gncInvoiceGetPostedAcc(fixture->invoice) == NULL);

    gncInvoiceSetCurrency(fixture->invoice, fixture->commodity);

    gncInvoiceSetOwner(fixture->invoice, &fixture->owner);

    g_test_message( "Will now post the invoice" );
    g_assert(!gncInvoiceIsPosted(fixture->invoice));
    gncInvoicePostToAccount(fixture->invoice, fixture->account, ts1, ts2, "memo", TRUE, FALSE);
    g_assert(gncInvoiceIsPosted(fixture->invoice));

    gncInvoiceUnpost(fixture->invoice, TRUE);
    g_assert(!gncInvoiceIsPosted(fixture->invoice));
}

static void
test_invoice_posted_trans ( Fixture *fixture, gconstpointer pData )
{
    const InvoiceData *data = (InvoiceData*) pData;

    gnc_numeric total = gncInvoiceGetTotal(fixture->invoice);
    gnc_numeric acct_balance, acct2_balance;

    g_assert (1 == xaccAccountCountSplits (fixture->account, FALSE));
    g_assert (1 == xaccAccountCountSplits (fixture->account2, FALSE));

    acct_balance = xaccAccountGetBalance(fixture->account);
    acct2_balance = xaccAccountGetBalance(fixture->account2);

    // Handle sign reversals (document values vs balance values)
    if (data->is_cn != !data->is_cust_doc)
    {
        g_assert (gnc_numeric_equal (gnc_numeric_neg(acct_balance), total));
        g_assert (gnc_numeric_equal (acct2_balance, total));
    }
    else
    {
        g_assert (gnc_numeric_equal (acct_balance, total));
        g_assert (gnc_numeric_equal (gnc_numeric_neg(acct2_balance), total));
    }
}

void
test_suite_gncInvoice ( void )
{
    static InvoiceData pData = { FALSE, FALSE, { 1000, 100 }, { 2000, 100 } };  // Vendor bill
    GNC_TEST_ADD( suitename, "post/unpost", Fixture, &pData, setup, test_invoice_post, teardown );

    GNC_TEST_ADD( suitename, "post trans - vendor bill", Fixture, &pData, setup_with_invoice, test_invoice_posted_trans, teardown_with_invoice );
    pData.is_cn = TRUE;   // Vendor credit note
    GNC_TEST_ADD( suitename, "post trans - vendor credit note", Fixture, &pData, setup_with_invoice, test_invoice_posted_trans, teardown_with_invoice );
    pData.is_cust_doc = TRUE;   // Customer credit note
    GNC_TEST_ADD( suitename, "post trans - customer creditnote", Fixture, &pData, setup_with_invoice, test_invoice_posted_trans, teardown_with_invoice );
    pData.is_cn = FALSE;   // Customer invoice
    GNC_TEST_ADD( suitename, "post trans - customer invoice", Fixture, &pData, setup_with_invoice, test_invoice_posted_trans, teardown_with_invoice );
}
