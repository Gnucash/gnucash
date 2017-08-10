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
#include "config.h"
#include <string.h>
#include <glib.h>
#include <qof.h>
#include <unittest-support.h>
#include "../gncInvoice.h"

static const gchar *suitename = "/engine/gncInvoice";
void test_suite_gncInvoice ( void );

typedef struct
{
    QofBook *book;
    Account *account;
    GncOwner owner;
    GncCustomer *customer;
    gnc_commodity *commodity;
} Fixture;

static void
setup( Fixture *fixture, gconstpointer pData )
{
    fixture->book = qof_book_new();

    fixture->account = xaccMallocAccount(fixture->book);
    fixture->commodity = gnc_commodity_new(fixture->book, "foo", "bar", "xy", "xy", 100);
    xaccAccountSetCommodity(fixture->account, fixture->commodity);

    fixture->customer = gncCustomerCreate(fixture->book);
    gncOwnerInitCustomer(&fixture->owner, fixture->customer);
}

static void
teardown( Fixture *fixture, gconstpointer pData )
{
    gncCustomerBeginEdit(fixture->customer);
    gncCustomerDestroy(fixture->customer);

    xaccAccountBeginEdit(fixture->account);
    xaccAccountDestroy(fixture->account);
    gnc_commodity_destroy(fixture->commodity);

    qof_book_destroy( fixture->book );
}


static void
test_invoice_post ( Fixture *fixture, gconstpointer pData )
{
    GncInvoice *invoice = gncInvoiceCreate(fixture->book);
    Timespec ts1 = timespec_now(), ts2 = ts1;
    g_assert(invoice);
    g_assert(!gncInvoiceGetIsCreditNote(invoice));
    g_assert(gncInvoiceGetActive(invoice));
    g_assert(gncInvoiceGetPostedAcc(invoice) == NULL);

    gncInvoiceSetCurrency(invoice, fixture->commodity);

    gncInvoiceSetOwner(invoice, &fixture->owner);

    g_test_message( "Will now post the invoice" );
    g_assert(!gncInvoiceIsPosted(invoice));
    gncInvoicePostToAccount(invoice, fixture->account, &ts1, &ts2, "memo", TRUE, FALSE);
    g_assert(gncInvoiceIsPosted(invoice));

    gncInvoiceUnpost(invoice, TRUE);
    g_assert(!gncInvoiceIsPosted(invoice));
}

void
test_suite_gncInvoice ( void )
{
    GNC_TEST_ADD( suitename, "post", Fixture, NULL, setup, test_invoice_post, teardown );
}
