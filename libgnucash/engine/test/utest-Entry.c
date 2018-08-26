/********************************************************************
 * utest-Entry.c: GLib g_test test suite for gncEntry.              *
 * Copyright 2014 Geert Janssens <geert@kobaltwit.be>               *
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
#include "../gncEntry.h"
#include "../gncTaxTableP.h"

static const gchar *suitename = "/engine/gncEntry";
void test_suite_gncEntry ( void );

typedef struct
{
    QofBook *book;
    Account *account;
    Account *vatacct;
    GncOwner owner;
    GncCustomer *customer;
    gnc_commodity *commodity;
    GncInvoice *invoice;
} Fixture;

static void
setup( Fixture *fixture, gconstpointer pData )
{
    fixture->book = qof_book_new();

    fixture->account = xaccMallocAccount(fixture->book);
    fixture->commodity = gnc_commodity_new(fixture->book, "foo", "bar", "xy", "xy", 100);
    xaccAccountSetCommodity(fixture->account, fixture->commodity);

    fixture->vatacct = xaccMallocAccount(fixture->book);
    xaccAccountSetCommodity(fixture->vatacct, fixture->commodity);

    fixture->invoice = gncInvoiceCreate(fixture->book);
    gncInvoiceSetCurrency(fixture->invoice, fixture->commodity);
}

static void
teardown( Fixture *fixture, gconstpointer pData )
{
    xaccAccountBeginEdit(fixture->account);
    xaccAccountDestroy(fixture->account);
    xaccAccountBeginEdit(fixture->vatacct);
    xaccAccountDestroy(fixture->vatacct);

    gncInvoiceBeginEdit(fixture->invoice);
    gncInvoiceDestroy(fixture->invoice);

    gnc_commodity_destroy(fixture->commodity);
    qof_book_destroy( fixture->book );
}


static void
test_entry_basics ( Fixture *fixture, gconstpointer pData )
{
    time64 ts1 = gnc_time(NULL), ts2;
    const char *desc = "Test description with éà unicode chars";
    const char *action = "Test action with éà unicode chars";
    const char *note = "Test note with éà unicode chars";
    gnc_numeric quantity = {500000, 100};
    gboolean is_cn = FALSE;

    GncEntry *entry = gncEntryCreate(fixture->book);
    g_assert(entry);

    g_test_message( "Test basic setters/getters" );
    g_test_message( "  Date" );
    gncEntrySetDate (entry, ts1);
    ts2 = gncEntryGetDate (entry);
    g_assert(ts2 == ts1);
    g_test_message( "  DateEntered" );
    gncEntrySetDateEntered (entry, ts1);
    ts2 = gncEntryGetDateEntered (entry);
    g_assert(ts2 == ts1);
    g_test_message( "  Description" );
    gncEntrySetDescription (entry, desc);
    g_assert(g_strcmp0 (gncEntryGetDescription (entry), desc) == 0);
    g_test_message( "  Action" );
    gncEntrySetAction (entry, action);
    g_assert(g_strcmp0 (gncEntryGetAction (entry), action) == 0);
    g_test_message( "  Notes" );
    gncEntrySetNotes (entry, note);
    g_assert(g_strcmp0 (gncEntryGetNotes (entry), note) == 0);
    g_test_message( "  Quantity" );
    gncEntrySetQuantity (entry, quantity);
    g_assert(gnc_numeric_eq (gncEntryGetQuantity (entry), quantity));
    g_test_message( "  DocQuantity (with is_cn = FALSE)" );
    gncEntrySetDocQuantity (entry, quantity, is_cn);
    g_assert(gnc_numeric_eq (gncEntryGetDocQuantity (entry, is_cn), quantity));
    g_assert(gnc_numeric_eq (gncEntryGetQuantity (entry), quantity));
    g_test_message( "  DocQuantity (with is_cn = TRUE)");
    is_cn = TRUE;
    gncEntrySetDocQuantity (entry, quantity, is_cn);
    g_assert(gnc_numeric_eq (gncEntryGetDocQuantity (entry, is_cn), quantity));
    g_assert(gnc_numeric_eq (gncEntryGetQuantity (entry), gnc_numeric_neg (quantity)));
    g_test_message( "  InvAccount" );
    gncEntrySetInvAccount (entry, fixture->account);
    g_assert(gncEntryGetInvAccount (entry) == fixture->account);

}

static void
test_entry_rounding ( Fixture *fixture, gconstpointer pData )
{
    GncEntry *entry = gncEntryCreate(fixture->book);
    GncTaxTable *taxtable;
    GncTaxTableEntry *tt_entry;

    gncTaxTableRegister();
    taxtable = gncTaxTableCreate(fixture->book);
    tt_entry = gncTaxTableEntryCreate();
    gncTaxTableSetName(taxtable, "Percent tax");
    gncTaxTableEntrySetAccount(tt_entry, fixture->vatacct);
    gncTaxTableEntrySetType(tt_entry, GNC_AMT_TYPE_PERCENT);
    gncTaxTableEntrySetAmount(tt_entry, gnc_numeric_create(1000000, 100000));
    gncTaxTableAddEntry(taxtable, tt_entry);

    // 1. Freestanding entry - a default denominator of 100000 is expected during rounding

    // Test with numbers that don't require rounding
    /* Tax 10% (high precision GncNumeric), tax not included */
    gncEntrySetInvTaxable(entry, TRUE);
    gncEntrySetInvTaxIncluded(entry, FALSE);
    gncEntrySetInvTaxTable(entry, taxtable);
    gncEntrySetQuantity(entry, gnc_numeric_create (2, 1));
    gncEntrySetInvPrice(entry, gnc_numeric_create (3, 1));
    /* Check unrounded result */
    g_assert(gnc_numeric_equal (gncEntryGetDocValue(entry, FALSE, TRUE, FALSE), gnc_numeric_create (6, 1)));
    g_assert(gnc_numeric_equal (gncEntryGetDocTaxValue(entry, FALSE, TRUE, FALSE), gnc_numeric_create (6, 10)));
    /* Check rounded result */
    g_assert(gnc_numeric_equal (gncEntryGetDocValue(entry, TRUE, TRUE, FALSE), gnc_numeric_create (6, 1)));
    g_assert(gnc_numeric_equal (gncEntryGetDocTaxValue(entry, TRUE, TRUE, FALSE), gnc_numeric_create (6, 10)));

    // Test with numbers that do require rounding
    /* Tax 10% (high precision GncNumeric), tax included */
    gncEntrySetInvTaxIncluded(entry, TRUE);
    /* Check unrounded result */
    g_assert(gnc_numeric_equal (gncEntryGetDocValue(entry, FALSE, TRUE, FALSE), gnc_numeric_create (60, 11)));
    g_assert(gnc_numeric_equal (gncEntryGetDocTaxValue(entry, FALSE, TRUE, FALSE), gnc_numeric_create (60, 110)));
    /* Check rounded result */
    g_assert(gnc_numeric_equal (gncEntryGetDocValue(entry, TRUE, TRUE, FALSE), gnc_numeric_create (545455, 100000)));
    g_assert(gnc_numeric_equal (gncEntryGetDocTaxValue(entry, TRUE, TRUE, FALSE), gnc_numeric_create (54545, 100000)));

    // Use different taxtable percentage precision
    /* Tax 10% (low precision GncNumeric), tax included */
    gncTaxTableEntrySetAmount(tt_entry, gnc_numeric_create(10, 1));
    gncEntrySetInvTaxTable(entry, NULL);
    gncEntrySetInvTaxTable(entry, taxtable);
    /* Check unrounded result */
    g_assert(gnc_numeric_equal (gncEntryGetDocValue(entry, FALSE, TRUE, FALSE), gnc_numeric_create (60, 11)));
    g_assert(gnc_numeric_equal (gncEntryGetDocTaxValue(entry, FALSE, TRUE, FALSE), gnc_numeric_create (60, 110)));
    /* Check rounded result */
    g_assert(gnc_numeric_equal (gncEntryGetDocValue(entry, TRUE, TRUE, FALSE), gnc_numeric_create (545455, 100000)));
    g_assert(gnc_numeric_equal (gncEntryGetDocTaxValue(entry, TRUE, TRUE, FALSE), gnc_numeric_create (54545, 100000)));

    // Test with odd tax percentage (Taken from a mailing list example)
    /* Tax 13% (high precision GncNumeric), tax not included */
    gncEntrySetInvTaxIncluded(entry, FALSE);
    gncTaxTableEntrySetAmount(tt_entry, gnc_numeric_create(1300000, 100000));
    gncEntrySetInvTaxTable(entry, NULL);
    gncEntrySetInvTaxTable(entry, taxtable);
    gncEntrySetQuantity(entry, gnc_numeric_create (1, 1));
    gncEntrySetInvPrice(entry, gnc_numeric_create (27750, 100));
    /* Check unrounded result */
    g_assert(gnc_numeric_equal (gncEntryGetDocValue(entry, FALSE, TRUE, FALSE), gnc_numeric_create (27750, 100)));
    g_assert(gnc_numeric_equal (gncEntryGetDocTaxValue(entry, FALSE, TRUE, FALSE), gnc_numeric_create (36075, 1000)));
    /* Check rounded result */
    g_assert(gnc_numeric_equal (gncEntryGetDocValue(entry, TRUE, TRUE, FALSE), gnc_numeric_create (27750, 100)));
    g_assert(gnc_numeric_equal (gncEntryGetDocTaxValue(entry, TRUE, TRUE, FALSE), gnc_numeric_create (36075, 1000)));
    /* Note on the above expected result: the standard gncEntry denom is 100000 if the entry has no invoice or
     * bill set. So with the example above no rounding is required yet */

    // Test with odd tax percentage (Taken from a mailing list example)
    /* Tax 13% (low precision GncNumeric), tax not included */
    gncEntrySetInvTaxIncluded(entry, FALSE);
    gncTaxTableEntrySetAmount(tt_entry, gnc_numeric_create(13, 1));
    gncEntrySetInvTaxTable(entry, NULL);
    gncEntrySetInvTaxTable(entry, taxtable);
    /* Check unrounded result */
    g_assert(gnc_numeric_equal (gncEntryGetDocValue(entry, FALSE, TRUE, FALSE), gnc_numeric_create (27750, 100)));
    g_assert(gnc_numeric_equal (gncEntryGetDocTaxValue(entry, FALSE, TRUE, FALSE), gnc_numeric_create (36075, 1000)));
    /* Check rounded result */
    g_assert(gnc_numeric_equal (gncEntryGetDocValue(entry, TRUE, TRUE, FALSE), gnc_numeric_create (27750, 100)));
    g_assert(gnc_numeric_equal (gncEntryGetDocTaxValue(entry, TRUE, TRUE, FALSE), gnc_numeric_create (36075, 1000)));
    /* Note on the above expected result: the standard gncEntry denom is 100000 if the entry has no invoice or
     * bill set. So with the example above no rounding is required yet */

    // 2. gncEntry as part of a gncInvoice - the invoice currency's denominator is expected during rounding
    gncInvoiceAddEntry(fixture->invoice, entry);

    // Test with numbers that don't require rounding
    /* Tax 10% (high precision GncNumeric), tax not included */
    gncEntrySetInvTaxIncluded(entry, FALSE);
    gncTaxTableEntrySetAmount(tt_entry, gnc_numeric_create(1000000, 100000));
    gncEntrySetQuantity(entry, gnc_numeric_create (2, 1));
    gncEntrySetInvPrice(entry, gnc_numeric_create (3, 1));
    /* Check unrounded result */
    g_assert(gnc_numeric_equal (gncEntryGetDocValue(entry, FALSE, TRUE, FALSE), gnc_numeric_create (6, 1)));
    g_assert(gnc_numeric_equal (gncEntryGetDocTaxValue(entry, FALSE, TRUE, FALSE), gnc_numeric_create (6, 10)));
    /* Check rounded result */
    g_assert(gnc_numeric_equal (gncEntryGetDocValue(entry, TRUE, TRUE, FALSE), gnc_numeric_create (6, 1)));
    g_assert(gnc_numeric_equal (gncEntryGetDocTaxValue(entry, TRUE, TRUE, FALSE), gnc_numeric_create (6, 10)));

    // Test with numbers that do require rounding
    /* Tax 10% (high precision GncNumeric), tax included */
    gncEntrySetInvTaxIncluded(entry, TRUE);
    /* Check unrounded result */
    g_assert(gnc_numeric_equal (gncEntryGetDocValue(entry, FALSE, TRUE, FALSE), gnc_numeric_create (60, 11)));
    g_assert(gnc_numeric_equal (gncEntryGetDocTaxValue(entry, FALSE, TRUE, FALSE), gnc_numeric_create (60, 110)));
    /* Check rounded result */
    g_assert(gnc_numeric_equal (gncEntryGetDocValue(entry, TRUE, TRUE, FALSE), gnc_numeric_create (545, 100)));
    g_assert(gnc_numeric_equal (gncEntryGetDocTaxValue(entry, TRUE, TRUE, FALSE), gnc_numeric_create (55, 100)));

    // Use different taxtable percentage precision
    /* Tax 10% (low precision GncNumeric), tax included */
    gncTaxTableEntrySetAmount(tt_entry, gnc_numeric_create(10, 1));
    gncEntrySetInvTaxTable(entry, NULL);
    gncEntrySetInvTaxTable(entry, taxtable);
    /* Check unrounded result */
    g_assert(gnc_numeric_equal (gncEntryGetDocValue(entry, FALSE, TRUE, FALSE), gnc_numeric_create (60, 11)));
    g_assert(gnc_numeric_equal (gncEntryGetDocTaxValue(entry, FALSE, TRUE, FALSE), gnc_numeric_create (60, 110)));
    /* Check rounded result */
    g_assert(gnc_numeric_equal (gncEntryGetDocValue(entry, TRUE, TRUE, FALSE), gnc_numeric_create (545, 100)));
    g_assert(gnc_numeric_equal (gncEntryGetDocTaxValue(entry, TRUE, TRUE, FALSE), gnc_numeric_create (55, 100)));

    // Test with odd tax percentage (Taken from a mailing list example)
    /* Tax 13% (high precision GncNumeric), tax not included */
    gncEntrySetInvTaxIncluded(entry, FALSE);
    gncTaxTableEntrySetAmount(tt_entry, gnc_numeric_create(1300000, 100000));
    gncEntrySetInvTaxTable(entry, NULL);
    gncEntrySetInvTaxTable(entry, taxtable);
    gncEntrySetQuantity(entry, gnc_numeric_create (1, 1));
    gncEntrySetInvPrice(entry, gnc_numeric_create (27750, 100));
    /* Check unrounded result */
    g_assert(gnc_numeric_equal (gncEntryGetDocValue(entry, FALSE, TRUE, FALSE), gnc_numeric_create (27750, 100)));
    g_assert(gnc_numeric_equal (gncEntryGetDocTaxValue(entry, FALSE, TRUE, FALSE), gnc_numeric_create (36075, 1000)));
    /* Check rounded result */
    g_assert(gnc_numeric_equal (gncEntryGetDocValue(entry, TRUE, TRUE, FALSE), gnc_numeric_create (27750, 100)));
    g_assert(gnc_numeric_equal (gncEntryGetDocTaxValue(entry, TRUE, TRUE, FALSE), gnc_numeric_create (3608, 100)));
    /* Note on the above expected result: the standard gncEntry denom is 100000 if the entry has no invoice or
     * bill set. So with the example above no rounding is required yet */

    // Test with odd tax percentage (Taken from a mailing list example)
    /* Tax 13% (low precision GncNumeric), tax not included */
    gncEntrySetInvTaxIncluded(entry, FALSE);
    gncTaxTableEntrySetAmount(tt_entry, gnc_numeric_create(13, 1));
    gncEntrySetInvTaxTable(entry, NULL);
    gncEntrySetInvTaxTable(entry, taxtable);
    /* Check unrounded result */
    g_assert(gnc_numeric_equal (gncEntryGetDocValue(entry, FALSE, TRUE, FALSE), gnc_numeric_create (27750, 100)));
    g_assert(gnc_numeric_equal (gncEntryGetDocTaxValue(entry, FALSE, TRUE, FALSE), gnc_numeric_create (36075, 1000)));
    /* Check rounded result */
    g_assert(gnc_numeric_equal (gncEntryGetDocValue(entry, TRUE, TRUE, FALSE), gnc_numeric_create (27750, 100)));
    g_assert(gnc_numeric_equal (gncEntryGetDocTaxValue(entry, TRUE, TRUE, FALSE), gnc_numeric_create (3608, 100)));
    /* Note on the above expected result: the standard gncEntry denom is 100000 if the entry has no invoice or
     * bill set. So with the example above no rounding is required yet */

    gncTaxTableBeginEdit(taxtable);
    gncTaxTableDestroy(taxtable);
}
void
test_suite_gncEntry ( void )
{
    GNC_TEST_ADD( suitename, "basics", Fixture, NULL, setup, test_entry_basics, teardown );
    GNC_TEST_ADD( suitename, "value rounding", Fixture, NULL, setup, test_entry_rounding, teardown );
}
