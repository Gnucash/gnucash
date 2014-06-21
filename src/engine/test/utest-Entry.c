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
#include "config.h"
#include <string.h>
#include <glib.h>
#include <qof.h>
#include <unittest-support.h>
#include "../gncEntry.h"

static const gchar *suitename = "/engine/gncEntry";
void test_suite_gncEntry ( void );

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

}

static void
teardown( Fixture *fixture, gconstpointer pData )
{
    xaccAccountBeginEdit(fixture->account);
    xaccAccountDestroy(fixture->account);
    gnc_commodity_destroy(fixture->commodity);

    qof_book_destroy( fixture->book );
}


static void
test_entry_basics ( Fixture *fixture, gconstpointer pData )
{
    Timespec ts1 = timespec_now(), ts2;
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
    g_assert(timespec_equal (&ts2, &ts1));
    g_test_message( "  DateEntered" );
    gncEntrySetDateEntered (entry, ts1);
    ts2 = gncEntryGetDateEntered (entry);
    g_assert(timespec_equal (&ts2, &ts1));
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

void
test_suite_gncEntry ( void )
{
    GNC_TEST_ADD( suitename, "basics", Fixture, NULL, setup, test_entry_basics, teardown );
}
