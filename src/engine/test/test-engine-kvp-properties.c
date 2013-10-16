/********************************************************************
 * test-engine-kvp-properties.c: GLib g_test test suite for         *
 * KVP-based properties in several engine classes.                  *
 * Copyright 2013 John Ralls <jralls@ceridwen.us>		    *
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

/**
 * Test Engine KVP Properties Acceptance testing for KVP Properties
 * added to various engine classes to make private the internals of
 * KVP storage used for a variety of parameters on several engine
 * classes.
 */

#include <config.h>
#include <glib.h>
#include <qof.h>
#include <unittest-support.h>
#include "../Transaction.h"
#include "../Split.h"
#include "../Account.h"
#include "../SchedXAction.h"
#include "../gncCustomer.h"
#include "../gncEmployee.h"
#include "../gncJob.h"
#include "../gncVendor.h"

typedef struct
{
    union
    {
	Account      *acct;
	Transaction  *trans;
	GncCustomer  *cust;
	GncEmployee  *emp;
	GncJob       *job;
	GncVendor    *vend;
    };
    GSList *split;
} Fixture;

/* Prototype to shut clang up */
void test_suite_engine_kvp_properties (void);

/* Private QofInstance function needed for testing */
extern void qof_instance_mark_clean (QofInstance*);

const gchar *suitename = "/engine/kvp-properties";

static void
setup_account (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_book_new ();
    fixture->acct = xaccMallocAccount (book);
}

static void
setup_trans (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_book_new ();
    fixture->trans = xaccMallocTransaction (book);
}

static void
teardown (Fixture *fixture, gconstpointer pData)
{
/* It doesn't actually matter which union member we use here, they're
 * all QofInstances, so this will work for any of them.
 */
    QofBook *book = qof_instance_get_book (QOF_INSTANCE (fixture->acct));
    test_destroy (fixture->acct);
    test_destroy (book);
}

static void
test_account_kvp_properties (Fixture *fixture, gconstpointer pData)
{
    gint64 next_id = 12345678909876;
    gint64 next_id_r;

    qof_instance_set (QOF_INSTANCE (fixture->acct),
		      "lot-next-id", next_id,
		      NULL);

    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->acct)));
    qof_instance_mark_clean (QOF_INSTANCE (fixture->acct));

    qof_instance_get (QOF_INSTANCE (fixture->acct),
		      "lot-next-id", &next_id_r,
		      NULL);
    g_assert_cmpint (next_id, ==, next_id_r);
    g_assert (!qof_instance_is_dirty (QOF_INSTANCE (fixture->acct)));
}

static void
test_trans_kvp_properties (Fixture *fixture, gconstpointer pData)
{
    GncGUID *guid = guid_malloc ();
    GncGUID *guid_r;

    qof_instance_set (QOF_INSTANCE (fixture->trans),
		      "invoice", guid,
		      NULL);

    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->trans)));
    qof_instance_mark_clean (QOF_INSTANCE (fixture->trans));

    qof_instance_get (QOF_INSTANCE (fixture->trans),
		      "invoice", &guid_r,
		      NULL);
    g_assert (guid_equal (guid, guid_r));
    g_assert (!qof_instance_is_dirty (QOF_INSTANCE (fixture->trans)));
    guid_free (guid);
    guid_free (guid_r);
}

void test_suite_engine_kvp_properties (void)
{
    GNC_TEST_ADD (suitename, "Account", Fixture, NULL, setup_account, test_account_kvp_properties, teardown);
    GNC_TEST_ADD (suitename, "Transaction", Fixture, NULL, setup_trans, test_trans_kvp_properties, teardown);
}
