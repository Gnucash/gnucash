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
#include "../SchedXaction.h"
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
	Split        *split;
	GNCLot       *lot;
	GncCustomer  *cust;
	GncEmployee  *emp;
	GncJob       *job;
	GncVendor    *vend;
    };
    GSList *hdlrs;
} Fixture;

/* Prototype to shut clang up */
void test_suite_engine_kvp_properties (void);

/* Private QofInstance functions needed for testing */
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
setup_split (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_book_new ();
    fixture->split = xaccMallocSplit (book);
}

static void
setup_lot (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_book_new ();
    fixture->lot = gnc_lot_new (book);
}

static void
setup_customer (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_book_new ();
    fixture->cust = gncCustomerCreate (book);
}

static void
setup_employee (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_book_new ();
    fixture->emp = gncEmployeeCreate (book);
}

static void
setup_job (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_book_new ();
    fixture->job = gncJobCreate (book);
}

static void
setup_vendor (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_book_new ();
    fixture->vend = gncVendorCreate (book);
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
    gint64 ab_acct_uid = 67890987654321;
    gint64 next_id_r, ab_acct_uid_r;
    gchar *online_id = "my online id";
    gchar *ab_acct_id = "1234-5678-9087";
    gchar *ab_bank_code = "0032340";
    gchar *online_id_r, *ab_acct_id_r, *ab_bank_code_r;
    GncGUID *ofx_income_acct = guid_malloc ();
    GncGUID *ofx_income_acct_r;
    Timespec trans_retr = timespec_now ();
    Timespec *trans_retr_r;

    xaccAccountBeginEdit (fixture->acct);
    qof_instance_set (QOF_INSTANCE (fixture->acct),
		      "lot-next-id", next_id,
		      "online-id", online_id,
		      "ofx-income-account", ofx_income_acct,
		      "ab-account-id", ab_acct_id,
		      "ab-bank-code", ab_bank_code,
		      "ab-account-uid", ab_acct_uid,
		      "ab-trans-retrieval", &trans_retr,
		      NULL);

    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->acct)));
    qof_instance_mark_clean (QOF_INSTANCE (fixture->acct));

    qof_instance_get (QOF_INSTANCE (fixture->acct),
		      "lot-next-id", &next_id_r,
		      "online-id", &online_id_r,
		      "ofx-income-account", &ofx_income_acct_r,
		      "ab-account-id", &ab_acct_id_r,
		      "ab-bank-code", &ab_bank_code_r,
		      "ab-account-uid", &ab_acct_uid_r,
		      "ab-trans-retrieval", &trans_retr_r,
		      NULL);
    g_assert_cmpint (next_id, ==, next_id_r);
    g_assert_cmpstr (online_id, ==, online_id_r);
    g_assert (guid_equal (ofx_income_acct, ofx_income_acct_r));
    g_assert_cmpstr (ab_acct_id, ==, ab_acct_id_r);
    g_assert_cmpstr (ab_bank_code, ==, ab_bank_code_r);
    g_assert_cmpint (ab_acct_uid, ==, ab_acct_uid_r);
    g_assert (timespec_equal (&trans_retr, trans_retr_r));
    g_assert (!qof_instance_is_dirty (QOF_INSTANCE (fixture->acct)));
}

static void
test_trans_kvp_properties (Fixture *fixture, gconstpointer pData)
{
    GncGUID *invoice = guid_malloc ();
    GncGUID *from_sx = guid_malloc ();
    GncGUID *invoice_r, *from_sx_r;
    gchar *online_id = "my online id";
    gchar *online_id_r;

    xaccTransBeginEdit (fixture->trans);
    qof_instance_set (QOF_INSTANCE (fixture->trans),
		      "invoice", invoice,
		      "from-sched-xaction", from_sx,
		      "online-id", online_id,
		      NULL);

    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->trans)));
    qof_instance_mark_clean (QOF_INSTANCE (fixture->trans));

    qof_instance_get (QOF_INSTANCE (fixture->trans),
		      "invoice", &invoice_r,
		      "from-sched-xaction", &from_sx_r,
		      "online-id", &online_id_r,
		      NULL);
    g_assert (guid_equal (invoice, invoice_r));
    g_assert (guid_equal (from_sx, from_sx_r));
    g_assert_cmpstr (online_id, ==, online_id_r);
    g_assert (!qof_instance_is_dirty (QOF_INSTANCE (fixture->trans)));
    guid_free (invoice);
    guid_free (invoice_r);
    guid_free (from_sx);
    guid_free (from_sx_r);
    g_free (online_id_r);
}

static void
test_split_kvp_properties (Fixture *fixture, gconstpointer pData)
{
    gchar *debit_formula = "e^xdydx";
    gchar *credit_formula = "seccostansin";
    gchar *sx_shares = "43";
    gchar *online_id = "my_online_id";
    gchar *debit_formula_r, *credit_formula_r, *sx_shares_r;
    gchar *online_id_r;
    GncGUID *sx_account = guid_malloc ();
    GncGUID *sx_account_r;
    gnc_numeric debit_numeric = gnc_numeric_create (123, 456);
    gnc_numeric credit_numeric = gnc_numeric_create (789, 456);
    gnc_numeric *debit_numeric_r, *credit_numeric_r;

    qof_begin_edit (QOF_INSTANCE (fixture->split));
    qof_instance_set (QOF_INSTANCE (fixture->split),
		      "sx-debit-formula", debit_formula,
		      "sx-debit-numeric", &debit_numeric,
		      "sx-credit-formula", credit_formula,
		      "sx-credit-numeric", &credit_numeric,
		      "sx-account", sx_account,
		      "sx-shares", sx_shares,
		      "online-id", online_id,
		      NULL);

    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->split)));
    qof_instance_mark_clean (QOF_INSTANCE (fixture->split));

    qof_instance_get (QOF_INSTANCE (fixture->split),
		      "sx-debit-formula", &debit_formula_r,
		      "sx-debit-numeric", &debit_numeric_r,
		      "sx-credit-formula", &credit_formula_r,
		      "sx-credit-numeric", &credit_numeric_r,
		      "sx-account", &sx_account_r,
		      "sx-shares", &sx_shares_r,
		      "online-id", &online_id_r,
		      NULL);
    g_assert_cmpstr (debit_formula, ==, debit_formula_r);
    g_assert (gnc_numeric_equal (debit_numeric, *debit_numeric_r));
    g_assert_cmpstr (credit_formula, ==, credit_formula_r);
    g_assert (gnc_numeric_equal (credit_numeric, *credit_numeric_r));
    g_assert (guid_equal (sx_account, sx_account_r));
    g_assert_cmpstr (sx_shares, ==, sx_shares_r);
    g_assert_cmpstr (online_id, ==, online_id_r);
    g_assert (!qof_instance_is_dirty (QOF_INSTANCE (fixture->split)));
    g_free (debit_formula_r);
    g_free (debit_numeric_r);
    g_free (credit_formula_r);
    g_free (credit_numeric_r);
    qof_begin_edit (QOF_INSTANCE (fixture->split));
    qof_instance_set (QOF_INSTANCE (fixture->split),
		      "sx-credit-formula", NULL,
		      NULL);
    qof_instance_get (QOF_INSTANCE (fixture->split),
		      "sx-credit-formula", &credit_numeric_r,
		      NULL);
    g_assert (credit_numeric_r == NULL);
    g_free (sx_shares_r);
    g_free (online_id_r);
    guid_free (sx_account);
    guid_free (sx_account_r);
}

static void
test_lot_kvp_properties (Fixture *fixture, gconstpointer pData)
{
    GncGUID *invoice = guid_malloc ();
    GncGUID *invoice_r;
    gint64 owner_type = 47;
    gint64 owner_type_r;
    GncGUID *owner = guid_malloc ();
    GncGUID *owner_r;

    qof_begin_edit (QOF_INSTANCE (fixture->lot));
    qof_instance_set (QOF_INSTANCE (fixture->lot),
		      "invoice", invoice,
		      GNC_OWNER_TYPE, owner_type,
		      GNC_OWNER_GUID, owner,
		      NULL);

    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->lot)));
    qof_instance_mark_clean (QOF_INSTANCE (fixture->lot));

    qof_instance_get (QOF_INSTANCE (fixture->lot),
		      "invoice", &invoice_r,
		      GNC_OWNER_TYPE, &owner_type_r,
		      GNC_OWNER_GUID, &owner_r,
		      NULL);
    g_assert (guid_equal (invoice, invoice_r));
    g_assert_cmpint (owner_type, ==, owner_type_r);
    g_assert (guid_equal (owner, owner_r));
    g_assert (!qof_instance_is_dirty (QOF_INSTANCE (fixture->lot)));
    guid_free (invoice);
    guid_free (invoice_r);
    guid_free (owner);
    guid_free (owner_r);
}

static void
test_customer_kvp_properties (Fixture *fixture, gconstpointer pData)
{
    gchar *pdf_dir = "/foo/bar/baz";
    gchar *pdf_dir_r;
    GncGUID *inv_acct = guid_malloc ();
    GncGUID *pmt_acct = guid_malloc ();
    GncGUID *inv_acct_r, *pmt_acct_r;

    qof_begin_edit (QOF_INSTANCE (fixture->cust));
    qof_instance_set (QOF_INSTANCE (fixture->cust),
		      "export-pdf-dir", pdf_dir,
		      "invoice-last-posted-account", inv_acct,
		      "payment-last-account", pmt_acct,
		      NULL);

    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->cust)));
    qof_instance_mark_clean (QOF_INSTANCE (fixture->cust));

    qof_instance_get (QOF_INSTANCE (fixture->cust),
		      "export-pdf-dir", &pdf_dir_r,
		      "invoice-last-posted-account", &inv_acct_r,
		      "payment-last-account", &pmt_acct_r,
		      NULL);

    g_assert_cmpstr (pdf_dir, ==, pdf_dir_r);
    g_assert (guid_equal (inv_acct, inv_acct_r));
    g_assert (guid_equal (pmt_acct, pmt_acct_r));
    guid_free (inv_acct);
    guid_free (inv_acct_r);
    guid_free (pmt_acct);
    guid_free (pmt_acct_r);
    g_free (pdf_dir_r);

}

static void
test_employee_kvp_properties (Fixture *fixture, gconstpointer pData)
{
    gchar *pdf_dir = "/foo/bar/baz";
    gchar *pdf_dir_r;
    GncGUID *inv_acct = guid_malloc ();
    GncGUID *pmt_acct = guid_malloc ();
    GncGUID *inv_acct_r, *pmt_acct_r;

    qof_begin_edit (QOF_INSTANCE (fixture->emp));
    qof_instance_set (QOF_INSTANCE (fixture->emp),
		      "export-pdf-dir", pdf_dir,
		      "invoice-last-posted-account", inv_acct,
		      "payment-last-account", pmt_acct,
		      NULL);

    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->emp)));
    qof_instance_mark_clean (QOF_INSTANCE (fixture->emp));

    qof_instance_get (QOF_INSTANCE (fixture->emp),
		      "export-pdf-dir", &pdf_dir_r,
		      "invoice-last-posted-account", &inv_acct_r,
		      "payment-last-account", &pmt_acct_r,
		      NULL);

    g_assert_cmpstr (pdf_dir, ==, pdf_dir_r);
    g_assert (guid_equal (inv_acct, inv_acct_r));
    g_assert (guid_equal (pmt_acct, pmt_acct_r));
    guid_free (inv_acct);
    guid_free (inv_acct_r);
    guid_free (pmt_acct);
    guid_free (pmt_acct_r);
    g_free (pdf_dir_r);

}

static void
test_job_kvp_properties (Fixture *fixture, gconstpointer pData)
{
    gchar *pdf_dir = "/foo/bar/baz";
    gchar *pdf_dir_r;

    qof_begin_edit (QOF_INSTANCE (fixture->job));
    qof_instance_set (QOF_INSTANCE (fixture->job),
		      "export-pdf-dir", pdf_dir,
		      NULL);

    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->job)));
    qof_instance_mark_clean (QOF_INSTANCE (fixture->job));

    qof_instance_get (QOF_INSTANCE (fixture->job),
		      "export-pdf-dir", &pdf_dir_r,
		      NULL);

    g_assert_cmpstr (pdf_dir, ==, pdf_dir_r);
    g_free (pdf_dir_r);

}

static void
test_vendor_kvp_properties (Fixture *fixture, gconstpointer pData)
{
    gchar *pdf_dir = "/foo/bar/baz";
    gchar *pdf_dir_r;
    GncGUID *inv_acct = guid_malloc ();
    GncGUID *pmt_acct = guid_malloc ();
    GncGUID *inv_acct_r, *pmt_acct_r;

    qof_begin_edit (QOF_INSTANCE (fixture->vend));
    qof_instance_set (QOF_INSTANCE (fixture->vend),
		      "export-pdf-dir", pdf_dir,
		      "invoice-last-posted-account", inv_acct,
		      "payment-last-account", pmt_acct,
		      NULL);

    g_assert (qof_instance_is_dirty (QOF_INSTANCE (fixture->vend)));
    qof_instance_mark_clean (QOF_INSTANCE (fixture->vend));

    qof_instance_get (QOF_INSTANCE (fixture->vend),
		      "export-pdf-dir", &pdf_dir_r,
		      "invoice-last-posted-account", &inv_acct_r,
		      "payment-last-account", &pmt_acct_r,
		      NULL);

    g_assert_cmpstr (pdf_dir, ==, pdf_dir_r);
    g_assert (guid_equal (inv_acct, inv_acct_r));
    g_assert (guid_equal (pmt_acct, pmt_acct_r));
    guid_free (inv_acct);
    guid_free (inv_acct_r);
    guid_free (pmt_acct);
    guid_free (pmt_acct_r);
    g_free (pdf_dir_r);

}

void test_suite_engine_kvp_properties (void)
{
    GNC_TEST_ADD (suitename, "Account", Fixture, NULL, setup_account, test_account_kvp_properties, teardown);
    GNC_TEST_ADD (suitename, "Transaction", Fixture, NULL, setup_trans, test_trans_kvp_properties, teardown);
    GNC_TEST_ADD (suitename, "Split", Fixture, NULL, setup_split, test_split_kvp_properties, teardown);
    GNC_TEST_ADD (suitename, "Lot", Fixture, NULL, setup_lot, test_lot_kvp_properties, teardown);
    GNC_TEST_ADD (suitename, "Customer", Fixture, NULL, setup_customer, test_customer_kvp_properties, teardown);
    GNC_TEST_ADD (suitename, "Employee", Fixture, NULL, setup_employee, test_employee_kvp_properties, teardown);
    GNC_TEST_ADD (suitename, "Job", Fixture, NULL, setup_job, test_job_kvp_properties, teardown);
    GNC_TEST_ADD (suitename, "Vendor", Fixture, NULL, setup_vendor, test_vendor_kvp_properties, teardown);
}
