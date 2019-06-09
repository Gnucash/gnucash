/********************************************************************
 * test-gnc-ui-util.c: GLib g_test test suite for gnc-ui-util.c.	    *
 * Copyright 2015 Alex Aycinena <alex.aycinena@gmail.com>		    *
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
 * along with this program; if not, you can retrieve it from        *
 * https://www.gnu.org/licenses/old-licenses/gpl-2.0.html            *
 * or contact:                                                      *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 ********************************************************************/

#include <config.h>
#include <glib.h>
#include <unittest-support.h>
#include <qof.h>
#include "test-engine-stuff.h"

#include "../gnc-ui-util.h"

static const gchar *suitename = "/app-utils/gnc-ui-util";
void test_suite_gnc_ui_util (void);

typedef struct
{
    QofBook *book;
    GSList *hdlrs;
} Fixture;

/* Expose a mostly-private QofInstance function to load options into
 * the Book.
 */
/*extern KvpFrame *qof_instance_get_slots (const QofInstance*); */

static void
setup (Fixture *fixture, gconstpointer pData)
{
    fixture->book = qof_book_new ();
    fixture->hdlrs = NULL;
}

static void
teardown (Fixture *fixture, gconstpointer pData)
{
    qof_book_destroy (fixture->book);
    g_slist_free_full (fixture->hdlrs, test_free_log_handler);
    test_clear_error_list();
}

static void
test_book_use_book_currency( Fixture *fixture, gconstpointer pData )
{
    const gchar *cur;
    const gchar *pol;
    Account *acct, *acc;

    g_test_message( "Testing with no currency accounting method selected" );
    cur = gnc_book_get_book_currency_name( fixture-> book );
    g_assert_cmpstr( cur, == , NULL );
    pol = gnc_book_get_default_gains_policy( fixture-> book );
    g_assert_cmpstr( pol, == , NULL );
    acct = gnc_book_get_default_gain_loss_acct ( fixture-> book );
    g_assert (acct == NULL );
    g_assert( !gnc_book_use_book_currency ( fixture-> book ));

    g_test_message( "Testing with trading accounts set to true - t" );
    qof_book_begin_edit (fixture->book);
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "trading-accts", "t",
		      NULL);
    cur = gnc_book_get_book_currency_name( fixture-> book );
    g_assert_cmpstr( cur, == , NULL );
    pol = gnc_book_get_default_gains_policy( fixture-> book );
    g_assert_cmpstr( pol, == , NULL );
    acct = gnc_book_get_default_gain_loss_acct ( fixture-> book );
    g_assert (acct == NULL );
    g_assert( !gnc_book_use_book_currency ( fixture-> book ));
    qof_book_commit_edit (fixture->book);

    qof_book_destroy( fixture->book );
    fixture->book = qof_book_new();

    g_test_message( "Testing with valid book-currency but default-gains-policy set to nonsense and default-gain-loss-account-guid set to random valid acct" );
    qof_book_begin_edit (fixture->book);
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "book-currency", "USD",
		      NULL);
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "default-gains-policy", "random",
		      NULL);
    acc = get_random_account( fixture-> book );
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "default-gain-loss-account-guid", qof_entity_get_guid(QOF_INSTANCE(acc)),
		      NULL);
    cur = gnc_book_get_book_currency_name( fixture-> book );
    g_assert_cmpstr( cur, == , NULL );
    pol = gnc_book_get_default_gains_policy( fixture-> book );
    g_assert_cmpstr( pol, == , NULL );
    acct = gnc_book_get_default_gain_loss_acct ( fixture-> book );
    g_assert (acct == NULL );
    g_assert( !gnc_book_use_book_currency ( fixture-> book ));
    qof_book_commit_edit (fixture->book);

    qof_book_destroy( fixture->book );
    fixture->book = qof_book_new();

/*    g_test_message( "Testing with valid default-gains-policy but book-currency set to nonsense and default-gain-loss-account-guid set to random valid acct" );
    qof_book_begin_edit (fixture->book);
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "book-currency", "myMoney",
		      NULL);
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "default-gains-policy", "fifo",
		      NULL);
    acc = get_random_account( fixture-> book );
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "default-gain-loss-account-guid", qof_entity_get_guid(QOF_INSTANCE(acc)),
		      NULL);
    cur = gnc_book_get_book_currency_name( fixture-> book );
    g_assert_cmpstr( cur, == , NULL );
    pol = gnc_book_get_default_gains_policy( fixture-> book );
    g_assert_cmpstr( pol, == , NULL );
    acct = gnc_book_get_default_gain_loss_acct ( fixture-> book );
    g_assert (acct == NULL );
    g_assert( !gnc_book_use_book_currency ( fixture-> book ));
    qof_book_commit_edit (fixture->book);

    qof_book_destroy( fixture->book );
    fixture->book = qof_book_new();

    g_test_message( "Testing with book-currency and default-gains-policy set to nonsense and default-gain-loss-account-guid set to random valid acct" );
    qof_book_begin_edit (fixture->book);
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "book-currency", "myMoney",
		      NULL);
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "default-gains-policy", "random",
		      NULL);
    acc = get_random_account( fixture-> book );
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "default-gain-loss-account-guid", qof_entity_get_guid(QOF_INSTANCE(acc)),
		      NULL);
    cur = gnc_book_get_book_currency_name( fixture-> book );
    g_assert_cmpstr( cur, == , NULL );
    pol = gnc_book_get_default_gains_policy( fixture-> book );
    g_assert_cmpstr( pol, == , NULL );
    acct = gnc_book_get_default_gain_loss_acct ( fixture-> book );
    g_assert (acct == NULL );
    g_assert( !gnc_book_use_book_currency ( fixture-> book ));
    qof_book_commit_edit (fixture->book);

    qof_book_destroy( fixture->book );
    fixture->book = qof_book_new();

    g_test_message( "Testing with book-currency set and no default-gains-policy and default-gain-loss-account-guid set to random valid acct" );
    qof_book_begin_edit (fixture->book);
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "book-currency", "USD",
		      NULL);
    acc = get_random_account( fixture-> book );
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "default-gain-loss-account-guid", qof_entity_get_guid(QOF_INSTANCE(acc)),
		      NULL);
    cur = gnc_book_get_book_currency_name( fixture-> book );
    g_assert_cmpstr( cur, == , NULL );
    pol = gnc_book_get_default_gains_policy( fixture-> book );
    g_assert_cmpstr( pol, == , NULL );
    acct = gnc_book_get_default_gain_loss_acct ( fixture-> book );
    g_assert (acct == NULL );
    g_assert( !gnc_book_use_book_currency ( fixture-> book ));
    qof_book_commit_edit (fixture->book);

    qof_book_destroy( fixture->book );
    fixture->book = qof_book_new();

    g_test_message( "Testing with default-gains-policy set and no book-currency and default-gain-loss-account-guid set to random valid acct" );
    qof_book_begin_edit (fixture->book);
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "default-gains-policy", "fifo",
		      NULL);
    acc = get_random_account( fixture-> book );
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "default-gain-loss-account-guid", qof_entity_get_guid(QOF_INSTANCE(acc)),
		      NULL);
    cur = gnc_book_get_book_currency_name( fixture-> book );
    g_assert_cmpstr( cur, == , NULL );
    pol = gnc_book_get_default_gains_policy( fixture-> book );
    g_assert_cmpstr( pol, == , NULL );
    acct = gnc_book_get_default_gain_loss_acct ( fixture-> book );
    g_assert (acct == NULL );
    g_assert( !gnc_book_use_book_currency ( fixture-> book ));
    qof_book_commit_edit (fixture->book);

    qof_book_destroy( fixture->book );
    fixture->book = qof_book_new();

    g_test_message( "Testing with book-currency, default-gains-policy and default-gain-loss-account-guid set to valid values and with trading accounts set to true - t" );
    qof_book_begin_edit (fixture->book);
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "trading-accts", "t",
		      NULL);
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "book-currency", "USD",
		      NULL);
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "default-gains-policy", "fifo",
		      NULL);
    acc = get_random_account( fixture-> book );
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "default-gain-loss-account-guid", qof_entity_get_guid(QOF_INSTANCE(acc)),
		      NULL);
    cur = gnc_book_get_book_currency_name( fixture-> book );
    g_assert_cmpstr( cur, == , NULL );
    pol = gnc_book_get_default_gains_policy( fixture-> book );
    g_assert_cmpstr( pol, == , NULL );
    acct = gnc_book_get_default_gain_loss_acct ( fixture-> book );
    g_assert (acct == NULL );
    g_assert( !gnc_book_use_book_currency ( fixture-> book ));
    qof_book_commit_edit (fixture->book);

    qof_book_destroy( fixture->book );
    fixture->book = qof_book_new();

     g_test_message( "Testing with book-currency, default-gains-policy and default-gain-loss-account-guid set to valid values and no trading accounts flag" );
    qof_book_begin_edit (fixture->book);
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "book-currency", "USD",
		      NULL);
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "default-gains-policy", "fifo",
		      NULL);
    qof_instance_set (QOF_INSTANCE (fixture->book),
		      "default-gain-loss-account-guid", qof_entity_get_guid(QOF_INSTANCE(acc)),
		      NULL);
    cur = gnc_book_get_book_currency_name( fixture-> book );
    g_assert_cmpstr( cur, == , "USD" );
    pol = gnc_book_get_default_gains_policy( fixture-> book );
    g_assert_cmpstr( pol, == , "fifo" );
    acct = gnc_book_get_default_gain_loss_acct ( fixture-> book );
    g_assert ( xaccAccountEqual(acct, acc, TRUE) );
    g_assert( gnc_book_use_book_currency ( fixture-> book ));
    qof_book_commit_edit (fixture->book); */
}

void
test_suite_gnc_ui_util (void)
{
    GNC_TEST_ADD( suitename, "use book-currency", Fixture, NULL, setup, test_book_use_book_currency, teardown );

}
