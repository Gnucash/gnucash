/********************************************************************
 * test-option-util.cpp: GLib test suite for option-util.c.	    *
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
 * along with this program; if not, you can retrieve it from        *
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html            *
 * or contact:                                                      *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 ********************************************************************/
#include <kvp-frame.hpp>
#include <gmp.h> 
extern "C"
{
#include <config.h>
#include <glib.h>
#include <unittest-support.h>
#include <qofbookslots.h>
#include "test-engine-stuff.h"
#include "../option-util.h"
}

static const gchar *suitename = "/app-utils/option-util";
extern "C" void test_suite_option_util (void);

typedef struct
{
    QofBook *book;
    GSList *hdlrs;
} Fixture;

/* Expose a mostly-private QofInstance function to load options into
 * the Book.
 */
extern "C" KvpFrame *qof_instance_get_slots (const QofInstance*);

static void
setup (Fixture *fixture, gconstpointer pData)
{
    fixture->book = qof_book_new ();
    fixture->hdlrs = NULL;
}

static void
setup_kvp (Fixture *fixture, gconstpointer pData)
{
    QofBook *book;
    KvpFrame *slots;
    setup (fixture, pData);
    book = fixture->book;
    slots = qof_instance_get_slots (QOF_INSTANCE (book));
    qof_begin_edit (QOF_INSTANCE (book));
    qof_instance_set (QOF_INSTANCE (book),
                     "trading-accts", "t",
                     "split-action-num-field", "t",
                     "autoreadonly-days", (double)21,
                     NULL);

    slots->set_path({"options", "Business", "Company Name"},
               new KvpValue("Bogus Company"));
    qof_commit_edit (QOF_INSTANCE (book));
}

static void
teardown (Fixture *fixture, gconstpointer pData)
{
    qof_book_destroy (fixture->book);
    g_slist_free_full (fixture->hdlrs, test_free_log_handler);
    test_clear_error_list();
}

static void
test_option_load (Fixture *fixture, gconstpointer pData)
{
    gchar *str = NULL;
    SCM symbol_value;
    QofBook *book = fixture->book;
    GNCOptionDB *odb = gnc_option_db_new_for_type (QOF_ID_BOOK);

    qof_book_load_options (book, gnc_option_db_load, odb);
    g_assert (gnc_option_db_lookup_boolean_option (odb, OPTION_SECTION_ACCOUNTS, OPTION_NAME_TRADING_ACCOUNTS, FALSE));
    g_assert (gnc_option_db_lookup_boolean_option (odb,
                        OPTION_SECTION_ACCOUNTS,
                        OPTION_NAME_NUM_FIELD_SOURCE, FALSE));
    g_assert_cmpstr (gnc_option_db_lookup_string_option (odb, "Business", "Company Name", FALSE), ==, "Bogus Company");
    g_assert_cmpfloat (gnc_option_db_lookup_number_option (odb, OPTION_SECTION_ACCOUNTS, OPTION_NAME_AUTO_READONLY_DAYS, FALSE), ==, 21);

    gnc_option_db_destroy (odb);
}

static void
test_option_save (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = fixture->book;
    GNCOptionDB *odb = gnc_option_db_new_for_type (QOF_ID_BOOK);
    KvpFrame *slots = qof_instance_get_slots (QOF_INSTANCE (book));

    g_assert (gnc_option_db_set_boolean_option (odb, OPTION_SECTION_ACCOUNTS,
						                       OPTION_NAME_TRADING_ACCOUNTS,
                                               TRUE));
    g_assert (gnc_option_db_set_boolean_option (odb, OPTION_SECTION_ACCOUNTS,
                                               OPTION_NAME_NUM_FIELD_SOURCE,
                                               TRUE));
    g_assert (gnc_option_db_set_string_option (odb, "Business", "Company Name",
					       "Bogus Company"));
    g_assert (gnc_option_db_set_number_option (odb, OPTION_SECTION_ACCOUNTS,
					       OPTION_NAME_AUTO_READONLY_DAYS,
					       17));
    qof_book_save_options (book, gnc_option_db_save, odb, TRUE);
    g_assert_cmpstr (slots->get_slot({"options", "Accounts", "Use Trading Accounts"})->get<const char*>(), == , "t");
    g_assert_cmpstr (slots->get_slot({"options", "Accounts", "Use Split Action Field for Number"})->get<const char*>(), == , "t");
    g_assert_cmpstr (slots->get_slot({"options", "Business", "Company Name"})->get<const char*>(), ==, "Bogus Company");
    g_assert_cmpfloat (slots->get_slot({"options", "Accounts", "Day Threshold for Read-Only Transactions (red line)"})->get<double>(), ==, 17);

    gnc_option_db_destroy (odb);
}

extern "C" void
test_suite_option_util (void)
{
    GNC_TEST_ADD (suitename, "Option DB Load", Fixture, NULL, setup_kvp, test_option_load, teardown);
    GNC_TEST_ADD (suitename, "Option DB Save", Fixture, NULL, setup, test_option_save, teardown);
}
