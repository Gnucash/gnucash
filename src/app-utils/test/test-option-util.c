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

#include <config.h>
#include <glib.h>
#include <unittest-support.h>
#include <qofbookslots.h>
#include <kvp_frame.h>

#include "../option-util.h"

static const gchar *suitename = "/app-utils/option-util";
void test_suite_option_util (void);

typedef struct
{
    QofBook *book;
    GSList *hdlrs;
} Fixture;

/* Expose a mostly-private QofInstance function to load options into
 * the Book.
 */
extern KvpFrame *qof_instance_get_slots (const QofInstance*);

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

    kvp_frame_set_string (slots, "options/Business/Company Name",
			  "Bogus Company");
    qof_commit_edit (QOF_INSTANCE (book));
}

static void
setup_kvp_book_currency (Fixture *fixture, gconstpointer pData)
{
    QofBook *book;
    setup (fixture, pData);
    book = fixture->book;
    qof_begin_edit (QOF_INSTANCE (book));
    qof_instance_set (QOF_INSTANCE (book),
                     "book-currency", "GTQ",
                     "default-gains-policy", "fifo",
                     NULL);
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
    symbol_value = gnc_currency_accounting_option_value_get_method (
                        gnc_option_db_lookup_option (odb,
                            OPTION_SECTION_ACCOUNTS,
                            OPTION_NAME_CURRENCY_ACCOUNTING,
                            SCM_BOOL_F));
    if (scm_is_symbol(symbol_value))
    {
        SCM string_value = scm_symbol_to_string (symbol_value);
        if (scm_is_string (string_value))
        {
            str = scm_to_utf8_string (string_value);
        }
    }
    g_assert_cmpstr (str, ==, "trading");
    if (str)
        g_free (str);
    g_assert (gnc_option_db_lookup_boolean_option (odb,
                        OPTION_SECTION_ACCOUNTS,
                        OPTION_NAME_NUM_FIELD_SOURCE, FALSE));
    g_assert_cmpstr (gnc_option_db_lookup_string_option (odb, "Business", "Company Name", FALSE), ==, "Bogus Company");
    g_assert_cmpfloat (gnc_option_db_lookup_number_option (odb, OPTION_SECTION_ACCOUNTS, OPTION_NAME_AUTO_READONLY_DAYS, FALSE), ==, 21);

    gnc_option_db_destroy (odb);
}

static void
test_option_load_book_currency (Fixture *fixture, gconstpointer pData)
{
    gchar *str = NULL;
    SCM symbol_value;
    const gchar *curr = NULL;
    SCM curr_scm;
    gnc_commodity *commodity;
    QofBook *book = fixture->book;
    GNCOptionDB *odb = gnc_option_db_new_for_type (QOF_ID_BOOK);

    qof_book_load_options (book, gnc_option_db_load, odb);
    symbol_value = gnc_currency_accounting_option_value_get_method (
                        gnc_option_db_lookup_option (odb,
                            OPTION_SECTION_ACCOUNTS,
                            OPTION_NAME_CURRENCY_ACCOUNTING,
                            SCM_BOOL_F));
    if (scm_is_symbol(symbol_value))
    {
        SCM string_value = scm_symbol_to_string (symbol_value);
        if (scm_is_string (string_value))
        {
            str = scm_to_utf8_string (string_value);
        }
    }
    g_assert_cmpstr (str, ==, "book-currency");
    if (str)
        g_free (str);
    symbol_value = gnc_currency_accounting_option_value_get_default_policy (
                        gnc_option_db_lookup_option (odb,
                            OPTION_SECTION_ACCOUNTS,
                            OPTION_NAME_CURRENCY_ACCOUNTING,
                            SCM_BOOL_F));
    if (scm_is_symbol(symbol_value))
    {
        SCM string_value = scm_symbol_to_string (symbol_value);
        if (scm_is_string (string_value))
        {
            str = scm_to_utf8_string (string_value);
        }
    }
    g_assert_cmpstr (str, ==, "fifo");
    if (str)
        g_free (str);
    curr_scm = gnc_currency_accounting_option_value_get_book_currency(
                        gnc_option_db_lookup_option(odb,
                            OPTION_SECTION_ACCOUNTS,
                            OPTION_NAME_CURRENCY_ACCOUNTING,
                            SCM_BOOL_F));
    commodity = gnc_scm_to_commodity (curr_scm);
    if (commodity)
    {
        curr = gnc_commodity_get_mnemonic (commodity);
    }
    g_assert_cmpstr (curr, ==, "GTQ");

    gnc_option_db_destroy (odb);
}

static void
test_option_save (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = fixture->book;
    GNCOptionDB *odb = gnc_option_db_new_for_type (QOF_ID_BOOK);
    KvpFrame *slots = qof_instance_get_slots (QOF_INSTANCE (book));

    g_assert (gnc_option_db_set_option (odb, OPTION_SECTION_ACCOUNTS,
						OPTION_NAME_CURRENCY_ACCOUNTING,
						scm_cons (scm_from_locale_symbol("trading"), SCM_EOL)));
    g_assert (gnc_option_db_set_boolean_option (odb, OPTION_SECTION_ACCOUNTS,
                                               OPTION_NAME_NUM_FIELD_SOURCE,
                                               TRUE));
    g_assert (gnc_option_db_set_string_option (odb, "Business", "Company Name",
					       "Bogus Company"));
    g_assert (gnc_option_db_set_number_option (odb, OPTION_SECTION_ACCOUNTS,
					       OPTION_NAME_AUTO_READONLY_DAYS,
					       17));
    qof_book_save_options (book, gnc_option_db_save, odb, TRUE);
    g_assert_cmpstr (kvp_frame_get_string (slots,  "options/Accounts/Use Trading Accounts"), == , "t");
    g_assert_cmpstr (kvp_frame_get_string (slots,  "options/Accounts/Use Split Action Field for Number"), == , "t");
    g_assert_cmpstr (kvp_frame_get_string (slots, "options/Business/Company Name"), ==, "Bogus Company");
    g_assert_cmpfloat (kvp_frame_get_double (slots, "options/Accounts/Day Threshold for Read-Only Transactions (red line)"), ==, 17);

    gnc_option_db_destroy (odb);
}

static void
test_option_save_book_currency (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = fixture->book;
    GNCOptionDB *odb = gnc_option_db_new_for_type (QOF_ID_BOOK);
    KvpFrame *slots = qof_instance_get_slots (QOF_INSTANCE (book));

    g_assert (gnc_option_db_set_option (odb, OPTION_SECTION_ACCOUNTS,
						OPTION_NAME_CURRENCY_ACCOUNTING,
						scm_cons (scm_from_locale_symbol("book-currency"),
                        scm_cons (scm_from_utf8_string("GTQ"),
                        scm_cons (scm_from_locale_symbol("fifo"), SCM_EOL)))));
    qof_book_save_options (book, gnc_option_db_save, odb, TRUE);
    g_assert_cmpstr (kvp_frame_get_string(slots, "options/Accounts/Book Currency"), == , "GTQ");
    g_assert_cmpstr (kvp_frame_get_string(slots, "options/Accounts/Default Gains Policy"), == , "fifo");

    gnc_option_db_destroy (odb);
}

void
test_suite_option_util (void)
{
    GNC_TEST_ADD (suitename, "Option DB Load", Fixture, NULL, setup_kvp, test_option_load, teardown);
    GNC_TEST_ADD (suitename, "Option DB Load - Book Currency", Fixture, NULL, setup_kvp_book_currency, test_option_load_book_currency, teardown);
    GNC_TEST_ADD (suitename, "Option DB Save", Fixture, NULL, setup, test_option_save, teardown);
    GNC_TEST_ADD (suitename, "Option DB Save - Book Currency", Fixture, NULL, setup, test_option_save_book_currency, teardown);
}
