/*
 * Created by:	Aaron Laws
 * Copyright (c) 2017 Aaron Laws
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include "config.h"
#include <glib.h>
#include <libguile.h>

#include "gnc-features.h"
#include "gnc-module.h"
#include "import-parse.h"

#include "import-match-map.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"
#include <unittest-support.h>
#include "kvp_frame.h"

typedef struct
{
    QofBook * book;
    Account * account1;
    Account * account2;
    Account * account3;
    GncImportMatchMap * map1;
    GList * tokens1;
    gchar * account1_guid;
    gchar * account2_guid;
} Fixture;

static gchar const * token1 = "one/one";
static gchar const * token2 = "two";
static gchar const * account1_name = "Asset";
static gchar const * account2_name = "Liability";
static gchar const * account3_name = "Equity";

static void
setup (Fixture *fixture, gconstpointer pData)
{
    Account * root;
    fixture->book = qof_book_new ();
    root = gnc_account_create_root (fixture->book);
    fixture->account1 = xaccMallocAccount (fixture->book);
    gnc_account_append_child (root, fixture->account1);
    xaccAccountSetName (fixture->account1, account1_name);
    fixture->account2 = xaccMallocAccount (fixture->book);
    gnc_account_append_child (root, fixture->account2);
    xaccAccountSetName (fixture->account2, account2_name);
    fixture->account3 = xaccMallocAccount (fixture->book);
    gnc_account_append_child (root, fixture->account3);
    xaccAccountSetName (fixture->account3, account3_name);
    fixture->map1 = gnc_imap_create_from_account (fixture->account1);
    fixture->tokens1 = g_list_append (g_list_append (NULL, g_strdup (token1)), g_strdup (token2));
    fixture->account1_guid = g_strdup (guid_to_string (xaccAccountGetGUID (fixture->account1)));
    fixture->account2_guid = g_strdup (guid_to_string (xaccAccountGetGUID (fixture->account2)));
}

static void
teardown (Fixture * fixture, gconstpointer pData)
{
    g_free (fixture->account1_guid);
    g_free (fixture->account2_guid);
    xaccAccountBeginEdit (gnc_book_get_root_account (fixture->book));
    xaccAccountDestroy (gnc_book_get_root_account (fixture->book));
    qof_book_destroy (fixture->book);
    gnc_imap_destroy (fixture->map1);
}

/*
 * Make sure that added imap entries are put in the right place and incremented properly.
 */
static void
write_normal_bayes_entry (Fixture * fixture, gconstpointer pData)
{
    /* By default, this version of gnucash should write divided KVP bayes entries without
     * dividing the token portion. */
    KvpFrame * frame;
    KvpValue * count_value;
    gint64 count;
    gnc_imap_add_account_bayes (fixture->map1, fixture->tokens1, fixture->account1);
    frame = qof_instance_get_slots (QOF_INSTANCE (fixture->account1));
    count_value = kvp_frame_get_slot_path (frame, "import-map-bayes", token1, account1_name, NULL);
    count = kvp_value_get_gint64 (count_value);
    g_assert_cmpint (1, ==, count);
    gnc_imap_add_account_bayes (fixture->map1, fixture->tokens1, fixture->account1);
    frame = qof_instance_get_slots (QOF_INSTANCE (fixture->account1));
    count_value = kvp_frame_get_slot_path (frame, "import-map-bayes", token1, account1_name, NULL);
    count = kvp_value_get_gint64 (count_value);
    g_assert_cmpint (2, ==, count);
}

/*
 * Test adding entries to the import map when FEATURE_GUID_BAYESIAN is enabled.
 */
static void
write_guid_bayes_entry (Fixture * fixture, gconstpointer pData)
{
    /* When the guid feature is present, gnucash should write and retrieve bayes
     * information by account GUID, but still hierarchically.*/
    KvpFrame * frame;
    KvpValue * count_value;
    gint64 count;
    kvp_frame_set_string (qof_instance_get_slots (QOF_INSTANCE (fixture->book)),
            "features/" GNC_FEATURE_GUID_BAYESIAN, "value");
    gnc_imap_add_account_bayes (fixture->map1, fixture->tokens1, fixture->account1);
    frame = qof_instance_get_slots (QOF_INSTANCE (fixture->account1));
    /*We'll look up the added import map information.*/
    count_value = kvp_frame_get_slot_path (frame, "import-map-bayes", token1, fixture->account1_guid, NULL);
    count = kvp_value_get_gint64 (count_value);
    g_assert_cmpint (1, ==, count);
    gnc_imap_add_account_bayes (fixture->map1, fixture->tokens1, fixture->account1);
    frame = qof_instance_get_slots (QOF_INSTANCE (fixture->account1));
    count_value = kvp_frame_get_slot_path (frame, "import-map-bayes", token1, fixture->account1_guid, NULL);
    count = kvp_value_get_gint64 (count_value);
    g_assert_cmpint (2, ==, count);
    gnc_imap_add_account_bayes (fixture->map1, fixture->tokens1, fixture->account1);
    frame = qof_instance_get_slots (QOF_INSTANCE (fixture->account1));
    count_value = kvp_frame_get_slot_path (frame, "import-map-bayes", token1, fixture->account1_guid, NULL);
    count = kvp_value_get_gint64 (count_value);
    g_assert_cmpint (3, ==, count);
}

/*
 * File created in 2.7.1 (or 2.7.2) with GUID_BAYESIAN, then edited
 * in 2.6.17. The former will create the import map with GUIDs, and the latter
 * may add account-name entries. This version of gnucash should be able
 * to cope with this.
 */
static void
group_account_name_and_guid_in_mixed_case (Fixture * fixture, gconstpointer pData)
{
    KvpFrame * frame;
    KvpValue * count_value;
    Account * chosen;
    gint64 count;
    kvp_frame_set_string (qof_instance_get_slots (QOF_INSTANCE (fixture->book)),
            "features/" GNC_FEATURE_GUID_BAYESIAN, "value");
    frame = qof_instance_get_slots (QOF_INSTANCE (fixture->account1));
    count_value = kvp_value_new_gint64 (9);
    /* Account 1 should have 18 "points" between the name and guid */
    kvp_frame_set_slot_path (frame, count_value, "import-map-bayes", token1, account1_name, NULL);
    kvp_frame_set_slot_path (frame, count_value, "import-map-bayes", token1, fixture->account1_guid, NULL);
    kvp_value_delete (count_value);
    count_value = kvp_value_new_gint64 (1);
    /* Account 2 and Account 3 should have 1 point each. */
    kvp_frame_set_slot_path (frame, count_value, "import-map-bayes", token1, fixture->account2_guid, NULL);
    kvp_frame_set_slot_path (frame, count_value, "import-map-bayes", token1, account3_name, NULL);
    kvp_value_delete (count_value);
    /* One might think that it's sufficient to give 4 total points (2 each) to account 1 name and guid
     * and 3 points to account 2 and account 3. This way, acconut 1 would win.
     * One would be mistaken in this case as it is not a simple majority win. The assurance of
     * victory must be rather great: 90%.
     * If the grouping by account name and guid is *not* working properly, this test will fail.
     */
    chosen = gnc_imap_find_account_bayes (fixture->map1, fixture->tokens1);
    g_assert (chosen == fixture->account1);
}

/*
 * File created in 2.7.3 with GUID_FLAT_BAYESIAN should be readable.
 */
static void
read_flat_bayes (Fixture * fixture, gconstpointer pData)
{
    KvpFrame * frame;
    KvpValue * count_value;
    Account * chosen;
    gchar * path = g_strdup_printf ("%s/%s/%s", "import-map-bayes", token1, fixture->account1_guid);
    kvp_frame_set_string (qof_instance_get_slots (QOF_INSTANCE (fixture->book)),
            "features/" GNC_FEATURE_GUID_FLAT_BAYESIAN, "value");
    frame = qof_instance_get_slots (QOF_INSTANCE (fixture->account1));
    count_value = kvp_value_new_gint64 (1);
    kvp_frame_set_slot (frame, path, count_value);
    kvp_value_delete (count_value);
    chosen = gnc_imap_find_account_bayes (fixture->map1, fixture->tokens1);
    g_assert (chosen == fixture->account1);
}

static void
write_flat_bayes (Fixture * fixture, gconstpointer pData)
{
    KvpFrame * frame;
    KvpValue * count_value;
    gint64 count;
    gchar * path = g_strdup_printf ("%s/%s/%s", "import-map-bayes", token1, fixture->account1_guid);
    kvp_frame_set_string (qof_instance_get_slots (QOF_INSTANCE (fixture->book)),
            "features/" GNC_FEATURE_GUID_FLAT_BAYESIAN, "value");
    gnc_imap_add_account_bayes (fixture->map1, fixture->tokens1, fixture->account1);
    gnc_imap_add_account_bayes (fixture->map1, fixture->tokens1, fixture->account1);
    frame = qof_instance_get_slots (QOF_INSTANCE (fixture->account1));
    count_value = kvp_frame_get_slot (frame, path);
    count = kvp_value_get_gint64 (count_value);
    g_assert (count == 2);
}

/*
 * We should always be able to retrieve an account from the import map after
 * entries for that account are added.
 */
static void
find_normal_bayes_account (Fixture * fixture, gconstpointer pData)
{
    KvpFrame * frame;
    Account * acc;
    gnc_imap_add_account_bayes (fixture->map1, fixture->tokens1, fixture->account1);
    gnc_imap_add_account_bayes (fixture->map1, fixture->tokens1, fixture->account1);
    acc = gnc_imap_find_account_bayes (fixture->map1, fixture->tokens1);
    g_assert (acc == fixture->account1);
}

static void
find_flat_bayes_account (Fixture * fixture, gconstpointer pData)
{
    KvpFrame * frame;
    Account * acc;
    kvp_frame_set_string (qof_instance_get_slots (QOF_INSTANCE (fixture->book)),
            "features/" GNC_FEATURE_GUID_FLAT_BAYESIAN, "value");
    gnc_imap_add_account_bayes (fixture->map1, fixture->tokens1, fixture->account1);
    gnc_imap_add_account_bayes (fixture->map1, fixture->tokens1, fixture->account1);
    gnc_imap_add_account_bayes (fixture->map1, fixture->tokens1, fixture->account1);
    gnc_imap_add_account_bayes (fixture->map1, fixture->tokens1, fixture->account1);
    gnc_imap_add_account_bayes (fixture->map1, fixture->tokens1, fixture->account2);
    acc = gnc_imap_find_account_bayes (fixture->map1, fixture->tokens1);
    g_assert (acc == fixture->account1);
}

static void
find_guid_bayes_account (Fixture * fixture, gconstpointer pData)
{
    KvpFrame * frame;
    Account * acc;
    kvp_frame_set_string (qof_instance_get_slots (QOF_INSTANCE (fixture->book)),
            "features/" GNC_FEATURE_GUID_BAYESIAN, "value");
    gnc_imap_add_account_bayes (fixture->map1, fixture->tokens1, fixture->account1);
    gnc_imap_add_account_bayes (fixture->map1, fixture->tokens1, fixture->account1);
    gnc_imap_add_account_bayes (fixture->map1, fixture->tokens1, fixture->account1);
    gnc_imap_add_account_bayes (fixture->map1, fixture->tokens1, fixture->account1);
    gnc_imap_add_account_bayes (fixture->map1, fixture->tokens1, fixture->account2);
    acc = gnc_imap_find_account_bayes (fixture->map1, fixture->tokens1);
    g_assert (acc == fixture->account1);
}

static void
find_normal_bayes_when_non_exists (Fixture * fixture, gconstpointer pData)
{
    KvpFrame * frame;
    Account * acc;
    acc = gnc_imap_find_account_bayes (fixture->map1, fixture->tokens1);
    g_assert (acc == NULL);
}

static gchar const * suitename = "/import-export/test-flat-bayes";

int
main (int argc, char *argv[])
{
    int result;
    qof_init();
    g_test_init (&argc, &argv, NULL);
    GNC_TEST_ADD (suitename, "normal_bayes_entry", Fixture, NULL, setup, &write_normal_bayes_entry, teardown);
    GNC_TEST_ADD (suitename, "guid_bayes_entry", Fixture, NULL, setup, &write_guid_bayes_entry, teardown);
    GNC_TEST_ADD (suitename, "normal_bayes_find", Fixture, NULL, setup, &find_normal_bayes_account, teardown);
    GNC_TEST_ADD (suitename, "guid_bayes_find", Fixture, NULL, setup, &find_guid_bayes_account, teardown);
    GNC_TEST_ADD (suitename, "normal_bayes_find_none", Fixture, NULL, setup, &find_normal_bayes_when_non_exists, teardown);
    GNC_TEST_ADD (suitename, "Mixed case", Fixture, NULL, setup, &group_account_name_and_guid_in_mixed_case, teardown);
    GNC_TEST_ADD (suitename, "Read flat bayes", Fixture, NULL, setup, &read_flat_bayes, teardown);
    GNC_TEST_ADD (suitename, "Write flat bayes", Fixture, NULL, setup, &write_flat_bayes, teardown);
    result =  g_test_run();
    qof_close();
}

