/*
 * test-standing-orders.c -- AqBanking standing-order synchronization tests
 * Copyright 2026 copystring
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 */

#include <config.h>

#include <glib.h>
#include <aqbanking/types/imexporter_accountinfo.h>
#include <aqbanking/types/imexporter_context.h>
#include <aqbanking/types/transaction.h>
#include <aqbanking/types/value.h>
#include <gwenhywfar/gwendate.h>

#include <unittest-support.h>
#include "Account.h"
#include "SX-book.h"
#include "SchedXaction.h"
#include "Split.h"
#include "Transaction.h"
#include "gnc-ab-kvp.h"
#include "gnc-ab-standing-orders.h"
#include "gnc-commodity.h"

#define LOCAL_IBAN "DE92500105175447461406"
#define LOCAL_BANK_CODE "50010517"
#define LOCAL_ACCOUNT_NUMBER "5447461406"
#define EXTERNAL_IBAN "GB82WEST12345698765432"
#define INTERNAL_IBAN "FR7630006000011234567890189"
#define LINKED_IBAN "DE24500105175804977567"
#define LINKED_BANK_CODE "50010517"
#define LINKED_ACCOUNT_NUMBER "5804977567"

void test_suite_aqb_standing_orders (void);

static const gchar *suitename = "/import-export/aqb/standing-orders";

typedef struct
{
    QofBook *book;
    gnc_commodity *currency;
    Account *local_account;
    Account *internal_account;
    Account *linked_account;
    Account *category_account;
} StandingOrderFixture;

static Account *
create_account (StandingOrderFixture *fixture, const gchar *name,
                GNCAccountType type)
{
    Account *account = xaccMallocAccount (fixture->book);

    xaccAccountBeginEdit (account);
    xaccAccountSetName (account, name);
    xaccAccountSetType (account, type);
    xaccAccountSetCommodity (account, fixture->currency);
    gnc_account_append_child (gnc_book_get_root_account (fixture->book),
                              account);
    xaccAccountCommitEdit (account);

    return account;
}

static void
setup (StandingOrderFixture *fixture, gconstpointer user_data)
{
    gnc_commodity_table *table;

    fixture->book = qof_book_new ();
    table = gnc_commodity_table_get_table (fixture->book);
    fixture->currency = gnc_commodity_table_lookup (
        table, GNC_COMMODITY_NS_CURRENCY, "EUR");
    g_assert_nonnull (fixture->currency);
    fixture->local_account = create_account (fixture, "Checking",
                                             ACCT_TYPE_BANK);
    fixture->internal_account = create_account (fixture, "Savings",
                                                 ACCT_TYPE_BANK);
    fixture->linked_account = create_account (fixture, "House",
                                              ACCT_TYPE_BANK);
    fixture->category_account = create_account (fixture, "Rent",
                                                 ACCT_TYPE_EXPENSE);

    gnc_ab_set_account_bankcode (fixture->local_account, LOCAL_BANK_CODE);
    gnc_ab_set_account_accountid (fixture->local_account,
                                  LOCAL_ACCOUNT_NUMBER);
    xaccAccountBeginEdit (fixture->internal_account);
    xaccAccountSetCode (fixture->internal_account, INTERNAL_IBAN);
    xaccAccountCommitEdit (fixture->internal_account);
    gnc_ab_set_account_bankcode (fixture->linked_account, LINKED_BANK_CODE);
    gnc_ab_set_account_accountid (fixture->linked_account,
                                  LINKED_ACCOUNT_NUMBER);
}

static void
teardown (StandingOrderFixture *fixture, gconstpointer user_data)
{
    qof_book_destroy (fixture->book);
}

static AB_IMEXPORTER_CONTEXT *
snapshot_new (void)
{
    AB_IMEXPORTER_CONTEXT *context = AB_ImExporterContext_new ();
    AB_IMEXPORTER_ACCOUNTINFO *account_info =
        AB_ImExporterAccountInfo_new ();

    AB_ImExporterAccountInfo_SetIban (account_info, LOCAL_IBAN);
    AB_ImExporterAccountInfo_SetBankCode (account_info, LOCAL_BANK_CODE);
    AB_ImExporterAccountInfo_SetAccountNumber (account_info,
                                               LOCAL_ACCOUNT_NUMBER);
    AB_ImExporterContext_AddAccountInfo (context, account_info);

    return context;
}

static AB_TRANSACTION *
snapshot_add_order (AB_IMEXPORTER_CONTEXT *context, const gchar *id,
                    const gchar *remote_iban, const gchar *amount,
                    const gchar *purpose)
{
    AB_IMEXPORTER_ACCOUNTINFO_LIST *account_infos =
        AB_ImExporterContext_GetAccountInfoList (context);
    AB_IMEXPORTER_ACCOUNTINFO *account_info =
        AB_ImExporterAccountInfo_List_First (account_infos);
    AB_TRANSACTION *transaction = AB_Transaction_new ();
    AB_VALUE *value = AB_Value_fromString (amount);
    GWEN_DATE *first_date =
        GWEN_Date_fromStringWithTemplate ("20260818", "YYYYMMDD");
    GWEN_DATE *next_date =
        GWEN_Date_fromStringWithTemplate ("20260918", "YYYYMMDD");

    AB_Transaction_SetType (transaction, AB_Transaction_TypeStandingOrder);
    AB_Transaction_SetFiId (transaction, id);
    AB_Transaction_SetLocalIban (transaction, LOCAL_IBAN);
    AB_Transaction_SetLocalBankCode (transaction, LOCAL_BANK_CODE);
    AB_Transaction_SetLocalAccountNumber (transaction,
                                          LOCAL_ACCOUNT_NUMBER);
    AB_Transaction_SetRemoteIban (transaction, remote_iban);
    AB_Transaction_SetValue (transaction, value);
    AB_Transaction_SetPurpose (transaction, purpose);
    AB_Transaction_SetPeriod (transaction, AB_Transaction_PeriodMonthly);
    AB_Transaction_SetCycle (transaction, 1);
    AB_Transaction_SetExecutionDay (transaction, 18);
    AB_Transaction_SetFirstDate (transaction, first_date);
    AB_Transaction_SetNextDate (transaction, next_date);
    AB_ImExporterAccountInfo_AddTransaction (account_info, transaction);

    GWEN_Date_free (next_date);
    GWEN_Date_free (first_date);
    AB_Value_free (value);

    return transaction;
}

static SchedXaction *
find_imported_sx (StandingOrderFixture *fixture, const gchar *id)
{
    SchedXactions *sxes = gnc_book_get_schedxactions (fixture->book);
    GList *node;

    for (node = sxes->sx_list; node; node = node->next)
    {
        SchedXaction *sx = node->data;
        gchar *stored_id = gnc_ab_get_standing_order_id (sx);
        gboolean matches = g_strcmp0 (stored_id, id) == 0;

        g_free (stored_id);
        if (matches)
            return sx;
    }

    return NULL;
}

static Account *
template_split_account (StandingOrderFixture *fixture, Split *split)
{
    GncGUID *account_guid = NULL;
    Account *account;

    qof_instance_get (QOF_INSTANCE (split),
                      "sx-account", &account_guid,
                      NULL);
    account = account_guid ? xaccAccountLookup (account_guid, fixture->book)
                           : NULL;
    guid_free (account_guid);
    return account;
}

static Split *
find_template_split (StandingOrderFixture *fixture, SchedXaction *sx,
                     Account *account)
{
    GList *splits = xaccSchedXactionGetSplits (sx);
    GList *node;
    Split *matched_split = NULL;

    for (node = splits; node; node = node->next)
    {
        Split *split = node->data;

        if (template_split_account (fixture, split) == account)
        {
            matched_split = split;
            break;
        }
    }
    g_list_free (splits);

    return matched_split;
}

static Split *
find_counter_split (StandingOrderFixture *fixture, SchedXaction *sx)
{
    GList *splits = xaccSchedXactionGetSplits (sx);
    GList *node;
    Split *counter_split = NULL;

    for (node = splits; node; node = node->next)
    {
        Split *split = node->data;

        if (template_split_account (fixture, split) != fixture->local_account)
        {
            counter_split = split;
            break;
        }
    }
    g_list_free (splits);

    return counter_split;
}

static gnc_numeric
split_numeric (Split *split, const gchar *property)
{
    gnc_numeric result = gnc_numeric_zero ();
    gnc_numeric *value = NULL;

    qof_instance_get (QOF_INSTANCE (split), property, &value, NULL);
    if (value)
        result = *value;
    g_free (value);

    return result;
}

static void
assert_template_amount (StandingOrderFixture *fixture, SchedXaction *sx,
                        gnc_numeric expected)
{
    Split *local_split = find_template_split (fixture, sx,
                                              fixture->local_account);
    Split *counter_split = find_counter_split (fixture, sx);

    g_assert_nonnull (local_split);
    g_assert_nonnull (counter_split);
    g_assert_true (gnc_numeric_equal (
        split_numeric (local_split, "sx-credit-numeric"), expected));
    g_assert_true (gnc_numeric_equal (
        split_numeric (counter_split, "sx-debit-numeric"), expected));
}

static void
sync_result_clear (GncABStandingOrderSyncResult *result)
{
    g_list_free (result->to_edit);
    result->to_edit = NULL;
}

static void
test_aqb_standing_order_external_account (StandingOrderFixture *fixture,
                                          gconstpointer user_data)
{
    AB_IMEXPORTER_CONTEXT *snapshot = snapshot_new ();
    GncABStandingOrderSyncResult result;
    SchedXaction *sx;
    Split *counter_split;
    Account *counter_account;

    snapshot_add_order (snapshot, "external-order", EXTERNAL_IBAN,
                        "75.55", "Monthly rent");
    result = gnc_ab_import_standing_orders (snapshot,
                                            fixture->local_account);

    g_assert_cmpuint (result.received, ==, 1);
    g_assert_cmpuint (result.created, ==, 1);
    g_assert_cmpuint (result.updated, ==, 0);
    sx = find_imported_sx (fixture, "fiid:external-order");
    g_assert_nonnull (sx);
    g_assert_cmpint (xaccAccountGetSplitsSize (sx->template_acct), ==, 2);
    assert_template_amount (fixture, sx, gnc_numeric_create (7555, 100));

    counter_split = find_counter_split (fixture, sx);
    counter_account = template_split_account (fixture, counter_split);
    g_assert_nonnull (counter_account);
    g_assert_true (g_str_has_prefix (xaccAccountGetName (counter_account),
                                     "Imbalance-"));

    sync_result_clear (&result);
    AB_ImExporterContext_free (snapshot);
}

static void
test_aqb_standing_order_account_code_is_not_iban (StandingOrderFixture *fixture,
                                                  gconstpointer user_data)
{
    AB_IMEXPORTER_CONTEXT *snapshot = snapshot_new ();
    GncABStandingOrderSyncResult result;
    SchedXaction *sx;
    Split *counter_split;

    snapshot_add_order (snapshot, "internal-order", INTERNAL_IBAN,
                        "150.00", "Savings");
    result = gnc_ab_import_standing_orders (snapshot,
                                            fixture->local_account);

    g_assert_cmpuint (result.created, ==, 1);
    sx = find_imported_sx (fixture, "fiid:internal-order");
    g_assert_nonnull (sx);
    counter_split = find_counter_split (fixture, sx);
    g_assert_true (template_split_account (fixture, counter_split)
                   != fixture->internal_account);
    g_assert_true (g_str_has_prefix (xaccAccountGetName (
        template_split_account (fixture, counter_split)), "Imbalance-"));

    sync_result_clear (&result);
    AB_ImExporterContext_free (snapshot);
}

static void
test_aqb_standing_order_linked_account (StandingOrderFixture *fixture,
                                        gconstpointer user_data)
{
    AB_IMEXPORTER_CONTEXT *snapshot = snapshot_new ();
    AB_TRANSACTION *order;
    GncABStandingOrderSyncResult result;
    gnc_commodity *usd;
    SchedXaction *sx;
    Split *counter_split;

    order = snapshot_add_order (snapshot, "linked-order", LINKED_IBAN,
                                "165.00", "House account");
    AB_Transaction_SetRemoteBankCode (order, LINKED_BANK_CODE);
    AB_Transaction_SetRemoteAccountNumber (order, LINKED_ACCOUNT_NUMBER);

    usd = gnc_commodity_table_lookup (
        gnc_commodity_table_get_table (fixture->book),
        GNC_COMMODITY_NS_CURRENCY, "USD");
    g_assert_nonnull (usd);
    xaccAccountBeginEdit (fixture->linked_account);
    xaccAccountSetCommodity (fixture->linked_account, usd);
    xaccAccountCommitEdit (fixture->linked_account);
    result = gnc_ab_import_standing_orders (snapshot,
                                            fixture->local_account);

    g_assert_cmpuint (result.created, ==, 1);
    sx = find_imported_sx (fixture, "fiid:linked-order");
    g_assert_nonnull (sx);
    counter_split = find_counter_split (fixture, sx);
    g_assert_true (template_split_account (fixture, counter_split)
                   == fixture->linked_account);

    sync_result_clear (&result);
    AB_ImExporterContext_free (snapshot);
}

static void
test_aqb_standing_order_weekly_without_purpose (StandingOrderFixture *fixture,
                                                gconstpointer user_data)
{
    AB_IMEXPORTER_CONTEXT *snapshot = snapshot_new ();
    AB_TRANSACTION *order;
    GncABStandingOrderSyncResult result;
    SchedXaction *sx;
    GList *schedule;
    Split *counter_split;

    order = snapshot_add_order (snapshot, "weekly-order", EXTERNAL_IBAN,
                                "25.00", NULL);
    AB_Transaction_SetPeriod (order, AB_Transaction_PeriodWeekly);
    AB_Transaction_SetCycle (order, 2);
    result = gnc_ab_import_standing_orders (snapshot,
                                            fixture->local_account);

    g_assert_cmpuint (result.created, ==, 1);
    sx = find_imported_sx (fixture, "fiid:weekly-order");
    g_assert_nonnull (sx);
    schedule = gnc_sx_get_schedule (sx);
    g_assert_cmpuint (g_list_length (schedule), ==, 1);
    g_assert_cmpint (recurrenceGetPeriodType (schedule->data), ==,
                     PERIOD_WEEK);
    g_assert_cmpuint (recurrenceGetMultiplier (schedule->data), ==, 2);
    counter_split = find_counter_split (fixture, sx);
    g_assert_cmpstr (xaccTransGetDescription (
        xaccSplitGetParent (counter_split)), ==, "");

    sync_result_clear (&result);
    AB_ImExporterContext_free (snapshot);
}

static void
test_aqb_standing_order_past_start_uses_next_occurrence (StandingOrderFixture *fixture,
                                                         gconstpointer user_data)
{
    AB_IMEXPORTER_CONTEXT *snapshot = snapshot_new ();
    AB_TRANSACTION *order;
    GWEN_DATE *past_date;
    GncABStandingOrderSyncResult result;
    SchedXaction *sx;
    const GDate *start_date;
    GDate today;

    order = snapshot_add_order (snapshot, "past-start", EXTERNAL_IBAN,
                                "30.00", "Existing order");
    past_date = GWEN_Date_fromStringWithTemplate ("20000118", "YYYYMMDD");
    AB_Transaction_SetFirstDate (order, past_date);
    AB_Transaction_SetNextDate (order, past_date);
    GWEN_Date_free (past_date);

    result = gnc_ab_import_standing_orders (snapshot,
                                            fixture->local_account);

    g_assert_cmpuint (result.created, ==, 1);
    sx = find_imported_sx (fixture, "fiid:past-start");
    g_assert_nonnull (sx);
    start_date = xaccSchedXactionGetStartDate (sx);
    gnc_gdate_set_today (&today);
    g_assert_true (g_date_valid (start_date));
    g_assert_cmpint (g_date_compare (start_date, &today), >=, 0);
    g_assert_cmpuint (g_date_get_day (start_date), ==, 18);

    sync_result_clear (&result);
    AB_ImExporterContext_free (snapshot);
}

static void
test_aqb_standing_order_update_preserves_local_edits (StandingOrderFixture *fixture,
                                                      gconstpointer user_data)
{
    AB_IMEXPORTER_CONTEXT *snapshot = snapshot_new ();
    AB_TRANSACTION *order;
    GncABStandingOrderSyncResult result;
    SchedXaction *sx;
    Split *counter_split;
    Split *local_split;
    Split *tagging_split;
    Transaction *template_trans;
    GList *schedule;
    gboolean autocreate;
    gboolean notify;
    gnc_numeric zero = gnc_numeric_zero ();
    const GncGUID *category_guid =
        xaccAccountGetGUID (fixture->category_account);
    const GncGUID *tagging_guid =
        xaccAccountGetGUID (fixture->internal_account);

    snapshot_add_order (snapshot, "updated-order", EXTERNAL_IBAN,
                        "75.00", "Initial purpose");
    result = gnc_ab_import_standing_orders (snapshot,
                                            fixture->local_account);
    g_assert_cmpuint (result.created, ==, 1);
    sx = find_imported_sx (fixture, "fiid:updated-order");
    g_assert_cmpuint (g_list_length (result.to_edit), ==, 1);
    g_assert_true (result.to_edit->data == sx);
    sync_result_clear (&result);
    counter_split = find_counter_split (fixture, sx);
    template_trans = xaccSplitGetParent (counter_split);
    xaccSchedXactionSetName (sx, "Local standing order");
    xaccTransBeginEdit (template_trans);
    xaccTransSetDescription (template_trans, "Local description");
    xaccTransSetNotes (template_trans, "Local notes");
    xaccSplitSetMemo (counter_split, "Local memo");
    xaccSplitSetAction (counter_split, "Local action");
    qof_instance_set (QOF_INSTANCE (counter_split),
                      "sx-account", category_guid,
                      NULL);
    tagging_split = xaccMallocSplit (fixture->book);
    xaccSplitSetMemo (tagging_split, "Local tag");
    xaccSplitSetAction (tagging_split, "Tag");
    xaccAccountInsertSplit (sx->template_acct, tagging_split);
    qof_instance_set (QOF_INSTANCE (tagging_split),
                      "sx-credit-formula", "",
                      "sx-credit-numeric", &zero,
                      "sx-debit-formula", "",
                      "sx-debit-numeric", &zero,
                      "sx-account", tagging_guid,
                      NULL);
    xaccTransAppendSplit (template_trans, tagging_split);
    xaccTransCommitEdit (template_trans);
    xaccSchedXactionSetAutoCreate (sx, TRUE, TRUE);
    xaccSchedXactionSetAdvanceCreation (sx, 93);
    xaccSchedXactionSetAdvanceReminder (sx, 14);
    AB_ImExporterContext_free (snapshot);

    snapshot = snapshot_new ();
    order = snapshot_add_order (snapshot, "updated-order", EXTERNAL_IBAN,
                                "80.00", "Updated purpose");
    AB_Transaction_SetPeriod (order, AB_Transaction_PeriodWeekly);
    AB_Transaction_SetCycle (order, 2);
    result = gnc_ab_import_standing_orders (snapshot,
                                            fixture->local_account);

    g_assert_cmpuint (result.created, ==, 0);
    g_assert_cmpuint (result.updated, ==, 1);
    g_assert_cmpuint (g_list_length (
        gnc_book_get_schedxactions (fixture->book)->sx_list), ==, 1);
    xaccSchedXactionGetAutoCreate (sx, &autocreate, &notify);
    g_assert_true (autocreate);
    g_assert_true (notify);
    g_assert_cmpint (xaccSchedXactionGetAdvanceCreation (sx), ==, 93);
    g_assert_cmpint (xaccSchedXactionGetAdvanceReminder (sx), ==, 14);
    schedule = gnc_sx_get_schedule (sx);
    g_assert_cmpint (recurrenceGetPeriodType (schedule->data), ==,
                     PERIOD_MONTH);
    g_assert_cmpuint (recurrenceGetMultiplier (schedule->data), ==, 1);
    g_assert_cmpstr (xaccSchedXactionGetName (sx), ==,
                     "Local standing order");
    counter_split = find_counter_split (fixture, sx);
    g_assert_true (template_split_account (fixture, counter_split)
                   == fixture->category_account);
    g_assert_cmpstr (xaccTransGetDescription (xaccSplitGetParent (counter_split)),
                     ==, "Local description");
    g_assert_cmpstr (xaccTransGetNotes (xaccSplitGetParent (counter_split)),
                     ==, "Local notes");
    g_assert_cmpstr (xaccSplitGetMemo (counter_split), ==, "Local memo");
    g_assert_cmpstr (xaccSplitGetAction (counter_split), ==, "Local action");
    local_split = find_template_split (fixture, sx, fixture->local_account);
    g_assert_true (gnc_numeric_equal (
        split_numeric (local_split, "sx-credit-numeric"),
        gnc_numeric_create (80, 1)));
    g_assert_true (gnc_numeric_equal (
        split_numeric (counter_split, "sx-debit-numeric"),
        gnc_numeric_create (75, 1)));
    g_assert_cmpint (xaccAccountGetSplitsSize (sx->template_acct), ==, 3);
    g_assert_true (template_split_account (fixture, tagging_split)
                   == fixture->internal_account);
    g_assert_cmpstr (xaccSplitGetMemo (tagging_split), ==, "Local tag");
    g_assert_cmpstr (xaccSplitGetAction (tagging_split), ==, "Tag");
    g_assert_true (gnc_numeric_zero_p (
        split_numeric (tagging_split, "sx-credit-numeric")));
    g_assert_true (gnc_numeric_zero_p (
        split_numeric (tagging_split, "sx-debit-numeric")));
    g_assert_cmpuint (g_list_length (result.to_edit), ==, 1);
    g_assert_true (result.to_edit->data == sx);

    sync_result_clear (&result);
    AB_ImExporterContext_free (snapshot);
}

static void
test_aqb_standing_order_missing_is_disabled (StandingOrderFixture *fixture,
                                             gconstpointer user_data)
{
    AB_IMEXPORTER_CONTEXT *snapshot = snapshot_new ();
    GncABStandingOrderSyncResult result;
    SchedXaction *sx;

    snapshot_add_order (snapshot, "removed-order", EXTERNAL_IBAN,
                        "40.00", "Service provider");
    result = gnc_ab_import_standing_orders (snapshot,
                                            fixture->local_account);
    g_assert_cmpuint (result.created, ==, 1);
    sx = find_imported_sx (fixture, "fiid:removed-order");
    g_assert_true (xaccSchedXactionGetEnabled (sx));
    sync_result_clear (&result);
    AB_ImExporterContext_free (snapshot);

    snapshot = snapshot_new ();
    result = gnc_ab_import_standing_orders (snapshot,
                                            fixture->local_account);

    g_assert_cmpuint (result.disabled, ==, 1);
    g_assert_false (xaccSchedXactionGetEnabled (sx));

    sync_result_clear (&result);
    AB_ImExporterContext_free (snapshot);
    snapshot = snapshot_new ();
    snapshot_add_order (snapshot, "removed-order", EXTERNAL_IBAN,
                        "40.00", "Service provider");
    result = gnc_ab_import_standing_orders (snapshot,
                                            fixture->local_account);

    g_assert_cmpuint (result.updated, ==, 1);
    g_assert_true (xaccSchedXactionGetEnabled (sx));

    sync_result_clear (&result);
    AB_ImExporterContext_free (snapshot);
}

static void
test_aqb_standing_order_invalid_update_stays_enabled (StandingOrderFixture *fixture,
                                                      gconstpointer user_data)
{
    AB_IMEXPORTER_CONTEXT *snapshot = snapshot_new ();
    GncABStandingOrderSyncResult result;
    SchedXaction *sx;

    snapshot_add_order (snapshot, "temporarily-invalid", EXTERNAL_IBAN,
                        "40.00", "Service provider");
    result = gnc_ab_import_standing_orders (snapshot,
                                            fixture->local_account);
    g_assert_cmpuint (result.created, ==, 1);
    sx = find_imported_sx (fixture, "fiid:temporarily-invalid");
    g_assert_nonnull (sx);
    sync_result_clear (&result);
    AB_ImExporterContext_free (snapshot);

    snapshot = snapshot_new ();
    snapshot_add_order (snapshot, "temporarily-invalid", EXTERNAL_IBAN,
                        "0.00", "Service provider");
    g_test_expect_message ("gnc.import.aqbanking", G_LOG_LEVEL_WARNING,
                           "*without an amount*");
    result = gnc_ab_import_standing_orders (snapshot,
                                            fixture->local_account);
    g_test_assert_expected_messages ();

    g_assert_cmpuint (result.updated, ==, 0);
    g_assert_cmpuint (result.skipped, ==, 1);
    g_assert_cmpuint (result.disabled, ==, 0);
    g_assert_true (xaccSchedXactionGetEnabled (sx));
    assert_template_amount (fixture, sx, gnc_numeric_create (40, 1));

    sync_result_clear (&result);
    AB_ImExporterContext_free (snapshot);
}

static void
test_aqb_standing_order_does_not_claim_manual_sx (StandingOrderFixture *fixture,
                                                  gconstpointer user_data)
{
    AB_IMEXPORTER_CONTEXT *snapshot = snapshot_new ();
    SchedXaction *manual_sx = xaccSchedXactionMalloc (fixture->book);
    GncABStandingOrderSyncResult result;

    xaccSchedXactionSetName (manual_sx, "Online Banking: Same name");
    gnc_sxes_add_sx (gnc_book_get_schedxactions (fixture->book), manual_sx);
    snapshot_add_order (snapshot, "separate-order", EXTERNAL_IBAN,
                        "20.00", "Same name");
    result = gnc_ab_import_standing_orders (snapshot,
                                            fixture->local_account);

    g_assert_cmpuint (result.created, ==, 1);
    g_assert_cmpuint (g_list_length (
        gnc_book_get_schedxactions (fixture->book)->sx_list), ==, 2);

    sync_result_clear (&result);
    AB_ImExporterContext_free (snapshot);
}

static void
test_aqb_standing_order_fallback_identity_distinguishes_amounts (StandingOrderFixture *fixture,
                                                                 gconstpointer user_data)
{
    AB_IMEXPORTER_CONTEXT *snapshot = snapshot_new ();
    GncABStandingOrderSyncResult result;

    snapshot_add_order (snapshot, NULL, EXTERNAL_IBAN,
                        "10.00", "Same destination");
    snapshot_add_order (snapshot, NULL, EXTERNAL_IBAN,
                        "20.00", "Same destination");
    result = gnc_ab_import_standing_orders (snapshot,
                                            fixture->local_account);

    g_assert_cmpuint (result.received, ==, 2);
    g_assert_cmpuint (result.created, ==, 2);
    g_assert_cmpuint (g_list_length (
        gnc_book_get_schedxactions (fixture->book)->sx_list), ==, 2);

    sync_result_clear (&result);
    AB_ImExporterContext_free (snapshot);
}

static void
test_aqb_standing_order_invalid_period_is_skipped (StandingOrderFixture *fixture,
                                                   gconstpointer user_data)
{
    AB_IMEXPORTER_CONTEXT *snapshot = snapshot_new ();
    AB_TRANSACTION *order;
    GncABStandingOrderSyncResult result;

    order = snapshot_add_order (snapshot, "invalid-period", EXTERNAL_IBAN,
                                "20.00", "Unsupported schedule");
    AB_Transaction_SetPeriod (order, AB_Transaction_PeriodUnknown);
    g_test_expect_message ("gnc.import.aqbanking", G_LOG_LEVEL_WARNING,
                           "*unsupported period*");
    result = gnc_ab_import_standing_orders (snapshot,
                                            fixture->local_account);
    g_test_assert_expected_messages ();

    g_assert_cmpuint (result.created, ==, 0);
    g_assert_cmpuint (result.received, ==, 1);
    g_assert_cmpuint (result.skipped, ==, 1);
    g_assert_null (gnc_book_get_schedxactions (fixture->book)->sx_list);

    sync_result_clear (&result);
    AB_ImExporterContext_free (snapshot);
}

static void
test_aqb_standing_order_invalid_start_date_is_skipped (StandingOrderFixture *fixture,
                                                       gconstpointer user_data)
{
    AB_IMEXPORTER_CONTEXT *snapshot = snapshot_new ();
    AB_TRANSACTION *order;
    GncABStandingOrderSyncResult result;

    order = snapshot_add_order (snapshot, "invalid-start", EXTERNAL_IBAN,
                                "20.00", "Missing start date");
    AB_Transaction_SetFirstDate (order, NULL);
    AB_Transaction_SetNextDate (order, NULL);
    g_test_expect_message ("gnc.import.aqbanking", G_LOG_LEVEL_WARNING,
                           "*valid start date*");
    result = gnc_ab_import_standing_orders (snapshot,
                                            fixture->local_account);
    g_test_assert_expected_messages ();

    g_assert_cmpuint (result.created, ==, 0);
    g_assert_cmpuint (result.received, ==, 1);
    g_assert_cmpuint (result.skipped, ==, 1);
    g_assert_null (gnc_book_get_schedxactions (fixture->book)->sx_list);

    sync_result_clear (&result);
    AB_ImExporterContext_free (snapshot);
}

static void
test_aqb_standing_order_zero_amount_is_skipped (StandingOrderFixture *fixture,
                                                gconstpointer user_data)
{
    AB_IMEXPORTER_CONTEXT *snapshot = snapshot_new ();
    GncABStandingOrderSyncResult result;

    snapshot_add_order (snapshot, "zero-amount", EXTERNAL_IBAN,
                        "0.00", "Invalid amount");
    g_test_expect_message ("gnc.import.aqbanking", G_LOG_LEVEL_WARNING,
                           "*without an amount*");
    result = gnc_ab_import_standing_orders (snapshot,
                                            fixture->local_account);
    g_test_assert_expected_messages ();

    g_assert_cmpuint (result.received, ==, 1);
    g_assert_cmpuint (result.created, ==, 0);
    g_assert_cmpuint (result.skipped, ==, 1);
    g_assert_null (gnc_book_get_schedxactions (fixture->book)->sx_list);

    sync_result_clear (&result);
    AB_ImExporterContext_free (snapshot);
}

static void
test_aqb_standing_order_missing_source_details_uses_default (StandingOrderFixture *fixture,
                                                             gconstpointer user_data)
{
    AB_IMEXPORTER_CONTEXT *snapshot = snapshot_new ();
    AB_IMEXPORTER_ACCOUNTINFO_LIST *account_infos =
        AB_ImExporterContext_GetAccountInfoList (snapshot);
    AB_IMEXPORTER_ACCOUNTINFO *account_info =
        AB_ImExporterAccountInfo_List_First (account_infos);
    GncABStandingOrderSyncResult result;

    AB_ImExporterAccountInfo_SetIban (account_info, NULL);
    AB_ImExporterAccountInfo_SetBankCode (account_info, NULL);
    AB_ImExporterAccountInfo_SetAccountNumber (account_info, NULL);
    snapshot_add_order (snapshot, "missing-source-details", EXTERNAL_IBAN,
                        "20.00", "No account details");
    result = gnc_ab_import_standing_orders (snapshot,
                                            fixture->local_account);

    g_assert_cmpuint (result.received, ==, 1);
    g_assert_cmpuint (result.created, ==, 1);
    g_assert_nonnull (find_imported_sx (fixture,
                                       "fiid:missing-source-details"));

    sync_result_clear (&result);
    AB_ImExporterContext_free (snapshot);
}

static void
test_aqb_standing_order_unknown_source_is_skipped (StandingOrderFixture *fixture,
                                                   gconstpointer user_data)
{
    AB_IMEXPORTER_CONTEXT *snapshot = snapshot_new ();
    AB_IMEXPORTER_ACCOUNTINFO_LIST *account_infos =
        AB_ImExporterContext_GetAccountInfoList (snapshot);
    AB_IMEXPORTER_ACCOUNTINFO *account_info =
        AB_ImExporterAccountInfo_List_First (account_infos);
    GncABStandingOrderSyncResult result;

    AB_ImExporterAccountInfo_SetIban (account_info, EXTERNAL_IBAN);
    AB_ImExporterAccountInfo_SetBankCode (account_info, "99999999");
    AB_ImExporterAccountInfo_SetAccountNumber (account_info, "1234567890");
    snapshot_add_order (snapshot, "unknown-source", EXTERNAL_IBAN,
                        "20.00", "Wrong source");
    g_test_expect_message ("gnc.import.aqbanking", G_LOG_LEVEL_WARNING,
                           "*unknown source account*");
    result = gnc_ab_import_standing_orders (snapshot,
                                            fixture->local_account);
    g_test_assert_expected_messages ();

    g_assert_cmpuint (result.received, ==, 1);
    g_assert_cmpuint (result.created, ==, 0);
    g_assert_cmpuint (result.skipped, ==, 1);
    g_assert_null (gnc_book_get_schedxactions (fixture->book)->sx_list);

    sync_result_clear (&result);
    AB_ImExporterContext_free (snapshot);
}

void
test_suite_aqb_standing_orders (void)
{
    GNC_TEST_ADD (suitename, "external-account", StandingOrderFixture, NULL,
                  setup, test_aqb_standing_order_external_account, teardown);
    GNC_TEST_ADD (suitename, "account-code-is-not-iban", StandingOrderFixture, NULL,
                  setup, test_aqb_standing_order_account_code_is_not_iban, teardown);
    GNC_TEST_ADD (suitename, "linked-account", StandingOrderFixture, NULL,
                  setup, test_aqb_standing_order_linked_account, teardown);
    GNC_TEST_ADD (suitename, "weekly-without-purpose", StandingOrderFixture, NULL,
                  setup, test_aqb_standing_order_weekly_without_purpose, teardown);
    GNC_TEST_ADD (suitename, "past-start-uses-next-occurrence", StandingOrderFixture, NULL,
                  setup, test_aqb_standing_order_past_start_uses_next_occurrence, teardown);
    GNC_TEST_ADD (suitename, "update-preserves-local-edits", StandingOrderFixture, NULL,
                  setup, test_aqb_standing_order_update_preserves_local_edits, teardown);
    GNC_TEST_ADD (suitename, "missing-is-disabled", StandingOrderFixture, NULL,
                  setup, test_aqb_standing_order_missing_is_disabled, teardown);
    GNC_TEST_ADD (suitename, "invalid-update-stays-enabled", StandingOrderFixture, NULL,
                  setup, test_aqb_standing_order_invalid_update_stays_enabled, teardown);
    GNC_TEST_ADD (suitename, "manual-sx-not-claimed", StandingOrderFixture, NULL,
                  setup, test_aqb_standing_order_does_not_claim_manual_sx, teardown);
    GNC_TEST_ADD (suitename, "fallback-identity-distinguishes-amounts", StandingOrderFixture, NULL,
                  setup, test_aqb_standing_order_fallback_identity_distinguishes_amounts, teardown);
    GNC_TEST_ADD (suitename, "invalid-period-is-skipped", StandingOrderFixture, NULL,
                  setup, test_aqb_standing_order_invalid_period_is_skipped, teardown);
    GNC_TEST_ADD (suitename, "invalid-start-date-is-skipped", StandingOrderFixture, NULL,
                  setup, test_aqb_standing_order_invalid_start_date_is_skipped, teardown);
    GNC_TEST_ADD (suitename, "zero-amount-is-skipped", StandingOrderFixture, NULL,
                  setup, test_aqb_standing_order_zero_amount_is_skipped, teardown);
    GNC_TEST_ADD (suitename, "missing-source-details-uses-default", StandingOrderFixture, NULL,
                  setup, test_aqb_standing_order_missing_source_details_uses_default, teardown);
    GNC_TEST_ADD (suitename, "unknown-source-is-skipped", StandingOrderFixture, NULL,
                  setup, test_aqb_standing_order_unknown_source_is_skipped, teardown);
}
