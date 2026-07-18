/*
 * gnc-ab-standing-orders.c -- AqBanking standing-order synchronization
 *
 * The original author places this work in the public domain, free
 * for anyone to use as they please.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 */

#include <config.h>

#include "gnc-ab-standing-orders.h"

#include <glib/gi18n.h>
#include <aqbanking/types/imexporter_accountinfo.h>
#include <aqbanking/types/imexporter_context.h>
#include <aqbanking/types/transaction.h>
#include <aqbanking/types/value.h>
#include <gwenhywfar/gwendate.h>

#include "Account.h"
#include "Recurrence.h"
#include "SchedXaction.h"
#include "Split.h"
#include "SX-book.h"
#include "Transaction.h"
#include "dialog-sx-editor.h"
#include "gnc-ab-kvp.h"
#include "gnc-ab-utils.h"
#include "gnc-date.h"
#include "gnc-numeric.h"
#include "gnc-prefs.h"
#include "gnc-ui-util.h"

#define MAX_IBAN_LENGTH 34

static gboolean
standing_order_normalize_iban (const gchar *iban,
                               gchar normalized[MAX_IBAN_LENGTH + 1])
{
    gsize len = 0;

    g_return_val_if_fail (normalized, FALSE);
    normalized[0] = '\0';

    if (!iban || !*iban)
        return FALSE;

    while (*iban)
    {
        if (g_ascii_isalnum (*iban))
        {
            if (len >= MAX_IBAN_LENGTH)
                return FALSE;
            normalized[len++] = g_ascii_toupper (*iban);
        }
        iban++;
    }

    normalized[len] = '\0';
    return len > 0;
}

static const gchar *
standing_order_skip_leading_zeroes (const gchar *value)
{
    if (!value)
        return NULL;

    while (*value == '0')
        value++;

    return value;
}

static gboolean
standing_order_equal_account_number (const gchar *left, const gchar *right)
{
    left = standing_order_skip_leading_zeroes (left);
    right = standing_order_skip_leading_zeroes (right);

    return left && right && *left && *right && g_strcmp0 (left, right) == 0;
}

static gboolean
standing_order_get_de_account_from_iban (const gchar *iban,
                                         gchar **bankcode,
                                         gchar **account_number)
{
    gchar normalized_iban[MAX_IBAN_LENGTH + 1];

    g_return_val_if_fail (bankcode && account_number, FALSE);

    *bankcode = NULL;
    *account_number = NULL;
    if (!standing_order_normalize_iban (iban, normalized_iban))
        return FALSE;

    if (strlen (normalized_iban) == 22
        && g_str_has_prefix (normalized_iban, "DE"))
    {
        *bankcode = g_strndup (normalized_iban + 4, 8);
        *account_number = g_strdup (normalized_iban + 12);
    }

    return *bankcode && *account_number;
}

static gboolean
standing_order_account_can_have_bank_details (Account *account)
{
    switch (xaccAccountGetType (account))
    {
    case ACCT_TYPE_BANK:
    case ACCT_TYPE_ASSET:
    case ACCT_TYPE_CREDIT:
    case ACCT_TYPE_LIABILITY:
        return TRUE;
    default:
        return FALSE;
    }
}

typedef struct
{
    const gchar *bankcode;
    const gchar *account_number;
    const gchar *online_id;
    Account *matched_account;
} StandingOrderAbAccountLookup;

static void
standing_order_find_by_ab_account_cb (Account *account, gpointer user_data)
{
    StandingOrderAbAccountLookup *lookup = user_data;
    const gchar *account_bankcode;
    const gchar *account_accountid;
    const gchar *account_online_id;

    if (lookup->matched_account
        || !standing_order_account_can_have_bank_details (account))
        return;

    account_bankcode = gnc_ab_get_account_bankcode (account);
    account_accountid = gnc_ab_get_account_accountid (account);
    account_online_id = xaccAccountGetOnlineID (account);

    if (account_bankcode && *account_bankcode
        && g_strcmp0 (account_bankcode, lookup->bankcode) == 0
        && standing_order_equal_account_number (account_accountid,
                                                lookup->account_number))
    {
        lookup->matched_account = account;
        return;
    }

    if (lookup->online_id && account_online_id && *account_online_id
        && g_strcmp0 (account_online_id, lookup->online_id) == 0)
        lookup->matched_account = account;
}

static Account *
standing_order_find_account_by_ab_account (Account *base_account,
                                           const gchar *bankcode,
                                           const gchar *account_number)
{
    StandingOrderAbAccountLookup lookup = { bankcode, account_number, NULL, NULL };

    if (!base_account || !bankcode || !*bankcode
        || !account_number || !*account_number)
        return NULL;

    lookup.online_id = gnc_ab_create_online_id (bankcode, account_number);
    gnc_account_foreach_descendant (gnc_account_get_root (base_account),
                                    standing_order_find_by_ab_account_cb,
                                    &lookup);
    g_free ((gchar *)lookup.online_id);

    return lookup.matched_account;
}

static Account *
standing_order_find_account (Account *base_account, const gchar *iban,
                             const gchar *bankcode,
                             const gchar *account_number)
{
    Account *account;
    gchar *iban_bankcode = NULL;
    gchar *iban_account_number = NULL;

    account = standing_order_find_account_by_ab_account (base_account,
                                                         bankcode,
                                                         account_number);
    if (account)
        return account;

    if (standing_order_get_de_account_from_iban (iban, &iban_bankcode,
                                                 &iban_account_number))
    {
        account = standing_order_find_account_by_ab_account (base_account,
                                                             iban_bankcode,
                                                             iban_account_number);
        g_free (iban_bankcode);
        g_free (iban_account_number);
        if (account)
            return account;
    }

    return NULL;
}

static GDate
standing_order_gdate_from_gwen_date (const GWEN_DATE *date)
{
    GDate gdate;

    g_date_clear (&gdate, 1);
    if (date)
        g_date_set_dmy (&gdate, GWEN_Date_GetDay (date),
                        (GDateMonth)GWEN_Date_GetMonth (date),
                        GWEN_Date_GetYear (date));
    return gdate;
}

static PeriodType
standing_order_period_type (const AB_TRANSACTION *ab_trans)
{
    switch (AB_Transaction_GetPeriod (ab_trans))
    {
    case AB_Transaction_PeriodWeekly:
        return PERIOD_WEEK;
    case AB_Transaction_PeriodMonthly:
        return PERIOD_MONTH;
    case AB_Transaction_PeriodNone:
    case AB_Transaction_PeriodUnknown:
    default:
        return PERIOD_INVALID;
    }
}

static GDate
standing_order_start_date (const AB_TRANSACTION *ab_trans)
{
    GDate start = standing_order_gdate_from_gwen_date (
        AB_Transaction_GetNextDate (ab_trans));
    GDate today;
    GDate reference;
    GDate next;
    Recurrence recurrence;
    PeriodType period_type;
    guint cycle;

    if (!g_date_valid (&start))
        start = standing_order_gdate_from_gwen_date (AB_Transaction_GetFirstDate (ab_trans));

    if (!g_date_valid (&start))
        return start;

    gnc_gdate_set_today (&today);
    if (g_date_compare (&start, &today) >= 0)
        return start;

    period_type = standing_order_period_type (ab_trans);
    if (period_type == PERIOD_INVALID)
        return start;

    cycle = AB_Transaction_GetCycle (ab_trans);
    recurrenceSet (&recurrence, cycle ? cycle : 1, period_type, &start,
                   WEEKEND_ADJ_NONE);
    reference = today;
    g_date_subtract_days (&reference, 1);
    recurrenceNextInstance (&recurrence, &reference, &next);

    return g_date_valid (&next) ? next : start;
}

static GList *
standing_order_schedule (const AB_TRANSACTION *ab_trans, const GDate *start)
{
    Recurrence *recurrence = g_new0 (Recurrence, 1);
    guint cycle = AB_Transaction_GetCycle (ab_trans);
    PeriodType period_type = standing_order_period_type (ab_trans);

    if (period_type == PERIOD_INVALID)
    {
        g_free (recurrence);
        return NULL;
    }

    recurrenceSet (recurrence, cycle ? cycle : 1, period_type, start,
                   WEEKEND_ADJ_NONE);
    return g_list_append (NULL, recurrence);
}

static gchar *
standing_order_key (const AB_TRANSACTION *ab_trans)
{
    const gchar *fiid = AB_Transaction_GetFiId (ab_trans);
    guint32 unique_id = AB_Transaction_GetUniqueId (ab_trans);
    const AB_VALUE *value = AB_Transaction_GetValue (ab_trans);
    gint64 value_num = value ? AB_Value_Num (value) : 0;
    gint64 value_denom = value ? AB_Value_Denom (value) : 1;

    if (fiid && *fiid)
        return g_strdup_printf ("fiid:%s", fiid);
    if (unique_id != 0)
        return g_strdup_printf ("uid:%" G_GUINT32_FORMAT, unique_id);

    return g_strdup_printf ("fingerprint:%s:%s:%d:%" G_GUINT32_FORMAT
                            ":%" G_GUINT32_FORMAT ":%" G_GINT64_FORMAT
                            "/%" G_GINT64_FORMAT ":%s",
                            AB_Transaction_GetLocalIban (ab_trans) ? AB_Transaction_GetLocalIban (ab_trans) : "",
                            AB_Transaction_GetRemoteIban (ab_trans) ? AB_Transaction_GetRemoteIban (ab_trans) : "",
                            AB_Transaction_GetPeriod (ab_trans),
                            AB_Transaction_GetCycle (ab_trans),
                            AB_Transaction_GetExecutionDay (ab_trans),
                            value_num,
                            value_denom,
                            AB_Transaction_GetPurpose (ab_trans) ? AB_Transaction_GetPurpose (ab_trans) : "");
}

static gchar *
standing_order_purpose (const AB_TRANSACTION *ab_trans)
{
    gchar *purpose = gnc_ab_get_purpose (ab_trans, FALSE);

    if (purpose)
    {
        g_strstrip (purpose);
        if (*purpose)
            return purpose;
    }

    g_free (purpose);
    return NULL;
}

static gchar *
standing_order_name (const AB_TRANSACTION *ab_trans)
{
    gchar *purpose = standing_order_purpose (ab_trans);
    gchar *name;

    if (purpose)
        name = g_strdup_printf (_("Online Banking: %s"), purpose);
    else
        name = g_strdup_printf (_("Online Banking: %s"), _("Bank Standing Order"));

    g_free (purpose);
    return name;
}
static SchedXaction *
standing_order_find_sx (QofBook *book, const gchar *key,
                        const gchar *account_guid)
{
    SchedXactions *sxes = gnc_book_get_schedxactions (book);
    GList *node;

    for (node = sxes ? sxes->sx_list : NULL; node; node = node->next)
    {
        SchedXaction *sx = GNC_SCHEDXACTION (node->data);
        gchar *stored_key = gnc_ab_get_standing_order_id (sx);
        gchar *stored_account_guid =
            gnc_ab_get_standing_order_account_guid (sx);

        if (stored_key && g_strcmp0 (stored_key, key) == 0
            && (!stored_account_guid
                || g_strcmp0 (stored_account_guid, account_guid) == 0))
        {
            g_free (stored_key);
            g_free (stored_account_guid);
            return sx;
        }
        g_free (stored_key);
        g_free (stored_account_guid);
    }

    return NULL;
}

static gnc_numeric
standing_order_amount (const AB_TRANSACTION *ab_trans)
{
    const AB_VALUE *ab_value = AB_Transaction_GetValue (ab_trans);
    gnc_numeric amount;

    if (!ab_value)
        return gnc_numeric_zero ();

    amount = gnc_numeric_create (AB_Value_Num (ab_value), AB_Value_Denom (ab_value));
    return gnc_numeric_abs (amount);
}

static gchar *
standing_order_formula (gnc_numeric amount)
{
    return g_strdup (xaccPrintAmount (amount, gnc_default_print_info (FALSE)));
}

static void
standing_order_append_template_split (QofBook *book, Transaction *trans,
                              Account *template_account, Account *account,
                              gnc_numeric debit_numeric,
                              gnc_numeric credit_numeric,
                              const gchar *debit_formula,
                              const gchar *credit_formula,
                              const gchar *memo)
{
    Split *split = xaccMallocSplit (book);
    const GncGUID *account_guid = xaccAccountGetGUID (account);

    xaccSplitSetMemo (split, memo);
    xaccAccountInsertSplit (template_account, split);
    qof_instance_set (QOF_INSTANCE (split),
                      "sx-credit-formula", credit_formula ? credit_formula : "",
                      "sx-credit-numeric", &credit_numeric,
                      "sx-debit-formula", debit_formula ? debit_formula : "",
                      "sx-debit-numeric", &debit_numeric,
                      "sx-account", account_guid,
                      NULL);
    xaccTransAppendSplit (trans, split);
}

static Account *
standing_order_get_or_make_imbalance_account (QofBook *book,
                                              gnc_commodity *commodity)
{
    Account *root;
    Account *account;
    gchar *name;

    g_return_val_if_fail (book && commodity, NULL);

    root = gnc_book_get_root_account (book);
    name = g_strconcat (_("Imbalance"), "-",
                        gnc_commodity_get_mnemonic (commodity), NULL);
    account = gnc_account_lookup_by_name (root, name);
    if (!account)
    {
        account = xaccMallocAccount (book);
        xaccAccountBeginEdit (account);
        xaccAccountSetName (account, name);
        xaccAccountSetCommodity (account, commodity);
        xaccAccountSetType (account, ACCT_TYPE_BANK);
        gnc_account_append_child (root, account);
        xaccAccountCommitEdit (account);
    }
    g_free (name);

    return account;
}

static void
standing_order_set_template (SchedXaction *sx,
                             const AB_TRANSACTION *ab_trans,
                             Account *local_account)
{
    QofBook *book;
    Transaction *trans;
    Account *remote_account;
    Account *counter_account;
    gnc_commodity *commodity;
    gnc_numeric amount;
    gnc_numeric zero = gnc_numeric_zero ();
    gchar *amount_str;
    gchar *description;
    gchar *memo;

    book = gnc_account_get_book (local_account);
    commodity = xaccAccountGetCommodity (local_account);
    trans = xaccMallocTransaction (book);
    remote_account = standing_order_find_account (local_account,
                                                   AB_Transaction_GetRemoteIban (ab_trans),
                                                   AB_Transaction_GetRemoteBankCode (ab_trans),
                                                   AB_Transaction_GetRemoteAccountNumber (ab_trans));

    counter_account = remote_account;
    if (!counter_account || counter_account == local_account)
    {
        if (counter_account == local_account)
            g_warning ("Standing order remote account matches the local GnuCash account; using an imbalance split.");
        counter_account = standing_order_get_or_make_imbalance_account (book, commodity);
    }

    amount = standing_order_amount (ab_trans);
    amount_str = standing_order_formula (amount);
    description = standing_order_purpose (ab_trans);
    if (!description)
        description = g_strdup ("");
    memo = g_strdup ("");

    xaccTransBeginEdit (trans);
    xaccTransSetDescription (trans, description);
    xaccTransSetDatePostedSecsNormalized (trans, gnc_time (NULL));
    xaccTransSetCurrency (trans, commodity);

    standing_order_append_template_split (book, trans, sx->template_acct, local_account,
                                          zero, amount, NULL, amount_str, memo);
    standing_order_append_template_split (book, trans, sx->template_acct, counter_account,
                                          amount, zero, amount_str, NULL, memo);

    xaccTransCommitEdit (trans);

    g_free (amount_str);
    g_free (description);
    g_free (memo);
}

static Split *
standing_order_find_local_split (SchedXaction *sx, Account *local_account)
{
    const GncGUID *local_guid = xaccAccountGetGUID (local_account);
    GList *splits = xaccSchedXactionGetSplits (sx);
    GList *node;
    Split *local_split = NULL;

    for (node = splits; node; node = node->next)
    {
        Split *split = node->data;
        GncGUID *split_guid = NULL;

        qof_instance_get (QOF_INSTANCE (split),
                          "sx-account", &split_guid,
                          NULL);
        if (split_guid && guid_equal (split_guid, local_guid))
            local_split = split;
        guid_free (split_guid);

        if (local_split)
            break;
    }
    g_list_free (splits);

    return local_split;
}

static gboolean
standing_order_update_local_formula (SchedXaction *sx,
                                     const AB_TRANSACTION *ab_trans,
                                     Account *local_account,
                                     gboolean *updated)
{
    Split *local_split = standing_order_find_local_split (sx, local_account);
    Transaction *trans;
    gnc_numeric amount = standing_order_amount (ab_trans);
    gnc_numeric zero = gnc_numeric_zero ();
    gchar *amount_str = standing_order_formula (amount);
    gchar *credit_formula = NULL;
    gchar *debit_formula = NULL;
    gboolean needs_edit;

    *updated = FALSE;
    if (!local_split)
    {
        g_warning ("Standing order has no template split for its source account.");
        g_free (amount_str);
        return TRUE;
    }

    qof_instance_get (QOF_INSTANCE (local_split),
                      "sx-credit-formula", &credit_formula,
                      "sx-debit-formula", &debit_formula,
                      NULL);
    needs_edit = g_strcmp0 (credit_formula, amount_str) != 0
                 || (debit_formula && *debit_formula);
    g_free (credit_formula);
    g_free (debit_formula);

    if (!needs_edit)
    {
        g_free (amount_str);
        return FALSE;
    }

    trans = xaccSplitGetParent (local_split);
    if (trans)
    {
        xaccTransBeginEdit (trans);
        qof_instance_set (QOF_INSTANCE (local_split),
                          "sx-credit-formula", amount_str,
                          "sx-credit-numeric", &amount,
                          "sx-debit-formula", "",
                          "sx-debit-numeric", &zero,
                          NULL);
        xaccTransCommitEdit (trans);
        *updated = TRUE;
    }

    g_free (amount_str);
    return TRUE;
}

static void
standing_order_apply_sx_defaults (SchedXaction *sx)
{
    gboolean autocreate;
    gboolean notify;
    gint days_in_advance;

    autocreate = gnc_prefs_get_bool (GNC_PREFS_GROUP_SXED,
                                     GNC_PREF_CREATE_AUTO);
    notify = gnc_prefs_get_bool (GNC_PREFS_GROUP_SXED,
                                 GNC_PREF_NOTIFY);
    xaccSchedXactionSetAutoCreate (sx, autocreate, autocreate && notify);

    days_in_advance = gnc_prefs_get_float (GNC_PREFS_GROUP_SXED,
                                           GNC_PREF_CREATE_DAYS);
    xaccSchedXactionSetAdvanceCreation (sx, days_in_advance);

    days_in_advance = gnc_prefs_get_float (GNC_PREFS_GROUP_SXED,
                                           GNC_PREF_REMIND_DAYS);
    xaccSchedXactionSetAdvanceReminder (sx, days_in_advance);
}

static gchar *
standing_order_snapshot_key (const gchar *account_guid, const gchar *key)
{
    return g_strconcat (account_guid, ":", key, NULL);
}

static void
standing_order_sync (QofBook *book, Account *local_account,
                     const AB_TRANSACTION *ab_trans, GHashTable *seen,
                     GncABStandingOrderSyncResult *result)
{
    SchedXaction *sx;
    GDate start_date;
    GDate end_date;
    GList *old_schedule;
    GList *schedule;
    gchar account_guid[GUID_ENCODING_LENGTH + 1];
    gchar *key;
    gchar *name;
    gchar *snapshot_key;

    if (!ab_trans || AB_Transaction_GetType (ab_trans) != AB_Transaction_TypeStandingOrder)
        return;

    key = standing_order_key (ab_trans);
    guid_to_string_buff (xaccAccountGetGUID (local_account), account_guid);
    snapshot_key = standing_order_snapshot_key (account_guid, key);
    g_hash_table_add (seen, snapshot_key);

    if (gnc_numeric_zero_p (standing_order_amount (ab_trans)))
    {
        g_warning ("Skipping standing order without an amount.");
        result->skipped++;
        g_free (key);
        return;
    }

    sx = standing_order_find_sx (book, key, account_guid);
    if (sx)
    {
        gboolean formula_updated = FALSE;
        gboolean needs_edit;
        gboolean was_enabled = xaccSchedXactionGetEnabled (sx);

        needs_edit = standing_order_update_local_formula (sx, ab_trans,
                                                          local_account,
                                                          &formula_updated);
        if (!was_enabled)
            xaccSchedXactionSetEnabled (sx, TRUE);
        gnc_ab_set_standing_order_metadata (sx, key, account_guid);

        if (formula_updated || !was_enabled)
            result->updated++;
        if (needs_edit && !g_list_find (result->to_edit, sx))
            result->to_edit = g_list_prepend (result->to_edit, sx);

        g_free (key);
        return;
    }

    start_date = standing_order_start_date (ab_trans);
    if (!g_date_valid (&start_date))
    {
        g_warning ("Skipping standing order without a valid start date.");
        result->skipped++;
        g_free (key);
        return;
    }

    schedule = standing_order_schedule (ab_trans, &start_date);
    if (!schedule)
    {
        g_warning ("Skipping standing order with an unsupported period.");
        result->skipped++;
        g_free (key);
        return;
    }

    sx = xaccSchedXactionMalloc (book);
    name = standing_order_name (ab_trans);
    end_date = standing_order_gdate_from_gwen_date (AB_Transaction_GetLastDate (ab_trans));

    gnc_sx_begin_edit (sx);
    xaccSchedXactionSetName (sx, name);
    old_schedule = gnc_sx_get_schedule (sx);
    gnc_sx_set_schedule (sx, schedule);
    recurrenceListFree (&old_schedule);
    xaccSchedXactionSetStartDate (sx, &start_date);
    xaccSchedXactionSetEndDate (sx, &end_date);
    xaccSchedXactionSetNumOccur (sx, 0);
    xaccSchedXactionSetEnabled (sx, TRUE);
    gnc_sx_commit_edit (sx);

    gnc_ab_set_standing_order_metadata (sx, key, account_guid);
    standing_order_apply_sx_defaults (sx);
    gnc_sx_set_instance_count (sx, 1);
    standing_order_set_template (sx, ab_trans, local_account);
    gnc_sxes_add_sx (gnc_book_get_schedxactions (book), sx);

    result->created++;
    result->to_edit = g_list_prepend (result->to_edit, sx);

    g_free (name);
    g_free (key);
}

static Account *
standing_order_account_for_account_info (AB_IMEXPORTER_ACCOUNTINFO *acc_info,
                                         Account *default_acc)
{
    Account *account;
    const gchar *iban = AB_ImExporterAccountInfo_GetIban (acc_info);
    const gchar *bankcode = AB_ImExporterAccountInfo_GetBankCode (acc_info);
    const gchar *account_number =
        AB_ImExporterAccountInfo_GetAccountNumber (acc_info);

    account = standing_order_find_account (default_acc, iban, bankcode,
                                           account_number);
    if (!account && (!iban || !*iban) && (!bankcode || !*bankcode)
        && (!account_number || !*account_number))
        account = default_acc;

    return account;
}

static void
standing_order_disable_missing (QofBook *book, GHashTable *imported_accounts,
                                GHashTable *seen,
                                GncABStandingOrderSyncResult *result)
{
    SchedXactions *sxes = gnc_book_get_schedxactions (book);
    GList *node;

    for (node = sxes ? sxes->sx_list : NULL; node; node = node->next)
    {
        SchedXaction *sx = GNC_SCHEDXACTION (node->data);
        gchar *key = gnc_ab_get_standing_order_id (sx);
        gchar *account_guid = gnc_ab_get_standing_order_account_guid (sx);
        gchar *snapshot_key;

        if (!key || !account_guid
            || !g_hash_table_contains (imported_accounts, account_guid))
        {
            g_free (key);
            g_free (account_guid);
            continue;
        }

        snapshot_key = standing_order_snapshot_key (account_guid, key);
        if (!g_hash_table_contains (seen, snapshot_key)
            && xaccSchedXactionGetEnabled (sx))
        {
            xaccSchedXactionSetEnabled (sx, FALSE);
            result->disabled++;
        }

        g_free (snapshot_key);
        g_free (key);
        g_free (account_guid);
    }
}

GncABStandingOrderSyncResult
gnc_ab_import_standing_orders (AB_IMEXPORTER_CONTEXT *context,
                               Account *default_acc)
{
    AB_IMEXPORTER_ACCOUNTINFO_LIST *account_info_list;
    AB_IMEXPORTER_ACCOUNTINFO *acc_info;
    GncABStandingOrderSyncResult result = { 0 };
    GHashTable *imported_accounts;
    GHashTable *seen;
    QofBook *book;

    g_return_val_if_fail (context && default_acc, result);

    book = gnc_account_get_book (default_acc);
    imported_accounts = g_hash_table_new_full (g_str_hash, g_str_equal,
                                               g_free, NULL);
    seen = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
    account_info_list = AB_ImExporterContext_GetAccountInfoList (context);

    for (acc_info = AB_ImExporterAccountInfo_List_First (account_info_list);
         acc_info;
         acc_info = AB_ImExporterAccountInfo_List_Next (acc_info))
    {
        AB_TRANSACTION_LIST *transactions;
        AB_TRANSACTION *ab_trans;
        Account *local_account;
        gchar local_account_guid[GUID_ENCODING_LENGTH + 1];
        gboolean warned_unknown_source = FALSE;

        transactions = AB_ImExporterAccountInfo_GetTransactionList (acc_info);
        local_account = standing_order_account_for_account_info (acc_info, default_acc);
        if (local_account)
        {
            guid_to_string_buff (xaccAccountGetGUID (local_account),
                                 local_account_guid);
            g_hash_table_add (imported_accounts, g_strdup (local_account_guid));
        }

        for (ab_trans = AB_Transaction_List_FindFirstByType (transactions,
                                                             AB_Transaction_TypeStandingOrder,
                                                             0);
             ab_trans;
             ab_trans = AB_Transaction_List_FindNextByType (ab_trans,
                                                            AB_Transaction_TypeStandingOrder,
                                                            0))
        {
            result.received++;
            if (!local_account)
            {
                if (!warned_unknown_source)
                {
                    g_warning ("Skipping standing-order data for an unknown source account.");
                    warned_unknown_source = TRUE;
                }
                result.skipped++;
                continue;
            }
            standing_order_sync (book, local_account, ab_trans, seen, &result);
        }
    }

    standing_order_disable_missing (book, imported_accounts, seen, &result);
    g_hash_table_destroy (seen);
    g_hash_table_destroy (imported_accounts);
    result.to_edit = g_list_reverse (result.to_edit);

    return result;
}
