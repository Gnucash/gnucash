/*
 * gnc-ab-utils.c --
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

/**
 * @internal
 * @file gnc-ab-utils.c
 * @brief AqBanking utility functions
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#include <config.h>

#include "gnc-ab-utils.h"

#include <glib/gi18n.h>
#include <gwenhywfar/gwenhywfar.h>
#include <aqbanking/banking.h>
#include <aqbanking/types/balance.h>
#if (AQBANKING_VERSION_INT >= 60400)
#include <aqbanking/types/refaccount.h>
#include <gnc-aqbanking-templates.h>
#endif
#include "window-reconcile.h"
#include "Transaction.h"
#include "dialog-ab-trans.h"
#include "gnc-ab-kvp.h"
#include "gnc-string-utils.h"
#include "gnc-gwen-gui.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "import-account-matcher.h"
#include "import-main-matcher.h"
#include "qof.h"
#include "engine-helpers.h"
#include <aqbanking/gui/abgui.h>

/* This static indicates the debugging module that this .o belongs to.  */
G_GNUC_UNUSED static QofLogModule log_module = G_LOG_DOMAIN;

/* Global variables for AB_BANKING caching. */
static AB_BANKING *gnc_AB_BANKING = NULL;
static gint gnc_AB_BANKING_refcount = 0;

static gpointer join_ab_strings_cb (const gchar *str, gpointer user_data);
static Account *gnc_ab_accinfo_to_gnc_acc (GtkWidget *parent,
                                           AB_IMEXPORTER_ACCOUNTINFO *account_info);
static Account *gnc_ab_txn_to_gnc_acc (GtkWidget *parent,
                                       const AB_TRANSACTION *transaction);
static const AB_TRANSACTION *txn_transaction_cb (const AB_TRANSACTION *element,
                                                 gpointer user_data);
static AB_IMEXPORTER_ACCOUNTINFO *txn_accountinfo_cb (AB_IMEXPORTER_ACCOUNTINFO *element,
                                                      gpointer user_data);
static AB_IMEXPORTER_ACCOUNTINFO *bal_accountinfo_cb (AB_IMEXPORTER_ACCOUNTINFO *element,
                                                      gpointer user_data);

typedef struct
{
    GncGUID account_guid;
    gnc_numeric balance;
    time64 balance_date;
    gchar *message;
} GncABReconcileRequest;

struct _GncABImExContextImport
{
    gint ref_count;
    guint awaiting;
    Account *gnc_acc;
    GNC_AB_ACCOUNT_SPEC *ab_acc;
    gboolean execute_txns;
    AB_BANKING *api;
    AB_IMEXPORTER_CONTEXT *context;
    GWeakRef parent;
    gboolean has_parent;
    QofBook *book;
    GncGUID book_guid;
    GCancellable *cancellable;
    GncABImportContextDoneCB done_cb;
    gpointer user_data;
    GNC_AB_JOB_LIST2 *job_list;
    GNCImportMainMatcher *generic_importer;
    GData *tmp_job_list;
    GQueue *job_errors;
    GQueue *reconcile_requests;
    gboolean has_importable_balance;
    gboolean finished;
};

static inline gboolean is_leap_year (int year)
{
    return (year % 400 == 0 || (year % 4 == 0 && year % 100 != 0 ));
}

static inline time64
gnc_gwen_date_to_time64 (const GNC_GWEN_DATE* date)
{
    int day = GWEN_Date_GetDay (date);
    int month = GWEN_Date_GetMonth (date);
    int year = GWEN_Date_GetYear (date);
    /* Some banks use nominal 30-day months and set the value date as
     * the day after the posted date. In February this can appear to
     * be an invalid date because February has fewer than 30 days. If
     * that's the case then back up a day to get a real date for
     * posting.
     */
    while (month == 2 && day <= 30 && day > (is_leap_year (year) ? 29 : 28))
        --day;
    return gnc_dmy2time64_neutral (day, month, year);
}

static GWEN_GUI *gnc_gwengui_extended_by_ABBanking;

AB_BANKING *
gnc_AB_BANKING_new (void)
{
    AB_BANKING *api;

    if (gnc_AB_BANKING)
    {
        /* API cached. */
        api = gnc_AB_BANKING;

        /* Init the API again. */
        if (gnc_AB_BANKING_refcount == 0)
            g_return_val_if_fail (AB_Banking_Init (api) == 0, NULL);

    }
    else
    {
        api = AB_Banking_new (PROJECT_NAME, NULL, 0);
        g_return_val_if_fail (api, NULL);

        /* These two values must be set because newest bank regulation requires
        the bank servers to require it. The string itself results from our
        registration with the German bank association at
        https://www.hbci-zka.de/register/prod_register.htm (where the
        registration was requested and is managed by cstim). The function call was
        introduced in aqbanking-5.99.25 and aqbanking-5.7.9. */
        AB_Banking_RuntimeConfig_SetCharValue (api, "fintsRegistrationKey", "412748A1836CDD07181CE1910");
        AB_Banking_RuntimeConfig_SetCharValue (api, "fintsApplicationVersionString", PROJECT_VERSION);

        /* Init the API */
        g_return_val_if_fail (AB_Banking_Init (api) == 0, NULL);
        gnc_gwengui_extended_by_ABBanking = GWEN_Gui_GetGui ();
        AB_Gui_Extend (gnc_gwengui_extended_by_ABBanking, api);

        /* Cache it */
        gnc_AB_BANKING = api;
        gnc_AB_BANKING_refcount = 0;
    }

    gnc_AB_BANKING_refcount++;

    return api;
}

void
gnc_AB_BANKING_delete (AB_BANKING *api)
{
    if (!api)
        api = gnc_AB_BANKING;

    if (api)
    {
        if (api == gnc_AB_BANKING)
        {
            gnc_AB_BANKING = NULL;
            gnc_AB_BANKING_fini (api);
        }

        AB_Banking_free (api);
    }
}


gint
gnc_AB_BANKING_fini (AB_BANKING *api)
{
    if (api == gnc_AB_BANKING)
    {
        if (--gnc_AB_BANKING_refcount == 0)
        {
            if (gnc_gwengui_extended_by_ABBanking)
                AB_Gui_Unextend (gnc_gwengui_extended_by_ABBanking);
            gnc_gwengui_extended_by_ABBanking = NULL;
            return AB_Banking_Fini (api);
        }
    }
    else
    {
        if (gnc_gwengui_extended_by_ABBanking)
            AB_Gui_Unextend (gnc_gwengui_extended_by_ABBanking);
        gnc_gwengui_extended_by_ABBanking = NULL;
        return AB_Banking_Fini (api);
    }
    return 0;
}

GNC_AB_ACCOUNT_SPEC *
gnc_ab_get_ab_account (const AB_BANKING *api, Account *gnc_acc)
{
    GNC_AB_ACCOUNT_SPEC *ab_account = NULL;
    const gchar *bankcode = NULL;
    const gchar *accountid = NULL;
    guint32 account_uid = 0;

    bankcode = gnc_ab_get_account_bankcode (gnc_acc);
    accountid = gnc_ab_get_account_accountid (gnc_acc);
    account_uid = gnc_ab_get_account_uid (gnc_acc);

    if (account_uid > 0)
    {
        gint rv;

        rv = AB_Banking_GetAccountSpecByUniqueId (api, account_uid, &ab_account);

        if ( (rv<0 || !ab_account) && bankcode && *bankcode &&
             accountid && *accountid)
        {
/* Finding the account by code and number is suspended in AQBANKING 6 pending
 * implementation of a replacement for AB_Banking_GetAccountByCodeAndNumber.
 */
            PINFO("gnc_ab_get_ab_account: No AB_ACCOUNT found for UID %d, "
                      "trying bank code\n", account_uid);
            return NULL;
        }
        return ab_account;
    }

    return NULL;
}

gchar *
gnc_AB_VALUE_to_readable_string (const AB_VALUE *value)
{
    if (value)
        return g_strdup_printf ("%.2f %s",
                                AB_Value_GetValueAsDouble (value),
                                AB_Value_GetCurrency (value));
    else
        return g_strdup_printf ("%.2f", 0.0);
}


gchar*
gnc_ab_create_online_id (const gchar *bankcode, const gchar *accountnumber)
{
    gchar *online_id;

    /* The accountnumber may have leading zeros, depending on where them
     * accountnumber is came from, e.g. the accountnumber of accountinfo
     * has no leading zeros while the (local)accountnumber of a transaction
     * has leading zeros.
     * So remove all leading '0', to get a consistent online_id.
     */
    while (accountnumber && *accountnumber == '0')
        accountnumber++;

    online_id = g_strconcat (bankcode ? bankcode : "",
                             accountnumber ? accountnumber : "",
                             (gchar*)NULL);

    return online_id;
}

/**
 * Take a string from a GWEN_STRINGLIST, strip invalid utf8 and join it
 * to the rest.
 */
static gpointer
join_ab_strings_cb (const gchar *str, gpointer user_data)
{
    gchar **acc = user_data;
    gchar *tmp;

    if (!str || !*str)
        return NULL;

    tmp = g_utf8_normalize (str, -1, G_NORMALIZE_NFC);
    g_strstrip (tmp);
    gnc_utf8_strip_invalid_and_controls (tmp);

    if (*acc)
    {
        if (!strstr (*acc, tmp))
        {
            gchar *join = g_strjoin (" ", *acc, tmp, (gchar*) NULL);
            g_free (*acc);
            *acc = join;
        }
        g_free (tmp);
    }
    else
    {
        *acc = tmp;
    }
    return NULL;
}

gchar *
gnc_ab_get_remote_name (const AB_TRANSACTION *ab_trans)
{
    const char* ab_remote_name;
    gchar *gnc_other_name = NULL;

    g_return_val_if_fail (ab_trans, NULL);

    ab_remote_name = AB_Transaction_GetRemoteName (ab_trans);
    if (ab_remote_name)
        gnc_other_name = g_strdup(ab_remote_name);
    if (!gnc_other_name || !*gnc_other_name)
    {
        g_free (gnc_other_name);
        gnc_other_name = NULL;
    }

    return gnc_other_name;
}

gchar *
gnc_ab_get_purpose (const AB_TRANSACTION *ab_trans, gboolean is_ofx)
{
    GWEN_STRINGLIST *ab_purpose;
    const char *ab_transactionText = NULL;
    gchar *gnc_description = NULL;

    g_return_val_if_fail (ab_trans, g_strdup (""));

    if (!is_ofx && gnc_prefs_get_bool (GNC_PREFS_GROUP_AQBANKING, GNC_PREF_USE_TRANSACTION_TXT))
    {
        /* According to AqBanking, some of the non-swift lines have a special
         * meaning. Some banks place valuable text into the transaction text,
         * hence we put this text in front of the purpose. */
        ab_transactionText = AB_Transaction_GetTransactionText (ab_trans);
        if (ab_transactionText && *ab_transactionText)
            gnc_description = g_utf8_normalize (ab_transactionText, -1, G_NORMALIZE_NFC);
    }

    ab_purpose = AB_Transaction_GetPurposeAsStringList (ab_trans);
    if (ab_purpose)
        GWEN_StringList_ForEach (ab_purpose, join_ab_strings_cb,
                                 &gnc_description);

    GWEN_StringList_free (ab_purpose);

    return gnc_description;
}

/* Ultimate Creditor and Ultimate Debtor are newish parameters added
 * to SWIFT MT940 and CAMT.053 designating the originating
 * payer or payee on the transaction. It's unlikely, but still
 * possible, that a bank would use both this markup and the Non-swift
 * TransactionText or RemoteName tags.
 */
static gchar *
ab_ultimate_creditor_debtor_to_gnc (const AB_TRANSACTION *ab_trans,
                                    gboolean is_ofx)
{
    const gchar* ultimate;

    if (is_ofx)
        return NULL;

    ultimate = AB_Transaction_GetUltimateCreditor (ab_trans);

    if (!ultimate || !*ultimate)
        ultimate = AB_Transaction_GetUltimateDebtor (ab_trans);

    if (!ultimate || !*ultimate)
        return NULL;

    return g_strdup (ultimate);
}

gchar *
gnc_ab_description_to_gnc (const AB_TRANSACTION *ab_trans, gboolean is_ofx)
{
    GList *acc = NULL;
    gchar *retval;

    acc = g_list_prepend (acc, gnc_ab_get_remote_name (ab_trans));
    acc = g_list_prepend (acc, gnc_ab_get_purpose (ab_trans, is_ofx));
    acc = g_list_prepend (acc, ab_ultimate_creditor_debtor_to_gnc (ab_trans, is_ofx));
    retval = gnc_g_list_stringjoin_nodups (acc, "; ");

    g_list_free_full (acc, g_free);
    return retval ? retval : g_strdup (_("Unspecified"));
}

gchar *
gnc_ab_memo_to_gnc (const AB_TRANSACTION *ab_trans)
{
    const gchar *ab_remote_accountnumber =
        AB_Transaction_GetRemoteAccountNumber (ab_trans);
    const gchar *ab_remote_bankcode =
        AB_Transaction_GetRemoteBankCode (ab_trans);

    gchar *ab_other_accountid;
    gchar *ab_other_bankcode;

    gboolean have_accountid;
    gboolean have_bankcode;

    gchar *retval;

    // For SEPA transactions, we need to ask for something different here
    if (!ab_remote_accountnumber)
        ab_remote_accountnumber = AB_Transaction_GetRemoteIban (ab_trans);
    if (!ab_remote_bankcode)
        ab_remote_bankcode = AB_Transaction_GetRemoteBic (ab_trans);

    ab_other_accountid = g_strdup (ab_remote_accountnumber ? ab_remote_accountnumber : "");
    ab_other_bankcode = g_strdup (ab_remote_bankcode ? ab_remote_bankcode : "");

    /* Ensure string is in utf8 */
    gnc_utf8_strip_invalid (ab_other_accountid);
    gnc_utf8_strip_invalid (ab_other_bankcode);

    /* and -then- trim it */
    g_strstrip (ab_other_accountid);
    g_strstrip (ab_other_bankcode);


    have_accountid = ab_other_accountid && *ab_other_accountid;
    have_bankcode = ab_other_bankcode && *ab_other_bankcode;

    if ( have_accountid || have_bankcode )
    {
        retval = g_strdup_printf ("%s %s %s %s",
                                  have_accountid ? _("Account") : "",
                                  have_accountid ? ab_other_accountid : "",
                                  have_bankcode  ? _("Bank") : "",
                                  have_bankcode  ? ab_other_bankcode : ""
                                 );
        g_strstrip (retval);
    }
    else
    {
        retval = g_strdup ("");
    }

    g_free (ab_other_accountid);
    g_free (ab_other_bankcode);

    return retval;
}

Transaction *
gnc_ab_trans_to_gnc (const AB_TRANSACTION *ab_trans, Account *gnc_acc)
{
    QofBook *book;
    Transaction *gnc_trans;
    const gchar *fitid;
    const GNC_GWEN_DATE *value_date, *post_date;
    time64 post_time;
    const char *custref;
    gchar *description;
    Split *split;
    gchar *memo;

    g_return_val_if_fail (ab_trans && gnc_acc, NULL);

    /* Create new GnuCash transaction for the given AqBanking one */
    book = gnc_account_get_book (gnc_acc);
    gnc_trans = xaccMallocTransaction (book);
    xaccTransBeginEdit (gnc_trans);

    /* Date / Time */
    /* SWIFT import formats (in particular MT940) provide for two
     * dates, the entry date and the value date (valuta is value in
     * German). The value date is the effective date for financial
     * calculation purposes and is mandatory, the entry date is the
     * date that the financial institution posted the
     * transaction. Unfortunately if the transaction field doesn't
     * provide an entry date AQBanking substitutes the date from the
     * last balance instead of using the value date or NULL, making
     * the field unreliable.
     */
    value_date = AB_Transaction_GetValutaDate (ab_trans);
    if (value_date)
         post_time = gnc_gwen_date_to_time64 (value_date);
    else if ((post_date = AB_Transaction_GetDate (ab_trans))) // always true
         post_time = gnc_gwen_date_to_time64 (post_date);
    else
    {
        g_warning ("transaction_cb: Import had no transaction date");
        post_time = gnc_time (NULL);
    }
    xaccTransSetDatePostedSecsNormalized (gnc_trans, post_time);

    xaccTransSetDateEnteredSecs (gnc_trans, gnc_time (NULL));

    /* Currency.  We take simply the default currency of the gnucash account */
    xaccTransSetCurrency (gnc_trans, xaccAccountGetCommodity (gnc_acc));

    /* Trans-Num or Split-Action set with gnc_set_num_action below per book
     * option */

    fitid = AB_Transaction_GetFiId (ab_trans);

    /* Description */
    description = gnc_ab_description_to_gnc (ab_trans, (fitid && *fitid));
    xaccTransSetDescription (gnc_trans, description);
    g_free (description);

    /* Notes. */
    /* xaccTransSetNotes(gnc_trans, g_notes); */
    /* But Nobody ever uses the Notes field? */

    /* Add one split */
    split = xaccMallocSplit (book);
    xaccSplitSetParent (split, gnc_trans);
    xaccSplitSetAccount (split, gnc_acc);

    /* Set the transaction number or split action field based on book option.
     * We use the "customer reference", if there is one. */
    custref = AB_Transaction_GetCustomerReference (ab_trans);
    if (custref && *custref && g_ascii_strncasecmp (custref, "NONREF", 6) != 0)
        gnc_set_num_action (gnc_trans, split, custref, NULL);

    /* Set OFX unique transaction ID */
    if (fitid && *fitid)
        xaccSplitSetOnlineID (split, fitid);

    /* FIXME: Extract function */
    {
        /* Amount into the split */
        const AB_VALUE *ab_value = AB_Transaction_GetValue (ab_trans);
        double d_value = ab_value ? AB_Value_GetValueAsDouble (ab_value) : 0.0;
        AB_TRANSACTION_TYPE ab_type = AB_Transaction_GetType (ab_trans);
        gnc_numeric gnc_amount;

        /*printf("Transaction with value %f has type %d\n", d_value, ab_type);*/
        /* If the value is positive, but the transaction type says the
           money is transferred away from our account (Transfer instead of
           DebitNote), we switch the value to negative. */
        if (d_value > 0.0 && ab_type == AB_Transaction_TypeTransfer)
            d_value = -d_value;

        gnc_amount = double_to_gnc_numeric (
                         d_value,
                         xaccAccountGetCommoditySCU (gnc_acc),
                         GNC_HOW_RND_ROUND_HALF_UP);
        if (!ab_value)
            g_warning ("transaction_cb: Oops, value was NULL.  Using 0");
        xaccSplitSetBaseValue (split, gnc_amount, xaccAccountGetCommodity (gnc_acc));
    }

    /* Memo in the Split. */
    memo = gnc_ab_memo_to_gnc (ab_trans);
    xaccSplitSetMemo (split, memo);
    g_free (memo);

    return gnc_trans;
}

/**
 * Call gnc_import_select_account() on the online id constructed using
 * the information in @a acc_info.
 *
 * @param parent Parent Widget
 * @param acc_info AB_IMEXPORTER_ACCOUNTINFO
 * @return A GnuCash account, or NULL otherwise
 */
static Account *
gnc_ab_accinfo_to_gnc_acc (GtkWidget *parent, AB_IMEXPORTER_ACCOUNTINFO *acc_info)
{
    const gchar *bankcode, *accountnumber;
    gchar *online_id;
    Account *gnc_acc;

    g_return_val_if_fail (acc_info, NULL);

    bankcode = AB_ImExporterAccountInfo_GetBankCode (acc_info);
    accountnumber = AB_ImExporterAccountInfo_GetAccountNumber (acc_info);
    online_id = gnc_ab_create_online_id (bankcode, accountnumber);
    gnc_acc = gnc_import_select_account (parent, online_id, 1, 
                  AB_ImExporterAccountInfo_GetAccountName (acc_info),
                  NULL, ACCT_TYPE_NONE, NULL, NULL);
    if (!gnc_acc)
    {
        g_warning ("gnc_ab_accinfo_to_gnc_acc: Could not determine source account"
                   " for online_id %s", online_id);
    }
    g_free (online_id);

    return gnc_acc;
}


/**
 * Call gnc_import_select_account() on the online id constructed using
 * the local information in @a transaction.
 *
 * @param parent Parent Widget
 * @param transaction AB_TRANSACTION
 * @return A GnuCash account, or NULL otherwise
 */
static Account *
gnc_ab_txn_to_gnc_acc (GtkWidget *parent, const AB_TRANSACTION *transaction)
{
    const gchar *bankcode, *accountnumber;
    gchar *online_id;
    Account *gnc_acc;

    g_return_val_if_fail(transaction, NULL);

    bankcode = AB_Transaction_GetLocalBankCode (transaction);
    accountnumber = AB_Transaction_GetLocalAccountNumber (transaction);
    if (!bankcode && !accountnumber)
    {
        return NULL;
    }

    online_id = gnc_ab_create_online_id (bankcode, accountnumber);
    gnc_acc = gnc_import_select_account (parent, online_id, 1,
                  AB_Transaction_GetLocalName (transaction),
                  NULL, ACCT_TYPE_NONE, NULL, NULL);
    if (!gnc_acc)
    {
        g_warning ("gnc_ab_txn_to_gnc_acc: Could not determine source account"
                   " for online_id %s", online_id);
    }
    g_free (online_id);

    return gnc_acc;
}

static GncABImExContextImport *
gnc_ab_ieci_ref (GncABImExContextImport *ieci)
{
    g_return_val_if_fail (ieci, NULL);
    g_atomic_int_inc (&ieci->ref_count);
    return ieci;
}

static void
free_tmp_job_cb (GQuark key_id, gpointer job, gpointer user_data)
{
    (void)key_id;
    (void)user_data;
    AB_Transaction_free (job);
}

static void
free_reconcile_request (GncABReconcileRequest *request)
{
    if (!request)
        return;
    g_free (request->message);
    g_free (request);
}

static void
free_import_context (GncABImExContextImport *ieci)
{
    if (!ieci)
        return;

    if (ieci->context)
        AB_ImExporterContext_free (ieci->context);
    if (ieci->job_list)
        AB_Transaction_List2_free (ieci->job_list);
    g_datalist_foreach (&ieci->tmp_job_list, free_tmp_job_cb, NULL);
    g_datalist_clear (&ieci->tmp_job_list);
    if (ieci->job_errors)
        g_queue_free (ieci->job_errors);
    if (ieci->reconcile_requests)
    {
        while (!g_queue_is_empty (ieci->reconcile_requests))
            free_reconcile_request (g_queue_pop_head (ieci->reconcile_requests));
        g_queue_free (ieci->reconcile_requests);
    }
    g_clear_object (&ieci->cancellable);
    g_weak_ref_clear (&ieci->parent);
    g_free (ieci);
}

static void
gnc_ab_ieci_unref (GncABImExContextImport *ieci)
{
    if (ieci && g_atomic_int_dec_and_test (&ieci->ref_count))
        free_import_context (ieci);
}

static GtkWindow *
gnc_ab_ieci_get_parent (GncABImExContextImport *ieci)
{
    GtkWidget *widget;
    GtkRoot *root;
    GtkWindow *window = NULL;

    widget = g_weak_ref_get (&ieci->parent);
    if (!widget)
        return NULL;
    if (GTK_IS_WINDOW (widget))
        return GTK_WINDOW (widget);

    root = gtk_widget_get_root (widget);
    if (root && GTK_IS_WINDOW (root))
        window = GTK_WINDOW (g_object_ref (root));
    g_object_unref (widget);
    return window;
}

static gboolean
gnc_ab_ieci_is_current (GncABImExContextImport *ieci)
{
    QofBook *book;

    if (!ieci || ieci->finished ||
        (ieci->cancellable && g_cancellable_is_cancelled (ieci->cancellable)))
        return FALSE;

    book = gnc_get_current_book ();
    return book && book == ieci->book &&
           guid_equal (qof_instance_get_guid (QOF_INSTANCE (book)), &ieci->book_guid);
}

static gboolean
gnc_ab_ieci_parent_is_available (GncABImExContextImport *ieci,
                                 GtkWindow **parent)
{
    *parent = gnc_ab_ieci_get_parent (ieci);
    return !ieci->has_parent || *parent != NULL;
}

static GtkWindow *
gnc_ab_ieci_get_balance_parent (GncABImExContextImport *ieci)
{
    GtkWidget *widget;
    GtkRoot *root;
    GtkWindow *window = NULL;

    if (!ieci->generic_importer)
        return gnc_ab_ieci_get_parent (ieci);

    widget = gnc_gen_trans_list_widget (ieci->generic_importer);
    if (!widget)
        return gnc_ab_ieci_get_parent (ieci);
    if (GTK_IS_WINDOW (widget))
        return GTK_WINDOW (g_object_ref (widget));
    root = gtk_widget_get_root (widget);
    if (root && GTK_IS_WINDOW (root))
        window = GTK_WINDOW (g_object_ref (root));
    return window;
}

static void gnc_ab_ieci_finish (GncABImExContextImport *ieci, gboolean completed);
static void gnc_ab_ieci_begin_balances (GncABImExContextImport *ieci);
static void gnc_ab_ieci_show_next_job_error (GncABImExContextImport *ieci);
static void gnc_ab_ieci_show_next_reconcile_request (GncABImExContextImport *ieci);

static const AB_TRANSACTION *
txn_transaction_cb (const AB_TRANSACTION *element, gpointer user_data)
{
    GncABImExContextImport *data = user_data;
    Transaction *gnc_trans;
    GncABTransType trans_type;
    Account *txnacc;
    GtkWindow *parent;

    g_return_val_if_fail (element && data, NULL);
    if (!gnc_ab_ieci_is_current (data) ||
        !gnc_ab_ieci_parent_is_available (data, &parent))
        return NULL;

    txnacc = gnc_ab_txn_to_gnc_acc (GTK_WIDGET (parent), element);
    gnc_trans = gnc_ab_trans_to_gnc (element, txnacc ? txnacc : data->gnc_acc);

    if (data->execute_txns && data->ab_acc)
    {
        AB_TRANSACTION *ab_trans = AB_Transaction_dup (element);
        GNC_AB_JOB *job;

        AB_Transaction_SetLocalBankCode (
            ab_trans, AB_AccountSpec_GetBankCode (data->ab_acc));
        AB_Transaction_SetLocalAccountNumber (
            ab_trans, AB_AccountSpec_GetAccountNumber (data->ab_acc));
        AB_Transaction_SetLocalCountry (ab_trans, "DE");

        switch (AB_Transaction_GetType (ab_trans))
        {
        case AB_Transaction_TypeDebitNote:
            trans_type = SINGLE_DEBITNOTE;
            break;
        case AB_Transaction_TypeTransaction:
            /* trans_type = SINGLE_INTERNAL_TRANSFER;
             * break; */
        case AB_Transaction_TypeTransfer:
        default:
            trans_type = SEPA_TRANSFER;
            break;
        }

        job = gnc_ab_get_trans_job (data->ab_acc, ab_trans, trans_type);
        if (!job || AB_AccountSpec_GetTransactionLimitsForCommand (
                        data->ab_acc, AB_Transaction_GetCommand (job)) == NULL)
        {
            if (job)
                AB_Transaction_free (job);
            g_queue_push_tail (data->job_errors, GINT_TO_POINTER (1));
        }
        else
        {
            gnc_gen_trans_list_add_trans_with_ref_id (data->generic_importer,
                                                      gnc_trans,
                                                      AB_Transaction_GetUniqueId (job));
            g_datalist_set_data (&data->tmp_job_list,
                                gnc_AB_JOB_to_readable_string (job), job);
        }
        AB_Transaction_free (ab_trans);
    }
    else
    {
        gnc_gen_trans_list_add_trans (data->generic_importer, gnc_trans);
    }

    g_clear_object (&parent);
    return NULL;
}

static void
gnc_ab_trans_processed_cb (GNCImportTransInfo *trans_info, gboolean imported,
                           gpointer user_data)
{
    GncABImExContextImport *data = user_data;
    gchar *jobname;
    GNC_AB_JOB *job;

    if (!data || !trans_info)
        return;

    jobname = gnc_AB_JOB_ID_to_string (gnc_import_TransInfo_get_ref_id (trans_info));
    job = g_datalist_get_data (&data->tmp_job_list, jobname);
    if (!job)
    {
        g_free (jobname);
        return;
    }

    if (imported && gnc_ab_ieci_is_current (data))
        AB_Transaction_List2_PushBack (data->job_list, job);
    else
        AB_Transaction_free (job);

    g_datalist_remove_data (&data->tmp_job_list, jobname);
    g_free (jobname);
}

gchar *
gnc_AB_JOB_to_readable_string (const GNC_AB_JOB *job)
{
    return gnc_AB_JOB_ID_to_string (job ? AB_Transaction_GetUniqueId (job) : 0);
}

gchar *
gnc_AB_JOB_ID_to_string (gulong job_id)
{
    return g_strdup_printf ("job_%lu", job_id);
}

static const AB_TRANSACTION *
get_first_importable_transaction (AB_IMEXPORTER_ACCOUNTINFO *element,
                                  gboolean import_noted_txns);
static AB_IMEXPORTER_ACCOUNTINFO *
find_transactions_cb (AB_IMEXPORTER_ACCOUNTINFO *element, gpointer user_data)
{
    GncABImExContextImport *data = user_data;
    const gboolean import_noted_txns =
        gnc_prefs_get_bool (GNC_PREFS_GROUP_AQBANKING,
                            GNC_PREF_IMPORT_NOTED_TXNS);

    if (get_first_importable_transaction (element, import_noted_txns))
        data->awaiting |= FOUND_TRANSACTIONS;
    return NULL;
}

static AB_IMEXPORTER_ACCOUNTINFO *
find_balances_cb (AB_IMEXPORTER_ACCOUNTINFO *element, gpointer user_data)
{
    GncABImExContextImport *data = user_data;
    const AB_BALANCE *booked_balance;

    if (!AB_ImExporterAccountInfo_GetFirstBalance (element))
        return NULL;

    data->awaiting |= FOUND_BALANCES;
    booked_balance = AB_Balance_List_GetLatestByType (
        AB_ImExporterAccountInfo_GetBalanceList (element), AB_Balance_TypeBooked);
    if (booked_balance && !AB_Value_IsZero (AB_Balance_GetValue (booked_balance)))
        data->has_importable_balance = TRUE;
    return NULL;
}
static const AB_TRANSACTION *
get_first_importable_transaction (AB_IMEXPORTER_ACCOUNTINFO *element,
                                  gboolean import_noted_txns)
{
    const AB_TRANSACTION *trans;

    trans = AB_ImExporterAccountInfo_GetFirstTransaction (
                element, AB_Transaction_TypeStatement, 0);
    if (trans)
        return trans;

    if (!import_noted_txns)
        return NULL;

    return AB_ImExporterAccountInfo_GetFirstTransaction (
               element, AB_Transaction_TypeNotedStatement, 0);
}

static void
import_account_transactions (AB_TRANSACTION_LIST *ab_trans_list,
                             GncABImExContextImport *data,
                             gboolean import_noted_txns)
{
    AB_Transaction_List_ForEachByType (ab_trans_list, txn_transaction_cb, data,
                                       AB_Transaction_TypeStatement, 0);

    if (import_noted_txns)
        AB_Transaction_List_ForEachByType (ab_trans_list, txn_transaction_cb,
                                           data,
                                           AB_Transaction_TypeNotedStatement, 0);
}

static AB_IMEXPORTER_ACCOUNTINFO *
txn_accountinfo_cb (AB_IMEXPORTER_ACCOUNTINFO *element, gpointer user_data)
{
    GncABImExContextImport *data = user_data;
    Account *gnc_acc;
    GtkWindow *parent;
    gboolean import_noted_txns;

    g_return_val_if_fail (element && data, NULL);

    import_noted_txns = gnc_prefs_get_bool (GNC_PREFS_GROUP_AQBANKING,
                                            GNC_PREF_IMPORT_NOTED_TXNS);
    if (data->awaiting & IGNORE_TRANSACTIONS ||
        !get_first_importable_transaction (element, import_noted_txns) ||
        !gnc_ab_ieci_is_current (data) ||
        !gnc_ab_ieci_parent_is_available (data, &parent))
        return NULL;

    gnc_acc = gnc_ab_accinfo_to_gnc_acc (GTK_WIDGET (parent), element);
    if (!gnc_acc)
    {
        g_clear_object (&parent);
        return NULL;
    }
    data->gnc_acc = gnc_acc;

    if (data->execute_txns)
    {
        data->ab_acc = gnc_ab_get_ab_account (data->api, gnc_acc);
        if (!data->ab_acc)
            gnc_error_dialog (parent, "%s",
                              _("No Online Banking account found for this "
                                "GnuCash account. These transactions will "
                                "not be executed by Online Banking."));
    }
    else
    {
        data->ab_acc = NULL;
    }

    if (!data->generic_importer)
    {
        data->generic_importer = gnc_gen_trans_list_new (GTK_WIDGET (parent), NULL,
                                                         TRUE, 14, TRUE);
        if (data->execute_txns)
            gnc_gen_trans_list_add_tp_cb (data->generic_importer,
                                          gnc_ab_trans_processed_cb, data);
    }

    {
        AB_TRANSACTION_LIST *ab_trans_list =
            AB_ImExporterAccountInfo_GetTransactionList (element);
        if (ab_trans_list)
            import_account_transactions (ab_trans_list, data, import_noted_txns);
    }
    g_clear_object (&parent);
    return NULL;
}

static AB_IMEXPORTER_ACCOUNTINFO *
bal_accountinfo_cb (AB_IMEXPORTER_ACCOUNTINFO *element, gpointer user_data)
{
    GncABImExContextImport *data = user_data;
    Account *gnc_acc;
    const AB_BALANCE *booked_bal, *noted_bal;
    const AB_VALUE *booked_val = NULL, *noted_val = NULL;
    gdouble booked_value, noted_value;
    gnc_numeric value;
    time64 booked_tt = 0;
    GtkAlertDialog *dialog;
    GtkWindow *parent;

    g_return_val_if_fail (element && data, NULL);
    if (data->awaiting & IGNORE_BALANCES ||
        !AB_ImExporterAccountInfo_GetFirstBalance (element) ||
        !gnc_ab_ieci_is_current (data) ||
        !gnc_ab_ieci_parent_is_available (data, &parent))
        return NULL;

    booked_bal = AB_Balance_List_GetLatestByType (
        AB_ImExporterAccountInfo_GetBalanceList (element), AB_Balance_TypeBooked);
    gnc_acc = gnc_ab_accinfo_to_gnc_acc (GTK_WIDGET (parent), element);
    if (!gnc_acc)
    {
        g_clear_object (&parent);
        return NULL;
    }
    data->gnc_acc = gnc_acc;

    if (booked_bal)
    {
        const GWEN_DATE *date = AB_Balance_GetDate (booked_bal);
        booked_tt = date ? gnc_gwen_date_to_time64 (date) :
                           gnc_time64_get_day_neutral (gnc_time (NULL));
        booked_val = AB_Balance_GetValue (booked_bal);
        booked_value = booked_val ? AB_Value_GetValueAsDouble (booked_val) : 0.0;
        if (!booked_val)
            g_warning ("bal_accountinfo_cb: booked_val == NULL. Assuming 0");
    }
    else
    {
        g_warning ("bal_accountinfo_cb: booked_bal == NULL. Assuming 0");
        booked_value = 0.0;
    }

    noted_bal = AB_Balance_List_GetLatestByType (
        AB_ImExporterAccountInfo_GetBalanceList (element), AB_Balance_TypeNoted);
    noted_val = noted_bal ? AB_Balance_GetValue (noted_bal) : NULL;
    noted_value = noted_val ? AB_Value_GetValueAsDouble (noted_val) : 0.0;
    if (noted_bal && !noted_val)
        g_warning ("bal_accountinfo_cb: noted_val == NULL. Assuming 0");

    value = double_to_gnc_numeric (booked_value,
                                   xaccAccountGetCommoditySCU (gnc_acc),
                                   GNC_HOW_RND_ROUND_HALF_UP);
    if (noted_value == 0.0 && booked_value == 0.0)
    {
        dialog = gtk_alert_dialog_new (
            "%s",
            _("The downloaded Online Banking Balance was zero.\n\n"
              "Either this is the correct balance, or your bank does not "
              "support Balance download with the parameters you have selected. "
              "In the latter case you should check the details of your connection "
              "like Server URL in the Online Banking (AqBanking) Setup. After "
              "that, try again to download the Online Banking Balance."));
        gtk_alert_dialog_show (dialog, parent);
        g_object_unref (dialog);
    }
    else
    {
        gnc_numeric reconciled = xaccAccountGetReconciledBalance (gnc_acc);
        gchar *booked_str = gnc_AB_VALUE_to_readable_string (booked_val);
        gchar *message1 = g_strdup_printf (
            _("Result of Online Banking job:\nAccount booked balance is %s"),
            booked_str);
        gchar *message2 = noted_value == 0.0 ? g_strdup ("") :
            g_strdup_printf (_("For your information: This account also has a "
                               "noted balance of %s\n"),
                             gnc_AB_VALUE_to_readable_string (noted_val));

        if (gnc_numeric_equal (value, reconciled))
        {
            const gchar *message3 = _("The booked balance is identical to the "
                                      "current reconciled balance of the account.");
            gchar *detail = g_strdup_printf ("%s\n%s", message2, message3);
            dialog = gtk_alert_dialog_new ("%s", message1);
            gtk_alert_dialog_set_detail (dialog, detail);
            gtk_alert_dialog_show (dialog, parent);
            g_object_unref (dialog);
            g_free (detail);
        }
        else
        {
            GncABReconcileRequest *request = g_new0 (GncABReconcileRequest, 1);
            request->account_guid = *xaccAccountGetGUID (gnc_acc);
            request->balance = value;
            request->balance_date = booked_tt;
            request->message = g_strdup_printf ("%s\n%s\n%s", message1, message2,
                                                _("Reconcile account now?"));
            g_queue_push_tail (data->reconcile_requests, request);
        }
        g_free (booked_str);
        g_free (message1);
        g_free (message2);
    }

    g_clear_object (&parent);
    return NULL;
}

static void
job_error_finished (GtkWindow *parent, gint response, gpointer user_data)
{
    GncABImExContextImport *ieci = user_data;

    if (!gnc_ab_ieci_is_current (ieci) || (ieci->has_parent && !parent))
        gnc_ab_ieci_finish (ieci, FALSE);
    else
    {
        if (response == GTK_RESPONSE_YES)
            gnc_error_dialog (parent, "%s",
                              _("Sorry, not implemented yet. Please check the "
                                "console or trace file logs to see which job was "
                                "rejected."));
        gnc_ab_ieci_show_next_job_error (ieci);
    }
    gnc_ab_ieci_unref (ieci);
}

static void
gnc_ab_ieci_after_transactions (GncABImExContextImport *ieci)
{
    if (!gnc_ab_ieci_is_current (ieci))
    {
        gnc_ab_ieci_finish (ieci, FALSE);
        return;
    }
    if (ieci->generic_importer)
        gnc_gen_trans_list_show_all (ieci->generic_importer);
    gnc_ab_ieci_begin_balances (ieci);
}

static void
gnc_ab_ieci_show_next_job_error (GncABImExContextImport *ieci)
{
    GtkWindow *parent;

    if (g_queue_is_empty (ieci->job_errors))
    {
        gnc_ab_ieci_after_transactions (ieci);
        return;
    }
    if (!gnc_ab_ieci_is_current (ieci) ||
        !gnc_ab_ieci_parent_is_available (ieci, &parent))
    {
        gnc_ab_ieci_finish (ieci, FALSE);
        return;
    }

    g_queue_pop_head (ieci->job_errors);
    gnc_ab_ieci_ref (ieci);
    gnc_verify_dialog_async (
        parent, FALSE, job_error_finished, ieci, "%s",
        _("The backend found an error during the preparation of the job. "
          "It is not possible to execute this job.\n\n"
          "Most probably the bank does not support your chosen job or your "
          "Online Banking account does not have the permission to execute this "
          "job. More error messages might be visible on your console log.\n\n"
          "Do you want to enter the job again?"));
    g_clear_object (&parent);
}

static void
transaction_choice_finished (GtkWindow *parent, gint response, gpointer user_data)
{
    GncABImExContextImport *ieci = user_data;

    if (!gnc_ab_ieci_is_current (ieci) || (ieci->has_parent && !parent))
        gnc_ab_ieci_finish (ieci, FALSE);
    else if (response == GTK_RESPONSE_YES)
    {
        AB_IMEXPORTER_ACCOUNTINFO_LIST *accounts =
            AB_ImExporterContext_GetAccountInfoList (ieci->context);
        ieci->awaiting |= AWAIT_TRANSACTIONS;
        if (accounts)
            AB_ImExporterAccountInfo_List_ForEach (accounts, txn_accountinfo_cb, ieci);
        gnc_ab_ieci_show_next_job_error (ieci);
    }
    else
    {
        ieci->awaiting |= IGNORE_TRANSACTIONS;
        gnc_ab_ieci_after_transactions (ieci);
    }
    gnc_ab_ieci_unref (ieci);
}

static void
gnc_ab_ieci_begin_transactions (GncABImExContextImport *ieci)
{
    AB_IMEXPORTER_ACCOUNTINFO_LIST *accounts;
    GtkWindow *parent;

    if (!gnc_ab_ieci_is_current (ieci))
    {
        gnc_ab_ieci_finish (ieci, FALSE);
        return;
    }
    accounts = AB_ImExporterContext_GetAccountInfoList (ieci->context);
    if (!accounts || !AB_ImExporterAccountInfo_List_GetCount (accounts) ||
        (ieci->awaiting & IGNORE_TRANSACTIONS))
    {
        gnc_ab_ieci_after_transactions (ieci);
        return;
    }

    if (!(ieci->awaiting & AWAIT_TRANSACTIONS) &&
        (ieci->awaiting & FOUND_TRANSACTIONS))
    {
        if (!gnc_ab_ieci_parent_is_available (ieci, &parent))
        {
            gnc_ab_ieci_finish (ieci, FALSE);
            return;
        }
        gnc_ab_ieci_ref (ieci);
        gnc_verify_dialog_async (
            parent, TRUE, transaction_choice_finished, ieci, "%s",
            _("The bank has sent transaction information in its response.\n"
              "Do you want to import it?"));
        g_clear_object (&parent);
        return;
    }

    AB_ImExporterAccountInfo_List_ForEach (accounts, txn_accountinfo_cb, ieci);
    gnc_ab_ieci_show_next_job_error (ieci);
}

static void
reconcile_finished (GtkWindow *parent, gint response, gpointer user_data)
{
    GncABImExContextImport *ieci = user_data;
    GncABReconcileRequest *request;

    if (!gnc_ab_ieci_is_current (ieci) || (ieci->has_parent && !parent))
    {
        gnc_ab_ieci_finish (ieci, FALSE);
        gnc_ab_ieci_unref (ieci);
        return;
    }

    request = g_queue_pop_head (ieci->reconcile_requests);
    if (response == GTK_RESPONSE_YES && request)
    {
        Account *account = xaccAccountLookup (&request->account_guid, ieci->book);
        if (account && !qof_instance_get_destroying (QOF_INSTANCE (account)))
            recnWindowWithBalance (GTK_WIDGET (parent), account, request->balance,
                                   request->balance_date);
    }
    free_reconcile_request (request);
    gnc_ab_ieci_show_next_reconcile_request (ieci);
    gnc_ab_ieci_unref (ieci);
}

static void
gnc_ab_ieci_show_messages (GncABImExContextImport *ieci)
{
    AB_MESSAGE *message;
    GtkWindow *parent;

    if (!gnc_ab_ieci_is_current (ieci) ||
        !gnc_ab_ieci_parent_is_available (ieci, &parent))
    {
        gnc_ab_ieci_finish (ieci, FALSE);
        return;
    }

    message = AB_ImExporterContext_GetFirstMessage (ieci->context);
    while (message)
    {
        gnc_info_dialog (parent, "%s\n%s %s\n%s",
                         _("The bank has sent a message in its response."),
                         _("Subject:"), AB_Message_GetSubject (message),
                         AB_Message_GetText (message));
        message = AB_Message_List_Next (message);
    }
    g_clear_object (&parent);
    gnc_ab_ieci_finish (ieci, TRUE);
}

static void
gnc_ab_ieci_show_next_reconcile_request (GncABImExContextImport *ieci)
{
    GncABReconcileRequest *request;
    GtkWindow *parent;

    if (g_queue_is_empty (ieci->reconcile_requests))
    {
        gnc_ab_ieci_show_messages (ieci);
        return;
    }
    if (!gnc_ab_ieci_is_current (ieci) ||
        !gnc_ab_ieci_parent_is_available (ieci, &parent))
    {
        gnc_ab_ieci_finish (ieci, FALSE);
        return;
    }

    request = g_queue_peek_head (ieci->reconcile_requests);
    gnc_ab_ieci_ref (ieci);
    gnc_verify_dialog_async (parent, TRUE, reconcile_finished, ieci, "%s",
                             request->message);
    g_clear_object (&parent);
}

static void
balance_choice_finished (GtkWindow *parent, gint response, gpointer user_data)
{
    GncABImExContextImport *ieci = user_data;
    AB_IMEXPORTER_ACCOUNTINFO_LIST *accounts;

    if (!gnc_ab_ieci_is_current (ieci) || (ieci->has_parent && !parent))
        gnc_ab_ieci_finish (ieci, FALSE);
    else if (response == GTK_RESPONSE_YES)
    {
        ieci->awaiting |= AWAIT_BALANCES;
        accounts = AB_ImExporterContext_GetAccountInfoList (ieci->context);
        if (accounts)
            AB_ImExporterAccountInfo_List_ForEach (accounts, bal_accountinfo_cb, ieci);
        gnc_ab_ieci_show_next_reconcile_request (ieci);
    }
    else
    {
        ieci->awaiting |= IGNORE_BALANCES;
        gnc_ab_ieci_show_messages (ieci);
    }
    gnc_ab_ieci_unref (ieci);
}

static void
gnc_ab_ieci_begin_balances (GncABImExContextImport *ieci)
{
    AB_IMEXPORTER_ACCOUNTINFO_LIST *accounts;
    GtkWindow *parent;

    if (!gnc_ab_ieci_is_current (ieci))
    {
        gnc_ab_ieci_finish (ieci, FALSE);
        return;
    }
    accounts = AB_ImExporterContext_GetAccountInfoList (ieci->context);
    if (!accounts || !AB_ImExporterAccountInfo_List_GetCount (accounts) ||
        (ieci->awaiting & IGNORE_BALANCES))
    {
        gnc_ab_ieci_show_messages (ieci);
        return;
    }

    if (!(ieci->awaiting & AWAIT_BALANCES))
    {
        if (!ieci->has_importable_balance)
        {
            gnc_ab_ieci_show_messages (ieci);
            return;
        }
        parent = gnc_ab_ieci_get_balance_parent (ieci);
        if (!parent && ieci->has_parent)
        {
            gnc_ab_ieci_finish (ieci, FALSE);
            return;
        }
        gnc_ab_ieci_ref (ieci);
        gnc_verify_dialog_async (
            parent, TRUE, balance_choice_finished, ieci, "%s",
            _("The bank has sent balance information in its response.\n"
              "Do you want to import it?"));
        g_clear_object (&parent);
        return;
    }

    AB_ImExporterAccountInfo_List_ForEach (accounts, bal_accountinfo_cb, ieci);
    gnc_ab_ieci_show_next_reconcile_request (ieci);
}

static void
gnc_ab_ieci_finish (GncABImExContextImport *ieci, gboolean completed)
{
    if (!ieci || ieci->finished)
        return;

    ieci->finished = TRUE;
    if (ieci->done_cb)
        ieci->done_cb (ieci, completed, ieci->user_data);
    gnc_ab_ieci_unref (ieci);
}

static gboolean
gnc_ab_ieci_start (gpointer user_data)
{
    GncABImExContextImport *ieci = user_data;
    AB_IMEXPORTER_ACCOUNTINFO_LIST *accounts;

    if (!gnc_ab_ieci_is_current (ieci))
    {
        gnc_ab_ieci_finish (ieci, FALSE);
        return G_SOURCE_REMOVE;
    }

    accounts = AB_ImExporterContext_GetAccountInfoList (ieci->context);
    if (accounts && AB_ImExporterAccountInfo_List_GetCount (accounts))
    {
        AB_ImExporterAccountInfo_List_ForEach (accounts, find_transactions_cb, ieci);
        AB_ImExporterAccountInfo_List_ForEach (accounts, find_balances_cb, ieci);
    }
    gnc_ab_ieci_begin_transactions (ieci);
    return G_SOURCE_REMOVE;
}

void
gnc_ab_import_context_async (AB_IMEXPORTER_CONTEXT *context, guint awaiting,
                             gboolean execute_txns, AB_BANKING *api,
                             GtkWidget *parent, GCancellable *cancellable,
                             GncABImportContextDoneCB done_cb,
                             gpointer user_data)
{
    GncABImExContextImport *ieci;

    g_return_if_fail (context);
    g_return_if_fail (!(awaiting & AWAIT_BALANCES) ||
                      !(awaiting & IGNORE_BALANCES));
    g_return_if_fail (!(awaiting & AWAIT_TRANSACTIONS) ||
                      !(awaiting & IGNORE_TRANSACTIONS));
    g_return_if_fail (awaiting & AWAIT_TRANSACTIONS || !execute_txns);
    g_return_if_fail (!execute_txns || api);

    ieci = g_new0 (GncABImExContextImport, 1);
    ieci->ref_count = 1;
    ieci->awaiting = awaiting;
    ieci->execute_txns = execute_txns;
    ieci->api = api;
    ieci->context = context;
    ieci->has_parent = parent != NULL;
    g_weak_ref_init (&ieci->parent, parent);
    ieci->book = gnc_get_current_book ();
    if (ieci->book)
        ieci->book_guid = *qof_instance_get_guid (QOF_INSTANCE (ieci->book));
    ieci->cancellable = cancellable ? g_object_ref (cancellable) : NULL;
    ieci->done_cb = done_cb;
    ieci->user_data = user_data;
    ieci->job_list = AB_Transaction_List2_new ();
    ieci->job_errors = g_queue_new ();
    ieci->reconcile_requests = g_queue_new ();
    g_datalist_init (&ieci->tmp_job_list);

    g_idle_add_full (G_PRIORITY_DEFAULT_IDLE, gnc_ab_ieci_start, ieci, NULL);
}

guint
gnc_ab_ieci_get_found (GncABImExContextImport *ieci)
{
    g_return_val_if_fail (ieci, 0);
    return ieci->awaiting;
}

GNC_AB_JOB_LIST2 *
gnc_ab_ieci_get_job_list (GncABImExContextImport *ieci)
{
    g_return_val_if_fail (ieci, NULL);
    return ieci->job_list;
}

typedef struct
{
    GncABImExContextImport *ieci;
    GncABMatcherDoneCB done_cb;
    gpointer user_data;
} GncABMatcherRun;

static void
gnc_ab_matcher_finished_cb (gboolean accepted, gpointer user_data)
{
    GncABMatcherRun *run = user_data;

    run->ieci->generic_importer = NULL;
    if (run->done_cb)
        run->done_cb (run->ieci, accepted, run->user_data);
    gnc_ab_ieci_unref (run->ieci);
    g_free (run);
}

void
gnc_ab_ieci_run_matcher_async (GncABImExContextImport *ieci,
                               GncABMatcherDoneCB done_cb,
                               gpointer user_data)
{
    GncABMatcherRun *run;

    g_return_if_fail (ieci);
    if (!ieci->generic_importer)
    {
        if (done_cb)
            done_cb (ieci, FALSE, user_data);
        return;
    }

    run = g_new0 (GncABMatcherRun, 1);
    run->ieci = gnc_ab_ieci_ref (ieci);
    run->done_cb = done_cb;
    run->user_data = user_data;
    gnc_gen_trans_list_present (ieci->generic_importer,
                                gnc_ab_matcher_finished_cb, run);
}
GWEN_DB_NODE *
gnc_ab_get_permanent_certs (void)
{
    int rv;
    GWEN_DB_NODE *perm_certs = NULL;
    AB_BANKING *banking = gnc_AB_BANKING_new ();

    g_return_val_if_fail (banking, NULL);
    rv = AB_Banking_LoadSharedConfig (banking, "certs", &perm_certs);
    gnc_AB_BANKING_fini (banking);
    g_return_val_if_fail (rv >= 0, NULL);
    return perm_certs;
}

#if (AQBANKING_VERSION_INT >= 60400)
GList*
gnc_ab_trans_templ_list_new_from_ref_accounts (GNC_AB_ACCOUNT_SPEC *ab_acc)
{
    GList *retval = NULL;
    AB_REFERENCE_ACCOUNT *ra;
    AB_REFERENCE_ACCOUNT_LIST *ral;
    GWEN_BUFFER *accNameForTemplate = GWEN_Buffer_new (0,120,0,0);
    gnc_numeric zero = gnc_numeric_zero ();

    /* get the target account list */
    ral = AB_AccountSpec_GetRefAccountList (ab_acc);
    ra = AB_ReferenceAccount_List_First (ral);

    /* fill the template list with the target accounts */
    while (ra)
    {
        GncABTransTempl *new_templ = gnc_ab_trans_templ_new ();
        const char *iban = AB_ReferenceAccount_GetIban (ra);
        const char *accName = AB_ReferenceAccount_GetAccountName (ra);
        GWEN_Buffer_Reset (accNameForTemplate);
        if (accName)
        {
            GWEN_Buffer_AppendString (accNameForTemplate, accName);
            GWEN_Buffer_AppendString (accNameForTemplate, ": ");
        }
        GWEN_Buffer_AppendString (accNameForTemplate, iban);
        gnc_ab_trans_templ_set_name (new_templ, GWEN_Buffer_GetStart (accNameForTemplate));
        gnc_ab_trans_templ_set_recp_name (new_templ, AB_ReferenceAccount_GetOwnerName (ra));
        gnc_ab_trans_templ_set_recp_account (new_templ, AB_ReferenceAccount_GetIban (ra));
        gnc_ab_trans_templ_set_recp_bankcode (new_templ, AB_ReferenceAccount_GetBic (ra));
        gnc_ab_trans_templ_set_amount (new_templ, zero);
        retval = g_list_prepend (retval, new_templ);
        ra = AB_ReferenceAccount_List_Next (ra);
    }
    retval = g_list_reverse (retval);

    GWEN_Buffer_free (accNameForTemplate);

    return retval;
}
#endif
static int
ab_node_pair_compare (AB_Node_Pair* left, AB_Node_Pair* right)
{
     return left ? (right ? g_strcmp0 (left->name, right->name) : -1) :
          (right ? 1 : 0);
}

GList*
gnc_ab_imexporter_list (AB_BANKING* api)
{
    GList* desc_list = NULL;
    GWEN_PLUGIN_DESCRIPTION_LIST2 *il =
        AB_Banking_GetImExporterDescrs (api);
    GWEN_PLUGIN_DESCRIPTION_LIST2_ITERATOR *ilit;
    g_return_val_if_fail (il, NULL);
    ilit = GWEN_PluginDescription_List2_First(il);

    for (GWEN_PLUGIN_DESCRIPTION *pd =
            GWEN_PluginDescription_List2Iterator_Data(ilit);
         pd;
         pd = GWEN_PluginDescription_List2Iterator_Next(ilit))
    {
        AB_Node_Pair *node = NULL;

        node = g_slice_new (AB_Node_Pair);
        node->name = g_strdup(GWEN_PluginDescription_GetName(pd));
        node->descr = g_strdup(GWEN_PluginDescription_GetShortDescr(pd));
        desc_list = g_list_prepend (desc_list, node);
    }
    GWEN_PluginDescription_List2_free(il);
    return g_list_sort (desc_list, (GCompareFunc)ab_node_pair_compare);
}

GList*
gnc_ab_imexporter_profile_list (AB_BANKING* api, const char* importer_name)
{
    GList* prof_list = NULL;
    GWEN_DB_NODE* db = AB_Banking_GetImExporterProfiles(api, importer_name);
    g_return_val_if_fail (db, NULL);

    for (GWEN_DB_NODE *profile = GWEN_DB_GetFirstGroup(db); profile;
         profile = GWEN_DB_GetNextGroup(profile))
    {
         AB_Node_Pair *node = g_slice_new(AB_Node_Pair);
        if (!profile) continue;
        node->name = g_strdup(GWEN_DB_GetCharValue(profile, "name", 0, NULL));
        node->descr = g_strdup(GWEN_DB_GetCharValue(profile, "shortDescr", 0, NULL));
        prof_list = g_list_prepend (prof_list, node);
    }
    return g_list_sort (prof_list, (GCompareFunc)ab_node_pair_compare);
}
