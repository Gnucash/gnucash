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

#include "config.h"

#include <glib/gi18n.h>
#include <gwenhywfar/gwenhywfar.h>
#include <aqbanking/banking.h>

#include "RecnWindow.h"
#include "Transaction.h"
#include "dialog-ab-trans.h"
#include "gnc-ab-kvp.h"
#include "gnc-ab-utils.h"
#include "gnc-glib-utils.h"
#include "gnc-gwen-gui.h"
#include "gnc-ui.h"
#include "import-account-matcher.h"
#include "import-main-matcher.h"
#include "import-utilities.h"
#include "qof.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

/* Global variables for AB_BANKING caching. */
static AB_BANKING *gnc_AB_BANKING = NULL;
static gint gnc_AB_BANKING_refcount = 0;

static gpointer join_ab_strings_cb(const gchar *str, gpointer user_data);
static Account *gnc_ab_accinfo_to_gnc_acc(
    AB_IMEXPORTER_ACCOUNTINFO *account_info);
static const AB_TRANSACTION *txn_transaction_cb(
    const AB_TRANSACTION *element, gpointer user_data);
static AB_IMEXPORTER_ACCOUNTINFO *txn_accountinfo_cb(
    AB_IMEXPORTER_ACCOUNTINFO *element, gpointer user_data);
static AB_IMEXPORTER_ACCOUNTINFO *bal_accountinfo_cb(
    AB_IMEXPORTER_ACCOUNTINFO *element, gpointer user_data);

struct _GncABImExContextImport {
    guint awaiting;
    gboolean txn_found;
    Account *gnc_acc;
    AB_ACCOUNT *ab_acc;
    gboolean execute_txns;
    AB_BANKING *api;
    GtkWidget *parent;
    AB_JOB_LIST2 *job_list;
    GNCImportMainMatcher *generic_importer;
};

void
gnc_GWEN_Init(void)
{
    gint i;

    /* Initialize gwen library */
    GWEN_Init();

    /* Initialize gwen logging */
    GWEN_Logger_SetLevel(NULL, GWEN_LoggerLevel_Debug);
    GWEN_Logger_SetLevel(GWEN_LOGDOMAIN, GWEN_LoggerLevel_Debug);
    GWEN_Logger_SetLevel(AQBANKING_LOGDOMAIN, GWEN_LoggerLevel_Debug);
    gnc_GWEN_Gui_log_init();
}

void
gnc_GWEN_Fini(void)
{
    /* Shutdown the GWEN_GUIs */
    gnc_GWEN_Gui_shutdown();
    GWEN_Logger_SetLevel(NULL, GWEN_LoggerLevel_Warning);
    GWEN_Logger_SetLevel(GWEN_LOGDOMAIN, GWEN_LoggerLevel_Warning);
    GWEN_Logger_SetLevel(AQBANKING_LOGDOMAIN, GWEN_LoggerLevel_Warning);

    /* Finalize gwen library */
    GWEN_Fini();
}

AB_BANKING *
gnc_AB_BANKING_new(void)
{
    AB_BANKING *api;

    if (gnc_AB_BANKING) {
        /* API cached. */
        api = gnc_AB_BANKING;

        /* Init the API again. */
        if (gnc_AB_BANKING_refcount == 0)
            g_return_val_if_fail(AB_Banking_Init(api) == 0, NULL);

    } else {
        api = AB_Banking_new("gnucash", NULL, 0);
        g_return_val_if_fail(api, NULL);

        /* Init the API */
        g_return_val_if_fail(AB_Banking_Init(api) == 0, NULL);

        /* Cache it */
        gnc_AB_BANKING = api;
        gnc_AB_BANKING_refcount = 0;
    }

    gnc_AB_BANKING_refcount++;

    return api;
}

void
gnc_AB_BANKING_delete(AB_BANKING *api)
{
    if (!api)
        api = gnc_AB_BANKING;

    if (api) {
        if (api == gnc_AB_BANKING) {
            gnc_AB_BANKING = NULL;
            if (gnc_AB_BANKING_refcount > 0)
                AB_Banking_Fini(api);
        }

        AB_Banking_free(api);
    }
}


gint
gnc_AB_BANKING_fini(AB_BANKING *api)
{
    if (api == gnc_AB_BANKING) {
        if (--gnc_AB_BANKING_refcount == 0)
            return AB_Banking_Fini(api);
    } else {
        return AB_Banking_Fini(api);
    }
    return 0;
}

AB_ACCOUNT *
gnc_ab_get_ab_account(const AB_BANKING *api, Account *gnc_acc)
{
    AB_ACCOUNT *ab_account = NULL;
    const gchar *bankcode = NULL;
    const gchar *accountid = NULL;
    guint32 account_uid = 0;

    bankcode = gnc_ab_get_account_bankcode(gnc_acc);
    accountid = gnc_ab_get_account_accountid(gnc_acc);
    account_uid = gnc_ab_get_account_uid (gnc_acc);

    if (account_uid > 0) {
        ab_account = AB_Banking_GetAccount(api, account_uid);

        if (!ab_account && bankcode && *bankcode && accountid && *accountid) {
            g_message("gnc_ab_get_ab_account: No AB_ACCOUNT found for UID %d, "
                      "trying bank code\n", account_uid);
            ab_account = AB_Banking_GetAccountByCodeAndNumber(api, bankcode,
                                                              accountid);
        }
        return ab_account;

    } else if (bankcode && *bankcode && accountid && *accountid) {
        ab_account = AB_Banking_GetAccountByCodeAndNumber(api, bankcode,
                                                          accountid);
        return ab_account;
    }

    return NULL;
}

gchar *
gnc_AB_VALUE_to_readable_string(const AB_VALUE *value)
{
    if (value)
        return g_strdup_printf("%.2f %s",
                               AB_Value_GetValueAsDouble(value),
                               AB_Value_GetCurrency(value));
    else
        return g_strdup_printf("%.2f", 0.0);
}

/**
 * Take a string from a GWEN_STRINGLIST, strip invalid utf8 and join it
 * to the rest.
 */
static gpointer
join_ab_strings_cb(const gchar *str, gpointer user_data)
{
    gchar **acc = user_data;
    gchar *tmp;

    if (!str || !*str)
        return NULL;

    tmp = g_strdup(str);
    g_strstrip(tmp);
    gnc_utf8_strip_invalid(tmp);

    if (*acc) {
        gchar *join = g_strjoin(" ", *acc, tmp, (gchar*) NULL);
        g_free(*acc);
        g_free(tmp);
        *acc = join;
    } else {
        *acc = tmp;
    }
    return NULL;
}

gchar *
gnc_ab_get_remote_name(const AB_TRANSACTION *ab_trans)
{
    const GWEN_STRINGLIST *ab_remote_name;
    gchar *gnc_other_name = NULL;

    g_return_val_if_fail(ab_trans, NULL);

    ab_remote_name = AB_Transaction_GetRemoteName(ab_trans);
    if (ab_remote_name)
        GWEN_StringList_ForEach(ab_remote_name, join_ab_strings_cb,
                                &gnc_other_name);

    if (!gnc_other_name || !*gnc_other_name) {
        g_free(gnc_other_name);
        gnc_other_name = NULL;
    }

    return gnc_other_name;
}

gchar *
gnc_ab_get_purpose(const AB_TRANSACTION *ab_trans)
{
    const GWEN_STRINGLIST *ab_purpose;
    gchar *gnc_description = NULL;

    g_return_val_if_fail(ab_trans, g_strdup(""));

    ab_purpose = AB_Transaction_GetPurpose(ab_trans);
    if (ab_purpose)
        GWEN_StringList_ForEach(ab_purpose, join_ab_strings_cb,
                                &gnc_description);

    if (!gnc_description)
        gnc_description = g_strdup("");

    return gnc_description;
}

gchar *
gnc_ab_description_to_gnc(const AB_TRANSACTION *ab_trans)
{
  /* Description */
    gchar *description = gnc_ab_get_purpose(ab_trans);
    gchar *other_name = gnc_ab_get_remote_name(ab_trans);
    gchar *retval;

    if (other_name) {
        if (description && *description) {
            retval = g_strdup_printf("%s; %s", description, other_name);
        } else {
            retval = g_strdup(other_name);
        }
    } else {
        if (description && *description) {
            retval = g_strdup(description);
        } else {
            retval = g_strdup(_("Unspecified"));
        }
    }
    g_free(description);
    g_free(other_name);

    return retval;
}

gchar *
gnc_ab_memo_to_gnc(const AB_TRANSACTION *ab_trans)
{
    const gchar *ab_remote_accountnumber =
        AB_Transaction_GetRemoteAccountNumber(ab_trans);
    const gchar *ab_remote_bankcode =
        AB_Transaction_GetRemoteBankCode(ab_trans);
    gchar *ab_other_accountid =
        g_strdup(ab_remote_accountnumber ? ab_remote_accountnumber
                 : _("unknown"));
    gchar *ab_other_bankcode =
        g_strdup(ab_remote_bankcode ? ab_remote_bankcode
                 : _("unknown"));
    gchar *retval;

    g_strstrip(ab_other_accountid);
    g_strstrip(ab_other_bankcode);
    /* Ensure string is in utf8 */
    gnc_utf8_strip_invalid(ab_other_accountid);
    gnc_utf8_strip_invalid(ab_other_bankcode);

    if (ab_other_accountid && *ab_other_accountid) {
        retval = g_strdup_printf("%s %s %s %s",
                                 _("Account"), ab_other_accountid,
                                 _("Bank"), ab_other_bankcode);
    } else {
        retval = g_strdup("");
    }
    g_free(ab_other_accountid);
    g_free(ab_other_bankcode);

    return retval;
}

Transaction *
gnc_ab_trans_to_gnc(const AB_TRANSACTION *ab_trans, Account *gnc_acc)
{
    QofBook *book;
    Transaction *gnc_trans;
    const gchar *fitid;
    const GWEN_TIME *valuta_date;
    time_t current_time;
    const char *custref;
    gchar *description;
    Split *split;
    const AB_VALUE *ab_value;
    gnc_numeric gnc_amount;
    gchar *memo;

    g_return_val_if_fail(ab_trans && gnc_acc, NULL);

    /* Create new GnuCash transaction for the given AqBanking one */
    book = gnc_account_get_book(gnc_acc);
    gnc_trans = xaccMallocTransaction(book);
    xaccTransBeginEdit(gnc_trans);

    /* Set OFX unique transaction ID */
    fitid = AB_Transaction_GetFiId(ab_trans);
    if (fitid && *fitid)
        gnc_import_set_trans_online_id(gnc_trans, fitid);

    /* Date / Time */
    valuta_date = AB_Transaction_GetValutaDate(ab_trans);
    if (!valuta_date) {
        const GWEN_TIME *normal_date = AB_Transaction_GetDate(ab_trans);
        if (normal_date)
            valuta_date = normal_date;
    }
    if (valuta_date)
        xaccTransSetDateSecs(gnc_trans, GWEN_Time_toTime_t(valuta_date));
    else
        g_warning("transaction_cb: Oops, date 'valuta_date' was NULL");

    current_time = time(NULL);
    xaccTransSetDateEnteredSecs(gnc_trans, mktime(localtime(&current_time)));

    /* Currency.  We take simply the default currency of the gnucash account */
    xaccTransSetCurrency(gnc_trans, xaccAccountGetCommodity(gnc_acc));

    /* Number.  We use the "customer reference", if there is one. */
    custref = AB_Transaction_GetCustomerReference(ab_trans);
    if (custref && *custref
        && g_ascii_strncasecmp(custref, "NONREF", 6) != 0)
        xaccTransSetNum(gnc_trans, custref);

    /* Description */
    description = gnc_ab_description_to_gnc(ab_trans);
    xaccTransSetDescription(gnc_trans, description);
    g_free(description);

    /* Notes. */
    /* xaccTransSetNotes(gnc_trans, g_notes); */
    /* But Nobody ever uses the Notes field? */

    /* Add one split */
    split = xaccMallocSplit(book);
    xaccSplitSetParent(split, gnc_trans);
    xaccSplitSetAccount(split, gnc_acc);

    /* Amount into the split */
    ab_value = AB_Transaction_GetValue(ab_trans);
    gnc_amount = double_to_gnc_numeric(
        ab_value ? AB_Value_GetValueAsDouble(ab_value) : 0.0,
        xaccAccountGetCommoditySCU(gnc_acc),
        GNC_RND_ROUND);
    if (!ab_value)
        g_warning("transaction_cb: Oops, value was NULL.  Using 0");
    xaccSplitSetBaseValue(split, gnc_amount, xaccAccountGetCommodity(gnc_acc));

    /* Memo in the Split. */
    memo = gnc_ab_memo_to_gnc(ab_trans);
    xaccSplitSetMemo(split, memo);
    g_free(memo);

    return gnc_trans;
}

/**
 * Call gnc_import_select_account() on the online id constructed using
 * the information in @a acc_info.
 *
 * @param acc_info AB_IMEXPORTER_ACCOUNTINFO
 * @return A GnuCash account, or NULL otherwise
 */
static Account *
gnc_ab_accinfo_to_gnc_acc(AB_IMEXPORTER_ACCOUNTINFO *acc_info)
{
    const gchar *bankcode, *accountnumber;
    gchar *online_id;
    Account *gnc_acc;

    g_return_val_if_fail(acc_info, NULL);

    bankcode = AB_ImExporterAccountInfo_GetBankCode(acc_info);
    accountnumber = AB_ImExporterAccountInfo_GetAccountNumber(acc_info);
    online_id = g_strconcat(bankcode ? bankcode : "",
                            accountnumber ? accountnumber : "",
                            (gchar*)NULL);
    gnc_acc = gnc_import_select_account(
        NULL, online_id, 1, AB_ImExporterAccountInfo_GetAccountName(acc_info),
        NULL, ACCT_TYPE_NONE, NULL, NULL);
    if (!gnc_acc) {
        g_warning("gnc_ab_accinfo_to_gnc_acc: Could not determine source account"
                  " for online_id %s", online_id);
    }
    g_free(online_id);

    return gnc_acc;
}

static const AB_TRANSACTION *
txn_transaction_cb(const AB_TRANSACTION *element, gpointer user_data)
{
    GncABImExContextImport *data = user_data;
    Transaction *gnc_trans;

    g_return_val_if_fail(element && data, NULL);

    /* Create a GnuCash transaction from ab_trans */
    gnc_trans = gnc_ab_trans_to_gnc(element, data->gnc_acc);

    /* Instead of xaccTransCommitEdit(gnc_trans)  */
    gnc_gen_trans_list_add_trans(data->generic_importer, gnc_trans);

    if (data->execute_txns && data->ab_acc) {
        AB_TRANSACTION *ab_trans = AB_Transaction_dup(element);
        AB_JOB *job;

        /* NEW: The imported transaction has been imported into gnucash.
         * Now also add it as a job to aqbanking */
        AB_Transaction_SetLocalBankCode(
            ab_trans, AB_Account_GetBankCode(data->ab_acc));
        AB_Transaction_SetLocalAccountNumber(
            ab_trans, AB_Account_GetAccountNumber(data->ab_acc));
        AB_Transaction_SetLocalCountry(ab_trans, "DE");

        job = gnc_ab_get_trans_job(data->ab_acc, ab_trans, SINGLE_DEBITNOTE);

        /* Check whether we really got a job */
        if (!job) {
            /* Oops, no job, probably not supported by bank */
            if (gnc_verify_dialog(
                    NULL, FALSE, "%s",
                    _("The backend found an error during the preparation "
                      "of the job. It is not possible to execute this job. \n"
                      "\n"
                      "Most probable the bank does not support your chosen "
                      "job or your Online Banking account does not have the permission "
                      "to execute this job. More error messages might be "
                      "visible on your console log.\n"
                      "\n"
                      "Do you want to enter the job again?"))) {
                gnc_error_dialog(NULL, "Sorry, not implemented yet.");
            }
            /* else */
        }
        AB_Job_List2_PushBack(data->job_list, job);

        AB_Transaction_free(ab_trans);
    }

    return NULL;
}

static AB_IMEXPORTER_ACCOUNTINFO *
txn_accountinfo_cb(AB_IMEXPORTER_ACCOUNTINFO *element, gpointer user_data)
{
    GncABImExContextImport *data = user_data;
    Account *gnc_acc;

    g_return_val_if_fail(element && data, NULL);

    if (data->awaiting & IGNORE_TRANSACTIONS)
        /* Ignore them */
        return NULL;

    if (!AB_ImExporterAccountInfo_GetFirstTransaction(element))
        /* No transaction found */
        return NULL;
    else
        data->awaiting |= FOUND_TRANSACTIONS;

    if (!(data->awaiting & AWAIT_TRANSACTIONS)) {
        if (gnc_verify_dialog(data->parent, TRUE, "%s",
                              _("The bank has sent transaction information "
                                "in its response."
                                "\n"
                                "Do you want to import it?"))) {
            data->awaiting |= AWAIT_TRANSACTIONS;
        } else {
            data->awaiting |= IGNORE_TRANSACTIONS;
            return NULL;
        }
    }

    /* Lookup the corresponding gnucash account */
    gnc_acc = gnc_ab_accinfo_to_gnc_acc(element);
    if (!gnc_acc) return NULL;
    data->gnc_acc = gnc_acc;

    if (data->execute_txns) {
        /* Retrieve the aqbanking account that belongs to this gnucash
         * account */
        data->ab_acc = gnc_ab_get_ab_account(data->api, gnc_acc);
        if (!data->ab_acc) {
            gnc_error_dialog(NULL, "%s",
                             _("No Online Banking account found for this "
                               "gnucash account. These transactions will "
                               "not be executed by Online Banking."));
        }
    } else {
        data->ab_acc = NULL;
    }

    if (!data->generic_importer)
        data->generic_importer = gnc_gen_trans_list_new(data->parent, NULL,
                                                        TRUE, 14);

    /* Iterate through all transactions */
    AB_ImExporterAccountInfo_TransactionsForEach(element, txn_transaction_cb,
                                                 data);

    return NULL;
}

static AB_IMEXPORTER_ACCOUNTINFO *
bal_accountinfo_cb(AB_IMEXPORTER_ACCOUNTINFO *element, gpointer user_data)
{
    GncABImExContextImport *data = user_data;
    Account *gnc_acc;
    AB_ACCOUNT_STATUS *item, *best = NULL;
    const GWEN_TIME *best_time;
    const AB_BALANCE *booked_bal, *noted_bal;
    const AB_VALUE *booked_val = NULL, *noted_val = NULL;
    gdouble booked_value, noted_value;
    gnc_numeric value;
    time_t booked_tt = 0;
    GtkWidget *dialog;
    gboolean show_recn_window = FALSE;

    g_return_val_if_fail(element && data, NULL);

    if (data->awaiting & IGNORE_BALANCES)
        /* Ignore them */
        return NULL;

    if (!AB_ImExporterAccountInfo_GetFirstAccountStatus(element))
        /* No balance found */
        return NULL;
    else
        data->awaiting |= FOUND_BALANCES;

    if (!(data->awaiting & AWAIT_BALANCES)) {
        if (gnc_verify_dialog(data->parent, TRUE, "%s",
                              _("The bank has sent balance information "
                                "in its response."
                                "\n"
                                "Do you want to import it?"))) {
            data->awaiting |= AWAIT_BALANCES;
        } else {
            data->awaiting |= IGNORE_BALANCES;
            return NULL;
        }
    }

    /* Lookup the corresponding gnucash account */
    gnc_acc = gnc_ab_accinfo_to_gnc_acc(element);
    if (!gnc_acc) return NULL;
    data->gnc_acc = gnc_acc;

    /* Lookup the most recent ACCOUNT_STATUS available */
    item = AB_ImExporterAccountInfo_GetFirstAccountStatus(element);
    while (item) {
        const GWEN_TIME *item_time = AB_AccountStatus_GetTime(item);
        if (!best || GWEN_Time_Diff(best_time, item_time) < 0.0) {
            best = item;
            best_time = item_time;
        }
        item = AB_ImExporterAccountInfo_GetNextAccountStatus(element);
    }

    /* Lookup booked balance and time */
    booked_bal = AB_AccountStatus_GetBookedBalance(best);
    if (booked_bal) {
        const GWEN_TIME *ti = AB_Balance_GetTime(booked_bal);
        if (ti) {
            booked_tt =  GWEN_Time_toTime_t(ti);
        } else {
            /* No time found? Use today because the HBCI query asked for today's
             * balance. */
            booked_tt = gnc_timet_get_day_start(time(NULL));
        }
        booked_val = AB_Balance_GetValue(booked_bal);
        if (booked_val) {
            booked_value = AB_Value_GetValueAsDouble(booked_val);
        } else {
            g_warning("bal_accountinfo_cb: booked_val == NULL.  Assuming 0");
            booked_value = 0.0;
        }
    } else {
        g_warning("bal_accountinfo_cb: booked_bal == NULL.  Assuming 0");
        booked_tt = 0;
        booked_value = 0.0;
    }

    /* Lookup noted balance */
    noted_bal = AB_AccountStatus_GetNotedBalance(best);
    if (noted_bal) {
        noted_val = AB_Balance_GetValue(noted_bal);
        if (noted_val)
            noted_value = AB_Value_GetValueAsDouble(noted_val);
        else {
            g_warning("bal_accountinfo_cb: noted_val == NULL.  Assuming 0");
            noted_value = 0.0;
        }
    } else {
        g_warning("bal_accountinfo_cb: noted_bal == NULL.  Assuming 0");
        noted_value = 0.0;
    }

    value = double_to_gnc_numeric(booked_value,
                                  xaccAccountGetCommoditySCU(gnc_acc),
                                  GNC_RND_ROUND);
    if (noted_value == 0.0 && booked_value == 0.0) {
        dialog = gtk_message_dialog_new(
            GTK_WINDOW(data->parent),
            GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
            GTK_MESSAGE_INFO,
            GTK_BUTTONS_OK,
            "%s",
            /* Translators: Strings from this file are needed only in
             * countries that have one of aqbanking's Online Banking
             * techniques available. This is 'OFX DirectConnect'
             * (U.S. and others), 'HBCI' (in Germany), or 'YellowNet'
             * (Switzerland). If none of these techniques are available
             * in your country, you may safely ignore strings from the
             * import-export/hbci subdirectory. */
            _("The downloaded Online Banking Balance was zero.\n\n"
              "Either this is the correct balance, or your bank does not "
              "support Balance download in this Online Banking version. "
              "In the latter case you should choose a different "
              "Online Banking version number in the Online Banking "
              "(AqBanking or HBCI) Setup. After that, try again to "
              "download the Online Banking Balance."));
        gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);

    } else {
        gnc_numeric reconc_balance = xaccAccountGetReconciledBalance(gnc_acc);

        gchar *booked_str = gnc_AB_VALUE_to_readable_string(booked_val);
        gchar *message1 = g_strdup_printf(
            _("Result of Online Banking job: \n"
              "Account booked balance is %s"),
            booked_str);
        gchar *message2 =
            (noted_value == 0.0) ?
            g_strdup("") :
            g_strdup_printf(_("For your information: This account also "
                              "has a noted balance of %s\n"),
                            gnc_AB_VALUE_to_readable_string(noted_val));

        if (gnc_numeric_equal(value, reconc_balance)) {
            const gchar *message3 =
                _("The booked balance is identical to the current "
                  "reconciled balance of the account.");
            dialog = gtk_message_dialog_new(
                GTK_WINDOW(data->parent),
                GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                GTK_MESSAGE_INFO,
                GTK_BUTTONS_OK,
                "%s\n%s\n%s",
                message1, message2, message3);
            gtk_dialog_run(GTK_DIALOG(dialog));
            gtk_widget_destroy(GTK_WIDGET(dialog));

        } else {
            const char *message3 = _("Reconcile account now?");

            show_recn_window = gnc_verify_dialog(data->parent,TRUE, "%s\n%s\n%s",
                                                 message1, message2, message3);
        }
        g_free(booked_str);
        g_free(message1);
        g_free(message2);
    }

    /* Show reconciliation window */
    if (show_recn_window)
        recnWindowWithBalance(data->parent, gnc_acc, value, booked_tt);

    return NULL;
}

GncABImExContextImport *
gnc_ab_import_context(AB_IMEXPORTER_CONTEXT *context,
                      guint awaiting, gboolean execute_txns,
                      AB_BANKING *api, GtkWidget *parent)
{
    GncABImExContextImport *data = g_new(GncABImExContextImport, 1);

    g_return_val_if_fail(context, NULL);
    /* Do not await and ignore at the same time */
    g_return_val_if_fail(!(awaiting & AWAIT_BALANCES)
                         || !(awaiting & IGNORE_BALANCES),
                         NULL);
    g_return_val_if_fail(!(awaiting & AWAIT_TRANSACTIONS)
                         || !(awaiting & IGNORE_TRANSACTIONS),
                         NULL);
    /* execute_txns must be FALSE if txns are not awaited */
    g_return_val_if_fail(awaiting & AWAIT_TRANSACTIONS || !execute_txns, NULL);
    /* An api is needed for the jobs */
    g_return_val_if_fail(!execute_txns || api, NULL);

    data->awaiting = awaiting;
    data->txn_found = FALSE;
    data->execute_txns = execute_txns;
    data->api = api;
    data->parent = parent;
    data->job_list = NULL;
    data->generic_importer = NULL;

    /* Import transactions */
    if (!(awaiting & IGNORE_TRANSACTIONS))
        AB_ImExporterContext_AccountInfoForEach(context, txn_accountinfo_cb,
                                                data);

    /* Check balances */
    if (!(awaiting & IGNORE_BALANCES))
        AB_ImExporterContext_AccountInfoForEach(context, bal_accountinfo_cb,
                                                data);

    return data;
}

guint
gnc_ab_ieci_get_found(GncABImExContextImport *ieci)
{
    g_return_val_if_fail(ieci, 0);

    return ieci->awaiting;
}

AB_JOB_LIST2 *
gnc_ab_ieci_get_job_list(GncABImExContextImport *ieci)
{
    g_return_val_if_fail(ieci, NULL);

    return ieci->job_list;
}

gboolean
gnc_ab_ieci_run_matcher(GncABImExContextImport *ieci)
{
    g_return_val_if_fail(ieci, FALSE);

    return gnc_gen_trans_list_run(ieci->generic_importer);
}
