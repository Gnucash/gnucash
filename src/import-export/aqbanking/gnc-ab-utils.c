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

#include "gnc-ab-kvp.h"
#include "gnc-ab-utils.h"
#include "gnc-glib-utils.h"
#include "gnc-gwen-gui.h"
#include "qof.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

/* Global variables for AB_BANKING caching. */
static AB_BANKING *gnc_AB_BANKING = NULL;
static gint gnc_AB_BANKING_refcount = 0;

static gpointer join_ab_strings_cb(const gchar *str, gpointer user_data);

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

    ab_remote_name = AB_Transaction_GetPurpose(ab_trans);
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

AB_ACCOUNT_STATUS *
gnc_ab_get_best_accountstatus(AB_IMEXPORTER_ACCOUNTINFO *acc_info)
{
    AB_ACCOUNT_STATUS *item, *best = NULL;
    const GWEN_TIME *best_time;

    g_return_val_if_fail(acc_info, NULL);

    item = AB_ImExporterAccountInfo_GetFirstAccountStatus(acc_info);
    while (item) {
        const GWEN_TIME *item_time = AB_AccountStatus_GetTime(item);
        if (!best || GWEN_Time_Diff(best_time, item_time) < 0.0) {
            best = item;
            best_time = item_time;
        }
        item = AB_ImExporterAccountInfo_GetNextAccountStatus(acc_info);
    }
    return best;
}
