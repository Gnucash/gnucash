/********************************************************************\
 * gnc-hbci-getbalance.c -- hbci getbalance functions               *
 * Copyright (C) 2002 Christian Stimming                            *
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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#define AQBANKING_NOWARN_DEPRECATED
#include <aqbanking/banking.h>

#include "RecnWindow.h"
#include "gnc-date.h"
#include "gnc-hbci-getbalance.h"
#include "gnc-hbci-utils.h"
#include "gnc-numeric.h"
#include "gnc-ui.h"
#include "hbci-interaction.h"


void gnc_hbci_getbalance_debugprint(AB_JOB *balance_job,
                                    const AB_ACCOUNT *h_acc);

#if 0
static void
bal_print_debug(const char *name,
                const AB_VALUE *val,
                gboolean negative,
                const char *date_str,
                const char *time_str)
{
    char *str = gnc_AB_VALUE_toReadableString (val);
    g_message("GetBalance: %s%s %s at date %s %s",
              (negative ? "-" : ""), str,
              name, date_str, time_str);
    free (str);
}
#endif


void
gnc_hbci_getbalance (GtkWidget *parent, Account *gnc_acc)
{
    AB_BANKING *api = NULL;
    const AB_ACCOUNT *h_acc = NULL;
    GNCInteractor *interactor = NULL;

    g_assert(parent);
    if (gnc_acc == NULL)
        return;

    /* Get API */
    api = gnc_AB_BANKING_new_currentbook (parent, &interactor);
    if (api == NULL)
    {
        g_message("gnc_hbci_getbalance: Couldn't get AB_BANKING API.\n");
        return;
    }
    g_assert (interactor);

    /* Get HBCI account */
    h_acc = gnc_hbci_get_hbci_acc (api, gnc_acc);
    if (h_acc == NULL)
    {
        g_warning("gnc_hbci_getbalance: No HBCI account found.\n");
        /* FIXME: free unneeded data */
        return;
    }
    /* g_message("gnc_hbci_getbalance: HBCI account no. %s found.\n",
       AB_ACCOUNT_accountId (h_acc)); */

    {
        /* Execute a GetBalance job. */
        AB_JOB *job;

        job = AB_JobGetBalance_new((AB_ACCOUNT*)h_acc);
        if (AB_Job_CheckAvailability(job))
        {
            g_message("gnc_hbci_getbalance: JobGetBalance not available for this account.\n");
            /* FIXME: free unneeded data */
            return;
        }

        /* Add job to API queue */
        AB_Banking_EnqueueJob(api, job);

        /* Execute Outbox. */
        if (!gnc_AB_BANKING_execute (parent, api, job, interactor))
        {

            /* AB_BANKING_executeOutbox failed. */
            gnc_hbci_cleanup_job(api, job);
            /* FIXME: free unneeded data */
            return;
        }

        /* gnc_hbci_getbalance_debugprint(balance_job, h_acc); */

        /* Finish this job. */
        gnc_hbci_getbalance_finish (parent,
                                    gnc_acc,
                                    job);

        /* Clean up after ourselves. */
        gnc_hbci_cleanup_job(api, job);
        gnc_AB_BANKING_fini (api);
        GNCInteractor_hide (interactor);
    }
}


#if 0
void gnc_hbci_getbalance_debugprint(AB_JOB *job,
                                    const AB_ACCOUNT *h_acc)
{
    GWEN_DB_NODE *response, *acc_bal;
    GWEN_DB_NODE *noted_grp, *booked_grp;
    AB_VALUE *booked_val, *noted_val;
    /* time_t balance_tt, noted_tt, booked_tt; */

    response = HBCI_Job_responseData(AB_JOB_Job(job));
    if (!response)
        return;
    acc_bal = GWEN_DB_GetGroup(response,
                               GWEN_PATH_FLAGS_NAMEMUSTEXIST, "balance");
    if (!acc_bal)
        return;

    noted_grp = GWEN_DB_GetGroup(response, GWEN_PATH_FLAGS_NAMEMUSTEXIST, "noted");
    booked_grp = GWEN_DB_GetGroup(response, GWEN_PATH_FLAGS_NAMEMUSTEXIST, "booked");

    booked_val = AB_VALUE_new(GWEN_DB_GetCharValue(booked_grp, "value", 0, "0"),
                              GWEN_DB_GetCharValue(booked_grp, "currency", 0, "EUR"));
    noted_val = AB_VALUE_new(GWEN_DB_GetCharValue(noted_grp, "value", 0, "0"),
                             GWEN_DB_GetCharValue(noted_grp, "currency", 0, "EUR"));

    g_message("GetBalance: Balances for account %s :\n",
              AB_ACCOUNT_accountId (h_acc));
    bal_print_debug("Booked balance",
                    booked_val,
                    (strcasecmp(GWEN_DB_GetCharValue(booked_grp, "debitmark", 0, "C"), "D") == 0),
                    GWEN_DB_GetCharValue(booked_grp, "date", 0, ""),
                    GWEN_DB_GetCharValue(booked_grp, "time", 0, ""));
    bal_print_debug("Noted balance",
                    noted_val,
                    (strcasecmp(GWEN_DB_GetCharValue(noted_grp, "debitmark", 0, "C"), "D") == 0),
                    GWEN_DB_GetCharValue(noted_grp, "date", 0, ""),
                    GWEN_DB_GetCharValue(noted_grp, "time", 0, ""));
    /*   bal_print_debug("Bank Line",  */
    /* 		  AB_ACCOUNTBalance_bankLine (acc_bal), FALSE, */
    /* 		  balance_tt); */
    /*   bal_print_debug("Disposable amount", */
    /* 		  AB_ACCOUNTBalance_disposable (acc_bal), FALSE, */
    /* 		  balance_tt); */
    /*   bal_print_debug("Already disposed", */
    /* 		  AB_ACCOUNTBalance_disposed (acc_bal), FALSE, */
    /* 		  balance_tt); */
    AB_VALUE_delete(booked_val);
    AB_VALUE_delete(noted_val);
}
#endif

static gchar*
bal_print_balance(const char *format,
                  const AB_VALUE *val)
{
    char *str = gnc_AB_VALUE_toReadableString (val);
    char *res = g_strdup_printf(format,
                                str);
    free (str);
    return res;
}



gboolean
gnc_hbci_getbalance_finish (GtkWidget *parent,
                            Account *gnc_acc,
                            const AB_JOB *job)
{
    const AB_ACCOUNT_STATUS *response;
    const AB_BALANCE *noted_grp, *booked_grp;
    const AB_VALUE *booked_val, *noted_val;
    time_t booked_tt = 0;
    gboolean dialogres;
    double booked_value, noted_value;
    gnc_numeric value;
    GtkWidget *dialog;

    response = AB_JobGetBalance_GetAccountStatus((AB_JOB*)job);
    if (!response)
    {
        g_critical("gnc_hbci_getbalance_finish: Oops, response == NULL.\n");
        return TRUE;
    }

    noted_grp = AB_AccountStatus_GetNotedBalance(response);
    booked_grp = AB_AccountStatus_GetBookedBalance(response);

    if (booked_grp)
    {
        const GWEN_TIME *ti;

        ti = AB_Balance_GetTime(booked_grp);
        if (ti)
            booked_tt = GWEN_Time_toTime_t (ti);
        else
            /* No time found? Use today because the HBCI query asked for
            	 today's balance. */
            booked_tt = gnc_timet_get_day_start(time(NULL));
        booked_val = AB_Balance_GetValue(booked_grp);
        if (booked_val)
            booked_value = AB_Value_GetValue (booked_val);
        else
        {
            g_warning("gnc_hbci_getbalance_finish: Warning: booked_val == NULL. Assuming 0.\n");
            booked_value = 0.0;
        }
    }
    else
    {
        g_warning("gnc_hbci_getbalance_finish: Warning: booked_grp == NULL. Assuming 0.\n");
        booked_value = 0.0;
        booked_val = NULL;
        booked_tt = 0;
    }

    if (noted_grp)
    {
        noted_val = AB_Balance_GetValue(noted_grp);
        /* noted_tt = GWEN_Time_toTime_t (AB_Balance_GetTime(noted_grp)); */
        if (noted_val)
            noted_value = AB_Value_GetValue (noted_val);
        else
        {
            g_warning("gnc_hbci_getbalance_finish: Warning: noted_val == NULL. Assuming 0.\n");
            noted_value = 0.0;
        }
    }
    else
    {
        g_warning("gnc_hbci_getbalance_finish: Warning: noted_grp == NULL. Assuming 0.\n");
        noted_value = 0.0;
        noted_val = NULL;
    }

    value = double_to_gnc_numeric (booked_value,
                                   xaccAccountGetCommoditySCU(gnc_acc),
                                   GNC_RND_ROUND);
    if ((noted_value == 0.0) && (booked_value == 0.0))
    {
        dialog = gtk_message_dialog_new(GTK_WINDOW(parent),
                                        GTK_DIALOG_MODAL
                                        | GTK_DIALOG_DESTROY_WITH_PARENT,
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
        gtk_widget_destroy(GTK_WIDGET(dialog));
        dialogres = FALSE;
    }
    else
    {
        gnc_numeric reconc_balance = xaccAccountGetReconciledBalance (gnc_acc);

        char *booked_str = gnc_AB_VALUE_toReadableString (booked_val);
        char *message1 = g_strdup_printf
                         (
                             _("Result of Online Banking job: \n"
                               "Account booked balance is %s"),
                             booked_str);
        char *message2 =
            ((noted_value == 0.0) ?
             g_strdup_printf("%s", "") :
             bal_print_balance
             (_("For your information: This account also "
                "has a noted balance of %s\n"),
              noted_val));

        if (gnc_numeric_equal(value, reconc_balance))
        {
            const char *message3 = _("The booked balance is identical to the current "
                                     "reconciled balance of the account.");
            dialog = gtk_message_dialog_new(GTK_WINDOW(parent),
                                            GTK_DIALOG_MODAL
                                            | GTK_DIALOG_DESTROY_WITH_PARENT,
                                            GTK_MESSAGE_INFO,
                                            GTK_BUTTONS_OK,
                                            "%s\n%s\n%s",
                                            message1, message2, message3);
            gtk_dialog_run(GTK_DIALOG(dialog));
            gtk_widget_destroy(GTK_WIDGET(dialog));
            dialogres = FALSE;

        }
        else
        {
            const char *message3 = _("Reconcile account now?");

            dialogres = gnc_verify_dialog
                        (parent,
                         TRUE,
                         "%s%s\n%s",
                         message1, message2, message3);
        }
        g_free (message1);
        g_free (message2);
        free (booked_str);
    }


    if (dialogres)
    {
        recnWindowWithBalance (parent,
                               gnc_acc,
                               value,
                               booked_tt);
    }

    return TRUE;
}
