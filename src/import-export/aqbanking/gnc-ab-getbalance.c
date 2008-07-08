/*
 * gnc-ab-getbalance.c --
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
 * @file gnc-ab-getbalance.c
 * @brief AqBanking getbalance functions
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#include "config.h"

#include <glib/gi18n.h>
#include <aqbanking/banking.h>
#include <aqbanking/jobgetbalance.h>

#include "gnc-ab-getbalance.h"
#include "gnc-ab-kvp.h"
#include "gnc-ab-utils.h"
#include "gnc-gwen-gui.h"
#include "gnc-ui.h"
#include "RecnWindow.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

void
gnc_ab_getbalance(GtkWidget *parent, Account *gnc_acc)
{
    AB_BANKING *api;
    gboolean online = FALSE;
    AB_ACCOUNT *ab_acc;
    AB_JOB *job = NULL;
    AB_JOB_LIST2 *job_list = NULL;
    GncGWENGui *gui = NULL;
    AB_IMEXPORTER_CONTEXT *context = NULL;
    AB_IMEXPORTER_ACCOUNTINFO *acc_info = NULL;
    AB_ACCOUNT_STATUS *status = NULL;
    const AB_BALANCE *booked_bal, *noted_bal;
    const AB_VALUE *booked_val = NULL, *noted_val = NULL;
    gdouble booked_value, noted_value;
    gnc_numeric value;
    time_t booked_tt = 0;
    GtkWidget *dialog;
    gboolean show_recn_window = FALSE;

    g_return_if_fail(parent && gnc_acc);

    /* Get the API */
    api = gnc_AB_BANKING_new();
    if (!api) {
        g_warning("gnc_ab_gettrans: Couldn't get AqBanking API");
        return;
    }
    if (AB_Banking_OnlineInit(api) != 0) {
        g_warning("gnc_ab_gettrans: Couldn't initialize AqBanking API");
        goto cleanup;
    }
    online = TRUE;

    /* Get the AqBanking Account */
    ab_acc = gnc_ab_get_ab_account(api, gnc_acc);
    if (!ab_acc) {
        g_warning("gnc_ab_getbalance: No AqBanking account found");
        goto cleanup;
    }

    /* Get a GetBalance job and enqueue it */
    job = AB_JobGetBalance_new(ab_acc);
    if (!job || AB_Job_CheckAvailability(job, 0)) {
        g_warning("gnc_ab_getbalance: JobGetBalance not available for this "
                  "account");
        goto cleanup;
    }
    job_list = AB_Job_List2_new();
    AB_Job_List2_PushBack(job_list, job);

    /* Get a GUI object */
    gui = gnc_GWEN_Gui_get(parent);
    if (!gui) {
        g_warning("gnc_ab_getbalance: Couldn't initialize Gwenhywfar GUI");
        goto cleanup;
    }

    /* Create a context to store the results */
    context = AB_ImExporterContext_new();

    /* Execute the job */
    if (AB_Banking_ExecuteJobs(api, job_list, context, 0)) {
        g_warning("gnc_ab_getbalance: Error on executing job");
        goto cleanup;
    }

    /* Lookup account in context */
    acc_info = AB_ImExporterContext_FindAccountInfo(
        context, gnc_ab_get_account_bankcode(gnc_acc),
        gnc_ab_get_account_accountid(gnc_acc));
    if (!acc_info) {
        g_warning("gnc_ab_getbalance: No accountinfo result for this account");
        goto cleanup;
    }

    /* Lookup newest data */
    status = gnc_ab_get_best_accountstatus(acc_info);
    if (!status) {
        g_warning("gnc_ab_getbalance: No accountstatus result for this account");
        goto cleanup;
    }

    /* Lookup booked balance and time */
    booked_bal = AB_AccountStatus_GetBookedBalance(status);
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
            g_warning("gnc_ab_getbalance: booked_val == NULL.  Assuming 0");
            booked_value = 0.0;
        }
    } else {
        g_warning("gnc_ab_getbalance: booked_bal == NULL.  Assuming 0");
        booked_tt = 0;
        booked_value = 0.0;
    }

    /* Lookup noted balance */
    noted_bal = AB_AccountStatus_GetNotedBalance(status);
    if (noted_bal) {
        noted_val = AB_Balance_GetValue(noted_bal);
        if (noted_val)
            noted_value = AB_Value_GetValueAsDouble(noted_val);
        else {
            g_warning("gnc_ab_getbalance: noted_val == NULL.  Assuming 0");
            noted_value = 0.0;
        }
    } else {
        g_warning("gnc_ab_getbalance: noted_bal == NULL.  Assuming 0");
        noted_value = 0.0;
    }

    value = double_to_gnc_numeric(booked_value,
                                  xaccAccountGetCommoditySCU(gnc_acc),
                                  GNC_RND_ROUND);
    if (noted_value == 0.0 && booked_value == 0.0) {
        dialog = gtk_message_dialog_new(
            GTK_WINDOW(parent),
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
                GTK_WINDOW(parent),
                GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                GTK_MESSAGE_INFO,
                GTK_BUTTONS_OK,
                "%s\n%s\n%s",
                message1, message2, message3);
            gtk_dialog_run(GTK_DIALOG(dialog));
            gtk_widget_destroy(GTK_WIDGET(dialog));

        } else {
            const char *message3 = _("Reconcile account now?");

            show_recn_window = gnc_verify_dialog(parent,TRUE, "%s\n%s\n%s",
                                                 message1, message2, message3);
        }
        g_free(booked_str);
        g_free(message1);
        g_free(message2);
    }

    /* Show reconciliation window */
    if (show_recn_window)
        recnWindowWithBalance(parent, gnc_acc, value, booked_tt);

cleanup:
    if (context)
        AB_ImExporterContext_free(context);
    if (gui)
        gnc_GWEN_Gui_release(gui);
    if (job_list)
        AB_Job_List2_free(job_list);
    if (job)
        AB_Job_free(job);
    if (online)
        AB_Banking_OnlineFini(api);
    gnc_AB_BANKING_fini(api);
}
