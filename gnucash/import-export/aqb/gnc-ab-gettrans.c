/*
 * gnc-ab-gettrans.c --
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
 * @file gnc-ab-gettrans.c
 * @brief AqBanking get transactions functions
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#include <config.h>

#include "gnc-ab-utils.h"

#include <glib/gi18n.h>
#include <aqbanking/banking.h>
#ifdef AQBANKING6
# include <aqbanking/types/transaction.h>
#else
# include <aqbanking/jobgettransactions.h>
#endif
#include "Account.h"
#include "dialog-ab-daterange.h"
#include "gnc-ab-gettrans.h"
#include "gnc-ab-kvp.h"
#include "gnc-gwen-gui.h"
#include "gnc-ui.h"

/* This static indicates the debugging module that this .o belongs to.  */
G_GNUC_UNUSED static QofLogModule log_module = G_LOG_DOMAIN;

static gboolean gettrans_dates(GtkWidget *parent, Account *gnc_acc, GWEN_TIME **from_date, GWEN_TIME **to_date);

static gboolean
gettrans_dates(GtkWidget *parent, Account *gnc_acc,
               GWEN_TIME **from_date, GWEN_TIME **to_date)
{
    time64 last, until;
    gboolean use_last_date = TRUE;
    gboolean use_earliest_date = TRUE;
    gboolean use_until_now = TRUE;

    g_return_val_if_fail(from_date && to_date, FALSE);

    /* Get time of last retrieval */
    last = gnc_ab_get_account_trans_retrieval(gnc_acc);
    if (last == 0)
    {
        use_last_date = FALSE;
        last = gnc_time (NULL);
    }
    until = gnc_time (NULL);

    /* Let the user choose the date range of retrieval */
    if (!gnc_ab_enter_daterange(parent, NULL,
                                &last,
                                &use_last_date, &use_earliest_date,
                                &until, &use_until_now))
        return FALSE;

    /* Now calculate from date */
    if (use_earliest_date)
    {
        *from_date = NULL;
    }
    else
    {
        if (use_last_date)
            last = gnc_ab_get_account_trans_retrieval(gnc_acc);
        *from_date = GWEN_Time_fromSeconds(last);
    }

    /* Now calculate to date */
    if (use_until_now)
        until = gnc_time (NULL);
    *to_date = GWEN_Time_fromSeconds(until);

    return TRUE;
}

void
gnc_ab_gettrans(GtkWidget *parent, Account *gnc_acc)
{
    AB_BANKING *api;
    gboolean online = FALSE;
    GNC_AB_ACCOUNT_SPEC *ab_acc;
    GWEN_TIME *from_date = NULL, *to_date = NULL;
    time64 until;
    GNC_AB_JOB *job = NULL;
    GNC_AB_JOB_LIST2 *job_list = NULL;
    GncGWENGui *gui = NULL;
    AB_IMEXPORTER_CONTEXT *context = NULL;
    GncABImExContextImport *ieci = NULL;
    GNC_AB_JOB_STATUS job_status;

    g_return_if_fail(parent && gnc_acc);

    /* Get the API */
    api = gnc_AB_BANKING_new();
    if (!api)
    {
        g_warning("gnc_ab_gettrans: Couldn't get AqBanking API");
        return;
    }
#ifndef AQBANKING6
    if (AB_Banking_OnlineInit(api) != 0)
    {
        g_warning("gnc_ab_gettrans: Couldn't initialize AqBanking API");
        goto cleanup;
    }
    online = TRUE;
#endif
    /* Get the AqBanking Account */
    ab_acc = gnc_ab_get_ab_account(api, gnc_acc);
    if (!ab_acc)
    {
        g_warning("gnc_ab_gettrans: No AqBanking account found");
        gnc_error_dialog (GTK_WINDOW (parent), _("No valid online banking account assigned."));
        goto cleanup;
    }

    /* Get the start and end dates for the GetTransactions job.  */
    if (!gettrans_dates(parent, gnc_acc, &from_date, &to_date))
    {
        g_debug("gnc_ab_gettrans: gettrans_dates aborted");
        goto cleanup;
    }
    /* Use this as a local storage for the until_time below. */
    until = GWEN_Time_toTime_t(to_date);

    /* Get a GetTransactions job and enqueue it */
#ifdef AQBANKING6
    if (!AB_AccountSpec_GetTransactionLimitsForCommand(ab_acc, AB_Transaction_CommandGetTransactions))
#else
    job = AB_JobGetTransactions_new(ab_acc);
    if (!job || AB_Job_CheckAvailability(job))
#endif
    {
        g_warning("gnc_ab_gettrans: JobGetTransactions not available for this "
                  "account");
        gnc_error_dialog (GTK_WINDOW (parent), _("Online action \"Get Transactions\" not available for this account."));
        goto cleanup;
    }
#ifdef AQBANKING6
    job = AB_Transaction_new();
    AB_Transaction_SetCommand(job, AB_Transaction_CommandGetTransactions);
    AB_Transaction_SetUniqueAccountId(job, AB_AccountSpec_GetUniqueId(ab_acc));

    if (from_date) /* TODO: this should be simplified */
    {
        GWEN_DATE *dt;

        dt=GWEN_Date_fromLocalTime(GWEN_Time_toTime_t(from_date));
        AB_Transaction_SetFirstDate(job, dt);
        GWEN_Date_free(dt);
    }

    if (to_date)
    {
        GWEN_DATE *dt;

        dt=GWEN_Date_fromLocalTime(GWEN_Time_toTime_t(to_date));
        AB_Transaction_SetLastDate(job, dt);
        GWEN_Date_free(dt);
    }

    job_list = AB_Transaction_List2_new();
    AB_Transaction_List2_PushBack(job_list, job);
#else
    AB_JobGetTransactions_SetFromTime(job, from_date);
    AB_JobGetTransactions_SetToTime(job, to_date);
    job_list = AB_Job_List2_new();
    AB_Job_List2_PushBack(job_list, job);
#endif
    /* Get a GUI object */
    gui = gnc_GWEN_Gui_get(parent);
    if (!gui)
    {
        g_warning("gnc_ab_gettrans: Couldn't initialize Gwenhywfar GUI");
        goto cleanup;
    }

    /* Create a context to store the results */
    context = AB_ImExporterContext_new();

    /* Execute the job */
#ifdef AQBANKING6
    AB_Banking_SendCommands(api, job_list, context);
#else
    AB_Banking_ExecuteJobs(api, job_list, context);
#endif
    /* Ignore the return value of AB_Banking_ExecuteJobs(), as the job's
     * status always describes better whether the job was actually
     * transferred to and accepted by the bank.  See also
     * https://lists.gnucash.org/pipermail/gnucash-de/2008-September/006389.html
     */
#ifdef AQBANKING6
    job_status = AB_Transaction_GetStatus(job);
    if (job_status != AB_Transaction_StatusAccepted
            && job_status != AB_Transaction_StatusPending)
#else
    job_status = AB_Job_GetStatus(job);
    if (job_status != AB_Job_StatusFinished
            && job_status != AB_Job_StatusPending)
#endif
    {
        g_warning("gnc_ab_gettrans: Error on executing job");
#ifdef AQBANKING6
        gnc_error_dialog (GTK_WINDOW (parent),
                          _("Error on executing job.\n\nStatus: %s (%d)"),
                          AB_Transaction_Status_toString(job_status),
                          job_status);
#else
        gnc_error_dialog (GTK_WINDOW (parent),
                          _("Error on executing job.\n\nStatus: %s - %s"),
                          AB_Job_Status2Char(job_status),
                          AB_Job_GetResultText(job));
#endif
        goto cleanup;
    }

    /* Import the results */
    ieci = gnc_ab_import_context(context, AWAIT_TRANSACTIONS, FALSE, NULL,
                                 parent);
    if (!(gnc_ab_ieci_get_found(ieci) & FOUND_TRANSACTIONS))
    {
        /* No transaction found */
        GtkWidget *dialog = gtk_message_dialog_new(
                                GTK_WINDOW(parent),
                                GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                                GTK_MESSAGE_INFO,
                                GTK_BUTTONS_OK,
                                "%s",
                                _("The Online Banking import returned no transactions "
                                  "for the selected time period."));
        gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);
    }

    /* Store the date of this retrieval */
    gnc_ab_set_account_trans_retrieval(gnc_acc, until);

cleanup:
    if (ieci)
        g_free(ieci);
    if (context)
        AB_ImExporterContext_free(context);
    if (gui)
        gnc_GWEN_Gui_release(gui);
    if (job_list)
#ifdef AQBANKING6
        AB_Transaction_List2_free(job_list);
#else
        AB_Job_List2_free(job_list);
#endif
    if (job)
#ifdef AQBANKING6
        AB_Transaction_free(job);
#else
        AB_Job_free(job);
#endif
    if (to_date)
        GWEN_Time_free(to_date);
    if (from_date)
        GWEN_Time_free(from_date);
#ifndef AQBANKING6
    if (online)
        AB_Banking_OnlineFini(api);
#endif
    gnc_AB_BANKING_fini(api);
}
