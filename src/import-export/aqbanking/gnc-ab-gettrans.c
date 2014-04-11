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

#include "config.h"

#include <glib/gi18n.h>
#include <aqbanking/banking.h>
#include <aqbanking/jobgettransactions.h>

#include "Account.h"
#include "dialog-daterange.h"
#include "gnc-ab-gettrans.h"
#include "gnc-ab-kvp.h"
#include "gnc-ab-utils.h"
#include "gnc-gwen-gui.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

static gboolean gettrans_dates(GtkWidget *parent, Account *gnc_acc, GWEN_TIME **from_date, GWEN_TIME **to_date);

static gboolean
gettrans_dates(GtkWidget *parent, Account *gnc_acc,
               GWEN_TIME **from_date, GWEN_TIME **to_date)
{
    Timespec last_timespec, until_timespec;
    time_t now = time(NULL);
    gboolean use_last_date = TRUE;
    gboolean use_earliest_date = TRUE;
    gboolean use_until_now = TRUE;

    g_return_val_if_fail(from_date && to_date, FALSE);

    /* Get time of last retrieval */
    last_timespec = gnc_ab_get_account_trans_retrieval(gnc_acc);
    if (last_timespec.tv_sec == 0) {
        use_last_date = FALSE;
        timespecFromTime_t(&last_timespec, now);
    }
    timespecFromTime_t(&until_timespec, now);

    /* Let the user choose the date range of retrieval */
    if (!gnc_ab_enter_daterange(parent, NULL,
                                &last_timespec,
                                &use_last_date, &use_earliest_date,
                                &until_timespec, &use_until_now))
        return FALSE;

    /* Now calculate from date */
    if (use_earliest_date) {
        *from_date = NULL;
    } else {
        if (use_last_date)
            last_timespec = gnc_ab_get_account_trans_retrieval(gnc_acc);
        *from_date = GWEN_Time_fromSeconds(timespecToTime_t(last_timespec));
    }

    /* Now calculate to date */
    if (use_until_now)
        timespecFromTime_t(&until_timespec, now);
    *to_date = GWEN_Time_fromSeconds(timespecToTime_t(until_timespec));

    return TRUE;
}

void
gnc_ab_gettrans(GtkWidget *parent, Account *gnc_acc)
{
    AB_BANKING *api;
    gboolean online = FALSE;
    AB_ACCOUNT *ab_acc;
    GWEN_TIME *from_date = NULL, *to_date = NULL;
    Timespec until_timespec;
    AB_JOB *job = NULL;
    AB_JOB_LIST2 *job_list = NULL;
    GncGWENGui *gui = NULL;
    AB_IMEXPORTER_CONTEXT *context = NULL;
    GncABImExContextImport *ieci = NULL;

    g_return_if_fail(parent && gnc_acc);

    /* Get the API */
    api = gnc_AB_BANKING_new();
    if (!api) {
        g_warning("gnc_ab_gettrans: Couldn't get AqBanking API");
        return;
    }
    if (AB_Banking_OnlineInit(api
#ifdef AQBANKING_VERSION_4_PLUS
			      , 0
#endif
			      ) != 0) {
        g_warning("gnc_ab_gettrans: Couldn't initialize AqBanking API");
        goto cleanup;
    }
    online = TRUE;

    /* Get the AqBanking Account */
    ab_acc = gnc_ab_get_ab_account(api, gnc_acc);
    if (!ab_acc) {
        g_warning("gnc_ab_gettrans: No AqBanking account found");
        goto cleanup;
    }

    /* Get the start and end dates for the GetTransactions job.  */
    if (!gettrans_dates(parent, gnc_acc, &from_date, &to_date)) {
        g_debug("gnc_ab_gettrans: gettrans_dates aborted");
        goto cleanup;
    }
    /* Use this as a local storage for the until_time below. */
    timespecFromTime_t(&until_timespec, GWEN_Time_toTime_t(to_date));

    /* Get a GetTransactions job and enqueue it */
    job = AB_JobGetTransactions_new(ab_acc);
    if (!job || AB_Job_CheckAvailability(job, 0)) {
        g_warning("gnc_ab_gettrans: JobGetTransactions not available for this "
                  "account");
        goto cleanup;
    }
    AB_JobGetTransactions_SetFromTime(job, from_date);
    AB_JobGetTransactions_SetToTime(job, to_date);
    job_list = AB_Job_List2_new();
    AB_Job_List2_PushBack(job_list, job);

    /* Get a GUI object */
    gui = gnc_GWEN_Gui_get(parent);
    if (!gui) {
        g_warning("gnc_ab_gettrans: Couldn't initialize Gwenhywfar GUI");
        goto cleanup;
    }

    /* Create a context to store the results */
    context = AB_ImExporterContext_new();

    /* Execute the job */
    if (AB_Banking_ExecuteJobs(api, job_list, context, 0)) {
        g_warning("gnc_ab_gettrans: Error on executing job");
        goto cleanup;
    }

    /* Import the results */
    ieci = gnc_ab_import_context(context, AWAIT_TRANSACTIONS, FALSE, NULL,
                                 parent);
    if (!(gnc_ab_ieci_get_found(ieci) & FOUND_TRANSACTIONS)) {
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
    gnc_ab_set_account_trans_retrieval(gnc_acc, until_timespec);

cleanup:
    if (ieci)
        g_free(ieci);
    if (context)
        AB_ImExporterContext_free(context);
    if (gui)
        gnc_GWEN_Gui_release(gui);
    if (job_list)
        AB_Job_List2_free(job_list);
    if (job)
        AB_Job_free(job);
    if (to_date)
        GWEN_Time_free(to_date);
    if (from_date)
        GWEN_Time_free(from_date);
    if (online)
#ifdef AQBANKING_VERSION_4_PLUS
	AB_Banking_OnlineFini(api, 0);
#else
	AB_Banking_OnlineFini(api);
#endif
    gnc_AB_BANKING_fini(api);
}
