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
    GncABImExContextImport *ieci = NULL;

    g_return_if_fail(parent && gnc_acc);

    /* Get the API */
    api = gnc_AB_BANKING_new();
    if (!api)
    {
        g_warning("gnc_ab_gettrans: Couldn't get AqBanking API");
        return;
    }
    if (AB_Banking_OnlineInit(api
#ifdef AQBANKING_VERSION_4_PLUS
                              , 0
#endif
                             ) != 0)
    {
        g_warning("gnc_ab_gettrans: Couldn't initialize AqBanking API");
        goto cleanup;
    }
    online = TRUE;

    /* Get the AqBanking Account */
    ab_acc = gnc_ab_get_ab_account(api, gnc_acc);
    if (!ab_acc)
    {
        g_warning("gnc_ab_getbalance: No AqBanking account found");
        goto cleanup;
    }

    /* Get a GetBalance job and enqueue it */
    job = AB_JobGetBalance_new(ab_acc);
    if (!job || AB_Job_CheckAvailability(job, 0))
    {
        g_warning("gnc_ab_getbalance: JobGetBalance not available for this "
                  "account");
        goto cleanup;
    }
    job_list = AB_Job_List2_new();
    AB_Job_List2_PushBack(job_list, job);

    /* Get a GUI object */
    gui = gnc_GWEN_Gui_get(parent);
    if (!gui)
    {
        g_warning("gnc_ab_getbalance: Couldn't initialize Gwenhywfar GUI");
        goto cleanup;
    }

    /* Create a context to store the results */
    context = AB_ImExporterContext_new();

    /* Execute the job */
    if (AB_Banking_ExecuteJobs(api, job_list, context, 0))
    {
        g_warning("gnc_ab_getbalance: Error on executing job");
        goto cleanup;
    }

    /* Import the results */
    ieci = gnc_ab_import_context(context, AWAIT_BALANCES, FALSE, NULL, parent);

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
    if (online)
#ifdef AQBANKING_VERSION_4_PLUS
        AB_Banking_OnlineFini(api, 0);
#else
        AB_Banking_OnlineFini(api);
#endif
    gnc_AB_BANKING_fini(api);
}
