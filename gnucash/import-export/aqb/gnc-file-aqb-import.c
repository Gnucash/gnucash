/*
 * gnc-file-aqb-import.c --
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
 * @file gnc-file-aqb-import.c
 * @brief DTAUS import module code
 * @author Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>
 * @author Copyright (C) 2003 Jan-Pascal van Best <janpascal@vanbest.org>
 * @author Copyright (C) 2006 Florian Steinel
 * @author Copyright (C) 2006 Christian Stimming
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 * @author Copyright (C) 2022 John Ralls <jralls@ceridwen.us>
 */

#include <config.h>

#include <platform.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif

#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <fcntl.h>
#include <unistd.h>

#include "gnc-ab-utils.h"

#include <gwenhywfar/syncio_file.h>
#include <gwenhywfar/syncio_buffered.h>
#include <gwenhywfar/gui.h>
typedef GWEN_SYNCIO GWEN_IO_LAYER;

#include "dialog-ab-select-imexporter.h"
#include "dialog-ab-trans.h"
#include "dialog-utils.h"
#include "gnc-file.h"
#include "gnc-file-aqb-import.h"
#include "gnc-gwen-gui.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "import-account-matcher.h"
#include "import-main-matcher.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_IMPORT;

static AB_IMEXPORTER_CONTEXT*
named_import_get_context (GtkWindow *parent, AB_BANKING *api,
                          const gchar *aqbanking_importername,
                          const gchar *aqbanking_profilename)
{
    AB_IMEXPORTER_CONTEXT *context;
    int success;
    /* Select a file */
    char *default_dir = gnc_get_default_directory(GNC_PREFS_GROUP_AQBANKING);
    char *selected_filename =
        gnc_file_dialog(parent, _("Select a file to import"),
                        NULL, default_dir, GNC_FILE_DIALOG_IMPORT);
    g_free(default_dir);

    if (!selected_filename)
        return NULL;
    DEBUG("filename: %s", selected_filename);

    /* Remember the directory as the default */
    default_dir = g_path_get_dirname(selected_filename);
    gnc_set_default_directory(GNC_PREFS_GROUP_AQBANKING, default_dir);
    g_free(default_dir);

/* Create a context to store the results */
    context = AB_ImExporterContext_new();
    success =
        AB_Banking_ImportFromFileLoadProfile(api, aqbanking_importername,
                                             context, aqbanking_profilename,
                                             NULL, selected_filename);
    g_free (selected_filename);
    if (success < 0)
    {
        AB_ImExporterContext_free(context);
        g_warning("gnc_file_aqbanking_import: Error on import");
        return NULL;
    }
    return context;
}

static void
report_failure (GtkWindow* parent, int num_jobs,
                int num_jobs_failed, GString* errstr)
{
    g_warning("%s", errstr->str);
    gnc_error_dialog(parent,
                     _("An error occurred while executing jobs: %d of %d failed. "
                       "Please check the log window or gnucash.trace for the exact "
                       "error message.\n\n%s")
                     , num_jobs_failed, num_jobs, errstr->str);
}

static void
report_no_jobs (GtkWindow *parent)
{
    gnc_info_dialog(parent,
                    _("No jobs to be sent.")
        );
}

static void
report_success (GtkWindow *parent, int num_jobs)
{
    gnc_info_dialog(parent, ngettext
                    ("The job was executed successfully, but as a precaution "
                     "please check the log window for potential errors.",
                     "All %d jobs were executed successfully, but as a precaution "
                     "please check the log window for potential errors.",
                     num_jobs), num_jobs);
}

static gboolean
check_job_status (GNC_AB_JOB_LIST2 *job_list, AB_BANKING *api, int *num_jobs,
                  int *num_jobs_failed, GString **errstr)
{
    static const int max_failures = 5;
    gboolean successful = TRUE;
    GNC_AB_JOB_LIST2_ITERATOR *jit;
    jit = AB_Transaction_List2_First(job_list);
    AB_Transaction_List2_freeAll(job_list);
    if (jit)
    {
        for (GNC_AB_JOB *job = AB_Transaction_List2Iterator_Data(jit);
             job != NULL;
             job = AB_Transaction_List2Iterator_Next(jit))
        {
            GNC_AB_JOB_STATUS job_status;
            *num_jobs += 1;
            job_status = AB_Transaction_GetStatus(job);
            if (job_status != AB_Transaction_StatusAccepted &&
                job_status != AB_Transaction_StatusPending)
            {
                successful = FALSE;
                *num_jobs_failed += 1;

                if (*num_jobs_failed <= max_failures)
                {
                    gchar *fmt_str =_("Job %d status %d - %s\n");
                    if (*num_jobs_failed == 1)
                    {
                        *errstr = g_string_new("Failed jobs:\n");
                    }
                    g_string_append_printf(*errstr, fmt_str, *num_jobs,
                                           job_status,
                                           AB_Transaction_Status_toString(job_status));
                }
                else
                {
                    if (*num_jobs_failed == (max_failures + 1) )
                    {
                        /* indicate that additional failures exist */
                        g_string_append(*errstr, _("...\n"));
                    }
                }
            }

        } /* while */
        AB_Transaction_List2Iterator_free(jit);
    }
    return successful;
}

static void
do_execute_transactions (GtkWindow *parent, AB_BANKING *api,
                         GncABImExContextImport *ieci)
{
    GString *errstr = NULL;
    if (!gnc_ab_ieci_run_matcher(ieci))
    {
        return;
    }
    else
    {
        int num_jobs = 0;
        int num_jobs_failed = 0;
        /* Extract the list of jobs */
        GNC_AB_JOB_LIST2 *job_list = gnc_ab_ieci_get_job_list(ieci);
        /* Create a context to store possible results */
        AB_IMEXPORTER_CONTEXT *execution_context =
            AB_ImExporterContext_new();
        /* Execute the jobs */
        AB_Banking_SendCommands(api, job_list, execution_context);
        AB_ImExporterContext_free(execution_context);

        /* Ignore the return value of AB_Banking_ExecuteJobs(), as the job's
         * status always describes better whether the job was actually
         * transferred to and accepted by the bank.  See also
         * https://lists.gnucash.org/pipermail/gnucash-de/2008-September/006389.html
         * So we must go through all jobs and check AB_Job_GetStatus(job)
         * to give the appropriate feedback if any of the jobs didn't
         * work.
         */
        if (check_job_status (job_list, api, &num_jobs,
                              &num_jobs_failed, &errstr))
        {
            if (num_jobs == 0)
                report_no_jobs (parent);
            else
                report_success (parent, num_jobs);
        }
        else
        {
            report_failure (parent, num_jobs, num_jobs_failed, errstr);
        }

        if (errstr)
            g_string_free(errstr, TRUE);

    }
}

void
gnc_file_aqbanking_import(GtkWindow *parent,
                          const gchar *aqbanking_importername,
                          const gchar *aqbanking_profilename,
                          gboolean execute_transactions)
{
    gint dtaus_fd = -1;
    AB_BANKING *api = NULL;
    AB_IMEXPORTER_CONTEXT *context = NULL;
    GncABImExContextImport *ieci = NULL;

    /* Get the API */
    api = gnc_AB_BANKING_new();
    if (!api)
    {
        g_warning("gnc_file_aqbanking_import: Couldn't get AqBanking API");
        return;
    }

    context = named_import_get_context (parent, api, aqbanking_importername,
                                        aqbanking_profilename);
    if (!context)
    {
        gnc_AB_BANKING_fini(api);
        return;
    }

    /* Before importing the results, if this is a new book, let user specify
     * book options, since they affect how transactions are created */
    if (gnc_is_new_book())
        gnc_new_book_option_display (GTK_WIDGET (parent));

    /* Import the results */
    ieci = gnc_ab_import_context(context, AWAIT_TRANSACTIONS,
                                 execute_transactions,
                                 execute_transactions ? api : NULL,
                                 GTK_WIDGET(parent));

    if (execute_transactions)
        do_execute_transactions (parent, api, ieci);

    g_free(ieci);
    AB_ImExporterContext_free(context);
    gnc_AB_BANKING_fini(api);
}

void
gnc_file_aqbanking_import_dialog (GtkWindow *parent)
{
     AB_BANKING* api = gnc_AB_BANKING_new ();
     GncABSelectImExDlg* imexd =
         gnc_ab_select_imex_dlg_new (GTK_WIDGET (parent), api);
     char *imexporter, *profile;
     AB_IMEXPORTER_CONTEXT* ctx = NULL;

     if (!imexd)
     {

         PERR ("Failed to create select imex dialog.");
         gnc_AB_BANKING_fini(api);
         return;
     }

     if (!gnc_ab_select_imex_dlg_run (imexd))
     {
         gnc_ab_select_imex_dlg_destroy (imexd);
         return;
     }

     imexporter = gnc_ab_select_imex_dlg_get_imexporter_name (imexd);
     profile = gnc_ab_select_imex_dlg_get_profile_name (imexd);

     if (imexporter && profile)
     {
         ctx = named_import_get_context (parent, api, imexporter, profile);
         gnc_ab_select_imex_dlg_destroy (imexd);

         if (ctx)
         {
             GncABImExContextImport* ieci = NULL;
             ieci = gnc_ab_import_context (ctx, 0, FALSE, api, GTK_WIDGET(parent));
             g_free(ieci);
             AB_ImExporterContext_free(ctx);
         }
         g_free (imexporter);
         g_free (profile);
     }

     gnc_AB_BANKING_fini(api);
}
