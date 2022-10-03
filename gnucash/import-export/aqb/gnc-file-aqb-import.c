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

# include <gwenhywfar/syncio_file.h>
# include <gwenhywfar/syncio_buffered.h>
typedef GWEN_SYNCIO GWEN_IO_LAYER;

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

void
gnc_file_aqbanking_import(GtkWindow *parent,
                          const gchar *aqbanking_importername,
                          const gchar *aqbanking_profilename,
                          gboolean execute_transactions)
{
    gchar *default_dir;
    gchar *selected_filename = NULL;
    gint dtaus_fd = -1;
    AB_BANKING *api = NULL;
    gboolean online = FALSE;
    GncGWENGui *gui = NULL;
    GWEN_DB_NODE *db_profiles = NULL;
    GWEN_DB_NODE *db_profile;
    AB_IMEXPORTER_CONTEXT *context = NULL;
    GWEN_IO_LAYER *io = NULL;
    GncABImExContextImport *ieci = NULL;
    GNC_AB_JOB_LIST2 *job_list = NULL;
    GNC_AB_JOB_LIST2_ITERATOR *jit;
    GNC_AB_JOB *job;
    GNC_AB_JOB_STATUS job_status;
    gboolean successful = TRUE;
    int num_jobs = 0;
    int num_jobs_failed = 0;
    int max_failures = 5;
    GString *errstr = NULL;

    /* Select a file */
    default_dir = gnc_get_default_directory(GNC_PREFS_GROUP_AQBANKING);
    selected_filename = gnc_file_dialog(parent, _("Select a file to import"),
                                        NULL, default_dir,
                                        GNC_FILE_DIALOG_IMPORT);
    g_free(default_dir);

    if (!selected_filename)
        goto cleanup;
    DEBUG("filename: %s", selected_filename);

    /* Remember the directory as the default */
    default_dir = g_path_get_dirname(selected_filename);
    gnc_set_default_directory(GNC_PREFS_GROUP_AQBANKING, default_dir);
    g_free(default_dir);

    /* Get the API */
    api = gnc_AB_BANKING_new();
    if (!api)
    {
        g_warning("gnc_file_aqbanking_import: Couldn't get AqBanking API");
        goto cleanup;
    }

    /* Create a context to store the results */
    context = AB_ImExporterContext_new();

    if (AB_Banking_ImportFromFileLoadProfile(api, aqbanking_importername,
                                             context, aqbanking_profilename,
                                             NULL, selected_filename) < 0)
    {
        g_warning("gnc_file_aqbanking_import: Error on import");
        goto cleanup;
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
    {
        if (gnc_ab_ieci_run_matcher(ieci))
        {
            AB_IMEXPORTER_CONTEXT *execution_context;

            /* Extract the list of jobs */
            job_list = gnc_ab_ieci_get_job_list(ieci);

            /* Create a context to store possible results */
            execution_context = AB_ImExporterContext_new();

            /* Get a GUI object */
            gui = gnc_GWEN_Gui_get(NULL);
            if (!gui)
            {
                g_warning("gnc_file_aqbanking_import: Couldn't initialize Gwenhywfar GUI");
                goto cleanup;
            }

            /* And execute the jobs */
            AB_Banking_SendCommands(api, job_list, execution_context);

            /* Ignore the return value of AB_Banking_ExecuteJobs(), as the job's
             * status always describes better whether the job was actually
             * transferred to and accepted by the bank.  See also
             * https://lists.gnucash.org/pipermail/gnucash-de/2008-September/006389.html
             */

            /* So we must go through all jobs and check AB_Job_GetStatus(job)
             * to give the appropriate feedback if any of the jobs didn't
             * work. */

            jit = AB_Transaction_List2_First(job_list);
            if (jit)
            {
                job = AB_Transaction_List2Iterator_Data(jit);

                while (job)
                {
                    num_jobs += 1;
                    job_status = AB_Transaction_GetStatus(job);
                    if (job_status != AB_Transaction_StatusAccepted &&
                        job_status != AB_Transaction_StatusPending)
                    {
                        successful = FALSE;
                        num_jobs_failed += 1;

                        if (num_jobs_failed <= max_failures)
                        {
                            gchar *fmt_str =_("Job %d status %d - %s\n");
                            if (num_jobs_failed == 1)
                            {
                                errstr = g_string_new("Failed jobs:\n");
                            }
                            g_string_append_printf(errstr, fmt_str, num_jobs,
                                                   job_status,
                                                   AB_Transaction_Status_toString(job_status));
                   }
                        else
                        {
                            if (num_jobs_failed == (max_failures + 1) )
                            {
                                /* indicate that additional failures exist */
                                g_string_append(errstr, _("...\n"));
                            }
                        }
                    }
                    job = AB_Transaction_List2Iterator_Next(jit);
                } /* while */
                AB_Transaction_List2Iterator_free(jit);
            }

            if (!successful)
            {
                g_warning("%s", errstr->str);
                gnc_error_dialog(parent,
                                 _("An error occurred while executing jobs: %d of %d failed. "
                                   "Please check the log window or gnucash.trace for the exact "
                                   "error message.\n\n%s")
                                 , num_jobs_failed, num_jobs, errstr->str);
            }
            else
            {
                if (num_jobs == 0)
                {
                    gnc_info_dialog(parent,
                                    _("No jobs to be sent.")
                                   );
                }
                else
                {
                    gnc_info_dialog(parent, ngettext
                                    ("The job was executed successfully, but as a precaution "
                                     "please check the log window for potential errors.",
                                     "All %d jobs were executed successfully, but as a precaution "
                                     "please check the log window for potential errors.",
                                     num_jobs), num_jobs);
                }
            }
            AB_ImExporterContext_free(execution_context);
        }
    }

cleanup:
    if (job_list)
        AB_Transaction_List2_freeAll(job_list);
    if (ieci)
        g_free(ieci);
    if (context)
        AB_ImExporterContext_free(context);
    if (gui)
        gnc_GWEN_Gui_release(gui);
    if (api)
        gnc_AB_BANKING_fini(api);
    if (selected_filename)
        g_free(selected_filename);
    if (errstr)
        g_string_free(errstr, TRUE);
}
