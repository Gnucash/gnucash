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

#include "config.h"

#include <glib/gi18n.h>
#include <glib/gstdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <gwenhywfar/io_file.h>
#include <gwenhywfar/io_buffered.h>
#include <gwenhywfar/iomanager.h>

#include "dialog-ab-trans.h"
#include "gnc-ab-utils.h"
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
gnc_file_aqbanking_import(const gchar *aqbanking_importername,
                          const gchar *aqbanking_profilename,
                          gboolean execute_transactions)
{
    gchar *default_dir;
    gchar *selected_filename = NULL;
    gint dtaus_fd = -1;
    AB_BANKING *api = NULL;
    gboolean online = FALSE;
    GncGWENGui *gui = NULL;
    AB_IMEXPORTER *importer;
    GWEN_DB_NODE *db_profiles = NULL;
    GWEN_DB_NODE *db_profile;
    AB_IMEXPORTER_CONTEXT *context = NULL;
    GWEN_IO_LAYER *io, *buffio;
    GncABImExContextImport *ieci = NULL;
    AB_JOB_LIST2 *job_list = NULL;

    /* Select a file */
    default_dir = gnc_get_default_directory(GCONF_SECTION_AQBANKING);
    selected_filename = gnc_file_dialog(_("Select a file to import"),
                                        NULL, default_dir,
                                        GNC_FILE_DIALOG_IMPORT);
    g_free(default_dir);

    if (!selected_filename)
        goto cleanup;
    DEBUG("filename: %s", selected_filename);

    /* Remember the directory as the default */
    default_dir = g_path_get_dirname(selected_filename);
    gnc_set_default_directory(GCONF_SECTION_AQBANKING, default_dir);
    g_free(default_dir);

    dtaus_fd = g_open(selected_filename, O_RDONLY, 0);
    if (dtaus_fd == -1) {
      DEBUG("Could not open file %s", selected_filename);
      goto cleanup;
    }

    /* Get the API */
    api = gnc_AB_BANKING_new();
    if (!api) {
        g_warning("gnc_file_aqbanking_import: Couldn't get AqBanking API");
        goto cleanup;
    }
    if (AB_Banking_OnlineInit(api) != 0) {
        g_warning("gnc_file_aqbanking_import: "
                  "Couldn't initialize AqBanking API");
        goto cleanup;
    }
    online = TRUE;

    /* Get a GUI object */
    gui = gnc_GWEN_Gui_get(NULL);
    if (!gui) {
        g_warning("gnc_ab_getbalance: Couldn't initialize Gwenhywfar GUI");
        goto cleanup;
    }

    /* Get import module */
    importer = AB_Banking_GetImExporter(api, aqbanking_importername);
    if (!importer) {
        g_warning("Import module %s not found", aqbanking_importername);
        gnc_error_dialog(NULL, "%s",
                         _("Import module for DTAUS import not found."));
        goto cleanup;
    }

    /* Load the import profile */
    db_profiles = AB_Banking_GetImExporterProfiles(api, aqbanking_importername);

    /* Select profile */
    db_profile = GWEN_DB_GetFirstGroup(db_profiles);
    while (db_profile) {
        const gchar *name;

        name = GWEN_DB_GetCharValue(db_profile, "name", 0, 0);
        g_return_if_fail(name);
        if (g_ascii_strcasecmp(name, aqbanking_profilename)==0)
            break;
        db_profile = GWEN_DB_GetNextGroup(db_profile);
    }
    if (!db_profile) {
        g_warning("Profile \"%s\" for importer \"%s\" not found",
                  aqbanking_profilename, aqbanking_importername);
        /* For debugging: Print those available names that have been found */
        db_profile = GWEN_DB_GetFirstGroup(db_profiles);
        while (db_profile) {
            const char *name = GWEN_DB_GetCharValue(db_profile, "name", 0, 0);
            g_warning("Only found profile \"%s\"\n", name ? name : "(null)");
            db_profile = GWEN_DB_GetNextGroup(db_profile);
        }
        goto cleanup;
    }

    /* Create a context to store the results */
    context = AB_ImExporterContext_new();

    /* Wrap file in buffered gwen io */
    io = GWEN_Io_LayerFile_new(dtaus_fd, -1);
    dtaus_fd = -1;
    buffio = GWEN_Io_LayerBuffered_new(io);
    if (GWEN_Io_Manager_RegisterLayer(buffio)) {
        g_warning("gnc_file_aqbanking_import: Failed to wrap file");
        goto cleanup;
    }

    /* Run the import */
    if (AB_ImExporter_Import(importer, context, buffio, db_profile, 0)) {
        g_warning("gnc_file_aqbanking_import: Error on import");
        goto cleanup;
    }

    /* Close the file */
    GWEN_Io_Layer_free(buffio);

    /* Import the results */
    ieci = gnc_ab_import_context(context, AWAIT_TRANSACTIONS,
                                 execute_transactions,
                                 execute_transactions ? api : NULL,
                                 NULL);

    /* Extract the list of jobs */
    job_list = gnc_ab_ieci_get_job_list(ieci);

    if (execute_transactions) {
        if (gnc_ab_ieci_run_matcher(ieci)) {
            /* FIXME */
            /* gnc_hbci_multijob_execute(NULL, api, job_list, gui); */
        }
    }

cleanup:
    if (job_list)
        AB_Job_List2_FreeAll(job_list);
    if (ieci)
        g_free(ieci);
    if (context)
        AB_ImExporterContext_free(context);
    if (db_profiles)
        GWEN_DB_Group_free(db_profiles);
    if (gui)
        gnc_GWEN_Gui_release(gui);
    if (online)
        AB_Banking_OnlineFini(api);
    if (api)
        gnc_AB_BANKING_fini(api);
    if (dtaus_fd != -1)
        close(dtaus_fd);
    if (selected_filename)
        g_free(selected_filename);
}
