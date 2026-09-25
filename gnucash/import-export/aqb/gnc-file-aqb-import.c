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
 * @brief File import module code
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
#include <gnc-state.h>

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_IMPORT;

static const char *GNC_STATE_SECTION = "dialogs.aqb.file-import";
static const char *STATE_KEY_LAST_FORMAT = "format";
static const char *STATE_KEY_LAST_PROFILE = "profile";

typedef struct
{
    AB_BANKING *api;
    GWeakRef parent;
    gboolean has_parent;
    gchar *imexporter;
    gchar *profile;
} AqBankingImportData;

static void
load_imexporter_and_profile (char **imexporter, char **profile)
{
    GKeyFile *state_file = gnc_state_get_current ();

    if (g_key_file_has_key (state_file, GNC_STATE_SECTION,
                            STATE_KEY_LAST_FORMAT, NULL))
        *imexporter = g_key_file_get_string (state_file, GNC_STATE_SECTION,
                                              STATE_KEY_LAST_FORMAT, NULL);

    if (g_key_file_has_key (state_file, GNC_STATE_SECTION,
                            STATE_KEY_LAST_PROFILE, NULL))
        *profile = g_key_file_get_string (state_file, GNC_STATE_SECTION,
                                           STATE_KEY_LAST_PROFILE, NULL);
}

static void
save_imexporter_and_profile (const char *imexporter, const char *profile)
{
    GKeyFile *state_file = gnc_state_get_current ();

    g_key_file_set_string (state_file, GNC_STATE_SECTION,
                           STATE_KEY_LAST_FORMAT, imexporter);
    g_key_file_set_string (state_file, GNC_STATE_SECTION,
                           STATE_KEY_LAST_PROFILE, profile);
}

static AqBankingImportData *
aqb_import_data_new (GtkWindow *parent, AB_BANKING *api)
{
    AqBankingImportData *data = g_new0 (AqBankingImportData, 1);

    data->api = api;
    data->has_parent = parent != NULL;
    g_weak_ref_init (&data->parent, parent);
    return data;
}

static void
aqb_import_data_free (AqBankingImportData *data)
{
    if (!data)
        return;

    g_weak_ref_clear (&data->parent);
    g_clear_pointer (&data->imexporter, g_free);
    g_clear_pointer (&data->profile, g_free);
    if (data->api)
        gnc_AB_BANKING_fini (data->api);
    g_free (data);
}

static GtkWindow *
aqb_import_data_get_parent (AqBankingImportData *data)
{
    if (!data->has_parent)
        return NULL;

    return g_weak_ref_get (&data->parent);
}

static void
aqb_report_file_dialog_error (const GError *error)
{
    if (error && !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
        g_warning ("AqBanking file import: %s", error->message);
}

static void
aqb_import_context_finished (GncABImExContextImport *ieci,
                             gboolean completed, gpointer user_data)
{
    AqBankingImportData *data = user_data;

    (void)ieci;
    if (!completed)
        g_debug ("AqBanking file import was cancelled before the response was applied");
    if (data->imexporter && data->profile)
        save_imexporter_and_profile (data->imexporter, data->profile);
    aqb_import_data_free (data);
}

static void
aqb_import_file_dialog_finished (GObject *source, GAsyncResult *result,
                                 gpointer user_data)
{
    AqBankingImportData *data = user_data;
    GncFileDialogRequest *request = GNC_FILE_DIALOG_REQUEST (source);
    GError *error = NULL;
    GFile *file = gnc_file_dialog_request_finish (request, result, &error);
    GtkWindow *parent;
    gchar *selected_filename = NULL;
    gchar *default_dir;
    AB_IMEXPORTER_CONTEXT *context;
    int success;

    if (!file)
    {
        aqb_report_file_dialog_error (error);
        g_clear_error (&error);
        goto out;
    }

    selected_filename = g_file_get_path (file);
    g_object_unref (file);
    if (!selected_filename)
    {
        g_warning ("AqBanking file import requires a local file");
        goto out;
    }

    parent = aqb_import_data_get_parent (data);
    if (data->has_parent && !parent)
    {
        g_free (selected_filename);
        goto out;
    }

    DEBUG ("filename: %s", selected_filename);
    default_dir = g_path_get_dirname (selected_filename);
    gnc_set_default_directory (GNC_PREFS_GROUP_AQBANKING, default_dir);
    g_free (default_dir);

    context = AB_ImExporterContext_new ();
    success = AB_Banking_ImportFromFileLoadProfile (data->api,
                                                     data->imexporter,
                                                     context,
                                                     data->profile,
                                                     NULL,
                                                     selected_filename);
    g_free (selected_filename);
    if (success < 0)
    {
        AB_ImExporterContext_free (context);
        g_warning ("gnc_file_aqbanking_import: Error on import");
    }
    else
    {
        gnc_ab_import_context_async (context, AWAIT_TRANSACTIONS, FALSE,
                                     data->api, GTK_WIDGET (parent), NULL,
                                     aqb_import_context_finished, data);
        context = NULL;
        g_clear_object (&parent);
        return;
    }
    g_clear_object (&parent);

out:
    if (data->imexporter && data->profile)
        save_imexporter_and_profile (data->imexporter, data->profile);
    aqb_import_data_free (data);
}

static void
aqb_select_imexporter_finished (GObject *source, GAsyncResult *result,
                                gpointer user_data)
{
    AqBankingImportData *data = user_data;
    GError *error = NULL;
    char *default_dir;
    GncFileDialogRequest *request;
    GtkWindow *parent;

    (void)source;
    if (!gnc_ab_select_imex_dlg_run_finish (result, &data->imexporter,
                                            &data->profile, &error))
    {
        aqb_report_file_dialog_error (error);
        g_clear_error (&error);
        aqb_import_data_free (data);
        return;
    }

    if (!data->imexporter || !data->profile)
    {
        aqb_import_data_free (data);
        return;
    }

    parent = aqb_import_data_get_parent (data);
    if (data->has_parent && !parent)
    {
        aqb_import_data_free (data);
        return;
    }

    default_dir = gnc_get_default_directory (GNC_PREFS_GROUP_AQBANKING);
    request = gnc_file_dialog_request_new (parent, _("Select a file to import"),
                                           NULL, default_dir,
                                           GNC_FILE_DIALOG_IMPORT);
    g_clear_object (&parent);
    g_free (default_dir);
    if (!request)
    {
        aqb_import_data_free (data);
        return;
    }

    gnc_file_dialog_request_open_async (request, NULL,
                                        aqb_import_file_dialog_finished, data);
    g_object_unref (request);
}

void
gnc_file_aqbanking_import_dialog (GtkWindow *parent)
{
    AB_BANKING *api = gnc_AB_BANKING_new ();
    GncABSelectImExDlg *imexd;
    AqBankingImportData *data;
    char *imexporter = NULL;
    char *profile = NULL;

    imexd = gnc_ab_select_imex_dlg_new (parent ? GTK_WIDGET (parent) : NULL, api);
    if (!imexd)
    {
        PERR ("Failed to create select imex dialog.");
        gnc_AB_BANKING_fini (api);
        return;
    }

    data = aqb_import_data_new (parent, api);
    load_imexporter_and_profile (&imexporter, &profile);
    gnc_ab_select_imex_dlg_set_imexporter_name (imexd, imexporter);
    gnc_ab_select_imex_dlg_set_profile_name (imexd, profile);
    g_free (imexporter);
    g_free (profile);

    gnc_ab_select_imex_dlg_run_async (imexd, NULL,
                                      aqb_select_imexporter_finished, data);
}
