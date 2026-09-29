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
#include "gnc-gnome-utils.h"
#include "import-account-matcher.h"
#include "import-main-matcher.h"
#include <gnc-state.h>

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_IMPORT;

static AB_IMEXPORTER_CONTEXT*
named_import_get_context (AB_BANKING *api, const gchar *aqbanking_importername,
                          const gchar *aqbanking_profilename,
                          const gchar *selected_filename)
{
    AB_IMEXPORTER_CONTEXT *context;
    int success;
    if (!selected_filename)
        return NULL;
    DEBUG("filename: %s", selected_filename);

    /* Remember the directory as the default */
    gchar *default_dir = g_path_get_dirname(selected_filename);
    gnc_set_default_directory(GNC_PREFS_GROUP_AQBANKING, default_dir);
    g_free(default_dir);

/* Create a context to store the results */
    context = AB_ImExporterContext_new();
    success =
        AB_Banking_ImportFromFileLoadProfile(api, aqbanking_importername,
                                             context, aqbanking_profilename,
                                             NULL, selected_filename);
    if (success < 0)
    {
        AB_ImExporterContext_free(context);
        g_warning("gnc_file_aqbanking_import: Error on import");
        return NULL;
    }
    return context;
}

static const char *GNC_STATE_SECTION = "dialogs.aqb.file-import";
static const char *STATE_KEY_LAST_FORMAT = "format";
static const char *STATE_KEY_LAST_PROFILE = "profile";

static void
load_imexporter_and_profile(char** imexporter, char** profile)
{
    GKeyFile *state_file = gnc_state_get_current();

    if (g_key_file_has_key(state_file, GNC_STATE_SECTION, STATE_KEY_LAST_FORMAT, NULL))
        *imexporter = g_key_file_get_string (state_file, GNC_STATE_SECTION, STATE_KEY_LAST_FORMAT, NULL);

    if (g_key_file_has_key(state_file, GNC_STATE_SECTION, STATE_KEY_LAST_PROFILE, NULL))
        *profile = g_key_file_get_string (state_file, GNC_STATE_SECTION, STATE_KEY_LAST_PROFILE, NULL);
}

static void
save_imexporter_and_profile(const char* imexporter, const char *profile)
{
    GKeyFile *state_file = gnc_state_get_current();

    g_key_file_set_string(state_file, GNC_STATE_SECTION, STATE_KEY_LAST_FORMAT, imexporter);
    g_key_file_set_string(state_file, GNC_STATE_SECTION, STATE_KEY_LAST_PROFILE, profile);
}

typedef struct
{
    GWeakRef parent;
    gulong parent_destroy_handler;
    gboolean parent_destroyed;
    QofBook *book;
    guint lease;
    guint aq_operation;
    AB_BANKING *api;
    GncGWENGui *gui;
    GncABSelectImExDlg *dialog;
    gchar *imexporter;
    gchar *profile;
    gchar *filename;
    AB_IMEXPORTER_CONTEXT *context;
    GncABImExContextImport *ieci;
} AqImportDialogRequest;

static void aq_import_work (GncGWENGui *gui, gpointer user_data);
static void aq_import_work_completed (gpointer user_data);
static void aq_import_dialog_import_completed (GncABImExContextImport *ieci,
                                               gpointer user_data);
static void aq_import_matcher_completed (gboolean accepted,
                                         gpointer user_data);

static void
aq_import_parent_destroyed (G_GNUC_UNUSED GtkWidget *parent,
                            gpointer user_data)
{
    ((AqImportDialogRequest *)user_data)->parent_destroyed = TRUE;
}

static void
aq_import_dialog_request_free (AqImportDialogRequest *request)
{
    if (request->ieci)
        gnc_ab_ieci_free (request->ieci);
    if (request->context)
        AB_ImExporterContext_free (request->context);
    if (request->dialog)
        gnc_ab_select_imex_dlg_destroy (request->dialog);
    if (request->api)
        gnc_AB_BANKING_fini (request->api);
    if (request->gui)
        gnc_GWEN_Gui_release (request->gui);
    if (request->aq_operation)
        gnc_ab_operation_release (request->aq_operation);
    gnc_gui_end_session_operation (request->lease);
    request->lease = 0;
    g_clear_pointer (&request->imexporter, g_free);
    g_clear_pointer (&request->profile, g_free);
    g_object_unref (request->book);
    GtkWindow *parent = g_weak_ref_get (&request->parent);
    if (parent && request->parent_destroy_handler &&
        g_signal_handler_is_connected (parent, request->parent_destroy_handler))
        g_signal_handler_disconnect (parent, request->parent_destroy_handler);
    g_clear_object (&parent);
    g_weak_ref_clear (&request->parent);
    g_free (request);
}

static void
aq_import_work (G_GNUC_UNUSED GncGWENGui *gui, gpointer user_data)
{
    AqImportDialogRequest *request = user_data;
    request->context = named_import_get_context (request->api,
        request->imexporter, request->profile, request->filename);
}

static void
aq_import_work_completed (gpointer user_data)
{
    AqImportDialogRequest *request = user_data;
    GtkWindow *parent = g_weak_ref_get (&request->parent);
    gboolean valid = parent && !request->parent_destroyed &&
        !gtk_widget_in_destruction (GTK_WIDGET (parent)) &&
        gnc_get_current_book () == request->book && qof_book_is_open (request->book);
    if (!valid || !request->context)
    {
        g_clear_object (&parent);
        aq_import_dialog_request_free (request);
        return;
    }
    gnc_ab_import_context_async (request->context, AWAIT_TRANSACTIONS, FALSE,
                                 request->api, GTK_WIDGET (parent),
                                 aq_import_dialog_import_completed, request);
    g_object_unref (parent);
}

static void
aq_import_dialog_import_completed (GncABImExContextImport *ieci,
                                  gpointer user_data)
{
    AqImportDialogRequest *request = user_data;
    if (!ieci)
    {
        aq_import_dialog_request_free (request);
        return;
    }
    request->ieci = ieci;
    gnc_ab_ieci_run_matcher_async (ieci, aq_import_matcher_completed,
                                   request);
}

static void
aq_import_matcher_completed (G_GNUC_UNUSED gboolean accepted,
                             gpointer user_data)
{
    AqImportDialogRequest *request = user_data;
    gnc_ab_ieci_free (request->ieci);
    request->ieci = NULL;
    aq_import_dialog_request_free (request);
}

static void
aq_import_file_selected (GSList *filenames, gpointer user_data)
{
    AqImportDialogRequest *request = user_data;
    GtkWindow *parent = g_weak_ref_get (&request->parent);
    gboolean valid = parent && !request->parent_destroyed &&
        !gtk_widget_in_destruction (GTK_WIDGET (parent)) &&
        gnc_get_current_book () == request->book && qof_book_is_open (request->book);
    if (!valid || !filenames || !filenames->data)
    {
        g_clear_object (&parent);
        g_slist_free_full (filenames, g_free);
        aq_import_dialog_request_free (request);
        return;
    }
    request->filename = g_strdup (filenames->data);
    gchar *default_dir = g_path_get_dirname (request->filename);
    gnc_set_default_directory (GNC_PREFS_GROUP_AQBANKING, default_dir);
    g_free (default_dir);
    g_slist_free_full (filenames, g_free);
    request->gui = gnc_GWEN_Gui_get (GTK_WIDGET (parent));
    if (!request->gui)
    {
        g_warning ("gnc_file_aqbanking_import: Couldn't initialize Gwenhywfar GUI");
        g_object_unref (parent);
        aq_import_dialog_request_free (request);
        return;
    }
    gnc_GWEN_Gui_run_job_async (request->gui, aq_import_work,
        aq_import_work_completed, request, NULL);
    g_object_unref (parent);
}

static void
aq_import_dialog_selected (gboolean accepted, const gchar *imexporter,
                           const gchar *profile, gpointer user_data)
{
    AqImportDialogRequest *request = user_data;
    GtkWindow *parent = g_weak_ref_get (&request->parent);
    gboolean valid = parent && !request->parent_destroyed &&
        !gtk_widget_in_destruction (GTK_WIDGET (parent)) &&
        gnc_get_current_book () == request->book && qof_book_is_open (request->book);
    request->imexporter = accepted ? g_strdup (imexporter) : NULL;
    request->profile = accepted ? g_strdup (profile) : NULL;
    if (request->dialog)
    {
        gnc_ab_select_imex_dlg_destroy (request->dialog);
        request->dialog = NULL;
    }
    if (!valid || !accepted || !request->imexporter || !request->profile)
    {
        g_clear_object (&parent);
        aq_import_dialog_request_free (request);
        return;
    }

    save_imexporter_and_profile (request->imexporter, request->profile);
    gchar *default_dir = gnc_get_default_directory (GNC_PREFS_GROUP_AQBANKING);
    gnc_file_dialog_async (parent, _("Select a file to import"), NULL,
                           default_dir, GNC_FILE_DIALOG_IMPORT, FALSE,
                           aq_import_file_selected, request, NULL);
    g_free (default_dir);
    g_object_unref (parent);
}

static void
aq_import_operation_acquired (guint token, gpointer user_data)
{
    AqImportDialogRequest *request = user_data;
    request->aq_operation = token;
    GtkWindow *parent = g_weak_ref_get (&request->parent);
    if (!parent || request->parent_destroyed ||
        gtk_widget_in_destruction (GTK_WIDGET (parent)) ||
        gnc_get_current_book () != request->book ||
        !qof_book_is_open (request->book))
    {
        g_clear_object (&parent);
        aq_import_dialog_request_free (request);
        return;
    }
    request->api = gnc_AB_BANKING_new ();
    if (!request->api)
    {
        g_object_unref (parent);
        aq_import_dialog_request_free (request);
        return;
    }
    request->dialog = gnc_ab_select_imex_dlg_new (GTK_WIDGET (parent),
                                                   request->api);

    if (!request->dialog)
    {
        PERR ("Failed to create select imex dialog.");
        g_object_unref (parent);
        aq_import_dialog_request_free (request);
        return;
    }
    gchar *imexporter = NULL;
    gchar *profile = NULL;
    load_imexporter_and_profile (&imexporter, &profile);
    gnc_ab_select_imex_dlg_set_imexporter_name (request->dialog, imexporter);
    gnc_ab_select_imex_dlg_set_profile_name (request->dialog, profile);
    g_free (imexporter);
    g_free (profile);
    gnc_ab_select_imex_dlg_run_async (request->dialog,
                                      aq_import_dialog_selected, request);
    g_object_unref (parent);
}

void
gnc_file_aqbanking_import_dialog (GtkWindow *parent)
{
    g_return_if_fail (GTK_IS_WINDOW (parent));
    QofBook *book = gnc_get_current_book ();
    guint lease = gnc_gui_begin_session_operation (book);
    if (!lease)
        return;
    AqImportDialogRequest *request = g_new0 (AqImportDialogRequest, 1);
    request->book = g_object_ref (book);
    request->lease = lease;
    g_weak_ref_init (&request->parent, G_OBJECT (parent));
    request->parent_destroy_handler = g_signal_connect (parent, "destroy",
        G_CALLBACK (aq_import_parent_destroyed), request);
    gnc_ab_operation_acquire_async (aq_import_operation_acquired, request);
}
