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

void
gnc_file_aqbanking_import_dialog (GtkWindow *parent)
{
     AB_BANKING* api = gnc_AB_BANKING_new ();
     GncABSelectImExDlg* imexd =
         gnc_ab_select_imex_dlg_new (GTK_WIDGET (parent), api);
     char *imexporter = NULL, *profile = NULL;
     AB_IMEXPORTER_CONTEXT* ctx = NULL;

     if (!imexd)
     {

         PERR ("Failed to create select imex dialog.");
         gnc_AB_BANKING_fini(api);
         return;
     }
     load_imexporter_and_profile(&imexporter, &profile);
     gnc_ab_select_imex_dlg_set_imexporter_name(imexd, imexporter);
     gnc_ab_select_imex_dlg_set_profile_name(imexd, profile);

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
             ieci = gnc_ab_import_context (ctx, AWAIT_TRANSACTIONS, FALSE, api, GTK_WIDGET(parent));
             g_free(ieci);
             AB_ImExporterContext_free(ctx);
         }

         save_imexporter_and_profile(imexporter, profile);
         g_free (imexporter);
         g_free (profile);
     }

     gnc_AB_BANKING_fini(api);
}
