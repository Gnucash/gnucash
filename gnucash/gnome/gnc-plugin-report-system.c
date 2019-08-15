/*
 * gnc-plugin-report-system.c --
 * Copyright (C) 2003 David Hampton <hampton@employees.org>
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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-report-style-sheet.h"
#include "file-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-html.h"
#include "gnc-plugin-page-report.h"
#include "gnc-plugin-report-system.h"
#include "gnc-plugin-manager.h"
#include "gnc-report.h"
#include "gnc-engine.h"
#include "window-report.h"

static void gnc_plugin_report_system_class_init (GncPluginReportSystemClass *klass);
static void gnc_plugin_report_system_init (GncPluginReportSystem *plugin);
static void gnc_plugin_report_system_finalize (GObject *object);


/* Command callbacks */
static void gnc_plugin_report_system_cmd_edit_style_sheet (GtkAction *action,
        GncMainWindowActionData *data);


#define PLUGIN_ACTIONS_NAME "gnc-plugin-report-system-actions"
#define PLUGIN_UI_FILENAME  "gnc-plugin-report-system-ui.xml"

static GtkActionEntry gnc_plugin_actions [] =
{
    /* Menu Items */
    {
        "EditStyleSheetsAction", NULL, N_("St_yle Sheets"), NULL,
        N_("Edit report style sheets"),
        G_CALLBACK (gnc_plugin_report_system_cmd_edit_style_sheet)
    },
};
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);


typedef struct GncPluginReportSystemPrivate
{
    gpointer dummy;
} GncPluginReportSystemPrivate;

G_DEFINE_TYPE_WITH_PRIVATE(GncPluginReportSystem, gnc_plugin_report_system, GNC_TYPE_PLUGIN)

#define GNC_PLUGIN_REPORT_SYSTEM_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_REPORT_SYSTEM, GncPluginReportSystemPrivate))

static GObjectClass *parent_class = NULL;

/************************************************************
 *                   Object Implementation                  *
 ************************************************************/

static void
gnc_plugin_report_system_class_init (GncPluginReportSystemClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginClass *plugin_class = GNC_PLUGIN_CLASS (klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_plugin_report_system_finalize;

    /* plugin info */
    plugin_class->plugin_name  = GNC_PLUGIN_REPORT_SYSTEM_NAME;

    /* widget addition/removal */
    plugin_class->actions_name  	   = PLUGIN_ACTIONS_NAME;
    plugin_class->actions       	   = gnc_plugin_actions;
    plugin_class->n_actions     	   = gnc_plugin_n_actions;
    plugin_class->ui_filename   	   = PLUGIN_UI_FILENAME;
}

static void
gnc_plugin_report_system_init (GncPluginReportSystem *plugin)
{
}

static void
gnc_plugin_report_system_finalize (GObject *object)
{
    g_return_if_fail (GNC_IS_PLUGIN_REPORT_SYSTEM (object));

    G_OBJECT_CLASS (parent_class)->finalize (object);
}

/************************************************************
 *                    Command Callbacks                     *
 ************************************************************/

static void
gnc_plugin_report_system_cmd_edit_style_sheet (GtkAction *action,
        GncMainWindowActionData *data)
{
    gnc_style_sheet_dialog_open(GTK_WINDOW (data->window));
}

/************************************************************
 *               Html url and stream handlers               *
 ************************************************************/

static gboolean
gnc_report_system_file_stream_cb (const char *location, char ** data, int *len)
{
    *len = gncReadFile (location, data);
    return (*len > 0);
}

static gboolean
gnc_report_system_report_stream_cb (const char *location, char ** data, int *len)
{
    gboolean ok;

    ok = gnc_run_report_id_string (location, data);

    if (!ok)
    {
        *data = g_strdup_printf ("<html><body><h3>%s</h3>"
        "<p>%s</p></body></html>",
        _("Report error"),
                                 _("An error occurred while running the report."));

        /* Make sure the progress bar is finished, which will also
         *           make the GUI sensitive again. Easier to do this via guile
         *           because otherwise we would need to link against gnome-utils
         *           and a lot more. */
        scm_c_eval_string("(gnc:report-finished)");
    }

    *len = strlen(*data);
    return ok;
}

/* TODO: unroll start_editor */
static gboolean
gnc_report_system_options_url_cb (const char *location, const char *label,
                         gboolean new_window, GNCURLResult *result)
{
    SCM report;
    int report_id;

    g_return_val_if_fail (location != NULL, FALSE);
    g_return_val_if_fail (result != NULL, FALSE);

    result->load_to_stream = FALSE;

    /* href="gnc-options:report-id=2676" */
    if (strncmp ("report-id=", location, 10) == 0)
    {
        if (sscanf (location + 10, "%d", &report_id) != 1)
        {
            result->error_message =
            g_strdup_printf (_("Badly formed options URL: %s"), location);

            return FALSE;
        }

        report = gnc_report_find(report_id);
        if (report == SCM_UNDEFINED ||
            report == SCM_BOOL_F)
        {
            result->error_message =
            g_strdup_printf (_("Badly-formed report id: %s"), location);

            return FALSE;
        }

        gnc_report_edit_options (report, GTK_WINDOW(result->parent));

        return TRUE;
    }
    else
    {
        result->error_message =
        g_strdup_printf (_("Badly formed options URL: %s"), location);

        return FALSE;
    }
}

static gboolean
gnc_report_system_report_url_cb (const char *location, const char *label,
                        gboolean new_window, GNCURLResult *result)
{
    g_return_val_if_fail (location != NULL, FALSE);
    g_return_val_if_fail (result != NULL, FALSE);

    /* make a new window if necessary */
    if (new_window)
    {
        char *url;

        url = gnc_build_url (URL_TYPE_REPORT, location, label);
        gnc_main_window_open_report_url (url, GNC_MAIN_WINDOW(result->parent));
        g_free (url);

        result->load_to_stream = FALSE;
    }
    else
    {
        result->load_to_stream = TRUE;
    }

    return TRUE;
}

static gboolean
gnc_report_system_help_url_cb (const char *location, const char *label,
                      gboolean new_window, GNCURLResult *result)
{
    g_return_val_if_fail (location != NULL, FALSE);

    if (label && (*label != '\0'))
        gnc_gnome_help (location, label);
    else
        gnc_gnome_help (location, NULL);
    return TRUE;
}


/************************************************************
 *                    Plugin Bootstrapping                   *
 ************************************************************/

void
gnc_plugin_report_system_new (void)
{
    GncPlugin *plugin;

    /* Reference the report page plugin to ensure it exists in the gtk
     * type system. */
    GNC_TYPE_PLUGIN_PAGE_REPORT;

    /* Register html handlers */
    gnc_html_register_stream_handler (URL_TYPE_HELP, gnc_report_system_file_stream_cb);
    gnc_html_register_stream_handler (URL_TYPE_FILE, gnc_report_system_file_stream_cb);
    gnc_html_register_stream_handler (URL_TYPE_REPORT, gnc_report_system_report_stream_cb);

    gnc_html_register_url_handler (URL_TYPE_OPTIONS, gnc_report_system_options_url_cb);
    gnc_html_register_url_handler (URL_TYPE_REPORT, gnc_report_system_report_url_cb);
    gnc_html_register_url_handler (URL_TYPE_HELP, gnc_report_system_help_url_cb);

    scm_c_use_module("gnucash reports");
    scm_c_use_module("gnucash report-menus");
    scm_c_eval_string("(gnc:report-menu-setup)");

    plugin = GNC_PLUGIN (g_object_new (GNC_TYPE_PLUGIN_REPORT_SYSTEM, NULL));
    gnc_plugin_manager_add_plugin (gnc_plugin_manager_get (), plugin);
}
