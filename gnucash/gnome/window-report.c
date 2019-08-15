/********************************************************************
 * window-report.c                                                  *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998 Linas Vepstas                                 *
 * Copyright (C) 1999 Jeremy Collins ( gtk-xmhtml port )            *
 * Copyright (C) 2000 Dave Peticolas                                *
 * Copyright (C) 2000 Bill Gribble                                  *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
 ********************************************************************/

#include <config.h>

#include <glib/gi18n.h>
#include <errno.h>
#include <libguile.h>
#include <sys/stat.h>

#include "swig-runtime.h"
#include "dialog-options.h"
#include "dialog-report-column-view.h"
#include "gnc-guile-utils.h"
#include "gnc-report.h"
#include "gnc-ui.h"
#include "option-util.h"
#include "window-report.h"
#include "guile-mappings.h"

#include "gnc-plugin-page-report.h"

#define WINDOW_REPORT_CM_CLASS "window-report"
#define MDI_CHILD_CONFIG "mdi_child_config"

/********************************************************************
 *
 ********************************************************************/

void
reportWindow(int report_id, GtkWindow *parent)
{
    gnc_set_busy_cursor (NULL, TRUE);
    gnc_main_window_open_report(report_id, GNC_MAIN_WINDOW(parent));
    gnc_unset_busy_cursor (NULL);
}

/********************************************************************
 * default parameters editor handling
 ********************************************************************/

struct report_default_params_data
{
    GNCOptionWin * win;
    GNCOptionDB  * db;
    SCM          scm_options;
    SCM          cur_report;
};


static void
gnc_options_dialog_apply_cb(GNCOptionWin * propertybox,
                            gpointer user_data)
{
    SCM  dirty_report = scm_c_eval_string("gnc:report-set-dirty?!");
    struct report_default_params_data * win = user_data;
    GList *results = NULL, *iter;

    if (!win) return;
    results = gnc_option_db_commit (win->db);
    for (iter = results; iter; iter = iter->next)
    {
        GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW (win->win),
                                                   0,
                                                   GTK_MESSAGE_ERROR,
                                                   GTK_BUTTONS_OK,
                                                   "%s",
                                                   (char*)iter->data);
        gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);
        g_free (iter->data);
    }
    g_list_free (results);

    scm_call_2(dirty_report, win->cur_report, SCM_BOOL_T);
}

static void
gnc_options_dialog_help_cb(GNCOptionWin * propertybox,
                           gpointer user_data)
{
    GtkWidget *dialog, *parent;
    struct report_default_params_data * prm = user_data;

    parent = gnc_options_dialog_widget(prm->win);
    dialog = gtk_message_dialog_new(GTK_WINDOW(parent),
                                    GTK_DIALOG_DESTROY_WITH_PARENT,
                                    GTK_MESSAGE_INFO,
                                    GTK_BUTTONS_OK,
                                    "%s",
                                    _("Set the report options you want using this dialog."));
    g_signal_connect(G_OBJECT(dialog), "response",
                     (GCallback)gtk_widget_destroy, NULL);
    gtk_widget_show(dialog);
}

static void
gnc_options_dialog_close_cb(GNCOptionWin * propertybox,
                            gpointer user_data)
{
    struct report_default_params_data * win = user_data;
    SCM    set_editor = scm_c_eval_string("gnc:report-set-editor-widget!");

    scm_call_2(set_editor, win->cur_report, SCM_BOOL_F);
    gnc_options_dialog_destroy(win->win);
    gnc_option_db_destroy(win->db);
    scm_gc_unprotect_object(win->scm_options);
    g_free(win);
}

static gboolean
gnc_report_raise_editor(SCM report)
{
    SCM get_editor   = scm_c_eval_string("gnc:report-editor-widget");
    SCM editor       = scm_call_1(get_editor, report);
    if (editor != SCM_BOOL_F)
    {
#define FUNC_NAME "gnc-report-raise-editor"
        GtkWidget *w = SWIG_MustGetPtr(editor,
                                   SWIG_TypeQuery("_p_GtkWidget"), 1, 0);
#undef FUNC_NAME
        gtk_window_present(GTK_WINDOW(w));
        return TRUE;
    }
    else
        return FALSE;
}


GtkWidget *
gnc_report_window_default_params_editor(SCM options, SCM report,
                                        GtkWindow *parent)
{
    SCM get_report_type   = scm_c_eval_string("gnc:report-type");
    SCM get_template      = scm_c_eval_string("gnc:find-report-template");
    SCM get_template_name = scm_c_eval_string("gnc:report-template-name");
    SCM ptr;

    const gchar *title = NULL;

    if (gnc_report_raise_editor (report))
        return NULL;
    else
    {
        struct report_default_params_data * prm =
            g_new0(struct report_default_params_data, 1);

        prm->scm_options = options;
        prm->cur_report  = report;
        prm->db          = gnc_option_db_new(prm->scm_options);

        /* Get the title of the report's template. */
        ptr = scm_call_1(get_report_type, report);
        if (ptr != SCM_BOOL_F)
        {
            ptr = scm_call_1(get_template, ptr);
            if (ptr != SCM_BOOL_F)
            {
                ptr = scm_call_1(get_template_name, ptr);
                if (scm_is_string(ptr))
                    title = gnc_scm_to_utf8_string (ptr);
            }
        }

        /* Don't forget to translate the window title */
        prm->win  = gnc_options_dialog_new((gchar*) (title && *title ? _(title) : ""), parent);

        g_free ((gpointer *) title);

        scm_gc_protect_object(prm->scm_options);
        scm_gc_protect_object(prm->cur_report);

        gnc_options_dialog_build_contents(prm->win, prm->db);
        gnc_option_db_clean(prm->db);

        gnc_options_dialog_set_apply_cb(prm->win,
                                        gnc_options_dialog_apply_cb,
                                        (gpointer)prm);
        gnc_options_dialog_set_help_cb(prm->win,
                                       gnc_options_dialog_help_cb,
                                       (gpointer)prm);
        gnc_options_dialog_set_close_cb(prm->win,
                                        gnc_options_dialog_close_cb,
                                        (gpointer)prm);
        return gnc_options_dialog_widget(prm->win);
    }
}

gboolean
gnc_report_edit_options(SCM report, GtkWindow *parent)
{
    SCM set_editor        = scm_c_eval_string("gnc:report-set-editor-widget!");
    SCM get_options       = scm_c_eval_string("gnc:report-options");
    SCM get_report_type   = scm_c_eval_string("gnc:report-type");
    SCM ptr;
    SCM options;
    GtkWidget *options_widget = NULL;

    /* If the options editor widget already exists we simply raise it */
    if (gnc_report_raise_editor (report))
        return TRUE;

    /* Check if this report has options to edit */
    options = scm_call_1(get_options, report);
    if (options == SCM_BOOL_F)
    {
        gnc_warning_dialog (parent, "%s",
                            _("There are no options for this report."));
        return FALSE;
    }

    /* Multi-column type reports need a special options dialog */
    ptr = scm_call_1(get_report_type, report);
    if (scm_is_string(ptr))
    {
        gchar *rpt_type = gnc_scm_to_utf8_string (ptr);
        if (g_strcmp0 (rpt_type, "d8ba4a2e89e8479ca9f6eccdeb164588") == 0)
            options_widget = gnc_column_view_edit_options (options, report);
        else
            options_widget = gnc_report_window_default_params_editor (options, report, parent);
        g_free (rpt_type);
    }

    /* Store the options editor widget for future reuse */
#define FUNC_NAME "gnc-report-edit-options"
    ptr = SWIG_NewPointerObj (options_widget, SWIG_TypeQuery("_p_GtkWidget"), 0);
#undef FUNC_NAME
    scm_call_2 (set_editor, report, ptr);

    return TRUE;
}
