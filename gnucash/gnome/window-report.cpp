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
#include <glib/gi18n.h>
#include <memory>
#include "dialog-options.hpp"
#include "dialog-report-column-view.hpp"
#include <libguile.h>

#include <config.h>

#include <errno.h>
#include <sys/stat.h>

#include "swig-runtime.h"
#include "gnc-guile-utils.h"
#include "gnc-ui.h"
#include "window-report.h"
#include "guile-mappings.h"

#include "gnc-plugin-page-report.h"
#include "gnc-report.h"

/********************************************************************
 *
 ********************************************************************/

void
reportWindow(int report_id, GtkWindow *parent)
{
    gnc_set_busy_cursor (nullptr, TRUE);
    gnc_main_window_open_report(report_id, GNC_MAIN_WINDOW(parent));
    gnc_unset_busy_cursor (nullptr);
}

/********************************************************************
 * default parameters editor handling
 ********************************************************************/

struct report_default_params_data
{
    GncOptionsDialog * win;
    GncOptionDB  * odb;
    SCM          cur_report;
};


static void
gnc_options_dialog_apply_cb(GncOptionsDialog *opt_dialog,
                            gpointer user_data)
{
    SCM  dirty_report = scm_c_eval_string("gnc:report-set-dirty?!");
    auto win{static_cast<struct report_default_params_data*>(user_data)};

    if (!win) return;
    auto results = gnc_option_db_commit (win->odb);
    for (auto iter = results; iter; iter = iter->next)
    {
        auto dialog = gtk_message_dialog_new(GTK_WINDOW (win->win),
                                             static_cast<GtkDialogFlags>(0),
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
gnc_options_dialog_help_cb(GncOptionsDialog *opt_dialog,
                           gpointer user_data)
{
    auto prm{static_cast<struct report_default_params_data*>(user_data)};

    auto parent = prm->win->get_widget();
    auto dialog = gtk_message_dialog_new(GTK_WINDOW(parent),
                                         GTK_DIALOG_DESTROY_WITH_PARENT,
                                         GTK_MESSAGE_INFO,
                                         GTK_BUTTONS_OK,
                                         "%s",
                                         _("Set the report options you want using this dialog."));
    g_signal_connect(G_OBJECT(dialog), "response",
                     (GCallback)gtk_widget_destroy, nullptr);
    gtk_widget_show(dialog);
}

static void
gnc_options_dialog_close_cb(GncOptionsDialog *opt_dialog,
                            gpointer user_data)
{
    auto win{static_cast<struct report_default_params_data*>(user_data)};
    SCM    set_editor = scm_c_eval_string("gnc:report-set-editor-widget!");

    scm_call_2(set_editor, win->cur_report, SCM_BOOL_F);
    delete win->win;
    gnc_option_db_destroy(win->odb);
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
        auto w{static_cast<GtkWidget *>(SWIG_MustGetPtr(editor, SWIG_TypeQuery("_p_GtkWidget"), 1, 0))};
#undef FUNC_NAME
        gtk_window_present(GTK_WINDOW(w));
        return TRUE;
    }
    else
        return FALSE;
}


GtkWidget *
gnc_report_window_default_params_editor(GncOptionDB* odb, SCM report,
                                        GtkWindow *parent)
{
    SCM get_report_type   = scm_c_eval_string("gnc:report-type");
    SCM get_template      = scm_c_eval_string("gnc:find-report-template");
    SCM get_template_name = scm_c_eval_string("gnc:report-template-name");
    SCM ptr;

    const gchar *title = nullptr;

    if (gnc_report_raise_editor (report))
        return nullptr;
    else
    {
        struct report_default_params_data * prm =
            g_new0(struct report_default_params_data, 1);

        prm->odb         = odb;
        prm->cur_report  = report;

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
        prm->win  = new GncOptionsDialog((gchar*) (title && *title ? _(title) : ""), parent);

        g_free ((gpointer *) title);

        scm_gc_protect_object(prm->cur_report);

        prm->win->build_contents(prm->odb);

        prm->win->set_apply_cb(gnc_options_dialog_apply_cb, (gpointer)prm);
        prm->win->set_help_cb(gnc_options_dialog_help_cb, (gpointer)prm);
        prm->win->set_close_cb(gnc_options_dialog_close_cb, (gpointer)prm);
        return prm->win->get_widget();
    }
}

gboolean
gnc_report_edit_options(SCM report, GtkWindow *parent)
{
    SCM set_editor        = scm_c_eval_string("gnc:report-set-editor-widget!");
    SCM get_report_type   = scm_c_eval_string("gnc:report-type");
    SCM ptr;
    GncOptionDB* odb;
    GtkWidget *options_widget = nullptr;

    /* If the options editor widget already exists we simply raise it */
    if (gnc_report_raise_editor (report))
        return TRUE;

    /* Check if this report has options to edit */
    odb = gnc_get_report_optiondb(report);
    if (!odb)
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
            options_widget = gnc_column_view_edit_options (odb, report);
        else
            options_widget = gnc_report_window_default_params_editor (odb, report, parent);
        g_free (rpt_type);
    }

    /* Store the options editor widget for future reuse */
#define FUNC_NAME "gnc-report-edit-options"
    ptr = SWIG_NewPointerObj (options_widget, SWIG_TypeQuery("_p_GtkWidget"), 0);
#undef FUNC_NAME
    scm_call_2 (set_editor, report, ptr);

    return TRUE;
}

GncOptionDB*
gnc_get_report_optiondb(SCM report_instance)
{
    SCM  get_report_options    = scm_c_eval_string("gnc:report-options");
    auto scm_dispatch{scm_call_1(get_report_options, report_instance)};
    return gnc_get_optiondb_from_dispatcher(scm_dispatch);
}
