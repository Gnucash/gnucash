/**************************************************************************\
 * dialog-custom-report.c -- dialog for managing custom reports           *
 *                                                                        *
 * Copyright (C) 2009 Andrew Sackville-West (andrew@swclan.homelinux.org) *
 *                                                                        *
 * This program is free software; you can redistribute it and/or          *
 * modify it under the terms of the GNU General Public License as         *
 * published by the Free Software Foundation; either version 2 of         *
 * the License, or (at your option) any later version.                    *
 *                                                                        *
 * This program is distributed in the hope that it will be useful,        *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of         *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          *
 * GNU General Public License for more details.                           *
 *                                                                        *
 * You should have received a copy of the GNU General Public License      *
 * along with this program; if not, contact:                              *
 *                                                                        *
 * Free Software Foundation           Voice:  +1-617-542-5942             *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652             *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                         *
\*************************************************************************/

#include "config.h"

#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <libguile.h>
#include "swig-runtime.h"

#include "dialog-custom-report.h"
#include "dialog-options.h"
#include "dialog-utils.h"
#include "gnc-main-window.h"
#include "option-util.h"
#include "window-report.h"
#include "guile-mappings.h"
#include "gnc-gui-query.h"
#include "gnc-ui.h"
#include "gnc-report.h"
#include "gnc-plugin-page-report.h"


/* convenience for accessing columns in the GtkListStore that holds
   the reports */
enum
{
    COL_NAME = 0,
    COL_NUM,
    NUM_COLS
};

/* all the pertinent stuff needed to pass around */
typedef struct _CustomReportDialog
{
    /* dialog */
    GtkWidget *dialog;
    GtkWidget *reportview;
    GncMainWindow *window;

    /* data */
    SCM reportlist;

} CustomReportDialog;

void custom_report_dialog_close_cb(GtkWidget* widget, gpointer data);
void cancel_custom_report_clicked_cb(GtkWidget* widget, gpointer data);
void run_custom_report_clicked_cb(GtkWidget* button, gpointer data);
void delete_custom_report_clicked_cb(GtkWidget *button, gpointer data);
void custom_report_list_view_row_activated_cb(GtkTreeView *view, GtkTreePath *path,
        GtkTreeViewColumn *column, gpointer data);

void
custom_report_dialog_close_cb(GtkWidget* widget, gpointer data)
{
    CustomReportDialog *crd = data;
    gtk_widget_destroy(crd->dialog);
    g_free(crd);
}


void
cancel_custom_report_clicked_cb(GtkWidget* widget, gpointer data)
{
    CustomReportDialog *crd = data;
    custom_report_dialog_close_cb(NULL, crd);
}


/********************************************************************
 * update_report_list
 *
 * this procedure does the real work of displaying a sorted list of
 * available custom reports
 ********************************************************************/
static void
update_report_list(GtkListStore *store, CustomReportDialog *crd)
{
    SCM get_names = scm_c_eval_string("gnc:custom-report-template-names");
    SCM template_menu_name = scm_c_eval_string("gnc:report-template-menu-name/report-guid");
    SCM names;
    const gchar *name;
    int i;
    GtkTreeIter iter;

    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(store), COL_NAME, GTK_SORT_ASCENDING);

    crd->reportlist = scm_call_0(get_names);
    names = crd->reportlist;

    gtk_list_store_clear(store);

    if (scm_is_list(names))
    {
        /* for all the names in the list, store them, with a reference,
        	 in the gtkliststore */
        for (i = 0; !scm_is_null(names); i++)
        {
            char * str;

            scm_dynwind_begin (0);
            str = scm_to_locale_string (scm_call_2(template_menu_name, SCM_CAR(names), SCM_BOOL_F));
            name = g_strdup (str);
            scm_dynwind_free (str);
            scm_dynwind_end ();

            gtk_list_store_append(store, &iter);
            gtk_list_store_set(store, &iter,
                               COL_NAME, name,
                               COL_NUM, i,
                               -1);
            names = SCM_CDR(names);
        }
    }
}


static GtkTreeModel *
create_and_fill_report_list(CustomReportDialog *crd)
{
    GtkListStore *store;

    store = gtk_list_store_new(NUM_COLS, G_TYPE_STRING, G_TYPE_INT);

    update_report_list(store, crd);

    return GTK_TREE_MODEL (store);
}


static void
set_reports_model(CustomReportDialog *crd)
{
    GtkCellRenderer *renderer;
    GtkTreeModel *model;

    renderer = gtk_cell_renderer_text_new();

    gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (crd->reportview), -1, "Report Name", renderer, "text", COL_NAME, NULL);

    model = create_and_fill_report_list(crd);

    gtk_tree_view_set_model (GTK_TREE_VIEW (crd->reportview), model);

    g_object_unref(model);
}


/**************************************************************
 * custom_report_run_report
 *
 * this procedure sets up and calls the report on the scheme
 * side. This is what makes the report actually run.
 **************************************************************/
static void
custom_report_run_report(SCM guid,
                         CustomReportDialog *crd)
{
    SCM make_report = scm_c_eval_string("gnc:make-report");
    int report_id;
    GncMainWindow *window = crd->window;

    if (!scm_is_null(guid))
    {
        /* this runs the report */
        report_id = SCM_INUM(scm_call_1(make_report, guid));

        /* do this *before* the report because sometimes the report
        	 takes a while... */
        custom_report_dialog_close_cb(NULL, crd);

        /* display the report */
        gnc_main_window_open_report(report_id, window);
    }
}


/********************************************************************
 * get_custom_report_selection
 *
 * this helper function is called to get the selection when the user
 * clicks on "Run" or "Delete". Includes calling a dialog when there
 * is no selection.
 *
 * const gchar* message -- the message to provide user if there is no
 * actual selection found.
 *********************************************************************/
static SCM
get_custom_report_selection(CustomReportDialog *crd,
                            const gchar* message)
{
    GtkTreeSelection *sel;
    GtkTreeModel *model;
    GtkTreeIter iter;
    SCM guid;

    sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(crd->reportview));

    if (gtk_tree_selection_get_selected(sel, &model, &iter))
    {
        int num;

        gtk_tree_model_get(model, &iter, COL_NUM, &num, -1);
        guid = scm_list_ref(crd->reportlist, scm_int2num(num));
    }
    else
    {
        /* no selection, notify user */
        gnc_error_dialog(GTK_WIDGET(crd->window), "%s", message);
        return SCM_EOL;

    }
    return guid;
}


/**************************************************************
 * custom_report_list_view_row_activated_cb
 *
 * this is the double-click signal. No need to call
 * get_custom_report_selection as the double-click implies the
 * selection.
 **************************************************************/
void
custom_report_list_view_row_activated_cb(GtkTreeView *view, GtkTreePath *path,
        GtkTreeViewColumn *column, gpointer data)
{
    CustomReportDialog *crd = data;
    GtkTreeModel *model;
    GtkTreeIter iter;

    model = gtk_tree_view_get_model(view);

    if (gtk_tree_model_get_iter(model, &iter, path))
    {
        int num;
        SCM guid;

        gtk_tree_model_get(model, &iter, COL_NUM, &num, -1);

        guid = scm_list_ref(crd->reportlist, scm_int2num(num));

        custom_report_run_report(guid, crd);
    }
}


/*********************************************************************
 * delete_custom_report_clicked_cb
 *
 * this will delete the report, update the reports list and leave the
 * dialog active for additional usage.
 *********************************************************************/
void
delete_custom_report_clicked_cb(GtkWidget *button, gpointer data)
{
    CustomReportDialog *crd = data;

    SCM template_menu_name = scm_c_eval_string("gnc:report-template-menu-name/report-guid");
    SCM guid;
    gchar* report_name;

    guid = get_custom_report_selection(crd, _("You must select a report to delete."));
    if (!scm_is_null(guid))
    {
        char * str;

        scm_dynwind_begin (0);
        str = scm_to_locale_string(scm_call_2(template_menu_name, guid, SCM_BOOL_F));
        report_name = g_strdup (str);
        scm_dynwind_free (str);
        scm_dynwind_end ();

        /* we must confirm the user wants to delete their precious custom report! */
        if (gnc_verify_dialog(crd->dialog, FALSE, "Are you sure you want to delete %s?", report_name))
        {
            SCM del_report = scm_c_eval_string("gnc:delete-report");
            scm_call_1(del_report, guid);
            update_report_list(GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(crd->reportview))),
                               crd);
        }
        g_free (report_name);
    }
}


void
run_custom_report_clicked_cb (GtkWidget* button, gpointer data)
{
    CustomReportDialog *crd = data;
    SCM guid = get_custom_report_selection(crd, _("You must select a report to run."));
    custom_report_run_report(guid, crd);

}


/***********************************************************
 * gnc_ui_custom_report
 *
 * this is the primary driver for the custom report dialog.
 ***********************************************************/
void gnc_ui_custom_report(GncMainWindow * window)
{

    GtkBuilder *builder;
    CustomReportDialog *crd;

    crd = g_new0(CustomReportDialog, 1);

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-custom-report.glade", "custom_report_dialog");

    crd->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "custom_report_dialog"));
    crd->reportview = GTK_WIDGET(gtk_builder_get_object (builder, "custom_report_list_view"));
    set_reports_model(crd);
    crd->window = window;

    /* connect the signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, crd);

    gtk_widget_show_all(crd->dialog);

    g_object_unref(G_OBJECT(builder));
}
