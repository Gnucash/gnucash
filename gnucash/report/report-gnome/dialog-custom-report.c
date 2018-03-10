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

#include <config.h>

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
#include "gnc-guile-utils.h"
#include "gnc-gui-query.h"
#include "gnc-ui.h"
#include "gnc-report.h"
#include "gnc-plugin-page-report.h"

#define GNC_PREFS_GROUP_REPORT_SAVED_CONFIGS "dialogs.report-saved-configs"

/* convenience for accessing columns in the GtkListStore that holds
   the reports */
enum
{
    COL_NAME = 0,
    COL_NUM,
    NUM_COLS
};

enum
{
    VIEW_COL_NAME = 0,
    VIEW_COL_RUN,
    VIEW_COL_EDIT,
    VIEW_COL_DELETE,
    NUM_VIEW_COLS
};

/* all the pertinent stuff needed to pass around */
typedef struct _CustomReportDialog
{
    /* dialog */
    GtkWidget *dialog;
    GtkWidget *reportview;
    GncMainWindow *window;
    GtkTreeViewColumn *namecol;
    GtkCellRenderer *namerenderer;
    GtkTreeViewColumn *runcol;
    GtkTreeViewColumn *editcol;
    GtkTreeViewColumn *delcol;

    /* data */
    SCM reportlist;

} CustomReportDialog;

void custom_report_dialog_close_cb(GtkWidget* widget, gpointer data);
void custom_report_help_cb(GtkWidget* widget, gpointer data);
void close_custom_report_clicked_cb(GtkWidget* widget, gpointer data);
void custom_report_list_view_row_activated_cb(GtkTreeView *view, GtkTreePath *path,
        GtkTreeViewColumn *column, gpointer data);
void custom_report_list_view_clicked_cb(GtkTreeView *view, GdkEventButton *event, gpointer data);
void custom_report_name_edited_cb(GtkCellRendererText *renderer, gchar *path, gchar *new_text, gpointer data);
void custom_report_query_tooltip_cb (GtkTreeView  *view,
                                     gint        x,
                                     gint        y,
                                     gboolean    keyboard_mode,
                                     GtkTooltip *tooltip,
                                     gpointer    data);

void
custom_report_dialog_close_cb(GtkWidget* widget, gpointer data)
{
    CustomReportDialog *crd = data;
    gnc_save_window_size(GNC_PREFS_GROUP_REPORT_SAVED_CONFIGS, GTK_WINDOW(crd->dialog));

    gtk_widget_destroy(crd->dialog);
    g_free(crd);
}

void
custom_report_help_cb (GtkWidget *widget, gpointer data)
{
    gnc_gnome_help(HF_HELP, HL_USAGE_CUSTOMREP);
}

void
close_custom_report_clicked_cb(GtkWidget* widget, gpointer data)
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
    SCM get_rpt_guids = scm_c_eval_string("gnc:custom-report-template-guids");
    SCM template_menu_name = scm_c_eval_string("gnc:report-template-menu-name/report-guid");
    SCM rpt_guids;
    int i;
    GtkTreeIter iter;
    GtkTreeModel *model = GTK_TREE_MODEL (store);
    gboolean valid_iter;

    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(store), COL_NAME, GTK_SORT_ASCENDING);

    crd->reportlist = scm_call_0(get_rpt_guids);
    rpt_guids = crd->reportlist;

    /* Empty current liststore */
    valid_iter = gtk_tree_model_get_iter_first (model, &iter);
    while (valid_iter)
    {
        GValue value = { 0, };
        GncGUID *row_guid;
        gtk_tree_model_get_value (model, &iter, COL_NUM, &value);
        row_guid = (GncGUID *) g_value_get_pointer (&value);
        guid_free (row_guid);
        g_value_unset (&value);
        valid_iter = gtk_tree_model_iter_next (model, &iter);
    }
    gtk_list_store_clear(store);

    if (scm_is_list(rpt_guids))
    {
        /* for all the report guids in the list, store them, with a reference,
        	 in the gtkliststore */
        for (i = 0; !scm_is_null(rpt_guids); i++)
        {
            GncGUID *guid = guid_malloc ();
            gchar *guid_str = scm_to_utf8_string (SCM_CAR(rpt_guids));
            gchar *name = gnc_scm_to_utf8_string (scm_call_2(template_menu_name, SCM_CAR(rpt_guids), SCM_BOOL_F));

            if (string_to_guid (guid_str, guid))
            {
                gtk_list_store_append(store, &iter);
                gtk_list_store_set(store, &iter,
                                   COL_NAME, name,
                                   COL_NUM, guid,
                                   -1);
            }
            g_free (name);
            g_free (guid_str);

            rpt_guids = SCM_CDR(rpt_guids);
        }
    }
}


static GtkTreeModel *
create_and_fill_report_list(CustomReportDialog *crd)
{
    GtkListStore *store;

    store = gtk_list_store_new(NUM_COLS, G_TYPE_STRING, G_TYPE_POINTER);

    update_report_list(store, crd);

    return GTK_TREE_MODEL (store);
}


static void
set_reports_view_and_model(CustomReportDialog *crd)
{
    GtkCellRenderer *renderer;
    GtkTreeModel *model;

    crd->namerenderer = gtk_cell_renderer_text_new();
    g_signal_connect (G_OBJECT (crd->namerenderer), "edited",
                      G_CALLBACK (custom_report_name_edited_cb), crd);
    gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (crd->reportview), -1,
            "Report Name", crd->namerenderer,
            "text", COL_NAME,
            NULL);
    crd->namecol = gtk_tree_view_get_column (GTK_TREE_VIEW (crd->reportview), VIEW_COL_NAME);
    gtk_tree_view_column_set_expand (crd->namecol, TRUE);

    renderer = gtk_cell_renderer_pixbuf_new();
    g_object_set (G_OBJECT (renderer), "icon-name", "system-run", NULL);
    gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (crd->reportview), -1,
            "R", renderer,
            NULL);
    crd->runcol = gtk_tree_view_get_column (GTK_TREE_VIEW (crd->reportview), VIEW_COL_RUN);

    renderer = gtk_cell_renderer_pixbuf_new();
    g_object_set (G_OBJECT (renderer), "icon-name", "accessories-text-editor", NULL);
    gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (crd->reportview), -1,
            "E", renderer,
            NULL);
    crd->editcol = gtk_tree_view_get_column (GTK_TREE_VIEW (crd->reportview), VIEW_COL_EDIT);

    renderer = gtk_cell_renderer_pixbuf_new();
    g_object_set (G_OBJECT (renderer), "icon-name", "edit-delete", NULL);
    gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (crd->reportview), -1,
             "D", renderer,
             NULL);
    crd->delcol = gtk_tree_view_get_column (GTK_TREE_VIEW (crd->reportview), VIEW_COL_DELETE);

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

    if (scm_is_null(guid))
        return;

    /* this generates the report */
    report_id = scm_to_int (scm_call_1(make_report, guid));

    /* do this *before* displaying the report because sometimes that
         takes a while... */
    custom_report_dialog_close_cb(NULL, crd);

    /* display the report */
    gnc_main_window_open_report(report_id, window);

}

/**************************************************************
 * custom_report_run_report
 *
 * this procedure sets up and calls the report on the scheme
 * side. This is what makes the report actually run.
 **************************************************************/
static void
custom_report_edit_report_name (SCM guid,
                                CustomReportDialog *crd,
                                gchar *new_name)
{
    SCM rename_report = scm_c_eval_string("gnc:rename-report");
    SCM new_name_scm = scm_from_utf8_string(new_name);

    if (scm_is_null(guid) || !new_name || (*new_name == '\0'))
        return;

    /* rename the report */
    scm_call_2(rename_report, guid, new_name_scm);
    update_report_list(GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(crd->reportview))),
                       crd);

}

/*********************************************************************
 * custom_report_delete
 *
 * this will delete the report, update the reports list and leave the
 * dialog active for additional usage.
 *********************************************************************/
static void
custom_report_delete (SCM guid, CustomReportDialog *crd)
{
    SCM template_menu_name = scm_c_eval_string("gnc:report-template-menu-name/report-guid");
    gchar *report_name;

    if (scm_is_null (guid))
        return;

    report_name = gnc_scm_to_utf8_string(scm_call_2(template_menu_name, guid, SCM_BOOL_F));

    /* we must confirm the user wants to delete their precious custom report! */
    if (gnc_verify_dialog( GTK_WINDOW (crd->dialog), FALSE, _("Are you sure you want to delete %s?"), report_name))
    {
        SCM del_report = scm_c_eval_string("gnc:delete-report");
        scm_call_1(del_report, guid);
        update_report_list(GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(crd->reportview))),
                           crd);
    }
    g_free (report_name);
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
    GncGUID *guid = guid_malloc ();
    gchar *guid_str;

    sel = gtk_tree_view_get_selection(GTK_TREE_VIEW(crd->reportview));

    if (gtk_tree_selection_get_selected(sel, &model, &iter))
    {
        gtk_tree_model_get(model, &iter, COL_NUM, &guid, -1);
        guid_str = g_new0 (gchar, GUID_ENCODING_LENGTH+1 );
        guid_to_string_buff (guid, guid_str);
    }
    else
    {
        /* no selection, notify user */
        gnc_error_dialog (GTK_WINDOW (crd->dialog), "%s", message);
        return SCM_EOL;

    }
    return scm_from_utf8_string (guid_str);
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
        GncGUID *guid = guid_malloc ();
        gchar *guid_str;

        gtk_tree_model_get(model, &iter, COL_NUM, &guid, -1);
        guid_str = g_new0 (gchar, GUID_ENCODING_LENGTH+1 );
        guid_to_string_buff (guid, guid_str);

        custom_report_run_report(scm_from_utf8_string (guid_str), crd);
    }
}


/**************************************************************
 * custom_report_list_view_clicked_cb
 *
 * this callback is called whenever a user clicked somewhere in
 * the treeview widget. If the click was on an edit or delete
 * pictogram, the corresponding action will be executed on the
 * selected row.
 **************************************************************/
void
custom_report_list_view_clicked_cb(GtkTreeView *view, GdkEventButton *event, gpointer data)
{
    CustomReportDialog *crd = data;
    GtkTreePath *path = NULL;
    GtkTreeViewColumn *column = NULL;
    gint cellx, celly;

    g_return_if_fail ( view != NULL );

    if (gtk_tree_view_get_path_at_pos (view, event->x, event->y,
                                       &path, &column,
                                       &cellx, &celly))
    {
        if (column == crd->runcol)
        {
            SCM guid = get_custom_report_selection(crd, _("You must select a report configuration to load."));
            custom_report_run_report (guid, crd);
        }
        else if (column == crd->editcol)
        {
            g_object_set(G_OBJECT(crd->namerenderer), "editable", TRUE, NULL);
            gtk_tree_view_set_cursor_on_cell (view, path, crd->namecol,
                                              crd->namerenderer, TRUE);
        }
        else if (column == crd->delcol)
        {
            SCM guid = get_custom_report_selection(crd, _("You must select a report configuration to delete."));
            custom_report_delete (guid, crd);
        }
    }
}

void custom_report_name_edited_cb(GtkCellRendererText *renderer, gchar *path, gchar *new_text, gpointer data)
{
    CustomReportDialog *crd = data;
    SCM guid = get_custom_report_selection(crd, _("Unable to change report configuration name."));
    SCM unique_name_func = scm_c_eval_string("gnc:report-template-has-unique-name?");
    SCM new_name_scm = scm_from_utf8_string(new_text);

    g_object_set(G_OBJECT(crd->namerenderer), "editable", FALSE, NULL);
    if (scm_is_null (guid))
        return;

    if (scm_is_true (scm_call_2 (unique_name_func, guid, new_name_scm)))
        custom_report_edit_report_name (guid, crd, new_text);
    else
        gnc_error_dialog (GTK_WINDOW (crd->dialog), "%s",
                          _("A saved report configuration with this name already exists, please choose another name.") );


}
void custom_report_query_tooltip_cb (GtkTreeView  *view,
                                     gint        x,
                                     gint        y,
                                     gboolean    keyboard_mode,
                                     GtkTooltip *tooltip,
                                     gpointer    data)
{
    CustomReportDialog *crd = data;
    GtkTreePath *path = NULL;
    GtkTreeViewColumn *column = NULL;
    gint cellx, celly;

    g_return_if_fail ( view != NULL );

    if (gtk_tree_view_get_path_at_pos (view, x, y,
                                       &path, &column,
                                       &cellx, &celly))
    {
        gtk_tree_view_set_tooltip_cell (view, tooltip, path, column, NULL);
        if (column == crd->runcol)
            gtk_tooltip_set_text (tooltip, _("Load report configuration"));
        else if (column == crd->editcol)
            gtk_tooltip_set_text (tooltip, _("Edit report configuration name"));
        else if (column == crd->delcol)
            gtk_tooltip_set_text (tooltip, _("Delete report configuration"));
        else
            gtk_tooltip_set_text (tooltip, NULL);
    }

}

/* Internal function that builds the dialog */
static CustomReportDialog *gnc_ui_custom_report_internal(GncMainWindow * window)
{

    GtkBuilder *builder;
    CustomReportDialog *crd;
    GtkTreeIter iter;
    GtkTreeModel *model;
    GtkWidget *no_report_notification;

    crd = g_new0(CustomReportDialog, 1);

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-custom-report.glade", "custom_report_dialog");

    crd->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "custom_report_dialog"));
    crd->reportview = GTK_WIDGET(gtk_builder_get_object (builder, "custom_report_list_view"));
    no_report_notification = GTK_WIDGET(gtk_builder_get_object (builder, "label2"));
    set_reports_view_and_model(crd);
    crd->window = window;

    gtk_window_set_transient_for (GTK_WINDOW (crd->dialog), GTK_WINDOW(window));

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(crd->dialog), "GncCustomReportDialog");

    gnc_restore_window_size (GNC_PREFS_GROUP_REPORT_SAVED_CONFIGS, GTK_WINDOW(crd->dialog));

    /* connect the signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, crd);

    gtk_widget_show_all(crd->dialog);

    /* check if there are currently saved reports available
     * by checking if there is a first element */
    model = gtk_tree_view_get_model (GTK_TREE_VIEW (crd->reportview));
    if (gtk_tree_model_get_iter_first (model, &iter))
    {
		/* saved reports available
		   -> hide the "no reports available" notification */
		gtk_widget_hide(no_report_notification);
	}
	else
	{
		/* hide the scrolled window of the report list */
		gtk_widget_hide(crd->reportview);
	}

    g_object_unref(G_OBJECT(builder));

    return crd;
}


/***********************************************************
 * gnc_ui_custom_report
 *
 * this is the primary driver for the custom report dialog.
 ***********************************************************/
void gnc_ui_custom_report(GncMainWindow * window)
{
    gnc_ui_custom_report_internal (window);
}


/***********************************************************
 * gnc_ui_custom_report_edit_name
 *
 * open the custom report dialog and highlight the given
 * report's name for editing.
 ***********************************************************/
void gnc_ui_custom_report_edit_name (GncMainWindow * window, SCM scm_guid)
{
    SCM is_custom_report;
    CustomReportDialog *crd = gnc_ui_custom_report_internal (window);
    GtkTreeModel *model;
    GtkTreeIter iter;
    GncGUID *guid;
    gchar *guid_str;
    gboolean valid_iter;

    is_custom_report = scm_c_eval_string ("gnc:report-template-is-custom/template-guid?");
    if (scm_is_false (scm_call_1 (is_custom_report, scm_guid)))
        return;

    guid = guid_malloc ();
    guid_str = scm_to_utf8_string (scm_guid);
    if (!string_to_guid (guid_str, guid))
        goto cleanup;

    /* Look up the row for the requested guid */
    model = gtk_tree_view_get_model (GTK_TREE_VIEW (crd->reportview));
    valid_iter = gtk_tree_model_get_iter_first (model, &iter);

    while (valid_iter)
    {
        GValue value = { 0, };
        GncGUID *row_guid;
        gtk_tree_model_get_value (model, &iter, COL_NUM, &value);
        row_guid = (GncGUID *) g_value_get_pointer (&value);

        if (guid_equal (guid, row_guid))
        {
            /* We found the row for the requested guid
             * Now let's set the report's name cell in edit mode
             * so the user can edit the name.
             */
            GtkTreePath *path;
            GtkTreeSelection *selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (crd->reportview));
            gtk_tree_selection_select_iter (selection, &iter);
            path = gtk_tree_model_get_path (model, &iter);
            g_object_set(G_OBJECT(crd->namerenderer), "editable", TRUE, NULL);
            gtk_tree_view_set_cursor_on_cell (GTK_TREE_VIEW (crd->reportview),
                                              path, crd->namecol,
                                              crd->namerenderer, TRUE);
            break;
        }

        g_value_unset (&value);
        valid_iter = gtk_tree_model_iter_next (model, &iter);
    }

cleanup:
    guid_free (guid);
}
