/********************************************************************
 * dialog-report-column-view.c -- editor for column view of reports *
 * Copyright (C) 2001 Bill Gribble <grib@billgribble.com>           *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
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
 ********************************************************************/

#include <config.h>

#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <libguile.h>
#include "swig-runtime.h"

#include "dialog-report-column-view.h"
#include "dialog-options.h"
#include "dialog-utils.h"
#include "option-util.h"
#include "window-report.h"
#include "guile-mappings.h"
#include "gnc-guile-utils.h"
#include "gnc-report.h"
#include "gnc-ui.h"

enum available_cols
{
    AVAILABLE_COL_NAME = 0,
    AVAILABLE_COL_GUID,
    NUM_AVAILABLE_COLS
};

enum contents_cols
{
    CONTENTS_COL_NAME = 0,
    CONTENTS_COL_ROW,
    CONTENTS_COL_REPORT_ROWS,
    CONTENTS_COL_REPORT_COLS,
    NUM_CONTENTS_COLS
};

struct gncp_column_view_edit
{
    GNCOptionWin * optwin;
    GtkTreeView  * available;
    GtkTreeView  * contents;

    SCM          options;
    SCM          view;
    GNCOptionDB  * odb;

    SCM       available_list;
    SCM       contents_list;
    int       contents_selected;

    GtkWidget *add_button;
    GtkWidget *remove_button;
    GtkWidget *up_button;
    GtkWidget *down_button;
    GtkWidget *size_button;
};

void gnc_column_view_edit_add_cb(GtkButton * button, gpointer user_data);
void gnc_column_view_edit_remove_cb(GtkButton * button, gpointer user_data);
void gnc_edit_column_view_move_up_cb(GtkButton * button, gpointer user_data);
void gnc_edit_column_view_move_down_cb(GtkButton * button, gpointer user_data);
void gnc_column_view_edit_size_cb(GtkButton * button, gpointer user_data);

static void
gnc_column_view_set_option(GNCOptionDB * odb, char * section, char * name,
                           SCM new_value)
{
    GNCOption *  option =
        gnc_option_db_get_option_by_name(odb, section, name);

    if (option)
    {
        gnc_option_db_set_option(odb, section, name, new_value);

        /* set_option doesn't do this */
        gnc_option_set_changed (option, TRUE);
    }
}

static void
gnc_column_view_edit_destroy(gnc_column_view_edit * view)
{
    gnc_options_dialog_destroy(view->optwin);
    scm_gc_unprotect_object(view->options);
    scm_gc_unprotect_object(view->view);
    gnc_option_db_destroy(view->odb);
    g_free(view);
}

static void
update_available_lists(gnc_column_view_edit * view)
{
    SCM   get_rpt_guids = scm_c_eval_string("gnc:all-report-template-guids");
    SCM   template_menu_name = scm_c_eval_string("gnc:report-template-menu-name/report-guid");
    SCM   rpt_guids = scm_call_0(get_rpt_guids);
    SCM   selection;

    gchar *name;
    gchar *guid_str;

    GtkTreeModel *model;
    GtkListStore *store;
    GtkTreeIter iter;
    GtkTreeSelection *tree_selection;

    /* Update the list of available reports (left selection box). */
    tree_selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(view->available));
    model = gtk_tree_view_get_model (GTK_TREE_VIEW(view->available));

    if (gtk_tree_selection_get_selected(tree_selection, &model, &iter))
    {
        gchar *guid_str;
        gtk_tree_model_get(model, &iter,
                           AVAILABLE_COL_GUID, &guid_str,
                           -1);
        selection = scm_from_utf8_string(guid_str);
        g_free (guid_str);
    }
    else
        selection = SCM_UNDEFINED;

    scm_gc_unprotect_object(view->available_list);
    view->available_list = rpt_guids;
    scm_gc_protect_object(view->available_list);

    store = GTK_LIST_STORE(model);
    gtk_list_store_clear(store);

    if (scm_is_list(rpt_guids))
    {
        for (int i = 0; !scm_is_null(rpt_guids); rpt_guids = SCM_CDR(rpt_guids), i++)
        {
            SCM rpt_guids_temp = SCM_CAR(rpt_guids);

            guid_str = scm_to_utf8_string (rpt_guids_temp);
            name = gnc_scm_to_utf8_string (scm_call_2(template_menu_name, rpt_guids_temp,
                                             SCM_BOOL_F));

            gtk_list_store_append(store, &iter);
            gtk_list_store_set(store, &iter,
                               AVAILABLE_COL_NAME, _(name),
                               AVAILABLE_COL_GUID, guid_str,
                               -1);

            if (scm_is_equal (rpt_guids_temp, selection))
                gtk_tree_selection_select_iter (tree_selection, &iter);

            g_free (name);
            g_free (guid_str);
        }
    }
}

static void
update_contents_lists(gnc_column_view_edit * view)
{
    SCM   report_menu_name = scm_c_eval_string("gnc:report-menu-name");
    SCM   contents =
        gnc_option_db_lookup_option(view->odb, "__general", "report-list",
                                    SCM_BOOL_F);
    SCM   this_report;
    SCM   selection;
    gchar *name;

    GtkListStore *store;
    GtkTreeIter iter;
    GtkTreeSelection *tree_selection;

    /* Update the list of selected reports (right selection box). */
    tree_selection = gtk_tree_view_get_selection(view->contents);

    if (scm_is_list(view->contents_list) && !scm_is_null (view->contents_list))
    {
        int row = view->contents_selected;
        row = MIN (row, scm_ilength (view->contents_list) - 1);
        selection = scm_list_ref (view->contents_list, scm_from_int (row));
    }
    else
        selection = SCM_UNDEFINED;

    scm_gc_unprotect_object(view->contents_list);
    view->contents_list = contents;
    scm_gc_protect_object(view->contents_list);

    store = GTK_LIST_STORE(gtk_tree_view_get_model(view->contents));
    gtk_list_store_clear(store);

    if (scm_is_list(contents))
    {
        for (int i = 0; !scm_is_null(contents); contents = SCM_CDR(contents), i++)
        {
            SCM contents_temp = SCM_CAR(contents);

            int id = scm_to_int(SCM_CAAR(contents));

            this_report = gnc_report_find(id);
            name = gnc_scm_to_utf8_string (scm_call_1(report_menu_name, this_report));

            gtk_list_store_append(store, &iter);
            gtk_list_store_set
            (store, &iter,
             CONTENTS_COL_NAME, _(name),
             CONTENTS_COL_ROW, i,
             CONTENTS_COL_REPORT_COLS, scm_to_int(SCM_CADR(contents_temp)),
             CONTENTS_COL_REPORT_ROWS, scm_to_int(SCM_CADDR(contents_temp)),
             -1);

            if (scm_is_equal (contents_temp, selection))
                gtk_tree_selection_select_iter (tree_selection, &iter);

            g_free (name);
        }
    }
}

static void
gnc_column_view_update_buttons_cb (GtkTreeSelection *selection,
                                       gnc_column_view_edit *r)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    gboolean is_selected;

    /* compare treeviews to establish which selected treeview */
    if (gtk_tree_selection_get_tree_view (selection) == r->available)
    {
        /* available treeview */
        is_selected = gtk_tree_selection_get_selected (selection, &model, &iter);
        gtk_widget_set_sensitive (r->add_button, is_selected);
        return;
    }

    /* contents treeview */
    is_selected = gtk_tree_selection_get_selected (selection, &model, &iter);
    gtk_widget_set_sensitive (r->size_button, is_selected);
    gtk_widget_set_sensitive (r->remove_button, is_selected);

    if (is_selected)
    {
        int len = scm_ilength (r->contents_list);

        gtk_tree_model_get (model, &iter,
                           CONTENTS_COL_ROW, &r->contents_selected, -1);

        if (len > 1)
        {
            gtk_widget_set_sensitive (r->up_button, TRUE);
            gtk_widget_set_sensitive (r->down_button, TRUE);

            if (r->contents_selected == len -1)
                gtk_widget_set_sensitive (r->down_button, FALSE);

            if (r->contents_selected == 0)
                gtk_widget_set_sensitive (r->up_button, FALSE);
        }
    }
    else
    {
        gtk_widget_set_sensitive (r->up_button, FALSE);
        gtk_widget_set_sensitive (r->down_button, FALSE);
    }
}

static void
gnc_column_view_edit_apply_cb(GNCOptionWin * w, gpointer user_data)
{
    SCM  dirty_report = scm_c_eval_string("gnc:report-set-dirty?!");
    gnc_column_view_edit * win = user_data;
    GList *results = NULL, *iter;

    if (!win) return;
    results = gnc_option_db_commit (win->odb);
    for (iter = results; iter; iter = iter->next)
    {
        GtkWidget *dialog = gtk_message_dialog_new(GTK_WINDOW(gnc_options_dialog_widget(w)),
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

    scm_call_2(dirty_report, win->view, SCM_BOOL_T);
}

static void
gnc_column_view_edit_close_cb(GNCOptionWin * win, gpointer user_data)
{
    gnc_column_view_edit * r = user_data;
    SCM set_editor = scm_c_eval_string("gnc:report-set-editor-widget!");

    scm_call_2(set_editor, r->view, SCM_BOOL_F);
    gnc_column_view_edit_destroy(r);
}


/********************************************************************
 * gnc_column_view_edit_options
 * create the editor.
 ********************************************************************/

GtkWidget *
gnc_column_view_edit_options(SCM options, SCM view)
{
    SCM get_editor = scm_c_eval_string("gnc:report-editor-widget");
    SCM ptr;
    GtkWidget * editor;
    GtkListStore *store;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;

    ptr = scm_call_1(get_editor, view);
    if (ptr != SCM_BOOL_F)
    {
#define FUNC_NAME "gtk_window_present"
        GtkWindow * w = SWIG_MustGetPtr(ptr, SWIG_TypeQuery("_p_GtkWidget"), 1, 0);
        gtk_window_present(w);
#undef FUNC_NAME
        return NULL;
    }
    else
    {
        gnc_column_view_edit * r = g_new0(gnc_column_view_edit, 1);
        GtkBuilder *builder;

        r->optwin = gnc_options_dialog_new (NULL, GTK_WINDOW(gnc_ui_get_main_window (NULL)));

        /* Hide the generic dialog page list. */
        gtk_widget_hide(gnc_options_page_list(r->optwin));

        builder = gtk_builder_new();
        gnc_builder_add_from_file (builder, "dialog-report.glade", "view_contents_table");

        editor       = GTK_WIDGET(gtk_builder_get_object (builder, "view_contents_table"));
        r->available = GTK_TREE_VIEW (gtk_builder_get_object (builder, "available_view"));
        r->contents  = GTK_TREE_VIEW (gtk_builder_get_object (builder, "contents_view"));

        r->add_button = GTK_WIDGET(gtk_builder_get_object (builder, "add_button1"));
        r->remove_button = GTK_WIDGET(gtk_builder_get_object (builder, "remove_button1"));
        r->up_button = GTK_WIDGET(gtk_builder_get_object (builder, "up_button1"));
        r->down_button = GTK_WIDGET(gtk_builder_get_object (builder, "down_button1"));
        r->size_button = GTK_WIDGET(gtk_builder_get_object (builder, "size_button1"));

        r->options   = options;
        r->view      = view;
        r->available_list = SCM_EOL;
        r->contents_selected = 0;
        r->contents_list = SCM_EOL;
        r->odb       = gnc_option_db_new(r->options);

        gnc_options_dialog_build_contents(r->optwin, r->odb);

        gtk_notebook_append_page(GTK_NOTEBOOK(gnc_options_dialog_notebook
                                              (r->optwin)),
                                 editor,
                                 gtk_label_new(_("Contents")));

        scm_gc_protect_object(r->options);
        scm_gc_protect_object(r->view);
        scm_gc_protect_object(r->available_list);
        scm_gc_protect_object(r->contents_list);

        /* Build the 'available' view */
        store = gtk_list_store_new (NUM_AVAILABLE_COLS, G_TYPE_STRING, G_TYPE_STRING);
        gtk_tree_view_set_model(r->available, GTK_TREE_MODEL(store));
        gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(store), AVAILABLE_COL_NAME, GTK_SORT_ASCENDING);
        g_object_unref(store);

        renderer = gtk_cell_renderer_text_new();
        column = gtk_tree_view_column_new_with_attributes("", renderer,
                 "text", AVAILABLE_COL_NAME,
                 NULL);
        gtk_tree_view_append_column(r->available, column);

        /* use the selection cb to update buttons */
        selection = gtk_tree_view_get_selection(r->available);
        g_signal_connect(selection, "changed",
                         G_CALLBACK(gnc_column_view_update_buttons_cb), r);

        /* Build the 'contents' view */
        store = gtk_list_store_new (NUM_CONTENTS_COLS, G_TYPE_STRING, G_TYPE_INT,
                                    G_TYPE_INT, G_TYPE_INT);
        gtk_tree_view_set_model(r->contents, GTK_TREE_MODEL(store));
        g_object_unref(store);

        renderer = gtk_cell_renderer_text_new();
        column = gtk_tree_view_column_new_with_attributes(_("Report"), renderer,
                 "text", CONTENTS_COL_NAME,
                 NULL);
        gtk_tree_view_append_column(r->contents, column);

        renderer = gtk_cell_renderer_text_new();
        column = gtk_tree_view_column_new_with_attributes(_("Rows"), renderer,
                 "text", CONTENTS_COL_REPORT_ROWS,
                 NULL);
        gtk_tree_view_append_column(r->contents, column);

        renderer = gtk_cell_renderer_text_new();
        column = gtk_tree_view_column_new_with_attributes(_("Cols"), renderer,
                 "text", CONTENTS_COL_REPORT_COLS,
                 NULL);
        gtk_tree_view_append_column(r->contents, column);

        /* use the selection cb to update buttons */
        selection = gtk_tree_view_get_selection(r->contents);
        g_signal_connect(selection, "changed",
                         G_CALLBACK(gnc_column_view_update_buttons_cb), r);

        update_available_lists(r);
        update_contents_lists(r);

        gnc_options_dialog_set_apply_cb(r->optwin,
                                        gnc_column_view_edit_apply_cb, r);
        gnc_options_dialog_set_close_cb(r->optwin,
                                        gnc_column_view_edit_close_cb, r);

        gtk_widget_show(gnc_options_dialog_widget(r->optwin));

        gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, r);

        g_object_unref(G_OBJECT(builder));

        return gnc_options_dialog_widget(r->optwin);
    }
}

void
gnc_column_view_edit_add_cb(GtkButton * button, gpointer user_data)
{
    gnc_column_view_edit * r = user_data;
    SCM make_report = scm_c_eval_string("gnc:make-report");
    SCM mark_report = scm_c_eval_string("gnc:report-set-needs-save?!");
    SCM template_name;
    SCM new_report;
    SCM newlist = SCM_EOL;
    SCM oldlist = r->contents_list;
    int count;
    int oldlength, id;
    gchar *guid_str;
    GtkTreeSelection *selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(r->available));
    GtkTreeModel *model;
    GtkTreeIter iter;

    /* make sure there is a selected entry */
    if (gtk_tree_selection_get_selected(selection, &model, &iter))
        gtk_tree_model_get(model, &iter,
                           AVAILABLE_COL_GUID, &guid_str, -1);
    else
        return;

    if (scm_is_list(r->available_list))
    {
        template_name = scm_from_utf8_string(guid_str);

        new_report = scm_call_1(make_report, template_name);
        id = scm_to_int(new_report);
        scm_call_2(mark_report, gnc_report_find(id), SCM_BOOL_T);
        oldlength = scm_ilength(r->contents_list);

        if (oldlength > r->contents_selected)
        {
            for (count = 0; count < r->contents_selected; count++)
            {
                newlist = scm_cons(SCM_CAR(oldlist), newlist);
                oldlist = SCM_CDR(oldlist);
            }
            newlist = scm_append
                      (scm_list_n (scm_reverse(scm_cons(SCM_LIST4(new_report,
                                               scm_from_int (1),
                                               scm_from_int (1),
                                               SCM_BOOL_F),
                                               newlist)),
                                   oldlist,
                                   SCM_UNDEFINED));
        }
        else
        {
            newlist = scm_append
                      (scm_list_n (oldlist,
                                   SCM_LIST1(SCM_LIST4(new_report,
                                             scm_from_int (1),
                                             scm_from_int (1),
                                             SCM_BOOL_F)),
                                   SCM_UNDEFINED));
            r->contents_selected = oldlength;
        }

        scm_gc_unprotect_object(r->contents_list);
        r->contents_list = newlist;
        scm_gc_protect_object(r->contents_list);

        gnc_column_view_set_option(r->odb, "__general", "report-list",
                                   r->contents_list);
        gnc_options_dialog_changed (r->optwin);
    }
    g_free (guid_str);
    update_contents_lists(r);
}

void
gnc_column_view_edit_remove_cb(GtkButton * button, gpointer user_data)
{
    gnc_column_view_edit * r = user_data;
    SCM newlist = SCM_EOL;
    SCM oldlist = r->contents_list;
    int count;
    int oldlength;

    if (scm_is_list(r->contents_list))
    {
        oldlength = scm_ilength(r->contents_list);
        if (oldlength > r->contents_selected)
        {
            for (count = 0; count < r->contents_selected; count++)
            {
                newlist = scm_cons(SCM_CAR(oldlist), newlist);
                oldlist = SCM_CDR(oldlist);
            }
            if (count <= oldlength)
            {
                newlist = scm_append(scm_list_n (scm_reverse(newlist), SCM_CDR(oldlist), SCM_UNDEFINED));
            }
        }

        if (r->contents_selected > 0 && oldlength == r->contents_selected + 1)
        {
            r->contents_selected --;
        }

        scm_gc_unprotect_object(r->contents_list);
        r->contents_list = newlist;
        scm_gc_protect_object(r->contents_list);

        gnc_column_view_set_option(r->odb, "__general", "report-list",
                                   r->contents_list);

        gnc_options_dialog_changed (r->optwin);
    }
    update_contents_lists(r);
}

void
gnc_edit_column_view_move_up_cb(GtkButton * button, gpointer user_data)
{
    gnc_column_view_edit * r = user_data;
    SCM oldlist = r->contents_list;
    SCM newlist = SCM_EOL;
    SCM temp;
    int oldlength;
    int count;

    oldlength = scm_ilength(r->contents_list);
    if ((r->contents_selected > 0) && (oldlength > r->contents_selected))
    {
        for (count = 1; count < r->contents_selected; count++)
        {
            newlist = scm_cons(SCM_CAR(oldlist), newlist);
            oldlist = SCM_CDR(oldlist);
        }
        temp = SCM_CAR(oldlist);
        oldlist = SCM_CDR(oldlist);
        newlist = scm_cons(temp, scm_cons(SCM_CAR(oldlist), newlist));
        newlist = scm_append(scm_list_n (scm_reverse(newlist), SCM_CDR(oldlist), SCM_UNDEFINED));

        scm_gc_unprotect_object(r->contents_list);
        r->contents_list = newlist;
        scm_gc_protect_object(r->contents_list);

        r->contents_selected = r->contents_selected - 1;

        gnc_column_view_set_option(r->odb, "__general", "report-list",
                                   r->contents_list);

        gnc_options_dialog_changed (r->optwin);

        update_contents_lists(r);
    }
}

void
gnc_edit_column_view_move_down_cb(GtkButton * button, gpointer user_data)
{
    gnc_column_view_edit * r = user_data;
    SCM oldlist = r->contents_list;
    SCM newlist = SCM_EOL;
    SCM temp;
    int oldlength;
    int count;

    oldlength = scm_ilength(r->contents_list);
    if (oldlength > (r->contents_selected + 1))
    {
        for (count = 0; count < r->contents_selected; count++)
        {
            newlist = scm_cons(SCM_CAR(oldlist), newlist);
            oldlist = SCM_CDR(oldlist);
        }
        temp = SCM_CAR(oldlist);
        oldlist = SCM_CDR(oldlist);
        newlist = scm_cons(temp, scm_cons(SCM_CAR(oldlist), newlist));
        newlist = scm_append(scm_list_n (scm_reverse(newlist), SCM_CDR(oldlist), SCM_UNDEFINED));

        scm_gc_unprotect_object(r->contents_list);
        r->contents_list = newlist;
        scm_gc_protect_object(r->contents_list);

        r->contents_selected = r->contents_selected + 1;

        gnc_column_view_set_option(r->odb, "__general", "report-list",
                                   r->contents_list);

        gnc_options_dialog_changed (r->optwin);

        update_contents_lists(r);
    }
}

void
gnc_column_view_edit_size_cb(GtkButton * button, gpointer user_data)
{
    gnc_column_view_edit * r = user_data;
    GtkWidget * rowspin;
    GtkWidget * colspin;
    GtkWidget * dlg;
    GtkBuilder *builder;
    SCM current;
    int length;
    int dlg_ret;

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-report.glade", "col_adjustment");
    gnc_builder_add_from_file (builder, "dialog-report.glade", "row_adjustment");
    gnc_builder_add_from_file (builder, "dialog-report.glade", "edit_report_size");
    dlg = GTK_WIDGET(gtk_builder_get_object (builder, "edit_report_size"));

    gtk_window_set_transient_for (GTK_WINDOW(dlg),
                         GTK_WINDOW(gtk_widget_get_toplevel (GTK_WIDGET(button))));

    /* get the spinner widgets */
    rowspin = GTK_WIDGET(gtk_builder_get_object (builder, "row_spin"));
    colspin = GTK_WIDGET(gtk_builder_get_object (builder, "col_spin"));

    length = scm_ilength(r->contents_list);
    if (length > r->contents_selected)
    {
        current = scm_list_ref(r->contents_list,
                               scm_from_int (r->contents_selected));
        gtk_spin_button_set_value(GTK_SPIN_BUTTON(colspin),
                                  (float)scm_to_int(SCM_CADR(current)));
        gtk_spin_button_set_value(GTK_SPIN_BUTTON(rowspin),
                                  (float)scm_to_int(SCM_CADDR(current)));

        dlg_ret = gtk_dialog_run(GTK_DIALOG(dlg));
        gtk_widget_hide(dlg);

        if (dlg_ret == GTK_RESPONSE_OK)
        {
            current = SCM_LIST4(SCM_CAR(current),
                                scm_from_int (gtk_spin_button_get_value_as_int
                                              (GTK_SPIN_BUTTON(colspin))),
                                scm_from_int (gtk_spin_button_get_value_as_int
                                              (GTK_SPIN_BUTTON(rowspin))),
                                SCM_BOOL_F);
            scm_gc_unprotect_object(r->contents_list);
            r->contents_list = scm_list_set_x(r->contents_list,
                                              scm_from_int (r->contents_selected),
                                              current);
            scm_gc_protect_object(r->contents_list);
            gnc_options_dialog_changed (r->optwin);
            update_contents_lists(r);
        }

        g_object_unref(G_OBJECT(builder));

        gtk_widget_destroy(dlg);
    }
}
