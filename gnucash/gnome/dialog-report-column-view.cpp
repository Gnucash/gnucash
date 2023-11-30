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

#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <algorithm>
#include <dialog-options.hpp>
#include <gnc-optiondb-impl.hpp>
#include <libguile.h>

#include <config.h>

#include "swig-runtime.h"

#include "dialog-utils.h"
#include "window-report.h"
#include "guile-mappings.h"
#include "gnc-guile-utils.h"
#include "gnc-ui.h"

#include "dialog-report-column-view.hpp"
#include <gnc-report.h>

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

using StrVec = std::vector<std::string>;

struct gncp_column_view_edit
{
    std::unique_ptr<GncOptionsDialog> optwin;
    GtkTreeView  * available;
    GtkTreeView  * contents;

    SCM          view;
    GncOptionDB  * odb;

    StrVec  available_list;
    GncOptionReportPlacementVec  contents_list;
    int       contents_selected;

    GtkWidget *add_button;
    GtkWidget *remove_button;
    GtkWidget *up_button;
    GtkWidget *down_button;
    GtkWidget *size_button;
};

/* Even though these aren't external nor used outside this file they must be
 * declared this way to ensure that they're in the library's symbol table and
 * aren't mangled. That's so that dlsym is able to find them when GtkBuilder
 * needs to connect the signals to them.
 */
extern "C"
{
void gnc_column_view_edit_add_cb(GtkButton * button, gpointer user_data);
void gnc_column_view_edit_remove_cb(GtkButton * button, gpointer user_data);
void gnc_edit_column_view_move_up_cb(GtkButton * button, gpointer user_data);
void gnc_edit_column_view_move_down_cb(GtkButton * button, gpointer user_data);
void gnc_column_view_edit_size_cb(GtkButton * button, gpointer user_data);
}

static void
gnc_column_view_set_option(GncOptionDB* odb, const char* section,
                           const char* name, const GncOptionReportPlacementVec& new_value)
{
    odb->find_option(section, name)->set_value(new_value);
}

static void
gnc_column_view_edit_destroy(gnc_column_view_edit * view)
{
    scm_gc_unprotect_object(view->view);
    gnc_option_db_destroy(view->odb);
    delete view;
}

static StrVec
get_available_reports ()
{
    StrVec sv;
    auto scm_list{scm_call_0(scm_c_eval_string("gnc:all-report-template-guids"))};
    for (auto next{scm_list}; !scm_is_null(next); next = scm_cdr(next))
    {
        auto guid{scm_to_utf8_string(scm_car(next))};
        sv.emplace_back(guid);
        g_free (guid);
    }
    return sv;
}

static void
update_available_lists(gnc_column_view_edit * view)
{
    SCM   template_menu_name = scm_c_eval_string("gnc:report-template-menu-name/report-guid");
    std::string selection;


    GtkTreeIter iter;

    /* Update the list of available reports (left selection box). */
    auto tree_selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(view->available));
    auto model = gtk_tree_view_get_model (GTK_TREE_VIEW(view->available));

    if (gtk_tree_selection_get_selected(tree_selection, &model, &iter))
    {
        gchar *guid_str;
        gtk_tree_model_get(model, &iter,
                           AVAILABLE_COL_GUID, &guid_str,
                           -1);
        selection = std::string(guid_str);
        g_free (guid_str);
    }
    view->available_list = get_available_reports();

    auto store = GTK_LIST_STORE(model);
    gtk_list_store_clear(store);

    for (const auto& guid : view->available_list)
    {
       auto rpt_guid{scm_from_utf8_string(guid.c_str())};
       auto name =
           gnc_scm_to_utf8_string (scm_call_2(template_menu_name, rpt_guid, SCM_BOOL_F));

       gtk_list_store_append(store, &iter);
       gtk_list_store_set(store, &iter,
                          AVAILABLE_COL_NAME, _(name),
                          AVAILABLE_COL_GUID, guid.c_str(),
                          -1);

       if (guid == selection)
           gtk_tree_selection_select_iter (tree_selection, &iter);

       g_free (name);
    }
}

static void
update_contents_lists(gnc_column_view_edit * view)
{
    SCM   report_menu_name = scm_c_eval_string("gnc:report-menu-name");
    auto contents{view->odb->find_option("__general", "report-list")->get_value<GncOptionReportPlacementVec>()};
    GtkTreeIter iter;
    GncOptionReportPlacement selection{0, 0, 0};

    /* Update the list of selected reports (right selection box). */
    auto tree_selection = gtk_tree_view_get_selection(view->contents);

    view->contents_list = contents;

    if (!contents.empty() && static_cast<size_t>(view->contents_selected) < contents.size())
        selection = contents[view->contents_selected];

    auto store = GTK_LIST_STORE(gtk_tree_view_get_model(view->contents));
    gtk_list_store_clear(store);

    for (size_t i = 0; i < contents.size(); ++i)
    {
        auto [id, wide, high] = contents[i];
        auto this_report = gnc_report_find(id);
        auto name = gnc_scm_to_utf8_string (scm_call_1(report_menu_name, this_report));

        gtk_list_store_append(store, &iter);
        gtk_list_store_set
            (store, &iter,
             CONTENTS_COL_NAME, _(name),
             CONTENTS_COL_ROW, i,
             CONTENTS_COL_REPORT_COLS, wide,
             CONTENTS_COL_REPORT_ROWS, high,
             -1);

        if (id == std::get<0>(selection))
            gtk_tree_selection_select_iter (tree_selection, &iter);

        g_free (name);
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
        int len = r->contents_list.size();

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
gnc_column_view_edit_apply_cb(GncOptionsDialog *dlg, gpointer user_data)
{
    SCM  dirty_report = scm_c_eval_string("gnc:report-set-dirty?!");
    auto win{static_cast<gnc_column_view_edit*>(user_data)};

    if (!win) return;
    auto results = gnc_option_db_commit (dlg->get_option_db());
    for (auto iter = results; iter; iter = iter->next)
    {
        GtkWidget *dialog =
            gtk_message_dialog_new(GTK_WINDOW(dlg->get_widget()),
                                   GTK_DIALOG_MODAL,
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
gnc_column_view_edit_close_cb(GncOptionsDialog *win, gpointer user_data)
{
    auto r{static_cast<gnc_column_view_edit*>(user_data)};
    SCM set_editor = scm_c_eval_string("gnc:report-set-editor-widget!");

    scm_call_2(set_editor, r->view, SCM_BOOL_F);
    gnc_column_view_edit_destroy(r);
}


/********************************************************************
 * gnc_column_view_edit_options
 * create the editor.
 ********************************************************************/

GtkWidget *
gnc_column_view_edit_options(GncOptionDB* odb, SCM view)
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
        auto w{static_cast<GtkWindow*>(SWIG_MustGetPtr(ptr, SWIG_TypeQuery("_p_GtkWidget"), 1, 0))};
        gtk_window_present(w);
#undef FUNC_NAME
        return nullptr;
    }
    else
    {
        auto r = new gnc_column_view_edit;
        GtkBuilder *builder;

        r->optwin = std::make_unique<GncOptionsDialog>(nullptr, GTK_WINDOW(gnc_ui_get_main_window (nullptr)));

        /* Hide the generic dialog page list. */
        gtk_widget_hide(r->optwin->get_page_list());

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

        r->view      = view;
        r->available_list.clear();
        r->contents_selected = 0;
        r->contents_list.clear();
        r->odb       = odb;

        r->optwin->build_contents(r->odb);

        gtk_notebook_append_page(GTK_NOTEBOOK(r->optwin->get_notebook()),
                                 editor,
                                 gtk_label_new(_("Contents")));

        scm_gc_protect_object(r->view);

        /* Build the 'available' view */
        store = gtk_list_store_new (NUM_AVAILABLE_COLS, G_TYPE_STRING, G_TYPE_STRING);
        gtk_tree_view_set_model(r->available, GTK_TREE_MODEL(store));
        gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(store), AVAILABLE_COL_NAME, GTK_SORT_ASCENDING);
        g_object_unref(store);

        renderer = gtk_cell_renderer_text_new();
        column = gtk_tree_view_column_new_with_attributes("", renderer,
                 "text", AVAILABLE_COL_NAME,
                 nullptr);
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
                 nullptr);
        gtk_tree_view_append_column(r->contents, column);

        renderer = gtk_cell_renderer_text_new();
        column = gtk_tree_view_column_new_with_attributes(_("Rows"), renderer,
                 "text", CONTENTS_COL_REPORT_ROWS,
                 nullptr);
        gtk_tree_view_append_column(r->contents, column);

        renderer = gtk_cell_renderer_text_new();
        column = gtk_tree_view_column_new_with_attributes(_("Cols"), renderer,
                 "text", CONTENTS_COL_REPORT_COLS,
                 nullptr);
        gtk_tree_view_append_column(r->contents, column);

        /* use the selection cb to update buttons */
        selection = gtk_tree_view_get_selection(r->contents);
        g_signal_connect(selection, "changed",
                         G_CALLBACK(gnc_column_view_update_buttons_cb), r);

        update_available_lists(r);
        update_contents_lists(r);

        r->optwin->set_apply_cb(gnc_column_view_edit_apply_cb, r);
        r->optwin->set_close_cb(gnc_column_view_edit_close_cb, r);

        gtk_widget_show(r->optwin->get_widget());

        gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, r);

        g_object_unref(G_OBJECT(builder));

        return r->optwin->get_widget();
    }
}

void
gnc_column_view_edit_add_cb(GtkButton * button, gpointer user_data)
{
    auto r = static_cast<gnc_column_view_edit *>(user_data);
    SCM make_report = scm_c_eval_string("gnc:make-report");
    SCM mark_report = scm_c_eval_string("gnc:report-set-needs-save?!");
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

    auto template_name = scm_from_utf8_string(guid_str);

    auto new_report = scm_call_1(make_report, template_name);
    auto id = scm_to_int(new_report);
    scm_call_2(mark_report, gnc_report_find(id), SCM_BOOL_T);
    auto oldlength = r->contents_list.size();
    GncOptionReportPlacement new_rpt_placement{id, 1, 1};

    if (oldlength > static_cast<size_t>(r->contents_selected))
        r->contents_list.emplace(r->contents_list.begin() + r->contents_selected + 1, id, 1, 1);
    else
    {
        r->contents_list.emplace_back(id, 1, 1);
        r->contents_selected = oldlength;
    }

    gnc_column_view_set_option(r->odb, "__general", "report-list",
                               r->contents_list);
    g_free (guid_str);
    r->optwin->changed();
    update_contents_lists(r);
}

void
gnc_column_view_edit_remove_cb(GtkButton * button, gpointer user_data)
{
    auto r = static_cast<gnc_column_view_edit *>(user_data);

    r->contents_list.erase(r->contents_list.begin() + r->contents_selected);
    if (r->contents_selected)
        --r->contents_selected;
    gnc_column_view_set_option(r->odb, "__general", "report-list",
                               r->contents_list);

    r->optwin->changed();
    update_contents_lists(r);
}

static void
move_selected_item(gnc_column_view_edit* r, int increment)
{
    if (!r || !increment)
        return;

    auto cur_sel{r->contents_list.begin() + r->contents_selected};
    auto move_to{cur_sel + increment};
    if (increment > 0)
        std::reverse(cur_sel, move_to + 1);
    else
        std::reverse(move_to, cur_sel + 1);
    r->contents_selected += increment;

    gnc_column_view_set_option(r->odb, "__general", "report-list",
                               r->contents_list);
    r->optwin->changed();
    update_contents_lists(r);
}

void
gnc_edit_column_view_move_up_cb(GtkButton * button, gpointer user_data)
{
    auto r = static_cast<gnc_column_view_edit *>(user_data);
    move_selected_item(r, -1);
}

void
gnc_edit_column_view_move_down_cb(GtkButton * button, gpointer user_data)
{
    auto r = static_cast<gnc_column_view_edit *>(user_data);
    move_selected_item(r, 1);
}

void
gnc_column_view_edit_size_cb(GtkButton * button, gpointer user_data)
{
    auto r = static_cast<gnc_column_view_edit *>(user_data);
    GtkWidget * rowspin;
    GtkWidget * colspin;
    GtkWidget * dlg;
    GtkBuilder *builder;
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

    if (r->contents_list.size() > static_cast<size_t>(r->contents_selected))
    {
        auto [id, wide, high] = r->contents_list[r->contents_selected];

        gtk_spin_button_set_value(GTK_SPIN_BUTTON(colspin),
                                  static_cast<float>(wide));
        gtk_spin_button_set_value(GTK_SPIN_BUTTON(rowspin),
                                  static_cast<float>(high));

        dlg_ret = gtk_dialog_run(GTK_DIALOG(dlg));
        gtk_widget_hide(dlg);

        if (dlg_ret == GTK_RESPONSE_OK)
        {
            std::get<1>(r->contents_list[r->contents_selected]) =
                gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(colspin));
            std::get<2>(r->contents_list[r->contents_selected]) =
                gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(rowspin));

            gnc_column_view_set_option(r->odb, "__general", "report-list",
                                       r->contents_list);
            r->optwin->changed();
            update_contents_lists(r);
        }

        g_object_unref(G_OBJECT(builder));

        gtk_widget_destroy(dlg);
    }
}
