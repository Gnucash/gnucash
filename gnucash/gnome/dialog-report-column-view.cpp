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

using StrVec = std::vector<std::string>;

struct ReportPlacementData
{
    int id;
    int columns;
    int rows;
};

struct ReportSizeRequest;
static void report_size_request_complete(ReportSizeRequest *request, gboolean apply);

struct gncp_column_view_edit
{
    std::unique_ptr<GncOptionsDialog> optwin;
    GtkColumnView *available;
    GtkColumnView *contents;
    GListStore *available_store;
    GListStore *contents_store;
    GtkSingleSelection *available_selection;
    GtkSingleSelection *contents_selection;
    ReportSizeRequest *size_request;

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

struct ReportSizeRequest
{
    gncp_column_view_edit *editor;
    GtkWindow *window;
    GtkSpinButton *row_spin;
    GtkSpinButton *column_spin;
    gboolean completed;
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
    if (view->size_request)
        report_size_request_complete(view->size_request, FALSE);

    gtk_column_view_set_model(view->available, nullptr);
    gtk_column_view_set_model(view->contents, nullptr);
    g_clear_object(&view->available_selection);
    g_clear_object(&view->contents_selection);
    g_clear_object(&view->available_store);
    g_clear_object(&view->contents_store);
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

static GQuark
available_guid_quark(void)
{
    static GQuark quark;

    if (!quark)
        quark = g_quark_from_static_string("gnc-report-template-guid");
    return quark;
}

static GQuark
contents_row_quark(void)
{
    static GQuark quark;

    if (!quark)
        quark = g_quark_from_static_string("gnc-report-placement-data");
    return quark;
}

static guint
selection_position(GtkSingleSelection *selection)
{
    return gtk_single_selection_get_selected(selection);
}

static std::string
selected_available_guid(gncp_column_view_edit *view)
{
    const auto position = selection_position(view->available_selection);
    GObject *item;

    if (position == GTK_INVALID_LIST_POSITION)
        return {};
    item = G_OBJECT(g_list_model_get_item(G_LIST_MODEL(view->available_selection), position));
    const auto guid = item ? static_cast<const gchar *>(g_object_get_qdata(item,
                                                                             available_guid_quark()))
                           : nullptr;
    std::string result = guid ? guid : "";

    if (item)
        g_object_unref(item);
    return result;
}

static void
select_available_guid(gncp_column_view_edit *view, const std::string& guid)
{
    const auto count = g_list_model_get_n_items(G_LIST_MODEL(view->available_selection));

    gtk_single_selection_set_selected(view->available_selection, GTK_INVALID_LIST_POSITION);
    for (guint position = 0; position < count; position++)
    {
        auto item = G_OBJECT(g_list_model_get_item(G_LIST_MODEL(view->available_selection), position));
        const auto item_guid = item ? static_cast<const gchar *>(g_object_get_qdata(item,
                                                                                      available_guid_quark()))
                                    : nullptr;
        const auto matches = item_guid && guid == item_guid;

        if (item)
            g_object_unref(item);
        if (matches)
        {
            gtk_single_selection_set_selected(view->available_selection, position);
            return;
        }
    }
}

static void
report_text_factory_setup_cb(G_GNUC_UNUSED GtkListItemFactory *factory,
                             GtkListItem *list_item,
                             G_GNUC_UNUSED gpointer user_data)
{
    auto label = gtk_label_new(nullptr);

    gtk_label_set_xalign(GTK_LABEL(label), 0.0F);
    gtk_label_set_ellipsize(GTK_LABEL(label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child(list_item, label);
}

static void
report_available_factory_bind_cb(G_GNUC_UNUSED GtkListItemFactory *factory,
                                 GtkListItem *list_item,
                                 G_GNUC_UNUSED gpointer user_data)
{
    auto object = GTK_STRING_OBJECT(gtk_list_item_get_item(list_item));
    auto label = GTK_LABEL(gtk_list_item_get_child(list_item));

    gtk_label_set_text(label, object ? gtk_string_object_get_string(object) : "");
}

static ReportPlacementData *
report_placement_from_list_item(GtkListItem *list_item)
{
    auto object = G_OBJECT(gtk_list_item_get_item(list_item));

    return object ? static_cast<ReportPlacementData *>(g_object_get_qdata(object,
                                                                            contents_row_quark()))
                  : nullptr;
}

static void
report_contents_name_factory_bind_cb(G_GNUC_UNUSED GtkListItemFactory *factory,
                                     GtkListItem *list_item,
                                     G_GNUC_UNUSED gpointer user_data)
{
    auto object = GTK_STRING_OBJECT(gtk_list_item_get_item(list_item));
    auto label = GTK_LABEL(gtk_list_item_get_child(list_item));

    gtk_label_set_text(label, object ? gtk_string_object_get_string(object) : "");
}

static void
report_contents_span_factory_bind_cb(G_GNUC_UNUSED GtkListItemFactory *factory,
                                     GtkListItem *list_item,
                                     gpointer user_data)
{
    auto placement = report_placement_from_list_item(list_item);
    auto label = GTK_LABEL(gtk_list_item_get_child(list_item));
    const auto span = GPOINTER_TO_INT(user_data) ? placement ? placement->columns : 0
                                                 : placement ? placement->rows : 0;
    auto text = g_strdup_printf("%d", span);

    gtk_label_set_text(label, text);
    g_free(text);
}

using ReportFactoryBindFunc = void (*)(GtkListItemFactory *, GtkListItem *, gpointer);

static void
append_report_column(GtkColumnView *view, const gchar *title, gboolean expand,
                     ReportFactoryBindFunc bind, gpointer bind_data)
{
    auto factory = gtk_signal_list_item_factory_new();
    GtkColumnViewColumn *column;

    g_signal_connect(factory, "setup", G_CALLBACK(report_text_factory_setup_cb), nullptr);
    g_signal_connect(factory, "bind", G_CALLBACK(bind), bind_data);
    column = gtk_column_view_column_new(title, GTK_LIST_ITEM_FACTORY(factory));
    gtk_column_view_column_set_expand(column, expand);
    gtk_column_view_append_column(view, column);
    g_object_unref(column);
}

static void gnc_column_view_update_buttons(gncp_column_view_edit *view);

static void
update_available_lists(gncp_column_view_edit *view)
{
    SCM template_menu_name = scm_c_eval_string("gnc:report-template-menu-name/report-guid");
    const auto selection = selected_available_guid(view);

    view->available_list = get_available_reports();
    g_list_store_remove_all(view->available_store);
    for (const auto& guid : view->available_list)
    {
        auto report_guid = scm_from_utf8_string(guid.c_str());
        auto name = gnc_scm_to_utf8_string(scm_call_2(template_menu_name, report_guid, SCM_BOOL_F));
        auto row = gtk_string_object_new(_(name));

        g_object_set_qdata_full(G_OBJECT(row), available_guid_quark(), g_strdup(guid.c_str()), g_free);
        g_list_store_append(view->available_store, row);
        g_object_unref(row);
        g_free(name);
    }
    select_available_guid(view, selection);
    gnc_column_view_update_buttons(view);
}

static void
update_contents_lists(gncp_column_view_edit *view)
{
    SCM report_menu_name = scm_c_eval_string("gnc:report-menu-name");
    auto contents = view->odb->find_option("__general", "report-list")
                        ->get_value<GncOptionReportPlacementVec>();
    int selected_id = 0;
    gboolean have_selection = FALSE;

    if (view->contents_selected >= 0 &&
        static_cast<size_t>(view->contents_selected) < view->contents_list.size())
    {
        selected_id = std::get<0>(view->contents_list[view->contents_selected]);
        have_selection = TRUE;
    }
    view->contents_list = contents;
    gtk_single_selection_set_selected(view->contents_selection, GTK_INVALID_LIST_POSITION);
    g_list_store_remove_all(view->contents_store);
    for (size_t position = 0; position < contents.size(); position++)
    {
        auto [id, columns, rows] = contents[position];
        auto report = gnc_report_find(id);
        auto name = gnc_scm_to_utf8_string(scm_call_1(report_menu_name, report));
        auto row = gtk_string_object_new(_(name));
        auto placement = g_new(ReportPlacementData, 1);

        *placement = {static_cast<int>(id), static_cast<int>(columns), static_cast<int>(rows)};
        g_object_set_qdata_full(G_OBJECT(row), contents_row_quark(), placement, g_free);
        g_list_store_append(view->contents_store, row);
        g_object_unref(row);
        g_free(name);
        if (have_selection && id == static_cast<guint>(selected_id))
        {
            gtk_single_selection_set_selected(view->contents_selection, position);
            view->contents_selected = static_cast<int>(position);
        }
    }
    if (!have_selection)
        gtk_single_selection_set_selected(view->contents_selection, GTK_INVALID_LIST_POSITION);
    gnc_column_view_update_buttons(view);
}

static void
selection_changed_cb(G_GNUC_UNUSED GObject *selection,
                     G_GNUC_UNUSED GParamSpec *property,
                     gpointer user_data)
{
    gnc_column_view_update_buttons(static_cast<gncp_column_view_edit *>(user_data));
}

static void
gnc_column_view_update_buttons(gncp_column_view_edit *view)
{
    const auto available_position = selection_position(view->available_selection);
    const auto contents_position = selection_position(view->contents_selection);
    const auto available_selected = available_position != GTK_INVALID_LIST_POSITION;
    const auto contents_selected = contents_position != GTK_INVALID_LIST_POSITION &&
                                   contents_position < view->contents_list.size();

    gtk_widget_set_sensitive(view->add_button, available_selected);
    gtk_widget_set_sensitive(view->size_button, contents_selected);
    gtk_widget_set_sensitive(view->remove_button, contents_selected);
    gtk_widget_set_sensitive(view->up_button, contents_selected && contents_position > 0);
    gtk_widget_set_sensitive(view->down_button, contents_selected &&
                             contents_position + 1 < view->contents_list.size());
    view->contents_selected = contents_selected ? static_cast<int>(contents_position) : -1;
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
        gnc_error_dialog (GTK_WINDOW (dlg->get_widget()), "%s",
                          static_cast<char *> (iter->data));
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
    SCM ptr = scm_call_1(get_editor, view);

    if (ptr != SCM_BOOL_F)
    {
#define FUNC_NAME "gtk_window_present"
        auto window = static_cast<GtkWindow *>(SWIG_MustGetPtr(ptr,
                                                                 SWIG_TypeQuery("_p_GtkWidget"),
                                                                 1, 0));
        gtk_window_present(window);
#undef FUNC_NAME
        return nullptr;
    }

    auto editor = new gncp_column_view_edit{};
    auto builder = gtk_builder_new();
    GtkWidget *contents_page;
    GtkExpression *expression;
    GtkSorter *sorter;
    GtkSortListModel *sorted_model;

    editor->optwin = std::make_unique<GncOptionsDialog>(nullptr,
        GTK_WINDOW(gnc_ui_get_main_window(nullptr)));
    gtk_widget_set_visible(GTK_WIDGET(editor->optwin->get_page_list()), false);
    gnc_builder_add_from_file(builder, "dialog-report.glade", "view_contents_table");

    contents_page = GTK_WIDGET(gtk_builder_get_object(builder, "view_contents_table"));
    editor->available = GTK_COLUMN_VIEW(gtk_builder_get_object(builder, "available_view"));
    editor->contents = GTK_COLUMN_VIEW(gtk_builder_get_object(builder, "contents_view"));
    editor->add_button = GTK_WIDGET(gtk_builder_get_object(builder, "add_button1"));
    editor->remove_button = GTK_WIDGET(gtk_builder_get_object(builder, "remove_button1"));
    editor->up_button = GTK_WIDGET(gtk_builder_get_object(builder, "up_button1"));
    editor->down_button = GTK_WIDGET(gtk_builder_get_object(builder, "down_button1"));
    editor->size_button = GTK_WIDGET(gtk_builder_get_object(builder, "size_button1"));
    editor->view = view;
    editor->odb = odb;
    editor->contents_selected = -1;

    editor->available_store = g_list_store_new(GTK_TYPE_STRING_OBJECT);
    expression = gtk_property_expression_new(GTK_TYPE_STRING_OBJECT, nullptr, "string");
    sorter = GTK_SORTER(gtk_string_sorter_new(expression));
    sorted_model = gtk_sort_list_model_new(G_LIST_MODEL(g_object_ref(editor->available_store)),
                                           sorter);
    editor->available_selection = gtk_single_selection_new(G_LIST_MODEL(sorted_model));
    gtk_single_selection_set_autoselect(editor->available_selection, FALSE);
    gtk_single_selection_set_can_unselect(editor->available_selection, TRUE);
    gtk_column_view_set_model(editor->available, GTK_SELECTION_MODEL(editor->available_selection));
    append_report_column(editor->available, "", TRUE, report_available_factory_bind_cb, nullptr);

    editor->contents_store = g_list_store_new(GTK_TYPE_STRING_OBJECT);
    editor->contents_selection = gtk_single_selection_new(
        G_LIST_MODEL(g_object_ref(editor->contents_store)));
    gtk_single_selection_set_autoselect(editor->contents_selection, FALSE);
    gtk_single_selection_set_can_unselect(editor->contents_selection, TRUE);
    gtk_column_view_set_model(editor->contents, GTK_SELECTION_MODEL(editor->contents_selection));
    append_report_column(editor->contents, _("Report"), TRUE,
                         report_contents_name_factory_bind_cb, nullptr);
    append_report_column(editor->contents, _("Rows"), FALSE,
                         report_contents_span_factory_bind_cb, nullptr);
    append_report_column(editor->contents, _("Cols"), FALSE,
                         report_contents_span_factory_bind_cb, GINT_TO_POINTER(1));

    g_signal_connect(editor->available_selection, "notify::selected",
                     G_CALLBACK(selection_changed_cb), editor);
    g_signal_connect(editor->contents_selection, "notify::selected",
                     G_CALLBACK(selection_changed_cb), editor);
    editor->optwin->build_contents(editor->odb);
    gtk_notebook_append_page(GTK_NOTEBOOK(editor->optwin->get_notebook()),
                             contents_page, gtk_label_new(_("Contents")));
    scm_gc_protect_object(editor->view);
    update_available_lists(editor);
    update_contents_lists(editor);
    editor->optwin->set_apply_cb(gnc_column_view_edit_apply_cb, editor);
    editor->optwin->set_close_cb(gnc_column_view_edit_close_cb, editor);
    gnc_builder_connect_signals_full(builder, gnc_builder_connect_full_func, editor);
    g_object_unref(builder);
    gtk_widget_set_visible(GTK_WIDGET(editor->optwin->get_widget()), true);

    return editor->optwin->get_widget();
}
void
gnc_column_view_edit_add_cb(G_GNUC_UNUSED GtkButton *button, gpointer user_data)
{
    auto editor = static_cast<gnc_column_view_edit *>(user_data);
    SCM make_report = scm_c_eval_string("gnc:make-report");
    SCM mark_report = scm_c_eval_string("gnc:report-set-needs-save?!");
    const auto guid = editor ? selected_available_guid(editor) : std::string{};

    if (guid.empty())
        return;

    auto template_name = scm_from_utf8_string(guid.c_str());
    auto new_report = scm_call_1(make_report, template_name);
    auto id = scm_to_int(new_report);
    auto old_length = editor->contents_list.size();
    size_t insert_at = old_length;

    scm_call_2(mark_report, gnc_report_find(id), SCM_BOOL_T);
    if (editor->contents_selected >= 0 &&
        static_cast<size_t>(editor->contents_selected) < old_length)
        insert_at = static_cast<size_t>(editor->contents_selected) + 1;
    editor->contents_list.emplace(editor->contents_list.begin() + insert_at, id, 1, 1);
    editor->contents_selected = static_cast<int>(insert_at);
    gnc_column_view_set_option(editor->odb, "__general", "report-list",
                               editor->contents_list);
    editor->optwin->changed();
    update_contents_lists(editor);
}

void
gnc_column_view_edit_remove_cb(G_GNUC_UNUSED GtkButton *button, gpointer user_data)
{
    auto editor = static_cast<gnc_column_view_edit *>(user_data);

    if (!editor || editor->contents_selected < 0 ||
        static_cast<size_t>(editor->contents_selected) >= editor->contents_list.size())
        return;

    editor->contents_list.erase(editor->contents_list.begin() + editor->contents_selected);
    if (editor->contents_selected >= static_cast<int>(editor->contents_list.size()))
        editor->contents_selected = static_cast<int>(editor->contents_list.size()) - 1;
    gnc_column_view_set_option(editor->odb, "__general", "report-list",
                               editor->contents_list);
    editor->optwin->changed();
    update_contents_lists(editor);
}

static void
move_selected_item(gncp_column_view_edit *editor, int increment)
{
    if (!editor || !increment || editor->contents_selected < 0)
        return;

    const auto from = static_cast<size_t>(editor->contents_selected);
    const auto destination = static_cast<int>(from) + increment;

    if (from >= editor->contents_list.size() || destination < 0 ||
        static_cast<size_t>(destination) >= editor->contents_list.size())
        return;

    std::iter_swap(editor->contents_list.begin() + from,
                   editor->contents_list.begin() + destination);
    editor->contents_selected = destination;
    gnc_column_view_set_option(editor->odb, "__general", "report-list",
                               editor->contents_list);
    editor->optwin->changed();
    update_contents_lists(editor);
}

void
gnc_edit_column_view_move_up_cb(G_GNUC_UNUSED GtkButton *button, gpointer user_data)
{
    move_selected_item(static_cast<gnc_column_view_edit *>(user_data), -1);
}

void
gnc_edit_column_view_move_down_cb(G_GNUC_UNUSED GtkButton *button, gpointer user_data)
{
    move_selected_item(static_cast<gnc_column_view_edit *>(user_data), 1);
}

static void
report_size_request_complete(ReportSizeRequest *request, gboolean apply)
{
    auto editor = request ? request->editor : nullptr;

    if (!request || request->completed)
        return;

    request->completed = TRUE;
    if (editor && editor->size_request == request)
        editor->size_request = nullptr;
    if (apply && editor && editor->contents_selected >= 0 &&
        static_cast<size_t>(editor->contents_selected) < editor->contents_list.size())
    {
        auto& placement = editor->contents_list[editor->contents_selected];

        std::get<1>(placement) = gtk_spin_button_get_value_as_int(request->column_spin);
        std::get<2>(placement) = gtk_spin_button_get_value_as_int(request->row_spin);
        gnc_column_view_set_option(editor->odb, "__general", "report-list",
                                   editor->contents_list);
        editor->optwin->changed();
        update_contents_lists(editor);
    }
    request->editor = nullptr;
    if (request->window)
        gtk_window_destroy(request->window);
    g_clear_object(&request->window);
    g_free(request);
}

static gboolean
report_size_close_request_cb(G_GNUC_UNUSED GtkWindow *window, gpointer user_data)
{
    report_size_request_complete(static_cast<ReportSizeRequest *>(user_data), FALSE);
    return TRUE;
}

static void
report_size_destroy_cb(G_GNUC_UNUSED GtkWidget *window, gpointer user_data)
{
    auto request = static_cast<ReportSizeRequest *>(user_data);

    if (!request->completed)
        report_size_request_complete(request, FALSE);
}

static void
report_size_accept_cb(G_GNUC_UNUSED GtkButton *button, gpointer user_data)
{
    report_size_request_complete(static_cast<ReportSizeRequest *>(user_data), TRUE);
}

static void
report_size_cancel_cb(G_GNUC_UNUSED GtkButton *button, gpointer user_data)
{
    report_size_request_complete(static_cast<ReportSizeRequest *>(user_data), FALSE);
}

void
gnc_column_view_edit_size_cb(GtkButton *button, gpointer user_data)
{
    auto editor = static_cast<gnc_column_view_edit *>(user_data);
    GtkBuilder *builder;
    GtkWindow *window;
    GtkSpinButton *row_spin;
    GtkSpinButton *column_spin;
    GtkWidget *cancel_button;
    GtkWidget *ok_button;
    GtkRoot *root;
    ReportSizeRequest *request;

    if (!editor || editor->contents_selected < 0 ||
        static_cast<size_t>(editor->contents_selected) >= editor->contents_list.size())
        return;
    if (editor->size_request)
    {
        gtk_window_present(editor->size_request->window);
        return;
    }

    builder = gtk_builder_new();
    gnc_builder_add_from_file(builder, "dialog-report.glade", "col_adjustment");
    gnc_builder_add_from_file(builder, "dialog-report.glade", "row_adjustment");
    gnc_builder_add_from_file(builder, "dialog-report.glade", "edit_report_size");
    window = GTK_WINDOW(gtk_builder_get_object(builder, "edit_report_size"));
    row_spin = GTK_SPIN_BUTTON(gtk_builder_get_object(builder, "row_spin"));
    column_spin = GTK_SPIN_BUTTON(gtk_builder_get_object(builder, "col_spin"));
    cancel_button = GTK_WIDGET(gtk_builder_get_object(builder, "cancelbutton"));
    ok_button = GTK_WIDGET(gtk_builder_get_object(builder, "okbutton"));
    root = gtk_widget_get_root(GTK_WIDGET(button));
    if (GTK_IS_WINDOW(root))
        gtk_window_set_transient_for(window, GTK_WINDOW(root));

    auto [id, columns, rows] = editor->contents_list[editor->contents_selected];
    gtk_spin_button_set_value(column_spin, columns);
    gtk_spin_button_set_value(row_spin, rows);
    gtk_window_set_modal(window, TRUE);
    gtk_window_set_default_widget(window, ok_button);
    request = g_new0(ReportSizeRequest, 1);
    request->editor = editor;
    request->window = GTK_WINDOW(g_object_ref(window));
    request->row_spin = row_spin;
    request->column_spin = column_spin;
    editor->size_request = request;
    g_signal_connect(window, "close-request", G_CALLBACK(report_size_close_request_cb), request);
    g_signal_connect(window, "destroy", G_CALLBACK(report_size_destroy_cb), request);
    g_signal_connect(cancel_button, "clicked", G_CALLBACK(report_size_cancel_cb), request);
    g_signal_connect(ok_button, "clicked", G_CALLBACK(report_size_accept_cb), request);
    g_object_unref(builder);
    gtk_window_present(window);
    gtk_widget_grab_focus(GTK_WIDGET(column_spin));
    (void)id;
}
