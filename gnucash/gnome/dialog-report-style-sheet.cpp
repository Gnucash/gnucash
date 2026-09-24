/********************************************************************
 * dialog-report-style-sheet.c -- window for configuring HTML style *
 *                                sheets in GnuCash                 *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
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

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <dialog-options.hpp>
#include <gnc-optiondb.h>
#include <libguile.h>

#include <config.h>

#include "dialog-report-style-sheet.h"
#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-session.h"
#include "gnc-gtk-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-guile-utils.h"
#include "gnc-ui.h"
#include <guile-mappings.h>
#include "gnc-report.h"

#define DIALOG_STYLE_SHEETS_CM_CLASS "style-sheets-dialog"
#define GNC_PREFS_GROUP              "dialogs.style-sheet"

StyleSheetDialog * gnc_style_sheet_dialog = NULL;

struct ss_info;
struct StyleSheetRow
{
    SCM stylesheet;
    ss_info *options_dialog;
};

struct _stylesheetdialog
{
    GtkWindow          *toplevel;
    GtkColumnView      *list_view;
    GListStore         *list_store;
    GtkSingleSelection *selection;
    gint                component_id;
    QofSession         *session;
};

typedef struct ss_info
{
    GncOptionsDialog *odialog;
    GncOptionDB      *odb;
    SCM               stylesheet;
    GObject          *row;
} ss_info;

typedef struct
{
    StyleSheetDialog *dialog;
    GtkWindow        *window;
    GtkDropDown      *template_dropdown;
    GtkEntry         *name_entry;
    GWeakRef          owner_window;
    gboolean          completed;
} NewStyleSheetRequest;

extern "C" // So that gtk_builder_connect_full can find them.
{
void gnc_style_sheet_select_dialog_new_cb (GtkWidget *widget, gpointer user_data);
void gnc_style_sheet_select_dialog_edit_cb (GtkWidget *widget, gpointer user_data);
void gnc_style_sheet_select_dialog_delete_cb (GtkWidget *widget, gpointer user_data);
void gnc_style_sheet_select_dialog_close_cb (GtkWidget *widget, gpointer user_data);
void gnc_style_sheet_select_dialog_destroy_cb (GtkWidget *widget, gpointer user_data);
}

static GQuark
style_sheet_row_quark (void)
{
    static GQuark quark = 0;

    if (!quark)
        quark = g_quark_from_static_string ("gnc-style-sheet-row");
    return quark;
}

static void
style_sheet_row_free (gpointer data)
{
    auto row = static_cast<StyleSheetRow *>(data);

    if (!row)
        return;
    scm_gc_unprotect_object (row->stylesheet);
    g_free (row);
}

static StyleSheetRow *
style_sheet_row_from_object (GObject *object)
{
    return object ? static_cast<StyleSheetRow *>(g_object_get_qdata (
                        object, style_sheet_row_quark ())) : nullptr;
}

static GObject *
style_sheet_selected_row (StyleSheetDialog *dialog)
{
    guint position;

    if (!dialog)
        return nullptr;
    position = gtk_single_selection_get_selected (dialog->selection);
    if (position == GTK_INVALID_LIST_POSITION)
        return nullptr;
    return G_OBJECT (g_list_model_get_item (G_LIST_MODEL (dialog->selection), position));
}

/************************************************************
 *     Style Sheet Edit Dialog (I.E. an options dialog)     *
 ************************************************************/

static void
dirty_same_stylesheet (gpointer key, gpointer val, gpointer data)
{
    auto dirty_ss{static_cast<SCM>(data)};
    auto report{static_cast<SCM>(val)};
    SCM func, rep_ss;

    func = scm_c_eval_string ("gnc:report-stylesheet");
    if (scm_is_procedure (func))
        rep_ss = scm_call_1 (func, report);
    else
        return;

    if (scm_is_true (scm_eq_p (rep_ss, dirty_ss)))
    {
        func = scm_c_eval_string ("gnc:report-set-dirty?!");
        /* This makes _me_ feel dirty! */
        if (scm_is_procedure (func))
            scm_call_2 (func, report, SCM_BOOL_T);
    }
}

static void
gnc_style_sheet_options_apply_cb (GncOptionsDialog *propertybox,
                                  gpointer user_data)
{
    auto ssi = static_cast<ss_info *>(user_data);
    GList *results = nullptr;

    gnc_reports_foreach (dirty_same_stylesheet, ssi->stylesheet);

    results = gnc_option_db_commit (ssi->odb);
    for (auto iter = results; iter; iter = iter->next)
    {
        gnc_error_dialog (nullptr, "%s", static_cast<char *> (iter->data));
        g_free (iter->data);
    }
    g_list_free (results);
}

static void
gnc_style_sheet_options_close_cb (G_GNUC_UNUSED GncOptionsDialog *opt_dialog,
                                  gpointer user_data)
{
    auto ssi = static_cast<ss_info *>(user_data);
    auto row_data = ssi && ssi->row ? style_sheet_row_from_object (ssi->row) : nullptr;

    if (!ssi)
        return;
    if (row_data && row_data->options_dialog == ssi)
        row_data->options_dialog = nullptr;
    g_clear_object (&ssi->row);
    delete ssi->odialog;
    gnc_option_db_destroy (ssi->odb);
    scm_gc_unprotect_object (ssi->stylesheet);
    g_free (ssi);
}

static ss_info *
gnc_style_sheet_dialog_create (StyleSheetDialog *dialog,
                               const gchar *name,
                               SCM sheet_info,
                               GObject *row)
{
    SCM get_options = scm_c_eval_string ("gnc:html-style-sheet-options");
    auto scm_dispatch = scm_call_1 (get_options, sheet_info);
    auto ssi = g_new0 (ss_info, 1);
    auto title = g_strdup_printf (_("HTML Style Sheet Properties: %s"), name);

    ssi->odialog = new GncOptionsDialog (title, dialog->toplevel);
    ssi->odb = gnc_get_optiondb_from_dispatcher (scm_dispatch);
    ssi->stylesheet = sheet_info;
    ssi->row = G_OBJECT (g_object_ref (row));
    g_free (title);

    scm_gc_protect_object (ssi->stylesheet);
    gtk_window_set_transient_for (GTK_WINDOW (ssi->odialog->get_widget ()), dialog->toplevel);
    g_signal_connect_object (dialog->toplevel, "destroy", G_CALLBACK (gtk_window_close),
                             ssi->odialog->get_widget (), G_CONNECT_SWAPPED);
    ssi->odialog->build_contents (ssi->odb, false);
    ssi->odialog->set_apply_cb (gnc_style_sheet_options_apply_cb, ssi);
    ssi->odialog->set_close_cb (gnc_style_sheet_options_close_cb, ssi);
    ssi->odialog->set_style_sheet_help_cb ();
    gtk_window_present (GTK_WINDOW (ssi->odialog->get_widget ()));
    return ssi;
}

static void
gnc_style_sheet_select_dialog_add_one (StyleSheetDialog *dialog,
                                       SCM sheet_info,
                                       gboolean select)
{
    auto get_name = scm_c_eval_string ("gnc:html-style-sheet-name");
    auto c_name = gnc_scm_call_1_to_string (get_name, sheet_info);
    auto row = gtk_string_object_new (c_name ? _(c_name) : "");
    auto row_data = g_new0 (StyleSheetRow, 1);

    if (!c_name)
    {
        g_object_unref (row);
        return;
    }

    row_data->stylesheet = sheet_info;
    scm_gc_protect_object (sheet_info);
    g_object_set_qdata_full (G_OBJECT (row), style_sheet_row_quark (), row_data,
                             style_sheet_row_free);
    g_list_store_append (dialog->list_store, row);
    if (select)
        gtk_single_selection_set_selected (dialog->selection,
                                           g_list_model_get_n_items (
                                               G_LIST_MODEL (dialog->list_store)) - 1);
    g_object_unref (row);
    g_free (c_name);
}

static void
gnc_style_sheet_select_dialog_fill (StyleSheetDialog *dialog)
{
    auto stylesheets = scm_c_eval_string ("(gnc:get-html-style-sheets)");

    for (; !scm_is_null (stylesheets); stylesheets = SCM_CDR (stylesheets))
        gnc_style_sheet_select_dialog_add_one (dialog, SCM_CAR (stylesheets), FALSE);
}

static void
gnc_style_sheet_select_dialog_edit_selected (StyleSheetDialog *dialog)
{
    auto row = style_sheet_selected_row (dialog);
    auto row_data = style_sheet_row_from_object (row);

    if (!row_data)
    {
        g_clear_object (&row);
        return;
    }
    if (row_data->options_dialog)
    {
        gtk_window_present (GTK_WINDOW (row_data->options_dialog->odialog->get_widget ()));
        g_object_unref (row);
        return;
    }

    auto name = gtk_string_object_get_string (GTK_STRING_OBJECT (row));
    auto ssi = gnc_style_sheet_dialog_create (dialog, name, row_data->stylesheet, row);

    row_data->options_dialog = ssi;
    g_object_unref (row);
}

static void
gnc_style_sheet_select_dialog_double_click_cb (GtkGestureClick *gesture,
                                                gint n_press,
                                                gdouble x,
                                                gdouble y,
                                                gpointer user_data)
{
    if (n_press == 2)
        gnc_style_sheet_select_dialog_edit_selected (static_cast<StyleSheetDialog *>(user_data));
}

static GObject *
style_sheet_new_selected_template (NewStyleSheetRequest *request)
{
    auto position = gtk_drop_down_get_selected (request->template_dropdown);

    if (position == GTK_INVALID_LIST_POSITION)
        return nullptr;
    return G_OBJECT (g_list_model_get_item (gtk_drop_down_get_model (request->template_dropdown),
                                            position));
}

static void
style_sheet_new_request_complete (NewStyleSheetRequest *request, gboolean accept)
{
    GtkWidget *owner = nullptr;

    if (!request || request->completed)
        return;
    request->completed = TRUE;

    owner = GTK_WIDGET (g_weak_ref_get (&request->owner_window));
    if (accept && owner && gnc_style_sheet_dialog == request->dialog &&
        GTK_WIDGET (request->dialog->toplevel) == owner)
    {
        auto template_row = style_sheet_new_selected_template (request);
        auto template_name = template_row ? static_cast<const gchar *>(g_object_get_data (
            template_row, "gnc-style-sheet-template-name")) : nullptr;
        auto name = gnc_entry_get_text (request->name_entry);

        if (name && *name == '\0')
        {
            gnc_error_dialog (request->dialog->toplevel, "%s",
                              _("You must provide a name for the new style sheet."));
        }
        else if (template_name && name)
        {
            auto make_ss = scm_c_eval_string ("gnc:make-html-style-sheet");
            auto sheet_info = scm_call_2 (make_ss, scm_from_utf8_string (template_name),
                                          scm_from_utf8_string (name));

            if (sheet_info != SCM_BOOL_F)
            {
                gnc_style_sheet_select_dialog_add_one (request->dialog, sheet_info, TRUE);
                gnc_style_sheet_select_dialog_edit_selected (request->dialog);
            }
        }
        g_clear_object (&template_row);
    }

    g_clear_object (&owner);
    if (request->window)
        gtk_window_destroy (request->window);
    g_clear_object (&request->window);
    g_weak_ref_clear (&request->owner_window);
    g_free (request);
}

static gboolean
style_sheet_new_close_request_cb (G_GNUC_UNUSED GtkWindow *window, gpointer user_data)
{
    style_sheet_new_request_complete (static_cast<NewStyleSheetRequest *>(user_data), FALSE);
    return TRUE;
}

static void
style_sheet_new_destroy_cb (G_GNUC_UNUSED GtkWidget *window, gpointer user_data)
{
    auto request = static_cast<NewStyleSheetRequest *>(user_data);

    if (!request->completed)
        style_sheet_new_request_complete (request, FALSE);
}

static void
style_sheet_new_cancel_cb (G_GNUC_UNUSED GtkButton *button, gpointer user_data)
{
    style_sheet_new_request_complete (static_cast<NewStyleSheetRequest *>(user_data), FALSE);
}

static void
style_sheet_new_accept_cb (G_GNUC_UNUSED GtkButton *button, gpointer user_data)
{
    style_sheet_new_request_complete (static_cast<NewStyleSheetRequest *>(user_data), TRUE);
}

static void
gnc_style_sheet_new (StyleSheetDialog *dialog)
{
    auto builder = gtk_builder_new ();
    auto request = g_new0 (NewStyleSheetRequest, 1);
    auto templates = scm_c_eval_string ("(gnc:get-html-templates)");
    auto template_name = scm_c_eval_string ("gnc:html-style-sheet-template-name");
    auto store = g_list_store_new (GTK_TYPE_STRING_OBJECT);
    GtkExpression *expression;
    GtkWidget *cancel_button;
    GtkWidget *ok_button;

    gnc_builder_add_from_file (builder, "dialog-report.glade", "new_style_sheet_dialog");
    request->dialog = dialog;
    request->window = GTK_WINDOW (g_object_ref (gtk_builder_get_object (
        builder, "new_style_sheet_dialog")));
    request->template_dropdown = GTK_DROP_DOWN (gtk_builder_get_object (
        builder, "template_dropdown"));
    request->name_entry = GTK_ENTRY (gtk_builder_get_object (builder, "name_entry"));
    cancel_button = GTK_WIDGET (gtk_builder_get_object (builder, "cancelbutton1"));
    ok_button = GTK_WIDGET (gtk_builder_get_object (builder, "okbutton1"));
    g_weak_ref_init (&request->owner_window, G_OBJECT (dialog->toplevel));

    gtk_widget_set_name (GTK_WIDGET (request->window), "gnc-id-style-sheet-new");
    gnc_widget_style_context_add_class (GTK_WIDGET (request->window), "gnc-class-style-sheets");
    gtk_window_set_transient_for (request->window, dialog->toplevel);
    gtk_window_set_destroy_with_parent (request->window, TRUE);
    gtk_window_set_modal (request->window, TRUE);
    gtk_window_set_default_widget (request->window, ok_button);

    for (; !scm_is_null (templates); templates = SCM_CDR (templates))
    {
        auto original_name = gnc_scm_call_1_to_string (template_name, SCM_CAR (templates));
        auto row = gtk_string_object_new (_(original_name));

        g_object_set_data_full (G_OBJECT (row), "gnc-style-sheet-template-name", original_name,
                                g_free);
        g_list_store_append (store, row);
        g_object_unref (row);
    }
    expression = gtk_property_expression_new (GTK_TYPE_STRING_OBJECT, nullptr, "string");
    gtk_drop_down_set_expression (request->template_dropdown, expression);
    gtk_drop_down_set_model (request->template_dropdown, G_LIST_MODEL (store));
    gtk_drop_down_set_selected (request->template_dropdown, 0);
    g_object_unref (expression);
    g_object_unref (store);

    g_signal_connect (request->window, "close-request",
                      G_CALLBACK (style_sheet_new_close_request_cb), request);
    g_signal_connect (request->window, "destroy",
                      G_CALLBACK (style_sheet_new_destroy_cb), request);
    g_signal_connect (cancel_button, "clicked", G_CALLBACK (style_sheet_new_cancel_cb), request);
    g_signal_connect (ok_button, "clicked", G_CALLBACK (style_sheet_new_accept_cb), request);
    g_object_unref (builder);
    gtk_window_present (request->window);
    gtk_widget_grab_focus (GTK_WIDGET (request->name_entry));
}

/************************************************************
 *               Style Sheet Selection Dialog               *
 ************************************************************/

void
gnc_style_sheet_select_dialog_new_cb (G_GNUC_UNUSED GtkWidget *widget, gpointer user_data)
{
    gnc_style_sheet_new (static_cast<StyleSheetDialog *>(user_data));
}

void
gnc_style_sheet_select_dialog_edit_cb (G_GNUC_UNUSED GtkWidget *widget, gpointer user_data)
{
    gnc_style_sheet_select_dialog_edit_selected (static_cast<StyleSheetDialog *>(user_data));
}

void
gnc_style_sheet_select_dialog_delete_cb (G_GNUC_UNUSED GtkWidget *widget, gpointer user_data)
{
    auto dialog = static_cast<StyleSheetDialog *>(user_data);
    auto row = style_sheet_selected_row (dialog);
    auto row_data = style_sheet_row_from_object (row);
    auto position = dialog ? gtk_single_selection_get_selected (dialog->selection)
                           : GTK_INVALID_LIST_POSITION;

    if (!row_data || position == GTK_INVALID_LIST_POSITION)
    {
        g_clear_object (&row);
        return;
    }

    if (row_data->options_dialog)
        row_data->options_dialog->odialog->call_close_cb ();
    auto remover = scm_c_eval_string ("gnc:html-style-sheet-remove");
    scm_call_1 (remover, row_data->stylesheet);
    g_list_store_remove (dialog->list_store, position);
    g_object_unref (row);
}

void
gnc_style_sheet_select_dialog_close_cb (G_GNUC_UNUSED GtkWidget *widget, gpointer user_data)
{
    auto dialog = static_cast<StyleSheetDialog *>(user_data);

    gnc_close_gui_component (dialog->component_id);
}

static gboolean
gnc_style_sheet_select_dialog_close_request_cb (GtkWindow *window,
                                                G_GNUC_UNUSED gpointer user_data)
{
    gnc_save_window_size (GNC_PREFS_GROUP, window);
    return FALSE;
}

void
gnc_style_sheet_select_dialog_destroy_cb (G_GNUC_UNUSED GtkWidget *widget, gpointer user_data)
{
    auto dialog = static_cast<StyleSheetDialog *>(user_data);

    if (!dialog)
        return;

    gnc_unregister_gui_component (dialog->component_id);
    gtk_column_view_set_model (dialog->list_view, nullptr);
    g_clear_object (&dialog->selection);
    g_clear_object (&dialog->list_store);
    dialog->toplevel = nullptr;
    gnc_style_sheet_dialog = nullptr;
    g_free (dialog);
}

static void
gnc_style_sheet_window_close_handler (gpointer user_data)
{
    auto dialog = static_cast<StyleSheetDialog *>(user_data);

    g_return_if_fail (dialog);
    gnc_save_window_size (GNC_PREFS_GROUP, dialog->toplevel);
    gtk_window_destroy (dialog->toplevel);
}

static gboolean
gnc_style_sheet_select_dialog_check_escape_cb (G_GNUC_UNUSED GtkEventControllerKey *key,
                                                guint keyval,
                                                G_GNUC_UNUSED guint keycode,
                                                G_GNUC_UNUSED GdkModifierType state,
                                                gpointer user_data)
{
    if (keyval == GDK_KEY_Escape)
    {
        auto dialog = static_cast<StyleSheetDialog *>(user_data);

        gnc_close_gui_component (dialog->component_id);
        return TRUE;
    }
    return FALSE;
}

static void
style_sheet_factory_setup_cb (G_GNUC_UNUSED GtkListItemFactory *factory,
                              GtkListItem *list_item,
                              G_GNUC_UNUSED gpointer user_data)
{
    auto label = gtk_label_new (nullptr);

    gtk_label_set_xalign (GTK_LABEL (label), 0.0F);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (list_item, label);
}

static void
style_sheet_factory_bind_cb (G_GNUC_UNUSED GtkListItemFactory *factory,
                             GtkListItem *list_item,
                             G_GNUC_UNUSED gpointer user_data)
{
    auto row = GTK_STRING_OBJECT (gtk_list_item_get_item (list_item));
    auto label = GTK_LABEL (gtk_list_item_get_child (list_item));

    gtk_label_set_text (label, row ? gtk_string_object_get_string (row) : "");
}

static StyleSheetDialog *
gnc_style_sheet_select_dialog_create (GtkWindow *parent)
{
    auto dialog = g_new0 (StyleSheetDialog, 1);
    auto builder = gtk_builder_new ();
    auto factory = gtk_signal_list_item_factory_new ();
    auto column = static_cast<GtkColumnViewColumn *>(nullptr);
    auto click = gtk_gesture_click_new ();

    gnc_builder_add_from_file (builder, "dialog-report.glade", "select_style_sheet_window");
    dialog->toplevel = GTK_WINDOW (gtk_builder_get_object (builder, "select_style_sheet_window"));
    dialog->list_view = GTK_COLUMN_VIEW (gtk_builder_get_object (builder, "style_sheet_list_view"));
    dialog->session = gnc_get_current_session ();
    dialog->list_store = g_list_store_new (GTK_TYPE_STRING_OBJECT);
    dialog->selection = gtk_single_selection_new (G_LIST_MODEL (g_object_ref (dialog->list_store)));

    gtk_single_selection_set_autoselect (dialog->selection, TRUE);
    gtk_single_selection_set_can_unselect (dialog->selection, FALSE);
    gtk_column_view_set_model (dialog->list_view, GTK_SELECTION_MODEL (dialog->selection));
    g_signal_connect (factory, "setup", G_CALLBACK (style_sheet_factory_setup_cb), nullptr);
    g_signal_connect (factory, "bind", G_CALLBACK (style_sheet_factory_bind_cb), nullptr);
    column = gtk_column_view_column_new (_("Style Sheet Name"), GTK_LIST_ITEM_FACTORY (factory));
    gtk_column_view_column_set_expand (column, TRUE);
    gtk_column_view_append_column (dialog->list_view, column);
    g_object_unref (column);

    gtk_gesture_single_set_button (GTK_GESTURE_SINGLE (click), GDK_BUTTON_PRIMARY);
    g_signal_connect (click, "released", G_CALLBACK (gnc_style_sheet_select_dialog_double_click_cb),
                      dialog);
    gtk_widget_add_controller (GTK_WIDGET (dialog->list_view), GTK_EVENT_CONTROLLER (click));

    gtk_widget_set_name (GTK_WIDGET (dialog->toplevel), "gnc-id-style-sheet-select");
    gnc_widget_style_context_add_class (GTK_WIDGET (dialog->toplevel), "gnc-class-style-sheets");
    if (parent)
        gtk_window_set_transient_for (dialog->toplevel, parent);
    g_signal_connect (dialog->toplevel, "destroy",
                      G_CALLBACK (gnc_style_sheet_select_dialog_destroy_cb), dialog);
    g_signal_connect (dialog->toplevel, "close-request",
                      G_CALLBACK (gnc_style_sheet_select_dialog_close_request_cb), dialog);

    auto event_controller = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET (dialog->toplevel), event_controller);
    g_signal_connect (event_controller, "key-pressed",
                      G_CALLBACK (gnc_style_sheet_select_dialog_check_escape_cb), dialog);
    gnc_style_sheet_select_dialog_fill (dialog);
    gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func, dialog);
    g_object_unref (builder);
    return dialog;
}

void
gnc_style_sheet_dialog_open (GtkWindow *parent)
{
    if (gnc_style_sheet_dialog)
    {
        gtk_window_present (gnc_style_sheet_dialog->toplevel);
        return;
    }

    gnc_style_sheet_dialog = gnc_style_sheet_select_dialog_create (parent);
    gnc_style_sheet_dialog->component_id = gnc_register_gui_component (
        DIALOG_STYLE_SHEETS_CM_CLASS, nullptr, gnc_style_sheet_window_close_handler,
        gnc_style_sheet_dialog);
    gnc_gui_component_set_session (gnc_style_sheet_dialog->component_id,
                                   gnc_style_sheet_dialog->session);
    gnc_restore_window_size (GNC_PREFS_GROUP, gnc_style_sheet_dialog->toplevel, parent);
    gtk_window_present (gnc_style_sheet_dialog->toplevel);
}