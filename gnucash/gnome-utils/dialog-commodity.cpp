/********************************************************************
 * dialog-commodity.c -- "select" and "new" commodity windows       *
 *                       (GnuCash)                                  *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
 * Copyright (c) 2011 Robert Fewell                                 *
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

/** @addtogroup GUI
    @{ */
/** @addtogroup GuiCommodity
    @{ */
/** @file dialog-commodity.c
    @brief "select" and "new" commodity windows
    @author Copyright (C) 2000 Bill Gribble <grib@billgribble.com>
    @author Copyright (c) 2006 David Hampton <hampton@employees.org>
*/


#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <stdio.h>

#include "dialog-commodity.h"
#include "dialog-utils.h"
#include "gnc-engine.h"
#include "gnc-gtk-utils.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

extern "C" {
void gnc_ui_commodity_quote_info_cb (GtkWidget *widget, gpointer user_data);
void gnc_ui_commodity_changed_cb (GtkWidget *widget, gpointer user_data);
}

struct select_commodity_window
{
    GtkWidget * dialog;
    GtkWidget * namespace_combo;
    GtkWidget * commodity_combo;
    GtkWidget * select_user_prompt;
    GtkWidget * new_button;
    GtkWidget * ok_button;

    gnc_commodity * selection;

    gchar *default_cusip;
    gchar *default_fullname;
    gchar *default_mnemonic;
    gchar *default_user_symbol;
    int default_fraction;

    GWeakRef parent;
    GCancellable *cancellable;
    GncSessionOperationContext *operation_context;
    gulong cancellable_id;
    gulong parent_destroy_id;
    GncCommoditySelectionCallback callback;
    gpointer callback_data;
    gboolean completed;
};

struct commodity_window
{
    GtkWidget * dialog;
    GtkWidget * table;
    GtkWidget * fullname_entry;
    GtkWidget * mnemonic_entry;
    GtkWidget * user_symbol_entry;
    GtkWidget * namespace_combo;
    GtkWidget * code_entry;
    GtkWidget * fraction_spinbutton;
    GtkWidget * get_quote_check;
    GtkWidget * source_label;
    GtkWidget * source_button[SOURCE_MAX];
    GtkWidget * source_menu[SOURCE_MAX];
    GtkWidget * quote_tz_label;
    GtkWidget * quote_tz_menu;
    GtkWidget * ok_button;

    guint comm_section_top;
    guint comm_section_bottom;
    guint comm_symbol_line;
    guint fq_section_top;
    guint fq_section_bottom;

    gboolean is_currency;
    GncGUID book_guid;
    GncGUID commodity_guid;
    gboolean editing_existing;

    GWeakRef parent;
    GCancellable *cancellable;
    GncSessionOperationContext *operation_context;
    gulong cancellable_id;
    gulong parent_destroy_id;
    GncCommoditySelectionCallback callback;
    gpointer callback_data;
    gboolean completed;
};

typedef struct select_commodity_window SelectCommodityWindow;
typedef struct commodity_window CommodityWindow;

namespace
{
constexpr const char *COMMODITY_PICKER_DATA = "gnc-commodity-picker-data";
constexpr const char *SOURCE_MENU_DATA = "gnc-source-menu-data";
constexpr const char *SOURCE_SUPPORTED_DATA = "gnc-source-supported";

struct CommodityPicker
{
    GtkEntry *entry;
    GtkStringList *model;
    GtkDropDown *drop_down;
};

struct SourceMenu
{
    guint selected;
    gboolean restoring;
};

static CommodityPicker *
commodity_picker_get (GtkWidget *picker)
{
    return static_cast<CommodityPicker *> (g_object_get_data (G_OBJECT (picker),
                                                                COMMODITY_PICKER_DATA));
}

static void
commodity_picker_select (CommodityPicker *picker, guint position)
{
    auto count = g_list_model_get_n_items (G_LIST_MODEL (picker->model));
    if (position >= count)
    {
        gtk_drop_down_set_selected (picker->drop_down, GTK_INVALID_LIST_POSITION);
        gtk_editable_set_text (GTK_EDITABLE (picker->entry), "");
        return;
    }

    auto item = static_cast<GtkStringObject *> (
        g_list_model_get_item (G_LIST_MODEL (picker->model), position));
    gtk_drop_down_set_selected (picker->drop_down, position);
    gtk_editable_set_text (GTK_EDITABLE (picker->entry),
                           gtk_string_object_get_string (item));
    g_object_unref (item);
}

static void
commodity_picker_selected_changed (GObject *object, GParamSpec *, gpointer user_data)
{
    auto picker = static_cast<CommodityPicker *> (user_data);
    auto position = gtk_drop_down_get_selected (GTK_DROP_DOWN (object));

    if (position != GTK_INVALID_LIST_POSITION)
        commodity_picker_select (picker, position);
}

static void
source_menu_selected_changed (GObject *object, GParamSpec *, gpointer user_data)
{
    auto menu = static_cast<SourceMenu *> (user_data);
    auto drop_down = GTK_DROP_DOWN (object);
    auto position = gtk_drop_down_get_selected (drop_down);

    if (menu->restoring || position == GTK_INVALID_LIST_POSITION)
        return;

    auto model = gtk_drop_down_get_model (drop_down);
    auto item = g_list_model_get_item (model, position);
    auto supported = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (item), SOURCE_SUPPORTED_DATA));
    g_object_unref (item);

    if (supported)
    {
        menu->selected = position;
        return;
    }

    menu->restoring = TRUE;
    gtk_drop_down_set_selected (drop_down, menu->selected);
    menu->restoring = FALSE;
}

static void
source_menu_select (GtkWidget *widget, guint position)
{
    auto menu = static_cast<SourceMenu *> (g_object_get_data (G_OBJECT (widget),
                                                                SOURCE_MENU_DATA));
    g_return_if_fail (menu);

    menu->restoring = TRUE;
    gtk_drop_down_set_selected (GTK_DROP_DOWN (widget), position);
    menu->selected = position;
    menu->restoring = FALSE;
}
}

GtkWidget *
gnc_ui_commodity_picker_new (void)
{
    auto picker = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    gtk_widget_set_hexpand (picker, TRUE);
    gnc_ui_commodity_picker_setup (picker);
    return picker;
}

void
gnc_ui_commodity_picker_setup (GtkWidget *picker)
{
    g_return_if_fail (GTK_IS_BOX (picker));
    if (commodity_picker_get (picker))
        return;

    auto data = g_new0 (CommodityPicker, 1);
    data->entry = GTK_ENTRY (gtk_entry_new ());
    data->model = gtk_string_list_new (nullptr);
    data->drop_down = gnc_gtk_drop_down_new (G_LIST_MODEL (g_object_ref (data->model)), nullptr);

    gtk_widget_set_hexpand (GTK_WIDGET (data->entry), TRUE);
    gtk_widget_set_hexpand (GTK_WIDGET (data->drop_down), FALSE);
    gtk_box_append (GTK_BOX (picker), GTK_WIDGET (data->entry));
    gtk_box_append (GTK_BOX (picker), GTK_WIDGET (data->drop_down));
    g_signal_connect (data->drop_down, "notify::selected",
                      G_CALLBACK (commodity_picker_selected_changed), data);
    g_object_set_data_full (G_OBJECT (picker), COMMODITY_PICKER_DATA, data, g_free);
    g_object_unref (data->model);
}

GtkEntry *
gnc_ui_commodity_picker_get_entry (GtkWidget *picker)
{
    auto data = commodity_picker_get (picker);
    g_return_val_if_fail (data, nullptr);
    return data->entry;
}

/* The commodity selection window */
static SelectCommodityWindow *
gnc_ui_select_commodity_create (const gnc_commodity *orig_sel,
                                dialog_commodity_mode mode);

struct SelectCommodityNewRequest
{
    GWeakRef selector;
};

static void select_commodity_window_finish (SelectCommodityWindow *window,
                                            gnc_commodity *selection);

static void
select_commodity_new_request_free (SelectCommodityNewRequest *request)
{
    g_weak_ref_clear (&request->selector);
    g_free (request);
}

static void
select_commodity_window_free (gpointer data)
{
    auto window = static_cast<SelectCommodityWindow *> (data);
    auto parent = static_cast<GtkWidget *> (g_weak_ref_get (&window->parent));

    if (parent && window->parent_destroy_id)
        g_signal_handler_disconnect (parent, window->parent_destroy_id);
    g_clear_object (&parent);
    if (window->cancellable && window->cancellable_id)
        g_cancellable_disconnect (window->cancellable, window->cancellable_id);
    g_clear_object (&window->cancellable);
    g_weak_ref_clear (&window->parent);
    gnc_session_operation_context_unref (window->operation_context);
    g_free (window->default_cusip);
    g_free (window->default_fullname);
    g_free (window->default_mnemonic);
    g_free (window->default_user_symbol);
    g_free (window);
}

static void
select_commodity_parent_destroyed (GtkWidget *, gpointer user_data)
{
    auto window = static_cast<SelectCommodityWindow *> (user_data);
    window->parent_destroy_id = 0;
    g_cancellable_cancel (window->cancellable);
}

static void
select_commodity_cancelled (GCancellable *, gpointer user_data)
{
    select_commodity_window_finish (static_cast<SelectCommodityWindow *> (user_data),
                                    nullptr);
}

static void
select_commodity_window_finish (SelectCommodityWindow *window,
                                gnc_commodity *selection)
{
    if (window->completed)
        return;

    if (selection && window->operation_context &&
        !gnc_session_operation_context_is_current (window->operation_context))
        selection = nullptr;
    window->completed = TRUE;
    auto callback = window->callback;
    auto callback_data = window->callback_data;
    window->callback = nullptr;
    window->callback_data = nullptr;
    if (callback)
        callback (selection, callback_data);
    gtk_window_destroy (GTK_WINDOW (window->dialog));
}

static void
select_commodity_new_finished (gnc_commodity *commodity, gpointer user_data)
{
    auto request = static_cast<SelectCommodityNewRequest *> (user_data);
    auto dialog = static_cast<GtkWidget *> (g_weak_ref_get (&request->selector));

    if (dialog)
    {
        auto window = static_cast<SelectCommodityWindow *> (
            g_object_get_data (G_OBJECT (dialog), "gnc-select-commodity-window"));
        if (window && window->operation_context &&
            !gnc_session_operation_context_is_current (window->operation_context))
        {
            select_commodity_window_finish (window, nullptr);
            g_object_unref (dialog);
            select_commodity_new_request_free (request);
            return;
        }
        if (window && !window->completed &&
            !g_cancellable_is_cancelled (window->cancellable))
        {
            gtk_widget_set_sensitive (window->new_button, TRUE);
            if (commodity)
            {
                gnc_ui_update_namespace_picker (window->namespace_combo,
                                                gnc_commodity_get_namespace (commodity),
                                                DIAG_COMM_ALL);
                gnc_ui_update_commodity_picker (window->commodity_combo,
                                                 gnc_commodity_get_namespace (commodity),
                                                 gnc_commodity_get_printname (commodity));
            }
        }
        g_object_unref (dialog);
    }
    select_commodity_new_request_free (request);
}

static void
gnc_ui_select_commodity_new_cb (GtkButton *button, gpointer user_data)
{
    auto window = static_cast<SelectCommodityWindow *> (user_data);
    if (!gtk_widget_get_sensitive (GTK_WIDGET (button)))
        return;
    if (window->operation_context &&
        !gnc_session_operation_context_is_current (window->operation_context))
    {
        select_commodity_window_finish (window, nullptr);
        return;
    }

    gtk_widget_set_sensitive (GTK_WIDGET (button), FALSE);
    auto request = g_new0 (SelectCommodityNewRequest, 1);
    auto name_space = gnc_ui_namespace_picker_ns (window->namespace_combo);

    g_weak_ref_init (&request->selector, window->dialog);
    gnc_ui_new_commodity_async_full_with_operation_context (
        name_space, window->dialog,
        window->default_cusip,
        window->default_fullname,
        window->default_mnemonic,
        window->default_user_symbol,
        window->default_fraction,
        window->cancellable, window->operation_context,
        select_commodity_new_finished, request);
    g_free (name_space);
}

static void
gnc_ui_select_commodity_ok_cb (GtkButton *, gpointer user_data)
{
    auto window = static_cast<SelectCommodityWindow *> (user_data);
    if (window->selection)
        select_commodity_window_finish (window, window->selection);
}

static void
gnc_ui_select_commodity_cancel_cb (GtkButton *, gpointer user_data)
{
    select_commodity_window_finish (static_cast<SelectCommodityWindow *> (user_data),
                                    nullptr);
}

static gboolean
gnc_ui_select_commodity_close_request_cb (GtkWindow *, gpointer user_data)
{
    select_commodity_window_finish (static_cast<SelectCommodityWindow *> (user_data),
                                    nullptr);
    return TRUE;
}

static void
gnc_ui_select_commodity_changed_cb (GtkEditable *entry, gpointer user_data)
{
    auto window = static_cast<SelectCommodityWindow *> (user_data);
    if (window->operation_context &&
        !gnc_session_operation_context_is_current (window->operation_context))
    {
        window->selection = nullptr;
        gtk_widget_set_sensitive (window->ok_button, FALSE);
        return;
    }
    auto name_space = gnc_ui_namespace_picker_ns (window->namespace_combo);
    auto fullname = gtk_editable_get_text (entry);

    window->selection = gnc_commodity_table_find_full (gnc_get_current_commodities (),
                                                        name_space, fullname);
    g_free (name_space);
    gtk_widget_set_sensitive (window->ok_button, window->selection != nullptr);
    gtk_window_set_default_widget (GTK_WINDOW (window->dialog),
                                   window->selection ? window->ok_button : nullptr);
}

static void
gnc_ui_select_commodity_namespace_changed_cb (GtkEditable *, gpointer user_data)
{
    auto window = static_cast<SelectCommodityWindow *> (user_data);
    if (window->operation_context &&
        !gnc_session_operation_context_is_current (window->operation_context))
    {
        return;
    }
    auto name_space = gnc_ui_namespace_picker_ns (window->namespace_combo);
    gnc_ui_update_commodity_picker (window->commodity_combo, name_space, nullptr);
    g_free (name_space);
}

static void
gnc_ui_select_commodity_async_full_internal (gnc_commodity *orig_sel,
                                    GtkWidget *parent,
                                    dialog_commodity_mode mode,
                                    const char *user_message,
                                    const char *cusip,
                                    const char *fullname,
                                    const char *mnemonic,
                                    GCancellable *cancellable,
                                    GncSessionOperationContext *operation_context,
                                    GncCommoditySelectionCallback callback,
                                    gpointer user_data)
{
    if (operation_context &&
        !gnc_session_operation_context_is_current (operation_context))
    {
        if (callback)
            callback (nullptr, user_data);
        return;
    }
    const char *initial;
    auto window = gnc_ui_select_commodity_create (orig_sel, mode);

    window->default_cusip = g_strdup (cusip);
    window->default_fullname = g_strdup (fullname);
    window->default_mnemonic = g_strdup (mnemonic);
    window->default_user_symbol = g_strdup ("");
    window->callback = callback;
    window->callback_data = user_data;
    window->cancellable = cancellable ? G_CANCELLABLE (g_object_ref (cancellable))
                                      : g_cancellable_new ();
    window->operation_context =
        gnc_session_operation_context_ref (operation_context);
    g_weak_ref_init (&window->parent, parent);

    if (g_cancellable_is_cancelled (window->cancellable))
    {
        select_commodity_window_finish (window, nullptr);
        return;
    }
    if (parent)
    {
        gtk_window_set_transient_for (GTK_WINDOW (window->dialog), GTK_WINDOW (parent));
        window->parent_destroy_id = g_signal_connect (parent, "destroy",
                                                       G_CALLBACK (select_commodity_parent_destroyed),
                                                       window);
    }
    window->cancellable_id = g_cancellable_connect (window->cancellable,
                                                     G_CALLBACK (select_commodity_cancelled),
                                                     window, nullptr);

    if (user_message)
        initial = user_message;
    else if (cusip || fullname || mnemonic)
        initial = _("\nPlease select a commodity to match");
    else
        initial = "";
    auto prompt = g_strdup_printf (
        "%s%s%s%s%s%s%s", initial,
        fullname ? _("\nCommodity: ") : "", fullname ? fullname : "",
        cusip ? _("\nExchange code (ISIN, CUSIP or similar): ") : "", cusip ? cusip : "",
        mnemonic ? _("\nMnemonic (Ticker symbol or similar): ") : "", mnemonic ? mnemonic : "");
    gtk_label_set_text (GTK_LABEL (window->select_user_prompt), prompt);
    g_free (prompt);
    gtk_window_set_modal (GTK_WINDOW (window->dialog), TRUE);
    gtk_window_present (GTK_WINDOW (window->dialog));
}

void
gnc_ui_select_commodity_async_full (gnc_commodity *orig_sel,
                                    GtkWidget *parent,
                                    dialog_commodity_mode mode,
                                    const char *user_message,
                                    const char *cusip,
                                    const char *fullname,
                                    const char *mnemonic,
                                    GCancellable *cancellable,
                                    GncCommoditySelectionCallback callback,
                                    gpointer user_data)
{
    gnc_ui_select_commodity_async_full_internal (
        orig_sel, parent, mode, user_message, cusip, fullname, mnemonic,
        cancellable, nullptr, callback, user_data);
}

void
gnc_ui_select_commodity_async_full_with_operation_context (
    gnc_commodity *orig_sel, GtkWidget *parent, dialog_commodity_mode mode,
    const char *user_message, const char *cusip, const char *fullname,
    const char *mnemonic, GCancellable *cancellable,
    GncSessionOperationContext *operation_context,
    GncCommoditySelectionCallback callback, gpointer user_data)
{
    gnc_ui_select_commodity_async_full_internal (
        orig_sel, parent, mode, user_message, cusip, fullname, mnemonic,
        cancellable, operation_context, callback, user_data);
}

void
gnc_ui_select_commodity_async (gnc_commodity *orig_sel,
                               GtkWidget *parent,
                               dialog_commodity_mode mode,
                               GCancellable *cancellable,
                               GncCommoditySelectionCallback callback,
                               gpointer user_data)
{
    gnc_ui_select_commodity_async_full (orig_sel, parent, mode, nullptr, nullptr,
                                        nullptr, nullptr, cancellable, callback,
                                        user_data);
}

static SelectCommodityWindow *
gnc_ui_select_commodity_create (const gnc_commodity *orig_sel,
                                dialog_commodity_mode mode)
{
    auto retval = g_new0 (SelectCommodityWindow, 1);
    auto builder = gtk_builder_new ();
    const char *title;
    const char *text;
    const char *initial_namespace = orig_sel ? gnc_commodity_get_namespace (orig_sel) : nullptr;
    const char *initial_commodity = orig_sel ? gnc_commodity_get_printname (orig_sel) : nullptr;

    gnc_builder_add_from_file (builder, "dialog-commodity.ui", "security_selector_window");
    retval->dialog = GTK_WIDGET (gtk_builder_get_object (builder, "security_selector_window"));
    retval->namespace_combo = GTK_WIDGET (gtk_builder_get_object (builder, "ss_namespace_cbwe"));
    retval->commodity_combo = GTK_WIDGET (gtk_builder_get_object (builder, "ss_commodity_cbwe"));
    retval->select_user_prompt = GTK_WIDGET (gtk_builder_get_object (builder, "select_user_prompt"));
    retval->ok_button = GTK_WIDGET (gtk_builder_get_object (builder, "ss_ok_button"));
    auto label = GTK_WIDGET (gtk_builder_get_object (builder, "item_label"));
    retval->new_button = GTK_WIDGET (gtk_builder_get_object (builder, "ss_new_button"));
    auto cancel_button = GTK_WIDGET (gtk_builder_get_object (builder, "ss_cancel_button"));

    gnc_ui_commodity_picker_setup (retval->namespace_combo);
    gnc_ui_commodity_picker_setup (retval->commodity_combo);
    gtk_label_set_mnemonic_widget (GTK_LABEL (gtk_builder_get_object (builder, "label807")),
                                   GTK_WIDGET (gnc_ui_commodity_picker_get_entry (retval->namespace_combo)));
    gtk_label_set_mnemonic_widget (GTK_LABEL (label),
                                   GTK_WIDGET (gnc_ui_commodity_picker_get_entry (retval->commodity_combo)));
    g_signal_connect (gnc_ui_commodity_picker_get_entry (retval->namespace_combo), "changed",
                      G_CALLBACK (gnc_ui_select_commodity_namespace_changed_cb), retval);
    g_signal_connect (gnc_ui_commodity_picker_get_entry (retval->commodity_combo), "changed",
                      G_CALLBACK (gnc_ui_select_commodity_changed_cb), retval);
    g_signal_connect (retval->new_button, "clicked",
                      G_CALLBACK (gnc_ui_select_commodity_new_cb), retval);
    g_signal_connect (cancel_button, "clicked", G_CALLBACK (gnc_ui_select_commodity_cancel_cb), retval);
    g_signal_connect (retval->ok_button, "clicked", G_CALLBACK (gnc_ui_select_commodity_ok_cb), retval);
    g_signal_connect (retval->dialog, "close-request",
                      G_CALLBACK (gnc_ui_select_commodity_close_request_cb), retval);
    g_object_set_data_full (G_OBJECT (retval->dialog), "gnc-select-commodity-window", retval,
                            select_commodity_window_free);

    gtk_widget_set_name (retval->dialog, "gnc-id-security-select");
    gnc_widget_style_context_add_class (retval->dialog, "gnc-class-securities");
    gtk_label_set_text (GTK_LABEL (retval->select_user_prompt), "");
    switch (mode)
    {
    case DIAG_COMM_ALL:
        title = _("Select security/currency"); text = _("_Security/currency"); break;
    case DIAG_COMM_NON_CURRENCY:
    case DIAG_COMM_NON_CURRENCY_SELECT:
        title = _("Select security"); text = _("_Security"); break;
    case DIAG_COMM_CURRENCY:
    default:
        title = _("Select currency"); text = _("Cu_rrency");
        gtk_widget_set_visible (retval->new_button, FALSE); break;
    }
    gtk_window_set_title (GTK_WINDOW (retval->dialog), title);
    gtk_label_set_text_with_mnemonic (GTK_LABEL (label), text);
    gnc_ui_update_namespace_picker (retval->namespace_combo, initial_namespace, mode);
    auto name_space = gnc_ui_namespace_picker_ns (retval->namespace_combo);
    gnc_ui_update_commodity_picker (retval->commodity_combo, name_space, initial_commodity);
    g_free (name_space);
    g_object_unref (builder);
    return retval;
}
static int
collate(gconstpointer a, gconstpointer b)
{
    if (!a)
        return -1;
    if (!b)
        return 1;
    return g_utf8_collate (static_cast<const char*>(a), static_cast<const char*>(b));
}


void
gnc_ui_update_commodity_picker (GtkWidget *cbwe,
                                const gchar * name_space,
                                const gchar * init_string)
{
    GList      * commodities;
    GList      * iterator = nullptr;
    GList      * commodity_items = nullptr;
    gnc_commodity_table *table;
    gint current = 0, match = 0;
    gchar *name;
    auto picker = commodity_picker_get (cbwe);

    g_return_if_fail(picker);
    g_return_if_fail(name_space);

    gtk_string_list_splice (picker->model, 0,
                            g_list_model_get_n_items (G_LIST_MODEL (picker->model)),
                            nullptr);

    table = gnc_commodity_table_get_table (gnc_get_current_book ());
    commodities = gnc_commodity_table_get_commodities(table, name_space);
    for (iterator = commodities; iterator; iterator = iterator->next)
    {
        commodity_items =
            g_list_prepend (commodity_items,
                            (gpointer) gnc_commodity_get_printname(GNC_COMMODITY(iterator->data)));
    }
    g_list_free(commodities);

    commodity_items = g_list_sort(commodity_items, collate);
    for (iterator = commodity_items; iterator; iterator = iterator->next)
    {
        name = (char *)iterator->data;
        gtk_string_list_append (picker->model, name);

        if (init_string && g_utf8_collate(name, init_string) == 0)
            match = current;
        current++;
    }

    commodity_picker_select (picker, current ? match : GTK_INVALID_LIST_POSITION);
    g_list_free(commodity_items);
}


/********************************************************************
 *
 * Commodity Selector dialog routines are above this line.
 *
 * Commodity New/Edit dialog routines are below this line.
 *
 ********************************************************************/
static void
gnc_set_commodity_section_sensitivity (GtkWidget *widget, gpointer user_data)
{
    auto cw = static_cast<CommodityWindow*>(user_data);
    gint offset = 0;

    gtk_grid_query_child (GTK_GRID (cw->table), widget,
                          nullptr, &offset, nullptr, nullptr);

    if ((offset < (gint)cw->comm_section_top) || (offset >= (gint)cw->comm_section_bottom))
        return;
    if (cw->is_currency)
        gtk_widget_set_sensitive(widget, offset == (gint)cw->comm_symbol_line);
}

static void
gnc_commodity_foreach_grid_child (GtkGrid *grid,
                                  void (*callback) (GtkWidget *, gpointer),
                                  gpointer user_data)
{
    for (GtkWidget *child = gtk_widget_get_first_child (GTK_WIDGET (grid)); child;
         child = gtk_widget_get_next_sibling (child))
        callback (child, user_data);
}

static guint
gnc_grid_get_row (GtkGrid *grid, GtkWidget *child)
{
    gint row = 0;
    gtk_grid_query_child (grid, child, nullptr, &row, nullptr, nullptr);
    return row;
}

static void
gnc_ui_update_commodity_info (CommodityWindow *cw)
{
    gnc_commodity_foreach_grid_child (GTK_GRID (cw->table),
                                      gnc_set_commodity_section_sensitivity, cw);
}


static void
gnc_set_fq_sensitivity (GtkWidget *widget, gpointer user_data)
{
    auto cw = static_cast<CommodityWindow*>(user_data);
    gint offset = 0;

    gtk_grid_query_child (GTK_GRID (cw->table), widget,
                          nullptr, &offset, nullptr, nullptr);

    if ((offset < (gint)cw->fq_section_top) || (offset >= (gint)cw->fq_section_bottom))
        return;
    g_object_set(widget, "sensitive", FALSE, nullptr);
}


static void
gnc_ui_update_fq_info (CommodityWindow *cw)
{
    gnc_commodity_foreach_grid_child (GTK_GRID (cw->table),
                                      gnc_set_fq_sensitivity, cw);
}


/********************************************************************
 * gnc_ui_update_namespace_picker
 ********************************************************************/
void
gnc_ui_update_namespace_picker (GtkWidget *cbwe,
                                const char * init_string,
                                dialog_commodity_mode mode)
{
    GList *namespaces, *node;
    guint current = 0, match = 0;
    gboolean matched = FALSE;
    auto picker = commodity_picker_get (cbwe);

    g_return_if_fail(picker);

    gtk_string_list_splice (picker->model, 0,
                            g_list_model_get_n_items (G_LIST_MODEL (picker->model)),
                            nullptr);

    /* fetch a list of the namespaces */
    switch (mode)
    {
    case DIAG_COMM_ALL:
        namespaces =
            gnc_commodity_table_get_namespaces (gnc_get_current_commodities());
        break;

    case DIAG_COMM_NON_CURRENCY:
    case DIAG_COMM_NON_CURRENCY_SELECT:
        namespaces =
            gnc_commodity_table_get_namespaces (gnc_get_current_commodities());
        node = g_list_find_custom (namespaces, GNC_COMMODITY_NS_CURRENCY, collate);
        if (node)
        {
            namespaces = g_list_remove_link (namespaces, node);
            g_list_free_1 (node);
        }

        if (gnc_commodity_namespace_is_iso (init_string))
            init_string = nullptr;
        break;

    case DIAG_COMM_CURRENCY:
    default:
        namespaces = g_list_prepend (nullptr, (gpointer)GNC_COMMODITY_NS_CURRENCY);
        break;
    }

    /* First insert "Currencies" entry if requested */
    if (mode == DIAG_COMM_CURRENCY || mode == DIAG_COMM_ALL)
    {
        gtk_string_list_append (picker->model, _(GNC_COMMODITY_NS_ISO_GUI));

        if (init_string &&
            (g_utf8_collate(GNC_COMMODITY_NS_ISO_GUI, init_string) == 0))
        {
            matched = TRUE;
            match = current;
        }
        current++;
    }

    /* Next insert "All non-currency" entry if requested */
    if (mode == DIAG_COMM_NON_CURRENCY_SELECT || mode == DIAG_COMM_ALL)
    {
        gtk_string_list_append (picker->model, GNC_COMMODITY_NS_NONISO_GUI);
        if (init_string &&
            (g_utf8_collate(GNC_COMMODITY_NS_NONISO_GUI, init_string) == 0))
        {
            matched = TRUE;
            match = current;
        }
        current++;
    }

    /* add all others to the combobox */
    namespaces = g_list_sort(namespaces, collate);
    for (node = namespaces; node; node = node->next)
    {
        auto ns = static_cast<const char*>(node->data);
        /* Skip template, legacy and currency namespaces.
           The latter was added as first entry earlier */
        if ((g_utf8_collate(ns, GNC_COMMODITY_NS_LEGACY) == 0) ||
            (g_utf8_collate(ns, GNC_COMMODITY_NS_TEMPLATE ) == 0) ||
            (g_utf8_collate(ns, GNC_COMMODITY_NS_CURRENCY ) == 0))
            continue;

        gtk_string_list_append (picker->model, ns);

        if (init_string &&
            (g_utf8_collate(ns, init_string) == 0))
        {
            matched = TRUE;
            match = current;
        }
        current++;
    }

    commodity_picker_select (picker, matched || current ? match : GTK_INVALID_LIST_POSITION);
    g_list_free(namespaces);
}


gchar *
gnc_ui_namespace_picker_ns (GtkWidget *cbwe)
{
    const gchar *name_space;
    auto picker = commodity_picker_get (cbwe);

    g_return_val_if_fail(picker, nullptr);

    name_space = gtk_editable_get_text (GTK_EDITABLE (picker->entry));

    /* Map several currency related names to one common namespace */
    if ((g_strcmp0 (name_space, GNC_COMMODITY_NS_ISO) == 0) ||
        (g_strcmp0 (name_space, GNC_COMMODITY_NS_ISO_GUI) == 0) ||
        (g_strcmp0 (name_space, _(GNC_COMMODITY_NS_ISO_GUI)) == 0))
        return g_strdup(GNC_COMMODITY_NS_CURRENCY);
    else
        return g_strdup(name_space);
}


/********************************************************************
 * gnc_ui_commodity_quote_info_cb                                   *
 *******************************************************************/
void
gnc_ui_commodity_quote_info_cb (GtkWidget *w, gpointer data)
{
    auto cw = static_cast<CommodityWindow*>(data);
    gboolean get_quote, allow_src, active;
    const gchar *text;
    gint i;

    ENTER(" ");
    get_quote = gtk_check_button_get_active (GTK_CHECK_BUTTON (w));

    text = gtk_editable_get_text (GTK_EDITABLE (
        gnc_ui_commodity_picker_get_entry (cw->namespace_combo)));

    allow_src = !gnc_commodity_namespace_is_iso(text);

    gtk_widget_set_sensitive(cw->source_label, get_quote && allow_src);

    for (i = SOURCE_SINGLE; i < SOURCE_MAX; i++)
    {
        if (!cw->source_button[i])
            continue;
        active =
            gtk_check_button_get_active(GTK_CHECK_BUTTON(cw->source_button[i]));
        gtk_widget_set_sensitive(cw->source_button[i], get_quote && allow_src);
        gtk_widget_set_sensitive(cw->source_menu[i], get_quote && allow_src && active);
    }
    gtk_widget_set_sensitive(cw->quote_tz_label, get_quote);
    gtk_widget_set_sensitive(cw->quote_tz_menu, get_quote);
    LEAVE(" ");
}


void
gnc_ui_commodity_changed_cb(GtkWidget * dummy, gpointer user_data)
{
    auto w = static_cast<CommodityWindow*>(user_data);
    gchar *name_space;
    const char * fullname;
    const char * mnemonic;
    gboolean ok;

    ENTER("widget=%p, user_data=%p", dummy, user_data);
    if (!w->is_currency)
    {
        name_space = gnc_ui_namespace_picker_ns (w->namespace_combo);
        fullname  = gtk_editable_get_text (GTK_EDITABLE (w->fullname_entry));
        mnemonic  = gtk_editable_get_text (GTK_EDITABLE (w->mnemonic_entry));
        DEBUG("namespace=%s, name=%s, mnemonic=%s", name_space, fullname, mnemonic);
        ok = (fullname    && name_space    && mnemonic &&
              fullname[0] && name_space[0] && mnemonic[0]);
        g_free(name_space);
    }
    else
    {
        ok = TRUE;
    }
    gtk_widget_set_sensitive(w->ok_button, ok);
    gtk_window_set_default_widget (GTK_WINDOW (w->dialog), ok ? w->ok_button : nullptr);
    LEAVE("sensitive=%d, default = %d", ok, ok ? 0 : 1);
}


/********************************************************************\
 * gnc_ui_source_menu_create                                        *
 *   create the menu of stock quote sources                         *
 *                                                                  *
 * Args:    account - account to use to set default choice          *
 * Returns: the menu                                                *
 \*******************************************************************/
static GtkWidget *
gnc_ui_source_menu_create(QuoteSourceType type)
{
    gint i, max;
    const gchar *name;
    gboolean supported;
    GtkStringList *store;
    GtkWidget *drop_down;
    gnc_quote_source *source;
    guint initial = GTK_INVALID_LIST_POSITION;

    store = gtk_string_list_new (nullptr);
    if (type == SOURCE_CURRENCY)
    {
        gtk_string_list_append (store, _("Currency"));
        auto item = g_list_model_get_item (G_LIST_MODEL (store), 0);
        g_object_set_data (G_OBJECT (item), SOURCE_SUPPORTED_DATA, GINT_TO_POINTER (TRUE));
        g_object_unref (item);
        initial = 0;
    }
    else
    {
        max = gnc_quote_source_num_entries(type);
        for (i = 0; i < max; i++)
        {
            source = gnc_quote_source_lookup_by_ti(type, i);
            if (source == nullptr)
                break;
            name = gnc_quote_source_get_user_name(source);
            supported = gnc_quote_source_get_supported(source);
            gtk_string_list_append (store, g_dpgettext2(NULL, "FQ Source", name));
            auto position = g_list_model_get_n_items (G_LIST_MODEL (store)) - 1;
            auto item = g_list_model_get_item (G_LIST_MODEL (store), position);
            g_object_set_data (G_OBJECT (item), SOURCE_SUPPORTED_DATA, GINT_TO_POINTER (supported));
            g_object_unref (item);
            if (supported && initial == GTK_INVALID_LIST_POSITION)
                initial = position;
        }
    }

    drop_down = GTK_WIDGET (gnc_gtk_drop_down_new (G_LIST_MODEL (store), nullptr));
    auto menu = g_new0 (SourceMenu, 1);
    menu->selected = initial;
    g_object_set_data_full (G_OBJECT (drop_down), SOURCE_MENU_DATA, menu, g_free);
    g_signal_connect (drop_down, "notify::selected",
                      G_CALLBACK (source_menu_selected_changed), menu);
    gtk_drop_down_set_selected (GTK_DROP_DOWN (drop_down), initial);
    return drop_down;
}


/********************************************************************
 * price quote timezone handling                                    *
 *******************************************************************/
static const gchar *
known_timezones[] =
{
    "Asia/Tokyo",
    "Australia/Sydney",
    "America/New_York",
    "America/Chicago",
    "Europe/London",
    "Europe/Paris",
    nullptr
};


static guint
gnc_find_timezone_menu_position(const gchar *timezone)
{
    /* returns 0 on failure, position otherwise. */
    gboolean found = FALSE;
    guint i = 0;
    while (!found && known_timezones[i])
    {
        if (g_strcmp0(timezone, known_timezones[i]) != 0)
        {
            i++;
        }
        else
        {
            found = TRUE;
        }
    }
    if (found) return i + 1;
    return 0;
}


static const gchar *
gnc_timezone_menu_position_to_string(guint pos)
{
    if (pos == 0) return nullptr;
    return known_timezones[pos - 1];
}


static GtkWidget *
gnc_ui_quote_tz_menu_create(void)
{
    GtkStringList *store;
    GtkWidget *drop_down;
    const gchar **itemstr;

    /* add items here as needed, but bear in mind that right now these
       must be timezones that GNU libc understands.  Also, I'd prefer if
       we only add things here we *know* we need.  That's because in
       order to be portable to non GNU OSes, we may have to support
       whatever we add here manually on those systems. */

    store = gtk_string_list_new (nullptr);
    gtk_string_list_append (store, _("Use local time"));
    for (itemstr = &known_timezones[0]; *itemstr; itemstr++)
        gtk_string_list_append (store, *itemstr);

    drop_down = GTK_WIDGET (gnc_gtk_drop_down_new (G_LIST_MODEL (store), nullptr));
    gtk_drop_down_set_selected (GTK_DROP_DOWN (drop_down), 0);
    return drop_down;
}


gnc_commodity *gnc_ui_commodity_dialog_to_object (CommodityWindow *window);

static void commodity_window_finish (CommodityWindow *window,
                                     gnc_commodity *commodity);

static void
commodity_window_free (gpointer data)
{
    auto window = static_cast<CommodityWindow *> (data);
    auto parent = static_cast<GtkWidget *> (g_weak_ref_get (&window->parent));

    if (parent && window->parent_destroy_id)
        g_signal_handler_disconnect (parent, window->parent_destroy_id);
    g_clear_object (&parent);
    if (window->cancellable && window->cancellable_id)
        g_cancellable_disconnect (window->cancellable, window->cancellable_id);
    g_clear_object (&window->cancellable);
    g_weak_ref_clear (&window->parent);
    gnc_session_operation_context_unref (window->operation_context);
    g_free (window);
}

static void
commodity_window_parent_destroyed (GtkWidget *, gpointer user_data)
{
    auto window = static_cast<CommodityWindow *> (user_data);
    window->parent_destroy_id = 0;
    g_cancellable_cancel (window->cancellable);
}

static void
commodity_window_cancelled (GCancellable *, gpointer user_data)
{
    commodity_window_finish (static_cast<CommodityWindow *> (user_data), nullptr);
}

static void
commodity_window_finish (CommodityWindow *window, gnc_commodity *commodity)
{
    if (window->completed)
        return;

    window->completed = TRUE;
    auto callback = window->callback;
    auto callback_data = window->callback_data;
    window->callback = nullptr;
    window->callback_data = nullptr;
    if (callback)
        callback (commodity, callback_data);
    gtk_window_destroy (GTK_WINDOW (window->dialog));
}

static void
commodity_window_ok_clicked (GtkButton *, gpointer user_data)
{
    auto window = static_cast<CommodityWindow *> (user_data);
    auto operation_started = !window->operation_context ||
        gnc_session_operation_context_begin (window->operation_context);
    if (!operation_started)
    {
        commodity_window_finish (window, nullptr);
        return;
    }
    auto commodity = gnc_ui_commodity_dialog_to_object (window);
    if (window->operation_context)
        gnc_session_operation_context_end (window->operation_context);
    if (commodity)
        commodity_window_finish (window, commodity);
}

static void
commodity_window_cancel_clicked (GtkButton *, gpointer user_data)
{
    commodity_window_finish (static_cast<CommodityWindow *> (user_data), nullptr);
}

static void
commodity_window_help_clicked (GtkButton *, gpointer user_data)
{
    auto window = static_cast<CommodityWindow *> (user_data);
    gnc_gnome_help (GTK_WINDOW (window->dialog), DF_MANUAL, DL_COMMODITY);
}

static gboolean
commodity_window_close_request (GtkWindow *, gpointer user_data)
{
    commodity_window_finish (static_cast<CommodityWindow *> (user_data), nullptr);
    return TRUE;
}
/*******************************************************
 * Build the new/edit commodity dialog box             *
 *******************************************************/
static CommodityWindow *
gnc_ui_build_commodity_dialog(const char * selected_namespace,
                              GtkWidget  *parent,
                              const char * fullname,
                              const char * mnemonic,
                              const char * user_symbol,
                              const char * cusip,
                              int          fraction,
                              gboolean     edit)
{
    CommodityWindow * retval = g_new0(CommodityWindow, 1);
    GtkWidget *box;
    GtkWidget *menu;
    GtkWidget *widget, *sec_label;
    GtkBuilder *builder;
    gboolean include_iso;
    const gchar *title;
    gchar *text;

    ENTER("widget=%p, selected namespace=%s, fullname=%s, mnemonic=%s",
          parent, selected_namespace, fullname, mnemonic);

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-commodity.ui", "adjustment1");
    gnc_builder_add_from_file (builder, "dialog-commodity.ui", "security_window");

    retval->dialog = GTK_WIDGET (gtk_builder_get_object (builder, "security_window"));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(retval->dialog), "gnc-id-security");
    gnc_widget_style_context_add_class (GTK_WIDGET(retval->dialog), "gnc-class-securities");

    if (parent != nullptr)
        gtk_window_set_transient_for (GTK_WINDOW (retval->dialog), GTK_WINDOW (parent));

    retval->editing_existing = FALSE;

    /* Get widget pointers */
    retval->fullname_entry = GTK_WIDGET(gtk_builder_get_object (builder, "fullname_entry"));
    retval->mnemonic_entry = GTK_WIDGET(gtk_builder_get_object (builder, "mnemonic_entry"));
    retval->user_symbol_entry = GTK_WIDGET(gtk_builder_get_object (builder, "user_symbol_entry"));
    retval->namespace_combo = GTK_WIDGET(gtk_builder_get_object (builder, "namespace_cbwe"));
    retval->code_entry = GTK_WIDGET(gtk_builder_get_object (builder, "code_entry"));
    retval->fraction_spinbutton = GTK_WIDGET(gtk_builder_get_object (builder, "fraction_spinbutton"));
    retval->ok_button = GTK_WIDGET (gtk_builder_get_object (builder, "ok_button"));
    auto help_button = GTK_WIDGET (gtk_builder_get_object (builder, "help_button"));
    auto cancel_button = GTK_WIDGET (gtk_builder_get_object (builder, "cancel_button"));
    g_signal_connect (help_button, "clicked", G_CALLBACK (commodity_window_help_clicked), retval);
    g_signal_connect (cancel_button, "clicked", G_CALLBACK (commodity_window_cancel_clicked), retval);
    g_signal_connect (retval->ok_button, "clicked", G_CALLBACK (commodity_window_ok_clicked), retval);
    g_signal_connect (retval->dialog, "close-request",
                      G_CALLBACK (commodity_window_close_request), retval);
    g_object_set_data_full (G_OBJECT (retval->dialog), "gnc-commodity-window", retval,
                            commodity_window_free);
    retval->get_quote_check = GTK_WIDGET(gtk_builder_get_object (builder, "get_quote_check"));
    retval->source_label = GTK_WIDGET(gtk_builder_get_object (builder, "source_label"));
    retval->source_button[SOURCE_SINGLE] = GTK_WIDGET(gtk_builder_get_object (builder, "single_source_button"));
    retval->source_button[SOURCE_MULTI] = GTK_WIDGET(gtk_builder_get_object (builder, "multi_source_button"));
    retval->quote_tz_label = GTK_WIDGET(gtk_builder_get_object (builder, "quote_tz_label"));

    gnc_ui_commodity_picker_setup (retval->namespace_combo);
    gtk_label_set_mnemonic_widget (GTK_LABEL (gtk_builder_get_object (builder, "label812")),
                                   GTK_WIDGET (gnc_ui_commodity_picker_get_entry (retval->namespace_combo)));
    g_signal_connect (gnc_ui_commodity_picker_get_entry (retval->namespace_combo), "changed",
                      G_CALLBACK (gnc_ui_commodity_changed_cb), retval);

    /* Determine the commodity section of the dialog */
    retval->table = GTK_WIDGET(gtk_builder_get_object (builder, "edit_table"));
    sec_label = GTK_WIDGET(gtk_builder_get_object (builder, "security_label"));
    retval->comm_section_top = gnc_grid_get_row (GTK_GRID (retval->table), sec_label);

    widget = GTK_WIDGET(gtk_builder_get_object (builder, "quote_label"));
    retval->comm_section_bottom = gnc_grid_get_row (GTK_GRID (retval->table), widget);

    retval->comm_symbol_line = gnc_grid_get_row (GTK_GRID (retval->table),
                                                 retval->user_symbol_entry);

    /* Build custom widgets */
    box = GTK_WIDGET(gtk_builder_get_object (builder, "single_source_box"));
    if (gnc_commodity_namespace_is_iso(selected_namespace))
    {
        menu = gnc_ui_source_menu_create(SOURCE_CURRENCY);
    }
    else
    {
        menu = gnc_ui_source_menu_create(SOURCE_SINGLE);
    }
    retval->source_menu[SOURCE_SINGLE] = menu;
    gnc_box_append_full(GTK_BOX(box), menu, TRUE, TRUE, 0);

    box = GTK_WIDGET(gtk_builder_get_object (builder, "multi_source_box"));
    menu = gnc_ui_source_menu_create(SOURCE_MULTI);
    retval->source_menu[SOURCE_MULTI] = menu;
    gnc_box_append_full(GTK_BOX(box), menu, TRUE, TRUE, 0);

    if (gnc_quote_source_num_entries(SOURCE_UNKNOWN))
    {
        retval->source_button[SOURCE_UNKNOWN] =
            GTK_WIDGET(gtk_builder_get_object (builder, "unknown_source_button"));
        box = GTK_WIDGET(gtk_builder_get_object (builder, "unknown_source_box"));
        menu = gnc_ui_source_menu_create(SOURCE_UNKNOWN);
        retval->source_menu[SOURCE_UNKNOWN] = menu;
        gnc_box_append_full(GTK_BOX(box), menu, TRUE, TRUE, 0);
    }
    else
    {
        gtk_grid_set_row_spacing(GTK_GRID(retval->table), 0);

        widget = GTK_WIDGET(gtk_builder_get_object (builder, "unknown_source_alignment"));
        gtk_widget_unparent (widget);

        widget = GTK_WIDGET(gtk_builder_get_object (builder, "unknown_source_box"));
        gtk_widget_unparent (widget);
    }

    box = GTK_WIDGET(gtk_builder_get_object (builder, "quote_tz_box"));
    retval->quote_tz_menu = gnc_ui_quote_tz_menu_create();
    gnc_box_append_full(GTK_BOX(box), retval->quote_tz_menu, TRUE, TRUE, 0);

    /* Commodity editing is next to nil */
    if (gnc_commodity_namespace_is_iso(selected_namespace))
    {
        retval->is_currency = TRUE;
        gnc_ui_update_commodity_info (retval);
        include_iso = TRUE;
        title = _("Edit currency");
        text = g_strdup_printf("<b>%s</b>", _("Currency Information"));
    }
    else
    {
        include_iso = FALSE;
        title = edit ? _("Edit security") : _("New security");
        text = g_strdup_printf("<b>%s</b>", _("Security Information"));
    }
    gtk_window_set_title(GTK_WINDOW(retval->dialog), title);
    gtk_label_set_markup(GTK_LABEL(sec_label), text);
    g_free(text);

    /* Are price quotes supported */
    if (gnc_quote_source_fq_installed())
    {
        gtk_widget_unparent (GTK_WIDGET(gtk_builder_get_object (builder, "finance_quote_warning")));
    }
    else
    {
        /* Determine the price quote of the dialog */
        widget = GTK_WIDGET(gtk_builder_get_object (builder, "fq_warning_alignment"));
        retval->fq_section_top = gnc_grid_get_row (GTK_GRID (retval->table), widget);

        widget = GTK_WIDGET(gtk_builder_get_object (builder, "bottom_alignment"));
        retval->fq_section_bottom = gnc_grid_get_row (GTK_GRID (retval->table), widget);

        gnc_ui_update_fq_info (retval);
    }

#ifdef DRH
    g_signal_connect (G_OBJECT (retval->dialog), "close",
                      G_CALLBACK (commodity_close), retval);
#endif
    /* Fill in any data, top to bottom */
    gtk_editable_set_text (GTK_EDITABLE (retval->fullname_entry), fullname ? fullname : "");
    gtk_editable_set_text (GTK_EDITABLE (retval->mnemonic_entry), mnemonic ? mnemonic : "");
    gtk_editable_set_text (GTK_EDITABLE (retval->user_symbol_entry), user_symbol ? user_symbol : "");
    gnc_ui_update_namespace_picker(retval->namespace_combo,
                                   selected_namespace,
                                   include_iso ? DIAG_COMM_ALL : DIAG_COMM_NON_CURRENCY);
    gtk_editable_set_text (GTK_EDITABLE (retval->code_entry), cusip ? cusip : "");

    if (fraction > 0)
        gtk_spin_button_set_value (GTK_SPIN_BUTTON (retval->fraction_spinbutton),
                                   fraction);

    g_object_unref(G_OBJECT(builder));

    LEAVE(" ");
    return retval;
}


static void
gnc_ui_commodity_update_quote_info(CommodityWindow *win,
                                   gnc_commodity *commodity)
{
    gnc_quote_source *source;
    QuoteSourceType type;
    gboolean has_quote_src;
    const char *quote_tz;
    int pos = 0;

    ENTER(" ");
    if (!commodity)
    {
        gtk_drop_down_set_selected (GTK_DROP_DOWN (win->quote_tz_menu), 0);
        LEAVE(" ");
        return;
    }
    has_quote_src = gnc_commodity_get_quote_flag (commodity);
    source = gnc_commodity_get_quote_source (commodity);
    if (source == nullptr)
        source = gnc_commodity_get_default_quote_source (commodity);
    quote_tz = gnc_commodity_get_quote_tz (commodity);

    gtk_check_button_set_active (GTK_CHECK_BUTTON (win->get_quote_check),
                                  has_quote_src);
    if (!gnc_commodity_is_iso(commodity))
    {
        type = gnc_quote_source_get_type(source);
        gtk_check_button_set_active(GTK_CHECK_BUTTON(win->source_button[type]), TRUE);
        source_menu_select (win->source_menu[type], gnc_quote_source_get_index (source));
    }

    if (quote_tz)
    {
        pos = gnc_find_timezone_menu_position(quote_tz);
//    if(pos == 0) {
//      PWARN("Unknown price quote timezone (%s), resetting to default.",
//	    quote_tz ? quote_tz : "(null)");
//    }
    }
    gtk_drop_down_set_selected (GTK_DROP_DOWN (win->quote_tz_menu), pos);
    LEAVE(" ");
}


static void
gnc_ui_common_commodity_async (gnc_commodity *commodity,
                               GtkWidget *parent,
                               const char *name_space,
                               const char *cusip,
                               const char *fullname,
                               const char *mnemonic,
                               const char *user_symbol,
                               int fraction,
                               GCancellable *cancellable,
                               GncSessionOperationContext *operation_context,
                               GncCommoditySelectionCallback callback,
                               gpointer user_data)
{
    if (cancellable && g_cancellable_is_cancelled (cancellable))
    {
        if (callback)
            callback (nullptr, user_data);
        return;
    }
    if (operation_context &&
        !gnc_session_operation_context_is_current (operation_context))
    {
        if (callback)
            callback (nullptr, user_data);
        return;
    }

    if (commodity)
    {
        name_space = gnc_commodity_get_namespace (commodity);
        fullname = gnc_commodity_get_fullname (commodity);
        mnemonic = gnc_commodity_get_mnemonic (commodity);
        user_symbol = gnc_commodity_get_nice_symbol (commodity);
        cusip = gnc_commodity_get_cusip (commodity);
        fraction = gnc_commodity_get_fraction (commodity);
    }
    else if (gnc_commodity_namespace_is_iso (name_space))
    {
        name_space = nullptr;
    }

    auto book = gnc_get_current_book ();
    if (!book)
    {
        if (callback)
            callback (nullptr, user_data);
        return;
    }

    auto window = gnc_ui_build_commodity_dialog (name_space, parent, fullname,
                                                  mnemonic, user_symbol, cusip,
                                                  fraction, commodity != nullptr);
    window->book_guid = *qof_instance_get_guid (QOF_INSTANCE (book));
    window->editing_existing = commodity != nullptr;
    if (commodity)
        window->commodity_guid = *qof_instance_get_guid (QOF_INSTANCE (commodity));
    window->callback = callback;
    window->callback_data = user_data;
    window->cancellable = cancellable ? G_CANCELLABLE (g_object_ref (cancellable))
                                      : g_cancellable_new ();
    window->operation_context =
        gnc_session_operation_context_ref (operation_context);
    g_weak_ref_init (&window->parent, parent);
    gnc_ui_commodity_update_quote_info (window, commodity);
    gnc_ui_commodity_quote_info_cb (window->get_quote_check, window);

    if (parent)
    {
        window->parent_destroy_id = g_signal_connect (parent, "destroy",
                                                       G_CALLBACK (commodity_window_parent_destroyed),
                                                       window);
    }
    window->cancellable_id = g_cancellable_connect (window->cancellable,
                                                     G_CALLBACK (commodity_window_cancelled),
                                                     window, nullptr);
    gtk_window_set_modal (GTK_WINDOW (window->dialog), TRUE);
    gtk_window_present (GTK_WINDOW (window->dialog));
}

void
gnc_ui_new_commodity_async_full (const char *name_space,
                                 GtkWidget *parent,
                                 const char *cusip,
                                 const char *fullname,
                                 const char *mnemonic,
                                 const char *user_symbol,
                                 int fraction,
                                 GCancellable *cancellable,
                                 GncCommoditySelectionCallback callback,
                                 gpointer user_data)
{
    gnc_ui_common_commodity_async (nullptr, parent, name_space, cusip, fullname,
                                   mnemonic, user_symbol, fraction, cancellable,
                                   nullptr, callback, user_data);
}

void
gnc_ui_new_commodity_async_full_with_operation_context (
    const char *name_space, GtkWidget *parent, const char *cusip,
    const char *fullname, const char *mnemonic, const char *user_symbol,
    int fraction, GCancellable *cancellable,
    GncSessionOperationContext *operation_context,
    GncCommoditySelectionCallback callback, gpointer user_data)
{
    gnc_ui_common_commodity_async (nullptr, parent, name_space, cusip, fullname,
                                   mnemonic, user_symbol, fraction, cancellable,
                                   operation_context, callback, user_data);
}

void
gnc_ui_new_commodity_async (const char *default_namespace,
                            GtkWidget *parent,
                            GCancellable *cancellable,
                            GncCommoditySelectionCallback callback,
                            gpointer user_data)
{
    gnc_ui_new_commodity_async_full (default_namespace, parent, nullptr, nullptr,
                                     nullptr, nullptr, 0, cancellable, callback,
                                     user_data);
}

void
gnc_ui_edit_commodity_async (gnc_commodity *commodity,
                             GtkWidget *parent,
                             GCancellable *cancellable,
                             GncCommoditySelectionCallback callback,
                             gpointer user_data)
{
    g_return_if_fail (commodity);
    gnc_ui_common_commodity_async (commodity, parent, nullptr, nullptr, nullptr,
                                   nullptr, nullptr, 0, cancellable, nullptr, callback,
                                   user_data);
}

/********************************************************************
 * gnc_ui_commodity_dialog_to_object()
 ********************************************************************/
gnc_commodity *
gnc_ui_commodity_dialog_to_object (CommodityWindow *window)
{
    auto name_space = gnc_ui_namespace_picker_ns (window->namespace_combo);
    auto book = gnc_get_current_book ();
    auto fullname = gtk_editable_get_text (GTK_EDITABLE (window->fullname_entry));
    auto mnemonic = gtk_editable_get_text (GTK_EDITABLE (window->mnemonic_entry));
    auto user_symbol = gtk_editable_get_text (GTK_EDITABLE (window->user_symbol_entry));
    auto code = gtk_editable_get_text (GTK_EDITABLE (window->code_entry));
    auto fraction = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (window->fraction_spinbutton));
    gnc_commodity *edit_commodity = nullptr;
    gnc_commodity *commodity;

    if (!book || !guid_equal (qof_instance_get_guid (QOF_INSTANCE (book)),
                              &window->book_guid))
    {
        g_free (name_space);
        return nullptr;
    }
    if (window->editing_existing)
    {
        edit_commodity = gnc_commodity_find_commodity_by_guid (&window->commodity_guid, book);
        if (!edit_commodity || qof_instance_get_destroying (QOF_INSTANCE (edit_commodity)))
        {
            gnc_warning_dialog (GTK_WINDOW (window->dialog), "%s",
                                _("The commodity was removed while it was being edited."));
            g_free (name_space);
            return nullptr;
        }
    }

    if (gnc_commodity_namespace_is_iso (name_space))
    {
        if (!edit_commodity)
        {
            gnc_warning_dialog (GTK_WINDOW (window->dialog), "%s",
                                _("You may not create a new national currency."));
            g_free (name_space);
            return nullptr;
        }

        auto quote_set = gtk_check_button_get_active (GTK_CHECK_BUTTON (window->get_quote_check));
        gnc_commodity_begin_edit (edit_commodity);
        gnc_commodity_user_set_quote_flag (edit_commodity, quote_set);
        if (quote_set)
        {
            auto selection = gtk_drop_down_get_selected (GTK_DROP_DOWN (window->quote_tz_menu));
            gnc_commodity_set_quote_tz (edit_commodity,
                                        gnc_timezone_menu_position_to_string (selection));
        }
        else
            gnc_commodity_set_quote_tz (edit_commodity, nullptr);
        gnc_commodity_set_user_symbol (edit_commodity, user_symbol);
        gnc_commodity_commit_edit (edit_commodity);
        g_free (name_space);
        return edit_commodity;
    }

    if (g_utf8_collate (name_space, GNC_COMMODITY_NS_TEMPLATE) == 0)
    {
        gnc_warning_dialog (GTK_WINDOW (window->dialog),
                            _("%s is a reserved commodity type. Please use something else."),
                            GNC_COMMODITY_NS_TEMPLATE);
        g_free (name_space);
        return nullptr;
    }

    if (!fullname || !*fullname || !name_space || !*name_space || !mnemonic || !*mnemonic)
    {
        gnc_warning_dialog (GTK_WINDOW (window->dialog), "%s",
                            _("You must enter a non-empty \"Full name\", "
                              "\"Symbol/abbreviation\", and \"Type\" for the commodity."));
        g_free (name_space);
        return nullptr;
    }

    commodity = gnc_commodity_table_lookup (gnc_get_current_commodities (),
                                             name_space, mnemonic);
    if ((!edit_commodity && commodity) ||
        (edit_commodity && commodity && commodity != edit_commodity))
    {
        gnc_warning_dialog (GTK_WINDOW (window->dialog), "%s",
                            _("That commodity already exists."));
        g_free (name_space);
        return nullptr;
    }

    if (!edit_commodity)
    {
        commodity = gnc_commodity_new (book, fullname, name_space, mnemonic, code, fraction);
        gnc_commodity_begin_edit (commodity);
        gnc_commodity_set_user_symbol (commodity, user_symbol);
    }
    else
    {
        commodity = edit_commodity;
        gnc_commodity_begin_edit (commodity);
        gnc_commodity_table_remove (gnc_get_current_commodities (), commodity);
        gnc_commodity_set_fullname (commodity, fullname);
        gnc_commodity_set_mnemonic (commodity, mnemonic);
        gnc_commodity_set_namespace (commodity, name_space);
        gnc_commodity_set_cusip (commodity, code);
        gnc_commodity_set_fraction (commodity, fraction);
        gnc_commodity_set_user_symbol (commodity, user_symbol);
    }

    gnc_commodity_user_set_quote_flag (commodity,
        gtk_check_button_get_active (GTK_CHECK_BUTTON (window->get_quote_check)));
    QuoteSourceType type;
    for (type = SOURCE_SINGLE; type < SOURCE_MAX;
         type = static_cast<QuoteSourceType> (type + 1))
    {
        if (gtk_check_button_get_active (GTK_CHECK_BUTTON (window->source_button[type])))
            break;
    }
    auto selection = gtk_drop_down_get_selected (GTK_DROP_DOWN (window->source_menu[type]));
    gnc_commodity_set_quote_source (commodity, gnc_quote_source_lookup_by_ti (type, selection));
    selection = gtk_drop_down_get_selected (GTK_DROP_DOWN (window->quote_tz_menu));
    gnc_commodity_set_quote_tz (commodity, gnc_timezone_menu_position_to_string (selection));
    gnc_commodity_commit_edit (commodity);
    gnc_commodity_table_insert (gnc_get_current_commodities (), commodity);
    g_free (name_space);
    return commodity;
}

/** @} */
/** @} */
