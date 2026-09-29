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
#include <memory>

#include "dialog-commodity.h"
#include "dialog-utils.h"
#include "gnc-engine.h"
#include "gnc-gtk-utils.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "gnc-session.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

enum
{
    SOURCE_COL_NAME = 0,
    SOURCE_COL_FQ_SUPPORTED,
    NUM_SOURCE_COLS
};

struct select_commodity_window
{
    GtkWidget * dialog;
    GtkWidget * namespace_combo;
    GtkWidget * commodity_combo;
    GtkWidget * select_user_prompt;
    GtkWidget * ok_button;

    gnc_commodity * selection;

    gchar *default_cusip;
    gchar *default_fullname;
    gchar *default_mnemonic;
    gchar *default_user_symbol;
    int          default_fraction;

    QofBook *book;
    gchar *selection_namespace;
    gchar *selection_mnemonic;
    GncGUID selection_guid;
    gboolean selection_guid_set;
    GncCommodityDialogCallback callback;
    gpointer callback_data;
    guint async_refs;
    gboolean completed;
    gboolean new_dialog_active;
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

    gboolean     is_currency;
    gnc_commodity *edit_commodity;
    QofBook *book;
    gchar *edit_namespace;
    gchar *edit_mnemonic;
    GncGUID edit_guid;
    gboolean edit_guid_set;
    GncGUID result_guid;
    gboolean result_guid_set;
    gchar *result_namespace;
    gchar *result_mnemonic;
    GncCommodityDialogCallback callback;
    gpointer callback_data;
    gboolean completed;
    gboolean response_active;
    gboolean async_mode;
};

typedef struct select_commodity_window SelectCommodityWindow;
typedef struct commodity_window CommodityWindow;

static gboolean gnc_ui_select_commodity_response_cb (GtkDialog *dialog,
                                                       gint response,
                                                       gpointer data);
static void gnc_ui_select_commodity_destroy_cb (GtkWidget *dialog,
                                                gpointer data);
static void gnc_ui_commodity_response_cb (GtkDialog *dialog, gint response,
                                           gpointer data);
static void gnc_ui_commodity_destroy_cb (GtkWidget *dialog, gpointer data);
static void gnc_ui_select_commodity_new_async_cb (QofBook *book,
                                                  gnc_commodity *commodity,
                                                  gpointer data);

static gboolean
commodity_book_is_live (QofBook *book)
{
    return book && !qof_book_shutting_down (book) && qof_book_is_open (book);
}

static gboolean
commodity_book_is_current (QofBook *book)
{
    return commodity_book_is_live (book) && gnc_current_session_exist () &&
           qof_session_get_book (gnc_get_current_session ()) == book;
}

static void
commodity_book_set_weak (QofBook **book)
{
    *book = gnc_get_current_book ();
    if (*book)
        g_object_add_weak_pointer (G_OBJECT (*book),
                                   reinterpret_cast<gpointer *>(book));
}

static void
commodity_book_clear_weak (QofBook **book)
{
    if (*book)
        g_object_remove_weak_pointer (G_OBJECT (*book),
                                      reinterpret_cast<gpointer *>(book));
}

static void
gnc_ui_select_commodity_free (SelectCommodityWindow *window)
{
    commodity_book_clear_weak (&window->book);
    g_free (window->default_cusip);
    g_free (window->default_fullname);
    g_free (window->default_mnemonic);
    g_free (window->default_user_symbol);
    g_free (window->selection_namespace);
    g_free (window->selection_mnemonic);
    g_free (window);
}

static void
gnc_ui_commodity_window_free (CommodityWindow *window)
{
    commodity_book_clear_weak (&window->book);
    g_free (window->edit_namespace);
    g_free (window->edit_mnemonic);
    g_free (window->result_namespace);
    g_free (window->result_mnemonic);
    g_free (window);
}

static void
gnc_ui_commodity_warning (CommodityWindow *window, const char *message)
{
    auto warning = gtk_message_dialog_new (
        GTK_WINDOW (window->dialog),
        static_cast<GtkDialogFlags>(GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT),
        GTK_MESSAGE_WARNING, GTK_BUTTONS_CLOSE, "%s", message);
    g_signal_connect_swapped (warning, "response",
                              G_CALLBACK (gtk_widget_destroy), warning);
    gtk_widget_show (warning);
}

static void
gnc_ui_select_commodity_ref (SelectCommodityWindow *window)
{
    ++window->async_refs;
}

static void
gnc_ui_select_commodity_unref (SelectCommodityWindow *window)
{
    if (--window->async_refs == 0)
        gnc_ui_select_commodity_free (window);
}

static gboolean
gnc_ui_select_commodity_is_valid (SelectCommodityWindow *window,
                                  gnc_commodity **result)
{
    gnc_commodity_table *table;
    *result = nullptr;
    if (!commodity_book_is_current (window->book) || !window->selection ||
        !window->selection_namespace || !window->selection_mnemonic)
        return FALSE;
    table = gnc_commodity_table_get_table (window->book);
    *result = gnc_commodity_table_lookup (table, window->selection_namespace,
                                          window->selection_mnemonic);
    return *result && window->selection_guid_set &&
        guid_equal (&window->selection_guid,
                    qof_instance_get_guid (*result));
}

static void
gnc_ui_select_commodity_complete (SelectCommodityWindow *window,
                                  gnc_commodity *result)
{
    auto callback = window->callback;
    auto data = window->callback_data;
    auto book = window->book;
    gnc_commodity *current = nullptr;
    if (!result || !gnc_ui_select_commodity_is_valid (window, &current) ||
        !commodity_book_is_current (book))
    {
        book = nullptr;
        result = nullptr;
    }
    else
        result = current;
    window->callback = nullptr;
    if (callback)
        callback (book, result, data);
}

static void
gnc_ui_select_commodity_destroy_cb (GtkWidget *dialog, gpointer data)
{
    auto window = static_cast<SelectCommodityWindow*>(data);
    window->dialog = nullptr;
    g_signal_handlers_disconnect_by_func (
        dialog, reinterpret_cast<gpointer> (gnc_ui_select_commodity_response_cb), window);
    if (!window->completed)
    {
        window->completed = TRUE;
        gnc_ui_select_commodity_complete (window, nullptr);
    }
    gnc_ui_select_commodity_unref (window); // dialog ownership
}

static gboolean
gnc_ui_select_commodity_response_cb (GtkDialog *dialog, gint response,
                                    gpointer data)
{
    auto window = static_cast<SelectCommodityWindow*>(data);
    if (response == GNC_RESPONSE_NEW)
    {
        if (window->completed || window->new_dialog_active)
            return TRUE;
        auto name_space = gnc_ui_namespace_picker_ns (window->namespace_combo);
        window->new_dialog_active = TRUE;
        gnc_ui_select_commodity_ref (window); // child continuation ownership
        gnc_ui_new_commodity_async_full (
            name_space, window->dialog, window->default_cusip,
            window->default_fullname, window->default_mnemonic,
            window->default_user_symbol, window->default_fraction,
            gnc_ui_select_commodity_new_async_cb, window);
        g_free (name_space);
        return TRUE;
    }

    if (window->completed)
        return TRUE;
    window->completed = TRUE;
    g_signal_handlers_disconnect_by_func (
        dialog, reinterpret_cast<gpointer> (gnc_ui_select_commodity_response_cb), window);
    gnc_ui_select_commodity_ref (window); // response continuation ownership
    gtk_widget_destroy (GTK_WIDGET (dialog));

    gnc_commodity *result = nullptr;
    if (response == GTK_RESPONSE_OK &&
        gnc_ui_select_commodity_is_valid (window, &result))
        gnc_ui_select_commodity_complete (window, result);
    else
        gnc_ui_select_commodity_complete (window, nullptr);
    gnc_ui_select_commodity_unref (window);
    return TRUE;
}

static void
gnc_ui_select_commodity_new_async_cb (QofBook *book, gnc_commodity *commodity,
                                     gpointer data)
{
    auto window = static_cast<SelectCommodityWindow*>(data);
    window->new_dialog_active = FALSE;
    if (!window->completed && window->dialog && commodity &&
        book == window->book && commodity_book_is_current (window->book))
    {
        auto guid = *qof_instance_get_guid (commodity);
        auto name_space = g_strdup (gnc_commodity_get_namespace (commodity));
        auto mnemonic = g_strdup (gnc_commodity_get_mnemonic (commodity));
        auto fullname = g_strdup (gnc_commodity_get_printname (commodity));
        auto dialog = GTK_WIDGET (g_object_ref (window->dialog));
        auto namespace_combo = GTK_WIDGET (g_object_ref (window->namespace_combo));
        auto commodity_combo = GTK_WIDGET (g_object_ref (window->commodity_combo));
        gnc_ui_update_namespace_picker (namespace_combo, name_space, DIAG_COMM_ALL);
        gnc_commodity *current = nullptr;
        if (commodity_book_is_current (window->book))
            current = gnc_commodity_table_lookup (
                gnc_commodity_table_get_table (window->book), name_space,
                mnemonic);
        if (!window->completed && window->dialog == dialog && current &&
            guid_equal (&guid, qof_instance_get_guid (current)) &&
            !gtk_widget_in_destruction (dialog))
            gnc_ui_update_commodity_picker (commodity_combo, name_space, fullname);
        g_object_unref (commodity_combo);
        g_object_unref (namespace_combo);
        g_object_unref (dialog);
        g_free (name_space);
        g_free (mnemonic);
        g_free (fullname);
    }
    gnc_ui_select_commodity_unref (window);
}

/* The commodity selection window */
static SelectCommodityWindow *
gnc_ui_select_commodity_create(const gnc_commodity * orig_sel,
                               dialog_commodity_mode mode);
void gnc_ui_select_commodity_new_cb(GtkButton * button,
                                    gpointer user_data);
extern "C" {
void gnc_ui_select_commodity_changed_cb(GtkComboBox *cbwe,
                                        gpointer user_data);
void gnc_ui_select_commodity_namespace_changed_cb(GtkComboBox *cbwe,
        gpointer user_data);

/* The commodity creation window */
void gnc_ui_commodity_changed_cb(GtkWidget * dummy, gpointer user_data);
void gnc_ui_commodity_quote_info_cb(GtkWidget *w, gpointer data);
}
gboolean gnc_ui_commodity_dialog_to_object(CommodityWindow * w);

void
gnc_ui_select_commodity_async_full (gnc_commodity *orig_sel,
                                   GtkWidget *parent,
                                   dialog_commodity_mode mode,
                                   const char *user_message,
                                   const char *cusip,
                                   const char *fullname,
                                   const char *mnemonic,
                                   GncCommodityDialogCallback callback,
                                   gpointer user_data)
{
    if (!callback)
        return;
    auto window = gnc_ui_select_commodity_create (orig_sel, mode);
    window->default_cusip = g_strdup (cusip);
    window->default_fullname = g_strdup (fullname);
    window->default_mnemonic = g_strdup (mnemonic);
    window->default_user_symbol = g_strdup ("");
    commodity_book_set_weak (&window->book);
    window->callback = callback;
    window->callback_data = user_data;
    window->async_refs = 1; // owned by the selector widget until destruction

    const char *initial;
    if (user_message)
        initial = user_message;
    else if (cusip || fullname || mnemonic)
        initial = _("\nPlease select a commodity to match");
    else
        initial = "";
    auto prompt = g_strdup_printf (
        "%s%s%s%s%s%s%s", initial,
        fullname ? _("\nCommodity: ") : "", fullname ? fullname : "",
        cusip ? _("\nExchange code (ISIN, CUSIP or similar): ") : "",
        cusip ? cusip : "",
        mnemonic ? _("\nMnemonic (Ticker symbol or similar): ") : "",
        mnemonic ? mnemonic : "");
    gtk_label_set_text (GTK_LABEL (window->select_user_prompt), prompt);
    g_free (prompt);

    if (parent && GTK_IS_WINDOW (parent))
    {
        gtk_window_set_transient_for (GTK_WINDOW (window->dialog),
                                      GTK_WINDOW (parent));
        gtk_window_set_destroy_with_parent (GTK_WINDOW (window->dialog), TRUE);
    }
    gtk_window_set_modal (GTK_WINDOW (window->dialog), TRUE);
    g_signal_connect (window->dialog, "response",
                      G_CALLBACK (gnc_ui_select_commodity_response_cb), window);
    g_signal_connect (window->dialog, "destroy",
                      G_CALLBACK (gnc_ui_select_commodity_destroy_cb), window);
    gtk_widget_show_all (window->dialog);
}


/********************************************************************
 * gnc_ui_select_commodity_create()
 ********************************************************************/
static SelectCommodityWindow *
gnc_ui_select_commodity_create(const gnc_commodity * orig_sel,
                               dialog_commodity_mode mode)
{
    SelectCommodityWindow * retval = g_new0(SelectCommodityWindow, 1);
    GtkBuilder *builder;
    const char *title, *text;
    gchar *name_space;
    GtkWidget *button, *label;

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-commodity.glade", "liststore1");
    gnc_builder_add_from_file (builder, "dialog-commodity.glade", "liststore2");
    gnc_builder_add_from_file (builder, "dialog-commodity.glade", "security_selector_dialog");

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, retval);

    retval->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "security_selector_dialog"));
    retval->namespace_combo = GTK_WIDGET(gtk_builder_get_object (builder, "ss_namespace_cbwe"));
    retval->commodity_combo = GTK_WIDGET(gtk_builder_get_object (builder, "ss_commodity_cbwe"));
    retval->select_user_prompt = GTK_WIDGET(gtk_builder_get_object (builder, "select_user_prompt"));
    retval->ok_button = GTK_WIDGET(gtk_builder_get_object (builder, "ss_ok_button"));
    label = GTK_WIDGET(gtk_builder_get_object (builder, "item_label"));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(retval->dialog), "gnc-id-security-select");
    gnc_widget_style_context_add_class (GTK_WIDGET(retval->dialog), "gnc-class-securities");

    gnc_cbwe_require_list_item(GTK_COMBO_BOX(retval->namespace_combo));
    gnc_cbwe_require_list_item(GTK_COMBO_BOX(retval->commodity_combo));

    gtk_label_set_text (GTK_LABEL (retval->select_user_prompt), "");

#ifdef DRH
    g_signal_connect (G_OBJECT (retval->dialog), "close",
                      G_CALLBACK (select_commodity_close), retval);
    g_signal_connect (G_OBJECT (retval->dialog), "response",
                      G_CALLBACK (gnc_ui_select_commodity_response_cb), retval);
#endif

    switch (mode)
    {
    case DIAG_COMM_ALL:
        title = _("Select security/currency");
        text = _("_Security/currency");
        break;
    case DIAG_COMM_NON_CURRENCY:
    case DIAG_COMM_NON_CURRENCY_SELECT:
        title = _("Select security");
        text = _("_Security");
        break;
    case DIAG_COMM_CURRENCY:
    default:
        title = _("Select currency");
        text = _("Cu_rrency");
        button = GTK_WIDGET(gtk_builder_get_object (builder, "ss_new_button"));
        gtk_widget_destroy(button);
        break;
    }
    gtk_window_set_title (GTK_WINDOW(retval->dialog), title);
    gtk_label_set_text_with_mnemonic (GTK_LABEL(label), text);

    /* build the menus of namespaces and commodities */
    gnc_ui_update_namespace_picker(retval->namespace_combo,
                                   gnc_commodity_get_namespace(orig_sel),
                                   mode);
    name_space = gnc_ui_namespace_picker_ns(retval->namespace_combo);
    gnc_ui_update_commodity_picker(retval->commodity_combo, name_space,
                                   gnc_commodity_get_printname(orig_sel));

    g_object_unref(G_OBJECT(builder));

    g_free(name_space);
    return retval;
}


/**
 *  This function is called whenever the user clicks on the "New"
 *  button in the commodity picker.  Its function is pop up a new
 *  dialog alling the user to create a new commodity.
 *
 *  @note This function is an internal helper function for the
 *  Commodity Selection dialog.  It should not be used outside of the
 *  dialog-commodity.c file.
 *
 *  @param button A pointer to the "new" button widget in the dialog.
 *
 *  @param user_data A pointer to the data structure describing the
 *  current state of the commodity picker.
 */
void
gnc_ui_select_commodity_new_cb(GtkButton * button,
                               gpointer user_data)
{
    auto w = static_cast<SelectCommodityWindow*>(user_data);
    if (w && !w->completed && w->dialog)
        gtk_dialog_response (GTK_DIALOG (w->dialog), GNC_RESPONSE_NEW);
}


/**
 *  This function is called whenever the commodity combo box is
 *  changed.  Its function is to determine if a valid commodity has
 *  been selected, record the selection, and update the OK button.
 *
 *  @note This function is an internal helper function for the
 *  Commodity Selection dialog.  It should not be used outside of the
 *  dialog-commodity.c file.
 *
 *  @param cbwe A pointer to the commodity name entry widget in the
 *  dialog.
 *
 *  @param user_data A pointer to the data structure describing the
 *  current state of the commodity picker.
 */
void
gnc_ui_select_commodity_changed_cb (GtkComboBox *cbwe,
                                    gpointer user_data)
{
    auto w = static_cast<SelectCommodityWindow*>(user_data);
    gchar *name_space;
    const gchar *fullname;
    gboolean ok;

    ENTER("cbwe=%p, user_data=%p", cbwe, user_data);
    name_space = gnc_ui_namespace_picker_ns (w->namespace_combo);
    fullname = gtk_entry_get_text(GTK_ENTRY (gtk_bin_get_child(GTK_BIN (GTK_COMBO_BOX(w->commodity_combo)))));

    DEBUG("namespace=%s, name=%s", name_space, fullname);
    w->selection = gnc_commodity_table_find_full(gnc_get_current_commodities(),
                   name_space, fullname);
    g_free (w->selection_namespace);
    g_free (w->selection_mnemonic);
    w->selection_namespace = w->selection ? g_strdup (name_space) : nullptr;
    w->selection_mnemonic = w->selection ?
        g_strdup (gnc_commodity_get_mnemonic (w->selection)) : nullptr;
    w->selection_guid_set = w->selection != nullptr;
    if (w->selection)
        w->selection_guid = *qof_instance_get_guid (w->selection);
    g_free(name_space);

    ok = (w->selection != nullptr);
    gtk_widget_set_sensitive(w->ok_button, ok);
    gtk_dialog_set_default_response(GTK_DIALOG(w->dialog), ok ? 0 : 2);
    LEAVE("sensitive=%d, default = %d", ok, ok ? 0 : 2);
}


/**
 *  This function is called whenever the commodity namespace combo box
 *  is changed.  Its function is to update the commodity name combo
 *  box with the strings that are appropriate to the selected
 *  namespace.
 *
 *  @note This function is an internal helper function for the
 *  Commodity Selection dialog.  It should not be used outside of the
 *  dialog-commodity.c file.
 *
 *  @param cbwe A pointer to the commodity namespace entry widget in
 *  the dialog.
 *
 *  @param user_data A pointer to the data structure describing the
 *  current state of the commodity picker.
 */
void
gnc_ui_select_commodity_namespace_changed_cb (GtkComboBox *cbwe,
        gpointer user_data)
{
    auto w = static_cast<SelectCommodityWindow*>(user_data);
    gchar *name_space;

    ENTER("cbwe=%p, user_data=%p", cbwe, user_data);
    name_space = gnc_ui_namespace_picker_ns (w->namespace_combo);
    DEBUG("name_space=%s", name_space);
    gnc_ui_update_commodity_picker(w->commodity_combo, name_space, nullptr);
    g_free(name_space);
    LEAVE(" ");
}


/********************************************************************
 * gnc_ui_update_commodity_picker
 ********************************************************************/
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
    GtkComboBox *combo_box;
    GtkEntry *entry;
    GtkTreeModel *model;
    GtkTreeIter iter;
    gnc_commodity_table *table;
    gint current = 0, match = 0;
    gchar *name;

    g_return_if_fail(GTK_IS_COMBO_BOX(cbwe));
    g_return_if_fail(name_space);

    /* Erase the old entries */
    combo_box = GTK_COMBO_BOX(cbwe);
    model = gtk_combo_box_get_model(combo_box);
    gtk_list_store_clear(GTK_LIST_STORE(model));

    /* Erase the entry text */
    entry = GTK_ENTRY(gtk_bin_get_child(GTK_BIN(combo_box)));
    gtk_editable_delete_text(GTK_EDITABLE(entry), 0, -1);

    gtk_combo_box_set_active(combo_box, -1);

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
        gtk_list_store_append(GTK_LIST_STORE(model), &iter);
        gtk_list_store_set (GTK_LIST_STORE(model), &iter, 0, name, -1);

        if (init_string && g_utf8_collate(name, init_string) == 0)
            match = current;
        current++;
    }

    gtk_combo_box_set_active(combo_box, match);
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
    guint offset = 0;

    gtk_container_child_get(GTK_CONTAINER(cw->table), widget,
                            "top-attach", &offset,
                            nullptr);

    if ((offset < cw->comm_section_top) || (offset >= cw->comm_section_bottom))
        return;
    if (cw->is_currency)
        gtk_widget_set_sensitive(widget, offset == cw->comm_symbol_line);
}

static void
gnc_ui_update_commodity_info (CommodityWindow *cw)
{
    gtk_container_foreach(GTK_CONTAINER(cw->table),
                          gnc_set_commodity_section_sensitivity, cw);
}


static void
gnc_set_fq_sensitivity (GtkWidget *widget, gpointer user_data)
{
    auto cw = static_cast<CommodityWindow*>(user_data);
    guint offset = 0;

    gtk_container_child_get(GTK_CONTAINER(cw->table), widget,
                            "top-attach", &offset,
                            nullptr);

    if ((offset < cw->fq_section_top) || (offset >= cw->fq_section_bottom))
        return;
    g_object_set(widget, "sensitive", FALSE, nullptr);
}


static void
gnc_ui_update_fq_info (CommodityWindow *cw)
{
    gtk_container_foreach(GTK_CONTAINER(cw->table),
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
    GtkComboBox *combo_box;
    GtkTreeModel *model;
    GtkTreeIter iter, match;
    GList *namespaces, *node;
    gboolean matched = FALSE;

    g_return_if_fail(GTK_IS_COMBO_BOX (cbwe));

    combo_box = GTK_COMBO_BOX(cbwe);
    model = gtk_combo_box_get_model(combo_box);
    g_return_if_fail (GTK_IS_LIST_STORE (model));

    /* Model notifications may close the dialog or switch books. Keep the
       objects alive, but do not continue updating a destroyed or replaced view. */
    struct UpdateScope
    {
        GtkComboBox *combo;
        GtkTreeModel *model;
        QofSession *session;
        QofBook *book{};
        gboolean destroyed{};
        gulong destroy_handler;

        UpdateScope (GtkComboBox *combo, GtkTreeModel *model)
            : combo (combo), model (model), session (gnc_get_current_session ())
        {
            g_object_ref (combo);
            g_object_ref (model);
            commodity_book_set_weak (&book);
            destroy_handler = g_signal_connect (
                combo, "destroy", G_CALLBACK (+[] ([[maybe_unused]] GtkWidget *widget,
                                                   gpointer data)
                {
                    static_cast<UpdateScope*> (data)->destroyed = TRUE;
                }), this);
        }

        ~UpdateScope ()
        {
            if (g_signal_handler_is_connected (combo, destroy_handler))
                g_signal_handler_disconnect (combo, destroy_handler);
            commodity_book_clear_weak (&book);
            g_object_unref (model);
            g_object_unref (combo);
        }

        bool valid () const
        {
            return !destroyed && commodity_book_is_current (book) &&
                gnc_get_current_session () == session &&
                gtk_combo_box_get_model (combo) == model;
        }
    } update (combo_box, model);
    if (!update.valid ())
        return;

    std::unique_ptr<gchar, decltype (&g_free)> initial_copy (
        g_strdup (init_string), g_free);
    init_string = initial_copy.get ();

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

    /* Namespace names are borrowed from the book. Take a snapshot before the
       first notification so book destruction cannot invalidate those strings. */
    auto owned_namespaces = g_list_copy_deep (
        namespaces, +[] (gconstpointer name, [[maybe_unused]] gpointer data) -> gpointer
        { return g_strdup (static_cast<const gchar*> (name)); }, nullptr);
    g_list_free (namespaces);
    namespaces = g_list_sort (owned_namespaces, collate);
    auto free_namespaces = +[] (GList *list) { g_list_free_full (list, g_free); };
    std::unique_ptr<GList, decltype (free_namespaces)> namespace_snapshot (
        namespaces, free_namespaces);

    gtk_list_store_clear (GTK_LIST_STORE (model));
    if (!update.valid ())
        return;

    /* First insert "Currencies" entry if requested */
    if (mode == DIAG_COMM_CURRENCY || mode == DIAG_COMM_ALL)
    {
        gtk_list_store_append(GTK_LIST_STORE(model), &iter);
        if (!update.valid ())
            return;
        gtk_list_store_set (GTK_LIST_STORE(model), &iter, 0,
                            _(GNC_COMMODITY_NS_ISO_GUI), -1);
        if (!update.valid ())
            return;

        if (init_string &&
            (g_utf8_collate(GNC_COMMODITY_NS_ISO_GUI, init_string) == 0))
        {
            matched = TRUE;
            match = iter;
        }
    }

    /* Next insert "All non-currency" entry if requested */
    if (mode == DIAG_COMM_NON_CURRENCY_SELECT || mode == DIAG_COMM_ALL)
    {
        gtk_list_store_append(GTK_LIST_STORE(model), &iter);
        if (!update.valid ())
            return;
        gtk_list_store_set (GTK_LIST_STORE(model), &iter, 0,
                            GNC_COMMODITY_NS_NONISO_GUI, -1);
        if (!update.valid ())
            return;
    }

    /* add all others to the combobox */
    for (node = namespaces; node; node = node->next)
    {
        auto ns = static_cast<const char*>(node->data);
        /* Skip template, legacy and currency namespaces.
           The latter was added as first entry earlier */
        if ((g_utf8_collate(ns, GNC_COMMODITY_NS_LEGACY) == 0) ||
            (g_utf8_collate(ns, GNC_COMMODITY_NS_TEMPLATE ) == 0) ||
            (g_utf8_collate(ns, GNC_COMMODITY_NS_CURRENCY ) == 0))
            continue;

        gtk_list_store_append(GTK_LIST_STORE(model), &iter);
        if (!update.valid ())
            return;
        gtk_list_store_set (GTK_LIST_STORE(model), &iter, 0, ns, -1);
        if (!update.valid ())
            return;

        if (init_string &&
            (g_utf8_collate(ns, init_string) == 0))
        {
            matched = TRUE;
            match = iter;
        }
    }

    if (!matched)
        matched = gtk_tree_model_get_iter_first (model, &match);

    if (matched)
        gtk_combo_box_set_active_iter (combo_box, &match);
}


gchar *
gnc_ui_namespace_picker_ns (GtkWidget *cbwe)
{
    const gchar *name_space;

    g_return_val_if_fail(GTK_IS_COMBO_BOX (cbwe), nullptr);

    name_space = gtk_entry_get_text( GTK_ENTRY( gtk_bin_get_child( GTK_BIN( GTK_COMBO_BOX(cbwe)))));

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
    get_quote = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (w));

    text = gtk_entry_get_text( GTK_ENTRY( gtk_bin_get_child( GTK_BIN( GTK_COMBO_BOX(cw->namespace_combo)))));

    allow_src = !gnc_commodity_namespace_is_iso(text);

    gtk_widget_set_sensitive(cw->source_label, get_quote && allow_src);

    for (i = SOURCE_SINGLE; i < SOURCE_MAX; i++)
    {
        if (!cw->source_button[i])
            continue;
        active =
            gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(cw->source_button[i]));
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
        fullname  = gtk_entry_get_text(GTK_ENTRY(w->fullname_entry));
        mnemonic  = gtk_entry_get_text(GTK_ENTRY(w->mnemonic_entry));
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
    gtk_dialog_set_default_response(GTK_DIALOG(w->dialog), ok ? 0 : 1);
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
    GtkListStore *store;
    GtkTreeIter iter;
    GtkWidget *combo;
    GtkCellRenderer *renderer;
    gnc_quote_source *source;

    store = gtk_list_store_new(NUM_SOURCE_COLS, G_TYPE_STRING, G_TYPE_BOOLEAN);
    if (type == SOURCE_CURRENCY)
    {
        gtk_list_store_append(store, &iter);
        gtk_list_store_set(store, &iter,
                           SOURCE_COL_NAME, _("Currency"),
                           SOURCE_COL_FQ_SUPPORTED, TRUE,
                           -1);
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
            gtk_list_store_append(store, &iter);
            gtk_list_store_set(store, &iter,
                               SOURCE_COL_NAME, g_dpgettext2(NULL, "FQ Source", name),
                               SOURCE_COL_FQ_SUPPORTED, supported,
                               -1);
        }
    }

    combo = gtk_combo_box_new_with_model(GTK_TREE_MODEL(store));
    g_object_unref(store);
    renderer = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combo), renderer, TRUE);
    gtk_cell_layout_add_attribute(GTK_CELL_LAYOUT(combo), renderer,
                                  "text", SOURCE_COL_NAME);
    gtk_cell_layout_add_attribute(GTK_CELL_LAYOUT(combo), renderer,
                                  "sensitive", SOURCE_COL_FQ_SUPPORTED);
    gtk_combo_box_set_active(GTK_COMBO_BOX(combo), 0);
    gtk_widget_show(combo);
    return combo;
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
    GtkWidget  *combo;
    const gchar     **itemstr;

    /* add items here as needed, but bear in mind that right now these
       must be timezones that GNU libc understands.  Also, I'd prefer if
       we only add things here we *know* we need.  That's because in
       order to be portable to non GNU OSes, we may have to support
       whatever we add here manually on those systems. */

    combo = gtk_combo_box_text_new();
    gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(combo), _("Use local time"));
    for (itemstr = &known_timezones[0]; *itemstr; itemstr++)
    {
        gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(combo), *itemstr);
    }

    gtk_widget_show(combo);
    return combo;
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
    commodity_book_set_weak (&retval->book);
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
    gnc_builder_add_from_file (builder, "dialog-commodity.glade", "liststore2");
    gnc_builder_add_from_file (builder, "dialog-commodity.glade", "adjustment1");
    gnc_builder_add_from_file (builder, "dialog-commodity.glade", "security_dialog");

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, retval);

    retval->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "security_dialog"));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(retval->dialog), "gnc-id-security");
    gnc_widget_style_context_add_class (GTK_WIDGET(retval->dialog), "gnc-class-securities");

    if (parent != nullptr)
        gtk_window_set_transient_for (GTK_WINDOW (retval->dialog), GTK_WINDOW (parent));

    retval->edit_commodity = nullptr;

    /* Get widget pointers */
    retval->fullname_entry = GTK_WIDGET(gtk_builder_get_object (builder, "fullname_entry"));
    retval->mnemonic_entry = GTK_WIDGET(gtk_builder_get_object (builder, "mnemonic_entry"));
    retval->user_symbol_entry = GTK_WIDGET(gtk_builder_get_object (builder, "user_symbol_entry"));
    retval->namespace_combo = GTK_WIDGET(gtk_builder_get_object (builder, "namespace_cbwe"));
    retval->code_entry = GTK_WIDGET(gtk_builder_get_object (builder, "code_entry"));
    retval->fraction_spinbutton = GTK_WIDGET(gtk_builder_get_object (builder, "fraction_spinbutton"));
    retval->ok_button = GTK_WIDGET(gtk_builder_get_object (builder, "ok_button"));
    retval->get_quote_check = GTK_WIDGET(gtk_builder_get_object (builder, "get_quote_check"));
    retval->source_label = GTK_WIDGET(gtk_builder_get_object (builder, "source_label"));
    retval->source_button[SOURCE_SINGLE] = GTK_WIDGET(gtk_builder_get_object (builder, "single_source_button"));
    retval->source_button[SOURCE_MULTI] = GTK_WIDGET(gtk_builder_get_object (builder, "multi_source_button"));
    retval->quote_tz_label = GTK_WIDGET(gtk_builder_get_object (builder, "quote_tz_label"));

    /* Determine the commodity section of the dialog */
    retval->table = GTK_WIDGET(gtk_builder_get_object (builder, "edit_table"));
    sec_label = GTK_WIDGET(gtk_builder_get_object (builder, "security_label"));
    gtk_container_child_get(GTK_CONTAINER(retval->table), sec_label,
                            "top-attach", &retval->comm_section_top, nullptr);

    widget = GTK_WIDGET(gtk_builder_get_object (builder, "quote_label"));
    gtk_container_child_get(GTK_CONTAINER(retval->table), widget,
                            "top-attach", &retval->comm_section_bottom, nullptr);

    gtk_container_child_get(GTK_CONTAINER(retval->table),
                            retval->user_symbol_entry, "top-attach",
                            &retval->comm_symbol_line, nullptr);

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
    gtk_box_pack_start(GTK_BOX(box), menu, TRUE, TRUE, 0);

    box = GTK_WIDGET(gtk_builder_get_object (builder, "multi_source_box"));
    menu = gnc_ui_source_menu_create(SOURCE_MULTI);
    retval->source_menu[SOURCE_MULTI] = menu;
    gtk_box_pack_start(GTK_BOX(box), menu, TRUE, TRUE, 0);

    if (gnc_quote_source_num_entries(SOURCE_UNKNOWN))
    {
        retval->source_button[SOURCE_UNKNOWN] =
            GTK_WIDGET(gtk_builder_get_object (builder, "unknown_source_button"));
        box = GTK_WIDGET(gtk_builder_get_object (builder, "unknown_source_box"));
        menu = gnc_ui_source_menu_create(SOURCE_UNKNOWN);
        retval->source_menu[SOURCE_UNKNOWN] = menu;
        gtk_box_pack_start(GTK_BOX(box), menu, TRUE, TRUE, 0);
    }
    else
    {
        gtk_grid_set_row_spacing(GTK_GRID(retval->table), 0);

        widget = GTK_WIDGET(gtk_builder_get_object (builder, "unknown_source_button"));
        gtk_widget_destroy(widget);

        widget = GTK_WIDGET(gtk_builder_get_object (builder, "unknown_source_box"));
        gtk_widget_destroy(widget);
    }

    box = GTK_WIDGET(gtk_builder_get_object (builder, "quote_tz_box"));
    retval->quote_tz_menu = gnc_ui_quote_tz_menu_create();
    gtk_box_pack_start(GTK_BOX(box), retval->quote_tz_menu, TRUE, TRUE, 0);

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
        gtk_widget_destroy(GTK_WIDGET(gtk_builder_get_object (builder, "finance_quote_warning")));
    }
    else
    {
        /* Determine the price quote of the dialog */
        widget = GTK_WIDGET(gtk_builder_get_object (builder, "fq_warning_alignment"));
        gtk_container_child_get(GTK_CONTAINER(retval->table), widget,
                                "top-attach", &retval->fq_section_top, nullptr);

        widget = GTK_WIDGET(gtk_builder_get_object (builder, "bottom_alignment"));
        gtk_container_child_get(GTK_CONTAINER(retval->table), widget,
                                "top-attach", &retval->fq_section_bottom, nullptr);

        gnc_ui_update_fq_info (retval);
    }

#ifdef DRH
    g_signal_connect (G_OBJECT (retval->dialog), "close",
                      G_CALLBACK (commodity_close), retval);
#endif
    /* Fill in any data, top to bottom */
    gtk_entry_set_text (GTK_ENTRY (retval->fullname_entry), fullname ? fullname : "");
    gtk_entry_set_text (GTK_ENTRY (retval->mnemonic_entry), mnemonic ? mnemonic : "");
    gtk_entry_set_text (GTK_ENTRY (retval->user_symbol_entry), user_symbol ? user_symbol : "");
    gnc_cbwe_add_completion(GTK_COMBO_BOX(retval->namespace_combo));
    gnc_ui_update_namespace_picker(retval->namespace_combo,
                                   selected_namespace,
                                   include_iso ? DIAG_COMM_ALL : DIAG_COMM_NON_CURRENCY);
    gtk_entry_set_text (GTK_ENTRY (retval->code_entry), cusip ? cusip : "");

    if (fraction > 0)
        gtk_spin_button_set_value (GTK_SPIN_BUTTON (retval->fraction_spinbutton),
                                   fraction);

    g_object_unref(G_OBJECT(builder));

    LEAVE(" ");
    return retval;
}

static gnc_commodity *
gnc_ui_commodity_window_result (CommodityWindow *window)
{
    if (!commodity_book_is_current (window->book) ||
        !window->result_namespace || !window->result_mnemonic)
        return nullptr;
    auto table = gnc_commodity_table_get_table (window->book);
    auto result = gnc_commodity_table_lookup (table, window->result_namespace,
                                               window->result_mnemonic);
    return result && window->result_guid_set &&
        guid_equal (&window->result_guid, qof_instance_get_guid (result))
        ? result : nullptr;
}

static void
gnc_ui_commodity_complete (CommodityWindow *window, gnc_commodity *result)
{
    auto callback = window->callback;
    auto data = window->callback_data;
    auto book = window->book;
    result = result ? gnc_ui_commodity_window_result (window) : nullptr;
    if (!result)
        book = nullptr;
    window->callback = nullptr;
    if (callback)
        callback (book, result, data);
}

static void
gnc_ui_commodity_destroy_cb (GtkWidget *dialog, gpointer data)
{
    auto window = static_cast<CommodityWindow*>(data);
    g_signal_handlers_disconnect_by_func (
        dialog, reinterpret_cast<gpointer> (gnc_ui_commodity_response_cb), window);
    if (!window->completed)
    {
        window->completed = TRUE;
        gnc_ui_commodity_complete (window, nullptr);
    }
    if (!window->response_active)
        gnc_ui_commodity_window_free (window);
}

static void
gnc_ui_commodity_response_cb (GtkDialog *dialog, gint response, gpointer data)
{
    auto window = static_cast<CommodityWindow*>(data);
    g_object_ref (dialog);
    window->response_active = TRUE;
    if (window->completed)
    {
        window->response_active = FALSE;
        gnc_ui_commodity_window_free (window);
        g_object_unref (dialog);
        return;
    }

    if (response == GTK_RESPONSE_HELP)
    {
        gnc_gnome_help (GTK_WINDOW (dialog), DF_MANUAL, DL_COMMODITY);
        if (!window->completed)
            window->response_active = FALSE;
        else
            gnc_ui_commodity_window_free (window);
        g_object_unref (dialog);
        return;
    }

    gnc_commodity *result = nullptr;
    if (response == GTK_RESPONSE_OK)
    {
        if (!commodity_book_is_current (window->book))
            result = nullptr;
        else if (window->edit_commodity &&
                 (!window->edit_guid_set ||
                  !gnc_commodity_table_lookup (
                      gnc_commodity_table_get_table (window->book),
                      window->edit_namespace, window->edit_mnemonic) ||
                  !guid_equal (&window->edit_guid,
                      qof_instance_get_guid (gnc_commodity_table_lookup (
                          gnc_commodity_table_get_table (window->book),
                          window->edit_namespace, window->edit_mnemonic)))))
            result = nullptr;
        else if (!gnc_ui_commodity_dialog_to_object (window))
        {
            if (!window->completed)
                window->response_active = FALSE;
            else
                gnc_ui_commodity_window_free (window);
            g_object_unref (dialog);
            return; // Validation warning; keep the editor open.
        }
        else
            result = window->edit_commodity;
    }

    window->completed = TRUE;
    g_signal_handlers_disconnect_by_func (
        dialog, reinterpret_cast<gpointer> (gnc_ui_commodity_response_cb), window);
    gtk_widget_destroy (GTK_WIDGET (dialog));
    gnc_ui_commodity_complete (window, result);
    window->response_active = FALSE;
    gnc_ui_commodity_window_free (window);
    g_object_unref (dialog);
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
    has_quote_src = gnc_commodity_get_quote_flag (commodity);
    source = gnc_commodity_get_quote_source (commodity);
    if (source == nullptr)
        source = gnc_commodity_get_default_quote_source (commodity);
    quote_tz = gnc_commodity_get_quote_tz (commodity);

    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (win->get_quote_check),
                                  has_quote_src);
    if (!gnc_commodity_is_iso(commodity))
    {
        type = gnc_quote_source_get_type(source);
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(win->source_button[type]), TRUE);
        gtk_combo_box_set_active(GTK_COMBO_BOX(win->source_menu[type]),
                                 gnc_quote_source_get_index(source));
    }

    if (quote_tz)
    {
        pos = gnc_find_timezone_menu_position(quote_tz);
//    if(pos == 0) {
//      PWARN("Unknown price quote timezone (%s), resetting to default.",
//	    quote_tz ? quote_tz : "(null)");
//    }
    }
    gtk_combo_box_set_active(GTK_COMBO_BOX(win->quote_tz_menu), pos);
    LEAVE(" ");
}


static void
gnc_ui_commodity_show_async (CommodityWindow *window, GtkWidget *parent,
                             GncCommodityDialogCallback callback,
                             gpointer user_data)
{
    if (!window || !callback)
    {
        if (window)
        {
            gtk_widget_destroy (window->dialog);
            gnc_ui_commodity_window_free (window);
        }
        return;
    }
    window->callback = callback;
    window->callback_data = user_data;
    window->async_mode = TRUE;
    if (parent && GTK_IS_WINDOW (parent))
    {
        gtk_window_set_transient_for (GTK_WINDOW (window->dialog),
                                      GTK_WINDOW (parent));
        gtk_window_set_destroy_with_parent (GTK_WINDOW (window->dialog), TRUE);
    }
    gtk_window_set_modal (GTK_WINDOW (window->dialog), TRUE);
    g_signal_connect (window->dialog, "response",
                      G_CALLBACK (gnc_ui_commodity_response_cb), window);
    g_signal_connect (window->dialog, "destroy",
                      G_CALLBACK (gnc_ui_commodity_destroy_cb), window);
    gtk_widget_show_all (window->dialog);
}

void
gnc_ui_new_commodity_async_full (const char *name_space, GtkWidget *parent,
                                 const char *cusip, const char *fullname,
                                 const char *mnemonic, const char *user_symbol,
                                 int fraction,
                                 GncCommodityDialogCallback callback,
                                 gpointer user_data)
{
    if (gnc_commodity_namespace_is_iso (name_space))
        name_space = nullptr;
    auto window = gnc_ui_build_commodity_dialog (
        name_space, parent, fullname, mnemonic, user_symbol, cusip, fraction,
        FALSE);
    gnc_ui_commodity_update_quote_info (window, nullptr);
    window->edit_commodity = nullptr;
    gnc_ui_commodity_quote_info_cb (window->get_quote_check, window);
    gnc_ui_commodity_show_async (window, parent, callback, user_data);
}

void
gnc_ui_edit_commodity_async (gnc_commodity *commodity, GtkWidget *parent,
                             GncCommodityDialogCallback callback,
                             gpointer user_data)
{
    if (!commodity || !callback)
    {
        if (callback)
            callback (nullptr, nullptr, user_data);
        return;
    }
    auto book = gnc_get_current_book ();
    auto name_space = g_strdup (gnc_commodity_get_namespace (commodity));
    auto mnemonic = g_strdup (gnc_commodity_get_mnemonic (commodity));
    if (!commodity_book_is_current (book) ||
        gnc_commodity_table_lookup (gnc_commodity_table_get_table (book),
                                    name_space, mnemonic) != commodity)
    {
        g_free (name_space);
        g_free (mnemonic);
        callback (nullptr, nullptr, user_data);
        return;
    }

    auto window = gnc_ui_build_commodity_dialog (
        name_space, parent, gnc_commodity_get_fullname (commodity), mnemonic,
        gnc_commodity_get_nice_symbol (commodity),
        gnc_commodity_get_cusip (commodity), gnc_commodity_get_fraction (commodity),
        TRUE);
    window->edit_commodity = commodity;
    window->edit_namespace = name_space;
    window->edit_mnemonic = mnemonic;
    window->edit_guid = *qof_instance_get_guid (commodity);
    window->edit_guid_set = TRUE;
    window->result_guid = window->edit_guid;
    window->result_guid_set = TRUE;
    window->result_namespace = g_strdup (name_space);
    window->result_mnemonic = g_strdup (mnemonic);
    gnc_ui_commodity_update_quote_info (window, commodity);
    gnc_ui_commodity_quote_info_cb (window->get_quote_check, window);
    gnc_ui_commodity_show_async (window, parent, callback, user_data);
}


/********************************************************************
 * gnc_ui_commodity_dialog_to_object()
 ********************************************************************/
gboolean
gnc_ui_commodity_dialog_to_object(CommodityWindow * w)
{
    gnc_quote_source *source;
    std::unique_ptr<gchar, decltype(&g_free)> fullname_copy (
        g_strdup (gtk_entry_get_text (GTK_ENTRY (w->fullname_entry))), g_free);
    const char *fullname = fullname_copy.get ();
    gchar *name_space = gnc_ui_namespace_picker_ns (w->namespace_combo);
    std::unique_ptr<gchar, decltype(&g_free)> mnemonic_copy (
        g_strdup (gtk_entry_get_text (GTK_ENTRY (w->mnemonic_entry))), g_free);
    std::unique_ptr<gchar, decltype(&g_free)> user_symbol_copy (
        g_strdup (gtk_entry_get_text (GTK_ENTRY (w->user_symbol_entry))), g_free);
    std::unique_ptr<gchar, decltype(&g_free)> code_copy (
        g_strdup (gtk_entry_get_text (GTK_ENTRY (w->code_entry))), g_free);
    const char *mnemonic = mnemonic_copy.get ();
    const char *user_symbol = user_symbol_copy.get ();
    const char *code = code_copy.get ();
    QofBook * book = w->book;
    gnc_commodity_table *table;
    int fraction = gtk_spin_button_get_value_as_int
                   (GTK_SPIN_BUTTON(w->fraction_spinbutton));
    const char *string = gnc_timezone_menu_position_to_string (
        gtk_combo_box_get_active (GTK_COMBO_BOX (w->quote_tz_menu)));
    gboolean quote_set = gtk_toggle_button_get_active (
        GTK_TOGGLE_BUTTON (w->get_quote_check));
    gnc_commodity * c;
    gnc_commodity *registered_edit = nullptr;
    gint selection;
    QuoteSourceType selected_type;
    for (selected_type = SOURCE_SINGLE; selected_type < SOURCE_MAX;
         selected_type = static_cast<QuoteSourceType>(selected_type + 1))
        if (gtk_toggle_button_get_active (
                GTK_TOGGLE_BUTTON (w->source_button[selected_type])))
            break;
    selection = selected_type < SOURCE_MAX ? gtk_combo_box_get_active (
        GTK_COMBO_BOX (w->source_menu[selected_type])) : -1;
    source = selected_type < SOURCE_MAX
        ? gnc_quote_source_lookup_by_ti (selected_type, selection) : nullptr;

    ENTER(" ");
    if (!commodity_book_is_live (book) || qof_book_is_readonly (book))
    {
        g_free (name_space);
        return FALSE;
    }
    table = gnc_commodity_table_get_table (book);
    if (w->edit_commodity)
    {
        registered_edit = gnc_commodity_table_lookup (
            table, w->edit_namespace, w->edit_mnemonic);
        if (!registered_edit || !w->edit_guid_set ||
            !guid_equal (&w->edit_guid,
                         qof_instance_get_guid (registered_edit)))
        {
            g_free (name_space);
            return FALSE;
        }
        w->edit_commodity = registered_edit;
    }
    /* Special case currencies */
    if (gnc_commodity_namespace_is_iso (name_space))
    {
        if (w->edit_commodity)
        {
            c = w->edit_commodity;
            gnc_commodity_begin_edit(c);
            if (!commodity_book_is_live (w->book))
            {
                g_free (name_space);
                return FALSE;
            }
            gnc_commodity_user_set_quote_flag (c, quote_set);
            if (!commodity_book_is_live (w->book))
            {
                g_free (name_space);
                return FALSE;
            }
            if (quote_set)
            {
                gnc_commodity_set_quote_tz(c, string);
            }
            else
                gnc_commodity_set_quote_tz(c, nullptr);
            if (!commodity_book_is_live (w->book))
            {
                g_free (name_space);
                return FALSE;
            }

            gnc_commodity_set_user_symbol(c, user_symbol);
            if (!commodity_book_is_live (w->book))
            {
                g_free (name_space);
                return FALSE;
            }

            g_free (w->result_namespace);
            g_free (w->result_mnemonic);
            w->result_namespace = g_strdup (w->edit_namespace);
            w->result_mnemonic = g_strdup (w->edit_mnemonic);
            gnc_commodity_commit_edit(c);
            g_free (name_space);
            return TRUE;
        }
        gnc_ui_commodity_warning (w, _("You may not create a new national currency."));
        g_free (name_space);
        return FALSE;
    }

    /* Don't allow user to create commodities in namespace
     * "template". That's reserved for scheduled transaction use.
     */
    if (name_space && g_utf8_collate(name_space, GNC_COMMODITY_NS_TEMPLATE) == 0)
    {
        auto message = g_strdup_printf (
            _("%s is a reserved commodity type. Please use something else."),
            GNC_COMMODITY_NS_TEMPLATE);
        gnc_ui_commodity_warning (w, message);
        g_free (message);
        g_free (name_space);
        return FALSE;
    }

    if (fullname && fullname[0] &&
            name_space && name_space[0] &&
            mnemonic && mnemonic[0])
    {
        c = gnc_commodity_table_lookup (table, name_space, mnemonic);

        if ((!w->edit_commodity && c) ||
                (w->edit_commodity && c && (c != w->edit_commodity)))
        {
            gnc_ui_commodity_warning (w, _("That commodity already exists."));
            g_free(name_space);
            return FALSE;
        }

        if (!w->edit_commodity)
        {
            c = gnc_commodity_new(book, fullname, name_space, mnemonic, code, fraction);
            if (!c)
            {
                g_free (name_space);
                return FALSE;
            }
            w->edit_commodity = c;
            w->result_guid = *qof_instance_get_guid (c);
            w->result_guid_set = TRUE;
            gnc_commodity_begin_edit(c);
            if (!commodity_book_is_live (w->book))
            {
                g_free (name_space);
                return FALSE;
            }

            gnc_commodity_set_user_symbol(c, user_symbol);
            if (!commodity_book_is_live (w->book))
            {
                g_free (name_space);
                return FALSE;
            }
        }
        else
        {
            c = w->edit_commodity;
            gnc_commodity_begin_edit(c);
            if (!commodity_book_is_live (w->book))
            {
                g_free (name_space);
                return FALSE;
            }

            gnc_commodity_table_remove (table, c);
            if (!commodity_book_is_live (w->book))
            {
                g_free (name_space);
                return FALSE;
            }

            gnc_commodity_set_fullname (c, fullname);
            if (!commodity_book_is_live (w->book))
            {
                g_free (name_space);
                return FALSE;
            }
            gnc_commodity_set_mnemonic (c, mnemonic);
            if (!commodity_book_is_live (w->book))
            {
                g_free (name_space);
                return FALSE;
            }
            gnc_commodity_set_namespace (c, name_space);
            if (!commodity_book_is_live (w->book))
            {
                g_free (name_space);
                return FALSE;
            }
            gnc_commodity_set_cusip (c, code);
            if (!commodity_book_is_live (w->book))
            {
                g_free (name_space);
                return FALSE;
            }
            gnc_commodity_set_fraction (c, fraction);
            if (!commodity_book_is_live (w->book))
            {
                g_free (name_space);
                return FALSE;
            }
            gnc_commodity_set_user_symbol(c, user_symbol);
            if (!commodity_book_is_live (w->book))
            {
                g_free (name_space);
                return FALSE;
            }
        }

        gnc_commodity_user_set_quote_flag (c, quote_set);
        if (!commodity_book_is_live (w->book))
        {
            g_free (name_space);
            return FALSE;
        }
        gnc_commodity_set_quote_source(c, source);
        if (!commodity_book_is_live (w->book))
        {
            g_free (name_space);
            return FALSE;
        }
        gnc_commodity_set_quote_tz(c, string);
        if (!commodity_book_is_live (w->book))
        {
            g_free (name_space);
            return FALSE;
        }
        g_free (w->result_namespace);
        g_free (w->result_mnemonic);
        w->result_namespace = g_strdup (name_space);
        w->result_mnemonic = g_strdup (mnemonic);

        auto conflict = gnc_commodity_table_lookup (table, name_space, mnemonic);
        if (conflict && conflict != c)
        {
            g_free (name_space);
            return FALSE;
        }

        /* Insert while the original book/table is still known. Commit may
         * emit events that change or destroy the current session. */
        w->edit_commodity = gnc_commodity_table_insert (table, c);
        if (!commodity_book_is_live (w->book))
        {
            g_free (name_space);
            return FALSE;
        }
        if (!w->edit_commodity)
        {
            g_free (name_space);
            return FALSE;
        }
        gnc_commodity_commit_edit(w->edit_commodity);
    }
    else
    {
        gnc_ui_commodity_warning (w, _("You must enter a non-empty \"Full name\", "
                                       "\"Symbol/abbreviation\", "
                                       "and \"Type\" for the commodity."));
        g_free(name_space);
        return FALSE;
    }
    g_free(name_space);
    LEAVE(" ");
    return TRUE;
}

/** @} */
/** @} */
