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

enum
{
    SOURCE_COL_NAME = 0,
    SOURCE_COL_FQ_SUPPORTED,
    NUM_SOURCE_COLS
};

struct select_commodity_window
{
    GtkWidget * window;
    GtkWidget * namespace_combo;
    GtkWidget * commodity_combo;
    GtkWidget * select_user_prompt;
    GtkWidget * ok_button;
    GtkWidget * cancel_button;
    GtkWidget * new_button;

    gnc_commodity * selection;

    const char * default_cusip;
    const char * default_fullname;
    const char * default_mnemonic;
    const char * default_user_symbol;
    int          default_fraction;
};

struct commodity_window
{
    GtkWidget * window;
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
    GtkWidget * cancel_button;
    GtkWidget * help_button;

    int comm_section_top;
    int comm_section_bottom;
    int comm_symbol_line;
    int fq_section_top;
    int fq_section_bottom;

    gboolean       is_currency;
    gnc_commodity * edit_commodity;
};

typedef struct select_commodity_window SelectCommodityWindow;
typedef struct commodity_window CommodityWindow;

/* The commodity selection window */
static SelectCommodityWindow *
gnc_ui_select_commodity_create (const gnc_commodity *orig_sel,
                                dialog_commodity_mode mode);
void gnc_ui_select_commodity_new_cb (GtkButton *button,
                                     gpointer user_data);
extern "C" {
void gnc_ui_select_commodity_changed_cb (GtkComboBox *cbwe,
                                         gpointer user_data);
void gnc_ui_select_commodity_namespace_changed_cb (GtkComboBox *cbwe,
                                                   gpointer user_data);
void gnc_ui_select_commodity_response_cb (GtkWidget *widget, gpointer user_data);

/* The commodity creation window */
void gnc_ui_commodity_changed_cb (GtkWidget *dummy, gpointer user_data);
void gnc_ui_commodity_quote_info_cb (GtkWidget *widget, gpointer user_data);
void gnc_ui_commodity_response_cb (GtkWidget *widget, gpointer user_data);
}
gboolean gnc_ui_commodity_dialog_to_object (CommodityWindow *cw);

void
gnc_ui_select_commodity_response_cb (GtkWidget *widget, gpointer user_data)
{
    auto scw = static_cast<SelectCommodityWindow *> (g_object_get_data (
                                                     G_OBJECT(user_data), "user-data"));

    if (widget == scw->ok_button)
    {
        DEBUG("case OK");
        gtk_window_destroy (GTK_WINDOW(scw->window));
        scw->window = nullptr;
    }
    if (widget == scw->new_button)
    {
        DEBUG("case NEW");
        gnc_ui_select_commodity_new_cb (nullptr, scw);
    }
    if (widget == scw->cancel_button)
    {
        DEBUG("case CANCEL");
        gtk_window_destroy (GTK_WINDOW(scw->window));
        scw->window = nullptr;
    }
}

/********************************************************************
 * gnc_ui_select_commodity_modal_full()
 ********************************************************************/
gnc_commodity *
gnc_ui_select_commodity_modal_full (gnc_commodity * orig_sel,
                                    GtkWidget * parent,
                                    dialog_commodity_mode mode,
                                    const char * user_message,
                                    const char * cusip,
                                    const char * fullname,
                                    const char * mnemonic)
{
    gnc_commodity *retval = nullptr;
    const gchar *initial;
    gchar *user_prompt_text;
    SelectCommodityWindow *scw;

    scw = gnc_ui_select_commodity_create (orig_sel, mode);
    scw->default_cusip = cusip;
    scw->default_fullname = fullname;
    scw->default_mnemonic = mnemonic;
    scw->default_user_symbol = "";

    if (parent)
        gtk_window_set_transient_for (GTK_WINDOW(scw->window),
                                      GTK_WINDOW(parent));

    if (user_message != nullptr)
        initial = user_message;
    else if ((cusip != nullptr) || (fullname != nullptr) || (mnemonic != nullptr))
        initial = _("\nPlease select a commodity to match");
    else
        initial = "";

    user_prompt_text =
        g_strdup_printf("%s%s%s%s%s%s%s",
                        initial,
                        fullname ? _("\nCommodity: ") : "",
                        fullname ? fullname : "",
                        /* Translators: Replace here and later CUSIP by the name of your local
                           National Securities Identifying Number
                           like gb:SEDOL, de:WKN, ch:Valorennummer, fr:SICOVAM ...
                           See https://en.wikipedia.org/wiki/ISIN and
                           https://en.wikipedia.org/wiki/National_numbering_agency for hints. */
                        cusip    ? _("\nExchange code (ISIN, CUSIP or similar): ") : "",
                        cusip    ? cusip : "",
                        mnemonic ? _("\nMnemonic (Ticker symbol or similar): ") : "",
                        mnemonic ? mnemonic : "");
    gtk_label_set_text ((GtkLabel *)(scw->select_user_prompt), user_prompt_text);
    g_free (user_prompt_text);

    /* Run the dialog, handling the terminal conditions. */
//FIXME gtk4 this may need changing
    while (scw->window)
        g_main_context_iteration (nullptr, false);

    retval = scw->selection;

    g_free (scw);

    return retval;
}


/********************************************************************
 * gnc_ui_select_commodity_modal()
 ********************************************************************/
gnc_commodity *
gnc_ui_select_commodity_modal (gnc_commodity *orig_sel,
                               GtkWidget *parent,
                               dialog_commodity_mode mode)
{
    return gnc_ui_select_commodity_modal_full (orig_sel,
                                               parent,
                                               mode,
                                               nullptr,
                                               nullptr,
                                               nullptr,
                                               nullptr);
}


static gboolean
select_commodity_dialog_key_press_cb (GtkEventControllerKey *key, guint keyval,
                                      guint keycode, GdkModifierType state,
                                      gpointer user_data)
{
    auto scw = static_cast<SelectCommodityWindow*>(user_data);

    if (keyval == GDK_KEY_Escape)
    {
        if (gnc_ok_to_close_window (GTK_WIDGET(scw->window)))
            gtk_window_destroy (GTK_WINDOW(scw->window));
        return true;
    }
    else
        return false;
}


static void
select_commodity_dialog_destroy_cb (GtkWidget *object, gpointer user_data)
{
    auto scw = static_cast<SelectCommodityWindow*>(user_data);

    if (scw->window)
         scw->window = nullptr;
}


/********************************************************************
 * gnc_ui_select_commodity_create()
 ********************************************************************/
static SelectCommodityWindow *
gnc_ui_select_commodity_create (const gnc_commodity *orig_sel,
                                dialog_commodity_mode mode)
{
    SelectCommodityWindow *scw = g_new0 (SelectCommodityWindow, 1);
    GtkBuilder *builder;
    const char *title, *text;
    gchar *name_space;
    GtkWidget *button, *label;

    builder = gtk_builder_new();
    gnc_builder_set_current_object (builder, scw);
    gnc_builder_add_from_file (builder, "dialog-commodity.ui", "liststore1");
    gnc_builder_add_from_file (builder, "dialog-commodity.ui", "liststore2");
    gnc_builder_add_from_file (builder, "dialog-commodity.ui", "security_selector_window");

    scw->window = GTK_WIDGET(gtk_builder_get_object (builder, "security_selector_window"));
    scw->namespace_combo = GTK_WIDGET(gtk_builder_get_object (builder, "ss_namespace_cbwe"));
    scw->commodity_combo = GTK_WIDGET(gtk_builder_get_object (builder, "ss_commodity_cbwe"));
    scw->select_user_prompt = GTK_WIDGET(gtk_builder_get_object (builder, "select_user_prompt"));
    scw->ok_button = GTK_WIDGET(gtk_builder_get_object (builder, "ss_ok_button"));
    scw->cancel_button = GTK_WIDGET(gtk_builder_get_object (builder, "ss_cancel_button"));
    scw->new_button = GTK_WIDGET(gtk_builder_get_object (builder, "ss_new_button"));
    label = GTK_WIDGET(gtk_builder_get_object (builder, "item_label"));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(scw->window), "gnc-id-security-select");
    gnc_widget_style_context_add_class (GTK_WIDGET(scw->window), "gnc-class-securities");

    gnc_cbwe_require_list_item (GTK_COMBO_BOX(scw->namespace_combo));
    gnc_cbwe_require_list_item (GTK_COMBO_BOX(scw->commodity_combo));

    gtk_label_set_text (GTK_LABEL(scw->select_user_prompt), "");

    g_signal_connect (G_OBJECT(scw->window), "destroy",
                      G_CALLBACK(select_commodity_dialog_destroy_cb), scw);

    GtkEventController *event_controller_window = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET(scw->window), event_controller_window);
    g_signal_connect (G_OBJECT(event_controller_window),
                      "key-pressed",
                      G_CALLBACK(select_commodity_dialog_key_press_cb), scw);

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
        gtk_box_remove (GTK_BOX(gtk_widget_get_parent (
                        GTK_WIDGET(scw->new_button))),
                        GTK_WIDGET(scw->new_button));
        gtk_widget_set_hexpand (GTK_WIDGET(scw->cancel_button), true);
        break;
    }
    gtk_window_set_title (GTK_WINDOW(scw->window), title);
    gtk_label_set_text_with_mnemonic (GTK_LABEL(label), text);

    /* build the menus of namespaces and commodities */
    gnc_ui_update_namespace_picker (scw->namespace_combo,
                                    gnc_commodity_get_namespace (orig_sel),
                                    mode);
    name_space = gnc_ui_namespace_picker_ns (scw->namespace_combo);
    gnc_ui_update_commodity_picker (scw->commodity_combo, name_space,
                                    gnc_commodity_get_printname (orig_sel));


    gtk_widget_set_visible (GTK_WIDGET(scw->window), true);

    g_object_unref (G_OBJECT(builder));

    g_free (name_space);
    return scw;
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
gnc_ui_select_commodity_new_cb (GtkButton *button,
                                gpointer user_data)
{
    auto scw = static_cast<SelectCommodityWindow*>(user_data);

    gchar *name_space = gnc_ui_namespace_picker_ns (scw->namespace_combo);

    const gnc_commodity * new_commodity =
        gnc_ui_new_commodity_modal_full (name_space,
                                         scw->window,
                                         scw->default_cusip,
                                         scw->default_fullname,
                                         scw->default_mnemonic,
                                         scw->default_user_symbol,
                                         scw->default_fraction);
    if (new_commodity)
    {
        gnc_ui_update_namespace_picker (scw->namespace_combo,
                                        gnc_commodity_get_namespace (new_commodity),
                                        DIAG_COMM_ALL);
        gnc_ui_update_commodity_picker (scw->commodity_combo,
                                        gnc_commodity_get_namespace (new_commodity),
                                        gnc_commodity_get_printname (new_commodity));
    }
    g_free (name_space);
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
    auto scw = static_cast<SelectCommodityWindow *> (g_object_get_data (
                                                     G_OBJECT(user_data), "user-data"));

    gchar *name_space;
    const gchar *fullname;
    gboolean ok;

    ENTER("cbwe=%p, user_data=%p", cbwe, user_data);
    name_space = gnc_ui_namespace_picker_ns (scw->namespace_combo);
    fullname = gnc_entry_get_text (GTK_ENTRY (gtk_combo_box_get_child (
                                   GTK_COMBO_BOX(scw->commodity_combo))));

    DEBUG("namespace=%s, name=%s", name_space, fullname);
    scw->selection = gnc_commodity_table_find_full (gnc_get_current_commodities(),
                                                    name_space, fullname);
    g_free (name_space);

    ok = (scw->selection != nullptr);
    gtk_widget_set_sensitive (GTK_WIDGET(scw->ok_button), ok);

    if (ok) //FIXME gtk4, may not work
        gtk_window_set_default_widget (GTK_WINDOW(scw->window), GTK_WIDGET(scw->ok_button));
    else
        gtk_window_set_default_widget (GTK_WINDOW(scw->window), GTK_WIDGET(scw->cancel_button));

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
    auto scw = static_cast<SelectCommodityWindow *> (g_object_get_data (
                                                     G_OBJECT(user_data), "user-data"));

    ENTER("cbwe=%p, user_data=%p", cbwe, user_data);
    gchar *name_space = gnc_ui_namespace_picker_ns (scw->namespace_combo);
    DEBUG("name_space=%s", name_space);
    gnc_ui_update_commodity_picker (scw->commodity_combo, name_space, nullptr);
    g_free (name_space);
    LEAVE(" ");
}


/********************************************************************
 * gnc_ui_update_commodity_picker
 ********************************************************************/
static int
collate (gconstpointer a, gconstpointer b)
{
    if (!a)
        return -1;
    if (!b)
        return 1;
    return g_utf8_collate (static_cast<const char*>(a), static_cast<const char*>(b));
}


void
gnc_ui_update_commodity_picker (GtkWidget *cbwe,
                                const gchar *name_space,
                                const gchar *init_string)
{
    GList *commodities;
    GList *iterator = nullptr;
    GList *commodity_items = nullptr;
    GtkComboBox *combo_box;
    GtkEntry *entry;
    GtkTreeModel *model;
    GtkTreeIter iter;
    gnc_commodity_table *table;
    gint current = 0, match = 0;
    gchar *name;

    g_return_if_fail (GTK_IS_COMBO_BOX(cbwe));
    g_return_if_fail (name_space);

    /* Erase the old entries */
    combo_box = GTK_COMBO_BOX(cbwe);
    model = gtk_combo_box_get_model (combo_box);
    gtk_list_store_clear (GTK_LIST_STORE(model));

    /* Erase the entry text */
    entry = GTK_ENTRY(gtk_combo_box_get_child (GTK_COMBO_BOX(combo_box)));
    gtk_editable_delete_text (GTK_EDITABLE(entry), 0, -1);

    gtk_combo_box_set_active (combo_box, -1);

    table = gnc_commodity_table_get_table (gnc_get_current_book ());
    commodities = gnc_commodity_table_get_commodities (table, name_space);
    for (iterator = commodities; iterator; iterator = iterator->next)
    {
        commodity_items =
            g_list_prepend (commodity_items, (gpointer) gnc_commodity_get_printname
                                              (GNC_COMMODITY(iterator->data)));
    }
    g_list_free (commodities);

    commodity_items = g_list_sort (commodity_items, collate);
    for (iterator = commodity_items; iterator; iterator = iterator->next)
    {
        name = (char *)iterator->data;
        gtk_list_store_append (GTK_LIST_STORE(model), &iter);
        gtk_list_store_set (GTK_LIST_STORE(model), &iter, 0, name, -1);

        if (init_string && g_utf8_collate (name, init_string) == 0)
            match = current;
        current++;
    }
    gtk_combo_box_set_active (combo_box, match);
    g_list_free (commodity_items);
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
    int offset = 0;

    gtk_grid_query_child (GTK_GRID(cw->table), GTK_WIDGET(widget),
                          nullptr, &offset, nullptr, nullptr); //col, row, w, h

    if ((offset < cw->comm_section_top) || (offset >= cw->comm_section_bottom))
        return;
    if (cw->is_currency)
        gtk_widget_set_sensitive (GTK_WIDGET(widget), offset == cw->comm_symbol_line);
}

static void
gnc_ui_update_commodity_info (CommodityWindow *cw)
{
    GtkWidget *child;
    for (child = gtk_widget_get_first_child (GTK_WIDGET(cw->table));
         child != nullptr;
         child = gtk_widget_get_next_sibling (GTK_WIDGET(child)))
    {
        gnc_set_commodity_section_sensitivity (GTK_WIDGET(child), cw);
    }
}


static void
gnc_set_fq_sensitivity (GtkWidget *widget, gpointer user_data)
{
    auto cw = static_cast<CommodityWindow*>(user_data);
    int offset = 0;

    gtk_grid_query_child (GTK_GRID(cw->table), GTK_WIDGET(widget),
                          nullptr, &offset, nullptr, nullptr); //col, row, w, h

    if ((offset < cw->fq_section_top) || (offset >= cw->fq_section_bottom))
        return;
    g_object_set (widget, "sensitive", false, nullptr);
}


static void
gnc_ui_update_fq_info (CommodityWindow *cw)
{
    GtkWidget *child;
    for (child = gtk_widget_get_first_child (GTK_WIDGET(cw->table));
         child != nullptr;
         child = gtk_widget_get_next_sibling (GTK_WIDGET(child)))
    {
        gnc_set_fq_sensitivity (GTK_WIDGET(child), cw);
    }
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
    gboolean matched = false;

    g_return_if_fail (GTK_IS_COMBO_BOX(cbwe));

    /* Erase the old entries */
    combo_box = GTK_COMBO_BOX(cbwe);
    model = gtk_combo_box_get_model (combo_box);
    gtk_list_store_clear (GTK_LIST_STORE(model));

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
        gtk_list_store_append (GTK_LIST_STORE(model), &iter);
        gtk_list_store_set (GTK_LIST_STORE(model), &iter, 0,
                            _(GNC_COMMODITY_NS_ISO_GUI), -1);

        if (init_string &&
            (g_utf8_collate (GNC_COMMODITY_NS_ISO_GUI, init_string) == 0))
        {
            matched = true;
            match = iter;
        }
    }

    /* Next insert "All non-currency" entry if requested */
    if (mode == DIAG_COMM_NON_CURRENCY_SELECT || mode == DIAG_COMM_ALL)
    {
        gtk_list_store_append (GTK_LIST_STORE(model), &iter);
        gtk_list_store_set (GTK_LIST_STORE(model), &iter, 0,
                            GNC_COMMODITY_NS_NONISO_GUI, -1);
    }

    /* add all others to the combobox */
    namespaces = g_list_sort (namespaces, collate);
    for (node = namespaces; node; node = node->next)
    {
        auto ns = static_cast<const char*>(node->data);
        /* Skip template, legacy and currency namespaces.
           The latter was added as first entry earlier */
        if ((g_utf8_collate (ns, GNC_COMMODITY_NS_LEGACY) == 0) ||
            (g_utf8_collate (ns, GNC_COMMODITY_NS_TEMPLATE ) == 0) ||
            (g_utf8_collate (ns, GNC_COMMODITY_NS_CURRENCY ) == 0))
            continue;

        gtk_list_store_append (GTK_LIST_STORE(model), &iter);
        gtk_list_store_set (GTK_LIST_STORE(model), &iter, 0, ns, -1);

        if (init_string &&
            (g_utf8_collate (ns, init_string) == 0))
        {
            matched = true;
            match = iter;
        }
    }

    if (!matched)
        matched = gtk_tree_model_get_iter_first (model, &match);

    if (matched)
        gtk_combo_box_set_active_iter (combo_box, &match);

    g_list_free (namespaces);
}


gchar *
gnc_ui_namespace_picker_ns (GtkWidget *cbwe)
{
    const gchar *name_space;

    g_return_val_if_fail (GTK_IS_COMBO_BOX(cbwe), nullptr);

    name_space = gnc_entry_get_text (GTK_ENTRY(gtk_combo_box_get_child (GTK_COMBO_BOX(cbwe))));

    /* Map several currency related names to one common namespace */
    if ((g_strcmp0 (name_space, GNC_COMMODITY_NS_ISO) == 0) ||
        (g_strcmp0 (name_space, GNC_COMMODITY_NS_ISO_GUI) == 0) ||
        (g_strcmp0 (name_space, _(GNC_COMMODITY_NS_ISO_GUI)) == 0))
        return g_strdup (GNC_COMMODITY_NS_CURRENCY);
    else
        return g_strdup (name_space);
}


/********************************************************************
 * gnc_ui_commodity_quote_info_cb                                   *
 *******************************************************************/
static void
gnc_ui_commodity_quote_info (GtkWidget *widget, CommodityWindow *cw)
{
    gboolean get_quote, allow_src, active;
    const gchar *text;
    gint i;

    ENTER(" ");
    get_quote = gtk_check_button_get_active (GTK_CHECK_BUTTON(widget));

    text = gnc_entry_get_text (GTK_ENTRY(gtk_combo_box_get_child (
                               GTK_COMBO_BOX(cw->namespace_combo))));

    allow_src = !gnc_commodity_namespace_is_iso (text);

    gtk_widget_set_sensitive (GTK_WIDGET(cw->source_label), get_quote && allow_src);

    for (i = SOURCE_SINGLE; i < SOURCE_MAX; i++)
    {
        if (!cw->source_button[i])
            continue;
        active = gtk_check_button_get_active (GTK_CHECK_BUTTON(cw->source_button[i]));
        gtk_widget_set_sensitive (GTK_WIDGET(cw->source_button[i]), get_quote && allow_src);
        gtk_widget_set_sensitive (GTK_WIDGET(cw->source_menu[i]), get_quote && allow_src && active);
    }
    gtk_widget_set_sensitive (GTK_WIDGET(cw->quote_tz_label), get_quote);
    gtk_widget_set_sensitive (GTK_WIDGET(cw->quote_tz_menu), get_quote);

    LEAVE(" ");
}

void
gnc_ui_commodity_quote_info_cb (GtkWidget *widget, gpointer user_data)
{
    auto cw = static_cast<CommodityWindow *> (g_object_get_data (
                                              G_OBJECT(user_data), "user-data"));

    gnc_ui_commodity_quote_info (widget, cw);
}

void
gnc_ui_commodity_changed_cb (GtkWidget * dummy, gpointer user_data)
{
    auto cw = static_cast<CommodityWindow *> (g_object_get_data (
                                              G_OBJECT(user_data), "user-data"));
    gboolean ok;

    ENTER("widget=%p, user_data=%p", dummy, user_data);
    if (!cw->is_currency)
    {
        gchar *name_space = gnc_ui_namespace_picker_ns (cw->namespace_combo);
        const char * fullname = gnc_entry_get_text (GTK_ENTRY(cw->fullname_entry));
        const char * mnemonic = gnc_entry_get_text (GTK_ENTRY(cw->mnemonic_entry));
        DEBUG("namespace=%s, name=%s, mnemonic=%s", name_space, fullname, mnemonic);
        ok = (fullname    && name_space    && mnemonic &&
              fullname[0] && name_space[0] && mnemonic[0]);
        g_free (name_space);
    }
    else
    {
        ok = TRUE;
    }
    gtk_widget_set_sensitive (GTK_WIDGET(cw->ok_button), ok);

    if (ok) //FIXME gtk4, may not work
        gtk_window_set_default_widget (GTK_WINDOW(cw->window), GTK_WIDGET(cw->ok_button));
    else
        gtk_window_set_default_widget (GTK_WINDOW(cw->window), GTK_WIDGET(cw->cancel_button));

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
gnc_ui_source_menu_create (QuoteSourceType type)
{
    gint i, max;
    const gchar *name;
    gboolean supported;
    GtkListStore *store;
    GtkTreeIter iter;
    GtkWidget *combo;
    GtkCellRenderer *renderer;
    gnc_quote_source *source;

    store = gtk_list_store_new (NUM_SOURCE_COLS, G_TYPE_STRING, G_TYPE_BOOLEAN);
    if (type == SOURCE_CURRENCY)
    {
        gtk_list_store_append (store, &iter);
        gtk_list_store_set (store, &iter,
                            SOURCE_COL_NAME, _("Currency"),
                            SOURCE_COL_FQ_SUPPORTED, TRUE,
                            -1);
    }
    else
    {
        max = gnc_quote_source_num_entries (type);
        for (i = 0; i < max; i++)
        {
            source = gnc_quote_source_lookup_by_ti (type, i);
            if (source == nullptr)
                break;
            name = gnc_quote_source_get_user_name (source);
            supported = gnc_quote_source_get_supported (source);
            gtk_list_store_append (store, &iter);
            gtk_list_store_set (store, &iter,
                                SOURCE_COL_NAME, name,
                                SOURCE_COL_FQ_SUPPORTED, supported,
                                -1);
        }
    }

    combo = gtk_combo_box_new_with_model (GTK_TREE_MODEL(store));
    g_object_unref (store);
    renderer = gtk_cell_renderer_text_new ();
    gtk_cell_layout_pack_start (GTK_CELL_LAYOUT(combo), renderer, true);
    gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT(combo), renderer,
                                   "text", SOURCE_COL_NAME);
    gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT(combo), renderer,
                                   "sensitive", SOURCE_COL_FQ_SUPPORTED);
    gtk_combo_box_set_active (GTK_COMBO_BOX(combo), 0);
    gtk_widget_set_visible (GTK_WIDGET(combo), true);
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
gnc_find_timezone_menu_position (const gchar *timezone)
{
    /* returns 0 on failure, position otherwise. */
    gboolean found = false;
    guint i = 0;
    while (!found && known_timezones[i])
    {
        if (g_strcmp0 (timezone, known_timezones[i]) != 0)
        {
            i++;
        }
        else
        {
            found = true;
        }
    }
    if (found) return i + 1;
    return 0;
}


static const gchar *
gnc_timezone_menu_position_to_string (guint pos)
{
    if (pos == 0) return nullptr;
    return known_timezones[pos - 1];
}


static GtkWidget *
gnc_ui_quote_tz_menu_create (void)
{
    GtkWidget  *combo;
    const gchar **itemstr;

    /* add items here as needed, but bear in mind that right now these
       must be timezones that GNU libc understands.  Also, I'd prefer if
       we only add things here we *know* we need.  That's because in
       order to be portable to non GNU OSes, we may have to support
       whatever we add here manually on those systems. */

    combo = gtk_combo_box_text_new ();
    gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT(combo), _("Use local time"));
    for (itemstr = &known_timezones[0]; *itemstr; itemstr++)
    {
        gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT(combo), *itemstr);
    }

    gtk_widget_set_visible (GTK_WIDGET(combo), true);
    return combo;
}


static gboolean
commodity_dialog_key_press_cb (GtkEventControllerKey *key, guint keyval,
                               guint keycode, GdkModifierType state,
                               gpointer user_data)
{
    auto cw = static_cast<CommodityWindow*>(user_data);

    if (keyval == GDK_KEY_Escape)
    {
        if (gnc_ok_to_close_window (GTK_WIDGET(cw->window)))
        {
            gtk_window_destroy (GTK_WINDOW(cw->window));
            cw->window = nullptr;
        }
        return true;
    }
    else
        return false;
}


static void
commodity_dialog_destroy_cb (GtkWidget *object, gpointer user_data)
{
    auto cw = static_cast<CommodityWindow*>(user_data);

    if (cw)
        cw->window = nullptr;
}


void
gnc_ui_commodity_response_cb (GtkWidget *widget, gpointer user_data)
{
    auto cw = static_cast<CommodityWindow *> (g_object_get_data (
                                              G_OBJECT(user_data), "user-data"));

    if (widget == cw->ok_button)
    {
        DEBUG("case OK");
        if (gnc_ui_commodity_dialog_to_object (cw))
        {
            gtk_window_destroy (GTK_WINDOW(cw->window));
            cw->window = nullptr;
        }
    }

    if (widget == cw->help_button)
    {
        DEBUG("case HELP");
        gnc_gnome_help (GTK_WINDOW(cw->window), DF_MANUAL, DL_COMMODITY);
    }

    if (widget == cw->cancel_button)
    {
        DEBUG("case CANCEL");
        gtk_window_destroy (GTK_WINDOW(cw->window));
        cw->window = nullptr;
    }
}


/*******************************************************
 * Build the new/edit commodity dialog box             *
 *******************************************************/
static CommodityWindow *
gnc_ui_build_commodity_dialog (const char *selected_namespace,
                               GtkWidget  *parent,
                               const char *fullname,
                               const char *mnemonic,
                               const char *user_symbol,
                               const char *cusip,
                               int         fraction,
                               gboolean    edit)
{
    CommodityWindow *cw = g_new0 (CommodityWindow, 1);
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
    gnc_builder_set_current_object (builder, cw);
    gnc_builder_add_from_file (builder, "dialog-commodity.ui", "liststore2");
    gnc_builder_add_from_file (builder, "dialog-commodity.ui", "adjustment1");
    gnc_builder_add_from_file (builder, "dialog-commodity.ui", "security_window");

    cw->window = GTK_WIDGET(gtk_builder_get_object (builder, "security_window"));

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(cw->window), "gnc-id-security");
    gnc_widget_style_context_add_class (GTK_WIDGET(cw->window), "gnc-class-securities");

    if (parent != nullptr)
        gtk_window_set_transient_for (GTK_WINDOW(cw->window), GTK_WINDOW(parent));

    cw->edit_commodity = nullptr;

    /* Get widget pointers */
    cw->fullname_entry = GTK_WIDGET(gtk_builder_get_object (builder, "fullname_entry"));
    cw->mnemonic_entry = GTK_WIDGET(gtk_builder_get_object (builder, "mnemonic_entry"));
    cw->user_symbol_entry = GTK_WIDGET(gtk_builder_get_object (builder, "user_symbol_entry"));
    cw->namespace_combo = GTK_WIDGET(gtk_builder_get_object (builder, "namespace_cbwe"));
    cw->code_entry = GTK_WIDGET(gtk_builder_get_object (builder, "code_entry"));
    cw->fraction_spinbutton = GTK_WIDGET(gtk_builder_get_object (builder, "fraction_spinbutton"));
    cw->ok_button = GTK_WIDGET(gtk_builder_get_object (builder, "ok_button"));
    cw->cancel_button = GTK_WIDGET(gtk_builder_get_object (builder, "cancel_button"));
    cw->help_button = GTK_WIDGET(gtk_builder_get_object (builder, "help_button"));
    cw->get_quote_check = GTK_WIDGET(gtk_builder_get_object (builder, "get_quote_check"));
    cw->source_label = GTK_WIDGET(gtk_builder_get_object (builder, "source_label"));
    cw->source_button[SOURCE_SINGLE] = GTK_WIDGET(gtk_builder_get_object (builder, "single_source_button"));
    cw->source_button[SOURCE_MULTI] = GTK_WIDGET(gtk_builder_get_object (builder, "multi_source_button"));
    cw->source_button[SOURCE_UNKNOWN]= GTK_WIDGET(gtk_builder_get_object (builder, "unknown_source_button"));
    cw->quote_tz_label = GTK_WIDGET(gtk_builder_get_object (builder, "quote_tz_label"));

    /* Determine the commodity section of the dialog */
    cw->table = GTK_WIDGET(gtk_builder_get_object (builder, "edit_table"));
    sec_label = GTK_WIDGET(gtk_builder_get_object (builder, "security_label"));

    gtk_grid_query_child (GTK_GRID(cw->table), GTK_WIDGET(sec_label),
                          nullptr, &cw->comm_section_top, nullptr, nullptr); //col, row, w, h

    widget = GTK_WIDGET(gtk_builder_get_object (builder, "quote_label"));

    gtk_grid_query_child (GTK_GRID(cw->table), GTK_WIDGET(widget),
                          nullptr, &cw->comm_section_bottom, nullptr, nullptr); //col, row, w, h

    gtk_grid_query_child (GTK_GRID(cw->table), GTK_WIDGET(cw->user_symbol_entry),
                          nullptr, &cw->comm_symbol_line, nullptr, nullptr); //col, row, w, h

    /* Build custom widgets */
    box = GTK_WIDGET(gtk_builder_get_object (builder, "single_source_box"));
    if (gnc_commodity_namespace_is_iso (selected_namespace))
    {
        menu = gnc_ui_source_menu_create (SOURCE_CURRENCY);
    }
    else
    {
        menu = gnc_ui_source_menu_create (SOURCE_SINGLE);
    }
    cw->source_menu[SOURCE_SINGLE] = menu;
    gtk_box_append (GTK_BOX(box), GTK_WIDGET(menu));
    gtk_widget_set_hexpand (GTK_WIDGET(menu), true);

    box = GTK_WIDGET(gtk_builder_get_object (builder, "multi_source_box"));
    menu = gnc_ui_source_menu_create (SOURCE_MULTI);
    cw->source_menu[SOURCE_MULTI] = menu;
    gtk_box_append (GTK_BOX(box), GTK_WIDGET(menu));
    gtk_widget_set_hexpand (GTK_WIDGET(menu), true);

    if (gnc_quote_source_num_entries(SOURCE_UNKNOWN))
    {
        cw->source_button[SOURCE_UNKNOWN] =
            GTK_WIDGET(gtk_builder_get_object (builder, "unknown_source_button"));
        box = GTK_WIDGET(gtk_builder_get_object (builder, "unknown_source_box"));
        menu = gnc_ui_source_menu_create (SOURCE_UNKNOWN);
        cw->source_menu[SOURCE_UNKNOWN] = menu;
        gtk_box_append (GTK_BOX(box), GTK_WIDGET(menu));
        gtk_widget_set_hexpand (GTK_WIDGET(menu), true);
    }
    else
    {
        gtk_grid_set_row_spacing (GTK_GRID(cw->table), 0);
        widget = GTK_WIDGET(gtk_builder_get_object (builder, "unknown_source_box"));
        gtk_grid_remove (GTK_GRID(cw->table), GTK_WIDGET(widget));
    }

    box = GTK_WIDGET(gtk_builder_get_object (builder, "quote_tz_box"));
    cw->quote_tz_menu = gnc_ui_quote_tz_menu_create();
    gtk_box_append (GTK_BOX(box), GTK_WIDGET(cw->quote_tz_menu));
    gtk_widget_set_hexpand (GTK_WIDGET(cw->quote_tz_menu), true);

    /* Commodity editing is next to nil */
    if (gnc_commodity_namespace_is_iso (selected_namespace))
    {
        cw->is_currency = true;
        gnc_ui_update_commodity_info (cw);
        include_iso = true;
        title = _("Edit currency");
        text = g_strdup_printf ("<b>%s</b>", _("Currency Information"));
    }
    else
    {
        include_iso = false;
        title = edit ? _("Edit security") : _("New security");
        text = g_strdup_printf ("<b>%s</b>", _("Security Information"));
    }
    gtk_window_set_title (GTK_WINDOW(cw->window), title);
    gtk_label_set_markup (GTK_LABEL(sec_label), text);
    g_free (text);

    /* Are price quotes supported */
    if (gnc_quote_source_fq_installed())
    {
        // note the "finance_quote_warning" is child of "fq_warning_alignment"
        gtk_grid_remove (GTK_GRID(cw->table),
                         GTK_WIDGET(gtk_builder_get_object (builder,"fq_warning_alignment")));
    }
    else
    {
        /* Determine the price quote of the dialog */
        widget = GTK_WIDGET(gtk_builder_get_object (builder, "fq_warning_alignment"));

        gtk_grid_query_child (GTK_GRID(cw->table), GTK_WIDGET(widget),
                              nullptr, &cw->fq_section_top, nullptr, nullptr); //col, row, w, h

        widget = GTK_WIDGET(gtk_builder_get_object (builder, "bottom_alignment"));

        gtk_grid_query_child (GTK_GRID(cw->table), GTK_WIDGET(widget),
                              nullptr, &cw->fq_section_bottom, nullptr, nullptr); //col, row, w, h

        gnc_ui_update_fq_info (cw);
    }

    g_signal_connect (G_OBJECT(cw->window), "destroy",
                      G_CALLBACK(commodity_dialog_destroy_cb), cw);

    GtkEventController *event_controller_window = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET(cw->window), event_controller_window);
    g_signal_connect (G_OBJECT(event_controller_window),
                      "key-pressed",
                      G_CALLBACK(commodity_dialog_key_press_cb), cw);

    /* Fill in any data, top to bottom */
    gnc_entry_set_text (GTK_ENTRY(cw->fullname_entry), fullname ? fullname : "");
    gnc_entry_set_text (GTK_ENTRY(cw->mnemonic_entry), mnemonic ? mnemonic : "");
    gnc_entry_set_text (GTK_ENTRY(cw->user_symbol_entry), user_symbol ? user_symbol : "");
    gnc_cbwe_add_completion (GTK_COMBO_BOX(cw->namespace_combo));
    gnc_ui_update_namespace_picker (cw->namespace_combo,
                                    selected_namespace,
                                    include_iso ? DIAG_COMM_ALL : DIAG_COMM_NON_CURRENCY);
    gnc_entry_set_text (GTK_ENTRY(cw->code_entry), cusip ? cusip : "");

    if (fraction > 0)
        gtk_spin_button_set_value (GTK_SPIN_BUTTON(cw->fraction_spinbutton),
                                   fraction);

    gtk_widget_set_visible (GTK_WIDGET(cw->window), true);

    g_object_unref (G_OBJECT(builder));

    LEAVE(" ");
    return cw;
}


static void
gnc_ui_commodity_update_quote_info (CommodityWindow *cw,
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

    gtk_check_button_set_active (GTK_CHECK_BUTTON (cw->get_quote_check),
                                 has_quote_src);
    if (!gnc_commodity_is_iso (commodity))
    {
        type = gnc_quote_source_get_type (source);
        gtk_check_button_set_active (GTK_CHECK_BUTTON(cw->source_button[type]), true);
        gtk_combo_box_set_active (GTK_COMBO_BOX(cw->source_menu[type]),
                                  gnc_quote_source_get_index (source));
    }

    if (quote_tz)
    {
        pos = gnc_find_timezone_menu_position (quote_tz);
//    if(pos == 0) {
//      PWARN("Unknown price quote timezone (%s), resetting to default.",
//	    quote_tz ? quote_tz : "(null)");
//    }
    }
    gtk_combo_box_set_active (GTK_COMBO_BOX(cw->quote_tz_menu), pos);
    LEAVE(" ");
}


static gnc_commodity *
gnc_ui_common_commodity_modal (gnc_commodity *commodity,
                               GtkWidget *parent,
                               const char *name_space,
                               const char *cusip,
                               const char *fullname,
                               const char *mnemonic,
                               const char *user_symbol,
                               int fraction)
{
    CommodityWindow *cw;
    gnc_commodity *retval = nullptr;

    ENTER(" ");

    /* If a commodity was provided, copy out the existing info */
    if (commodity)
    {
        name_space = gnc_commodity_get_namespace (commodity);
        fullname = gnc_commodity_get_fullname (commodity);
        mnemonic = gnc_commodity_get_mnemonic (commodity);
        user_symbol = gnc_commodity_get_nice_symbol (commodity);
        cusip = gnc_commodity_get_cusip (commodity);
        fraction = gnc_commodity_get_fraction (commodity);
    }
    else
    {
        /* Not allowed to create new currencies */
        if (gnc_commodity_namespace_is_iso (name_space))
        {
            name_space = nullptr;
        }
    }

    cw = gnc_ui_build_commodity_dialog (name_space, parent, fullname,
                                        mnemonic, user_symbol, cusip,
                                        fraction, (commodity != nullptr));

    /* Update stock quote info based on existing commodity */
    gnc_ui_commodity_update_quote_info (cw, commodity);
    cw->edit_commodity = commodity;

    /* Update stock quote sensitivities based on check box */
    gnc_ui_commodity_quote_info (cw->get_quote_check, cw);

    /* Run the dialog, handling the terminal conditions. */
//FIXME gtk4 this may need changing
    while (cw->window)
        g_main_context_iteration (nullptr, false);

    retval = cw->edit_commodity;

    g_free (cw);

    LEAVE(" ");
    return retval;
}


/********************************************************
 * Create and run the new/edit commodity dialog.        *
 ********************************************************/
gnc_commodity *
gnc_ui_new_commodity_modal_full (const char *name_space,
                                 GtkWidget *parent,
                                 const char *cusip,
                                 const char *fullname,
                                 const char *mnemonic,
                                 const char *user_symbol,
                                 int fraction)
{
    gnc_commodity *result;

    ENTER(" ");
    result = gnc_ui_common_commodity_modal (nullptr, parent, name_space, cusip,
                                            fullname, mnemonic, user_symbol,
                                            10000);
    LEAVE(" ");
    return result;
}


/********************************************************************
 * External routine for popping up the new commodity dialog box.    *
 ********************************************************************/
gnc_commodity *
gnc_ui_new_commodity_modal (const char *default_namespace,
                            GtkWidget *parent)
{
    gnc_commodity *result;

    ENTER(" ");
    result = gnc_ui_common_commodity_modal (nullptr, parent, default_namespace, nullptr,
                                            nullptr, nullptr, nullptr, 0);
    LEAVE(" ");
    return result;
}


/********************************************************************
 * gnc_ui_edit_commodity_modal()
 ********************************************************************/
/** Given an existing commodity, uses the
 *  gnc_ui_build_commodity_dialog() routine to build a basic edit
 *  dialog, then fills in the price quote information at the bottom of
 *  the dialog.
 */
gboolean
gnc_ui_edit_commodity_modal (gnc_commodity *commodity,
                             GtkWidget *parent)
{
    gnc_commodity *result;

    ENTER(" ");
    result = gnc_ui_common_commodity_modal (commodity, parent, nullptr, nullptr,
                                            nullptr, nullptr, nullptr, 0);
    LEAVE(" ");
    return result != nullptr;
}


/********************************************************************
 * gnc_ui_commodity_dialog_to_object()
 ********************************************************************/
gboolean
gnc_ui_commodity_dialog_to_object (CommodityWindow * cw)
{
    gnc_quote_source *source;
    QuoteSourceType type;
    const char * fullname = gnc_entry_get_text (GTK_ENTRY(cw->fullname_entry));
    gchar *name_space = gnc_ui_namespace_picker_ns (cw->namespace_combo);
    const char * mnemonic = gnc_entry_get_text (GTK_ENTRY(cw->mnemonic_entry));
    const char * user_symbol = gnc_entry_get_text (GTK_ENTRY(cw->user_symbol_entry));
    const char * code = gnc_entry_get_text (GTK_ENTRY(cw->code_entry));
    QofBook * book = gnc_get_current_book ();
    int fraction = gtk_spin_button_get_value_as_int
                   (GTK_SPIN_BUTTON(cw->fraction_spinbutton));
    const char *string;
    gnc_commodity * c;
    gint selection;

    ENTER(" ");
    /* Special case currencies */
    if (gnc_commodity_namespace_is_iso (name_space))
    {
        if (cw->edit_commodity)
        {
            gboolean quote_set;
            quote_set = gtk_check_button_get_active
                        (GTK_CHECK_BUTTON(cw->get_quote_check));
            c = cw->edit_commodity;
            gnc_commodity_begin_edit (c);
            gnc_commodity_user_set_quote_flag (c, quote_set);
            if (quote_set)
            {
                selection = gtk_combo_box_get_active (GTK_COMBO_BOX(cw->quote_tz_menu));
                string = gnc_timezone_menu_position_to_string (selection);
                gnc_commodity_set_quote_tz (c, string);
            }
            else
                gnc_commodity_set_quote_tz (c, nullptr);

            gnc_commodity_set_user_symbol (c, user_symbol);

            gnc_commodity_commit_edit (c);
            return true;
        }
        gnc_warning_dialog (GTK_WINDOW(cw->window), "%s",
                            _("You may not create a new national currency."));
        return false;
    }

    /* Don't allow user to create commodities in namespace
     * "template". That's reserved for scheduled transaction use.
     */
    if (g_utf8_collate (name_space, GNC_COMMODITY_NS_TEMPLATE) == 0)
    {
        gnc_warning_dialog (GTK_WINDOW(cw->window),
                            _("%s is a reserved commodity type."
                            " Please use something else."), GNC_COMMODITY_NS_TEMPLATE);
        return false;
    }

    if (fullname && fullname[0] &&
            name_space && name_space[0] &&
            mnemonic && mnemonic[0])
    {
        c = gnc_commodity_table_lookup (gnc_get_current_commodities(),
                                        name_space, mnemonic);

        if ((!cw->edit_commodity && c) ||
                (cw->edit_commodity && c && (c != cw->edit_commodity)))
        {
            gnc_warning_dialog (GTK_WINDOW(cw->window), "%s",  _("That commodity already exists."));
            g_free (name_space);
            return false;
        }

        if (!cw->edit_commodity)
        {
            c = gnc_commodity_new (book, fullname, name_space, mnemonic, code, fraction);
            cw->edit_commodity = c;
            gnc_commodity_begin_edit (c);

            gnc_commodity_set_user_symbol (c, user_symbol);
        }
        else
        {
            c = cw->edit_commodity;
            gnc_commodity_begin_edit (c);

            gnc_commodity_table_remove (gnc_get_current_commodities(), c);

            gnc_commodity_set_fullname (c, fullname);
            gnc_commodity_set_mnemonic (c, mnemonic);
            gnc_commodity_set_namespace (c, name_space);
            gnc_commodity_set_cusip (c, code);
            gnc_commodity_set_fraction (c, fraction);
            gnc_commodity_set_user_symbol(c, user_symbol);
        }

        gnc_commodity_user_set_quote_flag (c, gtk_check_button_get_active
                                           (GTK_CHECK_BUTTON(cw->get_quote_check)));

        for (type = SOURCE_SINGLE; type < SOURCE_MAX; type = static_cast<QuoteSourceType>(type+1))
        {
            if (gtk_check_button_get_active (GTK_CHECK_BUTTON(cw->source_button[type])))
                break;
        }
        selection = gtk_combo_box_get_active (GTK_COMBO_BOX(cw->source_menu[type]));
        source = gnc_quote_source_lookup_by_ti (type, selection);
        gnc_commodity_set_quote_source (c, source);

        selection = gtk_combo_box_get_active (GTK_COMBO_BOX(cw->quote_tz_menu));
        string = gnc_timezone_menu_position_to_string (selection);
        gnc_commodity_set_quote_tz (c, string);
        gnc_commodity_commit_edit (c);

        /* remember the commodity */
        gnc_commodity_table_insert (gnc_get_current_commodities(), c);
    }
    else
    {
        gnc_warning_dialog (GTK_WINDOW(cw->window), "%s",
                            _("You must enter a non-empty \"Full name\", "
                              "\"Symbol/abbreviation\", "
                              "and \"Type\" for the commodity."));
        g_free (name_space);
        return false;
    }
    g_free (name_space);
    LEAVE(" ");
    return true;
}

/** @} */
/** @} */
