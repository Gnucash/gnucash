/********************************************************************\
 * dialog-commodities.c -- commodities dialog                       *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Author: Dave Peticolas <dave@krondo.com>                         *
 * Copyright (C) 2003,2005 David Hampton                            *
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
\********************************************************************/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-commodity.h"
#include "gnc-commodity.hpp"
#include "dialog-utils.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "qof.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-gtk-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-session.h"
#include "gnc-warnings.h"
#include "Account.hpp"

#include <algorithm>
#include <vector>
#include <string>

#define DIALOG_COMMODITIES_CM_CLASS "dialog-commodities"
#define STATE_SECTION "dialogs/edit_commodities"
#define GNC_PREFS_GROUP   "dialogs.commodities"
#define GNC_PREF_INCL_ISO "include-iso"

/* This static indicates the debugging module that this .o belongs to.  */
/* static short module = MOD_GUI; */

typedef struct
{
    GtkWidget * window;
    QofSession *session;
    QofBook *book;

    GtkColumnView *commodity_view;
    GListStore *commodity_roots;
    GtkSortListModel *commodity_root_sort;
    GtkTreeListModel *commodity_tree_model;
    GtkSingleSelection *commodity_selection;
    GtkWidget *edit_button;
    GtkWidget *remove_button;
    gboolean show_currencies;
    GtkWidget *rename_namespace_button;
} CommoditiesDialog;

static void commodity_manager_rebuild (CommoditiesDialog *cd);
static void commodity_manager_select_commodity (CommoditiesDialog *cd,
                                                gnc_commodity *commodity);
static void commodity_manager_select_namespace (CommoditiesDialog *cd,
                                                const char *namespace_name);

enum CommodityManagerColumn
{
    COMMODITY_MANAGER_NAMESPACE,
    COMMODITY_MANAGER_SYMBOL,
    COMMODITY_MANAGER_DISPLAY_SYMBOL,
    COMMODITY_MANAGER_NAME,
    COMMODITY_MANAGER_PRINT_NAME,
    COMMODITY_MANAGER_UNIQUE_NAME,
    COMMODITY_MANAGER_CUSIP,
    COMMODITY_MANAGER_FRACTION,
    COMMODITY_MANAGER_QUOTE_FLAG,
    COMMODITY_MANAGER_QUOTE_SOURCE,
    COMMODITY_MANAGER_QUOTE_TIMEZONE,
    COMMODITY_MANAGER_NUM_COLUMNS
};

typedef struct
{
    gboolean is_namespace;
    GncGUID commodity_guid;
    gchar *namespace_name;
    gchar *values[COMMODITY_MANAGER_NUM_COLUMNS];
    guint fraction;
    gboolean quote_flag;
    GListStore *children;
    GtkSortListModel *sorted_children;
} CommodityManagerRow;

static GQuark commodity_manager_row_quark = 0;

static GQuark
commodity_manager_row_get_quark (void)
{
    if (G_UNLIKELY (!commodity_manager_row_quark))
        commodity_manager_row_quark = g_quark_from_static_string (
            "gnc-commodity-manager-row");
    return commodity_manager_row_quark;
}

static void
commodity_manager_row_free (gpointer data)
{
    auto row = static_cast<CommodityManagerRow *> (data);

    if (!row)
        return;
    for (auto& value : row->values)
        g_free (value);
    g_free (row->namespace_name);
    g_clear_object (&row->sorted_children);
    g_clear_object (&row->children);
    g_free (row);
}

static CommodityManagerRow *
commodity_manager_row_get (gpointer object)
{
    return object ? static_cast<CommodityManagerRow *> (
        g_object_get_qdata (G_OBJECT (object), commodity_manager_row_get_quark ())) : nullptr;
}

static CommodityManagerRow *
commodity_manager_tree_row_get (GtkTreeListRow *tree_row)
{
    auto item = tree_row ? gtk_tree_list_row_get_item (tree_row) : nullptr;
    auto row = commodity_manager_row_get (item);

    g_clear_object (&item);
    return row;
}

static void
commodity_manager_row_set_text (CommodityManagerRow *row, CommodityManagerColumn column,
                                const char *value)
{
    row->values[column] = g_strdup (value ? value : "");
}

static GObject *
commodity_manager_namespace_row_new (gnc_commodity_namespace *name_space)
{
    auto object = G_OBJECT (g_object_new (G_TYPE_OBJECT, nullptr));
    auto row = g_new0 (CommodityManagerRow, 1);

    row->is_namespace = TRUE;
    row->namespace_name = g_strdup (gnc_commodity_namespace_get_name (name_space));
    commodity_manager_row_set_text (row, COMMODITY_MANAGER_NAMESPACE,
                                    _(gnc_commodity_namespace_get_gui_name (name_space)));
    g_object_set_qdata_full (object, commodity_manager_row_get_quark (), row,
                             commodity_manager_row_free);
    return object;
}

static GObject *
commodity_manager_commodity_row_new (gnc_commodity *commodity)
{
    auto object = G_OBJECT (g_object_new (G_TYPE_OBJECT, nullptr));
    auto row = g_new0 (CommodityManagerRow, 1);
    const auto quote_flag = gnc_commodity_get_quote_flag (commodity);
    const auto quote_source = quote_flag ? gnc_commodity_get_quote_source (commodity) : nullptr;

    row->commodity_guid = *qof_instance_get_guid (QOF_INSTANCE (commodity));
    row->namespace_name = g_strdup (gnc_commodity_get_namespace (commodity));
    row->fraction = gnc_commodity_get_fraction (commodity);
    row->quote_flag = quote_flag;
    commodity_manager_row_set_text (row, COMMODITY_MANAGER_SYMBOL,
                                    gnc_commodity_get_mnemonic (commodity));
    commodity_manager_row_set_text (row, COMMODITY_MANAGER_DISPLAY_SYMBOL,
                                    gnc_commodity_get_nice_symbol (commodity));
    commodity_manager_row_set_text (row, COMMODITY_MANAGER_NAME,
                                    gnc_commodity_get_fullname (commodity));
    commodity_manager_row_set_text (row, COMMODITY_MANAGER_PRINT_NAME,
                                    gnc_commodity_get_printname (commodity));
    commodity_manager_row_set_text (row, COMMODITY_MANAGER_UNIQUE_NAME,
                                    gnc_commodity_get_unique_name (commodity));
    commodity_manager_row_set_text (row, COMMODITY_MANAGER_CUSIP,
                                    gnc_commodity_get_cusip (commodity));
    commodity_manager_row_set_text (row, COMMODITY_MANAGER_QUOTE_SOURCE,
                                    quote_source ? gnc_quote_source_get_internal_name (quote_source) : "");
    commodity_manager_row_set_text (row, COMMODITY_MANAGER_QUOTE_TIMEZONE,
                                    quote_flag ? gnc_commodity_get_quote_tz (commodity) : "");
    g_object_set_qdata_full (object, commodity_manager_row_get_quark (), row,
                             commodity_manager_row_free);
    return object;
}

static const char *
commodity_manager_row_text (const CommodityManagerRow *row, CommodityManagerColumn column)
{
    if (!row || column >= COMMODITY_MANAGER_NUM_COLUMNS || !row->values[column])
        return "";
    return row->values[column];
}
static gint
commodity_manager_row_compare (gconstpointer item1, gconstpointer item2, gpointer user_data)
{
    const auto column = static_cast<CommodityManagerColumn> (GPOINTER_TO_UINT (user_data));
    const auto row1 = commodity_manager_row_get (const_cast<gpointer> (item1));
    const auto row2 = commodity_manager_row_get (const_cast<gpointer> (item2));

    if ((row1 && row1->is_namespace) || (row2 && row2->is_namespace))
    {
        const auto comparison = g_utf8_collate (
            commodity_manager_row_text (row1, COMMODITY_MANAGER_NAMESPACE),
            commodity_manager_row_text (row2, COMMODITY_MANAGER_NAMESPACE));
        return comparison;
    }
    if (column == COMMODITY_MANAGER_FRACTION)
    {
        const auto fraction1 = row1 ? row1->fraction : 0;
        const auto fraction2 = row2 ? row2->fraction : 0;
        return (fraction1 > fraction2) - (fraction1 < fraction2);
    }
    if (column == COMMODITY_MANAGER_QUOTE_FLAG)
    {
        const auto quote1 = row1 && row1->quote_flag;
        const auto quote2 = row2 && row2->quote_flag;
        return (quote1 > quote2) - (quote1 < quote2);
    }

    const auto comparison = g_utf8_collate (commodity_manager_row_text (row1, column),
                                            commodity_manager_row_text (row2, column));
    return comparison;
}

static GListModel *
commodity_manager_create_children (gpointer item, gpointer user_data)
{
    const auto row = commodity_manager_row_get (item);

    (void)user_data;
    return row && row->sorted_children
        ? G_LIST_MODEL (g_object_ref (row->sorted_children)) : nullptr;
}

static CommodityManagerRow *
commodity_manager_list_item_row (GtkListItem *list_item)
{
    const auto tree_row = GTK_TREE_LIST_ROW (gtk_list_item_get_item (list_item));

    return commodity_manager_tree_row_get (tree_row);
}

static void
commodity_manager_text_item_setup (GtkListItemFactory *factory, GtkListItem *list_item,
                                   gpointer user_data)
{
    auto label = gtk_label_new (nullptr);

    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (list_item, label);
    (void)factory;
    (void)user_data;
}

static void
commodity_manager_text_item_bind (GtkListItemFactory *factory, GtkListItem *list_item,
                                  gpointer user_data)
{
    const auto column = static_cast<CommodityManagerColumn> (GPOINTER_TO_UINT (user_data));
    const auto row = commodity_manager_list_item_row (list_item);

    gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (list_item)),
                        commodity_manager_row_text (row, column));
    (void)factory;
}

static void
commodity_manager_namespace_item_setup (GtkListItemFactory *factory, GtkListItem *list_item,
                                        gpointer user_data)
{
    auto expander = GTK_TREE_EXPANDER (gtk_tree_expander_new ());
    auto label = gtk_label_new (nullptr);

    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_tree_expander_set_child (expander, label);
    gtk_list_item_set_child (list_item, GTK_WIDGET (expander));
    (void)factory;
    (void)user_data;
}

static void
commodity_manager_namespace_item_bind (GtkListItemFactory *factory, GtkListItem *list_item,
                                       gpointer user_data)
{
    const auto tree_row = GTK_TREE_LIST_ROW (gtk_list_item_get_item (list_item));
    const auto row = commodity_manager_tree_row_get (tree_row);
    const auto expander = GTK_TREE_EXPANDER (gtk_list_item_get_child (list_item));
    const auto label = GTK_LABEL (gtk_tree_expander_get_child (expander));

    gtk_tree_expander_set_list_row (expander, tree_row);
    gtk_label_set_text (label, commodity_manager_row_text (row, COMMODITY_MANAGER_NAMESPACE));
    (void)factory;
    (void)user_data;
}
static void
commodity_manager_quote_item_setup (GtkListItemFactory *factory, GtkListItem *list_item,
                                    gpointer user_data)
{
    auto button = gtk_check_button_new ();

    gtk_widget_set_halign (button, GTK_ALIGN_CENTER);
    gtk_widget_set_sensitive (button, FALSE);
    gtk_list_item_set_child (list_item, button);
    (void)factory;
    (void)user_data;
}

static void
commodity_manager_quote_item_bind (GtkListItemFactory *factory, GtkListItem *list_item,
                                   gpointer user_data)
{
    const auto row = commodity_manager_list_item_row (list_item);

    gtk_check_button_set_active (GTK_CHECK_BUTTON (gtk_list_item_get_child (list_item)),
                                 row && !row->is_namespace && row->quote_flag);
    (void)factory;
    (void)user_data;
}

static void
commodity_manager_fraction_item_bind (GtkListItemFactory *factory, GtkListItem *list_item,
                                      gpointer user_data)
{
    const auto row = commodity_manager_list_item_row (list_item);
    auto text = row && !row->is_namespace ? g_strdup_printf ("%u", row->fraction) : g_strdup ("");

    gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (list_item)), text);
    g_free (text);
    (void)factory;
    (void)user_data;
}

static GtkColumnViewColumn *
commodity_manager_column_new (const char *title, CommodityManagerColumn column, gboolean expand)
{
    auto factory = gtk_signal_list_item_factory_new ();
    auto sorter = GTK_SORTER (gtk_custom_sorter_new (commodity_manager_row_compare,
                                                     GUINT_TO_POINTER (column), nullptr));
    GtkColumnViewColumn *view_column;

    if (column == COMMODITY_MANAGER_NAMESPACE)
    {
        g_signal_connect (factory, "setup", G_CALLBACK (commodity_manager_namespace_item_setup), nullptr);
        g_signal_connect (factory, "bind", G_CALLBACK (commodity_manager_namespace_item_bind), nullptr);
    }
    else if (column == COMMODITY_MANAGER_QUOTE_FLAG)
    {
        g_signal_connect (factory, "setup", G_CALLBACK (commodity_manager_quote_item_setup), nullptr);
        g_signal_connect (factory, "bind", G_CALLBACK (commodity_manager_quote_item_bind), nullptr);
    }
    else
    {
        g_signal_connect (factory, "setup", G_CALLBACK (commodity_manager_text_item_setup), nullptr);
        g_signal_connect (factory, "bind", G_CALLBACK (
            column == COMMODITY_MANAGER_FRACTION ? commodity_manager_fraction_item_bind
                                                 : commodity_manager_text_item_bind),
                          GUINT_TO_POINTER (column));
    }

    view_column = gtk_column_view_column_new (title, GTK_LIST_ITEM_FACTORY (factory));
    gtk_column_view_column_set_resizable (view_column, TRUE);
    gtk_column_view_column_set_expand (view_column, expand);
    gtk_column_view_column_set_sorter (view_column, sorter);
    g_object_unref (sorter);
    return view_column;
}

static GtkColumnViewColumn *
commodity_manager_append_column (CommoditiesDialog *cd, const char *title,
                                 CommodityManagerColumn column, gboolean expand)
{
    auto view_column = commodity_manager_column_new (title, column, expand);

    gtk_column_view_append_column (cd->commodity_view, view_column);
    return view_column;
}

static void
commodity_manager_add_columns (CommoditiesDialog *cd)
{
    auto namespace_column = commodity_manager_append_column (cd, _("Namespace"),
                                                              COMMODITY_MANAGER_NAMESPACE, TRUE);
    auto symbol_column = commodity_manager_append_column (cd, _("Symbol"),
                                                           COMMODITY_MANAGER_SYMBOL, FALSE);
    auto display_symbol_column = commodity_manager_append_column (cd, _("Display symbol"),
                                                                   COMMODITY_MANAGER_DISPLAY_SYMBOL, FALSE);
    auto name_column = commodity_manager_append_column (cd, _("Name"),
                                                         COMMODITY_MANAGER_NAME, TRUE);
    auto print_name_column = commodity_manager_append_column (cd, _("Print Name"),
                                                               COMMODITY_MANAGER_PRINT_NAME, TRUE);
    auto unique_name_column = commodity_manager_append_column (cd, _("Unique Name"),
                                                                COMMODITY_MANAGER_UNIQUE_NAME, TRUE);
    auto cusip_column = commodity_manager_append_column (cd, _("ISIN/CUSIP"),
                                                          COMMODITY_MANAGER_CUSIP, FALSE);
    auto fraction_column = commodity_manager_append_column (cd, _("Fraction"),
                                                             COMMODITY_MANAGER_FRACTION, FALSE);
    auto quote_column = commodity_manager_append_column (cd, _("Get Quotes"),
                                                          COMMODITY_MANAGER_QUOTE_FLAG, FALSE);
    auto source_column = commodity_manager_append_column (cd, _("Source"),
                                                           COMMODITY_MANAGER_QUOTE_SOURCE, FALSE);
    auto timezone_column = commodity_manager_append_column (cd, _("Timezone"),
                                                             COMMODITY_MANAGER_QUOTE_TIMEZONE, FALSE);

    gtk_column_view_sort_by_column (cd->commodity_view, name_column, GTK_SORT_ASCENDING);
    g_object_unref (namespace_column);
    g_object_unref (symbol_column);
    g_object_unref (display_symbol_column);
    g_object_unref (name_column);
    g_object_unref (print_name_column);
    g_object_unref (unique_name_column);
    g_object_unref (cusip_column);
    g_object_unref (fraction_column);
    g_object_unref (quote_column);
    g_object_unref (source_column);
    g_object_unref (timezone_column);
}
static gboolean
commodity_manager_namespace_visible (CommoditiesDialog *cd, gnc_commodity_namespace *name_space)
{
    const auto name = gnc_commodity_namespace_get_name (name_space);

    return g_strcmp0 (name, GNC_COMMODITY_NS_TEMPLATE) != 0 &&
           (cd->show_currencies || !gnc_commodity_namespace_is_iso (name));
}

static void
commodity_manager_append_namespace (CommoditiesDialog *cd, GListStore *roots,
                                    gnc_commodity_namespace *name_space)
{
    auto commodities = gnc_commodity_namespace_get_commodity_list (name_space);
    auto children = g_list_store_new (G_TYPE_OBJECT);
    auto root = commodity_manager_namespace_row_new (name_space);
    auto root_row = commodity_manager_row_get (root);

    for (auto node = commodities; node; node = node->next)
    {
        auto commodity = static_cast<gnc_commodity *> (node->data);

        if (!cd->show_currencies && gnc_commodity_is_iso (commodity))
            continue;
        auto commodity_row = commodity_manager_commodity_row_new (commodity);
        g_list_store_append (children, commodity_row);
        g_object_unref (commodity_row);
    }
    g_list_free (commodities);

    if (g_list_model_get_n_items (G_LIST_MODEL (children)) == 0)
    {
        g_object_unref (children);
        g_object_unref (root);
        return;
    }

    root_row->children = children;
    root_row->sorted_children = gtk_sort_list_model_new (
        G_LIST_MODEL (g_object_ref (children)),
        g_object_ref (gtk_column_view_get_sorter (cd->commodity_view)));
    g_list_store_append (roots, root);
    g_object_unref (root);
}

static CommodityManagerRow *
commodity_manager_selected_row (CommoditiesDialog *cd)
{
    auto item = cd && cd->commodity_selection
        ? gtk_single_selection_get_selected_item (cd->commodity_selection) : nullptr;
    CommodityManagerRow *row = nullptr;

    if (item && GTK_IS_TREE_LIST_ROW (item))
        row = commodity_manager_tree_row_get (GTK_TREE_LIST_ROW (item));
    return row;
}

static gnc_commodity *
commodity_manager_selected_commodity (CommoditiesDialog *cd)
{
    auto row = commodity_manager_selected_row (cd);
    auto book = gnc_get_current_book ();
    auto commodity = row && !row->is_namespace && cd && book == cd->book
        ? gnc_commodity_find_commodity_by_guid (&row->commodity_guid, book) : nullptr;

    return commodity && !qof_instance_get_destroying (QOF_INSTANCE (commodity)) ? commodity : nullptr;
}

static void
commodity_manager_update_buttons (CommoditiesDialog *cd)
{
    const auto row = commodity_manager_selected_row (cd);
    const auto commodity = commodity_manager_selected_commodity (cd);
    const auto namespace_name = row && row->is_namespace ? row->namespace_name : nullptr;
    const auto can_rename = namespace_name &&
        g_strcmp0 (namespace_name, GNC_COMMODITY_NS_LEGACY) != 0 &&
        g_strcmp0 (namespace_name, GNC_COMMODITY_NS_CURRENCY) != 0;

    gtk_widget_set_sensitive (cd->edit_button, commodity != nullptr);
    gtk_widget_set_sensitive (cd->remove_button, commodity && !gnc_commodity_is_iso (commodity));
    gtk_widget_set_sensitive (cd->rename_namespace_button, can_rename);
}

static void
commodity_manager_selection_changed_cb (GtkSelectionModel *selection, guint position,
                                        guint n_items, CommoditiesDialog *cd)
{
    commodity_manager_update_buttons (cd);
    (void)selection;
    (void)position;
    (void)n_items;
}

struct CommodityManagerViewState
{
    gboolean has_commodity = FALSE;
    GncGUID commodity_guid {};
    std::string namespace_name;
    std::vector<std::string> expanded_namespaces;
};

static CommodityManagerViewState
commodity_manager_capture_view_state (CommoditiesDialog *cd)
{
    CommodityManagerViewState state;
    const auto selected = commodity_manager_selected_row (cd);

    if (selected)
    {
        state.has_commodity = !selected->is_namespace;
        state.commodity_guid = selected->commodity_guid;
        state.namespace_name = selected->namespace_name ? selected->namespace_name : "";
    }
    if (!cd || !cd->commodity_tree_model)
        return state;

    for (guint position = 0;
         position < g_list_model_get_n_items (G_LIST_MODEL (cd->commodity_tree_model));
         ++position)
    {
        auto tree_row = gtk_tree_list_model_get_row (cd->commodity_tree_model, position);
        auto row = commodity_manager_tree_row_get (tree_row);

        if (tree_row && gtk_tree_list_row_get_depth (tree_row) == 0 && row &&
            row->is_namespace && gtk_tree_list_row_get_expanded (tree_row))
            state.expanded_namespaces.emplace_back (row->namespace_name);
        g_clear_object (&tree_row);
    }
    return state;
}

static gboolean
commodity_manager_namespace_was_expanded (const CommodityManagerViewState& state,
                                          const char *namespace_name)
{
    return std::find (state.expanded_namespaces.cbegin (), state.expanded_namespaces.cend (),
                      namespace_name ? namespace_name : "") != state.expanded_namespaces.cend ();
}
static void
commodity_manager_apply_expansion (CommoditiesDialog *cd, const CommodityManagerViewState& state)
{
    for (guint position = 0;
         position < g_list_model_get_n_items (G_LIST_MODEL (cd->commodity_tree_model));
         ++position)
    {
        auto tree_row = gtk_tree_list_model_get_row (cd->commodity_tree_model, position);
        auto row = commodity_manager_tree_row_get (tree_row);

        if (tree_row && gtk_tree_list_row_get_depth (tree_row) == 0 && row && row->is_namespace)
            gtk_tree_list_row_set_expanded (tree_row,
                                            commodity_manager_namespace_was_expanded (
                                                state, row->namespace_name));
        g_clear_object (&tree_row);
    }
}

static void
commodity_manager_select_namespace (CommoditiesDialog *cd, const char *namespace_name)
{
    if (!cd || !cd->commodity_selection || !cd->commodity_tree_model)
        return;

    for (guint position = 0;
         position < g_list_model_get_n_items (G_LIST_MODEL (cd->commodity_tree_model));
         ++position)
    {
        auto tree_row = gtk_tree_list_model_get_row (cd->commodity_tree_model, position);
        auto row = commodity_manager_tree_row_get (tree_row);

        if (row && row->is_namespace && g_strcmp0 (row->namespace_name, namespace_name) == 0)
        {
            gtk_single_selection_set_selected (cd->commodity_selection, position);
            g_object_unref (tree_row);
            return;
        }
        g_clear_object (&tree_row);
    }
    gtk_single_selection_set_selected (cd->commodity_selection, GTK_INVALID_LIST_POSITION);
}

static void
commodity_manager_select_commodity (CommoditiesDialog *cd, gnc_commodity *commodity)
{
    const auto namespace_name = commodity ? gnc_commodity_get_namespace (commodity) : nullptr;
    GncGUID guid {};

    if (!cd || !commodity || !cd->commodity_selection || !cd->commodity_tree_model)
        return;
    guid = *qof_instance_get_guid (QOF_INSTANCE (commodity));

    for (guint position = 0;
         position < g_list_model_get_n_items (G_LIST_MODEL (cd->commodity_tree_model));
         ++position)
    {
        auto tree_row = gtk_tree_list_model_get_row (cd->commodity_tree_model, position);
        auto row = commodity_manager_tree_row_get (tree_row);

        if (row && row->is_namespace && g_strcmp0 (row->namespace_name, namespace_name) == 0)
        {
            gtk_tree_list_row_set_expanded (tree_row, TRUE);
            g_object_unref (tree_row);
            break;
        }
        g_clear_object (&tree_row);
    }

    for (guint position = 0;
         position < g_list_model_get_n_items (G_LIST_MODEL (cd->commodity_tree_model));
         ++position)
    {
        auto tree_row = gtk_tree_list_model_get_row (cd->commodity_tree_model, position);
        auto row = commodity_manager_tree_row_get (tree_row);

        if (row && !row->is_namespace && guid_equal (&row->commodity_guid, &guid))
        {
            gtk_single_selection_set_selected (cd->commodity_selection, position);
            g_object_unref (tree_row);
            return;
        }
        g_clear_object (&tree_row);
    }
    gtk_single_selection_set_selected (cd->commodity_selection, GTK_INVALID_LIST_POSITION);
}

static void
commodity_manager_restore_view_state (CommoditiesDialog *cd, const CommodityManagerViewState& state)
{
    commodity_manager_apply_expansion (cd, state);
    if (state.has_commodity)
    {
        auto book = gnc_get_current_book ();
        auto commodity = book == cd->book
            ? gnc_commodity_find_commodity_by_guid (&state.commodity_guid, book) : nullptr;

        commodity_manager_select_commodity (cd, commodity);
    }
    else if (!state.namespace_name.empty ())
        commodity_manager_select_namespace (cd, state.namespace_name.c_str ());
    else
        gtk_single_selection_set_selected (cd->commodity_selection, GTK_INVALID_LIST_POSITION);
}

static void
commodity_manager_clear_model (CommoditiesDialog *cd)
{
    if (!cd || !cd->commodity_view)
        return;
    gtk_column_view_set_model (cd->commodity_view, nullptr);
    g_clear_object (&cd->commodity_selection);
    g_clear_object (&cd->commodity_tree_model);
    g_clear_object (&cd->commodity_root_sort);
    g_clear_object (&cd->commodity_roots);
}

static void
commodity_manager_rebuild (CommoditiesDialog *cd)
{
    const auto state = commodity_manager_capture_view_state (cd);
    auto namespaces = cd && cd->book ? gnc_commodity_table_get_namespaces_list (
        gnc_commodity_table_get_table (cd->book)) : nullptr;

    commodity_manager_clear_model (cd);
    if (!cd || !cd->commodity_view)
    {
        g_list_free (namespaces);
        return;
    }

    cd->commodity_roots = g_list_store_new (G_TYPE_OBJECT);
    for (auto node = namespaces; node; node = node->next)
    {
        auto name_space = static_cast<gnc_commodity_namespace *> (node->data);

        if (commodity_manager_namespace_visible (cd, name_space))
            commodity_manager_append_namespace (cd, cd->commodity_roots, name_space);
    }
    g_list_free (namespaces);

    cd->commodity_root_sort = gtk_sort_list_model_new (
        G_LIST_MODEL (g_object_ref (cd->commodity_roots)),
        g_object_ref (gtk_column_view_get_sorter (cd->commodity_view)));
    cd->commodity_tree_model = gtk_tree_list_model_new (
        G_LIST_MODEL (g_object_ref (cd->commodity_root_sort)), FALSE, FALSE,
        commodity_manager_create_children, nullptr, nullptr);
    cd->commodity_selection = gtk_single_selection_new (
        G_LIST_MODEL (g_object_ref (cd->commodity_tree_model)));
    gtk_single_selection_set_autoselect (cd->commodity_selection, FALSE);
    gtk_single_selection_set_can_unselect (cd->commodity_selection, TRUE);
    gtk_column_view_set_model (cd->commodity_view,
                               GTK_SELECTION_MODEL (cd->commodity_selection));
    g_signal_connect (cd->commodity_selection, "selection-changed",
                      G_CALLBACK (commodity_manager_selection_changed_cb), cd);
    commodity_manager_restore_view_state (cd, state);
    commodity_manager_update_buttons (cd);
}
namespace
{
constexpr const char *RENAME_NAMESPACE_REQUEST_DATA = "gnc-rename-namespace-request";
constexpr const char *COMMODITIES_DIALOG_DATA = "gnc-commodities-dialog-data";

struct RenameNamespaceRequest
{
    GtkWindow *window;
    GtkEntry *entry;
    GtkLabel *label;
    GncGUID book_guid;
    gchar *old_name;
};

static void
rename_namespace_request_free (gpointer user_data)
{
    auto request = static_cast<RenameNamespaceRequest *> (user_data);

    g_free (request->old_name);
    g_free (request);
}

static gboolean
rename_namespace_request_matches_current_book (const RenameNamespaceRequest *request)
{
    auto book = gnc_get_current_book ();

    return book && guid_equal (qof_instance_get_guid (QOF_INSTANCE (book)),
                               &request->book_guid);
}

static void
rename_namespace_response_cb (GtkWindow *window, gint response, gpointer user_data)
{
    auto request = static_cast<RenameNamespaceRequest *> (user_data);

    if (response != GTK_RESPONSE_OK)
    {
        gtk_window_destroy (window);
        return;
    }

    if (!rename_namespace_request_matches_current_book (request))
    {
        gtk_window_destroy (window);
        return;
    }

    const auto new_name = gtk_editable_get_text (GTK_EDITABLE (request->entry));
    if (!new_name || !*new_name)
    {
        gtk_label_set_text (request->label, _("No new name"));
        return;
    }

    const auto commodity_table = gnc_get_current_commodities ();
    if (!gnc_commodity_table_rename_namespace (commodity_table, request->old_name,
                                                new_name))
    {
        gtk_label_set_text (request->label,
                            _("Rename failed, possibly new name exists"));
        return;
    }

    qof_book_mark_session_dirty (gnc_get_current_book ());
    auto parent = gtk_window_get_transient_for (window);
    auto commodities_dialog = parent ? static_cast<CommoditiesDialog *> (
        g_object_get_data (G_OBJECT (parent), COMMODITIES_DIALOG_DATA)) : nullptr;
    if (commodities_dialog)
    {
        commodity_manager_rebuild (commodities_dialog);
        commodity_manager_select_namespace (commodities_dialog, new_name);
    }
    gtk_window_destroy (window);
}

static void
rename_namespace_cancel_clicked_cb (GtkButton *, gpointer user_data)
{
    auto request = static_cast<RenameNamespaceRequest *> (user_data);
    rename_namespace_response_cb (request->window, GTK_RESPONSE_CANCEL, request);
}

static void
rename_namespace_confirm_clicked_cb (GtkButton *, gpointer user_data)
{
    auto request = static_cast<RenameNamespaceRequest *> (user_data);
    rename_namespace_response_cb (request->window, GTK_RESPONSE_OK, request);
}


constexpr const char *DELETE_COMMODITY_REQUEST_DATA = "gnc-delete-commodity-request";

struct DeleteCommodityRequest
{
    GtkWindow *dialog;
    GtkCheckButton *permanent;
    GtkCheckButton *temporary;
    GWeakRef parent;
    GncGUID book_guid;
    GncGUID commodity_guid;
    const gchar *warning;
    gboolean had_prices;
};

static void
commodity_delete_request_free (gpointer user_data)
{
    auto request = static_cast<DeleteCommodityRequest *> (user_data);

    g_weak_ref_clear (&request->parent);
    g_free (request);
}

static gboolean
commodity_delete_request_matches_current_book (const DeleteCommodityRequest *request)
{
    auto book = gnc_get_current_book ();

    return book && guid_equal (qof_instance_get_guid (QOF_INSTANCE (book)),
                               &request->book_guid);
}

static gboolean
commodity_is_used_by_account (QofBook *book, gnc_commodity *commodity)
{
    gboolean used = FALSE;

    gnc_account_foreach_descendant (gnc_book_get_root_account (book),
                                    [commodity, &used] (auto account)
                                    {
                                        if (commodity == xaccAccountGetCommodity (account))
                                            used = TRUE;
                                    });
    return used;
}

static gboolean
commodity_delete_request_complete (DeleteCommodityRequest *request)
{
    auto parent = GTK_WINDOW (g_weak_ref_get (&request->parent));
    CommoditiesDialog *commodities_dialog;

    if (!parent)
        return FALSE;

    commodities_dialog = static_cast<CommoditiesDialog *> (
        g_object_get_data (G_OBJECT (parent), COMMODITIES_DIALOG_DATA));
    if (!commodities_dialog ||
        !commodity_delete_request_matches_current_book (request))
    {
        g_object_unref (parent);
        return FALSE;
    }

    auto book = gnc_get_current_book ();
    auto commodity = gnc_commodity_find_commodity_by_guid (&request->commodity_guid, book);
    if (!commodity || qof_instance_get_destroying (QOF_INSTANCE (commodity)) ||
        gnc_commodity_is_iso (commodity))
    {
        g_object_unref (parent);
        return FALSE;
    }

    if (commodity_is_used_by_account (book, commodity))
    {
        gnc_warning_dialog (parent, "%s",
                            _("This commodity is now used by one or more accounts and may not be deleted."));
        g_object_unref (parent);
        return FALSE;
    }

    auto price_db = gnc_pricedb_get_db (book);
    auto prices = gnc_pricedb_get_prices (price_db, commodity, nullptr);
    if (!request->had_prices && prices)
    {
        gnc_price_list_destroy (prices);
        gnc_warning_dialog (parent, "%s",
                            _("This commodity acquired price quotes before deletion. Review it and try again."));
        g_object_unref (parent);
        return FALSE;
    }

    auto commodity_table = gnc_commodity_table_get_table (book);
    for (auto node = prices; node; node = node->next)
        gnc_pricedb_remove_price (price_db, GNC_PRICE (node->data));
    gnc_price_list_destroy (prices);

    gnc_commodity_table_remove (commodity_table, commodity);
    gnc_commodity_destroy (commodity);

    commodity_manager_rebuild (commodities_dialog);
    gnc_gui_refresh_all ();
    g_object_unref (parent);
    return TRUE;
}

static gboolean
commodity_delete_request_complete_idle (gpointer user_data)
{
    commodity_delete_request_complete (static_cast<DeleteCommodityRequest *> (user_data));
    return G_SOURCE_REMOVE;
}

static void
commodity_delete_response_cb (GtkWindow *dialog, gint response, gpointer user_data)
{
    auto request = static_cast<DeleteCommodityRequest *> (user_data);

    if (response == GTK_RESPONSE_OK)
    {
        const auto succeeded = commodity_delete_request_complete (request);
        if (succeeded && gtk_check_button_get_active (request->permanent))
            gnc_prefs_set_int (GNC_PREFS_GROUP_WARNINGS_PERM, request->warning,
                               GTK_RESPONSE_OK);
        else if (succeeded && gtk_check_button_get_active (request->temporary))
            gnc_prefs_set_int (GNC_PREFS_GROUP_WARNINGS_TEMP, request->warning,
                               GTK_RESPONSE_OK);
    }
    gtk_window_destroy (dialog);
}

static void
commodity_delete_cancel_clicked_cb (GtkButton *, gpointer user_data)
{
    auto request = static_cast<DeleteCommodityRequest *> (user_data);
    commodity_delete_response_cb (request->dialog, GTK_RESPONSE_CANCEL, request);
}

static void
commodity_delete_confirm_clicked_cb (GtkButton *, gpointer user_data)
{
    auto request = static_cast<DeleteCommodityRequest *> (user_data);
    commodity_delete_response_cb (request->dialog, GTK_RESPONSE_OK, request);
}

static void
commodity_delete_permanent_toggled_cb (GtkCheckButton *permanent, gpointer user_data)
{
    auto request = static_cast<DeleteCommodityRequest *> (user_data);
    const auto is_permanent = gtk_check_button_get_active (permanent);

    gtk_widget_set_sensitive (GTK_WIDGET (request->temporary), !is_permanent);
    if (is_permanent)
        gtk_check_button_set_active (request->temporary, FALSE);
}

static void
commodity_delete_confirm_async (CommoditiesDialog *cd, gnc_commodity *commodity,
                                gboolean had_prices, const gchar *message,
                                const gchar *warning)
{
    auto request = g_new0 (DeleteCommodityRequest, 1);
    request->book_guid = *qof_instance_get_guid (QOF_INSTANCE (cd->book));
    request->commodity_guid = *qof_instance_get_guid (QOF_INSTANCE (commodity));
    request->warning = warning;
    request->had_prices = had_prices;
    g_weak_ref_init (&request->parent, G_OBJECT (cd->window));

    auto remembered = gnc_prefs_get_int (GNC_PREFS_GROUP_WARNINGS_PERM, warning);
    if (!remembered)
        remembered = gnc_prefs_get_int (GNC_PREFS_GROUP_WARNINGS_TEMP, warning);
    if (remembered)
    {
        if (remembered == GTK_RESPONSE_OK)
            g_idle_add_full (G_PRIORITY_DEFAULT, commodity_delete_request_complete_idle,
                             request, commodity_delete_request_free);
        else
            commodity_delete_request_free (request);
        return;
    }

    auto dialog = GTK_WINDOW (gtk_window_new ());
    gnc_window_bind_to_application (dialog);
    auto content = gtk_box_new (GTK_ORIENTATION_VERTICAL, 12);
    auto detail = GTK_LABEL (gtk_label_new (message));
    auto permanent = GTK_CHECK_BUTTON (
        gtk_check_button_new_with_mnemonic (_("Remember and don't _ask me again.")));
    auto temporary = GTK_CHECK_BUTTON (
        gtk_check_button_new_with_mnemonic (_("Remember and don't ask me again this _session.")));
    auto actions = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    auto cancel = GTK_BUTTON (gtk_button_new_with_mnemonic (_("_Cancel")));
    auto confirm = GTK_BUTTON (gtk_button_new_with_mnemonic (_("_Delete")));

    request->dialog = dialog;
    request->permanent = permanent;
    request->temporary = temporary;
    gtk_window_set_title (dialog, _("Delete commodity?"));
    gtk_window_set_modal (dialog, TRUE);
    gtk_window_set_resizable (dialog, FALSE);
    gtk_window_set_transient_for (dialog, GTK_WINDOW (cd->window));
    gtk_widget_set_margin_top (content, 12);
    gtk_widget_set_margin_bottom (content, 12);
    gtk_widget_set_margin_start (content, 12);
    gtk_widget_set_margin_end (content, 12);
    gtk_label_set_wrap (detail, TRUE);
    gtk_widget_set_halign (actions, GTK_ALIGN_END);
    gtk_box_append (GTK_BOX (actions), GTK_WIDGET (cancel));
    gtk_box_append (GTK_BOX (actions), GTK_WIDGET (confirm));
    gtk_box_append (GTK_BOX (content), GTK_WIDGET (detail));
    gtk_box_append (GTK_BOX (content), GTK_WIDGET (permanent));
    gtk_box_append (GTK_BOX (content), GTK_WIDGET (temporary));
    gtk_box_append (GTK_BOX (content), actions);
    gtk_window_set_child (dialog, content);
    gtk_window_set_default_widget (dialog, GTK_WIDGET (cancel));

    g_object_set_data_full (G_OBJECT (dialog), DELETE_COMMODITY_REQUEST_DATA,
                            request, commodity_delete_request_free);
    g_signal_connect (cancel, "clicked", G_CALLBACK (commodity_delete_cancel_clicked_cb),
                      request);
    g_signal_connect (confirm, "clicked", G_CALLBACK (commodity_delete_confirm_clicked_cb),
                      request);
    g_signal_connect (permanent, "toggled",
                      G_CALLBACK (commodity_delete_permanent_toggled_cb), request);
    gtk_window_present (dialog);
}

struct CommodityDialogRequest
{
    GWeakRef parent;
    GncGUID book_guid;
    gboolean refresh;
};

static void
commodity_dialog_request_free (gpointer user_data)
{
    auto request = static_cast<CommodityDialogRequest *> (user_data);
    g_weak_ref_clear (&request->parent);
    g_free (request);
}

static void
commodity_dialog_operation_finished (gnc_commodity *commodity, gpointer user_data)
{
    auto request = static_cast<CommodityDialogRequest *> (user_data);
    auto parent = GTK_WINDOW (g_weak_ref_get (&request->parent));

    if (!parent)
    {
        commodity_dialog_request_free (request);
        return;
    }

    auto dialog = static_cast<CommoditiesDialog *> (
        g_object_get_data (G_OBJECT (parent), COMMODITIES_DIALOG_DATA));
    auto book = gnc_get_current_book ();
    if (dialog && commodity && book &&
        guid_equal (qof_instance_get_guid (QOF_INSTANCE (book)), &request->book_guid) &&
        !qof_instance_get_destroying (QOF_INSTANCE (commodity)))
    {
        auto commodity_guid = *qof_instance_get_guid (QOF_INSTANCE (commodity));
        auto current_commodity = gnc_commodity_find_commodity_by_guid (&commodity_guid, book);
        if (current_commodity)
        {
            commodity_manager_rebuild (dialog);
            commodity_manager_select_commodity (dialog, current_commodity);
            if (request->refresh)
                gnc_gui_refresh_all ();
        }
    }

    g_object_unref (parent);
    commodity_dialog_request_free (request);
}

static CommodityDialogRequest *
commodity_dialog_request_new (CommoditiesDialog *dialog, gboolean refresh)
{
    auto book = gnc_get_current_book ();
    if (!book)
        return nullptr;

    auto request = g_new0 (CommodityDialogRequest, 1);
    g_weak_ref_init (&request->parent, G_OBJECT (dialog->window));
    request->book_guid = *qof_instance_get_guid (QOF_INSTANCE (book));
    request->refresh = refresh;
    return request;
}

static void
commodity_dialog_edit_async (CommoditiesDialog *dialog, gnc_commodity *commodity)
{
    auto request = commodity_dialog_request_new (dialog, TRUE);
    if (!request)
        return;

    gnc_ui_edit_commodity_async (commodity, dialog->window, nullptr,
                                 commodity_dialog_operation_finished, request);
}

static void
commodity_dialog_add_async (CommoditiesDialog *dialog, const char *name_space)
{
    auto request = commodity_dialog_request_new (dialog, FALSE);
    if (!request)
        return;

    gnc_ui_new_commodity_async (name_space, dialog->window, nullptr,
                                commodity_dialog_operation_finished, request);
}
}

void gnc_commodities_window_destroy_cb (GtkWidget *object, CommoditiesDialog *cd);

extern "C" {
void gnc_commodities_dialog_add_clicked (GtkWidget *widget, gpointer data);
void gnc_commodities_dialog_edit_clicked (GtkWidget *widget, gpointer data);
void gnc_commodities_dialog_remove_clicked (GtkWidget *widget, gpointer data);
void gnc_commodities_dialog_close_clicked (GtkWidget *widget, gpointer data);

void gnc_commodities_dialog_rename_namespace_clicked (GtkWidget *widget, gpointer data);

void gnc_commodities_show_currencies_toggled (GtkCheckButton *toggle, CommoditiesDialog *cd);
}

static gboolean gnc_commodities_window_key_pressed_cb (GtkEventControllerKey *key,
                                                        guint keyval, guint keycode,
                                                        GdkModifierType state,
                                                        gpointer data);


void
gnc_commodities_window_destroy_cb (GtkWidget *object, CommoditiesDialog *cd)
{
    g_object_steal_data (G_OBJECT (object), COMMODITIES_DIALOG_DATA);
    gnc_unregister_gui_component_by_data (DIALOG_COMMODITIES_CM_CLASS, cd);
    commodity_manager_clear_model (cd);
    cd->window = nullptr;
    g_free (cd);
}
static gboolean
gnc_commodities_window_close_request_cb (GtkWindow *window, gpointer data)
{
    // This callback allows the window size to be saved on closing with the X.
    gnc_save_window_size (GNC_PREFS_GROUP, window);
    (void)data;
    return FALSE;
}

void
gnc_commodities_dialog_edit_clicked (GtkWidget *widget, gpointer data)
{
    auto cd = static_cast<CommoditiesDialog *> (data);
    auto commodity = commodity_manager_selected_commodity (cd);

    if (commodity)
        commodity_dialog_edit_async (cd, commodity);
    (void)widget;
}

static void
commodity_manager_row_activated_cb (GtkColumnView *view, guint position, CommoditiesDialog *cd)
{
    auto tree_row = cd && cd->commodity_tree_model
        ? gtk_tree_list_model_get_row (cd->commodity_tree_model, position) : nullptr;
    auto row = commodity_manager_tree_row_get (tree_row);

    if (row && row->is_namespace)
        gtk_tree_list_row_set_expanded (tree_row, !gtk_tree_list_row_get_expanded (tree_row));
    else if (row)
        gnc_commodities_dialog_edit_clicked (nullptr, cd);
    g_clear_object (&tree_row);
    (void)view;
}
void
gnc_commodities_dialog_remove_clicked (GtkWidget *widget, gpointer data)
{
    auto cd = static_cast<CommoditiesDialog *> (data);
    auto commodity = commodity_manager_selected_commodity (cd);

    if (!commodity)
        return;

    std::vector<Account *> commodity_accounts;
    gnc_account_foreach_descendant (gnc_book_get_root_account (cd->book),
                                    [commodity, &commodity_accounts] (auto account)
                                    {
                                        if (commodity == xaccAccountGetCommodity (account))
                                            commodity_accounts.push_back (account);
                                    });

    /* FIXME check for transaction references */
    if (!commodity_accounts.empty ())
    {
        std::string message {_("This commodity is currently used by the following accounts. You may "
                               "not delete it.\n")};
        for (const auto account : commodity_accounts)
        {
            auto full_name = gnc_account_get_full_name (account);
            message.append ("\n* ").append (full_name);
            g_free (full_name);
        }
        gnc_warning_dialog (GTK_WINDOW (cd->window), "%s", message.c_str ());
        return;
    }

    auto price_db = gnc_pricedb_get_db (cd->book);
    auto prices = gnc_pricedb_get_prices (price_db, commodity, nullptr);
    const auto had_prices = prices != nullptr;
    gnc_price_list_destroy (prices);

    commodity_delete_confirm_async (
        cd, commodity, had_prices,
        had_prices
            ? _("This commodity has price quotes. Are you sure you want to delete the selected "
                "commodity and its price quotes?")
            : _("Are you sure you want to delete the selected commodity?"),
        had_prices ? GNC_PREF_WARN_PRICE_COMM_DEL_QUOTES : GNC_PREF_WARN_PRICE_COMM_DEL);
    (void)widget;
}
void
gnc_commodities_dialog_add_clicked (GtkWidget *widget, gpointer data)
{
    auto cd = static_cast<CommoditiesDialog *> (data);
    auto commodity = commodity_manager_selected_commodity (cd);
    const auto name_space = commodity ? gnc_commodity_get_namespace (commodity) : nullptr;

    commodity_dialog_add_async (cd, name_space);
    (void)widget;
}
void
gnc_commodities_dialog_close_clicked (GtkWidget *widget, gpointer data)
{
    auto cd = static_cast<CommoditiesDialog*>(data);

    gnc_close_gui_component_by_data (DIALOG_COMMODITIES_CM_CLASS, cd);
    (void)widget;
}

void
gnc_commodities_dialog_rename_namespace_clicked (GtkWidget *widget, gpointer data)
{
    auto cd = static_cast<CommoditiesDialog *> (data);
    auto row = commodity_manager_selected_row (cd);
    const auto ns_name = row && row->is_namespace ? row->namespace_name : nullptr;

    if (!ns_name)
        return;

    auto dialog = GTK_WINDOW (gtk_window_new ());
    gnc_window_bind_to_application (dialog);
    auto content = gtk_box_new (GTK_ORIENTATION_VERTICAL, 12);
    auto form = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    auto label = GTK_LABEL (gtk_label_new_with_mnemonic (_("New _name:")));
    auto entry = GTK_ENTRY (gtk_entry_new ());
    auto feedback = GTK_LABEL (gtk_label_new (nullptr));
    auto actions = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    auto cancel = GTK_BUTTON (gtk_button_new_with_mnemonic (_("_Cancel")));
    auto confirm = GTK_BUTTON (gtk_button_new_with_mnemonic (_("_Rename")));
    auto request = g_new0 (RenameNamespaceRequest, 1);

    request->window = dialog;
    request->entry = entry;
    request->label = feedback;
    request->book_guid = *qof_instance_get_guid (QOF_INSTANCE (cd->book));
    request->old_name = g_strdup (ns_name);

    gtk_window_set_title (dialog, _("Rename Namespace"));
    gtk_window_set_modal (dialog, TRUE);
    gtk_window_set_resizable (dialog, FALSE);
    gtk_window_set_transient_for (dialog, GTK_WINDOW (cd->window));
    gtk_widget_set_name (GTK_WIDGET (dialog), "gnc-id-rename-namespace");
    gnc_widget_style_context_add_class (GTK_WIDGET (dialog), "gnc-class-securities");

    gtk_widget_set_margin_top (content, 12);
    gtk_widget_set_margin_bottom (content, 12);
    gtk_widget_set_margin_start (content, 12);
    gtk_widget_set_margin_end (content, 12);
    gtk_widget_set_hexpand (GTK_WIDGET (entry), TRUE);
    gtk_label_set_mnemonic_widget (label, GTK_WIDGET (entry));
    gtk_label_set_wrap (feedback, TRUE);
    gtk_widget_set_halign (actions, GTK_ALIGN_END);
    gtk_box_append (GTK_BOX (form), GTK_WIDGET (label));
    gtk_box_append (GTK_BOX (form), GTK_WIDGET (entry));
    gtk_box_append (GTK_BOX (actions), GTK_WIDGET (cancel));
    gtk_box_append (GTK_BOX (actions), GTK_WIDGET (confirm));
    gtk_box_append (GTK_BOX (content), form);
    gtk_box_append (GTK_BOX (content), GTK_WIDGET (feedback));
    gtk_box_append (GTK_BOX (content), actions);
    gtk_window_set_child (dialog, content);

    gtk_editable_set_text (GTK_EDITABLE (entry), ns_name);
    gtk_editable_select_region (GTK_EDITABLE (entry), 0, -1);
    gtk_entry_set_activates_default (entry, TRUE);
    gtk_window_set_default_widget (dialog, GTK_WIDGET (confirm));

    g_object_set_data_full (G_OBJECT (dialog), RENAME_NAMESPACE_REQUEST_DATA,
                            request, rename_namespace_request_free);
    g_signal_connect (cancel, "clicked", G_CALLBACK (rename_namespace_cancel_clicked_cb),
                      request);
    g_signal_connect (confirm, "clicked", G_CALLBACK (rename_namespace_confirm_clicked_cb),
                      request);
    gtk_window_present (dialog);
    (void)widget;
}

void
gnc_commodities_show_currencies_toggled (GtkCheckButton *toggle, CommoditiesDialog *cd)
{
    cd->show_currencies = gtk_check_button_get_active (toggle);
    commodity_manager_rebuild (cd);
}

static void
gnc_commodities_dialog_create (GtkWidget *parent, CommoditiesDialog *cd)
{
    GtkWidget *button;
    GtkBuilder *builder;

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-commodities.ui", "securities_window");

    cd->window = GTK_WIDGET (gtk_builder_get_object (builder, "securities_window"));
    g_object_set_data (G_OBJECT (cd->window), COMMODITIES_DIALOG_DATA, cd);
    cd->session = gnc_get_current_session ();
    cd->book = qof_session_get_book (cd->session);
    cd->show_currencies = gnc_prefs_get_bool (GNC_PREFS_GROUP, GNC_PREF_INCL_ISO);

    gtk_widget_set_name (cd->window, "gnc-id-commodity");
    gnc_widget_style_context_add_class (cd->window, "gnc-class-securities");

    cd->remove_button = GTK_WIDGET (gtk_builder_get_object (builder, "remove_button"));
    cd->edit_button = GTK_WIDGET (gtk_builder_get_object (builder, "edit_button"));
    cd->rename_namespace_button = GTK_WIDGET (gtk_builder_get_object (
        builder, "rename_namespace_button"));
    cd->commodity_view = GTK_COLUMN_VIEW (gtk_builder_get_object (builder, "commodity_view"));
    gtk_column_view_set_reorderable (cd->commodity_view, TRUE);
    commodity_manager_add_columns (cd);
    commodity_manager_rebuild (cd);
    g_signal_connect (cd->commodity_view, "activate",
                      G_CALLBACK (commodity_manager_row_activated_cb), cd);

    button = GTK_WIDGET (gtk_builder_get_object (builder, "show_currencies_button"));
    gtk_check_button_set_active (GTK_CHECK_BUTTON (button), cd->show_currencies);

    button = GTK_WIDGET (gtk_builder_get_object (builder, "close_button"));
    gtk_window_set_default_widget (GTK_WINDOW (cd->window), button);
    gtk_widget_grab_focus (button);

    g_signal_connect (cd->window, "destroy", G_CALLBACK (gnc_commodities_window_destroy_cb), cd);
    g_signal_connect (cd->window, "close-request",
                      G_CALLBACK (gnc_commodities_window_close_request_cb), cd);

    auto key_controller = gtk_event_controller_key_new ();
    gtk_widget_add_controller (cd->window, key_controller);
    g_signal_connect (key_controller, "key-pressed",
                      G_CALLBACK (gnc_commodities_window_key_pressed_cb), cd);

    gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func, cd);
    g_object_unref (builder);

    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW (cd->window), GTK_WINDOW (parent));
}
static void
close_handler (gpointer user_data)
{
    auto cd = static_cast<CommoditiesDialog*>(user_data);

    gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW(cd->window));

    gnc_prefs_set_bool (GNC_PREFS_GROUP, GNC_PREF_INCL_ISO, cd->show_currencies);

    gtk_window_destroy (GTK_WINDOW(cd->window));
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    auto cd = static_cast<CommoditiesDialog *> (user_data);

    g_return_if_fail (cd != nullptr);
    commodity_manager_rebuild (cd);
    (void)changes;
}
static gboolean
show_handler (const char *klass, gint component_id,
              gpointer user_data, gpointer iter_data)
{
    auto cd = static_cast<CommoditiesDialog*>(user_data);

    if (!cd)
        return(FALSE);
    gtk_window_present (GTK_WINDOW(cd->window));
    (void)klass;
    (void)component_id;
    (void)iter_data;
    return(TRUE);
}

static gboolean
gnc_commodities_window_key_pressed_cb (GtkEventControllerKey *key,
                                        guint keyval, guint keycode,
                                        GdkModifierType state, gpointer data)
{
    auto cd = static_cast<CommoditiesDialog*>(data);

    (void)key;
    (void)keycode;
    (void)state;
    if (keyval == GDK_KEY_Escape)
    {
        close_handler (cd);
        return TRUE;
    }
    return FALSE;
}

/********************************************************************\
 * gnc_commodities_dialog                                           *
 *   opens up a window to edit price information                    *
 *                                                                  *
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_commodities_dialog (GtkWidget * parent)
{
    gint component_id;

    if (gnc_forall_gui_components (DIALOG_COMMODITIES_CM_CLASS,
                                   show_handler, NULL))
        return;

    auto cd = static_cast<CommoditiesDialog*>(g_new0 (CommoditiesDialog, 1));

    gnc_commodities_dialog_create (parent, cd);

    component_id = gnc_register_gui_component (DIALOG_COMMODITIES_CM_CLASS,
                   refresh_handler, close_handler,
                   cd);
    gnc_gui_component_set_session (component_id, cd->session);

    gtk_widget_grab_focus (GTK_WIDGET (cd->commodity_view));

    gtk_window_present (GTK_WINDOW (cd->window));
}
