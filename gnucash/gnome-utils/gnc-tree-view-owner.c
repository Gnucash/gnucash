/*
 * Copyright (C) 2011 Geert Janssens <geert@kobaltwit.be>           *
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
 *                                                                  *
\********************************************************************/

/* GTK4 ColumnView implementation for business owners. */
#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-tree-model-owner.h"
#include "gnc-tree-view-owner.h"
#include "gnc-tree-view.h"
#include "dialog-utils.h"
#include "gncAddress.h"
#include "gncCustomer.h"
#include "gncVendor.h"
#include "gnc-ui-balances.h"
#include "gnc-commodity.h"
#include "gnc-session.h"
#include "gnc-ui-util.h"
#include "gnc-string-utils.h"

struct _GncTreeViewOwner
{
    GtkBox parent_instance;
    GtkColumnView *column_view;
    GncTreeModelOwner *owner_model;
    GtkCustomFilter *filter;
    GtkFilterListModel *filtered;
    GtkSortListModel *sorted;
    GtkSingleSelection *selection;
    gnc_tree_view_owner_filter_func filter_fn;
    gpointer filter_data;
    GDestroyNotify filter_destroy;
};

G_DEFINE_TYPE (GncTreeViewOwner, gnc_tree_view_owner, GTK_TYPE_BOX)

typedef enum
{
    OWNER_COL_NAME,
    OWNER_COL_TYPE,
    OWNER_COL_ID,
    OWNER_COL_CURRENCY,
    OWNER_COL_ADDRESS_NAME,
    OWNER_COL_ADDRESS_1,
    OWNER_COL_ADDRESS_2,
    OWNER_COL_ADDRESS_3,
    OWNER_COL_ADDRESS_4,
    OWNER_COL_PHONE,
    OWNER_COL_FAX,
    OWNER_COL_EMAIL,
    OWNER_COL_BALANCE,
    OWNER_COL_BALANCE_REPORT,
    OWNER_COL_NOTES,
    OWNER_COL_ACTIVE
} OwnerColumn;

static const gchar * const column_titles[] =
{
    N_("Name"), N_("Type"), N_("ID"), N_("Currency"), N_("Address Name"),
    N_("Address 1"), N_("Address 2"), N_("Address 3"), N_("Address 4"),
    N_("Phone"), N_("Fax"), N_("Email"), N_("Balance"), N_("Balance"), N_("Notes"),
    N_("Active")
};

static gchar *
owner_column_text (GncOwner *owner, OwnerColumn column)
{
    GncAddress *address;
    gnc_commodity *currency;

    if (!owner)
        return g_strdup ("");
    address = gncOwnerGetAddr (owner);
    switch (column)
    {
    case OWNER_COL_NAME: return g_strdup (gncOwnerGetName (owner));
    case OWNER_COL_TYPE: return g_strdup (_(qof_object_get_type_label (
                                      gncOwnerTypeToQofIdType (gncOwnerGetType (owner)))));
    case OWNER_COL_ID: return g_strdup (gncOwnerGetID (owner));
    case OWNER_COL_CURRENCY:
        currency = gncOwnerGetCurrency (owner);
        return g_strdup (currency ? gnc_commodity_get_fullname (currency) : "");
    case OWNER_COL_ADDRESS_NAME: return g_strdup (address ? gncAddressGetName (address) : "");
    case OWNER_COL_ADDRESS_1: return g_strdup (address ? gncAddressGetAddr1 (address) : "");
    case OWNER_COL_ADDRESS_2: return g_strdup (address ? gncAddressGetAddr2 (address) : "");
    case OWNER_COL_ADDRESS_3: return g_strdup (address ? gncAddressGetAddr3 (address) : "");
    case OWNER_COL_ADDRESS_4: return g_strdup (address ? gncAddressGetAddr4 (address) : "");
    case OWNER_COL_PHONE: return g_strdup (address ? gncAddressGetPhone (address) : "");
    case OWNER_COL_FAX: return g_strdup (address ? gncAddressGetFax (address) : "");
    case OWNER_COL_EMAIL: return g_strdup (address ? gncAddressGetEmail (address) : "");
    case OWNER_COL_BALANCE:
        return gnc_ui_owner_get_print_balance (owner, NULL);
    case OWNER_COL_BALANCE_REPORT:
        return gnc_ui_owner_get_print_report_balance (owner, NULL);
    case OWNER_COL_NOTES:
        switch (gncOwnerGetType (owner))
        {
        case GNC_OWNER_CUSTOMER:
            return g_strdup (gncCustomerGetNotes (gncOwnerGetCustomer (owner)));
        case GNC_OWNER_VENDOR:
            return g_strdup (gncVendorGetNotes (gncOwnerGetVendor (owner)));
        default:
            return g_strdup ("");
        }
    case OWNER_COL_ACTIVE: return g_strdup (gncOwnerGetActive (owner) ? _("Yes") : _("No"));
    }
    return g_strdup ("");
}

static gboolean
owner_filter_cb (gpointer item, gpointer user_data)
{
    GncTreeViewOwner *view = user_data;
    GncOwner *owner = gnc_tree_model_owner_get_row_owner (item);
    return !view->filter_fn || view->filter_fn (owner, view->filter_data);
}

static GtkOrdering
owner_sort_cb (gconstpointer a, gconstpointer b, gpointer user_data)
{
    OwnerColumn column = GPOINTER_TO_INT (user_data);
    GncOwner *left_owner = gnc_tree_model_owner_get_row_owner ((GObject *)a);
    GncOwner *right_owner = gnc_tree_model_owner_get_row_owner ((GObject *)b);
    gint result;

    if (column == OWNER_COL_BALANCE || column == OWNER_COL_BALANCE_REPORT)
    {
        const gnc_commodity *currency = column == OWNER_COL_BALANCE_REPORT
            ? gnc_default_report_currency () : NULL;
        result = gnc_numeric_compare (gnc_ui_owner_get_balance_full (left_owner, NULL, currency),
                                      gnc_ui_owner_get_balance_full (right_owner, NULL, currency));
    }
    else
    {
        gchar *left = owner_column_text (left_owner, column);
        gchar *right = owner_column_text (right_owner, column);
        result = column == OWNER_COL_ID
            ? safe_utf8_collate_natural (left, right)
            : g_utf8_collate (left, right);
        g_free (left);
        g_free (right);
    }
    if (result == 0)
        result = gncOwnerCompare (left_owner, right_owner);
    return result < 0 ? GTK_ORDERING_SMALLER : result > 0 ? GTK_ORDERING_LARGER : GTK_ORDERING_EQUAL;
}

static void
label_setup_cb (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    (void)factory;
    (void)user_data;
    gtk_list_item_set_child (item, gtk_label_new (NULL));
}

static void
label_bind_cb (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    OwnerColumn column = GPOINTER_TO_INT (user_data);
    GtkWidget *label = gtk_list_item_get_child (item);
    GncOwner *owner = gnc_tree_model_owner_get_row_owner (gtk_list_item_get_item (item));
    gchar *text;
    (void)factory;

    text = owner_column_text (owner, column);
    gtk_label_set_text (GTK_LABEL (label), text);
    gtk_label_set_xalign (GTK_LABEL (label),
                          (column == OWNER_COL_BALANCE || column == OWNER_COL_BALANCE_REPORT) ? 1.0f : 0.0f);
    if (column == OWNER_COL_BALANCE || column == OWNER_COL_BALANCE_REPORT)
    {
        gboolean negative = FALSE;
        gnc_ui_owner_get_balance_full (owner, &negative,
            column == OWNER_COL_BALANCE_REPORT ? gnc_default_report_currency () : NULL);
        if (negative)
            gtk_widget_add_css_class (label, "error");
        else
            gtk_widget_remove_css_class (label, "error");
    }
    g_free (text);
}

static void
active_toggled_cb (GtkCheckButton *button, gpointer user_data)
{
    GncOwner *owner = g_object_get_data (G_OBJECT (button), "owner-row-owner");
    (void)user_data;
    if (owner)
        gncOwnerSetActive (owner, gtk_check_button_get_active (button));
}

static void
active_setup_cb (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    GtkWidget *button = gtk_check_button_new ();
    (void)factory;
    (void)user_data;
    g_signal_connect (button, "toggled", G_CALLBACK (active_toggled_cb), NULL);
    gtk_list_item_set_child (item, button);
}

static void
active_bind_cb (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    GtkCheckButton *button = GTK_CHECK_BUTTON (gtk_list_item_get_child (item));
    GncOwner *owner = gnc_tree_model_owner_get_row_owner (gtk_list_item_get_item (item));
    (void)factory;
    (void)user_data;
    g_signal_handlers_block_by_func (button, active_toggled_cb, NULL);
    g_object_set_data (G_OBJECT (button), "owner-row-owner", owner);
    gtk_check_button_set_active (button, gncOwnerGetActive (owner));
    g_signal_handlers_unblock_by_func (button, active_toggled_cb, NULL);
}

static GtkColumnViewColumn *
append_column (GncTreeViewOwner *view, OwnerColumn column)
{
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();
    GtkColumnViewColumn *view_column;
    GtkCustomSorter *sorter = gtk_custom_sorter_new (owner_sort_cb,
                                                     GINT_TO_POINTER (column), NULL);
    g_autofree gchar *report_title = NULL;
    const gchar *title = _(column_titles[column]);

    if (column == OWNER_COL_BALANCE_REPORT)
    {
        gnc_commodity *currency = gnc_default_report_currency ();
        report_title = g_strdup_printf (_("Balance (%s)"),
                                        currency ? gnc_commodity_get_mnemonic (currency) : "");
        title = report_title;
    }

    if (column == OWNER_COL_ACTIVE)
    {
        g_signal_connect (factory, "setup", G_CALLBACK (active_setup_cb), NULL);
        g_signal_connect (factory, "bind", G_CALLBACK (active_bind_cb), NULL);
    }
    else
    {
        g_signal_connect (factory, "setup", G_CALLBACK (label_setup_cb), NULL);
        g_signal_connect (factory, "bind", G_CALLBACK (label_bind_cb),
                          GINT_TO_POINTER (column));
    }
    view_column = gtk_column_view_column_new (title,
                                              GTK_LIST_ITEM_FACTORY (factory));
    gtk_column_view_column_set_sorter (view_column, GTK_SORTER (sorter));
    gtk_column_view_append_column (view->column_view, view_column);
    g_object_unref (sorter);
    return view_column;
}

static void
gnc_tree_view_owner_dispose (GObject *object)
{
    GncTreeViewOwner *view = GNC_TREE_VIEW_OWNER (object);
    GDestroyNotify filter_destroy;
    gpointer filter_data;

    if (view->column_view)
    {
        gnc_column_view_unbind_grid_line_preferences (view->column_view);
        gtk_column_view_set_model (view->column_view, NULL);
        view->column_view = NULL;
    }
    if (view->filter)
        gtk_custom_filter_set_filter_func (view->filter, NULL, NULL, NULL);
    filter_destroy = g_steal_pointer (&view->filter_destroy);
    filter_data = g_steal_pointer (&view->filter_data);
    view->filter_fn = NULL;
    if (filter_destroy)
        filter_destroy (filter_data);
    g_clear_object (&view->selection);
    g_clear_object (&view->sorted);
    g_clear_object (&view->filtered);
    g_clear_object (&view->filter);
    g_clear_object (&view->owner_model);
    G_OBJECT_CLASS (gnc_tree_view_owner_parent_class)->dispose (object);
}

static void
gnc_tree_view_owner_class_init (GncTreeViewOwnerClass *klass)
{
    G_OBJECT_CLASS (klass)->dispose = gnc_tree_view_owner_dispose;
}

static void
gnc_tree_view_owner_init (GncTreeViewOwner *view)
{
    (void)view;
}

GtkWidget *
gnc_tree_view_owner_new (GncOwnerType owner_type)
{
    GncTreeViewOwner *view = g_object_new (GNC_TYPE_TREE_VIEW_OWNER, NULL);

    view->column_view = GTK_COLUMN_VIEW (gtk_column_view_new (NULL));
    gnc_column_view_bind_grid_line_preferences (view->column_view);
    gtk_widget_set_hexpand (GTK_WIDGET (view->column_view), TRUE);
    gtk_widget_set_vexpand (GTK_WIDGET (view->column_view), TRUE);
    gtk_box_append (GTK_BOX (view), GTK_WIDGET (view->column_view));

    view->owner_model = gnc_tree_model_owner_new (owner_type);
    view->filter = gtk_custom_filter_new (owner_filter_cb, view, NULL);
    view->filtered = gtk_filter_list_model_new (
        g_object_ref (gnc_tree_model_owner_get_model (view->owner_model)),
        GTK_FILTER (g_object_ref (view->filter)));
    view->sorted = gtk_sort_list_model_new (g_object_ref (G_LIST_MODEL (view->filtered)),
        g_object_ref (gtk_column_view_get_sorter (view->column_view)));
    view->selection = gtk_single_selection_new (g_object_ref (G_LIST_MODEL (view->sorted)));
    gtk_single_selection_set_autoselect (view->selection, FALSE);
    gtk_column_view_set_model (view->column_view, GTK_SELECTION_MODEL (view->selection));
    for (guint i = 0; i <= OWNER_COL_ACTIVE; i++)
    {
        GtkColumnViewColumn *column = append_column (view, (OwnerColumn)i);

        g_object_unref (column);
    }
    return GTK_WIDGET (view);
}

GtkSelectionModel *
gnc_tree_view_owner_get_selection_model (GncTreeViewOwner *view)
{
    g_return_val_if_fail (GNC_IS_TREE_VIEW_OWNER (view), NULL);
    return GTK_SELECTION_MODEL (view->selection);
}

GncOwner *
gnc_tree_view_owner_get_selected_owner (GncTreeViewOwner *view)
{
    GObject *row;
    guint position;

    g_return_val_if_fail (GNC_IS_TREE_VIEW_OWNER (view), NULL);
    position = gtk_single_selection_get_selected (view->selection);
    if (position == GTK_INVALID_LIST_POSITION)
        return NULL;
    row = g_list_model_get_item (G_LIST_MODEL (view->sorted), position);
    if (!row)
        return NULL;
    GncOwner *owner = gnc_tree_model_owner_get_row_owner (row);
    g_object_unref (row);
    return owner;
}

void
gnc_tree_view_owner_set_selected_owner (GncTreeViewOwner *view, GncOwner *owner)
{
    guint n_items;

    g_return_if_fail (GNC_IS_TREE_VIEW_OWNER (view));
    if (!owner)
    {
        gtk_single_selection_set_selected (view->selection, GTK_INVALID_LIST_POSITION);
        return;
    }
    n_items = g_list_model_get_n_items (G_LIST_MODEL (view->sorted));
    for (guint i = 0; i < n_items; i++)
    {
        GObject *row = g_list_model_get_item (G_LIST_MODEL (view->sorted), i);
        gboolean match = gncOwnerEqual (gnc_tree_model_owner_get_row_owner (row), owner);
        g_object_unref (row);
        if (match)
        {
            gtk_single_selection_set_selected (view->selection, i);
            gtk_column_view_scroll_to (view->column_view, i, NULL,
                                       GTK_LIST_SCROLL_FOCUS, NULL);
            return;
        }
    }
}

void
gnc_tree_view_owner_set_filter (GncTreeViewOwner *view,
                                gnc_tree_view_owner_filter_func func,
                                gpointer data, GDestroyNotify destroy)
{
    g_return_if_fail (GNC_IS_TREE_VIEW_OWNER (view));
    if (view->filter_destroy)
        view->filter_destroy (view->filter_data);
    view->filter_fn = func;
    view->filter_data = data;
    view->filter_destroy = destroy;
    gnc_tree_view_owner_refilter (view);
}

void
gnc_tree_view_owner_refilter (GncTreeViewOwner *view)
{
    g_return_if_fail (GNC_IS_TREE_VIEW_OWNER (view));
    gtk_filter_changed (GTK_FILTER (view->filter), GTK_FILTER_CHANGE_DIFFERENT);
}

gboolean
gnc_plugin_page_owner_tree_filter_owners (GncOwner *owner, gpointer user_data)
{
    OwnerFilterDialog *fd = user_data;
    if (!fd->show_inactive && !gncOwnerGetActive (owner))
        return FALSE;
    if (!fd->show_zero_total && gnc_numeric_zero_p (
            gncOwnerGetBalanceInCurrency (owner, NULL)))
        return FALSE;
    return TRUE;
}

void
gppot_filter_show_inactive_toggled_cb (GtkCheckButton *button, OwnerFilterDialog *fd)
{
    fd->show_inactive = !gtk_check_button_get_active (button);
    gnc_tree_view_owner_refilter (fd->tree_view);
}

void
gppot_filter_show_zero_toggled_cb (GtkCheckButton *button, OwnerFilterDialog *fd)
{
    fd->show_zero_total = gtk_check_button_get_active (button);
    gnc_tree_view_owner_refilter (fd->tree_view);
}

static void
owner_filter_dialog_destroy_cb (GtkWindow *window, OwnerFilterDialog *fd)
{
    if (fd->dialog == GTK_WIDGET (window))
        fd->dialog = NULL;
}

void
gppot_filter_apply_cb (GtkButton *button, OwnerFilterDialog *fd)
{
    (void)button;
    gtk_window_destroy (GTK_WINDOW (fd->dialog));
}

void
gppot_filter_cancel_cb (GtkButton *button, OwnerFilterDialog *fd)
{
    (void)button;
    fd->show_inactive = fd->original_show_inactive;
    fd->show_zero_total = fd->original_show_zero_total;
    gnc_tree_view_owner_refilter (fd->tree_view);
    gtk_window_destroy (GTK_WINDOW (fd->dialog));
}

void
owner_filter_dialog_create (OwnerFilterDialog *fd, GncPluginPage *page)
{
    GtkBuilder *builder;
    GtkWidget *button;
    gchar *title;

    if (fd->dialog)
    {
        gtk_window_present (GTK_WINDOW (fd->dialog));
        return;
    }
    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "gnc-tree-view-owner.glade", "filter_by_dialog");
    fd->dialog = GTK_WIDGET (gtk_builder_get_object (builder, "filter_by_dialog"));
    gtk_window_set_transient_for (GTK_WINDOW (fd->dialog),
                                  GTK_WINDOW (GNC_PLUGIN_PAGE (page)->window));
    title = g_strdup_printf (_("Filter %s by…"),
                             gnc_plugin_page_get_page_name (page));
    gtk_window_set_title (GTK_WINDOW (fd->dialog), title);
    g_free (title);
    fd->original_show_inactive = fd->show_inactive;
    fd->original_show_zero_total = fd->show_zero_total;
    button = GTK_WIDGET (gtk_builder_get_object (builder, "show_inactive"));
    gtk_check_button_set_active (GTK_CHECK_BUTTON (button), !fd->show_inactive);
    button = GTK_WIDGET (gtk_builder_get_object (builder, "show_zero"));
    gtk_check_button_set_active (GTK_CHECK_BUTTON (button), fd->show_zero_total);
    gnc_builder_connect_signals (builder, fd);
    g_signal_connect (fd->dialog, "destroy", G_CALLBACK (owner_filter_dialog_destroy_cb), fd);
    g_object_unref (builder);
    gtk_window_present (GTK_WINDOW (fd->dialog));
}

void
gnc_tree_view_owner_save (GncTreeViewOwner *view, OwnerFilterDialog *fd,
                          GKeyFile *key_file, const gchar *group_name)
{
    GncOwner *owner = gnc_tree_view_owner_get_selected_owner (view);
    g_key_file_set_boolean (key_file, group_name, "ShowInactive", fd->show_inactive);
    g_key_file_set_boolean (key_file, group_name, "ShowZeroTotal", fd->show_zero_total);
    if (owner)
    {
        gchar *guid = guid_to_string (gncOwnerGetGUID (owner));
        g_key_file_set_string (key_file, group_name, "SelectedOwner", guid);
        g_free (guid);
    }
}

void
gnc_tree_view_owner_restore (GncTreeViewOwner *view, OwnerFilterDialog *fd,
                             GKeyFile *key_file, const gchar *group_name,
                             GncOwnerType owner_type)
{
    GError *error = NULL;
    gchar *value;
    GncOwner owner;
    GncGUID guid;
    QofBook *book = qof_session_get_book (gnc_get_current_session ());

    fd->show_inactive = g_key_file_get_boolean (key_file, group_name, "ShowInactive", &error);
    if (error) { g_clear_error (&error); fd->show_inactive = TRUE; }
    fd->show_zero_total = g_key_file_get_boolean (key_file, group_name, "ShowZeroTotal", &error);
    if (error) { g_clear_error (&error); fd->show_zero_total = TRUE; }
    value = g_key_file_get_string (key_file, group_name, "SelectedOwner", NULL);
    if (value && string_to_guid (value, &guid) &&
        gncOwnerGetOwnerFromTypeGuid (book, &owner,
                                      gncOwnerTypeToQofIdType (owner_type), &guid))
        gnc_tree_view_owner_set_selected_owner (view, &owner);
    g_free (value);
    gnc_tree_view_owner_refilter (view);
}
