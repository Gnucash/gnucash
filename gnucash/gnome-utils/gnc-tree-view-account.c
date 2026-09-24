/**********************************************************************\
 * gnc-tree-view-account.c -- GTK4 account hierarchy view.            *
 * Copyright (C) 2003,2005,2006 David Hampton <hampton@employees.org> *
 *                                                                    *
 * This program is free software; you can redistribute it and/or      *
 * modify it under the terms of the GNU General Public License as     *
 * published by the Free Software Foundation; either version 2 of     *
 * the License, or (at your option) any later version.                *
 *                                                                    *
 * This program is distributed in the hope that it will be useful,    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *
 * GNU General Public License for more details.                       *
 *                                                                    *
 * You should have received a copy of the GNU General Public License  *
 * along with this program; if not, contact:                          *
 *                                                                    *
 * Free Software Foundation           Voice:  +1-617-542-5942         *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652         *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                     *
 *                                                                    *
\**********************************************************************/
#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-tree-view-account.h"
#include "gnc-tree-view.h"
#include "gnc-tree-model-account-types.h"
#include "Account.h"
#include "dialog-utils.h"
#include "gnc-engine.h"
#include "gnc-prefs.h"
#include "gnc-ui-util.h"
#include "gnc-ui-balances.h"
#include "qof.h"

#define FILTER_TREE_VIEW "filter_tree"
#define ACCT_COUNT "NumberOfOpenAccounts"
#define ACCT_OPEN "OpenAccount%d"
#define ACCT_SELECTED "SelectedAccount"
#define SHOW_HIDDEN "ShowHidden"
#define SHOW_ZERO "ShowZeroTotal"
#define SHOW_UNUSED "ShowUnused"
#define ACCT_TYPES "AccountTypes"
#define SHOW_HIDDEN_ACCOUNTS "Show_Hidden"
#define SHOW_ZERO_TOTALS "Show_ZeroTotal"
#define SHOW_UNUSED_ACCOUNTS "Show_Unused"
#define ACCOUNT_TYPES "Account_Types"

enum { ACCOUNT_ACTIVATED, LAST_SIGNAL };
static guint signals[LAST_SIGNAL];

typedef struct
{
    gatomicrefcount ref_count;
    GWeakRef view;
    gchar *name;
    GncTreeModelAccountColumn column;
    gboolean is_tree;
    gboolean is_toggle;
    gnc_tree_view_account_edited_func edited_cb;
} AccountColumn;

typedef struct
{
    GWeakRef view;
    gboolean disposed;
} AccountChildrenContext;

struct _GncTreeViewAccount
{
    GtkBox parent_instance;
    GncTreeModelAccount *account_model;
    GtkTreeListModel *rows;
    AccountChildrenContext *children_context;
    GtkSelectionModel *selection;
    GtkColumnView *column_view;
    GtkSorter *view_sorter;
    gulong view_sorter_changed_id;
    GtkSelectionMode selection_mode;
    GHashTable *selected;
    GHashTable *expanded;
    gboolean synchronizing;
    gboolean rebuilding;
    gboolean disposing;
    gboolean unrooting;
    guint restore_source;
    gchar *state_section;
    GFunc editing_started_cb;
    gpointer editing_started_data;
    GFunc editing_finished_cb;
    gpointer editing_finished_data;
    gnc_tree_view_account_selection_filter_func selection_filter;
    gpointer selection_filter_data;
    GDestroyNotify selection_filter_destroy;
    AccountViewInfo view_info;
    gboolean use_view_info;
    GPtrArray *column_sorters;
};

G_DEFINE_TYPE (GncTreeViewAccount, gnc_tree_view_account, GTK_TYPE_BOX)

static gchar *
account_guid (Account *account)
{
    gchar buffer[GUID_ENCODING_LENGTH + 1];
    guid_to_string_buff (xaccAccountGetGUID (account), buffer);
    return g_strdup (buffer);
}

static Account *
account_from_row (gpointer item)
{
    if (!item || !G_IS_OBJECT (item))
        return NULL;
    if (GNC_IS_ACCOUNT (item))
        return GNC_ACCOUNT (item);
    if (!GTK_IS_TREE_LIST_ROW (item))
        return NULL;
    gpointer account = gtk_tree_list_row_get_item (GTK_TREE_LIST_ROW (item));
    Account *result = (account && GNC_IS_ACCOUNT (account)) ? GNC_ACCOUNT (account) : NULL;
    g_clear_object (&account);
    return result;
}

static Account *
account_at (GncTreeViewAccount *view, guint position)
{
    gpointer item;
    Account *account;
    if (!view || view->disposing || !view->rows || position >= g_list_model_get_n_items (G_LIST_MODEL (view->rows)))
        return NULL;
    item = g_list_model_get_item (G_LIST_MODEL (view->rows), position);
    account = account_from_row (item);
    g_clear_object (&item);
    return account;
}

static GListModel *
create_children_cb (gpointer item, gpointer user_data)
{
    AccountChildrenContext *context = user_data;
    GncTreeViewAccount *view;
    Account *account = GNC_ACCOUNT (item);

    if (context->disposed)
        return NULL;
    view = g_weak_ref_get (&context->view);
    if (!view)
        return NULL;
    if (!GNC_IS_TREE_VIEW_ACCOUNT (view) || view->disposing)
    {
        g_object_unref (view);
        return NULL;
    }
    if (gnc_account_n_children (account) == 0)
    {
        g_object_unref (view);
        return NULL;
    }
    GListModel *children = gnc_tree_model_account_create_children (view->account_model, account);
    g_object_unref (view);
    return children;
}

static void
account_children_context_free (AccountChildrenContext *context)
{
    g_weak_ref_clear (&context->view);
    g_free (context);
}

static void
account_column_free (AccountColumn *column)
{
    g_weak_ref_clear (&column->view);
    g_free (column->name);
    g_free (column);
}

static AccountColumn *
account_column_ref (AccountColumn *column)
{
    g_atomic_ref_count_inc (&column->ref_count);
    return column;
}

static void
account_column_unref (AccountColumn *column)
{
    if (g_atomic_ref_count_dec (&column->ref_count))
        account_column_free (column);
}

static void
account_column_closure_free (gpointer data, GClosure *closure)
{
    account_column_unref (data);
    (void)closure;
}

static GncTreeViewAccount *
account_column_get_view (AccountColumn *column)
{
    if (!column)
        return NULL;
    GncTreeViewAccount *view = g_weak_ref_get (&column->view);
    if (!view || !GNC_IS_TREE_VIEW_ACCOUNT (view) || view->disposing)
    {
        g_clear_object (&view);
        return NULL;
    }
    return view;
}

static void
mark_selected (GncTreeViewAccount *view, Account *account, gboolean selected)
{
    gchar *guid = account_guid (account);
    if (selected)
        g_hash_table_add (view->selected, guid);
    else
    {
        g_hash_table_remove (view->selected, guid);
    }
}

static void
mark_expanded (GncTreeViewAccount *view, Account *account, gboolean expanded)
{
    gchar *guid = account_guid (account);
    if (expanded)
        g_hash_table_add (view->expanded, guid);
    else
    {
        g_hash_table_remove (view->expanded, guid);
    }
}

static gboolean
has_guid (GHashTable *set, Account *account)
{
    gchar *guid = account_guid (account);
    gboolean result = g_hash_table_contains (set, guid);
    g_free (guid);
    return result;
}

static void
row_expanded_changed (GtkTreeListRow *row, GParamSpec *pspec,
                      GncTreeViewAccount *view)
{
    if (!view || view->disposing || view->synchronizing || view->rebuilding)
        return;
    Account *account = account_from_row (row);
    if (account)
        mark_expanded (view, account, gtk_tree_list_row_get_expanded (row));
    (void)pspec;
}

static void
editing_changed (GtkEditableLabel *label, GParamSpec *pspec,
                 AccountColumn *column)
{
    GncTreeViewAccount *view = account_column_get_view (column);
    Account *account = g_object_get_data (G_OBJECT (label), "gnc-account");
    gboolean editing = gtk_editable_label_get_editing (label);
    gboolean was_editing = GPOINTER_TO_INT (g_object_get_data (
        G_OBJECT (label), "gnc-account-was-editing"));

    if (!view || view->disposing)
        goto out;
    if (editing)
    {
        g_object_set_data (G_OBJECT (label), "gnc-account-was-editing",
                           GINT_TO_POINTER (TRUE));
        if (view->editing_started_cb)
            view->editing_started_cb (label, view->editing_started_data);
    }
    else if (was_editing)
    {
        g_object_set_data (G_OBJECT (label), "gnc-account-was-editing", NULL);
        if (account && column->edited_cb)
            column->edited_cb (account, column, gtk_editable_get_text (GTK_EDITABLE (label)));
        if (view->disposing)
            goto out;
        if (view->editing_finished_cb)
            view->editing_finished_cb (label, view->editing_finished_data);
    }
out:
    g_clear_object (&view);
    (void)pspec;
}

static void
toggle_changed (GtkCheckButton *button, AccountColumn *column)
{
    GncTreeViewAccount *view = account_column_get_view (column);
    Account *account = g_object_get_data (G_OBJECT (button), "gnc-account");
    gboolean value;
    if (!view || view->disposing || !account || view->synchronizing)
    {
        g_clear_object (&view);
        return;
    }
    value = gtk_check_button_get_active (button);
    switch (column->column)
    {
    case GNC_TREE_MODEL_ACCOUNT_COL_HIDDEN: xaccAccountSetHidden (account, value); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER: xaccAccountSetPlaceholder (account, value); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_OPENING_BALANCE: xaccAccountSetIsOpeningBalance (account, value); break;
    default: break;
    }
    g_object_unref (view);
}

static void
cell_setup (GtkSignalListItemFactory *factory, GtkListItem *list_item,
            AccountColumn *column)
{
    GtkWidget *widget;
    if (column->is_toggle)
    {
        widget = gtk_check_button_new ();
        gtk_widget_set_halign (widget, GTK_ALIGN_CENTER);
        g_signal_connect_data (widget, "toggled", G_CALLBACK (toggle_changed),
                               account_column_ref (column),
                               account_column_closure_free, 0);
    }
    else
    {
        GtkWidget *label = gtk_editable_label_new ("");
        gtk_editable_set_editable (GTK_EDITABLE (label), column->edited_cb != NULL);
        gtk_editable_set_width_chars (GTK_EDITABLE (label), 1);
        gtk_widget_set_halign (label, column->is_tree ? GTK_ALIGN_FILL : GTK_ALIGN_END);
        if (column->is_tree)
        {
            GtkWidget *expander = gtk_tree_expander_new ();
            gtk_tree_expander_set_child (GTK_TREE_EXPANDER (expander), label);
            widget = expander;
        }
        else
            widget = label;
        g_signal_connect_data (label, "notify::editing", G_CALLBACK (editing_changed),
                               account_column_ref (column),
                               account_column_closure_free, 0);
    }
    gtk_list_item_set_child (list_item, widget);
    (void)factory;
}

static GtkEditableLabel *
cell_label (GtkListItem *list_item, AccountColumn *column)
{
    GtkWidget *widget = list_item ? gtk_list_item_get_child (list_item) : NULL;
    if (!widget)
        return NULL;
    if (column->is_tree)
    {
        if (!GTK_IS_TREE_EXPANDER (widget))
            return NULL;
        widget = gtk_tree_expander_get_child (GTK_TREE_EXPANDER (widget));
    }
    return GTK_IS_EDITABLE_LABEL (widget) ? GTK_EDITABLE_LABEL (widget) : NULL;
}

static void
cell_bind (GtkSignalListItemFactory *factory, GtkListItem *list_item,
           AccountColumn *column)
{
    GncTreeViewAccount *view = account_column_get_view (column);
    GtkTreeListRow *row = list_item ? GTK_TREE_LIST_ROW (gtk_list_item_get_item (list_item)) : NULL;
    Account *account = account_from_row (row);
    GtkWidget *widget = list_item ? gtk_list_item_get_child (list_item) : NULL;

    if (!view || view->disposing || !row || !widget)
    {
        g_clear_object (&view);
        return;
    }
    if (column->is_toggle)
    {
        if (!GTK_IS_CHECK_BUTTON (widget))
        {
            g_object_unref (view);
            return;
        }
        GtkCheckButton *button = GTK_CHECK_BUTTON (widget);
        view->synchronizing = TRUE;
        gtk_check_button_set_active (button,
            account && gnc_tree_model_account_get_boolean (account, column->column));
        view->synchronizing = FALSE;
        g_object_set_data (G_OBJECT (button), "gnc-account", account);
    }
    else
    {
        GtkEditableLabel *label = cell_label (list_item, column);
        if (!label)
        {
            g_object_unref (view);
            return;
        }
        gboolean negative = FALSE;
        gchar *text = account ? gnc_tree_model_account_get_string (
            view->account_model, account, column->column, &negative) : g_strdup ("");
        gtk_editable_set_text (GTK_EDITABLE (label), text);
        if (negative && gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                                            GNC_PREF_NEGATIVE_IN_RED))
            gtk_widget_add_css_class (GTK_WIDGET (label), "gnc-class-negative-numbers");
        else
            gtk_widget_remove_css_class (GTK_WIDGET (label), "gnc-class-negative-numbers");
        if (column->column == GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_LIMIT && account)
        {
            gchar *tooltip = gnc_ui_account_get_balance_limit_explanation (account);
            gtk_widget_set_tooltip_text (GTK_WIDGET (label), tooltip);
            g_free (tooltip);
        }
        else
            gtk_widget_set_tooltip_text (GTK_WIDGET (label), NULL);
        g_free (text);
        g_object_set_data (G_OBJECT (label), "gnc-account", account);
        if (column->is_tree && GTK_IS_TREE_EXPANDER (widget))
            gtk_tree_expander_set_list_row (GTK_TREE_EXPANDER (widget), row);
    }
    if (!g_object_get_data (G_OBJECT (row), "gnc-account-expansion-listener"))
    {
        g_signal_connect_object (row, "notify::expanded",
                                 G_CALLBACK (row_expanded_changed), view, 0);
        g_object_set_data (G_OBJECT (row), "gnc-account-expansion-listener",
                           GINT_TO_POINTER (TRUE));
    }
    g_object_unref (view);
    (void)factory;
}

static void
cell_unbind (GtkSignalListItemFactory *factory, GtkListItem *list_item,
             AccountColumn *column)
{
    GtkWidget *widget = list_item ? gtk_list_item_get_child (list_item) : NULL;
    if (!widget || !G_IS_OBJECT (widget))
        return;
    if (column->is_toggle)
    {
        if (GTK_IS_CHECK_BUTTON (widget))
            g_object_set_data (G_OBJECT (widget), "gnc-account", NULL);
    }
    else
    {
        GtkEditableLabel *label = cell_label (list_item, column);
        if (label && G_IS_OBJECT (label))
            g_object_set_data (G_OBJECT (label), "gnc-account", NULL);
    }
    (void)factory;
}
static gint
account_column_sort_cb (gconstpointer first, gconstpointer second, gpointer user_data)
{
    AccountColumn *column = user_data;
    GncTreeViewAccount *view = account_column_get_view (column);
    gchar *left_text;
    gchar *right_text;
    gint result;
    if (!view || view->disposing)
    {
        g_clear_object (&view);
        return GTK_ORDERING_EQUAL;
    }
    Account *left = account_from_row ((gpointer) first);
    Account *right = account_from_row ((gpointer) second);
    if (!left || !right)
    {
        gint res = left ? 1 : (right ? -1 : 0);
        g_object_unref (view);
        return res;
    }
    left_text = gnc_tree_model_account_get_string (view->account_model, left,
                                                    column->column, NULL);
    right_text = gnc_tree_model_account_get_string (view->account_model, right,
                                                     column->column, NULL);
    result = g_utf8_collate (left_text, right_text);
    g_free (left_text);
    g_free (right_text);
    g_object_unref (view);
    return result;
}
static void
column_sort_changed (GtkSorter *sorter, GtkSorterChange change,
                     GncTreeViewAccount *view)
{
    GtkColumnViewColumn *column;
    AccountColumn *data;
    GtkSortType order;

    g_object_ref (view);
    if (!view || !GNC_IS_TREE_VIEW_ACCOUNT (view) || view->disposing || view->unrooting || view->synchronizing)
        goto cleanup;
    column = gtk_column_view_sorter_get_primary_sort_column
        (GTK_COLUMN_VIEW_SORTER (sorter));
    if (!column)
        goto cleanup;
    data = g_object_get_data (G_OBJECT (column), "gnc-account-column");
    if (!data)
        goto cleanup;
    order = gtk_column_view_sorter_get_primary_sort_order
        (GTK_COLUMN_VIEW_SORTER (sorter));
    view->synchronizing = TRUE;
    gnc_tree_model_account_set_sort_column (view->account_model,
                                            data->column, order);
    view->synchronizing = FALSE;
cleanup:
    g_object_unref (view);
    (void)change;
}

static GtkColumnViewColumn *
add_column (GncTreeViewAccount *view, const gchar *title, const gchar *name,
            GncTreeModelAccountColumn value_column, gboolean tree,
            gboolean toggle, gboolean visible)
{
    AccountColumn *data = g_new0 (AccountColumn, 1);
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();
    GtkColumnViewColumn *column;
    GtkSorter *sorter;

    g_atomic_ref_count_init (&data->ref_count);
    g_weak_ref_init (&data->view, view);
    data->name = g_strdup (name);
    data->column = value_column;
    data->is_tree = tree;
    data->is_toggle = toggle;
    sorter = GTK_SORTER (gtk_custom_sorter_new (account_column_sort_cb,
                                                account_column_ref (data),
                                                (GDestroyNotify)account_column_unref));
    g_signal_connect_data (factory, "setup", G_CALLBACK (cell_setup),
                           account_column_ref (data), account_column_closure_free, 0);
    g_signal_connect_data (factory, "bind", G_CALLBACK (cell_bind),
                           account_column_ref (data), account_column_closure_free, 0);
    g_signal_connect_data (factory, "unbind", G_CALLBACK (cell_unbind),
                           account_column_ref (data), account_column_closure_free, 0);
    column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_id (column, name);
    gtk_column_view_column_set_visible (column, visible);
    gtk_column_view_column_set_resizable (column, TRUE);
    gtk_column_view_column_set_expand (column, tree);
    gtk_column_view_column_set_sorter (column, sorter);
    g_object_set_data_full (G_OBJECT (column), "gnc-account-column",
                            account_column_ref (data),
                            (GDestroyNotify)account_column_unref);
    gtk_column_view_append_column (view->column_view, column);
    g_object_unref (sorter);
    account_column_unref (data);
    return column;
}

static void
add_default_columns (GncTreeViewAccount *view)
{
    GtkColumnViewColumn *default_column = NULL;

#define COLUMN(t, n, c, tree, toggle, shown) \
    do { \
        GtkColumnViewColumn *_c = add_column (view, t, n, c, tree, toggle, shown); \
        if (_c) { \
            if (!default_column) default_column = _c; \
            else g_object_unref (_c); \
        } \
    } while (0)

    COLUMN (_("Account Name"), "name", GNC_TREE_MODEL_ACCOUNT_COL_NAME, TRUE, FALSE, TRUE);
    COLUMN (_("Type"), "type", GNC_TREE_MODEL_ACCOUNT_COL_TYPE, FALSE, FALSE, FALSE);
    COLUMN (_("Commodity"), "commodity", GNC_TREE_MODEL_ACCOUNT_COL_COMMODITY, FALSE, FALSE, FALSE);
    COLUMN (_("Account Code"), "account-code", GNC_TREE_MODEL_ACCOUNT_COL_CODE, FALSE, FALSE, FALSE);
    COLUMN (_("Description"), "description", GNC_TREE_MODEL_ACCOUNT_COL_DESCRIPTION, FALSE, FALSE, FALSE);
    COLUMN (_("Last Num"), "lastnum", GNC_TREE_MODEL_ACCOUNT_COL_LASTNUM, FALSE, FALSE, FALSE);
    COLUMN (_("Present"), "present", GNC_TREE_MODEL_ACCOUNT_COL_PRESENT, FALSE, FALSE, FALSE);
    COLUMN (_("Present (Report)"), "present-report", GNC_TREE_MODEL_ACCOUNT_COL_PRESENT_REPORT, FALSE, FALSE, FALSE);
    COLUMN (_("Balance"), "balance", GNC_TREE_MODEL_ACCOUNT_COL_BALANCE, FALSE, FALSE, FALSE);
    COLUMN (_("Balance (Report)"), "balance-report", GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_REPORT, FALSE, FALSE, FALSE);
    COLUMN (_("Balance (Period)"), "balance-period", GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_PERIOD, FALSE, FALSE, FALSE);
    COLUMN (C_("Column header for 'Balance Limit'", "L"), "account-balance-limit", GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_LIMIT, FALSE, FALSE, FALSE);
    COLUMN (_("Cleared"), "cleared", GNC_TREE_MODEL_ACCOUNT_COL_CLEARED, FALSE, FALSE, FALSE);
    COLUMN (_("Cleared (Report)"), "cleared-report", GNC_TREE_MODEL_ACCOUNT_COL_CLEARED_REPORT, FALSE, FALSE, FALSE);
    COLUMN (_("Reconciled"), "reconciled", GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED, FALSE, FALSE, FALSE);
    COLUMN (_("Reconciled (Report)"), "reconciled-report", GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED_REPORT, FALSE, FALSE, FALSE);
    COLUMN (_("Earliest Date"), "earliest-date", GNC_TREE_MODEL_ACCOUNT_COL_EARLIEST_DATE, FALSE, FALSE, FALSE);
    COLUMN (_("Last Reconcile Date"), "last-recon-date", GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED_DATE, FALSE, FALSE, FALSE);
    COLUMN (_("Future Minimum"), "future-min", GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN, FALSE, FALSE, FALSE);
    COLUMN (_("Future Minimum (Report)"), "future-min-report", GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN_REPORT, FALSE, FALSE, FALSE);
    COLUMN (_("Total"), "total", GNC_TREE_MODEL_ACCOUNT_COL_TOTAL, FALSE, FALSE, FALSE);
    COLUMN (_("Total (Report)"), "total-report", GNC_TREE_MODEL_ACCOUNT_COL_TOTAL_REPORT, FALSE, FALSE, FALSE);
    COLUMN (_("Total (Period)"), "total-period", GNC_TREE_MODEL_ACCOUNT_COL_TOTAL_PERIOD, FALSE, FALSE, FALSE);
    COLUMN (C_("Column header for 'Color'", "C"), "account-color", GNC_TREE_MODEL_ACCOUNT_COL_COLOR_ACCOUNT, FALSE, FALSE, FALSE);
    COLUMN (_("Notes"), "notes", GNC_TREE_MODEL_ACCOUNT_COL_NOTES, FALSE, FALSE, FALSE);
    COLUMN (_("Tax Info"), "tax-info", GNC_TREE_MODEL_ACCOUNT_COL_TAX_INFO, FALSE, FALSE, FALSE);
    COLUMN (C_("Column header for 'Hidden'", "H"), "hidden", GNC_TREE_MODEL_ACCOUNT_COL_HIDDEN, FALSE, TRUE, FALSE);
    COLUMN (C_("Column header for 'Placeholder'", "P"), "placeholder", GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER, FALSE, TRUE, FALSE);
    COLUMN (C_("Column header for 'Opening Balance'", "O"), "opening-balance", GNC_TREE_MODEL_ACCOUNT_COL_OPENING_BALANCE, FALSE, TRUE, FALSE);
#undef COLUMN
    if (default_column)
    {
        gtk_column_view_sort_by_column (view->column_view, default_column, GTK_SORT_ASCENDING);
        g_object_unref (default_column);
    }
    GtkSorter *sorter = gtk_column_view_get_sorter (view->column_view);
    view->view_sorter = sorter;
    if (view->view_sorter)
        view->view_sorter_changed_id = g_signal_connect
            (view->view_sorter, "changed", G_CALLBACK (column_sort_changed), view);
}

static gboolean
restore_state_cb (gpointer user_data)
{
    GncTreeViewAccount *view = GNC_TREE_VIEW_ACCOUNT (user_data);
    GtkTreeListModel *rows;
    GtkSelectionModel *selection;
    GHashTable *selected;
    GHashTable *expanded;
    guint count;

    view->restore_source = 0;
    if (view->disposing || !view->rows || !view->selection ||
        !view->selected || !view->expanded)
        return G_SOURCE_REMOVE;
    rows = g_object_ref (view->rows);
    selection = g_object_ref (view->selection);
    selected = g_hash_table_ref (view->selected);
    expanded = g_hash_table_ref (view->expanded);
    view->synchronizing = TRUE;
    for (guint position = 0;
         !view->disposing &&
         position < g_list_model_get_n_items (G_LIST_MODEL (rows));
         position++)
    {
        GtkTreeListRow *row = gtk_tree_list_model_get_row (rows, position);
        Account *account = account_from_row (row);
        if (account && gtk_tree_list_row_is_expandable (row))
            gtk_tree_list_row_set_expanded (row, has_guid (expanded, account));
        g_clear_object (&row);
    }
    count = g_list_model_get_n_items (G_LIST_MODEL (rows));
    if (!view->disposing)
        gtk_selection_model_unselect_all (selection);
    for (guint position = 0; !view->disposing && position < count; position++)
    {
        GtkTreeListRow *row = gtk_tree_list_model_get_row (rows, position);
        Account *account = account_from_row (row);

        if (account && has_guid (selected, account))
            gtk_selection_model_select_item (selection, position, FALSE);
        g_clear_object (&row);
    }
    g_hash_table_unref (expanded);
    g_hash_table_unref (selected);
    g_object_unref (selection);
    g_object_unref (rows);
    if (!view->disposing)
    {
        view->synchronizing = FALSE;
        view->rebuilding = FALSE;
    }
    return G_SOURCE_REMOVE;
}

static void
schedule_restore (GncTreeViewAccount *view)
{
    if (!view->disposing && view->rows && view->selection && view->selected &&
        view->expanded && !view->restore_source)
        view->restore_source = g_idle_add_full (G_PRIORITY_DEFAULT_IDLE,
                                                restore_state_cb,
                                                g_object_ref (view),
                                                g_object_unref);
}

static void
model_rebuilding (GncTreeModelAccount *model, GncTreeViewAccount *view)
{
    if (view->disposing)
        return;
    view->rebuilding = TRUE;
    (void)model;
}

static void
model_changed (GncTreeModelAccount *model, GncTreeViewAccount *view)
{
    if (view->disposing)
        return;
    schedule_restore (view);
    (void)model;
}

static void
selection_changed (GtkSelectionModel *selection, guint position, guint n_items,
                   GncTreeViewAccount *view)
{
    if (view->disposing || view->synchronizing || view->rebuilding)
        return;
    for (guint index = position; index < position + n_items; index++)
    {
        Account *account = account_at (view, index);
        if (!account)
            continue;
        if (gtk_selection_model_is_selected (selection, index) &&
            view->selection_filter &&
            !view->selection_filter (account, view->selection_filter_data))
        {
            view->synchronizing = TRUE;
            gtk_selection_model_unselect_item (selection, index);
            view->synchronizing = FALSE;
            continue;
        }
        mark_selected (view, account,
                       gtk_selection_model_is_selected (selection, index));
    }
}

static void
account_activated (GtkColumnView *column_view, guint position,
                   GncTreeViewAccount *view)
{
    if (view->disposing)
        return;
    Account *account = account_at (view, position);
    if (account)
        g_signal_emit (view, signals[ACCOUNT_ACTIVATED], 0, account);
    (void)column_view;
}

static void
view_dispose (GObject *object)
{
    GncTreeViewAccount *view = GNC_TREE_VIEW_ACCOUNT (object);
    GDestroyNotify selection_filter_destroy;
    gpointer selection_filter_data;
    guint restore_source;

    view->disposing = TRUE;
    if (view->view_sorter && view->view_sorter_changed_id)
    {
        g_signal_handler_disconnect (view->view_sorter,
                                     view->view_sorter_changed_id);
        view->view_sorter_changed_id = 0;
        view->view_sorter = NULL;
    }
    restore_source = view->restore_source;
    view->restore_source = 0;
    if (restore_source)
        g_source_remove (restore_source);
    if (view->children_context)
        view->children_context->disposed = TRUE;
    view->children_context = NULL;
    if (view->selection)
        g_signal_handlers_disconnect_by_func (view->selection, selection_changed, view);
    if (view->account_model)
    {
        g_signal_handlers_disconnect_by_func (view->account_model, model_rebuilding, view);
        g_signal_handlers_disconnect_by_func (view->account_model, model_changed, view);
    }
    if (view->column_view)
    {
        g_signal_handlers_disconnect_by_func (view->column_view, account_activated, view);
        gnc_column_view_unbind_grid_line_preferences (view->column_view);
        gtk_column_view_set_model (view->column_view, NULL);
    }
    selection_filter_destroy = g_steal_pointer (&view->selection_filter_destroy);
    selection_filter_data = g_steal_pointer (&view->selection_filter_data);
    view->selection_filter = NULL;
    if (selection_filter_destroy)
        selection_filter_destroy (selection_filter_data);

    /* Dispose child widgets (including view->column_view) while selection/models are alive */
    G_OBJECT_CLASS (gnc_tree_view_account_parent_class)->dispose (object);

    view->column_view = NULL;
    view->view_sorter = NULL;
    g_clear_pointer (&view->column_sorters, g_ptr_array_unref);
    g_clear_object (&view->selection);
    g_clear_object (&view->rows);
    g_clear_object (&view->account_model);
}

static void
view_finalize (GObject *object)
{
    GncTreeViewAccount *view = GNC_TREE_VIEW_ACCOUNT (object);

    g_clear_object (&view->selection);
    g_clear_object (&view->rows);
    g_clear_object (&view->account_model);
    view->view_sorter = NULL;
    g_clear_pointer (&view->column_sorters, g_ptr_array_unref);
    g_clear_pointer (&view->selected, g_hash_table_destroy);
    g_clear_pointer (&view->expanded, g_hash_table_destroy);
    g_clear_pointer (&view->state_section, g_free);

    G_OBJECT_CLASS (gnc_tree_view_account_parent_class)->finalize (object);
}

static void
view_unroot (GtkWidget *widget)
{
    GncTreeViewAccount *view = GNC_TREE_VIEW_ACCOUNT (widget);

    view->unrooting = TRUE;
    GTK_WIDGET_CLASS (gnc_tree_view_account_parent_class)->unroot (widget);
    view->unrooting = FALSE;
}

static void
gnc_tree_view_account_class_init (GncTreeViewAccountClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

    object_class->dispose = view_dispose;
    object_class->finalize = view_finalize;
    widget_class->unroot = view_unroot;

    signals[ACCOUNT_ACTIVATED] = g_signal_new ("account-activated",
        G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_LAST, 0, NULL, NULL, NULL,
        G_TYPE_NONE, 1, GNC_TYPE_ACCOUNT);
}

static void
gnc_tree_view_account_init (GncTreeViewAccount *view)
{
    gtk_orientable_set_orientation (GTK_ORIENTABLE (view), GTK_ORIENTATION_VERTICAL);
    view->column_sorters = g_ptr_array_new_with_free_func (g_object_unref);
    view->selected = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
    view->expanded = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
    view->selection_mode = GTK_SELECTION_SINGLE;
}

static GtkSelectionModel *
selection_model_new (GListModel *model, GtkSelectionMode mode)
{
    /* GtkSingleSelection defaults implement BROWSE. SINGLE must be configured
     * before attaching the model so it starts without an automatic selection. */
    switch (mode)
    {
    case GTK_SELECTION_NONE:
        return GTK_SELECTION_MODEL (gtk_no_selection_new (model));
    case GTK_SELECTION_MULTIPLE:
        return GTK_SELECTION_MODEL (gtk_multi_selection_new (model));
    case GTK_SELECTION_BROWSE:
        return GTK_SELECTION_MODEL (gtk_single_selection_new (model));
    case GTK_SELECTION_SINGLE:
    default:
    {
        GtkSingleSelection *selection = gtk_single_selection_new (NULL);

        gtk_single_selection_set_autoselect (selection, FALSE);
        gtk_single_selection_set_can_unselect (selection, TRUE);
        gtk_single_selection_set_model (selection, model);
        return GTK_SELECTION_MODEL (selection);
    }
    }
}

static void
retain_one_selected (GncTreeViewAccount *view)
{
    Account *current = gnc_tree_view_account_get_selected_account (view);
    gchar *retained = NULL;

    if (g_hash_table_size (view->selected) <= 1)
        return;
    if (current && has_guid (view->selected, current))
        retained = account_guid (current);
    else
    {
        GHashTableIter iter;
        gpointer key;

        g_hash_table_iter_init (&iter, view->selected);
        while (g_hash_table_iter_next (&iter, &key, NULL))
            if (!retained || g_strcmp0 (key, retained) < 0)
            {
                g_free (retained);
                retained = g_strdup (key);
            }
    }
    g_hash_table_remove_all (view->selected);
    if (retained)
        g_hash_table_add (view->selected, retained);
}

static GtkWidget *
new_with_model (Account *root, gboolean show_root)
{
    GncTreeViewAccount *view = g_object_new (GNC_TYPE_TREE_VIEW_ACCOUNT, NULL);
    view->account_model = gnc_tree_model_account_new (root, show_root);
    view->children_context = g_new0 (AccountChildrenContext, 1);
    g_weak_ref_init (&view->children_context->view, view);
    view->rows = gtk_tree_list_model_new (
        G_LIST_MODEL (g_object_ref (gnc_tree_model_account_get_roots (view->account_model))),
        FALSE, FALSE,
        create_children_cb, view->children_context,
        (GDestroyNotify) account_children_context_free);
    view->selection = selection_model_new (G_LIST_MODEL (g_object_ref (view->rows)),
                                           view->selection_mode);
    view->column_view = GTK_COLUMN_VIEW (gtk_column_view_new (g_object_ref (view->selection)));
    gnc_column_view_bind_grid_line_preferences (view->column_view);
    gtk_column_view_set_reorderable (view->column_view, TRUE);
    gtk_widget_set_hexpand (GTK_WIDGET (view->column_view), TRUE);
    gtk_widget_set_vexpand (GTK_WIDGET (view->column_view), TRUE);
    gtk_box_append (GTK_BOX (view), GTK_WIDGET (view->column_view));
    add_default_columns (view);
    g_signal_connect_object (view->selection, "selection-changed",
                             G_CALLBACK (selection_changed), view, 0);
    g_signal_connect_object (view->column_view, "activate",
                             G_CALLBACK (account_activated), view, 0);
    g_signal_connect_object (view->account_model, "rebuilding",
                             G_CALLBACK (model_rebuilding), view, 0);
    g_signal_connect_object (view->account_model, "changed",
                             G_CALLBACK (model_changed), view, 0);
    return GTK_WIDGET (view);
}

GtkWidget *
gnc_tree_view_account_new_with_root (Account *root, gboolean show_root)
{ return new_with_model (root, show_root); }
GtkWidget *
gnc_tree_view_account_new (gboolean show_root)
{ return new_with_model (gnc_book_get_root_account (gnc_get_current_book ()), show_root); }

GtkColumnView *
gnc_tree_view_account_get_column_view (GncTreeViewAccount *view)
{ g_return_val_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view), NULL); return view->column_view; }
GtkSelectionModel *
gnc_tree_view_account_get_selection_model (GncTreeViewAccount *view)
{ g_return_val_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view), NULL); return view->selection; }
void
gnc_tree_view_account_set_selection_mode (GncTreeViewAccount *view,
                                          GtkSelectionMode mode)
{
    GtkSelectionModel *selection;
    GtkSelectionModel *old_selection;
    g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view));
    if (mode == view->selection_mode)
        return;
    if (view->selection_mode == GTK_SELECTION_NONE)
        g_hash_table_remove_all (view->selected);
    if (view->selection_mode == GTK_SELECTION_BROWSE &&
        mode != GTK_SELECTION_NONE &&
        g_hash_table_size (view->selected) == 0)
    {
        Account *account = gnc_tree_view_account_get_selected_account (view);

        if (account)
            mark_selected (view, account, TRUE);
    }
    if (mode == GTK_SELECTION_NONE)
        g_hash_table_remove_all (view->selected);
    else if (mode != GTK_SELECTION_MULTIPLE)
        retain_one_selected (view);
    old_selection = g_steal_pointer (&view->selection);
    if (old_selection)
        g_signal_handlers_disconnect_by_func (old_selection, selection_changed, view);
    selection = selection_model_new (G_LIST_MODEL (g_object_ref (view->rows)), mode);
    g_signal_connect_object (selection, "selection-changed",
                             G_CALLBACK (selection_changed), view, 0);
    view->selection = selection;
    view->selection_mode = mode;
    gtk_column_view_set_model (view->column_view, selection);
    if (mode == GTK_SELECTION_BROWSE &&
        g_hash_table_size (view->selected) == 0)
    {
        Account *account = gnc_tree_view_account_get_selected_account (view);

        if (account)
            mark_selected (view, account, TRUE);
    }
    g_clear_object (&old_selection);
    schedule_restore (view);
}

void
gnc_tree_view_account_set_headers_visible (GncTreeViewAccount *view, gboolean visible)
{
    g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view));
    if (visible)
        gtk_widget_remove_css_class (GTK_WIDGET (view->column_view), "gnc-account-no-headers");
    else
        gtk_widget_add_css_class (GTK_WIDGET (view->column_view), "gnc-account-no-headers");
}

GtkColumnViewColumn *
gnc_tree_view_account_find_column (GncTreeViewAccount *view, const gchar *name)
{
    GListModel *columns;
    g_return_val_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view), NULL);
    columns = gtk_column_view_get_columns (view->column_view);
    for (guint index = 0; index < g_list_model_get_n_items (columns); index++)
    {
        GtkColumnViewColumn *column = g_list_model_get_item (columns, index);
        if (g_strcmp0 (gtk_column_view_column_get_id (column), name) == 0)
            return column;
        g_object_unref (column);
    }
    return NULL;
}

void
gnc_tree_view_account_set_column_visible (GncTreeViewAccount *view,
                                           const gchar *name, gboolean visible)
{
    GtkColumnViewColumn *column = gnc_tree_view_account_find_column (view, name);
    if (column)
    {
        gtk_column_view_column_set_visible (column, visible);
        g_object_unref (column);
    }
}

gboolean
gnc_tree_view_account_get_column_visible (GncTreeViewAccount *view,
                                           const gchar *name)
{
    GtkColumnViewColumn *column = gnc_tree_view_account_find_column (view, name);
    gboolean visible = column && gtk_column_view_column_get_visible (column);
    g_clear_object (&column);
    return visible;
}

void
gnc_tree_view_account_set_state_section (GncTreeViewAccount *view,
                                         const gchar *section)
{
    g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view));
    g_free (view->state_section);
    view->state_section = g_strdup (section);
}

const gchar *
gnc_tree_view_account_get_state_section (GncTreeViewAccount *view)
{
    g_return_val_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view), NULL);
    return view->state_section;
}

void
gnc_tree_view_account_get_view_info (GncTreeViewAccount *view, AccountViewInfo *avi)
{
    g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view));
    g_return_if_fail (avi);
    if (view->use_view_info)
        *avi = view->view_info;
    else
    {
        for (guint type = 0; type < NUM_ACCOUNT_TYPES; type++)
            avi->include_type[type] = TRUE;
        avi->show_hidden = TRUE;
    }
}

void
gnc_tree_view_account_set_view_info (GncTreeViewAccount *view, AccountViewInfo *avi)
{
    g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view));
    view->view_info = *avi;
    view->use_view_info = TRUE;
    gnc_tree_view_account_set_filter (view, gnc_tree_view_account_filter_by_view_info,
                                      &view->view_info, NULL);
}

gboolean
gnc_tree_view_account_filter_by_view_info (Account *account, gpointer user_data)
{
    AccountViewInfo *avi = user_data;
    GNCAccountType type = xaccAccountGetType (account);
    return avi && type >= 0 && type < NUM_ACCOUNT_TYPES && avi->include_type[type] &&
           (avi->show_hidden || !xaccAccountIsHidden (account));
}

void
gnc_tree_view_account_set_filter (GncTreeViewAccount *view,
                                  gnc_tree_view_account_filter_func func,
                                  gpointer data, GDestroyNotify destroy)
{
    g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view));
    if (func != gnc_tree_view_account_filter_by_view_info)
        view->use_view_info = FALSE;
    gnc_tree_model_account_set_filter (view->account_model, func, data, destroy);
}

void
gnc_tree_view_account_refilter (GncTreeViewAccount *view)
{
    g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view));
    gnc_tree_model_account_clear_cache (view->account_model);
}

void
gnc_tree_view_account_set_selection_filter (GncTreeViewAccount *view,
                                             gnc_tree_view_account_selection_filter_func filter,
                                             gpointer data, GDestroyNotify destroy)
{
    g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view));
    if (view->selection_filter_destroy)
        view->selection_filter_destroy (view->selection_filter_data);
    view->selection_filter = filter;
    view->selection_filter_data = data;
    view->selection_filter_destroy = destroy;
}

gint
gnc_tree_view_account_count_children (GncTreeViewAccount *view, Account *account)
{
    g_return_val_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view), 0);
    return account ? gnc_account_n_children (account) : 0;
}

void
gnc_tree_view_account_clear_model_cache (GncTreeViewAccount *view)
{
    g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view));
    gnc_tree_model_account_clear_cache (view->account_model);
}

void
gnc_tree_view_account_rebind_columns (GncTreeViewAccount *view)
{
    if (!GNC_IS_TREE_VIEW_ACCOUNT (view) || view->disposing || !view->rows || !G_IS_LIST_MODEL (view->rows))
        return;

    guint n = g_list_model_get_n_items (G_LIST_MODEL (view->rows));
    if (n > 0)
        g_list_model_items_changed (G_LIST_MODEL (view->rows), 0, n, n);
}

Account *
gnc_tree_view_account_get_account_at (GncTreeViewAccount *view, guint position)
{
    g_return_val_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view), NULL);
    return account_at (view, position);
}

Account *
gnc_tree_view_account_get_cursor_account (GncTreeViewAccount *view)
{
    g_return_val_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view), NULL);
    if (view->disposing || !view->rows || !view->selection)
        return NULL;
    for (guint position = 0;
         position < g_list_model_get_n_items (G_LIST_MODEL (view->rows)); position++)
        if (gtk_selection_model_is_selected (view->selection, position))
            return account_at (view, position);
    return NULL;
}

Account *
gnc_tree_view_account_get_selected_account (GncTreeViewAccount *view)
{ return gnc_tree_view_account_get_cursor_account (view); }

GList *
gnc_tree_view_account_get_selected_accounts (GncTreeViewAccount *view)
{
    GList *accounts = NULL;
    g_return_val_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view), NULL);
    if (view->disposing || !view->rows || !view->selection)
        return NULL;
    for (guint position = 0;
         position < g_list_model_get_n_items (G_LIST_MODEL (view->rows)); position++)
    {
        Account *account = account_at (view, position);
        if (account && gtk_selection_model_is_selected (view->selection, position))
            accounts = g_list_prepend (accounts, account);
    }
    return g_list_reverse (accounts);
}

void
gnc_tree_view_account_expand_to_account (GncTreeViewAccount *view, Account *account)
{
    g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view));
    for (Account *current = account; current; current = gnc_account_get_parent (current))
        mark_expanded (view, current, TRUE);
    schedule_restore (view);
}

void
gnc_tree_view_account_collapse_all (GncTreeViewAccount *view)
{
    g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view));
    g_hash_table_remove_all (view->expanded);
    schedule_restore (view);
}

void
gnc_tree_view_account_toggle_expand (GncTreeViewAccount *view, Account *account)
{
    g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view));
    if (view->disposing || !view->rows)
        return;
    for (guint position = 0; position < g_list_model_get_n_items (G_LIST_MODEL (view->rows)); position++)
    {
        GtkTreeListRow *row = gtk_tree_list_model_get_row (view->rows, position);
        Account *row_account = account_from_row (row);
        if (row_account == account && gtk_tree_list_row_is_expandable (row))
        {
            gtk_tree_list_row_set_expanded (row, !gtk_tree_list_row_get_expanded (row));
            g_clear_object (&row);
            return;
        }
        g_clear_object (&row);
    }
}
void
gnc_tree_view_account_set_selected_account (GncTreeViewAccount *view, Account *account)
{
    g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view));
    g_hash_table_remove_all (view->selected);
    if (view->selection_mode == GTK_SELECTION_NONE)
        return;
    if (account)
    {
        mark_selected (view, account, TRUE);
        gnc_tree_view_account_expand_to_account (view, account);
    }
    schedule_restore (view);
}
void
gnc_tree_view_account_set_selected_accounts (GncTreeViewAccount *view,
                                              GList *accounts, gboolean show_last)
{
    g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view));
    g_hash_table_remove_all (view->selected);
    if (view->selection_mode == GTK_SELECTION_NONE)
        return;
    for (GList *node = accounts; node; node = node->next)
    {
        Account *account = GNC_ACCOUNT (node->data);
        mark_selected (view, account, TRUE);
        gnc_tree_view_account_expand_to_account (view, account);
    }
    schedule_restore (view);
    (void)show_last;
}

void
gnc_tree_view_account_select_subaccounts (GncTreeViewAccount *view, Account *account)
{
    GList *descendants;
    g_return_if_fail (GNC_IS_TREE_VIEW_ACCOUNT (view));
    if (!account)
        return;
    descendants = gnc_account_get_descendants (account);
    gnc_tree_view_account_set_selected_accounts (view, descendants, FALSE);
    g_list_free (descendants);
}

static AccountColumn *
column_data (GncTreeViewAccount *view, const gchar *name)
{
    GtkColumnViewColumn *column = gnc_tree_view_account_find_column (view, name);
    AccountColumn *data = column ? g_object_get_data (G_OBJECT (column), "gnc-account-column") : NULL;
    g_clear_object (&column);
    return data;
}

static void
set_edited (GncTreeViewAccount *view, const gchar *name, gnc_tree_view_account_edited_func edited_cb)
{
    AccountColumn *data = column_data (view, name);
    if (!data)
        return;
    data->edited_cb = edited_cb;
    gnc_tree_view_account_clear_model_cache (view);
}

void gnc_tree_view_account_set_code_edited (GncTreeViewAccount *view, gnc_tree_view_account_edited_func callback)
{ set_edited (view, "account-code", callback); }
void gnc_tree_view_account_set_description_edited (GncTreeViewAccount *view, gnc_tree_view_account_edited_func callback)
{ set_edited (view, "description", callback); }
void gnc_tree_view_account_set_notes_edited (GncTreeViewAccount *view, gnc_tree_view_account_edited_func callback)
{ set_edited (view, "notes", callback); }
void gnc_tree_view_account_set_editing_started_cb (GncTreeViewAccount *view, GFunc callback, gpointer user_data)
{ view->editing_started_cb = callback; view->editing_started_data = user_data; }
void gnc_tree_view_account_set_editing_finished_cb (GncTreeViewAccount *view, GFunc callback, gpointer user_data)
{ view->editing_finished_cb = callback; view->editing_finished_data = user_data; }

void
gnc_tree_view_account_name_edited_cb (Account *account, gpointer column, const gchar *new_name)
{
    Account *parent = gnc_account_get_parent (account);
    Account *existing = gnc_account_lookup_by_name (parent, new_name);
    if (!existing || existing == account)
        xaccAccountSetName (account, new_name);
    (void)column;
}
void gnc_tree_view_account_code_edited_cb (Account *account, gpointer column, const gchar *new_code)
{ if (g_strcmp0 (xaccAccountGetCode (account), new_code) != 0) xaccAccountSetCode (account, new_code); (void)column; }
void gnc_tree_view_account_description_edited_cb (Account *account, gpointer column, const gchar *new_desc)
{ if (g_strcmp0 (xaccAccountGetDescription (account), new_desc) != 0) xaccAccountSetDescription (account, new_desc); (void)column; }
void gnc_tree_view_account_notes_edited_cb (Account *account, gpointer column, const gchar *new_notes)
{ if (g_strcmp0 (xaccAccountGetNotes (account), new_notes) != 0) xaccAccountSetNotes (account, new_notes); (void)column; }
/* Filter dialog and persisted filter state are independent of the GTK4
   presentation and operate solely on Account identities. */
gboolean
gnc_plugin_page_account_tree_filter_accounts (Account *account, gpointer user_data)
{
    AccountFilterDialog *fd = user_data;
    GNCAccountType type;
    gnc_numeric total;
    if (!fd)
        return TRUE;
    if (fd->filter_override && g_hash_table_contains (fd->filter_override, account))
        return TRUE;
    if (!fd->show_hidden && xaccAccountIsHidden (account))
        return FALSE;
    if (!fd->show_zero_total)
    {
        total = xaccAccountGetBalanceInCurrency (account, NULL, TRUE);
        if (gnc_numeric_zero_p (total)) return FALSE;
    }
    if (!fd->show_unused && gnc_account_and_descendants_empty (account))
        return FALSE;
    type = xaccAccountGetType (account);
    return type >= 0 && (fd->visible_types & (1u << type));
}

void gppat_filter_show_hidden_toggled_cb (GtkCheckButton *button, AccountFilterDialog *fd)
{ fd->show_hidden = gtk_check_button_get_active (button); gnc_tree_view_account_refilter (fd->tree_view); }
void gppat_filter_show_zero_toggled_cb (GtkCheckButton *button, AccountFilterDialog *fd)
{ fd->show_zero_total = gtk_check_button_get_active (button); gnc_tree_view_account_refilter (fd->tree_view); }
void gppat_filter_show_unused_toggled_cb (GtkCheckButton *button, AccountFilterDialog *fd)
{ fd->show_unused = gtk_check_button_get_active (button); gnc_tree_view_account_refilter (fd->tree_view); }

static void
set_type_selection (AccountFilterDialog *fd, gboolean selected)
{
    fd->updating_type_selection = TRUE;
    fd->visible_types = selected ? G_MAXUINT32 : 0;
    for (guint position = 0; fd->type_model && position < g_list_model_get_n_items (fd->type_model); position++)
    {
        GncAccountTypeItem *item = GNC_ACCOUNT_TYPE_ITEM (g_list_model_get_item (fd->type_model, position));
        gnc_account_type_item_set_selected (item, selected);
        g_object_unref (item);
    }
    fd->updating_type_selection = FALSE;
    gnc_tree_view_account_refilter (fd->tree_view);
}
void gppat_filter_clear_all_cb (GtkWidget *button, AccountFilterDialog *fd)
{ set_type_selection (fd, FALSE); (void)button; }
void gppat_filter_select_all_cb (GtkWidget *button, AccountFilterDialog *fd)
{ set_type_selection (fd, TRUE); (void)button; }
void gppat_filter_select_default_cb (GtkWidget *button, AccountFilterDialog *fd)
{ set_type_selection (fd, TRUE); (void)button; }

static void
type_selected_cb (GncAccountTypeItem *item, GParamSpec *pspec, GtkListItem *list_item)
{
    AccountFilterDialog *fd = g_object_get_data (G_OBJECT (list_item), "account-filter-dialog");
    GNCAccountType type = gnc_account_type_item_get_account_type (item);
    if (gnc_account_type_item_get_selected (item)) fd->visible_types |= 1u << type;
    else fd->visible_types &= ~(1u << type);
    if (!fd->updating_type_selection) gnc_tree_view_account_refilter (fd->tree_view);
    (void)pspec;
}
static void
type_setup_cb (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    GtkWidget *button = gtk_check_button_new ();
    gtk_list_item_set_child (item, button);
    g_object_set_data (G_OBJECT (item), "account-filter-dialog", user_data);
    (void)factory;
}
static void
type_bind_cb (GtkSignalListItemFactory *factory, GtkListItem *list_item, gpointer user_data)
{
    GncAccountTypeItem *item = GNC_ACCOUNT_TYPE_ITEM (gtk_list_item_get_item (list_item));
    GtkCheckButton *button = GTK_CHECK_BUTTON (gtk_list_item_get_child (list_item));
    GBinding *binding;
    gtk_check_button_set_label (button, gnc_account_type_item_get_name (item));
    binding = g_object_bind_property (item, "selected", button, "active",
                                      G_BINDING_BIDIRECTIONAL | G_BINDING_SYNC_CREATE);
    g_object_set_data_full (G_OBJECT (list_item), "account-filter-binding", binding,
                            (GDestroyNotify) g_binding_unbind);
    g_signal_connect_object (item, "notify::selected", G_CALLBACK (type_selected_cb), list_item, 0);
    (void)factory; (void)user_data;
}
static void
type_unbind_cb (GtkSignalListItemFactory *factory, GtkListItem *list_item, gpointer user_data)
{
    GncAccountTypeItem *item = GNC_ACCOUNT_TYPE_ITEM (gtk_list_item_get_item (list_item));
    g_signal_handlers_disconnect_by_func (item, type_selected_cb, list_item);
    g_object_set_data (G_OBJECT (list_item), "account-filter-binding", NULL);
    (void)factory; (void)user_data;
}

#define ACCOUNT_FILTER_ACCEPTED "gnc-account-filter-accepted"

static void
account_filter_dialog_restore (AccountFilterDialog *fd)
{
    fd->visible_types = fd->original_visible_types;
    fd->show_hidden = fd->original_show_hidden;
    fd->show_zero_total = fd->original_show_zero_total;
    fd->show_unused = fd->original_show_unused;
    gnc_tree_view_account_refilter (fd->tree_view);
}

static void
account_filter_dialog_destroy_cb (GtkWidget *dialog, AccountFilterDialog *fd)
{
    if (!fd)
        return;
    if (!g_object_get_data (G_OBJECT (dialog), ACCOUNT_FILTER_ACCEPTED))
        account_filter_dialog_restore (fd);
    g_clear_object (&fd->type_model);
    if (fd->dialog == dialog)
        fd->dialog = NULL;
}

static void
account_filter_dialog_finish (AccountFilterDialog *fd, gboolean accepted)
{
    GtkWidget *dialog;

    if (!fd || !fd->dialog)
        return;

    dialog = g_steal_pointer (&fd->dialog);
    if (accepted)
        g_object_set_data (G_OBJECT (dialog), ACCOUNT_FILTER_ACCEPTED, GINT_TO_POINTER (1));
    gtk_window_destroy (GTK_WINDOW (dialog));
    g_object_unref (dialog);
}

static void
account_filter_dialog_apply_cb (GtkButton *button, AccountFilterDialog *fd)
{
    account_filter_dialog_finish (fd, TRUE);
    (void)button;
}

static void
account_filter_dialog_cancel_cb (GtkButton *button, AccountFilterDialog *fd)
{
    account_filter_dialog_finish (fd, FALSE);
    (void)button;
}

static gboolean
account_filter_dialog_close_request_cb (GtkWindow *window, AccountFilterDialog *fd)
{
    account_filter_dialog_finish (fd, FALSE);
    (void)window;
    return TRUE;
}
void
account_filter_dialog_create (AccountFilterDialog *fd, GncPluginPage *page)
{
    GtkBuilder *builder;
    GtkWidget *dialog;
    GtkListView *list_view;
    GtkSelectionModel *selection;
    GtkListItemFactory *factory;
    gchar *title;
    if (fd->dialog) { gtk_window_present (GTK_WINDOW (fd->dialog)); return; }
    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-account.glade", "account_filter_by_dialog");
    dialog = GTK_WIDGET (gtk_builder_get_object (builder, "account_filter_by_dialog"));
    g_object_ref (dialog);
    fd->dialog = dialog;
    gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (GNC_PLUGIN_PAGE (page)->window));
    title = g_strdup_printf (_("Filter %s by..."), _(gnc_plugin_page_get_page_name (page)));
    gtk_window_set_title (GTK_WINDOW (dialog), title); g_free (title);
    fd->original_visible_types = fd->visible_types;
    fd->original_show_hidden = fd->show_hidden;
    fd->original_show_zero_total = fd->show_zero_total;
    fd->original_show_unused = fd->show_unused;
    gtk_check_button_set_active (GTK_CHECK_BUTTON (gtk_builder_get_object (builder, "show_hidden")), fd->show_hidden);
    gtk_check_button_set_active (GTK_CHECK_BUTTON (gtk_builder_get_object (builder, "show_zero")), fd->show_zero_total);
    gtk_check_button_set_active (GTK_CHECK_BUTTON (gtk_builder_get_object (builder, "show_unused")), fd->show_unused);
    list_view = GTK_LIST_VIEW (gtk_builder_get_object (builder, FILTER_TREE_VIEW));
    fd->type_model = gnc_account_type_list_new (~(1u << ACCT_TYPE_ROOT));
    for (guint position = 0; position < g_list_model_get_n_items (fd->type_model); position++)
    {
        GncAccountTypeItem *item = GNC_ACCOUNT_TYPE_ITEM (g_list_model_get_item (fd->type_model, position));
        GNCAccountType type = gnc_account_type_item_get_account_type (item);
        gnc_account_type_item_set_selected (item, (fd->visible_types & (1u << type)) != 0);
        g_object_unref (item);
    }
    selection = GTK_SELECTION_MODEL (gtk_no_selection_new (fd->type_model));
    factory = GTK_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());
    g_signal_connect (factory, "setup", G_CALLBACK (type_setup_cb), fd);
    g_signal_connect (factory, "bind", G_CALLBACK (type_bind_cb), fd);
    g_signal_connect (factory, "unbind", G_CALLBACK (type_unbind_cb), fd);
    gtk_list_view_set_factory (list_view, factory);
    gtk_list_view_set_model (list_view, selection);
    g_object_unref (selection);
    gnc_builder_connect_signals (builder, fd);
    g_signal_connect (gtk_builder_get_object (builder, "okbutton1"), "clicked",
                      G_CALLBACK (account_filter_dialog_apply_cb), fd);
    g_signal_connect (gtk_builder_get_object (builder, "cancelbutton1"), "clicked",
                      G_CALLBACK (account_filter_dialog_cancel_cb), fd);
    g_signal_connect (dialog, "close-request",
                      G_CALLBACK (account_filter_dialog_close_request_cb), fd);
    g_signal_connect (dialog, "destroy", G_CALLBACK (account_filter_dialog_destroy_cb), fd);
    gtk_window_set_default_widget (GTK_WINDOW (dialog),
                                   GTK_WIDGET (gtk_builder_get_object (builder, "okbutton1")));
    g_object_unref (builder);
    gtk_window_present (GTK_WINDOW (dialog));
}
static void
save_filter (AccountFilterDialog *fd, GKeyFile *key_file, const gchar *group,
             const gchar *types, const gchar *hidden, const gchar *zero, const gchar *unused)
{
    g_key_file_set_integer (key_file, group, types, fd->visible_types);
    g_key_file_set_boolean (key_file, group, hidden, fd->show_hidden);
    g_key_file_set_boolean (key_file, group, zero, fd->show_zero_total);
    g_key_file_set_boolean (key_file, group, unused, fd->show_unused);
}

void
gnc_tree_view_account_save_filter (GncTreeViewAccount *view, AccountFilterDialog *fd,
                                   GKeyFile *key_file, const gchar *group_name)
{
    save_filter (fd, key_file, group_name, ACCOUNT_TYPES, SHOW_HIDDEN_ACCOUNTS,
                 SHOW_ZERO_TOTALS, SHOW_UNUSED_ACCOUNTS);
    (void)view;
}

void
gnc_tree_view_account_save (GncTreeViewAccount *view, AccountFilterDialog *fd,
                            GKeyFile *key_file, const gchar *group_name)
{
    GHashTableIter iter;
    gpointer value;
    guint count = 0;
    Account *selected;
    save_filter (fd, key_file, group_name, ACCT_TYPES, SHOW_HIDDEN, SHOW_ZERO, SHOW_UNUSED);
    selected = gnc_tree_view_account_get_selected_account (view);
    if (selected)
    {
        gchar *name = gnc_account_get_full_name (selected);
        g_key_file_set_string (key_file, group_name, ACCT_SELECTED, name);
        g_free (name);
    }
    g_hash_table_iter_init (&iter, view->expanded);
    while (g_hash_table_iter_next (&iter, &value, NULL))
    {
        GncGUID guid;
        Account *account;
        if (!string_to_guid (value, &guid) ||
            !(account = xaccAccountLookup (&guid, gnc_get_current_book ())))
            continue;
        gchar *key = g_strdup_printf (ACCT_OPEN, ++count);
        gchar *name = gnc_account_get_full_name (account);
        g_key_file_set_string (key_file, group_name, key, name);
        g_free (name);
        g_free (key);
    }
    g_key_file_set_integer (key_file, group_name, ACCT_COUNT, count);
}

static void
restore_filter (AccountFilterDialog *fd, GKeyFile *key_file, const gchar *group,
                const gchar *types, const gchar *hidden, const gchar *zero, const gchar *unused)
{
    GError *error = NULL;
    gboolean value = g_key_file_get_boolean (key_file, group, hidden, &error);
    if (!error) fd->show_hidden = value; else g_clear_error (&error);
    value = g_key_file_get_boolean (key_file, group, zero, &error);
    if (!error) fd->show_zero_total = value; else g_clear_error (&error);
    value = g_key_file_get_boolean (key_file, group, unused, &error);
    if (!error) fd->show_unused = value; else g_clear_error (&error);
    gint values = g_key_file_get_integer (key_file, group, types, &error);
    if (!error) fd->visible_types = values; else g_clear_error (&error);
}

void
gnc_tree_view_account_restore_filter (GncTreeViewAccount *view, AccountFilterDialog *fd,
                                      GKeyFile *key_file, const gchar *group_name)
{
    restore_filter (fd, key_file, group_name, ACCOUNT_TYPES, SHOW_HIDDEN_ACCOUNTS,
                    SHOW_ZERO_TOTALS, SHOW_UNUSED_ACCOUNTS);
    (void)view;
}

void
gnc_tree_view_account_restore (GncTreeViewAccount *view, AccountFilterDialog *fd,
                               GKeyFile *key_file, const gchar *group_name)
{
    GError *error = NULL;
    gint count;
    gchar *name;
    restore_filter (fd, key_file, group_name, ACCT_TYPES, SHOW_HIDDEN, SHOW_ZERO, SHOW_UNUSED);
    count = g_key_file_get_integer (key_file, group_name, ACCT_COUNT, &error);
    if (!error)
        for (gint index = 1; index <= count; index++)
        {
            gchar *key = g_strdup_printf (ACCT_OPEN, index);
            name = g_key_file_get_string (key_file, group_name, key, NULL);
            if (name)
            {
                Account *account = gnc_account_lookup_by_full_name (
                    gnc_book_get_root_account (gnc_get_current_book ()), name);
                if (account) gnc_tree_view_account_expand_to_account (view, account);
                g_free (name);
            }
            g_free (key);
        }
    else
        g_clear_error (&error);
    name = g_key_file_get_string (key_file, group_name, ACCT_SELECTED, NULL);
    if (name)
    {
        Account *account = gnc_account_lookup_by_full_name (
            gnc_book_get_root_account (gnc_get_current_book ()), name);
        if (account) gnc_tree_view_account_set_selected_account (view, account);
        g_free (name);
    }
    gnc_tree_view_account_refilter (view);
}
