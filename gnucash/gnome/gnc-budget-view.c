/*
 * Author: Phil Longstaff Copyright (C) 2013 phil.longstaff@yahoo.ca
 *
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
 ******************************************************************
*/

/* GTK4 budget display: account hierarchy and totals use ColumnView models. */
#include <config.h>
#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include "gnc-budget-view.h"
#include "gnc-budget.h"
#include "gnc-features.h"
#include "dialog-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-gtk-utils.h"
#include "gnc-prefs.h"
#include "gnc-session.h"
#include "gnc-tree-view-account.h"
#include "gnc-ui-util.h"
#include "gnc-state.h"
#include "gnc-recurrence.h"
#include "Recurrence.h"

G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_BUDGET;
#define STATE_SECTION_PREFIX "Budget"
#define BUDGET_GUID "Budget GncGUID"

typedef struct GncBudgetViewPrivate GncBudgetViewPrivate;
typedef struct
{
    GWeakRef view;
    grefcount ref_count;
    gint period;
} BudgetColumnInfo;
typedef void (*BudgetFactoryCallback) (GtkSignalListItemFactory *, GtkListItem *, gpointer);

struct _GncBudgetView { GtkBox w; };
struct _GncBudgetViewClass { GtkBoxClass w; };
struct GncBudgetViewPrivate
{
    GncTreeViewAccount *account_view;
    GtkColumnView *account_columns;
    GtkColumnView *totals_columns;
    GtkWidget *totals_scroll;
    GtkAdjustment *account_hadjustment;
    GListStore *totals_rows;
    GncBudget *budget;
    GncGUID key;
    AccountFilterDialog *fd;
    Account *root_account;
    gboolean use_red_color;
    gboolean show_account_code;
    gboolean show_account_desc;
    GList *period_columns;
    GList *totals_period_columns;
    GtkColumnViewColumn *total_column;
    GtkColumnViewColumn *totals_total_column;
    GtkColumnViewColumn *totals_name_column;
    gulong account_activated_cb_id;
    gulong totals_hadjustment_cb_id;
    Account *active_account;
    guint active_period;
    gboolean disposing;
    gboolean unrooting;
    GPtrArray *column_sorters;
};

G_DEFINE_TYPE_WITH_PRIVATE (GncBudgetView, gnc_budget_view, GTK_TYPE_BOX)
#define PRIV(v) ((GncBudgetViewPrivate *)gnc_budget_view_get_instance_private (v))

enum { TOTALS_TYPE_INCOME, TOTALS_TYPE_EXPENSES, TOTALS_TYPE_ASSET_LIAB_EQ, TOTALS_TYPE_REMAINDER };

static gnc_numeric gbv_get_accumulated_budget_amount (GncBudget *budget, Account *account, guint period);
static void gnc_budget_view_refresh_totals (GncBudgetView *view);
static gnc_commodity * gnc_budget_view_get_currency (GncBudgetView *view);

static BudgetColumnInfo *
budget_column_info_new (GncBudgetView *view, gint period)
{
    BudgetColumnInfo *info = g_new0 (BudgetColumnInfo, 1);

    g_weak_ref_init (&info->view, view);
    g_ref_count_init (&info->ref_count);
    info->period = period;
    return info;
}

static BudgetColumnInfo *
budget_column_info_ref (BudgetColumnInfo *info)
{
    g_ref_count_inc (&info->ref_count);
    return info;
}

static void
budget_column_info_unref (BudgetColumnInfo *info)
{
    if (!g_ref_count_dec (&info->ref_count))
        return;

    g_weak_ref_clear (&info->view);
    g_free (info);
}

static void
budget_column_info_closure_destroy (gpointer data, GClosure *closure)
{
    budget_column_info_unref (data);
    (void)closure;
}

static GncBudgetView *
budget_column_info_get_view (BudgetColumnInfo *info)
{
    GObject *object;
    GncBudgetView *view;

    if (!info)
        return NULL;

    object = g_weak_ref_get (&info->view);
    if (!object)
        return NULL;

    view = GNC_BUDGET_VIEW (object);
    if (PRIV (view)->disposing)
    {
        g_object_unref (view);
        return NULL;
    }

    return view;
}

static Account *
account_from_list_item (GtkListItem *item)
{
    if (!item || !GTK_IS_LIST_ITEM (item))
        return NULL;
    GObject *item_obj = gtk_list_item_get_item (item);
    if (!item_obj || !G_IS_OBJECT (item_obj))
        return NULL;
    if (GNC_IS_ACCOUNT (item_obj))
        return GNC_ACCOUNT (item_obj);
    if (!GTK_IS_TREE_LIST_ROW (item_obj))
        return NULL;
    GObject *row_item = gtk_tree_list_row_get_item (GTK_TREE_LIST_ROW (item_obj));
    if (!row_item || !G_IS_OBJECT (row_item) || !GNC_IS_ACCOUNT (row_item))
    {
        g_clear_object (&row_item);
        return NULL;
    }
    Account *account = GNC_ACCOUNT (row_item);
    g_clear_object (&row_item);
    return account;
}

static gchar *
period_text (GncBudgetView *view, Account *account, guint period)
{
    if (!view || !account)
        return g_strdup ("");

    GncBudgetViewPrivate *priv = PRIV (view);
    gnc_numeric value;
    gchar text[100];
    if (!gnc_budget_is_account_period_value_set (priv->budget, account, period))
    {
        if (!gnc_account_n_children (account))
            return g_strdup ("");
        value = gbv_get_accumulated_budget_amount (priv->budget, account, period);
    }
    else
        value = gnc_budget_get_account_period_value (priv->budget, account, period);
    if (gnc_numeric_check (value))
        return g_strdup (_("error"));
    if (gnc_reverse_balance (account))
        value = gnc_numeric_neg (value);
    xaccSPrintAmount (text, value, gnc_account_print_info (account, FALSE));
    return g_strdup (text);
}

static gchar *
total_text (GncBudgetView *view, Account *account)
{
    if (!view || !account)
        return g_strdup ("");

    GncBudgetViewPrivate *priv = PRIV (view);
    gnc_numeric total = gnc_numeric_zero ();
    guint count = gnc_budget_get_num_periods (priv->budget);
    for (guint period = 0; period < count; period++)
    {
        gnc_numeric value = gnc_budget_is_account_period_value_set (priv->budget, account, period) ?
            gnc_budget_get_account_period_value (priv->budget, account, period) :
            gbv_get_accumulated_budget_amount (priv->budget, account, period);
        if (!gnc_numeric_check (value))
            total = gnc_numeric_add (total, value, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
    }
    if (gnc_reverse_balance (account)) total = gnc_numeric_neg (total);
    gchar text[100];
    xaccSPrintAmount (text, total, gnc_account_print_info (account, TRUE));
    return g_strdup (text);
}

typedef struct { gnc_numeric total; GncBudget *budget; guint period; GNCPriceDB *pdb; gnc_commodity *currency; } BudgetAccumulation;
static void
accumulate_child (Account *account, gpointer data)
{
    BudgetAccumulation *info = data;
    gnc_numeric value;
    if (gnc_budget_is_account_period_value_set (info->budget, account, info->period))
        value = gnc_budget_get_account_period_value (info->budget, account, info->period);
    else if (gnc_account_n_children (account))
        value = gbv_get_accumulated_budget_amount (info->budget, account, info->period);
    else
        return;
    value = gnc_pricedb_convert_balance_nearest_price_t64
        (info->pdb, value, gnc_account_get_currency_or_parent (account), info->currency,
         gnc_budget_get_period_start_date (info->budget, info->period));
    info->total = gnc_numeric_add (info->total, value, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
}

static gnc_numeric
gbv_get_accumulated_budget_amount (GncBudget *budget, Account *account, guint period)
{
    if (!budget || !account)
        return gnc_numeric_zero ();

    if (gnc_budget_is_account_period_value_set (budget, account, period))
        return gnc_budget_get_account_period_value (budget, account, period);
    BudgetAccumulation info = { gnc_numeric_zero (), budget, period,
        gnc_pricedb_get_db (gnc_account_get_book (account)), gnc_account_get_currency_or_parent (account) };
    gnc_account_foreach_child (account, accumulate_child, &info);
    return info.total;
}

static gnc_numeric
budget_total_for_kind (GncBudgetView *view, gint kind, gint period)
{
    if (!view)
        return gnc_numeric_zero ();

    GncBudgetViewPrivate *priv = PRIV (view);
    if (!priv->root_account || !priv->budget)
        return gnc_numeric_zero ();

    GNCPriceDB *pdb = gnc_pricedb_get_db (gnc_get_current_book ());
    gnc_commodity *currency = gnc_budget_view_get_currency (view);
    gnc_numeric total = gnc_numeric_zero ();
    GList *children = gnc_account_get_children (priv->root_account);
    for (GList *node = children; node; node = node->next)
    {
        Account *account = node->data;
        if (!account)
            continue;
        GNCAccountType type = xaccAccountTypeGetFundamental (xaccAccountGetType (account));
        if (!((kind == TOTALS_TYPE_INCOME && type == ACCT_TYPE_INCOME) ||
              (kind == TOTALS_TYPE_EXPENSES && type == ACCT_TYPE_EXPENSE) ||
              kind == TOTALS_TYPE_REMAINDER ||
              (kind == TOTALS_TYPE_ASSET_LIAB_EQ && (type == ACCT_TYPE_ASSET || type == ACCT_TYPE_LIABILITY || type == ACCT_TYPE_EQUITY))))
            continue;
        gnc_numeric value = period < 0 ? gnc_numeric_zero () : gbv_get_accumulated_budget_amount (priv->budget, account, period);
        if (period < 0)
        {
            guint n = gnc_budget_get_num_periods (priv->budget);
            for (guint p = 0; p < n; p++)
            {
                gnc_numeric part = gbv_get_accumulated_budget_amount (priv->budget, account, p);
                part = gnc_pricedb_convert_balance_nearest_price_t64 (pdb, part, gnc_account_get_currency_or_parent (account), currency, gnc_budget_get_period_start_date (priv->budget, p));
                value = gnc_numeric_add (value, part, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
            }
        }
        else
            value = gnc_pricedb_convert_balance_nearest_price_t64 (pdb, value, gnc_account_get_currency_or_parent (account), currency, gnc_budget_get_period_start_date (priv->budget, period));
        total = gnc_numeric_add (total, value, GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD);
    }
    g_list_free (children);
    return gnc_numeric_neg (total);
}

static void
budget_label_focus_enter (GtkEventControllerFocus *controller, gpointer data)
{
    BudgetColumnInfo *info = data;
    GtkWidget *label = gtk_event_controller_get_widget (GTK_EVENT_CONTROLLER (controller));
    GncBudgetView *view = budget_column_info_get_view (info);

    if (!view)
        return;

    if (info->period < 0)
    {
        g_object_unref (view);
        return;
    }

    GncBudgetViewPrivate *priv = PRIV (view);
    priv->active_account = g_object_get_data (G_OBJECT (label), "gnc-budget-account");
    priv->active_period = (guint)info->period;
    g_object_unref (view);
}

static gboolean
budget_label_key_pressed (GtkEventControllerKey *controller, guint keyval, guint keycode, GdkModifierType state, gpointer data)
{
    BudgetColumnInfo *info = data;
    GncBudgetView *view;
    GncBudgetViewPrivate *priv;
    gboolean handled = FALSE;

    if (keyval != GDK_KEY_Tab && keyval != GDK_KEY_ISO_Left_Tab && keyval != GDK_KEY_KP_Tab)
        return FALSE;

    view = budget_column_info_get_view (info);
    if (!view)
        return FALSE;

    priv = PRIV (view);
    guint periods = gnc_budget_get_num_periods (priv->budget);
    if (info->period < 0 || !periods || !priv->active_account)
        goto out;

    gboolean backwards = (state & GDK_SHIFT_MASK) || keyval == GDK_KEY_ISO_Left_Tab;
    guint target_period = backwards ? (info->period + periods - 1) % periods : (info->period + 1) % periods;
    GtkSelectionModel *selection_model = gnc_tree_view_account_get_selection_model (priv->account_view);
    if (!selection_model)
        goto out;
    guint n = g_list_model_get_n_items (G_LIST_MODEL (selection_model));
    if (!n)
        goto out;

    guint position = 0;
    for (; position < n; position++) if (gnc_tree_view_account_get_account_at (priv->account_view, position) == priv->active_account) break;
    if (target_period == (backwards ? periods - 1 : 0))
        position = backwards ? (position ? position - 1 : n - 1) : (position + 1) % n;
    GtkColumnViewColumn *column = g_list_nth_data (priv->period_columns, target_period);
    if (column && n)
        gtk_column_view_scroll_to (priv->account_columns, position, column, GTK_LIST_SCROLL_FOCUS, NULL);
    priv->active_period = target_period;
    priv->active_account = gnc_tree_view_account_get_account_at (priv->account_view, position);
    handled = TRUE;

out:
    g_object_unref (view);
    (void)controller; (void)keycode;
    return handled;
}

static void
budget_label_editing_changed (GtkEditableLabel *label, GParamSpec *pspec, gpointer data)
{
    BudgetColumnInfo *info = data;
    gboolean editing = gtk_editable_label_get_editing (label);
    gboolean was_editing = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (label), "gnc-budget-was-editing"));

    if (editing)
    {
        g_object_set_data (G_OBJECT (label), "gnc-budget-was-editing", GINT_TO_POINTER (TRUE));
        return;
    }

    if (!was_editing)
        return;

    g_object_set_data (G_OBJECT (label), "gnc-budget-was-editing", NULL);

    GncBudgetView *view = budget_column_info_get_view (info);
    if (!view)
        return;

    Account *account = g_object_get_data (G_OBJECT (label), "gnc-budget-account");
    GncBudgetViewPrivate *priv = PRIV (view);
    const gchar *text = gtk_editable_get_text (GTK_EDITABLE (label));
    gnc_numeric value = gnc_numeric_error (GNC_ERROR_ARG);
    if (!account || qof_book_is_readonly (gnc_get_current_book ()) ||
        (!xaccParseAmount (text, TRUE, &value, NULL) && text && *text))
    {
        g_object_unref (view);
        return;
    }

    if (!text || !*text)
        gnc_budget_unset_account_period_value (priv->budget, account, info->period);
    else
    {
        if (gnc_reverse_balance (account)) value = gnc_numeric_neg (value);
        gnc_budget_set_account_period_value (priv->budget, account, info->period, value);
    }
    gnc_budget_view_refresh_totals (view);
    g_object_unref (view);
    (void)pspec;
}

static void
period_setup (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer data)
{
    BudgetColumnInfo *info = data;
    GtkWidget *label = gtk_editable_label_new ("");
    gtk_editable_set_alignment (GTK_EDITABLE (label), 1.0f);
    GtkEventController *key = gtk_event_controller_key_new ();
    g_object_set_data (G_OBJECT (key), "gnc-budget-controller", GINT_TO_POINTER (1));
    gtk_widget_add_controller (label, key);
    g_signal_connect_data (key, "key-pressed", G_CALLBACK (budget_label_key_pressed),
                           budget_column_info_ref (info),
                           budget_column_info_closure_destroy, 0);
    GtkEventController *focus = gtk_event_controller_focus_new ();
    g_object_set_data (G_OBJECT (focus), "gnc-budget-controller", GINT_TO_POINTER (1));
    gtk_widget_add_controller (label, focus);
    g_signal_connect_data (focus, "enter", G_CALLBACK (budget_label_focus_enter),
                           budget_column_info_ref (info),
                           budget_column_info_closure_destroy, 0);
    g_signal_connect_data (label, "notify::editing", G_CALLBACK (budget_label_editing_changed),
                           budget_column_info_ref (info),
                           budget_column_info_closure_destroy, 0);
    gtk_list_item_set_child (item, label);
    (void)factory;
}

static void
period_bind (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer data)
{
    BudgetColumnInfo *info = data;
    GncBudgetView *view = budget_column_info_get_view (info);

    if (!view)
        return;

    Account *account = account_from_list_item (item);
    GtkWidget *label = gtk_list_item_get_child (item);
    if (!label || !GTK_IS_EDITABLE_LABEL (label))
    {
        label = gtk_editable_label_new ("");
        g_signal_connect_data (label, "notify::editing", G_CALLBACK (budget_label_editing_changed),
                               budget_column_info_ref (info),
                               budget_column_info_closure_destroy, 0);
        gtk_list_item_set_child (item, label);
    }

    g_object_set_data (G_OBJECT (label), "gnc-budget-account", account);
    if (!account)
    {
        gtk_editable_set_text (GTK_EDITABLE (label), "");
        gtk_widget_set_tooltip_text (label, NULL);
        g_object_unref (view);
        return;
    }

    g_autofree gchar *text = period_text (view, account, info->period);
    gtk_editable_set_text (GTK_EDITABLE (label), text);
    gtk_widget_set_tooltip_text (label, gnc_budget_get_account_period_note (PRIV (view)->budget, account, info->period));
    g_object_unref (view);
    (void)factory;
}

static void
clear_widget_controllers_recursively (GtkWidget *widget)
{
    if (!widget)
        return;

    if (GTK_IS_EDITABLE_LABEL (widget))
    {
        g_signal_handlers_disconnect_by_func (widget, (gpointer)G_CALLBACK (budget_label_editing_changed), NULL);
        if (gtk_editable_label_get_editing (GTK_EDITABLE_LABEL (widget)))
        {
            gboolean was_focusable = gtk_widget_get_focusable (widget);
            gtk_widget_set_focusable (widget, FALSE);
            gtk_editable_label_stop_editing (GTK_EDITABLE_LABEL (widget), FALSE);
            gtk_widget_set_focusable (widget, was_focusable);
        }
    }

    GListModel *controllers = gtk_widget_observe_controllers (widget);
    if (controllers)
    {
        guint n = g_list_model_get_n_items (controllers);
        for (int i = (int)n - 1; i >= 0; i--)
        {
            GtkEventController *controller = GTK_EVENT_CONTROLLER (g_list_model_get_item (controllers, i));
            if (controller)
            {
                if (g_object_get_data (G_OBJECT (controller), "gnc-budget-controller"))
                {
                    g_signal_handlers_disconnect_by_func (controller, (gpointer)G_CALLBACK (budget_label_key_pressed), NULL);
                    g_signal_handlers_disconnect_by_func (controller, (gpointer)G_CALLBACK (budget_label_focus_enter), NULL);
                    gtk_widget_remove_controller (widget, controller);
                }
                g_object_unref (controller);
            }
        }
        g_object_unref (controllers);
    }

    for (GtkWidget *child = gtk_widget_get_first_child (widget); child; child = gtk_widget_get_next_sibling (child))
        clear_widget_controllers_recursively (child);
}

static void
remove_controllers_and_clear_child (GtkListItem *item)
{
    GtkWidget *label = gtk_list_item_get_child (item);
    if (label)
    {
        g_object_set_data (G_OBJECT (label), "gnc-budget-account", NULL);
        g_object_set_data (G_OBJECT (label), "gnc-budget-was-editing", NULL);
        clear_widget_controllers_recursively (label);
    }
    gtk_list_item_set_child (item, NULL);
}

static void
period_unbind (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer data)
{
    BudgetColumnInfo *info = data;
    GtkWidget *label = gtk_list_item_get_child (item);
    GncBudgetView *view = budget_column_info_get_view (info);
    Account *account = NULL;

    if (label && GTK_IS_EDITABLE_LABEL (label))
    {
        account = g_object_get_data (G_OBJECT (label), "gnc-budget-account");
        gtk_widget_set_tooltip_text (label, NULL);
    }

    if (view)
    {
        GncBudgetViewPrivate *priv = PRIV (view);

        if (account && info->period >= 0 && priv->active_account == account &&
            priv->active_period == (guint)info->period)
            priv->active_account = NULL;

        g_object_unref (view);
    }

    remove_controllers_and_clear_child (item);
    (void)factory;
}

static void
period_teardown (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer data)
{
    remove_controllers_and_clear_child (item);
    (void)factory;
    (void)data;
}

static void
total_bind (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer data)
{
    BudgetColumnInfo *info = data;
    GncBudgetView *view = budget_column_info_get_view (info);

    if (!view)
        return;

    Account *account = account_from_list_item (item);
    GtkWidget *child = gtk_list_item_get_child (item);
    if (!child || !GTK_IS_LABEL (child))
    {
        child = gtk_label_new ("");
        gtk_label_set_xalign (GTK_LABEL (child), 0.0f);
        gtk_list_item_set_child (item, child);
    }
    GtkLabel *label = GTK_LABEL (child);
    if (!account)
    {
        gtk_label_set_text (label, "");
        g_object_unref (view);
        return;
    }
    g_autofree gchar *text = total_text (view, account);
    gtk_label_set_text (label, text);
    g_object_unref (view);
    (void)factory;
}

static void
total_unbind (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer data)
{
    gtk_list_item_set_child (item, NULL);
    (void)factory;
    (void)data;
}

static gnc_commodity *
gnc_budget_view_get_currency (GncBudgetView *view)
{
    GncBudgetViewPrivate *priv = PRIV (view);
    gnc_commodity *currency = gnc_default_currency ();

    if (currency)
        return currency;

    if (priv->root_account)
        currency = xaccAccountGetCommodity (priv->root_account);

    if (currency)
        return currency;

    return gnc_commodity_table_lookup (
        gnc_commodity_table_get_table (gnc_get_current_book ()),
        GNC_COMMODITY_NS_CURRENCY, "USD");
}

static void
total_row_bind (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer data)
{
    BudgetColumnInfo *info = data;
    GncBudgetView *view = budget_column_info_get_view (info);

    if (!view)
        return;

    GtkWidget *child = gtk_list_item_get_child (item);
    if (!child || !GTK_IS_LABEL (child))
    {
        g_object_unref (view);
        return;
    }

    gint kind = gtk_list_item_get_position (item);
    if (kind == (gint)GTK_INVALID_LIST_POSITION)
    {
        gtk_label_set_text (GTK_LABEL (child), "");
        g_object_unref (view);
        return;
    }

    gnc_numeric value = budget_total_for_kind (view, kind, info->period);
    gnc_commodity *currency = gnc_budget_view_get_currency (view);
    GNCPrintAmountInfo pinfo = currency ? gnc_commodity_print_info (currency, info->period < 0) : gnc_default_print_info (info->period < 0);
    gtk_label_set_text (GTK_LABEL (child), xaccPrintAmount (value, pinfo));
    g_object_unref (view);
    (void)factory;
}

static void
label_setup (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer data)
{ GtkWidget *label = gtk_label_new (""); gtk_label_set_xalign (GTK_LABEL (label), 0.0f); gtk_list_item_set_child (item, label); (void)factory; (void)data; }

static void
total_name_bind (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer data)
{
    GtkWidget *child = gtk_list_item_get_child (item);
    GObject *strobj = gtk_list_item_get_item (item);
    if (child && GTK_IS_LABEL (child) && strobj && GTK_IS_STRING_OBJECT (strobj))
    {
        gtk_label_set_text (GTK_LABEL (child), gtk_string_object_get_string (GTK_STRING_OBJECT (strobj)));
    }
    (void)factory;
    (void)data;
}

static void
label_teardown (GtkSignalListItemFactory *factory, GtkListItem *item, gpointer data)
{
    gtk_list_item_set_child (item, NULL);
    (void)factory;
    (void)item;
    (void)data;
}

static GtkColumnViewColumn *
new_column (const gchar *title, BudgetFactoryCallback setup, BudgetFactoryCallback bind,
            BudgetFactoryCallback unbind, GncBudgetView *view, gint period)
{
    BudgetColumnInfo *info = budget_column_info_new (view, period);
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();
    g_signal_connect_data (factory, "setup", G_CALLBACK (setup),
                           budget_column_info_ref (info),
                           budget_column_info_closure_destroy, 0);
    g_signal_connect_data (factory, "bind", G_CALLBACK (bind),
                           budget_column_info_ref (info),
                           budget_column_info_closure_destroy, 0);
    if (unbind)
        g_signal_connect_data (factory, "unbind", G_CALLBACK (unbind),
                               budget_column_info_ref (info),
                               budget_column_info_closure_destroy, 0);
    g_signal_connect_data (factory, "teardown", G_CALLBACK (setup == period_setup ? period_teardown : label_teardown),
                           budget_column_info_ref (info),
                           budget_column_info_closure_destroy, 0);
    budget_column_info_unref (info);
    GtkColumnViewColumn *column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_resizable (column, TRUE);
    return column;
}

static gchar *
period_title (GncBudget *budget, guint period)
{
    const Recurrence *recurrence = gnc_budget_get_recurrence (budget);
    GDate date = recurrence->start, next;
    gchar title[MAX_DATE_LENGTH + 1] = "";
    for (guint n = 0; n <= period; n++)
    {
        if (n == period) qof_print_gdate (title, MAX_DATE_LENGTH, &date);
        recurrenceNextInstance (recurrence, &date, &next);
        date = next;
    }
    return g_strdup (title);
}

static void
defer_free_columns (GList *columns)
{
    if (!columns)
        return;
    for (GList *l = columns; l; l = l->next)
    {
        GObject *obj = G_OBJECT (l->data);
        g_object_unref (obj);
    }
    g_list_free (columns);
}

static void
remove_columns (GncBudgetView *owner, GtkColumnView *view, GList **columns)
{
    GList *removed_columns = g_steal_pointer (columns);

    if (!removed_columns)
        return;

    if (view && gtk_column_view_get_model (view))
        gtk_column_view_sort_by_column (view, NULL, GTK_SORT_ASCENDING);

    for (GList *node = removed_columns; node; node = node->next)
    {
        GtkColumnViewColumn *col = GTK_COLUMN_VIEW_COLUMN (node->data);
        if (col)
        {
            gtk_column_view_column_set_visible (col, FALSE);
            if (view)
                gtk_column_view_remove_column (view, col);
        }
    }
    if (view)
        gtk_widget_queue_resize (GTK_WIDGET (view));

    defer_free_columns (removed_columns);
    (void)owner;
}

static void
remove_column (GncBudgetView *owner, GtkColumnView *view,
               GtkColumnViewColumn **column)
{
    GtkColumnViewColumn *removed_column = g_steal_pointer (column);

    if (!removed_column)
        return;

    gtk_column_view_column_set_visible (removed_column, FALSE);
    if (view)
    {
        if (gtk_column_view_get_model (view))
            gtk_column_view_sort_by_column (view, NULL, GTK_SORT_ASCENDING);
        gtk_column_view_remove_column (view, removed_column);
        gtk_widget_queue_resize (GTK_WIDGET (view));
    }

    defer_free_columns (g_list_append (NULL, removed_column));
    (void)owner;
}

static void
store_all_sorters (GncBudgetView *view)
{
    (void)view;
}

static void
stop_editing_in_widget (GtkWidget *widget)
{
    if (!widget)
        return;
    if (GTK_IS_EDITABLE_LABEL (widget))
    {
        if (gtk_editable_label_get_editing (GTK_EDITABLE_LABEL (widget)))
        {
            gboolean was_focusable = gtk_widget_get_focusable (widget);
            gtk_widget_set_focusable (widget, FALSE);
            gtk_editable_label_stop_editing (GTK_EDITABLE_LABEL (widget), FALSE);
            gtk_widget_set_focusable (widget, was_focusable);
        }
    }
    for (GtkWidget *child = gtk_widget_get_first_child (widget); child; child = gtk_widget_get_next_sibling (child))
        stop_editing_in_widget (child);
}

static void
clear_focus_if_owned (GncBudgetView *view, GtkRoot *root)
{
    GtkWidget *focus;

    if (!root)
        return;

    focus = gtk_root_get_focus (root);
    if (focus && (focus == GTK_WIDGET (view) ||
                  gtk_widget_is_ancestor (focus, GTK_WIDGET (view))))
        gtk_root_set_focus (root, NULL);
}

static void
create_columns (GncBudgetView *view)
{
    GncBudgetViewPrivate *priv = PRIV (view);
    GtkColumnView *account_columns = NULL;
    GtkColumnView *totals_columns = NULL;
    GtkRoot *root = gtk_widget_get_root (GTK_WIDGET (view));

    priv->active_account = NULL;
    priv->active_period = -1;

    clear_focus_if_owned (view, root);

    stop_editing_in_widget (GTK_WIDGET (view));

    clear_focus_if_owned (view, root);

    priv->active_account = NULL;
    priv->active_period = 0;

    if (priv->account_columns)
        account_columns = g_object_ref (priv->account_columns);
    if (priv->totals_columns)
        totals_columns = g_object_ref (priv->totals_columns);

    remove_columns (view, account_columns, &priv->period_columns);
    if (priv->disposing)
        goto out;

    remove_columns (view, totals_columns, &priv->totals_period_columns);
    if (priv->disposing)
        goto out;
    remove_column (view, account_columns, &priv->total_column);
    if (priv->disposing)
        goto out;
    remove_column (view, totals_columns, &priv->totals_total_column);
    if (priv->disposing)
        goto out;
    remove_column (view, totals_columns, &priv->totals_name_column);
    if (priv->disposing)
        goto out;

    if (totals_columns)
    {
        GtkListItemFactory *name_factory = gtk_signal_list_item_factory_new ();
        g_signal_connect (name_factory, "setup", G_CALLBACK (label_setup), NULL);
        g_signal_connect (name_factory, "bind", G_CALLBACK (total_name_bind), NULL);
        g_signal_connect (name_factory, "teardown", G_CALLBACK (label_teardown), NULL);
        GtkColumnViewColumn *name_col = gtk_column_view_column_new ("", name_factory);
        gtk_column_view_column_set_fixed_width (name_col, 240);
        gtk_column_view_append_column (totals_columns, name_col);
        if (priv->disposing)
        {
            g_object_unref (name_col);
            goto out;
        }
        priv->totals_name_column = name_col;
    }

    guint count = gnc_budget_get_num_periods (priv->budget);
    for (guint period = 0; period < count; period++)
    {
        g_autofree gchar *title = period_title (priv->budget, period);
        GtkColumnViewColumn *column = new_column (title, period_setup, period_bind,
                                                   period_unbind, view, period);
        gtk_column_view_column_set_fixed_width (column, 125);
        gtk_column_view_append_column (account_columns, column);
        if (priv->disposing)
        {
            g_object_unref (column);
            goto out;
        }
        priv->period_columns = g_list_append (priv->period_columns, column);

        column = new_column ("", label_setup, total_row_bind, NULL, view, period);
        gtk_column_view_column_set_fixed_width (column, 125);
        gtk_column_view_append_column (totals_columns, column);
        if (priv->disposing)
        {
            g_object_unref (column);
            goto out;
        }
        priv->totals_period_columns = g_list_append (priv->totals_period_columns, column);
    }
    GtkColumnViewColumn *total_column = new_column (_("Total"), label_setup, total_bind,
                                                     total_unbind, view, -1);
    gtk_column_view_column_set_fixed_width (total_column, 125);
    gtk_column_view_append_column (account_columns, total_column);
    if (priv->disposing)
    {
        g_object_unref (total_column);
        goto out;
    }
    priv->total_column = total_column;

    total_column = new_column ("", label_setup, total_row_bind, NULL, view, -1);
    gtk_column_view_column_set_fixed_width (total_column, 125);
    gtk_column_view_append_column (totals_columns, total_column);
    if (priv->disposing)
    {
        g_object_unref (total_column);
        goto out;
    }
    priv->totals_total_column = total_column;

out:
    g_clear_object (&totals_columns);
    g_clear_object (&account_columns);

    if (priv->disposing)
        return;

    if (priv->totals_columns)
        gtk_widget_set_focusable (GTK_WIDGET (priv->totals_columns), TRUE);
    if (priv->account_columns)
        gtk_widget_set_focusable (GTK_WIDGET (priv->account_columns), TRUE);
    if (priv->account_view)
    {
        gtk_widget_set_focusable (GTK_WIDGET (priv->account_view), TRUE);
        gnc_tree_view_account_rebind_columns (priv->account_view);
        gnc_tree_view_account_refilter (priv->account_view);
    }
    gtk_widget_set_focusable (GTK_WIDGET (view), TRUE);

    store_all_sorters (view);
}

static void
account_activated (GncTreeViewAccount *tree, Account *account, GncBudgetView *view)
{
    if (!PRIV (view)->disposing)
        g_signal_emit_by_name (view, "account-activated", account);
    (void)tree;
}
static void
sync_totals_hadjustment (GtkAdjustment *adjustment, GncBudgetView *view)
{
    GncBudgetViewPrivate *priv = PRIV (view);
    if (!priv->disposing && priv->account_hadjustment)
        gtk_adjustment_set_value (priv->account_hadjustment,
                                   gtk_adjustment_get_value (adjustment));
}
static void
update_negative_pref (gpointer prefs, gchar *pref, gpointer data)
{ PRIV (data)->use_red_color = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED); (void)prefs; (void)pref; }

static void
create_widget (GncBudgetView *view)
{
    GncBudgetViewPrivate *priv = PRIV (view);
    GtkWidget *accounts_scroll = gtk_scrolled_window_new ();
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (accounts_scroll), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    priv->account_view = GNC_TREE_VIEW_ACCOUNT (gnc_tree_view_account_new (FALSE));
    priv->account_columns = gnc_tree_view_account_get_column_view (priv->account_view);
    gnc_tree_view_account_set_headers_visible (priv->account_view, TRUE);
    gnc_tree_view_account_set_selection_mode (priv->account_view, GTK_SELECTION_MULTIPLE);
    gchar guid[GUID_ENCODING_LENGTH + 1]; guid_to_string_buff (&priv->key, guid);
    g_autofree gchar *section = g_strjoin (" ", STATE_SECTION_PREFIX, guid, NULL);
    gnc_tree_view_account_set_state_section (priv->account_view, section);
    priv->fd->tree_view = priv->account_view;
    gnc_tree_view_account_set_filter (priv->account_view, gnc_plugin_page_account_tree_filter_accounts, priv->fd, NULL);
    gnc_tree_view_account_set_column_visible (priv->account_view, "account-code", priv->show_account_code);
    gnc_tree_view_account_set_column_visible (priv->account_view, "description", priv->show_account_desc);
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (accounts_scroll), GTK_WIDGET (priv->account_view));
    gtk_widget_set_vexpand (accounts_scroll, TRUE);
    priv->account_hadjustment = gtk_scrolled_window_get_hadjustment (GTK_SCROLLED_WINDOW (accounts_scroll));
    priv->account_activated_cb_id = g_signal_connect (priv->account_view, "account-activated",
                                                       G_CALLBACK (account_activated), view);

    priv->totals_rows = g_list_store_new (GTK_TYPE_STRING_OBJECT);
    const gchar *names[] = { _("Income"), _("Expenses"), _("Transfer"), _("Remaining to Budget") };
    for (guint n = 0; n < G_N_ELEMENTS (names); n++)
    {
        GtkStringObject *object = gtk_string_object_new (names[n]);
        g_list_store_append (priv->totals_rows, object);
        g_object_unref (object);
    }
    GtkSelectionModel *totals_selection = GTK_SELECTION_MODEL (gtk_no_selection_new (G_LIST_MODEL (g_object_ref (priv->totals_rows))));
    priv->totals_columns = GTK_COLUMN_VIEW (gtk_column_view_new (totals_selection));
    store_all_sorters (view);
    priv->totals_scroll = gtk_scrolled_window_new ();
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (priv->totals_scroll), GTK_POLICY_AUTOMATIC, GTK_POLICY_NEVER);
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (priv->totals_scroll), GTK_WIDGET (priv->totals_columns));
    priv->totals_hadjustment_cb_id = g_signal_connect (
        gtk_scrolled_window_get_hadjustment (GTK_SCROLLED_WINDOW (priv->totals_scroll)),
        "value-changed", G_CALLBACK (sync_totals_hadjustment), view);
    gtk_box_append (GTK_BOX (view), accounts_scroll);
    gtk_box_append (GTK_BOX (view), gtk_separator_new (GTK_ORIENTATION_HORIZONTAL));
    gtk_box_append (GTK_BOX (view), priv->totals_scroll);
    gnc_tree_view_account_restore_filter (priv->account_view, priv->fd, gnc_state_get_current (), gnc_tree_view_account_get_state_section (priv->account_view));
    create_columns (view);
    store_all_sorters (view);
}

GncBudgetView *
gnc_budget_view_new (GncBudget *budget, AccountFilterDialog *fd)
{
    g_return_val_if_fail (GNC_IS_BUDGET (budget), NULL);
    GncBudgetView *view = g_object_new (GNC_TYPE_BUDGET_VIEW, NULL);
    GncBudgetViewPrivate *priv = PRIV (view);
    priv->budget = budget; priv->key = *gnc_budget_get_guid (budget); priv->fd = fd;
    priv->root_account = gnc_book_get_root_account (gnc_get_current_book ());
    priv->use_red_color = gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED);
    create_widget (view);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED, update_negative_pref, view);
    return view;
}

static void
dispose (GObject *object)
{
    GncBudgetView *view = GNC_BUDGET_VIEW (object);
    GncBudgetViewPrivate *priv = PRIV (view);

    if (!priv->disposing)
    {
        priv->disposing = TRUE;
        priv->account_hadjustment = NULL;
        gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                     GNC_PREF_NEGATIVE_IN_RED,
                                     update_negative_pref, view);
        if (priv->account_activated_cb_id && priv->account_view)
        {
            g_signal_handler_disconnect (priv->account_view, priv->account_activated_cb_id);
            priv->account_activated_cb_id = 0;
        }
        if (priv->totals_hadjustment_cb_id && priv->totals_scroll)
        {
            GtkAdjustment *adj = gtk_scrolled_window_get_hadjustment (GTK_SCROLLED_WINDOW (priv->totals_scroll));
            if (adj)
                g_signal_handler_disconnect (adj, priv->totals_hadjustment_cb_id);
            priv->totals_hadjustment_cb_id = 0;
        }

        remove_columns (view, priv->account_columns, &priv->period_columns);
        remove_columns (view, priv->totals_columns, &priv->totals_period_columns);
        remove_column (view, priv->account_columns, &priv->total_column);
        remove_column (view, priv->totals_columns, &priv->totals_total_column);
        remove_column (view, priv->totals_columns, &priv->totals_name_column);
        g_clear_object (&priv->totals_rows);
        g_clear_pointer (&priv->column_sorters, g_ptr_array_unref);
        stop_editing_in_widget (GTK_WIDGET (view));

        GtkWidget *child;
        while ((child = gtk_widget_get_first_child (GTK_WIDGET (view))))
            gtk_box_remove (GTK_BOX (view), child);

        GtkWidget *parent = gtk_widget_get_parent (GTK_WIDGET (view));
        if (parent)
            gtk_widget_unparent (GTK_WIDGET (view));
    }

    G_OBJECT_CLASS (gnc_budget_view_parent_class)->dispose (object);
}

static void
finalize (GObject *object)
{
    GncBudgetView *view = GNC_BUDGET_VIEW (object);
    GncBudgetViewPrivate *priv = PRIV (view);

    g_assert_null (priv->period_columns);
    g_assert_null (priv->totals_period_columns);
    g_assert_null (priv->total_column);
    g_assert_null (priv->totals_total_column);
    g_assert_null (priv->totals_name_column);
    g_assert_null (priv->totals_rows);
    g_assert_null (priv->column_sorters);

    G_OBJECT_CLASS (gnc_budget_view_parent_class)->finalize (object);
}
static void
gnc_budget_view_unroot (GtkWidget *widget)
{
    GncBudgetView *view = GNC_BUDGET_VIEW (widget);
    GncBudgetViewPrivate *priv = PRIV (view);
    GtkRoot *root = gtk_widget_get_root (widget);

    priv->unrooting = TRUE;

    stop_editing_in_widget (widget);
    clear_focus_if_owned (view, root);

    GTK_WIDGET_CLASS (gnc_budget_view_parent_class)->unroot (widget);

    clear_focus_if_owned (view, root);

    priv->unrooting = FALSE;
}

static void gnc_budget_view_class_init (GncBudgetViewClass *klass)
{
    G_OBJECT_CLASS (klass)->dispose = dispose;
    G_OBJECT_CLASS (klass)->finalize = finalize;
    GTK_WIDGET_CLASS (klass)->unroot = gnc_budget_view_unroot;
    g_signal_new ("account-activated", GNC_TYPE_BUDGET_VIEW, G_SIGNAL_RUN_LAST,
                  0, NULL, NULL, NULL, G_TYPE_NONE, 1, GNC_TYPE_ACCOUNT);
}
static void gnc_budget_view_init (GncBudgetView *view)
{
    GncBudgetViewPrivate *priv = PRIV (view);
    gtk_orientable_set_orientation (GTK_ORIENTABLE (view), GTK_ORIENTATION_VERTICAL);
    gtk_widget_set_name (GTK_WIDGET (view), "gnc-id-budget-page");
    priv->column_sorters = g_ptr_array_new_with_free_func (g_object_unref);
}

GtkSelectionModel *gnc_budget_view_get_selection (GncBudgetView *view) { return gnc_tree_view_account_get_selection_model (PRIV (view)->account_view); }
Account *gnc_budget_view_get_active_account (GncBudgetView *view) { return PRIV (view)->active_account; }
guint gnc_budget_view_get_active_period (GncBudgetView *view) { return PRIV (view)->active_period; }
GList *gnc_budget_view_get_selected_accounts (GncBudgetView *view) { return gnc_tree_view_account_get_selected_accounts (PRIV (view)->account_view); }
GtkWidget *gnc_budget_view_get_account_tree_view (GncBudgetView *view) { return GTK_WIDGET (PRIV (view)->account_view); }
void gnc_budget_view_set_show_account_code (GncBudgetView *view, gboolean show) { PRIV (view)->show_account_code = show; gnc_tree_view_account_set_column_visible (PRIV(view)->account_view, "account-code", show); }
gboolean gnc_budget_view_get_show_account_code (GncBudgetView *view) { return PRIV(view)->show_account_code; }
void gnc_budget_view_set_show_account_description (GncBudgetView *view, gboolean show) { PRIV(view)->show_account_desc = show; gnc_tree_view_account_set_column_visible (PRIV(view)->account_view, "description", show); }
gboolean gnc_budget_view_get_show_account_description (GncBudgetView *view) { return PRIV(view)->show_account_desc; }
void
gnc_budget_view_refresh (GncBudgetView *view)
{
    if (PRIV (view)->disposing)
        return;

    g_object_ref (view);
    create_columns (view);
    if (!PRIV (view)->disposing)
        gnc_budget_view_refresh_totals (view);
    g_object_unref (view);
}

static void
gnc_budget_view_refresh_totals (GncBudgetView *view)
{
    GncBudgetViewPrivate *priv = PRIV (view);

    if (priv->disposing || priv->unrooting || !gtk_widget_get_root (GTK_WIDGET (view)))
        return;

    if (priv->totals_columns)
        gtk_widget_queue_draw (GTK_WIDGET (priv->totals_columns));
    if (priv->account_columns)
        gtk_widget_queue_draw (GTK_WIDGET (priv->account_columns));
}
void gnc_budget_view_resized_cb (GObject *object, GParamSpec *pspec, gpointer data)
{
    if (data && GNC_IS_BUDGET_VIEW (data))
    {
        GncBudgetView *view = GNC_BUDGET_VIEW (data);
        GncBudgetViewPrivate *priv = PRIV (view);
        if (!priv->disposing && !priv->unrooting && gtk_widget_get_root (GTK_WIDGET (view)))
            gnc_budget_view_refresh_totals (view);
    }
    (void)object;
    (void)pspec;
}
void gnc_budget_view_save (GncBudgetView *view, GKeyFile *file, const gchar *group) { gnc_tree_view_account_save (PRIV(view)->account_view, PRIV(view)->fd, file, group); }
gboolean gnc_budget_view_restore (GncBudgetView *view, GKeyFile *file, const gchar *group) { gnc_tree_view_account_restore (PRIV(view)->account_view, PRIV(view)->fd, file, group); return TRUE; }
void gnc_budget_view_save_account_filter (GncBudgetView *view) { gnc_tree_view_account_save_filter (PRIV(view)->account_view, PRIV(view)->fd, gnc_state_get_current (), gnc_tree_view_account_get_state_section (PRIV(view)->account_view)); }
void gnc_budget_view_delete_budget (GncBudgetView *view) { gchar guid[GUID_ENCODING_LENGTH + 1]; guid_to_string_buff (&PRIV(view)->key, guid); gnc_state_drop_sections_for (guid); gnc_tree_view_account_set_state_section (PRIV(view)->account_view, NULL); }
