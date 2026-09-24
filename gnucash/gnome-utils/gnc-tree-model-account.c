/*
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2003 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/* gnc-tree-model-account.c -- GTK4 hierarchical account list model. */
#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-tree-model-account.h"
#include "Account.h"
#include "gnc-accounting-period.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "dialog-utils.h"
#include "gnc-event.h"
#include "gnc-ui-balances.h"
#include "gnc-ui-util.h"
#include "qof.h"

enum { MODEL_REBUILDING, MODEL_CHANGED, LAST_SIGNAL };
static guint signals[LAST_SIGNAL];

struct _GncTreeModelAccount
{
    GObject parent_instance;
    QofBook *book;
    Account *root;
    gboolean show_root;
    GListStore *roots;
    gint event_handler_id;
    GncTreeModelAccountFilterFunc filter;
    gpointer filter_data;
    GDestroyNotify filter_destroy;
    GncTreeModelAccountColumn sort_column;
    GtkSortType sort_order;
};

G_DEFINE_TYPE (GncTreeModelAccount, gnc_tree_model_account, G_TYPE_OBJECT)

static gboolean
account_matches (GncTreeModelAccount *model, Account *account)
{
    return !model->filter || model->filter (account, model->filter_data);
}

static gboolean
account_is_visible (GncTreeModelAccount *model, Account *account)
{
    GList *children;
    gboolean visible;

    visible = account_matches (model, account);
    children = gnc_account_get_children (account);
    for (GList *node = children; node && !visible; node = node->next)
        visible = account_is_visible (model, GNC_ACCOUNT (node->data));
    g_list_free (children);
    return visible;
}

static gint
account_compare (gconstpointer a, gconstpointer b, gpointer user_data)
{
    GncTreeModelAccount *model = user_data;
    gchar *left;
    gchar *right;
    gint result;

    left = gnc_tree_model_account_get_string (model, GNC_ACCOUNT (a),
                                              model->sort_column, NULL);
    right = gnc_tree_model_account_get_string (model, GNC_ACCOUNT (b),
                                               model->sort_column, NULL);
    result = g_utf8_collate (left ? left : "", right ? right : "");
    g_free (left);
    g_free (right);
    if (result == 0)
        result = xaccAccountOrder (GNC_ACCOUNT (a), GNC_ACCOUNT (b));
    return model->sort_order == GTK_SORT_DESCENDING ? -result : result;
}

static void
append_children (GncTreeModelAccount *model, GListStore *store, Account *parent)
{
    GList *children = gnc_account_get_children (parent);

    children = g_list_sort_with_data (children, account_compare, model);
    for (GList *node = children; node; node = node->next)
    {
        Account *account = GNC_ACCOUNT (node->data);
        if (account_is_visible (model, account))
            g_list_store_append (store, account);
    }
    g_list_free (children);
}

static void
rebuild (GncTreeModelAccount *model)
{
    /* Signal handlers may dispose the last view that owns this model. Keep the
     * model alive until the complete rebuilding/changed transaction ends. */
    g_object_ref (model);
    g_signal_emit (model, signals[MODEL_REBUILDING], 0);
    g_list_store_remove_all (model->roots);
    if (!model->root)
        goto cleanup;
    if (model->show_root)
    {
        if (account_is_visible (model, model->root))
            g_list_store_append (model->roots, model->root);
    }
    else
        append_children (model, model->roots, model->root);
    g_signal_emit (model, signals[MODEL_CHANGED], 0);
cleanup:
    g_object_unref (model);
}

static void
account_event (QofInstance *entity, QofEventId event_type,
               GncTreeModelAccount *model, GncEventData *event_data)
{
    Account *account;

    if (!GNC_IS_ACCOUNT (entity))
        return;
    account = GNC_ACCOUNT (entity);
    if (gnc_account_get_book (account) != model->book)
        return;
    /* A rebuild is intentionally atomic: hierarchy, current filter and sort
       order change together, so views cannot retain an invalid row identity. */
    rebuild (model);
    (void)event_type;
    (void)event_data;
}

static void
model_dispose (GObject *object)
{
    GncTreeModelAccount *model = GNC_TREE_MODEL_ACCOUNT (object);

    if (model->event_handler_id)
    {
        qof_event_unregister_handler (model->event_handler_id);
        model->event_handler_id = 0;
    }
    if (model->filter_destroy)
    {
        GDestroyNotify filter_destroy = g_steal_pointer (&model->filter_destroy);
        gpointer filter_data = g_steal_pointer (&model->filter_data);
        if (filter_destroy)
            filter_destroy (filter_data);
    }
    g_clear_object (&model->roots);
    g_clear_object (&model->root);
    model->book = NULL;
    G_OBJECT_CLASS (gnc_tree_model_account_parent_class)->dispose (object);
}

static void
gnc_tree_model_account_class_init (GncTreeModelAccountClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    object_class->dispose = model_dispose;
    signals[MODEL_REBUILDING] = g_signal_new ("rebuilding", G_TYPE_FROM_CLASS (klass),
                                               G_SIGNAL_RUN_LAST, 0, NULL, NULL,
                                               NULL, G_TYPE_NONE, 0);
    signals[MODEL_CHANGED] = g_signal_new ("changed", G_TYPE_FROM_CLASS (klass),
                                            G_SIGNAL_RUN_LAST, 0, NULL, NULL,
                                            NULL, G_TYPE_NONE, 0);
}

static void
gnc_tree_model_account_init (GncTreeModelAccount *model)
{
    model->roots = g_list_store_new (GNC_TYPE_ACCOUNT);
    model->sort_column = GNC_TREE_MODEL_ACCOUNT_COL_NAME;
    model->sort_order = GTK_SORT_ASCENDING;
}

GncTreeModelAccount *
gnc_tree_model_account_new (Account *root, gboolean show_root)
{
    GncTreeModelAccount *model;

    g_return_val_if_fail (root != NULL, NULL);
    model = g_object_new (GNC_TYPE_TREE_MODEL_ACCOUNT, NULL);
    model->book = gnc_account_get_book (root);
    model->root = GNC_ACCOUNT (g_object_ref (root));
    model->show_root = show_root;
    model->event_handler_id = qof_event_register_handler (
        (QofEventHandler) account_event, model);
    rebuild (model);
    return model;
}

GListModel *
gnc_tree_model_account_get_roots (GncTreeModelAccount *model)
{
    g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model), NULL);
    return G_LIST_MODEL (model->roots);
}

GListModel *
gnc_tree_model_account_create_children (GncTreeModelAccount *model,
                                        Account *account)
{
    GListStore *children;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model), NULL);
    g_return_val_if_fail (GNC_IS_ACCOUNT (account), NULL);
    children = g_list_store_new (GNC_TYPE_ACCOUNT);
    append_children (model, children, account);
    return G_LIST_MODEL (children);
}

void
gnc_tree_model_account_set_filter (GncTreeModelAccount *model,
                                   GncTreeModelAccountFilterFunc filter,
                                   gpointer user_data,
                                   GDestroyNotify destroy)
{
    g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model));
    if (model->filter_destroy)
        model->filter_destroy (model->filter_data);
    model->filter = filter;
    model->filter_data = user_data;
    model->filter_destroy = destroy;
    rebuild (model);
}

void
gnc_tree_model_account_set_sort_column (GncTreeModelAccount *model,
                                        GncTreeModelAccountColumn column,
                                        GtkSortType order)
{
    g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model));
    if (model->sort_column == column && model->sort_order == order)
        return;
    model->sort_column = column;
    model->sort_order = order;
    rebuild (model);
}

void
gnc_tree_model_account_clear_cache (GncTreeModelAccount *model)
{
    g_return_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model));
    rebuild (model);
}

static gchar *
period_balance (GncTreeModelAccount *model, Account *account,
                gboolean recurse, gboolean *negative)
{
    time64 start = gnc_accounting_period_fiscal_start ();
    time64 end = gnc_accounting_period_fiscal_end ();
    gnc_numeric amount;

    if (negative)
        *negative = FALSE;
    if (account == model->root || start > end)
        return g_strdup ("");
    amount = xaccAccountGetBalanceChangeForPeriod (account, start, end, recurse);
    if (gnc_reverse_balance (account))
        amount = gnc_numeric_neg (amount);
    if (negative)
        *negative = gnc_numeric_negative_p (amount);
    return g_strdup (gnc_print_amount_with_bidi_ltr_isolate (
        amount, gnc_account_print_info (account, TRUE)));
}

gchar *
gnc_tree_model_account_get_string (GncTreeModelAccount *model, Account *account,
                                   GncTreeModelAccountColumn column,
                                   gboolean *negative_out)
{
    gboolean negative = FALSE;
    gchar *value = NULL;

    g_return_val_if_fail (GNC_IS_TREE_MODEL_ACCOUNT (model), g_strdup (""));
    g_return_val_if_fail (GNC_IS_ACCOUNT (account), g_strdup (""));
    if (negative_out)
        *negative_out = FALSE;

    switch (column)
    {
    case GNC_TREE_MODEL_ACCOUNT_COL_NAME:
        value = g_strdup (account == model->root ? _("New top level account") :
                          xaccAccountGetName (account)); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_TYPE:
        value = g_strdup (xaccAccountGetTypeStr (xaccAccountGetType (account))); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_COMMODITY:
        value = g_strdup (gnc_commodity_get_fullname (xaccAccountGetCommodity (account))); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_CODE: value = g_strdup (xaccAccountGetCode (account)); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_DESCRIPTION: value = g_strdup (xaccAccountGetDescription (account)); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_LASTNUM: value = g_strdup (xaccAccountGetLastNum (account)); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_PRESENT:
        value = gnc_ui_account_get_print_balance (xaccAccountGetPresentBalanceInCurrency, account, TRUE, &negative); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_PRESENT_REPORT:
        value = gnc_ui_account_get_print_report_balance (xaccAccountGetPresentBalanceInCurrency, account, TRUE, &negative); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_BALANCE:
        value = gnc_ui_account_get_print_balance (xaccAccountGetBalanceInCurrency, account, FALSE, &negative); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_REPORT:
        value = gnc_ui_account_get_print_report_balance (xaccAccountGetBalanceInCurrency, account, FALSE, &negative); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_PERIOD: value = period_balance (model, account, FALSE, &negative); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_LIMIT: value = gnc_ui_account_get_balance_limit_icon_name (account); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_BALANCE_LIMIT_EXPLANATION: value = gnc_ui_account_get_balance_limit_explanation (account); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_CLEARED:
        value = gnc_ui_account_get_print_balance (xaccAccountGetClearedBalanceInCurrency, account, TRUE, &negative); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_CLEARED_REPORT:
        value = gnc_ui_account_get_print_report_balance (xaccAccountGetClearedBalanceInCurrency, account, TRUE, &negative); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED:
        value = gnc_ui_account_get_print_balance (xaccAccountGetReconciledBalanceInCurrency, account, TRUE, &negative); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED_REPORT:
        value = gnc_ui_account_get_print_report_balance (xaccAccountGetReconciledBalanceInCurrency, account, TRUE, &negative); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_RECONCILED_DATE:
    {
        time64 date;
        value = xaccAccountGetReconcileLastDate (account, &date) ? qof_print_date (date) : g_strdup ("");
        break;
    }
    case GNC_TREE_MODEL_ACCOUNT_COL_EARLIEST_DATE:
    {
        time64 date = gnc_account_get_earliest_date (account);
        value = date == INT64_MAX ? g_strdup ("") : qof_print_date (date);
        break;
    }
    case GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN:
        value = gnc_ui_account_get_print_balance (xaccAccountGetProjectedMinimumBalanceInCurrency, account, TRUE, &negative); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_FUTURE_MIN_REPORT:
        value = gnc_ui_account_get_print_report_balance (xaccAccountGetProjectedMinimumBalanceInCurrency, account, TRUE, &negative); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_TOTAL:
        value = gnc_ui_account_get_print_balance (xaccAccountGetBalanceInCurrency, account, TRUE, &negative); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_TOTAL_REPORT:
        value = gnc_ui_account_get_print_report_balance (xaccAccountGetBalanceInCurrency, account, TRUE, &negative); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_TOTAL_PERIOD: value = period_balance (model, account, TRUE, &negative); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_COLOR_ACCOUNT: value = g_strdup (xaccAccountGetColor (account)); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_NOTES: value = g_strdup (xaccAccountGetNotes (account)); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_TAX_INFO: value = g_strdup (""); break;
    case GNC_TREE_MODEL_ACCOUNT_COL_TAX_INFO_SUB_ACCT: value = g_strdup (""); break;
    default: value = g_strdup (""); break;
    }

    if (negative_out)
        *negative_out = negative;
    return value ? value : g_strdup ("");
}

gboolean
gnc_tree_model_account_get_boolean (Account *account,
                                    GncTreeModelAccountColumn column)
{
    g_return_val_if_fail (GNC_IS_ACCOUNT (account), FALSE);
    switch (column)
    {
    case GNC_TREE_MODEL_ACCOUNT_COL_HIDDEN: return xaccAccountGetHidden (account);
    case GNC_TREE_MODEL_ACCOUNT_COL_PLACEHOLDER: return xaccAccountGetPlaceholder (account);
    case GNC_TREE_MODEL_ACCOUNT_COL_OPENING_BALANCE: return xaccAccountGetIsOpeningBalance (account);
    default: return FALSE;
    }
}
