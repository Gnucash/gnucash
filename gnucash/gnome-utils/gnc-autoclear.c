/********************************************************************
 * gnc-autoclear.c -- Knapsack algorithm functions                  *
 *                                                                  *
 * Copyright 2020 Cristian Klein <cristian@kleinlabs.eu>            *
 * Modified  2021 Christopher Lam to clear same-amount splits       *
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
 *******************************************************************/
#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "Account.h"
#include "Split.h"
#include "Transaction.h"
#include "gncOwner.h"
#include "qof.h"
#include "gnc-autoclear.h"

/* the following functions are used in window-autoclear: */

typedef enum
{
    AUTOCLEAR_OVERLOAD = 1,
    AUTOCLEAR_UNABLE,
    AUTOCLEAR_MULTIPLE,
    AUTOCLEAR_NOP,
} autoclear_error_type;

#define MAXIMUM_SACK_SIZE 1000000

#define log_module "autoclear"

typedef struct
{
    GList *worklist;
    GHashTable *sack;
    Split *split;
} sack_data;

typedef struct
{
    gnc_numeric reachable_amount;
    GList *list_of_splits;
} WorkItem;

static GList *DUP_LIST;

static WorkItem *
make_workitem (GHashTable *hash, gnc_numeric amount,
               Split *split, GList *splits)
{
    WorkItem *item = g_new0 (WorkItem, 1);
    item->reachable_amount = amount;
    if (g_hash_table_lookup (hash, GINT_TO_POINTER (amount.num)) || splits == DUP_LIST)
        item->list_of_splits = DUP_LIST;
    else
        item->list_of_splits = g_list_prepend (g_list_copy (splits), split);
    return item;
}

static void
sack_foreach_func (gint thisvalue_num, GList *splits, sack_data *data)
{
    gnc_numeric itemval = xaccSplitGetAmount (data->split);
    gnc_numeric this_value = gnc_numeric_create (thisvalue_num, itemval.denom);
    gnc_numeric new_value = gnc_numeric_add_fixed (this_value, itemval);
    WorkItem *item = make_workitem (data->sack, new_value, data->split, splits);

    data->worklist = g_list_prepend (data->worklist, item);
}

static void
sack_free (gpointer thisvalue_num, GList *splits, sack_data *data)
{
    if (splits != DUP_LIST)
        g_list_free (splits);
}

static void
process_work (WorkItem *item, GHashTable *sack)
{
    GList *existing = g_hash_table_lookup (sack, GINT_TO_POINTER(item->reachable_amount.num));
    if (existing && existing != DUP_LIST)
    {
        DEBUG ("removing existing for %6.2f\n",
               gnc_numeric_to_double (item->reachable_amount));
        g_list_free (existing);
    }
    g_hash_table_insert (sack, GINT_TO_POINTER(item->reachable_amount.num), item->list_of_splits);
}

gboolean
gnc_autoclear_get_splits (Account *account, gnc_numeric toclear_value,
                          time64 end_date,
                          GList **splits, GError **error, GtkLabel *label)
{
    GList *nc_list = NULL, *toclear_list = NULL;
    GHashTable *sack;
    GQuark autoclear_quark = g_quark_from_static_string ("autoclear");

    g_return_val_if_fail (GNC_IS_ACCOUNT (account), FALSE);
    g_return_val_if_fail (splits != NULL, FALSE);

    sack = g_hash_table_new (NULL, NULL);
    DUP_LIST = g_list_prepend (NULL, NULL);

    /* Extract which splits are not cleared and compute the amount we have to clear */
    GList *acc_splits = xaccAccountGetSplitList (account);
    for (GList *node = acc_splits; node; node = node->next)
    {
        Split *split = (Split *)node->data;
        gnc_numeric amount = xaccSplitGetAmount (split);

        if (amount.denom != toclear_value.denom)
        {
            g_set_error (error, autoclear_quark, AUTOCLEAR_NOP, "Split amount and toclear amount have different denoms");
            goto skip_knapsack;
        }
        if (xaccSplitGetReconcile (split) != NREC)
            toclear_value = gnc_numeric_sub_fixed (toclear_value, amount);
        else if (gnc_numeric_zero_p (amount))
            DEBUG ("skipping zero-amount split %p", split);
        else if (end_date != INT64_MAX &&
                 xaccTransGetDate (xaccSplitGetParent (split)) > end_date)
            DEBUG ("skipping split after statement_date %p", split);
        else
            nc_list = g_list_prepend (nc_list, split);
    }
    g_list_free (acc_splits);

    if (gnc_numeric_zero_p (toclear_value))
    {
        g_set_error (error, autoclear_quark, AUTOCLEAR_NOP,
                     _("Account is already at Auto-Clear Balance."));
        goto skip_knapsack;
    }
    else if (!nc_list)
    {
        g_set_error (error, autoclear_quark, AUTOCLEAR_NOP,
                     _("No uncleared splits found."));
        goto skip_knapsack;
    }

    for (GList *node = nc_list; node; node = node->next)
    {
        Split *split = (Split *)node->data;
        WorkItem *item = make_workitem (sack, xaccSplitGetAmount (split), split, NULL);
        sack_data s_data = { g_list_prepend (NULL, item), sack, split };

        g_hash_table_foreach (sack, (GHFunc) sack_foreach_func, &s_data);
        g_list_foreach (s_data.worklist, (GFunc) process_work, sack);
        g_list_free_full (s_data.worklist, g_free);
        if (g_hash_table_size (sack) > MAXIMUM_SACK_SIZE)
        {
            g_set_error (error, autoclear_quark, AUTOCLEAR_OVERLOAD,
                         _("Too many uncleared splits"));
            goto skip_knapsack;
        }
        else if (g_hash_table_lookup (sack, GINT_TO_POINTER(toclear_value.num)) == DUP_LIST)
        {
            g_set_error (error, autoclear_quark, AUTOCLEAR_MULTIPLE,
                         _("Cannot uniquely clear splits. Found multiple possibilities."));
            goto skip_knapsack;
        }
    }

    toclear_list = g_hash_table_lookup (sack, GINT_TO_POINTER(toclear_value.num));

    /* Check solution */
    if (!toclear_list)
    {
        g_set_error (error, autoclear_quark, AUTOCLEAR_UNABLE,
                     _("The selected amount cannot be cleared."));
        goto skip_knapsack;
    }
    else
        /* copy GList because GHashTable value will be freed */
        *splits = g_list_copy (toclear_list);

 skip_knapsack:
    g_hash_table_foreach (sack, (GHFunc) sack_free, NULL);
    g_hash_table_destroy (sack);
    g_list_free (nc_list);
    g_list_free (DUP_LIST);

    return (toclear_list != NULL);
}


GList *
gnc_account_get_autoclear_splits (Account *account, gnc_numeric toclear_value,
                                  gchar **errmsg)
{
    GError *error = NULL;
    GList *splits = NULL;

    gnc_autoclear_get_splits (account, toclear_value, INT64_MAX,
                              &splits, &error, NULL);

    if (error)
    {
        *errmsg = g_strdup (error->message);
        g_error_free (error);
        return NULL;
    }

    *errmsg = NULL;
    return splits;
}
