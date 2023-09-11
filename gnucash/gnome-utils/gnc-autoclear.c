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

static inline GQuark
autoclear_quark (void)
{
    return g_quark_from_static_string ("autoclear");
}

static gboolean
numeric_equal (gnc_numeric *n1, gnc_numeric *n2)
{
    return gnc_numeric_equal (*n1, *n2);
}

static guint
numeric_hash (gnc_numeric *n1)
{
    gdouble d1 = gnc_numeric_to_double (*n1);
    return g_double_hash (&d1);
}

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
    if (g_hash_table_lookup (hash, &amount) || splits == DUP_LIST)
        item->list_of_splits = DUP_LIST;
    else
        item->list_of_splits = g_list_prepend (g_list_copy (splits), split);
    return item;
}

static void
sack_foreach_func (gnc_numeric *thisvalue, GList *splits, sack_data *data)
{
    gnc_numeric itemval = xaccSplitGetAmount (data->split);
    gnc_numeric new_value = gnc_numeric_add_fixed (*thisvalue, itemval);
    WorkItem *item = make_workitem (data->sack, new_value, data->split, splits);

    data->worklist = g_list_prepend (data->worklist, item);
}

static void dump_sack (gnc_numeric *thisvalue, GList *splits, sack_data *data)
{
    DEBUG ("%6.2f:", gnc_numeric_to_double (*thisvalue));
    if (splits == DUP_LIST)
        DEBUG (" DUPE");
    else
        for (GList *n = splits; n; n = n->next)
            DEBUG (" [%5.2f]", gnc_numeric_to_double (xaccSplitGetAmount (n->data)));
    DEBUG ("\n");
}

static void
sack_free (gnc_numeric *thisvalue, GList *splits, sack_data *data)
{
    dump_sack (thisvalue, splits, data);
    g_free (thisvalue);
    if (splits != DUP_LIST)
        g_list_free (splits);
}

static void
process_work (WorkItem *item, GHashTable *sack)
{
    GList *existing = g_hash_table_lookup (sack, &item->reachable_amount);
    if (existing && existing != DUP_LIST)
    {
        DEBUG ("removing existing for %6.2f\n",
               gnc_numeric_to_double (item->reachable_amount));
        g_list_free (existing);
    }
    g_hash_table_insert (sack, &item->reachable_amount, item->list_of_splits);
}

static void status_update (GtkLabel *label, gchar *status)
{
    if (!label) return;
    gtk_label_set_text (label, status);
    while (gtk_events_pending ())
        g_main_context_iteration (NULL,FALSE);
}

gboolean
gnc_autoclear_get_splits (Account *account, gnc_numeric toclear_value,
                          time64 end_date,
                          GList **splits, GError **error, GtkLabel *label)
{
    GList *nc_list = NULL, *toclear_list = NULL;
    GHashTable *sack;
    guint nc_progress = 0, nc_list_length = 0;

    g_return_val_if_fail (GNC_IS_ACCOUNT (account), FALSE);
    g_return_val_if_fail (splits != NULL, FALSE);

    sack = g_hash_table_new ((GHashFunc) numeric_hash, (GEqualFunc) numeric_equal);
    DUP_LIST = g_list_prepend (NULL, NULL);

    /* Extract which splits are not cleared and compute the amount we have to clear */
    for (GList *node = xaccAccountGetSplitList (account); node; node = node->next)
    {
        Split *split = (Split *)node->data;

        if (xaccSplitGetReconcile (split) != NREC)
            toclear_value = gnc_numeric_sub_fixed
                (toclear_value, xaccSplitGetAmount (split));
        else if (gnc_numeric_zero_p (xaccSplitGetAmount (split)))
            DEBUG ("skipping zero-amount split %p", split);
        else if (end_date != INT64_MAX &&
                 xaccTransGetDate (xaccSplitGetParent (split)) > end_date)
            DEBUG ("skipping split after statement_date %p", split);
        else
        {
            nc_list = g_list_prepend (nc_list, split);
            nc_list_length++;
        }
    }

    if (gnc_numeric_zero_p (toclear_value))
    {
        g_set_error (error, autoclear_quark (), AUTOCLEAR_NOP,
                     _("Account is already at Auto-Clear Balance."));
        goto skip_knapsack;
    }
    else if (!nc_list)
    {
        g_set_error (error, autoclear_quark (), AUTOCLEAR_NOP,
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
        g_list_free (s_data.worklist);
        nc_progress++;
        if (label)
        {
            gchar *text = g_strdup_printf ("%u/%u splits processed, %u combos",
                                           nc_progress, nc_list_length,
                                           g_hash_table_size (sack));
            status_update (label, text);
            g_free (text);
        }
        if (g_hash_table_size (sack) > MAXIMUM_SACK_SIZE)
        {
            g_set_error (error, autoclear_quark (), AUTOCLEAR_OVERLOAD,
                         _("Too many uncleared splits"));
            goto skip_knapsack;
        }
        else if (g_hash_table_lookup (sack, &toclear_value) == DUP_LIST)
        {
            g_set_error (error, autoclear_quark (), AUTOCLEAR_MULTIPLE,
                         _("Cannot uniquely clear splits. Found multiple possibilities."));
            goto skip_knapsack;
        }
    }

    status_update (label, "Cleaning up...");

    toclear_list = g_hash_table_lookup (sack, &toclear_value);

    /* Check solution */
    if (!toclear_list)
    {
        g_set_error (error, autoclear_quark (), AUTOCLEAR_UNABLE,
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

    status_update (label, NULL);

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
