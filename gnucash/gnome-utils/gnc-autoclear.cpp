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

#include "inttypes.h"
#include "Account.h"
#include "Split.h"
#include "Transaction.h"
#include "gncOwner.h"
#include "qof.h"
#include "gnc-autoclear.h"

static QofLogModule log_module = GNC_MOD_GUI;

/* the following functions are used in window-autoclear: */

typedef enum
{
    AUTOCLEAR_OVERLOAD = 1,
    AUTOCLEAR_UNABLE,
    AUTOCLEAR_MULTIPLE,
    AUTOCLEAR_NOP,
} autoclear_error_type;

#define UPDATES_PER_SECOND 5
#define MAX_AUTOCLEAR_SECONDS 10

#define MAX_UPDATE_TICKS CLOCKS_PER_SEC / UPDATES_PER_SECOND
#define MAX_AUTOCLEAR_TICKS CLOCKS_PER_SEC * MAX_AUTOCLEAR_SECONDS

static inline GQuark
autoclear_quark (void)
{
    return g_quark_from_static_string ("autoclear");
}

typedef struct
{
    GArray *workarray;
    GHashTable *sack;
    Split *split;
    gboolean debugging_enabled;
    guint nc_progress;
    guint nc_list_length;
    GtkLabel *label;
} sack_data;

typedef struct
{
    gint64 reachable_amount;
    GList *list_of_splits;
} WorkItem;

static GList *DUP_LIST;

static WorkItem *
make_workitem (GHashTable *hash, gint64 amount,
               Split *split, GList *splits)
{
    WorkItem *item = g_new (WorkItem, 1);
    item->reachable_amount = amount;
    if (splits == DUP_LIST || g_hash_table_lookup (hash, (gpointer)amount))
        item->list_of_splits = DUP_LIST;
    else
        item->list_of_splits = g_list_prepend (g_list_copy (splits), split);
    return item;
}

static void status_update (GtkLabel *label, gchar *status)
{
    if (!label) return;
    gtk_label_set_text (label, status);
    while (gtk_events_pending ())
        g_main_context_iteration (NULL,FALSE);
}

static clock_t next_update_tick;

static void looping_update_status (sack_data *data)
{
    clock_t now;
    if (!data->label)
        return;
    now = clock ();
    if (G_UNLIKELY (now > next_update_tick))
    {
        gchar *text = g_strdup_printf ("%u/%u splits processed, %u combos",
                                       data->nc_progress, data->nc_list_length,
                                       g_hash_table_size (data->sack));
        status_update (data->label, text);
        g_free (text);
        next_update_tick = now + MAX_UPDATE_TICKS;
    }
}

static void
sack_foreach_func (gint64 thisvalue, GList *splits, sack_data *data)
{
    gnc_numeric numeric = xaccSplitGetAmount (data->split);
    gint64 itemval = numeric.num;
    gint64 new_value = thisvalue + itemval;
    WorkItem *item = make_workitem (data->sack, new_value, data->split, splits);

    g_array_append_val (data->workarray, item);
    looping_update_status (data);
}

static void dump_sack (gint64 thisvalue, GList *splits, sack_data *data)
{
    DEBUG ("%" PRId64, thisvalue);
    if (splits == DUP_LIST)
        DEBUG (" DUPE");
    else
        for (GList *n = splits; n; n = n->next)
            DEBUG (" [%5.2f]", gnc_numeric_to_double (xaccSplitGetAmount ((Split*)n->data)));
    DEBUG ("\n");
}

static void
sack_free (gint64 thisvalue, GList *splits, gpointer user_data)
{
    if (splits != DUP_LIST)
        g_list_free (splits);
}

static void
process_work (WorkItem *item, sack_data *data)
{
    GList *existing = (GList *)g_hash_table_lookup (data->sack,
                                                    (gpointer)item->reachable_amount);
    g_hash_table_insert (data->sack, (gpointer)item->reachable_amount,
                         item->list_of_splits);
    if (existing)
    {
        if (G_UNLIKELY (data->debugging_enabled))
            DEBUG ("removing existing for %" PRId64 "\n", item->reachable_amount);
        if (existing != DUP_LIST)
            g_list_free (existing);
    }
    looping_update_status (data);
}

gboolean
gnc_autoclear_get_splits (Account *account, gnc_numeric toclear_value,
                          time64 end_date,
                          GList **splits, GError **error, GtkLabel *label)
{
    GList *nc_list = NULL, *toclear_list = NULL;
    GHashTable *sack;
    guint nc_progress = 0, nc_list_length = 0;
    clock_t start_ticks;
    gboolean debugging_enabled = qof_log_check (G_LOG_DOMAIN, QOF_LOG_DEBUG);

    g_return_val_if_fail (GNC_IS_ACCOUNT (account), FALSE);
    g_return_val_if_fail (splits != NULL, FALSE);

    sack = g_hash_table_new ((GHashFunc) g_direct_hash, (GEqualFunc) g_direct_equal);
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
        else if (gnc_numeric_check (xaccSplitGetAmount (split)))
            DEBUG ("skipping invalid-amount split %p", split);
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

    start_ticks = next_update_tick = clock ();
    for (GList *node = nc_list; node; node = node->next)
    {
        Split *split = (Split *)node->data;
        gnc_numeric amount = xaccSplitGetAmount (split);
        WorkItem *item = make_workitem (sack, amount.num, split, NULL);
        size_t work_size = g_hash_table_size (sack) + 1;
        GArray *workarray = g_array_sized_new
            (FALSE, FALSE, sizeof (WorkItem *), work_size);
        sack_data s_data = { workarray, sack, split,
            debugging_enabled, nc_progress, nc_list_length, label };

        g_array_append_val (workarray, item);
        g_hash_table_foreach (sack, (GHFunc) sack_foreach_func, &s_data);
        for (int i = 0; i < (int)work_size; i++)
        {
            WorkItem *a_item = g_array_index (workarray, WorkItem*, i);
            process_work (a_item, &s_data);
            g_free (a_item);
        }
        g_array_free (workarray, TRUE);
        nc_progress++;
        if (G_UNLIKELY ((clock () - start_ticks) > MAX_AUTOCLEAR_TICKS))
        {
            g_set_error (error, autoclear_quark (), AUTOCLEAR_OVERLOAD,
                         _("Too many uncleared splits"));
            goto skip_knapsack;
        }
        else if (G_UNLIKELY (g_hash_table_lookup (sack, (gpointer)toclear_value.num)
                             == DUP_LIST))
        {
            g_set_error (error, autoclear_quark (), AUTOCLEAR_MULTIPLE,
                         _("Cannot uniquely clear splits. Found multiple possibilities."));
            goto skip_knapsack;
        }
    }

    status_update (label, _("Cleaning up..."));

    toclear_list = (GList *)g_hash_table_lookup (sack, (gpointer)toclear_value.num);

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
