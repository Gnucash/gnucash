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
#include <vector>
#include <unordered_map>

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

struct WorkItem
{
    gint64 reachable_amount;
    GList *list_of_splits;
    WorkItem (const gint64 amount, GList *splits)
        : reachable_amount (amount), list_of_splits(splits){}
};

static GList *DUP_LIST;

static void status_update (GtkLabel *label, gchar *status)
{
    if (!label) return;
    gtk_label_set_text (label, status);
    while (gtk_events_pending ())
        g_main_context_iteration (NULL,FALSE);
}

static clock_t next_update_tick;

static void looping_update_status (GtkLabel *label, guint nc_progress,
                                   guint nc_size, guint size)
{
    clock_t now;
    if (!label)
        return;
    now = clock ();
    if (G_UNLIKELY (now > next_update_tick))
    {
        gchar *text = g_strdup_printf ("%u/%u splits processed, %u combos",
                                       nc_progress, nc_size, size);
        status_update (label, text);
        g_free (text);
        next_update_tick = now + MAX_UPDATE_TICKS;
    }
}

gboolean
gnc_autoclear_get_splits (Account *account, gnc_numeric toclear_value,
                          time64 end_date,
                          GList **splits, GError **error, GtkLabel *label)
{
    GList *toclear_list = NULL;
    std::vector<Split*> nc_vector;
    std::unordered_map<gint64, GList*> sack;
    guint nc_progress = 0;
    clock_t start_ticks;
    gboolean debugging_enabled = qof_log_check (G_LOG_DOMAIN, QOF_LOG_DEBUG);

    g_return_val_if_fail (GNC_IS_ACCOUNT (account), FALSE);
    g_return_val_if_fail (splits != NULL, FALSE);

    DUP_LIST = g_list_prepend (NULL, NULL);
    *splits = nullptr;

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
            nc_vector.emplace_back (split);
    }

    if (gnc_numeric_zero_p (toclear_value))
    {
        g_set_error (error, autoclear_quark (), AUTOCLEAR_NOP,
                     _("Account is already at Auto-Clear Balance."));
        goto skip_knapsack;
    }
    else if (nc_vector.empty ())
    {
        g_set_error (error, autoclear_quark (), AUTOCLEAR_NOP,
                     _("No uncleared splits found."));
        goto skip_knapsack;
    }

    start_ticks = next_update_tick = clock ();
    for (auto& split : nc_vector)
    {
        gnc_numeric amount = xaccSplitGetAmount (split);
        gint64 amountnum = amount.num;
        std::vector<WorkItem> workvector = {};

        workvector.reserve (sack.size () + 1);
        workvector.emplace_back (amountnum,
                                 sack.find (amountnum) != sack.end () ?
                                 DUP_LIST : g_list_prepend (nullptr, split));
        // printf ("new split. sack size = %ld\n", sack.size());

        for (auto& [thisvalue, splits] : sack)
        {
            gint64 new_value = thisvalue + amountnum;
            workvector.emplace_back
                (new_value,
                 (splits == DUP_LIST || sack.find (new_value) != sack.end ())
                 ? DUP_LIST
                 : g_list_prepend (g_list_copy (splits), split));
            looping_update_status (label, nc_progress, nc_vector.size (),
                                   sack.size ());
        }

        for (auto& item : workvector)
        {
            auto existing = sack.find (item.reachable_amount);
            if (existing != sack.end())
            {
                if (debugging_enabled)
                    DEBUG ("removing existing for %" PRId64 "\n", item.reachable_amount);
                if (existing->second != DUP_LIST)
                    g_list_free (existing->second);
            }
            sack[item.reachable_amount] = item.list_of_splits;
        }
        nc_progress++;
        if (G_UNLIKELY ((clock () - start_ticks) > MAX_AUTOCLEAR_TICKS))
        {
            g_set_error (error, autoclear_quark (), AUTOCLEAR_OVERLOAD,
                         _("Too many uncleared splits"));
            goto skip_knapsack;
        }

        auto try_toclear = sack.find (toclear_value.num);
        if (try_toclear != sack.end() && try_toclear->second == DUP_LIST)
        {
            g_set_error (error, autoclear_quark (), AUTOCLEAR_MULTIPLE,
                         _("Cannot uniquely clear splits. Found multiple possibilities."));
            goto skip_knapsack;
        }
    }

    status_update (label, _("Cleaning up..."));

    /*
    for (auto& [thisvalue, splits] : sack)
    {
        printf ("dump: %" PRId64 " = ", thisvalue);
        if (splits == DUP_LIST)
            printf (" DUPE");
        else
            for (GList *n = splits; n; n = n->next)
                printf (" [%5.2f]", gnc_numeric_to_double (xaccSplitGetAmount ((Split*)n->data)));
        printf ("\n");
    }
    */

    /* Check solution */
    if (sack.find (toclear_value.num) == sack.end())
    {
        g_set_error (error, autoclear_quark (), AUTOCLEAR_UNABLE,
                     _("The selected amount cannot be cleared."));
        goto skip_knapsack;
    }
    else
        /* copy GList because std::unordered_map value will be freed */
        *splits = g_list_copy (sack[toclear_value.num]);

 skip_knapsack:

    for (const auto& [thisvalue, thissplits] : sack)
        if (thissplits != DUP_LIST)
            g_list_free (thissplits);
    g_list_free (DUP_LIST);

    status_update (label, NULL);

    return (*splits != NULL);
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
