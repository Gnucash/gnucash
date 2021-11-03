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

using SplitVec = std::vector<Split*>;

struct WorkItem
{
    gint64 reachable_amount;
    SplitVec splits_vector;
    WorkItem (const gint64 amount, SplitVec splits)
        : reachable_amount (amount), splits_vector(splits){}
};

static void status_update (GtkLabel *label, gchar *status)
{
    if (!label) return;
    gtk_label_set_text (label, status);
    while (gtk_events_pending ())
        g_main_context_iteration (nullptr, FALSE);
}

static void looping_update_status (GtkLabel *label, guint nc_progress,
                                   guint nc_size, guint size,
                                   clock_t *next_update_tick)
{
    clock_t now;
    if (!label)
        return;
    now = clock ();
    if (G_UNLIKELY (now > *next_update_tick))
    {
        gchar *text = g_strdup_printf ("%u/%u splits processed, %u combos",
                                       nc_progress, nc_size, size);
        status_update (label, text);
        g_free (text);
        *next_update_tick = now + MAX_UPDATE_TICKS;
    }
}

class Hasher
{
public:
    size_t operator() (gint64 const& key) const
    {
        return key;
    }
};
class EqualFn
{
public:
    bool operator() (gint64 const& t1, gint64 const& t2) const
    {
        return (t1 == t2);
    }
};

gboolean
gnc_autoclear_get_splits (Account *account, gnc_numeric toclear_value,
                          time64 end_date,
                          GList **splits, GError **error, GtkLabel *label)
{
    SplitVec nc_vector {};
    std::unordered_map<gint64, SplitVec, Hasher, EqualFn> sack;
    guint nc_progress = 0;
    clock_t start_ticks, next_update_tick;
    gboolean debugging_enabled = qof_log_check (G_LOG_DOMAIN, QOF_LOG_DEBUG);
    GQuark autoclear_quark = g_quark_from_static_string ("autoclear");

    g_return_val_if_fail (GNC_IS_ACCOUNT (account), FALSE);
    g_return_val_if_fail (splits != nullptr, FALSE);

    *splits = nullptr;

    /* Extract which splits are not cleared and compute the amount we have to clear */
    for (GList *node = xaccAccountGetSplitList (account); node; node = node->next)
    {
        auto split = static_cast<Split*> (node->data);

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
            nc_vector.push_back (split);
    }

    if (gnc_numeric_zero_p (toclear_value))
    {
        g_set_error (error, autoclear_quark, AUTOCLEAR_NOP,
                     _("Account is already at Auto-Clear Balance."));
        goto skip_knapsack;
    }
    else if (nc_vector.empty ())
    {
        g_set_error (error, autoclear_quark, AUTOCLEAR_NOP,
                     _("No uncleared splits found."));
        goto skip_knapsack;
    }

    start_ticks = next_update_tick = clock ();

    for (auto& split : nc_vector)
    {
        auto amount = xaccSplitGetAmount (split);
        std::vector<WorkItem> workvector {};

        workvector.reserve (sack.size () + 1);
        if (sack.find (amount.num) != sack.end ())
            workvector.emplace_back (amount.num, static_cast<SplitVec>(1));
        else
        {
            SplitVec new_splits = { split };
            workvector.emplace_back (amount.num, new_splits);
        }

        // printf ("new split. sack size = %ld\n", sack.size());

        for (auto& [map_value, map_splits] : sack)
        {
            gint64 new_value;
            if (__builtin_add_overflow (map_value, amount.num, &new_value))
            {
                g_set_error (error, autoclear_quark, AUTOCLEAR_OVERLOAD,
                             "Overflow error: Amount numbers are too large!");
                goto skip_knapsack;
            }
            if (map_splits == static_cast<SplitVec>(1) ||
                sack.find (new_value) != sack.end ())
                workvector.emplace_back (new_value, static_cast<SplitVec>(1));
            else
            {
                auto new_splits = map_splits;
                new_splits.push_back (split);
                workvector.emplace_back (new_value, new_splits);
            }
            looping_update_status (label, nc_progress, nc_vector.size (),
                                   sack.size (), &next_update_tick);
        }

        for (auto& item : workvector)
            sack[item.reachable_amount] = item.splits_vector;

        ++nc_progress;

        if (G_UNLIKELY ((clock () - start_ticks) > MAX_AUTOCLEAR_TICKS))
        {
            g_set_error (error, autoclear_quark, AUTOCLEAR_OVERLOAD,
                         _("Too many uncleared splits"));
            goto skip_knapsack;
        }

        auto try_toclear = sack.find (toclear_value.num);
        if (G_UNLIKELY (try_toclear != sack.end() &&
                        try_toclear->second == static_cast<SplitVec>(1)))
        {
            g_set_error (error, autoclear_quark, AUTOCLEAR_MULTIPLE,
                         _("Cannot uniquely clear splits. Found multiple possibilities."));
            goto skip_knapsack;
        }
    }

    status_update (label, _("Cleaning up..."));

    if (debugging_enabled)
        for (auto& [this_value, this_splits] : sack)
        {
            printf ("dump: %" PRId64 " = ", this_value);
            if (this_splits == static_cast<SplitVec>(1))
                printf (" DUPE");
            else
                for (auto& s : this_splits)
                    printf (" [%5.2f]",
                            gnc_numeric_to_double (xaccSplitGetAmount (s)));
            printf ("\n");
        }

    /* Check solution */
    if (sack.find (toclear_value.num) == sack.end())
    {
        g_set_error (error, autoclear_quark, AUTOCLEAR_UNABLE,
                     _("The selected amount cannot be cleared."));
        goto skip_knapsack;
    }

    /* copy GList because std::unordered_map value will be freed */
    for (auto& s : sack[toclear_value.num])
        *splits = g_list_prepend (*splits, s);

 skip_knapsack:

    status_update (label, nullptr);

    return (*splits != nullptr);
}


GList *
gnc_account_get_autoclear_splits (Account *account, gnc_numeric toclear_value,
                                  gchar **errmsg)
{
    GError *error = nullptr;
    GList *splits = nullptr;

    gnc_autoclear_get_splits (account, toclear_value, INT64_MAX,
                              &splits, &error, nullptr);

    if (error)
    {
        *errmsg = g_strdup (error->message);
        g_error_free (error);
        return nullptr;
    }

    *errmsg = nullptr;
    return splits;
}
