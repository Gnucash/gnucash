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

#include <vector>
#include <unordered_map>

#include <config.h>
#include <gtk/gtk.h>
#include <inttypes.h>
#include <Account.h>
#include <Split.h>
#include <Transaction.h>
#include <gnc-autoclear.h>

static QofLogModule log_module = GNC_MOD_GUI;

/* the following functions are used in window-autoclear: */

typedef enum
{
    AUTOCLEAR_OVERLOAD = 1,
    AUTOCLEAR_UNABLE,
    AUTOCLEAR_MULTIPLE,
    AUTOCLEAR_NOP,
} autoclear_error_type;

#define MAX_AUTOCLEAR_SECONDS 10

#define MAX_AUTOCLEAR_TICKS CLOCKS_PER_SEC * MAX_AUTOCLEAR_SECONDS

using SplitVec = std::vector<Split*>;

struct WorkItem
{
    gint64 reachable_amount;
    SplitVec splits_vector;
    WorkItem (const gint64 amount, SplitVec&& splits)
        : reachable_amount (amount), splits_vector(std::move(splits)){}
};

static void status_update (GtkLabel *label, gchar *status)
{
    if (!label) return;
    gtk_label_set_text (label, status);
    while (gtk_events_pending ())
        g_main_context_iteration (nullptr, FALSE);
}

static void looping_update_status (GtkLabel *label, guint nc_progress,
                                   guint nc_size, guint size)
{
    if (!label)
        return;

    auto text = g_strdup_printf (_("%u/%u splits, %u combinations produced"),
                                 nc_progress, nc_size, size);
    status_update (label, text);
    g_free (text);
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

static inline gint64
normalize_num (gnc_numeric amount, int denom)
{
    auto normalized = gnc_numeric_convert (amount, denom, GNC_HOW_RND_ROUND);
    return normalized.num;
};

gboolean
gnc_autoclear_get_splits (Account *account, gnc_numeric toclear_value,
                          time64 end_date,
                          GList **splits, GError **error, GtkLabel *label)
{
    SplitVec nc_vector {};
    std::unordered_map<gint64, SplitVec, Hasher, EqualFn> sack;
    guint nc_progress = 0;
    clock_t start_ticks;
    int commodity_scu;
    gboolean debugging_enabled = qof_log_check (G_LOG_DOMAIN, QOF_LOG_DEBUG);
    GQuark autoclear_quark = g_quark_from_static_string ("autoclear");
    gint64 toclear_normalized;

    g_return_val_if_fail (GNC_IS_ACCOUNT (account), FALSE);
    g_return_val_if_fail (splits != nullptr, FALSE);

    *splits = nullptr;

    nc_vector.reserve(g_list_length(xaccAccountGetSplitList(account)));

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

    nc_vector.shrink_to_fit ();

    if (gnc_numeric_zero_p (toclear_value))
    {
        g_set_error (error, autoclear_quark, AUTOCLEAR_NOP, "%s",
                     _("Account is already at Auto-Clear Balance."));
        goto skip_knapsack;
    }
    else if (nc_vector.empty ())
    {
        g_set_error (error, autoclear_quark, AUTOCLEAR_NOP, "%s",
                     _("No uncleared splits found."));
        goto skip_knapsack;
    }

    commodity_scu = gnc_commodity_get_fraction (xaccAccountGetCommodity (account));
    toclear_normalized = normalize_num (toclear_value, commodity_scu);
    start_ticks = clock ();
    sack.reserve(nc_vector.size() * 4);

    for (auto& split : nc_vector)
    {
        auto amount = xaccSplitGetAmount (split);
        auto amount_normalized = normalize_num (amount, commodity_scu);
        std::vector<WorkItem> workvector {};

        workvector.reserve (sack.size () + 1);
        if (sack.find (amount_normalized) != sack.end ())
            workvector.emplace_back (amount_normalized, SplitVec{});
        else
        {
            SplitVec new_splits = { split };
            workvector.emplace_back (amount_normalized, std::move(new_splits));
        }

        // printf ("new split. sack size = %ld\n", sack.size());

        for (auto& [map_value, map_splits] : sack)
        {
            gint64 new_value;
            if (__builtin_add_overflow (map_value, amount_normalized, &new_value))
            {
                g_set_error (error, autoclear_quark, AUTOCLEAR_OVERLOAD, "%s",
                             "Overflow error: Amount numbers are too large!");
                goto skip_knapsack;
            }
            if (map_splits.empty() ||
                sack.find (new_value) != sack.end ())
                workvector.emplace_back (new_value, SplitVec{});
            else
            {
                SplitVec new_splits{};
                new_splits.reserve(map_splits.size() + 1);
                new_splits = map_splits;
                new_splits.push_back (split);
                workvector.emplace_back (new_value, std::move(new_splits));
            }
        }

        for (auto& item : workvector)
            sack.insert_or_assign(item.reachable_amount, std::move(item.splits_vector));

        looping_update_status (label, ++nc_progress, nc_vector.size (), sack.size ());

        if (G_UNLIKELY ((clock () - start_ticks) > MAX_AUTOCLEAR_TICKS))
        {
            g_set_error (error, autoclear_quark, AUTOCLEAR_OVERLOAD, "%s",
                         _("Too many uncleared splits"));
            goto skip_knapsack;
        }

        auto try_toclear = sack.find (toclear_normalized);
        if (G_UNLIKELY (try_toclear != sack.end() &&
                        try_toclear->second.empty()))
        {
            g_set_error (error, autoclear_quark, AUTOCLEAR_MULTIPLE, "%s",
                         _("Cannot uniquely clear splits. Found multiple possibilities."));
            goto skip_knapsack;
        }
    }

    if (debugging_enabled)
        for (auto& [this_value, this_splits] : sack)
        {
            printf ("dump: %" PRId64 " = ", this_value);
            if (this_splits.empty())
                printf (" DUPE");
            else
                for (auto& s : this_splits)
                    printf (" [%5.2f]",
                            gnc_numeric_to_double (xaccSplitGetAmount (s)));
            printf ("\n");
        }

    /* Check solution */
    if (sack.find (toclear_normalized) == sack.end())
    {
        g_set_error (error, autoclear_quark, AUTOCLEAR_UNABLE, "%s",
                     _("The selected amount cannot be cleared."));
        goto skip_knapsack;
    }

    /* copy GList because std::unordered_map value will be freed */
    for (auto& s : sack[toclear_normalized])
        *splits = g_list_prepend (*splits, s);

 skip_knapsack:

    // this is the right time to display Cleaning up because when this
    // function completes, c++ destructors will scorch earth.
    status_update (label, _("Cleaning up..."));

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
