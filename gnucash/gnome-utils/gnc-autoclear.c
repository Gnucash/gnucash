/********************************************************************
 * gnc-autoclear.c -- Knapsack algorithm functions                  *
 *                                                                  *
 * Copyright 2020 Cristian Klein <cristian@kleinlabs.eu>            *
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
#include "gncOwner.h"
#include "qof.h"
#include "gnc-autoclear.h"

/* the following functions are used in window-autoclear: */

#define MAXIMUM_SACK_SIZE 1000000

static gboolean
ght_gnc_numeric_equal(gconstpointer v1, gconstpointer v2)
{
    gnc_numeric n1 = *(gnc_numeric *)v1, n2 = *(gnc_numeric *)v2;
    return gnc_numeric_equal(n1, n2);
}

static guint
ght_gnc_numeric_hash(gconstpointer v1)
{
    gnc_numeric n1 = *(gnc_numeric *)v1;
    gdouble d1 = gnc_numeric_to_double(n1);
    return g_double_hash (&d1);
}

typedef struct _sack_foreach_data_t
{
    gnc_numeric split_value;
    GList *reachable_list;
} *sack_foreach_data_t;

static void sack_foreach_func(gpointer key, gpointer value, gpointer user_data)
{
    sack_foreach_data_t data = (sack_foreach_data_t) user_data;
    gnc_numeric thisvalue = *(gnc_numeric *) key;
    gnc_numeric reachable_value = gnc_numeric_add_fixed (thisvalue, data->split_value);
    gpointer new_value = g_malloc(sizeof(gnc_numeric));

    memcpy(new_value, &reachable_value, sizeof(gnc_numeric));
    data->reachable_list = g_list_prepend(data->reachable_list, new_value);
}

GList *
gnc_account_get_autoclear_splits (Account *account, gnc_numeric toclear_value,
                                  gchar **errmsg)
{
    GList *nc_list = NULL, *toclear_list = NULL;
    GHashTable *sack;
    gchar *msg = NULL;
    guint sack_size = 0;

    g_return_val_if_fail (GNC_IS_ACCOUNT (account), NULL);

    sack = g_hash_table_new_full (ght_gnc_numeric_hash, ght_gnc_numeric_equal,
                                  g_free, NULL);

    /* Extract which splits are not cleared and compute the amount we have to clear */
    for (GList *node = xaccAccountGetSplitList (account); node; node = node->next)
    {
        Split *split = (Split *)node->data;

        if (xaccSplitGetReconcile (split) == NREC)
            nc_list = g_list_prepend (nc_list, split);
        else
            toclear_value = gnc_numeric_sub_fixed
                (toclear_value, xaccSplitGetAmount (split));
    }

    if (gnc_numeric_zero_p (toclear_value))
    {
        msg = _("Account is already at Auto-Clear Balance.");
        goto skip_knapsack;
    }

    /* Run knapsack */
    /* Entries in the hash table are:
     *  - key   = amount to which we know how to clear (freed by GHashTable)
     *  - value = last split we used to clear this amount (not managed by GHashTable)
     */
    for (GList *node = nc_list; node; node = node->next)
    {
        Split *split = (Split *)node->data;
        gnc_numeric split_value = xaccSplitGetAmount (split);
        gpointer new_value = g_malloc(sizeof(gnc_numeric));

        struct _sack_foreach_data_t s_data[1];
        s_data->split_value = split_value;
        s_data->reachable_list = NULL;

        /* For each value in the sack, compute a new reachable value */
        g_hash_table_foreach (sack, sack_foreach_func, s_data);

        /* Add the value of the split itself to the reachable_list */
        memcpy(new_value, &split_value, sizeof(gnc_numeric));
        s_data->reachable_list = g_list_prepend
            (s_data->reachable_list, new_value);

        /* Add everything to the sack, looking out for duplicates */
        for (GList *s_node = s_data->reachable_list; s_node; s_node = s_node->next)
        {
            gnc_numeric *reachable_value = s_node->data;

            /* Check if it already exists */
            if (g_hash_table_lookup_extended (sack, reachable_value, NULL, NULL))
            {
                /* If yes, we are in trouble, we reached an amount
                   using two solutions */
                g_hash_table_insert (sack, reachable_value, NULL);
            }
            else
            {
                g_hash_table_insert (sack, reachable_value, split);
                sack_size++;

                if (sack_size > MAXIMUM_SACK_SIZE)
                {
                    msg = _("Too many uncleared splits");
                    goto skip_knapsack;
                }
            }
        }
        g_list_free (s_data->reachable_list);
    }

    /* Check solution */
    while (!gnc_numeric_zero_p (toclear_value))
    {
        Split *split = NULL;

        if (!g_hash_table_lookup_extended (sack, &toclear_value,
                                           NULL, (gpointer) &split))
        {
            msg = _("The selected amount cannot be cleared.");
            goto skip_knapsack;
        }

        if (!split)
        {
            msg = _("Cannot uniquely clear splits. Found multiple possibilities.");
            goto skip_knapsack;
        }

        toclear_list = g_list_prepend (toclear_list, split);
        toclear_value = gnc_numeric_sub_fixed (toclear_value,
                                               xaccSplitGetAmount (split));
    }

 skip_knapsack:
    g_hash_table_destroy (sack);
    g_list_free (nc_list);

    if (msg)
    {
        *errmsg = g_strdup (msg);
        g_list_free (toclear_list);
        return NULL;
    }

    *errmsg = NULL;
    return toclear_list;
}
