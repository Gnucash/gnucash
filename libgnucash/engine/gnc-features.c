/********************************************************************\
 * gnc-features.c -- manage GnuCash features table                  *
 * Copyright (C) 2011 Derek Atkins <derek@ihtfp.com>                *
 * Copyright (C) 2012 Geert Janssens <geert@kobaltwit.be>           *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
\********************************************************************/

#include <config.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "qof.h"
#include "gnc-features.h"

typedef struct
{
    const gchar *key;
    const gchar *desc;
} gncFeature;

static GHashTable *features_table = NULL;
static gncFeature known_features[] =
{
    { GNC_FEATURE_CREDIT_NOTES, "Customer and vendor credit notes (requires at least GnuCash 2.5.0)" },
    { GNC_FEATURE_NUM_FIELD_SOURCE, "User specifies source of 'num' field'; either transaction number or split action (requires at least GnuCash 2.5.0)" },
    { GNC_FEATURE_KVP_EXTRA_DATA, "Extra data for addresses, jobs or invoice entries (requires at least GnuCash 2.6.4)" },
    { GNC_FEATURE_BOOK_CURRENCY, "User specifies a 'book-currency'; costs of other currencies/commodities tracked in terms of book-currency (requires at least GnuCash 2.7.0)" },
    { GNC_FEATURE_GUID_BAYESIAN, "Use account GUID as key for Bayesian data (requires at least GnuCash 2.6.12)" },
    { GNC_FEATURE_GUID_FLAT_BAYESIAN, "Use account GUID as key for bayesian data and store KVP flat (requires at least Gnucash 2.6.19)" },
    { GNC_FEATURE_SQLITE3_ISO_DATES, "Use ISO formatted date-time strings in SQLite3 databases (requires at least GnuCash 2.6.20)"},
    { GNC_FEATURE_REG_SORT_FILTER, "Store the register sort and filter settings in .gcm metadata file (requires at least GnuCash 3.3)"},
    { GNC_FEATURE_BUDGET_UNREVERSED, "Store budget amounts unreversed (i.e. natural) signs (requires at least Gnucash 3.8)"},
    { GNC_FEATURE_BUDGET_SHOW_EXTRA_ACCOUNT_COLS, "Show extra account columns in the Budget View (requires at least Gnucash 3.8)"},
    { NULL },
};

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

/********************************************************************\
\********************************************************************/

static void gnc_features_init ()
{
    gint i;

    if (features_table)
        return;

    features_table = g_hash_table_new (g_str_hash, g_str_equal);
    for (i = 0; known_features[i].key; i++)
        g_hash_table_insert (features_table,
                             g_strdup (known_features[i].key),
                             g_strdup (known_features[i].desc));
}

static void gnc_features_test_one(gpointer pkey, gpointer value,
                  gpointer data)
{
    const gchar *key = (const gchar*)pkey;
    const gchar *feature_desc = (const gchar*)value;
    GList **unknown_features;

    g_assert(data);
    unknown_features = (GList**) data;

    /* Check if this feature is in the known features list. */
    if (g_hash_table_lookup_extended (features_table, key, NULL, NULL))
        return;

    /* It is unknown, so add the description to the unknown features list: */
    g_assert(feature_desc);

    *unknown_features = g_list_prepend(*unknown_features,
                       (gpointer)feature_desc);
}

/* Check if the session requires features unknown to this version of GnuCash.
 *
 * Returns a message to display if we found unknown features, NULL if
 * we're okay.
 */
gchar *gnc_features_test_unknown (QofBook *book)
{

    GList* features_list = NULL;
    GHashTable *features_used = qof_book_get_features (book);

    /* Setup the known_features hash table */
    gnc_features_init();

    /* Iterate over the members of this frame for unknown features */
    g_hash_table_foreach (features_used, &gnc_features_test_one,
              &features_list);
    if (features_list)
    {
        GList *i;
        char* msg = g_strdup(_("This Dataset contains features not supported "
                       "by this version of GnuCash. You must use a "
                       "newer version of GnuCash in order to support "
                       "the following features:"
                                 ));

        for (i = features_list; i; i = i->next)
        {
            char *tmp = g_strconcat(msg, "\n* ", i->data, NULL);
            g_free (msg);
            msg = tmp;
        }

        g_list_free(features_list);
        return msg;
    }
    g_hash_table_unref (features_used);
    return NULL;
}

void gnc_features_set_used (QofBook *book, const gchar *feature)
{
    const gchar *description;

    g_return_if_fail (book);
    g_return_if_fail (feature);

    gnc_features_init();

    /* Can't set an unknown feature */
    description = g_hash_table_lookup (features_table, feature);
    if (!description)
    {
        PWARN("Tried to set unknown feature as used.");
        return;
    }

    qof_book_set_feature (book, feature, description);
}

struct CheckFeature
{
    gchar const * checked_feature;
    gboolean found;
};

static void gnc_features_check_feature_cb (gpointer pkey, gpointer value,
                  gpointer data)
{
    const gchar *key = (const gchar*)pkey;
    struct CheckFeature * check_data = data;
    g_assert(data);
    if (!g_strcmp0 (key, check_data->checked_feature))
        check_data->found = TRUE;
}

gboolean gnc_features_check_used (QofBook *book, const gchar * feature)
{
    GHashTable *features_used = qof_book_get_features (book);
    struct CheckFeature check_data = {feature, FALSE};
    /* Setup the known_features hash table */
    gnc_features_init();
    g_hash_table_foreach (features_used, &gnc_features_check_feature_cb, &check_data);
    g_hash_table_unref (features_used);
    return check_data.found;
}

