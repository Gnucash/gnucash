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

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "libqof/qof/qof.h"
#include "gnc-features.h"

typedef struct {
        const gchar *key;
        const gchar *desc;
} gncFeature;

static GHashTable *features_table = NULL;
static gncFeature known_features[] =
{
        { GNC_FEATURE_CREDIT_NOTES, "Customer and vendor credit notes (requires at least GnuCash 2.5.0)" },
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

static void gnc_features_test_one(const gchar *key, KvpValue *value, gpointer data)
{
    GList **unknown_features;
    gchar *feature_desc;

    g_assert(data);
    unknown_features = (GList**) data;

    /* Check if this feature is in the known features list. */
    if (g_hash_table_lookup_extended (features_table, key, NULL, NULL))
        return;

    /* It is unknown, so add the description to the unknown features list: */
    feature_desc = kvp_value_get_string(value);
    g_assert(feature_desc);

    *unknown_features = g_list_prepend(*unknown_features, feature_desc);
}

/* Check if the session requires features unknown to this version of GnuCash.
 *
 * Returns a message to display if we found unknown features, NULL if we're okay.
 */
gchar *gnc_features_test_unknown (QofBook *book)
{
    KvpFrame *frame = qof_book_get_slots (book);
    KvpValue *value;

    /* Setup the known_features hash table */
    gnc_features_init();

    g_assert(frame);
    value = kvp_frame_get_value(frame, "features");

    if (value)
    {
        GList* features_list = NULL;
        frame = kvp_value_get_frame(value);
        g_assert(frame);

        /* Iterate over the members of this frame for unknown features */
        kvp_frame_for_each_slot(frame, &gnc_features_test_one, &features_list);
        if (features_list)
        {
            GList *i;
            char* msg = g_strdup(
                            _("This Dataset contains features not supported by this "
                              "version of GnuCash.  You must use a newer version of "
                              "GnuCash in order to support the following features:"
                             ));

            for (i = features_list; i; i = i->next)
            {
                char *tmp = g_strconcat(msg, "\n* ", i->data, NULL);
                g_free (msg);
                msg = tmp;
            }

            g_free(msg);
            g_list_free(features_list);
            return msg;
        }
    }

    return NULL;
}

void gnc_features_set_used (QofBook *book, const gchar *feature)
{
    KvpFrame *frame;
    const gchar *description;
    gchar *kvp_path;

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

    frame = qof_book_get_slots (book);
    kvp_path = g_strconcat ("/features/", feature, NULL);
    kvp_frame_set_string (frame, kvp_path, description);
    qof_book_kvp_changed (book);


}
