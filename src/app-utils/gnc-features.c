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

#include "gnc-engine.h"
#include "gnc-features.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

/********************************************************************\
\********************************************************************/
static void features_test(const gchar *key, KvpValue *value, gpointer data)
{
    GList** unknown_features = (GList**) data;
    char* feature_desc;

    g_assert(data);

    /* XXX: test if 'key' is an unknown feature. */

    /* Yes, it is unknown, so add the description to the list: */
    feature_desc = kvp_value_get_string(value);
    g_assert(feature_desc);

    *unknown_features = g_list_prepend(*unknown_features, feature_desc);
}

/*
 * Right now this is done by a KVP check for a features table.
 * Currently we don't know about any features, so the mere
 * existence of this KVP frame means we have a problem and
 * need to tell the user.
 *
 * returns a message to display if we found unknown features, NULL if we're okay.
 */
gchar *test_unknown_features(QofSession* new_session)
{
    KvpFrame *frame = qof_book_get_slots (qof_session_get_book (new_session));
    KvpValue *value;

    g_assert(frame);
    value = kvp_frame_get_value(frame, "features");

    if (value)
    {
        GList* features_list = NULL;
        frame = kvp_value_get_frame(value);
        g_assert(frame);

        /* Iterate over the members of this frame for unknown features */
        kvp_frame_for_each_slot(frame, &features_test, &features_list);
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
                char *tmp = g_strconcat(msg, "\n* ", _(i->data), NULL);
                g_free (msg);
                msg = tmp;
            }

            g_list_free(features_list);
            return msg;
        }
    }

    return NULL;
}
