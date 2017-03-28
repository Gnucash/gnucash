/********************************************************************\
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
\********************************************************************/
/** @addtogroup Import_Export
    @{ */
/** @internal
    @file import-pending-matches.c
    @brief The pending match container implementation to track import reconciles.
    @author Copyright (C) 2016 Jesse Olmer
*/

#include "config.h"
#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h> /* for references in import-backend.h */
#include "guid.h"

#include "import-pending-matches.h"
#include "import-backend.h"

typedef struct _pending_matches
{
    gint num_manual_matches;
    gint num_auto_matches;
} GNCPendingMatches;

GNCImportPendingMatches *gnc_import_PendingMatches_new (void)
{
    /* Can't use guid_hash_table_new() here since we want values to be taken
       care of for us on destroy. */
    return g_hash_table_new_full (guid_hash_to_guint,
                                  guid_g_hash_table_equal,
                                  g_free,
                                  g_free
                                 );
}

void gnc_import_PendingMatches_delete (GNCImportPendingMatches *map)
{
    g_return_if_fail (map);

    g_hash_table_destroy (map);
}

static const GncGUID *
gnc_import_PendingMatches_get_key (GNCImportMatchInfo *match_info)
{
    Split *split;
    const GncGUID *match_guid;

    g_return_val_if_fail (match_info, NULL);

    split = gnc_import_MatchInfo_get_split (match_info);
    match_guid = qof_instance_get_guid (split);

    return match_guid;
}

static GNCPendingMatches *
gnc_import_PendingMatches_get_value (GNCImportPendingMatches *map,
                                     GNCImportMatchInfo *match_info)
{
    GNCPendingMatches *pending_matches;
    const GncGUID *match_guid;

    g_return_val_if_fail (map, NULL);
    g_return_val_if_fail (match_info, NULL);

    match_guid = gnc_import_PendingMatches_get_key (match_info);

    pending_matches = g_hash_table_lookup (map, match_guid);

    return pending_matches;
}

void
gnc_import_PendingMatches_add_match (GNCImportPendingMatches *map,
                                     GNCImportMatchInfo *match_info,
                                     gboolean selected_manually)
{
    GNCPendingMatches *pending_matches;
    const GncGUID *match_guid;
    GncGUID *key;

    g_return_if_fail (map);
    g_return_if_fail (match_info);


    pending_matches = gnc_import_PendingMatches_get_value (map, match_info);
    match_guid = gnc_import_PendingMatches_get_key (match_info);

    if (pending_matches == NULL)
    {
        pending_matches = g_new0 (GNCPendingMatches, 1);
        key = g_new (GncGUID, 1);
        *key = *match_guid;
        g_hash_table_insert (map, key, pending_matches);
    }

    if (selected_manually)
    {
        pending_matches->num_manual_matches++;
    }
    else
    {
        pending_matches->num_auto_matches++;
    }
}

void
gnc_import_PendingMatches_remove_match (GNCImportPendingMatches *map,
                                        GNCImportMatchInfo *match_info,
                                        gboolean selected_manually)
{
    GNCPendingMatches *pending_matches;

    g_return_if_fail (map);
    g_return_if_fail (match_info);

    pending_matches = gnc_import_PendingMatches_get_value (map, match_info);

    g_return_if_fail (pending_matches);

    if (selected_manually)
    {
        pending_matches->num_manual_matches--;
    }
    else
    {
        pending_matches->num_auto_matches--;
    }

    if (pending_matches->num_auto_matches == 0 &&
        pending_matches->num_manual_matches == 0)
    {
        /* Key & Value are freed for us */
        g_hash_table_remove (map,
                             gnc_import_PendingMatches_get_key (match_info));
    }
}

GNCImportPendingMatchType
gnc_import_PendingMatches_get_match_type (GNCImportPendingMatches *map,
                                          GNCImportMatchInfo *match_info)
{
    GNCPendingMatches *pending_matches;

    g_return_val_if_fail (map, GNCImportPending_NONE);
    g_return_val_if_fail (match_info, GNCImportPending_NONE);

    pending_matches = gnc_import_PendingMatches_get_value (map, match_info);

    if (pending_matches == NULL)
    {
        return GNCImportPending_NONE;
    }

    if (pending_matches->num_manual_matches > 0)
    {
        return GNCImportPending_MANUAL;
    }

    if (pending_matches->num_auto_matches > 0)
    {
        return GNCImportPending_AUTO;
    }

    g_assert_not_reached();
}

const char *
gnc_import_PendingMatches_get_type_str (GNCImportPendingMatchType type)
{
    switch (type)
    {
        case GNCImportPending_NONE:
            return _("None");
        case GNCImportPending_MANUAL:
            return _("Manual");
        case GNCImportPending_AUTO:
            return _("Auto");
        default:
            g_assert_not_reached();
            return NULL;
    }
}
