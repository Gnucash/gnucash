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

#include <config.h>
#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h> /* for references in import-backend.h */
#include "guid.h"

#include "import-pending-matches.hpp"
#include "import-backend.h"

[[maybe_unused]] static QofLogModule log_module = GNC_MOD_IMPORT;

static const GncGUID *
gnc_import_PendingMatches_get_key (GNCImportMatchInfo *match_info)
{
    g_return_val_if_fail (match_info, NULL);
    return qof_instance_get_guid (gnc_import_MatchInfo_get_split (match_info));
}

void
gnc_import_PendingMatches_add_match (GNCImportPendingMatches& map,
                                     GNCImportMatchInfo *match_info,
                                     gboolean selected_manually)
{
    g_return_if_fail (match_info);

    auto match_guid = gnc_import_PendingMatches_get_key (match_info);
    auto& pending_matches = map[match_guid];

    if (selected_manually)
        pending_matches.m_num_manual_matches++;
    else
        pending_matches.m_num_auto_matches++;
}

void
gnc_import_PendingMatches_remove_match (GNCImportPendingMatches& map,
                                        GNCImportMatchInfo *match_info,
                                        gboolean selected_manually)
{
    g_return_if_fail (match_info);

    auto match_guid = gnc_import_PendingMatches_get_key (match_info);

    auto iter = map.find (match_guid);
    g_return_if_fail (iter != map.end());

    auto& pending_matches = iter->second;

    if (selected_manually)
        pending_matches.m_num_manual_matches--;
    else
        pending_matches.m_num_auto_matches--;

    if (pending_matches.all_zero())
        map.erase (iter);
}

GNCImportPendingMatchType
gnc_import_PendingMatches_get_match_type (const GNCImportPendingMatches& map,
                                          GNCImportMatchInfo *match_info)
{
    g_return_val_if_fail (match_info, GNCImportPending_NONE);

    auto match_guid = gnc_import_PendingMatches_get_key (match_info);

    auto iter = map.find (match_guid);
    if (iter == map.end())
        return GNCImportPending_NONE;

    const auto& pending_matches = iter->second;
    if (pending_matches.m_num_manual_matches > 0)
        return GNCImportPending_MANUAL;

    if (pending_matches.m_num_auto_matches > 0)
        return GNCImportPending_AUTO;

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
