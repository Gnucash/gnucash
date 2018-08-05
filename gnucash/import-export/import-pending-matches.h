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
 \ *********************************************************************/
/** @addtogroup Import_Export
    @{ */
/** @file import-pending-matches.h
    @brief Tracking container for pending match status.
    @author Copyright (C) 2016 Jesse Olmer
*/
 
#ifndef IMPORT_PENDING_MATCHES_H
#define IMPORT_PENDING_MATCHES_H

#include <glib.h>
#include "import-backend.h"

typedef GHashTable GNCImportPendingMatches;

typedef enum _import_match_type {
    GNCImportPending_NONE,
    GNCImportPending_AUTO,
    GNCImportPending_MANUAL
} GNCImportPendingMatchType;

GNCImportPendingMatches * gnc_import_PendingMatches_new(void);

void gnc_import_PendingMatches_delete(GNCImportPendingMatches *map);

void
gnc_import_PendingMatches_add_match(GNCImportPendingMatches *map,
                                    GNCImportMatchInfo *match_info,
                                    gboolean selected_manually);

void
gnc_import_PendingMatches_remove_match(GNCImportPendingMatches *map,
                                       GNCImportMatchInfo *match_info,
                                       gboolean selected_manually);

GNCImportPendingMatchType
gnc_import_PendingMatches_get_match_type(GNCImportPendingMatches *map,
                                         GNCImportMatchInfo *match_info);

const char *
gnc_import_PendingMatches_get_type_str(GNCImportPendingMatchType type);

#endif
/** @} */
