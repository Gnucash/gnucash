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
/** @file import-match-picker.h
   @brief The transaction match picker dialog interface
   @author Copyright (C) 2002 Benoit Grégoire
*/

#ifndef GNC_GEN_MATCH_PICKER_H
#define GNC_GEN_MATCH_PICKER_H

#include "import-backend.h"

typedef struct _transpickerdialog GNCImportMatchPicker;

/************************************************************************
 * GUI Functions
 ************************************************************************/

/**
 * Run a match_picker dialog where the user should pick the best match for 'one' given
 * transaction, so that the selected-MatchInfo in the
 * given trans_info is updated accordingly. This functions will only
 * return after the user clicked Ok, Cancel, or Window-Close.
 *
 * The dialog uses the same functionality as the one created through
 * gnc_import_add_trans(), except that its two listviews are shown
 * above one another, and the listview of downloaded transactions
 * shows only one transaction, namely, the given trans_info.
 *
 * This function is used from the gnc-gen-transaction code.
 *
 * @param transaction_info The TransInfo for which the user is supposed to
 * pick a matching transaction. */
void
gnc_import_match_picker_run_and_close (GNCImportTransInfo *transaction_info);
/**@}*/



#endif
