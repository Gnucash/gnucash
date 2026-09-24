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

#ifdef __cplusplus
extern "C" {
#endif

#include "import-backend.h"
#include "import-pending-matches.h"

typedef struct _transpickerdialog GNCImportMatchPicker;
typedef void (*GNCImportMatchPickerDoneCB) (GNCImportTransInfo *transaction_info,
                                            gpointer user_data);

/************************************************************************
 * GUI Functions
 ************************************************************************/

/**
 * Present a match_picker dialog where the user should pick the best match for one
 * given transaction. The selected MatchInfo is updated before @a done_cb is called.
 * The call returns immediately; the dialog result is delivered asynchronously.
 * The returned handle is valid only until @a done_cb runs. A caller that still
 * owns it may instead call gnc_import_match_picker_cancel(); that consumes and
 * invalidates the handle, and likewise delivers @a done_cb exactly once.
 *
 * The dialog uses the same functionality as the one created through
 * gnc_import_add_trans(), except that its two listviews are shown
 * above one another, and the listview of downloaded transactions
 * shows only one transaction, namely, the given trans_info.
 *
 * This function is used from the gnc-gen-transaction code.
 *
 * @param parent The parent widget
 * @param transaction_info The TransInfo for which the user is supposed to
 * pick a matching transaction.
 * @param pending_matches List of transactions */
GNCImportMatchPicker *
gnc_import_match_picker_run (GtkWidget *parent,
                             GNCImportTransInfo *transaction_info,
                             GNCImportPendingMatches *pending_matches,
                             GNCImportMatchPickerDoneCB done_cb,
                             gpointer user_data);

/** Cancel and destroy an outstanding asynchronous match-picker before the
 * borrowed transaction and pending-match data are released. @a matcher is
 * consumed and must not be reused after this call. */
void gnc_import_match_picker_cancel (GNCImportMatchPicker *matcher);
/**@}*/


#ifdef __cplusplus
}
#endif

#endif
