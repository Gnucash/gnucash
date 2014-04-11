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
/** @file import-backend.h
    @brief Generic importer backend interface
    @author Copyright (C) 2002 Benoit Grégoire
    @author Christian Stimming
    @author Copyright (c) 2006 David Hampton <hampton@employees.org>
*/
 
#ifndef TRANSACTION_MATCHER_H
#define TRANSACTION_MATCHER_H

#include "Transaction.h"
#include "import-match-map.h"
#include "import-settings.h"

typedef struct _transactioninfo GNCImportTransInfo;
typedef struct _matchinfo GNCImportMatchInfo;

typedef enum _action {
  GNCImport_SKIP,
  GNCImport_ADD,
  GNCImport_CLEAR,
  GNCImport_EDIT,
  GNCImport_LAST_ACTION,
  GNCImport_INVALID_ACTION
} GNCImportAction;

/************************************************************************
 ************************************************************************/
/** @name Non-GUI Functions */
/*@{*/

/** Checks whether the given transaction's online_id already exists in
 * its parent account. The given transaction has to be open for
 * editing. If a matching online_id exists, the transaction is
 * destroyed (!) and TRUE is returned, otherwise FALSE is returned.
 *
 * @param trans The transaction for which to check for an existing
 * online_id. */
gboolean gnc_import_exists_online_id (Transaction *trans);

/** Iterate through all splits of the originating account of the given
 * transaction, find all matching splits there, and store them in the
 * GNCImportTransInfo structure.
 *
 * @param trans_info The TransInfo for which the corresponding
 * matching existing transactions should be found.
 *
 * @param process_threshold Each match whose heuristics are smaller
 * than this value is totally ignored.
 *
 * @param fuzzy_amount_difference For fuzzy amount matching, a certain
 * fuzzyness in the matching amount is allowed up to this value. May
 * be e.g. 3.00 dollars for ATM fees, or 0.0 if you only want to allow
 * exact matches. 
 *
 * @param match_date_hardlimit The number of days that a matching
 * split may differ from the given transaction before it is discarded
 * immediately. In other words, any split that is more distant from
 * the given transaction than this match_date_hardlimit days will be
 * ignored altogether. For use cases without paper checks (e.g. HBCI),
 * values like 14 (days) might be appropriate, whereas for use cases
 * with paper checks (e.g. OFX, QIF), values like 42 (days) seem more
 * appropriate. 
 */
void gnc_import_find_split_matches(GNCImportTransInfo *trans_info,
				   gint process_threshold, 
				   double fuzzy_amount_difference,
				   gint match_date_hardlimit);

/** Iterates through all splits of the originating account of
 * trans_info. Sorts the resulting list and sets the selected_match
 * and action fields in the trans_info. 
 *
 * @param trans_info The TransInfo for which the matches should be
 * found, sorted, and selected. 
 *
 * @param settings The structure that holds all the user preferences.
 */
void 
gnc_import_TransInfo_init_matches (GNCImportTransInfo *trans_info,
				   GNCImportSettings *settings);

/** This function is intended to be called when the importer dialog is
 * finished. It should be called once for each imported transaction
 * and processes each ImportTransInfo according to its selected action:
 * For GNCImport_ADD, the transaction is added etc. etc. 
 *
 * Each succesful match is also stored in the given ImportMatchMap,
 * or, if that argument is NULL, in the ImportMatchMap of each
 * originating account.
 *
 * @param matchmap The ImportMatchMap where each match should be
 * stored. May be NULL, in which case the ImportMatchMap of each
 * account will be used.
 *
 * @param trans_info The ImportTransInfo item to process.
 *
 * @return TRUE if the item has been processed.
 */
gboolean
gnc_import_process_trans_item (GncImportMatchMap *matchmap,
			       GNCImportTransInfo *trans_info);

/** This function generates a new pixmap representing a match score.
    It is a series of vertical bars of different colors.  
    -Below or at the add_threshold the bars are red
    -Above or at the clear_threshold the bars are green
    -Between the two threshold the bars are yellow
 
    @param score The score for which to generate a pixmap.

    @param settings The user settings from which to get the threshold

    @param widget The parent widget in which the pixmap will eventually
    be added.  Will be used to generate the colormap.
 */
GdkPixbuf* gen_probability_pixbuf (gint score, 
				   GNCImportSettings *settings,
				   GtkWidget * widget);

/*@}*/


/************************************************************************
 *   Getter/Setter Functions for the Data Types. 
 ************************************************************************/

/** @name Getters/Setters for GNCImportTransInfo */
/*@{*/

/** Allocates a new TransInfo object, with the Transaction 'trans'
 * already stored in there. Also, this already checks the
 * ImportMatchMap for automated destination account matching. The
 * given MatchMap may be NULL, in which case the ImportMatchMap of the
 * originating account will be used.
 *
 * @param trans The transaction that this TransInfo should work with.
 *
 * @param matchmap MatchMap used for automated destination account
 * choosing. This may be NULL, in which case the MatchMap of the
 * originating account will be used. */
GNCImportTransInfo *
gnc_import_TransInfo_new (Transaction *trans, GncImportMatchMap *matchmap);

/** Destructor */
void gnc_import_TransInfo_delete (GNCImportTransInfo *info);

/** Returns the stored list of possible matches. */
GList *gnc_import_TransInfo_get_match_list (const GNCImportTransInfo *info);

/** Returns the transaction of this TransInfo. */
Transaction *gnc_import_TransInfo_get_trans (const GNCImportTransInfo *info);

/** Returns if the transaction stored in the TransInfo is currently balanced. */
gboolean gnc_import_TransInfo_is_balanced (const GNCImportTransInfo *info);

/** Returns the first split of the transaction of this TransInfo. */
Split *gnc_import_TransInfo_get_fsplit (const GNCImportTransInfo *info);

/** Returns the currently selected match in this TransInfo. */
GNCImportMatchInfo *
gnc_import_TransInfo_get_selected_match (const GNCImportTransInfo *info);

/** Sets the currently selected match in this TransInfo.
    @param selected_manually TRUE or FALSE; Was this match set as a result of a selection 
    by the user or by an algorithm?
*/
void
gnc_import_TransInfo_set_selected_match (GNCImportTransInfo *info,
					 GNCImportMatchInfo *match,
					 gboolean selected_manually);

/** Returns if the currently selected match was selected by the user. */
gboolean
gnc_import_TransInfo_get_match_selected_manually (const GNCImportTransInfo *info);

/** Returns the currently selected action for this TransInfo. */
GNCImportAction
gnc_import_TransInfo_get_action (const GNCImportTransInfo *info);

/** Set the action for this TransInfo. Also sets the previous action. */
void
gnc_import_TransInfo_set_action (GNCImportTransInfo *info, 
				 GNCImportAction action);

/** Returns the 'other account' of this transaction. May return NULL. */
Account *
gnc_import_TransInfo_get_destacc (const GNCImportTransInfo *info);

/** Set the 'other account' of this transaction (used for auto-balance if needed). May be set to NULL.
    @param selected_manually TRUE or FALSE; Was this account set as a result of a selection 
    by the user or by an algorithm?
*/
void 
gnc_import_TransInfo_set_destacc (GNCImportTransInfo *info,
				  Account *acc,
				  gboolean selected_manually);

/** Try to automatch a given transaction to a destination account */
gboolean
gnc_import_TransInfo_refresh_destacc (GNCImportTransInfo *transaction_info,
				      GncImportMatchMap *matchmap);

/** Returns if the currently selected destination account for auto-matching was selected by the user. */
gboolean
gnc_import_TransInfo_get_destacc_selected_manually (const GNCImportTransInfo *info);

/**@}*/

/** @name Getters/Setters for GNCImportMatchInfo */
/**@{*/

/** Get the split ('this-side split') of this MatchInfo. */
Split * 
gnc_import_MatchInfo_get_split (const GNCImportMatchInfo * info);


/** Get the probability (confidence level) of this MatchInfo. 
@param info Can be NULL, in which case the function returns 0*/
gint 
gnc_import_MatchInfo_get_probability (const GNCImportMatchInfo * info);
/**@}*/

#endif
/** @} */


