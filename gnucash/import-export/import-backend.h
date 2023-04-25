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

#ifndef IMPORT_BACKEND_H
#define IMPORT_BACKEND_H

#include "Transaction.h"
#include "import-settings.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _transactioninfo GNCImportTransInfo;
typedef struct _selected_match_info GNCImportSelectedMatchInfo;
typedef struct _matchinfo
{
    Transaction * trans;
    Split * split;
    /*GNC_match_probability probability;*/
    gint probability;
    gboolean update_proposed;
} GNCImportMatchInfo;

typedef struct _lsplitinfo
{
    gnc_numeric price;
    const char *action;
    const char *memo;
    gnc_numeric amount;
    Account *account;
    char rec_state;
    time64 rec_date;
} GNCImportLastSplitInfo;

typedef enum _action
{
    GNCImport_SKIP,
    GNCImport_ADD,
    GNCImport_CLEAR,
    GNCImport_UPDATE,
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
gboolean gnc_import_exists_online_id (Transaction *trans, GHashTable* acct_id_hash);

/** Evaluates the match between trans_info and split using the provided parameters.
 *
 * @param trans_info The TransInfo for the imported transaction
 *
 * @param split The register split that should be evaluated for a match.
 *
 * @param display_threshold Minimum match score to include split in the list of matches.
 *
 * @param date_threshold Maximum number of days a match considered likely.
 *
 * @param date_not_threshold Minimum number of days a match is considered unlikely.
 *
 * @param fuzzy_amount_difference Maximum amount difference to consider the match good.
 */
void split_find_match (GNCImportTransInfo * trans_info,
                       Split * split,
                       gint display_threshold,
                       gint date_threshold,
                       gint date_not_threshold,
                       double fuzzy_amount_difference);

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
 * Each successful match is also stored in the match map of the given
 * account, or if that argument is NULL, in the match map of each
 * originating account.
 *
 * @param base_acc The account where each match should be
 * stored. May be NULL, in which case each originating account
 * will be used.
 *
 * @param trans_info The ImportTransInfo item to process.
 *
 * @return TRUE if the item has been processed.
 */
gboolean
gnc_import_process_trans_item (Account *base_acc,
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
 * account's match map for automated destination account matching. The
 * given account may be NULL, in which case the match map of the
 * originating account will be used.
 *
 * @param trans The transaction that this TransInfo should work with.
 *
 * @param base_acc Account that will provide the match map to lookup a destination
 * account. This may be NULL, in which case the match map of the
 * originating account will be used. */
GNCImportTransInfo* gnc_import_TransInfo_new(Transaction* trans, Account* base_acc);

/** Destructor */
void gnc_import_TransInfo_delete (GNCImportTransInfo *info);

/** Returns the stored list of possible matches. */
GList *gnc_import_TransInfo_get_match_list (const GNCImportTransInfo *info);

/** Remove the first match in the list of possible matches  */
void gnc_import_TransInfo_remove_top_match (GNCImportTransInfo *info);

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
gnc_import_TransInfo_set_selected_match_info (GNCImportTransInfo *info,
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

/** Returns if the currently selected destination account for auto-matching was selected by the user. */
gboolean
gnc_import_TransInfo_get_destacc_selected_manually (const GNCImportTransInfo *info);


/** Returns the reference id for this TransInfo. */
guint32
gnc_import_TransInfo_get_ref_id (const GNCImportTransInfo *info);

/** Set the reference id for this TransInfo. */
void
gnc_import_TransInfo_set_ref_id (GNCImportTransInfo *info,
                                 guint32 ref_id);

/** Returns the exchange rate for this TransInfo. */
gnc_numeric
gnc_import_TransInfo_get_price (const GNCImportTransInfo *info);

/** Set the exchange rate for this TransInfo. */
void
gnc_import_TransInfo_set_price (GNCImportTransInfo *info,
                                gnc_numeric lprice);

/** Returns the destination split amount for this TransInfo. */
gnc_numeric
gnc_import_TransInfo_get_dest_amount (const GNCImportTransInfo *info);

/** Returns the destination split value for this TransInfo. */
gnc_numeric
gnc_import_TransInfo_get_dest_value (const GNCImportTransInfo *info);

/** Sets additional parameters to be used to generate the closing split */
void
gnc_import_TransInfo_set_last_split_info (GNCImportTransInfo *info,
                                          GNCImportLastSplitInfo *lsplit);

/** Set the append_text for this TransInfo. */
void
gnc_import_TransInfo_set_append_text (GNCImportTransInfo *info,
                                           gboolean append_text);

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

#ifdef __cplusplus
}
#endif

#endif
/** @} */


