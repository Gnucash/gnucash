/********************************************************************\
 * Transaction-matcher.h --                                         *
 * See file generic-import-design.txt for                           *
 * description                                                      *
 *                        (GnuCash)                                 *
 * Copyright (C) 2002 Benoit Grégoire <bock@step.polymtl.ca>        *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef TRANSACTION_MATCHER_H
#define TRANSACTION_MATCHER_H

#include "Transaction.h"

typedef struct _transactioninfo GNCImportTransInfo;
typedef struct _matchinfo GNCImportMatchInfo;
/* If you modify this, modify get_next_action */
typedef enum _action {
  GNCImport_SKIP,
  GNCImport_ADD,
  GNCImport_CLEAR,
  GNCImport_REPLACE,
  GNCImport_LAST_ACTION,
  GNCImport_INVALID_ACTION
} GNCImportAction;

/* Your import module should create a new transaction in the current book,
   add as many splits as it knows about, and associate each split with an 
   account.  It should then call gnc_import_add_trans() with that transaction
   if a transaction with the same online_id kvp_frame exists in any of the
   transaction's split's accounts, the transaction will be destroyed.  
   Otherwise it will be added.
   Not yet implemented:  GUI to balance the transaction using heuristics
*/
void gnc_import_add_trans (Transaction *trans);

/* Checks whether the given transaction's online_id already exists in
 * its parent account. The given transaction has to be open for
 * editing. If a matching online_id exists, the transaction is
 * destroyed and TRUE is returned, otherwise FALSE is returned.  */
gboolean gnc_import_exists_online_id (Transaction *trans);

/* Iterate through all splits of the originating account of the given
   transaction, find all matching splits there, and store them in the
   GNCImportTransInfo structure. */
void 
gnc_import_find_split_matches(GNCImportTransInfo *transaction_info,
			      gint process_threshold);

/* Allocated a new TransInfo object, with the Transaction trans
 * already stored there. */
GNCImportTransInfo *
gnc_import_TransInfo_new (Transaction *trans);
/* Destructors */
void gnc_import_TransInfo_delete (GNCImportTransInfo *info);

/* Iterates through all splits of the originating account of
 * trans_info. Sorts the resulting list and sets the selected_match
 * and action fields in the trans_info. */
void 
gnc_import_TransInfo_init_matches (GNCImportTransInfo *trans_info,
				   gint clear_threshold, 
				   gint add_threshold,
				   gint process_threshold);




/* Returns the stored list of possible matches. */
GList *gnc_import_TransInfo_get_match_list (const GNCImportTransInfo *info);
/* Returns the transaction of this TransInfo. */
Transaction *gnc_import_TransInfo_get_trans (const GNCImportTransInfo *info);
/* Returns the currently selected match in this TransInfo. */
GNCImportMatchInfo *
gnc_import_TransInfo_get_selected_match (const GNCImportTransInfo *info);
/* Sets the currently selected match in this TransInfo. */
void
gnc_import_TransInfo_set_selected_match (GNCImportTransInfo *info,
				       GNCImportMatchInfo *match);
GNCImportAction
gnc_import_TransInfo_get_action (GNCImportTransInfo *info);
void
gnc_import_TransInfo_set_action (GNCImportTransInfo *info, 
				 GNCImportAction action);
Account *
gnc_import_TransInfo_get_destacc (const GNCImportTransInfo *info);
void 
gnc_import_TransInfo_set_destacc (GNCImportTransInfo *info, Account *acc);


/* Get the transaction of this MatchInfo. */
Split * 
gnc_import_MatchInfo_get_split (const GNCImportMatchInfo * info);


/*Transaction who's best match probability is equal or higher than
  this will reconcile their best match by default */
#define DEFAULT_CLEAR_THRESHOLD 5
/*Transaction who's best match probability is below or equal to 
  this will be added as new by default */
#define DEFAULT_ADD_THRESHOLD 2
/*Transaction's match probability must be at least this much to be 
  displayed in the match list.  Dont set this to 0 except for 
  debugging purposes, otherwise all transactions of every accounts 
  will be shown in the list */
#define DEFAULT_DISPLAY_THRESHOLD 1


#endif
