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
/** @file import-match-map.h
    @brief Generic import mapper service, maps strings->accounts
    *
    An import mapper service that stores Account Maps for the
    generic importer.  This allows importers to map various
    "strings" to Gnucash accounts in a generic manner.
    @author Copyright (C) 2002,2003 Derek Atkins <derek@ihtfp.com>
 */
#ifndef GNC_IMPORT_MATCH_MAP_H
#define GNC_IMPORT_MATCH_MAP_H

typedef struct _GncImportMatchMap GncImportMatchMap;

#include "Account.h"
#include "gnc-book.h"

/** @{
Obtain an ImportMatchMap object from an Account or a Book */
GncImportMatchMap * gnc_imap_create_from_account (Account *acc);
GncImportMatchMap * gnc_imap_create_from_book (QofBook *book);
/*@}*/

/** Destroy an import map. But all stored entries will still continue
 to exist in the underlying kvp frame of the account or book. */
void gnc_imap_destroy (GncImportMatchMap *imap);

/** Clear an import map -- this removes ALL entries in the map */
void gnc_imap_clear (GncImportMatchMap *imap);

/** Look up an Account in the map */
Account* gnc_imap_find_account(GncImportMatchMap *imap, const char* category,
			       const char *key);

/** Store an Account in the map. This mapping is immediatly stored in
  the underlying kvp frame, regardless of whether the MatchMap is
  destroyed later or not. */
void gnc_imap_add_account (GncImportMatchMap *imap, const char *category,
			   const char *key, Account *acc);

/** Look up an Account in the map from a GList* of pointers to strings(tokens)
  from the current transaction */
Account* gnc_imap_find_account_bayes (GncImportMatchMap *imap, GList* tokens);

/** Store an Account in the map. This mapping is immediatly stored in
  the underlying kvp frame, regardless of whether the MatchMap is
  destroyed later or not. */
void gnc_imap_add_account_bayes (GncImportMatchMap *imap, GList* tokens,
				 Account *acc);


/** @name Some well-known categories
 
  NOTE: You DO NOT have to use these values in your importer -- these
  are just "well known" values, not "mandatory" values.  You are free
  to use these if they apply, map your own fields to these labels, or
  create your own category strings.
*/
/** @{*/
#define GNCIMPORT_DESC	"desc"
#define GNCIMPORT_MEMO	"memo"
#define GNCIMPORT_PAYEE	"payee"
/**@}*/

#endif /* GNC_IMPORT_MATCH_MAP_H */
/**@}*/
