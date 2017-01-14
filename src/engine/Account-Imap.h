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
 *                                                                  *
\********************************************************************/
/** @addtogroup Engine
    @{ */
/** @addtogroup Account
    Account Import mapping functions
    @{ */
/** @file Account-Imap.h
    @brief Account Imap handling public routines
    @author Copyright (C) 2007 David Hampton
    @author Copyright (C) 2016 Robert Fewell
*/

#ifndef ACCOUNT_IMAP_H
#define ACCOUNT_IMAP_H

#ifdef __cplusplus
extern "C"
{
#endif

#include "qof.h"
#include "gnc-engine.h"
#include "policy.h"

typedef struct imap_info
{
    Account        *source_account;
    Account        *map_account;
    GList          *list;
    char           *category_head;
    char           *full_category;
    char           *match_string;
    char           *count;
}GncImapInfo;

/* Predefined KVP paths */
static const char* IMAP_FRAME = "import-map";
static const char* IMAP_FRAME_BAYES = "import-map-bayes";

#ifdef __cplusplus
class GncImportMatchMap
{
public:
    // Constructor - Destructor
    GncImportMatchMap(Account *acc);
    ~GncImportMatchMap();

    Account *find_account (const char* category, const char *key);
    void add_account (const char *category, const char *key, Account *acc);
    void delete_account (const char *category, const char *key);

    Account *find_account_bayes (GList* tokens);
    void add_account_bayes (GList* tokens, Account *acc);

    Account *get_account();
    QofBook *get_book();

private:
    Account *m_account = nullptr;
    QofBook *m_book;
};
#else
  typedef
    struct GncImportMatchMap
      GncImportMatchMap;
#endif


/* ================================================================ */
/* The following functions are used by
 * src/import-export/import-backend.c to manipulate the contra-account
 * matching data. See src/import-export/import-backend.c for explanations.
 */

/** Obtain an ImportMatchMap object from an Account or a Book
 */
GncImportMatchMap *gnc_account_imap_create_imap (Account *acc);

/** Delete an ImportMatchMap object from an Account or a Book
 */
void gnc_account_imap_delete_imap (GncImportMatchMap *imap);

/* Look up an Account in the map non Baysian
 */
Account *gnc_account_imap_find_account (GncImportMatchMap *imap, const char* category, const char *key);

/* Store an Account in the map non Baysian
 */
void gnc_account_imap_add_account (GncImportMatchMap *imap, const char* category, const char *key, Account *acc);

/* Remove a reference to an Account in the map non Baysian
 */
void gnc_account_imap_delete_account (GncImportMatchMap *imap, const char *category, const char *key);

/** Look up an Account in the map using Baysian
 */
Account* gnc_account_imap_find_account_bayes (GncImportMatchMap *imap, GList* tokens);

/** Updates the imap for a given account using a list of tokens
 */
void gnc_account_imap_add_account_bayes (GncImportMatchMap *imap, GList* tokens, Account *acc);

/** Returns a GList of structure imap_info of all Bayesian mappings for
 *  required Account
 */
GList *gnc_account_imap_get_info_bayes (Account *acc);

/** Returns a GList of structure imap_info of all Non Bayesian mappings for
 *  required Account
 */
GList *gnc_account_imap_get_info (Account *acc, const char *category);

/** Returns the text string pointed to by full_category for the Account, free
 *  the returned text
 */
gchar *gnc_account_get_map_entry (Account *acc, const char *full_category);

/** Delete the entry for Account pointed to by full_category, if empty is TRUE then use
 *  delete if empty, full_category is freed
 */
void gnc_account_delete_map_entry (Account *acc, char *full_category, gboolean empty);

/** Search for Bayesian entries with mappings based on full account name and change
 *  them to be based on the account guid
 */
void gnc_account_imap_convert_bayes (QofBook *book);


#ifdef __cplusplus
}
#endif

#endif /* ACCOUNT_IMAP_H */
/** @} */
/** @} */
