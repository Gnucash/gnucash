/********************************************************************
 * gnc-engine.h  -- top-level include file for Gnucash Engine       *
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
 *                                                                  *
 ********************************************************************/
/** @addtogroup Engine
    @{ */
/** @file gnc-engine.h 
    @brief All type declarations for the whole Gnucash engine
    @author Copyright (C) 1997 Robin D. Clark
    @author Copyright (C) 2000 Bill Gribble <grib@billgribble.com>
    @author Copyright (C) 1997-2001 Linas Vepstas <linas@linas.org>
*/

#ifndef GNC_ENGINE_H
#define GNC_ENGINE_H

#include <glib.h>

/* TYPES **********************************************************/

/** @brief Account in Gnucash. 
 *
 * This is the typename for an account. The actual structure is
 * defined in the private header AccountP.h, but no one outside the
 * engine should include that file. Instead, access that data only
 * through the functions in Account.h .*/
typedef struct account_s             Account;

/** @brief A group of accounts in Gnucash. 
*/
typedef struct account_group_s       AccountGroup;

/** @brief Split in Gnucash. 
 *
 * A "split" is more commonly refered to as a "entry" in a
 * "transaction". Each split belongs to one Account and one
 * Transaction. The split is one out of several parts a Transaction is
 * divided into.
 *
 * This is the typename for a split. The actual structure is defined
 * in the private header TransactionP.h, but no one outside the engine
 * should include that file. Instead, access that data only through
 * the functions in Transaction.h .*/
typedef struct split_s               Split;

/** @brief Transaction in Gnucash.  
 *
 * A Transaction is a piece of business done; the transfer of money
 * from one account to one or more other accounts. Each Transaction is
 * divided into one or more Splits (usually two).
 *
 * This is the typename for a transaction. The actual structure is
 * defined in the private header TransactionP.h, but no one outside
 * the engine should include that file. Instead, access that data only
 * through the functions in Transaction.h .*/
typedef struct transaction_s         Transaction;

/** @brief Encapsulates all the information about a dataset
 * manipulated by GnuCash.
 *
 * A GNCBook holds the actual data.
 */
typedef struct gnc_book_struct       GNCBook;

/** @brief An article that is bought and sold. 
 *
 * A Commodity is the most general term of what an account keeps track
 * of. Usually this is a monetary currency, but it can also be a stock
 * share or even a precious metal. Every account keeps track of
 * exactly one gnc_commodity.
 *
 * (Up to version 1.6.x, we used to have currencies and
 * securities. Now these concepts have been merged into this
 * gnc_commodity. See the comments at xaccAccountSetCommodity() for
 * more about that.)
 *
 * This is the typename for a gnc_commodity. The actual structure is
 * defined in a private source file. For accessing that data, only use
 * the functions in gnc-commodity.h .*/
typedef struct gnc_commodity_s       gnc_commodity;

/** @brief A gnc_commodity_table is a database of commodity info. */
typedef struct gnc_commodity_table_s gnc_commodity_table;

/**
 * A GNCLot implements the fundamental conceptual idea behind
 * invoices, inventory lots, and stock market investment lots.  
 *
 * See the file src/doc/lots.txt for implmentation overview.
 */
typedef struct gnc_lot_struct        GNCLot;

/** @brief Encapsulates a connection to a GnuCash backend.  
 *
 * A GNCSession manages the connection to a persistant data store,
 * whereas the backend is the thing that performs the actual datastore
 * access.
 *
 * This class provides several important services:
 *
 * 1) It resolves and loads the appropriate backend, based on 
 *    the URL.
 *    
 * 2) It reports backend errors (e.g. network errors, storage 
 *    corruption errors) through a single, backend-independent 
 *    API.
 *
 * 3) It reports non-error events received from the backend.
 *
 * 4) It helps manage global dataset locks.  For example, for the
 *    file backend, the lock prevents multiple users from editing 
 *    the same file at the same time, thus avoiding lost data due 
 *    to race conditions.  Thus, an open session implies that the 
 *    associated file is locked.
 *
 * 5) Misc utilities, such as a search path for the file to be 
 *    edited, and/or other URL resolution utilities.  This should 
 *    simplify install & maintenance problems for naive users who
 *    may not have a good grasp on what a file system is, or where
 *    they want to keep their data files.
 *
 * 6) In the future, this class is probably a good place to manage 
 *    a portion of the user authentication process, and hold user
 *    credentials/cookies/keys/tokens.  This is because at the 
 *    coarsest level, authorization can happen at the datastore
 *    level: i.e. does this user even have the authority to connect
 *    to and open this datastore?
 *
 * A brief note about books & sessions: A GNCBook encapsulates the
 * datasets manipulated by GnuCash.  A GNCBook holds the actual data.
 * By contrast, the session mediates the connection between a book
 * (the thing that lives in virtual memory in the local process) and
 * the datastore (the place where book data lives permanently, e.g.,
 * file, database).
 *
 * In the current design, a session may hold multiple books.  For 
 * now, exactly what this means is somewhat vague, and code in  
 * various places makes some implicit assumptions: first, only
 * one book is 'current' and open for editing.  Next, its assumed 
 * that all of the books in a session are related in some way.
 * i.e. that they are all earlier accounting periods of the
 * currently open book.  In particular, the backends probably 
 * make that assumption, in order to store the different accounting
 * periods in a clump so that one can be found, given another.
 */
typedef struct gnc_session_struct    GNCSession;

/** GList of Account */
typedef GList                  AccountList;
/** GList of GNCBook */
typedef GList                  BookList;
/** GList of GNCLot */
typedef GList                  LotList;
/** GList of Split */
typedef GList                  SplitList;
/** GList of Transaction */
typedef GList                  TransList;
/** GList of GUIDs of a Account */
typedef GList                  AccountGUIDList;
/** GList of GUIDs of a GNCBook */
typedef GList                  BookGUIDList;

/** Function type for init hooks in the engine.  */
typedef void (* gnc_engine_init_hook_t)(int, char **);


/** PROTOTYPES ******************************************************/

/** GnuCash version number infomation. */
unsigned int gnucash_major_version (void);
/** GnuCash version number infomation. */
unsigned int gnucash_minor_version (void);
/** GnuCash version number infomation. */
unsigned int gnucash_micro_version (void);

/** gnc_engine_init MUST be called before gnc engine functions can 
 * be used. */
void gnc_engine_init(int argc, char ** argv);

/** Called to shutdown the engine */
void gnc_engine_shutdown (void);

/** Pass a function pointer to gnc_engine_add_init_hook and 
 * it will be called during the evaluation of gnc_engine_init */
void gnc_engine_add_init_hook(gnc_engine_init_hook_t hook);

/** Many strings used throughout the engine are likely to be duplicated.
 * So we provide a reference counted cache system for the strings, which
 * shares strings whenever possible.
 *
 * Use g_cache_insert to insert a string into the cache (it will return a
 * pointer to the cached string).
 * Basically you should use this instead of g_strdup.
 *
 * Use g_cache_remove (giving it a pointer to a cached string) if the string
 * is unused.  If this is the last reference to the string it will be
 * removed from the cache, otherwise it will just decrement the
 * reference count.
 * Basically you should use this instead of g_free.
 *
 * Note that all the work is done when inserting or removing.  Once
 * cached the strings are just plain C strings.
 */

/* get the gnc_string_cache.  Create it if it doesn't exist already */
GCache* gnc_engine_get_string_cache(void);

#endif
/** @} */
