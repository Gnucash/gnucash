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

/** GList of Account */
typedef GList                  AccountList;
/** GList of GNCBook */
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

#endif
/** @} */
