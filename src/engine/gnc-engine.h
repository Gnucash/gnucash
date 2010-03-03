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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
 ********************************************************************/
/** @addtogroup Engine GnuCash Engine: Core, Non-GUI Accounting Functions
    The GnuCash Engine provides a set of objects and classes that
    encapsulate typical financial accounting concepts.  The GnuCash
    GUI is expected to manipulate these objects through the provided
    engine API.
    @{ */
/** @file gnc-engine.h
    @brief All type declarations for the whole Gnucash engine
    @author Copyright (C) 1997 Robin D. Clark
    @author Copyright (C) 2000 Bill Gribble <grib@billgribble.com>
    @author Copyright (C) 2000 Dave Peticolas <peticola@cs.ucdavis.edu>
    @author Copyright (C) 1997-2001 Linas Vepstas <linas@linas.org>
*/

#ifndef GNC_ENGINE_H
#define GNC_ENGINE_H

#include <glib.h>
#include "qof.h"

/** \name QofLogModule identifiers */
// @{
#define GNC_MOD_ROOT      "gnc"
#define GNC_MOD_ENGINE    "gnc.engine"
#define GNC_MOD_ACCOUNT   "gnc.account"
#define GNC_MOD_SX        "gnc.engine.sx"
#define GNC_MOD_QUERY     "gnc.query"
#define GNC_MOD_SCRUB     "gnc.scrub"
#define GNC_MOD_LOT       "gnc.lots"
#define GNC_MOD_COMMODITY "gnc.commodity"
#define GNC_MOD_BACKEND   "gnc.backend"
#define GNC_MOD_PRICE     "gnc.pricedb"
#define GNC_MOD_BUSINESS  "gnc.business"
#define GNC_MOD_IO        "gnc.io"
#define GNC_MOD_BOOK      "gnc.book-period"
#define GNC_MOD_GUI       "gnc.gui"
#define GNC_MOD_GUI_SX    "gnc.gui.sx"
#define GNC_MOD_GUILE     "gnc.guile"
#define GNC_MOD_LEDGER    "gnc.ledger"
#define GNC_MOD_REGISTER  "gnc.register"
#define GNC_MOD_HTML      "gnc.html"
#define GNC_MOD_PREFS     "gnc.pref"
#define GNC_MOD_IMPORT    "gnc.import"
#define GNC_MOD_DRUID     "gnc.druids"
#define GNC_MOD_TEST      "gnc.tests"
#define GNC_MOD_BUDGET    "gnc.budget"
//@}

/** @brief IDENTIFIERS
 *  GUID Identifiers can be used to reference Accounts, Transactions,
 *  Splits and other objects. These Gnucash types are referred to as Gnucash
 *  entities. GUID Identifiers are globally-unique and permanent, i.e., once
 *  an entity has been assigned an identifier, it retains that same
 *  identifier for its lifetime.
 *  -
 *  Identifiers are 'typed' with strings. The ids used in gnucash are
 *  defined below. An id with type GNC_ID_NONE does not refer to any
 *  entity, although that may change as new ids are created. An id with
 *  type GNC_ID_NULL does not refer to any entity, and will never refer
 *  to any entity. An identifier with any other type may refer to an
 *  actual entity, but that is not guaranteed. If an id does refer to
 *  an entity, the type of the entity will match the type of the
 *  identifier.
 */

#define GNC_ID_NONE           QOF_ID_NONE
#define GNC_ID_BOOK           QOF_ID_BOOK
#define GNC_ID_SESSION        QOF_ID_SESSION
#define GNC_ID_NULL           QOF_ID_NULL

#define GNC_ID_ACCOUNT        "Account"
#define GNC_ID_COMMODITY      "Commodity"
#define GNC_ID_COMMODITY_NAMESPACE "CommodityNamespace"
#define GNC_ID_COMMODITY_TABLE "CommodityTable"
#define GNC_ID_LOT            "Lot"
#define GNC_ID_PERIOD         "Period"
#define GNC_ID_PRICE          "Price"
#define GNC_ID_PRICEDB        "PriceDB"
#define GNC_ID_SPLIT          "Split"
#define GNC_ID_BUDGET         "Budget"
#define GNC_ID_SCHEDXACTION   "SchedXaction"
#define GNC_ID_SXES           "SchedXactions"
#define GNC_ID_SXTG           "SXTGroup"
#define GNC_ID_SXTT           "SXTTrans"
#define GNC_ID_TRANS          "Trans"

/* TYPES **********************************************************/

/* CAS: ISTM, it would make more sense to put the typedefs in their
   corresponding header files, (e.g. Account.h), and to #include all
   the engine API header files right here.  After all, when I jump to
   the definition "Account", I want to end up in Account.h, not this
   file, like I do now.

   Also, as it is now, if I want to use the engine api, I need to
   include this header, plus all the other engine headers for the
   types whose functions I call, so this header is providing almost no
   benefit of aggregation.  But, if it included all the headers I
   could just include this file.  Or would that cause a massive
   recompile everytime one engine header changed?
   Even if including all the headers here doesn't make sense, I think
   distributing the stuff in the "Types" section does.
*/


/** @brief Account in Gnucash.
 * This is the typename for an account. The actual structure is
 * defined in the private header AccountP.h, but no one outside the
 * engine should include that file. Instead, access that data only
 * through the functions in Account.h .*/
typedef struct account_s             Account;

/** @brief Split in Gnucash.
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

/** @brief A gnc_commodity_namespace is an collection of commodities. */
typedef struct gnc_commodity_namespace_s gnc_commodity_namespace;

/** @brief A gnc_commodity_table is a database of commodity info. */
typedef struct gnc_commodity_table_s gnc_commodity_table;

/** @brief Identifies that something sold at one time was bought at another.
 * A GNCLot provides a way of tracking physical items as they are
 * bought and sold in different transactions.  By identifying
 * the individual, underlying physical objects, it provides the
 * needed framework for implementing depreciation, capital gains,
 * inventory control and invoices.
 *
 * See the file src/doc/lots.txt for implmentation overview.
 */
typedef struct gnc_lot_s             GNCLot;

/** @brief Price of commodity on a given date.
 * A GNCPrice encapsulates price information: the cost of a commodity
 * expressed as a currency, on a given date.  It also holds info about
 * the provenance of the price: where it came from, its general validity.
 */
typedef struct gnc_price_s           GNCPrice;
typedef struct gnc_quote_source_s    gnc_quote_source;

/** GList of Account */
typedef GList                  AccountList;
/** GList of GNCLots */
typedef GList                  LotList;
/** GList of Split */
typedef GList                  SplitList;
/** GList of Transaction */
typedef GList                  TransList;
/** GList of GUIDs of a Account */
typedef GList                  AccountGUIDList;
/** GList of GUIDs of a QofBook */
typedef GList                  BookGUIDList;

typedef void (*EngineCommitErrorCallback)( gpointer data, QofBackendError errcode );

typedef  gint (*SplitCallback)(Split *s, gpointer data);
typedef  gint (*TransactionCallback)(Transaction *t, void *data);

/** Function type for init hooks in the engine.  */
typedef void (* gnc_engine_init_hook_t)(int, char **);


/** PROTOTYPES ******************************************************/

/** GnuCash version number infomation. */
unsigned int gnucash_major_version (void);
/** GnuCash version number infomation. */
unsigned int gnucash_minor_version (void);
/** GnuCash version number infomation. */
unsigned int gnucash_micro_version (void);

/** gnc_engine_init should be called before gnc engine
 * functions can be used - see also ::qof_init for a
 * method that does not require Guile. */
void gnc_engine_init(int argc, char ** argv);

/** Called to shutdown the engine, see also ::qof_close
 * for use without Guile. */
void gnc_engine_shutdown (void);

/** check the engine is fully initialized */
gboolean gnc_engine_is_initialized(void);

/** enable default log modules */
void gnc_log_default(void);

/** Pass a function pointer to gnc_engine_add_init_hook and
 * it will be called during the evaluation of gnc_engine_init */
void gnc_engine_add_init_hook(gnc_engine_init_hook_t hook);

/** Set a callback function to be called in case an engine commit
 * fails */
void gnc_engine_add_commit_error_callback( EngineCommitErrorCallback cb, gpointer data );

void gnc_engine_signal_commit_error( QofBackendError errcode );

#endif
/** @} */
