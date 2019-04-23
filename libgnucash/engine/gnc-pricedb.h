/********************************************************************
 * gnc-pricedb.h -- a simple price database for gnucash.            *
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
 *******************************************************************/

#ifndef GNC_PRICEDB_H
#define GNC_PRICEDB_H

typedef struct _GncPriceClass GNCPriceClass;
typedef struct _GncPriceDBClass GNCPriceDBClass;

#include <stdio.h>
#include "qof.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"

#ifdef __cplusplus
extern "C" {
#endif

/* --- type macros --- */
#define GNC_TYPE_PRICE            (gnc_price_get_type ())
#define GNC_PRICE(o)              \
     (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_PRICE, GNCPrice))
#define GNC_PRICE_CLASS(k)        \
     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_PRICE, GNCPriceClass))
#define GNC_IS_PRICE(o)           \
     (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_PRICE))
#define GNC_IS_PRICE_CLASS(k)     \
     (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_PRICE))
#define GNC_PRICE_GET_CLASS(o)    \
     (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_PRICE, GNCPriceClass))
GType gnc_price_get_type(void);

/* --- type macros --- */
#define GNC_TYPE_PRICEDB            (gnc_pricedb_get_type ())
#define GNC_PRICEDB(o)              \
     (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_PRICEDB, GNCPriceDB))
#define GNC_PRICEDB_CLASS(k)        \
     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_PRICEDB, GNCPriceDBClass))
#define GNC_IS_PRICEDB(o)           \
     (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_PRICEDB))
#define GNC_IS_PRICEDB_CLASS(k)     \
     (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_PRICEDB))
#define GNC_PRICEDB_GET_CLASS(o)    \
     (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_PRICEDB, GNCPriceDBClass))
GType gnc_pricedb_get_type(void);


/** @addtogroup PriceDB
    @{ */
/** @file gnc-pricedb.h
    @author Copyright (C) 2001 Rob Browning
    @author Copyright (C) 2001,2003 Linas Vepstas <linas@linas.org>
    @brief a simple price database for gnucash
*/
/** @} */


/** @addtogroup Engine
    @{ */
/** @addtogroup PriceDB  Price Database
    @ingroup Engine
    The PriceDB is intended to be a database of price quotes, or more
    specifically, a database of GNCPrices.  For the time being, it is
    still a fairly simple database supporting only fairly simple
    queries.  It is expected that new queries will be added as needed,
    and that there is some advantage to delaying complex queries for
    now in the hope that we get a real DB implementation before
    they're really needed.

    Every QofBook contains a GNCPriceDB, accessible via
    gnc_pricedb_get_db.

    \warning The PriceDB does not currently use the object
    system used elsewhere in the GnuCash Engine, i.e. it does
    not use GUISD's, Entities and Collections.  It should.
    In particular, this means that currently prices cannot
    be queried with the same mechanism as everything else.
*/

/** @addtogroup Price  Prices
    @ingroup Engine
    Each price in the database represents an "instantaneous" quote for
    a given commodity with respect to another commodity.  For example,
    a given price might represent the value of LNUX in USD on
    2001-02-03.

    \par Fields:

    - commodity: the item being priced.
    - currency: the denomination of the value of the item being priced.
    - value: the value of the item being priced.
    - time: the time the price was valid.
    - source: a string describing the source of the quote.  These
      strings will be something like this: "Finance::Quote",
      "user:misc", "user:foo", etc.  If the quote came from a user,
      as a matter of policy, you *must* prefix the string you give
      with "user:".  For now, the only other reserved values are
      "Finance::Quote" and "old-file-import".  Any string used must
      be added to the source_list array in dialog-price-edit-db.c so
      that it can be properly translated. (There are unfortunately
      many strings in users' databases, so this string must be
      translated on output instead of always being used in untranslated
      form).
    - type: the type of quote - types possible right now are bid, ask, last,
      nav, transaction, and unknown. 'Transaction' is set when the price is
      created from an amount and value in a Split and is not available for users
      to set via the GUI.

    \par Implementation Details:

    \note
    For source and type, NULL and the empty string are
    considered the same, so if one of these is "", then after a file
    save/restore, it might be NULL.  Behave accordingly.

    GNCPrices are reference counted.  When you gnc_price_create one
    or clone it, the new price's count is set to 1.  When you are
    finished with a price, call gnc_price_unref.  If you hand the
    price pointer to some other code that needs to keep it, make
    sure it calls gnc_price_ref to indicate its interest in that
    price, and calls gnc_price_unref when it's finished with the
    price.  For those unfamiliar with reference counting, basically
    each price stores an integer count which starts at 1 and is
    incremented every time someone calls gnc_price_ref.  Conversely,
    the count is decremented every time someone calls
    gnc_price_unref.  If the count ever reaches 0, the price is
    destroyed.

      All of the getters return data that's internal to the GNCPrice,
      not copies, so don't free these values.

      All of the setters store copies of the data given, with the
      exception of the commodity field which just stores the pointer
      given.  It is assumed that commodities are a global resource and
      are pointer unique.
   */
/* ================================================================ */

/** @addtogroup Price
 @{ */

/** */
typedef GList PriceList;

/** Price source enum. Be sure to keep in sync with the source_name array in
 * gnc-pricedb.c. These are in preference order, so for example a quote with
 * PRICE_SOURCE_EDIT_DLG will overwrite one with PRICE_SOURCE_FQ but not the
 * other way around.
 */
typedef enum
{
    PRICE_SOURCE_EDIT_DLG,         // "user:price-editor"
    PRICE_SOURCE_FQ,               // "Finance::Quote"
    PRICE_SOURCE_USER_PRICE,       // "user:price"
    PRICE_SOURCE_XFER_DLG_VAL,     // "user:xfer-dialog"
    PRICE_SOURCE_SPLIT_REG,        // "user:split-register"
    PRICE_SOURCE_STOCK_SPLIT,      // "user:stock-split"
    PRICE_SOURCE_INVOICE,          // "user:invoice-post"
    PRICE_SOURCE_TEMP,             // "temporary"
    PRICE_SOURCE_INVALID,          // "invalid"
} PriceSource;

#define PRICE_TYPE_LAST "last"
#define PRICE_TYPE_UNK "unknown"
#define PRICE_TYPE_TRN "transaction"
/* ------------------ */
/** @name Constructors
    @{ */

/** gnc_price_create - returns a newly allocated and initialized price
   with a reference count of 1. */
/*@ dependent @*/
GNCPrice *gnc_price_create(QofBook *book);

/** gnc_price_clone - returns a newly allocated price that's a
   content-wise duplicate of the given price, p.  The returned clone
   will have a reference count of 1. */
GNCPrice *gnc_price_clone(GNCPrice* p, QofBook *book);

/** Return a newly-allocated price that's the inverse of the given price, p.
 *
 * Inverse means that the commodity and currency are swapped and the value is
 * the numeric inverse of the original's. The source is set to PRICE_SOURCE_TEMP
 * to prevent it being saved in the pricedb.
 * @param p The price to invert
 * @return a new price, with a ref-count of 1. Don't forget to unref it!
 */
GNCPrice *gnc_price_invert(GNCPrice *p);

/**  @} */

/* ------------------ */
/** @name  Memory Management
    @{ */

/** gnc_price_ref - indicate your need for a given price to stick
   around (i.e. increase its reference count by 1). */
void      gnc_price_ref(GNCPrice *p);

/** gnc_price_unref - indicate you're finished with a price
   (i.e. decrease its reference count by 1). */
void      gnc_price_unref(GNCPrice *p);
/** @} */

/* ------------------ */
/** @name  Setters
 * All of the setters store copies of the data
 * given, with the exception of the commodity field which just stores
 * the pointer given.  It is assumed that commodities are a global
 * resource and are pointer unique.
 *
 * Invocations of the setters should be wrapped with calls to
 * gnc_price_begin_edit() and commit_edit().  The begin/commit
 * calls help ensure that the local price db is synchronized with
 * the backend.
   @{ */
void gnc_price_begin_edit (GNCPrice *p);
void gnc_price_commit_edit (GNCPrice *p);

void gnc_price_set_commodity(GNCPrice *p, gnc_commodity *c);
void gnc_price_set_currency(GNCPrice *p, gnc_commodity *c);
void gnc_price_set_time64(GNCPrice *p, time64 t);
void gnc_price_set_source(GNCPrice *p, PriceSource source);
void gnc_price_set_source_string(GNCPrice *p, const char* s);
void gnc_price_set_typestr(GNCPrice *p, const char* type);
void gnc_price_set_value(GNCPrice *p, gnc_numeric value);
/** @} */

/* ------------------ */
/** @name  Getters
    All of the getters return data that's internal
    to the GNCPrice, not copies, so don't free these values.
    @{ */

    GNCPrice *      gnc_price_lookup (const GncGUID *guid, QofBook *book);
/*@ dependent @*/
gnc_commodity * gnc_price_get_commodity(const GNCPrice *p);
/*@ dependent @*/
gnc_commodity * gnc_price_get_currency(const GNCPrice *p);
time64          gnc_price_get_time64(const GNCPrice *p);
PriceSource     gnc_price_get_source(const GNCPrice *p);
const char *    gnc_price_get_source_string(const GNCPrice *p);
const char *    gnc_price_get_typestr(const GNCPrice *p);
gnc_numeric     gnc_price_get_value(const GNCPrice *p);
gboolean        gnc_price_equal(const GNCPrice *p1, const GNCPrice *p2);

#define gnc_price_get_guid(X)    qof_entity_get_guid(QOF_INSTANCE(X))
#define gnc_price_return_guid(X) (*(qof_entity_get_guid(QOF_INSTANCE(X))))
#define gnc_price_get_book(X)    qof_instance_get_book(QOF_INSTANCE(X))
/**  @} */

/** @name Internal/Debugging
    @{ */
/** This simple function can be useful for debugging the price code */
void gnc_price_print(GNCPrice *db, FILE *f, int indent);
/** @} */
/** @name Denominator Constants Price policy: In order to avoid rounding
 * problems, currency prices (often called exchange rates) are saved in terms of
 * the smaller currency, so that price > 1, with a fixed denominator of
 * 1/1000. Commodity prices in currency are always expressed as value per unit
 * of the commodity with a fixed denominator of the pricing currency's
 * SCU * 10000.
 */
#define CURRENCY_DENOM 10000
#define COMMODITY_DENOM_MULT 10000

/* ================================================================ */
/** @name GNCPrice lists
    The database communicates multiple prices in and out via gnc price
    lists.  These are just time sorted GLists of GNCPrice pointers.
    Functions for manipulating these lists are provided.  These
    functions are helpful in that they handle maintaining proper
    reference counts on behalf of the price list for every price being
    held in a given list.  I.e. insert "refs" the prices being
    inserted, remove and destroy "unref" the prices that will no
    longer be referred to by the list.
   @{
*/

/** gnc_price_list_insert - insert a price into the given list, calling
     gnc_price_ref on it during the process. */
gboolean gnc_price_list_insert(PriceList **prices, GNCPrice *p,
                               gboolean check_dupl);

/** gnc_price_list_remove - remove the price, p, from the given list,
     calling gnc_price_unref on it during the process. */
gboolean gnc_price_list_remove(PriceList **prices, GNCPrice *p);

/** gnc_price_list_destroy - destroy the given price list, calling
     gnc_price_unref on all the prices included in the list. */
void     gnc_price_list_destroy(PriceList *prices);

gboolean gnc_price_list_equal(PriceList *prices1, PriceList *prices2);
/**  @} */
/**  @}  end of the Price doxygen group */

/* ================================================================ */
/** @addtogroup PriceDB
  Whenever a you store a price in the pricedb, the pricedb adds its
  own reference to the price, so you can safely unref that price after
  inserting it into the DB if you're finished with it otherwise.

  Similarly, when the pricedb returns a price to you, either singly,
  or in a price list, the price will have had a ref added for you, so
  you only need to unref the price(s) when you're finished with
  it/them.
  @{
*/
/** Data type */
typedef struct gnc_price_db_s GNCPriceDB;

/** @brief Return the pricedb associated with the book
 * @param book The QofBook holding the pricedb
 * @return The GNCPriceDB associated with the book.
 */
GNCPriceDB * gnc_pricedb_get_db(QofBook *book);
/** @brief Return the pricedb via the Book's collection.
 * @param col The QofCollection holding the pricedb
 * @return The GNCPriceDB in the QofCollection
 */
GNCPriceDB * gnc_collection_get_pricedb(QofCollection *col);

/** @brief Destroy the given pricedb and unref all of the prices it contains.
 *
 * This may not deallocate all of those prices.  Other code may still be holding
 * references to them.
 * @param db The pricedb to destroy.
 */
void gnc_pricedb_destroy(GNCPriceDB *db);

/** @brief Begin an edit. */
void gnc_pricedb_begin_edit (GNCPriceDB *);
/** @brief Commit an edit. */
void gnc_pricedb_commit_edit (GNCPriceDB *);

/** @brief Set flag to indicate whether duplication checks should be performed.
 *
 * Normally used at load time to speed up loading the pricedb.
 * @param db The pricedb
 * @param bulk_update TRUE to disable duplication checks, FALSE to enable them.
 */
void gnc_pricedb_set_bulk_update(GNCPriceDB *db, gboolean bulk_update);

/** @brief Add a price to the pricedb.
 *
 * You may drop your reference to the price (i.e. call unref) after this
 * succeeds, whenever you're finished with the price.
 * @param db The pricedb
 * @param p The GNCPrice to add.
 * @return TRUE if the price was added, FALSE otherwise.
 */
gboolean     gnc_pricedb_add_price(GNCPriceDB *db, GNCPrice *p);

/** @brief Remove a price from the pricedb and unref the price.
 * @param db The Pricedb
 * @param p The price to remove.
 */
gboolean     gnc_pricedb_remove_price(GNCPriceDB *db, GNCPrice *p);

typedef enum
{
    PRICE_REMOVE_SOURCE_FQ = 1,   // this flag is set when added by F:Q checked
    PRICE_REMOVE_SOURCE_USER = 2, // this flag is set when added by the user checked
    PRICE_REMOVE_SOURCE_APP = 4,  // this flag is set when added by the app checked
    PRICE_REMOVE_SOURCE_COMM = 8, // this flag is set when we have commodities selected
} PriceRemoveSourceFlags;

typedef enum
{
    PRICE_REMOVE_KEEP_NONE,           // keep none
    PRICE_REMOVE_KEEP_LAST_WEEKLY,    // leave last one of every week
    PRICE_REMOVE_KEEP_LAST_MONTHLY,   // leave last one of every month
    PRICE_REMOVE_KEEP_LAST_QUARTERLY, // leave last one of every quarter
    PRICE_REMOVE_KEEP_LAST_PERIOD,    // leave last one of every annual period
    PRICE_REMOVE_KEEP_SCALED,         // leave one every week then one a month
} PriceRemoveKeepOptions;

/** @brief Remove and unref prices older than a certain time.
 * @param db The pricedb
 * @param comm_list A list of commodities
 * @param fiscal_end_date the end date of the current accounting period
 * @param cutoff The time before which prices should be deleted.
 * @param source Whether Finance::Quote, user or all prices should be deleted.
 * @param keep Whether scaled, monthly, weekly or no prices should be left.
 * @return True if there were prices to process, False if not.
 */
gboolean     gnc_pricedb_remove_old_prices(GNCPriceDB *db, GList *comm_list,
                                           GDate *fiscal_end_date, time64 cutoff,
                                           PriceRemoveSourceFlags source,
                                           PriceRemoveKeepOptions keep);

/** @brief Find the most recent price between the two commodities.
 *
 * The returned GNCPrice may be in either direction so check to ensure that its
 * value is correctly applied.
 * @param db The pricedb
 * @param commodity The first commodity
 * @param currency The second commodity
 * @return A GNCPrice or NULL if no price exists.
 */
GNCPrice   * gnc_pricedb_lookup_latest(GNCPriceDB *db,
                                       const gnc_commodity *commodity,
                                       const gnc_commodity *currency);

/** @brief Find the most recent price between a commodity and all other
 * commodities
 *
 * The returned GNCPrices may be in either direction so check to ensure that
 * their values are correctly applied.
 * @param db The pricedb
 * @param commodity The commodity for which to obtain prices
 * @return A PriceList of prices found, or NULL if none found.
 */
PriceList * gnc_pricedb_lookup_latest_any_currency(GNCPriceDB *db,
        const gnc_commodity *commodity);

/** @brief Report whether the pricedb contains prices for one commodity in
 * another.
 *
 * Does *not* check the reverse direction.
 * @param db The pricedb to check
 * @param commodity The commodity to check for the existence of prices
 * @param currency The commodity in which prices are sought. If NULL reports all
 * commodities.
 * @return TRUE if matching prices are found, FALSE otherwise.
 */
gboolean     gnc_pricedb_has_prices(GNCPriceDB *db,
                                    const gnc_commodity *commodity,
                                    const gnc_commodity *currency);

/** @brief Return all the prices for a given commodity in another.
 *
 * Does *not* retrieve reverse prices, i.e. prices of the second commodity in
 * the first.
 * @param db The pricedb from which to retrieve prices.
 * @param commodity The commodity for which prices should be retrieved.
 * @param currency The commodity in which prices should be quoted. If NULL, all
 * prices in any commodity are included.
 * @return A PriceList of matching prices or NULL if none were found.
*/
PriceList * gnc_pricedb_get_prices(GNCPriceDB *db,
                                   const gnc_commodity *commodity,
                                   const gnc_commodity *currency);

/** @brief Find the price between two commodities at a time64.
 *
 * The returned GNCPrice may be in either direction so check to ensure that its
 * value is correctly applied.
 * @param db The pricedb
 * @param commodity The first commodity
 * @param currency The second commodity
 * @param t The time64 at which to retrieve the price.
 * @return A GNCPrice or NULL if none matches.
 */
/* NOT USED */
GNCPrice * gnc_pricedb_lookup_at_time64(GNCPriceDB *db,
                                        const gnc_commodity *commodity,
                                        const gnc_commodity *currency,
                                        time64 t);

/** @brief Return the price between the two commodities on the indicated
 * day. Note that the notion of day might be distorted by changes in timezone.
 *
 * The returned GNCPrice may be in either direction so check to ensure that its
 * value is correctly applied.
 * @param db The pricedb
 * @param commodity The first commodity
 * @param currency The second commodity
 * @param t A time. The price returned will be in the same day as this time
 * according to the local timezone.
 * @return A GNCPrice or NULL on failure.
 */
GNCPrice * gnc_pricedb_lookup_day_t64(GNCPriceDB *db,
                                      const gnc_commodity *commodity,
                                      const gnc_commodity *currency,
                                      time64 t);

/** @brief Return the price between the two commoditiesz nearest to the given
 * time.
 *
 * The returned GNCPrice may be in either direction so check to ensure that its
 * value is correctly applied.
 * @param db The pricedb
 * @param c The first commodity
 * @param currency The second commodity
 * @param t The time nearest to which the returned price should be.
 * @return A GNCPrice or NULL if no prices exist between the two commodities.
 */
GNCPrice   * gnc_pricedb_lookup_nearest_in_time64(GNCPriceDB *db,
                                                  const gnc_commodity *c,
                                                  const gnc_commodity *currency,
                                                  time64 t);

/** @brief Return the price nearest in time to that given between the given
 * commodity and every other.
 *
 * The returned GNCPrices may be in either direction so check to ensure that
 * their values are correctly applied.
 *
 * @param db, The pricedb
 * @param c, The commodity for which prices should be obtained.
 * @param t, The time nearest to which the prices should be obtained.
 * @return A PriceList of prices for each commodity pair found or NULL if none
 * are.
 */
PriceList * gnc_pricedb_lookup_nearest_in_time_any_currency_t64(GNCPriceDB *db,
        const gnc_commodity *c,
        time64 t);

/** @brief Return the latest price between the given commodities before the
 * given time.
 *
 * The returned GNCPrice may be in either direction so check to ensure that its
 * value is correctly applied.
 * @param db The pricedb
 * @param c The first commodity
 * @param currency The second commodity
 * @param t The time before which to find the price
 * @return A GNCPrice or NULL if no prices are found before t.
 */
/* NOT USED, but see bug 743753 */
GNCPrice * gnc_pricedb_lookup_latest_before_t64(GNCPriceDB *db,
                                                gnc_commodity *c,
                                                gnc_commodity *currency,
                                                time64 t);

/** @brief Return the latest price between the given commodity and any other
 * before the given time.
 *
 * The returned GNCPrice may be in either direction so check to ensure that its
 * value is correctly applied.
 * @param db The pricedb
 * @param c The commodity
 * @param t The time before which to find prices
 * @return A PriceList of prices for each commodity found or NULL if none are.
 */
/* NOT USED, but see bug 743753 */
PriceList * gnc_pricedb_lookup_latest_before_any_currency_t64(GNCPriceDB *db,
                                                         const gnc_commodity *c,
                                                              time64 t);


/** @brief Convert a balance from one currency to another using the most recent
 * price between the two.
 * @param pdb The pricedb
 * @param balance The balance to be converted
 * @param balance_currency The commodity in which the balance is currently
 * expressed
 * @param new_currency The commodity to which the balance should be converted
 * @return A new balance or gnc_numeric_zero if no price is available.
 */
gnc_numeric
gnc_pricedb_convert_balance_latest_price(GNCPriceDB *pdb,
                                         gnc_numeric balance,
                                         const gnc_commodity *balance_currency,
                                         const gnc_commodity *new_currency);

/** @brief Convert a balance from one currency to another using the price
 * nearest to the given time.
 * @param pdb The pricedb
 * @param balance The balance to be converted
 * @param balance_currency The commodity in which the balance is currently
 * expressed
 * @param new_currency The commodity to which the balance should be converted
 * @param t The time nearest to which price should be used.
 * @return A new balance or gnc_numeric_zero if no price is available.
 */
gnc_numeric
gnc_pricedb_convert_balance_nearest_price_t64(GNCPriceDB *pdb,
                                              gnc_numeric balance,
                                              const gnc_commodity *balance_currency,
                                              const gnc_commodity *new_currency,
                                              time64 t);

typedef gboolean (*GncPriceForeachFunc)(GNCPrice *p, gpointer user_data);

/** @brief Call a GncPriceForeachFunction once for each price in db, until the
 * function returns FALSE.
 *
 * If stable_order is not FALSE, make sure the ordering of the traversal is
 * stable (i.e. the same order every time given the same db contents -- stable
 * traversals may be less efficient).
 * @param db The pricedb
 * @param f The function to call
 * @param user_data A data to pass to each invocation of f
 * @param stable_order Ensure that the traversal is performed in the same order
 * each time.
 * @return TRUE if all calls to f succeeded (unstable) or if the order of
 * processing was the same as the previous invocation (stable), FALSE otherwise.
 */
gboolean     gnc_pricedb_foreach_price(GNCPriceDB *db,
                                       GncPriceForeachFunc f,
                                       gpointer user_data,
                                       gboolean stable_order);

/** @brief Get the number of prices, in any currency, for a given commodity.
 * @param db The pricedb
 * @param c The commodity
 * @return The number of prices in the database for this commody, zero if none
 */
int
gnc_pricedb_num_prices(GNCPriceDB *db,
                       const gnc_commodity *c);

/** @brief Get the nth price for the given commodity in  reverse date order
 * @param db The pricedb
 * @param c The commodity whose nth price is needed
 * @param n Zero based index of the price wanted
 * @return The nth price for this commodity in reverse chronological order, without
 * regard for what currency the price is in
 */
GNCPrice *
gnc_pricedb_nth_price (GNCPriceDB *db,
                       const gnc_commodity *c,
                       const int n);

void gnc_pricedb_nth_price_reset_cache (GNCPriceDB *db);

/* The following two convenience functions are used to test the xml backend */
/** @brief Return the number of prices in the database.
 *
 * For XML Backend Testing
 */
guint gnc_pricedb_get_num_prices(GNCPriceDB *db);

/** @brief Test equality of two pricedbs
 *
 * For XML Backend Testing */
gboolean gnc_pricedb_equal (GNCPriceDB *db1, GNCPriceDB *db2);

/** @name Internal/Debugging
    @{ */
/** This simple function can be useful for debugging the pricedb code */
void gnc_pricedb_print_contents(GNCPriceDB *db, FILE *f);
/** @} */

/** @name Price Parameter Names
 *  For use with QofQuery
 */
/**@{*/
#define PRICE_COMMODITY  "price-commodity"
#define PRICE_CURRENCY   "price-currency"
#define PRICE_DATE       "price-date"
#define PRICE_SOURCE     "price-source"
#define PRICE_TYPE       "price-type"
#define PRICE_VALUE      "price-value"
/**@}*/

/** @} */

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* GNC_PRICEDB_H */
/** @} */
/** @} */
