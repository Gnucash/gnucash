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

    Every QofBook contains a GNCPriceDB, accessable via
    gnc_book_get_pricedb.

    \warning The PriceDB does not currently use the object
    system used elsewhere in the GnuCash Engine, i.e. it does
    not use GUISD's, Entities and Collections.  Its should.
    In particular, this means that currently prices cannot
    be queried with the same emchanism as everything else.
*/

/** @addtogroup Price  Prices
    @ingroup Engine
    Each price in the database represents an "instantaneous" quote for
    a given commodity with respect to another commodity.  For example,
    a given price might represent the value of LNUX in USD on
    2001-02-03.  

    Fields:

      commodity: the item being priced.

      currency: the denomination of the value of the item being priced.    

      value: the value of the item being priced.

      time: the time the price was valid.

      source: a string describing the source of the quote.  These
        strings will be something like this: "Finance::Quote",
        "user:misc", "user:foo", etc.  If the quote came from a user,
        as a matter of policy, you *must* prefix the string you give
        with "user:".  For now, the only other reserved values are
        "Finance::Quote" and "old-file-import".  Any string used must
        be added to the source_list array in dialog-price-edit-db.c so
        that it can be properly translated. (There are unfortunately
        many strings in users databased, so this string must be
        translated on output instead of always being used intranslated
        form.)

      type: the type of quote - types possible right now are bid, ask,
        last, nav, and unknown.

    Implementation Details:

      NOTE: for source and type, NULL and the empty string are
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
typedef struct gnc_price_lookup_s GNCPriceLookup;
typedef GList PriceList;

/* ------------------ */
/** @name Constructors 
    @{ */

/** gnc_price_create - returns a newly allocated and initialized price
   with a reference count of 1. */
/*@ dependent @*/ GNCPrice *gnc_price_create(QofBook *book);

/** gnc_price_clone - returns a newly allocated price that's a
   content-wise duplicate of the given price, p.  The returned clone
   will have a reference count of 1. */
GNCPrice *gnc_price_clone(GNCPrice* p, QofBook *book);
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
/**  @} */

/* ------------------ */
/** @name  Setters
    @{ */

/* As mentioned above, all of the setters store copies of the data
 * given, with the exception of the commodity field which just stores
 * the pointer given.  It is assumed that commodities are a global
 * resource and are pointer unique. 
 *
 * Invocations of the setters should be wrapped with calls to
 * gnc_price_begin_edit() and commit_edit().  The begin/commit
 * calls help ensure that the local price db is synchronized with 
 * the backend.
 */
void gnc_price_begin_edit (GNCPrice *p);
void gnc_price_commit_edit (GNCPrice *p);

void gnc_price_set_commodity(GNCPrice *p, gnc_commodity *c);
void gnc_price_set_currency(GNCPrice *p, gnc_commodity *c);
void gnc_price_set_time(GNCPrice *p, Timespec t);
void gnc_price_set_source(GNCPrice *p, const char *source);
void gnc_price_set_typestr(GNCPrice *p, const char* type);
void gnc_price_set_value(GNCPrice *p, gnc_numeric value);
/**  @} */

/* ------------------ */
/** @name  Getters
    @{ */

/** As mentioned above all of the getters return data that's internal
   to the GNCPrice, not copies, so don't free these values. */
GNCPrice *      gnc_price_lookup (const GUID *guid, QofBook *book);
/*@ dependent @*/ gnc_commodity * gnc_price_get_commodity(const GNCPrice *p);
/*@ dependent @*/ gnc_commodity * gnc_price_get_currency(const GNCPrice *p);
Timespec        gnc_price_get_time(const GNCPrice *p);
const char *    gnc_price_get_source(const GNCPrice *p);
const char *    gnc_price_get_typestr(const GNCPrice *p);
gnc_numeric     gnc_price_get_value(const GNCPrice *p);
gboolean        gnc_price_equal(const GNCPrice *p1, const GNCPrice *p2);

#define gnc_price_get_guid(X)    qof_entity_get_guid(QOF_INSTANCE(X))
#define gnc_price_return_guid(X) (*(qof_entity_get_guid(QOF_INSTANCE(X))))
#define gnc_price_get_book(X)    qof_instance_get_book(QOF_INSTANCE(X))
/**  @} */

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

/* XXX backwards-compat defines, remove these someday */
#define gnc_book_get_pricedb  gnc_pricedb_get_db

/** return the pricedb associated with the book */
/*@ dependent @*/ GNCPriceDB * gnc_pricedb_get_db(QofBook *book);
GNCPriceDB * gnc_collection_get_pricedb(QofCollection *col);

/** gnc_pricedb_destroy - destroy the given pricedb and unref all of
     the prices it contains.  This may not deallocate all of those
     prices.  Other code may still be holding references to them. */
void gnc_pricedb_destroy(GNCPriceDB *db);

/** Used for editing the pricedb en-mass */
void gnc_pricedb_begin_edit (GNCPriceDB *);
void gnc_pricedb_commit_edit (GNCPriceDB *);

/** Indicate whether or not the database is in the middle of a bulk
 *  update.  Setting this flag will disable checks for duplicate
 *  entries. */
void gnc_pricedb_set_bulk_update(GNCPriceDB *db, gboolean bulk_update);

/** gnc_pricedb_add_price - add a price to the pricedb, you may drop
     your reference to the price (i.e. call unref) after this
     succeeds, whenever you're finished with the price. */
gboolean     gnc_pricedb_add_price(GNCPriceDB *db, GNCPrice *p);

/** gnc_pricedb_remove_price - removes the given price, p, from the
     pricedb.   Returns TRUE if successful, FALSE otherwise. */
gboolean     gnc_pricedb_remove_price(GNCPriceDB *db, GNCPrice *p);

gboolean     gnc_pricedb_remove_old_prices(GNCPriceDB *db, Timespec cutoff,
					   const gboolean delete_user, gboolean delete_last);

/** gnc_pricedb_lookup_latest - find the most recent price for the
     given commodity in the given currency.  Returns NULL on
     failure. */
GNCPrice   * gnc_pricedb_lookup_latest(GNCPriceDB *db,
                                       const gnc_commodity *commodity,
                                       const gnc_commodity *currency);

/** gnc_pricedb_lookup_latest_any_currency - find the most recent prices
     for the given commodity in any available currency. Prices will be
     returned as a GNCPrice list (see above). */
PriceList * gnc_pricedb_lookup_latest_any_currency(GNCPriceDB *db,
                                                 const gnc_commodity *commodity);

/** gnc_pricedb_has_prices - return an indication of whether or not
    there are any prices for a given commodity in the given currency.
    Returns TRUE if there are prices, FALSE otherwise. */
gboolean     gnc_pricedb_has_prices(GNCPriceDB *db,
                                    const gnc_commodity *commodity,
                                    const gnc_commodity *currency);

/** gnc_pricedb_get_prices - return all the prices for a given
     commodity in the given currency.  Returns NULL on failure.  The
     result is a GNCPrice list (see above).  */
PriceList * gnc_pricedb_get_prices(GNCPriceDB *db,
                                 const gnc_commodity *commodity,
                                 const gnc_commodity *currency);

/** gnc_pricedb_lookup_at_time - return all prices that match the given
     commodity, currency, and timespec.  Prices will be returned as a
     GNCPrice list (see above). */
PriceList * gnc_pricedb_lookup_at_time(GNCPriceDB *db,
                                     const gnc_commodity *commodity,
                                     const gnc_commodity *currency,
                                     Timespec t);

/** gnc_pricedb_lookup_at_time_any_currency - return all prices that match the
     given commodity and timespec in any available currency.  Prices will be
     returned as a GNCPrice list (see above). */
PriceList * gnc_pricedb_lookup_at_time_any_currency(GNCPriceDB *db,
                                                  const gnc_commodity *c,
                                                  Timespec t);

/** gnc_pricedb_lookup_day - return all prices that match the given
     commodity, currency, and timespec.  Prices will be returned as a
     GNCPrice list (see above). */
PriceList * gnc_pricedb_lookup_day(GNCPriceDB *db,
                                 const gnc_commodity *commodity,
                                 const gnc_commodity *currency,
                                 Timespec t);

/** gnc_pricedb_lookup_day_any_currency - return all prices that match the
     given commodity and timespec in any available currency.  Prices will be
     returned as a GNCPrice list (see above). */
PriceList * gnc_pricedb_lookup_day_any_currency(GNCPriceDB *db,
                                              const gnc_commodity *c,
                                              Timespec t);

/** gnc_pricedb_lookup_nearest_in_time - return the price for the given
     commodity in the given currency nearest to the given time t. */
GNCPrice   * gnc_pricedb_lookup_nearest_in_time(GNCPriceDB *db,
                                                const gnc_commodity *c,
                                                const gnc_commodity *currency,
                                                Timespec t);

/** gnc_pricedb_lookup_nearest_in_time_any_currency - return all prices that
     match the given commodity and timespec in any available currency. Prices
     will be returned as a GNCPrice list (see above). */
PriceList * gnc_pricedb_lookup_nearest_in_time_any_currency(GNCPriceDB *db,
                                                          const gnc_commodity *c,
                                                          Timespec t);
/** gnc_pricedb_lookup_latest_before - return the latest price for the given commodity
    in the given currency up to and including time t. */
GNCPrice * gnc_pricedb_lookup_latest_before(GNCPriceDB *db,
					    gnc_commodity *c,
					    gnc_commodity *currency,
					    Timespec t);

/** gnc_pricedb_lookup_latest_before_any_currency - return recent prices that
     match the given commodity up to and including time t in any available currency. Prices
     will be returned as a GNCPrice list (see above). */
PriceList * gnc_pricedb_lookup_latest_before_any_currency(GNCPriceDB *db,
                                                        gnc_commodity *c,
                                                        Timespec t);


/** gnc_pricedb_convert_balance_latest_price - Convert a balance
    from one currency to another. */
gnc_numeric
gnc_pricedb_convert_balance_latest_price(GNCPriceDB *pdb,
				         gnc_numeric balance,
				         const gnc_commodity *balance_currency,
				         const gnc_commodity *new_currency);

/** gnc_pricedb_convert_balance_nearest_price - Convert a balance
    from one currency to another. */
gnc_numeric
gnc_pricedb_convert_balance_nearest_price(GNCPriceDB *pdb,
				          gnc_numeric balance,
				          const gnc_commodity *balance_currency,
				          const gnc_commodity *new_currency,
					  Timespec t);

/** gnc_pricedb_convert_balance_latest_before - Convert a balance from one currency
    to another using the lastest price prior to Timespec t. */
gnc_numeric
gnc_pricedb_convert_balance_latest_before(GNCPriceDB *pdb,
                                          gnc_numeric balance,
                                          gnc_commodity *balance_currency,
                                          gnc_commodity *new_currency,
                                          Timespec t);


/** gnc_pricedb_foreach_price - call f once for each price in db, until
     and unless f returns FALSE.  If stable_order is not FALSE, make
     sure the ordering of the traversal is stable (i.e. the same order
     every time given the same db contents -- stable traversals may be
     less efficient).  */
gboolean     gnc_pricedb_foreach_price(GNCPriceDB *db,
                                       gboolean (*f)(GNCPrice *p,
                                                     gpointer user_data),
                                       gpointer user_data,
                                       gboolean stable_order);

/** gnc_pricedb_get_num_prices - return the number of prices
   in the database. */
guint gnc_pricedb_get_num_prices(GNCPriceDB *db);

gboolean gnc_pricedb_equal (GNCPriceDB *db1, GNCPriceDB *db2);

/** semi-lame debugging code */
void gnc_price_print(GNCPrice *db, FILE *f, int indent);
void gnc_pricedb_print_contents(GNCPriceDB *db, FILE *f);


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

#endif /* GNC_PRICEDB_H */
/** @} */
/** @} */
