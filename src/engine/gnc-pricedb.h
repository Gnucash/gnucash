/********************************************************************
 * gnc-pricedb.h -- a simple price database for gnucash.            *
 * Copyright (C) 2001 Rob Browning, Linas Vepstas                   *
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
 *******************************************************************/

#ifndef GNC_PRICEDB_H
#define GNC_PRICEDB_H

#include "date.h"
#include "gnc-numeric.h"
#include "gnc-commodity.h"
#include "guid.h"

#include <stdio.h>

/**********************************************************************\

  Introduction:

    The PriceDB is intended to be a database of price quotes, or more
    specifically, a database of GNCPrices.  For the time being, it is
    still a fairly simple database supporting only fairly simple
    queries.  It is expected that new queries will be added as needed,
    and that there is some advantage to delaying complex queries for
    now in the hope that we get a real DB implementation before
    they're really needed.

    Every GNCBook contains a GNCPriceDB, accessable via
    gnc_book_get_pricedb.

*/


/**********************************************************************\

  GNCPrice:

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
        "Finance::Quote" and "old-file-import".

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

typedef struct _GNCPrice GNCPrice;
typedef struct _GNCPriceLookup GNCPriceLookup;

/****************/
/* constructors */

/* gnc_price_create - returns a newly allocated and initialized price
   with a reference count of 1. */
GNCPrice *gnc_price_create(void);

/* gnc_price_clone - returns a newly allocated price that's a
   content-wise duplicate of the given price, p.  The returned clone
   will have a reference count of 1. */
GNCPrice *gnc_price_clone(GNCPrice* p);

/*********************/
/* memory management */

/* gnc_price_ref - indicate your need for a given price to stick
   around (i.e. increase its reference count by 1). */
void      gnc_price_ref(GNCPrice *p);

/* gnc_price_ref - indicate you're finished with a price
   (i.e. decrease its reference count by 1). */
void      gnc_price_unref(GNCPrice *p);

/***********/
/* setters */

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
void gnc_price_set_type(GNCPrice *p, const char* type);
void gnc_price_set_value(GNCPrice *p, gnc_numeric value);
void gnc_price_set_version(GNCPrice *p, gint32 versn);

/***********/
/* getters */

/* As mentioned above all of the getters return data that's internal
   to the GNCPrice, not copies, so don't free these values. */
GNCPrice *      gnc_price_lookup (const GUID *guid);
const GUID *    gnc_price_get_guid (GNCPrice *p);
gnc_commodity * gnc_price_get_commodity(GNCPrice *p);
gnc_commodity * gnc_price_get_currency(GNCPrice *p);
Timespec        gnc_price_get_time(GNCPrice *p);
const char *    gnc_price_get_source(GNCPrice *p);
const char *    gnc_price_get_type(GNCPrice *p);
gnc_numeric     gnc_price_get_value(GNCPrice *p);
gint32          gnc_price_get_version(GNCPrice *p);

/**********************************************************************
  GNCPrice lists:

    The database communicates multiple prices in and out via gnc price
    lists.  These are just time sorted GLists of GNCPrice pointers.
    Functions for manipulating these lists are provided.  These
    functions are helpful in that they handle maintaining proper
    reference counts on behalf of the price list for every price being
    held in a given list.  I.e. insert "refs" the prices being
    inserted, remove and destroy "unref" the prices that will no
    longer be referred to by the list.

*/

/* gnc_price_list_insert - insert a price into the given list, calling
     gnc_price_ref on it during the process. */
gboolean gnc_price_list_insert(GList **prices, GNCPrice *p);
/* gnc_price_list_remove - remove the price, p, from the given list,
     calling gnc_price_unref on it during the process. */
gboolean gnc_price_list_remove(GList **prices, GNCPrice *p);
/* gnc_price_list_destroy - destroy the given price list, calling
     gnc_price_unref on all the prices included in the list. */
void     gnc_price_list_destroy(GList *prices);


/**********************************************************************
  GNCPriceDB

  Whenever a you store a price in the pricedb, the pricedb adds its
  own reference to the price, so you can safely unref that price after
  inserting it into the DB if you're finished with it otherwise.

  Similarly, when the pricedb returns a price to you, either singly,
  or in a price list, the price will have had a ref added for you, so
  you only need to unref the price(s) when you're finished with
  it/them.

*/

typedef struct _GNCPriceDB GNCPriceDB;


/* gnc_pricedb_create - create a new pricedb.  Normally you won't need
     this; you will get the pricedb via gnc_book_get_pricedb. */
GNCPriceDB * gnc_pricedb_create(void);

/* gnc_pricedb_destroy - destroy the given pricedb and unref all of
     the prices it contains.  This may not deallocate all of those
     prices.  Other code may still be holding references to them. */
void gnc_pricedb_destroy(GNCPriceDB *db);

/* gnc_pricedb_add_price - add a price to the pricedb, you may drop
     your reference to the price (i.e. call unref) after this
     succeeds, whenever you're finished with the price. */
gboolean     gnc_pricedb_add_price(GNCPriceDB *db, GNCPrice *p);

/* gnc_pricedb_remove_price - removes the given price, p, from the
     pricedb.   Returns TRUE if successful, FALSE otherwise. */
gboolean     gnc_pricedb_remove_price(GNCPriceDB *db, GNCPrice *p);

/* gnc_pricedb_lookup_latest - find the most recent price for the
     given commodity in the given currency.  Returns NULL on
     failure. */
GNCPrice   * gnc_pricedb_lookup_latest(GNCPriceDB *db,
                                       gnc_commodity *commodity,
                                       gnc_commodity *currency);

/* gnc_pricedb_get_prices - return all the prices for a given
     commodity in the given currency.  Returns NULL on failure.  The
     result is a GNCPrice list (see above).  */
GList      * gnc_pricedb_get_prices(GNCPriceDB *db,
                                    gnc_commodity *commodity,
                                    gnc_commodity *currency);

/* gnc_pricedb_lookup_at_time - return all prices that match the given
     commodity, currency, and timespec.  Prices will be returned as a
     GNCPrice list (see above). */
GList      * gnc_pricedb_lookup_at_time(GNCPriceDB *db,
                                        gnc_commodity *commodity,
                                        gnc_commodity *currency,
                                        Timespec t);

/* gnc_pricedb_lookup_nearest_in_time - return the price for the given
     commodity in the given currency nearest to the given time t. */
GNCPrice *
gnc_pricedb_lookup_nearest_in_time(GNCPriceDB *db,
                                   gnc_commodity *c,
                                   gnc_commodity *currency,
                                   Timespec t);

/* gnc_pricedb_foreach_price - call f once for each price in db, until
     and unless f returns FALSE.  If stable_order is not FALSE, make
     sure the ordering of the traversal is stable (i.e. the same order
     every time given the same db contents -- stable traversals may be
     less efficient).  */
gboolean     gnc_pricedb_foreach_price(GNCPriceDB *db,
                                       gboolean (*f)(GNCPrice *p,
                                                     gpointer user_data),
                                       gpointer user_data,
                                       gboolean stable_order);

/* gnc_pricedb_dirty - return FALSE if the database has not been
   modified. */
gboolean gnc_pricedb_dirty(GNCPriceDB *db);

/* gnc_pricedb_get_num_prices - return the number of prices
   in the database. */
guint gnc_pricedb_get_num_prices(GNCPriceDB *db);

/* semi-lame debugging code */
void gnc_price_print(GNCPrice *db, FILE *f, int indent);
void gnc_pricedb_print_contents(GNCPriceDB *db, FILE *f);

#endif
