/********************************************************************
 * gnc-pricedb.h -- a simple price database for gnucash.            *
 * Copyright (C) 2001 Rob Browning                                  *
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

#ifndef __GNC_PRICEDB_H__
#define __GNC_PRICEDB_H__

#include "date.h"
#include "gnc-numeric.h"
#include "gnc-commodity.h"

#include <stdio.h>

/**********************************************************************\

  GNCPrice:

    commodity: the item being priced.

    value: the value of the item being priced.

    currency: the denomination of the value of the item being priced.    

    time: the time the price was valid.

    source: a string describing the source of the quote.  These
    strings will be something like this: "Finance::Quote",
    "user:misc", "user:foo", etc.  If the quote came from a user, as a
    matter of policy, you *must* prefix the string you give with
    "user:".  For now, the only other reserved values are
    "Finance::Quote" and "old-file-import".

    type: the type of quote - types possible right now are bid, ask,
    last, and unknown.

    NOTE: for source and type, NULL and the empty string are
    considered the same, so if one of these is "", then after a file
    save/restore, it might be NULL.  Behave accordingly.

    GNCPrices are reference counted.  When you gnc_price_create one or
    clone it, the new price's count is set to 1.  When you're finished
    with a price, call gnc_price_unref.  If you hand the pointer to
    some other code that needs to keep it, make sure it calls
    gnc_price_ref to indicate it's interest in that price, and calls
    gnc_price_unref when it's finished with the price.

    All of the getters return data that's internal to the GNCPrice,
    not copies, so don't free these values.

    All of the setters store copies of the data, with the exception of
    the commodity field which just stores the pointer given.  It is
    assumed that commodities are a global resource and are pointer
    unique.

 */

typedef struct _GNCPrice GNCPrice;

/* allocation */
GNCPrice *gnc_price_create(void);     /* create and initialize a price */
GNCPrice *gnc_price_clone(GNCPrice* p);

void      gnc_price_ref(GNCPrice *p);
void      gnc_price_unref(GNCPrice *p);

/* setters */
void gnc_price_set_commodity(GNCPrice *p, gnc_commodity *c);
void gnc_price_set_currency(GNCPrice *p, gnc_commodity *c);
void gnc_price_set_time(GNCPrice *p, Timespec t);
void gnc_price_set_source(GNCPrice *p, const char *source);
void gnc_price_set_type(GNCPrice *p, const char* type);
void gnc_price_set_value(GNCPrice *p, gnc_numeric value);

/* getters */
gnc_commodity * gnc_price_get_commodity(GNCPrice *p);
gnc_commodity * gnc_price_get_currency(GNCPrice *p);
Timespec        gnc_price_get_time(GNCPrice *p);
const char *    gnc_price_get_source(GNCPrice *p);
const char *    gnc_price_get_type(GNCPrice *p);
gnc_numeric     gnc_price_get_value(GNCPrice *p);

/* price_list funcs

   A price list is a time sorted GList of prices.  The price list
   maintains a ref for itself for all the prices in the list
   (i.e. adding a price calls gnc_price_ref on it, removing a price
   calls gnc_price_unref, etc.  Destroying a list also removes this
   list reference from the prices in the list. */
gboolean gnc_price_list_insert(GList **prices, GNCPrice *p);
gboolean gnc_price_list_remove(GList **prices, GNCPrice *p);
void     gnc_price_list_destroy(GList *prices);


/**********************************************************************
  GNCPriceDB

  Whenever a you store a price in the pricedb, the pricedb adds its
  own reference to the price, so you can safely unref that price when
  you're finished with it.

  Similarly, when the pricedb returns a price to you, either singly,
  or in a price list, the price will have had a ref added for you, so
  you only need to unref the price(s) when you're finished with them.

*/

typedef struct _GNCPriceDB GNCPriceDB;

GNCPriceDB * gnc_pricedb_create(void);

void         gnc_pricedb_destroy(GNCPriceDB *db);

/* Add a price to the pricedb, you may drop your reference to the
   price (i.e. call unref) after this succeeds, whenever you're
   finished with the price. */
gboolean     gnc_pricedb_add_price(GNCPriceDB *db, GNCPrice *p);

/* Remove a price from the pricedb. */
gboolean     gnc_pricedb_remove_price(GNCPriceDB *db, GNCPrice *p);

GNCPrice   * gnc_pricedb_lookup_latest(GNCPriceDB *db,
                                       gnc_commodity *commodity,
                                       gnc_commodity *currency);

/* Return all prices that match the given commodity, currency, and
   timespec.  Prices will be returned as a price_list (see above) */
GList      * gnc_pricedb_lookup_at_time(GNCPriceDB *db,
                                        gnc_commodity *commodity,
                                        gnc_commodity *currency,
                                        Timespec t);

/* Call f once for each price in db, until and unless f returns FALSE.
   If stable_order is not FALSE, make sure the ordering of the
   traversal is stable (i.e. the same order every time given the same
   db contents).  */
gboolean     gnc_pricedb_foreach_price(GNCPriceDB *db,
                                       gboolean (*f)(GNCPrice *p,
                                                     gpointer user_data),
                                       gpointer user_data,
                                       gboolean stable_order);

/* Return FALSE if the database has not been modified */
gboolean gnc_pricedb_dirty(GNCPriceDB *p);

#if 0
/* semi-lame debugging code */
void gnc_price_print(GNCPrice *db, FILE *f);
void gnc_pricedb_print_contents(GNCPriceDB *db, FILE *f);
#endif

#endif
