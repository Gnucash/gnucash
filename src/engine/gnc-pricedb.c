/********************************************************************
 * gnc-pricedb.c -- a simple price database for gnucash.            *
 * Copyright (C) 2001 Rob Browning                                  *
 * Copyright (C) 2001,2003 Linas Vepstas <linas@linas.org>          *
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

#include "config.h"

#include <glib.h>
#include <string.h>

#include "gnc-engine.h"
#include "gnc-engine-util.h"
#include "gnc-event-p.h"
#include "gnc-pricedb-p.h"
#include "guid.h"
#include "kvp-util.h"
#include "qofbackend-p.h"
#include "qofbook.h"
#include "qofbook-p.h"
#include "qofid-p.h"
#include "qofobject.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_PRICE;

static gboolean add_price(GNCPriceDB *db, GNCPrice *p);
static gboolean remove_price(GNCPriceDB *db, GNCPrice *p, gboolean cleanup);

/* ==================================================================== */
/* GNCPrice functions
 */

/* allocation */
GNCPrice *
gnc_price_create (QofBook *book)
{
  GNCPrice *p;

  g_return_val_if_fail (book, NULL);

  p = g_new0(GNCPrice, 1);

  p->refcount = 1;
  p->editlevel = 0;
  p->not_saved = FALSE;
  p->do_free = FALSE;
  p->version = 0;
  p->version_check = 0;
  p->value = gnc_numeric_zero();

  p->book = book;
  p->entity_table = qof_book_get_entity_table (book);

  qof_entity_guid_new (p->entity_table, &p->guid);
  qof_entity_store (p->entity_table, p, &p->guid, GNC_ID_PRICE); 
  gnc_engine_generate_event (&p->guid, GNC_EVENT_CREATE);

  return p;
}

static void 
gnc_price_destroy (GNCPrice *p)
{
  ENTER(" ");
  gnc_engine_generate_event (&p->guid, GNC_EVENT_DESTROY);
  qof_entity_remove(p->entity_table, &p->guid);

  if(p->type) g_cache_remove(gnc_engine_get_string_cache(), p->type);
  if(p->source) g_cache_remove(gnc_engine_get_string_cache(), p->source);

  memset(p, 0, sizeof(GNCPrice));
  g_free(p);
}

void
gnc_price_ref(GNCPrice *p)
{
  if(!p) return;
  p->refcount++;
}

void
gnc_price_unref(GNCPrice *p)
{
  if(!p) return;
  ENTER("pr=%p refcount=%d", p, p->refcount);
  if(p->refcount == 0) {
    PERR("refcount == 0 !!!!");
    return;
  }

  p->refcount--;

  if(p->refcount <= 0) {
    if (NULL != p->db) {
       PERR("last unref while price in database");
    }
    gnc_price_destroy (p);
  }
}

/* ==================================================================== */

GNCPrice *
gnc_price_clone (GNCPrice* p, QofBook *book)
{
  /* the clone doesn't belong to a PriceDB */
  GNCPrice *new_p;

  ENTER ("pr=%p", p);

  g_return_val_if_fail (book, NULL);

  if(!p) return NULL;

  new_p = gnc_price_create(book);
  if(!new_p) return NULL;

  new_p->version = p->version;

  gnc_price_begin_edit(new_p);
  /* never ever clone guid's */
  gnc_price_set_commodity(new_p, gnc_price_get_commodity(p));
  gnc_price_set_time(new_p, gnc_price_get_time(p));
  gnc_price_set_source(new_p, gnc_price_get_source(p));
  gnc_price_set_type(new_p, gnc_price_get_type(p));
  gnc_price_set_value(new_p, gnc_price_get_value(p));
  gnc_price_set_currency(new_p, gnc_price_get_currency(p));
  gnc_price_commit_edit(new_p);

  return(new_p);
}

/* ==================================================================== */

void 
gnc_price_begin_edit (GNCPrice *p)
{
  if (!p) return;
  p->editlevel++;
  if (1 < p->editlevel) return;
  ENTER ("pr=%p, not-saved=%d do-free=%d", p, p->not_saved, p->do_free);

  if (0 >= p->editlevel)
  {
    PERR ("unbalanced call - resetting (was %d)", p->editlevel);
    p->editlevel = 1;
  }

  /* See if there's a backend.  If there is, invoke it. */
  /* We may not be able to find the backend, so make not of that .. */
  if (p->db) {
    QofBackend *be;
    be = xaccPriceDBGetBackend (p->db);
    if (be && be->begin) {
       (be->begin) (be, GNC_ID_PRICE, p);
    }
    p->not_saved = FALSE;
  } else {
    p->not_saved = TRUE;
  }

  LEAVE ("pr=%p, not-saved=%d do-free=%d", p, p->not_saved, p->do_free);
}

void 
gnc_price_commit_edit (GNCPrice *p)
{
  if (!p) return;

  p->editlevel--;
  if (0 < p->editlevel) return;

  ENTER ("pr=%p, not-saved=%d do-free=%d", p, p->not_saved, p->do_free);
  if (0 > p->editlevel)
  {
    PERR ("unbalanced call - resetting (was %d)", p->editlevel);
    p->editlevel = 0;
  }

  /* See if there's a backend.  If there is, invoke it. */
  /* We may not be able to find the backend, so make not of that .. */
  if (p->db) {
    QofBackend *be;
    be = xaccPriceDBGetBackend (p->db);
    if (be && be->commit) {
      QofBackendError errcode;

      /* clear errors */
      do {
        errcode = qof_backend_get_error (be);
      } while (ERR_BACKEND_NO_ERR != errcode);

      /* if we haven't been able to call begin edit before, call it now */
      if (TRUE == p->not_saved) {
        if (be->begin) {
          (be->begin) (be, GNC_ID_PRICE, p);
        }
      }

      (be->commit) (be, GNC_ID_PRICE, p);
      errcode = qof_backend_get_error (be);
      if (ERR_BACKEND_NO_ERR != errcode) 
      {
        /* XXX hack alert FIXME implement price rollback */
        PERR (" backend asked engine to rollback, but this isn't"
              " handled yet. Return code=%d", errcode);

        /* push error back onto the stack */
        qof_backend_set_error (be, errcode);
      }
    }
    p->not_saved = FALSE;
  } else {
    p->not_saved = TRUE;
  }
  LEAVE ("pr=%p, not-saved=%d do-free=%d", p, p->not_saved, p->do_free);
}

/* ==================================================================== */
/* setters */

void 
gnc_price_set_guid (GNCPrice *p, const GUID *guid)
{
   if (!p || !guid) return;
   qof_entity_remove (p->entity_table, &p->guid);
   p->guid = *guid;
   if(p->db) p->db->dirty = TRUE;
   qof_entity_store(p->entity_table, p, &p->guid, GNC_ID_PRICE);
}

void
gnc_price_set_commodity(GNCPrice *p, gnc_commodity *c)
{
  if(!p) return;

  if(!gnc_commodity_equiv(p->commodity, c)) 
  {
    /* Changing the commodity requires the hash table 
     * position to be modified. The easiest way of doing 
     * this is to remove and reinsert. */
    gnc_price_ref (p);
    remove_price (p->db, p, TRUE);
    gnc_price_begin_edit (p);
    p->commodity = c;
    if(p->db) p->db->dirty = TRUE;
    gnc_price_commit_edit (p);
    add_price (p->db, p);
    gnc_price_unref (p);
  }
}


void
gnc_price_set_currency(GNCPrice *p, gnc_commodity *c)
{
  if(!p) return;

  if(!gnc_commodity_equiv(p->currency, c)) 
  {
    /* Changing the currency requires the hash table 
     * position to be modified. The easiest way of doing 
     * this is to remove and reinsert. */
    gnc_price_ref (p);
    remove_price (p->db, p, TRUE);
    gnc_price_begin_edit (p);
    p->currency = c;
    if(p->db) p->db->dirty = TRUE;
    gnc_price_commit_edit (p);
    add_price (p->db, p);
    gnc_price_unref (p);
  }
}

void
gnc_price_set_time(GNCPrice *p, Timespec t)
{
  if(!p) return;
  if(!timespec_equal(&(p->tmspec), &t)) 
  {
    /* Changing the datestamp requires the hash table 
     * position to be modified. The easiest way of doing 
     * this is to remove and reinsert. */
    gnc_price_ref (p);
    remove_price (p->db, p, FALSE);
    gnc_price_begin_edit (p);
    p->tmspec = t;
    if(p->db) p->db->dirty = TRUE;
    gnc_price_commit_edit (p);
    add_price (p->db, p);
    gnc_price_unref (p);
  }
}

void
gnc_price_set_source(GNCPrice *p, const char *s)
{
  if(!p) return;
  if(safe_strcmp(p->source, s) != 0) 
  {
    GCache *cache;
    char *tmp;

    gnc_price_begin_edit (p);
    cache = gnc_engine_get_string_cache();
    tmp = g_cache_insert(cache, (gpointer) s);
    if(p->source) g_cache_remove(cache, p->source);
    p->source = tmp;
    if(p->db) p->db->dirty = TRUE;
    gnc_price_commit_edit (p);
  }
}

void
gnc_price_set_type(GNCPrice *p, const char* type)
{
  if(!p) return;
  if(safe_strcmp(p->type, type) != 0) 
  {
    GCache *cache;
    gchar *tmp;

    gnc_price_begin_edit (p);
    cache = gnc_engine_get_string_cache();
    tmp = g_cache_insert(cache, (gpointer) type);
    if(p->type) g_cache_remove(cache, p->type);
    p->type = tmp;
    if(p->db) p->db->dirty = TRUE;
    gnc_price_commit_edit (p);
  }
}

void
gnc_price_set_value(GNCPrice *p, gnc_numeric value)
{
  if(!p) return;
  if(!gnc_numeric_eq(p->value, value)) 
  {
    gnc_price_begin_edit (p);
    p->value = value;
    if(p->db) p->db->dirty = TRUE;
    gnc_price_commit_edit (p);
  }
}

void
gnc_price_set_version(GNCPrice *p, gint32 vers)
{
  /* begin/end edit is inappropriate here, this is a backend thing only. */
  if(!p) return;
  p->version = vers;
}


/* ==================================================================== */
/* getters */

GNCPrice *
gnc_price_lookup (const GUID *guid, QofBook *book)
{
  if (!guid) return NULL;
  g_return_val_if_fail (book, NULL);
  return qof_entity_lookup (qof_book_get_entity_table (book),
                           guid, GNC_ID_PRICE);
}

const GUID *
gnc_price_get_guid (GNCPrice *p)
{
  if (!p) return guid_null();
  return &p->guid;
}

const GUID
gnc_price_return_guid (GNCPrice *p)
{
  if (!p) return *guid_null();
  return p->guid;
}

QofBook *
gnc_price_get_book (GNCPrice *p)
{
  if (!p) return NULL;
  return p->book;
}

gnc_commodity *
gnc_price_get_commodity(GNCPrice *p)
{
  if(!p) return NULL;
  return p->commodity;
}

Timespec
gnc_price_get_time(GNCPrice *p)
{
  if(!p) {
    Timespec result;
    result.tv_sec = 0;
    result.tv_nsec = 0;
    return result;
  }
  return p->tmspec;
}

const char *
gnc_price_get_source(GNCPrice *p)
{
  if(!p) return NULL;
  return p->source;
}

const char *
gnc_price_get_type(GNCPrice *p)
{
  if(!p) return NULL;
  return p->type;
}

gnc_numeric
gnc_price_get_value(GNCPrice *p)
{
  if(!p) {
    PERR("price NULL.\n");
    return gnc_numeric_zero();
  }
  return p->value;
}

gnc_commodity *
gnc_price_get_currency(GNCPrice *p)
{
  if(!p) return NULL;
  return p->currency;
}

gint32
gnc_price_get_version(GNCPrice *p)
{
  if(!p) return 0;
  return (p->version);
}

gboolean
gnc_price_equal (GNCPrice *p1, GNCPrice *p2)
{
  Timespec ts1;
  Timespec ts2;

  if (p1 == p2) return TRUE;
  if (!p1 || !p2) return FALSE;

  if (!gnc_commodity_equiv (gnc_price_get_commodity (p1),
                            gnc_price_get_commodity (p2)))
    return FALSE;

  if (!gnc_commodity_equiv (gnc_price_get_currency (p1),
                            gnc_price_get_currency (p2)))
    return FALSE;

  ts1 = gnc_price_get_time (p1);
  ts2 = gnc_price_get_time (p2);

  if (!timespec_equal (&ts1, &ts2))
    return FALSE;

  if (safe_strcmp (gnc_price_get_source (p1),
                   gnc_price_get_source (p2)) != 0)
    return FALSE;

  if (safe_strcmp (gnc_price_get_type (p1),
                   gnc_price_get_type (p2)) != 0)
    return FALSE;

  if (!gnc_numeric_eq (gnc_price_get_value (p1),
                       gnc_price_get_value (p2)))
    return FALSE;

  return TRUE;
}

/* ==================================================================== */
/* price list manipulation functions */

static gint
compare_prices_by_date(gconstpointer a, gconstpointer b)
{
  Timespec time_a;
  Timespec time_b;
  gint result;

  if(!a && !b) return 0;
  /* nothing is always less than something */
  if(!a) return -1;

  time_a = gnc_price_get_time((GNCPrice *) a);
  time_b = gnc_price_get_time((GNCPrice *) b);

  result = -timespec_cmp(&time_a, &time_b);
  if (result) return result;

  /* For a stable sort */
  return guid_compare (gnc_price_get_guid((GNCPrice *) a),
                       gnc_price_get_guid((GNCPrice *) b));
}

gboolean
gnc_price_list_insert(GList **prices, GNCPrice *p)
{
  GList *result_list;

  if(!prices || !p) return FALSE;
  gnc_price_ref(p);
  result_list = g_list_insert_sorted(*prices, p, compare_prices_by_date);
  if(!result_list) return FALSE;
  *prices = result_list;
  return TRUE;
}

gboolean
gnc_price_list_remove(GList **prices, GNCPrice *p)
{
  GList *result_list;
  GList *found_element;

  if(!prices || !p) return FALSE;
  
  found_element = g_list_find(*prices, p);
  if(!found_element) return TRUE;

  result_list = g_list_remove_link(*prices, found_element);
  gnc_price_unref((GNCPrice *) found_element->data);
  g_list_free(found_element);

  *prices = result_list;
  return TRUE;
}

static void
price_list_destroy_helper(gpointer data, gpointer user_data)
{
  gnc_price_unref((GNCPrice *) data);
}

void
gnc_price_list_destroy(GList *prices)
{
  g_list_foreach(prices, price_list_destroy_helper, NULL);
  g_list_free(prices);
}

gboolean
gnc_price_list_equal(GList *prices1, GList *prices2)
{
  GList *n1, *n2;

  if (prices1 == prices2) return TRUE;

  if (g_list_length (prices1) < g_list_length (prices2))
  {
    PWARN ("prices2 has extra prices");
    return FALSE;
  }

  if (g_list_length (prices1) > g_list_length (prices2))
  {
    PWARN ("prices1 has extra prices");
    return FALSE;
  }

  for (n1 = prices1, n2 = prices2; n1 ; n1 = n1->next, n2 = n2->next)
    if (!gnc_price_equal (n1->data, n2->data))
      return FALSE;

  return TRUE;
}

/* ==================================================================== */
/* GNCPriceDB functions

   Structurally a GNCPriceDB contains a hash mapping price commodities
   (of type gnc_commodity*) to hashes mapping price currencies (of
   type gnc_commodity*) to GNCPrice lists (see gnc-pricedb.h for a
   description of GNCPrice lists).  The top-level key is the commodity
   you want the prices for, and the second level key is the commodity
   that the value is expressed in terms of.

 */

static guint
commodity_hash (gconstpointer key)
{
  gnc_commodity * com = (gnc_commodity *) key;

  g_return_val_if_fail (key, 0);

  return g_str_hash (gnc_commodity_get_unique_name (com));
}

static gint
commodity_equal (gconstpointer a, gconstpointer b)
{
  gnc_commodity * ca = (gnc_commodity *) a;
  gnc_commodity * cb = (gnc_commodity *) b;

  g_return_val_if_fail (a && b, FALSE);

  return gnc_commodity_equiv (ca, cb);
}

GNCPriceDB *
gnc_pricedb_create(QofBook * book)
{
  GNCPriceDB * result;

  g_return_val_if_fail (book, NULL);

  result = g_new0(GNCPriceDB, 1);
  result->book = book;
  result->commodity_hash = g_hash_table_new(commodity_hash, commodity_equal);
  g_return_val_if_fail (result->commodity_hash, NULL);
  return result;
}

static void
destroy_pricedb_currency_hash_data(gpointer key,
                                   gpointer data,
                                   gpointer user_data)
{
  GList *price_list = (GList *) data;
  GList *node;

  for (node = price_list; node; node = node->next)
  {
    GNCPrice *p = node->data;

    p->db = NULL;
  }

  gnc_price_list_destroy(price_list);
}

static void
destroy_pricedb_commodity_hash_data(gpointer key,
                                    gpointer data,
                                    gpointer user_data)
{
  GHashTable *currency_hash = (GHashTable *) data;
  if (!currency_hash) return;
  g_hash_table_foreach (currency_hash,
                        destroy_pricedb_currency_hash_data,
                        NULL);
  g_hash_table_destroy(currency_hash);
}

void
gnc_pricedb_destroy(GNCPriceDB *db)
{
  if(!db) return;
  g_hash_table_foreach (db->commodity_hash,
                        destroy_pricedb_commodity_hash_data,
                        NULL);
  g_hash_table_destroy (db->commodity_hash);
  db->commodity_hash = NULL;
  db->book = NULL;
  g_free(db);
}

/* ==================================================================== */

#define GNC_PRICEDB "gnc_pricedb"

GNCPriceDB *
gnc_pricedb_get_db(QofBook *book)
{
  if (!book) return NULL;
  return qof_book_get_data (book, GNC_PRICEDB);
}

void
gnc_pricedb_set_db(QofBook *book, GNCPriceDB *db)
{
  GNCPriceDB *old_db;
  
  if(!book) return;
  
  old_db = gnc_pricedb_get_db (book);
  if (db == old_db) return;
  
  qof_book_set_data (book, GNC_PRICEDB, db);
  if (db) db->book = book;

  gnc_pricedb_destroy (old_db);
}

/* ==================================================================== */

gboolean
gnc_pricedb_dirty(GNCPriceDB *p)
{
  if(!p) return FALSE;
  return p->dirty;
}

void
gnc_pricedb_mark_clean(GNCPriceDB *p)
{
  if(!p) return;
  p->dirty = FALSE;
}

/* ==================================================================== */

static gboolean
num_prices_helper (GNCPrice *p, gpointer user_data)
{
  guint *count = user_data;

  *count += 1;

  return TRUE;
}

guint
gnc_pricedb_get_num_prices(GNCPriceDB *db)
{
  guint count;

  if (!db) return 0;

  count = 0;

  gnc_pricedb_foreach_price(db, num_prices_helper, &count, FALSE);

  return count;
}

/* ==================================================================== */

typedef struct
{
  gboolean equal;
  GNCPriceDB *db2;
  gnc_commodity *commodity;
} GNCPriceDBEqualData;

static void
pricedb_equal_foreach_pricelist(gpointer key, gpointer val, gpointer user_data)
{
  GNCPriceDBEqualData *equal_data = user_data;
  gnc_commodity *currency = key;
  GList *price_list1 = val;
  GList *price_list2;

  price_list2 = gnc_pricedb_get_prices (equal_data->db2,
                                        equal_data->commodity,
                                        currency);

  if (!gnc_price_list_equal (price_list1, price_list2))
    equal_data->equal = FALSE;

  gnc_price_list_destroy (price_list2);
}

static void
pricedb_equal_foreach_currencies_hash (gpointer key, gpointer val,
                                       gpointer user_data)
{
  GHashTable *currencies_hash = val;
  GNCPriceDBEqualData *equal_data = user_data;

  equal_data->commodity = key;

  g_hash_table_foreach (currencies_hash,
                        pricedb_equal_foreach_pricelist,
                        equal_data);
}

gboolean
gnc_pricedb_equal (GNCPriceDB *db1, GNCPriceDB *db2)
{
  GNCPriceDBEqualData equal_data;

  if (db1 == db2) return TRUE;

  if (!db1 || !db2)
  {
    PWARN ("one is NULL");
    return FALSE;
  }

  equal_data.equal = TRUE;
  equal_data.db2 = db2;

  g_hash_table_foreach (db1->commodity_hash,
                        pricedb_equal_foreach_currencies_hash,
                        &equal_data);

  return equal_data.equal;
}

/* ==================================================================== */
/* The add_price() function is a utility that only manages the 
 * dual hash table instertion */

static gboolean
add_price(GNCPriceDB *db, GNCPrice *p)
{
  /* This function will use p, adding a ref, so treat p as read-only
     if this function succeeds. */
  GList *price_list;
  gnc_commodity *commodity;
  gnc_commodity *currency;
  GHashTable *currency_hash;

  if(!db || !p) return FALSE;
  ENTER ("db=%p, pr=%p not-saved=%d do-free=%d",
         db, p, p->not_saved, p->do_free);

  /* initialize the book pointer for teh first time, if needed */
  if (NULL == db->book) db->book = p->book;
  if (db->book != p->book)
  {
     PERR ("attempted to mix up prices across different books");
     return FALSE;
  }

  commodity = gnc_price_get_commodity(p);
  if(!commodity) {
    PWARN("no commodity");
    return FALSE;
  }
  currency = gnc_price_get_currency(p);
  if(!currency) {
    PWARN("no currency");
    return FALSE;
  }
  if(!db->commodity_hash) return FALSE;

  currency_hash = g_hash_table_lookup(db->commodity_hash, commodity);
  if(!currency_hash) {
    currency_hash = g_hash_table_new(commodity_hash, commodity_equal);
    g_hash_table_insert(db->commodity_hash, commodity, currency_hash);
  }

  price_list = g_hash_table_lookup(currency_hash, currency);
  if(!gnc_price_list_insert(&price_list, p)) return FALSE;
  if(!price_list) return FALSE;
  g_hash_table_insert(currency_hash, currency, price_list);
  p->db = db;

  LEAVE ("db=%p, pr=%p not-saved=%d do-free=%d",
         db, p, p->not_saved, p->do_free);
  return TRUE;
}

/* the gnc_pricedb_add_price() function will use p, adding a ref, so
   treat p as read-only if this function succeeds. (Huh ???) */
gboolean
gnc_pricedb_add_price(GNCPriceDB *db, GNCPrice *p)
{
  if(!db || !p) return FALSE;

  ENTER ("db=%p, pr=%p not-saved=%d do-free=%d",
         db, p, p->not_saved, p->do_free);

  if (FALSE == add_price(db, p)) return FALSE;

  /* if we haven't been able to call the backend before, call it now */
  if (TRUE == p->not_saved) {
    gnc_price_begin_edit(p);
    db->dirty = TRUE;
    gnc_price_commit_edit(p);
  }

  LEAVE ("db=%p, pr=%p not-saved=%d do-free=%d",
         db, p, p->not_saved, p->do_free);

  return TRUE;
}

/* remove_price() is a utility; its only function is to remove the price
 * from the double-hash tables.
 */

static gboolean
remove_price(GNCPriceDB *db, GNCPrice *p, gboolean cleanup)
{
  GList *price_list;
  gnc_commodity *commodity;
  gnc_commodity *currency;
  GHashTable *currency_hash;

  if(!db || !p) return FALSE;
  ENTER ("db=%p, pr=%p not-saved=%d do-free=%d",
         db, p, p->not_saved, p->do_free);

  commodity = gnc_price_get_commodity(p);
  if(!commodity) return FALSE;
  currency = gnc_price_get_currency(p);
  if(!currency) return FALSE;
  if(!db->commodity_hash) return FALSE;

  currency_hash = g_hash_table_lookup(db->commodity_hash, commodity);
  if(!currency_hash) return FALSE;

  price_list = g_hash_table_lookup(currency_hash, currency);
  gnc_price_ref(p);
  if(!gnc_price_list_remove(&price_list, p)) {
    gnc_price_unref(p);
    return FALSE;
  }

  /* if the price list is empty, then remove this currency from the
     commodity hash */
  if(price_list) {
    g_hash_table_insert(currency_hash, currency, price_list);
  } else {
    g_hash_table_remove(currency_hash, currency);

    if (cleanup) {
      /* chances are good that this commodity had only one currency.
       * If there are no currencies, we may as well destroy the
       * commodity too. */
      guint num_currencies = g_hash_table_size (currency_hash);
      if (0 == num_currencies) {
        g_hash_table_remove (db->commodity_hash, commodity);
        g_hash_table_destroy (currency_hash);
      }
    }
  }

  gnc_price_unref(p);
  LEAVE ("db=%p, pr=%p", db, p);
  return TRUE;
}

gboolean
gnc_pricedb_remove_price(GNCPriceDB *db, GNCPrice *p)
{
  gboolean rc;
  if(!db || !p) return FALSE;
  ENTER ("db=%p, pr=%p not-saved=%d do-free=%d",
         db, p, p->not_saved, p->do_free);

  gnc_price_ref(p);
  rc = remove_price (db, p, TRUE);

  /* invoke the backend to delete this price */
  gnc_price_begin_edit (p);
  db->dirty = TRUE;
  p->do_free = TRUE;
  gnc_price_commit_edit (p);

  p->db = NULL;
  gnc_price_unref(p);
  LEAVE ("db=%p, pr=%p", db, p);
  return rc;
}

/* ==================================================================== */
/* lookup/query functions */

GNCPrice *
gnc_pricedb_lookup_latest(GNCPriceDB *db,
                          gnc_commodity *commodity,
                          gnc_commodity *currency)
{
  GList *price_list;
  GNCPrice *result;
  GHashTable *currency_hash;

  ENTER ("db=%p commodity=%p currency=%p", db, commodity, currency);
  if(!db || !commodity || !currency) return NULL;

  if (db->book && db->book->backend && db->book->backend->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_LATEST;
     pl.prdb = db;
     pl.commodity = commodity;
     pl.currency = currency;
     (db->book->backend->price_lookup) (db->book->backend, &pl);
  }

  currency_hash = g_hash_table_lookup(db->commodity_hash, commodity);
  if(!currency_hash) return NULL;

  price_list = g_hash_table_lookup(currency_hash, currency);
  if(!price_list) return NULL;

  /* This works magically because prices are inserted in date-sorted
   * order, and the latest date always comes first. So return the
   * first in the list.  */
  result = price_list->data;
  gnc_price_ref(result);
  LEAVE(" ");
  return result;
}


static void
lookup_latest(gpointer key, gpointer val, gpointer user_data)
{
  //gnc_commodity *currency = (gnc_commodity *)key;
  GList *price_list = (GList *)val;
  GList **return_list = (GList **)user_data;

  if(!price_list) return;

  /* the latest price is the first in list */
  gnc_price_list_insert(return_list, price_list->data);
}

GList *
gnc_pricedb_lookup_latest_any_currency(GNCPriceDB *db,
                                       gnc_commodity *commodity)
{
  GList *result;
  GHashTable *currency_hash;

  result = NULL;

  ENTER ("db=%p commodity=%p", db, commodity);
  if(!db || !commodity) return NULL;

  if (db->book && db->book->backend && db->book->backend->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_LATEST;
     pl.prdb = db;
     pl.commodity = commodity;
     pl.currency = NULL;  /* can the backend handle this??? */
     (db->book->backend->price_lookup) (db->book->backend, &pl);
  }

  currency_hash = g_hash_table_lookup(db->commodity_hash, commodity);
  if(!currency_hash) return NULL;

  g_hash_table_foreach(currency_hash, lookup_latest, &result);

  if(!result) return NULL;

  result = g_list_sort(result, compare_prices_by_date);

  LEAVE(" ");
  return result;
}


GList *
gnc_pricedb_get_prices(GNCPriceDB *db,
                       gnc_commodity *commodity,
                       gnc_commodity *currency)
{
  GList *price_list;
  GList *result;
  GList *node;
  GHashTable *currency_hash;

  ENTER ("db=%p commodity=%p currency=%p", db, commodity, currency);
  if(!db || !commodity || !currency) return NULL;

  if (db->book && db->book->backend && db->book->backend->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_ALL;
     pl.prdb = db;
     pl.commodity = commodity;
     pl.currency = currency;
     (db->book->backend->price_lookup) (db->book->backend, &pl);
  }

  currency_hash = g_hash_table_lookup(db->commodity_hash, commodity);
  if(!currency_hash) return NULL;

  price_list = g_hash_table_lookup(currency_hash, currency);
  if(!price_list) return NULL;

  result = g_list_copy (price_list);
  for (node = result; node; node = node->next)
    gnc_price_ref (node->data);

  LEAVE (" ");
  return result;
}


GList *
gnc_pricedb_lookup_day(GNCPriceDB *db,
		       gnc_commodity *c,
		       gnc_commodity *currency,
		       Timespec t)
{
  GList *price_list;
  GList *result = NULL;
  GList *item = NULL;
  GHashTable *currency_hash;

  ENTER ("db=%p commodity=%p currency=%p", db, c, currency);
  if(!db || !c || !currency) return NULL;

  /* Convert to noon local time. */
  t = timespecCanonicalDayTime(t);

  if (db->book && db->book->backend && db->book->backend->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_AT_TIME;
     pl.prdb = db;
     pl.commodity = c;
     pl.currency = currency;
     pl.date = t;
     (db->book->backend->price_lookup) (db->book->backend, &pl);
  }

  currency_hash = g_hash_table_lookup(db->commodity_hash, c);
  if(!currency_hash) return NULL;

  price_list = g_hash_table_lookup(currency_hash, currency);
  if(!price_list) return NULL;

  item = price_list;
  while(item) {
    GNCPrice *p = item->data;
    Timespec price_time = timespecCanonicalDayTime(gnc_price_get_time(p));
    if(timespec_equal(&price_time, &t)) {
      result = g_list_prepend(result, p);
      gnc_price_ref(p);
    }
    item = item->next;
  }
  LEAVE (" ");
  return result;
}


static void
lookup_day(gpointer key, gpointer val, gpointer user_data)
{
  //gnc_commodity *currency = (gnc_commodity *)key;
  GList *price_list = (GList *)val;
  GList *item = NULL;
  GNCPriceLookupHelper *lookup_helper = (GNCPriceLookupHelper *)user_data;
  GList **return_list = lookup_helper->return_list;
  Timespec t = lookup_helper->time;

  item = price_list;
  while(item) {
    GNCPrice *p = item->data;
    Timespec price_time = timespecCanonicalDayTime(gnc_price_get_time(p));
    if(timespec_equal(&price_time, &t)) {
      gnc_price_list_insert(return_list, item->data);
    }
    item = item->next;
  }
}

GList *
gnc_pricedb_lookup_day_any_currency(GNCPriceDB *db,
 		                    gnc_commodity *c,
		                    Timespec t)
{
  GList *result = NULL;
  GHashTable *currency_hash;
  GNCPriceLookupHelper lookup_helper;

  ENTER ("db=%p commodity=%p", db, c);
  if(!db || !c) return NULL;

  /* Convert to noon local time. */
  t = timespecCanonicalDayTime(t);

  if (db->book && db->book->backend && db->book->backend->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_AT_TIME;
     pl.prdb = db;
     pl.commodity = c;
     pl.currency = NULL;  /* can the backend handle this??? */
     pl.date = t;
     (db->book->backend->price_lookup) (db->book->backend, &pl);
  }

  currency_hash = g_hash_table_lookup(db->commodity_hash, c);
  if(!currency_hash) return NULL;

  lookup_helper.return_list = &result;
  lookup_helper.time = t;
  g_hash_table_foreach(currency_hash, lookup_day, &lookup_helper);

  if(!result) return NULL;

  result = g_list_sort(result, compare_prices_by_date);

  LEAVE (" ");
  return result;
}


GList *
gnc_pricedb_lookup_at_time(GNCPriceDB *db,
                           gnc_commodity *c,
                           gnc_commodity *currency,
                           Timespec t)
{
  GList *price_list;
  GList *result = NULL;
  GList *item = NULL;
  GHashTable *currency_hash;

  ENTER ("db=%p commodity=%p currency=%p", db, c, currency);
  if(!db || !c || !currency) return NULL;

  if (db->book && db->book->backend && db->book->backend->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_AT_TIME;
     pl.prdb = db;
     pl.commodity = c;
     pl.currency = currency;
     pl.date = t;
     (db->book->backend->price_lookup) (db->book->backend, &pl);
  }

  currency_hash = g_hash_table_lookup(db->commodity_hash, c);
  if(!currency_hash) return NULL;

  price_list = g_hash_table_lookup(currency_hash, currency);
  if(!price_list) return NULL;

  item = price_list;
  while(item) {
    GNCPrice *p = item->data;
    Timespec price_time = gnc_price_get_time(p);
    if(timespec_equal(&price_time, &t)) {
      result = g_list_prepend(result, p);
      gnc_price_ref(p);
    }
    item = item->next;
  }
  LEAVE (" ");
  return result;
}

static void
lookup_time(gpointer key, gpointer val, gpointer user_data)
{
  //gnc_commodity *currency = (gnc_commodity *)key;
  GList *price_list = (GList *)val;
  GList *item = NULL;
  GNCPriceLookupHelper *lookup_helper = (GNCPriceLookupHelper *)user_data;
  GList **return_list = lookup_helper->return_list;
  Timespec t = lookup_helper->time;

  item = price_list;
  while(item) {
    GNCPrice *p = item->data;
    Timespec price_time = gnc_price_get_time(p);
    if(timespec_equal(&price_time, &t)) {
      gnc_price_list_insert(return_list, item->data);
    }
    item = item->next;
  }
}

GList *
gnc_pricedb_lookup_at_time_any_currency(GNCPriceDB *db,
 		                        gnc_commodity *c,
		                        Timespec t)
{
  GList *result = NULL;
  GHashTable *currency_hash;
  GNCPriceLookupHelper lookup_helper;

  ENTER ("db=%p commodity=%p", db, c);
  if(!db || !c) return NULL;

  if (db->book && db->book->backend && db->book->backend->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_AT_TIME;
     pl.prdb = db;
     pl.commodity = c;
     pl.currency = NULL;  /* can the backend handle this??? */
     pl.date = t;
     (db->book->backend->price_lookup) (db->book->backend, &pl);
  }

  currency_hash = g_hash_table_lookup(db->commodity_hash, c);
  if(!currency_hash) return NULL;

  lookup_helper.return_list = &result;
  lookup_helper.time = t;
  g_hash_table_foreach(currency_hash, lookup_time, &lookup_helper);

  if(!result) return NULL;

  result = g_list_sort(result, compare_prices_by_date);

  LEAVE (" ");
  return result;
}


GNCPrice *
gnc_pricedb_lookup_nearest_in_time(GNCPriceDB *db,
                                   gnc_commodity *c,
                                   gnc_commodity *currency,
                                   Timespec t)
{
  GList *price_list;
  GNCPrice *current_price = NULL;
  GNCPrice *next_price = NULL;
  GNCPrice *result = NULL;
  GList *item = NULL;
  GHashTable *currency_hash;

  ENTER ("db=%p commodity=%p currency=%p", db, c, currency);
  if(!db || !c || !currency) return NULL;

  if (db->book && db->book->backend && db->book->backend->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_NEAREST_IN_TIME;
     pl.prdb = db;
     pl.commodity = c;
     pl.currency = currency;
     pl.date = t;
     (db->book->backend->price_lookup) (db->book->backend, &pl);
  }

  currency_hash = g_hash_table_lookup(db->commodity_hash, c);
  if(!currency_hash) return NULL;

  price_list = g_hash_table_lookup(currency_hash, currency);
  if(!price_list) return NULL;

  item = price_list;

  /* default answer */
  current_price = item->data;

  /* find the first candidate past the one we want.  Remember that
     prices are in most-recent-first order. */
  while (!next_price && item) {
    GNCPrice *p = item->data;
    Timespec price_time = gnc_price_get_time(p);
    if (timespec_cmp(&price_time, &t) <= 0) {
      next_price = item->data;
      break;
    }
    current_price = item->data;
    item = item->next;
  }

  if (current_price) {
    if (!next_price) {
      result = current_price;
    } else {
      Timespec current_t = gnc_price_get_time(current_price);
      Timespec next_t = gnc_price_get_time(next_price);
      Timespec diff_current = timespec_diff(&current_t, &t);
      Timespec diff_next = timespec_diff(&next_t, &t);
      Timespec abs_current = timespec_abs(&diff_current);
      Timespec abs_next = timespec_abs(&diff_next);
      
      if (timespec_cmp(&abs_current, &abs_next) <= 0) {
	result = current_price;
      } else {
	result = next_price;
      }
    }
  }

  gnc_price_ref(result);
  LEAVE (" ");
  return result;
}

static void
lookup_nearest(gpointer key, gpointer val, gpointer user_data)
{
  //gnc_commodity *currency = (gnc_commodity *)key;
  GList *price_list = (GList *)val;
  GNCPrice *current_price = NULL;
  GNCPrice *next_price = NULL;
  GNCPrice *result = NULL;
  GList *item = NULL;
  GNCPriceLookupHelper *lookup_helper = (GNCPriceLookupHelper *)user_data;
  GList **return_list = lookup_helper->return_list;
  Timespec t = lookup_helper->time;

  item = price_list;

  /* default answer */
  current_price = item->data;

  /* find the first candidate past the one we want.  Remember that
     prices are in most-recent-first order. */
  while (!next_price && item) {
    GNCPrice *p = item->data;
    Timespec price_time = gnc_price_get_time(p);
    if (timespec_cmp(&price_time, &t) <= 0) {
      next_price = item->data;
      break;
    }
    current_price = item->data;
    item = item->next;
  }

  if (current_price) {
    if (!next_price) {
      result = current_price;
    } else {
      Timespec current_t = gnc_price_get_time(current_price);
      Timespec next_t = gnc_price_get_time(next_price);
      Timespec diff_current = timespec_diff(&current_t, &t);
      Timespec diff_next = timespec_diff(&next_t, &t);
      Timespec abs_current = timespec_abs(&diff_current);
      Timespec abs_next = timespec_abs(&diff_next);
      
      if (timespec_cmp(&abs_current, &abs_next) <= 0) {
	result = current_price;
      } else {
	result = next_price;
      }
    }
  }

  gnc_price_list_insert(return_list, result);
}

GList *
gnc_pricedb_lookup_nearest_in_time_any_currency(GNCPriceDB *db,
 		                                gnc_commodity *c,
		                                Timespec t)
{
  GList *result = NULL;
  GHashTable *currency_hash;
  GNCPriceLookupHelper lookup_helper;

  ENTER ("db=%p commodity=%p", db, c);
  if(!db || !c) return NULL;

  if (db->book && db->book->backend && db->book->backend->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_NEAREST_IN_TIME;
     pl.prdb = db;
     pl.commodity = c;
     pl.currency = NULL;  /* can the backend handle this??? */
     pl.date = t;
     (db->book->backend->price_lookup) (db->book->backend, &pl);
  }

  currency_hash = g_hash_table_lookup(db->commodity_hash, c);
  if(!currency_hash) return NULL;

  lookup_helper.return_list = &result;
  lookup_helper.time = t;
  g_hash_table_foreach(currency_hash, lookup_nearest, &lookup_helper);

  if(!result) return NULL;

  result = g_list_sort(result, compare_prices_by_date);

  LEAVE (" ");
  return result;
}


/*
 * Convert a balance from one currency to another.
 */
gnc_numeric
gnc_pricedb_convert_balance_latest_price(GNCPriceDB *pdb,
				         gnc_numeric balance,
				         gnc_commodity *balance_currency,
				         gnc_commodity *new_currency)
{
  GNCPrice *price, *currency_price;
  GList *price_list, *list_helper;
  gnc_numeric currency_price_value;
  gnc_commodity *intermediate_currency;

  if (gnc_numeric_zero_p (balance) ||
      gnc_commodity_equiv (balance_currency, new_currency))
    return balance;

  /* Look for a direct price. */
  price = gnc_pricedb_lookup_latest (pdb, balance_currency, new_currency);
  if (price) {
    balance = gnc_numeric_mul (balance, gnc_price_get_value (price),
			       gnc_commodity_get_fraction (new_currency),
			       GNC_RND_ROUND);
    gnc_price_unref (price);
    return balance;
  }

  /*
   * no direct price found, try if we find a price in another currency
   * and convert in two stages
   */
  price_list = gnc_pricedb_lookup_latest_any_currency(pdb, balance_currency);
  if (!price_list) {
    balance =  gnc_numeric_zero ();
    return balance;
  }

  list_helper = price_list;
  currency_price_value = gnc_numeric_zero();

  do {
    price = (GNCPrice *)(list_helper->data);

    intermediate_currency = gnc_price_get_currency(price);
    currency_price = gnc_pricedb_lookup_latest(pdb, intermediate_currency,
					       new_currency);
    if(currency_price) {
      currency_price_value = gnc_price_get_value(currency_price);
      gnc_price_unref(currency_price);
    } else {
      currency_price = gnc_pricedb_lookup_latest(pdb, new_currency,
						 intermediate_currency);
      if (currency_price) {
	/* here we need the reciprocal */
	currency_price_value = gnc_numeric_div(gnc_numeric_create(1, 1),
					       gnc_price_get_value(currency_price),
					       GNC_DENOM_AUTO,
					       GNC_DENOM_EXACT | GNC_RND_NEVER);
	gnc_price_unref(currency_price);
      }
    }

    list_helper = list_helper->next;
  } while((list_helper != NULL) &&
	  (!gnc_numeric_zero_p(currency_price_value)));

  balance = gnc_numeric_mul (balance, currency_price_value,
			     gnc_commodity_get_fraction (new_currency),
			     GNC_RND_ROUND);      
  balance = gnc_numeric_mul (balance, gnc_price_get_value (price),
			     gnc_commodity_get_fraction (new_currency),
			     GNC_RND_ROUND);      

  gnc_price_list_destroy(price_list);
  return balance;
}

gnc_numeric
gnc_pricedb_convert_balance_nearest_price(GNCPriceDB *pdb,
				          gnc_numeric balance,
				          gnc_commodity *balance_currency,
				          gnc_commodity *new_currency,
					  Timespec t)
{
  GNCPrice *price, *currency_price;
  GList *price_list, *list_helper;
  gnc_numeric currency_price_value;
  gnc_commodity *intermediate_currency;

  if (gnc_numeric_zero_p (balance) ||
      gnc_commodity_equiv (balance_currency, new_currency))
    return balance;

  /* Look for a direct price. */
  price = gnc_pricedb_lookup_nearest_in_time (pdb, balance_currency, new_currency, t);
  if (price) {
    balance = gnc_numeric_mul (balance, gnc_price_get_value (price),
			       gnc_commodity_get_fraction (new_currency),
			       GNC_RND_ROUND);
    gnc_price_unref (price);
    return balance;
  }

  /*
   * no direct price found, try if we find a price in another currency
   * and convert in two stages
   */
  price_list = gnc_pricedb_lookup_nearest_in_time_any_currency(pdb, balance_currency, t);
  if (!price_list) {
    balance =  gnc_numeric_zero ();
    return balance;
  }

  list_helper = price_list;
  currency_price_value = gnc_numeric_zero();

  do {
    price = (GNCPrice *)(list_helper->data);

    intermediate_currency = gnc_price_get_currency(price);
    currency_price = gnc_pricedb_lookup_nearest_in_time(pdb, intermediate_currency,
					                new_currency, t);
    if(currency_price) {
      currency_price_value = gnc_price_get_value(currency_price);
      gnc_price_unref(currency_price);
    } else {
      currency_price = gnc_pricedb_lookup_nearest_in_time(pdb, new_currency,
						          intermediate_currency, t);
      if (currency_price) {
	/* here we need the reciprocal */
	currency_price_value = gnc_numeric_div(gnc_numeric_create(1, 1),
					       gnc_price_get_value(currency_price),
					       gnc_commodity_get_fraction (new_currency),
					       GNC_RND_ROUND);
	gnc_price_unref(currency_price);
      }
    }

    list_helper = list_helper->next;
  } while((list_helper != NULL) &&
	  (!gnc_numeric_zero_p(currency_price_value)));

  balance = gnc_numeric_mul (balance, currency_price_value,
			     gnc_commodity_get_fraction (new_currency),
			     GNC_RND_ROUND);      
  balance = gnc_numeric_mul (balance, gnc_price_get_value (price),
			     gnc_commodity_get_fraction (new_currency),
			     GNC_RND_ROUND);      

  gnc_price_list_destroy(price_list);
  return balance;
}


/* ==================================================================== */
/* gnc_pricedb_foreach_price infrastructure
 */

typedef struct {
  gboolean ok;
  gboolean (*func)(GNCPrice *p, gpointer user_data);
  gpointer user_data;
} GNCPriceDBForeachData;

static void
pricedb_foreach_pricelist(gpointer key, gpointer val, gpointer user_data)
{
  GList *price_list = (GList *) val;
  GList *node = price_list;
  GNCPriceDBForeachData *foreach_data = (GNCPriceDBForeachData *) user_data;

  /* stop traversal when func returns FALSE */
  while(foreach_data->ok && node) {
    GNCPrice *p = (GNCPrice *) node->data;
    foreach_data->ok = foreach_data->func(p, foreach_data->user_data);
    node = node->next;
  }
}

static void
pricedb_foreach_currencies_hash(gpointer key, gpointer val, gpointer user_data)
{
  GHashTable *currencies_hash = (GHashTable *) val;
  g_hash_table_foreach(currencies_hash, pricedb_foreach_pricelist, user_data);
}

static gboolean
unstable_price_traversal(GNCPriceDB *db,
                       gboolean (*f)(GNCPrice *p, gpointer user_data),
                       gpointer user_data)
{
  GNCPriceDBForeachData foreach_data;
  
  if(!db || !f) return FALSE;
  foreach_data.ok = TRUE;
  foreach_data.func = f;
  foreach_data.user_data = user_data;

  g_hash_table_foreach(db->commodity_hash,
                       pricedb_foreach_currencies_hash,
                       &foreach_data);

  return foreach_data.ok;
}

static gint
compare_kvpairs_by_commodity_key(gconstpointer a, gconstpointer b)
{
  GHashTableKVPair *kvpa = (GHashTableKVPair *) a;
  GHashTableKVPair *kvpb = (GHashTableKVPair *) b;
  gnc_commodity *ca;
  gnc_commodity *cb;
  int cmp_result;

  if(a == b) return 0;
  if(!a && !b) return 0;
  if(!a) return -1;
  if(!b) return 1;
  
  ca = (gnc_commodity *) kvpa->key;
  cb = (gnc_commodity *) kvpb->key;

  cmp_result = safe_strcmp(gnc_commodity_get_namespace(ca),
                           gnc_commodity_get_namespace(cb));
  
  if(cmp_result != 0) return cmp_result;

  return safe_strcmp(gnc_commodity_get_mnemonic(ca),
                     gnc_commodity_get_mnemonic(cb));
}

static gboolean
stable_price_traversal(GNCPriceDB *db,
                       gboolean (*f)(GNCPrice *p, gpointer user_data),
                       gpointer user_data)
{
  GSList *currency_hashes = NULL;
  gboolean ok = TRUE;
  GSList *i = NULL;
  
  if(!db || !f) return FALSE;

  currency_hashes = g_hash_table_key_value_pairs(db->commodity_hash);
  currency_hashes = g_slist_sort(currency_hashes,
				 compare_kvpairs_by_commodity_key);

  for(i = currency_hashes; i; i = i->next) {
    GHashTableKVPair *kv_pair = (GHashTableKVPair *) i->data;
    GHashTable *currency_hash = (GHashTable *) kv_pair->value;
    GSList *price_lists = g_hash_table_key_value_pairs(currency_hash);
    GSList *j;

    price_lists = g_slist_sort(price_lists, compare_kvpairs_by_commodity_key);
    for(j = price_lists; j; j = j->next) {
      GHashTableKVPair *pricelist_kvp = (GHashTableKVPair *) j->data;
      GList *price_list = (GList *) pricelist_kvp->value;
      GList *node;

      for(node = (GList *) price_list; node; node = node->next) {
        GNCPrice *price = (GNCPrice *) node->data;

        /* stop traversal when f returns FALSE */
        if (FALSE == ok) break;
        if(!f(price, user_data)) ok = FALSE;
      }
    }
    if(price_lists) {
      g_slist_foreach(price_lists, g_hash_table_kv_pair_free_gfunc, NULL);
      g_slist_free(price_lists);
      price_lists = NULL;
    }
  }

  if(currency_hashes) {
    g_slist_foreach(currency_hashes, g_hash_table_kv_pair_free_gfunc, NULL);
    g_slist_free(currency_hashes);
  }
  return ok;
}

gboolean
gnc_pricedb_foreach_price(GNCPriceDB *db,
                          gboolean (*f)(GNCPrice *p, gpointer user_data),
                          gpointer user_data,
                          gboolean stable_order)
{
  ENTER ("db=%p f=%p", db, f);
  if(stable_order) return stable_price_traversal(db, f, user_data);
  return unstable_price_traversal(db, f, user_data);
  LEAVE (" ");
}

/* ==================================================================== */
/* commodity substitution */

typedef struct {
  gnc_commodity *old_c;
  gnc_commodity *new_c;
} GNCPriceFixupData;

static gboolean
add_price_to_list (GNCPrice *p, gpointer data)
{
  GList **list = data;

  *list = g_list_prepend (*list, p);

  return TRUE;
}

static void
gnc_price_fixup_legacy_commods(gpointer data, gpointer user_data)
{
  GNCPrice *p = data;
  GNCPriceFixupData *fixup_data = user_data;
  gnc_commodity *price_c;

  if (!p) return;

  price_c = gnc_price_get_commodity(p);
  if (gnc_commodity_equiv(price_c, fixup_data->old_c)) {
    gnc_price_set_commodity (p, fixup_data->new_c);
  }
  price_c = gnc_price_get_currency(p);
  if (gnc_commodity_equiv(price_c, fixup_data->old_c)) {
    gnc_price_set_currency (p, fixup_data->new_c);
  }
}

void
gnc_pricedb_substitute_commodity(GNCPriceDB *db,
                                 gnc_commodity *old_c,
                                 gnc_commodity *new_c)
{
  GNCPriceFixupData data;
  GList *prices = NULL;

  if(!db || !old_c || !new_c) return;

  data.old_c = old_c;
  data.new_c = new_c;

  gnc_pricedb_foreach_price (db, add_price_to_list, &prices, FALSE);

  g_list_foreach (prices, gnc_price_fixup_legacy_commods, &data);

  g_list_free (prices);
}

/***************************************************************************/

/* Semi-lame debugging code */

void
gnc_price_print(GNCPrice *p, FILE *f, int indent)
{
  gnc_commodity *commodity;
  gnc_commodity *currency;
  gchar *istr = NULL;           /* indent string */
  const char *str;

  if(!p) return;
  if(!f) return;

  commodity = gnc_price_get_commodity(p);
  currency = gnc_price_get_currency(p);

  if(!commodity) return;
  if(!currency) return;

  istr = g_strnfill(indent, ' ');

  fprintf(f, "%s<pdb:price>\n", istr);
  fprintf(f, "%s  <pdb:commodity pointer=%p>\n", istr, commodity);
  str = gnc_commodity_get_namespace(commodity);
  str = str ? str : "(null)";
  fprintf(f, "%s    <cmdty:ref-space>%s</gnc:cmdty:ref-space>\n", istr, str);
  str = gnc_commodity_get_mnemonic(commodity);
  str = str ? str : "(null)";
  fprintf(f, "%s    <cmdty:ref-id>%s</cmdty:ref-id>\n", istr, str);
  fprintf(f, "%s  </pdb:commodity>\n", istr);
  fprintf(f, "%s  <pdb:currency pointer=%p>\n", istr, currency);
  str = gnc_commodity_get_namespace(currency);
  str = str ? str : "(null)";
  fprintf(f, "%s    <cmdty:ref-space>%s</gnc:cmdty:ref-space>\n", istr, str);
  str = gnc_commodity_get_mnemonic(currency);
  str = str ? str : "(null)";
  fprintf(f, "%s    <cmdty:ref-id>%s</cmdty:ref-id>\n", istr, str);
  fprintf(f, "%s  </pdb:currency>\n", istr);
  str = gnc_price_get_source(p);
  str = str ? str : "(null)";
  fprintf(f, "%s  %s\n", istr, str);
  str = gnc_price_get_type(p);
  str = str ? str : "(null)";
  fprintf(f, "%s  %s\n", istr, str);
  fprintf(f, "%s  %g\n", istr, gnc_numeric_to_double(gnc_price_get_value(p)));
  fprintf(f, "%s</pdb:price>\n", istr);

  g_free(istr);
}

static gboolean
print_pricedb_adapter(GNCPrice *p, gpointer user_data)
{
  FILE *f = (FILE *) user_data;
  gnc_price_print(p, f, 1);
  return TRUE;
}

void
gnc_pricedb_print_contents(GNCPriceDB *db, FILE *f)
{
  if(!db) { PERR("NULL PriceDB\n"); return; }
  if(!f) { PERR("NULL FILE*\n"); return; }

  fprintf(f, "<gnc:pricedb>\n");
  gnc_pricedb_foreach_price(db, print_pricedb_adapter, f, FALSE);
  fprintf(f, "</gnc:pricedb>\n");
}

/* ==================================================================== */

QofBackend *
xaccPriceDBGetBackend (GNCPriceDB *prdb)
{
  if (!prdb || !prdb->book) return NULL;
  return prdb->book->backend;
}

/* ==================================================================== */
/* gncObject function implementation and registration */

static void 
pricedb_book_begin (QofBook *book)
{
  gnc_pricedb_set_db (book, gnc_pricedb_create(book));
}

static void 
pricedb_book_end (QofBook *book)
{
  /* unhook the prices */
  gnc_pricedb_set_db (book, NULL);
}

static gboolean
pricedb_is_dirty (QofBook *book)
{
  return gnc_pricedb_dirty(gnc_pricedb_get_db(book));
}

static void
pricedb_mark_clean(QofBook *book)
{
  gnc_pricedb_mark_clean(gnc_pricedb_get_db(book));
}

static QofObject pricedb_object_def = 
{
  interface_version: QOF_OBJECT_VERSION,
  name:              GNC_ID_PRICEDB,
  type_label:        "PriceDB",
  book_begin:        pricedb_book_begin,
  book_end:          pricedb_book_end,
  is_dirty:          pricedb_is_dirty,
  mark_clean:        pricedb_mark_clean,
  foreach:           NULL,
  printable:         NULL,
};

gboolean 
gnc_pricedb_register (void)
{
  return qof_object_register (&pricedb_object_def);
}

/* ========================= END OF FILE ============================== */
