/********************************************************************
 * gnc-pricedb.c -- a simple price database for gnucash.            *
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

#include "config.h"

#include <glib.h>
#include <string.h>

#include "gnc-pricedb.h"
#include "gnc-pricedb-p.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_ENGINE;

struct _GNCPriceDB {
  GHashTable *commodity_hash;
  gboolean dirty;
};

struct _GNCPrice {
  guint32 refcount;
  GNCPriceDB *db;
  gnc_commodity *commodity;
  gnc_commodity *currency;
  Timespec time;
  char *source;
  char *type;
  gnc_numeric value;
};

/****************************************************************************/
/* GNCPrice functions
 */

/* allocation */
GNCPrice *
gnc_price_create()
{
  GNCPrice *p = g_new0(GNCPrice, 1);
  p->refcount = 1;
  return p;
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
  if(p->refcount == 0) {
    PERR("gnc_price_unref: refcount == 0!");
    assert(p->refcount != 0);
  }
  p->refcount--;
  if(p->refcount == 0) {
    if(p->type) g_cache_remove(gnc_engine_get_string_cache(), p->type);
    if(p->source) g_cache_remove(gnc_engine_get_string_cache(), p->source);
    memset(p, 0, sizeof(GNCPrice));
    g_free(p);
  }
}

GNCPrice *
gnc_price_clone(GNCPrice* p)
{
  /* the clone doesn't belong to a PriceDB */
  GNCPrice *new_p;
  
  if(!p) return NULL;
  new_p = gnc_price_create();
  if(!new_p) return NULL;
  gnc_price_set_commodity(new_p, gnc_price_get_commodity(p));
  gnc_price_set_time(new_p, gnc_price_get_time(p));
  gnc_price_set_source(new_p, gnc_price_get_source(p));
  gnc_price_set_type(new_p, gnc_price_get_type(p));
  gnc_price_set_value(new_p, gnc_price_get_value(p));
  gnc_price_set_currency(new_p, gnc_price_get_currency(p));
  return(new_p);
}

/* setters */
void
gnc_price_set_commodity(GNCPrice *p, gnc_commodity *c)
{
  if(!p) return;
  if(!gnc_commodity_equiv(p->commodity, c)) {
    p->commodity = c;
    if(p->db) p->db->dirty = TRUE;
  }
}

void
gnc_price_set_currency(GNCPrice *p, gnc_commodity *c)
{
  if(!p) return;
  if(!gnc_commodity_equiv(p->currency, c)) {
    p->currency = c;
    if(p->db) p->db->dirty = TRUE;
  }
}

void
gnc_price_set_time(GNCPrice *p, Timespec t)
{
  if(!p) return;
  if(!timespec_equal(&(p->time), &t)) {
    p->time = t;
    if(p->db) p->db->dirty = TRUE;
  }
}

void
gnc_price_set_source(GNCPrice *p, const char *s)
{
  if(!p) return;
  if(safe_strcmp(p->source, s) != 0) {
    GCache *cache = gnc_engine_get_string_cache();
    char *tmp = g_cache_insert(cache, (gpointer) s);
    if(p->source) g_cache_remove(cache, p->source);
    p->source = tmp;
    if(p->db) p->db->dirty = TRUE;
  }
}

void
gnc_price_set_type(GNCPrice *p, const char* type)
{
  if(!p) return;
  if(safe_strcmp(p->type, type) != 0) {
    GCache *cache = gnc_engine_get_string_cache();
    gchar *tmp = g_cache_insert(cache, (gpointer) type);
    if(p->type) g_cache_remove(cache, p->type);
    p->type = tmp;
    if(p->db) p->db->dirty = TRUE;
  }
}

void
gnc_price_set_value(GNCPrice *p, gnc_numeric value)
{
  if(!p) return;
  if(!gnc_numeric_eq(p->value, value)) {
    p->value = value;
    if(p->db) p->db->dirty = TRUE;
  }
}


/***********/
/* getters */
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
  return p->time;
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
    PERR("gnc_price_get_value: price NULL.\n");
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

/* setters */

static gint
compare_prices_by_date(gconstpointer a, gconstpointer b)
{
  Timespec time_a;
  Timespec time_b;
  if(!a && !b) return 0;
  /* nothing is always less than something */
  if(!a) return -1;

  time_a = gnc_price_get_time((GNCPrice *) a);
  time_b = gnc_price_get_time((GNCPrice *) b);

  return -timespec_cmp(&time_a, &time_b);
}

gboolean
gnc_price_list_insert(GList **prices, GNCPrice *p)
{
  GList *result_list;

  if(!prices) return FALSE;
  if(!p) return FALSE;
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

  if(!prices) return FALSE;
  if(!p) return FALSE;
  
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

/****************************************************************************/
/* GNCPriceDB functions

   A pricedb is a hash mapping price commodities (of type
   gnc_commodity*) to hashes mapping price currencies (of type
   gnc_commodity*) to price lists.  The top-level key is the commodity
   you want the prices for, and the second level key is the commodity
   that the value is expressed in terms of.

   See the header for other info.

 */

GNCPriceDB *
gnc_pricedb_create(void)
{
  GNCPriceDB * result = g_new0(GNCPriceDB, 1);
  result->commodity_hash = g_hash_table_new(g_direct_hash, g_direct_equal);
  g_return_val_if_fail (result->commodity_hash, NULL);
  return result;
}

static void
destroy_pricedb_currency_hash_data(gpointer key,
                                   gpointer data,
                                   gpointer user_data)
{
  GList *price_list = (GList *) data;
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
  currency_hash = NULL;
}

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

void
gnc_pricedb_destroy(GNCPriceDB *db)
{
  if(!db) return;
  g_hash_table_foreach (db->commodity_hash,
                        destroy_pricedb_commodity_hash_data,
                        NULL);
  g_hash_table_destroy (db->commodity_hash);
  db->commodity_hash = NULL;
  g_free(db);
}

gboolean
gnc_pricedb_add_price(GNCPriceDB *db, GNCPrice *p)
{
  /* this function will use p, adding a ref, so treat p as read-only
     if this function succeeds. */
  GList *price_list;
  gnc_commodity *commodity;
  gnc_commodity *currency;
  GHashTable *currency_hash;

  if(!db || !p) return FALSE;
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
    currency_hash = g_hash_table_new(g_direct_hash, g_direct_equal);
    g_hash_table_insert(db->commodity_hash, commodity, currency_hash);
  }

  price_list = g_hash_table_lookup(currency_hash, currency);
  if(!gnc_price_list_insert(&price_list, p)) return FALSE;
  if(!price_list) return FALSE;
  g_hash_table_insert(currency_hash, currency, price_list);
  db->dirty = TRUE;
  return TRUE;
}

gboolean
gnc_pricedb_remove_price(GNCPriceDB *db, GNCPrice *p)
{
  GList *price_list;
  gnc_commodity *commodity;
  gnc_commodity *currency;
  GHashTable *currency_hash;

  if(!db || !p) return FALSE;
  commodity = gnc_price_get_commodity(p);
  if(!commodity) return FALSE;
  currency = gnc_price_get_currency(p);
  if(!currency) return FALSE;
  if(!db->commodity_hash) return FALSE;

  currency_hash = g_hash_table_lookup(db->commodity_hash, commodity);
  if(!currency_hash) return FALSE;

  price_list = g_hash_table_lookup(currency_hash, currency);
  if(!gnc_price_list_remove(&price_list, p)) return FALSE;
  if(price_list) {
    g_hash_table_insert(currency_hash, currency, price_list);
  } else {
    g_hash_table_remove(currency_hash, currency);
  }
  db->dirty = TRUE;
  return TRUE;
}                     

GNCPrice *
gnc_pricedb_lookup_latest(GNCPriceDB *db,
                          gnc_commodity *commodity,
                          gnc_commodity *currency)
{
  GList *price_list;
  GNCPrice *result;
  GHashTable *currency_hash;

  if(!db || !commodity || !currency) return NULL;

  currency_hash = g_hash_table_lookup(db->commodity_hash, commodity);
  if(!currency_hash) return NULL;

  price_list = g_hash_table_lookup(currency_hash, currency);
  if(!price_list) return NULL;

  result = price_list->data;
  gnc_price_ref(result);
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

  if(!db || !commodity || !currency) return NULL;

  currency_hash = g_hash_table_lookup(db->commodity_hash, commodity);
  if(!currency_hash) return NULL;

  price_list = g_hash_table_lookup(currency_hash, currency);
  if(!price_list) return NULL;

  result = g_list_copy (price_list);
  for (node = result; node; node = node->next)
    gnc_price_ref (node->data);

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

  if(!db || !c || !currency) return NULL;

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
  return result;
}

/***************************************************************************/
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

  while(foreach_data->ok && node) {
    GNCPrice *p = (GNCPrice *) node->data;
    foreach_data->func(p, foreach_data->user_data);
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
  GSList *foo = NULL;
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
  if(stable_order) return stable_price_traversal(db, f, user_data);
  return unstable_price_traversal(db, f, user_data);
}

/***************************************************************************/
/* commodity substitution
 */

typedef struct {
  gnc_commodity *old_c;
  gnc_commodity *new_c;
} GNCPriceFixupData;

static gboolean
gnc_price_fixup_legacy_commods(GNCPrice *p, gpointer data)
{
  GNCPriceFixupData *fixup_data = (GNCPriceFixupData *) data;
  gnc_commodity *price_c;

  if (!p) return FALSE;

  price_c = gnc_price_get_commodity(p);
  if (gnc_commodity_equiv(price_c, fixup_data->old_c)) {
    gnc_price_set_commodity(p, fixup_data->new_c);
  }
  price_c = gnc_price_get_currency(p);
  if (gnc_commodity_equiv(price_c, fixup_data->old_c)) {
    gnc_price_set_currency(p, fixup_data->new_c);
  }
  return TRUE;
}


static void
remap_currency_hash_keys(gpointer key, gpointer val, gpointer user_data)
{
  GHashTable *currencies_hash = (GHashTable *) val;
  GNCPriceFixupData *fixup_data = (GNCPriceFixupData *) user_data;
  GList *price_list;
  
  price_list = g_hash_table_lookup(currencies_hash, fixup_data->old_c);
  if(price_list) {
    g_hash_table_remove(currencies_hash, fixup_data->old_c);
    g_hash_table_insert(currencies_hash, fixup_data->new_c, price_list);
  }
}

void
gnc_pricedb_substitute_commodity(GNCPriceDB *db,
                                 gnc_commodity *old_c,
                                 gnc_commodity *new_c)
{
  GHashTable *currency_hash;
  GNCPriceFixupData data;

  if(!db || !old_c || !new_c) return;

  data.old_c = old_c;
  data.new_c = new_c;

  /* first remap the relevant commodity -> currency hash, if any */
  currency_hash = g_hash_table_lookup(db->commodity_hash, old_c);
  if(currency_hash) {
    g_hash_table_remove(db->commodity_hash, old_c);
    g_hash_table_insert(db->commodity_hash, new_c, currency_hash);
  }

  g_hash_table_foreach(db->commodity_hash, remap_currency_hash_keys, &data);

  if(!gnc_pricedb_foreach_price(db,
                                gnc_price_fixup_legacy_commods,
                                &data,
                                FALSE)) {
    PERR("Adjustments to legacy commodity pointers in pricedb failed!");
  }   
}

/***************************************************************************/

/* Semi-lame debugging code */

void
gnc_price_print(GNCPrice *p, FILE *f, int indent)
{
  gnc_commodity *commodity;
  gnc_commodity *currency;
  gchar *istr = NULL;           /* indent string */
  if(!p) return;
  if(!f) return;

  commodity = gnc_price_get_commodity(p);
  currency = gnc_price_get_currency(p);
  
  if(!commodity) return;
  if(!currency) return;

  istr = g_strnfill(indent, ' ');

  fprintf(f, "%s<pdb:price>\n", istr);
  fprintf(f, "%s  <pdb:commodity pointer=%p>\n", istr, commodity);
  fprintf(f, "%s    <cmdty:ref-space> %s</gnc:cmdty:ref-space>\n", istr,
          gnc_commodity_get_namespace(commodity));
  fprintf(f, "%s    <cmdty:ref-id>%s</cmdty:ref-id>\n", istr,
          gnc_commodity_get_mnemonic(commodity));
  fprintf(f, "%s  </pdb:commodity>\n", istr);
  fprintf(f, "%s  <pdb:currency pointer=%p>\n", istr, currency);
  fprintf(f, "%s    <cmdty:ref-space>%s</gnc:cmdty:ref-space>\n", istr,
          gnc_commodity_get_namespace(currency));
  fprintf(f, "%s    <cmdty:ref-id>%s</cmdty:ref-id>\n", istr,
          gnc_commodity_get_mnemonic(currency));
  fprintf(f, "%s  </pdb:currency>\n", istr);
  fprintf(f, "%s  %s\n", istr, gnc_price_get_source(p));
  fprintf(f, "%s  %s\n", istr, gnc_price_get_type(p));
  fprintf(f, "%s  %g\n", istr, gnc_numeric_to_double(gnc_price_get_value(p)));
  fprintf(f, "%s</pdb:price>\n", istr);

  g_free(istr);
}

static void
gnc_price_print_stdout(GNCPrice *p, int indent)
{
  gnc_price_print(p, stdout, indent);
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
