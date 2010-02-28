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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
 *******************************************************************/

#include "config.h"

#include <glib.h>
#include <string.h>
#include "gnc-pricedb-p.h"
#include "qofbackend-p.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_PRICE;

static gboolean add_price(GNCPriceDB *db, GNCPrice *p);
static gboolean remove_price(GNCPriceDB *db, GNCPrice *p, gboolean cleanup);

enum {
    PROP_0,
	PROP_SOURCE,
	PROP_TYPE,
	PROP_VALUE
};

/* GObject Initialization */
G_DEFINE_TYPE(GNCPrice, gnc_price, QOF_TYPE_INSTANCE);

static void
gnc_price_init(GNCPrice* price)
{
  price->refcount = 1;
  price->value = gnc_numeric_zero();
  price->type = NULL;
  price->source = NULL;
}

static void
gnc_price_dispose(GObject *pricep)
{
    G_OBJECT_CLASS(gnc_price_parent_class)->dispose(pricep);
}

static void
gnc_price_finalize(GObject* pricep)
{
    G_OBJECT_CLASS(gnc_price_parent_class)->finalize(pricep);
}

static void
gnc_price_get_property(GObject* object, guint prop_id, GValue* value, GParamSpec* pspec)
{
    GNCPrice* price;

	g_return_if_fail(GNC_IS_PRICE(object));

	price = GNC_PRICE(object);
	switch (prop_id) {
	case PROP_SOURCE:
	    g_value_set_string(value, price->source);
        break;
	case PROP_TYPE:
	    g_value_set_string(value, price->type);
        break;
	case PROP_VALUE:
	    g_value_set_boxed(value, &price->value);
		break;
	default:
	    G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
	    break;
    }
}

static void
gnc_price_set_property(GObject* object, guint prop_id, const GValue* value, GParamSpec* pspec)
{
    GNCPrice* price;
	gnc_numeric* number;

	g_return_if_fail(GNC_IS_PRICE(object));

	price = GNC_PRICE(object);
	switch (prop_id) {
	case PROP_SOURCE:
	    gnc_price_set_source(price, g_value_get_string(value));
        break;
	case PROP_TYPE:
	    gnc_price_set_typestr(price, g_value_get_string(value));
        break;
	case PROP_VALUE:
	    number = g_value_get_boxed(value);
	    gnc_price_set_value(price, *number);
	    break;
	default:
	    G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
	    break;
    }
}

static void
gnc_price_class_init(GNCPriceClass *klass)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	
    gobject_class->dispose = gnc_price_dispose;
    gobject_class->finalize = gnc_price_finalize;
    gobject_class->set_property = gnc_price_set_property;
    gobject_class->get_property = gnc_price_get_property;

    g_object_class_install_property
	(gobject_class,
	 PROP_SOURCE,
	 g_param_spec_string ("source",
			      "Price source",
			      "The price source is a string describing the "
				  "source of a price quote.  It will be something "
				  "like this: 'Finance::Quote', 'user:misc', "
				  "'user:foo', etc.",
			      NULL,
			      G_PARAM_READWRITE));

    g_object_class_install_property
	(gobject_class,
	 PROP_TYPE,
	 g_param_spec_string ("type",
			      "Quote type",
			      "The quote type is a string describing the "
				  "type of a price quote.  Types possible now "
				  "are 'bid', 'ask', 'last', 'nav' and 'unknown'.",
			      NULL,
				  G_PARAM_READWRITE));

    g_object_class_install_property
	(gobject_class,
	 PROP_VALUE,
	 g_param_spec_boxed("value",
                            "Value",
                            "The value of the price quote.",
                            GNC_TYPE_NUMERIC,
                            G_PARAM_READWRITE));
}

/* ==================================================================== */
/* GNCPrice functions
 */

/* allocation */
GNCPrice *
gnc_price_create (QofBook *book)
{
  GNCPrice *p;

  g_return_val_if_fail (book, NULL);

  p = g_object_new(GNC_TYPE_PRICE, NULL);

  qof_instance_init_data (&p->inst, GNC_ID_PRICE, book);
  qof_event_gen (&p->inst, QOF_EVENT_CREATE, NULL);

  return p;
}

static void
gnc_price_destroy (GNCPrice *p)
{
  ENTER(" ");
  qof_event_gen (&p->inst, QOF_EVENT_DESTROY, NULL);

  if(p->type) CACHE_REMOVE(p->type);
  if(p->source) CACHE_REMOVE(p->source);

  /* qof_instance_release (&p->inst); */
  g_object_unref(p);
  LEAVE (" ");
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

  g_return_val_if_fail (book, NULL);

  ENTER ("pr=%p", p);

  if(!p) { LEAVE (" "); return NULL; }

  new_p = gnc_price_create(book);
  if(!new_p) { LEAVE (" "); return NULL; }

  qof_instance_copy_version(new_p, p);

  gnc_price_begin_edit(new_p);
  /* never ever clone guid's */
  gnc_price_set_commodity(new_p, gnc_price_get_commodity(p));
  gnc_price_set_time(new_p, gnc_price_get_time(p));
  gnc_price_set_source(new_p, gnc_price_get_source(p));
  gnc_price_set_typestr(new_p, gnc_price_get_typestr(p));
  gnc_price_set_value(new_p, gnc_price_get_value(p));
  gnc_price_set_currency(new_p, gnc_price_get_currency(p));
  gnc_price_commit_edit(new_p);
  LEAVE (" ");
  return(new_p);
}

/* ==================================================================== */

void
gnc_price_begin_edit (GNCPrice *p)
{
  qof_begin_edit(&p->inst);
}

static void commit_err (QofInstance *inst, QofBackendError errcode)
{
  PERR ("Failed to commit: %d", errcode);
  gnc_engine_signal_commit_error( errcode );
}

static void noop (QofInstance *inst) {}

void
gnc_price_commit_edit (GNCPrice *p)
{
  if (!qof_commit_edit (QOF_INSTANCE(p))) return;
  qof_commit_edit_part2 (&p->inst, commit_err, noop, noop);
}

/* ==================================================================== */

void
gnc_pricedb_begin_edit (GNCPriceDB *pdb)
{
  qof_begin_edit(&pdb->inst);
}

void
gnc_pricedb_commit_edit (GNCPriceDB *pdb)
{
  if (!qof_commit_edit (QOF_INSTANCE(pdb))) return;
  qof_commit_edit_part2 (&pdb->inst, commit_err, noop, noop);
}

/* ==================================================================== */
/* setters */

static void
gnc_price_set_dirty (GNCPrice *p)
{
  qof_instance_set_dirty(&p->inst);
  qof_event_gen(&p->inst, QOF_EVENT_MODIFY, NULL);
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
    gnc_price_set_dirty(p);
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
    gnc_price_set_dirty(p);
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
    gnc_price_set_dirty(p);
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
    char *tmp;

    gnc_price_begin_edit (p);
    tmp = CACHE_INSERT((gpointer) s);
    if(p->source) CACHE_REMOVE(p->source);
    p->source = tmp;
    gnc_price_set_dirty(p);
    gnc_price_commit_edit (p);
  }
}

void
gnc_price_set_typestr(GNCPrice *p, const char* type)
{
  if(!p) return;
  if(safe_strcmp(p->type, type) != 0)
  {
    gchar *tmp;

    gnc_price_begin_edit (p);
    tmp = CACHE_INSERT((gpointer) type);
    if(p->type) CACHE_REMOVE(p->type);
    p->type = tmp;
    gnc_price_set_dirty(p);
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
    gnc_price_set_dirty(p);
    gnc_price_commit_edit (p);
  }
}

/* ==================================================================== */
/* getters */

GNCPrice *
gnc_price_lookup (const GUID *guid, QofBook *book)
{
  QofCollection *col;

  if (!guid || !book) return NULL;
  col = qof_book_get_collection (book, GNC_ID_PRICE);
  return (GNCPrice *) qof_collection_lookup_entity (col, guid);
}

gnc_commodity *
gnc_price_get_commodity(const GNCPrice *p)
{
  if(!p) return NULL;
  return p->commodity;
}

Timespec
gnc_price_get_time(const GNCPrice *p)
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
gnc_price_get_source(const GNCPrice *p)
{
  if(!p) return NULL;
  return p->source;
}

const char *
gnc_price_get_typestr(const GNCPrice *p)
{
  if(!p) return NULL;
  return p->type;
}

gnc_numeric
gnc_price_get_value(const GNCPrice *p)
{
  if(!p) {
    PERR("price NULL.\n");
    return gnc_numeric_zero();
  }
  return p->value;
}

gnc_commodity *
gnc_price_get_currency(const GNCPrice *p)
{
  if(!p) return NULL;
  return p->currency;
}

gboolean
gnc_price_equal (const GNCPrice *p1, const GNCPrice *p2)
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

  if (safe_strcmp (gnc_price_get_typestr (p1),
                   gnc_price_get_typestr (p2)) != 0)
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

typedef struct {
	GNCPrice* pPrice;
	gboolean isDupl;
} PriceListIsDuplStruct;

static void
price_list_is_duplicate( gpointer data, gpointer user_data )
{
	GNCPrice* pPrice = (GNCPrice*)data;
	PriceListIsDuplStruct* pStruct = (PriceListIsDuplStruct*)user_data;
	Timespec time_a, time_b;

    time_a = timespecCanonicalDayTime( gnc_price_get_time( pPrice ) );
    time_b = timespecCanonicalDayTime( gnc_price_get_time( pStruct->pPrice ) );

	/* If the date, currency, commodity and price match, it's a duplicate */
	if( !gnc_numeric_equal( gnc_price_get_value( pPrice ),  gnc_price_get_value( pStruct->pPrice ) ) ) return;
	if( gnc_price_get_commodity( pPrice ) != gnc_price_get_commodity( pStruct->pPrice ) ) return;
	if( gnc_price_get_currency( pPrice ) != gnc_price_get_currency( pStruct->pPrice ) ) return;

  if( timespec_cmp( &time_a, &time_b ) != 0 ) return;

	pStruct->isDupl = TRUE;
}

gboolean
gnc_price_list_insert(PriceList **prices, GNCPrice *p, gboolean check_dupl)
{
  GList *result_list;
  PriceListIsDuplStruct* pStruct;
  gboolean isDupl;

  if(!prices || !p) return FALSE;
  gnc_price_ref(p);

  if (check_dupl) {
    pStruct = g_new0( PriceListIsDuplStruct, 1 );
    pStruct->pPrice = p;
    pStruct->isDupl = FALSE;
    g_list_foreach( *prices, price_list_is_duplicate, pStruct );
    isDupl = pStruct->isDupl;
    g_free( pStruct );

    if( isDupl ) {
      return TRUE;
    }
  }

  result_list = g_list_insert_sorted(*prices, p, compare_prices_by_date);
  if(!result_list) return FALSE;
  *prices = result_list;
  return TRUE;
}

gboolean
gnc_price_list_remove(PriceList **prices, GNCPrice *p)
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
gnc_price_list_destroy(PriceList *prices)
{
  g_list_foreach(prices, price_list_destroy_helper, NULL);
  g_list_free(prices);
}

gboolean
gnc_price_list_equal(PriceList *prices1, PriceList *prices2)
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

/* GObject Initialization */
QOF_GOBJECT_IMPL(gnc_pricedb, GNCPriceDB, QOF_TYPE_INSTANCE);

static void
gnc_pricedb_init(GNCPriceDB* pdb)
{
}

static void
gnc_pricedb_dispose_real (GObject *pdbp)
{
}

static void
gnc_pricedb_finalize_real(GObject* pdbp)
{
}

static GNCPriceDB *
gnc_pricedb_create(QofBook * book)
{
  GNCPriceDB * result;
  QofCollection *col;

  g_return_val_if_fail (book, NULL);

  /* There can only be one pricedb per book.  So if one exits already,
   * then use that.  Warn user, they shouldn't be creating two ...
   */
  col = qof_book_get_collection (book, GNC_ID_PRICEDB);
  result = qof_collection_get_data (col);
  if (result)
  {
    PWARN ("A price database already exists for this book!");
    return result;
  }

  result = g_object_new(GNC_TYPE_PRICEDB, NULL);
  qof_instance_init_data (&result->inst, GNC_ID_PRICEDB, book);
  qof_collection_mark_clean(col);

  /** \todo This leaks result when the collection is destroyed.  When
     qofcollection is fixed to allow a destroy notifier, we'll need to
     provide one here. */
  qof_collection_set_data (col, result);

  result->commodity_hash = g_hash_table_new(NULL, NULL);
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
  GNCPrice *p;

  for (node = price_list; node; node = node->next)
  {
    p = node->data;

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
  if(db->commodity_hash) {
  g_hash_table_foreach (db->commodity_hash,
                        destroy_pricedb_commodity_hash_data,
                        NULL);
  }
  g_hash_table_destroy (db->commodity_hash);
  db->commodity_hash = NULL;
  /* qof_instance_release (&db->inst); */
  g_object_unref(db);
}

void
gnc_pricedb_set_bulk_update(GNCPriceDB *db, gboolean bulk_update)
{
  db->bulk_update = bulk_update;
}

/* ==================================================================== */
/* This is kind of weird, the way its done.  Each collection of prices
 * for a given commodity should get its own guid, be its own entity, etc.
 * We really shouldn't be using the collection data.  But, hey I guess its OK,
 * yeah? Umm, possibly not. (NW). See TODO below.
*/
/** \todo Collections of prices are not destroyed fully.

    \par
    gnc_pricedb_destroy does not clean up properly because
    gnc_pricedb_create reports an existing PriceDB after
    running gnc_pricedb_destroy. To change the pricedb, we need to
    destroy and recreate the book. Yuk.
 */

GNCPriceDB *
gnc_collection_get_pricedb(QofCollection *col)
{
  if (!col) return NULL;
  return qof_collection_get_data (col);
}

GNCPriceDB *
gnc_pricedb_get_db(QofBook *book)
{
  QofCollection *col;

  if (!book) return NULL;
  col = qof_book_get_collection (book, GNC_ID_PRICEDB);
  return gnc_collection_get_pricedb (col);
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
  ENTER ("db=%p, pr=%p dirty=%d destroying=%d",
         db, p, qof_instance_get_dirty_flag(p),
         qof_instance_get_destroying(p));

  if (!qof_instance_books_equal(db, p))
  {
     PERR ("attempted to mix up prices across different books");
	 LEAVE (" ");
     return FALSE;
  }

  commodity = gnc_price_get_commodity(p);
  if(!commodity) {
    PWARN("no commodity");
	LEAVE (" ");
    return FALSE;
  }
  currency = gnc_price_get_currency(p);
  if(!currency) {
    PWARN("no currency");
	LEAVE (" ");
    return FALSE;
  }
  if(!db->commodity_hash) { LEAVE ("no commodity hash found "); return FALSE; }

  currency_hash = g_hash_table_lookup(db->commodity_hash, commodity);
  if(!currency_hash) {
    currency_hash = g_hash_table_new(NULL, NULL);
    g_hash_table_insert(db->commodity_hash, commodity, currency_hash);
  }

  price_list = g_hash_table_lookup(currency_hash, currency);
  if(!gnc_price_list_insert(&price_list, p, !db->bulk_update)) 
  {
	  LEAVE ("gnc_price_list_insert failed");
	  return FALSE;
  }
  if(!price_list) 
  {
	LEAVE (" no price list");
	return FALSE;
  }
  g_hash_table_insert(currency_hash, currency, price_list);
  p->db = db;
  qof_event_gen (&p->inst, QOF_EVENT_ADD, NULL);

  LEAVE ("db=%p, pr=%p dirty=%d dextroying=%d commodity=%s/%s currency_hash=%p",
         db, p, qof_instance_get_dirty_flag(p),
         qof_instance_get_destroying(p),
         gnc_commodity_get_namespace(p->commodity),
         gnc_commodity_get_mnemonic(p->commodity),
         currency_hash);
  return TRUE;
}

/* the gnc_pricedb_add_price() function will use p, adding a ref, so
   treat p as read-only if this function succeeds. (Huh ???) */
gboolean
gnc_pricedb_add_price(GNCPriceDB *db, GNCPrice *p)
{
  if(!db || !p) return FALSE;

  ENTER ("db=%p, pr=%p dirty=%d destroying=%d",
         db, p, qof_instance_get_dirty_flag(p),
         qof_instance_get_destroying(p));

  if (FALSE == add_price(db, p)) 
  {
	  LEAVE (" failed to add price");
	  return FALSE;
  }

  gnc_pricedb_begin_edit(db);
  qof_instance_set_dirty(&db->inst);
  gnc_pricedb_commit_edit(db);

  LEAVE ("db=%p, pr=%p dirty=%d destroying=%d",
         db, p, qof_instance_get_dirty_flag(p),
         qof_instance_get_destroying(p));

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
  ENTER ("db=%p, pr=%p dirty=%d destroying=%d",
         db, p, qof_instance_get_dirty_flag(p),
         qof_instance_get_destroying(p));

  commodity = gnc_price_get_commodity(p);
  if(!commodity) { LEAVE (" no commodity"); return FALSE; }
  currency = gnc_price_get_currency(p);
  if(!currency) { LEAVE (" no currency"); return FALSE;}
  if(!db->commodity_hash) 
  {
	  LEAVE (" no commodity hash");
	  return FALSE;
  }

  currency_hash = g_hash_table_lookup(db->commodity_hash, commodity);
  if(!currency_hash) { LEAVE (" no currency hash"); return FALSE; }

  qof_event_gen (&p->inst, QOF_EVENT_REMOVE, NULL);
  price_list = g_hash_table_lookup(currency_hash, currency);
  gnc_price_ref(p);
  if(!gnc_price_list_remove(&price_list, p)) {
    gnc_price_unref(p);
	LEAVE (" cannot remove price list");
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
  ENTER ("db=%p, pr=%p dirty=%d destroying=%d",
         db, p, qof_instance_get_dirty_flag(p),
         qof_instance_get_destroying(p));

  gnc_price_ref(p);
  rc = remove_price (db, p, TRUE);
  gnc_pricedb_begin_edit(db);
  qof_instance_set_dirty(&db->inst);
  gnc_pricedb_commit_edit(db);

  /* invoke the backend to delete this price */
  gnc_price_begin_edit (p);
  qof_instance_set_destroying(p, TRUE);
  gnc_price_commit_edit (p);
  p->db = NULL;
  gnc_price_unref(p);
  LEAVE ("db=%p, pr=%p", db, p);
  return rc;
}

typedef struct {
  GNCPriceDB *db;
  Timespec cutoff;
  gboolean delete_user;
  gboolean delete_last;
  GSList *list;
} remove_info;

static gboolean
check_one_price_date (GNCPrice *price, gpointer user_data)
{
  remove_info *data = user_data;
  const gchar *source;
  Timespec pt;

  ENTER("price %p (%s), data %p", price,
	gnc_commodity_get_mnemonic(gnc_price_get_commodity(price)),
	user_data);
  if (!data->delete_user) {
    source = gnc_price_get_source (price);
    if (safe_strcmp(source, "Finance::Quote") != 0) {
      LEAVE("Not an automatic quote");
      return TRUE;
    }
  }

  pt = gnc_price_get_time (price);
  {
    gchar buf[40];
    gnc_timespec_to_iso8601_buff(pt , buf);
    DEBUG("checking date %s", buf);
  }
  if (timespec_cmp (&pt, &data->cutoff) < 0) {
    data->list = g_slist_prepend(data->list, price);
    DEBUG("will delete");
  }
  LEAVE(" ");
  return TRUE;
}

static void
pricedb_remove_foreach_pricelist (gpointer key,
				  gpointer val,
				  gpointer user_data)
{
  GList *price_list = (GList *) val;
  GList *node = price_list;
  remove_info *data = (remove_info *) user_data;

  ENTER("key %p, value %p, data %p", key, val, user_data);

  /* The most recent price is the first in the list */
  if (!data->delete_last)
    node = g_list_next(node);

  /* now check each item in the list */
  g_list_foreach(node, (GFunc)check_one_price_date, data);

  LEAVE(" ");
}

static void
pricedb_remove_foreach_currencies_hash (gpointer key,
					gpointer val,
					gpointer user_data)
{
  GHashTable *currencies_hash = (GHashTable *) val;

  ENTER("key %p, value %p, data %p", key, val, user_data);
  g_hash_table_foreach(currencies_hash,
		       pricedb_remove_foreach_pricelist, user_data);
  LEAVE(" ");
}


gboolean
gnc_pricedb_remove_old_prices(GNCPriceDB *db,
			      Timespec cutoff,
			      gboolean delete_user,
			      gboolean delete_last)
{
  remove_info data;
  GSList *item;

  data.db = db;
  data.cutoff = cutoff;
  data.delete_user = delete_user;
  data.delete_last = delete_last;
  data.list = NULL;

  ENTER("db %p, delet_user %d, delete_last %d", db, delete_user, delete_last);
  {
    gchar buf[40];
    gnc_timespec_to_iso8601_buff(cutoff, buf);
    DEBUG("checking date %s", buf);
  }

  /* Traverse the database once building up an external list of prices
   * to be deleted */
  g_hash_table_foreach(db->commodity_hash,
                       pricedb_remove_foreach_currencies_hash,
                       &data);

  if (data.list == NULL)
    return FALSE;

  /* Now run this external list deleting prices */
  for (item = data.list; item; item = g_slist_next(item)) {
    gnc_pricedb_remove_price(db, item->data);
  }

  g_slist_free(data.list);
  LEAVE(" ");
  return TRUE;
}

/* ==================================================================== */
/* lookup/query functions */

GNCPrice *
gnc_pricedb_lookup_latest(GNCPriceDB *db,
                          const gnc_commodity *commodity,
                          const gnc_commodity *currency)
{
  GList *price_list;
  GNCPrice *result;
  GHashTable *currency_hash;
  QofBook *book;
  QofBackend *be;

  if(!db || !commodity || !currency) return NULL;
  ENTER ("db=%p commodity=%p currency=%p", db, commodity, currency);
  book = qof_instance_get_book(&db->inst);
  be = qof_book_get_backend(book);
#ifdef GNUCASH_MAJOR_VERSION
  if (be && be->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_LATEST;
     pl.prdb = db;
     pl.commodity = commodity;
     pl.currency = currency;
     (be->price_lookup) (be, &pl);
  }
#endif

  currency_hash = g_hash_table_lookup(db->commodity_hash, commodity);
  if(!currency_hash) { LEAVE (" no currency hash"); return NULL; }

  price_list = g_hash_table_lookup(currency_hash, currency);
  if(!price_list) { LEAVE (" no price list"); return NULL; }

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
  gnc_price_list_insert(return_list, price_list->data, FALSE);
}

PriceList *
gnc_pricedb_lookup_latest_any_currency(GNCPriceDB *db,
                                       const gnc_commodity *commodity)
{
  GList *result;
  GHashTable *currency_hash;
  QofBook *book;
  QofBackend *be;

  result = NULL;

  if(!db || !commodity) return NULL;
  ENTER ("db=%p commodity=%p", db, commodity);
  book = qof_instance_get_book(&db->inst);
  be = qof_book_get_backend(book);
#ifdef GNUCASH_MAJOR_VERSION
  if (be && be->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_LATEST;
     pl.prdb = db;
     pl.commodity = commodity;
     pl.currency = NULL;  /* can the backend handle this??? */
		(be->price_lookup) (be, &pl);
  }
#endif
  currency_hash = g_hash_table_lookup(db->commodity_hash, commodity);
  if(!currency_hash) { LEAVE (" no currency hash"); return NULL; }

  g_hash_table_foreach(currency_hash, lookup_latest, &result);

  if(!result) { LEAVE (" "); return NULL; }

  result = g_list_sort(result, compare_prices_by_date);

  LEAVE(" ");
  return result;
}


static void
hash_values_helper(gpointer key, gpointer value, gpointer data)
{
  GList ** l = data;
  *l = g_list_concat(*l, g_list_copy (value));
}

gboolean
gnc_pricedb_has_prices(GNCPriceDB *db,
                       const gnc_commodity *commodity,
                       const gnc_commodity *currency)
{
  GList *price_list;
  GHashTable *currency_hash;
  gint size;
  QofBook *book;
  QofBackend *be;

  if(!db || !commodity) return FALSE;
  ENTER ("db=%p commodity=%p currency=%p", db, commodity, currency);
  book = qof_instance_get_book(&db->inst);
  be = qof_book_get_backend(book);
#ifdef GNUCASH_MAJOR_VERSION
  if (book && be && be->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_ALL;
     pl.prdb = db;
     pl.commodity = commodity;
     pl.currency = currency;
     (be->price_lookup) (be, &pl);
  }
#endif
  currency_hash = g_hash_table_lookup(db->commodity_hash, commodity);
  if(!currency_hash) {
    LEAVE("no, no currency_hash table");
    return FALSE;
  }

  if (currency) {
    price_list = g_hash_table_lookup(currency_hash, currency);
    if (price_list) {
      LEAVE("yes");
      return TRUE;
    }
    LEAVE("no, no price list");
    return FALSE;
  }

  size = g_hash_table_size (currency_hash);
  LEAVE("%s", size > 0 ? "yes" : "no");
  return size > 0;
}


PriceList *
gnc_pricedb_get_prices(GNCPriceDB *db,
                       const gnc_commodity *commodity,
                       const gnc_commodity *currency)
{
  GList *price_list;
  GList *result;
  GList *node;
  GHashTable *currency_hash;
  QofBook *book;
  QofBackend *be;

  if(!db || !commodity) return NULL;
  ENTER ("db=%p commodity=%p currency=%p", db, commodity, currency);
  book = qof_instance_get_book(&db->inst);
  be = qof_book_get_backend(book);
#ifdef GNUCASH_MAJOR_VERSION
  if (be && be->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_ALL;
     pl.prdb = db;
     pl.commodity = commodity;
     pl.currency = currency;
     (be->price_lookup) (be, &pl);
  }
#endif
  currency_hash = g_hash_table_lookup(db->commodity_hash, commodity);
  if(!currency_hash) { LEAVE (" no currency hash"); return NULL; }

  if (currency) {
    price_list = g_hash_table_lookup(currency_hash, currency);
    if(!price_list) { LEAVE (" no price list"); return NULL; }
    result = g_list_copy (price_list);
  } else {
    result = NULL;
    g_hash_table_foreach(currency_hash, hash_values_helper, (gpointer)&result);
  }
  for (node = result; node; node = node->next)
    gnc_price_ref (node->data);

  LEAVE (" ");
  return result;
}


PriceList *
gnc_pricedb_lookup_day(GNCPriceDB *db,
                       const gnc_commodity *c,
                       const gnc_commodity *currency,
                       Timespec t)
{
  GList *price_list;
  GList *result = NULL;
  GList *item = NULL;
  GHashTable *currency_hash;
  QofBook *book;
  QofBackend *be;

  if(!db || !c || !currency) return NULL;
  ENTER ("db=%p commodity=%p currency=%p", db, c, currency);
  book = qof_instance_get_book(&db->inst);
  be = qof_book_get_backend(book);
  /* Convert to noon local time. */
  t = timespecCanonicalDayTime(t);
#ifdef GNUCASH_MAJOR_VERSION
  if (be && be->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_AT_TIME;
     pl.prdb = db;
     pl.commodity = c;
     pl.currency = currency;
     pl.date = t;
     (be->price_lookup) (be, &pl);
  }
#endif
  currency_hash = g_hash_table_lookup(db->commodity_hash, c);
  if(!currency_hash) { LEAVE (" no currency hash"); return NULL; }

  price_list = g_hash_table_lookup(currency_hash, currency);
  if(!price_list) { LEAVE (" no price list"); return NULL; }

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
      gnc_price_list_insert(return_list, item->data, FALSE);
    }
    item = item->next;
  }
}

PriceList *
gnc_pricedb_lookup_day_any_currency(GNCPriceDB *db,
				    const gnc_commodity *c,
                                    Timespec t)
{
  GList *result = NULL;
  GHashTable *currency_hash;
  GNCPriceLookupHelper lookup_helper;
  QofBook *book;
  QofBackend *be;

  if(!db || !c) return NULL;
  ENTER ("db=%p commodity=%p", db, c);
  book = qof_instance_get_book(&db->inst);
  be = qof_book_get_backend(book);
  /* Convert to noon local time. */
  t = timespecCanonicalDayTime(t);
#ifdef GNUCASH_MAJOR_VERSION
  if (be && be->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_AT_TIME;
     pl.prdb = db;
     pl.commodity = c;
     pl.currency = NULL;  /* can the backend handle this??? */
     pl.date = t;
     (be->price_lookup) (be, &pl);
  }
#endif
  currency_hash = g_hash_table_lookup(db->commodity_hash, c);
  if(!currency_hash) { LEAVE ("no currency hash"); return NULL; }

  lookup_helper.return_list = &result;
  lookup_helper.time = t;
  g_hash_table_foreach(currency_hash, lookup_day, &lookup_helper);

  if(!result) { LEAVE (" "); return NULL; }

  result = g_list_sort(result, compare_prices_by_date);

  LEAVE (" ");
  return result;
}


PriceList *
gnc_pricedb_lookup_at_time(GNCPriceDB *db,
                           const gnc_commodity *c,
                           const gnc_commodity *currency,
                           Timespec t)
{
  GList *price_list;
  GList *result = NULL;
  GList *item = NULL;
  GHashTable *currency_hash;
  QofBook *book;
  QofBackend *be;

  if(!db || !c || !currency) return NULL;
  ENTER ("db=%p commodity=%p currency=%p", db, c, currency);
  book = qof_instance_get_book(&db->inst);
  be = qof_book_get_backend(book);
#ifdef GNUCASH_MAJOR_VERSION
  if (be && be->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_AT_TIME;
     pl.prdb = db;
     pl.commodity = c;
     pl.currency = currency;
     pl.date = t;
     (be->price_lookup) (be, &pl);
  }
#endif
  currency_hash = g_hash_table_lookup(db->commodity_hash, c);
  if(!currency_hash) { LEAVE (" no currency hash"); return NULL; }

  price_list = g_hash_table_lookup(currency_hash, currency);
  if(!price_list) { LEAVE (" no price list"); return NULL; }

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
      gnc_price_list_insert(return_list, item->data, FALSE);
    }
    item = item->next;
  }
}

PriceList *
gnc_pricedb_lookup_at_time_any_currency(GNCPriceDB *db,
					const gnc_commodity *c,
                                        Timespec t)
{
  GList *result = NULL;
  GHashTable *currency_hash;
  GNCPriceLookupHelper lookup_helper;
  QofBook *book;
  QofBackend *be;

  if(!db || !c) return NULL;
  ENTER ("db=%p commodity=%p", db, c);
  book = qof_instance_get_book(&db->inst);
  be = qof_book_get_backend(book);
#ifdef GNUCASH_MAJOR_VERSION
  if (be && be->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_AT_TIME;
     pl.prdb = db;
     pl.commodity = c;
     pl.currency = NULL;  /* can the backend handle this??? */
     pl.date = t;
     (be->price_lookup) (be, &pl);
  }
#endif
  currency_hash = g_hash_table_lookup(db->commodity_hash, c);
  if(!currency_hash) { LEAVE (" no currency hash"); return NULL; }

  lookup_helper.return_list = &result;
  lookup_helper.time = t;
  g_hash_table_foreach(currency_hash, lookup_time, &lookup_helper);

  if(!result) { LEAVE (" "); return NULL; }

  result = g_list_sort(result, compare_prices_by_date);

  LEAVE (" ");
  return result;
}


GNCPrice *
gnc_pricedb_lookup_nearest_in_time(GNCPriceDB *db,
                                   const gnc_commodity *c,
                                   const gnc_commodity *currency,
                                   Timespec t)
{
  GList *price_list;
  GNCPrice *current_price = NULL;
  GNCPrice *next_price = NULL;
  GNCPrice *result = NULL;
  GList *item = NULL;
  GHashTable *currency_hash;
  QofBook *book;
  QofBackend *be;

  if(!db || !c || !currency) return NULL;
  ENTER ("db=%p commodity=%p currency=%p", db, c, currency);
  book = qof_instance_get_book(&db->inst);
  be = qof_book_get_backend(book);
#ifdef GNUCASH_MAJOR_VERSION
  if (be && be->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_NEAREST_IN_TIME;
     pl.prdb = db;
     pl.commodity = c;
     pl.currency = currency;
     pl.date = t;
     (be->price_lookup) (be, &pl);
  }
#endif
  currency_hash = g_hash_table_lookup(db->commodity_hash, c);
  if(!currency_hash) { LEAVE ("no currency hash"); return NULL; }

  price_list = g_hash_table_lookup(currency_hash, currency);
  if(!price_list) { LEAVE ("no price list"); return NULL; }

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

      /* Choose the price that is closest to the given time. In case of
       * a tie, prefer the older price since it actually existed at the
       * time. (This also fixes bug #541970.) */
      if (timespec_cmp(&abs_current, &abs_next) < 0) {
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

GNCPrice *
gnc_pricedb_lookup_latest_before (GNCPriceDB *db,
                                   gnc_commodity *c,
                                   gnc_commodity *currency,
                                   Timespec t)
{
  GList *price_list;
  GNCPrice *current_price = NULL;
  /*  GNCPrice *next_price = NULL;
      GNCPrice *result = NULL;*/
  GList *item = NULL;
  GHashTable *currency_hash;
  QofBook *book;
  QofBackend *be;
  Timespec price_time;

  if(!db || !c || !currency) return NULL;
  ENTER ("db=%p commodity=%p currency=%p", db, c, currency);
  book = qof_instance_get_book(&db->inst);
  be = qof_book_get_backend(book);
#ifdef GNUCASH_MAJOR_VERSION
  if (be && be->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_LATEST_BEFORE;
     pl.prdb = db;
     pl.commodity = c;
     pl.currency = currency;
     pl.date = t;
     (be->price_lookup) (be, &pl);
  }
#endif
  currency_hash = g_hash_table_lookup(db->commodity_hash, c);
  if(!currency_hash) { LEAVE ("no currency hash"); return NULL; }

  price_list = g_hash_table_lookup(currency_hash, currency);
  if(!price_list) { LEAVE ("no price list"); return NULL; }

  item = price_list;
  do
    {
      price_time = gnc_price_get_time (item->data);
      if (timespec_cmp(&price_time, &t) <= 0)
	current_price = item->data;
      item = item->next;
    }
    while (timespec_cmp(&price_time, &t) > 0 && item);    
  gnc_price_ref(current_price);
  LEAVE (" ");
  return current_price;
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

  gnc_price_list_insert(return_list, result, FALSE);
}


static void
lookup_latest_before(gpointer key, gpointer val, gpointer user_data)
{
  //gnc_commodity *currency = (gnc_commodity *)key;
  GList *price_list = (GList *)val;
  GNCPrice *current_price = NULL;
  /*  GNCPrice *next_price = NULL;
      GNCPrice *result = NULL;*/
  GList *item = NULL;
  GNCPriceLookupHelper *lookup_helper = (GNCPriceLookupHelper *)user_data;
  GList **return_list = lookup_helper->return_list;
  Timespec t = lookup_helper->time;
  Timespec price_time;

  if (price_list)
    {
      item = price_list;
      do 
	{
	  price_time = gnc_price_get_time (item->data);
	  if (timespec_cmp(&price_time, &t) <= 0)
	    current_price = item->data;
	  item = item->next;
	}
	while (timespec_cmp(&price_time, &t) > 0 && item);
    }

  gnc_price_list_insert(return_list, current_price, FALSE);
}


PriceList *
gnc_pricedb_lookup_nearest_in_time_any_currency(GNCPriceDB *db,
                                                const gnc_commodity *c,
                                                Timespec t)
{
  GList *result = NULL;
  GHashTable *currency_hash;
  GNCPriceLookupHelper lookup_helper;
  QofBook *book;
  QofBackend *be;

  if(!db || !c) return NULL;
  ENTER ("db=%p commodity=%p", db, c);
  book = qof_instance_get_book(&db->inst);
  be = qof_book_get_backend(book);
#ifdef GNUCASH_MAJOR_VERSION
  if (be && be->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_NEAREST_IN_TIME;
     pl.prdb = db;
     pl.commodity = c;
     pl.currency = NULL;  /* can the backend handle this??? */
     pl.date = t;
     (be->price_lookup) (be, &pl);
  }
#endif
  currency_hash = g_hash_table_lookup(db->commodity_hash, c);
  if(!currency_hash) { LEAVE (" no currency hash"); return NULL; }

  lookup_helper.return_list = &result;
  lookup_helper.time = t;
  g_hash_table_foreach(currency_hash, lookup_nearest, &lookup_helper);

  if(!result) { LEAVE (" "); return NULL; }

  result = g_list_sort(result, compare_prices_by_date);

  LEAVE (" ");
  return result;
}


PriceList *
gnc_pricedb_lookup_latest_before_any_currency(GNCPriceDB *db,
					      gnc_commodity *c,
					      Timespec t)
{
  GList *result = NULL;
  GHashTable *currency_hash;
  GNCPriceLookupHelper lookup_helper;
  QofBook *book;
  QofBackend *be;

  if(!db || !c) return NULL;
  ENTER ("db=%p commodity=%p", db, c);
  book = qof_instance_get_book(&db->inst);
  be = qof_book_get_backend(book);
#ifdef GNUCASH_MAJOR_VERSION
  if (be && be->price_lookup)
  {
     GNCPriceLookup pl;
     pl.type = LOOKUP_LATEST_BEFORE;
     pl.prdb = db;
     pl.commodity = c;
     pl.currency = NULL;  /* can the backend handle this??? */
     pl.date = t;
     (be->price_lookup) (be, &pl);
  }
#endif
  currency_hash = g_hash_table_lookup(db->commodity_hash, c);
  if(!currency_hash) { LEAVE (" no currency hash"); return NULL; }

  lookup_helper.return_list = &result;
  lookup_helper.time = t;
  g_hash_table_foreach(currency_hash, lookup_latest_before, &lookup_helper);

  if(!result) { LEAVE (" "); return NULL; }

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
                                         const gnc_commodity *balance_currency,
                                         const gnc_commodity *new_currency)
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
                               GNC_HOW_RND_ROUND);
    gnc_price_unref (price);
    return balance;
  }

  /* Look for a price of the new currency in the balance currency and use
   * the reciprocal if we find it 
   */
  price = gnc_pricedb_lookup_latest (pdb, new_currency, balance_currency);
  if (price) {
    balance = gnc_numeric_div (balance, gnc_price_get_value (price),
                               gnc_commodity_get_fraction (new_currency),
                               GNC_HOW_RND_ROUND);
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
                              GNC_HOW_DENOM_EXACT | GNC_HOW_RND_NEVER);
        gnc_price_unref(currency_price);
      }
    }

    list_helper = list_helper->next;
  } while((list_helper != NULL) &&
          (gnc_numeric_zero_p(currency_price_value)));

  balance = gnc_numeric_mul (balance, currency_price_value,
                             GNC_DENOM_AUTO,
                             GNC_HOW_DENOM_EXACT | GNC_HOW_RND_NEVER);
  balance = gnc_numeric_mul (balance, gnc_price_get_value (price),
                             gnc_commodity_get_fraction (new_currency),
                             GNC_HOW_RND_ROUND);

  gnc_price_list_destroy(price_list);
  return balance;
}

gnc_numeric
gnc_pricedb_convert_balance_nearest_price(GNCPriceDB *pdb,
                                          gnc_numeric balance,
                                          const gnc_commodity *balance_currency,
                                          const gnc_commodity *new_currency,
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
                               GNC_HOW_RND_ROUND);
    gnc_price_unref (price);
    return balance;
  }

  /* Look for a price of the new currency in the balance currency and use
   * the reciprocal if we find it 
   */
  price = gnc_pricedb_lookup_nearest_in_time (pdb, new_currency, balance_currency, t);
  if (price) {
    balance = gnc_numeric_div (balance, gnc_price_get_value (price),
                               gnc_commodity_get_fraction (new_currency),
                               GNC_HOW_RND_ROUND);
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
                                               GNC_HOW_RND_ROUND);
        gnc_price_unref(currency_price);
      }
    }

    list_helper = list_helper->next;
  } while((list_helper != NULL) &&
	  (gnc_numeric_zero_p(currency_price_value)));

  balance = gnc_numeric_mul (balance, currency_price_value,
                             gnc_commodity_get_fraction (new_currency),
                             GNC_HOW_RND_ROUND);

  balance = gnc_numeric_mul (balance, gnc_price_get_value (price),
                             gnc_commodity_get_fraction (new_currency),
                             GNC_HOW_RND_ROUND);

  gnc_price_list_destroy(price_list);
  return balance;
}


gnc_numeric
gnc_pricedb_convert_balance_latest_before(GNCPriceDB *pdb,
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
  price = gnc_pricedb_lookup_latest_before (pdb, balance_currency, new_currency, t);
  
  if (price) {
    balance = gnc_numeric_mul (balance, gnc_price_get_value (price),
                               gnc_commodity_get_fraction (new_currency),
                               GNC_HOW_RND_ROUND);
    gnc_price_unref (price);
    return balance;
  }

  /* Look for a price of the new currency in the balance currency and use
   * the reciprocal if we find it.
   */
  price = gnc_pricedb_lookup_latest_before (pdb, new_currency, balance_currency, t);
  if (price) {
    balance = gnc_numeric_div (balance, gnc_price_get_value (price),
                               gnc_commodity_get_fraction (new_currency),
                               GNC_HOW_RND_ROUND);
    gnc_price_unref (price);
    return balance;
  }
  
  /*
   * no direct price found, try if we find a price in another currency
   * and convert in two stages
   */
  price_list = gnc_pricedb_lookup_latest_before_any_currency(pdb, balance_currency, t);
  if (!price_list) {
    balance =  gnc_numeric_zero ();
    return balance;
  }

  list_helper = price_list;
  currency_price_value = gnc_numeric_zero();

  do {
    price = (GNCPrice *)(list_helper->data);

    intermediate_currency = gnc_price_get_currency(price);
    currency_price = gnc_pricedb_lookup_latest_before(pdb, intermediate_currency,
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
                                               GNC_HOW_RND_ROUND);
        gnc_price_unref(currency_price);
      }
    }

    list_helper = list_helper->next;
  } while((list_helper != NULL) &&
          (gnc_numeric_zero_p(currency_price_value)));

  balance = gnc_numeric_mul (balance, currency_price_value,
                             gnc_commodity_get_fraction (new_currency),
                             GNC_HOW_RND_ROUND);
  balance = gnc_numeric_mul (balance, gnc_price_get_value (price),
                             gnc_commodity_get_fraction (new_currency),
                             GNC_HOW_RND_ROUND);

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
  if(db->commodity_hash == NULL) { return FALSE; }
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
  if(stable_order) {
	  LEAVE (" stable order found");
	  return stable_price_traversal(db, f, user_data);
  }
  LEAVE (" use unstable order");
  return unstable_price_traversal(db, f, user_data);
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
  str = gnc_price_get_typestr(p);
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
/* gncObject function implementation and registration */

static void
pricedb_book_begin (QofBook *book)
{
  GNCPriceDB *db;

  db = gnc_pricedb_create(book);
}

static void
pricedb_book_end (QofBook *book)
{
  GNCPriceDB *db;
  QofCollection *col;

  if (!book)
    return;
  col = qof_book_get_collection(book, GNC_ID_PRICEDB);
  db = qof_collection_get_data(col);
  qof_collection_set_data(col, NULL);
  gnc_pricedb_destroy(db);
}

static gpointer
price_create (QofBook *book)
{
  return gnc_price_create(book);
}

/* ==================================================================== */
/* a non-boolean foreach. Ugh */

typedef struct
{
  void (*func)(GNCPrice *p, gpointer user_data);
  gpointer user_data;
}
VoidGNCPriceDBForeachData;

static void
void_pricedb_foreach_pricelist(gpointer key, gpointer val, gpointer user_data)
{
  GList *price_list = (GList *) val;
  GList *node = price_list;
  VoidGNCPriceDBForeachData *foreach_data = (VoidGNCPriceDBForeachData *) user_data;

  while(node)
  {
    GNCPrice *p = (GNCPrice *) node->data;
    foreach_data->func(p, foreach_data->user_data);
    node = node->next;
  }
}

static void
void_pricedb_foreach_currencies_hash(gpointer key, gpointer val, gpointer user_data)
{
  GHashTable *currencies_hash = (GHashTable *) val;
  g_hash_table_foreach(currencies_hash, void_pricedb_foreach_pricelist, user_data);
}

static void
void_unstable_price_traversal(GNCPriceDB *db,
                       void (*f)(GNCPrice *p, gpointer user_data),
                       gpointer user_data)
{
  VoidGNCPriceDBForeachData foreach_data;

  if(!db || !f) return;
  foreach_data.func = f;
  foreach_data.user_data = user_data;

  g_hash_table_foreach(db->commodity_hash,
                       void_pricedb_foreach_currencies_hash,
                       &foreach_data);
}

static void
price_foreach(const QofCollection *col, QofInstanceForeachCB cb, gpointer data)
{
  GNCPriceDB *db;

  db = qof_collection_get_data(col);
  void_unstable_price_traversal(db,
                       (void (*)(GNCPrice *, gpointer)) cb,
                       data);
}

/* ==================================================================== */

static const char *
price_printable(gpointer obj)
{
  GNCPrice *pr = obj;
  gnc_commodity *commodity;
  gnc_commodity *currency;
  static char buff[2048];  /* nasty static OK for printing */
  char *val, *da;

  if(!pr) return "";

  val = gnc_numeric_to_string (pr->value);
  da = qof_print_date (pr->tmspec.tv_sec);

  commodity = gnc_price_get_commodity(pr);
  currency = gnc_price_get_currency(pr);

  g_snprintf (buff, 2048, "%s %s / %s on %s", val,
        gnc_commodity_get_unique_name(commodity),
        gnc_commodity_get_unique_name(currency),
        da);
  g_free (val);
  g_free (da);
  return buff;
}

#ifdef _MSC_VER
/* MSVC compiler doesn't have C99 "designated initializers"
 * so we wrap them in a macro that is empty on MSVC. */
# define DI(x) /* */
#else
# define DI(x) x
#endif
static QofObject price_object_def =
{
  DI(.interface_version =) QOF_OBJECT_VERSION,
  DI(.e_type            =) GNC_ID_PRICE,
  DI(.type_label        =) "Price",
  DI(.create            =) price_create,
  DI(.book_begin        =) NULL,
  DI(.book_end          =) NULL,
  DI(.is_dirty          =) qof_collection_is_dirty,
  DI(.mark_clean        =) qof_collection_mark_clean,
  DI(.foreach           =) price_foreach,
  DI(.printable         =) price_printable,
  DI(.version_cmp       =) NULL,
};

static QofObject pricedb_object_def =
{
  DI(.interface_version =) QOF_OBJECT_VERSION,
  DI(.e_type            =) GNC_ID_PRICEDB,
  DI(.type_label        =) "PriceDB",
  DI(.create            =) NULL,
  DI(.book_begin        =) pricedb_book_begin,
  DI(.book_end          =) pricedb_book_end,
  DI(.is_dirty          =) qof_collection_is_dirty,
  DI(.mark_clean        =) qof_collection_mark_clean,
  DI(.foreach           =) NULL,
  DI(.printable         =) NULL,
  DI(.version_cmp       =) NULL,
};

gboolean
gnc_pricedb_register (void)
{
  static QofParam params[] = {
    { PRICE_COMMODITY, GNC_ID_COMMODITY, (QofAccessFunc)gnc_price_get_commodity, (QofSetterFunc)gnc_price_set_commodity },
    { PRICE_CURRENCY, GNC_ID_COMMODITY, (QofAccessFunc)gnc_price_get_currency, (QofSetterFunc)gnc_price_set_currency },
    { PRICE_DATE, QOF_TYPE_DATE, (QofAccessFunc)gnc_price_get_time, (QofSetterFunc)gnc_price_set_time },
    { PRICE_SOURCE, QOF_TYPE_STRING, (QofAccessFunc)gnc_price_get_source, (QofSetterFunc)gnc_price_set_source },
    { PRICE_TYPE, QOF_TYPE_STRING, (QofAccessFunc)gnc_price_get_typestr, (QofSetterFunc)gnc_price_set_typestr },
    { PRICE_VALUE, QOF_TYPE_NUMERIC, (QofAccessFunc)gnc_price_get_value, (QofSetterFunc)gnc_price_set_value },
    { NULL },
  };

  qof_class_register (GNC_ID_PRICE, NULL, params);

  if (!qof_object_register (&price_object_def))
    return FALSE;
  return qof_object_register (&pricedb_object_def);
}

/* ========================= END OF FILE ============================== */
