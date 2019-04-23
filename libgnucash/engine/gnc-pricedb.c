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

#include <config.h>

#include <glib.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include "gnc-date.h"
#include "gnc-pricedb-p.h"
#include <qofinstance-p.h>

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_PRICE;

static gboolean add_price(GNCPriceDB *db, GNCPrice *p);
static gboolean remove_price(GNCPriceDB *db, GNCPrice *p, gboolean cleanup);
static GNCPrice *lookup_nearest_in_time(GNCPriceDB *db, const gnc_commodity *c,
                                        const gnc_commodity *currency,
                                        time64 t, gboolean sameday);
static gboolean
pricedb_pricelist_traversal(GNCPriceDB *db,
                            gboolean (*f)(GList *p, gpointer user_data),
                            gpointer user_data);

enum
{
    PROP_0,
    PROP_COMMODITY,	/* Table */
    PROP_CURRENCY,	/* Table */
    PROP_DATE,		/* Table */
    PROP_SOURCE,	/* Table */
    PROP_TYPE,		/* Table */
    PROP_VALUE,		/* Table, 2 fields (numeric) */
};

typedef struct
{
     gpointer key;
     gpointer value;
} HashEntry;

/* Like strcmp, returns -1 if a < b, +1 if a > b, and 0 if they're equal. */
static inline int
time64_cmp (time64 a, time64 b)
{
    return a < b ? -1 : a > b ? 1 : 0;
}

static void
hash_entry_insert(gpointer key, gpointer val, gpointer user_data)
{
    GSList **result = (GSList **) user_data;
    HashEntry *entry = g_new(HashEntry, 1);

    entry->key = key;
    entry->value = val;
    *result = g_slist_prepend(*result, entry);
}

static GSList *
hash_table_to_list(GHashTable *table)
{
    GSList *result_list = NULL;
    g_hash_table_foreach(table, hash_entry_insert, &result_list);
    return result_list;
}

static void
hash_entry_free_gfunc(gpointer data, G_GNUC_UNUSED gpointer user_data)
{
    HashEntry *entry = (HashEntry *) data;
    g_free(entry);
}

/* GObject Initialization */
G_DEFINE_TYPE(GNCPrice, gnc_price, QOF_TYPE_INSTANCE);

static void
gnc_price_init(GNCPrice* price)
{
    price->refcount = 1;
    price->value = gnc_numeric_zero();
    price->type = NULL;
    price->source = PRICE_SOURCE_INVALID;
}

/* Array of char constants for converting price-source enums. Be sure to keep in
 * sync with the enum values in gnc-pricedb.h The string user:price-editor is
 * explicitly used by price_to_gui() in dialog-price-editor.c. Beware
 * that the strings are used to store the enum values in the backends so any
 * changes will affect backward data compatibility.
 * The last two values, temporary and invalid, are *not* used.
 */
static const char* source_names[(size_t)PRICE_SOURCE_INVALID + 1] =
{
    /* sync with price_to_gui in dialog-price-editor.c */
    "user:price-editor",
    /* sync with commidity-tz-quote->price in price-quotes.scm */
    "Finance::Quote",
    "user:price",
    /* String retained for backwards compatibility. */
    "user:xfer-dialog",
    "user:split-register",
    "user:stock-split",
    "user:invoice-post", /* Retained for backwards compatibility */
    "temporary",
    "invalid"
};

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

/* Note that g_value_set_object() refs the object, as does
 * g_object_get(). But g_object_get() only unrefs once when it disgorges
 * the object, leaving an unbalanced ref, which leaks. So instead of
 * using g_value_set_object(), use g_value_take_object() which doesn't
 * ref the object when used in get_property().
 */
static void
gnc_price_get_property(GObject* object, guint prop_id, GValue* value, GParamSpec* pspec)
{
    GNCPrice* price;

    g_return_if_fail(GNC_IS_PRICE(object));

    price = GNC_PRICE(object);
    switch (prop_id)
    {
    case PROP_SOURCE:
        g_value_set_string(value, gnc_price_get_source_string(price));
        break;
    case PROP_TYPE:
        g_value_set_string(value, price->type);
        break;
    case PROP_VALUE:
        g_value_set_boxed(value, &price->value);
        break;
    case PROP_COMMODITY:
        g_value_take_object(value, price->commodity);
        break;
    case PROP_CURRENCY:
        g_value_take_object(value, price->currency);
        break;
    case PROP_DATE:
        g_value_set_boxed(value, &price->tmspec);
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
    Time64* time;

    g_return_if_fail(GNC_IS_PRICE(object));

    price = GNC_PRICE(object);
    g_assert (qof_instance_get_editlevel(price));

    switch (prop_id)
    {
    case PROP_SOURCE:
        gnc_price_set_source_string(price, g_value_get_string(value));
        break;
    case PROP_TYPE:
        gnc_price_set_typestr(price, g_value_get_string(value));
        break;
    case PROP_VALUE:
        number = g_value_get_boxed(value);
        gnc_price_set_value(price, *number);
        break;
    case PROP_COMMODITY:
        gnc_price_set_commodity(price, g_value_get_object(value));
        break;
    case PROP_CURRENCY:
        gnc_price_set_currency(price, g_value_get_object(value));
        break;
    case PROP_DATE:
        time = g_value_get_boxed(value);
        gnc_price_set_time64(price, time->t);
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
     PROP_COMMODITY,
     g_param_spec_object ("commodity",
                          "Commodity",
                          "The commodity field denotes the base kind of "
                          "'stuff' for the units of this quote, whether "
                          "it is USD, gold, stock, etc.",
                          GNC_TYPE_COMMODITY,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_CURRENCY,
     g_param_spec_object ("currency",
                          "Currency",
                          "The currency field denotes the external kind "
                          "'stuff' for the units of this quote, whether "
                          "it is USD, gold, stock, etc.",
                          GNC_TYPE_COMMODITY,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_SOURCE,
     g_param_spec_string ("source",
                          "Price source",
                          "The price source is PriceSource enum describing how"
                          " the price was created. This property works on the"
                          " string values in source_names for SQL database"
                          " compatibility.",
                          NULL,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_TYPE,
     g_param_spec_string ("type",
                          "Quote type",
                          "The quote type is a string describing the "
                          "type of a price quote.  Types possible now "
                          "are 'bid', 'ask', 'last', 'nav', 'transaction', "
                          "and 'unknown'.",
                          NULL,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_DATE,
     g_param_spec_boxed("date",
                        "Date",
                        "The date of the price quote.",
                        GNC_TYPE_NUMERIC,
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

    ENTER(" ");
    p = g_object_new(GNC_TYPE_PRICE, NULL);

    qof_instance_init_data (&p->inst, GNC_ID_PRICE, book);
    qof_event_gen (&p->inst, QOF_EVENT_CREATE, NULL);
    LEAVE ("price created %p", p);
    return p;
}

static void
gnc_price_destroy (GNCPrice *p)
{
    ENTER("destroy price %p", p);
    qof_event_gen (&p->inst, QOF_EVENT_DESTROY, NULL);

    if (p->type) CACHE_REMOVE(p->type);

    /* qof_instance_release (&p->inst); */
    g_object_unref(p);
    LEAVE (" ");
}

void
gnc_price_ref(GNCPrice *p)
{
    if (!p) return;
    p->refcount++;
}

void
gnc_price_unref(GNCPrice *p)
{
    if (!p) return;
    if (p->refcount == 0)
    {
        return;
    }

    p->refcount--;

    if (p->refcount <= 0)
    {
        if (NULL != p->db)
        {
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

    if (!p)
    {
        LEAVE ("return NULL");
        return NULL;
    }

    new_p = gnc_price_create(book);
    if (!new_p)
    {
        LEAVE ("return NULL");
        return NULL;
    }

    qof_instance_copy_version(new_p, p);

    gnc_price_begin_edit(new_p);
    /* never ever clone guid's */
    gnc_price_set_commodity(new_p, gnc_price_get_commodity(p));
    gnc_price_set_time64(new_p, gnc_price_get_time64(p));
    gnc_price_set_source(new_p, gnc_price_get_source(p));
    gnc_price_set_typestr(new_p, gnc_price_get_typestr(p));
    gnc_price_set_value(new_p, gnc_price_get_value(p));
    gnc_price_set_currency(new_p, gnc_price_get_currency(p));
    gnc_price_commit_edit(new_p);
    LEAVE ("return cloned price %p", new_p);
    return(new_p);
}

GNCPrice *
gnc_price_invert (GNCPrice *p)
{
    QofBook *book = qof_instance_get_book (QOF_INSTANCE(p));
    GNCPrice *new_p = gnc_price_create (book);
    qof_instance_copy_version(new_p, p);
    gnc_price_begin_edit(new_p);
    gnc_price_set_time64(new_p, gnc_price_get_time64(p));
    gnc_price_set_source(new_p, PRICE_SOURCE_TEMP);
    gnc_price_set_typestr(new_p, gnc_price_get_typestr(p));
    gnc_price_set_commodity(new_p, gnc_price_get_currency(p));
    gnc_price_set_currency(new_p, gnc_price_get_commodity(p));
    gnc_price_set_value(new_p, gnc_numeric_invert(gnc_price_get_value(p)));
    gnc_price_commit_edit(new_p);
    return new_p;
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
    if (!p) return;

    if (!gnc_commodity_equiv(p->commodity, c))
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
    if (!p) return;

    if (!gnc_commodity_equiv(p->currency, c))
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
gnc_price_set_time64(GNCPrice *p, time64 t)
{
    if (!p) return;
    if (p->tmspec != t)
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
gnc_price_set_source(GNCPrice *p, PriceSource s)
{
    if (!p) return;
    gnc_price_begin_edit (p);
    p->source = s;
    gnc_price_set_dirty(p);
    gnc_price_commit_edit(p);
}

void
gnc_price_set_source_string(GNCPrice *p, const char* str)
{
    if (!p) return;
    for (PriceSource s = PRICE_SOURCE_EDIT_DLG;
         s < PRICE_SOURCE_INVALID; ++s)
        if (strcmp(source_names[s], str) == 0)
        {
            gnc_price_set_source(p, s);
            return;
        }


}
void
gnc_price_set_typestr(GNCPrice *p, const char* type)
{
    if (!p) return;
    if (g_strcmp0(p->type, type) != 0)
    {
        gchar *tmp;

        gnc_price_begin_edit (p);
        tmp = CACHE_INSERT((gpointer) type);
        if (p->type) CACHE_REMOVE(p->type);
        p->type = tmp;
        gnc_price_set_dirty(p);
        gnc_price_commit_edit (p);
    }
}

void
gnc_price_set_value(GNCPrice *p, gnc_numeric value)
{
    if (!p) return;
    if (!gnc_numeric_eq(p->value, value))
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
gnc_price_lookup (const GncGUID *guid, QofBook *book)
{
    QofCollection *col;

    if (!guid || !book) return NULL;
    col = qof_book_get_collection (book, GNC_ID_PRICE);
    return (GNCPrice *) qof_collection_lookup_entity (col, guid);
}

gnc_commodity *
gnc_price_get_commodity(const GNCPrice *p)
{
    if (!p) return NULL;
    return p->commodity;
}

time64
gnc_price_get_time64(const GNCPrice *p)
{
    return p ? p->tmspec : 0;
}

PriceSource
gnc_price_get_source(const GNCPrice *p)
{
    if (!p) return PRICE_SOURCE_INVALID;
    return p->source;
}

const char*
gnc_price_get_source_string(const GNCPrice *p)
{
    if (!p) return NULL;
    return source_names[p->source];
}

const char *
gnc_price_get_typestr(const GNCPrice *p)
{
    if (!p) return NULL;
    return p->type;
}

gnc_numeric
gnc_price_get_value(const GNCPrice *p)
{
    if (!p)
    {
        PERR("price NULL.\n");
        return gnc_numeric_zero();
    }
    return p->value;
}

gnc_commodity *
gnc_price_get_currency(const GNCPrice *p)
{
    if (!p) return NULL;
    return p->currency;
}

gboolean
gnc_price_equal (const GNCPrice *p1, const GNCPrice *p2)
{
    time64 time1, time2;

    if (p1 == p2) return TRUE;
    if (!p1 || !p2) return FALSE;

    if (!gnc_commodity_equiv (gnc_price_get_commodity (p1),
                              gnc_price_get_commodity (p2)))
        return FALSE;

    if (!gnc_commodity_equiv (gnc_price_get_currency (p1),
                              gnc_price_get_currency (p2)))
        return FALSE;

    time1 = gnc_price_get_time64 (p1);
    time2 = gnc_price_get_time64 (p2);

    if (time1 != time2)
        return FALSE;

    if (gnc_price_get_source (p1) != gnc_price_get_source (p2))
        return FALSE;

    if (g_strcmp0 (gnc_price_get_typestr (p1),
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
    time64 time_a, time_b;
    gint result;

    if (!a && !b) return 0;
    /* nothing is always less than something */
    if (!a) return -1;

    time_a = gnc_price_get_time64((GNCPrice *) a);
    time_b = gnc_price_get_time64((GNCPrice *) b);

    /* Note we return -1 if time_b is before time_a. */
    result = time64_cmp(time_b, time_a);
    if (result) return result;

    /* For a stable sort */
    return guid_compare (gnc_price_get_guid((GNCPrice *) a),
                         gnc_price_get_guid((GNCPrice *) b));
}

typedef struct
{
    GNCPrice* pPrice;
    gboolean isDupl;
} PriceListIsDuplStruct;

static void
price_list_is_duplicate( gpointer data, gpointer user_data )
{
    GNCPrice* pPrice = (GNCPrice*)data;
    PriceListIsDuplStruct* pStruct = (PriceListIsDuplStruct*)user_data;
    time64 time_a, time_b;

    time_a = time64CanonicalDayTime( gnc_price_get_time64( pPrice ) );
    time_b = time64CanonicalDayTime( gnc_price_get_time64( pStruct->pPrice ) );

    /* If the date, currency, commodity and price match, it's a duplicate */
    if ( !gnc_numeric_equal( gnc_price_get_value( pPrice ),  gnc_price_get_value( pStruct->pPrice ) ) ) return;
    if ( gnc_price_get_commodity( pPrice ) != gnc_price_get_commodity( pStruct->pPrice ) ) return;
    if ( gnc_price_get_currency( pPrice ) != gnc_price_get_currency( pStruct->pPrice ) ) return;

    if (time_a != time_b) return;

    pStruct->isDupl = TRUE;
}

gboolean
gnc_price_list_insert(PriceList **prices, GNCPrice *p, gboolean check_dupl)
{
    GList *result_list;
    PriceListIsDuplStruct* pStruct;
    gboolean isDupl;

    if (!prices || !p) return FALSE;
    gnc_price_ref(p);

    if (check_dupl)
    {
        pStruct = g_new0( PriceListIsDuplStruct, 1 );
        pStruct->pPrice = p;
        pStruct->isDupl = FALSE;
        g_list_foreach( *prices, price_list_is_duplicate, pStruct );
        isDupl = pStruct->isDupl;
        g_free( pStruct );

        if ( isDupl )
        {
            return TRUE;
        }
    }

    result_list = g_list_insert_sorted(*prices, p, compare_prices_by_date);
    if (!result_list) return FALSE;
    *prices = result_list;
    return TRUE;
}

gboolean
gnc_price_list_remove(PriceList **prices, GNCPrice *p)
{
    GList *result_list;
    GList *found_element;

    if (!prices || !p) return FALSE;

    found_element = g_list_find(*prices, p);
    if (!found_element) return TRUE;

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
    pdb->reset_nth_price_cache = FALSE;
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
    if (!db) return;
    if (db->commodity_hash)
    {
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
    GNCPrice *old_price;

    if (!db || !p) return FALSE;
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
    if (!commodity)
    {
        PWARN("no commodity");
        LEAVE (" ");
        return FALSE;
    }
    currency = gnc_price_get_currency(p);
    if (!currency)
    {
        PWARN("no currency");
        LEAVE (" ");
        return FALSE;
    }
    if (!db->commodity_hash)
    {
        LEAVE ("no commodity hash found ");
        return FALSE;
    }
/* Check for an existing price on the same day. If there is no existing price,
 * add this one. If this price is of equal or better precedence than the old
 * one, copy this one over the old one.
 */
    old_price = gnc_pricedb_lookup_day_t64 (db, p->commodity, p->currency,
                                        p->tmspec);
    if (!db->bulk_update && old_price != NULL)
    {
        if (p->source > old_price->source)
        {
            gnc_price_unref(p);
            LEAVE ("Better price already in DB.");
            return FALSE;
        }
        gnc_pricedb_remove_price(db, old_price);
    }

    currency_hash = g_hash_table_lookup(db->commodity_hash, commodity);
    if (!currency_hash)
    {
        currency_hash = g_hash_table_new(NULL, NULL);
        g_hash_table_insert(db->commodity_hash, commodity, currency_hash);
    }

    price_list = g_hash_table_lookup(currency_hash, currency);
    if (!gnc_price_list_insert(&price_list, p, !db->bulk_update))
    {
        LEAVE ("gnc_price_list_insert failed");
        return FALSE;
    }

    if (!price_list)
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

/* If gnc_pricedb_add_price() succeeds, it takes ownership of the
   passed-in GNCPrice and inserts it into the pricedb. Writing to this
   pointer afterwards will have interesting results, so don't.
 */
gboolean
gnc_pricedb_add_price(GNCPriceDB *db, GNCPrice *p)
{
    if (!db || !p) return FALSE;

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

    if (!db || !p) return FALSE;
    ENTER ("db=%p, pr=%p dirty=%d destroying=%d",
           db, p, qof_instance_get_dirty_flag(p),
           qof_instance_get_destroying(p));

    commodity = gnc_price_get_commodity(p);
    if (!commodity)
    {
        LEAVE (" no commodity");
        return FALSE;
    }
    currency = gnc_price_get_currency(p);
    if (!currency)
    {
        LEAVE (" no currency");
        return FALSE;
    }
    if (!db->commodity_hash)
    {
        LEAVE (" no commodity hash");
        return FALSE;
    }

    currency_hash = g_hash_table_lookup(db->commodity_hash, commodity);
    if (!currency_hash)
    {
        LEAVE (" no currency hash");
        return FALSE;
    }

    qof_event_gen (&p->inst, QOF_EVENT_REMOVE, NULL);
    price_list = g_hash_table_lookup(currency_hash, currency);
    gnc_price_ref(p);
    if (!gnc_price_list_remove(&price_list, p))
    {
        gnc_price_unref(p);
        LEAVE (" cannot remove price list");
        return FALSE;
    }

    /* if the price list is empty, then remove this currency from the
       commodity hash */
    if (price_list)
    {
        g_hash_table_insert(currency_hash, currency, price_list);
    }
    else
    {
        g_hash_table_remove(currency_hash, currency);

        if (cleanup)
        {
            /* chances are good that this commodity had only one currency.
             * If there are no currencies, we may as well destroy the
             * commodity too. */
            guint num_currencies = g_hash_table_size (currency_hash);
            if (0 == num_currencies)
            {
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
    char datebuff[MAX_DATE_LENGTH + 1];
    memset(datebuff, 0, sizeof(datebuff));
    if (!db || !p) return FALSE;
    ENTER ("db=%p, pr=%p dirty=%d destroying=%d",
           db, p, qof_instance_get_dirty_flag(p),
           qof_instance_get_destroying(p));

    gnc_price_ref(p);
    qof_print_date_buff(datebuff, sizeof(datebuff), gnc_price_get_time64 (p));
    DEBUG("Remove Date is %s, Commodity is %s, Source is %s", datebuff,
          gnc_commodity_get_fullname (gnc_price_get_commodity (p)),
          gnc_price_get_source_string (p));

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

typedef struct
{
    GNCPriceDB *db;
    time64 cutoff;
    gboolean delete_fq;
    gboolean delete_user;
    gboolean delete_app;
    GSList *list;
} remove_info;

static gboolean
check_one_price_date (GNCPrice *price, gpointer user_data)
{
    remove_info *data = user_data;
    PriceSource source;
    time64 time;

    ENTER("price %p (%s), data %p", price,
          gnc_commodity_get_mnemonic(gnc_price_get_commodity(price)),
          user_data);

    source = gnc_price_get_source (price);

    if ((source == PRICE_SOURCE_FQ) && data->delete_fq)
        PINFO ("Delete Quote Source");
    else if ((source == PRICE_SOURCE_USER_PRICE) && data->delete_user)
        PINFO ("Delete User Source");
    else if ((source != PRICE_SOURCE_FQ) && (source != PRICE_SOURCE_USER_PRICE) && data->delete_app)
        PINFO ("Delete App Source");
    else
    {
        LEAVE("Not a matching source");
        return TRUE;
    }

    time = gnc_price_get_time64 (price);
    {
        gchar buf[40];
        gnc_time64_to_iso8601_buff(time, buf);
        DEBUG("checking date %s", buf);
    }
    if (time < data->cutoff)
    {
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

    /* now check each item in the list */
    g_list_foreach(node, (GFunc)check_one_price_date, data);

    LEAVE(" ");
}

static gint
compare_prices_by_commodity_date (gconstpointer a, gconstpointer b)
{
    time64 time_a, time_b;
    gnc_commodity *comma;
    gnc_commodity *commb;
    gnc_commodity *curra;
    gnc_commodity *currb;
    gint result;

    if (!a && !b) return 0;
    /* nothing is always less than something */
    if (!a) return -1;
    if (!b) return 1;

    comma = gnc_price_get_commodity ((GNCPrice *) a);
    commb = gnc_price_get_commodity ((GNCPrice *) b);

    if (!gnc_commodity_equal(comma, commb))
        return gnc_commodity_compare(comma, commb);

    curra = gnc_price_get_currency ((GNCPrice *) a);
    currb = gnc_price_get_currency ((GNCPrice *) b);

    if (!gnc_commodity_equal(curra, currb))
        return gnc_commodity_compare(curra, currb);

    time_a = gnc_price_get_time64((GNCPrice *) a);
    time_b = gnc_price_get_time64((GNCPrice *) b);

    /* Note we return -1 if time_b is before time_a. */
    result = time64_cmp(time_b, time_a);
    if (result) return result;

    /* For a stable sort */
    return guid_compare (gnc_price_get_guid((GNCPrice *) a),
                         gnc_price_get_guid((GNCPrice *) b));
}

static gboolean
price_commodity_and_currency_equal (GNCPrice *a, GNCPrice *b)
{
    gboolean ret_comm = FALSE;
    gboolean ret_curr = FALSE;

    if (gnc_commodity_equal (gnc_price_get_commodity(a), gnc_price_get_commodity (b)))
        ret_comm = TRUE;

    if (gnc_commodity_equal (gnc_price_get_currency(a), gnc_price_get_currency (b)))
        ret_curr = TRUE;

    return (ret_comm && ret_curr);
}

static void
gnc_pricedb_remove_old_prices_pinfo (GNCPrice *price, gboolean keep_message)
{
    GDate price_date = time64_to_gdate (gnc_price_get_time64 (price));
    char date_buf[MAX_DATE_LENGTH+1];

    if (g_date_valid (&price_date))
    {
        qof_print_gdate (date_buf, MAX_DATE_LENGTH, &price_date);

        if (keep_message)
        {
            PINFO("#### Keep price with date %s, commodity is %s, currency is %s", date_buf,
                     gnc_commodity_get_printname(gnc_price_get_commodity(price)),
                     gnc_commodity_get_printname(gnc_price_get_currency(price)));
        }
        else
            PINFO("## Remove price with date %s", date_buf);
    }
    else
        PINFO("Keep price date is invalid");
}

static void
clone_price (GNCPrice **price, GNCPrice *source_price)
{
    QofBook *book;

    if (!source_price) return;
    if (price == NULL) return;

    book = qof_instance_get_book (QOF_INSTANCE(source_price));

    if (*price)
        gnc_price_unref (*price);

    *price = gnc_price_clone (source_price, book);

    gnc_pricedb_remove_old_prices_pinfo (source_price, TRUE);
}

static gint
roundUp (gint numToRound, gint multiple)
{
    gint remainder;

    if (multiple == 0)
        return numToRound;

    remainder = numToRound % multiple;
    if (remainder == 0)
        return numToRound;

    return numToRound + multiple - remainder;
}

static gint
get_fiscal_quarter (GDate *date, GDateMonth fiscal_start)
{
    GDateMonth month = g_date_get_month (date);

    gint q = ((roundUp (22 - fiscal_start + month, 3)/3) % 4) + 1;

    PINFO("Return fiscal quarter is %d", q);
    return q;
}

static void
gnc_pricedb_process_removal_list (GNCPriceDB *db, GDate *fiscal_end_date,
                                  remove_info data, PriceRemoveKeepOptions keep)
{
    GSList *item;
    gboolean save_first_price = FALSE;
    gint saved_test_value = 0, next_test_value = 0;
    GNCPrice *cloned_price = NULL;
    GDateMonth fiscal_month_start;
    GDate *tmp_date = g_date_new_dmy (g_date_get_day (fiscal_end_date),
                                      g_date_get_month (fiscal_end_date),
                                      g_date_get_year (fiscal_end_date));

    // get the fiscal start month
    g_date_subtract_months (tmp_date, 12);
    fiscal_month_start = g_date_get_month (tmp_date) + 1;
    g_date_free (tmp_date);

    // sort the list by commodity / currency / date
    data.list = g_slist_sort (data.list, compare_prices_by_commodity_date);

    /* Now run this external list deleting prices */
    for (item = data.list; item; item = g_slist_next(item))
    {
        GDate saved_price_date;
        GDate next_price_date;

        // Keep None
        if (keep == PRICE_REMOVE_KEEP_NONE)
        {
            gnc_pricedb_remove_old_prices_pinfo (item->data, FALSE);
            gnc_pricedb_remove_price (db, item->data);
            continue;
        }

        save_first_price = !price_commodity_and_currency_equal (item->data, cloned_price); // Not Equal
        if (save_first_price == TRUE)
        {
            clone_price (&cloned_price, item->data);
            continue;
        }

        // get the price dates
        saved_price_date = time64_to_gdate (gnc_price_get_time64 (cloned_price));
        next_price_date = time64_to_gdate (gnc_price_get_time64 (item->data));

        // Keep last price in fiscal year
        if (keep == PRICE_REMOVE_KEEP_LAST_PERIOD && save_first_price == FALSE)
        {
            GDate *saved_fiscal_end = g_date_new_dmy (g_date_get_day (&saved_price_date),
                                                      g_date_get_month (&saved_price_date),
                                                      g_date_get_year (&saved_price_date));

            GDate *next_fiscal_end = g_date_new_dmy (g_date_get_day (&next_price_date),
                                                     g_date_get_month (&next_price_date),
                                                     g_date_get_year (&next_price_date));

            gnc_gdate_set_fiscal_year_end (saved_fiscal_end, fiscal_end_date);
            gnc_gdate_set_fiscal_year_end (next_fiscal_end, fiscal_end_date);

            saved_test_value = g_date_get_year (saved_fiscal_end);
            next_test_value = g_date_get_year (next_fiscal_end);

            PINFO("Keep last price in fiscal year");

            g_date_free (saved_fiscal_end);
            g_date_free (next_fiscal_end);
        }

        // Keep last price in fiscal quarter
        if (keep == PRICE_REMOVE_KEEP_LAST_QUARTERLY && save_first_price == FALSE)
        {
            saved_test_value = get_fiscal_quarter (&saved_price_date, fiscal_month_start);
            next_test_value = get_fiscal_quarter (&next_price_date, fiscal_month_start);

            PINFO("Keep last price in fiscal quarter");
        }

        // Keep last price of every month
        if (keep == PRICE_REMOVE_KEEP_LAST_MONTHLY && save_first_price == FALSE)
        {
            saved_test_value = g_date_get_month (&saved_price_date);
            next_test_value = g_date_get_month (&next_price_date);

            PINFO("Keep last price of every month");
        }

        // Keep last price of every week
        if (keep == PRICE_REMOVE_KEEP_LAST_WEEKLY && save_first_price == FALSE)
        {
            saved_test_value = g_date_get_iso8601_week_of_year (&saved_price_date);
            next_test_value = g_date_get_iso8601_week_of_year (&next_price_date);

            PINFO("Keep last price of every week");
        }

        // Now compare the values
        if (saved_test_value == next_test_value)
        {
            gnc_pricedb_remove_old_prices_pinfo (item->data, FALSE);
            gnc_pricedb_remove_price (db, item->data);
        }
        else
            clone_price (&cloned_price, item->data);
    }
    if (cloned_price)
        gnc_price_unref (cloned_price);
}

gboolean
gnc_pricedb_remove_old_prices (GNCPriceDB *db, GList *comm_list,
                              GDate *fiscal_end_date, time64 cutoff,
                              PriceRemoveSourceFlags source,
                              PriceRemoveKeepOptions keep)
{
    remove_info data;
    GList *node;
    char datebuff[MAX_DATE_LENGTH + 1];
    memset (datebuff, 0, sizeof(datebuff));

    data.db = db;
    data.cutoff = cutoff;
    data.list = NULL;
    data.delete_fq = FALSE;
    data.delete_user = FALSE;
    data.delete_app = FALSE;

    ENTER("Remove Prices for Source %d, keeping %d", source, keep);

    // setup the source options
    if (source & PRICE_REMOVE_SOURCE_APP)
        data.delete_app = TRUE;

    if (source & PRICE_REMOVE_SOURCE_FQ)
        data.delete_fq = TRUE;

    if (source & PRICE_REMOVE_SOURCE_USER)
        data.delete_user = TRUE;

    // Walk the list of commodities
    for (node = g_list_first (comm_list); node; node = g_list_next (node))
    {
        GHashTable *currencies_hash = g_hash_table_lookup (db->commodity_hash, node->data);
        g_hash_table_foreach (currencies_hash, pricedb_remove_foreach_pricelist, &data);
    }

    if (data.list == NULL)
    {
        LEAVE("Empty price list");
        return FALSE;
    }
    qof_print_date_buff (datebuff, sizeof(datebuff), cutoff);
    DEBUG("Number of Prices in list is %d, Cutoff date is %s",
          g_slist_length (data.list), datebuff);

    // Check for a valid fiscal end of year date
    if (fiscal_end_date == NULL)
    {
        GDateYear year_now = g_date_get_year (gnc_g_date_new_today ());
        fiscal_end_date = g_date_new ();
        g_date_set_dmy (fiscal_end_date, 31, 12, year_now);
    }
    else if (g_date_valid (fiscal_end_date) == FALSE)
    {
        GDateYear year_now = g_date_get_year (gnc_g_date_new_today ());
        g_date_clear (fiscal_end_date, 1);
        g_date_set_dmy (fiscal_end_date, 31, 12, year_now);
    }
    gnc_pricedb_process_removal_list (db, fiscal_end_date, data, keep);

    g_slist_free (data.list);
    LEAVE(" ");
    return TRUE;
}

/* ==================================================================== */
/* lookup/query functions */

static PriceList *pricedb_price_list_merge (PriceList *a, PriceList *b);

static void
hash_values_helper(gpointer key, gpointer value, gpointer data)
{
    GList ** l = data;
    if (*l)
    {
        GList *new_l;
        new_l = pricedb_price_list_merge(*l, value);
        g_list_free (*l);
        *l = new_l;
    }
    else
        *l = g_list_copy (value);
}

static PriceList *
price_list_from_hashtable (GHashTable *hash, const gnc_commodity *currency)
{
    GList *price_list = NULL, *result = NULL ;
    if (currency)
    {
        price_list = g_hash_table_lookup(hash, currency);
        if (!price_list)
        {
            LEAVE (" no price list");
            return NULL;
        }
        result = g_list_copy (price_list);
    }
    else
    {
        g_hash_table_foreach(hash, hash_values_helper, (gpointer)&result);
    }
    return result;
}

static PriceList *
pricedb_price_list_merge (PriceList *a, PriceList *b)
{
    PriceList *merged_list = NULL;
    GList *next_a = a;
    GList *next_b = b;

    while (next_a || next_b)
    {
        if (next_a == NULL)
        {
            merged_list = g_list_prepend (merged_list, next_b->data);
            next_b = next_b->next;
        }
        else if (next_b == NULL)
        {
            merged_list = g_list_prepend (merged_list, next_a->data);
            next_a = next_a->next;
        }
        /* We're building the list in reverse order so reverse the comparison. */
        else if (compare_prices_by_date (next_a->data, next_b->data) < 0)
        {
            merged_list = g_list_prepend (merged_list, next_a->data);
            next_a = next_a->next;
        }
        else
        {
            merged_list = g_list_prepend (merged_list, next_b->data);
            next_b = next_b->next;
        }
    }
    return g_list_reverse (merged_list);
}

static PriceList*
pricedb_get_prices_internal(GNCPriceDB *db, const gnc_commodity *commodity,
                            const gnc_commodity *currency, gboolean bidi)
{
    GHashTable *forward_hash = NULL, *reverse_hash = NULL;
    PriceList *forward_list = NULL, *reverse_list = NULL;
    g_return_val_if_fail (db != NULL, NULL);
    g_return_val_if_fail (commodity != NULL, NULL);
    forward_hash = g_hash_table_lookup(db->commodity_hash, commodity);
    if (currency && bidi)
        reverse_hash = g_hash_table_lookup(db->commodity_hash, currency);
    if (!forward_hash && !reverse_hash)
    {
        LEAVE (" no currency hash");
        return NULL;
    }
    if (forward_hash)
        forward_list = price_list_from_hashtable (forward_hash, currency);
    if (currency && reverse_hash)
    {
        reverse_list = price_list_from_hashtable (reverse_hash, commodity);
        if (reverse_list)
        {
            if (forward_list)
            {
                /* Since we have a currency both lists are a direct copy of a price
                   list in the price DB.  This means the lists are already sorted
                   from newest to oldest and we can just merge them together.  This
                   is substantially faster than concatenating them and sorting the
                   resulting list. */
                PriceList *merged_list;
                merged_list = pricedb_price_list_merge (forward_list, reverse_list);
                g_list_free (forward_list);
                g_list_free (reverse_list);
                forward_list = merged_list;
            }
            else
            {
                forward_list = reverse_list;
            }
        }
    }

    return forward_list;
}

GNCPrice *gnc_pricedb_lookup_latest(GNCPriceDB *db,
                          const gnc_commodity *commodity,
                          const gnc_commodity *currency)
{
    GList *price_list;
    GNCPrice *result;

    if (!db || !commodity || !currency) return NULL;
    ENTER ("db=%p commodity=%p currency=%p", db, commodity, currency);

    price_list = pricedb_get_prices_internal(db, commodity, currency, TRUE);
    if (!price_list) return NULL;
    /* This works magically because prices are inserted in date-sorted
     * order, and the latest date always comes first. So return the
     * first in the list.  */
    result = price_list->data;
    gnc_price_ref(result);
    g_list_free (price_list);
    LEAVE("price is %p", result);
    return result;
}

typedef struct
{
    GList **list;
    const gnc_commodity *com;
    time64 t;
} UsesCommodity;

/* price_list_scan_any_currency is the helper function used with
 * pricedb_pricelist_traversal by the "any_currency" price lookup functions. It
 * builds a list of prices that are either to or from the commodity "com".
 * The resulting list will include the last price newer than "t" and the first
 * price older than "t".  All other prices will be ignored.  Since in the most
 * common cases we will be looking for recent prices which are at the front of
 * the various price lists, this is considerably faster than concatenating all
 * the relevant price lists and sorting the result.
*/

static gboolean
price_list_scan_any_currency(GList *price_list, gpointer data)
{
    UsesCommodity *helper = (UsesCommodity*)data;
    GList *node = price_list;
    gnc_commodity *com;
    gnc_commodity *cur;

    if (!price_list)
        return TRUE;

    com = gnc_price_get_commodity(node->data);
    cur = gnc_price_get_currency(node->data);

    /* if this price list isn't for the commodity we are interested in,
       ignore it. */
    if (com != helper->com && cur != helper->com)
        return TRUE;

    /* The price list is sorted in decreasing order of time.  Find the first
       price on it that is older than the requested time and add it and the
       previous price to the result list. */
    while (node != NULL)
    {
        GNCPrice *price = node->data;
        time64 price_t = gnc_price_get_time64(price);
        if (price_t < helper->t)
        {
            /* If there is a previous price add it to the results. */
            if (node->prev)
            {
                GNCPrice *prev_price = node->prev->data;
                gnc_price_ref(prev_price);
                *helper->list = g_list_prepend(*helper->list, prev_price);
            }
            /* Add the first price before the desired time */
            gnc_price_ref(price);
            *helper->list = g_list_prepend(*helper->list, price);
            /* No point in looking further, they will all be older */
            break;
        }
        else if (node->next == NULL)
        {
            /* The last price is later than given time, add it */
            gnc_price_ref(price);
            *helper->list = g_list_prepend(*helper->list, price);
        }
        node = node->next;
    }

    return TRUE;
}

static gboolean
is_in_list (GList *list, const gnc_commodity *c)
{
    GList *node;
    for (node = list; node != NULL; node = g_list_next(node))
        if ((gnc_commodity*)node->data == c)
            return TRUE;
    return FALSE;
}

/* This operates on the principal that the prices are sorted by date and that we
 * want only the first one before the specified time containing both the target
 * and some other commodity. */
static PriceList*
latest_before (PriceList *prices, const gnc_commodity* target, time64 t)
{
    GList *node, *found_coms = NULL, *retval = NULL;
    for (node = prices; node != NULL; node = g_list_next(node))
    {
        GNCPrice *price = (GNCPrice*)node->data;
        gnc_commodity *com = gnc_price_get_commodity(price);
        gnc_commodity *cur = gnc_price_get_currency(price);
        time64 price_t = gnc_price_get_time64(price);
        if (t < price_t ||
            (com == target && is_in_list(found_coms, cur)) ||
            (cur == target && is_in_list(found_coms, com)))
            continue;
        else
        {
            gnc_price_ref(price);
            retval = g_list_prepend(retval, price);
            found_coms = g_list_prepend(found_coms, com == target ? cur : com);
        }
    }
    return g_list_reverse(retval);
}

static GNCPrice**
find_comtime(GPtrArray* array, gnc_commodity *com)
{
    unsigned int index = 0;
    GNCPrice** retval = NULL;
    for (index = 0; index < array->len; ++index)
    {
        GNCPrice **price_p = g_ptr_array_index(array, index);
        if (gnc_price_get_commodity(*price_p) == com ||
            gnc_price_get_currency(*price_p) == com)
            retval = price_p;
    }
    return retval;
}

static GList*
add_nearest_price(GList *target_list, GPtrArray *price_array, GNCPrice *price,
                  const gnc_commodity *target, time64 t)
{
        gnc_commodity *com = gnc_price_get_commodity(price);
        gnc_commodity *cur = gnc_price_get_currency(price);
        time64 price_t = gnc_price_get_time64(price);
        gnc_commodity *other = com == target ? cur : com;
        GNCPrice **com_price = find_comtime(price_array, other);
        time64 com_t;
        if (com_price == NULL)
        {
            com_price = (GNCPrice**)g_slice_new(gpointer);
            *com_price = price;
            g_ptr_array_add(price_array, com_price);
            /* If the first price we see for this commodity is not newer than
               the target date add it to the return list. */
            if (price_t <= t)
            {
                gnc_price_ref(price);
                target_list = g_list_prepend(target_list, price);
            }
            return target_list;
        }
        com_t = gnc_price_get_time64(*com_price);
        if (com_t <= t)
       /* No point in checking any more prices, they'll all be further from
        * t. */
            return target_list;
        if (price_t > t)
        /* The price list is sorted newest->oldest, so as long as this price
         * is newer than t then it should replace the saved one. */
        {
            *com_price = price;
        }
        else
        {
            time64 com_diff = com_t - t;
            time64 price_diff = t - price_t;
            if (com_diff < price_diff)
            {
                gnc_price_ref(*com_price);
                target_list = g_list_prepend(target_list, *com_price);
            }
            else
            {
                gnc_price_ref(price);
                target_list = g_list_prepend(target_list, price);
            }
            *com_price = price;
        }
        return target_list;
}

static PriceList *
nearest_to (PriceList *prices, const gnc_commodity* target, time64 t)
{
    GList *node, *retval = NULL;
    const guint prealloc_size = 5; /*More than 5 "other" is unlikely as long as
                                    * target isn't the book's default
                                    * currency. */


    GPtrArray *price_array = g_ptr_array_sized_new(prealloc_size);
    guint index;
    for (node = prices; node != NULL; node = g_list_next(node))
    {
        GNCPrice *price = (GNCPrice*)node->data;
        retval = add_nearest_price(retval, price_array, price, target, t);
    }
    /* There might be some prices in price_array that are newer than t. Those
     * will be cases where there wasn't a price older than t to push one or the
     * other into the retval, so we need to get them now.
     */
    for (index = 0; index < price_array->len; ++index)
    {
        GNCPrice **com_price = g_ptr_array_index(price_array, index);
        time64 price_t = gnc_price_get_time64(*com_price);
        if (price_t >= t)
        {
            gnc_price_ref(*com_price);
            retval = g_list_prepend(retval, *com_price);
        }
    }
    g_ptr_array_free(price_array, TRUE);
    return g_list_sort(retval, compare_prices_by_date);
}



PriceList *
gnc_pricedb_lookup_latest_any_currency(GNCPriceDB *db,
                                       const gnc_commodity *commodity)
{
    return gnc_pricedb_lookup_latest_before_any_currency_t64(db, commodity,
                                                         gnc_time(NULL));
}

PriceList *
gnc_pricedb_lookup_nearest_in_time_any_currency_t64(GNCPriceDB *db,
                                                    const gnc_commodity *commodity,
                                                    time64 t)
{
    GList *prices = NULL, *result;
    UsesCommodity helper = {&prices, commodity, t};
    result = NULL;

    if (!db || !commodity) return NULL;
    ENTER ("db=%p commodity=%p", db, commodity);

    pricedb_pricelist_traversal(db, price_list_scan_any_currency, &helper);
    prices = g_list_sort(prices, compare_prices_by_date);
    result = nearest_to(prices, commodity, t);
    gnc_price_list_destroy(prices);
    LEAVE(" ");
    return result;
}

PriceList *
gnc_pricedb_lookup_latest_before_any_currency_t64(GNCPriceDB *db,
                                                  const gnc_commodity *commodity,
                                                  time64 t)
{
    GList *prices = NULL, *result;
    UsesCommodity helper = {&prices, commodity, t};
    result = NULL;

    if (!db || !commodity) return NULL;
    ENTER ("db=%p commodity=%p", db, commodity);

    pricedb_pricelist_traversal(db, price_list_scan_any_currency,
                                       &helper);
    prices = g_list_sort(prices, compare_prices_by_date);
    result = latest_before(prices, commodity, t);
    gnc_price_list_destroy(prices);
    LEAVE(" ");
    return result;
}

/* gnc_pricedb_has_prices is used explicitly for filtering cases where the
 * commodity is the left side of commodity->currency price, so it checks only in
 * that direction.
 */
gboolean
gnc_pricedb_has_prices(GNCPriceDB *db,
                       const gnc_commodity *commodity,
                       const gnc_commodity *currency)
{
    GList *price_list;
    GHashTable *currency_hash;
    gint size;

    if (!db || !commodity) return FALSE;
    ENTER ("db=%p commodity=%p currency=%p", db, commodity, currency);
    currency_hash = g_hash_table_lookup(db->commodity_hash, commodity);
    if (!currency_hash)
    {
        LEAVE("no, no currency_hash table");
        return FALSE;
    }

    if (currency)
    {
        price_list = g_hash_table_lookup(currency_hash, currency);
        if (price_list)
        {
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


/* gnc_pricedb_get_prices is used to construct the tree in the Price Editor and
 * so needs to be single-direction.
 */
PriceList *
gnc_pricedb_get_prices(GNCPriceDB *db,
                       const gnc_commodity *commodity,
                       const gnc_commodity *currency)
{
    GList *result;
    GList *node;


    if (!db || !commodity) return NULL;
    ENTER ("db=%p commodity=%p currency=%p", db, commodity, currency);
    result = pricedb_get_prices_internal (db, commodity, currency, FALSE);
    if (!result) return NULL;
    for (node = result; node; node = node->next)
        gnc_price_ref (node->data);

    LEAVE (" ");
    return result;
}

/* Return the number of prices in the data base for the given commodity
 */
static void
price_count_helper(gpointer key, gpointer value, gpointer data)
{
    int *result = data;
    GList *price_list = value;

    *result += g_list_length(price_list);
}

int
gnc_pricedb_num_prices(GNCPriceDB *db,
                       const gnc_commodity *c)
{
    int result = 0;
    GHashTable *currency_hash;

    if (!db || !c) return 0;
    ENTER ("db=%p commodity=%p", db, c);

    currency_hash = g_hash_table_lookup(db->commodity_hash, c);
    if (currency_hash)
    {
        g_hash_table_foreach(currency_hash, price_count_helper,  (gpointer)&result);
    }

    LEAVE ("count=%d", result);
    return result;
}

/* Helper function for combining the price lists in gnc_pricedb_nth_price. */
static void
list_combine (gpointer element, gpointer data)
{
    GList *list = *(GList**)data;
    if (list == NULL)
        *(GList**)data = g_list_copy (element);
    else
    {
        GList *new_list = g_list_concat ((GList *)list, g_list_copy (element));
        *(GList**)data = new_list;
    }
}

/* This function is used by gnc-tree-model-price.c for iterating through the
 * prices when building or filtering the pricedb dialog's
 * GtkTreeView. gtk-tree-view-price.c sorts the results after it has obtained
 * the values so there's nothing gained by sorting. However, for very large
 * collections of prices in multiple currencies (here commodity is the one being
 * priced and currency the one in which the price is denominated; note that they
 * may both be currencies or not) just concatenating the price lists together
 * can be expensive because the recieving list must be traversed to obtain its
 * end. To avoid that cost n times we cache the commodity and merged price list.
 * Since this is a GUI-driven function there is no concern about concurrency.
 */

GNCPrice *
gnc_pricedb_nth_price (GNCPriceDB *db,
                       const gnc_commodity *c,
                       const int n)
{
    static const gnc_commodity *last_c = NULL;
    static GList *prices = NULL;

    GNCPrice *result = NULL;
    GHashTable *currency_hash;
    g_return_val_if_fail (GNC_IS_COMMODITY (c), NULL);

    if (!db || !c || n < 0) return NULL;
    ENTER ("db=%p commodity=%s index=%d", db, gnc_commodity_get_mnemonic(c), n);

    if (last_c && prices && last_c == c && db->reset_nth_price_cache == FALSE)
    {
        result = g_list_nth_data (prices, n);
        LEAVE ("price=%p", result);
        return result;
    }

    last_c = c;

    if (prices)
    {
        g_list_free (prices);
        prices = NULL;
    }

    db->reset_nth_price_cache = FALSE;

    currency_hash = g_hash_table_lookup (db->commodity_hash, c);
    if (currency_hash)
    {
        GList *currencies = g_hash_table_get_values (currency_hash);
        g_list_foreach (currencies, list_combine, &prices);
        result = g_list_nth_data (prices, n);
        g_list_free (currencies);
    }

    LEAVE ("price=%p", result);
    return result;
}

void
gnc_pricedb_nth_price_reset_cache (GNCPriceDB *db)
{
    if (db)
        db->reset_nth_price_cache = TRUE;
}

GNCPrice *
gnc_pricedb_lookup_day_t64(GNCPriceDB *db,
                           const gnc_commodity *c,
                           const gnc_commodity *currency,
                           time64 t)
{
    return lookup_nearest_in_time(db, c, currency, t, TRUE);
}

GNCPrice *
gnc_pricedb_lookup_at_time64(GNCPriceDB *db,
                             const gnc_commodity *c,
                             const gnc_commodity *currency,
                             time64 t)
{
    GList *price_list;
    GList *item = NULL;

    if (!db || !c || !currency) return NULL;
    ENTER ("db=%p commodity=%p currency=%p", db, c, currency);
    price_list = pricedb_get_prices_internal (db, c, currency, TRUE);
    item = price_list;
    while (item)
    {
        GNCPrice *p = item->data;
        time64 price_time = gnc_price_get_time64(p);
        if (price_time == t)
        {
            gnc_price_ref(p);
            g_list_free (price_list);
            LEAVE("price is %p", p);
            return p;
        }
        item = item->next;
    }
    g_list_free (price_list);
    LEAVE (" ");
    return NULL;
}

static GNCPrice *
lookup_nearest_in_time(GNCPriceDB *db,
                       const gnc_commodity *c,
                       const gnc_commodity *currency,
                       time64 t,
                       gboolean sameday)
{
    GList *price_list;
    GNCPrice *current_price = NULL;
    GNCPrice *next_price = NULL;
    GNCPrice *result = NULL;
    GList *item = NULL;

    if (!db || !c || !currency) return NULL;
    if (t == INT64_MAX) return NULL;
    ENTER ("db=%p commodity=%p currency=%p", db, c, currency);
    price_list = pricedb_get_prices_internal (db, c, currency, TRUE);
    if (!price_list) return NULL;
    item = price_list;

    /* default answer */
    current_price = item->data;

    /* find the first candidate past the one we want.  Remember that
       prices are in most-recent-first order. */
    while (!next_price && item)
    {
        GNCPrice *p = item->data;
        time64 price_time = gnc_price_get_time64(p);
        if (price_time <= t)
        {
            next_price = item->data;
            break;
        }
        current_price = item->data;
        item = item->next;
    }

    if (current_price)      /* How can this be null??? */
    {
        if (!next_price)
        {
            /* It's earlier than the last price on the list */
            result = current_price;
            if (sameday)
            {
                /* Must be on the same day. */
                time64 price_day;
                time64 t_day;
                price_day = time64CanonicalDayTime(gnc_price_get_time64(current_price));
                t_day = time64CanonicalDayTime(t);
                if (price_day != t_day)
                    result = NULL;
            }
        }
        else
        {
            /* If the requested time is not earlier than the first price on the
               list, then current_price and next_price will be the same. */
            time64 current_t = gnc_price_get_time64(current_price);
            time64 next_t = gnc_price_get_time64(next_price);
            time64 diff_current = current_t - t;
            time64 diff_next = next_t - t;
            time64 abs_current = llabs(diff_current);
            time64 abs_next = llabs(diff_next);

            if (sameday)
            {
                /* Result must be on same day, see if either of the two isn't */
                time64 t_day = time64CanonicalDayTime(t);
                time64 current_day = time64CanonicalDayTime(current_t);
                time64 next_day = time64CanonicalDayTime(next_t);
                if (current_day == t_day)
                {
                    if (next_day == t_day)
                    {
                        /* Both on same day, return nearest */
                        if (abs_current < abs_next)
                            result = current_price;
                        else
                            result = next_price;
                    }
                    else
                        /* current_price on same day, next_price not */
                        result = current_price;
                }
                else if (next_day == t_day)
                    /* next_price on same day, current_price not */
                    result = next_price;
            }
            else
            {
                /* Choose the price that is closest to the given time. In case of
                 * a tie, prefer the older price since it actually existed at the
                 * time. (This also fixes bug #541970.) */
                if (abs_current < abs_next)
                {
                    result = current_price;
                }
                else
                {
                    result = next_price;
                }
            }
        }
    }

    gnc_price_ref(result);
    g_list_free (price_list);
    LEAVE (" ");
    return result;
}

GNCPrice *
gnc_pricedb_lookup_nearest_in_time64(GNCPriceDB *db,
                                     const gnc_commodity *c,
                                     const gnc_commodity *currency,
                                     time64 t)
{
    return lookup_nearest_in_time(db, c, currency, t, FALSE);
}


GNCPrice *
gnc_pricedb_lookup_latest_before_t64 (GNCPriceDB *db,
                                      gnc_commodity *c,
                                      gnc_commodity *currency,
                                      time64 t)
{
    GList *price_list;
    GNCPrice *current_price = NULL;
    /*  GNCPrice *next_price = NULL;
        GNCPrice *result = NULL;*/
    GList *item = NULL;
    time64 price_time;

    if (!db || !c || !currency) return NULL;
    ENTER ("db=%p commodity=%p currency=%p", db, c, currency);
    price_list = pricedb_get_prices_internal (db, c, currency, TRUE);
    if (!price_list) return NULL;
    item = price_list;
    do
    {
        price_time = gnc_price_get_time64 (item->data);
        if (price_time <= t)
            current_price = item->data;
        item = item->next;
    }
    while (price_time > t && item);
    gnc_price_ref(current_price);
    g_list_free (price_list);
    LEAVE (" ");
    return current_price;
}

static gnc_numeric
direct_balance_conversion (GNCPriceDB *db, gnc_numeric bal,
                           const gnc_commodity *from, const gnc_commodity *to,
                           time64 t)
{
    GNCPrice *price;
    gnc_numeric retval = gnc_numeric_zero();
    if (from == NULL || to == NULL)
        return retval;
    if (gnc_numeric_zero_p(bal))
        return retval;
    if (t != INT64_MAX)
        price = gnc_pricedb_lookup_nearest_in_time64(db, from, to, t);
    else
        price = gnc_pricedb_lookup_latest(db, from, to);
    if (price == NULL)
        return retval;
    if (gnc_price_get_commodity(price) == from)
        retval = gnc_numeric_mul (bal, gnc_price_get_value (price),
                                  gnc_commodity_get_fraction (to),
                                  GNC_HOW_RND_ROUND);
    else
        retval = gnc_numeric_div (bal, gnc_price_get_value (price),
                                  gnc_commodity_get_fraction (to),
                                  GNC_HOW_RND_ROUND);
    gnc_price_unref (price);
    return retval;

}

typedef struct
{
    GNCPrice *from;
    GNCPrice *to;
} PriceTuple;

static PriceTuple
extract_common_prices (PriceList *from_prices, PriceList *to_prices,
                       const gnc_commodity *from, const gnc_commodity *to)
{
    PriceTuple retval = {NULL, NULL};
    GList *from_node = NULL, *to_node = NULL;
    GNCPrice *from_price, *to_price;

    for (from_node = from_prices; from_node != NULL;
         from_node = g_list_next(from_node))
    {
        for (to_node = to_prices; to_node != NULL;
             to_node = g_list_next(to_node))
        {
            gnc_commodity *to_com, *to_cur;
            gnc_commodity *from_com, *from_cur;
            to_price = GNC_PRICE(to_node->data);
            from_price = GNC_PRICE(from_node->data);
            to_com = gnc_price_get_commodity (to_price);
            to_cur = gnc_price_get_currency (to_price);
            from_com = gnc_price_get_commodity (from_price);
            from_cur = gnc_price_get_currency (from_price);
            if (((to_com == from_com || to_com == from_cur) &&
                 (to_com != from && to_com != to)) ||
                ((to_cur == from_com || to_cur == from_cur) &&
                 (to_cur != from && to_cur != to)))
                break;
            to_price = NULL;
            from_price = NULL;
        }
        if (to_price != NULL && from_price != NULL)
            break;
    }
    if (from_price == NULL || to_price == NULL)
        return retval;
    gnc_price_ref(from_price);
    gnc_price_ref(to_price);
    retval.from = from_price;
    retval.to = to_price;
    return retval;
}

static gnc_numeric
convert_balance(gnc_numeric bal, const gnc_commodity *from,
                const gnc_commodity *to, PriceTuple tuple)
{
    gnc_commodity *from_com = gnc_price_get_commodity(tuple.from);
    gnc_commodity *from_cur = gnc_price_get_currency(tuple.from);
    gnc_commodity *to_com = gnc_price_get_commodity(tuple.to);
    gnc_commodity *to_cur = gnc_price_get_currency(tuple.to);
    gnc_numeric from_val = gnc_price_get_value(tuple.from);
    gnc_numeric to_val = gnc_price_get_value(tuple.to);
    int fraction = gnc_commodity_get_fraction(to);

    int no_round = GNC_HOW_DENOM_EXACT | GNC_HOW_RND_NEVER;
    if (from_cur == from && to_cur == to)
        return gnc_numeric_div(gnc_numeric_mul(bal, to_val, GNC_DENOM_AUTO,
                                               no_round),
                               from_val, fraction, GNC_HOW_RND_ROUND);
    if (from_com == from && to_com == to)
        return gnc_numeric_div(gnc_numeric_mul(bal, from_val, GNC_DENOM_AUTO,
                                               no_round),
                               to_val, fraction, GNC_HOW_RND_ROUND);
    if (from_cur == from)
        return gnc_numeric_div(bal, gnc_numeric_mul(from_val, to_val,
                                                    GNC_DENOM_AUTO, no_round),
                               fraction, GNC_HOW_RND_ROUND);
    return gnc_numeric_mul(bal, gnc_numeric_mul(from_val, to_val,
                                                GNC_DENOM_AUTO, no_round),
                           fraction, GNC_HOW_RND_ROUND);

}
static gnc_numeric
indirect_balance_conversion (GNCPriceDB *db, gnc_numeric bal,
                             const gnc_commodity *from, const gnc_commodity *to,
                             time64 t )
{
    GList *from_prices = NULL, *to_prices = NULL;
    PriceTuple tuple;
    gnc_numeric zero = gnc_numeric_zero();
    if (from == NULL || to == NULL)
        return zero;
    if (gnc_numeric_zero_p(bal))
        return zero;
    if (t == INT64_MAX)
    {
        from_prices = gnc_pricedb_lookup_latest_any_currency(db, from);
        /* "to" is often the book currency which may have lots of prices,
            so avoid getting them if they aren't needed. */
        if (from_prices)
            to_prices = gnc_pricedb_lookup_latest_any_currency(db, to);
    }
    else
    {
        from_prices = gnc_pricedb_lookup_nearest_in_time_any_currency_t64(db,
                                                                      from, t);
        if (from_prices)
            to_prices = gnc_pricedb_lookup_nearest_in_time_any_currency_t64(db,
                                                                    to, t);
    }
    if (from_prices == NULL || to_prices == NULL)
        return zero;
    tuple = extract_common_prices(from_prices, to_prices, from, to);
    gnc_price_list_destroy(from_prices);
    gnc_price_list_destroy(to_prices);
    if (tuple.from)
        return convert_balance(bal, from, to, tuple);
    return zero;
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
    gnc_numeric new_value;

    if (gnc_numeric_zero_p (balance) ||
            gnc_commodity_equiv (balance_currency, new_currency))
        return balance;

    /* Look for a direct price. */
    new_value = direct_balance_conversion(pdb, balance, balance_currency,
                                          new_currency, INT64_MAX);
    if (!gnc_numeric_zero_p(new_value))
        return new_value;

    /*
     * no direct price found, try if we find a price in another currency
     * and convert in two stages
     */
    return indirect_balance_conversion(pdb, balance, balance_currency,
                                       new_currency, INT64_MAX);
}

gnc_numeric
gnc_pricedb_convert_balance_nearest_price_t64(GNCPriceDB *pdb,
                                              gnc_numeric balance,
                                              const gnc_commodity *balance_currency,
                                              const gnc_commodity *new_currency,
                                              time64 t)
{
    gnc_numeric new_value;

    if (gnc_numeric_zero_p (balance) ||
        gnc_commodity_equiv (balance_currency, new_currency))
        return balance;

    /* Look for a direct price. */
    new_value = direct_balance_conversion(pdb, balance, balance_currency,
                                          new_currency, t);
    if (!gnc_numeric_zero_p(new_value))
        return new_value;

    /*
     * no direct price found, try if we find a price in another currency
     * and convert in two stages
     */
    return indirect_balance_conversion(pdb, balance, balance_currency,
                                       new_currency, t);
}


/* ==================================================================== */
/* gnc_pricedb_foreach_price infrastructure
 */

typedef struct
{
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
    while (foreach_data->ok && node)
    {
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

    if (!db || !f) return FALSE;
    foreach_data.ok = TRUE;
    foreach_data.func = f;
    foreach_data.user_data = user_data;
    if (db->commodity_hash == NULL)
    {
        return FALSE;
    }
    g_hash_table_foreach(db->commodity_hash,
                         pricedb_foreach_currencies_hash,
                         &foreach_data);

    return foreach_data.ok;
}

/* foreach_pricelist */
typedef struct
{
    gboolean ok;
    gboolean (*func)(GList *p, gpointer user_data);
    gpointer user_data;
} GNCPriceListForeachData;

static void
pricedb_pricelist_foreach_pricelist(gpointer key, gpointer val, gpointer user_data)
{
    GList *price_list = (GList *) val;
    GNCPriceListForeachData *foreach_data = (GNCPriceListForeachData *) user_data;
    if (foreach_data->ok)
    {
        foreach_data->ok = foreach_data->func(price_list, foreach_data->user_data);
    }
}

static void
pricedb_pricelist_foreach_currencies_hash(gpointer key, gpointer val, gpointer user_data)
{
    GHashTable *currencies_hash = (GHashTable *) val;
    g_hash_table_foreach(currencies_hash, pricedb_pricelist_foreach_pricelist, user_data);
}

static gboolean
pricedb_pricelist_traversal(GNCPriceDB *db,
                         gboolean (*f)(GList *p, gpointer user_data),
                         gpointer user_data)
{
    GNCPriceListForeachData foreach_data;

    if (!db || !f) return FALSE;
    foreach_data.ok = TRUE;
    foreach_data.func = f;
    foreach_data.user_data = user_data;
    if (db->commodity_hash == NULL)
    {
        return FALSE;
    }
    g_hash_table_foreach(db->commodity_hash,
                         pricedb_pricelist_foreach_currencies_hash,
                         &foreach_data);

    return foreach_data.ok;
}

static gint
compare_hash_entries_by_commodity_key(gconstpointer a, gconstpointer b)
{
    HashEntry *he_a = (HashEntry *) a;
    HashEntry *he_b = (HashEntry *) b;
    gnc_commodity *ca;
    gnc_commodity *cb;
    int cmp_result;

    if (a == b) return 0;
    if (!a && !b) return 0;
    if (!a) return -1;
    if (!b) return 1;

    ca = (gnc_commodity *) he_a->key;
    cb = (gnc_commodity *) he_b->key;

    cmp_result = g_strcmp0(gnc_commodity_get_namespace(ca),
                           gnc_commodity_get_namespace(cb));

    if (cmp_result != 0) return cmp_result;

    return g_strcmp0(gnc_commodity_get_mnemonic(ca),
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

    if (!db || !f) return FALSE;

    currency_hashes = hash_table_to_list(db->commodity_hash);
    currency_hashes = g_slist_sort(currency_hashes,
                                   compare_hash_entries_by_commodity_key);

    for (i = currency_hashes; i; i = i->next)
    {
        HashEntry *entry = (HashEntry *) i->data;
        GHashTable *currency_hash = (GHashTable *) entry->value;
        GSList *price_lists = hash_table_to_list(currency_hash);
        GSList *j;

        price_lists = g_slist_sort(price_lists, compare_hash_entries_by_commodity_key);
        for (j = price_lists; j; j = j->next)
        {
            HashEntry *pricelist_entry = (HashEntry *) j->data;
            GList *price_list = (GList *) pricelist_entry->value;
            GList *node;

            for (node = (GList *) price_list; node; node = node->next)
            {
                GNCPrice *price = (GNCPrice *) node->data;

                /* stop traversal when f returns FALSE */
                if (FALSE == ok) break;
                if (!f(price, user_data)) ok = FALSE;
            }
        }
        if (price_lists)
        {
            g_slist_foreach(price_lists, hash_entry_free_gfunc, NULL);
            g_slist_free(price_lists);
            price_lists = NULL;
        }
    }

    if (currency_hashes)
    {
        g_slist_foreach(currency_hashes, hash_entry_free_gfunc, NULL);
        g_slist_free(currency_hashes);
    }
    return ok;
}

gboolean
gnc_pricedb_foreach_price(GNCPriceDB *db,
                          GncPriceForeachFunc f,
                          gpointer user_data,
                          gboolean stable_order)
{
    ENTER ("db=%p f=%p", db, f);
    if (stable_order)
    {
        LEAVE (" stable order found");
        return stable_price_traversal(db, f, user_data);
    }
    LEAVE (" use unstable order");
    return unstable_price_traversal(db, f, user_data);
}

/* ==================================================================== */
/* commodity substitution */

typedef struct
{
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
    if (gnc_commodity_equiv(price_c, fixup_data->old_c))
    {
        gnc_price_set_commodity (p, fixup_data->new_c);
    }
    price_c = gnc_price_get_currency(p);
    if (gnc_commodity_equiv(price_c, fixup_data->old_c))
    {
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

    if (!db || !old_c || !new_c) return;

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

    if (!p) return;
    if (!f) return;

    commodity = gnc_price_get_commodity(p);
    currency = gnc_price_get_currency(p);

    if (!commodity) return;
    if (!currency) return;

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
    str = source_names[gnc_price_get_source(p)];
    str = str ? str : "invalid";
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
    if (!db)
    {
        PERR("NULL PriceDB\n");
        return;
    }
    if (!f)
    {
        PERR("NULL FILE*\n");
        return;
    }

    fprintf(f, "<gnc:pricedb>\n");
    gnc_pricedb_foreach_price(db, print_pricedb_adapter, f, FALSE);
    fprintf(f, "</gnc:pricedb>\n");
}

/* ==================================================================== */
/* gncObject function implementation and registration */

static void
pricedb_book_begin (QofBook *book)
{
    gnc_pricedb_create(book);
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

    while (node)
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

    if (!db || !f) return;
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

#ifdef DUMP_FUNCTIONS
/* For debugging only, don't delete this */
static void price_list_dump(GList *price_list, const char *tag);
#endif

static const char *
price_printable(gpointer obj)
{
    GNCPrice *pr = obj;
    gnc_commodity *commodity;
    gnc_commodity *currency;
    static char buff[2048];  /* nasty static OK for printing */
    char *val, *da;

    if (!pr) return "";

#ifdef DUMP_FUNCTIONS
    /* Reference it so the compiler doesn't optimize it out. bit
       don't actually call it. */
    if (obj == buff)
        price_list_dump(NULL, "");
#endif

    val = gnc_numeric_to_string (pr->value);
    da = qof_print_date (pr->tmspec);

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

#ifdef DUMP_FUNCTIONS
/* For debugging only, don't delete this */
static void
price_list_dump(GList *price_list, const char *tag)
{
    GNCPrice *price;
    GList *node;
    printf("Price list %s\n", tag);
    for (node = price_list; node != NULL; node = node->next)
    {
        printf("%s\n", price_printable(node->data));
    }
}
#endif

#ifdef _MSC_VER
/* MSVC compiler doesn't have C99 "designated initializers"
 * so we wrap them in a macro that is empty on MSVC. */
# define DI(x) /* */
#else
# define DI(x) x
#endif
static QofObject price_object_def =
{
    DI(.interface_version = ) QOF_OBJECT_VERSION,
    DI(.e_type            = ) GNC_ID_PRICE,
    DI(.type_label        = ) "Price",
    DI(.create            = ) price_create,
    DI(.book_begin        = ) NULL,
    DI(.book_end          = ) NULL,
    DI(.is_dirty          = ) qof_collection_is_dirty,
    DI(.mark_clean        = ) qof_collection_mark_clean,
    DI(.foreach           = ) price_foreach,
    DI(.printable         = ) price_printable,
    DI(.version_cmp       = ) NULL,
};

static QofObject pricedb_object_def =
{
    DI(.interface_version = ) QOF_OBJECT_VERSION,
    DI(.e_type            = ) GNC_ID_PRICEDB,
    DI(.type_label        = ) "PriceDB",
    DI(.create            = ) NULL,
    DI(.book_begin        = ) pricedb_book_begin,
    DI(.book_end          = ) pricedb_book_end,
    DI(.is_dirty          = ) qof_collection_is_dirty,
    DI(.mark_clean        = ) qof_collection_mark_clean,
    DI(.foreach           = ) NULL,
    DI(.printable         = ) NULL,
    DI(.version_cmp       = ) NULL,
};

gboolean
gnc_pricedb_register (void)
{
    static QofParam params[] =
    {
        { PRICE_COMMODITY, GNC_ID_COMMODITY, (QofAccessFunc)gnc_price_get_commodity, (QofSetterFunc)gnc_price_set_commodity },
        { PRICE_CURRENCY, GNC_ID_COMMODITY, (QofAccessFunc)gnc_price_get_currency, (QofSetterFunc)gnc_price_set_currency },
        { PRICE_DATE, QOF_TYPE_DATE, (QofAccessFunc)gnc_price_get_time64, (QofSetterFunc)gnc_price_set_time64 },
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
