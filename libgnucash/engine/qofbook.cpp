/********************************************************************\
 * qofbook.c -- dataset access (set of books of entities)           *
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
\********************************************************************/

/*
 * FILE:
 * qofbook.cpp
 *
 * FUNCTION:
 * Encapsulate all the information about a QOF dataset.
 *
 * HISTORY:
 * Created by Linas Vepstas December 1998
 * Copyright (c) 1998-2001,2003 Linas Vepstas <linas@linas.org>
 * Copyright (c) 2000 Dave Peticolas
 * Copyright (c) 2007 David Hampton <hampton@employees.org>
 */

extern "C"
{

#include "config.h"

#include <stdlib.h>
#include <string.h>

#include <glib.h>
#ifdef GNC_PLATFORM_WINDOWS
  /* Mingw disables the standard type macros for C++ without this override. */
#define __STDC_FORMAT_MACROS = 1
#endif
#include <inttypes.h>

}

#include "qof.h"
#include "qofevent-p.h"
#include "qofbackend.h"
#include "qofbook-p.h"
#include "qofid-p.h"
#include "qofobject-p.h"
#include "qofbookslots.h"
#include "kvp-frame.hpp"

static QofLogModule log_module = QOF_MOD_ENGINE;
#define AB_KEY "hbci"
#define AB_TEMPLATES "template-list"

enum
{
    PROP_0,
//  PROP_ROOT_ACCOUNT,		/* Table */
//  PROP_ROOT_TEMPLATE,		/* Table */
/*   keep trading accounts property, while adding book-currency, default gains
     policy and default gains account properties, so that files prior to 2.7 can
     be read/processed; GUI changed to use all four properties as of 2.7.
     Trading accounts, on the one hand, and book-currency plus default-gains-
     policy, and optionally, default gains account, on the other, are mutually
     exclusive */
    PROP_OPT_TRADING_ACCOUNTS,	/* KVP */
/*   Book currency and default gains policy properties only apply if currency
     accounting method selected in GUI is 'book-currency'; both required and
     both are exclusive with trading accounts */
    PROP_OPT_BOOK_CURRENCY, 	/* KVP */
    PROP_OPT_DEFAULT_GAINS_POLICY, 	/* KVP */
/*   Default gains account property only applies if currency accounting method
     selected in GUI is 'book-currency'; its use is optional but exclusive with
     trading accounts */
    PROP_OPT_DEFAULT_GAINS_ACCOUNT_GUID, 	/* KVP */
    PROP_OPT_AUTO_READONLY_DAYS,/* KVP */
    PROP_OPT_NUM_FIELD_SOURCE,	/* KVP */
    PROP_OPT_DEFAULT_BUDGET,	/* KVP */
    PROP_OPT_FY_END,		/* KVP */
    PROP_AB_TEMPLATES,		/* KVP */
    N_PROPERTIES		/* Just a counter */
};

QOF_GOBJECT_GET_TYPE(QofBook, qof_book, QOF_TYPE_INSTANCE, {});
QOF_GOBJECT_DISPOSE(qof_book);
QOF_GOBJECT_FINALIZE(qof_book);

static GParamSpec *obj_properties[N_PROPERTIES] = { NULL, };
#undef G_PARAM_READWRITE
#define G_PARAM_READWRITE static_cast<GParamFlags>(G_PARAM_READABLE | G_PARAM_WRITABLE)
/* ====================================================================== */
/* constructor / destructor */

static void coll_destroy(gpointer col)
{
    qof_collection_destroy((QofCollection *) col);
}

static void
qof_book_init (QofBook *book)
{
    if (!book) return;

    book->hash_of_collections = g_hash_table_new_full(
                                    g_str_hash, g_str_equal,
                                    (GDestroyNotify)qof_string_cache_remove,  /* key_destroy_func   */
                                    coll_destroy);                            /* value_destroy_func */

    qof_instance_init_data (&book->inst, QOF_ID_BOOK, book);

    book->data_tables = g_hash_table_new (g_str_hash, g_str_equal);
    book->data_table_finalizers = g_hash_table_new (g_str_hash, g_str_equal);

    book->book_open = 'y';
    book->read_only = FALSE;
    book->session_dirty = FALSE;
    book->version = 0;
}

static void
qof_book_get_property (GObject* object,
		       guint prop_id,
		       GValue* value,
		       GParamSpec* pspec)
{
    QofBook *book;
    gchar *key;

    g_return_if_fail (QOF_IS_BOOK (object));
    book = QOF_BOOK (object);
    switch (prop_id)
    {
    case PROP_OPT_TRADING_ACCOUNTS:
	key = g_strdup_printf ("%s/%s/%s", KVP_OPTION_PATH,
			       OPTION_SECTION_ACCOUNTS,
			       OPTION_NAME_TRADING_ACCOUNTS);
	qof_instance_get_kvp (QOF_INSTANCE (book), key, value);
	g_free (key);
	break;
    case PROP_OPT_BOOK_CURRENCY:
	key = g_strdup_printf ("%s/%s/%s", KVP_OPTION_PATH,
			       OPTION_SECTION_ACCOUNTS,
                   OPTION_NAME_BOOK_CURRENCY);
	qof_instance_get_kvp (QOF_INSTANCE (book), key, value);
	g_free (key);
	break;
    case PROP_OPT_DEFAULT_GAINS_POLICY:
	key = g_strdup_printf ("%s/%s/%s", KVP_OPTION_PATH,
			       OPTION_SECTION_ACCOUNTS,
                   OPTION_NAME_DEFAULT_GAINS_POLICY);
	qof_instance_get_kvp (QOF_INSTANCE (book), key, value);
	g_free (key);
	break;
    case PROP_OPT_DEFAULT_GAINS_ACCOUNT_GUID:
	key = g_strdup_printf ("%s/%s/%s", KVP_OPTION_PATH,
			       OPTION_SECTION_ACCOUNTS,
                   OPTION_NAME_DEFAULT_GAINS_LOSS_ACCT_GUID);
	qof_instance_get_kvp (QOF_INSTANCE (book), key, value);
	g_free (key);
	break;
    case PROP_OPT_AUTO_READONLY_DAYS:
	key = g_strdup_printf ("%s/%s/%s", KVP_OPTION_PATH,
			       OPTION_SECTION_ACCOUNTS,
			       OPTION_NAME_AUTO_READONLY_DAYS);
	qof_instance_get_kvp (QOF_INSTANCE (book), key, value);
	g_free (key);
	break;
    case PROP_OPT_NUM_FIELD_SOURCE:
	key = g_strdup_printf ("%s/%s/%s", KVP_OPTION_PATH,
			       OPTION_SECTION_ACCOUNTS,
			       OPTION_NAME_NUM_FIELD_SOURCE);
	qof_instance_get_kvp (QOF_INSTANCE (book), key, value);
	g_free (key);
	break;
    case PROP_OPT_DEFAULT_BUDGET:
	key = g_strdup_printf ("%s/%s/%s", KVP_OPTION_PATH,
			       OPTION_SECTION_ACCOUNTS,
			       OPTION_NAME_DEFAULT_BUDGET);
	qof_instance_get_kvp (QOF_INSTANCE (book), key, value);
	g_free (key);
	break;
    case PROP_OPT_FY_END:
	key = const_cast<char*>("fy_end");
	qof_instance_get_kvp (QOF_INSTANCE (book), key, value);
	break;
    case PROP_AB_TEMPLATES:
	key = const_cast<char*>(AB_KEY "/" AB_TEMPLATES);
	qof_instance_get_kvp (QOF_INSTANCE (book), key, value);
	break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
qof_book_set_property (GObject      *object,
		       guint         prop_id,
		       const GValue *value,
		       GParamSpec   *pspec)
{
    QofBook *book;
    gchar *key;

    g_return_if_fail (QOF_IS_BOOK (object));
    book = QOF_BOOK (object);
    g_assert (qof_instance_get_editlevel(book));

    switch (prop_id)
    {
    case PROP_OPT_TRADING_ACCOUNTS:
	key = g_strdup_printf ("%s/%s/%s", KVP_OPTION_PATH,
			       OPTION_SECTION_ACCOUNTS,
			       OPTION_NAME_TRADING_ACCOUNTS);
	qof_instance_set_kvp (QOF_INSTANCE (book), key, value);
	g_free (key);
	break;
    case PROP_OPT_BOOK_CURRENCY:
	key = g_strdup_printf ("%s/%s/%s", KVP_OPTION_PATH,
			       OPTION_SECTION_ACCOUNTS,
                   OPTION_NAME_BOOK_CURRENCY);
	qof_instance_set_kvp (QOF_INSTANCE (book), key, value);
	g_free (key);
	break;
    case PROP_OPT_DEFAULT_GAINS_POLICY:
	key = g_strdup_printf ("%s/%s/%s", KVP_OPTION_PATH,
			       OPTION_SECTION_ACCOUNTS,
                   OPTION_NAME_DEFAULT_GAINS_POLICY);
	qof_instance_set_kvp (QOF_INSTANCE (book), key, value);
	g_free (key);
	break;
    case PROP_OPT_DEFAULT_GAINS_ACCOUNT_GUID:
	key = g_strdup_printf ("%s/%s/%s", KVP_OPTION_PATH,
			       OPTION_SECTION_ACCOUNTS,
                   OPTION_NAME_DEFAULT_GAINS_LOSS_ACCT_GUID);
	qof_instance_set_kvp (QOF_INSTANCE (book), key, value);
	g_free (key);
	break;
    case PROP_OPT_AUTO_READONLY_DAYS:
	key = g_strdup_printf ("%s/%s/%s", KVP_OPTION_PATH,
			       OPTION_SECTION_ACCOUNTS,
			       OPTION_NAME_AUTO_READONLY_DAYS);
	qof_instance_set_kvp (QOF_INSTANCE (book), key, value);
	g_free (key);
	break;
    case PROP_OPT_NUM_FIELD_SOURCE:
	key = g_strdup_printf ("%s/%s/%s", KVP_OPTION_PATH,
			       OPTION_SECTION_ACCOUNTS,
			       OPTION_NAME_NUM_FIELD_SOURCE);
	qof_instance_set_kvp (QOF_INSTANCE (book), key, value);
	g_free (key);
	break;
    case PROP_OPT_DEFAULT_BUDGET:
	key = g_strdup_printf ("%s/%s/%s", KVP_OPTION_PATH,
			       OPTION_SECTION_ACCOUNTS,
			       OPTION_NAME_DEFAULT_BUDGET);
	qof_instance_set_kvp (QOF_INSTANCE (book), key, value);
	g_free (key);
	break;
    case PROP_OPT_FY_END:
	key = const_cast<char*>("fy_end");
	qof_instance_set_kvp (QOF_INSTANCE (book), key, value);
	break;
    case PROP_AB_TEMPLATES:
	key = const_cast<char*>(AB_KEY "/" AB_TEMPLATES);
	qof_instance_set_kvp (QOF_INSTANCE (book), key, value);
	break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void
qof_book_class_init (QofBookClass *klass)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
    gobject_class->dispose = qof_book_dispose;
    gobject_class->finalize = qof_book_finalize;
    gobject_class->get_property = qof_book_get_property;
    gobject_class->set_property = qof_book_set_property;

    g_object_class_install_property
    (gobject_class,
     PROP_OPT_TRADING_ACCOUNTS,
     g_param_spec_string("trading-accts",
                         "Use Trading Accounts",
			 "Scheme true ('t') or NULL. If 't', then the book "
			 "uses trading accounts for managing multiple-currency "
			 "transactions.",
                         NULL,
                         G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_OPT_BOOK_CURRENCY,
     g_param_spec_string("book-currency",
                         "Select Book Currency",
			 "The reference currency used to manage multiple-currency "
             "transactions when 'book-currency' currency accounting method "
             "selected; requires valid default gains/loss policy.",
                         NULL,
                         G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_OPT_DEFAULT_GAINS_POLICY,
     g_param_spec_string("default-gains-policy",
                         "Select Default Gains Policy",
			 "The default policy to be used to calculate gains/losses on "
             "dispositions of currencies/commodities other than "
             "'book-currency' when 'book-currency' currency accounting "
             "method selected; requires valid book-currency.",
                         NULL,
                         G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_OPT_DEFAULT_GAINS_ACCOUNT_GUID,
     g_param_spec_boxed("default-gain-loss-account-guid",
                        "Select Default Gain/Loss Account",
			 "The default account to be used for calculated gains/losses on "
             "dispositions of currencies/commodities other than "
             "'book-currency' when 'book-currency' currency accounting "
             "method selected; requires valid book-currency.",
                         GNC_TYPE_GUID,
                         G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_OPT_NUM_FIELD_SOURCE,
     g_param_spec_string("split-action-num-field",
                         "Use Split-Action in the Num Field",
			 "Scheme true ('t') or NULL. If 't', then the book "
			 "will put the split action value in the Num field.",
                         NULL,
                         G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_OPT_AUTO_READONLY_DAYS,
     g_param_spec_double("autoreadonly-days",
                         "Transaction Auto-read-only Days",
			 "Prevent editing of transactions posted more than "
			 "this many days ago.",
			 0,
                         G_MAXDOUBLE,
			 0,
                         G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_OPT_DEFAULT_BUDGET,
     g_param_spec_boxed("default-budget",
                        "Book Default Budget",
                        "The default Budget for this book.",
                        GNC_TYPE_GUID,
                        G_PARAM_READWRITE));
    g_object_class_install_property
    (gobject_class,
     PROP_OPT_FY_END,
     g_param_spec_boxed("fy-end",
                        "Book Fiscal Year End",
                        "A GDate with a bogus year having the last Month and "
			"Day of the Fiscal year for the book.",
                        G_TYPE_DATE,
                        G_PARAM_READWRITE));
    g_object_class_install_property
    (gobject_class,
     PROP_AB_TEMPLATES,
     g_param_spec_boxed("ab-templates",
                        "AQBanking Template List",
                        "A GList of AQBanking Templates",
                        GNC_TYPE_VALUE_LIST,
                        G_PARAM_READWRITE));
}

QofBook *
qof_book_new (void)
{
    QofBook *book;

    ENTER (" ");
    book = static_cast<QofBook*>(g_object_new(QOF_TYPE_BOOK, NULL));
    qof_object_book_begin (book);

    qof_event_gen (&book->inst, QOF_EVENT_CREATE, NULL);
    LEAVE ("book=%p", book);
    return book;
}

static void
book_final (gpointer key, gpointer value, gpointer booq)
{
    QofBookFinalCB cb = reinterpret_cast<QofBookFinalCB>(value);
    QofBook *book = static_cast<QofBook*>(booq);

    gpointer user_data = g_hash_table_lookup (book->data_tables, key);
    (*cb) (book, key, user_data);
}

static void
qof_book_dispose_real (G_GNUC_UNUSED GObject *bookp)
{
}

static void
qof_book_finalize_real (G_GNUC_UNUSED GObject *bookp)
{
}

void
qof_book_destroy (QofBook *book)
{
    GHashTable* cols;

    if (!book) return;
    ENTER ("book=%p", book);

    book->shutting_down = TRUE;
    qof_event_force (&book->inst, QOF_EVENT_DESTROY, NULL);

    /* Call the list of finalizers, let them do their thing.
     * Do this before tearing into the rest of the book.
     */
    g_hash_table_foreach (book->data_table_finalizers, book_final, book);

    qof_object_book_end (book);

    g_hash_table_destroy (book->data_table_finalizers);
    book->data_table_finalizers = NULL;
    g_hash_table_destroy (book->data_tables);
    book->data_tables = NULL;

    /* qof_instance_release (&book->inst); */

    /* Note: we need to save this hashtable until after we remove ourself
     * from it, otherwise we'll crash in our dispose() function when we
     * DO remove ourself from the collection but the collection had already
     * been destroyed.
     */
    cols = book->hash_of_collections;
    g_object_unref (book);
    g_hash_table_destroy (cols);
    /*book->hash_of_collections = NULL;*/

    LEAVE ("book=%p", book);
}

/* ====================================================================== */

gboolean
qof_book_session_not_saved (const QofBook *book)
{
    if (!book) return FALSE;
    return book->session_dirty;

}

void
qof_book_mark_session_saved (QofBook *book)
{
    if (!book) return;

    book->dirty_time = 0;
    if (book->session_dirty)
    {
        /* Set the session clean upfront, because the callback will check. */
        book->session_dirty = FALSE;
        if (book->dirty_cb)
            book->dirty_cb(book, FALSE, book->dirty_data);
    }
}

void qof_book_mark_session_dirty (QofBook *book)
{
    if (!book) return;
    if (!book->session_dirty)
    {
        /* Set the session dirty upfront, because the callback will check. */
        book->session_dirty = TRUE;
        book->dirty_time = gnc_time (NULL);
        if (book->dirty_cb)
            book->dirty_cb(book, TRUE, book->dirty_data);
    }
}

void
qof_book_print_dirty (const QofBook *book)
{
    if (qof_book_session_not_saved(book))
        PINFO("book is dirty.");
    qof_book_foreach_collection
    (book, (QofCollectionForeachCB)qof_collection_print_dirty, NULL);
}

time64
qof_book_get_session_dirty_time (const QofBook *book)
{
    return book->dirty_time;
}

void
qof_book_set_dirty_cb(QofBook *book, QofBookDirtyCB cb, gpointer user_data)
{
    if (book->dirty_cb)
        PWARN("Already existing callback %p, will be overwritten by %p\n",
                  book->dirty_cb, cb);
    book->dirty_data = user_data;
    book->dirty_cb = cb;
}

/* ====================================================================== */
/* getters */

QofBackend *
qof_book_get_backend (const QofBook *book)
{
    if (!book) return NULL;
    return book->backend;
}

gboolean
qof_book_shutting_down (const QofBook *book)
{
    if (!book) return FALSE;
    return book->shutting_down;
}

/* ====================================================================== */
/* setters */

void
qof_book_set_backend (QofBook *book, QofBackend *be)
{
    if (!book) return;
    ENTER ("book=%p be=%p", book, be);
    book->backend = be;
    LEAVE (" ");
}

/* ====================================================================== */
/* Store arbitrary pointers in the QofBook for data storage extensibility */
/* XXX if data is NULL, we should remove the key from the hash table!
 */
void
qof_book_set_data (QofBook *book, const char *key, gpointer data)
{
    if (!book || !key) return;
    g_hash_table_insert (book->data_tables, (gpointer)key, data);
}

void
qof_book_set_data_fin (QofBook *book, const char *key, gpointer data, QofBookFinalCB cb)
{
    if (!book || !key) return;
    g_hash_table_insert (book->data_tables, (gpointer)key, data);

    if (!cb) return;
    g_hash_table_insert (book->data_table_finalizers, (gpointer)key,
			 reinterpret_cast<void*>(cb));
}

gpointer
qof_book_get_data (const QofBook *book, const char *key)
{
    if (!book || !key) return NULL;
    return g_hash_table_lookup (book->data_tables, (gpointer)key);
}

/* ====================================================================== */
gboolean
qof_book_is_readonly(const QofBook *book)
{
    g_return_val_if_fail( book != NULL, TRUE );
    return book->read_only;
}

void
qof_book_mark_readonly(QofBook *book)
{
    g_return_if_fail( book != NULL );
    book->read_only = TRUE;
}
/* ====================================================================== */

QofCollection *
qof_book_get_collection (const QofBook *book, QofIdType entity_type)
{
    QofCollection *col;

    if (!book || !entity_type) return NULL;

    col = static_cast<QofCollection*>(g_hash_table_lookup (book->hash_of_collections, entity_type));
    if (!col)
    {
        col = qof_collection_new (entity_type);
        g_hash_table_insert(
            book->hash_of_collections,
            qof_string_cache_insert((gpointer) entity_type), col);
    }
    return col;
}

struct _iterate
{
    QofCollectionForeachCB  fn;
    gpointer                data;
};

static void
foreach_cb (G_GNUC_UNUSED gpointer key, gpointer item, gpointer arg)
{
    struct _iterate *iter = static_cast<_iterate*>(arg);
    QofCollection *col = static_cast<QofCollection*>(item);

    iter->fn (col, iter->data);
}

void
qof_book_foreach_collection (const QofBook *book,
                             QofCollectionForeachCB cb, gpointer user_data)
{
    struct _iterate iter;

    g_return_if_fail (book);
    g_return_if_fail (cb);

    iter.fn = cb;
    iter.data = user_data;

    g_hash_table_foreach (book->hash_of_collections, foreach_cb, &iter);
}

/* ====================================================================== */

void qof_book_mark_closed (QofBook *book)
{
    if (!book)
    {
        return;
    }
    book->book_open = 'n';
}

gint64
qof_book_get_counter (QofBook *book, const char *counter_name)
{
    KvpFrame *kvp;
    KvpValue *value;

    if (!book)
    {
        PWARN ("No book!!!");
        return -1;
    }

    if (!counter_name || *counter_name == '\0')
    {
        PWARN ("Invalid counter name.");
        return -1;
    }

    /* Use the KVP in the book */
    kvp = qof_instance_get_slots (QOF_INSTANCE (book));

    if (!kvp)
    {
        PWARN ("Book has no KVP_Frame");
        return -1;
    }

    value = kvp->get_slot({"counters", counter_name});
    if (value)
    {
        /* found it */
        return value->get<int64_t>();
    }
    else
    {
        /* New counter */
        return 0;
    }
}

gchar *
qof_book_increment_and_format_counter (QofBook *book, const char *counter_name)
{
    KvpFrame *kvp;
    KvpValue *value;
    gint64 counter;
    gchar* format;
    gchar* result;

    if (!book)
    {
        PWARN ("No book!!!");
        return NULL;
    }

    if (!counter_name || *counter_name == '\0')
    {
        PWARN ("Invalid counter name.");
        return NULL;
    }

    /* Get the current counter value from the KVP in the book. */
    counter = qof_book_get_counter(book, counter_name);

    /* Check if an error occurred */
    if (counter < 0)
        return NULL;

    /* Increment the counter */
    counter++;

    /* Get the KVP from the current book */
    kvp = qof_instance_get_slots (QOF_INSTANCE (book));

    if (!kvp)
    {
        PWARN ("Book has no KVP_Frame");
        return NULL;
    }

    /* Save off the new counter */
    qof_book_begin_edit(book);
    value = new KvpValue(counter);
    delete kvp->set_path({"counters", counter_name}, value);
    qof_instance_set_dirty (QOF_INSTANCE (book));
    qof_book_commit_edit(book);

    format = qof_book_get_counter_format(book, counter_name);

    if (!format)
    {
        PWARN("Cannot get format for counter");
        return NULL;
    }

    /* Generate a string version of the counter */
    result = g_strdup_printf(format, counter);
    g_free (format);
    return result;
}

char *
qof_book_get_counter_format(const QofBook *book, const char *counter_name)
{
    KvpFrame *kvp;
    const char *user_format = NULL;
    gchar *norm_format = NULL;
    KvpValue *value;
    gchar *error = NULL;

    if (!book)
    {
        PWARN ("No book!!!");
        return NULL;
    }

    if (!counter_name || *counter_name == '\0')
    {
        PWARN ("Invalid counter name.");
        return NULL;
    }

    /* Get the KVP from the current book */
    kvp = qof_instance_get_slots (QOF_INSTANCE (book));

    if (!kvp)
    {
        PWARN ("Book has no KVP_Frame");
        return NULL;
    }

    /* Get the format string */
    value = kvp->get_slot({"counter_formats", counter_name});
    if (value)
    {
        user_format = value->get<const char*>();
        norm_format = qof_book_normalize_counter_format(user_format, &error);
        if (!norm_format)
        {
            PWARN("Invalid counter format string. Format string: '%s' Counter: '%s' Error: '%s')", user_format, counter_name, error);
            /* Invalid format string */
            user_format = NULL;
            g_free(error);
        }
    }

    /* If no (valid) format string was found, use the default format
     * string */
    if (!norm_format)
    {
        /* Use the default format */
        norm_format = g_strdup ("%.6" PRIi64);
    }
    return norm_format;
}

gchar *
qof_book_normalize_counter_format(const gchar *p, gchar **err_msg)
{
    const gchar *valid_formats [] = {
            G_GINT64_FORMAT,
            "lli",
            "I64i",
            PRIi64,
            "li",
            NULL,
    };
    int i = 0;
    gchar *normalized_spec = NULL;

    while (valid_formats[i])
    {

        if (err_msg && *err_msg)
        {
            g_free (*err_msg);
            *err_msg = NULL;
        }

        normalized_spec = qof_book_normalize_counter_format_internal(p, valid_formats[i], err_msg);
        if (normalized_spec)
            return normalized_spec;  /* Found a valid format specifier, return */
        i++;
    }

    return NULL;
}

gchar *
qof_book_normalize_counter_format_internal(const gchar *p,
        const gchar *gint64_format, gchar **err_msg)
{
    const gchar *conv_start, *base, *tmp = NULL;
    gchar *normalized_str = NULL, *aux_str = NULL;

    /* Validate a counter format. This is a very simple "parser" that
     * simply checks for a single gint64 conversion specification,
     * allowing all modifiers and flags that printf(3) specifies (except
     * for the * width and precision, which need an extra argument). */
    base = p;

    /* Skip a prefix of any character except % */
    while (*p)
    {
        /* Skip two adjacent percent marks, which are literal percent
         * marks */
        if (p[0] == '%' && p[1] == '%')
        {
            p += 2;
            continue;
        }
        /* Break on a single percent mark, which is the start of the
         * conversion specification */
        if (*p == '%')
            break;
        /* Skip all other characters */
        p++;
    }

    if (!*p)
    {
        if (err_msg)
            *err_msg = g_strdup("Format string ended without any conversion specification");
        return NULL;
    }

    /* Store the start of the conversion for error messages */
    conv_start = p;

    /* Skip the % */
    p++;

    /* See whether we have already reached the correct format
     * specification (e.g. "li" on Unix, "I64i" on Windows). */
    tmp = strstr(p, gint64_format);

    if (!tmp)
    {
        if (err_msg)
            *err_msg = g_strdup_printf("Format string doesn't contain requested format specifier: %s", gint64_format);
        return NULL;
    }

    /* Skip any number of flag characters */
    while (*p && (tmp != p) && strchr("#0- +'I", *p))
    {
        p++;
        tmp = strstr(p, gint64_format);
    }

    /* Skip any number of field width digits,
     * and precision specifier digits (including the leading dot) */
    while (*p && (tmp != p) && strchr("0123456789.", *p))
    {
        p++;
        tmp = strstr(p, gint64_format);
    }

    if (!*p)
    {
        if (err_msg)
            *err_msg = g_strdup_printf("Format string ended during the conversion specification. Conversion seen so far: %s", conv_start);
        return NULL;
    }

    /* See if the format string starts with the correct format
     * specification. */
    tmp = strstr(p, gint64_format);
    if (tmp == NULL)
    {
        if (err_msg)
            *err_msg = g_strdup_printf("Invalid length modifier and/or conversion specifier ('%.4s'), it should be: %s", p, gint64_format);
        return NULL;
    }
    else if (tmp != p)
    {
        if (err_msg)
            *err_msg = g_strdup_printf("Garbage before length modifier and/or conversion specifier: '%*s'", (int)(tmp - p), p);
        return NULL;
    }

    /* Copy the string we have so far and add normalized format specifier for long int */
    aux_str = g_strndup (base, p - base);
    normalized_str = g_strconcat (aux_str, PRIi64, NULL);
    g_free (aux_str);

    /* Skip length modifier / conversion specifier */
    p += strlen(gint64_format);
    tmp = p;

    /* Skip a suffix of any character except % */
    while (*p)
    {
        /* Skip two adjacent percent marks, which are literal percent
         * marks */
        if (p[0] == '%' && p[1] == '%')
        {
            p += 2;
            continue;
        }
        /* Break on a single percent mark, which is the start of the
         * conversion specification */
        if (*p == '%')
        {
            if (err_msg)
                *err_msg = g_strdup_printf("Format string contains unescaped %% signs (or multiple conversion specifications) at '%s'", p);
            g_free (normalized_str);
            return NULL;
        }
        /* Skip all other characters */
        p++;
    }

    /* Add the suffix to our normalized string */
    aux_str = normalized_str;
    normalized_str = g_strconcat (aux_str, tmp, NULL);
    g_free (aux_str);

    /* If we end up here, the string was valid, so return no error
     * message */
    return normalized_str;
}

/** Returns pointer to book-currency name for book, if one exists in the
  * KVP, or NULL; does not validate contents nor determine if there is a valid
  * default gain/loss policy, both of which are required, for the
  * 'book-currency' currency accounting method to apply. Use instead
  * 'gnc_book_get_book_currency_name' which does these validations. */
const gchar *
qof_book_get_book_currency_name (QofBook *book)
{
    const gchar *opt = NULL;
    qof_instance_get (QOF_INSTANCE (book),
		      "book-currency", &opt,
		      NULL);
    return opt;
}

/** Returns pointer to default gain/loss policy for book, if one exists in the
  * KVP, or NULL; does not validate contents nor determine if there is a valid
  * book-currency, both of which are required, for the 'book-currency'
  * currency accounting method to apply. Use instead
  * 'gnc_book_get_default_gains_policy' which does these validations. */
const gchar *
qof_book_get_default_gains_policy (QofBook *book)
{
    const gchar *opt = NULL;
    qof_instance_get (QOF_INSTANCE (book),
		      "default-gains-policy", &opt,
		      NULL);
    return opt;
}

/** Returns pointer to default gain/loss account GUID for book, if one exists in
  * the KVP, or NULL; does not validate contents nor determine if there is a
  * valid book-currency, both of which are required, for the 'book-currency'
  * currency accounting method to apply. Use instead
  * 'gnc_book_get_default_gain_loss_acct' which does these validations. */
const GncGUID *
qof_book_get_default_gain_loss_acct_guid (QofBook *book)
{
    GncGUID *guid = NULL;
    qof_instance_get (QOF_INSTANCE (book),
		      "default-gain-loss-account-guid", &guid,
		      NULL);
    return guid;

}

/* Determine whether this book uses trading accounts */
gboolean
qof_book_use_trading_accounts (const QofBook *book)
{
    const char *opt = NULL;
    qof_instance_get (QOF_INSTANCE (book),
		      "trading-accts", &opt,
		      NULL);
    if (opt && opt[0] == 't' && opt[1] == 0)
        return TRUE;
    return FALSE;
}

/* Returns TRUE if this book uses split action field as the 'Num' field, FALSE
 * if it uses transaction number field */
gboolean
qof_book_use_split_action_for_num_field (const QofBook *book)
{
    const char *opt = NULL;
    qof_instance_get (QOF_INSTANCE (book),
		      "split-action-num-field", &opt,
		      NULL);

    if (opt && opt[0] == 't' && opt[1] == 0)
        return TRUE;
    return FALSE;
}

gboolean qof_book_uses_autoreadonly (const QofBook *book)
{
    g_assert(book);
    return (qof_book_get_num_days_autoreadonly(book) != 0);
}

gint qof_book_get_num_days_autoreadonly (const QofBook *book)
{
    g_assert(book);
    double tmp;
    qof_instance_get (QOF_INSTANCE (book),
		      "autoreadonly-days", &tmp,
		      NULL);
    return (gint) tmp;
}

GDate* qof_book_get_autoreadonly_gdate (const QofBook *book)
{
    gint num_days;
    GDate* result = NULL;

    g_assert(book);
    num_days = qof_book_get_num_days_autoreadonly(book);
    if (num_days > 0)
    {
        result = gnc_g_date_new_today();
        g_date_subtract_days(result, num_days);
    }
    return result;
}

const char*
qof_book_get_string_option(const QofBook* book, const char* opt_name)
{
    auto slot = qof_instance_get_slots(QOF_INSTANCE (book))->get_slot(opt_name);
    if (slot == nullptr)
        return nullptr;
    return slot->get<const char*>();
}

void
qof_book_set_string_option(QofBook* book, const char* opt_name, const char* opt_val)
{
    qof_book_begin_edit(book);
    auto frame = qof_instance_get_slots(QOF_INSTANCE(book));
    if (opt_val && (*opt_val != '\0'))
        delete frame->set(opt_name, new KvpValue(g_strdup(opt_val)));
    else
        delete frame->set(opt_name, nullptr);
    qof_instance_set_dirty (QOF_INSTANCE (book));
    qof_book_commit_edit(book);
}

void
qof_book_begin_edit (QofBook *book)
{
    qof_begin_edit(&book->inst);
}

static void commit_err (G_GNUC_UNUSED QofInstance *inst, QofBackendError errcode)
{
    PERR ("Failed to commit: %d", errcode);
//  gnc_engine_signal_commit_error( errcode );
}

#define GNC_FEATURES "features"
static void
add_feature_to_hash (const gchar *key, KvpValue *value, GHashTable * user_data)
{
    gchar *descr = g_strdup(value->get<const char*>());
    g_hash_table_insert (*(GHashTable**)user_data, (gchar*)key, descr);
}

GHashTable *
qof_book_get_features (QofBook *book)
{
    KvpFrame *frame = qof_instance_get_slots (QOF_INSTANCE (book));
    GHashTable *features = g_hash_table_new_full (g_str_hash, g_str_equal,
                                                  NULL, g_free);

    auto slot = frame->get_slot(GNC_FEATURES);
    if (slot != nullptr)
    {
        frame = slot->get<KvpFrame*>();
        frame->for_each_slot_temp(&add_feature_to_hash, features);
    }
    return features;
}

void
qof_book_set_feature (QofBook *book, const gchar *key, const gchar *descr)
{
    KvpFrame *frame = qof_instance_get_slots (QOF_INSTANCE (book));
    KvpValue* feature = nullptr;
    auto feature_slot = frame->get_slot(GNC_FEATURES);
    if (feature_slot)
    {
        auto feature_frame = feature_slot->get<KvpFrame*>();
        feature = feature_frame->get_slot(key);
    }
    if (feature == nullptr || g_strcmp0 (feature->get<const char*>(), descr))
    {
        qof_book_begin_edit (book);
        delete frame->set_path({GNC_FEATURES, key}, new KvpValue(descr));
        qof_instance_set_dirty (QOF_INSTANCE (book));
        qof_book_commit_edit (book);
    }
}

void
qof_book_load_options (QofBook *book, GNCOptionLoad load_cb, GNCOptionDB *odb)
{
    load_cb (odb, book);
}

void
qof_book_save_options (QofBook *book, GNCOptionSave save_cb,
		       GNCOptionDB* odb, gboolean clear)
{
    /* Wrap this in begin/commit so that it commits only once instead of doing
     * so for every option. Qof_book_set_option will take care of dirtying the
     * book.
     */
    qof_book_begin_edit (book);
    save_cb (odb, book, clear);
    qof_book_commit_edit (book);
}

static void noop (QofInstance *inst) {}

void
qof_book_commit_edit(QofBook *book)
{
    if (!qof_commit_edit (QOF_INSTANCE(book))) return;
    qof_commit_edit_part2 (&book->inst, commit_err, noop, noop/*lot_free*/);
}

void
qof_book_set_option (QofBook *book, KvpValue *value, GSList *path)
{
    KvpFrame *root = qof_instance_get_slots (QOF_INSTANCE (book));
    Path path_v {KVP_OPTION_PATH};
    for (auto item = path; item != nullptr; item = g_slist_next(item))
        path_v.push_back(static_cast<const char*>(item->data));
    qof_book_begin_edit (book);
    delete root->set_path(path_v, value);
    qof_instance_set_dirty (QOF_INSTANCE (book));
    qof_book_commit_edit (book);
}

KvpValue*
qof_book_get_option (QofBook *book, GSList *path)
{
    KvpFrame *root = qof_instance_get_slots(QOF_INSTANCE (book));
    Path path_v {KVP_OPTION_PATH};
    for (auto item = path; item != nullptr; item = g_slist_next(item))
        path_v.push_back(static_cast<const char*>(item->data));
    return root->get_slot(path_v);
}

void
qof_book_options_delete (QofBook *book, GSList *path)
{
    KvpFrame *root = qof_instance_get_slots(QOF_INSTANCE (book));
    if (path != nullptr)
    {
        Path path_v {KVP_OPTION_PATH};
        for (auto item = path; item != nullptr; item = g_slist_next(item))
            path_v.push_back(static_cast<const char*>(item->data));
        delete root->set_path(path_v, nullptr);
    }
    else
        delete root->set_path(KVP_OPTION_PATH, nullptr);
}

/* QofObject function implementation and registration */
gboolean qof_book_register (void)
{
    static QofParam params[] =
    {
        { QOF_PARAM_GUID, QOF_TYPE_GUID, (QofAccessFunc)qof_entity_get_guid, NULL },
        { QOF_PARAM_KVP,  QOF_TYPE_KVP,  (QofAccessFunc)qof_instance_get_slots, NULL },
        { NULL },
    };

    qof_class_register (QOF_ID_BOOK, NULL, params);

    return TRUE;
}

/* ========================== END OF FILE =============================== */
