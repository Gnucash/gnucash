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
 * qofbook.c
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

#include "config.h"

#include <stdlib.h>
#include <string.h>

#include <glib.h>
#include <libguile.h>

#include "qof.h"
#include "qofevent-p.h"
#include "qofbackend-p.h"
#include "qofbook-p.h"
#include "qofid-p.h"
#include "qofobject-p.h"

static QofLogModule log_module = QOF_MOD_ENGINE;

QOF_GOBJECT_IMPL(qof_book, QofBook, QOF_TYPE_INSTANCE);

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
                                    (GDestroyNotify)qof_util_string_cache_remove,  /* key_destroy_func   */
                                    coll_destroy);                            /* value_destroy_func */

    qof_instance_init_data (&book->inst, QOF_ID_BOOK, book);

    book->data_tables = g_hash_table_new (g_str_hash, g_str_equal);
    book->data_table_finalizers = g_hash_table_new (g_str_hash, g_str_equal);

    book->book_open = 'y';
    book->version = 0;
}

QofBook *
qof_book_new (void)
{
    QofBook *book;

    ENTER (" ");
    book = g_object_new(QOF_TYPE_BOOK, NULL);
    qof_object_book_begin (book);

    qof_event_gen (&book->inst, QOF_EVENT_CREATE, NULL);
    LEAVE ("book=%p", book);
    return book;
}

static void
book_final (gpointer key, gpointer value, gpointer booq)
{
    QofBookFinalCB cb = value;
    QofBook *book = booq;

    gpointer user_data = g_hash_table_lookup (book->data_tables, key);
    (*cb) (book, key, user_data);
}

static void
qof_book_dispose_real (GObject *bookp)
{
}

static void
qof_book_finalize_real (GObject *bookp)
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
    book->hash_of_collections = NULL;

    LEAVE ("book=%p", book);
}

/* ====================================================================== */
/* XXX this should probably be calling is_equal callbacks on gncObject */

gboolean
qof_book_equal (const QofBook *book_1, const QofBook *book_2)
{
    if (book_1 == book_2) return TRUE;
    if (!book_1 || !book_2) return FALSE;
    return FALSE;
}

/* ====================================================================== */

gboolean
qof_book_not_saved (const QofBook *book)
{
    if (!book) return FALSE;

    return(qof_instance_get_dirty_flag(book) || qof_object_is_dirty(book));
}

void
qof_book_mark_saved (QofBook *book)
{
    gboolean was_dirty;

    if (!book) return;

    was_dirty = qof_instance_get_dirty_flag(book);
    qof_instance_set_dirty_flag(book, FALSE);
    book->dirty_time = 0;
    qof_object_mark_clean (book);
    if (was_dirty)
    {
        if (book->dirty_cb)
            book->dirty_cb(book, FALSE, book->dirty_data);
    }
}

void qof_book_mark_dirty (QofBook *book)
{
    gboolean was_dirty;

    if (!book) return;

    was_dirty = qof_instance_get_dirty_flag(book);
    qof_instance_set_dirty_flag(book, TRUE);
    if (!was_dirty)
    {
        book->dirty_time = time(NULL);
        if (book->dirty_cb)
            book->dirty_cb(book, TRUE, book->dirty_data);
    }
}

void
qof_book_print_dirty (const QofBook *book)
{
    if (qof_instance_get_dirty_flag(book))
        printf("book is dirty.\n");
    qof_book_foreach_collection
    (book, (QofCollectionForeachCB)qof_collection_print_dirty, NULL);
}

time_t
qof_book_get_dirty_time (const QofBook *book)
{
    return book->dirty_time;
}

void
qof_book_set_dirty_cb(QofBook *book, QofBookDirtyCB cb, gpointer user_data)
{
    if (book->dirty_cb)
        g_warning("qof_book_set_dirty_cb: Already existing callback %p, will be overwritten by %p\n",
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

void qof_book_kvp_changed (QofBook *book)
{
    qof_book_mark_dirty(book);
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
    g_hash_table_insert (book->data_table_finalizers, (gpointer)key, cb);
}

gpointer
qof_book_get_data (const QofBook *book, const char *key)
{
    if (!book || !key) return NULL;
    return g_hash_table_lookup (book->data_tables, (gpointer)key);
}

/* ====================================================================== */

QofCollection *
qof_book_get_collection (const QofBook *book, QofIdType entity_type)
{
    QofCollection *col;

    if (!book || !entity_type) return NULL;

    col = g_hash_table_lookup (book->hash_of_collections, entity_type);
    if (!col)
    {
        col = qof_collection_new (entity_type);
        g_hash_table_insert(
            book->hash_of_collections,
            qof_util_string_cache_insert((gpointer) entity_type), col);
    }
    return col;
}

struct _iterate
{
    QofCollectionForeachCB  fn;
    gpointer                data;
};

static void
foreach_cb (gpointer key, gpointer item, gpointer arg)
{
    struct _iterate *iter = arg;
    QofCollection *col = item;

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

gchar qof_book_get_open_marker(const QofBook *book)
{
    if (!book)
    {
        return 'n';
    }
    return book->book_open;
}

gint32 qof_book_get_version (const QofBook *book)
{
    if (!book)
    {
        return -1;
    }
    return book->version;
}

void qof_book_set_version (QofBook *book, gint32 version)
{
    if (!book && version < 0)
    {
        return;
    }
    book->version = version;
}

gint64
qof_book_get_counter (const QofBook *book, const char *counter_name)
{
    QofBackend *be;
    KvpFrame *kvp;
    KvpValue *value;
    gint64 counter;

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

    /* If we've got a backend with a counter method, call it */
    be = book->backend;
    if (be && be->counter)
        return ((be->counter)(be, counter_name));

    /* If not, then use the KVP in the book */
    kvp = qof_book_get_slots (book);

    if (!kvp)
    {
        PWARN ("Book has no KVP_Frame");
        return -1;
    }

    value = kvp_frame_get_slot_path (kvp, "counters", counter_name, NULL);
    if (value)
    {
        /* found it */
        counter = kvp_value_get_gint64 (value);
    }
    else
    {
        /* New counter */
        counter = 0;
    }

    /* Counter is now valid; increment it */
    counter++;

    /* Save off the new counter */
    value = kvp_value_new_gint64 (counter);
    kvp_frame_set_slot_path (kvp, value, "counters", counter_name, NULL);
    kvp_value_delete (value);

    /* and return the value */
    return counter;
}

static char *get_scm_string(const char *str_name)
{
    SCM scm_string = scm_c_eval_string (str_name);
    if (! SCM_STRINGP(scm_string))
        return NULL;
        
    return g_strdup(SCM_STRING_CHARS(scm_string));
}

/* Determine whether this book uses trading accounts */
gboolean qof_book_use_trading_accounts (const QofBook *book)
{
    static char *options_name = NULL;
    static char *acct_section = NULL;
    static char *trading_opt = NULL;
    
    const char *opt;
    kvp_value *kvp_val;
    
    if (options_name == NULL)
    {
        options_name = get_scm_string ("(car gnc:*kvp-option-path*)");
        acct_section = get_scm_string ("gnc:*book-label*");
        trading_opt = get_scm_string ("gnc:*trading-accounts*");
        if (options_name == NULL || acct_section == NULL || trading_opt == NULL)
        {
            PWARN ("Unable to find trading account preference");
        }
    }
    
    kvp_val = kvp_frame_get_slot_path (qof_book_get_slots (book), options_name, acct_section,
                                       trading_opt, NULL);
    if (kvp_val == NULL)
        return FALSE;
    
    opt = kvp_value_get_string (kvp_val);
    
    if (opt && opt[0] == 't' && opt[1] == 0)
        return TRUE;
    return FALSE;
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
