/********************************************************************\
 * qofbook.c -- dataset access (set of accounting books)            *
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
\********************************************************************/

/*
 * FILE:
 * qofbook.c
 *
 * FUNCTION:
 * Encapsulate all the information about a gnucash dataset.
 * See src/doc/books.txt for design overview.
 *
 * HISTORY:
 * Created by Linas Vepstas December 1998
 * Copyright (c) 1998-2001,2003 Linas Vepstas <linas@linas.org>
 * Copyright (c) 2000 Dave Peticolas
 */

#include "config.h"

#include <stdlib.h>
#include <string.h>

#include <glib.h>

#include "gnc-event.h"
#include "gnc-event-p.h"
#include "gnc-trace.h"
#include "qofbackend-p.h"
#include "qofbook.h"
#include "qofbook-p.h"
#include "qofclass.h"
#include "qofid-p.h"
#include "qofobject-p.h"

static short module = MOD_ENGINE;

/* ====================================================================== */
/* constructor / destructor */

static void
qof_book_init (QofBook *book)
{
  QofCollection *col;
  if (!book) return;

  book->hash_of_collections = g_hash_table_new (g_str_hash, g_str_equal);

  col = qof_book_get_collection (book, QOF_ID_BOOK);
  qof_entity_init (&book->entity, QOF_ID_BOOK, col);

  book->kvp_data = kvp_frame_new ();
  
  book->data_tables = g_hash_table_new (g_str_hash, g_str_equal);
  
  book->book_open = 'y';
  book->version = 0;
  book->idata = 0;
}

QofBook *
qof_book_new (void)
{
  QofBook *book;

  ENTER (" ");
  book = g_new0(QofBook, 1);
  qof_book_init(book);
  qof_object_book_begin (book);

  gnc_engine_gen_event (&book->entity, GNC_EVENT_CREATE);
  LEAVE ("book=%p", book);
  return book;
}

static gboolean
coll_destroy(gpointer key, gpointer value, gpointer not_used)
{
  QofCollection *col = value;
  qof_collection_destroy (col);
  return TRUE;
}

void
qof_book_destroy (QofBook *book) 
{
  if (!book) return;

  ENTER ("book=%p", book);
  gnc_engine_force_event (&book->entity, GNC_EVENT_DESTROY);

  qof_object_book_end (book);

  kvp_frame_delete (book->kvp_data);

  /* FIXME: Make sure the data_table is empty */
  g_hash_table_destroy (book->data_tables);

  qof_entity_release (&book->entity);

  g_hash_table_foreach_remove (book->hash_of_collections,
                               coll_destroy, NULL);
  g_hash_table_destroy (book->hash_of_collections);
  book->hash_of_collections = NULL;

  g_free (book);
  LEAVE ("book=%p", book);
}

/* ====================================================================== */
/* XXX this should probably be calling is_equal callbacks on gncObject */

gboolean
qof_book_equal (QofBook *book_1, QofBook *book_2)
{
  if (book_1 == book_2) return TRUE;
  if (!book_1 || !book_2) return FALSE;
  return TRUE;
}

/* ====================================================================== */

gboolean
qof_book_not_saved(QofBook *book)
{
  if (!book) return FALSE;

  return(book->dirty || qof_object_is_dirty (book));
}

void
qof_book_mark_saved(QofBook *book)
{
  if (!book) return;

  book->dirty = FALSE;
  qof_object_mark_clean (book);
}

/* ====================================================================== */
/* getters */

KvpFrame *
qof_book_get_slots (QofBook *book)
{
  if (!book) return NULL;
  return book->kvp_data;
}

QofBackend * 
qof_book_get_backend (QofBook *book)
{
   if (!book) return NULL;
   return book->backend;
}

/* ====================================================================== */
/* setters */

void
qof_book_set_backend (QofBook *book, QofBackend *be)
{
  if (!book) return;
  ENTER ("book=%p be=%p", book, be);
  book->backend = be;
}

void qof_book_kvp_changed (QofBook *book)
{
  if (!book) return;
  book->dirty = TRUE;
}

/* ====================================================================== */

/* Store arbitrary pointers in the QofBook for data storage extensibility */
/* XXX if data is NULL, we should remove the key from the hash table!
 *
 * XXX We need some design comments:  an equivalent storage mechanism
 * would have been to give each item a GUID, store the GUID in a kvp frame,
 * and then do a GUID lookup to get the pointer to the actual object.
 * Of course, doing a kvp lookup followed by a GUID lookup would be 
 * a good bit slower, but may be that's OK? In most cases, book data
 * is accessed only infrequently?  --linas
 */
void 
qof_book_set_data (QofBook *book, const char *key, gpointer data)
{
  if (!book || !key) return;
  g_hash_table_insert (book->data_tables, (gpointer)key, data);
}

gpointer 
qof_book_get_data (QofBook *book, const char *key)
{
  if (!book || !key) return NULL;
  return g_hash_table_lookup (book->data_tables, (gpointer)key);
}

/* ====================================================================== */

QofCollection *
qof_book_get_collection (QofBook *book, QofIdType entity_type)
{
  QofCollection *col;
                                                                                
  col = g_hash_table_lookup (book->hash_of_collections, entity_type);
  if (col) return col;
                                                                                
  col = qof_collection_new (entity_type);
                                                                                
  /* XXX should use string_cache instead of strdup */
  /* Need strdup because values are sometimes freed */
  /* this is a memory leak, since malloc'ed value is not freed */
  /* ??? huh ?? freed where? by whom? */
  g_hash_table_insert (book->hash_of_collections,
          (gpointer)g_strdup(entity_type), col);
                                                                                
  return col;
}

struct _iterate {
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
qof_book_foreach_collection (QofBook *book, 
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

gint64
qof_book_get_counter (QofBook *book, const char *counter_name)
{
  QofBackend *be;
  KvpFrame *kvp;
  KvpValue *value;
  gint64 counter;

  if (!book) {
    PWARN ("No book!!!");
    return -1;
  }

  if (!counter_name || *counter_name == '\0') {
    PWARN ("Invalid counter name.");
    return -1;
  }

  /* If we've got a backend with a counter method, call it */
  be = book->backend;
  if (be && be->counter)
    return ((be->counter)(be, counter_name));

  /* If not, then use the KVP in the book */
  kvp = qof_book_get_slots (book);

  if (!kvp) {
    PWARN ("Book has no KVP_Frame");
    return -1;
  }

  value = kvp_frame_get_slot_path (kvp, "counters", counter_name, NULL);
  if (value) {
    /* found it */
    counter = kvp_value_get_gint64 (value);
  } else {
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

/* QofObject function implementation and registration */
gboolean qof_book_register (void)
{
  static QofParam params[] = {
    { QOF_BOOK_KVP, QOF_TYPE_KVP, (QofAccessFunc)qof_book_get_slots, NULL },
    { QOF_QUERY_PARAM_GUID, QOF_TYPE_GUID, (QofAccessFunc)qof_entity_get_guid, NULL },
    { NULL },
  };

  qof_class_register (QOF_ID_BOOK, NULL, params);

  return TRUE;
}

/* ========================== END OF FILE =============================== */
