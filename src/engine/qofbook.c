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
#include "qofid-p.h"
#include "qofobject-p.h"
#include "qofqueryobject.h"

static short module = MOD_ENGINE;

/* ====================================================================== */
/* constructor / destructor */

static void
qof_book_init (QofBook *book)
{
  if (!book) return;

  book->entity_table = qof_entity_new ();

  qof_entity_guid_new (book->entity_table, &book->guid);
  qof_entity_store(book->entity_table, book, &book->guid, QOF_ID_BOOK);

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

  gnc_engine_generate_event (&book->guid, QOF_ID_BOOK, GNC_EVENT_CREATE);
  LEAVE ("book=%p", book);
  return book;
}

void
qof_book_destroy (QofBook *book) 
{
  if (!book) return;

  ENTER ("book=%p", book);
  gnc_engine_force_event (&book->guid, QOF_ID_BOOK, GNC_EVENT_DESTROY);

  qof_object_book_end (book);

  qof_entity_remove (book->entity_table, &book->guid);
  qof_entity_destroy (book->entity_table);
  book->entity_table = NULL;

  /* FIXME: Make sure the data_table is empty */
  g_hash_table_destroy (book->data_tables);

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

const GUID *
qof_book_get_guid (QofBook *book)
{
  if (!book) return NULL;
  return &book->guid;
}

KvpFrame *
qof_book_get_slots (QofBook *book)
{
  if (!book) return NULL;
  return book->kvp_data;
}

QofEntityTable *
qof_book_get_entity_table (QofBook *book)
{
  if (!book) return NULL;
  return book->entity_table;
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
qof_book_set_guid (QofBook *book, GUID uid)
{
  if (!book) return;

  if (guid_equal (&book->guid, &uid)) return;

  qof_entity_remove(book->entity_table, &book->guid);
  book->guid = uid;
  qof_entity_store(book->entity_table, book, &book->guid, QOF_ID_BOOK);
}

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
/* XXX if data is NULL, we ashould remove the key from the hash table!
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

/* gncObject function implementation and registration */
gboolean qof_book_register (void)
{
  static QofQueryObject params[] = {
    { QOF_BOOK_KVP, QOF_QUERYCORE_KVP, (QofAccessFunc)qof_book_get_slots },
    { QOF_QUERY_PARAM_GUID, QOF_QUERYCORE_GUID, (QofAccessFunc)qof_book_get_guid },
    { NULL },
  };

  qof_query_object_register (QOF_ID_BOOK, NULL, params);

  return TRUE;
}

/* ========================== END OF FILE =============================== */
