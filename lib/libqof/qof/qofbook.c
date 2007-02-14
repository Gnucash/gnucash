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
 */

#include "config.h"

#include <stdlib.h>
#include <string.h>

#include <glib.h>

#include "qof.h"
#include "qofevent-p.h"
#include "qofbackend-p.h"
#include "qofbook-p.h"
#include "qofid-p.h"
//#include "qofobject-p.h"

static QofLogModule log_module = QOF_MOD_ENGINE;

/* GObject declarations */

static void qof_book_class_init(QofBookClass *klass);
static void qof_book_init(QofBook *sp);
static void qof_book_finalize(GObject *object);

/* Book structure */
struct _QofBookPrivate
{
  /* The time when the book was first dirtied.  This is a secondary
   * indicator. It should only be used when inst.dirty is TRUE. */
  time_t dirty_time;

  /* This callback function is called any time the book dirty flag
   * changes state. Both clean->dirty and dirty->clean transitions
   * trigger a callback. */
  QofBookDirtyCB dirty_cb;

  /* This is the user supplied data that is returned in the dirty
   * callback function.*/
  gpointer dirty_data;

  /* The entity table associates the GUIDs of all the objects
   * belonging to this book, with their pointers to the respective
   * objects.  This allows a lookup of objects based on thier guid.
   */
  GHashTable * hash_of_collections;

  /* In order to store arbitrary data, for extensibility, add a table
   * that will be used to hold arbitrary pointers.
   */
  GHashTable *data_tables;

  /* Hash table of destroy callbacks for the data table. */
  GHashTable *data_table_finalizers;

  /* state flag: 'y' means 'open for editing',
   * 'n' means 'book is closed'
   * xxxxx shouldn't this be replaced by the instance editlevel ???
   */
  char book_open;

  /* a flag denoting whether the book is closing down, used to
   * help the QOF objects shut down cleanly without maintaining
   * internal consistency.
   * XXX shouldn't this be replaced by instance->do_free ???
   */
  gboolean shutting_down;

  /* version number, used for tracking multiuser updates */
  gint32  version;

  /* To be technically correct, backends belong to sessions and
   * not books.  So the pointer below "really shouldn't be here",
   * except that it provides a nice convenience, avoiding a lookup
   * from the session.  Better solutions welcome ... */
  QofBackend *backend;

  /* -------------------------------------------------------------- */
  /* Backend private expansion data */
  guint32  idata;     /* used by the sql backend for kvp management */
};

typedef struct _QofBookSignal QofBookSignal;
typedef enum _QofBookSignalType QofBookSignalType;

enum _QofBookSignalType {
	/* Signals */
	LAST_SIGNAL
};

/* properties */
enum
{
        PROP_0
};

struct _QofBookSignal {
	QofBook *object;
};

static guint qof_book_signals[LAST_SIGNAL] = { 0 };
static GObjectClass *parent_class = NULL;

GType
qof_book_get_type()
{
	static GType type = 0;

	if(type == 0) {
		static const GTypeInfo our_info = {
			sizeof (QofBookClass),
			NULL,
			NULL,
			(GClassInitFunc)qof_book_class_init,
			NULL,
			NULL,
			sizeof (QofBook),
			0,
			(GInstanceInitFunc)qof_book_init,
		};

		type = g_type_register_static(QOF_TYPE_INSTANCE, 
			"QofBook", &our_info, 0);
	}

	return type;
}

static void
qof_book_class_init(QofBookClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	parent_class = g_type_class_peek_parent(klass);
	object_class->finalize = qof_book_finalize;
	object_class->set_property = qof_book_set_property;
    object_class->get_property = qof_book_get_property;

	/* Install properties */
	
	/* Create signals here:*/
 	
}

static void
qof_book_init(QofBook *obj)
{
	/* Initialize private members, etc. */

  book->hash_of_collections = g_hash_table_new_full(
      g_int_hash, g_int_equal, NULL,  /* key_destroy_func   */
      coll_destroy);                            /* value_destroy_func */

  book->data_tables = g_hash_table_new (g_str_hash, g_str_equal);
  book->data_table_finalizers = g_hash_table_new (g_str_hash, g_str_equal);
  
  book->book_open = 'y';
  book->version = 0;
  book->idata = 0;

}

static void
qof_book_finalize(GObject *object)
{
	
	/* Free private members, etc. */

  book->shutting_down = TRUE;
  qof_event_force (&book->inst.entity, QOF_EVENT_DESTROY, NULL); // signal emit with confirmation?

  /* Call the list of finalizers, let them do their thing. 
   * Do this before tearing into the rest of the book.
   */
  g_hash_table_foreach (book->data_table_finalizers, book_final, book);

  g_hash_table_destroy (book->data_table_finalizers);
  book->data_table_finalizers = NULL;
  g_hash_table_destroy (book->data_tables);
  book->data_tables = NULL;

  g_hash_table_destroy (book->hash_of_collections);
  book->hash_of_collections = NULL;

	G_OBJECT_CLASS(parent_class)->finalize(object);
}

static void
qof_book_set_property (GObject *object,
				  guint param_id,
				  const GValue *value,
				  GParamSpec *pspec)
{
	QofBook *obj;
	
	obj = QOF_BOOK (object);
	switch (param_id) {		
		default:
   			/* We don't have any other property... */
    		G_OBJECT_WARN_INVALID_PROPERTY_ID(object,property_id,pspec);
    	break;
	}
}

static void
qof_book_get_property (GObject      *object,
                        guint         property_id,
                        GValue       *value,
                        GParamSpec   *pspec)
{
  QofBook *obj;
  
  obj = QOF_BOOK(object);

  switch (property_id) {
  default:
    /* We don't have any other property... */
    G_OBJECT_WARN_INVALID_PROPERTY_ID(object,property_id,pspec);
    break;
  }
}


/* ====================================================================== */
/* constructor / destructor */

static void coll_destroy(gpointer col)
{
  qof_collection_destroy((QofCollection *) col);
}

static void
qof_book_init (QofBook *book)
{
  }

QofBook *
qof_book_new (void)
{
  QofBook *book;

  ENTER (" ");
  book = QOF_BOOK (g_object_new (QOF_TYPE_BOOK, NULL));
  
  qof_object_book_begin (book);

  qof_event_gen (&book->inst.entity, QOF_EVENT_CREATE, NULL);
  
  g_signal_emit_by_name ( QOF_ENTITY (book), "created");
  
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

void
qof_book_destroy (QofBook *book) 
{
  g_object_unref (G_OBJECT (book));
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

  return(book->inst.dirty || qof_object_is_dirty (book));
}

void
qof_book_mark_saved (QofBook *book)
{
  gboolean was_dirty;

  if (!book) return;

  was_dirty = book->inst.dirty;
  book->inst.dirty = FALSE;
  book->dirty_time = 0;
  qof_object_mark_clean (book);
  if (was_dirty) {
    if (book->dirty_cb)
      book->dirty_cb(book, FALSE, book->dirty_data);
  }
}

void qof_book_mark_dirty (QofBook *book)
{
  gboolean was_dirty;

  g_return_if_fail (QOF_IS_BOOK(book));

  was_dirty = book->inst.dirty;
  book->inst.dirty = TRUE;
  if (!was_dirty) {
    book->dirty_time = time(NULL);
    if (book->dirty_cb)
      book->dirty_cb(book, TRUE, book->dirty_data);
  }
}

void
qof_book_print_dirty (const QofBook *book)
{
  if (book->inst.dirty)
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
qof_book_get_collection (const QofBook *book, GType type)
{
  QofCollection *col;

  g_return_val_if_fail (QOF_IS_BOOK (book) || G_TYPE_IS_OBJECT (type), NULL);

  col = g_hash_table_lookup (book->hash_of_collections, type);
  
  if (!QOF_IS_COLLECTION(col)) {
      col = qof_collection_new (type);
      g_hash_table_insert(book->hash_of_collections,
          								type, col);
  }
  return col;
}

gboolean
qof_book_remove_element (QofBook *book, QofInstance *inst)
{
	QofCollection *coll;
	
	gboolean res;
	
	g_return_val_if_fail (QOF_IS_BOOK (book) && QOF_IS_INSTANCE (inst), FALSE);
	
	coll = qof_book_get_collection (book, G_OBJECT_TYPE (inst));
	
	res = qof_collection_remove_element (coll, inst);
	
	return res;
	
}

gboolean
qof_book_insert_element (QofBook *book, QofInstance *inst)
{
	QofCollection *coll;
	gboolean res;
	
	g_return_val_if_fail (QOF_IS_BOOK (book) && QOF_IS_INSTANCE (inst));
	
	coll = qof_book_get_collection (book, G_OBJECT_TYPE (inst));
	
	res = qof_collection_add_element (coll, inst);
	
	return res;
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

/** Invoke the indicated callback on each collection in the book. */
typedef void (*QofCollectionForeachCB) (QofCollection *, gpointer user_data);
void qof_book_foreach_collection (const QofBook *, QofCollectionForeachCB, gpointer);

void 
qof_book_foreach_collection (const QofBook *book, 
                             QofCollectionForeachCB cb, gpointer user_data)
{
  struct _iterate iter;

  g_return_if_fail (QOF_IS_BOOK (book));
  g_return_if_fail (cb);

  iter.fn = cb;
  iter.data = user_data;

  g_hash_table_foreach (book->hash_of_collections, foreach_cb, &iter);
}

void
qof_book_foreach (const QofBook *book, GType type, QofInstanceForeachCB cb, gpointer user_data)
{
	QofCollection *col;
	
	col = qof_book_get_collection (book, type);
	
	qof_collection_foreach (col, cb, user_data);
}

/* ====================================================================== */

void qof_book_mark_closed (QofBook *book)
{
	if(!book) { return; }
	book->book_open = 'n';
}

gchar qof_book_get_open_marker (const QofBook *book)
{
	if(!QOF_IS_BOOK(book))
		return 'n';
	else
		return book->book_open;
}

gint32 qof_book_get_version (const QofBook *book)
{
	if(!QOF_IS_BOOK (book)) 
		return -1;
	else
		return book->version;
}

guint32 qof_book_get_idata (const QofBook *book)
{
	if(!QOF_IS_BOOK (book)) 
		return 0;
	else
		return book->idata;
}

void qof_book_set_version (QofBook *book, gint32 version)
{
	g_return_if_fail (QOF_IS_BOOK (book) && version < 0);
	
	book->version = version;
}

void qof_book_set_idata(QofBook *book, guint32 idata)
{
	g_return_if_fail (QOF_IS_BOOK (book) && idata < 0);
	
	book->idata = idata;
}

gint64
qof_book_get_counter (QofBook *book, const char *counter_name)
{
  QofBackend *be;
  KvpFrame *kvp;
  KvpValue *value;
  gint64 counter;

  if (!QOF_IS_BOOK (book)) {
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
    { QOF_PARAM_GUID, QOF_TYPE_GUID, (QofAccessFunc)qof_entity_get_guid, NULL },
    { QOF_PARAM_KVP,  QOF_TYPE_KVP,  (QofAccessFunc)qof_instance_get_slots, NULL },
    { NULL },
  };

  qof_class_register (QOF_ID_BOOK, NULL, params);

  return TRUE;
}

/* ========================== END OF FILE =============================== */
