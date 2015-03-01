/********************************************************************\
 * qofbook.h -- Encapsulate all the information about a dataset.    *
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
\********************************************************************/
/** @addtogroup Object
    @{ */
/** @addtogroup Book
    A QOF Book is a dataset.  It provides a single handle
    through which all the various collections of entities
    can be found.   In particular, given only the type of
    the entity, the collection can be found.

    Books also provide the 'natural' place to working with
    a storage backend, as a book can encapsulate everything
    held in storage.
    @{ */
/** @file qofbook.h
 * @brief Encapsulate all the information about a dataset.
 *
 * @author Copyright (c) 1998, 1999, 2001, 2003 Linas Vepstas <linas@linas.org>
 * @author Copyright (c) 2000 Dave Peticolas
 */

#ifndef QOF_BOOK_H
#define QOF_BOOK_H

#ifdef __cplusplus
extern "C"
{
#endif

/* We only want a few things exported to Guile */
#ifndef SWIG

typedef struct _QofBookClass  QofBookClass;

#include "qofid.h"
#include "kvp_frame.h"
#include "qofinstance.h"

/* --- type macros --- */
#define QOF_TYPE_BOOK            (qof_book_get_type ())
#define QOF_BOOK(o)              \
     (G_TYPE_CHECK_INSTANCE_CAST ((o), QOF_TYPE_BOOK, QofBook))
#define QOF_BOOK_CLASS(k)        \
     (G_TYPE_CHECK_CLASS_CAST((k), QOF_TYPE_BOOK, QofBookClass))
#define QOF_IS_BOOK(o)           \
     (G_TYPE_CHECK_INSTANCE_TYPE ((o), QOF_TYPE_BOOK))
#define QOF_IS_BOOK_CLASS(k)     \
     (G_TYPE_CHECK_CLASS_TYPE ((k), QOF_TYPE_BOOK))
#define QOF_BOOK_GET_CLASS(o)    \
     (G_TYPE_INSTANCE_GET_CLASS ((o), QOF_TYPE_BOOK, QofBookClass))

typedef void (*QofBookDirtyCB) (QofBook *, gboolean dirty, gpointer user_data);

typedef struct gnc_option_db GNCOptionDB;

typedef void (*GNCOptionSave) (GNCOptionDB*, KvpFrame*, gboolean);
typedef void (*GNCOptionLoad) (GNCOptionDB*, KvpFrame*);

/* Book structure */
struct _QofBook
{
    QofInstance   inst;     /* Unique guid for this book. */

    /* Boolean indicates that the session is dirty -- that is, it has
     * not yet been written out to disk after the last time the
     * backend ran commit_edit(). This is distinct from the inherited
     * QofInstance::dirty, which indicates that some persisitent
     * property of the book object itself has been edited and not
     * committed. Some backends write data out as part of
     * commit_edit() and so don't use this flag.
     */
    gboolean session_dirty;

    /* The time when the book was first dirtied.  This is a secondary
     * indicator. It should only be used when session_saved is FALSE. */
    time64 dirty_time;

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

    /* Boolean indicates whether book is safe to write to (true means
     * that it isn't). The usual reason will be a database version
     * mismatch with the running instance of Gnucash.
     */
    gboolean read_only;

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
};

struct _QofBookClass
{
    QofInstanceClass parent_class;
};

GType qof_book_get_type(void);

/** @brief Encapsulates all the information about a dataset
 * manipulated by QOF.  This is the top-most structure
 * used for anchoring data.
 */

/** This macro looks up an entity by GncGUID and returns a pointer to the
 * entity by ending with a "return" statement. Hence, this macro can
 * only be used as the last statement in the definition of a function,
 * but not somewhere inline in the code. */
#define QOF_BOOK_RETURN_ENTITY(book,guid,e_type,c_type) {   \
  QofInstance *val = NULL;                                  \
  if ((guid != NULL) && (book != NULL)) {		    \
    const QofCollection *col;                               \
    col = qof_book_get_collection (book, e_type);           \
    val = qof_collection_lookup_entity (col, guid);         \
  }                                                         \
  return (c_type *) val;                                    \
}



/** GList of QofBook */
typedef GList                 QofBookList;

typedef void (*QofBookFinalCB) (QofBook *, gpointer key, gpointer user_data);

/** Register the book object with the QOF object system. */
gboolean qof_book_register (void);

/** Allocate, initialise and return a new QofBook.  Books contain references
 *  to all of the top-level object containers. */
QofBook * qof_book_new (void);

/** End any editing sessions associated with book, and free all memory
    associated with it. */
void      qof_book_destroy (QofBook *book);

/** Close a book to editing.

It is up to the application to check this flag,
and once marked closed, books cannnot be marked as open.
*/
void qof_book_mark_closed (QofBook *book);

/** Return The table of entities of the given type.
 *
 *  When an object's constructor calls qof_instance_init(), a
 *  reference to the object is stored in the book.  The book stores
 *  all the references to initialized instances, sorted by type.  This
 *  function returns a collection of the references for the specified
 *  type.
 *
 *  If the collection doesn't yet exist for the indicated type,
 *  it is created.  Thus, this routine is gaurenteed to return
 *  a non-NULL value.  (Unless the system malloc failed (out of
 *  memory) in which case what happens??).
 */
/*@ dependent @*/
QofCollection  * qof_book_get_collection (const QofBook *, QofIdType);

/** Invoke the indicated callback on each collection in the book. */
typedef void (*QofCollectionForeachCB) (QofCollection *, gpointer user_data);
void qof_book_foreach_collection (const QofBook *, QofCollectionForeachCB, gpointer);

/** The qof_book_set_data() allows arbitrary pointers to structs
 *    to be stored in QofBook. This is the "preferred" method for
 *    extending QofBook to hold new data types.  This is also
 *    the ideal location to store other arbitrary runtime data
 *    that the application may need.
 */
void qof_book_set_data (QofBook *book, const gchar *key, gpointer data);

/** Same as qof_book_set_data(), except that the callback will be called
 *  when the book is destroyed.  The argument to the callback will be
 *  the book followed by the data pointer.
 */
void qof_book_set_data_fin (QofBook *book, const gchar *key, gpointer data,
                            QofBookFinalCB);

/** Retrieves arbitrary pointers to structs stored by qof_book_set_data. */
gpointer qof_book_get_data (const QofBook *book, const gchar *key);

/** Return whether the book is read only. */
gboolean qof_book_is_readonly(const QofBook *book);

/** Mark the book as read only. */
void qof_book_mark_readonly(QofBook *book);

#endif /* SWIG */

/** Returns flag indicating whether this book uses trading accounts */
gboolean qof_book_use_trading_accounts (const QofBook *book);

/** Returns TRUE if this book uses a book-currency */
gboolean qof_book_use_book_currency (const QofBook *book);

/** Returns pointer to Book Currency unique_name for book or NULL */
const gchar * qof_book_get_book_currency_unique_name (QofBook *book);

/** Returns TRUE if the auto-read-only feature should be used, otherwise
 * FALSE. This is just a wrapper on qof_book_get_num_days_autoreadonly() == 0. */
gboolean qof_book_uses_autoreadonly (const QofBook *book);

/** Returns the number of days for auto-read-only transactions. If zero,
 * the auto-read-only feature should be disabled (and qof_book_uses_autoreadonly()
 * returns FALSE). */
gint qof_book_get_num_days_autoreadonly (const QofBook *book);

/** Returns the GDate that is the threshold for auto-read-only. Any txn
 * with posted-date lesser than this date should be considered read-only.
 *
 * If the auto-read-only feature is not used (qof_book_uses_autoreadonly()
 * returns FALSE), NULL is returned here.
 *
 * The returned object was allocated newly; the caller must
 * g_date_free() the object afterwards. */
GDate* qof_book_get_autoreadonly_gdate (const QofBook *book);

/** Returns TRUE if this book uses split action field as the 'Num' field, FALSE
 *  if it uses transaction number field */
gboolean qof_book_use_split_action_for_num_field (const QofBook *book);

/** Is the book shutting down? */
gboolean qof_book_shutting_down (const QofBook *book);

/** qof_book_not_saved() returns the value of the session_dirty flag,
 * set when changes to any object in the book are committed
 * (qof_backend->commit_edit has been called) and the backend hasn't
 * yet written out the changes. (Note that SQL backends write commits
 * out immediately; file backends don't, and use the flag to control
 * an autosave timer.)
 */
gboolean qof_book_session_not_saved (const QofBook *book);

/* The following functions are not useful in scripting languages */
#ifndef SWIG

/** The qof_book_mark_saved() routine marks the book as having been
 *    saved (to a file, to a database). Used by backends to mark the
 *    notsaved flag as FALSE just after loading.  Can also be used
 *    by the frontend when the used has said to abandon any changes.
 */
void qof_book_mark_session_saved(QofBook *book);

/** The qof_book_mark_dirty() routine marks the book as having been
 *    modified. It can be used by frontend when the used has made a
 *    change at the book level.
 */
void qof_book_mark_session_dirty(QofBook *book);

/** Retrieve the earliest modification time on the book. */
time64 qof_book_get_session_dirty_time(const QofBook *book);

/** Set the function to call when a book transitions from clean to
 *    dirty, or vice versa.
 */
void qof_book_set_dirty_cb(QofBook *book, QofBookDirtyCB cb, gpointer user_data);

/** This will get the named counter for this book. The return value is
 *    -1 on error or the current value of the counter.
 */
gint64 qof_book_get_counter (QofBook *book, const char *counter_name);

/** This will increment the named counter for this book and format it.
 *    The return value is NULL on error or the formatted (new) value of
 *    the counter. The caller should free the result with g_gree.
 */
gchar *qof_book_increment_and_format_counter (QofBook *book, const char *counter_name);

/** Validate a counter format string. Returns an error message if the
 *    format string was invalid, or NULL if it is ok. The caller should
 *    free the error message with g_free.
 */
gchar * qof_book_validate_counter_format(const gchar *format);

/** Get the format string to use for the named counter.
 *    The return value is NULL on error or the format string of the
 *    counter. The string should not be freed.
 */
const char *qof_book_get_counter_format (const QofBook *book,
					 const char *counter_name);

const char* qof_book_get_string_option(const QofBook* book, const char* opt_name);
void qof_book_set_string_option(QofBook* book, const char* opt_name, const char* opt_val);

/** Access functions for reading and setting the used-features on this book.
 */
GHashTable *qof_book_get_features (QofBook *book);
void qof_book_set_feature (QofBook *book, const gchar *key, const gchar *descr);

void qof_book_begin_edit(QofBook *book);
void qof_book_commit_edit(QofBook *book);

/* Access functions for loading and saving the file options */
void qof_book_load_options (QofBook *book, GNCOptionLoad load_cb,
			    GNCOptionDB *odb);
void
qof_book_save_options (QofBook *book, GNCOptionSave save_cb,
		       GNCOptionDB* odb, gboolean clear);


/** deprecated */
#define qof_book_get_guid(X) qof_entity_get_guid (QOF_INSTANCE(X))

#endif /* SWIG */
#ifdef __cplusplus
}
#endif

#endif /* QOF_BOOK_H */
/** @} */
/** @} */
