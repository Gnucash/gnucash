/********************************************************************\
 * qof-book-p.h -- private functions for QOF books.                 *
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
\********************************************************************/
/** @addtogroup Object
    @{ */
/** @addtogroup Object_Private
    Private interfaces, not meant to be used by applications.
    @{ */
/** @name  Book_Private
    @{ */
/*
 * HISTORY:
 * Created 2001 by Rob Browning
 * Copyright (c) 2001 Rob Browning
 * Copyright (c) 2001,2003 Linas Vepstas <linas@linas.org>
 */

#ifndef QOF_BOOK_P_H
#define QOF_BOOK_P_H

#include "qofbackend.h"
#include "qofbook.h"
#include "qofid.h"
#include "qofid-p.h"
#include "qofinstance-p.h"

#include <memory>
#include <string>
#include <boost/container/flat_map.hpp>

struct QofCollectionDeleter
{
    void operator()(QofCollection* col) const noexcept { qof_collection_destroy (col); }
};

using QofCollectionPtr = std::unique_ptr<QofCollection, QofCollectionDeleter>;
using CollectionMap = boost::container::flat_map<std::string, QofCollectionPtr>;
using QofDataMap = boost::container::flat_map<std::string, gpointer>;
using QofDataFinMap = boost::container::flat_map<std::string, QofBookFinalCB>;

struct QofBook
{
    QofInstance   inst;     /* Unique guid for this book. */

    /* Boolean indicates that the session is dirty -- that is, it has
     * not yet been written out to disk after the last time the
     * backend ran commit_edit(). This is distinct from the inherited
     * QofInstance::dirty, which indicates that some persistent
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
     * objects.  This allows a lookup of objects based on their guid.
     */
    CollectionMap hash_of_collections;

    /* In order to store arbitrary data, for extensibility, add a table
     * that will be used to hold arbitrary pointers.
     */
    QofDataMap data_tables;

    /* Hash table of destroy callbacks for the data table. */
    QofDataFinMap data_table_finalizers;

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

    /* A cached value of the OPTION_NAME_NUM_FIELD_SOURCE option value
     * because it is queried quite a lot, so we want to avoid a KVP
     * lookup on each query */
    gboolean cached_num_field_source;
    /* Whether the above cached value is valid. */
    gboolean cached_num_field_source_isvalid;

    /* A cahed value of the "autoreadonly-days" option value because
     * it is queried quite a lot, so we want to avoid a KVP lookup on
     * each query */
    gint cached_num_days_autoreadonly;
    /* Whether the above cached value is valid. */
    gboolean cached_num_days_autoreadonly_isvalid;
};


/* Structure for accessing static functions for testing */
typedef struct
{
    gboolean (*get_session_dirty)(const QofBook*);
    gboolean (*get_read_only)(const QofBook*);
    QofBookDirtyCB (*get_dirty_cb)(const QofBook*);
    void (*set_shutting_down)(QofBook*, gboolean);
    gpointer (*get_dirty_data)(const QofBook*);
    const CollectionMap& (*get_collections)(const QofBook*);
    const QofDataMap& (*get_data_tables)(const QofBook*);
    const QofDataFinMap& (*get_data_table_finalizers)(const QofBook*);
    char (*get_book_open)(const QofBook*);
    int (*get_version)(const QofBook*);
} QofBookTestFunctions;

QofBookTestFunctions* _utest_qofbook_fill_functions (void);

/*
 *    qof_book_set_backend() is used by backends to
 *    initialize the pointers in the book structure to
 *    something that contains actual data.  These routines
 *    should not be used otherwise.  (Its somewhat questionable
 *    if the backends should even be doing this much, but for
 *    backwards compatibility, we leave these here.)
 */
void qof_book_set_backend (QofBook *book, QofBackend *be);

/* Register books with the engine */
gboolean qof_book_register (void);

/** Validate a counter format string with a given format specifier.
 *    If valid, returns a normalized format string,
 *    that is whatever long int specifier was used will be replaced with the value of
 *    the posix "PRIx64" macro.
 *    If not valid returns NULL and optionally set an error message is a non-null
 *    err_msg parameter was passed.
 *    The caller should free the returned format string and  error message with g_free.
 */
gchar *qof_book_normalize_counter_format_internal(const gchar *p,
        const gchar* gint64_format, gchar **err_msg);

/** This debugging function can be used to traverse the book structure
 *    and all subsidiary structures, printing out which structures
 *    have been marked dirty.
 */
void qof_book_print_dirty (const QofBook *book);

/* @} */
/* @} */
/* @} */

#endif /* QOF_BOOK_P_H */
