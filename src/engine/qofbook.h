/********************************************************************\
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
\********************************************************************/
/** @addtogroup Engine
    @{ */
/** @file qofbook.h
 * @brief dataset access (an "accounting book")
 * Encapsulate all the information about a dataset.
 * See src/docs/books.txt for implementation overview.
 *
 * HISTORY:
 * Created by Linas Vepstas December 1998
 * @author Copyright (c) 1998, 1999, 2001, 2003 Linas Vepstas <linas@linas.org>
 * @author Copyright (c) 2000 Dave Peticolas
 */

#ifndef QOF_BOOK_H
#define QOF_BOOK_H

#include <glib.h>

#include "qofid.h"
#include "qofbackend.h"
#include "kvp_frame.h"

/** @brief Encapsulates all the information about a dataset
 * manipulated by GnuCash.  This is the top-most structure
 * used for anchoring data.
 */

/** Lookup an entity by guid, returning pointer to the entity */
#define QOF_BOOK_LOOKUP_ENTITY(book,guid,e_type,c_type) ({  \
  QofEntity *val = NULL;                                    \
  if (guid && book) {                                       \
    QofCollection *col;                                     \
    col = qof_book_get_collection (book, e_type);           \
    val = qof_collection_lookup_entity (col, guid);         \
  }                                                         \
  (c_type *) val;                                           \
})

typedef struct _QofBook       QofBook;
                                                                                
/** GList of QofBook */
typedef GList                 QofBookList;

/** Register the book object with the QOF object system. */
gboolean qof_book_register (void);
                                                                                
/** Allocate, initialise and return a new QofBook.  Books contain references
 *  to all of the top-level object containers. */
QofBook * qof_book_new (void);

/** End any editing sessions associated with book, and free all memory 
    associated with it. */
void      qof_book_destroy (QofBook *book);

/** \return The table of entities of the given type. */
QofCollection  * qof_book_get_collection (QofBook *, QofIdType);

/** Invoke the indicated callback on each collection in the book. */
typedef void (*QofCollectionForeachCB) (QofCollection *, gpointer user_data);
void qof_book_foreach_collection (QofBook *, QofCollectionForeachCB, gpointer);

/** \return The kvp data for the book */
KvpFrame   * qof_book_get_slots (QofBook *book);

/** The qof_book_set_data() allows
 *    arbitrary pointers to structs to be stored in QofBook.
 *    This is the "prefered" method for extending QofBook to hold
 *    new data types.
 */
void qof_book_set_data (QofBook *book, const char *key, gpointer data);

/** Retreives arbitrary pointers to structs stored by qof_book_set_data. */
gpointer qof_book_get_data (QofBook *book, const char *key);

/** DOCUMENT ME! */
QofBackend *qof_book_get_backend (QofBook *book);

void qof_book_set_backend (QofBook *book, QofBackend *);

/** qof_book_not_saved() will return TRUE if any 
 *    data in the book hasn't been saved to long-term storage.
 *    (Actually, that's not quite true.  The book doesn't know 
 *    anything about saving.  Its just that whenever data is modified,
 *    the 'dirty' flag is set.  This routine returns the value of the 
 *    'dirty' flag.  Its up to the backend to periodically reset this 
 *    flag, when it acutally does save the data.)
 */
gboolean qof_book_not_saved (QofBook *book);

/** Call this function when you change the book kvp, to make sure the book
 * is marked 'dirty'. */
void qof_book_kvp_changed (QofBook *book);

/** The qof_book_equal() method returns TRUE if books are equal. 
 * XXX this routine is broken, and does not currently compare data.
 */
gboolean qof_book_equal (QofBook *book_1, QofBook *book_2);

/** This will 'get and increment' the named counter for this book.
 * The return value is -1 on error or the incremented counter.
 */
gint64 qof_book_get_counter (QofBook *book, const char *counter_name);

/** Book parameter names */
/**@{*/ 

#define QOF_BOOK_KVP     "qof-kvp"

/**@}*/
 
/** deprecated */
#define qof_book_get_guid(X) qof_entity_get_guid (QOF_ENTITY(X))

#endif /* QOF_BOOK_H */
/** @} */
