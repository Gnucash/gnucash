/********************************************************************\
 * qofinstance.h -- fields common to all object instances           *
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
/** @addtogroup Entity
    @{ */
/** @addtogroup Instance
    Qof Instances are a derived type of QofEntity.  The Instance
    adds some common features and functions that most objects
    will want to use.

    @{ */
/** @file qofinstance.h 
 *  @brief Object instance holds common fields that most gnucash objects use.
 * 
 *  @author Copyright (C) 2003,2004 Linas Vepstas <linas@linas.org>
 */

#ifndef QOF_INSTANCE_H
#define QOF_INSTANCE_H

#include <glib-object.h>
#include "guid.h"
#include "gnc-date.h"
#include "kvp_frame.h"

#ifndef QOF_BOOK_H
#define QOF_BOOK_DEFINED
typedef struct _QofBook QofBook;
#endif

/* GObject declarations */

#define QOF_TYPE_INSTANCE            (qof_instance_get_type ())
#define QOF_INSTANCE(o)              (G_TYPE_CHECK_INSTANCE_CAST ((o), QOF_TYPE_INSTANCE, QofInstance))
#define QOF_INSTANCE_CLASS(k)        (G_TYPE_CHECK_CLASS_CAST((k), QOF_TYPE_INSTANCE, QofInstanceClass))
#define QOF_IS_INSTANCE(o)           (G_TYPE_CHECK_INSTANCE_TYPE ((o), QOF_TYPE_INSTANCE))
#define QOF_IS_INSTANCE_CLASS(k)     (G_TYPE_CHECK_CLASS_TYPE ((k), QOF_TYPE_INSTANCE))
#define QOF_INSTANCE_GET_CLASS(o)    (G_TYPE_INSTANCE_GET_CLASS ((o), QOF_TYPE_INSTANCE, QofInstanceClass))

typedef struct _QofInstanceClass QofInstanceClass;
typedef struct _QofInstancePrivate QofInstancePrivate;
typedef struct _QofInstance QofInstance;
#define QofEntity QofInstance /* Backward compatibility, the QofEntity is now "part" of QofInstance */

struct _QofInstance {
	GObject object;
	QofInstancePrivate *priv;
};

typedef void (*QofInstanceForeachCB) (QofInstance *inst, gpointer user_data);

typedef QofInstanceForeachCB QofEntityForeachCB; /* Backward compatibility */

struct _QofInstanceClass {
	GObjectClass parent_class;
	/* virtual table */
	 void        (*foreach) 			(QofInstance *inst, QofInstanceForeachCB cb_func, gpointer user_data);
	 gchar*      (*to_string)			(gpointer instance);
	/* Add Signal Functions Here */
};

GType   qof_instance_get_type (void);




/** Return the book pointer */
QofBook * qof_instance_get_book (const QofInstance *inst);
void      qof_instance_set_book (QofInstance *inst, QofBook *book);

#define qof_instance_get_collection(o) qof_book_get_collection(qof_instance_get_book(o), G_OBJECT_TYPE(o))

/** Return the GUID of this instance */
GUID* qof_instance_get_guid (QofInstance *inst);

/** Set the ID of the entity, over-riding the previous ID. 
 *  Very dangerous, use only for file i/o work. 
 */
void qof_instance_set_guid (QofInstance *inst, GUID *guid);

/** Return the pointer to the kvp_data */
KvpFrame* qof_instance_get_slots (const QofInstance *);


/** Return the last time this instance was modified.  If QofInstances
 *  are used with the QofObject storage backends, then the instance
 *  update times are reserved for use by the backend, for managing
 *  multi-user updates.  Non-backend code should not set the update 
 *  times. 
 */
Timespec qof_instance_get_last_update (const QofInstance *inst);

/** Compare two instances, based on thier last update times. 
 *  Returns a negative, zero or positive value, respectively, 
 *  if 'left' is earlier, same as or later than 'right'.  
 *  Accepts NULL pointers, NULL's are by definition earlier
 *  than any value.
 */
int qof_instance_version_cmp (const QofInstance *left, const QofInstance *right);

void qof_instance_print_dirty (QofInstance *inst, gpointer dummy);

/** Return value of is_dirty flag */
gboolean qof_instance_is_dirty (QofInstance *);

/** \brief Set the dirty flag

Sets this instance AND the collection as dirty.
*/
void qof_instance_set_dirty(QofInstance* inst, gboolean value);

gboolean qof_instance_check_edit(const QofInstance *inst);

gboolean qof_instance_do_free(const QofInstance *inst);

void qof_instance_mark_free(QofInstance *inst);

gboolean qof_instance_get_do_free (QofInstance *inst);

void     qof_instance_set_do_free (QofInstance *inst, gboolean val);

QofInstance* qof_instance_create (GType type, QofBook *book);

gboolean qof_instance_destroy (QofInstance *inst, GError **error);

void qof_instance_release (QofInstance *instance);

#define qof_instancerelease(obj) qof_instance_release (QOF_INSTANCE (obj))

/** Pair things up.  This routine inserts a kvp value into each instance
 *  containing the guid of the other.  In this way, if one has one of the
 *  pair, one can always find the other by looking up it's guid.  Typically,
 *  you will want to use qof_instance_lookup_twin() to find the twin.
 *  (The current implementation assumes the two instances belong to different
 *  books, and will not add gemini kvp's unless the books differ.  Note that
 *  the gemini kvp includes the book guid as well, so that the right book can
 *  be found.
 */
void qof_instance_gemini (QofInstance *to, const QofInstance *from);

/** The qof_instance_lookup_twin() routine will find the "twin" of this
 *    instance 'src' in the given other 'book' (if the twin exists).
 *
 *    When instances are gemini'ed or cloned, both of the pair are marked
 *    with the guid of thier copy, thus allowing the sibling-copy of
 *    an instance to be found.  Since the sibling may end up in a
 *    different book, we need a way of finding it, given only that we
 *    know the book, and that we know its twin.
 *
 *    That's what this routine does.  Given some book 'book', and an
 *    instance 'src', it will find the sibling instance of 'src' that is
 *    in 'book', and return it.  If not found, it returns NULL.  This
 *    routine uses the 'gemini' kvp values to do its work. 
 */
QofInstance * qof_instance_lookup_twin (const QofInstance *src, QofBook *book);

KvpFrame* qof_instance_get_kvp_data (const QofInstance *instance);
void			qof_instance_set_kvp_data (QofInstance *instance, KvpFrame *data);
void      qof_instance_delete_kvp_data (QofInstance *inst);

gint		 	qof_instance_get_edit_level (const QofInstance *instance);
void 		qof_instance_set_edit_level (QofInstance *instance, gint editlevel);

#define qof_object_new_instance(type, book) g_object_new (type, "book", book, NULL)

void    qof_instance_foreach			(QofInstance *inst, QofInstanceForeachCB cb_func, gpointer user_data);


gchar *      qof_instance_to_string			(QofInstance *instance);

#define qof_object_printable(type, instance) qof_instance_to_string (instance)

gboolean qof_instance_begin_edit (QofInstance *inst, GError **error);

gboolean qof_instance_commit_edit(QofInstance *inst, GError **error);


/** Return The kvp data for the book.
 *  Note that the book KVP data is persistent, and is stored/retrieved
 *  from the file/database.  Thus, the book KVP is the correct place to
 *  store data that needs to be persistent accross sessions (or shared
 *  between multiple users).  To store application runtime data, use
 *  qof_book_set_data() instead.

		DEPRECATED: use qof_instance_get_slots instead
 */
#define qof_book_get_slots(book) qof_instance_get_slots(QOF_INSTANCE(book))

/* @} */
/* @} */
#endif /* QOF_INSTANCE_H */
