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
    Qof Instances are a derived type of QofInstance.  The Instance
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

#include "guid.h"
#include "gnc-date.h"
#include "kvp_frame.h"
#include "qofbook.h"
#include "qofid.h"

/* --- type macros --- */
/* cheesy, but will do for now, eventually should be more gtk-like, handle
 * thunks, etc.  */
#define QOF_INSTANCE(object) ((QofInstance *)(object))

/*typedef struct QofInstance_s QofInstance;*/

/** Initialise the memory associated with an instance */
void qof_instance_init (QofInstance *, QofIdType, QofBook *);

/** release the data associated with this instance. Dont actually free 
 * the memory associated with the instance. */
void qof_instance_release (QofInstance *inst);

/** Return the book pointer */
QofBook * qof_instance_get_book (const QofInstance *);

/** Return the GUID of this instance */
const GUID * qof_instance_get_guid (const QofInstance *);

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

void qof_instance_print_dirty (const QofInstance *entity, gpointer dummy);

/** Return value of is_dirty flag */
gboolean qof_instance_is_dirty (QofInstance *);

/** \brief Set the dirty flag

Sets this instance AND the collection as dirty.
*/
void qof_instance_set_dirty(QofInstance* inst);

/* reset the dirty flag */
void qof_instance_mark_clean (QofInstance *);

gboolean qof_instance_check_edit(const QofInstance *inst);

gboolean qof_instance_do_free(const QofInstance *inst);

void qof_instance_mark_free(QofInstance *inst);

QofInstance* qof_instance_create (QofIdType type, QofBook *book);

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

/* @} */
/* @} */
#endif /* QOF_INSTANCE_H */
