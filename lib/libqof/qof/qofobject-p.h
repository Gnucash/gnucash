/********************************************************************\
 * qofobject-p.h -- the private Object Registration/Lookup Interface   *
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
/** @name  Objects_Private
    @{ */
/** @file qofobject-p.h
 * @brief the Core Object Registration/Lookup Private Interface
 * @author Copyright (c) 2001,2002, Derek Atkins <warlord@MIT.EDU>
 */

#ifndef QOF_OBJECT_P_H_
#define QOF_OBJECT_P_H_

#include "qofbook.h"
#include "qofobject.h"

/** To be called from within the book */
void qof_object_book_begin (QofBook *book);
void qof_object_book_end (QofBook *book);

gboolean qof_object_is_dirty (const QofBook *book);
void qof_object_mark_clean (QofBook *book);

/** \brief check an object can be created and supports iteration

\param type_name object to check
\param warn If called only once per operation, pass TRUE to log objects
that fail the compliance check. To prevent repeated log messages when
calling more than once, pass FALSE.

\return TRUE if object can be created and supports iteration, else FALSE.
*/
gboolean 
qof_object_compliance (QofIdTypeConst type_name, gboolean warn);

#endif /* QOF_OBJECT_P_H_ */
/** @} */
/** @} */
/** @} */
