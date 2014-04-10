/********************************************************************\
 * gncObjectP.h -- the Core Object Registration/Lookup Interface    *
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
/** @file gncObject.h
 * @breif the Core Object Registration/Lookup Private Interface
 *
 * @author Copyright (c) 2001,2002, Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_OBJECTP_H_
#define GNC_OBJECTP_H_

#include "gncObject.h"
#include "qofbook.h"

/* Initialize the object registration subsystem */
void qof_object_initialize (void);
void qof_object_shutdown (void);

/* Note that the following are per-class and not per-instance */
/* To be called from within the book */
void qof_object_book_begin (QofBook *book);
void qof_object_book_end (QofBook *book);

gboolean qof_object_is_dirty (QofBook *book);
void qof_object_mark_clean (QofBook *book);

#endif /* GNC_OBJECTP_H_ */
/** @} */
