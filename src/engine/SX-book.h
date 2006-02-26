/********************************************************************\
 * SX-book.h -- scheduled transaction dataset access                *
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

/**
 * @addtogroup Engine
 * @{ */
/**
 * @addtogroup SchedXaction
 * @{ */
/**
 * @file SX-book.h
 * @brief Anchor Scheduled Transaction info in a book.
 *        See src/doc/books.txt for design overview.
 * @author Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 * 
 * XXX currently, this is crufty, it should be modified to use
 * entities a bit more whole-heartedly than it does.
 **/

#ifndef GNC_SX_BOOK_H
#define GNC_SX_BOOK_H

#include "config.h"

#include <glib.h>
#include "qof.h"

typedef struct xaccSchedXactionsDef SchedXactions;

SchedXactions * gnc_collection_get_schedxaction_list(QofCollection *col);
GList * gnc_collection_get_schedxactions(QofCollection *col);
GList * gnc_book_get_schedxactions(QofBook *book);

/** Returns the template group from the book. **/
AccountGroup * gnc_book_get_template_group(QofBook *book);
AccountGroup * gnc_collection_get_template_group(QofCollection *col);

#endif /* GNC_SX_BOOK_H */
/** @} */
/** @} */
