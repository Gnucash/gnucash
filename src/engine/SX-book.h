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
 * @author Copyright (c) 2006 Joshua Sled <jsled@asynchronous.org>
 * 
 * XXX currently, this is crufty, it should be modified to use
 * entities a bit more whole-heartedly than it does.
 **/

#ifndef GNC_SX_BOOK_H
#define GNC_SX_BOOK_H

#include <glib.h>
#include "SchedXaction.h"
#include "qof.h"

typedef struct xaccSchedXactionsDef SchedXactions;

struct xaccSchedXactionsDef {
  QofInstance inst;
  GList* sx_list;
  gboolean sx_notsaved;
};

#define GNC_IS_SXES(obj)  (QOF_CHECK_TYPE((obj), GNC_ID_SXES))
#define GNC_SXES(obj)     (QOF_CHECK_CAST((obj), GNC_ID_SXES, SchedXactions))

SchedXactions* gnc_book_get_schedxactions(QofBook* book);

void gnc_sxes_add_sx(SchedXactions* sxes, SchedXaction* sx);
void gnc_sxes_del_sx(SchedXactions* sxes, SchedXaction* sx);

/** Returns the template group from the book. **/
Account *gnc_book_get_template_root(QofBook *book);

/** @return The list of SXes which reference the given Account. Caller should free this list. **/
GList* gnc_sx_get_sxes_referencing_account(QofBook *book, Account *acct);

#endif /* GNC_SX_BOOK_H */
/** @} */
/** @} */
