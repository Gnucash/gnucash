/********************************************************************\
 * SX-book-p.h -- private scheduled transaction dataset access      *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

/*
 * FILE:
 * SX-book-p.h
 *
 * FUNCTION:
 * Private members of SX-in-book utils.
 * See src/doc/books.txt for design overview.
 *
 * HISTORY:
 * Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 */

#ifndef GNC_SX_BOOK_P_H
#define GNC_SX_BOOK_P_H

#include "qofbook.h"

/* ====================================================================== */

struct xaccSchedXactionsDef {
   QofBook *book;
	GList *sx_list;
	gboolean sx_notsaved;
};

void gnc_book_set_schedxactions( QofBook *book, GList *newList );

/* Associate the given template group with a book */
void gnc_book_set_template_group (QofBook *book, AccountGroup *templateGroup);

gboolean gnc_sxtt_register (void);

#endif /* GNC_SX_BOOK_P_H */
