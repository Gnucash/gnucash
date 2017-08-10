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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
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

#include "qof.h"
#include "SX-book.h"

/* ====================================================================== */

SchedXactions* gnc_collection_get_schedxactions(const QofCollection *col);

/* Associate the given template root account with a book */
void gnc_book_set_template_root (QofBook *book, Account *templateRoot);

gboolean gnc_sxtt_register (void);

#endif /* GNC_SX_BOOK_P_H */
