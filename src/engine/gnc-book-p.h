/********************************************************************\
 * gnc-book-p.h -- private functions for gnc books.                 *
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
 *                                                                  *
\********************************************************************/

/*
 * HISTORY:
 * Created 2001 by Rob Browning
 * Copyright (c) 2001 Rob Browning
 */

#ifndef __GNC_BOOK_P_H__
#define __GNC_BOOK_P_H__

#include "gnc-book.h"
#include "gnc-pricedb.h"
#include "Group.h"

void gnc_book_set_group(GNCBook *book, AccountGroup *grp);
void gnc_book_set_pricedb(GNCBook *book, GNCPriceDB *db);

void gnc_book_mark_saved(GNCBook *book);

#endif /* __GNC_BOOK_P_H__ */
