/********************************************************************\
 * gnc-book.h -- dataset access (set of accounting books)           *
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
 * FILE:
 * gnc-book.h
 *
 * FUNCTION:
 * Encapsulate all the information about a gnucash dataset.
 *
 * HISTORY:
 * Created by Linas Vepstas December 1998
 * Copyright (c) 1998, 1999 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas
 */

#ifndef GNC_BOOK_H
#define GNC_BOOK_H

#include "gnc-engine.h"
#include "gnc-pricedb.h"
#include "Backend.h"
#include "Group.h"

/** TYPES **********************************************************/

struct gnc_book_struct;

typedef struct gnc_book_struct GNCBook;

/** PROTOTYPES ******************************************************/

GNCBook * gnc_book_new (GNCSession *session);
void      gnc_book_destroy (GNCBook *book);

AccountGroup *gnc_book_get_group (GNCBook *book);
void gnc_book_set_group(GNCBook *book, AccountGroup *group);
GNCPriceDB   *gnc_book_get_pricedb (GNCBook *book);

GNCBook * xaccGroupGetBook (AccountGroup *group);
GNCBook * xaccAccountGetBook (Account *account);

guint gnc_book_count_transactions(GNCBook *book);

gnc_commodity_table* gnc_book_get_commodity_table(GNCBook *book);

/**
 * Returns the list of scheduled transactions.
 **/
GList * gnc_book_get_schedxactions( GNCBook *book );
void gnc_book_set_schedxactions( GNCBook *book, GList *newList );

AccountGroup *gnc_book_get_template_group( GNCBook *book );
void gnc_book_set_template_group( GNCBook *book, AccountGroup *templateGroup );

/*
 * The gnc_book_not_saved() subroutine will return TRUE
 *    if any data in the book hasn't been saved to long-term storage.
 */
gboolean gnc_book_not_saved(GNCBook *book);

/* The gnc_book_equal() method returns TRUE if the engine data
 * in the two given books is equal. */
gboolean gnc_book_equal (GNCBook *book_1, GNCBook *book_2);

#endif /* GNC_BOOK_H */
