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
 * Copyright (c) 1998, 1999, 2001 Linas Vepstas <linas@linas.org>
 * Copyright (c) 2000 Dave Peticolas
 */

#ifndef GNC_BOOK_H
#define GNC_BOOK_H

#include "gnc-engine.h"
#include "gnc-pricedb.h"
#include "kvp_frame.h"

/** PROTOTYPES ******************************************************/

GNCBook * gnc_book_new (void);
void      gnc_book_destroy (GNCBook *book);

/* 
 * The gnc_book_get_guid() routine returns the GUID for this book.
 * The gnc_book_get_slots() method will return the kvp data 
 *    for the book 
 *
 * The gnc_book_get_group() returns the top-level group in the book.
 * The gnc_book_get_pricedb() ditto 
 * The gnc_book_get_commodity_table() ditto
 * The gnc_book_get_schedxactions() returns the list of scheduled transactions.
 * The gnc_book_get_template_group() ditto
 */
const GUID          * gnc_book_get_guid (GNCBook *book);
kvp_frame           * gnc_book_get_slots (GNCBook *book);
AccountGroup        * gnc_book_get_group (GNCBook *book);
GNCPriceDB          * gnc_book_get_pricedb (GNCBook *book);
gnc_commodity_table * gnc_book_get_commodity_table(GNCBook *book);
GList               * gnc_book_get_schedxactions( GNCBook *book );
AccountGroup        * gnc_book_get_template_group( GNCBook *book );

/* The gnc_book_set_data() and gnc_book_get_data() routines allow
 *    arbitrary pointers to structs to be stored in GNCBook.
 *    This is the "prefered" method for extending GNCBook to hold
 *    new data types.
 */
void gnc_book_set_data (GNCBook *book, const char *key, gpointer data);
gpointer gnc_book_get_data (GNCBook *book, const char *key);

/*
 * The gnc_book_not_saved() subroutine will return TRUE if any 
 *    data in the book hasn't been saved to long-term storage.
 *    (Actually, that's not quite true.  The book doesn't know 
 *    anything about saving.  Its just that whenever data is modified,
 *    the 'dirty' flag is set.  This routine returns the value of the 
 *    'dirty' flag.  Its up to the backend to periodically reset this 
 *    flag, when it acutally does save the data.)
 */
gboolean gnc_book_not_saved (GNCBook *book);

/* The gnc_book_equal() method returns TRUE if the engine data
 * in the two given books is equal. */
gboolean gnc_book_equal (GNCBook *book_1, GNCBook *book_2);

/* XXX FIXME count_transactions is a utility function, needs 
 * to be moved to some utility/support file.  */
guint gnc_book_count_transactions(GNCBook *book);

#endif /* GNC_BOOK_H */
