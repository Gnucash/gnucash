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
 * Copyright (c) 2001 Linas Vepstas <linas@linas.org>
 */

#ifndef GNC_BOOK_P_H
#define GNC_BOOK_P_H

#include "Backend.h"
#include "GNCIdP.h"
#include "gnc-book.h"
#include "gnc-engine.h"
#include "kvp_frame.h"

struct gnc_book_struct
{
  /* Unique guid for this book. */
  GUID guid;

  /* The kvp_frame provides a place for top-level data associated 
   * with this book. */
  kvp_frame *kvp_data;
  
  /* The entity table associates the GUIDs of all the objects
   * belonging to this book, with their pointers to the respective
	* objects.  This allows a lookup of objects based on thier guid.
   */
  GNCEntityTable *entity_table;

  /* Pointers to top-level data structures. */
  AccountGroup *topgroup;
  GNCPriceDB *pricedb;

  GList *sched_xactions;
  AccountGroup *template_group;
  gboolean sx_notsaved; /* true if sched_xactions is changed */

  gnc_commodity_table *commodity_table;

  /* In order to store arbitrary data, for extensibility, add a table
   * that will be used to hold arbitrary pointers.
   */
  GHashTable *data_tables;

  /* state flag: 'y' means 'open for editing', 
   * 'n' means 'book is closed'  
   */
  char book_open;

  /* version number, used for tracking multiuser updates */
  gint32  version;

  /* To be technically correct, backends belong to sessions and
   * not books.  So the pointer below "really shouldn't be here", 
   * except that it provides a nice convenience, avoiding a lookup 
   * from the session.  Better solutions welcome ... */ 
  Backend *backend;

  /* -------------------------------------------------------------- */
  /* Backend private expansion data */
  guint32  idata;     /* used by the sql backend for kvp management */
};

/*
 * These gnc_book_set_*() routines are used by backends to 
 *    initialize the pointers in the book structure to 
 *    something that contains actual data.  These routines 
 *    should not be used otherwise.  (Its somewhat questionable
 *    if the backends should even be doing this much, but for
 *    backwards compatibility, we leave these here.)
 */
void gnc_book_set_guid(GNCBook *book, GUID guid);
void gnc_book_set_group(GNCBook *book, AccountGroup *grp);
void gnc_book_set_pricedb(GNCBook *book, GNCPriceDB *db);
void gnc_book_set_schedxactions( GNCBook *book, GList *newList );
void gnc_book_set_template_group( GNCBook *book, AccountGroup *templateGroup );

void gnc_book_set_backend (GNCBook *book, Backend *be);

GNCEntityTable * gnc_book_get_entity_table (GNCBook *book);

/* The gnc_book_mark_saved() routine marks the book as having been
 *    saved (to a file, to a database). Used by backends to mark the 
 *    notsaved flag as FALSE just after loading.  Do not use otherwise!
 */
void gnc_book_mark_saved(GNCBook *book);

#endif /* GNC_BOOK_P_H */
