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
  /* Unique guid for this book */
  GUID guid;

  /* The kvp_frame providea a place for top-level data associated 
   * with this book. */
  kvp_frame *kvp_data;
  
  /* The entity table associates the GUIDs of all the objects
   * created in the session with their respective objects
   * (pointer addresses) */
  GNCEntityTable *entity_table;

  /* Pointers to top-level data structures */
  AccountGroup *topgroup;
  GNCPriceDB *pricedb;

  GList *sched_xactions;
  AccountGroup *template_group;
  gboolean sx_notsaved; /* true if sched_xactions is changed */

  gnc_commodity_table *commodity_table;

  /* To be technically correct, backends belong to sessions and
   * not books.  So the pointer below "really shouldn't be here", 
   * except that it provides a nice convenience, avoiding a lookup 
   * from the session.  Better solutions welcome ... */ 
  Backend *backend;
};

void gnc_book_set_group(GNCBook *book, AccountGroup *grp);
void gnc_book_set_pricedb(GNCBook *book, GNCPriceDB *db);
void gnc_book_set_schedxactions( GNCBook *book, GList *newList );
void gnc_book_set_template_group( GNCBook *book, AccountGroup *templateGroup );

void gnc_book_set_backend (GNCBook *book, Backend *be);

GNCEntityTable * gnc_book_get_entity_table (GNCBook *book);

/*
 * used by backends to mark the notsaved as FALSE just after 
 * loading.  Do not use otherwise!
 */

void gnc_book_mark_saved(GNCBook *book);

#endif /* GNC_BOOK_P_H */
