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

#ifndef GNC_BOOK_P_H
#define GNC_BOOK_P_H

#include "BackendP.h"
#include "DateUtils.h"
#include "TransLog.h"
#include "gnc-book.h"
#include "gnc-engine-util.h"
#include "gnc-engine.h"
#include "gnc-pricedb-p.h"

struct gnc_book_struct
{
  AccountGroup *topgroup;
  GNCPriceDB *pricedb;

  GList *sched_xactions;
  AccountGroup *template_group;
  gboolean sx_notsaved; /* true if sched_xactions is changed */

  gnc_commodity_table *commodity_table;

  Backend *backend;
};


void gnc_book_set_group(GNCBook *book, AccountGroup *grp);
void gnc_book_set_pricedb(GNCBook *book, GNCPriceDB *db);

void gnc_book_set_backend (GNCBook *book, Backend *be);

/*
 * used by backends to mark the notsaved as FALSE just after 
 * loading.  Do not use otherwise!
 */

void gnc_book_mark_saved(GNCBook *book);

#endif /* GNC_BOOK_P_H */
