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

#include "Group.h"
#include "Backend.h"
#include "BackendP.h"
#include "gnc-pricedb.h"
#include "TransLog.h"
#include "gnc-engine-util.h"
#include "gnc-pricedb-p.h"
#include "DateUtils.h"

#include "gnc-engine.h"
#include "gnc-engine-util.h"
#include "gnc-book.h"
#include "gnc-pricedb.h"
#include "Group.h"

#define GNC_BACKEND_INTERFACE 0

struct gnc_book_struct
{
  AccountGroup *topgroup;
  GNCPriceDB *pricedb;
  GList *sched_xactions;
  AccountGroup *template_group;

  /*
   * should be set true if sched_xactions is changed
   * before saving
   */

  gboolean sx_notsaved; 
 
  /* the requested book id, in the form or a URI, such as
   * file:/some/where, or sql:server.host.com:555
   */
  char *book_id;

  /* if any book subroutine failed, this records the failure reason 
   * (file not found, etc).
   * This is a 'stack' that is one deep.
   * FIXME: This is a hack.  I'm trying to move us away from static
   * global vars. This may be a temp fix if we decide to integrate
   * FileIO errors into GNCBook errors. 
   */
  GNCBackendError last_err;
  char *error_message;

  char *fullpath;

  /* ---------------------------------------------------- */
  /* This struct member applies for network, rpc and SQL i/o */
  /* It is not currently used for file i/o, but it should be. */
  Backend *backend;
};


void gnc_book_set_group(GNCBook *book, AccountGroup *grp);
void gnc_book_set_pricedb(GNCBook *book, GNCPriceDB *db);

/*
 * used by backends to mark the notsaved as FALSE just after 
 * loading.  Do not use otherwise!
 */


void gnc_book_mark_saved(GNCBook *book);

void gnc_book_push_error (GNCBook *book, GNCBackendError err,
                          const char *message);

Backend* gncBackendInit_file(const char *book_id, void *data);

#endif /* GNC_BOOK_P_H */
