/********************************************************************\
 * Backend.c -- utility routines for dealing with the data backend  *
 * Copyright (C) 2000 Linas Vepstas <linas@linas.org>               *
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

#include "config.h"

#include <glib.h>

#include "Account.h"
#include "AccountP.h"
#include "BackendP.h"
#include "Group.h"
#include "GroupP.h"
#include "gnc-book-p.h"
#include "gnc-engine-util.h"
#include "gnc-pricedb.h"
#include "gnc-pricedb-p.h"
#include "TransactionP.h"

/* static short module = MOD_ENGINE; */

/********************************************************************\
 * error handling                                                   *
\********************************************************************/

void 
xaccBackendSetError (Backend *be, GNCBackendError err)
{
   if (!be) return;

   /* use stack-push semantics. Only the earliest error counts */
   if (ERR_BACKEND_NO_ERR != be->last_err) return;
   be->last_err = err;
}

GNCBackendError 
xaccBackendGetError (Backend *be)
{
   GNCBackendError err;
   if (!be) return ERR_BACKEND_NO_BACKEND;

   /* use 'stack-pop' semantics */
   err = be->last_err;
   be->last_err = ERR_BACKEND_NO_ERR;
   return err;
}

/********************************************************************\
 * Fetch the backend                                                *
\********************************************************************/

Backend *
xaccAccountGetBackend (Account * acc)
{
  if (!acc || !acc->book) return NULL;
  return acc->book->backend;
}

Backend *
xaccGroupGetBackend (AccountGroup *grp)
{
  grp = xaccGroupGetRoot (grp);
  if (!grp || !grp->book) return NULL;
  return grp->book->backend;
}

Backend *
xaccPriceDBGetBackend (GNCPriceDB *prdb)
{
  if (!prdb || !prdb->book) return NULL;
  return prdb->book->backend;
}

Backend *
xaccTransactionGetBackend (Transaction *trans)
{
  if (!trans || !trans->book) return NULL;
  return trans->book->backend;
}

/***********************************************************************/
/* Get a clean backend */
void
xaccInitBackend(Backend *be)
{
    be->session_begin = NULL;
    be->book_load = NULL;
    be->price_load = NULL;
    be->session_end = NULL;
    be->destroy_backend = NULL;

    be->account_begin_edit = NULL;
    be->account_commit_edit = NULL;
    be->trans_begin_edit = NULL;
    be->trans_commit_edit = NULL;
    be->trans_rollback_edit = NULL;
    be->price_begin_edit = NULL;
    be->price_commit_edit = NULL;

    be->run_query = NULL;
    be->price_lookup = NULL;
    be->sync_all = NULL;
    be->sync_group = NULL;
    be->sync_price = NULL;

    be->events_pending = NULL;
    be->process_events = NULL;

    be->book_transfer_begin = NULL;
    be->book_transfer_commit = NULL;

    be->last_err = ERR_BACKEND_NO_ERR;
}

/************************* END OF FILE ********************************/
