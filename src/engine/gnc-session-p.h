/********************************************************************\
 * gnc-session-p.h -- private functions for gnc sessions.           *
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
 * Copyright (c) 2001 Linux Developers Group
 */

#ifndef GNC_SESSION_P_H
#define GNC_SESSION_P_H

#include "BackendP.h"
#include "GNCIdP.h"
#include "TransLog.h"
#include "gnc-engine-util.h"
#include "gnc-engine.h"
#include "gnc-session.h"

struct gnc_session_struct
{
  /* The book is the cache for the underlying gnucash dataset. */
  GNCBook *book;

  /* The entity table associates the GUIDs of all the objects
   * created in the session with their respective objects
   * (pointer addresses) */
  GNCEntityTable *entity_table;

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
  char *logpath;

  /* ---------------------------------------------------- */
  /* This struct member applies for network, rpc and SQL i/o */
  /* It is not currently used for file i/o, but it should be. */
  Backend *backend;
};


void gnc_session_set_book (GNCSession *session, GNCBook *book);

GNCEntityTable *gnc_session_get_entity_table (GNCSession *session);

Backend * gnc_session_get_backend (GNCSession *session);

/*
 * used by backends to mark the notsaved as FALSE just after 
 * loading.  Do not use otherwise!
 */


void gnc_session_push_error (GNCSession *session, GNCBackendError err,
                             const char *message);

Backend* gncBackendInit_file(const char *book_id, void *data);

#endif

