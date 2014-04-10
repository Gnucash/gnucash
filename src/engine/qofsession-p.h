/********************************************************************\
 * qofsession-p.h -- private functions for QOF sessions.            *
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
 * Copyright (c) 1998-2003 Linas Vepstas <linas@linas.org>
 */

#ifndef QOF_SESSION_P_H
#define QOF_SESSION_P_H

#include "qofbook.h"
#include "qofsession.h"

struct _QofSession
{
  /* A book holds pointers to the various types of datasets used
   * by GnuCash.  A session may have open multiple books.  */
  GList *books;

  /* The requested book id, in the form or a URI, such as
   * file:/some/where, or sql:server.host.com:555
   */
  char *book_id;

  /* If any book subroutine failed, this records the failure reason 
   * (file not found, etc).
   * This is a 'stack' that is one deep.  (Should be deeper ??)
   * FIXME: Each backend has its own error stack. The session
   * and the backends should all be using (or making it look like)
   * there is only one stack.
   */
  QofBackendError last_err;
  char *error_message;

  char *fullpath;
  char *logpath;

  /* ---------------------------------------------------- */
  /* Pointer to the backend that is actually used to move data
   * between the persistant store and the local engine.  */
  QofBackend *backend;
};


QofBackend * qof_session_get_backend (QofSession *session);

void qof_session_push_error (QofSession *session, QofBackendError err,
                             const char *message);

QofBackend* gncBackendInit_file(const char *book_id, void *data);

#endif

