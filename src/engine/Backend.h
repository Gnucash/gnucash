/********************************************************************\
 * Backend.h -- api for engine Backend                              *
 *                                                                  *
 * Copyright (c) 2000, 2001 Linas Vepstas <linas@linas.org>         *
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
 * Backend.h
 *
 * FUNCTION:
 * The 'backend' is a pseudo-object providing an interface between the
 * engine and a persistant data store (e.g. a server, a database, or
 * a file).  There are no backend functions that are 'public' to
 * users of the engine.  The backend can, however, report errors to
 * the GUI & other front-end users.  This file defines these errors.
 */

#ifndef __XACC_BACKEND_H__
#define __XACC_BACKEND_H__

#include "config.h"

/* NOTE: if you modify GNCBackendError, please update src/scm/gnc.gwp */
typedef enum {
  ERR_BACKEND_NO_ERR = 0,
  ERR_BACKEND_NO_BACKEND,   /* Backend * pointer was null the err routine */
                            /* or no backend handler (ENOSYS) */
  ERR_BACKEND_LOCKED,       /* in use by another user (ETXTBSY) */
  ERR_BACKEND_NO_SUCH_DB,   /* the named database doesn't exist */
  ERR_BACKEND_ALLOC,        /* internal memory allocation failure */
  ERR_BACKEND_MISC,         /* undetermined error */

  /* fileio errors */
  ERR_FILEIO_FILE_BAD_READ,  /* read failed or file prematurely truncated */
  ERR_FILEIO_FILE_EMPTY,     /* file exists, is readable, but is empty */
  ERR_FILEIO_FILE_LOCKERR,   /* mangled locks (unspecified error) */
  ERR_FILEIO_FILE_NOT_FOUND, /* not found / no such file */
  ERR_FILEIO_FILE_TOO_NEW,   /* file version newer than what we can read */
  ERR_FILEIO_FILE_TOO_OLD,   /* file version so old we can't read it */
  ERR_FILEIO_UNKNOWN_FILE_TYPE,
  
  /* network errors */
  ERR_NETIO_NO_CONNECTION,      /* network failure, can't connect to server */
  ERR_NETIO_SHORT_READ,         /* not enough bytes received */
  ERR_NETIO_WRONG_CONTENT_TYPE, /* wrong kind of server, wrong data served */
  ERR_NETIO_NOT_GNCXML,         /* whatever it is, we can't parse it. */

  /* database errors */
  ERR_SQL_BAD_LOCATION,        /* can't parse url */
  ERR_SQL_CANT_CONNECT,        /* bad dbname/login/passwd or network failure */
  ERR_SQL_SEND_QUERY_FAILED,   /* can't send to database */
  ERR_SQL_FINISH_QUERY_FAILED, /* can't finish out sent request */
  ERR_SQL_GET_RESULT_FAILED,   /* can't read response from the db. */
  ERR_SQL_CORRUPT_DB,          /* data in db is corrupt */
  ERR_SQL_MISSING_DATA,        /* database doesn't contain expected data */

  /* RPC errors */
  ERR_RPC_BAD_URL,		/* Can't parse url */
  ERR_RPC_HOST_UNK,		/* Host unknown */
  ERR_RPC_CANT_CONNECT,		/* bad hostname/port/dbname/etc. */
  ERR_RPC_CANT_BIND,		/* can't bind to address */
  ERR_RPC_CANT_ACCEPT,		/* can't accept connection */
  ERR_RPC_NO_CONNECTION,	/* no connection to server */
  ERR_RPC_CONNECTION_LOST,	/* Lost connection to server */
  ERR_RPC_BAD_VERSION,		/* RPC Version Mismatch */
  ERR_RPC_SERVER_STATE,		/* Invalid/bad server state */
  ERR_RPC_FAILED,		/* Operation failed */
  ERR_RPC_NOT_ADDED,		/* object not added */

} GNCBackendError;
/* NOTE: if you modify GNCBackendError, please update src/scm/gnc.gwp */


#endif /* __XACC_BACKEND_H__ */
