/********************************************************************\
 * kvp-sql.h : store KVP frames in SQL db                           *
 * Copyright (C) 2001 Linas Vepstas <linas@linas.org>               *
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
\********************************************************************/

/*
 * FILE:
 * kvp-sql.h
 *
 * FUNCTION:
 * save & restore of KVP frames
 *
 * HISTORY:
 * Copyright (c) 2001 Linas Vepstas
 */

#ifndef __KVP_SQL_H__
#define __KVP_SQL_H__

#include "kvp_frame.h"
#include "guid.h"
#include "PostgresBackend.h"

/*
 * The pgendKVPInit() routine initializes a local cache. This routine
 *    must be called before any db access that might require kvp 
 *    handling (e.g. a fetch of accounts).
 */
void pgendKVPInit (PGBackend *);

/* The pgendKVPStore() routine copies the contents of the kvp-frame
 *    to the SQL database, associating the root of the kvp-frame
 *    with the indicated GUID.
 *    (Note that currently it does not delete excess kvp data.
 *    That is, if the database has more kvp data in it than
 *    what was passed to this routine, then it does not delete 
 *    the excess. This should be considered to be a bug that needs
 *    fixing.)
 *
 * The pgendKVPDelete() and pgendKVPDeleteStr() routines delete
 *    all kvp data in the database associated with the indicated 
 *    GUID.
 *
 * The pgendKVPFetch() routine pulls kvp data out of the database.
 */

void pgendKVPStore (PGBackend *, const GUID *, kvp_frame *);
void pgendKVPDelete (PGBackend *, const GUID *);
void pgendKVPDeleteStr (PGBackend *, const char *guid_string);

kvp_frame * pgendKVPFetch (PGBackend *, const GUID *, kvp_frame *);

#endif /* __KVP_SQL_H__ */
