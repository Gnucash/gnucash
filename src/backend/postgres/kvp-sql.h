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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
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

#ifndef KVP_SQL_H
#define KVP_SQL_H

#include "qof.h"
#include "PostgresBackend.h"

/*
 * The pgendKVPInit() routine initializes a local cache. This routine
 *    must be called before any db access that might require kvp
 *    handling (e.g. a fetch of accounts).
 */
void pgendKVPInit (PGBackend *);

/* The pgendKVPStore() routine copies the contents of the kvp-frame
 *    to the SQL database, associating the root of the kvp-frame
 *    with the indicated GUID cache index 'iguid'.
 *    (Note that currently it does not delete excess kvp data.
 *    That is, if the database has more kvp data in it than
 *    what was passed to this routine, then it does not delete
 *    the excess. This should be considered to be a bug that needs
 *    fixing.)
 *
 * The pgendKVPDelete() and pgendKVPDeleteStr() routines delete
 *    all kvp data in the database associated with the indicated
 *    GUID cache index 'iguid'.
 *
 * The pgendKVPFetch() routine pulls kvp data out of the database.
 *
 * The pgendNewGUIDidx() routine generates a new number suitable for
 *    use as a GUID cache index.
 */

void pgendKVPStore (PGBackend *, guint32 iguid, KvpFrame *);
void pgendKVPDelete (PGBackend *, guint32 iguid);

KvpFrame * pgendKVPFetch (PGBackend *, guint32 iguid, KvpFrame *);

guint32 pgendNewGUIDidx (PGBackend *be);


#endif /* KVP_SQL_H */
