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
