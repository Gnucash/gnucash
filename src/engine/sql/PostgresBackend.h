/* 
 * FILE:
 * PostgresBackend.h
 *
 * FUNCTION:
 * Implements the callbacks for the postgres backend.
 *
 * HISTORY:
 * Copyright (c) 2000, 2001 Linas Vepstas 
 */


#ifndef __POSTGRES_BACKEND_H__
#define __POSTGRES_BACKEND_H__

#include <pgsql/libpq-fe.h>

#include "Group.h"
#include "guid.h"
#include "Transaction.h"

#include "builder.h"
#include "BackendP.h"

typedef struct _pgend PGBackend;

typedef enum {
   MODE_NONE = 0,
   MODE_SINGLE_FILE =1,
   MODE_SINGLE_UPDATE,
   MODE_POLL,
   MODE_EVENT
} AccessMode;

struct _pgend {
   Backend be;

   /* session mode */
   AccessMode session_mode;
   GUID *sessionGuid;

   /* sql query compiler */
   sqlBuilder *builder;

   /* postgres-specific connection data */
   char * hostname;
   char * portno;
   char * dbName;
   PGconn * connection;

   /* counter used to nest callback disables */
   int nest_count;
   /* callback hooks are saved in snr during disables */
   Backend snr;    


   /* scratch space for constructing queries */ 
   int bufflen;
   char *buff;
   int nrows;  /* number of rows in query result */

   /* kvp path cache */
   char **path_cache;
   int path_cache_size;
   int ipath_max;
};

/*
 * pgendNew creates a new postgress backend
 */
Backend * pgendNew (void);


/* -------------------------------------------------------- */
/* The balance checkpoint structure is used to store partial,
 * running balances.  The balances are correct for the checkpoint
 * date shown.  The commodity indicates what commodity the 
 * balances are valued in (they need not be in the same 
 * commodity as the account)
 */

/* the MIN_CHECKPOINT_COUNT value is the number of splits that
 * each checkpoint will handle, on avergage.  30 seems like a good
 * number.  The number of splits in a checkpoint will vary; 
 * checkpoints can onmly occur in between entry dates, so a 
 * bunch of entries with the same date will go into the same 
 * checkpoint (and there might be an arbitrarily large number of these)
 */
#define MIN_CHECKPOINT_COUNT 3

typedef struct _checkpoint {
   const GUID *account_guid;
   const char * commodity;
   Timespec datetime;
   gint64 balance;
   gint64 cleared_balance;
   gint64 reconciled_balance;
} Checkpoint;

/* -------------------------------------------------------- */
/* the following prototypes belong in a 'checkpoint.h' file */

void pgendGroupRecomputeAllCheckpoints (PGBackend *be, AccountGroup *grp);
void pgendGroupGetAllCheckpoints (PGBackend *be, AccountGroup*grp);

#endif /* __POSTGRES_BACKEND_H__ */
