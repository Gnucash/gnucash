/********************************************************************\
 * PostgresBackend.h -- implements postgres backend                 *
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

#include <libpq-fe.h>

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
   char session_guid_str[GUID_ENCODING_LENGTH+1];

   /* sql query compiler */
   sqlBuilder *builder;

   /* postgres-specific connection data */
   char * hostname;
   char * portno;
   char * username;
   char * dbName;
   PGconn * connection;

   /* counter used to nest callback disables */
   int nest_count;
   /* callback hooks are saved in snr during disables */
   Backend snr;    

   /* my postgres backend pid, used for telling apart notifies */
   int my_pid;

   /* notify counters */
   int do_account;
   int do_checkpoint;
   int do_price;
   int do_session;
   int do_transaction;

   /* notify dates */
   Timespec last_account;
   Timespec last_price;
   Timespec last_transaction;

   /* scratch space for constructing queries */ 
   int bufflen;
   char *buff;
   int nrows;  /* number of rows in query result */

   /* kvp path cache */
   char **path_cache;
   int path_cache_size;
   int ipath_max;

   /* enginge data caches -- not used anywhere except in session_end */
   AccountGroup *topgroup;
};

/*
 * pgendNew creates a new postgress backend
 */
Backend * pgendNew (void);

void pgendDisable (PGBackend *be);
void pgendEnable (PGBackend *be);

void pgendStoreOneTransactionOnly (PGBackend *be, Transaction *ptr, sqlBuild_QType update);

void pgendPutOneAccountOnly (PGBackend *be, Account *ptr);
void pgendPutOneCommodityOnly (PGBackend *be, gnc_commodity *ptr);
void pgendPutOnePriceOnly (PGBackend *be, GNCPrice *ptr);
void pgendPutOneSplitOnly (PGBackend *be, Split *ptr);
void pgendPutOneTransactionOnly (PGBackend *be, Transaction *ptr);

int pgendAccountCompareVersion (PGBackend *be, Account *ptr);
int pgendPriceCompareVersion (PGBackend *be, GNCPrice *ptr);
int pgendTransactionCompareVersion (PGBackend *be, Transaction *ptr);

void pgendStoreAuditAccount (PGBackend *be, Account *ptr, sqlBuild_QType update);
void pgendStoreAuditPrice (PGBackend *be, GNCPrice *ptr, sqlBuild_QType update);
void pgendStoreAuditSplit (PGBackend *be, Split *ptr, sqlBuild_QType update);
void pgendStoreAuditTransaction (PGBackend *be, Transaction *ptr, sqlBuild_QType update);

int pgendTransactionGetDeletedVersion (PGBackend *be, Transaction *ptr);

#endif /* __POSTGRES_BACKEND_H__ */
