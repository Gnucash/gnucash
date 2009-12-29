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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

/*
 * FILE:
 * PostgresBackend.h
 *
 * FUNCTION:
 * Implements the callbacks for the postgres backend.
 *
 * HISTORY:
 * Copyright (c) 2000, 2001, 2002 Linas Vepstas <linas@linas.org>
 */


#ifndef POSTGRES_BACKEND_H
#define POSTGRES_BACKEND_H

#include <gmodule.h>
#include <libpq-fe.h>

#include "qof.h"
#include "Transaction.h"

#include "builder.h"
#include "qof.h"

#define GNC_MOD_TXN    "gnucash-postgres-transaction"

typedef struct _pgend PGBackend;

typedef enum
{
    MODE_NONE = 0,
    MODE_SINGLE_FILE = 1,
    MODE_SINGLE_UPDATE,
    MODE_POLL,
    MODE_EVENT
} AccessMode;

#define MAX_VERSION_AGE 10

#include "qofbackend-p.h"
struct _pgend
{
    QofBackend be;

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
    gboolean freshly_created_db;
    gboolean freshly_created_prdb;

    /* counter used to nest callback disables */
    int nest_count;
    /* callback hooks are saved in snr during disables */
    QofBackend snr;

    /* my postgres backend pid, used for telling apart notifies */
    int my_pid;

    /* notify counters */
    int do_account;
    int do_book;
    int do_checkpoint;
    int do_price;
    int do_session;
    int do_transaction;

    /* notify dates */
    Timespec last_account;
    Timespec last_price;
    Timespec last_transaction;

    guint32 version_check; /* data aging timestamp */

    /* scratch space for constructing queries */
    int bufflen;
    char *buff;
    int nrows;  /* number of rows in query result */

    /* kvp path cache */
    char **path_cache;
    int path_cache_size;
    int ipath_max;

    /* engine data caches */
    QofSession *session;
    QofBook *book;  /* the currently open book -- XXX -- depricate ???*/
    QofBookList *blist;  /* list of books in this db */

    GList *tmp_return;
};

/*
 * pgendNew creates a new postgres backend
 */
QofBackend * pgendNew (void);

Account * pgendAccountLookup (PGBackend *be, const GUID *acct_guid);
Transaction * pgendTransLookup (PGBackend *be, const GUID *txn_guid);
Split * pgendSplitLookup (PGBackend *be, const GUID *split_guid);
QofIdType pgendGUIDType (PGBackend *be, const GUID *guid);
QofBook * pgendGetBook(PGBackend *pbe);

void pgendDisable (PGBackend *be);
void pgendEnable (PGBackend *be);

G_MODULE_EXPORT void
qof_backend_module_init(void);

#endif /* POSTGRES_BACKEND_H */
