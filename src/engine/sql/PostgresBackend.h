/* 
 * PostgresBackend.h
 *
 * Implements the callbacks for the postgres backend.
 * 
 */


#include <pgsql/libpq-fe.h>
#include "BackendP.h"

typedef struct _pgend PGBackend;

struct _pgend {
   Backend be;

   /* sql query compiler */
   sqlBuilder *builder;

   /* postgres-specific connection data */
   char * hostname;
   char * portno;
   char * dbName;
   PGconn * connection;

   /* scratch space for constructing queries */ 
   int bufflen;
   char *buff;

   /* counter used to nest callback disables */
   int nest_count;
};

/*
 * pgendNew creates a new postgress backend
 */
Backend * pgendNew (void);

