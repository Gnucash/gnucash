/* 
 * PostgressBackend.h
 *
 * Implements the callbacks for the postgress backend.
 * 
 */


#include <pgsql/libpq-fe.h>
#include "BackendP.h"

typedef struct _pgend PGBackend;

struct _pgend {
   Backend be;

   /* postgres-specific conection data */
   char * dbName;
   PGconn * connection;
};

/*
 * pgendNew creates a new postgress backend
 */
Backend * pgendNew (void);

