/* 
 * FILE:
 * Backend.h
 *
 * FUNCTION:
 * Pseudo-object defining how the engine can interact with different
 * back-ends (which may be SQL databases, or network interfaces to 
 * remote gnucash servers.  In theory, file-io should be a type of 
 * backend).
 * 
 * The callbacks will be called at the appropriate times during 
 * a book session to allow the backend to store the data as needed.
 *
 */

#ifndef __XACC_BACKEND_H__
#define __XACC_BACKEND_H__

#include "config.h"


typedef enum {
  ERR_BACKEND_NO_ERR = 0,
  ERR_BACKEND_NO_BACKEND,   /* Backend * pointer was null the err routine */
                            /* or no backend handler (ENOSYS) */
  ERR_BACKEND_LOCKED,       /* in use by another user (ETXTBSY) */
  ERR_BACKEND_MISC,         /* undetermined error */

  /* fileio errors */
  ERR_FILEIO_FILE_BAD_READ,
  ERR_FILEIO_FILE_EMPTY,
  ERR_FILEIO_FILE_LOCKED,
  ERR_FILEIO_FILE_NOT_FOUND,
  ERR_FILEIO_FILE_TOO_NEW,
  ERR_FILEIO_FILE_TOO_OLD,
  ERR_FILEIO_ALLOC,
  ERR_FILEIO_MISC,              /* unknown weird error */

  /* network errors */
  ERR_NETIO_NO_CONNECTION,      /* network failure */
  ERR_NETIO_SHORT_READ,         /* not enough bytes received */
  ERR_NETIO_WRONG_CONTENT_TYPE, /* wrong kind of server, wrong data served */
  ERR_NETIO_NOT_GNCXML,

  /* database errors */
  ERR_SQL_BUSY,              /* single-mode access doesn't allow other users */
  ERR_SQL_CANT_CONNECT,      /* network failure */
  ERR_SQL_SEND_QUERY_FAILED,
  ERR_SQL_FINISH_QUERY_FAILED,
  ERR_SQL_GET_RESULT_FAILED,
  ERR_SQL_CORRUPT_DB,
  ERR_SQL_MISSING_DATA,     /* database doesn't contain expected data */
} GNCBackendError;


#endif /* __XACC_BACKEND_H__ */
