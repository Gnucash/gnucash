/*
 * io-gncxml.h -- api for new XML-based file format
 *
 * Initial code by Rob l. Browning 4Q 2000
 * Tuneups by James Lewis Moss Dec 2000
 *
 * Copyright (c) 2000,2001 Gnumatic Incorporated
 *
 */
#ifndef __IO_GNCXML_H__
#define __IO_GNCXML_H__

#include <glib.h>

#include "Group.h"
#include "Query.h"

/* read in an account group from a file */
gboolean gncxml_read(const gchar *filename, AccountGroup **result_group);


/* read an account group from a buffer in memory 
 * the pointer bufp must point at the ascii xml, and bufsz
 * must be the size of the buffer.
 */
AccountGroup *
gncxml_read_from_buf (char *bufp, int bufsz);

Query *
gncxml_read_query (char *bufp, int bufsz);

/* write all account info to a file */
gboolean gncxml_write(AccountGroup *group, const gchar *name);

/* The is_gncxml_file() routine checks to see if the first few 
 * chars of the file look like gnc-xml data.
 */
gboolean is_gncxml_file(const gchar *name);

/* write all account info into memory.  This routine returns a
 * pointer to freshly malloced memory in bufp. You muse free
 * this memory when done.  The size that was written out is
 * returned in sz
 */
void gncxml_write_to_buf (AccountGroup *group, char **bufp, int *sz);

/*
 * write only the account and currency info to the buf, do *not*
 * write any of the splits or transactions
 */
void gncxml_write_group_to_buf (AccountGroup *group, char **bufp, int *sz);

/* write the query terms to memory */
void gncxml_write_query_to_buf (Query *q, char **bufp, int *sz);

#endif /* __IO_GNCXML_H__ */
