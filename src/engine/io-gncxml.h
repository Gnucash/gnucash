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

/* read in an account group from a file */
gboolean gncxml_read(const gchar *name, AccountGroup **result_group);

/* write all account info to a file */
gboolean gncxml_write(AccountGroup *group, const gchar *name);

/* The is_gncxml_file() routine checks to see if the first few 
 * chars of the file look like gnc-xml data.
 */
gboolean is_gncxml_file(const gchar *name);

void gncxml_write_to_buf (AccountGroup *group, char **bufp, int *sz);

#endif /* __IO_GNCXML_H__ */
