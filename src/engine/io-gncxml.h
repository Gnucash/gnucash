/********************************************************************\
 * io-gncxml.h -- api for gnucash xml i/o                           *
 *                                                                  *
 * Copyright (c) 2000,2001 Gnumatic Incorporated                    *
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
 *                                                                  *
\********************************************************************/

/*
 * io-gncxml.h -- api for new XML-based file format
 *
 * Initial code by Rob l. Browning 4Q 2000
 * Tuneups by James LewisMoss Dec 2000
 */

#ifndef __IO_GNCXML_H__
#define __IO_GNCXML_H__

#include <glib.h>

#include "gnc-book.h"
#include "Query.h"

/* FIXME: eventually, we probably need to add an error stack
   accessable via gnc_book_get_xml_io_error() a la binfile. */

/* read in an account group from a file */
gboolean gnc_book_load_from_xml_file(GNCBook *book);

/* write all account info to a file */
gboolean gnc_book_write_to_xml_file(GNCBook *book, const char *filename);

#if 0

/* read an account group from a buffer in memory 
 * the pointer bufp must point at the ascii xml, and bufsz
 * must be the size of the buffer.
 */
AccountGroup *gncxml_read_from_buf (char *bufp, int bufsz);

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

#endif

/* write/read query terms to/from memory */
void gncxml_write_query_to_buf (Query *q, char **bufp, int *sz);
Query *gncxml_read_query (char *bufp, int bufsz);

/* The is_gncxml_file() routine checks to see if the first few 
 * chars of the file look like gnc-xml data.
 */
gboolean gnc_is_xml_data_file(const gchar *name);
 
#endif /* __IO_GNCXML_H__ */
