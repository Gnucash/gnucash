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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/**
 * @file io-gncxml.h
 * @brief api for Version 1 XML-based file format
 *
 * Initial code by Rob L. Browning 4Q 2000
 * Tuneups by James LewisMoss Dec 2000
 */

#ifndef IO_GNCXML_H
#define IO_GNCXML_H

#include <glib.h>
#include "qof.h"

/* FIXME: eventually, we probably need to add an error stack
   accessable via gnc_book_get_xml_io_error() a la binfile. */

/** Read in an account group from a file */
gboolean qof_session_load_from_xml_file(QofBook *, const char * filename);

/** The is_gncxml_file() routine checks to see if the first few
 * chars of the file look like gnc-xml data.
 */
gboolean gnc_is_xml_data_file(const gchar *name);

#endif /* IO_GNCXML_H */
