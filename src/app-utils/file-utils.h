/********************************************************************\
 * file-utils.h -- simple file utilities                            *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998 Linas Vepstas                                 *
 * Copyright (C) 1998 Rob Browning                                  *
 * Copyright (C) 2004 Derek Atkins <derek@ihtfp.com>                *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

/** @addtogroup Utils Utility functions
    @{ */
/** @addtogroup UtilFile File
 * @{ */
/** @file file-utils.h
 *  @brief  Utility functions for file access
 *  @author Copyright (C) 1997 Robin D. Clark
 *  @author Copyright (C) 1998 Linas Vepstas
 *  @author Copyright (C) 1998 Rob Browning
 *  @author Copyright (C) 2004 Derek Atkins <derek@ihtfp.com>
 *
 *  These functions help you to locate files that can reside
 *  in multiple locations.
 */

#ifndef GNC_FILE_UTILS_H
#define GNC_FILE_UTILS_H

#include <stdio.h>		/* for FILE* */

/** Locates a file in the search path of the program and
 * returns its absolute pathname.
 *
 * If the file is not found
 * this function will return NULL.
 *
 *  Uses the global xxxPath as the path to search
 *
 * @param filename The filename to search
 *
 * @return The absolute path to the file or NULL if the file
 * wasn't found.
 */
char * gncFindFile (const char * filename);

/** Reads the contents of a file into a buffer for further processing.
 *
 *  If the filename is not an absolute filename, it will be searched
 *  for in the search path available to the program. This can be used
 *  to find for example help files,...
 *
 *  Uses the global xxxPath as the path to search
 *
 *  @param filename the name of the html file to read
 *
 *  @param data Pointer to a buffer to store the file's content.
 *
 *  @return The file size
*/
int gncReadFile (const char * filename, char ** data);


/** Read a line from the input file, up to and including the newline.
 *
 *  The caller MUST g_free() the line returned from this call in all
 *  cases where it is non-NULL!
 *
 *  @param line pointer to hold the buffer for the whole line (allocated by
 *              this function)
 *
 *  @param file the file from which to read
 *
 *  @return The number of bytes read
 */
gint64 gnc_getline (gchar **line, FILE *file);


/* Definitions shared by file-utils.c and gnc-main-window.c */
#define STATE_FILE_TOP           "Top"
#define STATE_FILE_BOOK_GUID     "BookGuid"
#define STATE_FILE_BOOK_GUID_OLD "Book Guid"

/** Find the state file that corresponds to this URL and guid.
 *
 * The URL is used to compute the base name of the file (which will be in
 *  ~/.gnucash/books) and the guid is used to differentiate when the
 *  user has multiple data files with the same name.
 *
 *  @param url The url of the data file being used.
 *
 *  @param guid The guid of the book associated with this data file.
 *
 *  @param next_filename Return the next available file name if the
 *  data file cannot be found.
 *
 *  @return The name of the data file that was located.
 */
GKeyFile *gnc_find_state_file (const gchar *url, const gchar *guid, gchar **filename);

#endif /* GNC_FILE_UTILS_H */
/** @} */
/** @} */

