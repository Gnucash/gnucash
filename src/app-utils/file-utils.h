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


#ifndef GNC_FILE_UTILS_H
#define GNC_FILE_UTILS_H

#include <stdio.h>		/* for FILE* */

char * gncFindFile (const char * filename);

/********************************************************************\
 * gncReadFile                                                      * 
 *                                                                  * 
 * Args:   file - the name of the html file to read                 *
 *         data - pointer to data pointer                           * 
 * Return: file size                                                * 
 * Global: xxxPath - the path to search                             * 
\********************************************************************/
int gncReadFile (const char * file, char ** data);


/**
 * gnc_getline -- read a line from the input file, up to and including
 *                the newline.
 *
 * Args:   line - pointer to hold the buffer for the whole line (allocated by
 *                this function)
 *         file - the file from which to read
 * Return: the number of bytes read
 *
 * The caller MUST g_free() the line returned from this call in all
 * cases where it is non-NULL!
 */
gint64 gnc_getline (gchar **line, FILE *file);


#endif /* GNC_FILE_UTILS_H */
