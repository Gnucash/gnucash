/********************************************************************\
 * FileIO.h -- read and write binary format file for gnucash        *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998, 1999 Linas Vepstas                           *
 * Copyright (C) 1999, 2000 Rob Browning                            *
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
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#ifndef __FILE_IO_H__
#define __FILE_IO_H__

#include "Group.h"

typedef enum {
  ERR_FILEIO_NONE = 0,
  ERR_FILEIO_MISC,
  ERR_FILEIO_FILE_BAD_READ,
  ERR_FILEIO_FILE_EMPTY,
  ERR_FILEIO_FILE_NOT_FOUND,
  ERR_FILEIO_FILE_TOO_NEW,
  ERR_FILEIO_FILE_TOO_OLD,
  ERR_FILEIO_ALLOC
} GNCFileIOError;

/*
  
  These function read/write the data in an AccountGroup to a file.
  The read functions will automatically detect the format of the file
  if possible.  The write functions write the file in the "current"
  format.  These days, that means XML.

  The read functions return NULL on error, and set the error parameter
  (if it's not NULL) to indicate what went wrong.

  The write functions return FALSE on error and set the error
  parameter similarly.

  In most cases, these functions should not be used directly.  They
  are not "safe" against file-locking errors.  Use the Session object
  instead.

*/

AccountGroup *xaccReadAccountGroupFile(const char *datafile,
                                       GNCFileIOError *error);

gboolean      xaccWriteAccountGroupFile(const char *datafile,
                                        AccountGroup *grp,
                                        gboolean make_backup,
                                        GNCFileIOError *error);
/* If make_backup is true, write out a time-stamped copy of the file
   into the same directory as the indicated file, with a filename of
   "file.YYYYMMDDHHMMSS.xac" where YYYYMMDDHHMMSS is replaced with the
   current year/month/day/hour/minute/second. */

#endif /* __XACC_FILEIO_H__ */
