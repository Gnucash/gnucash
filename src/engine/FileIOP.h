/********************************************************************\
 * FileIOP.h -- private header to read from and writing to a        *
 *              datafile for gnucash (X-Accountant)                 *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998 Linas Vepstas                                 *
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
\********************************************************************/

#ifndef __XACC_FILEIO_P_H__
#define __XACC_FILEIO_P_H__

#include "Group.h"

/** PROTOTYPES ******************************************************/
/*
 * NOTE:
 * The Read and WriteAccountGroup routines should not be used directly.
 * They are not "safe" against file-locking errors.  Use the Session
 * object instead.
 *
 * These routines read and write the GnuCash "xac" byte stream (file) 
 * format.  This is a binary format that exactly represents all of the 
 * data that can appear in the AccountGroup structure as a sequence of 
 * bytes.  The Read and Write routines are exact inverses of each other: 
 * that is, there is no loss of data involved in converting an 
 * AccountGroup into a byte stream and back again.  These routines
 * can be thought of as implementing a kind of "object persistance"
 * for the AccountGroup object.  Note that these routines can also
 * be used to provide inter-process communication using either pipes or 
 * sockets.  That is, by writing into a socket or pipe with the 
 * xaccWriteAccountGroupFD() routine, and reading from it at the other end with
 * the xaccReadAccountGroupFD() routine, an exact duplicate of the
 * AccountGroup can be created in a different process.
 *
 * The xaccReadAccountGroup() method will open and read the indicated 
 *    filename.  It is expected that the file stores an "xac" format 
 *    gnucash data file.  It will return the contents of the file, 
 *    transcribed as an AccountGroup data structure.  If a read error
 *    occurred during reading, the returned value may or may not be 
 *    null. Use the xaccGetFileIOError() routine to check for read 
 *    errors.
 *
 * The xaccReadAccountGroupFD() subroutine works the same as the
 *    xaccReadAccountGroup() routine, except that it will read the 
 *    indicated file descriptor.  The file descriptor must have been 
 *    previously opened for reading.  The fd may be a pipe.  
 *
 * The xaccWriteAccountGroup() method will open the indicated
 *    file and write the indicated account group to it, in the 
 *    gnucash "xac" format.  It will also write out a time-stamped
 *    copy of the file into the same directory as the indicated file,
 *    with a filename of "file.YYYYMMDDHHMMSS.xac" where YYYYMMDDHHMMSS
 *    is replaced with the current year/month/day/hour/minute/second.
 *    Returns a negative number if an error occured.
 *
 * The xaccWriteAccountGroupFD() method will convert the indicated
 *    account group into the "xac" byte stream and write it out to the 
 *    indicated file descriptor.
 *    Returns a negative number if an error occured.
 */
AccountGroup *xaccReadAccountGroup  (char *datafile);
AccountGroup *xaccReadAccountGroupFD  (int fd);

int           xaccWriteAccountGroup (char *datafile, AccountGroup *grp);
int           xaccWriteAccountGroupFD (int fd, AccountGroup *grp);

#endif /* __XACC_FILEIO_P_H__ */
