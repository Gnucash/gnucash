/********************************************************************\
 * FileIO.h -- read from and writing to a datafile for gnucash      *
 *             (X-Accountant)                                       *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998, 1999 Linas Vepstas                           *
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

#ifndef __XACC_FILEIO_H__
#define __XACC_FILEIO_H__

#include "Group.h"

#define ERR_FILEIO_NO_ERROR          0
#define ERR_FILEIO_FILE_BAD_READ     1 
#define ERR_FILEIO_FILE_EMPTY        2 
#define ERR_FILEIO_FILE_NOT_FOUND    3
#define ERR_FILEIO_FILE_TOO_NEW      4
#define ERR_FILEIO_FILE_TOO_OLD      5

/** PROTOTYPES ******************************************************/

AccountGroup *xaccReadQIFAccountGroup (char *datafile);
int           xaccGetQIFIOError (void);

/*
 * The xaccReadAccountGroupFD() and xaccWriteAccountGroupFD()
 * routines read and write the GnuCash "xacc" byte stream (file) 
 * format.  This is a binary format that exactly represents all of the 
 * data that can appear in the AccountGroup structure as a sequence of 
 * bytes.  The Read and Write routines are exact inverses of each other: 
 * that is, there is no loss of data involved in converting an 
 * AccountGroup into a byte stream and back again.  These routines
 * can be thought of as implementing a kind of "object persistance"
 * for the AccountGroup object.  Note that these routines can also
 * be used to provide inter-process communication using either pipes or 
 * sockets.  That is, by writing into a socket or pipe with the 
 * xaccWriteAccountGroupFD() routine, and reading from it at the other 
 * end with the xaccReadAccountGroupFD() routine, an exact duplicate of 
 * the AccountGroup can be created in a different process.
 *
 * NOTE: These routines should not be used directly for file IO.
 *    They are not inherently safe against file-locking errors.
 *    For direct file IO, the Session object should be used.
 *
 * The xaccReadAccountGroupFD() method will read the "xacc" format 
 *    byte stream from the indicated file descriptor, and build
 *    the corresponding AccountGroup structure.  The file descriptor 
 *    must have been previously opened for reading.  The fd may be a 
 *    pipe or a socket.  This routine returns a pointer to the 
 *    resulting group.  
 *
 *    If a read error occurred during reading, the returned value 
 *    may or may not be null. Use the xaccGetFileIOError() routine 
 *    to check for read errors.
 *
 * The xaccWriteAccountGroupFD() method will convert the indicated
 *    account group into the "xacc" byte stream and write it out to the 
 *    indicated file descriptor.
 *    Returns a negative number if an error occured.
 *
 * The xaccGetFileIOError() method will return an error code for any
 *    error detected that occured during reading or writing.  It will 
 *    reset the error code after being called.
 *    The current implementation can be thought of as a "stack of 
 *    depth one", and this routine as a "pop".  Future implementations 
 *    may have a deeper stack.
 *    
 */
AccountGroup *xaccReadAccountGroup  (int fd);
int           xaccWriteAccountGroup (int fd, AccountGroup *grp);
int           xaccGetFileIOError (void);

#endif /* __XACC_FILEIO_H__ */
