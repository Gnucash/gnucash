/********************************************************************\
 * io-gncbin.h -- read and write (old format) binary datafile       *
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

#ifndef __IO_GNCBIN_H__
#define __IO_GNCBIN_H__

#include "Backend.h"
#include "Group.h"
#include "FileIO.h"

/** PROTOTYPES ******************************************************/

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
 * The xaccReadOldBinAccountGroupFile() method will read the "xacc"
 *    format byte stream from the indicated file, and build and return
 *    the corresponding AccountGroup structure.
 *
 *    If a read error occurred during reading, the returned value 
 *    may or may not be null. Use the xaccGetOldBinFileIOError() routine 
 *    to check for read errors.
 *
 * The xaccGetOldBinFileIOError() method will return an error code for
 *    any error detected that occured during reading or writing.  It
 *    will reset the error code after being called.  The current
 *    implementation can be thought of as a "stack of depth one", and
 *    this routine as a "pop".  Future implementations may have a
 *    deeper stack.
 *     */
AccountGroup  *xaccReadGncBinAccountGroupFile  (const char *filename);
GNCBackendError xaccGetGncBinFileIOError (void);

#endif /* __IO_GNCBIN_H__ */
