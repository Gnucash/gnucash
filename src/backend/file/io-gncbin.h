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

#ifndef IO_GNCBIN_H
#define IO_GNCBIN_H

#include "Backend.h"
#include "gnc-book.h"

/** PROTOTYPES ******************************************************/

/*
 * NOTE: These routines should not be used directly for file IO.  They
 *    are not inherently safe against file-locking errors.  For direct
 *    file IO, the gnc-book's higher level functions should be used.
 *
 * gnc_book_load_from_binfile() will load the financial data
 *   represented by the book's file_path into the indicated book.
 *
 * gnc_book_get_binfile_io_error() will return an error code for any
 *   error detected that occured during reading or writing.  It will
 *   reset the error code after being called.  The current
 *   implementation can be thought of as a "stack of depth one", and
 *   this routine as a "pop".  Future implementations may have a
 *   deeper stack.
 *     */
void            gnc_book_load_from_binfile(GNCBook *book);
GNCBackendError gnc_book_get_binfile_io_error(void);

#endif /* IO_GNCBIN_H */
