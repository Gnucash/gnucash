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
 */
AccountGroup *xaccReadAccountGroup  (char *datafile);
int           xaccWriteAccountGroup (char *datafile, AccountGroup *grp);

#endif /* __XACC_FILEIO_P_H__ */
