/********************************************************************\
 * FileIO.h -- read from and writing to a datafile for xacc         *
 *             (X-Accountant)                                       *
 * Copyright (C) 1997 Robin D. Clark                                *
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

#ifndef __FILEIO_H__
#define __FILEIO_H__

#include "config.h"
#include "Data.h"

#define ERR_FILEIO_NO_ERROR          0
#define ERR_FILEIO_FILE_BAD_READ     1 
#define ERR_FILEIO_FILE_EMPTY        2 
#define ERR_FILEIO_FILE_NOT_FOUND    3
#define ERR_FILEIO_FILE_TOO_NEW      4
#define ERR_FILEIO_FILE_TOO_OLD      5

/** PROTOTYPES ******************************************************/
AccountGroup *xaccReadData  (char *datafile);
int           xaccWriteData (char *datafile, AccountGroup *grp);
int           xaccGetFileIOError (void);

AccountGroup *xaccReadQIFData (char *datafile);

#endif
