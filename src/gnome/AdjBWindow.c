/********************************************************************\
 * AdjBWindow.c -- the adjust balance window                        *
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
 *                                                                  *
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#include <stdio.h>
#include <nana.h>

#include "config.h"

#include "AdjBWindow.h"

/********************************************************************\
 * adjBWindow                                                       *
 *   opens up the window to adjust the balance                      *
 *                                                                  *
 * Args:   parent  - the parent of this window                      *
 *         account - the account to adjust                          *
 * Return: adjBData - the instance of this AdjBWindow               *
\********************************************************************/
AdjBWindow *
adjBWindow( Account *acc ) {
  AdjBWindow *adjBData = NULL;

  L("STUB: adjBWindow needs to be written for GNOME.\n");

  return adjBData;
}

/********************************************************************\
 * Don't delete any structures, the close callback will do this     *
\********************************************************************/

void
xaccDestroyAdjBWindow (Account *acc) {

  L("STUB: xaccDestroyAdjBWindow needs to be written for GNOME.\n");
  
}


/******************** END OF FILE ***********************************\
\********************************************************************/
