/********************************************************************\
 * AccWindow.c -- window for creating new accounts for xacc         *
 *                (X-Accountant)                                    *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997, 1998, 1999 Linas Vepstas                     *
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

#include <nana.h>
#include <stdio.h>

#include "config.h"

#include "AccWindow.h"

/* Please look at ../motif/AccWindow.c for info on what should be
   going on in these functions */

/********************************************************************\
 * accWindow                                                        *
 *   opens up a window to create a new account... the account is    * 
 *   actually created in the "create" callback                      * 
 *                                                                  * 
 * Args:   parent   - the parent of the window to be created        * 
 * Return: none                                                     *
\********************************************************************/
AccWindow *
accWindow (AccountGroup *grp) {
  AccWindow *accData = NULL;

  L("STUB: accWindow needs to be written for GNOME.\n");
  
  return accData;
}

/********************************************************************\
 * editAccWindow                                                    *
 *   opens up a window to edit an account                           * 
 *                                                                  * 
 * Args:   parent   - the parent of the window to be created        * 
 *         account  - the account to edit                           * 
 * Return: none                                                     *
\********************************************************************/
EditAccWindow *
editAccWindow( Account *acc ) {
  EditAccWindow *editAccData = NULL;

  L("STUB: editAccWindow needs to be written for GNOME.\n");

  return editAccData;
}

/********************************************************************\
 * Don't delete any structures -- the close callback wil do this    *
\********************************************************************/

void
xaccDestroyEditAccWindow (Account * acc) {

  L("STUB: xaccDestroyEditAccWindow needs to be written for GNOME.\n");
  
}

/********************************************************************\
 *                                                                  * 
\********************************************************************/

EditNotesWindow *
editNotesWindow (Account *acc) {
  EditNotesWindow *enw = NULL;

  L("STUB: editNotesWindow needs to be written for GNOME.\n");

  return enw;
}

/********************************************************************\
 * don't delete any structures; the close callack will do this       *
\********************************************************************/

void 
xaccDestroyEditNotesWindow (Account *acc) {
  
  L("STUB: xaccDestroyEditNotesWindow needs to be written for GNOME.\n");

}


/********************** END OF FILE *********************************\
\********************************************************************/
