/*******************************************************************\
 * Destroy.c -- utilities for the window destruction (X-Accountant) *
 * Copyright (C) 1997 Linas Vepstas                                 *
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
\********************************************************************/

#include "config.h"

#include "Account.h"
#include "AccWindow.h"
#include "AdjBWindow.h"
#include "Data.h"
#include "LedgerUtils.h"
#include "RegWindow.h"
#include "RecnWindow.h"
#include "util.h"

   
/* ------------------------------------------------------ */
void
xaccAccountWindowDestroy (Account *acc)
{
   int i;

   if (!acc) return;

   /* recursively destroy windows associated with children */
   if (acc->children) {
      for (i=0; i<acc->children->numAcc; i++) {
         xaccAccountWindowDestroy (acc->children->account[i]);
      }
   }

   xaccDestroyRegWindow (acc->regData);
   xaccDestroyRegWindow (acc->regLedger);
   xaccDestroyRecnWindow (acc->recnData);
   xaccDestroyAdjBWindow (acc->adjBData);
   xaccDestroyEditAccWindow (acc->editAccData);
   xaccDestroyEditNotesWindow (acc->editNotesData);

}

/* ------------------------------------------------------ */

void
xaccGroupWindowDestroy (AccountGroup *grp)
{
   int i;

   if (!grp) return;

   /* recursively destroy windows associated with children */
   for (i=0; i<grp->numAcc; i++) {
      xaccAccountWindowDestroy (grp->account[i]);
   }
}

/************************** END OF FILE *************************/
