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
#include "Destroy.h"
#include "Group.h"
#include "LedgerUtils.h"
#include "MultiLedger.h"

#ifndef KDE
#include "RecnWindow.h"
#endif

#include "util.h"

   
/* ------------------------------------------------------ */
void
xaccAccountWindowDestroy (Account *acc)
{
   int i;
   AccountGroup *acc_children;

   if (!acc) return;

   /* recursively destroy windows associated with children */
   acc_children = xaccAccountGetChildren (acc);
   if (acc_children) {
      int nacc = xaccGroupGetNumAccounts (acc_children);
      for (i=0; i<nacc; i++) {
         Account *child = xaccGroupGetAccount (acc_children, i);
         xaccAccountWindowDestroy (child);
      }
   }

   xaccDestroyLedgerDisplay (acc);
   xaccDestroyRecnWindow (acc);
   xaccDestroyAdjBWindow (acc);
   xaccDestroyEditAccWindow (acc);
   xaccDestroyEditNotesWindow (acc);
}

/* ------------------------------------------------------ */

void
xaccGroupWindowDestroy (AccountGroup *grp)
{
   int i;
   int nacc;

   if (!grp) return;

   /* recursively destroy windows associated with children */
   nacc = xaccGroupGetNumAccounts (grp);
   for (i=0; i<nacc; i++) {
      Account *acc = xaccGroupGetAccount (grp, i);
      xaccAccountWindowDestroy (acc);
   }
}

/************************** END OF FILE *************************/
