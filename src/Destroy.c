/********************************************************************\
 * Destroy.c -- utilities for the window destruction GnuCash        *
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
#include "Destroy.h"
#include "Group.h"
#include "MultiLedger.h"
#include "RecnWindow.h"

   
/* ------------------------------------------------------ */
void
xaccAccountWindowDestroySimple (Account *account)
{
  xaccDestroyLedgerDisplay (account);
  xaccDestroyRecnWindow (account);
  gnc_ui_destroy_edit_account_window (account);
}

/* ------------------------------------------------------ */
void
xaccAccountWindowDestroy (Account *account)
{
   int i;
   AccountGroup *account_children;

   if (!account) return;

   /* recursively destroy windows associated with children */
   account_children = xaccAccountGetChildren (account);
   if (account_children) {
      int nacc = xaccGroupGetNumAccounts (account_children);
      for (i=0; i<nacc; i++) {
         Account *child = xaccGroupGetAccount (account_children, i);
         xaccAccountWindowDestroy (child);
      }
   }

   xaccAccountWindowDestroySimple(account);
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
      Account *account = xaccGroupGetAccount (grp, i);
      xaccAccountWindowDestroy (account);
   }
}

/************************** END OF FILE *************************/
