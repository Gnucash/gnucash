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
#include "Destroy.h"
#include "Group.h"
#include "MultiLedger.h"
#include "RecnWindow.h"

   
/* ------------------------------------------------------ */
static void
xaccAccountWindowDestroySimple (Account *account)
{
  xaccDestroyLedgerDisplay (account);
}

/* ------------------------------------------------------ */
static void
xaccAccountWindowDestroy (Account *account)
{
  AccountGroup *account_children;
  GList *list;
  GList *node;

  if (!account) return;

  /* recursively destroy windows associated with children */
  account_children = xaccAccountGetChildren (account);

  list = xaccGroupGetAccountList (account_children);

  for (node = list; node; node = node->next)
  {
    Account *account = node->data;
    xaccAccountWindowDestroy (account);
  }

  xaccAccountWindowDestroySimple (account);
}

/* ------------------------------------------------------ */

void
xaccGroupWindowDestroy (AccountGroup *grp)
{
  GList *list;
  GList *node;

  if (!grp) return;

  /* recursively destroy windows associated with children */
  list = xaccGroupGetAccountList (grp);

  for (node = list; node; node = node->next)
  {
    Account *account = node->data;
    xaccAccountWindowDestroy (account);
  }
}

/************************** END OF FILE *************************/
