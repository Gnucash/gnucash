/*******************************************************************\
 * Refresh.c -- utilities for window refresh (GnuCash)              *
 * Copyright (C) 1999 Linas Vepstas                                 *
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

#include "top-level.h"

#include "Refresh.h"
#include "LedgerUtils.h"
#include "MultiLedger.h"

#ifdef KDE
#if 0
#include "qt/reconciledlg.h"
#endif
#endif

#ifdef GNOME
#include "window-reconcile.h"
#endif

#ifdef MOTIF
#include "RecnWindow.h"
#endif

#include "util.h"

   
/* ------------------------------------------------------ */
void
gnc_account_ui_refresh(Account *account)
{
   AccountGroup *account_children;
   Account *child;
   int num_accounts;
   int i;

   if (account == NULL)
     return;

   /* recursively refresh windows associated with children */
   account_children = xaccAccountGetChildren(account);

   if (account_children != NULL)
   {
      num_accounts = xaccGroupGetNumAccounts(account_children);

      for (i = 0; i < num_accounts; i++)
      {
         child = xaccGroupGetAccount(account_children, i);
         gnc_account_ui_refresh(child);
      }
   }

   xaccAccountDisplayRefresh(account);
   recnRefresh(account);
}


/* ------------------------------------------------------ */
void 
gnc_account_list_ui_refresh(Account **account_list)
{
   Account *account;
   int i;

   xaccAccListDisplayRefresh(account_list);

   i = 0;
   account = account_list[0];

   while (account != NULL)
   {
     recnRefresh(account);

     i++;
     account = account_list[i];
   }
}


/* ------------------------------------------------------ */
void
gnc_group_ui_refresh(AccountGroup *group)
{
  Account *account;
  int num_accounts;
  int i;

  if (group == NULL)
    return;

  /* recursively refresh windows associated with children */
  num_accounts = xaccGroupGetNumAccounts(group);

  for (i = 0; i < num_accounts; i++)
  {
    account = xaccGroupGetAccount(group, i);
    gnc_account_ui_refresh(account);
  }
}


/* ------------------------------------------------------ */
void 
gnc_transaction_ui_refresh(Transaction *trans)
{
  Account *account;
  Split *split;
  int i, num_splits;  

  xaccTransDisplayRefresh(trans);

  num_splits = xaccTransCountSplits(trans);

  for (i = 0; i < num_splits; i++)
  {
    split = xaccTransGetSplit(trans, i);
    account = xaccSplitGetAccount(split);

    recnRefresh(account);
  }
}

/************************** END OF FILE *************************/
