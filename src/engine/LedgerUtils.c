/*******************************************************************\
 * LedgerUtils.c -- utilities for the ledger window (X-Accountant)  *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include "Account.h"
#include "AccountP.h"
#include "Group.h"
#include "GroupP.h"
#include "Transaction.h"
#include "util.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_ENGINE;

/* ------------------------------------------------------ */

gncBoolean accListHasAccount (Account **list, Account *findme)
{
   Account *acc;
   int nacc = 0;
   if (!list || !findme) return GNC_F;

   acc = list[0];
   while (acc) {
      if (acc == findme)
        return GNC_T;
      nacc++;
      acc = list[nacc];
   }
   return GNC_F;
}   
   
/* ------------------------------------------------------ */

int accListCount (Account **list)
{
   Account *acc;
   int nacc = 0;
   if (!list) return 0;

   acc = list[0];
   while (acc) {
      nacc++;
      acc = list[nacc];
   }
   return nacc;
}   
   
/* ------------------------------------------------------ */

Account ** accListCopy (Account **list)
{
   Account **newlist;
   int i;
   int nacc = 0;

   if (!list) return NULL;
   nacc = accListCount (list);
   if (0 == nacc) return NULL;

   newlist = (Account **) _malloc ((nacc+1) * sizeof (Account *));
   for (i=0; i<nacc; i++) {
     newlist[i] = list[i];
   }
   newlist [nacc] = NULL;
   return newlist;
}
   
/* ------------------------------------------------------ */
Account **
xaccGroupToList (Account *acc)
{
   Account **list;
   int nacc;
   int i, n;

   if (!acc) return NULL;

   ENTER ("acc=%p \n", acc);
   nacc = xaccGetNumAccounts (acc->children);
   nacc ++;  /* add one for this account */

   list = (Account **) _malloc ((nacc+1) * sizeof (Account *));

   list[0] = acc;  /* must be first -- other code depends on this */
   n = 1;
   if (acc->children) {
      for (i=0; i<acc->children->numAcc; i++) {
         list[n] = acc->children->account[i];

         /* recursively add children too */
         if (acc->children->account[i]->children) {
            Account **childlist;
            Account *childacc;
            int ic = 1;

            /* get the children */
            childlist = xaccGroupToList (acc->children->account[i]);

            /* copy them over */
            childacc = childlist[1];
            while (childacc) {
              n ++;
              list[n] = childacc;
              ic ++;
              childacc = childlist[ic];
            }
            _free(childlist);
         }
         n++;
      }
   }
   list[n] = NULL;
   LEAVE ("n=%d nacc=%d \n", n, nacc);
   assert (n==nacc);

   return list;
}

/************************** END OF FILE *************************/
