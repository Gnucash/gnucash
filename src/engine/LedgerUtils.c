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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include "Account.h"
#include "Group.h"
#include "Transaction.h"
#include "util.h"

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
/* sort the transactions in date order */
Split ** accListGetSortedSplits (Account **list)
{
   Account *acc;
   Split **sarray;
   int nacc = 0;
   int nsplits = 0;
   int i, j;

   if (!list) return 0;

   /* count the total number of transactions */
   nacc = 0;
   acc = list[0];
   while (acc) {
      nsplits += acc->numSplits;
      nacc++;
      acc = list[nacc];
   }
   nsplits ++;

   /* malloc the array of transactions */
   sarray = (Split **) _malloc (nsplits * sizeof (Split *));

   /* put all of the transactions in the flat array */
   nacc = 0;
   nsplits = 0;
   acc = list[0];
   while (acc) {
      for (i=0; i<acc->numSplits; i++) {
         sarray[nsplits] = acc->splits[i];
         nsplits ++;
      }
      nacc++;
      acc = list[nacc];
   }
   sarray [nsplits] = NULL;

   /* search and destroy duplicates. */
   /* duplicates are possible due to double-entry */
   /* one transaction can appear at most twice in the list */
   for (i=0; i<nsplits; i++) {
      for (j=i+1; j<nsplits; j++) {
         if (sarray[i] == sarray[j]) {
            sarray[j] = sarray [nsplits-1];
            sarray[nsplits-1] = NULL;
            nsplits --;
         }
      }
   }

   /* run the sort routine on the array */
   qsort (sarray, nsplits, sizeof (Split *), 
            (int(*)(const void*, const void *)) xaccSplitOrder);

   return sarray;
}   
   
/* ------------------------------------------------------ */
Account **
xaccGroupToList (Account *acc)
{
   Account **list;
   int nacc;
   int i, n;

   if (!acc) return NULL;

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
            int ic = 0;

            /* get the children */
            childlist = xaccGroupToList (acc->children->account[i]);

            /* copy them over */
            childacc = childlist[0];
            while (childacc) {
              n++;
              list[n] = childacc;
              childacc = childlist[ic];
              ic ++;
            }
            _free(childlist);
         }
         n++;
      }
   }
   list[n] = NULL;

   return list;
}

/************************** END OF FILE *************************/
