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
#include "Data.h"
#include "Transaction.h"
#include "util.h"

typedef struct _RegWindow xxxRegWindow;

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

/* ------------------------------------------------------ */

int 
ledgerListCount (struct _RegWindow **list)
{
   struct _RegWindow *reg;
   int n;

   if (!list) return 0;

   n = 0;
   reg = list[0];
   while (reg) {
      n++;
      reg = list[n];
   }
   return n;
}

/* ------------------------------------------------------ */

void 
ledgerListAdd (Account * acc, struct _RegWindow *addreg)
{
   struct _RegWindow **oldlist;
   struct _RegWindow **newlist;
   struct _RegWindow *reg;
   int n;

   if (!acc) return;
   if (!addreg) return;

/*   oldlist = acc->ledgerList; */
   n = ledgerListCount (oldlist);

   newlist = (struct _RegWindow **) 
               _malloc ((n+2) * sizeof (struct _RegWindow *));

   n = 0;
   if (oldlist) {
      reg = oldlist[0];
      while (reg) {
         newlist[n] = reg;
         n++;
         reg = oldlist[n];
      }
      _free (oldlist);
   }
   newlist[n] = addreg;
   newlist[n+1] = NULL;

/*    acc->ledgerList = newlist; */
}

/* ------------------------------------------------------ */

void 
ledgerListRemove (Account * acc, struct _RegWindow *delreg)
{
   struct _RegWindow **oldlist;
   struct _RegWindow **newlist;
   struct _RegWindow *reg;
   int n, i;

   if (!acc) return;
   if (!delreg) return;

   /* oldlist = acc->ledgerList; */
   n = ledgerListCount (oldlist);

   newlist = (struct _RegWindow **) 
               _malloc ((n+1) * sizeof (struct _RegWindow *));

   n = 0;
   i = 0;
   if (oldlist) {
      reg = oldlist[0];
      while (reg) {
         newlist[i] = reg;
         if (delreg == reg) i--;
         i++;
         n++;
         reg = oldlist[n];
      }
      _free (oldlist);
   }
   newlist[i] = NULL;

   /* acc->ledgerList = newlist; */
}

/* ------------------------------------------------------ */

int
ledgerListIsMember (Account * acc, struct _RegWindow *memreg)
{
   struct _RegWindow **list;
   struct _RegWindow *reg;
   int n;

   if (!acc) return 0;
   if (!memreg) return 0;

   /* list = acc->ledgerList; */
   if (!list) return 0;

   n = 0;
   reg = list[0];
   while (reg) {
      if (memreg == reg) return 1;
      n++;
      reg = list[n];
   }
   return 0;
}

/* ------------------------------------------------------ */

void
ledgerListAddList (Account ** list, struct _RegWindow *reg)
{
   Account *acc;
   int n = 0;

   if (!list) return;
   if (!reg) return;

   acc = list[0];
   while (acc) {
      ledgerListAdd (acc, reg);
      n++;
      acc = list[n];
   }
}

/* ------------------------------------------------------ */

void
ledgerListRemoveList (Account ** list, struct _RegWindow *reg)
{
   Account *acc;
   int n = 0;

   if (!list) return;
   if (!reg) return;

   acc = list[0];
   while (acc) {
      ledgerListRemove (acc, reg);
      n++;
      acc = list[n];
   }
}

/************************** END OF FILE *************************/
