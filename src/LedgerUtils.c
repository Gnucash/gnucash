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

#include "Account.h"
#include "Transaction.h"
#include "util.h"

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
   
Account ** accListCopy (Account **list)
{
   Account **newlist;
   int i;
   int nacc = 0;

   if (!list) return NULL;
   nacc = accListCount (list);
   if (0 == nacc) return NULL;

   newlist = (Account **) _malloc (nacc * sizeof (Account *));
   for (i=0; i<nacc; i++) {
     newlist[i] = list[i];
   }
   return newlist;
}

Transaction ** accListGetSortedTrans (Account **list)
{
   Account *acc;
   Transaction *trans;
   Transaction **tarray;
   int nacc = 0;
   int ntrans = 0;
   int i;

   if (!list) return 0;

   /* count the total number of transactions */
   nacc = 0;
   acc = list[0];
   while (acc) {
      ntrans += acc->numTrans;
      nacc++;
      acc = list[nacc];
   }
   ntrans ++;

   /* malloc the array of transactions */
   tarray = (Transaction **) _malloc (ntrans * sizeof (Transaction *));

   /* put all of the transactions in the flat array */
   nacc = 0;
   ntrans = 0;
   acc = list[0];
   while (acc) {
      for (i=0; i<acc->numTrans; i++) {
         tarray[ntrans] = getTransaction (acc, i);
         ntrans ++;
      }
      nacc++;
      acc = list[nacc];
   }
   tarray [ntrans] = NULL;

   /* run the sort routine on the array */
   qsort (tarray, ntrans, sizeof (Transaction *), xaccTransOrder);

   return tarray;
}   
   
/************************** END OF FILE *************************/
