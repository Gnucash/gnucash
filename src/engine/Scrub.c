/********************************************************************\
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

/*
 * FILE:
 * Scrub.c
 *
 * FUNCTION:
 * Provides a set of functions and utilities for scrubbing clean 
 * single-entry accounts so that they can be promoted into 
 * self-consistent, clean double-entry accounts.
 *
 * HISTORY:
 * Created by Linas Vepstas December 1998
 * Copyright (c) 1998, 1999, 2000 Linas Vepstas
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "Account.h"
#include "Group.h"
#include "GroupP.h"
#include "Scrub.h"
#include "Transaction.h"
#include "messages.h"
#include "util.h"

static short module = MOD_SCRUB;
static Account * GetOrMakeAccount (Account *, Transaction *, const char *);

void 
Scrub (Account *acc)
{
   xaccAccountTreeScrubOrphans (acc);
   xaccAccountScrubImbalance (acc);
}

/* ================================================================ */

void
xaccGroupScrubOrphans (AccountGroup *grp)
{
   int i=0;
   if (!grp) return;

   assert ((0 == grp->numAcc) || (grp->account));
   for (i=0; i<grp->numAcc; i++) {
      xaccAccountTreeScrubOrphans (grp->account[i]);
   }
}

void
xaccAccountTreeScrubOrphans (Account *acc)
{
   xaccGroupScrubOrphans (xaccAccountGetChildren(acc));
   xaccAccountScrubOrphans (acc);
}

#ifndef ORPHAN_STR
#  define ORPHAN_STR "Orphan"
#endif

void
xaccAccountScrubOrphans (Account *acc)
{
   int i=0;
   Split *split, **slist;
   Transaction *trans;
   Account * parent;

   PINFO ("Looking for orphans in account %s \n", xaccAccountGetName(acc));

   slist = xaccAccountGetSplitList (acc);
   split = slist[0];
   while (split) {
      Split * s;
      int j = 0;
      trans = xaccSplitGetParent (split);

      s = xaccTransGetSplit (trans, 0);
      while (s) {
         parent = xaccSplitGetAccount (s);
         if (!parent) {
            Account *orph;
            DEBUG ("Found an orphan \n");
            /* OK, we found an orphan.  Put it in an orphan account. */
            orph = GetOrMakeAccount (acc, trans, ORPHAN_STR);
            xaccAccountBeginEdit (orph, 1);
            xaccAccountInsertSplit (orph, s);
            xaccAccountCommitEdit (orph);
         }
         j++; 
         s = xaccTransGetSplit (trans, j);
      }
      i++; split = slist[i];
   }
}

/* ================================================================ */

void
xaccGroupScrubImbalance (AccountGroup *grp)
{
   int i=0;
   if (!grp) return;

   assert ((0 == grp->numAcc) || (grp->account));
   for (i=0; i<grp->numAcc; i++) {
      xaccAccountTreeScrubImbalance (grp->account[i]);
   }
}

void
xaccAccountTreeScrubImbalance (Account *acc)
{
   xaccGroupScrubImbalance (xaccAccountGetChildren(acc));
   xaccAccountScrubImbalance (acc);
}

#ifndef IMBALANCE_STR
#  define IMBALANCE_STR "Imbalance"
#endif

void
xaccAccountScrubImbalance (Account *acc)
{
   int i=0;
   Split *split, **slist;
   Transaction *trans;

   PINFO ("Looking for imbalance in account %s \n", xaccAccountGetName(acc));

   slist = xaccAccountGetSplitList (acc);
   split = slist[0];
   while (split) {
      double imbalance;
      trans = xaccSplitGetParent (split);

      imbalance = xaccTransGetImbalance (trans);
      if (!(DEQ (imbalance, 0.0))) {
         Split *splat;
         Account *orph;
         DEBUG ("Found imbalance of %g\n", imbalance);
         /* OK, we found an imbalanced trans.  Put it in the imbal account. */
         orph = GetOrMakeAccount (acc, trans, IMBALANCE_STR);
         
         /* put split into account before setting split value */
         splat = xaccMallocSplit();
         xaccAccountBeginEdit (orph, 1);
         xaccAccountInsertSplit (orph, splat);
         xaccAccountCommitEdit (orph);

         xaccTransBeginEdit (trans, 1);
         xaccSplitSetValue (splat, -imbalance);
         xaccTransAppendSplit (trans, splat);
         xaccTransCommitEdit (trans);
      }
      i++; split = slist[i];
   }
}

/* ================================================================ */

static Account *
GetOrMakeAccount (Account *peer, Transaction *trans, const char *name_root)
{
   char * accname;
   const char * currency;
   Account * acc;
   AccountGroup *root;

   /* build the account name */
   currency = xaccTransFindCommonCurrency (trans);
   accname = alloca (strlen (name_root) + strlen (currency) + 2);
   strcpy (accname, name_root);
   strcat (accname, "-");
   strcat (accname, currency); 

   /* see if we've got one of these going already ... */
   acc = xaccGetPeerAccountFromName (peer, accname);
   if (acc) return acc;

   /* guess not. We'll have to build one */
   acc = xaccMallocAccount ();
   xaccAccountBeginEdit (acc, 1);
   xaccAccountSetName (acc, accname);
   xaccAccountSetCurrency (acc, currency);
   xaccAccountSetType (acc, BANK);

   /* hang the account off the root */
   root = xaccGetAccountRoot (peer);
   xaccGroupInsertAccount (root, acc);
   xaccAccountCommitEdit (acc);

   return acc;
}

/* ==================== END OF FILE ==================== */
