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

void
xaccAccountScrubOrphans (Account *acc) {
  GList *slp;
  Transaction *trans;
  Account * parent;
  
  PINFO ("Looking for orphans in account %s \n", xaccAccountGetName(acc));
  
  for(slp = xaccAccountGetSplitList(acc); slp; slp = slp->next) {
    Split *split = (Split *) slp->data;
    Split * tsplit;
    int j = 0;

    trans = xaccSplitGetParent (split);
    tsplit = xaccTransGetSplit (trans, 0);
    while (tsplit) {
      parent = xaccSplitGetAccount (tsplit);
      if (!parent) {
        Account *orph;
        DEBUG ("Found an orphan \n");
        /* OK, we found an orphan.  Put it in an orphan account. */
        orph = GetOrMakeAccount (acc, trans, _("Orphan"));
        xaccAccountBeginEdit (orph);
        xaccAccountInsertSplit (orph, tsplit);
        xaccAccountCommitEdit (orph);
      }
      j++; 
      tsplit = xaccTransGetSplit (trans, j);
    }
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

void
xaccAccountScrubImbalance (Account *acc) {
   GList *slp;

   PINFO ("Looking for imbalance in account %s \n", xaccAccountGetName(acc));

   for(slp = xaccAccountGetSplitList(acc); slp; slp = slp->next) {
     Split *split = (Split *) slp->data;
     Transaction *trans = xaccSplitGetParent(split);
     double imbalance;

      imbalance = DxaccTransGetImbalance (trans);
      if (!(DEQ (imbalance, 0.0))) {
         Split *splat;
         Account *orph;
         DEBUG ("Found imbalance of %g\n", imbalance);
         /* OK, we found an imbalanced trans.  Put it in the imbal account. */
         orph = GetOrMakeAccount (acc, trans, _("Imbalance"));
         
         /* put split into account before setting split value */
         splat = xaccMallocSplit();
         xaccAccountBeginEdit (orph);
         xaccAccountInsertSplit (orph, splat);
         xaccAccountCommitEdit (orph);

         xaccTransBeginEdit (trans, 1);
         DxaccSplitSetValue (splat, -imbalance);
         xaccTransAppendSplit (trans, splat);
         xaccTransCommitEdit (trans);
      }
   }
}

/* ================================================================ */

static Account *
GetOrMakeAccount (Account *peer, Transaction *trans, const char *name_root)
{
   char * accname;
   const gnc_commodity * currency;
   Account * acc;
   AccountGroup *root;

   /* build the account name */
   currency = xaccTransFindCommonCurrency (trans);
   accname = alloca (strlen (name_root) + 
                     strlen (gnc_commodity_get_mnemonic(currency)) + 2);
   strcpy (accname, name_root);
   strcat (accname, "-");
   strcat (accname, gnc_commodity_get_mnemonic(currency)); 

   /* see if we've got one of these going already ... */
   acc = xaccGetPeerAccountFromName (peer, accname);
   if (acc) return acc;

   /* guess not. We'll have to build one */
   acc = xaccMallocAccount ();
   xaccAccountBeginEdit (acc);
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
