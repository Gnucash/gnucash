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
 * Copyright (c) 1998 Linas Vepstas
 */

#ifndef __XACC_SCRUB_H__
#define __XACC_SCRUB_H__
#include "Account.h"
#include "Transaction.h"

/*
 * The xaccTransGetUnbalance() method returns the sum of the values of the
 *    splits in this transaction.
 */

double xaccTransGetUnbalance (Transaction * trans);

/* The ScrubOrphans() methods search for transacations that contain
 *    splits that do not have a parent account. These "orphaned splits"
 *    are placed into an "orphan account" which the user will have to 
 *    go into and clean up.  Kind of like the unix "Lost+Found" directory
 *    for orphaned inodes.
 *
 * The xaccAccountScrubOrphans() method performs this scrub only for the 
 *    indicated account, and not for any of its children.
 *
 * The xaccAccountTreeScrubOrphans() method performs this scrub for the 
 *    indicated account and its children.
 *
 * The xaccGroupScrubOrphans() method performs this scrub for the 
 *    child accounts of this group.
 */
void xaccAccountScrubOrphans (Account *acc);
void xaccAccountTreeScrubOrphans (Account *acc);
void xaccGroupScrubOrphans (AccountGroup *grp);

/* The xaccScrubImbalance() method searches for transactions that do
 *    not balance to zero. If any such transactions are found, a split
 *    is created to offset this amount and is added to an "imbalance"
 *    account.
 */
void xaccAccountScrubImbalance (Account *acc);
void xaccAccountTreeScrubImbalance (Account *acc);
void xaccGroupScrubImbalance (AccountGroup *grp);

#endif /* __XACC_SCRUB_H__ */

#include <nana.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "Account.h"
#include "Group.h"
#include "GroupP.h"
#include "Transaction.h"
#include "util.h"

static short module = MOD_SCRUB;
static Account * GetOrMakeAccount (Account *, Transaction *, const char *);

double
xaccGetUnbalance (Transaction * trans) { return 0.0; }

void 
Scrub (Account *acc)
{
   xaccAccountScrubOrphans (acc);
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

/* hack alert -- this string should probably be i18n'ed */
#define ORPHAN_STR "Orphan-"

void
xaccAccountScrubOrphans (Account *acc)
{
   int i=0;
   Split *split, **slist;
   Transaction *trans;
   Account * parent;

   PINFO ("xaccAccountScrubOrphans(): "
          "Looking for orphans in account %s \n", xaccAccountGetName(acc));

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
            DEBUG ("xaccAccountScrubOrphans(): Found an orphan \n");
            /* OK, we found an orphan.  Put it in an orphan account. */
            orph = GetOrMakeAccount (acc, trans, ORPHAN_STR);
            xaccAccountInsertSplit (orph, s);
         }
         j++; 
         s = xaccTransGetSplit (trans, j);
      }
      i++; split = slist[i];
   }
}

/* ================================================================ */

static Account *
GetOrMakeAccount (Account *peer, Transaction *trans, const char *name_root)
{
   char * accname;
   char * currency;
   Account * acc;
   AccountGroup *root;

   /* build the account name */
   currency = xaccTransFindCommonCurrency (trans);
   accname = alloca (strlen (name_root) + strlen (currency) + 1);
   strcpy (accname, name_root);
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
