
/*
 * Scrub.c
 *
 * FUNCTION:
 * Provides a set of functions and utilities for scrubbing clean 
 * single-entry accounts so that they can be promoted into 
 * self-consistent, clean double-entry accounts.
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

/* The xaccScrubOrphans() method searches for transacations that contain
 *    splits that do not have a parent account. These "orphaned splits"
 *    are placed into an "orphan account" which the user will have to 
 *    go into and clean up.  Kind of like the unix "Lost+Found" directory
 *    for orphaned inodes.
 */

#endif /* __XACC_SCRUB_H__ */


#include "AccountP.h"
#include "TransactionP.h"

double
xaccGetUnbalance (Transaction * trans) { return 0.0; }

void 
Scrub (Account *acc)
{
   ScrubOrphans (acc);
}


void
ScrubOrphans (Account *acc)
{
   int i=0;
   Split *split;
   Transaction * trans;

   split = acc->splits[0];
   while (split) {
      Split * s;
      int j = 0;
      trans = split->parent;

      s = trans->splits[0];
      while (s) {
         if (!s->acc) {
printf ("got one\n");
         }
         j++; s = trans->splits[j];
      }
      i++; split = acc->splits[i];
   }
}

/* ==================== END OF FILE ==================== */
