/********************************************************************\
 * SplitP.h -- private header for splits                            *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-2000 Linas Vepstas <linas@linas.org>          *
 * Copyright (C) 2000 Bill Gribble                                  *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/*
 * FILE:
 * SplitP.h
 *
 * FUNCTION:
 * The is the *private* split header file.  Code outside of
 * engine should *not* include this file.  This is because code
 * outside of the engine should *never* access any of the structure
 * members directly.
 *
 * Note that this header file also defines prototypes for various
 * routines that perform sub-atomic updates of the accounting
 * structures.  If these routines are not used properly, they
 * can result in inconsistent, unbalanced accounting structures.
 * In other words, their use is dangerous, and their use outside
 * of the scope of the engine is forbidden.
 *
 */

#ifndef XACC_SPLIT_P_H
#define XACC_SPLIT_P_H

#include <time.h>
#include <glib.h>

#include "gnc-engine.h"   /* for typedefs */
#include "qof.h"


/** STRUCTS *********************************************************/
/* A "split" is more commonly referred to as an "entry" in a "transaction".
 */

/* Flags for handling cap-gains status */
#define GAINS_STATUS_UNKNOWN        0xff
#define GAINS_STATUS_CLEAN           0x0
#define GAINS_STATUS_GAINS           0x3
#define GAINS_STATUS_DATE_DIRTY     0x10
#define GAINS_STATUS_AMNT_DIRTY     0x20
#define GAINS_STATUS_VALU_DIRTY     0x40
#define GAINS_STATUS_LOT_DIRTY      0x80
#define GAINS_STATUS_ADIRTY    (GAINS_STATUS_AMNT_DIRTY|GAINS_STATUS_LOT_DIRTY)
#define GAINS_STATUS_VDIRTY    (GAINS_STATUS_VALU_DIRTY)
#define GAINS_STATUS_A_VDIRTY  (GAINS_STATUS_AMNT_DIRTY|GAINS_STATUS_VALU_DIRTY|GAINS_STATUS_LOT_DIRTY)

struct split_s
{
    QofInstance inst;

    Account *acc;              /* back-pointer to debited/credited account  */
    Account *orig_acc;
    GNCLot *lot;               /* back-pointer to debited/credited lot */

    Transaction *parent;       /* parent of split                           */
    Transaction *orig_parent;

    /* The memo field is an arbitrary user-assiged value.
     * It is intended to hold a short (zero to forty character) string
     * that is displayed by the GUI along with this split.
     */
    char  * memo;

    /* The action field is an arbitrary user-assigned value.
     * It is meant to be a very short (one to ten character) string that
     * signifies the "type" of this split, such as e.g. Buy, Sell, Div,
     * Withdraw, Deposit, ATM, Check, etc. The idea is that this field
     * can be used to create custom reports or graphs of data.
     */
    char  * action;            /* Buy, Sell, Div, etc.                      */

    Timespec date_reconciled;  /* date split was reconciled                 */
    char    reconciled;        /* The reconciled field                      */

    /* gains is a flag used to track the relationship between
     * capital-gains splits. Depending on its value, this flag indicates
     * if this split is the source of gains, if this split is a record
     * of the gains, and if values are 'dirty' and need to be recomputed.
     */
    unsigned char  gains;

    /* 'gains_split' is a convenience pointer used to track down the
     * other end of a cap-gains transaction pair.  NULL if this split
     * doesn't involve cap gains.
     */
    Split *gains_split;

    /* 'value' is the quantity of the transaction balancing commodity
     * (i.e. currency) involved, 'amount' is the amount of the account's
     * commodity involved. */
    gnc_numeric  value;
    gnc_numeric  amount;

    /* -------------------------------------------------------------- */
    /* Below follow some 'temporary' fields */

    /* The various "balances" are the sum of all of the values of
     * all the splits in the account, up to and including this split.
     * These balances apply to a sorting order by date posted
     * (not by date entered). */
    gnc_numeric  balance;
    gnc_numeric  cleared_balance;
    gnc_numeric  reconciled_balance;
};

struct _SplitClass
{
    QofInstanceClass parent_class;
};


/* Set the split's GUID. This should only be done when reading
 * a split from a datafile, or some other external source. Never
 * call this on an existing split! */
#define xaccSplitSetGUID(s,g) qof_instance_set_guid(QOF_INSTANCE(s),g)

/* The xaccFreeSplit() method simply frees all memory associated
 * with the split.  It does not verify that the split isn't
 * referenced in some account.  If the split is referenced by an
 * account, then calling this method will leave the system in an
 * inconsistent state.  This *will* lead to crashes and hangs.
 */
void  xaccFreeSplit (Split *split);    /* frees memory */

Split * xaccSplitClone (const Split *s);

Split *xaccDupeSplit (const Split *s);
void mark_split (Split *s);

void xaccSplitVoid(Split *split);
void xaccSplitUnvoid(Split *split);
void xaccSplitCommitEdit(Split *s);
void xaccSplitRollbackEdit(Split *s);

/* Compute the value of a list of splits in the given currency,
 * excluding the skip_me split. */
gnc_numeric xaccSplitsComputeValue (GList *splits, const Split * skip_me,
                                    const gnc_commodity * base_currency);

/* Code to register Split type with the engine */
gboolean xaccSplitRegister (void);

/* The xaccSplitDetermineGainStatus() routine will analyze the
 *   the split, and try to set the internal status flags
 *   appropriately for the split.  These flags indicate if the split
 *   represents cap gains, and if the gains value/amount needs to be
 *   recomputed.
 */
void xaccSplitDetermineGainStatus (Split *split);

/* ---------------------------------------------------------------- */
/* Deprecated routines */
void         DxaccSplitSetSharePriceAndAmount (Split *split,
        double price,
        double amount);
void         DxaccSplitSetShareAmount (Split *split, double amount);

/********************************************************************\
 * sorting comparison function
 *
 * returns a negative value if transaction a is dated earlier than b,
 * returns a positive value if transaction a is dated later than b,
 *
 * This function tries very hard to uniquely order all transactions.
 * If two transactions occur on the same date, then their "num" fields
 * are compared.  If the num fields are identical, then the description
 * fields are compared.  If these are identical, then the memo fields
 * are compared.  Hopefully, there will not be any transactions that
 * occur on the same day that have all three of these values identical.
 *
 * Note that being able to establish this kind of absolute order is
 * important for some of the ledger display functions.
 *
 * Yes, this kind of code dependency is ugly, but the alternatives seem
 * ugly too.
 *
\********************************************************************/


#define DATE_CMP(aaa,bbb,field) {                       \
  /* if dates differ, return */                         \
  if ( (aaa->field.tv_sec) <                            \
       (bbb->field.tv_sec)) {                           \
    return -1;                                          \
  } else                                                \
  if ( (aaa->field.tv_sec) >                            \
       (bbb->field.tv_sec)) {                           \
    return +1;                                          \
  }                                                     \
                                                        \
  /* else, seconds match. check nanoseconds */          \
  if ( (aaa->field.tv_nsec) <                           \
       (bbb->field.tv_nsec)) {                          \
    return -1;                                          \
  } else                                                \
  if ( (aaa->field.tv_nsec) >                           \
       (bbb->field.tv_nsec)) {                          \
    return +1;                                          \
  }                                                     \
}

#define CHECK_GAINS_STATUS(s)  \
   if (GAINS_STATUS_UNKNOWN == s->gains) xaccSplitDetermineGainStatus(s);

#define SET_GAINS_DIRTY(s,flg) do {                                     \
   if (FALSE == (GAINS_STATUS_GAINS & s->gains)) {                      \
      s->gains |= flg;                                                  \
   } else {                                                             \
      if (s->gains_split) s->gains_split->gains |= flg;                 \
   }                                                                    \
} while (0)

#define SET_GAINS_ADIRTY(s)  SET_GAINS_DIRTY(s,GAINS_STATUS_ADIRTY);
#define SET_GAINS_A_VDIRTY(s) SET_GAINS_DIRTY(s,GAINS_STATUS_A_VDIRTY);
#define SET_GAINS_VDIRTY(s)  SET_GAINS_DIRTY(s,GAINS_STATUS_VDIRTY);

/*@}*/


#endif /* XACC_SPLIT_P_H */
