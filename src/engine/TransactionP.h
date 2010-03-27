/********************************************************************\
 * TransactionP.h -- private header for transaction & splits        *
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
 * TransactionP.h
 *
 * FUNCTION:
 * The is the *private* transaction header file.  Code outside of
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

#ifndef XACC_TRANSACTION_P_H
#define XACC_TRANSACTION_P_H

#include <time.h>
#include <glib.h>

#include "gnc-engine.h"   /* for typedefs */
#include "SplitP.h"
#include "qof.h"


/** STRUCTS *********************************************************/
/*
 * Double-entry is forced by having at least two splits in every
 * transaction.  By convention, (and only by convention, not by
 * any innate requirement), the first split is considered to be
 * the source split or the crediting split, and the others are
 * the destination, or debiting splits.  The grand total of all
 * of the splits must always be kept zero.
 */

/* A split transaction is one which shows up as a credit (or debit) in
 * one account, and pieces of it show up as debits (or credits) in other
 * accounts.  Thus, a single credit-card transaction might be split
 * between "dining", "tips" and "taxes" categories.
 *
 * A "split" is more commonly referred to as an "entry" in a "transaction".
 */

struct transaction_s
{
    QofInstance inst;     /* glbally unique id */

    Timespec date_entered;     /* date register entry was made              */
    Timespec date_posted;      /* date transaction was posted at bank       */

    /* The num field is a arbitrary user-assigned field.
     * It is intended to store a short id number, typically the check number,
     * deposit number, invoice number or other tracking number.
     */
    char * num;

    /* The description field is an arbitrary user-assigned value.
     * It is meant to be a short descriptive phrase.
     */
    char * description;

    /* The common_currency field is the balancing common currency for
     * all the splits in the transaction.  Alternate, better(?) name:
     * "valuation currency": it is the currency in which all of the
     * splits can be valued.  */
    gnc_commodity *common_currency;

    GList * splits; /* list of splits */

    /* marker is used to track the progress of transaction traversals.
     * 0 is never a legitimate marker value, so we can tell is we hit
     * a new transaction in the middle of a traversal. All each new
     * traversal cares about is whether or not the marker stored in
     * a transaction is the same as or different than the one
     * corresponding to the current traversal. */
    unsigned char  marker;

    /* The orig pointer points at a copy of the original transaction,
     * before editing was started.  This orig copy is used to rollback
     * any changes made if/when the edit is abandoned.
     */
    Transaction *orig;
};

struct _TransactionClass
{
    QofInstanceClass parent_class;
};

/* Set the transaction's GncGUID. This should only be done when reading
 * a transaction from a datafile, or some other external source. Never
 * call this on an existing transaction! */
#define xaccTransSetGUID(t,g) qof_instance_set_guid(QOF_INSTANCE(t),g)

/* This routine makes a 'duplicate' of the indicated transaction.
 * This routine cannot be exposed publically since the duplicate
 * is wrong in many ways: it is not issued a unique guid, and thus
 * not a properly registered Entity.  The splits are copied, but
 * these are also funny: they aren't inserted into the accounts
 * they claim to be in.  The splits also have bogus GncGUID's.
 * Another 'feature': the splits point at the old transaction
 * as the parent, not the new transaction.
 */
Transaction * xaccDupeTransaction (const Transaction *t);

/* The xaccTransSet/GetVersion() routines set & get the version
 *    numbers on this transaction.  The version number is used to manage
 *    multi-user updates.  These routines are private because we don't
 *    want anyone except the backend to mess with them.
 */
void xaccTransSetVersion (Transaction*, gint32);
gint32 xaccTransGetVersion (const Transaction*);

/* Code to register Transaction type with the engine */
gboolean xaccTransRegister (void);

/* The xaccTransactionGetBackend() subroutine will find the
 *    persistent-data storage backend associated with this
 *    transaction.
 */
QofBackend * xaccTransactionGetBackend (Transaction *trans);

/* The xaccEnable/DisableDataScrubbing() routines affect what
 *   happens during transaction commit.  When scrubbing is enabled,
 *   then transactions are fixed up during transaction commit,
 *   so that only consistent transactions are commited to the engine.
 *   However, when data is being loaded from a backend (in particular,
 *   from the file backend), the data might not be consistent until
 *   its completely loaded.   In particular, gains transactions might
 *   be loaded at a different time than the transactions that casued
 *   the gains.  Thus, scrubbing needs do be disabled during file
 *   load.  These routines enable that.
 */
void xaccEnableDataScrubbing(void);
void xaccDisableDataScrubbing(void);

/** Set the KvpFrame slots of this transaction to the given frm by
 *  * directly using the frm pointer (i.e. non-copying).
 *   * XXX this is wrong, nedds to be replaced with a transactional thingy
 *   in kvp + qofinstance. for now, this is a quasi-unctional placeholder.
 *    */
#define xaccTransSetSlots_nc(T,F) qof_instance_set_slots(QOF_INSTANCE(T),F)

void xaccTransRemoveSplit (Transaction *trans, const Split *split);
G_INLINE_FUNC void check_open (const Transaction *trans);

/*@}*/


#endif /* XACC_TRANSACTION_P_H */
