/********************************************************************\
 * SchedXaction.h -- Scheduled Transaction                          *
 * Copyright (C) 2001 Joshua Sled <jsled@asynchronous.org>          *
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

#ifndef XACC_SCHEDXACTION_H
#define XACC_SCHEDXACTION_H

#include "config.h"

#include <time.h>
#include <glib.h>
#include "GNCId.h"
#include "FreqSpec.h"
#include "date.h"
#include "kvp_frame.h"
#include "gnc-book.h"

/* 
 * #defines for kvp_frame strings
 * FIXME: Is this the right spot for them <rgmerk>?
 */

#define GNC_SX_ID                    "sched-xaction"
#define GNC_SX_ACCOUNT               "account"
#define GNC_SX_CREDIT_FORMULA        "credit-formula"
#define GNC_SX_DEBIT_FORMULA         "debit-formula"
#define GNC_SX_SHARES                "shares"
#define GNC_SX_AMOUNT                "amnt"
#define GNC_SX_FROM_SCHED_XACTION    "from-sched-xaction"

/**
 * A single scheduled transaction.
 *
 * Scheduled transactions have a list of transactions, and a frequency
 * [and associated date anchors] with which they are scheduled.
 *
 * Things that make sense to have in a template transaction:
 *   [not] Date [though eventually some/multiple template transactions
 *               might have relative dates].
 *   Memo
 *   Account
 *   Funds In/Out... or an expr involving 'amt' [A, x, y, a?] for
 *     variable expenses.
 *
 * Template transactions are instantiated by:
 *  . copying the fields of the template
 *  . setting the date to the calculated "due" date.
 *
 * We should be able to use the GeneralLedger [or, yet-another-subtype
 * of the internal ledger] for this editing.
 **/
typedef struct gncp_SchedXaction {
  gchar           *name;
  
  FreqSpec        *freq;
  
  GDate           last_date;
  
  GDate           start_date;
  /* if end_date is invalid, then no end. */
  GDate           end_date;

  /* if num_occurances_total == 0, then no limit */
  gint            num_occurances_total;
  /* reminaing occurances are as-of the 'last_date'. */
  gint            num_occurances_remain;
  
  gboolean        autoCreateOption;
  gboolean        autoCreateNotify;
  gint            advanceCreateDays;
  gint            advanceRemindDays;
 
  Account        *template_acct;
  GUID             guid;
  
  /* Changed since last save? */
  gboolean       dirty;

  kvp_frame        *kvp_data;


} SchedXaction;

/**
 * Creates and initializes a scheduled transaction.
 **/
SchedXaction *xaccSchedXactionMalloc(GNCSession *session);

/*
 * returns true if the scheduled transaction is dirty and needs to
 * be saved
 */

gboolean xaccSchedXactionIsDirty(SchedXaction *sx);

/*
 * Set dirtyness state.  Only save/load code should modify this outside
 * SX engine CODE . . . 
 * (set it to FALSE after backend completes reading in data 
 *
 * FIXME: put this into a private header . . . .
 */

void xaccSchedXactionSetDirtyness(SchedXaction *sx, gboolean dirty_p);
/*
 * Cleans up and frees a SchedXaction and it's associated data.
 **/
void xaccSchedXactionFree( SchedXaction *sx );

FreqSpec *xaccSchedXactionGetFreqSpec( SchedXaction *sx );
/**
 * The FreqSpec is given to the SchedXaction for mem mgmt; it should
 * not be freed by the external code.
 **/
void xaccSchedXactionSetFreqSpec( SchedXaction *sx, FreqSpec *fs );

gchar *xaccSchedXactionGetName( SchedXaction *sx );
/**
 * A copy of the name is made.
 **/
void xaccSchedXactionSetName( SchedXaction *sx, const gchar *newName );

GDate* xaccSchedXactionGetStartDate( SchedXaction *sx );
void xaccSchedXactionSetStartDate( SchedXaction *sx, GDate* newStart );

int xaccSchedXactionHasEndDate( SchedXaction *sx );
/**
 * Returns invalid date when there is no end-date specified.
 **/
GDate* xaccSchedXactionGetEndDate( SchedXaction *sx );
void xaccSchedXactionSetEndDate( SchedXaction *sx, GDate* newEnd );

GDate* xaccSchedXactionGetLastOccurDate( SchedXaction *sx );
void xaccSchedXactionSetLastOccurDate( SchedXaction *sx, GDate* newLastOccur );

/**
 * Returns true if the scheduled transaction has a defined number of
 * occurances, false if not.
 **/
gboolean xaccSchedXactionHasOccurDef( SchedXaction *sx );
gint xaccSchedXactionGetNumOccur( SchedXaction *sx );
void xaccSchedXactionSetNumOccur( SchedXaction *sx, gint numNum );
gint xaccSchedXactionGetRemOccur( SchedXaction *sx );
void xaccSchedXactionSetRemOccur( SchedXaction *sx, gint numRemain );

GList *xaccSchedXactionGetSplits( SchedXaction *sx );
void xaccSchedXactionSetSplits( SchedXaction *sx, GList *newSplits );

void xaccSchedXactionGetAutoCreate( SchedXaction *sx, gboolean *outAutoCreate, gboolean *outNotify );
void xaccSchedXactionSetAutoCreate( SchedXaction *sx, gboolean newAutoCreate, gboolean newNotify );

gint xaccSchedXactionGetAdvanceCreation( SchedXaction *sx );
void xaccSchedXactionSetAdvanceCreation( SchedXaction *sx, gint createDays );

gint xaccSchedXactionGetAdvanceReminder( SchedXaction *sx );
void xaccSchedXactionSetAdvanceReminder( SchedXaction *sx, gint reminderDays );

#if 0
#error vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
GList *xaccSchedXactionGetXactions( SchedXaction *sx );
void xaccSchedXactionClearXactions( SchedXaction *sx );
void xaccSchedXactionAddXaction( SchedXaction *sx,
                                 Transaction *t );
#error ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#endif /* 0 */


/*
 * The following function is slightly risky.  If you change
 * the retrieved kvp_frame you must mark the SchedXaction
 * dirty with xaccSchedXactionSetDirtyness
 */
kvp_frame *xaccSchedXactionGetSlots( SchedXaction *sx );
/**
 * Sets the SX kvp data to the given kvp_frame.
 * NOTE: This is not copied, but set directly.
 **/
void xaccSchedXactionSetSlots( SchedXaction *sx,
                               kvp_frame *frm );


/**
 * Use the following two functions in preference to 
 * the above two . . .
 */
kvp_value *xaccSchedXactionGetSlot( SchedXaction *sx, 
				    const char *slot );

/*
 * This function copies value, so you don't have to
 */

void xaccSchedXactionSetSlot( SchedXaction *sx, 
			      const char *slot,
			      const kvp_value *value );

const GUID *xaccSchedXactionGetGUID( SchedXaction *sx );
void xaccSchedXactionSetGUID( SchedXaction *sx, GUID g );

/**
 * Returns the next occurance of a scheduled transaction.  If the
 * transaction hasn't occured, then it's based off the start date.
 * Otherwise, it's based off the last-occurance date.
 **/
GDate xaccSchedXactionGetNextInstance( SchedXaction *sx );
GDate xaccSchedXactionGetInstanceAfter( SchedXaction *sx, GDate *date );

/*
 * Set the schedxaction's template transaction.  t_t_list is a glist
 * of TTInfo's as defined in SX-ttinfo.h
 * the edit dialog doesn't use this mechanism.  Maybe it should
 */

void xaccSchedXactionSetTemplateTrans(SchedXaction *sx, GList *t_t_list,
                                      GNCSession *session);

#endif /* XACC_SCHEDXACTION_H */
