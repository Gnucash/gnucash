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

#include <time.h>
#include <glib.h>

#include "GNCId.h"
#include "FreqSpec.h"
#include "date.h"
#include "kvp_frame.h"
#include "gnc-engine.h"

/* 
 * #defines for kvp_frame strings
 * FIXME: Is this the right spot for them <rgmerk>?
 * FIXME: No, they should be private data and there should
 *        be an api for getting/setting the values <dave_p>
 */

#define GNC_SX_ID                    "sched-xaction"
#define GNC_SX_ACCOUNT               "account"
#define GNC_SX_CREDIT_FORMULA        "credit-formula"
#define GNC_SX_DEBIT_FORMULA         "debit-formula"
#define GNC_SX_SHARES                "shares"
#define GNC_SX_AMOUNT                "amnt"
#define GNC_SX_FROM_SCHED_XACTION    "from-sched-xaction"

typedef struct gncp_SchedXaction SchedXaction;

/**
 * Creates and initializes a scheduled transaction.
 **/
SchedXaction *xaccSchedXactionMalloc(GNCBook *book);

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
/**
 * Set to an invalid GDate to turn off 'end-date' definition.
 **/
void xaccSchedXactionSetEndDate( SchedXaction *sx, GDate* newEnd );

GDate* xaccSchedXactionGetLastOccurDate( SchedXaction *sx );
void xaccSchedXactionSetLastOccurDate( SchedXaction *sx, GDate* newLastOccur );

/**
 * Returns true if the scheduled transaction has a defined number of
 * occurances, false if not.
 **/
gboolean xaccSchedXactionHasOccurDef( SchedXaction *sx );
gint xaccSchedXactionGetNumOccur( SchedXaction *sx );
/**
 * Set to '0' to turn off number-of-occurances definition.
 **/
void xaccSchedXactionSetNumOccur( SchedXaction *sx, gint numNum );
gint xaccSchedXactionGetRemOccur( SchedXaction *sx );
void xaccSchedXactionSetRemOccur( SchedXaction *sx, gint numRemain );

/**
 * Set the instance count.  This is incremented by one for every created
 * instance of the SX.
 * @param stateData may be NULL.
 **/
gint gnc_sx_get_instance_count( SchedXaction *sx, void *stateData );
/**
 * Sets the instance count to something other than the default.  As the
 * default is the incorrect value '0', callers should DTRT here.
 **/
void gnc_sx_set_instance_count( SchedXaction *sx, gint instanceNum );

GList *xaccSchedXactionGetSplits( SchedXaction *sx );
void xaccSchedXactionSetSplits( SchedXaction *sx, GList *newSplits );

void xaccSchedXactionGetAutoCreate( SchedXaction *sx,
                                    gboolean *outAutoCreate,
                                    gboolean *outNotify );
void xaccSchedXactionSetAutoCreate( SchedXaction *sx,
                                    gboolean newAutoCreate,
                                    gboolean newNotify );

gint xaccSchedXactionGetAdvanceCreation( SchedXaction *sx );
void xaccSchedXactionSetAdvanceCreation( SchedXaction *sx, gint createDays );

gint xaccSchedXactionGetAdvanceReminder( SchedXaction *sx );
void xaccSchedXactionSetAdvanceReminder( SchedXaction *sx, gint reminderDays );

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

///@{
/**
 * Next-Instance state data.
 *
 * If you're looking at to-create [but not-yet-created] scheduled
 * transactions, you'll want to use this to keep track of any
 * SX-type-specific information that relates to the sequence and when it
 * should end.  This is an opaque structure to the caller; it should be
 * created and freed with the following functions, and passed into the
 * GetNextInstance and GetInstanceAfter functions.
 *
 * The necessity for this arose in dealing with SXes with some number of
 * remaining occurances, and thinking about how to keep track of this when
 * looking at reminders... and generally thinking about not wanting the
 * caller to know every possible thing that needs to be kept track of in a
 * forward-looking sequence of SXes.
 *
 * 	THESE (3) FUNCTIONS ARE DEPRECATED.  See/Use the
 * 	'temporal_state_snapshot' functions below.
 **/
void *xaccSchedXactionCreateSequenceState( SchedXaction *sx );
void xaccSchedXactionIncrSequenceState( SchedXaction *sx, void *stateData );
void xaccSchedXactionDestroySequenceState( void *stateData );
///@}

///@{
/**
 * Temporal state data.
 *
 * These functions allow us to opaquely save the entire temporal state of
 * ScheduledTransactions.  This is used by the "since-last-run" dialog to
 * store the initial state of SXes before modification ... if it later
 * becomes necessary to revert an entire set of changes, we can 'revert' the
 * SX without having to rollback all the individual state changes.
 *
 * NOTE that this is similar to the above SequenceState interface, and
 * perhaps can be seen as entailing the above interface.  In fact, the above
 * interface is deprecated in favor of this one.
 **/
void *gnc_sx_create_temporal_state_snapshot( SchedXaction *sx );
void gnc_sx_revert_to_temporal_state_snapshot( SchedXaction *sx, void *stateData );
void gnc_sx_destroy_temporal_state_snapshot( void *stateData );
///@}

/**
 * Returns the next occurance of a scheduled transaction.  If the
 * transaction hasn't occured, then it's based off the start date.
 * Otherwise, it's based off the last-occurance date.
 **/
GDate xaccSchedXactionGetNextInstance( SchedXaction *sx, void *stateData );
GDate xaccSchedXactionGetInstanceAfter( SchedXaction *sx,
                                        GDate *date,
                                        void *stateData );

/*
 * Set the schedxaction's template transaction.  t_t_list is a glist
 * of TTInfo's as defined in SX-ttinfo.h
 * the edit dialog doesn't use this mechanism.  Maybe it should
 */
void xaccSchedXactionSetTemplateTrans(SchedXaction *sx, GList *t_t_list,
                                      GNCBook *book);

#endif /* XACC_SCHEDXACTION_H */
