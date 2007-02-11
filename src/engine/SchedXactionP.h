/********************************************************************\
 * SchedXactionP.h -- Scheduled Transaction private header          *
 * Copyright (C) 2001 Linux Developers Group                        *
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

#ifndef XACC_SCHEDXACTION_P_H
#define XACC_SCHEDXACTION_P_H

#include "SchedXaction.h"

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
struct gncp_SchedXaction
{
  QofInstance     inst;
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

  /* the current instance-count of the SX. */
  gint            instance_num;
  
  gboolean        enabled;
  gboolean        autoCreateOption;
  gboolean        autoCreateNotify;
  gint            advanceCreateDays;
  gint            advanceRemindDays;
 
  Account        *template_acct;
  
  /** The list of deferred SX instances.  This list is of temporalStateData
   * instances.  */
  GList /* <temporalStateData*> */ *deferredList;
};

/** Just the variable temporal bits from the SX structure. */
typedef struct _temporalStateData {
  GDate last_date;
  gint num_occur_rem;
  gint num_inst;
} temporalStateData;

#define xaccSchedXactionSetGUID(X,G) qof_entity_set_guid(QOF_ENTITY(X),(G))

#endif /* XACC_SCHEDXACTION_P_H */
