/********************************************************************\
 * FreqSpec.h -- Frequency Specification                            *
 * Copyright (C) 2001 Joshua Sled <jsled@asynchronous.org>          *
 * Copyright (C) 2001 Ben Stanley <bds02@uow.edu.au>                *
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

/********************************************************************\
This file contains private definitions and should not be used by
other parts of the engine. This is private data and is subject to
change.
Currently the only files which include this file are:
  FreqSpec.c
  gnc-freqspec-xml-v2.c
\********************************************************************/
#ifndef XACC_FREQSPECP_H
#define XACC_FREQSPECP_H

#include "FreqSpec.h"

/**
 * Scheduled transactions have a frequency defined by a frequency
 * specifier.  This specifier, given a start date, end date [present
 * in the scheduled transaction] and last occurance date [possibly not
 * present] can be used to determine that a s.transaction should be
 * instantiated on a given date [the given query date].
 *
 * There is a split between the UIFreqType and the 'internal' FreqType
 * to reduce the complexity of some of the code involved.
 *
 * Hmmm... having this in the private header file actually prevents
 * client code from allocating this 'class' on the stack.
 * Is that a problem?
 *
 * This still needs to deal with:
 * . exceptions
 * . yearly 360/365?
 * . re-based frequencies [based around a non-standard [read:
 *   not-Jan-1-based/fiscal] year]
 * . "business days" -- m-f sans holidays [per-user list thereof]
 **/
struct gncp_freq_spec {
        FreqType        type;
        UIFreqType        uift;
        union u {
                struct {
                        GDate date; /** The date on which the single event occurs. */
                } once;
                struct {
                        guint interval_days; /** number of days from one repeat to the next. */
                        guint offset_from_epoch; /** epoch is defined by glib to be 1/1/1. Offset measured in days. 0 <= offset < interval */
                } daily;
                struct {
                        /* A week here is measured as 7 days. The first week starts at epoch.
                         * 1/1/1 was a ?. */
                        guint interval_weeks; /** number of weeks from one repeat to the next. */
                        guint offset_from_epoch; /* offset measured in days.
                                                  * This combines the week 
                                                  * offset and the day of the 
                                                  * week offset.  */
/*                        guint offset_from_epoch;*/ /* offset measured in weeks, 0 <= offset < interval */
/*                        guint day_of_week;*/ /* I'm not sure what days each value represents, but it's not important. */
                } weekly;
                struct {
                        guint interval_months; /** number of months from one repeat to the next. */
                        guint offset_from_epoch; /* offset measured in months */
                        guint day_of_month; /* Which day of the month it occurs on. */
                } monthly;
                struct {
                        guint interval_months; /** Number of months from one repeat to the next. */
                        guint offset_from_epoch; /* offset measured in months */
                        guint weekday; /* stores a value equivalent to a GDateWeekday. */
                        guint occurrence; /* the 1st occurrence to the 5th occurrence. */
                } month_relative;
                struct {
                        /** A list of specs for a composite freq. */
                        GList *subSpecs;
                } composites;
                /**
                 * The dateAnchor anchors the spec to determinable days.
                 *
                 * ONCE:
                 *   dA[0] contains time_t
                 * DAILY:
                 *   dA[0] contains day multiplier
                 *   dA[1] contains offset from epoch.
                 * WEEKLY:
                 *   dA[0] contains week multiplier
                 *   dA[1] contains 0..6 [sun-based]
                 * SEMI_MONTHLY: bstanley disused...
                 *   dA[0] contains month multiplier
                 *   dA[1] contains the first date-of-month,
                 *   dA[2] the second.
                 * MONTHLY:
                 *   dA[0] contains month multiplier
                 *   dA[1] contains the date-of-month
                 * MONTH_RELATIVE:
                 *   dA[0] continas month multiplier
                 *   dA[1] contains week number [1..5, 6=="last"]
                 *    [1..5 is really 1..4.428 [31/7], but it's a UI issue]
                 *   dA[2] contains 0..6 [sun-based day of week]
                 * COMPOSITE:
                 *   ... list ...
                 * RELATIVE:
                 *   ... don't know yet ...
                 **/
        } s;
        GUID guid;
};

#endif /* XACC_FREQSPECP_H */
