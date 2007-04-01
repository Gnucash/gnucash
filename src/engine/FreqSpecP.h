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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/* *******************************************************************\
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

struct gncp_freq_spec 
{
        QofInstance       entity;
        FreqType        type;
        UIFreqType      uift;
        union u {
                struct {
                         /* The date on which the single event occurs. */
                        GDate date;
                } once;
                struct {
                         /* number of days from one repeat to the next. */
                        guint interval_days;
                         /* epoch is defined by glib to be 1/1/1. Offset
                             measured in days. 0 <= offset < interval */
                        guint offset_from_epoch;
                } daily;
                struct {
                        /* A week here is measured as 7 days. The first week starts at epoch.
                         * 1/1/1 was a ?. */

                        /* number of weeks from one repeat to the next. */
                        guint interval_weeks;
                         /* offset measured in days.  This combines the week
                          * offset and the day of the week offset.  */
                        guint offset_from_epoch;
                         /* guint offset_from_epoch;*/ /* offset measured in weeks, 0 <= offset < interval */
                         /* guint day_of_week;*/ /* I'm not sure what days each value represents, but it's not important. */
                } weekly;
                struct {
                         /* number of months from one repeat to the next. */
                        guint interval_months;
                         /* offset measured in months */
                        guint offset_from_epoch;
                         /* Which day of the month it occurs on. */
                        guint day_of_month;
                } monthly;
                struct {
                         /* Number of months from one repeat to the next. */
                        guint interval_months;
                         /* offset measured in months */
                        guint offset_from_epoch;
                        /* stores a value equivalent to a GDateWeekday. */
                        guint weekday;
                         /* the 1st occurrence to the 5th occurrence. */
                        guint occurrence;
                } month_relative;
                struct {
                        /* A list of specs for a composite freq. */
                        GList *subSpecs;
                } composites;
        } s;
        /* temporary storage for QOF */
        gint            value;
};

#endif /* XACC_FREQSPECP_H */
