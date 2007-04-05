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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/
/** @addtogroup SchedXaction
    @{ */
/** @addtogroup FreqSpec Specifying Recurring Dates (Periods)

 Frequency specifications include how to let an event recur on a
 predictable frequency, from a core of once, daily, weekly, monthly or annually.
 More complex frequencies like twice weekly, quarterly, bi-annually and
 custom frequencies consisting of a series of distinct dates are built from
 the core types.
 
 Although defined, MONTH_RELATIVE is not yet supported.

 Scheduled transactions have a frequency defined by a frequency
 specifier.  This specifier, given a start date, end date [present
 in the scheduled transaction] and last occurance date [possibly not
 present] can be used to determine that a s.transaction should be
 instantiated on a given date [the given query date].

 There is a split between the UIFreqType and the 'internal' FreqType
 to reduce the complexity of some of the code involved.

 This still needs to deal with:
 . exceptions
 . 13 periods: (4 weeks/period 4x13=52 weeks/year)
 . yearly 360/365?
 . re-based frequencies [based around a non-standard [read:
   not-Jan-1-based/fiscal] year]
 . "business days" -- m-f sans holidays [per-user list thereof]

 \todo  add month-relative getter 

@{ */
/** @file FreqSpec.h
    @brief Period / Date Frequency Specification
    @author Copyright (C) 2001 Joshua Sled <jsled@asynchronous.org>
    @author Copyright (C) 2001 Ben Stanley <bds02@uow.edu.au>
    @author Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>
*/

#ifndef XACC_FREQSPEC_H
#define XACC_FREQSPEC_H

typedef struct _FreqSpecClass FreqSpecClass;

#include "gnc-engine.h"
#include <glib.h>
#include "qof.h"

/* --- type macros --- */
#define GNC_TYPE_FREQSPEC            (gnc_freqspec_get_type ())
#define GNC_FREQSPEC(o)              \
     (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_FREQSPEC, FreqSpec))
#define GNC_FREQSPEC_CLASS(k)        \
     (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_FREQSPEC, FreqSpecClass))
#define GNC_IS_FREQSPEC(o)           \
     (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_FREQSPEC))
#define GNC_IS_FREQSPEC_CLASS(k)     \
     (G_TYPE_CHECK_CLASS_TYPE ((k), GNC_TYPE_FREQSPEC))
#define GNC_FREQSPEC_GET_CLASS(o)    \
     (G_TYPE_INSTANCE_GET_CLASS ((o), GNC_TYPE_FREQSPEC, FreqSpecClass))
GType gnc_freqspec_get_type(void);


#define ENUM_LIST_TYPE(_) \
        _(INVALID,) \
        _(ONCE,) \
        _(DAILY,) \
        _(WEEKLY,) /**< Hmmm... This is sort of DAILY[7]... */ \
        _(MONTHLY,) \
        _(MONTH_RELATIVE,) \
        _(COMPOSITE,)

DEFINE_ENUM(FreqType, ENUM_LIST_TYPE) /**< \enum Frequency specification.

For BI_WEEKLY, use weekly[2] 
 SEMI_MONTHLY, use composite 
 YEARLY, monthly[12] */

AS_STRING_DEC(FreqType, ENUM_LIST_TYPE)
FROM_STRING_DEC(FreqType, ENUM_LIST_TYPE)

#define ENUM_LIST_UI(_) \
        _(UIFREQ_NONE,) /**< no frequency */ \
        _(UIFREQ_ONCE,) /**< Just occurs once */ \
        _(UIFREQ_DAILY,) /**< Repeat every day. */ \
        _(UIFREQ_DAILY_MF,) /**< Repeat Monday to Friday, skip weekend. */ \
        _(UIFREQ_WEEKLY,) /**< Repeat once each week. */ \
        _(UIFREQ_BI_WEEKLY,) /**< Repeat twice a week. */ \
        _(UIFREQ_SEMI_MONTHLY,) /**< Repeat twice a month. */ \
        _(UIFREQ_MONTHLY,) /**< Repeat once a month. */ \
        _(UIFREQ_QUARTERLY,) /**< Repeat every quarter. */ \
        _(UIFREQ_TRI_ANUALLY,) /**< Repeat three times a year. */ \
        _(UIFREQ_SEMI_YEARLY,) /**< Repeat twice a year. */ \
        _(UIFREQ_YEARLY,) /**< Repeat once a year. */ \
        _(UIFREQ_NUM_UI_FREQSPECS,) 

DEFINE_ENUM( UIFreqType, ENUM_LIST_UI) /**< \enum UIFreqType

 * The user's conception of the frequency.  It is expected that this
 * list will grow, while the former ::FreqType will not. */

AS_STRING_DEC(UIFreqType, ENUM_LIST_UI) 
FROM_STRING_DEC(UIFreqType, ENUM_LIST_UI)


/**
 * Forward declaration of FreqSpec type for storing
 * date repetition information. This is an opaque type.
 */

struct gncp_freq_spec;
typedef struct gncp_freq_spec FreqSpec;

/** PROTOTYPES ******************************************************/

/**
 * Allocates memory for a FreqSpec and initializes it.
 **/
FreqSpec* xaccFreqSpecMalloc(QofBook *book);

/**
 * destroys any private data belonging to the FreqSpec.
 * Use this for a stack object.
 */
void xaccFreqSpecCleanUp( FreqSpec *fs );

/**
 * Frees a heap allocated FreqSpec.
 * This is the opposite of xaccFreqSpecMalloc().
 **/
void xaccFreqSpecFree( FreqSpec *fs );

/**
 * Gets the type of a FreqSpec.
 **/
FreqType xaccFreqSpecGetType( FreqSpec *fs );

/**
 * Sets the type of a FreqSpec.
 * Setting the type re-initializes any spec-data; this means
 * destroying any sub-types in the case of COMPOSITE.
 *
 * \warning THESE FUNCTIONS HAVE NOT BEEN MAINTAINED THROUGH BEN'S CHANGES.
 * They need to be checked.
 **/
/* void xaccFreqSpecSetType( FreqSpec *fs, FreqType newType ); */
void xaccFreqSpecSetUIType( FreqSpec *fs, UIFreqType newUIFreqType );

/** 
 * Returns the frequency part of the FreqSpec, specifically,
 * one of the 'user-interface' enumerants.
 */
UIFreqType xaccFreqSpecGetUIType( FreqSpec *fs );


void xaccFreqSpecSetNone( FreqSpec *fs );

/**
 * Sets the type to once-off, and initialises the
 * date it occurs on.
 * Disposes of any previous data.
 */
void xaccFreqSpecSetOnceDate( FreqSpec *fs,
                              const GDate* when );

/**
 * Sets the type to DAILY. Disposes of any previous data.
 * Uses the start date to figure
 * out how many days after epoch (1/1/1900) this repeat would
 * have first occurred on if it had been running back then.
 * This is used later to figure out absolute repeat dates.
 */
void xaccFreqSpecSetDaily( FreqSpec *fs,
                           const GDate* initial_date,
                           guint interval_days );

/**
 * Sets the type to WEEKLY. Disposes of any previous data.
 * Uses the inital date to figure out the day of the week to use.
 */
void xaccFreqSpecSetWeekly( FreqSpec *fs,
                            const GDate* inital_date,
                            guint interval_weeks );

/**
 * Sets the type to MONTHLY. Disposes of any previous data.
 * Uses the inital date to figure out the day of the month to use.
 */
void xaccFreqSpecSetMonthly( FreqSpec *fs,
                             const GDate* inital_date,
                             guint interval_months );

/**
 * Sets the type to MONTH_RELATIVE. Disposes of any previous data.
 * Uses the inital date to figure out the day of the month to use.
 */
void xaccFreqSpecSetMonthRelative( FreqSpec *fs,
                                   const GDate* inital_date,
                                   guint interval_months );

/**
 * Sets the type to COMPOSITE. Disposes of any previous data.
 * You must Add some repeats to the composite before using
 * it for repeating.
 */
void xaccFreqSpecSetComposite( FreqSpec *fs );

/**
 * Returns a human-readable string of the Frequency.  This uses
 * UIFreqType to unroll the internal structure.  It is an assertion
 * failure if the FreqSpec data doesn't match the UIFreqType.
 * Caller allocates the GString [natch].
 **/
void xaccFreqSpecGetFreqStr( FreqSpec *fs, GString *str );

/** DOCUMENT ME! */
int xaccFreqSpecGetOnce( FreqSpec *fs, GDate *outGD );
/** DOCUMENT ME! */
int xaccFreqSpecGetDaily( FreqSpec *fs, int *outRepeat );
/** DOCUMENT ME! */
int xaccFreqSpecGetWeekly( FreqSpec *fs, int *outRepeat, int *outDayOfWeek );
/** DOCUMENT ME! */
int xaccFreqSpecGetMonthly( FreqSpec *fs, int *outRepeat,
                            int *outDayOfMonth, int *outMonthOffset );
/* FIXME: add month-relative */

/**
 * Returns the list of FreqSpecs in a COMPOSITE FreqSpec.
 * It is an error to use this if the type is not COMPOSITE.
 * The caller should not modify this list;
 * only add/remove composites and use this fn to get
 * the perhaps-changed list head.
 **/
GList* xaccFreqSpecCompositeGet( FreqSpec *fs );

/**
 * Adds a FreqSpec to the list in a COMPOSITE FreqSpec; if the
 * FreqSpec is not COMPOSITE, this is an assertion failure.
 **/
void xaccFreqSpecCompositeAdd( FreqSpec *fs, FreqSpec *fsToAdd );

/**
 * Computes the next instance of the FreqSpec after a given input date.
 * The object pointed at by 'out_date' is set to the computed value.
 * The 'in_date' can be any date.  It is gaurenteed that the 'out_date'
 * is strictly greater than the 'in_date'.  That is, if the 'in_date'
 * happens to be a repeat date (e.g. a previous out_date), then
 * the out_date will be the next repeat date after that.
 **/
void xaccFreqSpecGetNextInstance( FreqSpec *fs,
                                  const GDate* in_date,
                                  GDate* out_date );

/**
 * qsort-style comparison of FreqSpecs.
 * More frequently-occuring FreqSpecs are sorted before less-frequent FreqSpecs.
 * FIXME: What to do for composites?
 **/
int gnc_freq_spec_compare( FreqSpec *a, FreqSpec *b );

/** \name QOF handling.

QOF requires parameters to use get and set routines individually -
one parameter, one set routine, one get routine. QOF also passes
parameter values directly and expects to receive the parameter value
directly. These functions provide this mechanism. Note that in each
case, where the xacc.. function uses a *int, QOF uses the int.

In keeping with the rest of QOF, dates are handled as Timespec.
@{
*/
#define QOF_ID_FREQSPEC       "FreqSpec"
#define FS_UI_TYPE            "fs-frequency"
#define FS_REPEAT             "fs-repeat"
#define FS_BASE_DATE          "fs-initial-date"
#define FS_MONTH_DAY          "fs-day-of-month"
#define FS_MONTH_OFFSET       "fs-month-offset"

/** \todo Need support for monthly and weekly extra values and composite. */
gboolean FreqSpecRegister(void);

/** @} */
/** @} */
/** @} */

#endif /* XACC_FREQSPEC_H */
