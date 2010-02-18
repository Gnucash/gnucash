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
/** @file FreqSpec.h
    @brief Period / Date Frequency Specification
    @author Copyright (C) 2001 Joshua Sled <jsled@asynchronous.org>
*/

#ifndef XACC_FREQSPEC_H
#define XACC_FREQSPEC_H

#include "gnc-engine.h"
#include <glib.h>
#include "qof.h"

#define ENUM_LIST_TYPE(_) \
        _(INVALID,) \
        _(ONCE,) \
        _(DAILY,) \
        _(WEEKLY,) /**< Hmmm... This is sort of DAILY[7]... */ \
        _(MONTHLY,) \
        _(MONTH_RELATIVE,) \
        _(COMPOSITE,)

DEFINE_ENUM(FreqType, ENUM_LIST_TYPE) /**< \enum FreqType
Frequency specification.

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

/** @} */

#endif /* XACC_FREQSPEC_H */
