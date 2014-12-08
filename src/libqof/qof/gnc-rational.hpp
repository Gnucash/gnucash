/********************************************************************
 * gnc-rational.hpp - A rational number library                     *
 * Copyright 2014 John Ralls <jralls@ceridwen.us>                   *
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
 *******************************************************************/
#include "gnc-int128.hpp"
#include "gnc-numeric.h"

struct GncDenom;

class GncRational
{
public:
    GncRational (gnc_numeric n) noexcept;
    GncRational (GncInt128 num, GncInt128 den) noexcept;
/** Conversion operator; use static_cast<gnc_numeric>(foo). */
    operator gnc_numeric() const noexcept;
/** Make a new GncRational with the opposite sign. */
    GncRational operator-() const noexcept;
/** Round/convert this to the denominator provided by d, according to d's
 * m_round value.
 */
    void round (GncDenom& d) noexcept;
/* These are mutators; in other words, they implement the equivalent of
 * operators *=, /=, +=, and -=. They return a reference to this for chaining.
 */
    GncRational& mul(const GncRational& b, GncDenom& d) noexcept;
    GncRational& div(GncRational b, GncDenom& d) noexcept;
    GncRational& add(const GncRational& b, GncDenom& d) noexcept;
    GncRational& sub(const GncRational& b, GncDenom& d) noexcept;


    GncInt128 m_num;
    GncInt128 m_den;
    GNCNumericErrorCode m_error;
};

struct GncDenom
{
    GncDenom (GncRational& a, GncRational& b, int64_t spec, unsigned int how) noexcept;
    void reduce (const GncRational& a) noexcept;
    GncInt128 get () const noexcept { return m_value; }

    enum class RoundType : int
    {
        floor = GNC_HOW_RND_FLOOR,
            ceiling = GNC_HOW_RND_CEIL,
            truncate = GNC_HOW_RND_TRUNC,
            promote = GNC_HOW_RND_PROMOTE,
            half_down = GNC_HOW_RND_ROUND_HALF_DOWN,
            half_up = GNC_HOW_RND_ROUND_HALF_UP,
            bankers = GNC_HOW_RND_ROUND,
            never = GNC_HOW_RND_NEVER,
    };
    enum class DenomType : int
    {
        exact = GNC_HOW_DENOM_EXACT,
            reduce = GNC_HOW_DENOM_REDUCE,
            lcd = GNC_HOW_DENOM_LCD,
            fixed = GNC_HOW_DENOM_FIXED,
            sigfigs = GNC_HOW_DENOM_SIGFIG,
    };

    GncInt128 m_value;
    RoundType m_round;
    DenomType m_type;
    bool m_auto;
    unsigned int m_sigfigs;
    GNCNumericErrorCode m_error;
};

