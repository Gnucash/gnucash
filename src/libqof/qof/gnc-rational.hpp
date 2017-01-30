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

#ifndef __GNC_RATIONAL_HPP__
#define __GNC_RATIONAL_HPP__

#include "gnc-numeric.h"
#include "gnc-int128.hpp"

struct GncDenom;

enum class RoundType
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

enum class DenomType
{
    den_auto = GNC_DENOM_AUTO,
    exact = GNC_HOW_DENOM_EXACT,
    reduce = GNC_HOW_DENOM_REDUCE,
    lcd = GNC_HOW_DENOM_LCD,
    fixed = GNC_HOW_DENOM_FIXED,
    sigfigs = GNC_HOW_DENOM_SIGFIG,
};

/** @ingroup QOF
 *  @brief Rational number class using GncInt128 for the numerator and denominator.
 */

class GncRational
{
public:
    GncRational() : m_num(0), m_den(1), m_error(GNC_ERROR_OK) {}
    GncRational (gnc_numeric n) noexcept;
    GncRational (GncInt128 num, GncInt128 den,
                 GNCNumericErrorCode err=GNC_ERROR_OK) noexcept
        : m_num(num), m_den(den), m_error(err) {}

/** Conversion operator; use static_cast<gnc_numeric>(foo). */
    operator gnc_numeric() const noexcept;
/** Make a new GncRational with the opposite sign. */
    GncRational operator-() const noexcept;
/**
 * Reduce this to an equivalent fraction with the least common multiple as the
 * denominator.
 *
 * @return reduced GncRational
 */
    GncRational reduce() const;
/**
 * Round to fit an int64_t, finding the closest possible approximation.
 *
 * Throws std::overflow_error if m_den is 1 and m_num is big.
 * @return rounded GncRational
 */
    GncRational round_to_numeric() const;
/** Round/convert this to the denominator provided by d, according to d's
 * m_round value.
 */
/* These are mutators; in other words, they implement the equivalent of
 * operators *=, /=, +=, and -=. They return a reference to this for chaining.
 */
    GncRational& mul(const GncRational& b, GncDenom& d);
    GncRational& div(GncRational b, GncDenom& d);
    GncRational& add(const GncRational& b, GncDenom& d);
    GncRational& sub(const GncRational& b, GncDenom& d);
    void round (GncInt128 new_den, RoundType rtype);
    void operator+=(GncRational b);
    void operator-=(GncRational b);
    void operator*=(GncRational b);
    void operator/=(GncRational b);
/** Inverts the number, equivalent of /= {1, 1} */
    GncRational& inv() noexcept;

    GncInt128 m_num;
    GncInt128 m_den;
    GNCNumericErrorCode m_error;
};

GncRational operator+(GncRational a, GncRational b);
GncRational operator-(GncRational a, GncRational b);
GncRational operator*(GncRational a, GncRational b);
GncRational operator/(GncRational a, GncRational b);


/** Encapsulates the rounding specifications computations. */
struct GncDenom
{
    GncDenom (GncRational& a, GncRational& b, int64_t spec, unsigned int how) noexcept;
    void reduce (const GncRational& a) noexcept;
    GncInt128 get () const noexcept { return m_value; }

    GncInt128 m_value;
    RoundType m_round;
    DenomType m_type;
    bool m_auto;
    unsigned int m_sigfigs;
    GNCNumericErrorCode m_error;
};
#endif //__GNC_RATIONAL_HPP__
