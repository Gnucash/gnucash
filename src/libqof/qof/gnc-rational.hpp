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

class GncNumeric;
enum class RoundType;
enum class DenomType;

/** @ingroup QOF
 *  @brief Rational number class using GncInt128 for the numerator and denominator.
 */

class GncRational
{
public:
    GncRational() : m_num(0), m_den(1) {}
    GncRational (gnc_numeric n) noexcept;
    GncRational(GncNumeric n) noexcept;
    GncRational (GncInt128 num, GncInt128 den) noexcept
        : m_num(num), m_den(den) {}
    GncRational(const GncRational& rhs) = default;
    GncRational(GncRational&& rhs) = default;
    GncRational& operator=(const GncRational& rhs) = default;
    GncRational& operator=(GncRational&& rhs) = default;
/** Report if both members are valid numbers.
 * \return true if neither numerator nor denominator are Nan or Overflowed.
 */
    bool valid() const noexcept;
/** Report if either numerator or denominator are too big to fit in an int64_t.
 * \return true if either is too big.
 */
    bool is_big() const noexcept;
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
    void round (GncInt128 new_den, RoundType rtype);
    void operator+=(GncRational b);
    void operator-=(GncRational b);
    void operator*=(GncRational b);
    void operator/=(GncRational b);
/** Inverts the number, equivalent of /= {1, 1} */
    GncRational& inv() noexcept;

    GncInt128 m_num;
    GncInt128 m_den;
};

GncRational operator+(GncRational a, GncRational b);
GncRational operator-(GncRational a, GncRational b);
GncRational operator*(GncRational a, GncRational b);
GncRational operator/(GncRational a, GncRational b);

#endif //__GNC_RATIONAL_HPP__
