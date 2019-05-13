/********************************************************************
 * gnc-rational-rounding.hpp - Template functions for rounding      *
 * Copyright 2017 John Ralls <jralls@ceridwen.us>                   *
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

#ifndef __GNC_RATIONAL_ROUNDING_HPP__
#define __GNC_RATIONAL_ROUNDING_HPP__
#include "gnc-numeric.h"
#include "gnc-int128.hpp"

template <typename T> inline bool
quotient_is_positive(T dividend, T divisor)
{
    return (dividend > 0 && divisor > 0) || (dividend < 0 && divisor < 0);
}

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


template <RoundType rt>
struct RT2T
{
    RoundType value = rt;
};

/* The following templates implement the rounding policies for the convert and
 * convert_sigfigs template functions.
 */
template <typename T> inline T
round(T num, T den, T rem, RT2T<RoundType::never>)
{
    if (rem == 0)
        return num;
    throw std::domain_error("Rounding required when 'never round' specified.");
}

template <typename T> inline T
round(T num, T den, T rem, RT2T<RoundType::floor>)
{
//    std::cout << "Rounding to floor  with num " << num << " den " << den
//              << ", and rem " << rem << ".\n";
    if (rem == 0)
        return num;
    // floor num==0 that is the quotient of two numbers with opposite signs
    if (num < 0 || (num == 0 && !quotient_is_positive(rem, den)))
        return num - 1;
    return num;
}


template <> inline GncInt128
round<GncInt128>(GncInt128 num, GncInt128 den, GncInt128 rem,
                 RT2T<RoundType::floor>)
{
//    std::cout << "Rounding to floor  with num " << num << " den " << den
//              << ", and rem " << rem << ".\n";
    if (rem == 0)
        return num;
    if (num.isNeg())
        return num - 1;
    return num;
}

template <typename T> inline T
round(T num, T den, T rem, RT2T<RoundType::ceiling>)
{
    if (rem == 0)
        return num;
    if (num > 0 || (num == 0 && quotient_is_positive(rem, den)))
        return num + 1;
    return num;
}

template <> inline GncInt128
round<GncInt128>(GncInt128 num, GncInt128 den, GncInt128 rem,
      RT2T<RoundType::ceiling>)
{
    if (rem == 0)
        return num;
    if (!num.isNeg())
        return num + 1;
    return num;
}

template <typename T> inline T
round(T num, T den, T rem, RT2T<RoundType::truncate>)
{
    return num;
}

template <typename T> inline T
round(T num, T den, T rem, RT2T<RoundType::promote>)
{
    if (rem == 0)
        return num;
    if (num == 0)
        return (!quotient_is_positive(rem, den) ? -1 : 1);
    return num + (num < 0 ? -1 : 1);
}

template <> inline GncInt128
round<GncInt128>(GncInt128 num, GncInt128 den, GncInt128 rem,
                 RT2T<RoundType::promote>)
{
    if (rem == 0)
        return num;
    return num + (num.isNeg() ? -1 : 1);
}

template <typename T> inline T
round(T num, T den, T rem, RT2T<RoundType::half_down>)
{
    if (rem == 0)
        return num;
    if (std::abs(rem * 2) > std::abs(den))
    {
        if (num == 0)
            return (!quotient_is_positive(rem, den) ? -1 : 1);
        return num + (num < 0 ? -1 : 1);
    }
    return num;
}

template <> inline GncInt128
round<GncInt128>(GncInt128 num, GncInt128 den, GncInt128 rem,
                 RT2T<RoundType::half_down>)
{
    if (rem == 0)
        return num;
    if (rem.abs() * 2 > den.abs())
        return num + (num.isNeg() ? -1 : 1);
    return num;
}

template <typename T> inline T
round(T num, T den, T rem, RT2T<RoundType::half_up>)
{
    if (rem == 0)
        return num;
    if (std::abs(rem) * 2 >= std::abs(den))
    {
        if (num == 0)
            return (!quotient_is_positive(rem, den) ? -1 : 1);
        return num + (num < 0 ? -1 : 1);
    }
    return num;
}

template <> inline GncInt128
round<GncInt128>(GncInt128 num, GncInt128 den, GncInt128 rem,
                 RT2T<RoundType::half_up>)
{
    if (rem == 0)
        return num;
    if (rem.abs() * 2 >= den.abs())
        return num + (num.isNeg() ? -1 : 1);
    return num;
}

template <typename T> inline T
round(T num, T den, T rem, RT2T<RoundType::bankers>)
{
    if (rem == 0)
        return num;
    if (std::abs(rem * 2) > std::abs(den) ||
        (std::abs(rem * 2) == std::abs(den) && num % 2))
    {
        if (num == 0)
            return (!quotient_is_positive(rem, den) ? -1 : 1);
        return num + (num < 0 ? -1 : 1);
    }
    return num;
}

template <> inline GncInt128
round<GncInt128>(GncInt128 num, GncInt128 den, GncInt128 rem,
                 RT2T<RoundType::bankers>)
{
    if (rem == 0)
        return num;
    if (rem.abs() * 2 > den.abs() ||
        (rem.abs() * 2 == den.abs() && num % 2))
        return num + (num.isNeg() ? -1 : 1);
    return num;
}
#endif //__GNC_RATIONAL_ROUNDING_HPP__
