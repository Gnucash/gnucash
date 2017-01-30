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

#include <sstream>
#include "gnc-rational.hpp"
#include "gnc-numeric.hpp"


GncRational::GncRational(GncNumeric n) noexcept :
    m_num(n.num()), m_den(n.denom()), m_error(GNC_ERROR_OK)
{
    if (m_den.isNeg())
    {
        m_num *= -m_den;
        m_den = 1;
    }
}

GncRational::GncRational (gnc_numeric n) noexcept :
    m_num (n.num), m_den (n.denom), m_error {GNC_ERROR_OK}
{
    if (m_den.isNeg())
    {
          m_num *= -m_den;
          m_den = 1;
    }
}

GncRational::operator gnc_numeric () const noexcept
{
     if (m_num.isOverflow() || m_num.isNan() ||
        m_den.isOverflow() || m_den.isNan())
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    if (m_error != GNC_ERROR_OK)
        return gnc_numeric_error (m_error);
    try
    {
        return {static_cast<int64_t>(m_num), static_cast<int64_t>(m_den)};
    }
    catch (std::overflow_error)
    {
        return gnc_numeric_error (GNC_ERROR_OVERFLOW);
    }
}

GncRational
GncRational::operator-() const noexcept
{
    GncRational b(*this);
    b.m_num = - b.m_num;
    return b;
}

GncRational&
GncRational::inv () noexcept
{
    if (m_den < 0)
    {
        m_num *= -m_den;
        m_den = 1;
    }
    std::swap(m_num, m_den);

    reduce();
    return *this;
}

GncRational
operator+(GncRational a, GncRational b)
{
    if (a.m_error || b.m_error)
    {
        if (b.m_error)
            return GncRational(0, 1, b.m_error);
        return GncRational(0, 1, a.m_error);
    }
    GncInt128 lcm = a.m_den.lcm(b.m_den);
    GncInt128 num(a.m_num * lcm / a.m_den + b.m_num * lcm / b.m_den);
    if (lcm.isOverflow() || lcm.isNan() || num.isOverflow() || num.isNan())
        return GncRational(0, 1, GNC_ERROR_OVERFLOW);
    GncRational retval(num, lcm);
    return retval;
}

GncRational
operator-(GncRational a, GncRational b)
{
    GncRational retval = a + (-b);
    return retval;
}

GncRational
operator*(GncRational a, GncRational b)
{
    if (a.m_error || b.m_error)
    {
        if (b.m_error)
            return GncRational(0, 1, b.m_error);
        return GncRational(0, 1, a.m_error);
    }
    GncInt128 num (a.m_num * b.m_num), den(a.m_den * b.m_den);
    if (num.isOverflow() || num.isNan() || den.isOverflow() || den.isNan())
        return GncRational(0, 1, GNC_ERROR_OVERFLOW);
    GncRational retval(num, den);
    return retval;
}

GncRational
operator/(GncRational a, GncRational b)
{
    if (a.m_error || b.m_error)
    {
        if (b.m_error)
            return GncRational(0, 1, b.m_error);
        return GncRational(0, 1, a.m_error);
    }
    if (b.m_num.isNeg())
    {
        a.m_num = -a.m_num;
        b.m_num = -b.m_num;
    }

   /* q = (a_num * b_den)/(b_num * a_den). If a_den == b_den they cancel out
     * and it's just a_num/b_num.
     */
    if (a.m_den == b.m_den)
        return GncRational(a.m_num, b.m_num);

    /* Protect against possibly preventable overflow: */
    if (a.m_num.isBig() || a.m_den.isBig() ||
        b.m_num.isBig() || b.m_den.isBig())
    {
        GncInt128 gcd = b.m_den.gcd(a.m_den);
        b.m_den /= gcd;
        a.m_den /= gcd;
    }

    GncInt128 num(a.m_num * b.m_den), den(a.m_den * b.m_num);
    if (num.isOverflow() || num.isNan() || den.isOverflow() || den.isNan())
        return GncRational(0, 1, GNC_ERROR_OVERFLOW);
    return GncRational(num, den);
}

void
GncRational::operator+=(GncRational b)
{
    GncRational new_val = *this + b;
    *this = std::move(new_val);
}

void
GncRational::operator-=(GncRational b)
{
    GncRational new_val = *this - b;
    *this = std::move(new_val);
}

void
GncRational::operator*=(GncRational b)
{
    GncRational new_val = *this * b;
    *this = std::move(new_val);
}

void
GncRational::operator/=(GncRational b)
{
    GncRational new_val = *this / b;
    *this = std::move(new_val);
}

void
GncRational::round (GncInt128 new_den, RoundType rtype)
{
    if (new_den == 0) new_den = m_den;
    if (!(m_num.isBig() || new_den.isBig() ))
    {
        if (m_den == new_den)
            return;

        if (m_num.isZero())
        {
            m_den = new_den;
            return;
        }
    }
    GncInt128 new_num {}, remainder {};
    if (new_den.isNeg())
        m_num.div(-new_den * m_den, new_num, remainder);
    else if (new_den != m_den)
        (m_num * new_den).div(m_den, new_num, remainder);
    else
    {
        new_num = m_num;
        new_den = m_den;
        remainder = 0;
    }
    if (new_num.isOverflow() || new_den.isOverflow() || remainder.isOverflow())
        throw std::overflow_error("Overflow during rounding.");
    if (new_num.isNan() || new_den.isNan() || remainder.isNan())
    {
        throw std::underflow_error("Underflow during rounding.");
    }
    if (remainder.isZero() && !(new_num.isBig() || new_den.isBig()))
    {
        m_num = new_num;
        m_den = new_den;
        return;
    }

    if (new_num.isBig() || new_den.isBig())
    {
      /* First, try to reduce it */
        GncInt128 gcd = new_num.gcd(new_den);
        if (!(gcd.isNan() || gcd.isOverflow()))
        {
            new_num /= gcd;
            new_den /= gcd;
            remainder /= gcd;
        }

/* if that didn't work, shift both num and den down until neither is "big", then
 * fall through to rounding.
 */
        while (rtype != RoundType::never && new_num && new_num.isBig() &&
               new_den && new_den.isBig())
        {
            new_num >>= 1;
            new_den >>= 1;
            remainder >>= 1;
        }
    }
    if (remainder == 0)
    {
        m_num = new_num;
        m_den = new_den;
        return;
    }
/* If we got here, then we can't exactly represent the rational with
 * new_denom. We must either round or punt.
 */
    switch (rtype)
    {
    case RoundType::never:
        m_error = GNC_ERROR_REMAINDER;
        return;
    case RoundType::floor:
        if (new_num.isNeg()) ++new_num;
        break;
    case RoundType::ceiling:
        if (! new_num.isNeg()) ++new_num;
        break;
    case RoundType::truncate:
        break;
    case RoundType::promote:
        new_num += new_num.isNeg() ? -1 : 1;
        break;
    case RoundType::half_down:
        if (new_den.isNeg())
        {
            if (remainder * 2 > m_den * new_den)
                new_num += new_num.isNeg() ? -1 : 1;
        }
        else if (remainder * 2 > m_den)
            new_num += new_num.isNeg() ? -1 : 1;
        break;
    case RoundType::half_up:
        if (new_den.isNeg())
        {
            if (remainder * 2 >= m_den * new_den)
                new_num += new_num.isNeg() ? -1 : 1;
        }
        else if (remainder * 2 >= m_den)
            new_num += new_num.isNeg() ? -1 : 1;
        break;
    case RoundType::bankers:
        if (new_den.isNeg())
        {
            if (remainder * 2 > m_den * -new_den ||
                (remainder * 2 == m_den * -new_den && new_num % 2))
                new_num += new_num.isNeg() ? -1 : 1;
        }
        else
        {
            if (remainder * 2 > m_den ||
                (remainder * 2 == m_den && new_num % 2))
                new_num += new_num.isNeg() ? -1 : 1;
        }
        break;
    }
    m_num = new_num;
    m_den = new_den;
    return;
}

GncRational
GncRational::reduce() const
{
    auto gcd = m_den.gcd(m_num);
    if (gcd.isNan() || gcd.isOverflow())
        throw std::overflow_error("Reduce failed, calculation of gcd overflowed.");
    return GncRational(m_num / gcd, m_den / gcd);
}

GncRational
GncRational::round_to_numeric() const
{
    if (m_num.isZero())
        return GncRational(); //Default constructor makes 0/1
    if (!(m_num.isBig() || m_den.isBig()))
        return *this;
    if (m_num.abs() > m_den)
    {
        auto quot(m_num / m_den);
        if (quot.isBig())
        {
            std::ostringstream msg;
            msg << " Cannot be represented as a "
                << "GncNumeric. Its integer value is too large.\n";
            throw std::overflow_error(msg.str());
        }
        GncRational new_rational(*this);
        GncRational scratch(1, 1);
        new_rational.round(m_den / (m_num.abs() >> 62), RoundType::half_down);
        return new_rational;
    }
    auto quot(m_den / m_num);
    if (quot.isBig())
        return GncRational(); //Smaller than can be represented as a GncNumeric
    auto divisor = m_den >> 62;
    if (m_num.isBig())
    {
        GncInt128 oldnum(m_num), num, rem;
        oldnum.div(divisor, num, rem);
        auto den = m_den / divisor;
        num += rem * 2 >= den ? 1 : 0;
        GncRational new_rational(num, den);
        return new_rational;
    }
    GncRational new_rational(*this);
    GncRational scratch(1, 1);
    new_rational.round(m_den / divisor, RoundType::half_down);
    return new_rational;
}
