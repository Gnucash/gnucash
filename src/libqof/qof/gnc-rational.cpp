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
    m_num(n.num()), m_den(n.denom())
{
    if (m_den.isNeg())
    {
        m_num *= -m_den;
        m_den = 1;
    }
}

GncRational::GncRational (gnc_numeric n) noexcept :
    m_num (n.num), m_den (n.denom)
{
    if (m_den.isNeg())
    {
          m_num *= -m_den;
          m_den = 1;
    }
}

bool
GncRational::valid() const noexcept
{
    if (m_num.valid() && m_den.valid())
        return true;
    return false;
}

bool
GncRational::is_big() const noexcept
{
    if (m_num.isBig() || m_den.isBig())
        return true;
    return false;
}

GncRational::operator gnc_numeric () const noexcept
{
    if (!valid())
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
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
    if (!(a.valid() && b.valid()))
        throw std::range_error("Operator+ called with out-of-range operand.");
    GncInt128 lcm = a.m_den.lcm(b.m_den);
    GncInt128 num(a.m_num * lcm / a.m_den + b.m_num * lcm / b.m_den);
    if (!(lcm.valid() && num.valid()))
        throw std::overflow_error("Operator+ overflowed.");
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
    if (!(a.valid() && b.valid()))
        throw std::range_error("Operator* called with out-of-range operand.");
    GncInt128 num (a.m_num * b.m_num), den(a.m_den * b.m_den);
    if (!(num.valid() && den.valid()))
        throw std::overflow_error("Operator* overflowed.");
    GncRational retval(num, den);
    return retval;
}

GncRational
operator/(GncRational a, GncRational b)
{
    if (!(a.valid() && b.valid()))
        throw std::range_error("Operator/ called with out-of-range operand.");
    if (b.m_num == 0)
        throw std::underflow_error("Divide by 0.");
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
    if (!(num.valid() && den.valid()))
        throw std::overflow_error("Operator/ overflowed.");
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

GncRational::round_param
GncRational::prepare_conversion (GncInt128 new_denom) const
{
    if (new_denom == m_den || new_denom == GNC_DENOM_AUTO)
        return {m_num, m_den, 0};
    GncRational conversion(new_denom, m_den);
    auto red_conv = conversion.reduce();
    GncInt128 old_num(m_num);
    auto new_num = old_num * red_conv.m_num;
    auto rem = new_num % red_conv.m_den;
    new_num /= red_conv.m_den;
    return {new_num, red_conv.m_den, rem};
}

GncInt128
GncRational::sigfigs_denom(unsigned figs) const noexcept
{
    auto num_abs = m_num.abs();
    bool not_frac = num_abs > m_den;
    int64_t val{ not_frac ? num_abs / m_den : m_den / num_abs };
    unsigned digits{};
    while (val >= 10)
    {
        ++digits;
        val /= 10;
    }
    return not_frac ? powten(figs - digits - 1) : powten(figs + digits);
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
        GncRational new_v(*this);
        new_v = new_v.convert<RoundType::half_down>(m_den / (m_num.abs() >> 62));
        return new_v;
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
    GncRational new_v(*this);
    new_v = new_v.convert<RoundType::half_down>(m_den / divisor);
    return new_v;
}
