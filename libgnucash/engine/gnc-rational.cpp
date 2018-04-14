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
    catch (std::overflow_error&)
    {
        return gnc_numeric_error (GNC_ERROR_OVERFLOW);
    }
}

GncRational
GncRational::operator-() const noexcept
{
    return GncRational(-m_num, m_den);
}

GncRational
GncRational::inv () const noexcept
{
    if (m_num == 0)
        return *this;
    if (m_num < 0)
        return GncRational(-m_den, -m_num);
    return GncRational(m_den, m_num);
}

GncRational
GncRational::abs() const noexcept
{
    if (m_num < 0)
        return -*this;
    return *this;
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

int
GncRational::cmp(GncRational b)
{
    if (m_den == b.denom())
    {
        auto b_num = b.num();
        return m_num < b_num ? -1 : b_num < m_num ? 1 : 0;
    }
    auto gcd = m_den.gcd(b.denom());
    GncInt128 a_num(m_num * b.denom() / gcd), b_num(b.num() * m_den / gcd);
    return a_num < b_num ? -1 : b_num < a_num ? 1 : 0;
}

GncRational::round_param
GncRational::prepare_conversion (GncInt128 new_denom) const
{
    if (new_denom == m_den || new_denom == GNC_DENOM_AUTO)
        return {m_num, m_den, 0};
    GncRational conversion(new_denom, m_den);
    auto red_conv = conversion.reduce();
    GncInt128 old_num(m_num);
    auto new_num = old_num * red_conv.num();
    if (new_num.isOverflow())
        throw std::overflow_error("Conversion overflow");
    auto rem = new_num % red_conv.denom();
    new_num /= red_conv.denom();
    return {new_num, red_conv.denom(), rem};
}

GncInt128
GncRational::sigfigs_denom(unsigned figs) const noexcept
{
    if (m_num == 0)
        return 1;

    auto num_abs = m_num.abs();
    bool not_frac = num_abs > m_den;
    int64_t val{ not_frac ? num_abs / m_den : m_den / num_abs };
    unsigned digits{};
    while (val >= 10)
    {
        ++digits;
        val /= 10;
    }
    return not_frac ? 
            powten(digits < figs ? figs - digits - 1 : 0) : 
            powten(figs + digits);
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
    unsigned int ll_bits = GncInt128::legbits;
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
        GncRational new_v;
        while (new_v.num().isZero())
        {
            try
            {
                new_v = convert<RoundType::half_down>(m_den / (m_num.abs() >> ll_bits));
                if (new_v.is_big())
                {
                    --ll_bits;
                    new_v = GncRational();
                }
            }
            catch(const std::overflow_error& err)
            {
                --ll_bits;
            }
        }
        return new_v;
    }
    auto quot(m_den / m_num);
    if (quot.isBig())
        return GncRational(); //Smaller than can be represented as a GncNumeric
    GncRational new_v;
    while (new_v.num().isZero())
    {
        auto divisor = m_den >> ll_bits;
        if (m_num.isBig())
        {
            GncInt128 oldnum(m_num), num, rem;
            oldnum.div(divisor, num, rem);
            auto den = m_den / divisor;
            num += rem * 2 >= den ? 1 : 0;
            if (num.isBig() || den.isBig())
            {
                --ll_bits;
                continue;
            }
            GncRational new_rational(num, den);
            return new_rational;
        }
        new_v = convert<RoundType::half_down>(m_den / divisor);
        if (new_v.is_big())
        {
            --ll_bits;
            new_v = GncRational();
        }
    }
    return new_v;
}

GncRational
operator+(GncRational a, GncRational b)
{
    if (!(a.valid() && b.valid()))
        throw std::range_error("Operator+ called with out-of-range operand.");
    GncInt128 lcm = a.denom().lcm(b.denom());
    GncInt128 num(a.num() * lcm / a.denom() + b.num() * lcm / b.denom());
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
    GncInt128 num (a.num() * b.num()), den(a.denom() * b.denom());
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
    auto a_num = a.num(), b_num = b.num(), a_den = a.denom(), b_den = b.denom();
    if (b_num == 0)
        throw std::underflow_error("Divide by 0.");
    if (b_num.isNeg())
    {
        a_num = -a_num;
        b_num = -b_num;
    }

   /* q = (a_num * b_den)/(b_num * a_den). If a_den == b_den they cancel out
     * and it's just a_num/b_num.
     */
    if (a_den == b_den)
        return GncRational(a_num, b_num);

    /* Protect against possibly preventable overflow: */
    if (a_num.isBig() || a_den.isBig() ||
        b_num.isBig() || b_den.isBig())
    {
        GncInt128 gcd = b_den.gcd(a_den);
        b_den /= gcd;
        a_den /= gcd;
    }

    GncInt128 num(a_num * b_den), den(a_den * b_num);
    if (!(num.valid() && den.valid()))
        throw std::overflow_error("Operator/ overflowed.");
    return GncRational(num, den);
}
