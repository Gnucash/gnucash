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

#include "gnc-rational.hpp"

static const gint64 pten[] = { 1, 10, 100, 1000, 10000, 100000, 1000000,
			       10000000, 100000000, 1000000000, 10000000000,
			       100000000000, 1000000000000, 10000000000000,
			       100000000000000, 10000000000000000,
			       100000000000000000, 1000000000000000000};
static const int POWTEN_OVERFLOW {-5};

static inline gint64
powten (int exp)
{
    if (exp > 18 || exp < -18)
	return POWTEN_OVERFLOW;
    return exp < 0 ? -pten[-exp] : pten[exp];
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

GncRational::GncRational (GncInt128 num, GncInt128 den) noexcept :
    m_num (num), m_den (den), m_error {}
{
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
GncRational::mul (const GncRational& b, GncDenom& d) noexcept
{
    if (m_error || b.m_error)
    {
        if (b.m_error)
            m_error = b.m_error;
        return *this;
    }
    m_num *= b.m_num;
    m_den *= b.m_den;
    round (d);
    return *this;
}

GncRational&
GncRational::div (GncRational b, GncDenom& d) noexcept
{
    if (m_error || b.m_error)
    {
        if (b.m_error)
            m_error = b.m_error;
        return *this;
    }

     if (b.m_num.isNeg())
    {
        m_num = -m_num;
        b.m_num = -b.m_num;
    }

   /* q = (a_num * b_den)/(b_num * a_den). If a_den == b_den they cancel out
     * and it's just a_num/b_num.
     */
    if (m_den == b.m_den)
    {
        m_den = b.m_num;
        round(d);
        return *this;
    }
    /* Protect against possibly preventable overflow: */
    if (m_num.isBig() || m_den.isBig() ||
        b.m_num.isBig() || b.m_den.isBig())
    {
        GncInt128 gcd = b.m_den.gcd(m_den);
        b.m_den /= gcd;
        m_den /= gcd;
    }

    m_num *= b.m_den;
    m_den *= b.m_num;
    round (d);
    return *this;
}

GncRational&
GncRational::add (const GncRational& b, GncDenom& d) noexcept
{
    if (m_error || b.m_error)
    {
        if (b.m_error)
            m_error = b.m_error;
        return *this;
    }
    GncInt128 lcm = m_den.lcm (b.m_den);
    m_num = m_num * lcm / m_den + b.m_num * lcm / b.m_den;
    m_den = lcm;
    round (d);
    return *this;
}

GncRational&
GncRational::sub (const GncRational& b, GncDenom& d) noexcept
{
    return add(-b, d);
}

void
GncRational::round (GncDenom& denom) noexcept
{
    denom.reduce (*this);
    if (m_error == GNC_ERROR_OK && denom.m_error != GNC_ERROR_OK)
    {
        m_error = denom.m_error;
        return;
    }
    GncInt128 new_den = denom.get();
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
    else
        (m_num * new_den).div(m_den, new_num, remainder);

    if (remainder.isZero() && !(new_num.isBig() || new_den.isBig()))
    {
        m_num = new_num;
        m_den = new_den;
        return;
    }

    if (new_num.isBig() || new_den.isBig())
    {
        if (!denom.m_auto)
        {
            m_error = GNC_ERROR_OVERFLOW;
            return;
        }

      /* First, try to reduce it */
        GncInt128 gcd = new_num.gcd(new_den);
        new_num /= gcd;
        new_den /= gcd;
        remainder /= gcd;

/* if that didn't work, shift both num and den down until neither is "big", th
 * fall through to rounding.
 */
        while (new_num && new_num.isBig() && new_den && new_den.isBig())
        {
            new_num >>= 1;
            new_den >>= 1;
            remainder >>= 1;
        }
    }

/* If we got here, then we can't exactly represent the rational with
 * new_denom. We must either round or punt.
 */
    switch (denom.m_round)
    {
    case GncDenom::RoundType::never:
        m_error = GNC_ERROR_REMAINDER;
        return;
    case GncDenom::RoundType::floor:
        if (new_num.isNeg()) ++new_num;
        break;
    case GncDenom::RoundType::ceiling:
        if (! new_num.isNeg()) ++new_num;
        break;
    case GncDenom::RoundType::truncate:
        break;
    case GncDenom::RoundType::promote:
        new_num += new_num.isNeg() ? -1 : 1;
        break;
    case GncDenom::RoundType::half_down:
        if (new_den.isNeg())
        {
            if (remainder * 2 > m_den * new_den)
                new_num += new_num.isNeg() ? -1 : 1;
        }
        else if (remainder * 2 > m_den)
            new_num += new_num.isNeg() ? -1 : 1;
        break;
    case GncDenom::RoundType::half_up:
        if (new_den.isNeg())
        {
            if (remainder * 2 >= m_den * new_den)
                new_num += new_num.isNeg() ? -1 : 1;
        }
        else if (remainder * 2 >= m_den)
            new_num += new_num.isNeg() ? -1 : 1;
        break;
    case GncDenom::RoundType::bankers:
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

GncDenom::GncDenom (GncRational& a, GncRational& b,
                    int64_t spec, uint how) noexcept :
    m_value (spec),
    m_round (static_cast<GncDenom::RoundType>(how & GNC_NUMERIC_RND_MASK)),
    m_type (static_cast<GncDenom::DenomType>(how & GNC_NUMERIC_DENOM_MASK)),
    m_auto (spec == GNC_DENOM_AUTO),
    m_sigfigs ((how & GNC_NUMERIC_SIGFIGS_MASK) >> 8),
    m_error (GNC_ERROR_OK)

{

    if (!m_auto)
        return;
    switch (m_type)
    {
    case DenomType::fixed:
        if (a.m_den == b.m_den)
        {
            m_value = a.m_den;
        }
        else if (b.m_num == 0)
        {
            m_value = a.m_den;
            b.m_den = a.m_den;
        }
        else if (a.m_num == 0)
        {
            m_value = b.m_den;
            a.m_den = b.m_den;
        }
        else
        {
            m_error = GNC_ERROR_DENOM_DIFF;
        }
        m_auto = false;
        break;

    case DenomType::lcd:
        m_value = a.m_den.lcm(b.m_den);
        m_auto = false;
        break;
    default:
        break;

    }
}

void
GncDenom::reduce (const GncRational& a) noexcept
{
    if (!m_auto)
        return;
    switch (m_type)
    {
    default:
        break;
    case DenomType::reduce:
        m_value = a.m_den / a.m_num.gcd(a.m_den);
        break;

    case DenomType::sigfigs:
        GncInt128 val {};
        if (a.m_num.abs() > a.m_den)
            val = a.m_num.abs() / a.m_den;
        else
            val = a.m_den / a.m_num.abs();
        uint digits {};
        while (val >= 10)
        {
            ++digits;
            val /= 10;
        }
        m_value = (a.m_num.abs() > a.m_den ? powten (m_sigfigs - digits - 1) :
                   powten (m_sigfigs + digits));
        m_auto = false;
        break;
    }
}
