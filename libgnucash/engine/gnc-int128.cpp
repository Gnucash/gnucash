/********************************************************************
 * qofmath128.c -- an 128-bit integer library                       *
 * Copyright (C) 2004 Linas Vepstas <linas@linas.org>               *
 * Copyright (C) 2014 John Ralls <jralls@ceridwen.us>               *
 *                                                                  *
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
extern "C"
{
#include <config.h>
}

#include "gnc-int128.hpp"

#include <iomanip>
#include <utility>
#include <cassert>
#include <cstdio>
#include <sstream>

/* All algorithms from Donald E. Knuth, "The Art of Computer
 * Programming, Volume 2: Seminumerical Algorithms", 3rd Ed.,
 * Addison-Wesley, 1998.
 */

namespace {
    static const unsigned int upper_num_bits = 61;
    static const unsigned int sublegs = GncInt128::numlegs * 2;
    static const unsigned int sublegbits = GncInt128::legbits / 2;
    static const uint64_t sublegmask = (UINT64_C(1) << sublegbits) - 1;
    static const uint64_t flagmask = UINT64_C(0xe000000000000000);
    static const uint64_t nummask = UINT64_C(0x1fffffffffffffff);
/* Assumes that the high bits represent old flags to be replaced.
 * Any overflow must be detected first!
 */
    static inline uint64_t set_flags(uint64_t leg, uint8_t flags)
    {
        auto flag_part = static_cast<uint64_t>(flags) << upper_num_bits;
        return flag_part + (leg & nummask);
    }
    static inline uint8_t get_flags(uint64_t leg)
    {
        return (leg & flagmask) >> upper_num_bits;
    }
    static inline uint64_t get_num(uint64_t leg)
    {
        return leg & nummask;
    }
}

GncInt128::GncInt128 () : m_hi {0}, m_lo {0}{}

GncInt128::GncInt128 (int64_t upper, int64_t lower, uint8_t flags) :
    m_hi {static_cast<uint64_t>(upper < 0 ? -upper : upper)},
    m_lo {static_cast<uint64_t>(lower < 0 ? -lower : lower)}
{
    if ((upper < 0 && lower > 0) || (upper > 0 && lower < 0))
        m_lo = (m_hi << 63) - m_lo;
    else
        m_lo += (m_hi << 63);

    m_hi >>= 1;
    if (m_hi & flagmask)
    {
        std::ostringstream ss;
        ss << "Constructing GncInt128 with int64_t " << upper
           << " which is too big.";
        throw std::overflow_error(ss.str());
    }
    flags ^= (upper < 0 ? neg : upper == 0 && lower < 0 ? neg : pos);
    m_hi = set_flags(m_hi, flags);
}

GncInt128::GncInt128 (int64_t upper, uint64_t lower, uint8_t flags) :
    m_hi {static_cast<uint64_t>(upper < 0 ? -upper : upper)}, m_lo {lower}
{
    if (m_hi & flagmask)
    {
        std::ostringstream ss;
        ss << "Constructing GncInt128 with int64_t " << upper
           << " which is too big when lower is unsigned.";
        throw std::overflow_error(ss.str());
    }
    flags ^= (upper < 0 ? neg : pos);
    m_hi = set_flags(m_hi, flags);
}

GncInt128::GncInt128 (uint64_t upper, uint64_t lower, uint8_t flags) :
    m_hi {upper}, m_lo {lower}
 {
     /* somewhere in the bowels of gnc_module.scm compilation this gets called
      * with upper=INT64_MAX, which would otherwise throw. Make it our max value
      * instead.
      */
    if (m_hi == UINT64_MAX)
         m_hi = nummask;
     if (m_hi & flagmask)
    {
        std::ostringstream ss;
        ss << "Constructing GncInt128 with uint64_t " << upper
           << " which is too big.";
        throw std::overflow_error(ss.str());
    }

     m_hi = set_flags(m_hi, flags);
}

GncInt128&
GncInt128::zero () noexcept
{
    m_lo = m_hi = UINT64_C(0);
    return *this;
}

GncInt128::operator int64_t() const
{
    auto flags = get_flags(m_hi);
    if ((flags & neg) && isBig())
        throw std::underflow_error ("Negative value too large to represent as int64_t");
    if ((flags & (overflow | NaN)) || isBig())
        throw std::overflow_error ("Value too large to represent as int64_t");
    int64_t retval = static_cast<int64_t>(m_lo);
    return flags & neg ? -retval : retval;
}

GncInt128::operator uint64_t() const
{
    auto flags = get_flags(m_hi);
    if ((flags & neg) && !isZero()) // exclude negative zero
        throw std::underflow_error ("Can't represent negative value as uint64_t");
    if ((flags & (overflow | NaN)) || (m_hi || m_lo > UINT64_MAX))
        throw std::overflow_error ("Value to large to represent as uint64_t");
    return m_lo;
}


int
GncInt128::cmp (const GncInt128& b) const noexcept
{
    auto flags = get_flags(m_hi);
    if (flags & (overflow | NaN))
        return -1;
    if (b.isOverflow () || b.isNan ())
        return 1;
    auto hi = get_num(m_hi);
    auto bhi = get_num(b.m_hi);
    if (isZero() && b.isZero()) return 0;
    if (flags & neg)
    {
        if (!b.isNeg()) return -1;
        if (hi > bhi) return -1;
        if (hi < bhi) return 1;
        if (m_lo > b.m_lo) return -1;
        if (m_lo < b.m_lo) return 1;
        return 0;
    }
    if (b.isNeg()) return 1;
    if (hi < bhi) return -1;
    if (hi > bhi) return 1;
    if (m_lo < b.m_lo) return -1;
    if (m_lo > b.m_lo) return 1;
    return 0;
}

/* Knuth 4.5.3 Algo B, recommended by GMP as much faster than Algo A (Euclidean
 * method).
 */
GncInt128
GncInt128::gcd(GncInt128 b) const noexcept
{
    if (b.isZero())
        return *this;
    if (isZero())
        return b;

    if (b.isOverflow() || b.isNan())
        return b;
    if (isOverflow() || isNan())
        return *this;

    GncInt128 a (isNeg() ? -(*this) : *this);
    if (b.isNeg()) b = -b;

    unsigned int k {};
    const uint64_t one {1};
    while (!((a & one) || (b & one))) //B1
    {
        a >>= 1;
        b >>= 1;
        ++k;
    }
    GncInt128 t {a & one ? -b : a}; //B2
    while (a != b)
    {
        while (t && ((t & one) ^ one)) t >>= 1;  //B3 & B4
        if (t.isNeg())  //B5
            b = -t;
        else
            a = t;
        t = a - b;  //B6
    }
    return a << k;
}

/* Since u * v = gcd(u, v) * lcm(u, v), we find lcm by u / gcd * v. */

GncInt128
GncInt128::lcm(const GncInt128& b) const noexcept
{
    auto common = gcd(b);
    return *this / common * b.abs(); //Preserve our sign, discard the other's.
}

/* Knuth section 4.6.3 */
GncInt128
GncInt128::pow(unsigned int b) const noexcept
{
    if (isZero() || (m_lo == 1 && m_hi == 0) || isNan() || isOverflow())
        return *this;
    if (b == 0)
        return GncInt128 (1);
    GncInt128 retval (1), squares = *this;
    while (b && !retval.isOverflow())
    {
        if (b & 1)
            retval *= squares;
        squares *= squares;
        b >>= 1;
    }
    return retval;
}

bool
GncInt128::isNeg () const noexcept
{
    return (get_flags(m_hi) & neg);
}

bool
GncInt128::isBig () const noexcept
{
    return (get_num(m_hi) || m_lo > INT64_MAX);
}

bool
GncInt128::isOverflow () const noexcept
{
    return (get_flags(m_hi) & overflow);
}

bool
GncInt128::isNan () const noexcept
{
    return (get_flags(m_hi) & NaN);
}

bool
GncInt128::valid() const noexcept
{
    return !(get_flags(m_hi) & (overflow | NaN));
}

bool
GncInt128::isZero() const noexcept
{
    return ((get_flags(m_hi) & (overflow | NaN)) == 0 &&
            get_num(m_hi) == 0 && m_lo == 0);
}

GncInt128
GncInt128::abs() const noexcept
{
    if (isNeg())
        return operator-();

    return *this;
}

unsigned int
GncInt128::bits() const noexcept
{
    auto hi = get_num(m_hi);
    unsigned int bits {static_cast<unsigned int>(hi == 0 ? 0 : 64)};
    uint64_t temp {(hi == 0 ? m_lo : hi)};
    for (;temp > 0; temp >>= 1)
        ++bits;
    return bits;
}


GncInt128
GncInt128::operator-() const noexcept
{
    auto retval = *this;
    auto flags = get_flags(retval.m_hi);
    if (isNeg())
        flags ^= neg;
    else
        flags |= neg;
    retval.m_hi = set_flags(retval.m_hi, flags);
    return retval;
}

GncInt128::operator bool() const noexcept
{
    return ! isZero ();
}

GncInt128&
GncInt128::operator++ () noexcept
{
    return operator+=(UINT64_C(1));
}

GncInt128&
GncInt128::operator++ (int) noexcept
{
    return operator+=(UINT64_C(1));
}

GncInt128&
GncInt128::operator-- () noexcept
{
    return operator-=(UINT64_C(1));
}

GncInt128&
GncInt128::operator-- (int) noexcept
{
    return operator-=(UINT64_C(1));
}

GncInt128&
GncInt128::operator+= (const GncInt128& b) noexcept
{
    auto flags = get_flags(m_hi);
    if (b.isOverflow())
        flags |= overflow;
    if (b.isNan())
        flags |= NaN;
    m_hi = set_flags(m_hi, flags);
    if (isOverflow() || isNan())
        return *this;
    if ((isNeg () && !b.isNeg ()) || (!isNeg () && b.isNeg ()))
        return this->operator-= (-b);
    uint64_t result = m_lo + b.m_lo;
    uint64_t carry = static_cast<int64_t>(result < m_lo);  //Wrapping
    m_lo = result;
    auto hi = get_num(m_hi);
    auto bhi = get_num(b.m_hi);
    result = hi + bhi + carry;
    if (result < hi || result & flagmask)
        flags |= overflow;
    m_hi = set_flags(result, flags);
    return *this;
}

GncInt128&
GncInt128::operator<<= (unsigned int i) noexcept
{
    auto flags = get_flags(m_hi);
    if (i == 0)
        return *this;
    if (i > maxbits)
    {
        flags &= 0xfe;
        m_hi = set_flags(0, flags);
        m_lo = 0;
        return *this;
    }
    auto hi = get_num(m_hi);
    if (i < legbits)
    {
        uint64_t carry {(m_lo & (((UINT64_C(1) << i) - 1) << (legbits - i))) >> (legbits - i)};
        m_lo <<= i;
        hi <<= i;
        hi += carry;
        m_hi = set_flags(hi, flags);
        return *this;
    }
    hi = m_lo << (i - legbits);
    m_hi = set_flags(hi, flags);
    m_lo = 0;
    return *this;
}

GncInt128&
GncInt128::operator>>= (unsigned int i) noexcept
{
    auto flags = get_flags(m_hi);
    if (i > maxbits)
    {
        flags &= 0xfe;
        m_hi = set_flags(0, flags);
        m_lo = 0;
        return *this;
    }
    auto hi = get_num(m_hi);
    if (i < legbits)
    {
        uint64_t carry {(hi & ((UINT64_C(1) << i) - 1))};
        m_lo >>= i;
        hi >>= i;
        m_lo += (carry << (legbits - i));
        m_hi = set_flags(hi, flags);
        return *this;
    }
    m_lo = hi >> (i - legbits);
    m_hi = set_flags(0, flags);
    return *this;
}

GncInt128&
GncInt128::operator-= (const GncInt128& b) noexcept
{
    auto flags = get_flags(m_hi);
    if (b.isOverflow())
        flags |= overflow;
    if (b.isNan())
        flags |= NaN;
    m_hi = set_flags(m_hi, flags);

    if (isOverflow() || isNan())
        return *this;

    if ((!isNeg() && b.isNeg()) || (isNeg() && !b.isNeg()))
        return this->operator+= (-b);
    bool operand_bigger {abs().cmp (b.abs()) < 0};
    auto hi = get_num(m_hi);
    auto far_hi = get_num(b.m_hi);
    if (operand_bigger)
    {
        flags ^= neg; // ^= flips the bit
        if (m_lo > b.m_lo)
        {
            /* The + 1 on the end is because we really want to use 2^64, or
             * UINT64_MAX + 1, but that can't be represented in a uint64_t.
             */
            m_lo = UINT64_MAX - m_lo + b.m_lo + 1;
            --far_hi; //borrow
        }
        else
            m_lo = b.m_lo - m_lo;
        hi = far_hi - hi;
        m_hi = set_flags(hi, flags);
        return *this;
    }
    if (m_lo < b.m_lo)
    {
        m_lo = UINT64_MAX - b.m_lo + m_lo + 1; //See UINT64_MAX comment above
        --hi; //borrow
    }
    else
        m_lo -= b.m_lo;

    hi -= far_hi;
    m_hi = set_flags(hi, flags);
    return *this;
}

GncInt128&
GncInt128::operator*= (const GncInt128& b) noexcept
{
    /* Test for 0 first */
    auto flags = get_flags(m_hi);
    /* Handle the sign; ^ flips if b is negative */
    flags ^= (get_flags(b.m_hi) & neg);
    if (isZero() || b.isZero())
    {
        m_lo = 0;
        m_hi = set_flags(0, flags);
        return *this;
    }
    if (b.isOverflow())
        flags |= overflow;
    if (b.isNan())
        flags |= NaN;
    m_hi = set_flags(m_hi, flags);
    if (isOverflow() || isNan())
        return *this;

    /* Test for overflow before spending time on the calculation */
    auto hi = get_num(m_hi);
    auto bhi = get_num(b.m_hi);
    if (hi && bhi)
    {
        flags |= overflow;
        m_hi = set_flags(hi, flags);
        return *this;
    }

    unsigned int abits {bits()}, bbits {b.bits()};
    /* If the product of the high bytes < 7fff then the result will have abits +
     * bbits -1 bits and won't actually overflow. It's not worth the effort to
     * work that out for this corner-case, so we'll take the risk and calculate
     * the product and catch the overflow later.
     */
    if (abits + bbits - 1 > maxbits)
    {
        flags |= overflow;
        m_hi = set_flags(m_hi, flags);
        return *this;
    }

    /* The trivial case */
    if (abits + bbits <= legbits)
    {
        m_lo *= b.m_lo;
        m_hi = set_flags(m_hi, flags);
        return *this;
    }

/* This is Knuth's "classical" multi-precision multiplication algorithm
 * truncated to a GncInt128 result with the loop unrolled for clarity and with
 * overflow and zero checks beforehand to save time. See Donald Knuth, "The Art
 * of Computer Programming Volume 2: Seminumerical Algorithms", Addison-Wesley,
 * 1998, p 268.
 *
 * There are potentially faster algorithms (O(n^1.6) instead of O(n^2) for the
 * full precision), but this is already close to that fast because of truncation
 * and it's not clear that the truncation is applicable to the faster
 * algorithms.
 */

    uint64_t av[sublegs] {(m_lo & sublegmask), (m_lo >> sublegbits),
            (hi & sublegmask), (hi >> sublegbits)};
    uint64_t bv[sublegs] {(b.m_lo & sublegmask), (b.m_lo >> sublegbits),
            (bhi & sublegmask), (bhi >> sublegbits)};
    uint64_t rv[sublegs] {};
    uint64_t carry {}, scratch {};

    rv[0] = av[0] * bv[0];

    rv[1] = av[1] * bv [0];
    scratch = rv[1] + av[0] * bv[1];
    carry = rv[1] > scratch ? 1 : 0;
    rv[1] = scratch;

    rv[2] = av[2] * bv[0] + carry; //0xffff^2 + 1 < 0xffffffff, can't overflow
    scratch = rv[2] + av[1] * bv[1];
    carry = rv[2] > scratch ? 1 : 0;
    rv[2] = scratch + av[0] * bv[2];
    carry += scratch > rv[2] ? 1 : 0;

    rv[3] = av[3] * bv[0] + carry;
    scratch = rv[3] + av[2] * bv[1];
    carry = rv[3] > scratch ? 1 : 0;
    rv[3] = scratch + av[1] * bv[2];
    carry += scratch > rv[3] ? 1 : 0;
    scratch = rv[3] + av[0] * bv[3];
    carry += rv[3] > scratch ? 1 : 0;
    rv[3] = scratch;

    if (carry) //Shouldn't happen because of the checks above
    {
        flags |= overflow;
        m_hi = set_flags(m_hi, flags);
        return *this;
    }

    m_lo = rv[0] + (rv[1] << sublegbits);
    carry = rv[1] >> sublegbits;
    carry += (rv[1] << sublegbits) > m_lo || rv[0] > m_lo ? 1 : 0;
    hi = rv[2] + (rv[3] << sublegbits) + carry;
    if ((rv[3] << sublegbits) > hi || rv[2] > hi || (rv[3] >> sublegbits) ||
        hi & flagmask)
    {
        flags |= overflow;
        m_hi = set_flags(hi, flags);
        return *this;
    }
    m_hi = set_flags(hi, flags);
    return *this;
}

namespace {
/* Algorithm from Knuth (full citation at operator*=) p272ff.  Again, there
 * are faster algorithms out there, but they require much larger numbers to
 * be of benefit.
 */
/* We're using arrays here instead of vectors to avoid an alloc. */
void
div_multi_leg (uint64_t* u, size_t m, uint64_t* v, size_t n,
               GncInt128& q, GncInt128& r) noexcept
{
/* D1, Normalization */
    uint64_t qv[sublegs] {};
    uint64_t d {(UINT64_C(1) << sublegbits)/(v[n - 1] + UINT64_C(1))};
    uint64_t carry {UINT64_C(0)};
    bool negative {q.isNeg()};
    bool rnegative {r.isNeg()};
    for (size_t i = 0; i < m; ++i)
    {
        u[i] = u[i] * d + carry;
        if (u[i] > sublegmask)
        {
            carry = u[i] >> sublegbits;
            u[i] &= sublegmask;
        }
        else
            carry = UINT64_C(0);
        assert (u[i] <= sublegmask);
    }
    if (carry)
    {
        u[m++] = carry;
        carry = UINT64_C(0);
    }
    for (size_t i = 0; i < n; ++i)
    {
        v[i] = v[i] * d + carry;
        if (v[i] > sublegmask)
        {
            carry = v[i] >> sublegbits;
            v[i] &= sublegmask;
        }
        else
            carry = UINT64_C(0);
        assert (v[i] < sublegmask);
    }
    assert (carry == UINT64_C(0));
    for (int j = m - n; j >= 0; j--) //D3
    {
        uint64_t qhat, rhat;
        qhat = ((u[j + n] << sublegbits) + u[j + n - 1]) / v[n - 1];
        rhat = ((u[j + n] << sublegbits) + u[j + n - 1]) % v[n - 1];

        while (qhat > sublegmask ||
               (rhat <= sublegmask &&
                ((qhat * v[n - 2]) > ((rhat << sublegbits) + u[j + n - 2]))))
        {
            --qhat;
            rhat += v[n - 1];
        }
        carry = UINT64_C(0);
        uint64_t borrow {};
        for (size_t k = 0; k < n; ++k) //D4
        {
            auto subend = qhat * v[k] + carry;
            carry = subend >> sublegbits;
            subend &= sublegmask;
            if (u[j + k] >= subend)
            {
                u[j + k] = u[j + k] - subend;
                borrow = UINT64_C(0);
            }
            else
            {
                if (u[j + k + 1] > 0)
                    --u[j + k + 1];
                else
                    ++borrow;
                u[j + k] = u[j + k] + sublegmask + 1 - subend;
                u[j + k] &= sublegmask;
            }
        }
        u[j + n] -= carry;
        qv[j] = qhat;
        if (borrow) //D5
        { //D6
            --qv[j];
            carry = UINT64_C(0);
            for (size_t k = 0; k < n; ++k)
            {
                u[j + k] += v[k] + carry;
                if (u[j + k] > sublegmask)
                {
                    carry = u[j + k] >> sublegbits;
                    u[j + k] &= sublegmask;
                }
            }
            u[j + n] += carry;
        }
    }//D7
    q = GncInt128 ((qv[3] << sublegbits) + qv[2], (qv[1] << sublegbits) + qv[0]);
    r = GncInt128 ((u[3] << sublegbits) + u[2], (u[1] << sublegbits) + u[0]);
    r /= d;
    if (negative) q = -q;
    if (rnegative) r = -r;
}

void
div_single_leg (uint64_t* u, size_t m, uint64_t v,
                GncInt128& q, GncInt128& r) noexcept
{
    uint64_t qv[sublegs] {};
    uint64_t carry {};
    bool negative {q.isNeg()};
    bool rnegative {r.isNeg()};
    for (int i = m - 1; i >= 0; --i)
    {
        qv[i] = u[i] / v;
        if (i > 0)
        {
            u[i - 1] += ((u[i] % v) << sublegbits);
            u[i] = UINT64_C(0);
        }
        else
            u[i] %= v;
    }

    q = GncInt128 ((qv[3] << sublegbits) + qv[2], (qv[1] << sublegbits) + qv[0]);
    r = GncInt128 ((u[3] << sublegbits) + u[2], (u[1] << sublegbits) + u[0]);
    if (negative) q = -q;
    if (rnegative) r = -r;
}

}// namespace

void
GncInt128::div (const GncInt128& b, GncInt128& q, GncInt128& r) const noexcept
{
    // clear remainder and quotient
    r = GncInt128(0);
    q = GncInt128(0);

    auto qflags = get_flags(q.m_hi);
    auto rflags = get_flags(r.m_hi);
    if (isOverflow() || b.isOverflow())
    {
        qflags |= overflow;
        rflags |= overflow;
        q.m_hi = set_flags(q.m_hi, qflags);
        r.m_hi = set_flags(r.m_hi, rflags);
        return;
    }

    if (isNan() || b.isNan())
    {
        qflags |= NaN;
        rflags |= NaN;
        q.m_hi = set_flags(q.m_hi, qflags);
        r.m_hi = set_flags(r.m_hi, rflags);
        return;
    }
    assert (&q != this);
    assert (&r != this);
    assert (&q != &b);
    assert (&r != &b);

    q.zero(), r.zero();
    qflags = rflags = 0;
    if (b.isZero())
    {
        qflags |= NaN;
        rflags |= NaN;
        q.m_hi = set_flags(q.m_hi, qflags);
        r.m_hi = set_flags(r.m_hi, rflags);
        return;
    }

    if (isNeg())
    {
        qflags |= neg;
        rflags |= neg;          // the remainder inherits the dividend's sign
    }

    if (b.isNeg())
        qflags ^= neg;

    q.m_hi = set_flags(q.m_hi, qflags);
    r.m_hi = set_flags(r.m_hi, rflags);

    if (abs() < b.abs())
    {
        r = *this;
        return;
    }
    auto hi = get_num(m_hi);
    auto bhi = get_num(b.m_hi);

    if (hi == 0 && bhi == 0) //let the hardware do it
    {
        assert (b.m_lo != 0); // b.m_hi is 0 but b isn't or we didn't get here.
        q.m_lo = m_lo / b.m_lo;
        r.m_lo = m_lo % b.m_lo;
        return;
    }

    uint64_t u[sublegs + 2] {(m_lo & sublegmask), (m_lo >> sublegbits),
            (hi & sublegmask), (hi >> sublegbits), 0, 0};
    uint64_t v[sublegs] {(b.m_lo & sublegmask), (b.m_lo >> sublegbits),
            (bhi & sublegmask), (bhi >> sublegbits)};
    auto m = u[3] ? 4 : u[2] ? 3 : u[1] ? 2 : u[0] ? 1 : 0;
    auto n = v[3] ? 4 : v[2] ? 3 : v[1] ? 2 : v[0] ? 1 : 0;
    if (m == 0 || n == 0) //Shouldn't happen
        return;
    if (n == 1)
        return div_single_leg (u, m, v[0], q, r);

    return div_multi_leg (u, m, v, n, q, r);
}

GncInt128&
GncInt128::operator/= (const GncInt128& b) noexcept
{
    GncInt128 q {}, r {};
    div(b, q, r);
    std::swap (*this, q);
    return *this;
}

GncInt128&
GncInt128::operator%= (const GncInt128& b) noexcept
{
    GncInt128 q {}, r {};
    div(b, q, r);
    std::swap (*this, r);
    if (q.isNan())
        m_hi = set_flags(m_hi, (get_flags(m_hi) | NaN));
    return *this;
}

GncInt128&
GncInt128::operator&= (const GncInt128& b) noexcept
{
    auto flags = get_flags(m_hi);
    if (b.isOverflow())
        flags |= overflow;
    if (b.isNan())
        flags |= NaN;
    m_hi = set_flags(m_hi, flags);
    if (isOverflow() || isNan())
        return *this;
    auto hi = get_num(m_hi);
    hi &= get_num(b.m_hi);
    m_lo &= b.m_lo;
    m_hi = set_flags(hi, flags);
    return *this;
}

GncInt128&
GncInt128::operator|= (const GncInt128& b) noexcept
{
    auto flags = get_flags(m_hi);
    auto hi = get_num(m_hi);
    hi  ^= get_num(b.m_hi);
    m_hi = set_flags(hi, flags);
    m_lo ^= b.m_lo;
    return *this;
}

GncInt128&
GncInt128::operator^= (const GncInt128& b) noexcept
{
    auto flags = get_flags(m_hi);
    if (b.isOverflow())
        flags |= overflow;
    if (b.isNan())
        flags |= NaN;
    m_hi = set_flags(m_hi, flags);
    if (isOverflow() || isNan())
        return *this;
    auto hi = get_num(m_hi);
    hi ^= get_num(b.m_hi);
    m_hi = set_flags(hi, flags);
    m_lo ^= b.m_lo;
    return *this;
}

static const uint8_t dec_array_size {5};
/* Convert a uint128 represented as a binary number into 4 10-digit decimal
 * equivalents. Adapted from Douglas W. Jones, "Binary to Decimal Conversion in
 * Limited Precision", http://homepage.cs.uiowa.edu/~jones/bcd/decimal.html,
 * accessed 28 Oct 2014. */
static void
decimal_from_binary (uint64_t d[dec_array_size], uint64_t hi, uint64_t lo)
{
    /* Coefficients are the values of 2^96, 2^64, and 2^32 divided into 8-digit
     * segments:
     * 2^96 =               79228,16251426,43375935,43950336
     * 2^64 =                         1844,67440737,09551616
     * 2^32 =                                    42,94967296
     */
    const uint8_t coeff_array_size = dec_array_size - 1;
    const uint32_t coeff_3 [coeff_array_size] {79228, 16251426, 43375935, 43950336};
    const uint32_t coeff_2 [coeff_array_size] {0, 1844, 67440737, 9551616};
    const uint32_t coeff_1 [coeff_array_size] {0, 0, 42, 94967296};
    const uint32_t bin_mask {0xffffffff};
    const uint32_t dec_div {UINT32_C(100000000)};
    const uint8_t last {dec_array_size - 1};

    d[0] = lo & bin_mask;
    d[1] = (lo >> 32) & bin_mask;
    d[2] = hi & bin_mask;
    d[3] = (hi >> 32) & bin_mask, 0;

    d[0] += coeff_3[3] * d[3] + coeff_2[3] * d[2] + coeff_1[3] * d[1];
    uint64_t q {d[0] / dec_div};
    d[0] %= dec_div;

    for (int i {1}; i < coeff_array_size; ++i)
    {
        int j = coeff_array_size - i - 1;
        d[i] = q + coeff_3[j] * d[3] + coeff_2[j] * d[2] + coeff_1[j] * d[1];
        q = d[i] / dec_div;
        d[i] %= dec_div;
    }
    d[last] = q;
    return;
}

static const uint8_t char_buf_size {41}; //39 digits plus sign and trailing null

char*
GncInt128::asCharBufR(char* buf) const noexcept
{
    if (isOverflow())
    {
        sprintf (buf, "%s", "Overflow");
        return buf;
    }
    if (isNan())
    {
        sprintf (buf, "%s", "NaN");
        return buf;
    }
    if (isZero())
    {
        sprintf (buf, "%d", 0);
        return buf;
    }
    uint64_t d[dec_array_size] {};
    decimal_from_binary(d, get_num(m_hi), m_lo);
    char* next = buf;
    char neg {'-'};

    if (isNeg()) *(next++) = neg;
    bool trailing {false};
    for (unsigned int i {dec_array_size}; i; --i)
        if (d[i - 1] || trailing)
        {
            if (trailing)
                next += sprintf (next, "%8.8" PRIu64, d[i - 1]);
            else
                next += sprintf (next, "%" PRIu64, d[i - 1]);

            trailing = true;
        }

    return buf;
}

std::ostream&
operator<< (std::ostream& stream, const GncInt128& a) noexcept
{
    char buf[char_buf_size] {};
    stream << a.asCharBufR (buf);
    return stream;
}

bool
operator== (const GncInt128& a, const GncInt128& b) noexcept
{
    return a.cmp(b) == 0;
}

bool
operator!= (const GncInt128& a, const GncInt128& b) noexcept
{
    return a.cmp(b) != 0;
}

bool
operator< (const GncInt128& a, const GncInt128& b) noexcept
{
    return a.cmp(b) < 0;
}

bool
operator> (const GncInt128& a, const GncInt128& b) noexcept
{
    return a.cmp(b) > 0;
}

bool
operator<= (const GncInt128& a, const GncInt128& b) noexcept
{
    return a.cmp(b) <= 0;
}

bool
operator>= (const GncInt128& a, const GncInt128& b) noexcept
{
    return a.cmp(b) >= 0;
}

GncInt128
operator+ (GncInt128 a, const GncInt128& b) noexcept
{
    a += b;
    return a;
}

GncInt128
operator- (GncInt128 a, const GncInt128& b) noexcept
{
    a -= b;
    return a;
}
GncInt128
operator* (GncInt128 a, const GncInt128& b) noexcept
{
    a *= b;
    return a;
}

GncInt128
operator/ (GncInt128 a, const GncInt128& b) noexcept
{
    a /= b;
    return a;
}

GncInt128
operator% (GncInt128 a, const GncInt128& b) noexcept
{
    a %= b;
    return a;
}

GncInt128
operator& (GncInt128 a, const GncInt128& b) noexcept
{
    a &= b;
    return a;
}

GncInt128
operator| (GncInt128 a, const GncInt128& b) noexcept
{
    a |= b;
    return a;
}

GncInt128
operator^ (GncInt128 a, const GncInt128& b) noexcept
{
    a ^= b;
    return a;
}

GncInt128
operator<< (GncInt128 a, unsigned int b) noexcept
{
    a <<= b;
    return a;
}

GncInt128
operator>> (GncInt128 a, unsigned int b) noexcept
{
    a >>= b;
    return a;
}
