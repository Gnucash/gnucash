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
#include "config.h"
#include <inttypes.h>
}

#include "qofint128.hpp"

#include <iomanip>
#include <utility>
#include <cassert>

/* All algorithms from Donald E. Knuth, "The Art of Computer
 * Programming, Volume 2: Seminumerical Algorithms", 3rd Ed.,
 * Addison-Wesley, 1998.
 */

namespace {
    static const uint sublegs = QofInt128::numlegs * 2;
    static const uint sublegbits = QofInt128::legbits / 2;
    static const uint64_t sublegmask = (UINT64_C(1) << sublegbits) - 1;
}

QofInt128::QofInt128 () : m_flags {}, m_hi {0}, m_lo {0}{}

QofInt128::QofInt128 (int64_t lower) :
    m_flags {static_cast<unsigned char>(lower < 0 ? neg : pos)},
    m_hi {0},
    m_lo {static_cast<uint64_t>(lower < 0 ? -lower : lower)} {}

QofInt128::QofInt128 (uint64_t lower) :
    m_flags {}, m_hi {0}, m_lo {lower} {}

QofInt128::QofInt128 (int64_t upper, int64_t lower, unsigned char flags) :
    m_flags {static_cast<unsigned char>(flags ^ (upper < 0 ? neg :
    upper == 0 && lower < 0 ? neg : pos))},
    m_hi {static_cast<uint64_t>(upper < 0 ? -upper : upper)},
    m_lo {static_cast<uint64_t>(lower < 0 ? -lower : lower)}
{
    if ((upper < 0 && lower > 0) || (upper > 0 && lower < 0))
	m_lo = (m_hi << 63) - m_lo;
    else
        m_lo += (m_hi << 63);

    m_hi >>= 1;
}

    QofInt128::QofInt128 (uint64_t upper, uint64_t lower, unsigned char flags) :
    m_flags {flags}, m_hi {upper},
    m_lo {lower} {}

QofInt128&
QofInt128::zero () noexcept
{
    m_flags = 0;
    m_lo = m_hi = UINT64_C(0);
    return *this;
}

QofInt128::operator int64_t() const
{
    if ((m_flags & neg) && isBig())
        throw std::underflow_error ("Negative value to large to represent as int64_t");
    if ((m_flags & (overflow | NaN)) || isBig())
        throw std::overflow_error ("Value to large to represent as int64_t");
    int64_t retval = static_cast<int64_t>(m_lo);
    return m_flags & neg ? -retval : retval;
}

QofInt128::operator uint64_t() const
{
    if (m_flags & neg)
        throw std::underflow_error ("Can't represent negative value as uint64_t");
    if ((m_flags & (overflow | NaN)) || (m_hi || m_lo > UINT64_MAX))
        throw std::overflow_error ("Value to large to represent as uint64_t");
    return m_lo;
}


int
QofInt128::cmp (const QofInt128& b) const noexcept
{
    if (m_flags & (overflow | NaN))
        return -9;
    if (b.isOverflow () || b.isNan ())
        return 9;
    if (m_flags & neg)
    {
	if (!b.isNeg()) return -1;
	if (m_hi > b.m_hi) return -3;
	if (m_hi < b.m_hi) return 3;
	if (m_lo > b.m_lo) return -2;
	if (m_lo < b.m_lo) return 2;
	return 0;
    }
    if (b.isNeg()) return 1;
    if (m_hi < b.m_hi) return -5;
    if (m_hi > b.m_hi) return 5;
    if (m_lo < b.m_lo) return -4;
    if (m_lo > b.m_lo) return 4;
    return 0;
}

bool
QofInt128::isNeg () const noexcept
{
    return (m_flags & neg);
}

bool
QofInt128::isBig () const noexcept
{
    return (m_hi || m_lo > INT64_MAX);
}

bool
QofInt128::isOverflow () const noexcept
{
    return (m_flags & overflow);
}

bool
QofInt128::isNan () const noexcept
{
    return (m_flags & NaN);
}

bool
QofInt128::isZero() const noexcept
{
    return ((m_flags & (overflow | NaN)) == 0 &&  m_hi == 0 && m_lo == 0);
}

QofInt128
QofInt128::abs() const noexcept
{
    if (isNeg())
        return operator-();

    return *this;
}

uint
QofInt128::bits() const noexcept
{
    uint bits {static_cast<uint>(m_hi == 0 ? 0 : 64)};
    uint64_t temp {(m_hi == 0 ? m_lo : m_hi)};
    for (;temp > 0; temp >>= 1)
        ++bits;
    return bits;
}


QofInt128
QofInt128::operator-() const noexcept
{
    auto retval = *this;
    if (isNeg())
	retval.m_flags ^= neg;
    else
	retval.m_flags |= neg;
    return retval;
}

QofInt128::operator bool() const noexcept
{
    return ! isZero ();
}

QofInt128&
QofInt128::operator++ () noexcept
{
    return operator+=(UINT64_C(1));
}

QofInt128&
QofInt128::operator++ (int) noexcept
{
    return operator+=(UINT64_C(1));
}

QofInt128&
QofInt128::operator-- () noexcept
{
    return operator-=(UINT64_C(1));
}

QofInt128&
QofInt128::operator-- (int) noexcept
{
    return operator-=(UINT64_C(1));
}

QofInt128&
QofInt128::operator+= (const QofInt128& b) noexcept
{
    if (b.isOverflow())
        m_flags |= overflow;
    if (b.isNan())
        m_flags |= NaN;

    if (isOverflow() || isNan())
        return *this;
    if ((isNeg () && !b.isNeg ()) || (!isNeg () && b.isNeg ()))
	return this->operator-= (-b);
    uint64_t result = m_lo + b.m_lo;
    uint64_t carry = static_cast<int64_t>(result < m_lo);  //Wrapping
    m_lo = result;
    result = m_hi + b.m_hi + carry;
    if (result < m_hi)
	m_flags |= overflow;
    m_hi = result;
    return *this;
}

QofInt128&
QofInt128::operator<<= (uint i) noexcept
{
    if (i > maxbits)
    {
        m_flags &= 0xfe;
        m_hi = 0;
        m_lo = 0;
        return *this;
    }
    uint64_t carry {(m_lo & (((UINT64_C(1) << i) - 1) << (legbits - i)))};
    m_lo <<= i;
    m_hi <<= i;
    m_hi += carry;
    return *this;
}

QofInt128&
QofInt128::operator>>= (uint i) noexcept
{
    if (i > maxbits)
    {
        m_flags &= 0xfe;
        m_hi = 0;
        m_lo = 0;
        return *this;
    }
    uint64_t carry {(m_hi & ((UINT64_C(1) << i) - 1))};
    m_lo >>= i;
    m_hi >>= i;
    m_lo += (carry << (legbits - i));
    return *this;
}

QofInt128&
QofInt128::operator-= (const QofInt128& b) noexcept
{
    if (b.isOverflow())
        m_flags |= overflow;
    if (b.isNan())
        m_flags |= NaN;

    if (isOverflow() || isNan())
        return *this;

    if ((!isNeg() && b.isNeg()) || (isNeg() && !b.isNeg()))
	return this->operator+= (-b);
    bool operand_bigger {cmp (b) < 0};
    if (operand_bigger)
    {
        m_flags ^= neg; // ^= flips the bit
        uint64_t far_hi {b.m_hi};
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

        m_hi = far_hi - m_hi;
        return *this;
    }
    if (m_lo < b.m_lo)
    {
        m_lo = UINT64_MAX - b.m_lo + m_lo + 1; //See UINT64_MAX comment above
        --m_hi; //borrow
    }
    else
        m_lo -= b.m_lo;

    m_hi -= b.m_hi;

    return *this;
}

QofInt128&
QofInt128::operator*= (const QofInt128& b) noexcept
{
    /* Test for 0 first */
    if (isZero() || b.isZero())
    {
        m_hi = m_lo = 0;
        return *this;
    }
    if (b.isOverflow())
        m_flags |= overflow;
    if (b.isNan())
        m_flags |= NaN;

    if (isOverflow() || isNan())
        return *this;

    /* Test for overflow before spending time on the calculation */
    if (m_hi && b.m_hi)
    {
        m_flags |= overflow;
        return *this;
    }

    uint abits {bits()}, bbits {b.bits()};
    if (abits + bbits > maxbits)
    {
        m_flags |= overflow;
        return *this;
    }
    /* Handle the sign; ^ flips if b is negative */
    m_flags ^= (b.m_flags & neg);
    /* The trivial case */
    if (abits + bbits <= legbits)
    {
        m_lo *= b.m_lo;
        return *this;
    }

/* This is Knuth's "classical" multi-precision multiplication algorithm
 * truncated to a QofInt128 result with the loop unrolled for clarity and with
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
            (m_hi & sublegmask), (m_hi >> sublegbits)};
    uint64_t bv[sublegs] {(b.m_lo & sublegmask), (b.m_lo >> sublegbits),
            (b.m_hi & sublegmask), (b.m_hi >> sublegbits)};
    uint64_t rv[sublegs] {};
    int64_t carry {}, scratch {};

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
        m_flags |= overflow;
        return *this;
    }

    m_lo = rv[0] + (rv[1] << sublegbits);
    carry = rv[1] >> sublegbits;
    carry += (rv[1] << sublegbits) > m_lo || rv[0] > m_lo ? 1 : 0;
    m_hi = rv[2] + (rv[3] << sublegbits) + carry;
    if ((rv[3] << sublegbits) > m_hi || rv[2] > m_hi || (rv[3] >> sublegbits))
    {
        m_flags |= overflow;
        return *this;
    }
    return *this;
}

namespace {
/* Algorithm from Knuth (full citation at operator*=) p272ff.  Again, there
 * are faster algorithms out there, but they require much larger numbers to
 * be of benefit.
 */
/* We're using arrays here instead of vectors to avoid an alloc. */
void
div_multi_leg (uint64_t* u, size_t m, uint64_t* v, size_t n, QofInt128& q, QofInt128& r) noexcept
{
/* D1, Normalization */
    uint64_t qv[sublegs] {};
    uint64_t d {(UINT64_C(1) << sublegbits)/(v[n - 1] + UINT64_C(1))};
    uint64_t carry {UINT64_C(0)};
    for (auto i = 0; i < m; ++i)
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
    for (auto i = 0; i < n; ++i)
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
        int64_t qhat, rhat;
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
        for (auto k = 0; k < n; ++k) //D4
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
            for (auto k = 0; k < n; ++k)
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
    q = QofInt128 ((qv[3] << sublegbits) + qv[2], (qv[1] << sublegbits) + qv[0]);
    r = QofInt128 ((u[3] << sublegbits) + u[2], (u[1] << sublegbits) + u[0]);
    r /= d;
}

void
div_single_leg (uint64_t* u, size_t m, uint64_t v, QofInt128& q, QofInt128& r) noexcept
{
    uint64_t qv[sublegs] {};
    uint64_t carry {};
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

    q = QofInt128 ((qv[3] << sublegbits) + qv[2], (qv[1] << sublegbits) + qv[0]);
    r = QofInt128 ((u[3] << sublegbits) + u[2], (u[1] << sublegbits) + u[0]);
}

}// namespace

void
QofInt128::div (const QofInt128& b, QofInt128& q, QofInt128& r) noexcept
{
    if (isOverflow() || b.isOverflow())
    {
        q.m_flags |= overflow;
        r.m_flags |= overflow;
        return;
    }

    if (isNan() || b.isNan())
    {
        q.m_flags |= NaN;
        r.m_flags |= NaN;
        return;
    }

    q.zero(), r.zero();
    if (b.isZero())
    {
        q.m_flags |= NaN;
        r.m_flags |= NaN;
        return;
    }
    if (b.isNeg())
        q.m_flags ^= neg;

    if (abs() < b.abs())
    {
        r = *this;
        return;
    }
    if (m_hi == 0 && b.m_hi == 0) //let the hardware do it
    {
        q.m_lo = m_lo / b.m_lo;
        r.m_lo = m_lo % b.m_lo;
        return;
    }

    uint64_t u[sublegs + 2] {(m_lo & sublegmask), (m_lo >> sublegbits),
            (m_hi & sublegmask), (m_hi >> sublegbits), 0, 0};
    uint64_t v[sublegs] {(b.m_lo & sublegmask), (b.m_lo >> sublegbits),
            (b.m_hi & sublegmask), (b.m_hi >> sublegbits)};
    auto m = u[3] ? 4 : u[2] ? 3 : u[1] ? 2 : u[0] ? 1 : 0;
    auto n = v[3] ? 4 : v[2] ? 3 : v[1] ? 2 : v[0] ? 1 : 0;
    if (m == 0 || n == 0) //Shouldn't happen
        return;
    if (n == 1)
        return div_single_leg (u, m, v[0], q, r);

    return div_multi_leg (u, m, v, n, q, r);
}

QofInt128&
QofInt128::operator/= (const QofInt128& b) noexcept
{
    QofInt128 q {}, r {};
    div(b, q, r);
    std::swap (*this, q);
    return *this;
}

QofInt128&
QofInt128::operator%= (const QofInt128& b) noexcept
{
    QofInt128 q {}, r {};
    div(b, q, r);
    std::swap (*this, r);
    if (q.isNan())
        m_flags |= NaN;
    return *this;
}

QofInt128&
QofInt128::operator&= (const QofInt128& b) noexcept
{
    if (b.isOverflow())
        m_flags |= overflow;
    if (b.isNan())
        m_flags |= NaN;

    if (isOverflow() || isNan())
        return *this;

    m_hi &= b.m_hi;
    m_lo &= b.m_lo;
    return *this;
}

QofInt128&
QofInt128::operator|= (const QofInt128& b) noexcept
{
    m_hi ^= b.m_hi;
    m_lo ^= b.m_lo;
    return *this;
}

QofInt128&
QofInt128::operator^= (const QofInt128& b) noexcept
{
    if (b.isOverflow())
        m_flags |= overflow;
    if (b.isNan())
        m_flags |= NaN;

    if (isOverflow() || isNan())
        return *this;

    m_hi ^= b.m_hi;
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
QofInt128::asCharBufR(char* buf) const noexcept
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
    uint64_t d[dec_array_size] {};
    decimal_from_binary(d, m_hi, m_lo);
    char* next = buf;
    char neg {'-'};

    if (isNeg()) *(next++) = neg;
    bool trailing {false};
    for (uint i {dec_array_size}; i; --i)
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
operator<< (std::ostream& stream, const QofInt128& a) noexcept
{
    char buf[char_buf_size] {};
    stream << a.asCharBufR (buf);
    return stream;
}

bool
operator== (const QofInt128& a, const QofInt128& b) noexcept
{
    return a.cmp(b) == 0;
}

bool
operator!= (const QofInt128& a, const QofInt128& b) noexcept
{
    return a.cmp(b) != 0;
}

bool
operator< (const QofInt128& a, const QofInt128& b) noexcept
{
    return a.cmp(b) < 0;
}

bool
operator> (const QofInt128& a, const QofInt128& b) noexcept
{
    return a.cmp(b) > 0;
}

bool
operator<= (const QofInt128& a, const QofInt128& b) noexcept
{
    return a.cmp(b) <= 0;
}

bool
operator>= (const QofInt128& a, const QofInt128& b) noexcept
{
    return a.cmp(b) >= 0;
}

QofInt128
operator+ (QofInt128 a, const QofInt128& b) noexcept
{
    a += b;
    return a;
}

QofInt128
operator- (QofInt128 a, const QofInt128& b) noexcept
{
    a -= b;
    return a;
}
QofInt128
operator* (QofInt128 a, const QofInt128& b) noexcept
{
    a *= b;
    return a;
}

QofInt128
operator/ (QofInt128 a, const QofInt128& b) noexcept
{
    a /= b;
    return a;
}

QofInt128
operator% (QofInt128 a, const QofInt128& b) noexcept
{
    a %= b;
    return a;
}

QofInt128
operator& (QofInt128 a, const QofInt128& b) noexcept
{
    a &= b;
    return a;
}

QofInt128
operator| (QofInt128 a, const QofInt128& b) noexcept
{
    a |= b;
    return a;
}

QofInt128
operator^ (QofInt128 a, const QofInt128& b) noexcept
{
    a ^= b;
    return a;
}

QofInt128
operator<< (QofInt128 a, uint b) noexcept
{
    a <<= b;
    return a;
}

QofInt128
operator>> (QofInt128 a, uint b) noexcept
{
    a >>= b;
    return a;
}

