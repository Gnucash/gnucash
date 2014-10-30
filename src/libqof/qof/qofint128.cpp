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

/* All algorithms from Donald E. Knuth, "The Art of Computer
 * Programming, Volume 2: Seminumerical Algorithms", 3rd Ed.,
 * Addison-Wesley, 1998.
 */

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
QofInt128::operator+= (const QofInt128& b) noexcept
{
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
QofInt128::operator-= (const QofInt128& b) noexcept
{
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
    return *this;
}

QofInt128&
QofInt128::operator/= (const QofInt128& b) noexcept
{
    return *this;
}

QofInt128&
QofInt128::operator%= (const QofInt128& b) noexcept
{
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

