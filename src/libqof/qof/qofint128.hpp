/********************************************************************
 * qofmath128.h -- an 128-bit integer library                       *
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

#ifndef QOFINT128_H
#define QOFINT128_H

#include <stdint.h>

#include <stdexcept>
#include <string>
#include <ostream>

//using std::string;

/**
 * @brief provides a 128-bit int as a base class for GncNumeric.
 *
 * All the usual operators are provided. Only the explicit integer
 * conversions throw; all other errors are indicated by the overflow
 * and NaN ("Not a Number") flags. Note that performing any operation
 * on an overflowed or NaN QofInt128 will yield an overflowed or NaN
 * result, so calling routines need not check until the end of a
 * chained calculation.
 * QofInt128 uses implicit copy and move constructors and implicit destructor.
 */
class QofInt128
{
    unsigned char m_flags;
    uint64_t m_hi;
    uint64_t m_lo;

public:
static const uint numlegs = 2;
static const uint legbits = 64;
static const uint maxbits = legbits * numlegs;

enum // Values for m_flags
{
    pos = 0,
    neg = 1,
    overflow = 2,
    NaN = 4
};

    QofInt128 ();
    QofInt128 (int16_t lower) : QofInt128{static_cast<int64_t>(lower)} {};
    QofInt128 (uint16_t lower) : QofInt128{static_cast<uint64_t>(lower)} {};
    QofInt128 (int32_t lower) : QofInt128{static_cast<int64_t>(lower)} {};
    QofInt128 (uint32_t lower) : QofInt128{static_cast<uint64_t>(lower)} {};
    QofInt128 (int64_t lower);
    QofInt128 (uint64_t lower);
/**
 * Construct a QofInt128 from two int64_t.
 *
 * N.B.: If the two parameters are of differing sign, it's taken to
 * mean that the lower magnitude is *reducing* the magnitude of the
 * upper, so the lower magnitude will be subracted from UINT64_MAX to
 * obtain the lower limb value.
 */
    QofInt128 (int16_t upper, int16_t lower, unsigned char flags = '\0') :
        QofInt128{static_cast<int64_t>(upper), static_cast<int64_t>(lower),
                      flags} {};
    QofInt128 (uint16_t upper, uint16_t lower, unsigned char flags = '\0') :
        QofInt128{static_cast<int64_t>(upper), static_cast<int64_t>(lower),
                      flags} {};
    QofInt128 (int32_t upper, int32_t lower, unsigned char flags = '\0') :
        QofInt128{static_cast<int64_t>(upper), static_cast<int64_t>(lower),
                      flags} {};
    QofInt128 (uint32_t upper, uint32_t lower, unsigned char flags = '\0') :
        QofInt128{static_cast<int64_t>(upper), static_cast<int64_t>(lower),
                      flags} {};
    QofInt128 (int64_t upper, int64_t lower, unsigned char flags = '\0');
    QofInt128 (uint64_t upper, uint64_t lower, unsigned char flags = '\0');

/**
 * Clear the object.
 *
 * Sets all member variables to zero.
 * @return A reference to the object for chaining.
 */
    QofInt128& zero() noexcept;

/**
 * Compare function.
 *
 * @return -1 if the object is less than the parameter, 0 if they're
 * equal, and 1 if the object is greater.
 */
    int cmp (const QofInt128& b) const noexcept;

/**
 * Computes the Greatest Common Divisor between the object and paramter
 *
 * @return A QofInt128 having the GCD.
 */
    QofInt128 gcd (const QofInt128& b) const noexcept;
/**
 * Computes the Least Common Multiple between the object and parameter
 *
 * @return A QofInt128 having the LCM.
 */
    QofInt128 lcm (const QofInt128& b) const noexcept;

/**
 * Computes the object raised to the parameter's power
 *
 * @return A QofInt128; it will be NaN if the parameter is negative.
 */
    QofInt128 pow (const QofInt128& b) const noexcept;

/**
 * Computes a quotient and a remainder, passed as reference parameters.
 *
 * 'this' is the dividend. The quotient and remainder args must be initialized
 * to zero.
 * @param d The divisor
 * @param q The quotient; will be NaN if divisor = 0
 * @param r The remainder; will be 0 if divisor = 0
 */
    void div (const QofInt128& d, QofInt128& q, QofInt128& r) noexcept;

/**
 * Explicit conversion to int64_t.
 *
 * @return A int64_t
 * @throws std::overflow_error if the object's value is > INT64_MAX or NaN.
 * @throws std::underflow_error if the object's value is < INT64_MIN
 */
    explicit operator int64_t() const;
/**
 * Explicit conversion to uint64_t.
 *
 * @return A uint64_t
 * @throws std::overflow_error if the object's value is > UINT64_MAX or NaN.
 * @throws std::underflow_error if the object's value is < 0.
 */
    explicit operator uint64_t() const;

/**
 * @return true if the object value is < 0
 */
    bool isNeg () const noexcept;
/**
 * @return true if the object value is > INT64_MAX or < INT64_MIN
 */
    bool isBig () const noexcept;
/**
 * @return true if a calculation has produced a result of larger
 * magnitude than can be contained in the 128 bits available.
 */
    bool isOverflow () const noexcept;
/**
 * @return true if an illegal calculation has occured.
 */
    bool isNan () const noexcept;
/**
 * @return true if the object represents 0.
 */
    bool isZero() const noexcept;

/**
 * Fills a supplied buffer with a representation of the number in base 10. If
 * the QofInt128 is overflowed or NaN it will contain the words "Overflow" or
 * "NaN" respectively.
 *
 * @param buf char[41], 39 digits plus sign and trailing 0.
 * @return pointer to the buffer for convenience
 */
    char* asCharBufR(char* buf) const noexcept;

    QofInt128 abs() const noexcept;

    QofInt128 operator-() const noexcept;
    explicit operator bool() const noexcept;

    QofInt128& operator++ () noexcept;
    QofInt128& operator++ (int) noexcept;
    QofInt128& operator-- () noexcept;
    QofInt128& operator-- (int) noexcept;
    QofInt128& operator<<= (uint i) noexcept;
    QofInt128& operator>>= (uint i) noexcept;
    QofInt128& operator+= (const QofInt128& b) noexcept;
    QofInt128& operator-= (const QofInt128& b) noexcept;
    QofInt128& operator*= (const QofInt128& b) noexcept;
    QofInt128& operator/= (const QofInt128& b) noexcept;
    QofInt128& operator%= (const QofInt128& b) noexcept;
    QofInt128& operator&= (const QofInt128& b) noexcept;
    QofInt128& operator|= (const QofInt128& b) noexcept;
    QofInt128& operator^= (const QofInt128& b) noexcept;

};

static const QofInt128 k_qofInt128_Max {UINT64_MAX, UINT64_MAX, QofInt128::pos};
static const QofInt128 k_qofInt128_Min {UINT64_MAX, UINT64_MAX, QofInt128::neg};

QofInt128 operator+ (QofInt128 a, const QofInt128& b) noexcept;
QofInt128 operator- (QofInt128 a, const QofInt128& b) noexcept;
QofInt128 operator* (QofInt128 a, const QofInt128& b) noexcept;
QofInt128 operator/ (QofInt128 a, const QofInt128& b) noexcept;
QofInt128 operator% (QofInt128 a, const QofInt128& b) noexcept;
QofInt128 operator& (QofInt128 a, const QofInt128& b) noexcept;
QofInt128 operator| (QofInt128 a, const QofInt128& b) noexcept;
QofInt128 operator^ (QofInt128 a, const QofInt128& b) noexcept;
QofInt128 operator<< (QofInt128 a, uint b) noexcept;
QofInt128 operator>> (QofInt128 a, uint b) noexcept;

bool operator== (const QofInt128& a, const QofInt128& b) noexcept;
bool operator!= (const QofInt128& a, const QofInt128& b) noexcept;
bool operator<= (const QofInt128& a, const QofInt128& b) noexcept;
bool operator>= (const QofInt128& a, const QofInt128& b) noexcept;
bool operator< (const QofInt128& a, const QofInt128& b) noexcept;
bool operator> (const QofInt128& a, const QofInt128& b) noexcept;

std::ostream& operator<< (std::ostream&, const QofInt128&) noexcept;

/** Compute the greatest common denominator of two integers
 */
QofInt128 gcd (int64_t a, int64_t b);

/** Compute the least common multiple of two integers
 */
QofInt128 lcm (int64_t a, int64_t b);

#endif //QOFINT128_H

/** @} */
