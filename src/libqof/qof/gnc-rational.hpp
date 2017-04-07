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
#include "gnc-rational-rounding.hpp"

class GncNumeric;
enum class RoundType;
enum class DenomType;

/** @ingroup QOF
 *  @brief Rational number class using GncInt128 for the numerator
 *  and denominator.
 *
 * This class provides far greater overflow protection compared to GncNumeric at
 * the expense of doubling the size, so GncNumeric is preferred for storage into
 * objects. Furthermore the backends are not able to store GncRational numbers;
 * storage in SQL would require using BLOBs which would preclude calculations in
 * queries. GncRational exists *primarily* as a more overflow-resistant
 * calculation facility for GncNumeric. It's available for cases where one needs
 * an error instead of an automatically rounded value for a calculation that
 * produces a result that won't fit into an int64 without rounding.

 * Errors: Errors are signalled by exceptions as follows:
 * * A zero denominator will raise a std::invalid_argument.
 * * Division by zero will raise a std::underflow_error.
 * * Overflowing 128 bits will raise a std::overflow_error.
 * * Failure to convert a number as specified by the arguments to convert() will
 * raise a std::domain_error.
 *
 */


class GncRational
{
public:
    /**
     * Default constructor provides the zero value.
     */
    GncRational() : m_num(0), m_den(1) {}
    /**
     * GncInt128 constructor. This will take any flavor of built-in integer
     * thanks to implicit construction of the GncInt128s.
     */
    GncRational (GncInt128 num, GncInt128 den) noexcept
        : m_num(num), m_den(den) {}
    /** Convenience constructor from the C API's gnc_numeric. */
    GncRational (gnc_numeric n) noexcept;
    /** GncNumeric constructor. */
    GncRational(GncNumeric n) noexcept;
    GncRational(const GncRational& rhs) = default;
    GncRational(GncRational&& rhs) = default;
    GncRational& operator=(const GncRational& rhs) = default;
    GncRational& operator=(GncRational&& rhs) = default;
    ~GncRational() = default;
    /** Report if both members are valid numbers.
     * \return true if neither numerator nor denominator are Nan or Overflowed.
     */
    bool valid() const noexcept;
    /** Report if either numerator or denominator are too big to fit in an
     * int64_t.
     * \return true if either is too big.
     */
    bool is_big() const noexcept;
    /** Conversion operator; use static_cast<gnc_numeric>(foo). */
    operator gnc_numeric() const noexcept;
    /** Make a new GncRational with the opposite sign. */
    GncRational operator-() const noexcept;
    /**
     * Return an equivalent fraction with all common factors between the
     * numerator and the denominator removed.
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
    /**
     * Convert a GncRational to use a new denominator. If rounding is necessary
     * use the indicated template specification. For example, to use half-up
     * rounding you'd call bar = foo.convert<RoundType::half_up>(1000). If you
     * specify RoundType::never this will throw std::domain_error if rounding is
     * required.
     *
     * \param new_denom The new denominator to convert the fraction to.
     * \return A new GncRational having the requested denominator.
     */
    template <RoundType RT>
    GncRational convert (GncInt128 new_denom) const
    {
        auto params = prepare_conversion(new_denom);
        if (new_denom == GNC_DENOM_AUTO)
            new_denom = m_den;
        if (params.rem == 0)
            return GncRational(params.num, new_denom);
        return GncRational(round(params.num, params.den,
                                 params.rem, RT2T<RT>()), new_denom);
    }

    /**
     * Convert with the specified sigfigs. The resulting denominator depends on
     * the value of the GncRational, such that the specified significant digits
     * are retained in the numerator and the denominator is always a power of
     * 10. This is of rather dubious benefit in an accounting program, but it's
     * used in several places so it needs to be implemented.
     *
     * @param figs The number of digits to use for the numerator.
     * @return A GncRational with the specified number of digits in the
     * numerator and the appropriate power-of-ten denominator.
     */
    template <RoundType RT>
    GncRational convert_sigfigs(unsigned int figs) const
    {
        auto new_denom(sigfigs_denom(figs));
        auto params = prepare_conversion(new_denom);
        if (new_denom == 0) //It had better not, but just in case...
            new_denom = 1;
        if (params.rem == 0)
            return GncRational(params.num, new_denom);
        return GncRational(round(params.num, params.den,
                                params.rem, RT2T<RT>()), new_denom);
    }

    /** Numerator accessor */
    GncInt128 num() const noexcept { return m_num; }
    /** Denominator accessor */
    GncInt128 denom() const noexcept { return m_den; }
    /** @defgroup gnc_rational_mutators
     *  @{
     * Standard mutating arithmetic operators.
     */
    void operator+=(GncRational b);
    void operator-=(GncRational b);
    void operator*=(GncRational b);
    void operator/=(GncRational b);
    /** @} */
    /** Inverts the number, equivalent of /= {1, 1} */
    GncRational inv() const noexcept;
    /** Absolute value; return value is always >= 0 and of same magnitude. */
    GncRational abs() const noexcept;
    /** Compare function
     *
     * @param b GncNumeric or integer value to compare to.
     * @return -1 if < b, 0 if equal, 1 if > b.
     */
    int cmp(GncRational b);
    int cmp(GncInt128 b) { return cmp(GncRational(b, 1)); }

private:
    struct round_param
    {
        GncInt128 num;
        GncInt128 den;
        GncInt128 rem;
    };
    /* Calculates the denominator required to convert to figs sigfigs. Note that
     * it uses the same powten function that the GncNumeric version does because
     * 17 significant figures should be plenty.
     */
    GncInt128 sigfigs_denom(unsigned figs) const noexcept;
    /* Calculates a round_param struct to pass to a rounding function that will
     * finish computing a GncNumeric with the new denominator.
     */
    round_param prepare_conversion(GncInt128 new_denom) const;
    GncInt128 m_num;
    GncInt128 m_den;
};

/**
 * @return -1 if a < b, 0 if a == b, 1 if a > b.
 */
inline int cmp(GncRational a, GncRational b) { return a.cmp(b); }
inline int cmp(GncRational a, GncInt128 b) { return a.cmp(b); }
inline int cmp(GncInt128 a, GncRational b) { return GncRational(a, 1).cmp(b); }

/**
 * \defgroup gnc_rational_comparison_operators
 * @{
 * Standard comparison operators, which do what one would expect.
 */
inline bool operator<(GncRational a, GncRational b) { return cmp(a, b) < 0; }
inline bool operator<(GncRational a, GncInt128 b) { return cmp(a, b) < 0; }
inline bool operator<(GncInt128 a, GncRational b) { return cmp(a, b) < 0; }
inline bool operator>(GncRational a, GncRational b) { return cmp(a, b) > 0; }
inline bool operator>(GncRational a, GncInt128 b) { return cmp(a, b) > 0; }
inline bool operator>(GncInt128 a, GncRational b) { return cmp(a, b) > 0; }
inline bool operator==(GncRational a, GncRational b) { return cmp(a, b) == 0; }
inline bool operator==(GncRational a, GncInt128 b) { return cmp(a, b) == 0; }
inline bool operator==(GncInt128 a, GncRational b) { return cmp(a, b) == 0; }
inline bool operator<=(GncRational a, GncRational b) { return cmp(a, b) <= 0; }
inline bool operator<=(GncRational a, GncInt128 b) { return cmp(a, b) <= 0; }
inline bool operator<=(GncInt128 a, GncRational b) { return cmp(a, b) <= 0; }
inline bool operator>=(GncRational a, GncRational b) { return cmp(a, b) >= 0; }
inline bool operator>=(GncRational a, GncInt128 b) { return cmp(a, b) >= 0; }
inline bool operator>=(GncInt128 a, GncRational b) { return cmp(a, b) >= 0; }
inline bool operator!=(GncRational a, GncRational b) { return cmp(a, b) != 0; }
inline bool operator!=(GncRational a, GncInt128 b) { return cmp(a, b) != 0; }
inline bool operator!=(GncInt128 a, GncRational b) { return cmp(a, b) != 0; }
/** @} */

/**
 * \defgroup gnc_rational_arithmetic_operators
 *
 * Normal arithmetic operators. The class arithmetic operators are implemented
 * in terms of these operators.
 *
 * These operators can throw std::overflow_error, std::underflow_error, or
 * std::invalid argument as indicated in the class documentation.
 *
 * \param a The right-side operand
 * \param b The left-side operand
 * \return A GncRational computed from the operation.
 */
GncRational operator+(GncRational a, GncRational b);
inline GncRational operator+(GncRational a, GncInt128 b)
{
    return a + GncRational(b, 1);
}
inline GncRational operator+(GncInt128 a, GncRational b)
{
    return GncRational(a, 1) + b;
}
GncRational operator-(GncRational a, GncRational b);
inline GncRational operator-(GncRational a, GncInt128 b)
{
    return a - GncRational(b, 1);
}
inline GncRational operator-(GncInt128 a, GncRational b)
{
    return GncRational(a, 1) - b;
}
GncRational operator*(GncRational a, GncRational b);
inline GncRational operator*(GncRational a, GncInt128 b)
{
    return a * GncRational(b, 1);
}
inline GncRational operator*(GncInt128 a, GncRational b)
{
    return GncRational(a, 1) * b;
}
GncRational operator/(GncRational a, GncRational b);
inline GncRational operator/(GncRational a, GncInt128 b)
{
    return a / GncRational(b, 1);
}
inline GncRational operator/(GncInt128 a, GncRational b)
{
    return GncRational(a, 1) / b;
}

inline std::ostream& operator<<(std::ostream& stream, const GncRational& val) noexcept
{
    stream << val.num() << "/" << val.denom();
    return stream;
}
/** @} */
#endif //__GNC_RATIONAL_HPP__
