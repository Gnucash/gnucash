/********************************************************************
 * gnc-numeric.hpp - A rational number library for int64            *
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

#ifndef __GNC_NUMERIC_HPP__
#define __GNC_NUMERIC_HPP__

#include <string>
#include <iostream>
#include <locale>
#include <typeinfo> // For std::bad_cast exception
#include "gnc-rational-rounding.hpp"

class GncRational;

/**@ingroup QOF
 *  @brief The primary numeric class for representing amounts and values.
 *
 * Calculations are generally performed in 128-bit (by converting to
 * GncRational) and reducing the result. If the result would overflow a 64-bit
 * representation then the GncNumeric(GncRational&) constructor will call
 * GncRational::round_to_numeric() to get the value to fit. It will not raise an
 * exeception, so in the unlikely event that you need an error instead of
 * rounding, use GncRational directly.
 *
 * Errors: Errors are signalled by exceptions as follows:
 * * A zero denominator will raise a std::invalid_argument.
 * * Division by zero will raise a std::underflow_error.
 * * Overflowing 128 bits will raise a std::overflow_error.
 * * Failure to convert a number as specified by the arguments to convert() will
 * raise a std::domain_error.
 *
 * Rounding Policy: GncNumeric provides a convert() member function that object
 * amount and value setters (and *only* those functions!) should call to set a
 * number which is represented in the commodity's SCU. Since SCUs are seldom 18
 * digits the convert may result in rounding, which will be performed in the
 * method specified by the arguments passed to convert(). Overflows may result
 * in internal rounding as described earlier.
 */

class GncNumeric
{
public:
    /**
     * Default constructor provides the zero value.
     */
    GncNumeric() : m_num (0), m_den(1) {}
    /**
     * Integer constructor.
     *
     * Unfortunately specifying a default for denom causes ambiguity errors with
     * the other single-argument constructors on older versions of gcc, so one
     * must always specify both arguments.
     *
     * \param num The Numerator
     * \param denom The Denominator
     */
    GncNumeric(int64_t num, int64_t denom) :
        m_num(num), m_den(denom) {
        if (denom == 0)
            throw std::invalid_argument("Attempt to construct a GncNumeric with a 0 denominator.");
    }
    /**
     * GncRational constructor.
     *
     * This constructor will round rr's GncInt128s to fit into the int64_t
     * members using RoundType::half-down.
     *
     * \param rr A GncRational.
     */
    GncNumeric(GncRational rr);
    /**
     * gnc_numeric constructor, used for interfacing old code. This function
     * should not be used outside of gnc-numeric.cpp.
     *
     * \param in A gnc_numeric.
     */
    GncNumeric(gnc_numeric in) : m_num(in.num), m_den(in.denom)
    {
        if (in.denom == 0)
            throw std::invalid_argument("Attempt to construct a GncNumeric with a 0 denominator.");
        /* gnc_numeric has a dumb convention that a negative denominator means
         * to multiply the numerator by the denominator instead of dividing.
         */
        if (in.denom < 0)
        {
            m_num *= -in.denom;
            m_den = 1;
        }
    }
    /**
     * Double constructor.
     *
     * @param d The double to be converted. In order to fit in an int64_t, its
     * absolute value must be < 1e18; if its absolute value is < 1e-18 it will
     * be represented as 0, though for practical purposes nearly all commodities
     * will round to zero at 1e-9 or larger.
     */
    GncNumeric(double d);

    /**
     * String constructor.
     *
     * Accepts integer values in decimal and hexadecimal. Does not accept
     * thousands separators. If the string contains a '/' it is taken to
     * separate the numerator and denominator; if it conains either a '.' or a
     * ',' it is taken as a decimal point and the integers on either side will
     * be combined and a denominator will be the appropriate power of 10. If
     * neither is present the number will be treated as an integer and m_den
     * will be set to 1.
     *
     * Whitespace around a '/' is ignored. A correctly-formatted number will be
     * extracted from a larger string.
     *
     * Numbers that cannot be represented with int64_ts will throw
     * std::out_of_range unless a decimal point is found (see above). A 0
     * denominator will cause the constructor to throw std::underflow_error. An
     * empty string or one which contains no recognizable number will result in
     * std::invalid_argument.
     */
    GncNumeric(const std::string& str, bool autoround=false);
    GncNumeric(const GncNumeric& rhs) = default;
    GncNumeric(GncNumeric&& rhs) = default;
    GncNumeric& operator=(const GncNumeric& rhs) = default;
    GncNumeric& operator=(GncNumeric&& rhs) = default;
    ~GncNumeric() = default;
    /**
     * gnc_numeric conversion. Use static_cast<gnc_numeric>(foo)
     */
    operator gnc_numeric() const noexcept;
    /**
     * double conversion. Use static_cast<double>(foo)
     */
    operator double() const noexcept;

    /**
     * Accessor for numerator value.
     */
    int64_t num() const noexcept { return m_num; }
    /**
     * Accessor for denominator value.
     */
    int64_t denom() const noexcept { return m_den; }
    /**
     * @return A GncNumeric with the opposite sign.
     */
    GncNumeric operator-() const noexcept;
    /**
     * @return 0 if this == 0 else 1/this.
     */
    GncNumeric inv() const noexcept;
    /**
     * @return -this if this < 0 else this.
     */
    GncNumeric abs() const noexcept;
    /**
     * Return an equivalent fraction with all common factors between the
     * numerator and the denominator removed.
     *
     * @return reduced GncNumeric
     */
    GncNumeric reduce() const noexcept;
    /**
     * Convert a GncNumeric to use a new denominator. If rounding is necessary
     * use the indicated template specification. For example, to use half-up
     * rounding you'd call bar = foo.convert<RoundType::half_up>(1000). If you
     * specify RoundType::never this will throw std::domain_error if rounding is
     * required.
     *
     * \param new_denom The new denominator to convert the fraction to.
     * \return A new GncNumeric having the requested denominator.
     */
    template <RoundType RT>
    GncNumeric convert(int64_t new_denom) const
    {
        auto params = prepare_conversion(new_denom);
        if (new_denom == GNC_DENOM_AUTO)
            new_denom = m_den;
        if (params.rem == 0)
            return GncNumeric(params.num, new_denom);
        return GncNumeric(round(params.num, params.den,
                                params.rem, RT2T<RT>()), new_denom);
    }

    /**
     * Convert with the specified sigfigs. The resulting denominator depends on
     * the value of the GncNumeric, such that the specified significant digits
     * are retained in the numerator and the denominator is always a power of
     * 10. This is of rather dubious benefit in an accounting program, but it's
     * used in several places so it needs to be implemented.
     *
     * @param figs The number of digits to use for the numerator.
     * @return A GncNumeric with the specified number of digits in the numerator
     * and the appropriate power-of-ten denominator.
     */
    template <RoundType RT>
    GncNumeric convert_sigfigs(unsigned int figs) const
    {
        auto new_denom(sigfigs_denom(figs));
        auto params = prepare_conversion(new_denom);
        if (new_denom == 0) //It had better not, but just in case...
            new_denom = 1;
        if (params.rem == 0)
            return GncNumeric(params.num, new_denom);
        return GncNumeric(round(params.num, params.den,
                                params.rem, RT2T<RT>()), new_denom);
    }
    /**
     * Return a string representation of the GncNumeric. See operator<< for
     * details.
     */
    std::string to_string() const noexcept;
    /**
     * @return true if the denominator is a power of ten, false otherwise.
     */
    bool is_decimal() const noexcept;
    /**
     * Convert the numeric to have a power-of-10 denominator if possible without
     * rounding. Throws a std::range_error on failure; the message will explain
     * the details.
     *
     * @param max_places exponent of the largest permissible denominator.
     * @return A GncNumeric value with the new denominator.
     */
    GncNumeric to_decimal(unsigned int max_places=17) const;
    /**
     * \defgroup gnc_numeric_mutators
     *
     * These are the standard mutating operators. They use GncRational's
     * operators and then call the GncRational constructor, which will silently
     * round half-down.
     *
     * @{
     */
    void operator+=(GncNumeric b);
    void operator-=(GncNumeric b);
    void operator*=(GncNumeric b);
    void operator/=(GncNumeric b);
    /* @} */
    /** Compare function
     *  \defgroup gnc_numeric_comparison
     *  @param b GncNumeric or int to compare to.
     *  @return -1 if this < b, 0 if ==, 1 if this > b.
     * @{
     */
    int cmp(GncNumeric b);
    int cmp(int64_t b) { return cmp(GncNumeric(b, 1)); }
    /** @} */
private:
    struct round_param
    {
        int64_t num;
        int64_t den;
        int64_t rem;
    };
    /* Calculates the denominator required to convert to figs sigfigs. */
    int64_t sigfigs_denom(unsigned figs) const noexcept;
    /* Calculates a round_param struct to pass to a rounding function that will
     * finish computing a GncNumeric with the new denominator.
     */
    round_param prepare_conversion(int64_t new_denom) const;
    int64_t m_num;
    int64_t m_den;
};

/**
 * \defgroup gnc_numeric_arithmetic_operators
 * @{
 * Normal arithmetic operators. The class arithmetic operators are implemented
 * in terms of these operators. They use GncRational operators internally then
 * call the GncNumeric(GncRational&) constructor which will silently round
 * half-down to obtain int64_t members.
 *
 * These operators can throw std::overflow_error, std::underflow_error, or
 * std::invalid argument as indicated in the class GncNumeric documentation.
 *
 * \param a The right-side operand
 * \param b The left-side operand
 * \return A GncNumeric computed from the operation.
 */
GncNumeric operator+(GncNumeric a, GncNumeric b);
inline GncNumeric operator+(GncNumeric a, int64_t b)
{
    return a + GncNumeric(b, 1);
}
inline GncNumeric operator+(int64_t a, GncNumeric b)
{
    return b + GncNumeric(a, 1);
}
GncNumeric operator-(GncNumeric a, GncNumeric b);
inline GncNumeric operator-(GncNumeric a, int64_t b)
{
    return a - GncNumeric(b, 1);
}
inline GncNumeric operator-(int64_t a, GncNumeric b)
{
    return b - GncNumeric(a, 1);
}
GncNumeric operator*(GncNumeric a, GncNumeric b);
inline GncNumeric operator*(GncNumeric a, int64_t b)
{
    return a * GncNumeric(b, 1);
}
inline GncNumeric operator*(int64_t a, GncNumeric b)
{
    return b * GncNumeric(a, 1);
}
GncNumeric operator/(GncNumeric a, GncNumeric b);
inline GncNumeric operator/(GncNumeric a, int64_t b)
{
    return a / GncNumeric(b, 1);
}
inline GncNumeric operator/(int64_t a, GncNumeric b)
{
    return b / GncNumeric(a, 1);
}
/** @} */
/**
 * std::stream output operator. Uses standard integer operator<< so should obey
 * locale rules. Numbers are presented as integers if the denominator is 1, as a
 * decimal if the denominator is a multiple of 10, otherwise as
 * "numerator/denominator".
 */
std::ostream& operator<<(std::ostream&, GncNumeric);

/* Implementation adapted from "The C++ Standard Library, Second Edition" by
 * Nicolai M. Josuttis, Addison-Wesley, 2012, ISBN 978-0-321-62321-8.
 */
template <typename charT, typename traits>
std::basic_ostream<charT, traits>& operator<<(std::basic_ostream<charT, traits>& s, GncNumeric n)
{
    std::basic_ostringstream<charT, traits> ss;
    std::locale loc = s.getloc();
    ss.imbue(loc);
    auto dec_pt = static_cast<charT>('.');
    try
    {
        dec_pt = std::use_facet<std::numpunct<charT>>(loc).decimal_point();
    }
    catch(const std::bad_cast& err) {} //Don't do anything, num_sep is already set.

    ss.copyfmt(s);
    ss.width(0);
    if (n.denom() == 1)
         ss << n.num();
    else if (n.is_decimal())
        ss << n.num() / n.denom() << dec_pt
           << (n.num() > 0 ? n.num() : -n.num()) % n.denom();
    else
        ss << n.num() << "/" << n.denom();
    s << ss.str();
    return s;
}

/**
 * std::stream input operator.
 *
 * Doesn't do any special space handling, spaces in the input stream will
 * delimit elements. The result will be that if a number is presented as "123 /
 * 456", the resulting GncNumeric will be 123/1 and the rest will go to the next
 * parameter in the stream call. The GncNumeric will be constructed with the
 * string constructor with autorounding. It will throw in the event of any
 * errors noted in the string constructor documentation.
 */
/* Implementation adapted from "The C++ Standard Library, Second Edition" by
 * Nicolai M. Josuttis, Addison-Wesley, 2012, ISBN 978-0-321-62321-8.
 */
template <typename charT, typename traits>
std::basic_istream<charT, traits>& operator>>(std::basic_istream<charT, traits>& s, GncNumeric& n)
{
    std::basic_string<charT, traits> instr;
    s >> instr;
    if (s)
        n = GncNumeric(instr, true);
    return s;
}

/**
 * @return -1 if a < b, 0 if a == b, 1 if a > b.
 */
inline int cmp(GncNumeric a, GncNumeric b) { return a.cmp(b); }
inline int cmp(GncNumeric a, int64_t b) { return a.cmp(b); }
inline int cmp(int64_t a, GncNumeric b) { return GncNumeric(a, 1).cmp(b); }

/**
 * \defgroup gnc_numeric_comparison_operators
 * @{
 * Standard comparison operators, which do what one would expect.
 */
inline bool operator<(GncNumeric a, GncNumeric b) { return cmp(a, b) < 0; }
inline bool operator<(GncNumeric a, int64_t b) { return cmp(a, b) < 0; }
inline bool operator<(int64_t a, GncNumeric b) { return cmp(a, b) < 0; }
inline bool operator>(GncNumeric a, GncNumeric b) { return cmp(a, b) > 0; }
inline bool operator>(GncNumeric a, int64_t b) { return cmp(a, b) > 0; }
inline bool operator>(int64_t a, GncNumeric b) { return cmp(a, b) > 0; }
inline bool operator==(GncNumeric a, GncNumeric b) { return cmp(a, b) == 0; }
inline bool operator==(GncNumeric a, int64_t b) { return cmp(a, b) == 0; }
inline bool operator==(int64_t a, GncNumeric b) { return cmp(a, b) == 0; }
inline bool operator<=(GncNumeric a, GncNumeric b) { return cmp(a, b) <= 0; }
inline bool operator<=(GncNumeric a, int64_t b) { return cmp(a, b) <= 0; }
inline bool operator<=(int64_t a, GncNumeric b) { return cmp(a, b) <= 0; }
inline bool operator>=(GncNumeric a, GncNumeric b) { return cmp(a, b) >= 0; }
inline bool operator>=(GncNumeric a, int64_t b) { return cmp(a, b) >= 0; }
inline bool operator>=(int64_t a, GncNumeric b) { return cmp(a, b) >= 0; }
inline bool operator!=(GncNumeric a, GncNumeric b) { return cmp(a, b) != 0; }
inline bool operator!=(GncNumeric a, int64_t b) { return cmp(a, b) != 0; }
inline bool operator!=(int64_t a, GncNumeric b) { return cmp(a, b) != 0; }
/** @} */
/**
 * Convenience function to quickly return 10**digits.
 * \param digits The desired exponent. Maximum value is 17.
 * \return 10**digits
 */
int64_t powten(unsigned int digits);

#endif // __GNC_NUMERIC_HPP__
