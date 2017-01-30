/********************************************************************
 * gnc-numeric.c -- an exact-number library for accounting use      *
 * Copyright (C) 2000 Bill Gribble                                  *
 * Copyright (C) 2004 Linas Vepstas <linas@linas.org>               *
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

#include <glib.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "qof.h"
}

#include <stdint.h>
#include <regex>
#include <sstream>
#include <cstdlib>

#include "gnc-numeric.hpp"
#include "gnc-rational.hpp"

static QofLogModule log_module = "qof";
static const gint64 pten[] = { 1, 10, 100, 1000, 10000, 100000, 1000000,
                               10000000, 100000000, 1000000000,
                               INT64_C(10000000000), INT64_C(100000000000),
                               INT64_C(1000000000000), INT64_C(10000000000000),
                               INT64_C(100000000000000),
                               INT64_C(10000000000000000),
                               INT64_C(100000000000000000),
                               INT64_C(1000000000000000000)};
#define POWTEN_OVERFLOW -5

int64_t
powten (int64_t exp)
{
    if (exp > 18 || exp < -18)
        return POWTEN_OVERFLOW;
    return exp < 0 ? -pten[-exp] : pten[exp];
}

GncNumeric::GncNumeric(GncRational rr)
{
    if (rr.m_num.isNan() || rr.m_den.isNan())
        throw std::underflow_error("Operation resulted in NaN.");
    if (rr.m_num.isOverflow() || rr.m_den.isOverflow())
        throw std::overflow_error("Operation overflowed a 128-bit int.");
    if (rr.m_num.isBig() || rr.m_den.isBig())
    {
        GncRational reduced(rr.reduce());
        rr = reduced.round_to_numeric(); // A no-op if it's already small.
    }
    m_num = static_cast<int64_t>(rr.m_num);
    m_den = static_cast<int64_t>(rr.m_den);
}

GncNumeric::GncNumeric(double d) : m_num(0), m_den(1)
{
    if (isnan(d) || fabs(d) > 1e18)
    {
        std::ostringstream msg;
        msg << "Unable to construct a GncNumeric from " << d << ".\n";
        throw std::invalid_argument(msg.str());
    }
    constexpr auto max_denom = INT64_MAX / 10;
    auto logval = log10(fabs(d));
    int64_t den;
    if (logval > 0.0)
        den = powten(18 - static_cast<int>(floor(logval) + 1.0));
    else
        den = powten(17);
    auto num = static_cast<int64_t>(floor(static_cast<double>(den) * d));

    if (num == 0)
        return;
    GncNumeric q(num, den);
    auto r = q.reduce();
    m_num = r.num();
    m_den = r.denom();
}

GncNumeric::GncNumeric(const std::string& str, bool autoround)
{
    static const std::string numer_frag("(-?[0-9]+)");
    static const std::string denom_frag("([0-9]+)");
    static const std::string hex_frag("(0x[a-f0-9]+)");
    static const std::string slash( "[ \\t]*/[ \\t]*");
    /* The llvm standard C++ library refused to recognize the - in the
     * numer_frag patter with the default ECMAScript syntax so we use the awk
     * syntax.
     */
    static const std::regex numeral(numer_frag, std::regex::awk);
    static const std::regex hex(hex_frag, std::regex::awk);
    static const std::regex numeral_rational(numer_frag + slash + denom_frag,
                                             std::regex::awk);
    static const std::regex hex_rational(hex_frag + slash + hex_frag,
                                         std::regex::awk);
    static const std::regex hex_over_num(hex_frag + slash + denom_frag,
                                         std::regex::awk);
    static const std::regex num_over_hex(numer_frag + slash + hex_frag,
                                         std::regex::awk);
    static const std::regex decimal(numer_frag + "[.,]" + denom_frag,
                                    std::regex::awk);
    std::smatch m;
/* The order of testing the regexes is from the more restrictve to the less
 * restrictive, as less-restrictive ones will match patterns that would also
 * match the more-restrictive and so invoke the wrong construction.
 */
    if (str.empty())
        throw std::invalid_argument("Can't construct a GncNumeric from an empty string.");
    if (std::regex_search(str, m, hex_rational))
    {
        GncNumeric n(stoll(m[1].str(), nullptr, 16),
                     stoll(m[2].str(), nullptr, 16));
        m_num = n.num();
        m_den = n.denom();
        return;
    }
    if (std::regex_search(str, m, hex_over_num))
    {
        GncNumeric n(stoll(m[1].str(), nullptr, 16),
                     stoll(m[2].str()));
        m_num = n.num();
        m_den = n.denom();
        return;
    }
    if (std::regex_search(str, m, num_over_hex))
    {
        GncNumeric n(stoll(m[1].str()),
                     stoll(m[2].str(), nullptr, 16));
        m_num = n.num();
        m_den = n.denom();
        return;
    }
    if (std::regex_search(str, m, numeral_rational))
    {
        GncNumeric n(stoll(m[1].str()), stoll(m[2].str()));
        m_num = n.num();
        m_den = n.denom();
        return;
    }
    if (std::regex_search(str, m, decimal))
    {
        GncInt128 high(stoll(m[1].str()));
        GncInt128 low(stoll(m[2].str()));
        int64_t d = powten(m[2].str().length());
        GncInt128 n = high * d + (high > 0 ? low : -low);
        if (!autoround && n.isBig())
        {
            std::ostringstream errmsg;
            errmsg << "Decimal string " << m[1].str() << "." << m[2].str()
                   << "can't be represented in a GncNumeric without rounding.";
            throw std::overflow_error(errmsg.str());
        }
        while (n.isBig() && d > 0)
        {
            n >>= 1;
            d >>= 1;
        }
        if (n.isBig()) //Shouldn't happen, of course
        {
            std::ostringstream errmsg;
            errmsg << "Decimal string " << m[1].str() << "." << m[2].str()
            << " can't be represented in a GncNumeric, even after reducing denom to " << d;
            throw std::overflow_error(errmsg.str());
        }
        GncNumeric gncn(static_cast<int64_t>(n), d);
        m_num = gncn.num();
        m_den = gncn.denom();
        return;
    }
    if (std::regex_search(str, m, hex))
    {
        GncNumeric n(stoll(m[1].str(), nullptr, 16),INT64_C(1));
        m_num = n.num();
        m_den = n.denom();
        return;
    }
    if (std::regex_search(str, m, numeral))
    {
        GncNumeric n(stoll(m[1].str()), INT64_C(1));
        m_num = n.num();
        m_den = n.denom();
        return;
    }
    std::ostringstream errmsg;
    errmsg << "String " << str << " contains no recognizable numeric value.";
    throw std::invalid_argument(errmsg.str());
}

GncNumeric::operator gnc_numeric() const noexcept
{
    return {m_num, m_den};
}

GncNumeric::operator double() const noexcept
{
    return static_cast<double>(m_num) / static_cast<double>(m_den);
}

GncNumeric
GncNumeric::operator-() const noexcept
{
    GncNumeric b(*this);
    b.m_num = - b.m_num;
    return b;
}

GncNumeric
GncNumeric::inv() const noexcept
{
    if (m_num == 0)
        return *this;
    if (m_num < 0)
        return GncNumeric(-m_den, -m_num);
    return GncNumeric(m_den, m_num);
}

GncNumeric
GncNumeric::abs() const noexcept
{
    if (m_num < 0)
        return -*this;
    return *this;
}

GncNumeric
GncNumeric::reduce() const noexcept
{
    return static_cast<GncNumeric>(GncRational(*this).reduce());
}

GncNumeric::round_param
GncNumeric::prepare_conversion(int64_t new_denom) const
{
    if (new_denom == m_den || new_denom == GNC_DENOM_AUTO)
        return {m_num, m_den, 0};
    GncRational conversion(new_denom, m_den);
    auto red_conv = conversion.reduce();
    GncInt128 old_num(m_num);
    auto new_num = old_num * red_conv.m_num;
    auto rem = new_num % red_conv.m_den;
    new_num /= red_conv.m_den;
    if (new_num.isBig())
    {
        GncRational rr(new_num, new_denom);
        GncNumeric nn(rr);
        rr.round(new_denom, RoundType::truncate);
        return {static_cast<int64_t>(rr.m_num), new_denom, 0};
    }
    return {static_cast<int64_t>(new_num), static_cast<int64_t>(red_conv.m_den),
            static_cast<int64_t>(rem)};
}

int64_t
GncNumeric::sigfigs_denom(unsigned figs) const noexcept
{
    int64_t num_abs{std::abs(m_num)};
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

std::string
GncNumeric::to_string() const noexcept
{
    std::ostringstream out;
    out << *this;
    return out.str();
}

GncNumeric
GncNumeric::to_decimal(unsigned int max_places) const
{
    if (max_places > 17)
        max_places = 17;
    bool is_pwr_ten = true;
    for (int pwr = 1; pwr <= 17 && m_den > powten(pwr); ++pwr)
        if (m_den % powten(pwr))
        {
            is_pwr_ten = false;
            break;
        }

    if (m_num == 0 || (is_pwr_ten && m_den < powten(max_places)))
        return *this; // Nothing to do.
    if (is_pwr_ten)
    {
        /* See if we can reduce m_num to fit in max_places */
        auto excess = m_den / powten(max_places);
        if (m_num % excess)
        {
            std::ostringstream msg;
            msg << "GncNumeric " << *this
                << " could not be represented in " << max_places
                << " decimal places without rounding.\n";
            throw std::range_error(msg.str());
        }
        return GncNumeric(m_num / excess, powten(max_places));
    }
    GncRational rr(*this);
    rr.round(powten(max_places), RoundType::never);
    if (rr.m_error)
    {
        std::ostringstream msg;
        msg << "GncNumeric " << *this
            << " could not be represented as a decimal without rounding.\n";
        throw std::range_error(msg.str());
    }
    /* rr might have gotten reduced a bit too much; if so, put it back: */
    unsigned int pwr{1};
    for (; pwr <= max_places && !(rr.m_den % powten(pwr)); ++pwr);
    if (rr.m_den % powten(pwr))
    {
        auto factor(powten(pwr) / rr.m_den);
        rr.m_num *= factor;
        rr.m_den *= factor;
    }
    while (rr.m_num % 10 == 0)
    {
        rr.m_num /= 10;
        rr.m_den /= 10;
    }
    try
    {
        /* Construct from the parts to avoid the GncRational constructor's
         * automatic rounding.
         */
        return {static_cast<int64_t>(rr.m_num), static_cast<int64_t>(rr.m_den)};
    }
    catch (const std::invalid_argument& err)
    {
        std::ostringstream msg;
        msg << "GncNumeric " << *this
            << " could not be represented as a decimal without rounding.\n";
        throw std::range_error(msg.str());
    }
    catch (const std::overflow_error& err)
    {
        std::ostringstream msg;
        msg << "GncNumeric " << *this
            << " overflows when attempting to convert it to decimal.\n";
        throw std::range_error(msg.str());
    }
}

void
GncNumeric::operator+=(GncNumeric b)
{
    *this = *this + b;
}

void
GncNumeric::operator-=(GncNumeric b)
{
    *this = *this - b;
}

void
GncNumeric::operator*=(GncNumeric b)
{
    *this = *this * b;
}

void
GncNumeric::operator/=(GncNumeric b)
{
    *this = *this / b;
}

int
GncNumeric::cmp(GncNumeric b)
{
    if (m_den == b.denom())
    {
        auto b_num = b.num();
        return m_num < b_num ? -1 : b_num < m_num ? 1 : 0;
    }
//    GncInt128 a_den(m_den), b_den(b.denom());
//    auto lcm = a_den.gcd(b_den);
//    GncInt128 a_num(m_num * gcd / a_den), b_num(b.num() * gcd / b_den);
//    return a_num < b_num ? -1 : b_num < a_num ? 1 : 0;
    GncRational an(*this), bn(b);
    return (an.m_num * bn.m_den).cmp(bn.m_num * an.m_den);
}

GncNumeric
operator+(GncNumeric a, GncNumeric b)
{
    if (a.num() == 0)
        return b;
    if (b.num() == 0)
        return a;
    GncRational ar(a), br(b);
    auto rr = ar + br;
    return static_cast<GncNumeric>(rr);
}

GncNumeric
operator-(GncNumeric a, GncNumeric b)
{
    return a + (-b);
}

GncNumeric
operator*(GncNumeric a, GncNumeric b)
{
    if (a.num() == 0 || b.num() == 0)
    {
        GncNumeric retval;
        return retval;
    }
    GncRational ar(a), br(b);
    auto rr = ar * br;
    return static_cast<GncNumeric>(rr);
}

GncNumeric
operator/(GncNumeric a, GncNumeric b)
{
    if (a.num() == 0)
    {
        GncNumeric retval;
        return retval;
    }
    if (b.num() == 0)
        throw std::underflow_error("Attempt to divide by zero.");

    GncRational ar(a), br(b);
    auto rr = ar / br;
    return static_cast<GncNumeric>(rr);
}

int
cmp(GncNumeric a, GncNumeric b)
{
    return a.cmp(b);
}

bool
operator<(GncNumeric a, GncNumeric b)
{
    return a.cmp(b) < 0;
}

bool
operator>(GncNumeric a, GncNumeric b)
{
    return a.cmp(b) > 0;
}

bool
operator==(GncNumeric a, GncNumeric b)
{
    return a.cmp(b) == 0;
}

bool
operator<=(GncNumeric a, GncNumeric b)
{
    return a.cmp(b) <= 0;
}

bool
operator>=(GncNumeric a, GncNumeric b)
{
    return a.cmp(b) >= 0;
}

bool
operator!=(GncNumeric a, GncNumeric b)
{
    return a.cmp(b) != 0;
}

static gnc_numeric
convert(GncNumeric num, int64_t new_denom, int how)
{
//    std::cout << "Converting " << num << ".\n";
    auto rtype = static_cast<RoundType>(how & GNC_NUMERIC_RND_MASK);
    unsigned int figs = GNC_HOW_GET_SIGFIGS(how);

    auto dtype = static_cast<DenomType>(how & GNC_NUMERIC_DENOM_MASK);
    bool sigfigs = dtype == DenomType::sigfigs;
    if (dtype == DenomType::reduce)
        num = num.reduce();
    try
    {
        switch (rtype)
        {
            case RoundType::floor:
                if (sigfigs)
                    return static_cast<gnc_numeric>(num.convert_sigfigs<RoundType::floor>(figs));
                else
                    return static_cast<gnc_numeric>(num.convert<RoundType::floor>(new_denom));
            case RoundType::ceiling:
                if (sigfigs)
                    return static_cast<gnc_numeric>(num.convert_sigfigs<RoundType::ceiling>(figs));
                else
                    return static_cast<gnc_numeric>(num.convert<RoundType::ceiling>(new_denom));
            case RoundType::truncate:
                if (sigfigs)
                    return static_cast<gnc_numeric>(num.convert_sigfigs<RoundType::truncate>(figs));
                else
                    return static_cast<gnc_numeric>(num.convert<RoundType::truncate>(new_denom));
            case RoundType::promote:
                if (sigfigs)
                    return static_cast<gnc_numeric>(num.convert_sigfigs<RoundType::promote>(figs));
                else
                    return static_cast<gnc_numeric>(num.convert<RoundType::promote>(new_denom));
            case RoundType::half_down:
                if (sigfigs)
                    return static_cast<gnc_numeric>(num.convert_sigfigs<RoundType::half_down>(figs));
                else
                    return static_cast<gnc_numeric>(num.convert<RoundType::half_down>(new_denom));
            case RoundType::half_up:
                if (sigfigs)
                    return static_cast<gnc_numeric>(num.convert_sigfigs<RoundType::half_up>(figs));
                else
                    return static_cast<gnc_numeric>(num.convert<RoundType::half_up>(new_denom));
            case RoundType::bankers:
                if (sigfigs)
                    return static_cast<gnc_numeric>(num.convert_sigfigs<RoundType::bankers>(figs));
                else
                    return static_cast<gnc_numeric>(num.convert<RoundType::bankers>(new_denom));
            case RoundType::never:
                if (sigfigs)
                    return static_cast<gnc_numeric>(num.convert_sigfigs<RoundType::never>(figs));
                else
                    return static_cast<gnc_numeric>(num.convert<RoundType::never>(new_denom));
            default:
/* round-truncate just returns the numerator unchanged. The old gnc-numeric
 * convert had no "default" behavior at rounding that had the same result, but
 * we need to make it explicit here to run the rest of the conversion code.
 */
                if (sigfigs)
                    return static_cast<gnc_numeric>(num.convert_sigfigs<RoundType::truncate>(figs));
                else
                    return static_cast<gnc_numeric>(num.convert<RoundType::truncate>(new_denom));

//                return static_cast<gnc_numeric>(num);
        }
    }
    catch (const std::domain_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_REMAINDER);
    }
    catch (const std::overflow_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }
    catch (const std::exception& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_ARG);
    }
}

/* =============================================================== */
/* This function is small, simple, and used everywhere below,
 * lets try to inline it.
 */
GNCNumericErrorCode
gnc_numeric_check(gnc_numeric in)
{
    if (G_LIKELY(in.denom != 0))
    {
        return GNC_ERROR_OK;
    }
    else if (in.num)
    {
        if ((0 < in.num) || (-4 > in.num))
        {
            in.num = (gint64) GNC_ERROR_OVERFLOW;
        }
        return (GNCNumericErrorCode) in.num;
    }
    else
    {
        return GNC_ERROR_ARG;
    }
}


/* *******************************************************************
 *  gnc_numeric_zero_p
 ********************************************************************/

gboolean
gnc_numeric_zero_p(gnc_numeric a)
{
    if (gnc_numeric_check(a))
    {
        return 0;
    }
    else
    {
        if ((a.num == 0) && (a.denom != 0))
        {
            return 1;
        }
        else
        {
            return 0;
        }
    }
}

/* *******************************************************************
 *  gnc_numeric_negative_p
 ********************************************************************/

gboolean
gnc_numeric_negative_p(gnc_numeric a)
{
    if (gnc_numeric_check(a))
    {
        return 0;
    }
    else
    {
        if ((a.num < 0) && (a.denom != 0))
        {
            return 1;
        }
        else
        {
            return 0;
        }
    }
}

/* *******************************************************************
 *  gnc_numeric_positive_p
 ********************************************************************/

gboolean
gnc_numeric_positive_p(gnc_numeric a)
{
    if (gnc_numeric_check(a))
    {
        return 0;
    }
    else
    {
        if ((a.num > 0) && (a.denom != 0))
        {
            return 1;
        }
        else
        {
            return 0;
        }
    }
}


/* *******************************************************************
 *  gnc_numeric_compare
 *  returns 1 if a>b, -1 if b>a, 0 if a == b
 ********************************************************************/

int
gnc_numeric_compare(gnc_numeric a, gnc_numeric b)
{
    gint64 aa, bb;

    if (gnc_numeric_check(a) || gnc_numeric_check(b))
    {
        return 0;
    }

    if (a.denom == b.denom)
    {
        if (a.num == b.num) return 0;
        if (a.num > b.num) return 1;
        return -1;
    }

    GncRational an (a), bn (b);

    return (an.m_num * bn.m_den).cmp(bn.m_num * an.m_den);
}


/* *******************************************************************
 *  gnc_numeric_eq
 ********************************************************************/

gboolean
gnc_numeric_eq(gnc_numeric a, gnc_numeric b)
{
    return ((a.num == b.num) && (a.denom == b.denom));
}


/* *******************************************************************
 *  gnc_numeric_equal
 ********************************************************************/

gboolean
gnc_numeric_equal(gnc_numeric a, gnc_numeric b)
{
    if (gnc_numeric_check(a))
    {
        /* a is not a valid number, check b */
        if (gnc_numeric_check(b))
            /* Both invalid, consider them equal */
            return TRUE;
        else
            /* a invalid, b valid */
            return FALSE;
    }
    if (gnc_numeric_check(b))
        /* a valid, b invalid */
        return FALSE;

    return gnc_numeric_compare (a, b) == 0;
}


/* *******************************************************************
 *  gnc_numeric_same
 *  would a and b be equal() if they were both converted to the same
 *  denominator?
 ********************************************************************/

int
gnc_numeric_same(gnc_numeric a, gnc_numeric b, gint64 denom,
                 gint how)
{
    gnc_numeric aconv, bconv;

    aconv = gnc_numeric_convert(a, denom, how);
    bconv = gnc_numeric_convert(b, denom, how);

    return(gnc_numeric_equal(aconv, bconv));
}


/* *******************************************************************
 *  gnc_numeric_add
 ********************************************************************/

gnc_numeric
gnc_numeric_add(gnc_numeric a, gnc_numeric b,
                gint64 denom, gint how)
{
    if (gnc_numeric_check(a) || gnc_numeric_check(b))
    {
        return gnc_numeric_error(GNC_ERROR_ARG);
    }

    GncRational an (a), bn (b);
    GncDenom new_denom (an, bn, denom, how);
    if (new_denom.m_error)
        return gnc_numeric_error (new_denom.m_error);

    try
    {
        return static_cast<gnc_numeric>(an.add(bn, new_denom));
    }
    catch (const std::overflow_error& err)
    {
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }
}

/* *******************************************************************
 *  gnc_numeric_sub
 ********************************************************************/

gnc_numeric
gnc_numeric_sub(gnc_numeric a, gnc_numeric b,
                gint64 denom, gint how)
{
    gnc_numeric nb;
    if (gnc_numeric_check(a) || gnc_numeric_check(b))
    {
        return gnc_numeric_error(GNC_ERROR_ARG);
    }

    nb = b;
    nb.num = -nb.num;
    return gnc_numeric_add (a, nb, denom, how);
}

/* *******************************************************************
 *  gnc_numeric_mul
 ********************************************************************/

gnc_numeric
gnc_numeric_mul(gnc_numeric a, gnc_numeric b,
                gint64 denom, gint how)
{
    if (gnc_numeric_check(a) || gnc_numeric_check(b))
    {
        return gnc_numeric_error(GNC_ERROR_ARG);
    }

    GncRational an (a), bn (b);
    GncDenom new_denom (an, bn, denom, how);
    if (new_denom.m_error)
        return gnc_numeric_error (new_denom.m_error);
    try
    {
        return static_cast<gnc_numeric>(an.mul(bn, new_denom));
    }
    catch (const std::overflow_error& err)
    {
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }

}


/* *******************************************************************
 *  gnc_numeric_div
 ********************************************************************/

gnc_numeric
gnc_numeric_div(gnc_numeric a, gnc_numeric b,
                gint64 denom, gint how)
{
    if (gnc_numeric_check(a) || gnc_numeric_check(b))
    {
        return gnc_numeric_error(GNC_ERROR_ARG);
    }

    GncRational an (a), bn (b);
    GncDenom new_denom (an, bn, denom, how);
    if (new_denom.m_error)
        return gnc_numeric_error (new_denom.m_error);
    try
    {
        return static_cast<gnc_numeric>(an.div(bn, new_denom));
    }
    catch (const std::overflow_error& err)
    {
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }
}

/* *******************************************************************
 *  gnc_numeric_neg
 *  negate the argument
 ********************************************************************/

gnc_numeric
gnc_numeric_neg(gnc_numeric a)
{
    if (gnc_numeric_check(a))
    {
        return gnc_numeric_error(GNC_ERROR_ARG);
    }
    return gnc_numeric_create(- a.num, a.denom);
}

/* *******************************************************************
 *  gnc_numeric_abs
 *  return the absolute value of the argument
 ********************************************************************/

gnc_numeric
gnc_numeric_abs(gnc_numeric a)
{
    if (gnc_numeric_check(a))
    {
        return gnc_numeric_error(GNC_ERROR_ARG);
    }
    return gnc_numeric_create(ABS(a.num), a.denom);
}


/* *******************************************************************
 *  gnc_numeric_convert
 ********************************************************************/

gnc_numeric
gnc_numeric_convert(gnc_numeric in, int64_t denom, int how)
{
    GncRational a (in), b (gnc_numeric_zero());
    GncDenom d (a, b, denom, how);
    try
    {
        d.reduce(a);
        a.round (d.get(), d.m_round);
        return static_cast<gnc_numeric>(a);
    }
    catch (const std::overflow_error& err)
    {
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }
}


/* *******************************************************************
 *  reduce a fraction by GCF elimination.  This is NOT done as a
 *  part of the arithmetic API unless GNC_HOW_DENOM_REDUCE is specified
 *  as the output denominator.
 ********************************************************************/

gnc_numeric
gnc_numeric_reduce(gnc_numeric in)
{
    if (gnc_numeric_check(in))
    {
        return gnc_numeric_error(GNC_ERROR_ARG);
    }

    if (in.denom < 0) /* Negative denoms multiply num, can't be reduced. */
        return in;
    GncRational a (in), b (gnc_numeric_zero());
    GncDenom d (a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE | GNC_HOW_RND_ROUND);
    try
    {
        d.reduce(a);
        a.round (d.get(), d.m_round);
        return static_cast<gnc_numeric>(a);
    }
    catch (const std::overflow_error& err)
    {
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }
}


/* *******************************************************************
 * gnc_numeric_to_decimal
 *
 * Attempt to convert the denominator to an exact power of ten without
 * rounding. TRUE is returned if 'a' has been converted or was already
 * decimal. Otherwise, FALSE is returned and 'a' remains unchanged.
 * The 'max_decimal_places' parameter may be NULL.
 ********************************************************************/

gboolean
gnc_numeric_to_decimal(gnc_numeric *a, guint8 *max_decimal_places)
{
    guint8 decimal_places = 0;
    gnc_numeric converted_val;
    gint64 fraction;

    g_return_val_if_fail(a, FALSE);

    if (gnc_numeric_check(*a) != GNC_ERROR_OK)
        return FALSE;

    converted_val = *a;
    if (converted_val.denom <= 0)
    {
        converted_val = gnc_numeric_convert(converted_val, 1, GNC_HOW_DENOM_EXACT);
        if (gnc_numeric_check(converted_val) != GNC_ERROR_OK)
            return FALSE;
        *a = converted_val;
        if (max_decimal_places)
            *max_decimal_places = decimal_places;
        return TRUE;
    }

    /* Zero is easily converted. */
    if (converted_val.num == 0)
        converted_val.denom = 1;

    fraction = converted_val.denom;
    while (fraction != 1)
    {
        switch (fraction % 10)
        {
        case 0:
            fraction = fraction / 10;
            break;

        case 5:
            converted_val = gnc_numeric_mul(converted_val,
                                            gnc_numeric_create(2, 2),
                                            GNC_DENOM_AUTO,
                                            GNC_HOW_DENOM_EXACT |
                                            GNC_HOW_RND_NEVER);
            if (gnc_numeric_check(converted_val) != GNC_ERROR_OK)
                return FALSE;
            fraction = fraction / 5;
            break;

        case 2:
        case 4:
        case 6:
        case 8:
            converted_val = gnc_numeric_mul(converted_val,
                                            gnc_numeric_create(5, 5),
                                            GNC_DENOM_AUTO,
                                            GNC_HOW_DENOM_EXACT |
                                            GNC_HOW_RND_NEVER);
            if (gnc_numeric_check(converted_val) != GNC_ERROR_OK)
                return FALSE;
            fraction = fraction / 2;
            break;

        default:
            return FALSE;
        }

        decimal_places += 1;
    }

    if (max_decimal_places)
        *max_decimal_places = decimal_places;

    *a = converted_val;

    return TRUE;
}

gnc_numeric
gnc_numeric_invert(gnc_numeric num)
{
    if (num.num == 0)
        return gnc_numeric_zero();
    return static_cast<gnc_numeric>(GncRational(num).inv());
}
/* *******************************************************************
 *  double_to_gnc_numeric
 ********************************************************************/

#ifdef _MSC_VER
# define rint /* */
#endif
gnc_numeric
double_to_gnc_numeric(double in, gint64 denom, gint how)
{
    gnc_numeric out;
    gint64 int_part = 0;
    double frac_part;
    gint64 frac_int = 0;
    double logval;
    double sigfigs;

    if (isnan (in) || fabs (in) > 1e18)
        return gnc_numeric_error (GNC_ERROR_OVERFLOW);

    if ((denom == GNC_DENOM_AUTO) && (how & GNC_HOW_DENOM_SIGFIG))
    {
        if (fabs(in) < 10e-20)
        {
            logval = 0;
        }
        else
        {
            logval   = log10(fabs(in));
            logval   = ((logval > 0.0) ?
                        (floor(logval) + 1.0) : (ceil(logval)));
        }
        sigfigs  = GNC_HOW_GET_SIGFIGS(how);
        if ((denom = powten (sigfigs - logval)) == POWTEN_OVERFLOW)
            return gnc_numeric_error(GNC_ERROR_OVERFLOW);

        how =  how & ~GNC_HOW_DENOM_SIGFIG & ~GNC_NUMERIC_SIGFIGS_MASK;
    }

    int_part  = (gint64)(floor(fabs(in)));
    frac_part = in - (double)int_part;

    int_part = int_part * denom;
    frac_part = frac_part * (double)denom;

    switch (how & GNC_NUMERIC_RND_MASK)
    {
    case GNC_HOW_RND_FLOOR:
        frac_int = (gint64)floor(frac_part);
        break;

    case GNC_HOW_RND_CEIL:
        frac_int = (gint64)ceil(frac_part);
        break;

    case GNC_HOW_RND_TRUNC:
        frac_int = (gint64)frac_part;
        break;

    case GNC_HOW_RND_ROUND:
    case GNC_HOW_RND_ROUND_HALF_UP:
        frac_int = (gint64)rint(frac_part);
        break;

    case GNC_HOW_RND_NEVER:
        frac_int = (gint64)floor(frac_part);
        if (frac_part != (double) frac_int)
        {
            /* signal an error */
        }
        break;
    }

    out.num   = int_part + frac_int;
    out.denom = denom;
    return out;
}

/* *******************************************************************
 *  gnc_numeric_to_double
 ********************************************************************/

double
gnc_numeric_to_double(gnc_numeric in)
{
    if (in.denom > 0)
    {
        return (double)in.num / (double)in.denom;
    }
    else
    {
        return (double)(in.num * -in.denom);
    }
}

/* *******************************************************************
 *  gnc_numeric_error
 ********************************************************************/

gnc_numeric
gnc_numeric_error(GNCNumericErrorCode error_code)
{
    return gnc_numeric_create(error_code, 0LL);
}



/* *******************************************************************
 *  gnc_numeric text IO
 ********************************************************************/

gchar *
gnc_numeric_to_string(gnc_numeric n)
{
    gchar *result;
    gint64 tmpnum = n.num;
    gint64 tmpdenom = n.denom;

    result = g_strdup_printf("%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT, tmpnum, tmpdenom);

    return result;
}

gchar *
gnc_num_dbg_to_string(gnc_numeric n)
{
    static char buff[1000];
    static char *p = buff;
    gint64 tmpnum = n.num;
    gint64 tmpdenom = n.denom;

    p += 100;
    if (p - buff >= 1000) p = buff;

    sprintf(p, "%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT, tmpnum, tmpdenom);

    return p;
}

gboolean
string_to_gnc_numeric(const gchar* str, gnc_numeric *n)
{
    gint64 tmpnum;
    gint64 tmpdenom;

    if (!str) return FALSE;

    tmpnum = g_ascii_strtoll (str, NULL, 0);
    str = strchr (str, '/');
    if (!str) return FALSE;
    str ++;
    tmpdenom = g_ascii_strtoll (str, NULL, 0);

    n->num = tmpnum;
    n->denom = tmpdenom;
    return TRUE;
}

/* *******************************************************************
 *  GValue handling
 ********************************************************************/
static gpointer
gnc_numeric_boxed_copy_func( gpointer in_gnc_numeric )
{
    gnc_numeric* newvalue;

    newvalue = static_cast<gnc_numeric*>(g_malloc (sizeof (gnc_numeric)));
    memcpy( newvalue, in_gnc_numeric, sizeof( gnc_numeric ) );

    return newvalue;
}

static void
gnc_numeric_boxed_free_func( gpointer in_gnc_numeric )
{
    g_free( in_gnc_numeric );
}

GType
gnc_numeric_get_type( void )
{
    static GType type = 0;

    if ( type == 0 )
    {
        type = g_boxed_type_register_static( "gnc_numeric",
                                             gnc_numeric_boxed_copy_func,
                                             gnc_numeric_boxed_free_func );
    }

    return type;
}

/* *******************************************************************
 *  gnc_numeric misc testing
 ********************************************************************/
#ifdef _GNC_NUMERIC_TEST

static char *
gnc_numeric_print(gnc_numeric in)
{
    char * retval;
    if (gnc_numeric_check(in))
    {
        retval = g_strdup_printf("<ERROR> [%" G_GINT64_FORMAT " / %" G_GINT64_FORMAT "]",
                                 in.num,
                                 in.denom);
    }
    else
    {
        retval = g_strdup_printf("[%" G_GINT64_FORMAT " / %" G_GINT64_FORMAT "]",
                                 in.num,
                                 in.denom);
    }
    return retval;
}

int
main(int argc, char ** argv)
{
    gnc_numeric a = gnc_numeric_create(1, 3);
    gnc_numeric b = gnc_numeric_create(1, 4);
    gnc_numeric c;

    gnc_numeric err;


    printf("multiply (EXACT): %s * %s = %s\n",
           gnc_numeric_print(a), gnc_numeric_print(b),
           gnc_numeric_print(gnc_numeric_mul(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT)));

    printf("multiply (REDUCE): %s * %s = %s\n",
           gnc_numeric_print(a), gnc_numeric_print(b),
           gnc_numeric_print(gnc_numeric_mul(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE)));


    return 0;
}
#endif

const char* gnc_numeric_errorCode_to_string(GNCNumericErrorCode error_code)
{
    switch (error_code)
    {
    case GNC_ERROR_OK:
        return "GNC_ERROR_OK";
    case GNC_ERROR_ARG:
        return "GNC_ERROR_ARG";
    case GNC_ERROR_OVERFLOW:
        return "GNC_ERROR_OVERFLOW";
    case GNC_ERROR_DENOM_DIFF:
        return "GNC_ERROR_DENOM_DIFF";
    case GNC_ERROR_REMAINDER:
        return "GNC_ERROR_REMAINDER";
    default:
        return "<unknown>";
    }
}

/* ======================== END OF FILE =================== */
