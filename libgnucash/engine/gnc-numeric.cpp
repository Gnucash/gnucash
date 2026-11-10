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

#include <glib.h>

#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cstdint>
#include <sstream>
#include <ctre.hpp>
#include <boost/locale/encoding_utf.hpp>

#include <config.h>
#include <stdexcept>
#include <stdint.h>
#include "gnc-int128.hpp"
#include "qof.h"

#include "gnc-numeric.hpp"
#include "gnc-rational.hpp"

#include <optional>
#include <charconv>

static QofLogModule log_module = "qof";

static const uint8_t max_leg_digits{18};
static const int64_t pten[] = { 1, 10, 100, 1000, 10000, 100000, 1000000,
                               10000000, 100000000, 1000000000,
                               INT64_C(10000000000), INT64_C(100000000000),
                               INT64_C(1000000000000), INT64_C(10000000000000),
                               INT64_C(100000000000000),
                               INT64_C(1000000000000000),
                               INT64_C(10000000000000000),
                               INT64_C(100000000000000000),
                               INT64_C(1000000000000000000)};
#define POWTEN_OVERFLOW -5

int64_t
powten (unsigned int exp)
{
    if (exp > max_leg_digits)
        exp = max_leg_digits;
    return pten[exp];
}

GncNumeric::GncNumeric(GncRational rr)
{
    /* Can't use isValid here because we want to throw different exceptions. */
    if (rr.num().isNan() || rr.denom().isNan())
        throw std::underflow_error("Operation resulted in NaN.");
    if (rr.num().isOverflow() || rr.denom().isOverflow())
        throw std::overflow_error("Operation overflowed a 128-bit int.");
    if (rr.num().isBig() || rr.denom().isBig())
    {
        GncRational reduced(rr.reduce());
        rr = reduced.round_to_numeric(); // A no-op if it's already small.
    }
    m_num = static_cast<int64_t>(rr.num());
    m_den = static_cast<int64_t>(rr.denom());
}

GncNumeric::GncNumeric(double d) : m_num(0), m_den(1)
{
    static uint64_t max_leg_value{INT64_C(1000000000000000000)};
    if (std::isnan(d) || fabs(d) > max_leg_value)
    {
        std::ostringstream msg;
        msg << "Unable to construct a GncNumeric from " << d << ".\n";
        throw std::invalid_argument(msg.str());
    }
    constexpr auto max_num = static_cast<double>(INT64_MAX);
    auto logval = log10(fabs(d));
    int64_t den;
    uint8_t den_digits;
    if (logval > 0.0)
        den_digits = (max_leg_digits + 1) - static_cast<int>(floor(logval));
    else
        den_digits = max_leg_digits;
    den = powten(den_digits);
    auto num_d = static_cast<double>(den) * d;
    while (fabs(num_d) > max_num && den_digits > 1)
    {
        den = powten(--den_digits);
        num_d = static_cast<double>(den) * d;
    }
    auto num = static_cast<int64_t>(floor(num_d));

    if (num == 0)
        return;
    GncNumeric q(num, den);
    auto r = q.reduce();
    m_num = r.num();
    m_den = r.denom();
}

/* Compile-time concatenation of ctll::fixed_string regex fragments, so the
 * individual patterns below can still be assembled from small, reusable
 * pieces the way the previous boost::regex-based implementation did (which
 * built its patterns by concatenating std::string fragments at run time).
 */
namespace {
template <size_t N, size_t M>
constexpr ctll::fixed_string<N + M>
operator+(const ctll::fixed_string<N>& a, const ctll::fixed_string<M>& b)
{
    char32_t buf[N + M + 1] = {};
    for (size_t i = 0; i < N; ++i)
        buf[i] = a[i];
    for (size_t i = 0; i < M; ++i)
        buf[N + i] = b[i];
    return ctll::fixed_string<N + M>(buf);
}

static std::string
to_std_string(std::u32string_view v)
{
    return boost::locale::conv::utf_to_utf<char>(v.data(), v.data() + v.size());
}
} // anonymous namespace

static std::pair<int64_t, int64_t>
reduce_number_pair(std::pair<GncInt128, GncInt128>num_pair,
                   const std::string& num_str, bool autoround)
{
    auto [n, d] = num_pair;
    if (!autoround && n.isBig()) {
        std::ostringstream errmsg;
        errmsg << "Decimal string " << num_str
               << "can't be represented in a GncNumeric without rounding.";
        throw std::overflow_error(errmsg.str());
    }
    while (n.isBig() && d > 0) {
        n >>= 1;
        d >>= 1;
    }
    if (n.isBig()) // Shouldn't happen, of course
    {
        std::ostringstream errmsg;
        errmsg << "Decimal string " << num_str
               << " can't be represented in a GncNumeric, even after reducing "
            "denom to "
               << d;
        throw std::overflow_error(errmsg.str());
    }
    return std::make_pair(static_cast<int64_t>(n), static_cast<int64_t>(d));
}

static std::pair<GncInt128, int64_t>
numeric_from_decimal_match(const std::string& integer, const std::string& decimal)
{
    auto neg = (!integer.empty() && integer[0] == '-');
    GncInt128 high((neg && integer.length() > 1) || (!neg && !integer.empty())
                   ? stoll(integer)
                   : 0);
    GncInt128 low{ decimal.empty() ? 0 : stoll(decimal)};
    auto exp10 = decimal.length();
    int64_t d = powten(exp10);
    GncInt128 n = high * d + (neg ? -low : low);
    while (exp10 > max_leg_digits) {
        /* If the arg to powten is bigger than max_leg_digits
           it returns 10**max_leg_digits so reduce exp10 by
           that amount */
        n = n / powten(exp10 - max_leg_digits);
        exp10 -= max_leg_digits;
    }
    return std::make_pair(n, d);
}

template <typename Match>
static std::pair<GncInt128, GncInt128>
numeric_from_scientific_match(Match &m)
{
    int exp{m.template get<4>() ? stoi(m.template get<4>().to_string()) : 0};
    auto neg_exp{exp < 0};
    exp = neg_exp ? -exp : exp;
    if (exp >= max_leg_digits)
    {
        std::ostringstream errmsg;
        errmsg << "Exponent " << m.template get<3>().to_string() << " in match "
               << m.template get<0>().to_string()
               << " exceeds range that GnuCash can parse.";
        throw std::overflow_error(errmsg.str());
    }

    GncInt128 num, denom;
    auto mult = powten(exp);

    if (m.template get<1>())
    {
        denom = neg_exp ? mult : 1;
        num = neg_exp ? stoll(m.template get<1>().to_string())
                       : mult * stoll(m.template get<1>().to_string());
    }
    else
    {
        auto [d_num, d_denom] = numeric_from_decimal_match(
            m.template get<2>().to_string(), m.template get<3>().to_string());

        if (neg_exp || d_denom > mult)
        {
            num = d_num;
            denom = neg_exp ? d_denom * mult : d_denom / mult;
        }
        else
        {
            num = d_num * mult / d_denom;
            denom = 1;
        }
    }
    return std::make_pair(num, denom);
}

static std::optional<gnc_numeric>
fast_numeral_rational (const char* str)
{
    if (!str || !str[0])
        return {};

    // because minint64 = -9223372036854775808 and has 20 characters,
    // the maximum strlen to handle is 20+19+1 = 40. 48 is a nice
    // number in 64-bit.
    auto end_ptr{(const char*)memchr (str, '\0', 48)};
    if (!end_ptr)
        return {};

    int64_t num, denom{};
    auto result = std::from_chars (str, end_ptr, num);
    if (result.ec != std::errc())
        return {};

    if (result.ptr == end_ptr)
        return gnc_numeric_create (num, 1);

    if (*result.ptr != '/')
        return {};

    result = std::from_chars (result.ptr + 1, end_ptr, denom);
    if (result.ec != std::errc() || result.ptr != end_ptr || denom <= 0)
        return {};

    return gnc_numeric_create (num, denom);
}

GncNumeric::GncNumeric(const std::string &str, bool autoround) {
    static constexpr ctll::fixed_string begin("^[^\\-.0-9]*");
    static constexpr ctll::fixed_string end("[^0-9]*$");
    static constexpr ctll::fixed_string begin_group("(?:");
    static constexpr ctll::fixed_string end_group(")");
    static constexpr ctll::fixed_string or_op("|");
    static constexpr ctll::fixed_string maybe_sign ("(-?)");
    static constexpr ctll::fixed_string opt_signed_int("(-?[0-9]*)");
    static constexpr ctll::fixed_string opt_signed_separated_int("(-?[0-9]{1,3})");
    static constexpr ctll::fixed_string unsigned_int("([0-9]+)");
    /* [:space:] in the original POSIX/ICU pattern matched any Unicode
     * whitespace codepoint (e.g. the narrow no-break space some European
     * locales use as a thousands separator); \h ("horizontal space") is
     * ctre's closest equivalent, and this whole pattern is matched against
     * a UTF-8 view of str below so that multi-byte separators are handled
     * correctly.
     */
    static constexpr ctll::fixed_string eu_separated_int("(?:[\\h'.]([0-9]{3}))?");
    static constexpr ctll::fixed_string en_separated_int("(?:,([0-9]{3}))?");
    static constexpr ctll::fixed_string eu_decimal_part("(?:,([0-9]+))?");
    static constexpr ctll::fixed_string en_decimal_part("(?:\\.([0-9]+))?");
    static constexpr ctll::fixed_string hex_frag("(0[xX][A-Fa-f0-9]+)");
    static constexpr ctll::fixed_string slash("[ \\t]*/[ \\t]*");
    static constexpr ctll::fixed_string whitespace("[ \\t]+");
    static constexpr ctll::fixed_string dot_or_comma("[.,]");
    static constexpr auto eu_sep_decimal = begin_group + opt_signed_separated_int + eu_separated_int + eu_separated_int + eu_separated_int + eu_separated_int + eu_decimal_part + end_group;
    static constexpr auto en_sep_decimal = begin_group + opt_signed_separated_int + en_separated_int + en_separated_int + en_separated_int + en_separated_int + en_decimal_part + end_group;

    static constexpr auto numeral = begin + opt_signed_int + end;
    static constexpr auto hex = begin + hex_frag + end;
    static constexpr auto numeral_rational = begin + opt_signed_int + slash + unsigned_int + end;
    static constexpr auto integer_and_fraction = begin + maybe_sign + unsigned_int + whitespace + unsigned_int + slash + unsigned_int + end;
    static constexpr auto hex_rational = begin + hex_frag + slash + hex_frag + end;
    static constexpr auto hex_over_num = begin + hex_frag + slash + unsigned_int + end;
    static constexpr auto num_over_hex = begin + opt_signed_int + slash + hex_frag + end;
    static constexpr auto decimal = begin + opt_signed_int + dot_or_comma + unsigned_int + end;
    static constexpr auto sep_decimal = begin + begin_group + eu_sep_decimal + or_op + en_sep_decimal + end_group + end;
    static constexpr ctll::fixed_string scientific("(?:(-?[0-9]+[.,]?)|(-?[0-9]*)[.,]([0-9]+))[Ee](-?[0-9]+)");
    static constexpr ctll::fixed_string has_hex_prefix(".*0[xX]$");
    /* The order of testing the regexes is from the more restrictve to the less
     * restrictive, as less-restrictive ones will match  patterns that would also
     * match the more-restrictive and so invoke the wrong construction.
     */
    if (str.empty())
        throw std::invalid_argument(
            "Can't construct a GncNumeric from an empty string.");
    if (auto res = fast_numeral_rational (str.c_str()))
    {
        m_num = res->num;
        m_den = res->denom;
        return;
    }
    if (auto m = ctre::search<hex_rational>(str))
    {
        GncNumeric n(stoll(m.get<1>().to_string(), nullptr, 16),
                     stoll(m.get<2>().to_string(), nullptr, 16));

        m_num = n.num();
        m_den = n.denom();
        return;
    }
    if (auto m = ctre::search<hex_over_num>(str))
    {
        GncNumeric n(stoll(m.get<1>().to_string(), nullptr, 16), stoll(m.get<2>().to_string()));
        m_num = n.num();
        m_den = n.denom();
        return;
    }
    if (auto m = ctre::search<num_over_hex>(str))
    {
        GncNumeric n(stoll(m.get<1>().to_string()), stoll(m.get<2>().to_string(), nullptr, 16));
        m_num = n.num();
        m_den = n.denom();
        return;
    }
    if (auto m = ctre::search<integer_and_fraction>(str))
    {
        GncNumeric n(stoll(m.get<3>().to_string()), stoll(m.get<4>().to_string()));
        n += stoll(m.get<2>().to_string());
        m_num = m.get<1>().to_view().empty() ? n.num() : -n.num();
        m_den = n.denom();
        return;
    }
    if (auto m = ctre::search<numeral_rational>(str))
    {
        GncNumeric n(stoll(m.get<1>().to_string()), stoll(m.get<2>().to_string()));
        m_num = n.num();
        m_den = n.denom();
        return;
    }
    if (auto m = ctre::search<scientific>(str);
        m && !ctre::match<has_hex_prefix>(std::string(str.begin(), m.get<0>().begin())))
    {
        auto [num, denom] =
            reduce_number_pair(numeric_from_scientific_match(m),
                                   str, autoround);
        m_num = num;
        m_den = denom;
        return;
    }
    if (auto m = ctre::search<decimal>(str))
    {
        std::string integer{m.get<1>() ? m.get<1>().to_string() : ""};
        std::string decimal{m.get<2>() ? m.get<2>().to_string() : ""};
        auto [num, denom] = reduce_number_pair(numeric_from_decimal_match(integer, decimal), str, autoround);
        m_num = num;
        m_den = denom;
        return;
    }
    if (auto u32str = boost::locale::conv::utf_to_utf<char32_t>(str);
        auto m = ctre::search<sep_decimal>(u32str))
    {
        /* There's a bit of magic here because of the complexity of
         * the regex. It supports two formats, one for locales that
         * use space, apostrophe, or dot for thousands separator and
         * comma for decimal separator and the other for locales that
         * use comma for thousands and dot for decimal. For each
         * format there are 5 captures for thousands-groups (allowing
         * up to 10^16 - 1) and one for decimal, hence the two capture
         * groups 1-5 and 7-11 with the decimal being either capture 6 or
         * capture 12. (ctre capture indices are compile-time template
         * arguments, so the original run-time loops are unrolled here.)
         */
        auto append = [](std::string& acc, auto&& cap) {
            if (cap) acc += to_std_string(cap.to_view());
        };
        std::string integer, decimal;
        append(integer, m.get<1>());
        append(integer, m.get<2>());
        append(integer, m.get<3>());
        append(integer, m.get<4>());
        append(integer, m.get<5>());
        append(decimal, m.get<6>());
        if (integer.empty() && decimal.empty())
        {
            append(integer, m.get<7>());
            append(integer, m.get<8>());
            append(integer, m.get<9>());
            append(integer, m.get<10>());
            append(integer, m.get<11>());
            append(decimal, m.get<12>());
        }
        auto [num, denom] =
            reduce_number_pair(numeric_from_decimal_match(integer, decimal),
                               str, autoround);
        m_num = num;
        m_den = denom;
        return;
    }
    if (auto m = ctre::search<hex>(str))
    {
        GncNumeric n(stoll(m.get<1>().to_string(), nullptr, 16), INT64_C(1));
        m_num = n.num();
        m_den = n.denom();
        return;
    }
    if (auto m = ctre::search<numeral>(str))
    {
        GncNumeric n(stoll(m.get<1>().to_string()), INT64_C(1));
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
    auto new_num = old_num * red_conv.num();
    auto rem = new_num % red_conv.denom();
    new_num /= red_conv.denom();
    if (new_num.isBig())
    {
        GncRational rr(new_num, new_denom);
        GncNumeric nn(rr);
        rr = rr.convert<RoundType::truncate>(new_denom);
        return {static_cast<int64_t>(rr.num()), new_denom, 0};
    }
    return {static_cast<int64_t>(new_num),
            static_cast<int64_t>(red_conv.denom()), static_cast<int64_t>(rem)};
}

int64_t
GncNumeric::sigfigs_denom(unsigned figs) const noexcept
{
    if (m_num == 0)
        return 1;

    int64_t num_abs{std::abs(m_num)};
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

std::string
GncNumeric::to_string() const noexcept
{
    std::ostringstream out;
    out << *this;
    return out.str();
}

bool
GncNumeric::is_decimal() const noexcept
{
    for (unsigned pwr = 0; pwr < max_leg_digits && m_den >= pten[pwr]; ++pwr)
    {
        if (m_den == pten[pwr])
            return true;
        if (m_den % pten[pwr])
            return false;
    }
    return false;
}

GncNumeric
GncNumeric::to_decimal(unsigned int max_places) const
{
    if (max_places > max_leg_digits)
        max_places = max_leg_digits;

    if (m_num == 0)
	return GncNumeric();

    if (is_decimal())
    {
        if (m_num == 0 || m_den < powten(max_places))
            return *this; // Nothing to do.
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
    rr = rr.convert<RoundType::never>(powten(max_places)); //May throw
    /* rr might have gotten reduced a bit too much; if so, put it back: */
    unsigned int pwr{1};
    for (; pwr <= max_places && !(rr.denom() % powten(pwr)); ++pwr);
    auto reduce_to = powten(pwr);
    GncInt128 rr_num(rr.num()), rr_den(rr.denom());
    if (rr_den % reduce_to)
    {
        auto factor(reduce_to / rr.denom());
        rr_num *= factor;
        rr_den *= factor;
    }
    while (!rr_num.isZero() && rr_num > 9 && rr_den > 9 && rr_num % 10 == 0)
    {
        rr_num /= 10;
        rr_den /= 10;
    }
    try
    {
        /* Construct from the parts to avoid the GncRational constructor's
         * automatic rounding.
         */
        return {static_cast<int64_t>(rr_num), static_cast<int64_t>(rr_den)};
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
    catch (const std::underflow_error& err)
    {
        std::ostringstream msg;
        msg << "GncNumeric " << *this
            << " underflows when attempting to convert it to decimal.\n";
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
    GncRational an(*this), bn(b);
    return an.cmp(bn);
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

template <typename T, typename I> T
convert(T num, I new_denom, int how)
{
    auto rtype = static_cast<RoundType>(how & GNC_NUMERIC_RND_MASK);
    unsigned int figs = GNC_HOW_GET_SIGFIGS(how);

    auto dtype = static_cast<DenomType>(how & GNC_NUMERIC_DENOM_MASK);
    bool sigfigs = dtype == DenomType::sigfigs;
    if (dtype == DenomType::reduce)
        num = num.reduce();

    switch (rtype)
    {
        case RoundType::floor:
            if (sigfigs)
                return num.template convert_sigfigs<RoundType::floor>(figs);
            else
                return num.template convert<RoundType::floor>(new_denom);
        case RoundType::ceiling:
            if (sigfigs)
                return num.template convert_sigfigs<RoundType::ceiling>(figs);
            else
                return num.template convert<RoundType::ceiling>(new_denom);
        case RoundType::truncate:
            if (sigfigs)
                return num.template convert_sigfigs<RoundType::truncate>(figs);
            else
                return num.template convert<RoundType::truncate>(new_denom);
        case RoundType::promote:
            if (sigfigs)
                return num.template convert_sigfigs<RoundType::promote>(figs);
            else
                return num.template convert<RoundType::promote>(new_denom);
        case RoundType::half_down:
            if (sigfigs)
                return num.template convert_sigfigs<RoundType::half_down>(figs);
            else
                return num.template convert<RoundType::half_down>(new_denom);
        case RoundType::half_up:
            if (sigfigs)
                return num.template convert_sigfigs<RoundType::half_up>(figs);
            else
                return num.template convert<RoundType::half_up>(new_denom);
        case RoundType::bankers:
            if (sigfigs)
                return num.template convert_sigfigs<RoundType::bankers>(figs);
            else
                return num.template convert<RoundType::bankers>(new_denom);
        case RoundType::never:
            if (sigfigs)
                return num.template convert_sigfigs<RoundType::never>(figs);
            else
                return num.template convert<RoundType::never>(new_denom);
        default:
            /* round-truncate just returns the numerator unchanged. The old
             * gnc-numeric convert had no "default" behavior at rounding that
             * had the same result, but we need to make it explicit here to
             * run the rest of the conversion code.
             */
            if (sigfigs)
                return num.template convert_sigfigs<RoundType::truncate>(figs);
            else
                return num.template convert<RoundType::truncate>(new_denom);

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

    GncNumeric an (a), bn (b);

    return an.cmp(bn);
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

static int64_t
denom_lcd(gnc_numeric a, gnc_numeric b, int64_t denom, int how)
{
    if (denom == GNC_DENOM_AUTO &&
        (how & GNC_NUMERIC_DENOM_MASK) == GNC_HOW_DENOM_LCD)
    {
        GncInt128 ad(a.denom), bd(b.denom);
        denom = static_cast<int64_t>(ad.lcm(bd));
    }
    return denom;
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
    try
    {
        denom = denom_lcd(a, b, denom, how);
        if ((how & GNC_NUMERIC_DENOM_MASK) != GNC_HOW_DENOM_EXACT)
        {
            GncNumeric an (a), bn (b);
            GncNumeric sum = an + bn;
            return static_cast<gnc_numeric>(convert(sum, denom, how));
        }
        GncRational ar(a), br(b);
        auto sum = ar + br;
        if (denom == GNC_DENOM_AUTO &&
            (how & GNC_NUMERIC_RND_MASK) != GNC_HOW_RND_NEVER)
            return static_cast<gnc_numeric>(sum.round_to_numeric());
        sum = convert(sum, denom, how);
        if (sum.is_big() || !sum.valid())
            return gnc_numeric_error(GNC_ERROR_OVERFLOW);
        return static_cast<gnc_numeric>(sum);
    }
    catch (const std::overflow_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }
    catch (const std::invalid_argument& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_ARG);
    }
    catch (const std::underflow_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }
    catch (const std::domain_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_REMAINDER);
    }
}

/* *******************************************************************
 *  gnc_numeric_sub
 ********************************************************************/

gnc_numeric
gnc_numeric_sub(gnc_numeric a, gnc_numeric b,
                gint64 denom, gint how)
{
    if (gnc_numeric_check(a) || gnc_numeric_check(b))
    {
        return gnc_numeric_error(GNC_ERROR_ARG);
    }
    try
    {
        denom = denom_lcd(a, b, denom, how);
        if ((how & GNC_NUMERIC_DENOM_MASK) != GNC_HOW_DENOM_EXACT)
        {
            GncNumeric an (a), bn (b);
            auto sum = an - bn;
            return static_cast<gnc_numeric>(convert(sum, denom, how));
        }
        GncRational ar(a), br(b);
        auto sum = ar - br;
        if (denom == GNC_DENOM_AUTO &&
            (how & GNC_NUMERIC_RND_MASK) != GNC_HOW_RND_NEVER)
            return static_cast<gnc_numeric>(sum.round_to_numeric());
        sum = convert(sum, denom, how);
        if (sum.is_big() || !sum.valid())
            return gnc_numeric_error(GNC_ERROR_OVERFLOW);
        return static_cast<gnc_numeric>(sum);
    }
    catch (const std::overflow_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }
    catch (const std::invalid_argument& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_ARG);
    }
    catch (const std::underflow_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }
    catch (const std::domain_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_REMAINDER);
    }
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

    try
    {
        denom = denom_lcd(a, b, denom, how);
        if ((how & GNC_NUMERIC_DENOM_MASK) != GNC_HOW_DENOM_EXACT)
        {
            GncNumeric an (a), bn (b);
            auto prod = an * bn;
            return static_cast<gnc_numeric>(convert(prod, denom, how));
        }
        GncRational ar(a), br(b);
        auto prod = ar * br;
        if (denom == GNC_DENOM_AUTO &&
            (how & GNC_NUMERIC_RND_MASK) != GNC_HOW_RND_NEVER)
            return static_cast<gnc_numeric>(prod.round_to_numeric());
        prod = convert(prod, denom, how);
        if (prod.is_big() || !prod.valid())
            return gnc_numeric_error(GNC_ERROR_OVERFLOW);
        return static_cast<gnc_numeric>(prod);
     }
    catch (const std::overflow_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }
    catch (const std::invalid_argument& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_ARG);
    }
    catch (const std::underflow_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }
    catch (const std::domain_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_REMAINDER);
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
    try
    {
        denom = denom_lcd(a, b, denom, how);
        if ((how & GNC_NUMERIC_DENOM_MASK) != GNC_HOW_DENOM_EXACT)
        {
            GncNumeric an (a), bn (b);
            auto quot = an / bn;
            return static_cast<gnc_numeric>(convert(quot, denom, how));
        }
        GncRational ar(a), br(b);
        auto quot = ar / br;
        if (denom == GNC_DENOM_AUTO &&
            (how & GNC_NUMERIC_RND_MASK) != GNC_HOW_RND_NEVER)
            return static_cast<gnc_numeric>(quot.round_to_numeric());
        quot =  static_cast<gnc_numeric>(convert(quot, denom, how));
        if (quot.is_big() || !quot.valid())
            return gnc_numeric_error(GNC_ERROR_OVERFLOW);
        return static_cast<gnc_numeric>(quot);
    }
    catch (const std::overflow_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }
    catch (const std::invalid_argument& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_ARG);
    }
    catch (const std::underflow_error& err) //Divide by zero
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }
    catch (const std::domain_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_REMAINDER);
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
    if (gnc_numeric_check(in))
        return in;
    try
    {
        return convert(GncNumeric(in), denom, how);
    }
    catch (const std::invalid_argument& err)
    {
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }
    catch (const std::overflow_error& err)
    {
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }
    catch (const std::underflow_error& err)
    {
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }
    catch (const std::domain_error& err)
    {
        return gnc_numeric_error(GNC_ERROR_REMAINDER);
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
    try
    {
        GncNumeric an (in);
        return static_cast<gnc_numeric>(an.reduce());
    }
    catch (const std::overflow_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }
    catch (const std::invalid_argument& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_ARG);
    }
    catch (const std::underflow_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_ARG);
    }
    catch (const std::domain_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_REMAINDER);
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
    int max_places =  max_decimal_places == NULL ? max_leg_digits :
        *max_decimal_places;
    if (a->num == 0) return TRUE;
    try
    {
        GncNumeric an (*a);
        auto bn = an.to_decimal(max_places);
        *a = static_cast<gnc_numeric>(bn);
        return TRUE;
    }
    catch (const std::exception& err)
    {
        PINFO ("%s", err.what());
        return FALSE;
    }
}


gnc_numeric
gnc_numeric_invert(gnc_numeric num)
{
    if (num.num == 0)
        return gnc_numeric_zero();
    try
    {
        return static_cast<gnc_numeric>(GncNumeric(num).inv());
    }
    catch (const std::overflow_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }
    catch (const std::invalid_argument& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_ARG);
    }
    catch (const std::underflow_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_ARG);
    }
    catch (const std::domain_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_REMAINDER);
    }
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
    try
    {
        GncNumeric an(in);
        return convert(an, denom, how);
    }
    catch (const std::overflow_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_OVERFLOW);
    }
    catch (const std::invalid_argument& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_ARG);
    }
    catch (const std::underflow_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_ARG);
    }
    catch (const std::domain_error& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error(GNC_ERROR_REMAINDER);
    }
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
    char *result;
    int64_t tmpnum = n.num;
    int64_t tmpdenom = n.denom;

    result = g_strdup_printf("%" PRId64 "/%" PRId64, tmpnum, tmpdenom);

    return result;
}

gchar *
gnc_num_dbg_to_string(gnc_numeric n)
{
    static char buff[1000];
    static char *p = buff;
    static const size_t size = 50;
    int64_t tmpnum = n.num;
    int64_t tmpdenom = n.denom;

    p += size;
    if ((size_t)(p - buff) > (sizeof(buff) - size))
        p = buff;

    snprintf(p, size, "%" PRId64 "/%" PRId64, tmpnum, tmpdenom);

    return p;
}

gnc_numeric
gnc_numeric_from_string (const gchar* str)
{
    if (!str)
        return gnc_numeric_error (GNC_ERROR_ARG);

    // the default gnc_numeric string format is "num/denom", whereby
    // the denom must be >= 1. this speedily parses it. this also
    // parses "num" as num/1.
    if (auto res = fast_numeral_rational (str))
        return *res;

    try
    {
        return GncNumeric (str);
    }
    catch (const std::exception& err)
    {
        PWARN("%s", err.what());
        return gnc_numeric_error (GNC_ERROR_ARG);
    }
}

/* *******************************************************************
 *  GValue handling
 ********************************************************************/
static gnc_numeric*
gnc_numeric_boxed_copy_func( gnc_numeric *in_gnc_numeric )
{
    if (!in_gnc_numeric)
        return nullptr;

    /* newvalue will be passed to g_free so we must allocate with g_malloc. */
    auto newvalue = static_cast<gnc_numeric*>(g_malloc (sizeof (gnc_numeric)));
    *newvalue = *in_gnc_numeric;

    return newvalue;
}

static void
gnc_numeric_boxed_free_func( gnc_numeric *in_gnc_numeric )
{
    g_free( in_gnc_numeric );
}

G_DEFINE_BOXED_TYPE (gnc_numeric, gnc_numeric, gnc_numeric_boxed_copy_func, gnc_numeric_boxed_free_func)

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


std::ostream&
operator<<(std::ostream& s, GncNumeric n)
{
    using boost::locale::conv::utf_to_utf;
    std::basic_ostringstream<wchar_t> ss;
    ss.imbue(s.getloc());
    ss << n;
    s << utf_to_utf<char>(ss.str());
    return s;
}

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
