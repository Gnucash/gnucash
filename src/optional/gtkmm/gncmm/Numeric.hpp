/*
 * Numeric.hpp
 * Copyright (C) 2011 Christian Stimming
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifndef GNC_NUMERIC_HPP
#define GNC_NUMERIC_HPP

// gnucash includes
#include "config.h"
extern "C"
{
#include "qof.h"
#include "gnc-date.h"
#include "app-utils/gnc-ui-util.h"
#include "app-utils/gnc-ui-balances.h"
}

#include <glibmm/ustring.h>
#include <glibmm/refptr.h>

#if GLIB_HAVE_DATETIME
// Glib::DateTime is new in glibmm-2.29 but very useful
# include <glibmm/datetime.h>
#endif

namespace gnc
{

class Account;
class Split;

/** Conversion of a newly allocated gchar* to QString, which will
 * correctly g_free() the newly allocated gchar* as well.
 *
 * If a gchar* does not have to be freed again, the standard
 * conversion constructor Glib::ustring(const gchar*) is sufficient
 * (as it expects its input in UTF-8) and we do not need to define an
 * extra function for that.
 */
inline Glib::ustring gchar_to_ustring(gchar* tmp_string)
{
    if (!tmp_string)
        return Glib::ustring();
    Glib::ustring result(tmp_string); // implies source string in UTF-8
    g_free(tmp_string);
    return result;
}

#if GLIB_HAVE_DATETIME
// Glib::DateTime is new in glibmm-2.29 but very useful
inline Glib::DateTime to_gdatetime(const ::Timespec& timespec)
{
    Glib::DateTime result = Glib::DateTime::create_now_utc(timespec.tv_sec);
    result.add_seconds(timespec.tv_nsec * 1e-9);
    return result;
}
inline ::Timespec to_timespec(const Glib::DateTime& gdt)
{
    ::Timespec result;
    result.tv_sec = qdt.to_unix;
    result.tv_nsec = qdt.get_microseconds() * 1000;
    return result;
}
#endif

/** Copies the pointer values from the given GList into the specified output
 * list type, such as std::vector<FooBar*>. */
template<class ResultListType>
ResultListType from_glist(GList* glist)
{
    ResultListType result;
    GList* list = glist;
    while (list)
    {
        result.push_back(reinterpret_cast< typename ResultListType::value_type >(list->data));
        list = g_list_next(list);
    }
    return result;
}

/** Wrapper around a gnucash ::GNCPrintAmountInfo structure with C++
 * methods for easier setter and getter access.
 *
 * Since this class is a derived class of the original gnucash struct,
 * it keeps the data by-value and its member variables will always
 * exist as long as this object exists.
 */
class PrintAmountInfo : public ::GNCPrintAmountInfo
{
public:
    typedef ::GNCPrintAmountInfo base_class;

    PrintAmountInfo(const base_class& other)
        : base_class(other)
    {}
    PrintAmountInfo(bool use_symbol)
        : base_class(gnc_default_print_info(use_symbol))
    {}

    /* If the boolean set to true, then prefix 3 letter ISO 4217
       currency code to the amount. */
    PrintAmountInfo(const Glib::RefPtr<Account> account, bool use_symbol);
    PrintAmountInfo(const Glib::RefPtr<Split> split, bool use_symbol);

    static PrintAmountInfo share_places(int decplaces)
    {
        return gnc_share_print_info_places(decplaces);
    }
    static PrintAmountInfo default_share()
    {
        return gnc_default_share_print_info();
    }
    static PrintAmountInfo default_price()
    {
        return gnc_default_price_print_info();
    }
    static PrintAmountInfo integral()
    {
        return gnc_integral_print_info();
    }
};


/** Wrapper around a gnucash ::gnc_numeric structure with C++ methods
 * for easier setter and getter access.
 *
 * Since this class is a derived class of the original gnucash struct,
 * it keeps the data by-value and its member variables will always
 * exist as long as this object exists.
 */
class Numeric : public ::gnc_numeric
{
public:
    typedef ::gnc_numeric base_class;

    /// Constructor for value zero
    Numeric()
    {
        base_class::num = 0;
        base_class::denom = 1;
    }

    /// Constructor with given nominator and denominator
    Numeric(gint64 num, gint64 denom)
    {
        base_class::num = num;
        base_class::denom = denom;
    }

    /// Copy constructor
    Numeric(const base_class& other) : base_class(other) {}

    /// Constructor for value zero
    static Numeric zero()
    {
        return Numeric(0, 1);
    }

    /// Constructor for value one
    static Numeric one()
    {
        return Numeric(1, 1);
    }

    /// Conversion from a double value, with the given target
    /// denominator and the specified rounding method "how".
    Numeric(double in, gint64 denom, gint how)
    {
        *this = double_to_gnc_numeric(in, denom, how);
    }

    /// Watch out: This conversion never seems to work!
    static bool string_to_numeric(const Glib::ustring& str, Numeric& n)
    {
        return string_to_gnc_numeric(str.c_str(), &n);
    }

    /// Constructor for representing any of the given
    /// GNCNumericErrorCode values
    Numeric(GNCNumericErrorCode error_code)
    {
        *this = gnc_numeric_error(error_code);
    }

    /// Returns the numerator
    gint64 num() const
    {
        return base_class::num;
    }
    /// Returns the denominator
    gint64 denom() const
    {
        return base_class::denom;
    }
    /// Conversion to double
    gdouble to_double() const
    {
        return gnc_numeric_to_double(*this);
    }

    /// Conversion to string, but only as a fractional representation
    /// i.e. "123/456". Use printAmount() for user-visible values
    /// instead.
    Glib::ustring to_string() const
    {
        return gchar_to_ustring(gnc_numeric_to_string(*this));
    }

    GNCNumericErrorCode check() const
    {
        return gnc_numeric_check(*this);
    }
    gint compare(const Numeric& b) const
    {
        return gnc_numeric_compare(*this, b);
    }
    bool zero_p() const
    {
        return gnc_numeric_zero_p(*this);
    }
    bool negative_p() const
    {
        return gnc_numeric_negative_p(*this);
    }
    bool positive_p() const
    {
        return gnc_numeric_positive_p(*this);
    }
    bool eq(const Numeric& b) const
    {
        return gnc_numeric_eq(*this, b);
    }
    bool equal(const Numeric& b) const
    {
        return gnc_numeric_equal(*this, b);
    }
    bool same(const Numeric& b, gint64 denom, gint how) const
    {
        return gnc_numeric_same(*this, b, denom, how);
    }

    Numeric add(const Numeric& b, gint64 denom, gint how) const
    {
        return gnc_numeric_add(*this, b, denom, how);
    }
    Numeric sub(const Numeric& b, gint64 denom, gint how) const
    {
        return gnc_numeric_sub(*this, b, denom, how);
    }
    Numeric mul(const Numeric& b, gint64 denom, gint how) const
    {
        return gnc_numeric_mul(*this, b, denom, how);
    }
    Numeric div(const Numeric& b, gint64 denom, gint how) const
    {
        return gnc_numeric_div(*this, b, denom, how);
    }
    Numeric neg() const
    {
        return gnc_numeric_neg(*this);
    }
    Numeric abs() const
    {
        return gnc_numeric_abs(*this);
    }

    Numeric add_fixed(const Numeric& b) const
    {
        return gnc_numeric_add_fixed(*this, b);
    }
    Numeric sub_fixed(const Numeric& b) const
    {
        return gnc_numeric_sub_fixed(*this, b);
    }

    Numeric reduce() const
    {
        return gnc_numeric_reduce(*this);
    }

    Glib::ustring printAmount(const PrintAmountInfo& info) const;

    /** Parses the given string by the expression parser. On success,
     * the value is written into this object and an empty string is
     * returned. On error, this object is unchanged and a string with
     * the error message is returned. */
    Glib::ustring parse(const Glib::ustring& str);
};

inline bool operator==(const Numeric& a, const Numeric& b)
{
    return a.equal(b);
}

inline bool operator!=(const Numeric& a, const Numeric& b)
{
    return !(a == b);
}

/// Returns the negative value of a
inline Numeric operator-(const Numeric& a)
{
    return a.neg();
}

/// Returns a + b
inline Numeric operator+(const Numeric& a, const Numeric& b)
{
    return a.add_fixed(b);
}

/// Returns a - b
inline Numeric operator-(const Numeric& a, const Numeric& b)
{
    return a.sub_fixed(b);
}

} // END namespace gnc


#endif
