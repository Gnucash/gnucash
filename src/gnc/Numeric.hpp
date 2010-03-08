/*
 * Numeric.hpp
 * Copyright (C) 2010 Christian Stimming
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
#include "engine/gnc-ui-util.h"
}

#include <QString>
#include <QDateTime>

namespace gnc
{

class Account;
class Split;

inline QString gchar_to_QString(gchar* tmp_string)
{
    QString result = QString::fromUtf8(tmp_string);
    g_free(tmp_string);
    return result;
}

inline QDateTime toQDateTime(const ::Timespec& timespec)
{
    QDateTime result = QDateTime::fromTime_t(timespec.tv_sec);
    result.addMSecs(timespec.tv_nsec / 1000000);
    result.setTimeSpec(Qt::UTC);
    return result;
}
inline ::Timespec toTimespec(const QDateTime& qdt)
{
    ::Timespec result;
    result.tv_sec = qdt.toTime_t();
    result.tv_nsec = qdt.time().msec() * 1000000;
    return result;
}



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

    PrintAmountInfo(const Account& account, bool use_symbol);
    PrintAmountInfo(const Split& split, bool use_symbol);

    static PrintAmountInfo share_places(int decplaces) { return gnc_share_print_info_places(decplaces); }
    static PrintAmountInfo default_share() { return gnc_default_share_print_info(); }
    static PrintAmountInfo default_price() { return gnc_default_price_print_info(); }
    static PrintAmountInfo integral() { return gnc_integral_print_info(); }
};

class Numeric : public ::gnc_numeric
{
public:
    typedef ::gnc_numeric base_class;
    Numeric(gint64 num, gint64 denom)
    {
        base_class::num = num;
        base_class::denom = denom;
    }
    Numeric(const base_class& other) : base_class(other) {}
    static Numeric zero() { return Numeric(0, 1); }
    Numeric(double in, gint64 denom, gint how)
    {
        *this = double_to_gnc_numeric(in, denom, how);
    }
    static bool string_to_numeric(const QString& str, Numeric& n)
    {
        return string_to_gnc_numeric(str.toUtf8(), &n);
    }
    Numeric(GNCNumericErrorCode error_code)
    {
        *this = gnc_numeric_error(error_code);
    }

    gint64 num() const { return base_class::num; }
    gint64 denom() const { return base_class::denom; }
    gdouble to_double() const { return gnc_numeric_to_double(*this); }
    QString to_string() const
    {
        return gchar_to_QString(gnc_numeric_to_string(*this));
    }

    GNCNumericErrorCode check() const { return gnc_numeric_check(*this); }
    gint compare(const Numeric& b) const { return gnc_numeric_compare(*this, b); }
    bool zero_p() const { return gnc_numeric_zero_p(*this); }
    bool negative_p() const { return gnc_numeric_negative_p(*this); }
    bool positive_p() const { return gnc_numeric_positive_p(*this); }
    bool eq(const Numeric& b) const { return gnc_numeric_eq(*this, b); }
    bool equal(const Numeric& b) const { return gnc_numeric_equal(*this, b); }
    bool same(const Numeric& b, gint64 denom, gint how) const { return gnc_numeric_same(*this, b, denom, how); }

    Numeric add(const Numeric& b, gint64 denom, gint how) const { return gnc_numeric_add(*this, b, denom, how); }
    Numeric sub(const Numeric& b, gint64 denom, gint how) const { return gnc_numeric_sub(*this, b, denom, how); }
    Numeric mul(const Numeric& b, gint64 denom, gint how) const { return gnc_numeric_mul(*this, b, denom, how); }
    Numeric div(const Numeric& b, gint64 denom, gint how) const { return gnc_numeric_div(*this, b, denom, how); }
    Numeric neg() const { return gnc_numeric_neg(*this); }
    Numeric abs() const { return gnc_numeric_abs(*this); }

    Numeric add_fixed(const Numeric& b) const { return gnc_numeric_add_fixed(*this, b); }
    Numeric sub_fixed(const Numeric& b) const { return gnc_numeric_sub_fixed(*this, b); }

    Numeric reduce() const { return gnc_numeric_reduce(*this); }

    QString printAmount(const PrintAmountInfo& info);
};

inline bool operator==(const Numeric& a, const Numeric& b)
{
    return a.equal(b);
}

} // END namespace gnc

#endif
