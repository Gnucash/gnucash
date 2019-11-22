/********************************************************************\
 * gnc-option.cpp -- Application options system                     *
 * Copyright (C) 2019 John Ralls <jralls@ceridwen.us>               *
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
\********************************************************************/

//#include "options.h"
#include "gnc-option.hpp"
#include <gnc-datetime.hpp>
#include <guid.hpp>
extern "C"
{
#include "gnc-accounting-period.h"
#include "gnc-ui-util.h"
}

bool
GncOptionAccountValue::validate(const GncOptionAccountList& values) const
{
    if (values.empty())
        return false;
    if (get_ui_type() == GncOptionUIType::ACCOUNT_SEL && values.size() != 1)
        return false;
    if (m_allowed.empty())
        return true;
    for(auto account : values) {
        if (std::find(m_allowed.begin(), m_allowed.end(),
                      xaccAccountGetType(account)) == m_allowed.end())
            return false;
    }
    return true;
}

static constexpr int days_in_month[12]{31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

static void
normalize_month(struct tm& now)
{
    if (now.tm_mon < 0)
    {
        now.tm_mon += 12;
        --now.tm_year;
    }
    else if (now.tm_mon > 11)
    {
        now.tm_mon -= 12;
        ++now.tm_year;
    }
}

static void
set_day_and_time(struct tm& now, bool starting)
{
    if (starting)
    {
        now.tm_hour = now.tm_min = now.tm_sec = 0;
        now.tm_mday = 1;
    }
    else
    {
        now.tm_min = now.tm_sec = 59;
        now.tm_hour = 23;
        now.tm_mday = days_in_month[now.tm_mon];
        // Check for Februrary in a leap year
        if (int year = now.tm_year + 1900; now.tm_mon == 1 &&
            year % 4 == 0 && (year % 100 != 0 || year % 400 == 0))
            ++now.tm_mday;
    }
};

time64
GncOptionDateValue::get_value() const
{
    if (m_type == DateType::ABSOLUTE)
        return m_date;
    if (m_period == RelativeDatePeriod::TODAY)
        return static_cast<time64>(GncDateTime());
    if (m_period == RelativeDatePeriod::ACCOUNTING_PERIOD)
        return m_type == DateType::STARTING ?
            gnc_accounting_period_fiscal_start() :
            gnc_accounting_period_fiscal_end();

    struct tm now{static_cast<tm>(GncDateTime())};
    struct tm period{static_cast<tm>(GncDateTime(gnc_accounting_period_fiscal_start()))};

    if (period.tm_mon == now.tm_mon && period.tm_mday == now.tm_mday)
    {
        //No set accounting period, use the calendar year
        period.tm_mon = 0;
        period.tm_mday = 0;
    }

    if (m_period == RelativeDatePeriod::CAL_YEAR ||
        m_period == RelativeDatePeriod::PREV_YEAR)
    {
        if (m_period == RelativeDatePeriod::PREV_YEAR)
            --now.tm_year;
        now.tm_mon = m_type == DateType::STARTING ? 0 : 11;
    }
    else if (m_period == RelativeDatePeriod::PREV_QUARTER ||
             m_period == RelativeDatePeriod::CURRENT_QUARTER)
    {
        auto offset = (now.tm_mon > period.tm_mon ? now.tm_mon - period.tm_mon :
                       period.tm_mon - now.tm_mon) % 3;
        now.tm_mon = now.tm_mon - offset;
        if (m_period == RelativeDatePeriod::PREV_QUARTER)
            now.tm_mon -= 3;
        if (m_type == DateType::ENDING)
            now.tm_mon += 2;
    }
    else if (m_period == RelativeDatePeriod::PREV_MONTH)
        --now.tm_mon;
    normalize_month(now);
    set_day_and_time(now, m_type == DateType::STARTING);
    return static_cast<time64>(GncDateTime(now));
}
static const char* date_type_str[] {"absolute", "relative"};
static const std::array<const char*, 16> date_period_str
{
    "today", "today",
    "start-this-month", "end-this-month",
    "start-prev-month", "end-prev-month",
    "start-current-quarter", "end-current-quarter",
    "start-prev-quarter", "end-prev-quarter",
    "start-cal-year", "end-cal-year",
    "start-prev-year", "end-prev-year",
    "start-prev-fin-year", "end-prev-fin-year"
};


std::ostream&
GncOptionDateValue::out_stream(std::ostream& oss) const noexcept
{
    if (m_type == DateType::ABSOLUTE)
        oss << date_type_str[0] << " . " << m_date;
    else
    {
        int n{ m_type == DateType::STARTING ? 0 : 1};
        n += 2 * static_cast<int>(m_period);
        oss << date_type_str[1] << " . " << date_period_str[n];
    }
    return oss;
}

std::istream&
GncOptionDateValue::in_stream(std::istream& iss)
{
    std::string type_str;
    std::getline(iss, type_str, '.');
    if (type_str == "absolute ")
    {
        time64 time;
        iss >> time;
        set_value(time);
    }
    else if (type_str == "relative ")
    {
        std::string period_str;
        iss >> period_str;
        auto period = std::find(date_period_str.begin(), date_period_str.end(),
                                period_str);
        if (period == date_period_str.end())
        {
            std::string err{"Unknown period string in date option: '"};
            err += period_str;
            err += "'";
            throw std::invalid_argument(err);
        }

        int64_t index = period - date_period_str.begin();
        DateType type = index % 2 ? DateType::ENDING : DateType::STARTING;
        set_value(std::make_pair(type, index / 2));
    }
    else
    {
        std::string err{"Unknown date type string in date option: '"};
        err += type_str;
        err += "'";
        throw std::invalid_argument{err};
    }
    return iss;
}

void
GncOptionDateValue::set_value(DateSetterValue value)
{
    auto [type, val] = value;
    m_type = type;
    if (type == DateType::ABSOLUTE)
    {
        m_period = RelativeDatePeriod::TODAY;
        m_date = static_cast<time64>(val);
        return;
    }

    m_period = static_cast<RelativeDatePeriod>(val);
    m_date = 0;
}


QofInstance*
qof_instance_from_string(const std::string& str, GncOptionUIType type)
{
    auto guid{static_cast<GncGUID>(gnc::GUID::from_string(str))};
    QofIdType qof_type;
    switch(type)
    {
        case GncOptionUIType::CURRENCY:
        case GncOptionUIType::COMMODITY:
            qof_type = "Commodity";
            break;
        case GncOptionUIType::BUDGET:
            qof_type = "Budget";
            break;
        case GncOptionUIType::OWNER:
            qof_type = "gncOwner";
            break;
        case GncOptionUIType::CUSTOMER:
            qof_type = "gncCustomer";
            break;
        case GncOptionUIType::VENDOR:
            qof_type = "gncVendor";
            break;
        case GncOptionUIType::EMPLOYEE:
            qof_type = "gncEmployee";
            break;
        case GncOptionUIType::INVOICE:
            qof_type = "gncInvoice";
            break;
        case GncOptionUIType::TAX_TABLE:
            qof_type = "gncTaxtable";
            break;
        case GncOptionUIType::QUERY:
            qof_type = "gncQuery";
            break;
        case GncOptionUIType::ACCOUNT_LIST:
        case GncOptionUIType::ACCOUNT_SEL:
        default:
            qof_type = "Account";
            break;
    }
    auto book{gnc_get_current_book()};
    auto col{qof_book_get_collection(book, qof_type)};
    return QOF_INSTANCE(qof_collection_lookup_entity(col, &guid));
}

std::string
qof_instance_to_string(const QofInstance* inst)
{
    gnc::GUID guid{*qof_instance_get_guid(inst)};
    return guid.to_string();
}
