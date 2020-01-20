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
    if (m_period == RelativeDatePeriod::ABSOLUTE)
        return m_date;
    if (m_period == RelativeDatePeriod::TODAY)
        return static_cast<time64>(GncDateTime());
    if (m_period == RelativeDatePeriod::START_ACCOUNTING_PERIOD)
        return gnc_accounting_period_fiscal_start();
    if (m_period == RelativeDatePeriod::END_ACCOUNTING_PERIOD)
        return gnc_accounting_period_fiscal_end();

    GncDateTime now_t;
    if (m_period == RelativeDatePeriod::TODAY)
        return static_cast<time64>(now_t);
    struct tm now{static_cast<tm>(now_t)};
    struct tm period{static_cast<tm>(GncDateTime(gnc_accounting_period_fiscal_start()))};
    bool starting =  m_period == RelativeDatePeriod::START_PREV_MONTH ||
        m_period == RelativeDatePeriod::START_THIS_MONTH ||
        m_period == RelativeDatePeriod::START_CAL_YEAR ||
        m_period == RelativeDatePeriod::START_PREV_YEAR ||
        m_period == RelativeDatePeriod::START_CURRENT_QUARTER ||
        m_period == RelativeDatePeriod::START_PREV_QUARTER;

    bool prev = m_period == RelativeDatePeriod::START_PREV_YEAR ||
        m_period == RelativeDatePeriod::END_PREV_YEAR ||
        m_period == RelativeDatePeriod::START_PREV_QUARTER ||
        m_period == RelativeDatePeriod::END_PREV_QUARTER;

    if (period.tm_mon == now.tm_mon && period.tm_mday == now.tm_mday)
    {
        //No set accounting period, use the calendar year
        period.tm_mon = 0;
        period.tm_mday = 0;
    }

    if (m_period == RelativeDatePeriod::START_CAL_YEAR ||
        m_period == RelativeDatePeriod::END_CAL_YEAR ||
        m_period == RelativeDatePeriod::START_PREV_YEAR ||
        m_period == RelativeDatePeriod::END_PREV_YEAR)
    {

        if (prev)
            --now.tm_year;
        now.tm_mon = starting ? 0 : 11;
    }
    else if (m_period == RelativeDatePeriod::START_PREV_QUARTER ||
             m_period == RelativeDatePeriod::END_PREV_QUARTER ||
             m_period == RelativeDatePeriod::START_CURRENT_QUARTER ||
             m_period == RelativeDatePeriod::END_CURRENT_QUARTER)
    {
        auto offset = (now.tm_mon > period.tm_mon ? now.tm_mon - period.tm_mon :
                       period.tm_mon - now.tm_mon) % 3;
        now.tm_mon = now.tm_mon - offset;
        if (prev)
            now.tm_mon -= 3;
        if (!starting)
            now.tm_mon += 2;
    }
    else if (m_period == RelativeDatePeriod::START_PREV_MONTH ||
             m_period == RelativeDatePeriod::END_PREV_MONTH)
        --now.tm_mon;
    normalize_month(now);
    set_day_and_time(now, starting);
    return static_cast<time64>(GncDateTime(now));
}
static const char* date_type_str[] {"absolute", "relative"};
static const std::array<const char*, 15> date_period_str
{
    "today",
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
    if (m_period == RelativeDatePeriod::ABSOLUTE)
        oss << date_type_str[0] << " . " << m_date;
    else
        oss << date_type_str[1] << " . " <<
            date_period_str[static_cast<int>(m_period)];
    return oss;
}

std::istream&
GncOptionDateValue::in_stream(std::istream& iss)
{
    char type_str[10]; //The length of both "absolute" and "relative" plus 1.
    iss.getline(type_str, sizeof(type_str), '.');
    if(!iss)
        throw std::invalid_argument("Date Type separator missing");
    /* strcmp is safe, istream::getline null terminates the buffer. */
    if (strcmp(type_str, "absolute ") == 0)
    {
        time64 time;
        iss >> time;
        set_value(time);
        if (iss.get() != ')')
            iss.unget();
    }
    else if (strcmp(type_str, "relative ") == 0)
    {
        std::string period_str;
        iss >> period_str;
        if (period_str.back() == ')')
            period_str.pop_back();
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
        set_value(static_cast<RelativeDatePeriod>(index));
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

QofInstance*
qof_instance_from_guid(GncGUID* guid, GncOptionUIType type)
{
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
    return QOF_INSTANCE(qof_collection_lookup_entity(col, guid));
}

QofInstance*
qof_instance_from_string(const std::string& str, GncOptionUIType type)
{
    if (type == GncOptionUIType::CURRENCY ||
        type == GncOptionUIType::COMMODITY)
    {
        auto book{gnc_get_current_book()};
        auto sep{str.find(":")};
        auto name_space{str.substr(0, sep)};
        auto mnemonic{str.substr(sep + 1, -1)};
        auto table = gnc_commodity_table_get_table(book);
        return QOF_INSTANCE(gnc_commodity_table_lookup(table,
                                                       name_space.c_str(),
                                                       mnemonic.c_str()));
    }
    auto guid{static_cast<GncGUID>(gnc::GUID::from_string(str))};
    return qof_instance_from_guid(&guid, type);
}

std::string
qof_instance_to_string(const QofInstance* inst)
{
    gnc::GUID guid{*qof_instance_get_guid(inst)};
    return guid.to_string();
}
