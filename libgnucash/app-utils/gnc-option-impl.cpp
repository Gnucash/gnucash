/********************************************************************\
 * gnc-option-impl.cpp -- Application options system                     *
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
#include "gnc-option-impl.hpp"
#include <gnc-datetime.hpp>
#include <guid.hpp>
#include <cassert>

extern "C"
{
#include "gnc-accounting-period.h"
#include "gnc-ui-util.h"
}

const std::string GncOptionMultichoiceValue::c_empty_string{""};
const std::string GncOptionMultichoiceValue::c_list_string{"multiple values"};

bool
GncOptionAccountValue::validate(const GncOptionAccountList& values) const
{
    if (values.empty())
        return true;
    if ((get_ui_type() == GncOptionUIType::ACCOUNT_SEL || !m_multiselect) &&
        values.size() != 1)
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

/**
 * Create a GList of account types to pass to gnc_account_sel_set_acct_filters.
 * gnc_account_sel_set_acct_filters copies the list so the intermediary caller
 * is responsible for freeing the list.
 *
 * @return an allocated GList* or nullptr if the list is empty.
 */
GList*
GncOptionAccountValue::account_type_list() const noexcept
{
    if (m_allowed.empty())
        return nullptr;
    GList* retval;
    for (auto type : m_allowed)
        retval = g_list_prepend(retval, GINT_TO_POINTER(type));
    return g_list_reverse(retval);
}

bool
GncOptionDateValue::validate(RelativeDatePeriod value) {
    if (m_period_set.empty())
        return true; // No restrictions
    if (std::find(m_period_set.begin(), m_period_set.end(),
                  value) != m_period_set.end())
        return true;
    return false;
}

time64
GncOptionDateValue::get_value() const noexcept
{
    if (m_period == RelativeDatePeriod::ABSOLUTE)
        return m_date;
    return gnc_relative_date_to_time64(m_period);
}

time64
GncOptionDateValue::get_default_value() const noexcept
{
    if (m_default_period == RelativeDatePeriod::ABSOLUTE)
        return m_default_date;
    return gnc_relative_date_to_time64(m_default_period);
}

/* Use asserts for pre- and post-conditions to deliberately crash if they're not
 * met as the program design should prevent that from happening.
 */
size_t
GncOptionDateValue::get_period_index() const noexcept
{
    assert (m_period != RelativeDatePeriod::ABSOLUTE);
    assert(!m_period_set.empty());
    auto item{std::find(m_period_set.begin(), m_period_set.end(), m_period)};
    assert(item != m_period_set.end());
    return item - m_period_set.begin();
}

size_t
GncOptionDateValue::get_default_period_index() const noexcept
{
    assert(m_period != RelativeDatePeriod::ABSOLUTE);
    assert(!m_period_set.empty());
    auto item{std::find(m_period_set.begin(), m_period_set.end(),
                        m_default_period)};
    assert (item != m_period_set.end());
    return item - m_period_set.begin();
}

void
GncOptionDateValue::set_value(size_t index) noexcept
{
    assert(!m_period_set.empty());
    assert(index < m_period_set.size());
    m_date = INT64_MAX;
    m_period = m_period_set[index];
}

size_t
GncOptionDateValue::permissible_value_index(const char* key) const noexcept
{
    auto index = std::find_if(m_period_set.begin(), m_period_set.end(),
                              [key](auto period) -> bool {
                                  return strcmp(gnc_relative_date_display_string(period),
                                                key) == 0;
                              });
    return index != m_period_set.end() ? index - m_period_set.begin() : 0;
}

static const char* date_type_str[] {"absolute", "relative"};

std::ostream&
GncOptionDateValue::out_stream(std::ostream& oss) const noexcept
{
    if (m_period == RelativeDatePeriod::ABSOLUTE)
        oss << date_type_str[0] << " . " << m_date;
    else
        oss << date_type_str[1] << " . " <<
            gnc_relative_date_storage_string(m_period);
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
        auto period = gnc_relative_date_from_storage_string(period_str.c_str());
        if (period == RelativeDatePeriod::ABSOLUTE)
        {
            std::string err{"Unknown period string in date option: '"};
            err += period_str;
            err += "'";
            throw std::invalid_argument(err);
        }

        set_value(period);
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
