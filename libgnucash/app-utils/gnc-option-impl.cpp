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

static const QofLogModule log_module{"gnc.options"};

const std::string GncOptionMultichoiceValue::c_empty_string{""};
const std::string GncOptionMultichoiceValue::c_list_string{"multiple values"};

bool
GncOptionAccountListValue::validate(const GncOptionAccountList& values) const
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

GncOptionAccountList
GncOptionAccountListValue::get_value() const
{
    return !m_value.empty() ? m_value : get_default_value();
}

GncOptionAccountList
GncOptionAccountListValue::get_default_value() const
{
    if (!m_default_value.empty())
        return m_default_value;

    /* If no default has been set and there's an allowed set then find the first
     * account that matches one of the allowed account types.
     */
    GncOptionAccountList retval{};
    if (m_allowed.empty())
        return retval;

    auto root{gnc_get_current_root_account()};
    auto account_list{gnc_account_get_descendants_sorted(root)};
    if (!account_list)
        return retval;

    for (auto node = account_list; node; node = g_list_next (node))
        if (std::find(m_allowed.begin(), m_allowed.end(),
                      xaccAccountGetType(GNC_ACCOUNT(node->data))) != m_allowed.end())
        {
            retval.push_back(GNC_ACCOUNT(node->data));
            break;
        }
    g_list_free(account_list);
    return retval;
}


/**
 * Create a GList of account types to pass to gnc_account_sel_set_acct_filters.
 * gnc_account_sel_set_acct_filters copies the list so the intermediary caller
 * is responsible for freeing the list.
 *
 * @return an allocated GList* or nullptr if the list is empty.
 */
GList*
GncOptionAccountListValue::account_type_list() const noexcept
{
    if (m_allowed.empty())
        return nullptr;
    GList* retval{nullptr};
    for (auto type : m_allowed)
        retval = g_list_prepend(retval, GINT_TO_POINTER(type));
    return g_list_reverse(retval);
}

bool
GncOptionAccountSelValue::validate(const Account* value) const
{
    if (m_allowed.empty() || !value)
        return true;
    if (std::find(m_allowed.begin(), m_allowed.end(),
                  xaccAccountGetType(value)) == m_allowed.end())
            return false;
    return true;
}

const Account*
GncOptionAccountSelValue::get_value() const
{
    return m_value ? m_value : get_default_value();
}

const Account*
GncOptionAccountSelValue::get_default_value() const
{

    if (m_default_value)
        return m_default_value;

    /* If no default has been set and there's an allowed set then find the first
     * account that matches one of the allowed account types.
     */
    if (m_allowed.empty())
        return nullptr;

    const Account* retval{nullptr};
    auto root{gnc_get_current_root_account()};
    auto account_list{gnc_account_get_descendants_sorted(root)};
    if (!account_list)
        return nullptr;

    for (auto node = account_list; node; node = g_list_next (node))
        if (std::find(m_allowed.begin(), m_allowed.end(),
                      xaccAccountGetType(GNC_ACCOUNT(node->data))) != m_allowed.end())
        {
            retval = GNC_ACCOUNT(node->data);
            break;
        }
    g_list_free(account_list);
    return retval;
}


/**
 * Create a GList of account types to pass to gnc_account_sel_set_acct_filters.
 * gnc_account_sel_set_acct_filters copies the list so the intermediary caller
 * is responsible for freeing the list.
 *
 * @return an allocated GList* or nullptr if the list is empty.
 */
GList*
GncOptionAccountSelValue::account_type_list() const noexcept
{
    if (m_allowed.empty())
        return nullptr;
    GList* retval{nullptr};
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
        case GncOptionUIType::JOB:
            qof_type = "gncJob";
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
    QofInstance* retval{nullptr};
    // Commodities are often serialized as Namespace::Mnemonic or just Mnemonic
    if (type == GncOptionUIType::CURRENCY ||
        type == GncOptionUIType::COMMODITY)
    {
        auto book{gnc_get_current_book()};
        auto table = gnc_commodity_table_get_table(book);
        auto sep{str.find(":")};
        if (sep != std::string::npos)
        {
            auto name_space{str.substr(0, sep)};
            auto mnemonic{str.substr(sep + 1, -1)};
            retval = QOF_INSTANCE(gnc_commodity_table_lookup(table,
                                                             name_space.c_str(),
                                                             mnemonic.c_str()));
        }
        if (!retval && type == GncOptionUIType::CURRENCY)
            retval = QOF_INSTANCE(gnc_commodity_table_lookup(table,
                                                             "CURRENCY",
                                                             str.c_str()));
    }

    if (!retval)
    {
        try {
            auto guid{static_cast<GncGUID>(gnc::GUID::from_string(str))};
            retval = qof_instance_from_guid(&guid, type);
        }
        catch (const gnc::guid_syntax_exception& err)
        {
            PWARN("Failed to convert %s to a GUID", str.c_str());
        }
    }
    return retval;
}

std::string
qof_instance_to_string(const QofInstance* inst)
{
    gnc::GUID guid{*qof_instance_get_guid(inst)};
    return guid.to_string();
}

template <typename ValueType> void
GncOptionValue<ValueType>::set_value(ValueType new_value)
{
    m_value = new_value;
}

template <typename ValueType> void
GncOptionValue<ValueType>::set_default_value(ValueType new_value)
{
    m_value = m_default_value = new_value;
}

template <typename ValueType> void
GncOptionValue<ValueType>::reset_default_value()
{
    m_value = m_default_value;
}

template <> void
GncOptionValue<SCM>::set_value(SCM new_value)
{
    if (m_value)
        scm_gc_unprotect_object(m_value);
    m_value = new_value;
    scm_gc_protect_object(m_value);
}

template <> void
GncOptionValue<SCM>::set_default_value(SCM new_value)
{
    if (m_value)
        scm_gc_unprotect_object(m_value);
    if (m_default_value)
        scm_gc_unprotect_object(m_default_value);
    m_value = m_default_value = new_value;
    scm_gc_protect_object(m_value);
    scm_gc_protect_object(m_default_value);
}

template <> void
GncOptionValue<SCM>::reset_default_value()
{
    if (m_value)
        scm_gc_unprotect_object(m_value);
    m_value = m_default_value;
    scm_gc_protect_object(m_value);
}

template GncOptionValue<bool>::GncOptionValue(const GncOptionValue<bool>&);
template GncOptionValue<int>::GncOptionValue(const GncOptionValue<int>&);
template GncOptionValue<int64_t>::GncOptionValue(const GncOptionValue<int64_t>&);
template GncOptionValue<double>::GncOptionValue(const GncOptionValue<double>&);
template GncOptionValue<char*>::GncOptionValue(const GncOptionValue<char*>&);
template GncOptionValue<const char*>::GncOptionValue(const GncOptionValue<const char*>&);
template GncOptionValue<std::string>::GncOptionValue(const GncOptionValue<std::string>&);
template GncOptionValue<const QofInstance*>::GncOptionValue(const GncOptionValue<const QofInstance*>&);
template GncOptionValue<const QofQuery*>::GncOptionValue(const GncOptionValue<const QofQuery*>&);
template GncOptionValue<const GncOwner*>::GncOptionValue(const GncOptionValue<const GncOwner*>&);
template GncOptionValue<RelativeDatePeriod>::GncOptionValue(const GncOptionValue<RelativeDatePeriod>&);
template GncOptionValue<size_t>::GncOptionValue(const GncOptionValue<size_t>&);
template GncOptionValue<GncOptionAccountList>::GncOptionValue(const GncOptionValue<GncOptionAccountList>&);
template GncOptionValue<GncMultichoiceOptionIndexVec>::GncOptionValue(const GncOptionValue<GncMultichoiceOptionIndexVec>&);
template GncOptionValue<SCM>::GncOptionValue(const GncOptionValue<SCM>&);
template void GncOptionValue<bool>::set_value(bool);
template void GncOptionValue<int>::set_value(int);
template void GncOptionValue<int64_t>::set_value(int64_t);
template void GncOptionValue<double>::set_value(double);
template void GncOptionValue<char*>::set_value(char*);
template void GncOptionValue<const char*>::set_value(const char*);
template void GncOptionValue<std::string>::set_value(std::string);
template void GncOptionValue<const QofInstance*>::set_value(const QofInstance*);
template void GncOptionValue<const QofQuery*>::set_value(const QofQuery*);
template void GncOptionValue<const GncOwner*>::set_value(const GncOwner*);
template void GncOptionValue<RelativeDatePeriod>::set_value(RelativeDatePeriod);
template void GncOptionValue<size_t>::set_value(size_t);
template void GncOptionValue<GncOptionAccountList>::set_value(GncOptionAccountList);
template void GncOptionValue<GncMultichoiceOptionIndexVec>::set_value(GncMultichoiceOptionIndexVec);
template void GncOptionValue<bool>::set_default_value(bool);
template void GncOptionValue<int>::set_default_value(int);
template void GncOptionValue<int64_t>::set_default_value(int64_t);
template void GncOptionValue<double>::set_default_value(double);
template void GncOptionValue<char*>::set_default_value(char*);
template void GncOptionValue<const char*>::set_default_value(const char*);
template void GncOptionValue<std::string>::set_default_value(std::string);
template void GncOptionValue<const QofInstance*>::set_default_value(const QofInstance*);
template void GncOptionValue<const QofQuery*>::set_default_value(const QofQuery*);
template void GncOptionValue<const GncOwner*>::set_default_value(const GncOwner*);
template void GncOptionValue<RelativeDatePeriod>::set_default_value(RelativeDatePeriod);
template void GncOptionValue<size_t>::set_default_value(size_t);
template void GncOptionValue<GncOptionAccountList>::set_default_value(GncOptionAccountList);
template void GncOptionValue<GncMultichoiceOptionIndexVec>::set_default_value(GncMultichoiceOptionIndexVec);
template void GncOptionValue<bool>::reset_default_value();
template void GncOptionValue<int>::reset_default_value();
template void GncOptionValue<int64_t>::reset_default_value();
template void GncOptionValue<double>::reset_default_value();
template void GncOptionValue<char*>::reset_default_value();
template void GncOptionValue<const char*>::reset_default_value();
template void GncOptionValue<std::string>::reset_default_value();
template void GncOptionValue<const QofInstance*>::reset_default_value();
template void GncOptionValue<const QofQuery*>::reset_default_value();
template void GncOptionValue<const GncOwner*>::reset_default_value();
template void GncOptionValue<RelativeDatePeriod>::reset_default_value();
template void GncOptionValue<size_t>::reset_default_value();
template void GncOptionValue<GncOptionAccountList>::reset_default_value();
template void GncOptionValue<GncMultichoiceOptionIndexVec>::reset_default_value();
