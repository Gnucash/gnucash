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
#include <sstream>
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
    std::string retval;
    if (GNC_IS_COMMODITY(inst))
    {
        auto commodity{GNC_COMMODITY(inst)};
        if (!gnc_commodity_is_currency(commodity))
        {
            auto name_space{gnc_commodity_get_namespace(GNC_COMMODITY(inst))};
            if (name_space && *name_space != '\0')
            {
                retval = name_space;
                retval += ":";
            }
        }
        retval += gnc_commodity_get_mnemonic(GNC_COMMODITY(inst));
        return retval;
    }
    else
    {
        gnc::GUID guid{*qof_instance_get_guid(inst)};
        retval = guid.to_string();
    }
    return retval;
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

/* Missing on purpose: QofQuery because for current usage it's serialized with
 * gnc_query2scm. The future is to replace QofQuery with SQL queries so there's
 * not much point to spending the time to create a std::string serialization for
 * them.
 */
template <typename ValueType> std::string
GncOptionValue<ValueType>::serialize() const noexcept
{
    static const std::string no_value{"No Value"};
    if constexpr(std::is_same_v<ValueType, const QofInstance*>)
        return m_value ? qof_instance_to_string(m_value) : no_value;
    if constexpr(std::is_same_v<ValueType, const GncOwner*>)
    {
        if (!m_value)
            return no_value;
        auto guid{qof_instance_to_string(qofOwnerGetOwner(m_value))};
        auto type{qofOwnerGetType(m_value)};
        std::ostringstream ostr{};
        ostr << type << " " << guid;
        return ostr.str();
    }
    else if constexpr(is_same_decayed_v<ValueType, std::string>)
        return m_value;
    else if constexpr(is_same_decayed_v<ValueType, bool>)
        return m_value ? "True" : "False";
    else if constexpr(std::is_arithmetic_v<ValueType>)
        return std::to_string(m_value);
    else
        return "Serialization not implemented";
}

template <typename ValueType> bool
GncOptionValue<ValueType>::deserialize(const std::string& str) noexcept
{
    if constexpr(std::is_same_v<ValueType, const QofInstance*>)
        set_value(qof_instance_from_string(str, get_ui_type()));
    if constexpr(std::is_same_v<ValueType, const GncOwner*>)
    {
        std::istringstream istr{str};
        std::string type, guid;
        istr >> type >> guid;
        auto inst{qof_instance_from_string(guid, get_ui_type())};
        qofOwnerSetEntity(const_cast<GncOwner*>(m_value), inst);
    }
    else if constexpr(is_same_decayed_v<ValueType, std::string>)
        set_value(str);
    else if constexpr(is_same_decayed_v<ValueType, bool>)
        set_value(str == "True");
    else if constexpr(is_same_decayed_v<ValueType, int>)
        set_value(stoi(str));
    else if constexpr(is_same_decayed_v<ValueType, int64_t>)
        set_value(stoll(str));
    else if constexpr(is_same_decayed_v<ValueType, double>)
        set_value(stod(str));
    else
        return false;
    return true;
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

template <typename ValueType> std::string
GncOptionValidatedValue<ValueType>::serialize() const noexcept
{
    static const std::string no_value{"No Value"};
    if constexpr(std::is_same_v<ValueType, const QofInstance*>)
        return m_value ? qof_instance_to_string(m_value) : no_value;
    else if constexpr(is_same_decayed_v<ValueType, std::string>)
        return m_value;
    else if constexpr(is_same_decayed_v<ValueType, bool>)
        return m_value ? "True" : "False";
    else if constexpr(std::is_arithmetic_v<ValueType>)
        return std::to_string(m_value);
    else
        return "Invalid Value Type";
}

template <typename ValueType> bool
GncOptionValidatedValue<ValueType>::deserialize(const std::string& str) noexcept
{
    if constexpr(std::is_same_v<ValueType, const QofInstance*>)
        set_value(qof_instance_from_string(str, get_ui_type()));
    else if constexpr(is_same_decayed_v<ValueType, std::string>)
        set_value(str);
    else if constexpr(is_same_decayed_v<ValueType, bool>)
        set_value(str == "True");
    else if constexpr(is_same_decayed_v<ValueType, int>)
        set_value(stoi(str));
    else if constexpr(is_same_decayed_v<ValueType, int64_t>)
        set_value(stoll(str));
    else if constexpr(is_same_decayed_v<ValueType, double>)
        set_value(stod(str));
    else
        return false;
    return true;
}

std::string
GncOptionAccountListValue::serialize() const noexcept
{
    static const std::string no_value{"No Value"};
    std::string retval;
    bool first = true;
    if (m_value.empty())
        return no_value;
    for (auto val : m_value)
    {
        if (!first)
            retval += " ";
        first = false;
        retval += qof_instance_to_string(QOF_INSTANCE(val));
    }
    return retval;
}

bool
GncOptionAccountListValue::deserialize(const std::string& str) noexcept
{
    if (str.empty() || str.size() < GUID_ENCODING_LENGTH)
        return false;
    m_value.clear();
    m_value.reserve(str.size() / GUID_ENCODING_LENGTH);
    bool first = true;
    size_t pos{};
    while (pos + GUID_ENCODING_LENGTH < str.size())
    {
        if (!first)
            ++pos;
        first = false;
        auto ptr = qof_instance_from_string(str.substr(pos, pos + GUID_ENCODING_LENGTH), get_ui_type());
        m_value.push_back(reinterpret_cast<Account*>(ptr));
        pos += GUID_ENCODING_LENGTH;
    }
    return true;
}

std::string
GncOptionAccountSelValue::serialize() const noexcept
{
    static const std::string no_value{"No Value"};
    return m_value ?qof_instance_to_string(QOF_INSTANCE(m_value)) : no_value;
}

bool
GncOptionAccountSelValue::deserialize(const std::string& str) noexcept
{
    set_value(reinterpret_cast<Account*>(qof_instance_from_string(str, get_ui_type())));
    return true;
}

std::string
GncOptionMultichoiceValue::serialize() const noexcept
{
    static const std::string no_value{"No Value"};
    std::string retval;
    bool first = true;
    if (m_value.empty())
        return no_value;
    for (auto index : m_value)
    {
        if (!first)
            retval += " ";
        first = false;
        retval += std::get<0>(m_choices[index]);
    }
    return retval;
}

bool
GncOptionMultichoiceValue::deserialize(const std::string& str) noexcept
{
    static const auto size_t_max = std::numeric_limits<std::size_t>::max();
    if (str.empty())

        return false;
    size_t pos{};
    while (pos < str.size())
    {
        auto endpos{str.find(' ', pos)};
        if (endpos == std::string::npos)
            endpos = str.size();
        //need a null-terminated char* to pass to permissible_value_index
        auto index{permissible_value_index(str.substr(pos, endpos).c_str())};
        if (index == size_t_max)
            return false;
        m_value.push_back(index);
        pos = endpos + 1;
    }
    return true;
}

template <typename ValueType> std::string
GncOptionRangeValue<ValueType>::serialize() const noexcept
{
    if constexpr (std::is_arithmetic_v<ValueType>)
        return std::to_string(m_value);
    return "";
}

template <typename ValueType> bool
GncOptionRangeValue<ValueType>::deserialize(const std::string& str) noexcept
{
    if constexpr(is_same_decayed_v<ValueType, int>)
        set_value(stoi(str));
    else if constexpr(is_same_decayed_v<ValueType, double>)
        set_value(stod(str));
    return true;
}

std::string
GncOptionDateValue::serialize() const noexcept
{
    std::string retval{"("};
    if (m_period == RelativeDatePeriod::ABSOLUTE)
    {
        retval += date_type_str[0];
        retval += " . ";
        retval += std::to_string(m_date);
    }
    else
    {
        retval += date_type_str[1];
        retval +=  " . ";
        retval += gnc_relative_date_storage_string(m_period);
    }
    retval += ")";
    return retval;
}

bool
GncOptionDateValue::deserialize(const std::string& str) noexcept
{
 //The length of both "absolute" and "relative".
    static constexpr size_t date_type_len{9};
    // date_type_len plus the length of " . ".
    static constexpr size_t date_value_pos{12};
    auto type_str{str.substr(0, date_type_len)};
    auto period_str{str.substr(date_value_pos)};
    if (type_str == "absolute")
    {
        // Need a cast to disambiguate from time64.
        set_value(static_cast<size_t>(std::stoll(period_str)));
        return true;
    }
    else if (type_str == "relative ")
    {
        auto period = gnc_relative_date_from_storage_string(period_str.c_str());
        if (period == RelativeDatePeriod::ABSOLUTE)
        {
            PWARN("Unknown period string in date option: '%s'",
                  period_str.c_str());
            return false;
        }

        set_value(period);
        return true;
    }
    else
    {
        PWARN("Unknown date type string in date option: '%s'",
              type_str.c_str());
        return false;
    }
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
template std::string GncOptionValue<bool>::serialize() const noexcept;
template std::string GncOptionValue<int>::serialize() const noexcept;
template std::string GncOptionValue<int64_t>::serialize() const noexcept;
template std::string GncOptionValue<double>::serialize() const noexcept;
template std::string GncOptionValue<char*>::serialize() const noexcept;
template std::string GncOptionValue<const char*>::serialize() const noexcept;
template std::string GncOptionValue<std::string>::serialize() const noexcept;
template std::string GncOptionValue<const QofInstance*>::serialize() const noexcept;
template std::string GncOptionValue<const QofQuery*>::serialize() const noexcept;
template std::string GncOptionValue<const GncOwner*>::serialize() const noexcept;
template std::string GncOptionValue<SCM>::serialize() const noexcept;
template std::string GncOptionValidatedValue<bool>::serialize() const noexcept;
template std::string GncOptionValidatedValue<int>::serialize() const noexcept;
template std::string GncOptionValidatedValue<int64_t>::serialize() const noexcept;
template std::string GncOptionValidatedValue<double>::serialize() const noexcept;
template std::string GncOptionValidatedValue<char*>::serialize() const noexcept;
template std::string GncOptionValidatedValue<const char*>::serialize() const noexcept;
template std::string GncOptionValidatedValue<std::string>::serialize() const noexcept;
template std::string GncOptionValidatedValue<const QofInstance*>::serialize() const noexcept;
template std::string GncOptionValidatedValue<const QofQuery*>::serialize() const noexcept;
template std::string GncOptionValidatedValue<const GncOwner*>::serialize() const noexcept;
template std::string GncOptionRangeValue<int>::serialize() const noexcept;
template std::string GncOptionRangeValue<double>::serialize() const noexcept;
template bool GncOptionValue<bool>::deserialize(const std::string&) noexcept;
template bool GncOptionValue<int>::deserialize(const std::string&) noexcept;
template bool GncOptionValue<int64_t>::deserialize(const std::string&) noexcept;
template bool GncOptionValue<double>::deserialize(const std::string&) noexcept;
template bool GncOptionValue<char*>::deserialize(const std::string&) noexcept;
template bool GncOptionValue<const char*>::deserialize(const std::string&) noexcept;
template bool GncOptionValue<std::string>::deserialize(const std::string&) noexcept;
template bool GncOptionValue<const QofInstance*>::deserialize(const std::string&) noexcept;
template bool GncOptionValue<const QofQuery*>::deserialize(const std::string&) noexcept;
template bool GncOptionValue<const GncOwner*>::deserialize(const std::string&) noexcept;
template bool GncOptionValue<SCM>::deserialize(const std::string&) noexcept;
template bool GncOptionValidatedValue<bool>::deserialize(const std::string&) noexcept;
template bool GncOptionValidatedValue<int>::deserialize(const std::string&) noexcept;
template bool GncOptionValidatedValue<int64_t>::deserialize(const std::string&) noexcept;
template bool GncOptionValidatedValue<double>::deserialize(const std::string&) noexcept;
template bool GncOptionValidatedValue<char*>::deserialize(const std::string&) noexcept;
template bool GncOptionValidatedValue<const char*>::deserialize(const std::string&) noexcept;
template bool GncOptionValidatedValue<std::string>::deserialize(const std::string&) noexcept;
template bool GncOptionValidatedValue<const QofInstance*>::deserialize(const std::string&) noexcept;
template bool GncOptionValidatedValue<const QofQuery*>::deserialize(const std::string&) noexcept;
template bool GncOptionValidatedValue<const GncOwner*>::deserialize(const std::string&) noexcept;
template bool GncOptionRangeValue<int>::deserialize(const std::string&) noexcept;
template bool GncOptionRangeValue<double>::deserialize(const std::string&) noexcept;
