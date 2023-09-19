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

#include "gnc-option-impl.hpp"
#include "gnc-datetime.hpp"
#include "gnc-option-uitype.hpp"
#include "guid.hpp"
#include <cassert>
#include <sstream>
#include <numeric>

#include "gnc-accounting-period.h"
#include "gnc-session.h"
#include "gncOwner.h"

static const QofLogModule log_module{"gnc.options"};

const std::string GncOptionMultichoiceValue::c_empty_string{""};
const std::string GncOptionMultichoiceValue::c_list_string{"multiple values"};

static inline GncOwnerType
ui_type_to_owner_type(GncOptionUIType ui_type)
{
    if (ui_type == GncOptionUIType::CUSTOMER)
        return GNC_OWNER_CUSTOMER;
    if (ui_type == GncOptionUIType::VENDOR)
        return GNC_OWNER_VENDOR;
    if (ui_type == GncOptionUIType::EMPLOYEE)
        return GNC_OWNER_EMPLOYEE;
    if (ui_type == GncOptionUIType::JOB)
        return GNC_OWNER_JOB;
    return GNC_OWNER_NONE;
}

static GncOwner*
make_owner_ptr(const GncOwner* owner)
{
    if (!owner)
        return nullptr;
    auto rv{gncOwnerNew()};
    gncOwnerCopy(owner, rv);
    return rv;
}

GncOptionGncOwnerValue::GncOptionGncOwnerValue(
    const char* section, const char* name,
    const char* key, const char* doc_string,
    const GncOwner* value, GncOptionUIType ui_type) :
    OptionClassifier{section, name, key, doc_string},
    m_ui_type(ui_type), m_value{make_owner_ptr(value)},
    m_default_value{make_owner_ptr(value)} {}

GncOptionGncOwnerValue::GncOptionGncOwnerValue(const GncOptionGncOwnerValue& from) :
    OptionClassifier{from.m_section, from.m_name, from.m_sort_tag,
                     from.m_doc_string},
    m_ui_type(from.get_ui_type()), m_value{make_owner_ptr(from.get_value())},
    m_default_value{make_owner_ptr(from.get_default_value())} {}

void
GncOptionGncOwnerValue::set_value(const GncOwner* new_value)
{
    m_value.reset(make_owner_ptr(new_value));
    m_dirty = true;
}

void
GncOptionGncOwnerValue::set_default_value(const GncOwner *new_value)
{
    m_value.reset(make_owner_ptr(new_value));
    m_default_value.reset(make_owner_ptr(new_value));
}

const GncOwner*
GncOptionGncOwnerValue::get_value() const
{
    return m_value.get();
}

const GncOwner*
GncOptionGncOwnerValue::get_default_value() const
{
    return m_default_value.get();
}

void
GncOptionGncOwnerValue::reset_default_value()
{
    gncOwnerCopy(m_default_value.get(), m_value.get());
}

bool
GncOptionGncOwnerValue::is_changed() const noexcept
{
    return gncOwnerEqual(m_value.get(), m_default_value.get());
}

bool
GncOptionGncOwnerValue::deserialize(const std::string& str) noexcept
{
    try {
        auto guid{static_cast<GncGUID>(gnc::GUID::from_string(str))};
        auto inst = qof_instance_from_guid(&guid, m_ui_type);
        if (inst)
        {
            GncOwner owner{};
            owner.type = ui_type_to_owner_type(m_ui_type);
            owner.owner.undefined = inst;
            set_default_value(&owner);
            return true;
        }
    }
    catch (const gnc::guid_syntax_exception& err)
    {
        PWARN("Failed to convert %s to a GUID", str.c_str());
    }
    return false;
}

std::string
GncOptionGncOwnerValue::serialize() const noexcept
{

    auto owner{m_value.get()};
    gnc::GUID guid{*qof_instance_get_guid(static_cast<QofInstance*>(owner->owner.undefined))};
    std::string retval{guid.to_string()};

    return retval;
}

static GncItem
make_gnc_item(const QofInstance* inst)
{
    if (!inst)
        return std::make_pair<QofIdTypeConst, GncGUID>("", guid_new_return());
    auto type{qof_collection_get_type(qof_instance_get_collection(inst))};
    auto guid{qof_instance_get_guid(inst)};
    return std::make_pair(std::move(type), std::move(*const_cast<GncGUID*>(guid)));
}

static inline QofBook*
get_current_book(void)
{
    return qof_session_get_book(gnc_get_current_session());
}

static inline Account*
get_current_root_account(void)
{
    return gnc_book_get_root_account(get_current_book());
}

static const QofInstance*
qof_instance_from_gnc_item(const GncItem& item)
{
    auto [type, guid] = item;
    auto book{get_current_book()};
    auto coll{qof_book_get_collection(book, type)};
    return static_cast<QofInstance*>(qof_collection_lookup_entity(coll, &guid));
}

GncOptionQofInstanceValue::GncOptionQofInstanceValue(
    const char* section, const char* name,
    const char* key, const char* doc_string,
    const QofInstance* value, GncOptionUIType ui_type) :
    OptionClassifier{section, name, key, doc_string},
    m_ui_type(ui_type), m_value{},
    m_default_value{} {
    m_value = make_gnc_item(value);
    m_default_value = make_gnc_item(value);
}

GncOptionQofInstanceValue::GncOptionQofInstanceValue(const GncOptionQofInstanceValue& from) :
    OptionClassifier{from.m_section, from.m_name, from.m_sort_tag,
                     from.m_doc_string},
    m_ui_type(from.get_ui_type()), m_value{from.get_item()},
    m_default_value{from.get_default_item()}
{
}
void
GncOptionQofInstanceValue::set_value(const QofInstance* new_value)
{
    m_value = make_gnc_item(new_value);
    m_dirty = true;
}

void
GncOptionQofInstanceValue::set_default_value(const QofInstance *new_value)
{
    m_value = m_default_value = make_gnc_item(new_value);

}

const QofInstance*
GncOptionQofInstanceValue::get_value() const
{
    return qof_instance_from_gnc_item(m_value);
}

const QofInstance*
GncOptionQofInstanceValue::get_default_value() const
{
    return qof_instance_from_gnc_item(m_default_value);
}

void
GncOptionQofInstanceValue::reset_default_value()
{
    m_value = m_default_value;
}

bool
GncOptionQofInstanceValue::is_changed() const noexcept
{
    return m_value != m_default_value;
}

bool
GncOptionQofInstanceValue::deserialize(const std::string& str) noexcept
{
    QofInstance* inst{};
    // Commodities are often serialized as Namespace::Mnemonic or just Mnemonic
    try {
        auto guid{static_cast<GncGUID>(gnc::GUID::from_string(str))};
        inst = qof_instance_from_guid(&guid, m_ui_type);
        if (inst)
        {
            m_value = make_gnc_item(inst);
            return true;
        }
    }
    catch (const gnc::guid_syntax_exception& err)
    {
        PWARN("Failed to convert %s to a GUID", str.c_str());
    }
    return false;
}

std::string
GncOptionQofInstanceValue::serialize() const noexcept
{
    auto inst{get_value()};
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
        gnc::GUID guid{m_value.second};
        retval = guid.to_string();
    }
    return retval;
}

static gnc_commodity*
gnc_commodity_from_namespace_and_mnemonic(std::string_view name_space,
                                          std::string_view mnemonic)
{
    auto book{get_current_book()};
    auto table = gnc_commodity_table_get_table(book);
    return gnc_commodity_table_lookup(table, name_space.data(),
                                      mnemonic.data());
}

gnc_commodity*
GncOptionCommodityValue::get_value() const
{
    return gnc_commodity_from_namespace_and_mnemonic(m_namespace, m_mnemonic);
}

gnc_commodity*
GncOptionCommodityValue::get_default_value() const
{
    return gnc_commodity_from_namespace_and_mnemonic(m_default_namespace,
                                                     m_default_mnemonic);
}

void
GncOptionCommodityValue::set_value(gnc_commodity* value)
{
    if (!validate(value))
        throw std::invalid_argument("Value not a currency when required or not a commodity. Value not set.");
    m_mnemonic = gnc_commodity_get_mnemonic(value);
    m_namespace = gnc_commodity_get_namespace(value);
    m_dirty = true;
}

void
GncOptionCommodityValue::set_default_value(gnc_commodity* value)
{
    if (!validate(value))
        throw std::invalid_argument("Value not a currency when required or not a commodity. Value not set.");
    m_mnemonic = m_default_mnemonic = gnc_commodity_get_mnemonic(value);
    m_namespace = m_default_namespace = gnc_commodity_get_namespace(value);
}

void
GncOptionCommodityValue::reset_default_value()
{
    m_mnemonic = m_default_mnemonic;
    m_namespace = m_default_namespace;
}

bool
GncOptionCommodityValue::is_changed() const noexcept
{
    return m_namespace != m_default_namespace || m_mnemonic != m_default_mnemonic;
}

bool
GncOptionCommodityValue::validate(gnc_commodity* comm) const noexcept
{
    if (!GNC_IS_COMMODITY(comm))
        return false;
    if (m_is_currency && !gnc_commodity_is_currency(comm))
        return false;
    return true;
}

std::string
GncOptionCommodityValue::serialize() const noexcept
{
    if (m_is_currency)
        return m_mnemonic;
    else
        return m_namespace + ":" + m_mnemonic;
}

bool
GncOptionCommodityValue::deserialize(const std::string& str) noexcept
{
   auto sep{str.find(":")};
    gnc_commodity* comm{};
    std::string mnemonic, name_space;
    if (sep != std::string::npos)
    {
        name_space = str.substr(0, sep);
        mnemonic = str.substr(sep + 1, -1);
    }
    else
    {
        name_space = "CURRENCY";
        mnemonic = str;
    }
    comm = gnc_commodity_from_namespace_and_mnemonic(name_space, mnemonic);
    if (!validate(comm))
        return false;
    m_namespace = std::move(name_space);
    m_mnemonic = std::move(mnemonic);
    return true;
}

bool
GncOptionAccountListValue::validate(const GncOptionAccountList& values) const
{
    if (values.empty())
        return true;
    if ((get_ui_type() == GncOptionUIType::ACCOUNT_SEL || !m_multiselect) &&
        values.size() != 1)
    {
        PWARN("GncOptionAccountListValue::validate: Multiple values for a non-multiselect option.");
        return false;
    }
    if (m_allowed.empty())
        return true;
    auto book{get_current_book()};
    for(auto& guid : values)
    {
        if (std::find(m_allowed.begin(), m_allowed.end(),
                      xaccAccountGetType(xaccAccountLookup(&guid, book))) == m_allowed.end())
        {
            PWARN("GncOptionAccountListValue::validate: Account %s is not of an allowed type", gnc::GUID(guid).to_string().c_str());
            return false; }
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

    auto root{get_current_root_account()};
    auto account_list{gnc_account_get_descendants_sorted(root)};
    if (!account_list)
        return retval;

    for (auto node = account_list; node; node = g_list_next (node))
    {
        if (std::find(m_allowed.begin(), m_allowed.end(),
                      xaccAccountGetType(GNC_ACCOUNT(node->data))) != m_allowed.end())
        {
            retval.push_back(*qof_entity_get_guid(GNC_ACCOUNT(node->data)));
            break;
        }
    }
    g_list_free(account_list);
    return retval;
}

bool
GncOptionAccountListValue::is_changed() const noexcept
{
    return m_value != m_default_value;
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
    auto book{get_current_book()};
    return guid_equal(guid_null(), &m_value) ? get_default_value() :
           xaccAccountLookup(&m_value, book);
}

const Account*
GncOptionAccountSelValue::get_default_value() const
{

    if (!guid_equal(guid_null(), &m_default_value))
    {
        auto book{get_current_book()};
        return xaccAccountLookup(&m_default_value, book);
    }

    /* If no default has been set and there's an allowed set then find the first
     * account that matches one of the allowed account types.
     */
    if (m_allowed.empty())
        return nullptr;

    const Account* retval{nullptr};
    auto root{get_current_root_account()};
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
uint16_t
GncOptionDateValue::get_period_index() const noexcept
{
    assert (m_period != RelativeDatePeriod::ABSOLUTE);
    assert(!m_period_set.empty());
    auto item{std::find(m_period_set.begin(), m_period_set.end(), m_period)};
    assert(item != m_period_set.end());
    return item - m_period_set.begin();
}

uint16_t
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
GncOptionDateValue::set_value(uint16_t index) noexcept
{
    assert(!m_period_set.empty());
    assert(index < m_period_set.size());
    m_date = INT64_MAX;
    m_period = m_period_set[index];
    m_dirty = true;
}

uint16_t
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
            qof_type = "gncTaxTable";
            break;
        case GncOptionUIType::ACCOUNT_LIST:
        case GncOptionUIType::ACCOUNT_SEL:
        default:
            qof_type = "Account";
            break;
    }
    auto book{get_current_book()};
    auto col{qof_book_get_collection(book, qof_type)};
    return QOF_INSTANCE(qof_collection_lookup_entity(col, guid));
}

QofInstance*
qof_instance_from_string(const std::string& str, GncOptionUIType type)
{
    QofInstance* retval{nullptr};
    try {
        auto guid{static_cast<GncGUID>(gnc::GUID::from_string(str))};
        retval = qof_instance_from_guid(&guid, type);
    }
    catch (const gnc::guid_syntax_exception& err)
    {
        PWARN("Failed to convert %s to a GUID", str.c_str());
    }
    return retval;
}

std::string
qof_instance_to_string(const QofInstance* inst)
{
    std::string retval;
    gnc::GUID guid{*qof_instance_get_guid(inst)};
    retval = guid.to_string();
    return retval;
}

template <typename ValueType> void
GncOptionValue<ValueType>::set_value(ValueType new_value)
{
    m_value = new_value;
    m_dirty = true;
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
    if constexpr(std::is_same_v<ValueType, GncOptionReportPlacementVec>)
    {
        std::ostringstream ostr{};
        ostr << "'(";
        std::for_each(m_value.begin(), m_value.end(),
                 [&ostr](auto rp){
                    auto [id, wide, high] = rp;
                    ostr << "(" << id << " " << wide << " " << high << " #f) ";
                 });
        ostr << ")";
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
    if constexpr(std::is_same_v<ValueType, GncOptionReportPlacementVec>)
    {
        std::istringstream istr{str};
        GncOptionReportPlacementVec rpv;
        while (istr)
        {
            uint32_t id, wide, high;
            istr >> id >> wide >> high;
            rpv.emplace_back(id, wide, high);
        }
        set_value(rpv);
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

std::string
GncOptionAccountListValue::serialize() const noexcept
{
    static const std::string no_value{"No Value"};
    std::string retval;
    bool first = true;
    if (m_value.empty())
        return no_value;
    gchar guidstr[GUID_ENCODING_LENGTH + 1];
    for (auto val : m_value)
    {
        if (!first)
            retval += " ";
        first = false;
        guid_to_string_buff (&val, guidstr);
        retval += guidstr;
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
        GncGUID guid{};
        string_to_guid(str.substr(pos, pos + GUID_ENCODING_LENGTH).c_str(), &guid);
        m_value.push_back(guid);
        pos += GUID_ENCODING_LENGTH;
    }
    return true;
}

std::string
GncOptionAccountSelValue::serialize() const noexcept
{
    static const std::string no_value{"No Value"};
    if (guid_equal(guid_null(), &m_value))
        return no_value;

    gchar strbuff[GUID_ENCODING_LENGTH + 1];
    guid_to_string_buff (&m_value, strbuff);
    return strbuff;
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
    static const std::string no_value{""};
    std::string retval;
    bool first = true;
    bool list_context = m_ui_type == GncOptionUIType::LIST;
    if (m_value.empty())
        return no_value;

    if (list_context)
        retval += '(';
    for (auto index : m_value)
    {
        if (!first)
            retval += " ";
        first = false;
        retval += std::get<0>(m_choices[index]);
    }
    if (list_context)
        retval += ')';
    return retval;
}

bool
GncOptionMultichoiceValue::deserialize(const std::string& str) noexcept
{
    static const auto uint16_t_max = std::numeric_limits<uint16_t>::max();
    if (str.empty())

        return false;
    uint16_t pos{};
    while (pos < str.size())
    {
        auto endpos{str.find(' ', pos)};
        if (endpos == std::string::npos)
            endpos = str.size();
        //need a null-terminated char* to pass to permissible_value_index
        auto index{permissible_value_index(str.substr(pos, endpos).c_str())};
        if (index == uint16_t_max)
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
    {
        std::ostringstream ostr;
        if constexpr(is_same_decayed_v<ValueType, double>)
            ostr << std::showpoint << std::fixed;
        ostr << m_value;
        return ostr.str();
    }
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
        set_value(static_cast<uint16_t>(std::stoll(period_str)));
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

std::istream&
operator>> (std::istream& iss, GncOptionCommodityValue& opt)
{
    std::string instr;
    iss >> instr;
    if (!opt.deserialize(instr))
        throw std::invalid_argument("Invalid commodity string in stream.");
    return iss;
}

template void GncOptionValue<bool>::set_value(bool);
template void GncOptionValue<int>::set_value(int);
template void GncOptionValue<int64_t>::set_value(int64_t);
template void GncOptionValue<double>::set_value(double);
template void GncOptionValue<char*>::set_value(char*);
template void GncOptionValue<const char*>::set_value(const char*);
template void GncOptionValue<std::string>::set_value(std::string);
template void GncOptionValue<const QofQuery*>::set_value(const QofQuery*);
template void GncOptionValue<const GncOwner*>::set_value(const GncOwner*);
template void GncOptionValue<RelativeDatePeriod>::set_value(RelativeDatePeriod);
template void GncOptionValue<uint16_t>::set_value(uint16_t);
template void GncOptionValue<GncOptionAccountList>::set_value(GncOptionAccountList);
template void GncOptionValue<GncMultichoiceOptionIndexVec>::set_value(GncMultichoiceOptionIndexVec);
template void GncOptionValue<GncOptionReportPlacementVec>::set_value(GncOptionReportPlacementVec);
template void GncOptionValue<GncOptionDateFormat>::set_value(GncOptionDateFormat);
template void GncOptionValue<bool>::set_default_value(bool);
template void GncOptionValue<int>::set_default_value(int);
template void GncOptionValue<int64_t>::set_default_value(int64_t);
template void GncOptionValue<double>::set_default_value(double);
template void GncOptionValue<char*>::set_default_value(char*);
template void GncOptionValue<const char*>::set_default_value(const char*);
template void GncOptionValue<std::string>::set_default_value(std::string);
template void GncOptionValue<const QofQuery*>::set_default_value(const QofQuery*);
template void GncOptionValue<const GncOwner*>::set_default_value(const GncOwner*);
template void GncOptionValue<RelativeDatePeriod>::set_default_value(RelativeDatePeriod);
template void GncOptionValue<uint16_t>::set_default_value(uint16_t);
template void GncOptionValue<GncOptionAccountList>::set_default_value(GncOptionAccountList);
template void GncOptionValue<GncMultichoiceOptionIndexVec>::set_default_value(GncMultichoiceOptionIndexVec);
template void GncOptionValue<GncOptionReportPlacementVec>::set_default_value(GncOptionReportPlacementVec);
template void GncOptionValue<GncOptionDateFormat>::set_default_value(GncOptionDateFormat);
template void GncOptionValue<bool>::reset_default_value();
template void GncOptionValue<int>::reset_default_value();
template void GncOptionValue<int64_t>::reset_default_value();
template void GncOptionValue<double>::reset_default_value();
template void GncOptionValue<char*>::reset_default_value();
template void GncOptionValue<const char*>::reset_default_value();
template void GncOptionValue<std::string>::reset_default_value();
template void GncOptionValue<const QofQuery*>::reset_default_value();
template void GncOptionValue<const GncOwner*>::reset_default_value();
template void GncOptionValue<RelativeDatePeriod>::reset_default_value();
template void GncOptionValue<uint16_t>::reset_default_value();
template void GncOptionValue<GncOptionAccountList>::reset_default_value();
template void GncOptionValue<GncMultichoiceOptionIndexVec>::reset_default_value();
template void GncOptionValue<GncOptionReportPlacementVec>::reset_default_value();
template void GncOptionValue<GncOptionDateFormat>::reset_default_value();
template std::string GncOptionValue<bool>::serialize() const noexcept;
template std::string GncOptionValue<int>::serialize() const noexcept;
template std::string GncOptionValue<int64_t>::serialize() const noexcept;
template std::string GncOptionValue<double>::serialize() const noexcept;
template std::string GncOptionValue<char*>::serialize() const noexcept;
template std::string GncOptionValue<const char*>::serialize() const noexcept;
template std::string GncOptionValue<std::string>::serialize() const noexcept;
template std::string GncOptionValue<const QofQuery*>::serialize() const noexcept;
template std::string GncOptionValue<const GncOwner*>::serialize() const noexcept;
template std::string GncOptionValue<GncOptionReportPlacementVec>::serialize() const noexcept;
template std::string GncOptionValue<GncOptionDateFormat>::serialize() const noexcept;
template std::string GncOptionRangeValue<int>::serialize() const noexcept;
template std::string GncOptionRangeValue<double>::serialize() const noexcept;
template bool GncOptionValue<bool>::deserialize(const std::string&) noexcept;
template bool GncOptionValue<int>::deserialize(const std::string&) noexcept;
template bool GncOptionValue<int64_t>::deserialize(const std::string&) noexcept;
template bool GncOptionValue<double>::deserialize(const std::string&) noexcept;
template bool GncOptionValue<char*>::deserialize(const std::string&) noexcept;
template bool GncOptionValue<const char*>::deserialize(const std::string&) noexcept;
template bool GncOptionValue<std::string>::deserialize(const std::string&) noexcept;
template bool GncOptionValue<const QofQuery*>::deserialize(const std::string&) noexcept;
template bool GncOptionValue<const GncOwner*>::deserialize(const std::string&) noexcept;
template bool GncOptionValue<GncOptionReportPlacementVec>::deserialize(const std::string&) noexcept;
template bool GncOptionValue<GncOptionDateFormat>::deserialize(const std::string&) noexcept;
template bool GncOptionRangeValue<int>::deserialize(const std::string&) noexcept;
template bool GncOptionRangeValue<double>::deserialize(const std::string&) noexcept;
