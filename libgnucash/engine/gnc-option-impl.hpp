/********************************************************************\
 * gnc-option-impl.hpp -- Application options system                *
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

/** @addtogroup Engine
    @{ */
/** @addtogroup Options
    @{ */
/** @file gnc-option-impl.hpp
    @brief Implementation templates and specializtions for GncOption values.
    Objecte created by these templates are wrapped by the GncOption variant.
    @author Copyright 2019-2021 John Ralls <jralls@ceridwen.us>
*/
#ifndef GNC_OPTION_IMPL_HPP_
#define GNC_OPTION_IMPL_HPP_

#include "gnc-option.hpp"

#include <config.h>
#include "qof.h"
#include "Account.h"
#include "gnc-budget.h"
#include "gnc-commodity.h"
#include "gnc-datetime.hpp"
#include <string>
#include <utility>
#include <vector>
#include <exception>
#include <functional>
#include <variant>
#include <iostream>
#include <limits>

#include "gnc-option-uitype.hpp"


#ifndef SWIG
size_t constexpr classifier_size_max{50};
size_t constexpr sort_tag_size_max{10};
#endif

/** @struct OptionClassifier
 * This class is the parent of all option implementations. It contains the
 * elements that the optiondb uses to retrieve option values and that the
 * options dialog determines on which tab to place the option, in what order,
 * and what string to display as a tooltip.
 */
struct OptionClassifier
{
    std::string m_section;
    std::string m_name;
    std::string m_sort_tag;
//  std::type_info m_kvp_type;
    std::string m_doc_string;
};


#ifndef SWIG
auto constexpr uint16_t_max = std::numeric_limits<uint16_t>::max();
#endif

/** @class GncOptionValue
 *  The generic option-value class. Most option types can use this template.
 */
template <typename ValueType>
class GncOptionValue : public OptionClassifier
{
public:
    GncOptionValue(const char* section, const char* name,
                   const char* key, const char* doc_string,
                   ValueType value,
                   GncOptionUIType ui_type = GncOptionUIType::INTERNAL) :
        OptionClassifier{section, name, key, doc_string},
        m_ui_type(ui_type), m_value{value}, m_default_value{value} { }
    GncOptionValue(const GncOptionValue&) = default;
    GncOptionValue(GncOptionValue&&) = default;
    GncOptionValue& operator=(const GncOptionValue&) = default;
    GncOptionValue& operator=(GncOptionValue&&) = default;
    ~GncOptionValue() = default;
    ValueType get_value() const { return m_value; }
    ValueType get_default_value() const { return m_default_value; }
    void set_value(ValueType new_value);
    void set_default_value(ValueType new_value);
    void reset_default_value();
    bool is_changed() const noexcept { return m_value != m_default_value; }
    GncOptionUIType get_ui_type() const noexcept { return m_ui_type; }
    void make_internal() { m_ui_type = GncOptionUIType::INTERNAL; }
    bool is_internal() { return m_ui_type == GncOptionUIType::INTERNAL; }
    std::string serialize() const noexcept;
    bool deserialize(const std::string& str) noexcept;
private:
    GncOptionUIType m_ui_type;
    ValueType m_value;
    ValueType m_default_value;
};


/** class GncOptionGncOwnerValue
 *
 * Unlike QofInstance based classes GncOwners are created on the fly, aren't
 * placed in QofCollection, and therefore their lifetimes have to be managed.
 * We use GncOwnerPtr for the purpose.
 */
struct GncOwnerDeleter
{
    void operator()(GncOwner* o) {
        g_free(o);
    }
};

using GncOwnerPtr = std::unique_ptr<GncOwner, GncOwnerDeleter>;

class GncOptionGncOwnerValue: public OptionClassifier {
public:
    GncOptionGncOwnerValue(
        const char* section, const char* name,
        const char* key, const char* doc_string,
        const GncOwner* value,
        GncOptionUIType ui_type = GncOptionUIType::INTERNAL);
    GncOptionGncOwnerValue(const GncOptionGncOwnerValue& from);
    GncOptionGncOwnerValue(GncOptionGncOwnerValue&&) = default;
    ~GncOptionGncOwnerValue() = default;
    const GncOwner* get_value() const;
    const GncOwner* get_default_value() const;
    void set_value(const GncOwner* new_value);
    void set_default_value(const GncOwner* new_value);
    void reset_default_value();
    bool is_changed() const noexcept;
    GncOptionUIType get_ui_type() const noexcept { return m_ui_type; }
    void make_internal() { m_ui_type = GncOptionUIType::INTERNAL; }
    bool is_internal() { return m_ui_type == GncOptionUIType::INTERNAL; }
    std::string serialize() const noexcept;
    bool deserialize(const std::string& str) noexcept;
private:
    GncOptionUIType m_ui_type;
    GncOwnerPtr m_value;
    GncOwnerPtr m_default_value;
};

/** class GncOptionQofinstanceValue
 *
 * QofInstances know what type they are but getting them to tell you is a pain
 * so we put them in a pair with a type identifier.
 */
using GncItem = std::pair<QofIdTypeConst, GncGUID>;

class GncOptionQofInstanceValue: public OptionClassifier {
public:
    GncOptionQofInstanceValue(
        const char* section, const char* name,
        const char* key, const char* doc_string,
        const QofInstance* value,
        GncOptionUIType ui_type = GncOptionUIType::INTERNAL);
    GncOptionQofInstanceValue(const GncOptionQofInstanceValue& from);
    GncOptionQofInstanceValue(GncOptionQofInstanceValue&&) = default;
    GncOptionQofInstanceValue& operator=(GncOptionQofInstanceValue&&) = default;
    ~GncOptionQofInstanceValue() = default;
    const QofInstance* get_value() const;
    const QofInstance* get_default_value() const;
    GncItem get_item() const { return m_value; }
    GncItem get_default_item() const { return m_default_value; }
    void set_value(const QofInstance* new_value);
    void set_default_value(const QofInstance* new_value);
    void reset_default_value();
    bool is_changed() const noexcept;
    GncOptionUIType get_ui_type() const noexcept { return m_ui_type; }
    void make_internal() { m_ui_type = GncOptionUIType::INTERNAL; }
    bool is_internal() { return m_ui_type == GncOptionUIType::INTERNAL; }
    std::string serialize() const noexcept;
    bool deserialize(const std::string& str) noexcept;
private:
    GncOptionUIType m_ui_type;
    GncItem m_value;
    GncItem m_default_value;
};

/** class GncOptionCommodityValue
 * Commodities are stored with their namespace and mnemonic instead of their gncGUID
 * so that they can be correctly retrieved even if they're deleted and recreated.
 * Additionally if GncOptionCommodityValue is created with GncOptionUIType::CURRENCY
 * it will throw std::invalid_argument if one attempts to set a value that isn't a
 * currency.
 */

class GncOptionCommodityValue : public OptionClassifier
{
public:
    GncOptionCommodityValue() = delete;
    GncOptionCommodityValue(const char* section, const char* name,
                                       const char* key, const char* doc_string,
                                       gnc_commodity* value,
                                       GncOptionUIType ui_type = GncOptionUIType::COMMODITY) :
        OptionClassifier{section, name, key, doc_string},
        m_ui_type{ui_type}, m_is_currency{ui_type == GncOptionUIType::CURRENCY},
        m_namespace{gnc_commodity_get_namespace(value)},
        m_mnemonic{gnc_commodity_get_mnemonic(value)},
        m_default_namespace{gnc_commodity_get_namespace(value)},
        m_default_mnemonic{gnc_commodity_get_mnemonic(value)}
    {
       if (!validate(value))
            throw std::invalid_argument("Attempt to create GncOptionCommodityValue with currency UIType and non-currency value.");
    }
    GncOptionCommodityValue(const GncOptionCommodityValue&) = default;
    GncOptionCommodityValue(GncOptionCommodityValue&&) = default;
    GncOptionCommodityValue& operator=(const GncOptionCommodityValue&) = default;
    GncOptionCommodityValue& operator=(GncOptionCommodityValue&&) = default;
    gnc_commodity* get_value() const;
    gnc_commodity* get_default_value() const;
    bool validate(gnc_commodity*) const noexcept;
    void set_value(gnc_commodity* value);
    void set_default_value(gnc_commodity* value);
    void reset_default_value();
    bool is_changed() const noexcept;
    GncOptionUIType get_ui_type() const noexcept { return m_ui_type; }
    void make_internal() { m_ui_type = GncOptionUIType::INTERNAL; }
    bool is_internal() { return m_ui_type == GncOptionUIType::INTERNAL; }
    std::string serialize() const noexcept;
    bool deserialize(const std::string& str) noexcept;
private:
    GncOptionUIType m_ui_type;
    bool m_is_currency;
    std::string m_namespace;
    std::string m_mnemonic;
    std::string m_default_namespace;
    std::string m_default_mnemonic;
};

QofInstance* qof_instance_from_string(const std::string& str,
                                      GncOptionUIType type);
QofInstance* qof_instance_from_guid(GncGUID*, GncOptionUIType type);
std::string qof_instance_to_string(const QofInstance* inst);

template <typename T>
struct is_GncOwnerValue
{
    static constexpr bool value =
        std::is_same_v<std::decay_t<T>, GncOptionGncOwnerValue>;
};

template <typename T> inline constexpr bool
is_GncOwnerValue_v = is_GncOwnerValue<T>::value;

template <typename T>
struct is_QofInstanceValue
{
    static constexpr bool value =
        std::is_same_v<std::decay_t<T>, GncOptionQofInstanceValue>;
};

template <typename T> inline constexpr bool
is_QofInstanceValue_v = is_QofInstanceValue<T>::value;

template <typename T>
struct is_QofQueryValue
{
    static constexpr bool value =
         std::is_same_v<std::decay_t<T>, GncOptionValue<const QofQuery*>>;
};

template <typename T> inline constexpr bool
is_QofQueryValue_v = is_QofQueryValue<T>::value;

/* These will work when m_value is a built-in class; GnuCash class and container
 * values will need specialization unless they happen to define operators << and
 * >>.
 * Note that SWIG 3.0.12 chokes on elaborate enable_if so just hide the
 * following templates from SWIG. (Ignoring doesn't work because SWIG still has
 * to parse the templates to figure out the symbols.
 */
#ifndef SWIG
template<class OptType,
         typename std::enable_if_t<is_OptionClassifier_v<OptType> &&
                                   ! (is_QofInstanceValue_v<OptType> ||
                                      is_RangeValue_v<OptType>), int> = 0>
std::ostream& operator<<(std::ostream& oss, const OptType& opt)
{
    oss << opt.get_value();
    return oss;
}

template<> inline std::ostream&
operator<< <GncOptionValue<bool>>(std::ostream& oss,
                                  const GncOptionValue<bool>& opt)
{
    oss << (opt.get_value() ? "#t" : "#f");
    return oss;
}

inline std::ostream&
operator<< (std::ostream& oss, const GncOptionCommodityValue& opt)
{
    oss << opt.serialize();
    return oss;
}

template<class OptType,
         typename std::enable_if_t<is_QofInstanceValue_v<OptType>, int> = 0>
inline std::ostream&
operator<< (std::ostream& oss, const OptType& opt)
{
    auto value = opt.get_value();
    oss << qof_instance_to_string(value);
    return oss;
}

template<class OptType,
         typename std::enable_if_t<is_OptionClassifier_v<OptType> &&
                                   !(is_QofInstanceValue_v<OptType> ||
                                     is_RangeValue_v<OptType>), int> = 0>
std::istream& operator>>(std::istream& iss, OptType& opt)
{
    if constexpr (std::is_same_v<std::decay_t<decltype(opt.get_value())>, const _gncOwner*> ||
                  std::is_same_v<std::decay_t<decltype(opt.get_value())>, const _QofQuery*>)
        return iss;
    else
    {
        std::decay_t<decltype(opt.get_value())> value;
        iss >> value;
        opt.set_value(value);
        return iss;
    }
}

std::istream& operator>> (std::istream& iss, GncOptionCommodityValue& opt);

template<class OptType,
         typename std::enable_if_t<is_QofInstanceValue_v<OptType>, int> = 0>
std::istream&
operator>> (std::istream& iss, OptType& opt)
{
    std::string instr;
    iss >> instr;
    opt.set_value(qof_instance_from_string(instr, opt.get_ui_type()));
    return iss;
}

template<> inline std::istream&
operator>> <GncOptionValue<bool>>(std::istream& iss,
                                  GncOptionValue<bool>& opt)
{
    std::string instr;
    iss >> instr;
    opt.set_value(instr == "#t" ? true : false);
    return iss;
}

template<> inline std::istream&
operator>> <GncOptionValue<GncOptionReportPlacementVec>>(std::istream& iss,
    GncOptionValue<GncOptionReportPlacementVec>& opt)
{
    uint32_t id, wide, high;
    iss >> id >> wide >> high;
        opt.set_value(GncOptionReportPlacementVec{{id, wide, high}});
    return iss;
}
#endif // SWIG

/** @class GncOptionRangeValue
 * Used for numeric ranges and plot sizes.
 */

template <typename ValueType>
class GncOptionRangeValue : public OptionClassifier
{
public:
    GncOptionRangeValue<ValueType>(const char* section, const char* name,
                                   const char* key, const char* doc_string,
                                   ValueType value, ValueType min,
                                   ValueType max, ValueType step) :
        GncOptionRangeValue<ValueType>{section, name, key, doc_string, value, min,
                                       max, step, GncOptionUIType::NUMBER_RANGE} {}
    GncOptionRangeValue<ValueType>(const char* section, const char* name,
                                   const char* key, const char* doc_string,
                                   ValueType value, ValueType min,
                                   ValueType max, ValueType step, GncOptionUIType ui) :
        OptionClassifier{section, name, key, doc_string}, m_ui_type{ui},
        m_value{value >= min && value <= max ? value : min},
        m_default_value{value >= min && value <= max ? value : min},
        m_min{min}, m_max{max}, m_step{step} {
           if constexpr(is_same_decayed_v<ValueType, int>)
                set_alternate(true);}
    GncOptionRangeValue<ValueType>(const GncOptionRangeValue<ValueType>&) = default;
    GncOptionRangeValue<ValueType>(GncOptionRangeValue<ValueType>&&) = default;
    GncOptionRangeValue<ValueType>& operator=(const GncOptionRangeValue<ValueType>&) = default;
    GncOptionRangeValue<ValueType>& operator=(GncOptionRangeValue<ValueType>&&) = default;
    ValueType get_value() const { return m_value; }
    ValueType get_default_value() const { return m_default_value; }
    bool validate(ValueType value) { return value >= m_min && value <= m_max; }
    void set_value(ValueType value)
    {
        if (this->validate(value))
            m_value = value;
        else
            throw std::invalid_argument("Validation failed, value not set.");
    }
    void set_default_value(ValueType value)
    {
        if (this->validate(value))
            m_value = m_default_value = value;
        else
            throw std::invalid_argument("Validation failed, value not set.");
    }
    void get_limits(ValueType& upper, ValueType& lower, ValueType& step) const noexcept
    {
        upper = m_max;
        lower = m_min;
        step = m_step;
    }
    void reset_default_value() { m_value = m_default_value; }
    bool is_changed() const noexcept { return m_value != m_default_value; }
    GncOptionUIType get_ui_type() const noexcept { return m_ui_type; }
    void make_internal() { m_ui_type = GncOptionUIType::INTERNAL; }
    bool is_internal() { return m_ui_type == GncOptionUIType::INTERNAL; }
    bool is_alternate() const noexcept { return m_alternate; }
    void set_alternate(bool value) noexcept { m_alternate = value; }
    std::string serialize() const noexcept;
    bool deserialize(const std::string& str) noexcept;
private:
    GncOptionUIType m_ui_type = GncOptionUIType::NUMBER_RANGE;
    ValueType m_value;
    ValueType m_default_value;
    ValueType m_min;
    ValueType m_max;
    ValueType m_step;
    bool m_alternate = false;
};

template<class OptType,
         typename std::enable_if_t<is_RangeValue_v<OptType>, int> = 0>
inline std::ostream&
operator<< (std::ostream& oss, const OptType& opt)
{
    if (opt.get_ui_type() == GncOptionUIType::PLOT_SIZE)
        oss << (opt.is_alternate() ? "pixels" : "percent") << " ";
    oss << opt.get_value();
    return oss;
}

template<class OptType,
         typename std::enable_if_t<is_RangeValue_v<OptType>, int> = 0>
inline std::istream&
operator>> (std::istream& iss, OptType& opt)
{
    if (opt.get_ui_type() == GncOptionUIType::PLOT_SIZE)
    {
        std::string alt;
        iss >> alt;
        opt.set_alternate(strncmp(alt.c_str(), "percent",
                                  strlen("percent")) == 0);
    }
    if constexpr (std::is_same_v<std::decay_t<OptType>,
                  GncOptionRangeValue<double>>)
    {
        double d;
        iss >> d;
        opt.set_value(d);
    }
    else
    {
        int i;
        iss >> i;
        opt.set_value(i);
    }
    return iss;
}

using GncMultichoiceOptionEntry = std::tuple<const std::string,
                                             const std::string,
                                             GncOptionMultichoiceKeyType>;
using GncMultichoiceOptionIndexVec = std::vector<uint16_t>;
using GncMultichoiceOptionChoices = std::vector<GncMultichoiceOptionEntry>;

/** @class GncOptionMultichoiceValue
 * Multichoice options have a vector of valid options
 * (GncMultichoiceOptionChoices) and validate the selection as being one of
 * those values. The value is the index of the selected item in the vector.

 * GncMultichoiceOptionEntry is a tuple of two strings and a
 * GncOptionMultichoiceKeyType value; the first string is the internal value of
 * the option, the second is the display name that should be localized at the
 * point of use (so mark it with N_() or (N_ ) when creating the multichoices)
 * and the third is an enum value indicating whether the key should be
 * interpreted as a Scheme symbol, a string, or a number.
 *
 *
 */

class GncOptionMultichoiceValue : public OptionClassifier
{
public:
    GncOptionMultichoiceValue(const char* section, const char* name,
                              const char* key, const char* doc_string,
                              const char* value,
                              GncMultichoiceOptionChoices&& choices,
                              GncOptionUIType ui_type = GncOptionUIType::MULTICHOICE) :
        OptionClassifier{section, name, key, doc_string},
        m_ui_type{ui_type},
        m_value{}, m_default_value{}, m_choices{std::move(choices)}
    {
        if (value)
        {
            if (auto index = find_key(value);
                index != uint16_t_max)
            {
                m_value.push_back(index);
                m_default_value.push_back(index);
            }
        }
    }

    GncOptionMultichoiceValue(const char* section, const char* name,
                              const char* key, const char* doc_string,
                              uint16_t index,
                              GncMultichoiceOptionChoices&& choices,
                              GncOptionUIType ui_type = GncOptionUIType::MULTICHOICE) :
        OptionClassifier{section, name, key, doc_string},
        m_ui_type{ui_type},
        m_value{}, m_default_value{}, m_choices{std::move(choices)}
    {
        if (index < m_choices.size())
        {
            m_value.push_back(index);
            m_default_value.push_back(index);
        }
    }

    GncOptionMultichoiceValue(const char* section, const char* name,
                              const char* key, const char* doc_string,
                              GncMultichoiceOptionIndexVec&& indices,
                              GncMultichoiceOptionChoices&& choices,
                              GncOptionUIType ui_type = GncOptionUIType::LIST) :
        OptionClassifier{section, name, key, doc_string},
        m_ui_type{ui_type},
        m_value{indices}, m_default_value{std::move(indices)},
        m_choices{std::move(choices)} {}
    GncOptionMultichoiceValue(const GncOptionMultichoiceValue&) = default;
    GncOptionMultichoiceValue(GncOptionMultichoiceValue&&) = default;
    GncOptionMultichoiceValue& operator=(const GncOptionMultichoiceValue&) = default;
    GncOptionMultichoiceValue& operator=(GncOptionMultichoiceValue&&) = default;

    const std::string& get_value() const
    {
        auto vec{m_value.size() > 0 ? m_value : m_default_value};
        if (vec.size() == 0)
            return c_empty_string;
        if (vec.size() == 1)
            return std::get<0>(m_choices.at(vec[0]));
        else
            return c_list_string;

    }
    const std::string& get_default_value() const
    {
        if (m_default_value.size() == 1)
            return std::get<0>(m_choices.at(m_default_value[0]));
        else if (m_default_value.size() == 0)
            return c_empty_string;
        else
            return c_list_string;
    }

    uint16_t get_index() const
    {
        if (m_value.size() > 0)
            return m_value[0];
        if (m_default_value.size() > 0)
            return m_default_value[0];
        return 0;
    }
    const GncMultichoiceOptionIndexVec& get_multiple() const noexcept
    {
        return m_value;
    }
    const GncMultichoiceOptionIndexVec& get_default_multiple() const noexcept
    {
        return m_default_value;
    }
    bool validate(const std::string& value) const noexcept
    {
        auto index = find_key(value);
        return index != uint16_t_max;

    }
    bool validate(const GncMultichoiceOptionIndexVec& indexes) const noexcept
    {
        for (auto index : indexes)
            if (index >= m_choices.size())
                return false;
        return true;

    }
    void set_value(const std::string& value)
    {
        auto index = find_key(value);
        if (index != uint16_t_max)
        {
            m_value.clear();
            m_value.push_back(index);
        }
        else
            throw std::invalid_argument("Value not a valid choice.");

    }
    void set_value(uint16_t index)
    {
        if (index < m_choices.size())
        {
            m_value.clear();
            m_value.push_back(index);
        }
        else
            throw std::invalid_argument("Value not a valid choice.");

    }
    void set_default_value(const std::string& value)
    {
        auto index = find_key(value);
        if (index != uint16_t_max)
        {
            m_value.clear();
            m_value.push_back(index);
            m_default_value.clear();
            m_default_value.push_back(index);
        }
        else
            throw std::invalid_argument("Value not a valid choice.");

    }
    void set_default_value(uint16_t index)
    {
        if (index < m_choices.size())
        {
            m_value.clear();
            m_value.push_back(index);
            m_default_value.clear();
            m_default_value.push_back(index);
        }
        else
            throw std::invalid_argument("Value not a valid choice.");

    }
    void set_multiple(const GncMultichoiceOptionIndexVec& indexes)
    {
        if (validate(indexes))
            m_value = indexes;
        else
            throw std::invalid_argument("One of the supplied indexes was out of range.");
    }
    void set_default_multiple(const GncMultichoiceOptionIndexVec& indexes)
    {
        if (validate(indexes))
            m_value = m_default_value = indexes;
        else
            throw std::invalid_argument("One of the supplied indexes was out of range.");
    }
    uint16_t num_permissible_values() const noexcept
    {
        return m_choices.size();
    }
    uint16_t permissible_value_index(const char* key) const noexcept
    {
            return find_key(key);
    }
    const char* permissible_value(uint16_t index) const
    {
        return std::get<0>(m_choices.at(index)).c_str();
    }
    const char* permissible_value_name(uint16_t index) const
    {
        return std::get<1>(m_choices.at(index)).c_str();
    }
    void reset_default_value() { m_value = m_default_value; }
    bool is_changed() const noexcept { return m_value != m_default_value; }
    GncOptionUIType get_ui_type() const noexcept { return m_ui_type; }
    void make_internal() { m_ui_type = GncOptionUIType::INTERNAL; }
    bool is_internal() { return m_ui_type == GncOptionUIType::INTERNAL; }
    GncOptionMultichoiceKeyType get_keytype(unsigned i) const { return std::get<2>(m_choices.at(i)); }
    std::string serialize() const noexcept;
    bool deserialize(const std::string& str) noexcept;
private:
    uint16_t find_key (const std::string& key) const noexcept
    {
        auto iter = std::find_if(m_choices.begin(), m_choices.end(),
                              [key](auto choice) {
                                  return std::get<0>(choice) == key; });
        if (iter != m_choices.end())
            return iter - m_choices.begin();
        else
            return uint16_t_max;

    }
    GncOptionUIType m_ui_type;
    GncMultichoiceOptionIndexVec m_value;
    GncMultichoiceOptionIndexVec m_default_value;
    GncMultichoiceOptionChoices m_choices;
    static const std::string c_empty_string;
    static const std::string c_list_string;
};

template<> inline std::ostream&
operator<< <GncOptionMultichoiceValue>(std::ostream& oss,
                                       const GncOptionMultichoiceValue& opt)
{
    auto vec{opt.get_multiple()};
    bool first{true};
    for (auto index : vec)
    {
        if (first)
            first = false;
        else
            oss << " ";
        oss << opt.permissible_value(index);
    }
    return oss;
}

template<> inline std::istream&
operator>> <GncOptionMultichoiceValue>(std::istream& iss,
                                       GncOptionMultichoiceValue& opt)
{
    GncMultichoiceOptionIndexVec values;
    while (true)
    {
        std::string str;
        std::getline(iss, str, ' ');
        if (!str.empty())
        {
            auto index = opt.permissible_value_index(str.c_str());
            if (index != uint16_t_max)
                values.push_back(index);
            else
            {
                std::string err = str + " is not one of ";
                err += opt.m_name;
                err += "'s permissible values.";
                throw std::invalid_argument(err);
            }
        }
        else
            break;
    }
    opt.set_multiple(values);
    iss.clear();
    return iss;
}


using GncOptionAccountList = std::vector<GncGUID>;

using GncOptionAccountTypeList = std::vector<GNCAccountType>;

/** @class GncOptionAccountListValue
 *
 * Set one or more accounts on which to report, optionally restricted to certain
 * account types. Many calls to make-account-list-option will pass a get-default
 * function that retrieves all of the accounts of a list of types.
 *
 * Some reports (examples/daily-reports.scm and standard/ account-piechart.scm,
 * advanced-portfolio.scm, category-barchart.scm, net-charts.scm, and
 * portfolio.scm) also provide a validator that rejects accounts that don't meet
 * an account-type criterion.
 *
 * There are two types of option, account-list which permits more than one
 * account selection and account-sel, which doesn't.
 *

 */

class GncOptionAccountListValue : public OptionClassifier
{
public:
    GncOptionAccountListValue(const char* section, const char* name,
                          const char* key, const char* doc_string,
                          GncOptionUIType ui_type, bool multi=true) :
        OptionClassifier{section, name, key, doc_string}, m_ui_type{ui_type},
        m_value{}, m_default_value{}, m_allowed{}, m_multiselect{multi} {}

    GncOptionAccountListValue(const char* section, const char* name,
                          const char* key, const char* doc_string,
                          GncOptionUIType ui_type,
                          const GncOptionAccountList& value, bool multi=true) :
        OptionClassifier{section, name, key, doc_string}, m_ui_type{ui_type},
        m_value{value}, m_default_value{std::move(value)}, m_allowed{},
        m_multiselect{multi}  {}
    GncOptionAccountListValue(const char* section, const char* name,
                          const char* key, const char* doc_string,
                          GncOptionUIType ui_type,
                          GncOptionAccountTypeList&& allowed, bool multi=true) :
        OptionClassifier{section, name, key, doc_string}, m_ui_type{ui_type},
        m_value{}, m_default_value{}, m_allowed{std::move(allowed)},
        m_multiselect{multi} {}
    GncOptionAccountListValue(const char* section, const char* name,
                          const char* key, const char* doc_string,
                          GncOptionUIType ui_type,
                          const GncOptionAccountList& value,
                          GncOptionAccountTypeList&& allowed, bool multi=true) :
        OptionClassifier{section, name, key, doc_string}, m_ui_type{ui_type},
        m_value{}, m_default_value{}, m_allowed{std::move(allowed)},
        m_multiselect{multi} {
            if (!validate(value))
                throw std::invalid_argument("Supplied Value not in allowed set.");
            m_value = value;
            m_default_value = std::move(value);
        }

    /* These aren't const& because if m_default_value hasn't been set
     * get_default_value finds the first account that matches the allowed types
     * and returns a GncOptionAccountList containing it. That's a stack variable
     * and must be returned by value.
     */
    GncOptionAccountList get_value() const;
    GncOptionAccountList get_default_value() const;
    bool validate (const GncOptionAccountList& values) const;
    void set_value (GncOptionAccountList values) {
        if (validate(values))
            //throw!
            m_value = values;
    }
    void set_default_value (GncOptionAccountList values) {
        if (validate(values))
            //throw!
            m_value = m_default_value = values;
    }
    GList* account_type_list() const noexcept;
    void reset_default_value() { m_value = m_default_value; }
    bool is_changed() const noexcept;
    GncOptionUIType get_ui_type() const noexcept { return m_ui_type; }
    void make_internal() { m_ui_type = GncOptionUIType::INTERNAL; }
    bool is_internal() { return m_ui_type == GncOptionUIType::INTERNAL; }
    bool is_multiselect() const noexcept { return m_multiselect; }
    std::string serialize() const noexcept;
    bool deserialize(const std::string& str) noexcept;
private:
    GncOptionUIType m_ui_type;
    GncOptionAccountList m_value;
    GncOptionAccountList m_default_value;
    GncOptionAccountTypeList m_allowed;
    bool m_multiselect;
};

template<> inline std::ostream&
operator<< <GncOptionAccountListValue>(std::ostream& oss,
                                       const GncOptionAccountListValue& opt)
{
    auto values{opt.get_value()};
    bool first = true;
    for (auto value : values)
    {
        if (first)
            first = false;
        else
            oss << " ";
        oss << guid_to_string(&value);
    }
    return oss;
}

template<> inline std::istream&
operator>> <GncOptionAccountListValue>(std::istream& iss,
                                   GncOptionAccountListValue& opt)
{
    GncOptionAccountList values;
    while (true)
    {
        std::string str;
        std::getline(iss, str, ' ');
        if (!str.empty())
        {
            auto guid{qof_entity_get_guid(qof_instance_from_string(str, opt.get_ui_type()))};
            values.push_back(*guid);
        }
        else
            break;
    }
    opt.set_value(values);
    iss.clear();
    return iss;
}

/* @class GncOptionAccountSelValue
 * Like GncOptionAccountListValue but contains only a single account.
 */

class GncOptionAccountSelValue : public OptionClassifier
{
public:
    GncOptionAccountSelValue(const char* section, const char* name,
                          const char* key, const char* doc_string,
                          GncOptionUIType ui_type) :
        OptionClassifier{section, name, key, doc_string}, m_ui_type{ui_type},
        m_value{*guid_null()}, m_default_value{*guid_null()}, m_allowed{} {}

    GncOptionAccountSelValue(const char* section, const char* name,
                          const char* key, const char* doc_string,
                          GncOptionUIType ui_type,
                          const Account* value) :
        OptionClassifier{section, name, key, doc_string}, m_ui_type{ui_type},
        m_value{*qof_entity_get_guid(value)},
        m_default_value{*qof_entity_get_guid(value)}, m_allowed{} {}
    GncOptionAccountSelValue(const char* section, const char* name,
                          const char* key, const char* doc_string,
                          GncOptionUIType ui_type,
                          GncOptionAccountTypeList&& allowed) :
        OptionClassifier{section, name, key, doc_string}, m_ui_type{ui_type},
        m_value{*guid_null()}, m_default_value{*guid_null()},
        m_allowed{std::move(allowed)} {}
    GncOptionAccountSelValue(const char* section, const char* name,
                          const char* key, const char* doc_string,
                          GncOptionUIType ui_type,
                          const Account* value,
                          GncOptionAccountTypeList&& allowed) :
        OptionClassifier{section, name, key, doc_string}, m_ui_type{ui_type},
        m_value{*guid_null()}, m_default_value{*guid_null()}, m_allowed{std::move(allowed)} {
            if (!validate(value))
                throw std::invalid_argument("Supplied Value not in allowed set.");
            m_value = m_default_value = *qof_entity_get_guid(value);
        }

    const Account* get_value() const;
    const Account* get_default_value() const;
    bool validate (const Account* value) const;
    void set_value (const Account* value) {
        if (validate(value))
        {
            auto guid{qof_entity_get_guid(value)};
            m_value = *guid;
        }
        //else throw
    }
    void set_default_value (const Account* value) {
        if (validate(value))
        {
            auto guid{qof_entity_get_guid(value)};
            m_value = m_default_value = *guid;
        }
        //else throw
    }
    GList* account_type_list() const noexcept;
    void reset_default_value() { m_value = m_default_value; }
    bool is_changed() const noexcept { return !guid_equal(&m_value, &m_default_value); }
    GncOptionUIType get_ui_type() const noexcept { return m_ui_type; }
    void make_internal() { m_ui_type = GncOptionUIType::INTERNAL; }
    bool is_internal() { return m_ui_type == GncOptionUIType::INTERNAL; }
    std::string serialize() const noexcept;
    bool deserialize(const std::string& str) noexcept;
private:
    GncOptionUIType m_ui_type;
    GncGUID m_value;
    GncGUID m_default_value;
    GncOptionAccountTypeList m_allowed;
};

template<> inline std::ostream&
operator<< <GncOptionAccountSelValue>(std::ostream& oss,
                                       const GncOptionAccountSelValue& opt)
{
    auto value{opt.get_value()};
    oss << qof_instance_to_string(QOF_INSTANCE(value));
    return oss;
}

template<> inline std::istream&
operator>> <GncOptionAccountSelValue>(std::istream& iss,
                                   GncOptionAccountSelValue& opt)
{
    Account* value{nullptr};
    std::string str;
    std::getline(iss, str, ' ');
    if (!str.empty())
        value = (Account*)qof_instance_from_string(str, opt.get_ui_type());
    opt.set_value(value);
    iss.clear();
    return iss;
}

/** @class GncOptionDateValue
 * A legal date value is a pair of either a RelativeDatePeriod, the absolute
 * flag and a time64, or for legacy purposes the absolute flag and a timespec.
 */
/*
gnc-date-option-show-time? -- option_data[1]
gnc-date-option-get-subtype -- option_data[0]
gnc-date-option-value-type m_value
gnc-date-option-absolute-time m_type == RelativeDatePeriod::ABSOLUTE
gnc-date-option-relative-time m_type != RelativeDatePeriod::ABSOLUTE
 */

class GncOptionDateValue : public OptionClassifier
{
public:
    GncOptionDateValue(const char* section, const char* name,
                       const char* key, const char* doc_string,
                       GncOptionUIType ui_type) :
        OptionClassifier{section, name, key, doc_string},
        m_ui_type{ui_type}, m_date{INT64_MAX}, m_default_date{INT64_MAX},
        m_period{RelativeDatePeriod::TODAY},
        m_default_period{RelativeDatePeriod::TODAY},
        m_period_set{} {}
    GncOptionDateValue(const char* section, const char* name,
                       const char* key, const char* doc_string,
                       GncOptionUIType ui_type, time64 time) :
        OptionClassifier{section, name, key, doc_string},
        m_ui_type{ui_type}, m_date{time}, m_default_date{time},
        m_period{RelativeDatePeriod::ABSOLUTE},
        m_default_period{RelativeDatePeriod::ABSOLUTE},
        m_period_set{} {}
    GncOptionDateValue(const char* section, const char* name,
                       const char* key, const char* doc_string,
                       GncOptionUIType ui_type,
                       RelativeDatePeriod period) :
        OptionClassifier{section, name, key, doc_string},
        m_ui_type{ui_type}, m_date{INT64_MAX}, m_default_date{INT64_MAX},
        m_period{period}, m_default_period{period},
        m_period_set{} {}
    GncOptionDateValue(const char* section, const char* name,
                       const char* key, const char* doc_string,
                       GncOptionUIType ui_type,
                       const RelativeDatePeriodVec& period_set) :
        OptionClassifier{section, name, key, doc_string},
        m_ui_type{ui_type}, m_date{INT64_MAX}, m_default_date{INT64_MAX},
        m_period{period_set.back()},
        m_default_period{period_set.back()},
        m_period_set{period_set} {}
    GncOptionDateValue(const GncOptionDateValue&) = default;
    GncOptionDateValue(GncOptionDateValue&&) = default;
    GncOptionDateValue& operator=(const GncOptionDateValue&) = default;
    GncOptionDateValue& operator=(GncOptionDateValue&&) = default;
    time64 get_value() const noexcept;
    time64 get_default_value() const noexcept;
    RelativeDatePeriod get_period() const noexcept { return m_period; }
    RelativeDatePeriod get_default_period() const noexcept { return m_default_period; }
    uint16_t get_period_index() const noexcept;
    uint16_t get_default_period_index() const noexcept;
    std::ostream& out_stream(std::ostream& oss) const noexcept;
    std::istream& in_stream(std::istream& iss);
    bool validate(RelativeDatePeriod value);
    bool validate(time64 time) {
        if (time > MINTIME && time < MAXTIME)
            return true;
        return false;
    }
    void set_value(RelativeDatePeriod value) {
        if (validate(value))
        {
            m_period = value;
            m_date = INT64_MAX;
        }
    }
    void set_value(time64 time) {
        if (validate(time))
        {
            m_period = RelativeDatePeriod::ABSOLUTE;
            m_date = time;
        }
    }
    void set_value(uint16_t index) noexcept;
    void set_default_value(RelativeDatePeriod value) {
        if (validate(value))
        {
            m_period = m_default_period = value;
            m_date = m_default_date = INT64_MAX;
        }
    }
    void set_default_value(time64 time) {
        if (validate(time))
        {
            m_period = m_default_period = RelativeDatePeriod::ABSOLUTE;
            m_date = m_default_date = time;
        }
    }
    uint16_t num_permissible_values() const noexcept
    {
        return m_period_set.size();
    }
    uint16_t permissible_value_index(const char* key) const noexcept;
    const char* permissible_value(uint16_t index) const
    {
        return gnc_relative_date_storage_string(m_period_set.at(index));
    }
    const char* permissible_value_name(uint16_t index) const
    {
        return gnc_relative_date_display_string(m_period_set.at(index));
    }
    void reset_default_value() {
        m_period = m_default_period;
        m_date = m_default_date;
    }
    bool is_changed() const noexcept { return m_period != m_default_period &&
            m_date != m_default_date; }
    GncOptionUIType get_ui_type() const noexcept { return m_ui_type; }
    void make_internal() { m_ui_type = GncOptionUIType::INTERNAL; }
    bool is_internal() { return m_ui_type == GncOptionUIType::INTERNAL; }
    const RelativeDatePeriodVec& get_period_set() const { return m_period_set; }
    std::string serialize() const noexcept;
    bool deserialize(const std::string& str) noexcept;
private:
    GncOptionUIType m_ui_type;
    time64 m_date;
    time64 m_default_date;
    RelativeDatePeriod m_period;
    RelativeDatePeriod m_default_period;
    RelativeDatePeriodVec m_period_set;
};

template<> inline std::ostream&
operator<< <GncOptionDateValue>(std::ostream& oss,
                                       const GncOptionDateValue& opt)
{
    return opt.out_stream(oss);
}

template<> inline std::istream&
operator>> <GncOptionDateValue>(std::istream& iss,
                                   GncOptionDateValue& opt)
{
    return opt.in_stream(iss);
}


#endif //GNC_OPTION_IMPL_HPP_
/**@}
   @} */
