/********************************************************************\
 * gnc-option.hpp -- Application options system                     *
 * Copyright (C) 2020 John Ralls <jralls@ceridwen.us>               *
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

/** @file gnc-option.hpp
    @brief C++ Public interface for individual options.
    @author Copyright 2020-2021 John Ralls <jralls@ceridwen.us>
*/

#ifndef GNC_OPTION_HPP_
#define GNC_OPTION_HPP_

#include <glib.h>
#include <any>
#include <string>
#include <iostream>
#include <iomanip>
#include <type_traits>
#include <variant>
#include <memory>
#include <tuple>
#include "gnc-option-ui.hpp"
#include "gnc-option-date.hpp"
#include "guid.hpp"

struct OptionClassifier;
class GncOptionUIItem;
using GncOptionUIItemPtr = std::unique_ptr<GncOptionUIItem>;
#ifndef SWIG //SWIG pulls in GncOwner from swig-engine.
struct _gncOwner;
using GncOwner = _gncOwner;
struct _QofQuery;
using QofQuery = _QofQuery;
#endif
struct QofInstance_s;
using QofInstance = QofInstance_s;
template <typename ValueType> class GncOptionValue;
class GncOptionGncOwnerValue;
class GncOptionQofInstanceValue;
class GncOptionAccountListValue;
class GncOptionAccountSelValue;
class GncOptionMultichoiceValue;
template <typename ValueType> class GncOptionRangeValue;
class GncOptionCommodityValue;
class GncOptionDateValue;
using GncOptionReportPlacement = std::tuple<uint32_t, uint32_t, uint32_t>;
using GncOptionReportPlacementVec = std::vector<GncOptionReportPlacement>;
template <typename T>
struct is_OptionClassifier
{
    static constexpr bool value =
        std::is_base_of_v<OptionClassifier, std::decay_t<T>>;
};

template <typename T> inline constexpr bool
is_OptionClassifier_v = is_OptionClassifier<T>::value;

template <typename T, typename U>
struct is_same_decayed
{
    static constexpr bool value = std::is_same_v<std::decay_t<T>,
                                                 std::decay_t<U>>;
};

template <typename T, typename U> inline constexpr bool
is_same_decayed_v = is_same_decayed<T, U>::value;

template <typename T>
struct is_RangeValue
{
    static constexpr bool value =
         (is_same_decayed_v<T, GncOptionRangeValue<int>> ||
          is_same_decayed_v<T, GncOptionRangeValue<double>>);
};

template <typename T> inline constexpr bool
is_RangeValue_v = is_RangeValue<T>::value;


using GncOptionVariant = std::variant<GncOptionValue<std::string>,
                                      GncOptionValue<bool>,
                                      GncOptionValue<int64_t>,
                                      GncOptionQofInstanceValue,
                                      GncOptionGncOwnerValue,
                                      GncOptionValue<const QofQuery*>,
                                      GncOptionValue<GncOptionReportPlacementVec>,
                                      GncOptionAccountListValue,
                                      GncOptionAccountSelValue,
                                      GncOptionMultichoiceValue,
                                      GncOptionRangeValue<int>,
                                      GncOptionRangeValue<double>,
                                      GncOptionCommodityValue,
                                      GncOptionDateValue>;

using GncOptionVariantPtr = std::unique_ptr<GncOptionVariant>;

enum class GncOptionMultichoiceKeyType
{
    SYMBOL,
    STRING,
    NUMBER,
};

/** @class GncOption
 *  @brief Represents the public interface for an option.
 *  Polymorphism is provided by a std::variant member containing GncOptionValue
 *  types.
*/

class GncOption
{
public:
    template <typename OptionType,
              typename std::enable_if_t<is_OptionClassifier_v<OptionType>,
                               int>  = 0>

    GncOption(OptionType option) :
        m_option{std::make_unique<GncOptionVariant>(option)} {}
    template <typename ValueType,
              typename std::enable_if_t<!is_OptionClassifier_v<ValueType>,
                               int>  = 0>
    GncOption(const char* section, const char* name,
              const char* key, const char* doc_string,
              ValueType value,
              GncOptionUIType ui_type = GncOptionUIType::INTERNAL);
    template <typename ValueType> void set_value(ValueType value);
    template <typename ValueType> void set_default_value(ValueType value);
    template <typename ValueType> ValueType get_default_value() const;
    template <typename ValueType> ValueType get_value() const;
    void reset_default_value();

    const std::string& get_section() const;
    const std::string& get_name() const;
    const std::string& get_key() const;
    const std::string& get_docstring() const;
    void set_ui_item(GncOptionUIItemPtr&& ui_elem);
    const GncOptionUIType get_ui_type() const;
    void set_ui_item_selectable(bool) const noexcept;
    GncOptionUIItem* const get_ui_item() const;
    void set_ui_item_from_option();
    void set_option_from_ui_item();
    void make_internal();
    bool is_internal();
    bool is_changed() const noexcept;
/** @returns false unless m_option contains a GncOptionMultiselectValue or
 * GncOptionAccountListValue for which multiple selections have been enabled.
 */
    bool is_multiselect() const noexcept;
/** Implemented only for GncOptionNumericRange */
    template <typename ValueType> void get_limits(ValueType&, ValueType&,
                                                  ValueType&) const noexcept;
/** Not implemented for GncOptionValue. */
    template <typename ValueType> bool validate(ValueType value) const;
/** Implemented only for GncOptionMultiselectValue. */
    uint16_t num_permissible_values() const;
/** Implemented only for GncOptionMultiselectValue. */
    uint16_t permissible_value_index(const char* value) const;
/** Implemented only for GncOptionMultiselectValue. */
    const char* permissible_value(uint16_t index) const;
/** Implemented only for GncOptionMultiselectValue. */
    const char* permissible_value_name(uint16_t index) const;
/** Implemented only for GncOptionAccountListValue. */
    GList* account_type_list() const noexcept;
    bool is_alternate() const noexcept;
    void set_alternate(bool) noexcept;
/** Get a string suitable for storage representing the option's value.
 *  @return a std::string
 */
    std::string serialize() const;
/** Set the option's value from a character sequence.
 * @param str: The character sequence representing the value
 * @return true if the value was set, false otherwise.
 */
    bool deserialize(const std::string& str);
/** Set the option's value from an input stream
 *  @param iss: An input stream reference.
 *  @return the stream reference for chaining.
 */
    std::istream& in_stream(std::istream& iss);
    friend GncOptionVariant& swig_get_option(GncOption*);
    void set_widget_changed (std::any cb) { m_widget_changed = cb; }
    std::any& get_widget_changed () { return m_widget_changed; }
private:
    inline static const std::string c_empty_string{""};
    GncOptionVariantPtr m_option;
    GncOptionUIItemPtr m_ui_item{nullptr};
    std::any m_widget_changed{};
};

inline bool
operator<(const GncOption& right, const GncOption& left)
{
    return right.get_key() < left.get_key();
}

inline std::ostream&
operator<<(std::ostream& oss, const GncOption& opt)
{
    oss << opt.serialize();
    return oss;
}

inline std::istream&
operator>>(std::istream& iss, GncOption& opt)
{
    return opt.in_stream(iss);
}

inline std::ostream&
output_color_value(std::ostream& oss, const std::string& value)
{
    oss << "'(";
    oss << std::fixed << std::showpoint << std::setprecision(1);
    auto len{value.length() > 8 ? 8 : value.length()};
    for (size_t i{}; i < len; i += 2)
    {
        oss << static_cast<float>(stoi(value.substr(i, 2), nullptr, 16));
        if (i < 6)
            oss << " ";
    }
    if (len < 8)
        oss << 256.0;
    oss << ")";
    return oss;
}

/**
 * Free function wrapping GncOption's constructor. The type of GncOptionValue to
 * create is determined from the UI type. Some GncOptionValue types require more
 * parameters for their constructors and can't be created with this function.
 */
template<typename ValueType> GncOption*
gnc_make_option(const char* section, const char* name,
                const char* key, const char* doc_string,
                ValueType value, GncOptionUIType ui_type)
{
    return new GncOption(section, name, key, doc_string, value, ui_type);
}

#endif //GNC_OPTION_HPP_

/** @}
    @} */
