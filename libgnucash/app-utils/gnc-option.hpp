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

#ifndef GNC_OPTION_HPP_
#define GNC_OPTION_HPP_

extern "C"
{
#include <glib.h>
}

#include <string>
#include <iostream>
#include <variant>
#include <memory>
#include "gnc-option-uitype.hpp"

class GncOptionUIItem;
using GncOptionUIItemPtr = std::unique_ptr<GncOptionUIItem>;
struct QofInstance_s;
using QofInstance = QofInstance_s;
template <typename ValueType> class GncOptionValue;
class GncOptionAccountValue;
class GncOptionMultichoiceValue;
template <typename ValueType> class GncOptionRangeValue;
template <typename ValueType> class GncOptionValidatedValue;
class GncOptionDateValue;

using GncOptionVariant = std::variant<GncOptionValue<std::string>,
                                      GncOptionValue<bool>,
                                      GncOptionValue<int64_t>,
                                      GncOptionValue<const QofInstance*>,
                                      GncOptionAccountValue,
                                      GncOptionMultichoiceValue,
                                      GncOptionRangeValue<int>,
                                      GncOptionRangeValue<double>,
                                      GncOptionValidatedValue<const QofInstance*>,
                                      GncOptionDateValue>;

using GncOptionVariantPtr = std::unique_ptr<GncOptionVariant>;

class GncOption
{
public:
    template <typename OptionType>
    GncOption(OptionType option) :
        m_option{std::make_unique<GncOptionVariant>(option)} {}
    template <typename ValueType>
    GncOption(const char* section, const char* name,
              const char* key, const char* doc_string,
              ValueType value,
              GncOptionUIType ui_type = GncOptionUIType::INTERNAL);
    template <typename ValueType> void set_value(ValueType value);
    template <typename ValueType> ValueType get_default_value() const;
    template <typename ValueType> ValueType get_value() const;

    const std::string& get_section() const;
    const std::string& get_name() const;
    const std::string& get_key() const;
    const std::string& get_docstring() const;
    void set_ui_item(GncOptionUIItemPtr&& ui_elem);
    const GncOptionUIType get_ui_type() const;
    const GncOptionUIItem* get_ui_item() const;
    void set_ui_item_from_option();
    void set_option_from_ui_item();
    void make_internal();
    bool is_changed() const noexcept;
    template <typename ValueType> bool validate(ValueType value) const;
    std::size_t num_permissible_values() const;
    std::size_t permissible_value_index(const char* value) const;
    const char* permissible_value(std::size_t index) const;
    const char* permissible_value_name(std::size_t index) const;
    const char* permissible_value_description(std::size_t index) const;
    GList* account_type_list() const noexcept;
    std::ostream& out_stream(std::ostream& oss) const;
    std::istream& in_stream(std::istream& iss);
    std::ostream& to_scheme(std::ostream& oss) const;
    std::istream& from_scheme(std::istream& iss);


    friend GncOptionVariant& swig_get_option(GncOption*);

private:
    inline static const std::string c_empty_string{""};
    GncOptionVariantPtr m_option;
    GncOptionUIItemPtr m_ui_item{nullptr};
};

inline std::ostream&
operator<<(std::ostream& oss, const GncOption& opt)
{
    return opt.out_stream(oss);
}

inline std::istream&
operator>>(std::istream& iss, GncOption& opt)
{
    return opt.in_stream(iss);
}
/**
 * Reporting periods relative to the current date.
 *
 * The original design allowed custom RelativeDatePeriods, but that facility is
 * unused so we'll go with compiled-in enums.
 */
enum class RelativeDatePeriod : int
{
    ABSOLUTE = -1,
    TODAY,
    START_THIS_MONTH,
    END_THIS_MONTH,
    START_PREV_MONTH,
    END_PREV_MONTH,
    START_CURRENT_QUARTER,
    END_CURRENT_QUARTER,
    START_PREV_QUARTER,
    END_PREV_QUARTER,
    START_CAL_YEAR,
    END_CAL_YEAR,
    START_PREV_YEAR,
    END_PREV_YEAR,
    START_ACCOUNTING_PERIOD,
    END_ACCOUNTING_PERIOD
};


#endif //GNC_OPTION_HPP_
