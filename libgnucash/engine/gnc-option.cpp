/********************************************************************\
 * gnc-option.cpp -- Application options system                     *
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

#include "gnc-option.hpp"
#include "gnc-option-impl.hpp"
#include "gnc-option-uitype.hpp"
#include "gnc-option-ui.hpp"
#include "gncOwner.h"
#include "kvp-value.hpp"

static const char* log_module{"gnc.engine.gnc-option"};

#include "qoflog.h"

template <typename ValueType,
          typename std::enable_if_t<!is_OptionClassifier_v<ValueType>,
                                    int>>
GncOption::GncOption(const char* section, const char* name,
                     const char* key, const char* doc_string,
                     ValueType value, GncOptionUIType ui_type) :
    m_option{std::make_unique<GncOptionVariant>(
            std::in_place_type<GncOptionValue<ValueType>>,
            section, name, key, doc_string, value, ui_type)}
{
}

template <typename ValueType> ValueType
GncOption::get_value() const
{
    return std::visit(
        [](const auto& option)->ValueType {
            if constexpr (is_same_decayed_v<decltype(option.get_value()),
                          ValueType>)
                return option.get_value();
            if constexpr (is_same_decayed_v<decltype(option),
                          GncOptionDateValue>)
            {
                if constexpr (is_same_decayed_v<ValueType,
                              RelativeDatePeriod>)
                    return option.get_period();
                if constexpr (std::is_same_v<ValueType, time64>)
                    return option.get_value();
                if constexpr (std::is_same_v<ValueType, uint16_t>)
                    return option.get_period_index();
                return ValueType{};
            }
            if constexpr (is_same_decayed_v<decltype(option),
                          GncOptionMultichoiceValue>)
            {
                if constexpr (std::is_same_v<ValueType, uint16_t>)
                    return option.get_index();
                if constexpr (is_same_decayed_v<ValueType,
                              GncMultichoiceOptionIndexVec>)
                    return option.get_multiple();
            }
            return ValueType {};
        }, *m_option);
}

template <typename ValueType> ValueType
GncOption::get_default_value() const
{
    return std::visit(
        [](const auto& option)->ValueType {
            if constexpr (is_same_decayed_v<decltype(option.get_value()),
                          ValueType>)
                return option.get_default_value();
            if constexpr (is_same_decayed_v<decltype(option),
                          GncOptionDateValue>)
            {
                if constexpr (is_same_decayed_v<ValueType,
                              RelativeDatePeriod>)
                    return option.get_default_period();
                if constexpr (std::is_same_v<ValueType, time64>)
                    return option.get_value();
                if constexpr (std::is_same_v<ValueType, uint16_t>)
                    return option.get_default_period_index();
                return ValueType{};
            }
            if constexpr
                (is_same_decayed_v<decltype(option),
                 GncOptionMultichoiceValue> &&
                 is_same_decayed_v<ValueType,
                 GncMultichoiceOptionIndexVec>)
                return option.get_default_multiple();
            return ValueType {};
        }, *m_option);

}

template <typename ValueType> void
GncOption::set_value(ValueType value)
{
    std::visit(
        [value](auto& option) {
            if constexpr
                (is_same_decayed_v<decltype(option.get_value()), ValueType> ||
                 is_same_decayed_v<decltype(option), GncOptionDateFormat> ||
                 (is_same_decayed_v<decltype(option),
                  GncOptionDateValue> &&
                  (is_same_decayed_v<ValueType, RelativeDatePeriod> ||
                   std::is_same_v<ValueType, time64> ||
                   std::is_same_v<ValueType, uint16_t>)))
                option.set_value(value);
           else if constexpr (is_same_decayed_v<decltype(option),
                          GncOptionMultichoiceValue>)
            {
                if constexpr (is_same_decayed_v<ValueType,
                              GncMultichoiceOptionIndexVec>)
                    option.set_multiple(value);
                else if constexpr
                    (std::is_same_v<ValueType, uint16_t> ||
                     is_same_decayed_v<ValueType, std::string> ||
                     std::is_same_v<std::remove_cv<ValueType>, char*>)
                    option.set_value(value);
            }
            else
                PWARN("No set_value handler: get_value returns %s, value_type is %s",
                      typeid(option.get_value()).name(), typeid(value).name());
        }, *m_option);
}

template <typename ValueType> void
GncOption::set_default_value(ValueType value)
{
    std::visit(
        [value](auto& option) {
            if constexpr
                (is_same_decayed_v<decltype(option.get_value()), ValueType>||
                 is_same_decayed_v<decltype(option), GncOptionDateFormat> ||
                 (is_same_decayed_v<decltype(option), GncOptionDateValue> &&
                  (is_same_decayed_v<ValueType, RelativeDatePeriod> ||
                   std::is_same_v<ValueType, time64> ||
                   std::is_same_v<ValueType, uint16_t>)))
                option.set_default_value(value);
            if constexpr (is_same_decayed_v<decltype(option),
                          GncOptionMultichoiceValue>)
            {
                if constexpr (is_same_decayed_v<ValueType,
                              GncMultichoiceOptionIndexVec>)
                    option.set_default_multiple(value);
                else if constexpr
                    (std::is_same_v<ValueType, uint16_t> ||
                     is_same_decayed_v<ValueType, std::string> ||
                     std::is_same_v<std::remove_cv<ValueType>, char*>)
                    option.set_default_value(value);
            }
        }, *m_option);
}
void
GncOption::reset_default_value()
{
    std::visit([](auto& option) { option.reset_default_value(); }, *m_option);
}

template <typename ValueType> void
GncOption::get_limits(ValueType& max, ValueType& min, ValueType& step) const noexcept
{
    std::visit([&max, &min, &step](const auto& option) {
                   if constexpr
                       (is_same_decayed_v<decltype(option),
                        GncOptionRangeValue<ValueType>>)
                       option.get_limits(max, min, step);
               }, *m_option);
}

const std::string&
GncOption::get_section() const
{
    return std::visit([](const auto& option)->const std::string& {
                          return option.m_section;
                      }, *m_option);
}

const std::string&
GncOption::get_name() const
{
    return std::visit([](const auto& option)->const std::string& {
                          return option.m_name;
                      }, *m_option);
}

const std::string&
GncOption::get_key() const
{
    return std::visit([](const auto& option)->const std::string& {
                          return option.m_sort_tag;
                      }, *m_option);
}

const std::string&
GncOption::get_docstring() const
{
    return std::visit([](const auto& option)->const std::string& {
                          return option.m_doc_string;
                      }, *m_option);
}

void
GncOption::set_ui_item(GncOptionUIItemPtr&& ui_item)
{

    auto opt_ui_type = std::visit([](const auto& option)->GncOptionUIType {
                                      return option.get_ui_type();
                                  }, *m_option);

    //ui_item may be nullptr to free the old m_ui_item.
    if (ui_item && ui_item->get_ui_type() != opt_ui_type)
    {
        PERR("Setting option %s:%s UI element failed, mismatched UI types.",
              get_section().c_str(), get_name().c_str());
        return;
    }

    m_ui_item = std::move(ui_item);
}

void
GncOption::set_ui_item_selectable(bool selectable) const noexcept
{
    if (m_ui_item)
        m_ui_item->set_selectable(selectable);
}

const GncOptionUIType
GncOption::get_ui_type() const
{
    return std::visit([](const auto& option)->GncOptionUIType {
                          return option.get_ui_type();
                      }, *m_option);
}

GncOptionUIItem* const
GncOption::get_ui_item() const
{
    return m_ui_item.get();
}

void
GncOption::set_ui_item_from_option()
{
    if (!m_ui_item)
        return;
    m_ui_item->set_ui_item_from_option(*this);
}

void
GncOption::set_option_from_ui_item()
{
    if (!m_ui_item)
        return;
    m_ui_item->set_option_from_ui_item(*this);
}

void
GncOption::make_internal()
{
    if (m_ui_item)
    {
        PERR("Option %s:%s has a UI Element, can't be INTERNAL.",
             get_section().c_str(), get_name().c_str());
        return;
    }
    std::visit([](auto& option) {
                   option.make_internal();
               }, *m_option);
}

bool
GncOption::is_internal()
{
    return std::visit([](auto& option)->bool {
                   return option.is_internal();
               }, *m_option);
}

void
GncOption::mark_saved() noexcept
{
    std::visit([](auto& option)->void {
                          option.mark_saved();
                      }, *m_option);
}

bool
GncOption::is_dirty() const noexcept
{
    return std::visit([](const auto& option)->bool {
                          return option.is_dirty();
                      }, *m_option);
}

bool
GncOption::is_changed() const noexcept
{
    return std::visit([](const auto& option)->bool {
                          return option.is_changed();
                      }, *m_option);
}

bool
GncOption::is_multiselect() const noexcept
{
    return std::visit(
        [](const auto& option)->bool {
            if constexpr (is_same_decayed_v<decltype(option),
                          GncOptionAccountListValue>)
                return option.is_multiselect();
            else
                return false;
        }, *m_option);
}

template<typename ValueType> bool
GncOption::validate(ValueType value) const
{
    return std::visit(
        [value] (const auto& option) -> bool {
            if constexpr ((is_same_decayed_v<decltype(option),
                                             GncOptionMultichoiceValue> &&
                           is_same_decayed_v<ValueType, std::string>) ||
                          (is_same_decayed_v<decltype(option),
                                             GncOptionMultichoiceValue> &&
                           is_same_decayed_v<ValueType,
                                             GncMultichoiceOptionIndexVec>) ||
                          (is_same_decayed_v<decltype(option),
                                             GncOptionCommodityValue> &&
                           is_same_decayed_v<ValueType, gnc_commodity*>))
                return option.validate(value);
            else
                return false;
        }, *m_option);
}

std::uint16_t
GncOption::num_permissible_values() const
{
    return std::visit(
        [] (const auto& option) -> uint16_t {
            if constexpr (is_same_decayed_v<decltype(option),
                          GncOptionMultichoiceValue>  ||
                          is_same_decayed_v<decltype(option),
                          GncOptionDateValue>)
                return option.num_permissible_values();
            else
                return uint16_t_max;
        }, *m_option);
}

std::uint16_t
GncOption::permissible_value_index(const char* value) const
{
    return std::visit(
        [&value] (const auto& option) -> uint16_t {
            if constexpr (is_same_decayed_v<decltype(option),
                                            GncOptionMultichoiceValue> ||
                          is_same_decayed_v<decltype(option),
                                            GncOptionDateValue>)
                return option.permissible_value_index(value);
            else
                return uint16_t_max;
        }, *m_option);
}

const char*
GncOption::permissible_value(std::uint16_t index) const
{
    return std::visit([index] (const auto& option) -> const char* {
                          if constexpr (std::is_same_v<std::decay_t<decltype(option)>,
                                        GncOptionMultichoiceValue>  ||
                                        std::is_same_v<std::decay_t<decltype(option)>,
                                        GncOptionDateValue>)
                                           return option.permissible_value(index);
                          else
                              return "";
                      }, *m_option);
}

const char*
GncOption::permissible_value_name(std::uint16_t index) const
{
    return std::visit([index] (const auto& option) -> const char* {
                          if constexpr (std::is_same_v<std::decay_t<decltype(option)>,
                                        GncOptionMultichoiceValue>  ||
                                        std::is_same_v<std::decay_t<decltype(option)>,
                                        GncOptionDateValue>)
                                           return option.permissible_value_name(index);
                          else
                              return "";
                      }, *m_option);
}

GList*
GncOption::account_type_list() const noexcept
{
    return std::visit([] (const auto& option) -> GList* {
                          if constexpr (std::is_same_v<std::decay_t<decltype(option)>,
                                        GncOptionAccountListValue>)
                              return option.account_type_list();
                          else
                              return nullptr;
                      }, *m_option);
}

bool
GncOption::is_alternate() const noexcept
{
    return std::visit([](auto& option) -> bool {
                          if constexpr(is_RangeValue_v<decltype(option)>)
                              return option.is_alternate();
                          return false;
                      }, *m_option);
}

void
GncOption::set_alternate(bool alt) noexcept
{
    std::visit([alt](auto& option) {
                   if constexpr(is_RangeValue_v<decltype(option)>)
                       option.set_alternate(alt);
               }, *m_option);
}

std::string
GncOption::serialize() const
{
    if (m_option->valueless_by_exception())
        return "Valueless Option";
    return std::visit([&](auto& option) -> std::string {
                          return option.serialize();
                      }, *m_option);
}

bool
GncOption::deserialize(const std::string& str)
{
    return std::visit([&str](auto& option) -> bool {
                          return option.deserialize(str);
                      }, *m_option);
}

std::istream&
GncOption::in_stream(std::istream& iss)
{
    return std::visit([&iss](auto& option) -> std::istream& {
                          iss >> option;
                          return iss;
                      }, *m_option);
}

/* We must instantiate all of the templates we need here because we don't expose
 * the template implementation in the public header.
 */


template GncOption::GncOption(const char*, const char*, const char*,
                              const char*, bool, GncOptionUIType);
//template GncOption::GncOption(const char*, const char*, const char*,
//                              const char*, int, GncOptionUIType);
template GncOption::GncOption(const char*, const char*, const char*,
                              const char*, int64_t, GncOptionUIType);
//template GncOption::GncOption(const char*, const char*, const char*,
//                              const char*, const char*, GncOptionUIType);
//template GncOption::GncOption(const char*, const char*, const char*,
//                              const char*, double, GncOptionUIType);
template GncOption::GncOption(const char*, const char*, const char*,
                              const char*, std::string, GncOptionUIType);
template GncOption::GncOption(const char*, const char*, const char*,
                              const char*, const QofQuery*, GncOptionUIType);

template GncOption::GncOption(const char *, const char*, const char*,
                              const char *, GncOptionDateFormat,
                              GncOptionUIType);

template bool GncOption::get_value<bool>() const;
template int GncOption::get_value<int>() const;
template int64_t GncOption::get_value<int64_t>() const;
template double GncOption::get_value<double>() const;
template uint16_t GncOption::get_value<uint16_t>() const;
template const char* GncOption::get_value<const char*>() const;
template std::string GncOption::get_value<std::string>() const;
template const QofInstance* GncOption::get_value<const QofInstance*>() const;
template const GncOwner* GncOption::get_value<const GncOwner*>() const;
template gnc_commodity* GncOption::get_value<gnc_commodity*>() const;
template const Account* GncOption::get_value<const Account*>() const;
template RelativeDatePeriod GncOption::get_value<RelativeDatePeriod>() const;
template GncOptionAccountList GncOption::get_value<GncOptionAccountList>() const;
template GncMultichoiceOptionIndexVec GncOption::get_value<GncMultichoiceOptionIndexVec>() const;
template GncOptionReportPlacementVec GncOption::get_value<GncOptionReportPlacementVec>() const;
template GncOptionDateFormat GncOption::get_value<GncOptionDateFormat>() const;

template bool GncOption::get_default_value<bool>() const;
template int GncOption::get_default_value<int>() const;
template int64_t GncOption::get_default_value<int64_t>() const;
template double GncOption::get_default_value<double>() const;
template const char* GncOption::get_default_value<const char*>() const;
template std::string GncOption::get_default_value<std::string>() const;
template const QofInstance* GncOption::get_default_value<const QofInstance*>() const;
template gnc_commodity* GncOption::get_default_value<gnc_commodity*>() const;
template const Account* GncOption::get_default_value<const Account*>() const;
template RelativeDatePeriod GncOption::get_default_value<RelativeDatePeriod>() const;
template GncOptionAccountList GncOption::get_default_value<GncOptionAccountList>() const;
template GncMultichoiceOptionIndexVec GncOption::get_default_value<GncMultichoiceOptionIndexVec>() const;
template GncOptionReportPlacementVec GncOption::get_default_value<GncOptionReportPlacementVec>() const;
template GncOptionDateFormat GncOption::get_default_value<GncOptionDateFormat>() const;

template void GncOption::set_value(bool);
template void GncOption::set_value(int);
template void GncOption::set_value(int64_t);
template void GncOption::set_value(double);
template void GncOption::set_value(char*);
template void GncOption::set_value(const char*);
template void GncOption::set_value(std::string);
template void GncOption::set_value(const QofInstance*);
template void GncOption::set_value(const GncOwner*);
template void GncOption::set_value(gnc_commodity*);
template void GncOption::set_value(const Account*);
template void GncOption::set_value(RelativeDatePeriod);
template void GncOption::set_value(uint16_t);
template void GncOption::set_value(GncOptionAccountList);
template void GncOption::set_value(GncMultichoiceOptionIndexVec);
template void GncOption::set_value(GncOptionReportPlacementVec);
template void GncOption::set_value(GncOptionDateFormat);

template void GncOption::set_default_value(bool);
template void GncOption::set_default_value(int);
template void GncOption::set_default_value(int64_t);
template void GncOption::set_default_value(double);
template void GncOption::set_default_value(char*);
template void GncOption::set_default_value(const char*);
template void GncOption::set_default_value(std::string);
template void GncOption::set_default_value(const QofInstance*);
template void GncOption::set_default_value(const Account*);
template void GncOption::set_default_value(RelativeDatePeriod);
template void GncOption::set_default_value(uint16_t);
template void GncOption::set_default_value(GncOptionAccountList);
template void GncOption::set_default_value(GncMultichoiceOptionIndexVec);
template void GncOption::set_default_value(GncOptionReportPlacementVec);
template void GncOption::set_default_value(GncOptionDateFormat);

template void GncOption::get_limits(double&, double&, double&) const noexcept;
template void GncOption::get_limits(int&, int&, int&) const noexcept;
template bool GncOption::validate(bool) const;
template bool GncOption::validate(int) const;
template bool GncOption::validate(int64_t) const;
template bool GncOption::validate(double) const;
template bool GncOption::validate(const char*) const;
template bool GncOption::validate(std::string) const;
template bool GncOption::validate(const QofInstance*) const;
template bool GncOption::validate(gnc_commodity*) const;
template bool GncOption::validate(const Account*) const;
template bool GncOption::validate(const QofQuery*) const;
template bool GncOption::validate(RelativeDatePeriod) const;
template bool GncOption::validate(GncMultichoiceOptionIndexVec) const;
template bool GncOption::validate(GncOptionReportPlacementVec) const;
template bool GncOption::validate(GncOptionDateFormat) const;

template GncOption* gnc_make_option<const std::string&>(const char*,
                                                        const char*,
                                                        const char*,
                                                        const char*,
                                                        const std::string&,
                                                        GncOptionUIType);
template GncOption* gnc_make_option<bool>(const char*, const char*, const char*,
                                          const char*, bool, GncOptionUIType);
template GncOption* gnc_make_option<int64_t>(const char*, const char*,
                                             const char*, const char*, int64_t,
                                             GncOptionUIType);
