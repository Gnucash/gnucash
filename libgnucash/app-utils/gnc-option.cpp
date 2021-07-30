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

static const char* log_module{"gnc.app-utils.gnc-option"};

extern "C"
{
#include <qoflog.h>
}

template <typename ValueType,
          typename std::enable_if_t<!std::is_base_of_v<OptionClassifier,
                                                       std::decay_t<ValueType>>,
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
    return std::visit([](const auto option)->ValueType {
                          if constexpr (std::is_same_v<std::decay_t<decltype(option.get_value())>, std::decay_t<ValueType>>)
                                           return option.get_value();
                          if constexpr (std::is_same_v<std::decay_t<decltype(option)>,
                                        GncOptionDateValue> &&
                                        std::is_same_v<std::decay_t<ValueType>,
                                        RelativeDatePeriod>)
                              return option.get_period();
                          if constexpr (std::is_same_v<std::decay_t<decltype(option)>,
                                        GncOptionDateValue> &&
                                        std::is_same_v<std::decay_t<ValueType>,
                                        size_t>)
                              return option.get_period_index();
                          if constexpr
                              (std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionMultichoiceValue> &&
                               std::is_same_v<std::decay_t<ValueType>,
                               size_t>)
                              return option.get_index();
                          if constexpr
                              (std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionMultichoiceValue> &&
                               std::is_same_v<std::decay_t<ValueType>,
                               GncMultichoiceOptionIndexVec>)
                              return option.get_multiple();
                          return ValueType {};
                      }, *m_option);
}

template <typename ValueType> ValueType
GncOption::get_default_value() const
{
    return std::visit([](const auto option)->ValueType {
                          if constexpr (std::is_same_v<std::decay_t<decltype(option.get_value())>, std::decay_t<ValueType>>)
                                           return option.get_default_value();
                          if constexpr (std::is_same_v<std::decay_t<decltype(option)>,
                                        GncOptionDateValue> &&
                                        std::is_same_v<std::decay_t<ValueType>,
                                        RelativeDatePeriod>)
                              return option.get_default_period();
                          if constexpr (std::is_same_v<std::decay_t<decltype(option)>,
                                        GncOptionDateValue> &&
                                        std::is_same_v<std::decay_t<ValueType>,
                                        size_t>)
                              return option.get_default_period_index();
                          if constexpr
                              (std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionMultichoiceValue> &&
                               std::is_same_v<std::decay_t<ValueType>,
                               GncMultichoiceOptionIndexVec>)
                              return option.get_default_multiple();
                          return ValueType {};
                      }, *m_option);

}

template <typename ValueType> void
GncOption::set_value(ValueType value)
{
    std::visit([value](auto& option) {
                   if constexpr
                       (std::is_same_v<std::decay_t<decltype(option.get_value())>,
                        std::decay_t<ValueType>> ||
                        (std::is_same_v<std::decay_t<decltype(option)>,
                         GncOptionDateValue> &&
                         (std::is_same_v<std::decay_t<ValueType>,
                         RelativeDatePeriod> ||
                          std::is_same_v<std::decay_t<ValueType>, size_t>)))
                           option.set_value(value);
                   if constexpr
                       (std::is_same_v<std::decay_t<decltype(option)>,
                        GncOptionMultichoiceValue> &&
                        std::is_same_v<std::decay_t<ValueType>,
                        GncMultichoiceOptionIndexVec>)
                       option.set_multiple(value);
               }, *m_option);
}

template <typename ValueType> void
GncOption::set_default_value(ValueType value)
{
    std::visit([value](auto& option) {
                   if constexpr
                       (std::is_same_v<std::decay_t<decltype(option.get_value())>,
                        std::decay_t<ValueType>> ||
                        (std::is_same_v<std::decay_t<decltype(option)>,
                         GncOptionDateValue> &&
                         (std::is_same_v<std::decay_t<ValueType>,
                         RelativeDatePeriod> ||
                          std::is_same_v<std::decay_t<ValueType>, size_t>)))
                           option.set_default_value(value);
                   if constexpr
                       (std::is_same_v<std::decay_t<decltype(option)>,
                        GncOptionMultichoiceValue> &&
                        std::is_same_v<std::decay_t<ValueType>,
                        GncMultichoiceOptionIndexVec>)
                       option.set_multiple(value);
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
                       (std::is_same_v<std::decay_t<decltype(option)>,
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

    if (ui_item->get_ui_type() != opt_ui_type)
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
GncOption::is_changed() const noexcept
{
    return std::visit([](const auto& option)->bool {
                          return option.is_changed();
                      }, *m_option);
}

bool
GncOption::is_multiselect() const noexcept
{
    return std::visit([](const auto& option)->bool {
                          if constexpr (std::is_same_v<std::decay_t<decltype(option)>,
                                        GncOptionAccountValue>)
                              return option.is_multiselect();
                          else
                              return false;
                      }, *m_option);
}

template<typename ValueType> bool
GncOption::validate(ValueType value) const
{
    return std::visit([value] (const auto& option) -> bool {
                          if constexpr ((std::is_same_v<std::decay_t<decltype(option)>,
                                         GncOptionMultichoiceValue> &&
                                         std::is_same_v<std::decay_t<ValueType>,
                                         std::string>) ||
                                        (std::is_same_v<std::decay_t<decltype(option)>,
                                         GncOptionMultichoiceValue> &&
                                         std::is_same_v<std::decay_t<ValueType>,
                                         GncMultichoiceOptionIndexVec>) ||
                                        std::is_same_v<std::decay_t<decltype(option)>,
                                        GncOptionValidatedValue<ValueType>>)
                                           return option.validate(value);
                          else
                              return false;
                      }, *m_option);
}

std::size_t
GncOption::num_permissible_values() const
{
    return std::visit([] (const auto& option) -> size_t {
                          if constexpr (std::is_same_v<std::decay_t<decltype(option)>,
                                        GncOptionMultichoiceValue>  ||
                                        std::is_same_v<std::decay_t<decltype(option)>,
                                        GncOptionDateValue>)
                                           return option.num_permissible_values();
                          else
                              return size_t_max;
                      }, *m_option);
}

std::size_t
GncOption::permissible_value_index(const char* value) const
{
    return std::visit([&value] (const auto& option) -> size_t {
                          if constexpr (std::is_same_v<std::decay_t<decltype(option)>,
                                        GncOptionMultichoiceValue>  ||
                                        std::is_same_v<std::decay_t<decltype(option)>,
                                        GncOptionDateValue>)
                                           return option.permissible_value_index(value);
                          else
                              return size_t_max;;
                      }, *m_option);
}

const char*
GncOption::permissible_value(std::size_t index) const
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
GncOption::permissible_value_name(std::size_t index) const
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
                                        GncOptionAccountValue>)
                              return option.account_type_list();
                          else
                              return nullptr;
                      }, *m_option);
}

bool
GncOption::is_alternate() const noexcept
{
    return std::visit([](auto& option) -> bool {
                          if constexpr(std::is_same_v<std::decay_t<decltype(option)>,
                                       GncOptionRangeValue<int>> ||
                       std::is_same_v<std::decay_t<decltype(option)>,
                                       GncOptionRangeValue<double>>)
                              return option.is_alternate();
                          return false;
                      }, *m_option);
}

void
GncOption::set_alternate(bool alt) noexcept
{
    std::visit([alt](auto& option) {
                   if constexpr(std::is_same_v<std::decay_t<decltype(option)>,
                                GncOptionRangeValue<int>> ||
                       std::is_same_v<std::decay_t<decltype(option)>,
                                GncOptionRangeValue<double>>)
                       option.set_alternate(alt);
               }, *m_option);
}

std::ostream&
GncOption::out_stream(std::ostream& oss) const
{
    return std::visit([&oss](auto& option) -> std::ostream& {
                          oss << option;
                          return oss;
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

std::ostream&
GncOption::to_scheme(std::ostream& oss) const
{
    return std::visit([&oss](auto& option) ->std::ostream& {
                          if constexpr
                              ((std::is_same_v<std::decay_t<decltype(option)>,
                                GncOptionAccountValue>) ||
                               (std::is_same_v<std::decay_t<decltype(option)>,
                                GncOptionMultichoiceValue>) ||
                               std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionValue<const QofInstance*>> ||
                               std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionValidatedValue<const QofInstance*>>)
                              gnc_option_to_scheme(oss, option);
                          else if constexpr
                              (std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionDateValue>)
                                  oss << "'(" << option << ")";
                          else if constexpr
                              (std::is_same_v<std::decay_t<decltype(option.get_value())>,
                               std::string>)
                                  oss << '"' << option << '"';
                          else if constexpr
                              (std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionRangeValue<int>> ||
                               std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionRangeValue<double>>)
                          {
                              if (option.get_ui_type() == GncOptionUIType::PLOT_SIZE)
                                  oss << "'(" << (option.is_alternate() ?
                                          "\"percent\" . " : "\"pixels\" . ");
                              oss << option.get_value();
                              if (option.get_ui_type() == GncOptionUIType::PLOT_SIZE)
                                  oss << ")";
                          }
                          else
                              oss << option;
                          return oss;
                      }, *m_option);
}

std::istream&
GncOption::from_scheme(std::istream& iss)
{
    return std::visit([&iss](auto& option) -> std::istream& {
                          if constexpr
                              ((std::is_same_v<std::decay_t<decltype(option)>,
                                GncOptionAccountValue>) ||
                               (std::is_same_v<std::decay_t<decltype(option)>,
                                GncOptionMultichoiceValue>) ||
                               std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionValue<const GncOwner*>> ||
                               std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionValue<const QofQuery*>> ||
                               std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionValue<const QofInstance*>> ||
                               std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionValidatedValue<const QofQuery*>> ||
                               std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionValidatedValue<const QofInstance*>>)
                              gnc_option_from_scheme(iss, option);
                          else if constexpr
                              (std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionDateValue>)
                          {
                              iss.ignore(2, '(');
                              iss >> option;
                              //operator >> clears the trailing ')'
                          }
                          else if constexpr
                              (std::is_same_v<std::decay_t<decltype(option.get_value())>,
                               std::string>)
                          {
                              iss.ignore(1, '"');
                              std::string input;
                              std::getline(iss, input, '"');
                              option.set_value(input);
                          }
                          else if constexpr
                              (std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionRangeValue<int>> ||
                               std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionRangeValue<double>>)
                          {
                              if (option.get_ui_type() == GncOptionUIType::PLOT_SIZE)
                              {
                                  iss.ignore(3, '"');
                                  std::string alt;
                                  iss >> alt;
                                  option.set_alternate(
                                      strncmp(alt.c_str(), "pixels",
                                              strlen("pixels")) == 0);
                                  iss.ignore(4, ' ');
                              }
                              if constexpr(std::is_same_v<std::decay_t<decltype(option)>,
                                           GncOptionRangeValue<int>>)
                              {
                                  int val;
                                  iss >> val;
                                  option.set_value(val);
                              }
                              else
                              {
                                  double val;
                                  iss >> val;
                                  option.set_value(val);
                              }
                          }
                          else
                              iss >> option;
                          return iss;
                      }, *m_option);
}

GncOption*
gnc_make_SCM_option(const char* section, const char* name,
                    const char* key, const char* doc_string,
                    SCM value, GncOptionUIType ui_type)
{
    return new GncOption(section, name, key, doc_string,
                         reinterpret_cast<SCM>(value), ui_type);
}


/* We must instantiate all of the templates we need here because we don't expose
 * the template implementation in the public header.
 */


template class GncOptionValidatedValue<const QofInstance*>;

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
                              const char*, const QofInstance*, GncOptionUIType);
template GncOption::GncOption(const char*, const char*, const char*,
                              const char*, SCM, GncOptionUIType);
template GncOption::GncOption(const char*, const char*, const char*,
                              const char*, const QofQuery*, GncOptionUIType);
template GncOption::GncOption(const char*, const char*, const char*,
                              const char*, const GncOwner*, GncOptionUIType);

template bool GncOption::get_value<bool>() const;
template int GncOption::get_value<int>() const;
template int64_t GncOption::get_value<int64_t>() const;
template double GncOption::get_value<double>() const;
template size_t GncOption::get_value<size_t>() const;
template const char* GncOption::get_value<const char*>() const;
template std::string GncOption::get_value<std::string>() const;
template const QofInstance* GncOption::get_value<const QofInstance*>() const;
template RelativeDatePeriod GncOption::get_value<RelativeDatePeriod>() const;
template GncOptionAccountList GncOption::get_value<GncOptionAccountList>() const;
template GncMultichoiceOptionIndexVec GncOption::get_value<GncMultichoiceOptionIndexVec>() const;
template SCM GncOption::get_value<SCM>() const;

template bool GncOption::get_default_value<bool>() const;
template int GncOption::get_default_value<int>() const;
template int64_t GncOption::get_default_value<int64_t>() const;
template double GncOption::get_default_value<double>() const;
template const char* GncOption::get_default_value<const char*>() const;
template std::string GncOption::get_default_value<std::string>() const;
template const QofInstance* GncOption::get_default_value<const QofInstance*>() const;
template RelativeDatePeriod GncOption::get_default_value<RelativeDatePeriod>() const;
template GncOptionAccountList GncOption::get_default_value<GncOptionAccountList>() const;
template GncMultichoiceOptionIndexVec GncOption::get_default_value<GncMultichoiceOptionIndexVec>() const;
template SCM GncOption::get_default_value<SCM>() const;

template void GncOption::set_value(bool);
template void GncOption::set_value(int);
template void GncOption::set_value(int64_t);
template void GncOption::set_value(double);
template void GncOption::set_value(char*);
template void GncOption::set_value(const char*);
template void GncOption::set_value(std::string);
template void GncOption::set_value(const QofInstance*);
template void GncOption::set_value(RelativeDatePeriod);
template void GncOption::set_value(size_t);
template void GncOption::set_value(GncOptionAccountList);
template void GncOption::set_value(GncMultichoiceOptionIndexVec);
template void GncOption::set_value(SCM);

template void GncOption::set_default_value(bool);
template void GncOption::set_default_value(int);
template void GncOption::set_default_value(int64_t);
template void GncOption::set_default_value(double);
template void GncOption::set_default_value(char*);
template void GncOption::set_default_value(const char*);
template void GncOption::set_default_value(std::string);
template void GncOption::set_default_value(const QofInstance*);
template void GncOption::set_default_value(RelativeDatePeriod);
template void GncOption::set_default_value(size_t);
template void GncOption::set_default_value(GncOptionAccountList);
template void GncOption::set_default_value(GncMultichoiceOptionIndexVec);
template void GncOption::set_default_value(SCM);

template void GncOption::get_limits(double&, double&, double&) const noexcept;
template void GncOption::get_limits(int&, int&, int&) const noexcept;
template bool GncOption::validate(bool) const;
template bool GncOption::validate(int) const;
template bool GncOption::validate(int64_t) const;
template bool GncOption::validate(double) const;
template bool GncOption::validate(const char*) const;
template bool GncOption::validate(std::string) const;
template bool GncOption::validate(const QofInstance*) const;
template bool GncOption::validate(const QofQuery*) const;
template bool GncOption::validate(RelativeDatePeriod) const;
template bool GncOption::validate(GncMultichoiceOptionIndexVec) const;

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
template GncOption* gnc_make_option<const QofInstance*>(const char*,
                                                        const char*,
                                                        const char*,
                                                        const char*,
                                                        const QofInstance*,
                                                        GncOptionUIType);
