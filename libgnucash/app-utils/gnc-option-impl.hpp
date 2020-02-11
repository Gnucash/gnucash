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

#ifndef GNC_OPTION_IMPL_HPP_
#define GNC_OPTION_IMPL_HPP_

#include "gnc-option.hpp"
extern "C"
{
#include <config.h>
#include <qof.h>
#include <Account.h>
#include <gnc-budget.h>
#include <gnc-commodity.h>
}
#include <gnc-datetime.hpp>
#include <libguile.h>
#include <string>
#include <utility>
#include <vector>
#include <exception>
#include <functional>
#include <variant>
#include <iostream>

#include "gnc-option-uitype.hpp"

/*
 * Unused base class to document the structure of the current Scheme option
 * vector, re-expressed in C++. The comment-numbers on the right indicate which
 * item in the Scheme vector each item implements.
 *
 * Not everything here needs to be implemented, nor will it necessarily be
 * implemented the same way. For example, part of the purpose of this redesign
 * is to convert from saving options as strings of Scheme code to some form of
 * key-value pair in the book options, so generate_restore_form() will likely be
 * supplanted with save_to_book().

template <typename ValueType>
class GncOptionBase
{
public:
    virtual ~GncOption = default;
    virtual ValueType get_value() const = 0;                             //5
    virtual ValueType get_default_value() = 0;
    virtual SCM get_SCM_value() = 0;
    virtual SCM get_SCM_default_value() const = 0;                       //7
    virtual void set_value(ValueType) = 0;                               //6
// generate_restore_form outputs a Scheme expression (a "form") that finds an
// option and sets it to the current value. e.g.:
//(let ((option (gnc:lookup-option options
//                                 "Display"
//                                 "Amount")))
//  ((lambda (option) (if option ((gnc:option-setter option) 'none))) option))
// it uses gnc:value->string to generate the "'none" (or whatever the option's
// value would be as input to the scheme interpreter).

    virtual std::string generate_restore_form();                         //8
    virtual void save_to_book(QofBook*) const noexcept;                  //9
    virtual void read_from_book(QofBook*);                               //10
    virtual std::vector<std::string> get_option_strings();               //15
    virtual set_changed_callback(std::function<void(void*)>);            //14
protected:
    const std::string m_section;                                         //0
    const std::string m_name;                                            //1
    const std::string m_sort_tag;                                        //2
    const std::type_info m_kvp_type;                                     //3
    const std::string m_doc_string;                                      //4
    std::function<void(void*)> m_changed_callback;   //Part of the make-option closure
    std::function<void(void*)>m_option_widget_changed_callback;          //16
};
*/


static const char* commodity_scm_intro{"'(commodity-scm "};
#ifndef SWIG
size_t constexpr classifier_size_max{50};
size_t constexpr sort_tag_size_max{10};
#endif

struct OptionClassifier
{
    std::string m_section;
    std::string m_name;
    std::string m_sort_tag;
//  std::type_info m_kvp_type;
    std::string m_doc_string;
};

#ifndef SWIG
auto constexpr size_t_max = std::numeric_limits<std::size_t>::max();
#endif

template <typename ValueType>
class GncOptionValue : public OptionClassifier
{
public:
    GncOptionValue<ValueType>(const char* section, const char* name,
                              const char* key, const char* doc_string,
                              ValueType value,
                              GncOptionUIType ui_type = GncOptionUIType::INTERNAL) :
        OptionClassifier{section, name, key, doc_string},
        m_ui_type(ui_type), m_value{value}, m_default_value{value} {}
    GncOptionValue<ValueType>(const GncOptionValue<ValueType>&) = default;
    GncOptionValue<ValueType>(GncOptionValue<ValueType>&&) = default;
    GncOptionValue<ValueType>& operator=(const GncOptionValue<ValueType>&) = default;
    GncOptionValue<ValueType>& operator=(GncOptionValue<ValueType>&&) = default;
    ValueType get_value() const { return m_value; }
    ValueType get_default_value() const { return m_default_value; }
    void set_value(ValueType new_value) { m_value = new_value; }
    bool is_changed() const noexcept { return m_value != m_default_value; }
    GncOptionUIType get_ui_type() const noexcept { return m_ui_type; }
    void make_internal() { m_ui_type = GncOptionUIType::INTERNAL; }
private:
    GncOptionUIType m_ui_type;
    ValueType m_value;
    ValueType m_default_value;
};

template <typename ValueType>
class GncOptionValidatedValue : public OptionClassifier
{
public:
    GncOptionValidatedValue<ValueType>() = delete;
    GncOptionValidatedValue<ValueType>(const char* section, const char* name,
                                       const char* key, const char* doc_string,
                                       ValueType value,
                                       std::function<bool(ValueType)>validator,
                                       GncOptionUIType ui_type = GncOptionUIType::INTERNAL
        ) :
        OptionClassifier{section, name, key, doc_string},
        m_ui_type{ui_type}, m_value{value}, m_default_value{value},
        m_validator{validator}
        {
            if (!this->validate(value))
            throw std::invalid_argument("Attempt to create GncValidatedOption with bad value.");
        }
    GncOptionValidatedValue<ValueType>(const char* section, const char* name,
                                       const char* key, const char* doc_string,
                                       ValueType value,
                                       std::function<bool(ValueType)>validator,
                                       ValueType val_data) :
        OptionClassifier{section, name, key, doc_string},
        m_ui_type{GncOptionUIType::INTERNAL}, m_value{value},
        m_default_value{value}, m_validator{validator}, m_validation_data{val_data}
    {
            if (!this->validate(value))
            throw std::invalid_argument("Attempt to create GncValidatedOption with bad value.");
    }
    GncOptionValidatedValue<ValueType>(const GncOptionValidatedValue<ValueType>&) = default;
    GncOptionValidatedValue<ValueType>(GncOptionValidatedValue<ValueType>&&) = default;
    GncOptionValidatedValue<ValueType>& operator=(const GncOptionValidatedValue<ValueType>&) = default;
    GncOptionValidatedValue<ValueType>& operator=(GncOptionValidatedValue<ValueType>&&) = default;
    ValueType get_value() const { return m_value; }
    ValueType get_default_value() const { return m_default_value; }
    bool validate(ValueType value) const { return m_validator(value); }
    void set_value(ValueType value)
    {
        if (this->validate(value))
            m_value = value;
        else
            throw std::invalid_argument("Validation failed, value not set.");
    }
    bool is_changed() const noexcept { return m_value != m_default_value; }
    std::ostream& to_scheme(std::ostream&) const;
    std::istream& from_scheme(std::istream&);
    GncOptionUIType get_ui_type() const noexcept { return m_ui_type; }
    void make_internal() { m_ui_type = GncOptionUIType::INTERNAL; }
private:
    GncOptionUIType m_ui_type;
    ValueType m_value;
    ValueType m_default_value;
    std::function<bool(ValueType)> m_validator;                         //11
    ValueType m_validation_data;
};

QofInstance* qof_instance_from_string(const std::string& str,
                                      GncOptionUIType type);
QofInstance* qof_instance_from_guid(GncGUID*, GncOptionUIType type);
std::string qof_instance_to_string(const QofInstance* inst);

/* These will work when m_value is a built-in class; GnuCash class and container
 * values will need specialization unless they happen to define operators << and
 * >>.
 * Note that SWIG 3.0.12 chokes on elaborate enable_if so just hide the
 * following templates from SWIG. (Ignoring doesn't work because SWIG still has
 * to parse the templates to figure out the symbols.
 */
#ifndef SWIG
template<class OptionValueClass,
         typename std::enable_if_t<std::is_base_of_v<OptionClassifier,
                                                     std::decay_t<OptionValueClass>> &&
                                   !(std::is_same_v<std::decay_t<OptionValueClass>,
                                     GncOptionValue<const QofInstance*>> ||
                                     std::is_same_v<std::decay_t<OptionValueClass>,
                                     GncOptionValidatedValue<const QofInstance*>>), int> = 0>
std::ostream& operator<<(std::ostream& oss, const OptionValueClass& opt)
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

template<class OptType, typename std::enable_if_t<std::is_same_v<std::decay_t<OptType>, GncOptionValidatedValue<const QofInstance*>> || std::is_same_v<std::decay_t<OptType>, GncOptionValue<const QofInstance*> >, int> = 0>
inline std::ostream&
operator<< (std::ostream& oss, const OptType& opt)
{
    auto value = opt.get_value();
    if (auto type = opt.get_ui_type(); type == GncOptionUIType::COMMODITY ||
        type == GncOptionUIType::CURRENCY)
    {
        if (auto type = opt.get_ui_type(); type == GncOptionUIType::COMMODITY)
        {
            oss << gnc_commodity_get_namespace(GNC_COMMODITY(value)) << " ";
        }
        oss << gnc_commodity_get_mnemonic(GNC_COMMODITY(value));
    }
    else
    {
        oss << qof_instance_to_string(value);
    }
    return oss;
}

template<class OptionValueClass,
         typename std::enable_if_t<std::is_base_of_v<OptionClassifier, std::decay_t<OptionValueClass>> &&
                                   !(std::is_same_v<std::decay_t<OptionValueClass>,
                                     GncOptionValue<const QofInstance*>> ||
                                     std::is_same_v<std::decay_t<OptionValueClass>,
                                     GncOptionValidatedValue<const QofInstance*>>), int> = 0>
std::istream& operator>>(std::istream& iss, OptionValueClass& opt)
{
    std::decay_t<decltype(opt.get_value())> value;
    iss >> value;
    opt.set_value(value);
    return iss;
}

template<class OptType, typename std::enable_if_t<std::is_same_v<std::decay_t<OptType>, GncOptionValidatedValue<const QofInstance*>> || std::is_same_v<std::decay_t<OptType>, GncOptionValue<const QofInstance*> >, int> = 0>
std::istream&
operator>> (std::istream& iss, OptType& opt)
{
    std::string instr;
    if (auto type = opt.get_ui_type(); type == GncOptionUIType::COMMODITY ||
        type == GncOptionUIType::CURRENCY)
    {
        std::string name_space, mnemonic;
        if (type = opt.get_ui_type(); type == GncOptionUIType::COMMODITY)
        {
            iss >> name_space;
        }
        else
            name_space = GNC_COMMODITY_NS_CURRENCY;
        iss >> mnemonic;
        instr = name_space + ":";
        instr += mnemonic;
     }
    else
    {
        iss >> instr;
    }
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
template<class OptType, typename std::enable_if_t<std::is_same_v<std::decay_t<OptType>, GncOptionValidatedValue<const QofInstance*>> || std::is_same_v<std::decay_t<OptType>, GncOptionValue<const QofInstance*>>, int> = 0>
inline std::ostream&
gnc_option_to_scheme (std::ostream& oss, const OptType& opt)
{
    auto value = opt.get_value();
    auto type = opt.get_ui_type();
    if (type == GncOptionUIType::COMMODITY || type == GncOptionUIType::CURRENCY)
    {
        if (type == GncOptionUIType::COMMODITY)
        {
            oss << commodity_scm_intro;
            oss << "\"" <<
                gnc_commodity_get_namespace(GNC_COMMODITY(value)) << "\" ";
        }

        oss << "\"" << gnc_commodity_get_mnemonic(GNC_COMMODITY(value)) << "\"";

        if (type == GncOptionUIType::COMMODITY)
        {
            oss << ")";
        }
    }
    else
    {
        oss << "\"" << qof_instance_to_string(value) << "\"";
    }
    return oss;
}

template<class OptType, typename std::enable_if_t<std::is_same_v<std::decay_t<OptType>, GncOptionValidatedValue<const QofInstance*>> || std::is_same_v<std::decay_t<OptType>, GncOptionValue<const QofInstance*>>, int> = 0>
inline std::istream&
gnc_option_from_scheme (std::istream& iss, OptType& opt)
{
    std::string instr;
    auto type = opt.get_ui_type();
    if (type == GncOptionUIType::COMMODITY || type == GncOptionUIType::CURRENCY)
    {
        std::string name_space, mnemonic;
        if (type == GncOptionUIType::COMMODITY)
        {
            iss.ignore(strlen(commodity_scm_intro) + 1, '"');
            std::getline(iss, name_space, '"');
 // libc++ doesn't consume the end character, libstdc++ does
#ifdef _LIBCPP_VERSION
            iss.ignore(1, '"');
#endif
        }
        else
            name_space = GNC_COMMODITY_NS_CURRENCY;
        iss.ignore(1, '"');
        std::getline(iss, mnemonic, '"');

        if (type == GncOptionUIType::COMMODITY)
            iss.ignore(2, ')');
        else
            iss.ignore(1, '"');

        instr = name_space + ":";
        instr += mnemonic;
     }
    else
    {
        iss.ignore(1, '"');
        std::getline(iss, instr, '"');
    }
    opt.set_value(qof_instance_from_string(instr, opt.get_ui_type()));
    return iss;
}
#endif // SWIG

/**
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
        OptionClassifier{section, name, key, doc_string},
        m_value{value >= min && value <= max ? value : min},
        m_default_value{value >= min && value <= max ? value : min},
        m_min{min}, m_max{max}, m_step{step} {}

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
    bool is_changed() const noexcept { return m_value != m_default_value; }
    GncOptionUIType get_ui_type() const noexcept { return m_ui_type; }
    void make_internal() { m_ui_type = GncOptionUIType::INTERNAL; }
private:
    GncOptionUIType m_ui_type = GncOptionUIType::NUMBER_RANGE;
    ValueType m_value;
    ValueType m_default_value;
    ValueType m_min;
    ValueType m_max;
    ValueType m_step;
};

using GncMultiChoiceOptionEntry = std::tuple<const std::string,
                                             const std::string,
                                             const std::string>;
using GncMultiChoiceOptionChoices = std::vector<GncMultiChoiceOptionEntry>;

/** MultiChoice options have a vector of valid options
 * (GncMultiChoiceOptionChoices) and validate the selection as being one of
 * those values. The value is the index of the selected item in the vector. The
 * tuple contains three strings, a key, a display
 * name and a brief description for the tooltip. Both name and description
 * should be localized at the point of use. 
 *
 *
 */

class GncOptionMultichoiceValue : public OptionClassifier
{
public:
    GncOptionMultichoiceValue(const char* section, const char* name,
                              const char* key, const char* doc_string,
                              const char* value,
                              GncMultiChoiceOptionChoices&& choices,
                              GncOptionUIType ui_type = GncOptionUIType::MULTICHOICE) :
        OptionClassifier{section, name, key, doc_string},
        m_ui_type{ui_type},
        m_value{}, m_default_value{}, m_choices{std::move(choices)} {
            if (value)
            {
                if (auto index = find_key(value);
                    index != size_t_max)
                {
                    m_value = index;
                    m_default_value = index;
                }
            }
        }

    GncOptionMultichoiceValue(const GncOptionMultichoiceValue&) = default;
    GncOptionMultichoiceValue(GncOptionMultichoiceValue&&) = default;
    GncOptionMultichoiceValue& operator=(const GncOptionMultichoiceValue&) = default;
    GncOptionMultichoiceValue& operator=(GncOptionMultichoiceValue&&) = default;

    const std::string& get_value() const
    {
        return std::get<0>(m_choices.at(m_value));
    }
    const std::string& get_default_value() const
    {
        return std::get<0>(m_choices.at(m_default_value));
    }
     bool validate(const std::string& value) const noexcept
    {
        auto index = find_key(value);
        return index != size_t_max;

    }
    void set_value(const std::string& value)
    {
        auto index = find_key(value);
        if (index != size_t_max)
            m_value = index;
        else
            throw std::invalid_argument("Value not a valid choice.");

    }
    std::size_t num_permissible_values() const noexcept
    {
        return m_choices.size();
    }
    std::size_t permissible_value_index(const std::string& key) const noexcept
    {
            return find_key(key);
    }
    const std::string& permissible_value(std::size_t index) const
    {
        return std::get<0>(m_choices.at(index));
    }
    const std::string& permissible_value_name(std::size_t index) const
    {
        return std::get<1>(m_choices.at(index));
    }
    const std::string& permissible_value_description(std::size_t index) const
    {
        return std::get<2>(m_choices.at(index));
    }
    bool is_changed() const noexcept { return m_value != m_default_value; }
    GncOptionUIType get_ui_type() const noexcept { return m_ui_type; }
    void make_internal() { m_ui_type = GncOptionUIType::INTERNAL; }
private:
    std::size_t find_key (const std::string& key) const noexcept
    {
        auto iter = std::find_if(m_choices.begin(), m_choices.end(),
                              [key](auto choice) {
                                  return std::get<0>(choice) == key; });
        if (iter != m_choices.end())
            return iter - m_choices.begin();
        else
            return size_t_max;

    }
    GncOptionUIType m_ui_type;
    std::size_t m_value;
    std::size_t m_default_value;
    GncMultiChoiceOptionChoices m_choices;
};

using GncOptionAccountList = std::vector<const Account*>;

using GncOptionAccountTypeList = std::vector<GNCAccountType>;

/** Account options
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

class GncOptionAccountValue : public OptionClassifier
{
public:
    GncOptionAccountValue(const char* section, const char* name,
                          const char* key, const char* doc_string,
                          GncOptionUIType ui_type) :
        OptionClassifier{section, name, key, doc_string},
        m_ui_type{ui_type}, m_value{}, m_default_value{}, m_allowed{} {}

    GncOptionAccountValue(const char* section, const char* name,
                          const char* key, const char* doc_string,
                          GncOptionUIType ui_type,
                          const GncOptionAccountList& value) :
        OptionClassifier{section, name, key, doc_string},
        m_ui_type{ui_type}, m_value{value},
        m_default_value{std::move(value)}, m_allowed{} {}
    GncOptionAccountValue(const char* section, const char* name,
                          const char* key, const char* doc_string,
                          GncOptionUIType ui_type,
                          GncOptionAccountTypeList&& allowed) :
        OptionClassifier{section, name, key, doc_string},
        m_ui_type{ui_type}, m_value{},
        m_default_value{}, m_allowed{std::move(allowed)} {}
    GncOptionAccountValue(const char* section, const char* name,
                          const char* key, const char* doc_string,
                          GncOptionUIType ui_type,
                          const GncOptionAccountList& value,
                          GncOptionAccountTypeList&& allowed) :
        OptionClassifier{section, name, key, doc_string},
        m_ui_type{ui_type}, m_value{},
        m_default_value{}, m_allowed{std::move(allowed)} {
            if (!validate(value))
                throw std::invalid_argument("Supplied Value not in allowed set.");
            m_value = value;
            m_default_value = std::move(value);
        }

    const GncOptionAccountList& get_value() const { return m_value; }
    const GncOptionAccountList& get_default_value() const { return m_default_value; }
    bool validate (const GncOptionAccountList& values) const;
    void set_value (const GncOptionAccountList& values) {
        if (validate(values))
            //throw!
            m_value = values;
    }
    bool is_changed() const noexcept { return m_value != m_default_value; }
    GncOptionUIType get_ui_type() const noexcept { return m_ui_type; }
    void make_internal() { m_ui_type = GncOptionUIType::INTERNAL; }
private:
    GncOptionUIType m_ui_type;
    GncOptionAccountList m_value;
    GncOptionAccountList m_default_value;
    GncOptionAccountTypeList m_allowed;
};

template<> inline std::ostream&
operator<< <GncOptionAccountValue>(std::ostream& oss,
                                       const GncOptionAccountValue& opt)
{
    auto values{opt.get_value()};
    bool first = true;
    for (auto value : values)
    {
        if (first)
            first = false;
        else
            oss << " ";
        oss << qof_instance_to_string(QOF_INSTANCE(value));
    }
    return oss;
}

template<> inline std::istream&
operator>> <GncOptionAccountValue>(std::istream& iss,
                                   GncOptionAccountValue& opt)
{
    GncOptionAccountList values;
    while (true)
    {
        std::string str;
        std::getline(iss, str, ' ');
        if (!str.empty())
            values.emplace_back((Account*)qof_instance_from_string(str, opt.get_ui_type()));
        else
            break;
    }
    opt.set_value(values);
    iss.clear();
    return iss;
}

template<class OptType, typename std::enable_if_t<std::is_same_v<std::decay_t<OptType>, GncOptionAccountValue>, int> = 0>
inline std::ostream&
gnc_option_to_scheme(std::ostream& oss, const OptType& opt)
{
    auto values{opt.get_value()};
    oss << "'(\"";
    bool first = true;
    for (auto value : values)
    {
        if (first)
            first = false;
        else
            oss << " \"";
        oss << qof_instance_to_string(QOF_INSTANCE(value)) << '"';
    }
    oss << ')';
    return oss;
}

template<class OptType, typename std::enable_if_t<std::is_same_v<std::decay_t<OptType>, GncOptionAccountValue>, int> = 0>
inline std::istream&
gnc_option_from_scheme(std::istream& iss, OptType& opt)
{
    GncOptionAccountList values;
    iss.ignore(3, '"');
    while (true)
    {
        std::string str;
        std::getline(iss, str, '"');
        if (!str.empty())
        {
            values.emplace_back((Account*)qof_instance_from_string(str, opt.get_ui_type()));
            iss.ignore(2, '"');
        }
        else
            break;
    }
    opt.set_value(values);
    iss.ignore(1, ')');
    return iss;
}

/** Date options
 * A legal date value is a pair of either and a RelativeDatePeriod, the absolute
 * flag and a time64, or for legacy purposes the absolute flag and a timespec.
 */
/*
gnc-date-option-show-time? -- option_data[1]
gnc-date-option-get-subtype -- option_data[0]
gnc-date-option-value-type m_value
gnc-date-option-absolute-time m_type == DateTyupe::Absolute
gnc-date-option-relative-time m_type != DateTyupe::Absolute
 */

class GncOptionDateValue : public OptionClassifier
{
public:
    GncOptionDateValue(const char* section, const char* name,
                              const char* key, const char* doc_string) :
        OptionClassifier{section, name, key, doc_string},
        m_period{RelativeDatePeriod::END_ACCOUNTING_PERIOD},
        m_date{INT64_MAX},
        m_default_period{RelativeDatePeriod::END_ACCOUNTING_PERIOD},
        m_default_date{INT64_MAX} {}
    GncOptionDateValue(const char* section, const char* name,
                       const char* key, const char* doc_string,
                       time64 time) :
        OptionClassifier{section, name, key, doc_string},
        m_period{RelativeDatePeriod::ABSOLUTE}, m_date{time},
        m_default_period{RelativeDatePeriod::ABSOLUTE}, m_default_date{time} {}
    GncOptionDateValue(const char* section, const char* name,
                       const char* key, const char* doc_string,
                       const RelativeDatePeriod period) :
        OptionClassifier{section, name, key, doc_string},
        m_period{period}, m_date{INT64_MAX},
        m_default_period{period}, m_default_date{INT64_MAX} {}
        GncOptionDateValue(const GncOptionDateValue&) = default;
        GncOptionDateValue(GncOptionDateValue&&) = default;
        GncOptionDateValue& operator=(const GncOptionDateValue&) = default;
        GncOptionDateValue& operator=(GncOptionDateValue&&) = default;
    time64 get_value() const;
    time64 get_default_value() const { return static_cast<time64>(GncDateTime()); }
    std::ostream& out_stream(std::ostream& oss) const noexcept;
    std::istream& in_stream(std::istream& iss);
    void set_value(RelativeDatePeriod value) {
        m_period = value;
        m_date = INT64_MAX;
    }
    void set_value(time64 time) {
        m_period = RelativeDatePeriod::ABSOLUTE;
        m_date = time;
    }
    bool is_changed() const noexcept { return m_period != m_default_period &&
            m_date != m_default_date; }
    GncOptionUIType get_ui_type() const noexcept { return m_ui_type; }
    void make_internal() { m_ui_type = GncOptionUIType::INTERNAL; }
private:
    GncOptionUIType m_ui_type = GncOptionUIType::DATE;
    RelativeDatePeriod m_period;
    time64 m_date;
    RelativeDatePeriod m_default_period;
    time64 m_default_date;
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
