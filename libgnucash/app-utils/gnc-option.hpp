/********************************************************************\
 * gnc-option.hpp -- Application options system                     *
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

#ifndef GNC_OPTION_HPP_
#define GNC_OPTION_HPP_

extern "C"
{
#include <config.h>
#include <qof.h>
#include <gnc-budget.h>
#include <gnc-commodity.h>
}
#include <libguile.h>
#include <string>

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

struct OptionClassifier
{
    const std::string m_section;
    const std::string m_name;
    const std::string m_sort_tag;
//    const std::type_info m_kvp_type;
    const std::string m_doc_string;
};

template <typename ValueType>
SCM scm_from_value(ValueType);

/* This design pattern is called the Curiously Recursive Template Pattern, or
 * CRTP. See https://en.wikipedia.org/wiki/Curiously_recurring_template_pattern
 * for a detailed explanation.
 */
template <typename ValueType, class ValueClass>
class GncOption
{
public:
    ValueType get_value() const
    {
        return static_cast<ValueClass const&>(*this).get_value();
    }
    void set_value(ValueType value)
    {
        static_cast<ValueClass&>(*this).set_value(value);
    }
    ValueType get_default_value() const
    {
        return static_cast<ValueClass const&>(*this).get_default_value();
    }
    SCM get_scm_value()
    {
        ValueType value{static_cast<ValueClass const&>(*this).get_value()};
        return scm_from_value<ValueType>(value);
    }
    SCM get_scm_default_value()
    {
        ValueType value{static_cast<ValueClass const&>(*this).get_default_value()};
        return scm_from_value<ValueType>(value);
    }
};

template <typename ValueType>
class GncOptionValue :
    public OptionClassifier,
    public GncOption<ValueType, GncOptionValue<ValueType>>
{
public:
    GncOptionValue<ValueType>(const char* section, const char* name,
                              const char* key, const char* doc_string,
                              ValueType value) :
        OptionClassifier{section, name, key, doc_string},
        m_value{value}, m_default_value{value} {}
    ValueType get_value() const { return m_value; }
    ValueType get_default_value() const { return m_default_value; }
    void set_value(ValueType new_value) { m_value = new_value; }
protected:
    ValueType m_value;
    ValueType m_default_value;
};

template <typename ValueType>
class GncOptionValidatedValue :
    public OptionClassifier,
    public GncOption<ValueType, GncOptionValidatedValue<ValueType>>
{
public:
    GncOptionValidatedValue<ValueType>(const char* section, const char* name,
                                       const char* key, const char* doc_string,
                                       ValueType value,
                                      std::function<bool(ValueType)>validator) :
        OptionClassifier{section, name, key, doc_string},
        m_value{value}, m_default_value{value}, m_validator{validator}
        {
            if (!this->validate(value))
            throw std::invalid_argument("Attempt to create GncValidatedOption with bad value.");
        }
    GncOptionValidatedValue<ValueType>(const char* section, const char* name,
                                       const char* key, const char* doc_string,
                                       ValueType value,
                                       std::function<bool(ValueType)>validator,
                                       ValueType val_data) :
        OptionClassifier{section, name, key, doc_string}, m_value{value},
        m_default_value{value}, m_validator{validator}, m_validation_data{val_data}
    {
            if (!this->validate(value))
            throw std::invalid_argument("Attempt to create GncValidatedOption with bad value.");
    }
    ValueType get_value() const { return m_value; }
    ValueType get_default_value() const { return m_default_value; }
    bool validate(ValueType value) { return m_validator(value); }
    void set_value(ValueType value)
    {
        if (this->validate(value))
            m_value = value;
        else
            throw std::invalid_argument("Validation failed, value not set.");
    }
private:
    ValueType m_value;
    ValueType m_default_value;
    std::function<bool(ValueType)> m_validator;                         //11
    ValueType m_validation_data;
};

std::shared_ptr<GncOptionValue<std::string>>
gnc_make_string_option(const char* section, const char* name,
                       const char* key, const char* doc_string,
                       std::string value);

std::shared_ptr<GncOptionValue<std::string>>
gnc_make_text_option(const char* section, const char* name,
                     const char* key, const char* doc_string,
                     std::string value);

std::shared_ptr<GncOptionValue<QofInstance*>>
gnc_make_budget_option(const char* section, const char* name,
                       const char* key, const char* doc_string,
                       GncBudget* value);

std::shared_ptr<GncOptionValue<QofInstance*>>
gnc_make_commodity_option(const char* section, const char* name,
                          const char* key, const char* doc_string,
                          gnc_commodity* value);

std::shared_ptr<GncOptionValidatedValue<QofInstance*>>
gnc_make_currency_option(const char* section, const char* name,
                         const char* key, const char* doc_string,
                         gnc_commodity* value);

#endif //GNC_OPTION_HPP_
