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
#include <gnc-numeric.hpp>
#include <libguile.h>
#include <string>
#include <exception>
#include <functional>
#include <boost/variant.hpp>

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

enum GncOptionUIType
{
    INTERNAL,
    BOOLEAN,
    STRING,
    TEXT,
    CURRENCY,
    COMMODITY,
    MULTICHOICE,
    DATE,
    ACCOUNT_LIST,
    ACCOUNT_SEL,
    LIST,
    NUMBER_RANGE,
    COLOR,
    FONT,
    BUDGET,
    PIXMAP,
    RADIOBUTTON,
    DATE_FORMAT,
    OWNER,
    CUSTOMER,
    VENDOR,
    EMPLOYEE,
    INVOICE,
    TAX_TABLE,
    QUERY,
};

struct OptionClassifier
{
    std::string m_section;
    std::string m_name;
    std::string m_sort_tag;
//  std::type_info m_kvp_type;
    std::string m_doc_string;
};

using GncMultiChoiceOptionEntry = std::pair<std::string, std::string>;
using GncMultiChoiceOptionChoices = std::vector<GncMultiChoiceOptionEntry>;

/**
 * Holds a pointer to the UI item which will control the option and an enum
 * representing the type of the option for dispatch purposes; all of that
 * happens in gnucash/gnome-utils/dialog-options and
 * gnucash/gnome/business-option-gnome.
 *
 * This class takes no ownership responsibility, so calling code is responsible
 * for ensuring that the UI_Item is alive. For convenience the public
 * clear_ui_item function can be used as a weak_ptr's destruction callback to
 * ensure that the ptr will be nulled if the ui_item is destroyed elsewhere.
 */
class OptionUIItem
{
public:
    GncOptionUIType get_ui_type() { return m_ui_type; }
    void* const get_ui_item() {return m_ui_item; }
    void clear_ui_item() { m_ui_item = nullptr; }
    void set_ui_item(void* ui_item)
    {
        if (m_ui_type == GncOptionUIType::INTERNAL)
        {
            std::string error{"INTERNAL option, setting the UI item forbidden."};
            throw std::logic_error(std::move(error));
        }
        m_ui_item = ui_item;
    }
    void make_internal()
    {
        if (m_ui_item != nullptr)
        {
            std::string error("Option has a UI Element, can't be INTERNAL.");
            throw std::logic_error(std::move(error));
        }
        m_ui_type = GncOptionUIType::INTERNAL;
    }
protected:
    OptionUIItem(GncOptionUIType ui_type) :
        m_ui_item{nullptr}, m_ui_type{ui_type} {}
    OptionUIItem(const OptionUIItem&) = default;
    OptionUIItem(OptionUIItem&&) = default;
    ~OptionUIItem() = default;
    OptionUIItem& operator=(const OptionUIItem&) = default;
    OptionUIItem& operator=(OptionUIItem&&) = default;
private:
    void* m_ui_item;
    GncOptionUIType m_ui_type;
};

template <typename ValueType>
SCM scm_from_value(ValueType);

template <typename ValueType>
class GncOptionValue :
    public OptionClassifier, public OptionUIItem
{
public:
    GncOptionValue<ValueType>(const char* section, const char* name,
                              const char* key, const char* doc_string,
                              ValueType value,
                              GncOptionUIType ui_type = GncOptionUIType::INTERNAL) :
        OptionClassifier{section, name, key, doc_string},
        OptionUIItem(ui_type),
        m_value{value}, m_default_value{value} {}
    ValueType get_value() const { return m_value; }
    ValueType get_default_value() const { return m_default_value; }
    SCM get_scm_value() const
    {
        return scm_from_value(m_value);
    }
    SCM get_scm_default_value() const
    {
        return scm_from_value(m_default_value);
    }
    void set_value(ValueType new_value) { m_value = new_value; }
private:
    ValueType m_value;
    ValueType m_default_value;
};

template <typename ValueType>
class GncOptionValidatedValue :
    public OptionClassifier, public OptionUIItem
{
public:
    GncOptionValidatedValue<ValueType>(const char* section, const char* name,
                                       const char* key, const char* doc_string,
                                       ValueType value,
                                       std::function<bool(ValueType)>validator,
                                       GncOptionUIType ui_type = GncOptionUIType::INTERNAL
        ) :
        OptionClassifier{section, name, key, doc_string},
        OptionUIItem(ui_type),
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
    SCM get_scm_value() const
    {
        return scm_from_value(m_value);
    }
    SCM get_scm_default_value() const
    {
        return scm_from_value(m_default_value);
    }
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

template <typename ValueType>
class GncOptionRangeValue :
    public OptionClassifier, public OptionUIItem
{
public:
    GncOptionRangeValue<ValueType>(const char* section, const char* name,
                                   const char* key, const char* doc_string,
                                   ValueType value, ValueType min,
                                   ValueType max, ValueType step) :
        OptionClassifier{section, name, key, doc_string},
        OptionUIItem(GncOptionUIType::NUMBER_RANGE),
        m_value{value >= min && value <= max ? value : min},
        m_default_value{value >= min && value <= max ? value : min},
        m_min{min}, m_step{step} {}

    ValueType get_value() const { return m_value; }
    ValueType get_default_value() const { return m_default_value; }
    SCM get_scm_value() const
    {
        return scm_from_value(m_value);
    }
    SCM get_scm_default_value() const
    {
        return scm_from_value(m_default_value);
    }
    bool validate(ValueType value) { return value >= m_min && value <= m_max; }
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
    ValueType m_min;
    ValueType m_max;
    ValueType m_step;
};

using GncOptionVariant = boost::variant<GncOptionValue<std::string>,
                                        GncOptionValue<bool>,
                                        GncOptionValue<int64_t>,
                                        GncOptionValue<QofInstance*>,
                                        GncOptionValue<QofQuery*>,
                                        GncOptionValue<std::vector<GncGUID>>,
                                    GncOptionValue<GncMultiChoiceOptionChoices>,
                                        GncOptionRangeValue<int>,
                                        GncOptionRangeValue<GncNumeric>,
                                        GncOptionValidatedValue<QofInstance*>>;
class GncOption
{
public:
    template <typename OptionType>
    GncOption(OptionType option) : m_option{option} {}

    template <typename ValueType>
    GncOption(const char* section, const char* name,
              const char* key, const char* doc_string,
              ValueType value,
              GncOptionUIType ui_type = GncOptionUIType::INTERNAL) :
        m_option{GncOptionValue<ValueType> {
            section, name, key, doc_string, value, ui_type}} {}

    template <typename ValueType> ValueType get_value() const
    {
        return boost::apply_visitor(GetValueVisitor<ValueType>(), m_option);
    }
    template <typename ValueType> ValueType get_default_value() const
    {
        return boost::apply_visitor(GetDefaultValueVisitor<ValueType>(), m_option);
    }
    SCM get_scm_value() const
    {
        return boost::apply_visitor(GetSCMVisitor(), m_option);
    }
    SCM get_scm_default_value() const
    {
        return boost::apply_visitor(GetSCMDefaultVisitor(), m_option);
    }
    template <typename ValueType> void set_value(ValueType value)
    {
        boost::apply_visitor(SetValueVisitor<ValueType>(value), m_option);
    }
    const std::string& get_section() const
    {
        return boost::apply_visitor(GetSectionVisitor(), m_option);
    }
    const std::string& get_name() const
    {
        return boost::apply_visitor(GetNameVisitor(), m_option);
    }
    const std::string& get_key() const
    {
        return boost::apply_visitor(GetKeyVisitor(), m_option);
    }
    const std::string& get_docstring() const
    {
        return boost::apply_visitor(GetDocstringVisitor(), m_option);
    }
    void set_ui_item(void* ui_elem)
    {
        return boost::apply_visitor(SetUIItemVisitor(ui_elem), m_option);
    }
    const GncOptionUIType get_ui_type()
    {
        return boost::apply_visitor(GetUITypeVisitor(), m_option);
    }
    void* const get_ui_item()
    {
        return boost::apply_visitor(GetUIItemVisitor(), m_option);
    }
    void make_internal()
    {
        return boost::apply_visitor(MakeInternalVisitor(), m_option);
    }
private:
    template <typename ValueType>
    struct GetValueVisitor : public boost::static_visitor<ValueType>
    {
        ValueType operator()(const GncOptionValue<ValueType>& option) const {
            return option.get_value();
        }
        ValueType operator()(const GncOptionValidatedValue<ValueType>& option) const {
            return option.get_value();
        }
        template <class OptionType>
        ValueType operator()(OptionType& option) const {
            return ValueType{};
        }
    };
    template <typename ValueType>
    struct GetDefaultValueVisitor : public boost::static_visitor<ValueType>
    {
        ValueType operator()(const GncOptionValue<ValueType>& option) const {
            return option.get_default_value();
        }
        ValueType operator()(const GncOptionValidatedValue<ValueType>& option) const {
            return option.get_default_value();
        }
        template <class OptionType>
        ValueType operator()(OptionType& option) const {
            return ValueType();
        }
    };
    template <typename ValueType>
    struct SetValueVisitor : public boost::static_visitor<>
    {
        SetValueVisitor(ValueType value) : m_value{value} {}
        void operator()(GncOptionValue<ValueType>& option) const {
            option.set_value(m_value);
        }
        void operator()(GncOptionValidatedValue<ValueType>& option) const {
            option.set_value(m_value);
        }
        template <class OptionType>
        void operator()(OptionType& option) const {
        }
    private:
        ValueType m_value;
    };
    struct GetSCMVisitor : public boost::static_visitor<SCM>
    {
        template <class OptionType>
        SCM operator()(OptionType& option) const {
            return option.get_scm_value();
        }
    };
    struct GetSCMDefaultVisitor : public boost::static_visitor<SCM>
    {
        template <class OptionType>
        SCM operator()(OptionType& option) const {
            return option.get_scm_default_value();
        }
    };
    struct GetSectionVisitor : public boost::static_visitor<const std::string&>
    {
        template <class OptionType>
        const std::string& operator()(OptionType& option) const {
            return option.m_section;
        }
    };
    struct GetNameVisitor : public boost::static_visitor<const std::string&>
    {
        template <class OptionType>
        const std::string& operator()(OptionType& option) const {
            return option.m_name;
        }
    };
    struct GetKeyVisitor : public boost::static_visitor<const std::string&>
    {
        template <class OptionType>
        const std::string& operator()(OptionType& option) const {
            return option.m_sort_tag;
        }
    };
    struct GetDocstringVisitor :
        public boost::static_visitor<const std::string&>
    {
        template <class OptionType>
        const std::string& operator()(OptionType& option) const {
            return option.m_doc_string;
        }
    };
    struct SetUIItemVisitor : public boost::static_visitor<>
    {
        SetUIItemVisitor(void* ui_item) : m_ui_item{ui_item} {}
        template <class OptionType>
        void operator()(OptionType& option) const {
            option.set_ui_item(m_ui_item);
        }
    private:
        void* m_ui_item;
    };
    struct GetUITypeVisitor :
        public boost::static_visitor<GncOptionUIType>
    {
        template <class OptionType>
        const GncOptionUIType operator()(OptionType& option) const {
            return option.get_ui_type();
        }
    };
    struct GetUIItemVisitor :
        public boost::static_visitor<void* const >
    {
        template <class OptionType>
        void* const operator()(OptionType& option) const {
            return option.get_ui_item();
        }
    };
    struct MakeInternalVisitor : public boost::static_visitor<>
    {
        template <class OptionType>
        void operator()(OptionType& option) const {
            return option.make_internal();
        }
    };

    GncOptionVariant m_option;
};

#endif //GNC_OPTION_HPP_
