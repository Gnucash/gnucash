/*
 * Temporary swig interface file while developing C++ options.
 *
 * unique_ptr SWIG wrapper from https://stackoverflow.com/questions/27693812/how-to-handle-unique-ptrs-with-swig
 */

namespace std {
  %feature("novaluewrapper") unique_ptr;
  template <typename Type>
  struct unique_ptr {
     typedef Type* pointer;

     explicit unique_ptr( pointer Ptr );
     unique_ptr (unique_ptr&& Right);
     template<class Type2, Class Del2> unique_ptr( unique_ptr<Type2, Del2>&& Right );
     unique_ptr( const unique_ptr& Right) = delete;


     pointer operator-> () const;
     pointer release ();
     void reset (pointer __p=pointer());
     void swap (unique_ptr &__u);
     pointer get () const;
//     operator bool () const;

     ~unique_ptr();
  };
}

%define wrap_unique_ptr(Name, Type)
  %template(Name) std::unique_ptr<Type>;
  %newobject std::unique_ptr<Type>::release;

  %typemap(out) std::unique_ptr<Type> %{
    $result = SWIG_NewPointerObj(new $1_ltype(std::move($1)), $&1_descriptor, SWIG_POINTER_OWN);
  %}

%enddef

%module sw_gnc_optiondb
%{
#include <libguile.h>
#include "gnc-optiondb.hpp"
extern "C" SCM scm_init_sw_gnc_optiondb_module(void);
%}

%include <std_string.i>
%import <base-typemaps.i>

 /* Implementation Note: Plain overloads failed to compile because
  *    auto value{option.get_value()};
  *    return scm_from_value(value);
  * applied implicit conversions among bool, int, and int64_t and ranked them as
  * equal to the non-converted types in overload resolution. Template type
  * resolution is more strict so the overload prefers the exact decltype(value)
  * to implicit conversion candidates.
  */
%inline %{
template <typename ValueType> inline SCM
scm_from_value(ValueType value)
{
    return SCM_BOOL_F;
}
template <> inline SCM
scm_from_value<std::string>(std::string value)
{
    return scm_from_utf8_string(value.c_str());
}

template <> inline SCM
scm_from_value<bool>(bool value)
{
    return value ? SCM_BOOL_T : SCM_BOOL_F;
}

template <> inline SCM
scm_from_value<int64_t>(int64_t value)
{
    return scm_from_int64(value);
}

template <> inline SCM
scm_from_value<int>(int value)
{
    return scm_from_int(value);
}

template <> inline SCM
scm_from_value<double>(double value)
{
    return scm_from_double(value);
}

template <> inline SCM
scm_from_value<const QofInstance*>(const QofInstance* value)
{
    auto guid = guid_to_string(qof_instance_get_guid(value));
    auto scm_guid = scm_from_utf8_string(guid);
    g_free(guid);
    return scm_guid;
}

/* Not needed now, the default template will do this
template <> inline SCM
scm_from_value<QofQuer*>(const QofQuery* value)
{
    return SCM_BOOL_F;
}
*/

template <>inline SCM
scm_from_value<const std::vector<GncGUID>&>(const std::vector<GncGUID>& value)
{
    SCM s_list;
    for (auto guid : value)
    {
        auto guid_s = guid_to_string(qof_instance_get_guid(&guid));
        auto scm_guid = scm_from_utf8_string(guid_s);
        auto scm_guid_list1 = scm_list_1(scm_guid);
        s_list = scm_append(scm_list_2(s_list, scm_guid_list1));
        g_free(guid_s);
    }
    return s_list;
}
/* default template
template <>inline SCM
    scm_from_value<(const std::vector<std::pair<std::string, std::string>>&)
{
    return SCM_BOOL_F;
}
*/
%}
%ignore OptionClassifier;
%ignore OptionUIItem;
%nodefaultctor GncOption;
%ignore GncOptionMultichoiceValue(GncOptionMultichoiceValue&&);
%ignore GncOptionMultichoiceValue::operator=(const GncOptionMultichoiceValue&);
%ignore GncOptionMultichoiceValue::operator=(GncOptionMultichoiceValue&&);
%ignore GncOptionDateValue(GncOptionDateValue&&);
%ignore GncOptionDateValue::operator=(const GncOptionDateValue&);
%ignore GncOptionDateValue::operator=(GncOptionDateValue&&);

%typemap(typecheck, precedence=SWIG_TYPECHECK_INT64) time64 {
    $1 = scm_is_signed_integer($input, INT64_MAX, INT64_MIN);
}

%typemap(in) GncMultiChoiceOptionChoices&& (GncMultiChoiceOptionChoices choices)
{
    auto len = scm_to_size_t(scm_length($input));
    for (std::size_t i = 0; i < len; ++i)
    {
        SCM vec = scm_list_ref($input, scm_from_size_t(i));
        std::string key{scm_to_utf8_string(SCM_SIMPLE_VECTOR_REF(vec, 0))};
        std::string name{scm_to_utf8_string(SCM_SIMPLE_VECTOR_REF(vec, 1))};
        std::string desc{scm_to_utf8_string(SCM_SIMPLE_VECTOR_REF(vec, 2))};
        choices.push_back({std::move(key), std::move(name), std::move(desc)});
    }
    $1 = &choices;
 }

wrap_unique_ptr(GncOptionDBPtr, GncOptionDB);
%include "gnc-option.hpp"
%include "gnc-optiondb.hpp"

%extend GncOption {
    SCM get_scm_value() const
    {
        return std::visit([](const auto& option)->SCM {
                auto value{option.get_value()};
                return scm_from_value(static_cast<decltype(value)>(value));
            }, $self->_get_option());
    }
    SCM get_scm_default_value() const
    {
        return std::visit([](const auto& option)->SCM {
                auto value{option.get_default_value()};
                return scm_from_value(static_cast<decltype(value)>(value));
            }, $self->_get_option());
    }
};

%extend GncOptionDB {
    %template(set_option_string) set_option<std::string>;
    %template(set_option_int) set_option<int>;
    %template(set_option_time64) set_option<time64>;
 };

%inline %{
    SCM gnc_option_value(const GncOptionDBPtr& optiondb, const char* section,
                         const char* name)
    {
        auto db_opt = optiondb->find_option(section, name);
        if (!db_opt)
            return SCM_BOOL_F;
        return GncOption_get_scm_value(&(db_opt->get()));
    }

%}
/*
TEST(GncOption, test_string_scm_functions)
{
    GncOption option("foo", "bar", "baz", "Phony Option", std::string{"waldo"});
    auto scm_value = option.get_scm_value();
    auto str_value = scm_to_utf8_string(scm_value);
    EXPECT_STREQ("waldo", str_value);
    g_free(str_value);
    scm_value = option.get_scm_default_value();
    str_value = scm_to_utf8_string(scm_value);
    EXPECT_STREQ("waldo", str_value);
    g_free(str_value);
}

TEST(GNCOption, test_budget_scm_functions)
{
    auto book = qof_book_new();
    auto budget = gnc_budget_new(book);
    GncOption option("foo", "bar", "baz", "Phony Option",
                     QOF_INSTANCE(budget));
    auto scm_budget = option.get_scm_value();
    auto str_value = scm_to_utf8_string(scm_budget);
    auto guid = guid_to_string(qof_instance_get_guid(budget));
    EXPECT_STREQ(guid, str_value);
    g_free(guid);
    gnc_budget_destroy(budget);
    qof_book_destroy(book);
}

*/
