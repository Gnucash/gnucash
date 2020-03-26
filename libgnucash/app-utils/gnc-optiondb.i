/*
 * Temporary swig interface file while developing C++ options.
 *
 * unique_ptr SWIG wrapper from https://stackoverflow.com/questions/27693812/how-to-handle-unique-ptrs-with-swig
 */
#if defined(SWIGGUILE)

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

 //%module sw_gnc_optiondb
%{
#include "gnc-optiondb.hpp"
#include "gnc-optiondb-impl.hpp"
#include "gnc-option-date.hpp"

SCM scm_init_sw_gnc_optiondb_module(void);
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
    scm_from_value(ValueType value);
/*{
    return SCM_BOOL_F;
    }*/
template <> inline SCM
scm_from_value<QofQuery*>(QofQuery* value)
{
        return SCM_BOOL_F;
}

template <> inline SCM
scm_from_value<QofInstance*>(QofInstance* value)
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

template <typename ValueType> inline ValueType
scm_to_value(SCM new_value)
{
    return ValueType{};
}

template <> inline std::string
scm_to_value<std::string>(SCM new_value)
{
    auto strval = scm_to_utf8_stringn(new_value, nullptr);
    std::string retval{strval};
    free(strval);
    return retval;
}

template <> inline int
scm_to_value<int>(SCM new_value)
{
    return scm_to_int(new_value);
}

template <> inline int64_t
scm_to_value<int64_t>(SCM new_value)
{
    return scm_to_int64(new_value);
}

template <>inline SCM
scm_from_value<GncOptionAccountList>(GncOptionAccountList value)
{
    SCM s_list = SCM_EOL;
    for (auto acct : value)
    {
        SCM elem = scm_list_1(SWIG_NewPointerObj(acct, SWIGTYPE_p_Account, 0));
        s_list = scm_append(scm_list_2(s_list, elem));
    }

    return s_list;
}

QofBook* gnc_option_test_book_new();
void gnc_option_test_book_destroy(QofBook*);

QofBook*
gnc_option_test_book_new()
{
    return static_cast<QofBook*>(g_object_new(QOF_TYPE_BOOK, nullptr));
}

void
gnc_option_test_book_destroy(QofBook* book)
{
    g_object_unref(book);
}

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
%ignore GncOptionDateValue::set_value(size_t); // Used only by dialog-options
%ignore operator<<(std::ostream&, const GncOption&);
%ignore operator>>(std::istream&, GncOption&);
%ignore GncOption::_get_option();

%rename(absolute) RelativeDatePeriod::ABSOLUTE;
%rename(today) RelativeDatePeriod::TODAY;
%rename(one_week_ago) RelativeDatePeriod::ONE_WEEK_AGO;
%rename(one_week_ahead) RelativeDatePeriod::ONE_WEEK_AHEAD;
%rename(one_month_ago) RelativeDatePeriod::ONE_MONTH_AGO;
%rename(one_month_ahead) RelativeDatePeriod::ONE_MONTH_AHEAD;
%rename(three_months_ago) RelativeDatePeriod::THREE_MONTHS_AGO;
%rename(three_months_ahead) RelativeDatePeriod::THREE_MONTHS_AHEAD;
%rename(six_months_ago) RelativeDatePeriod::SIX_MONTHS_AGO;
%rename(six_months_ahead) RelativeDatePeriod::SIX_MONTHS_AHEAD;
%rename(one_year_ago) RelativeDatePeriod::ONE_YEAR_AGO;
%rename(one_year_ahead) RelativeDatePeriod::ONE_YEAR_AHEAD;
%rename(start_this_month) RelativeDatePeriod::START_THIS_MONTH;
%rename(end_this_month) RelativeDatePeriod::END_THIS_MONTH;
%rename(start_prev_month) RelativeDatePeriod::START_PREV_MONTH;
%rename(end_prev_month) RelativeDatePeriod::END_PREV_MONTH;
%rename(start_current_quarter) RelativeDatePeriod::START_CURRENT_QUARTER;
%rename(end_current_quarter) RelativeDatePeriod::END_CURRENT_QUARTER;
%rename(start_prev_quarter) RelativeDatePeriod::START_PREV_QUARTER;
%rename(end_prev_quarter) RelativeDatePeriod::END_PREV_QUARTER;
%rename(start_cal_year) RelativeDatePeriod::START_CAL_YEAR;
%rename(end_cal_yea) RelativeDatePeriod::END_CAL_YEAR;
%rename(start_prev_year) RelativeDatePeriod::START_PREV_YEAR;
%rename(end_prev_year) RelativeDatePeriod::END_PREV_YEAR;
%rename(start_accounting_period) RelativeDatePeriod::START_ACCOUNTING_PERIOD;
%rename(end_accounting_period) RelativeDatePeriod::END_ACCOUNTING_PERIOD;

%rename(gnc_register_date_option_set)
    gnc_register_date_option(const GncOptionDBPtr&, const char*, const char*,
                             const char*, const char*, RelativeDatePeriodVec&,
                             bool);

%typemap(typecheck, precedence=SWIG_TYPECHECK_INT64) time64 {
    $1 = scm_is_signed_integer($input, INT64_MAX, INT64_MIN);
}

%typemap(in) RelativeDatePeriodVec& (RelativeDatePeriodVec period_set)
{
    auto len = scm_to_size_t(scm_length($input));
    for (std::size_t i = 0; i < len; ++i)
    {
        SCM s_reldateperiod = scm_list_ref($input, scm_from_size_t(i));
        period_set.push_back((RelativeDatePeriod)scm_to_int(s_reldateperiod));
    }
    $1 = &period_set;
}

%typemap(in) GncMultichoiceOptionChoices&& (GncMultichoiceOptionChoices choices)
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


%typemap(in) GncOptionAccountList
{
    auto len = scm_to_size_t(scm_length($input));
    for (std::size_t i = 0; i < len; ++i)
    {
        SCM s_account = scm_list_ref($input, scm_from_size_t(i));
        Account* acct = (Account*)SWIG_MustGetPtr(s_account,
                                                  SWIGTYPE_p_Account, 1, 0);
        $1.push_back(acct);
    }
}

%typemap(in) GncOptionAccountTypeList& (GncOptionAccountTypeList types)
{
    auto len = scm_to_size_t(scm_length($input));
    for (std::size_t i = 0; i < len; ++i)
    {
        SCM s_type = scm_list_ref($input, scm_from_size_t(i));
        GNCAccountType type = (GNCAccountType)scm_to_int(s_type);
        types.push_back(type);
    }
    $1 = &types;
}

%typemap(in) GncOptionAccountTypeList&& (GncOptionAccountTypeList types)
{
    auto len = scm_to_size_t(scm_length($input));
    for (std::size_t i = 0; i < len; ++i)
    {
        SCM s_type = scm_list_ref($input, scm_from_size_t(i));
        GNCAccountType type = (GNCAccountType)scm_to_int(s_type);
        types.push_back(type);
    }
    $1 = &types;
}

%typemap(in) GncOptionAccountList
{
    auto len = scm_to_size_t(scm_length($input));
    for (std::size_t i = 0; i < len; ++i)
    {
        SCM s_account = scm_list_ref($input, scm_from_size_t(i));
        Account* acct = (Account*)SWIG_MustGetPtr(s_account,
                                                  SWIGTYPE_p_Account, 1, 0);
        $1.push_back(acct);
    }
}

%typemap(in) GncOptionAccountList& (GncOptionAccountList acclist)
{
    auto len = scm_to_size_t(scm_length($input));
    for (std::size_t i = 0; i < len; ++i)
    {
        SCM s_account = scm_list_ref($input, scm_from_size_t(i));
        Account* acct = (Account*)SWIG_MustGetPtr(s_account,
                                                  SWIGTYPE_p_Account, 1, 0);
        acclist.push_back(acct);
    }
    $1 = &acclist;
}

%typemap(out) GncOptionAccountList
{
    $result = SCM_EOL;
    for (auto acct : $1)
    {
        SCM elem = scm_list_1(SWIG_NewPointerObj(acct, SWIGTYPE_p_Account, 0));
        $result = scm_append(scm_list_2($result, elem));
    }
}

%typemap(out) GncOptionAccountList&
{
    $result = SCM_EOL;
    for (auto acct : *$1)
    {
        SCM elem = scm_list_1(SWIG_NewPointerObj(acct, SWIGTYPE_p_Account, 0));
        $result = scm_append(scm_list_2($result, elem));
    }
}

wrap_unique_ptr(GncOptionDBPtr, GncOptionDB);

%ignore swig_get_option(GncOption&);
%inline %{
#include "gnc-option.hpp"
#include "gnc-option-ui.hpp"

    GncOptionVariant& swig_get_option(GncOption* option)
    {
        return *option->m_option;
    }
%}

%ignore gnc_option_to_scheme;
%ignore gnc_option_from_scheme;

%include "gnc-option-date.hpp"
%include "gnc-option.hpp"
%include "gnc-option-impl.hpp"
%include "gnc-optiondb.hpp"
%include "gnc-optiondb-impl.hpp"

%extend GncOption {

    SCM get_scm_value()
    {
        return std::visit([](const auto& option)->SCM {
                auto value{option.get_value()};
                return scm_from_value(static_cast<decltype(value)>(value));
            }, swig_get_option($self));
    }
    SCM get_scm_default_value()
    {
        return std::visit([](const auto& option)->SCM {
                auto value{option.get_default_value()};
                return scm_from_value(static_cast<decltype(value)>(value));
            }, swig_get_option($self));
    }
    void set_value_from_scm(SCM new_value)
    {
        std::visit([new_value](auto& option) {
                option.set_value(scm_to_value<std::decay_t<decltype(option.get_value())>>(new_value));
            }, swig_get_option($self));
    }
};
%extend GncOptionDB {
    %template(set_option_string) set_option<std::string>;
    %template(set_option_int) set_option<int>;
    %template(set_option_time64) set_option<time64>;
 };

%inline %{
    using GncOptionDBPtr = std::unique_ptr<GncOptionDB>;

    static SCM
    gnc_option_value(const GncOptionDBPtr& optiondb, const char* section,
                     const char* name)
    {
        auto db_opt = optiondb->find_option(section, name);
        if (!db_opt)
            return SCM_BOOL_F;
        return GncOption_get_scm_value(db_opt);
    }

    static SCM
    gnc_option_default_value(const GncOptionDBPtr& optiondb,
                             const char* section, const char* name)
    {
        auto db_opt = optiondb->find_option(section, name);
        if (!db_opt)
            return SCM_BOOL_F;
        return GncOption_get_scm_default_value(db_opt);
    }

    static void
    gnc_set_option(const GncOptionDBPtr& optiondb, const char* section,
                   const char* name, SCM new_value)
    {
        auto db_opt = optiondb->find_option(section, name);
        if (!db_opt)
        {
//          PWARN("Attempt to write non-existent option %s/%s", section, name);
            return;
        }
        GncOption_set_value_from_scm(db_opt, new_value);
    }

    GncOptionDBPtr
    new_gnc_optiondb()
    {
        auto db_ptr{std::make_unique<GncOptionDB>()};
        return db_ptr;
    }
%}

#endif //SWIGGUILE
