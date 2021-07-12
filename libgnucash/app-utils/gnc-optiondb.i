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

      ~unique_ptr() = delete; //Otherwise swig takes the unique_ptr and calls delete on it.
  };
}

%define wrap_unique_ptr(Name, Type)
  %template(Name) std::unique_ptr<Type>;
  %newobject std::unique_ptr<Type>::release;

  %typemap(out) std::unique_ptr<Type> %{
    $result = SWIG_NewPointerObj(new $1_ltype(std::move($1)), $&1_descriptor, SWIG_POINTER_OWN);
  %}

%enddef

%typemap(in) std::size_t "$1 = scm_to_ulong($input);";
%typemap(out) std::size_t "$result = scm_from_ulong($1);";

 //%module sw_gnc_optiondb
%{
#include "gnc-optiondb.h"
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

%typemap (out) QofInstance_s* {
    swig_type_info *type = $descriptor(QofInstance_s);
    if ($1 == nullptr)
    {
        $result = SWIG_NewPointerObj(nullptr, type, FALSE);
    }
    else
    {

        auto ptr{static_cast<void*>(const_cast<QofInstance*>($1))};
        if (GNC_IS_COMMODITY($1))
            type = $descriptor(gnc_commodity*);
        else if (GNC_IS_ACCOUNT($1))
            type = $descriptor(Account*);
        else if (GNC_IS_BUDGET($1))
            type = $descriptor(GncBudget*);
        else if (GNC_IS_INVOICE($1))
            type = $descriptor(GncInvoice*);
        else if (GNC_IS_TAXTABLE($1))
            type = $descriptor(GncTaxTable*);
        else if (GNC_IS_CUSTOMER($1))
            type = $descriptor(_gncCustomer*);
        else if (GNC_IS_EMPLOYEE($1))
            type = $descriptor(_gncEmployee*);
        else if (GNC_IS_JOB($1))
            type = $descriptor(_gncJob*);
        else if (GNC_IS_VENDOR($1))
            type = $descriptor(_gncVendor*);
        $result = SWIG_NewPointerObj(ptr, type, FALSE);
    }
}

%typemap (in) QofInstance_s* {
    if (scm_is_true($input))
    {
        static const std::array<swig_type_info*, 10> types {
            $descriptor(QofInstance_s*), $descriptor(gnc_commodity*),
                $descriptor(GncBudget*), $descriptor(GncInvoice*),
                $descriptor(GncTaxTable*), $descriptor(Account*),
                $descriptor(_gncCustomer*), $descriptor(_gncEmployee*),
                $descriptor(_gncJob*), $descriptor(_gncVendor*)
                };
        void* ptr{};
        auto pos = std::find_if(types.begin(), types.end(),
                                [&$input, &ptr](auto type){
                                    SWIG_ConvertPtr($input, &ptr, type, 0);
                                    return ptr != nullptr; });
        if (pos == types.end())
            $1 = nullptr;
        else
            $1 = static_cast<QofInstance*>(ptr);
    }
    else
    {
        $1 = nullptr;
    }
 }

%inline %{
template <typename ValueType> inline SCM
    scm_from_value(ValueType value);
/*{
    return SCM_BOOL_F;
    }*/
template <> inline SCM
scm_from_value<SCM>(SCM value)
{
    return value;
}

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
    swig_type_info *type = SWIGTYPE_p_QofInstance_s;
    if (!value)
        return SWIG_NewPointerObj(nullptr, type, FALSE);

    auto ptr{static_cast<void*>(const_cast<QofInstance*>(value))};
    if (GNC_IS_COMMODITY(value))
        type = SWIGTYPE_p_gnc_commodity;
    else if (GNC_IS_ACCOUNT(value))
        type = SWIGTYPE_p_Account;
    else if (GNC_IS_BUDGET(value))
        type = SWIGTYPE_p_GncBudget;
    else if (GNC_IS_INVOICE(value))
        type = SWIGTYPE_p_GncInvoice;
    else if (GNC_IS_TAXTABLE(value))
        type = SWIGTYPE_p_GncTaxTable;
    else if (GNC_IS_CUSTOMER(value))
        type = SWIGTYPE_p__gncCustomer;
    else if (GNC_IS_EMPLOYEE(value))
        type = SWIGTYPE_p__gncEmployee;
    else if (GNC_IS_JOB(value))
        type = SWIGTYPE_p__gncJob;
    else if (GNC_IS_VENDOR(value))
        type = SWIGTYPE_p__gncVendor;
/* There is no type macro for QofQuery, it's not a GObject.
    else if (GNC_IS_QOFQUERY(value))
        type = SWIGTYPE_p_Query;
*/
    return SWIG_NewPointerObj(ptr, type, FALSE);
}

template <> inline SCM
scm_from_value<const QofQuery*>(const QofQuery* value)
{
    auto ptr{static_cast<void*>(const_cast<QofQuery*>(value))};
    return SWIG_NewPointerObj(ptr, SWIGTYPE_p__QofQuery, FALSE);
}

template <typename ValueType> inline ValueType
scm_to_value(SCM new_value)
{
    if constexpr (std::is_same_v<std::decay_t<ValueType>, SCM>)
        return new_value;
    return ValueType{};
}

template <> inline bool
scm_to_value<bool>(SCM new_value)
{
    return scm_is_true(new_value);
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

template <> inline double
scm_to_value<double>(SCM new_value)
{
    return scm_to_double(new_value);
}

template <> inline const QofInstance*
scm_to_value<const QofInstance*>(SCM new_value)
{
    if (new_value == SCM_BOOL_F)
        return nullptr;

    static const std::array<swig_type_info*, 10> types{
            SWIGTYPE_p_QofInstance_s, SWIGTYPE_p_gnc_commodity,
            SWIGTYPE_p_GncBudget, SWIGTYPE_p_GncInvoice,
            SWIGTYPE_p_GncTaxTable, SWIGTYPE_p_Account,
            SWIGTYPE_p__gncCustomer, SWIGTYPE_p__gncEmployee,
            SWIGTYPE_p__gncJob, SWIGTYPE_p__gncVendor
                };
    void* ptr{};
    auto pos = std::find_if(types.begin(), types.end(),
                            [&new_value, &ptr](auto type){
                                SWIG_ConvertPtr(new_value, &ptr, type, 0);
                                return ptr != nullptr; });
    if (pos == types.end())
        return nullptr;

    return static_cast<const QofInstance*>(ptr);
}

template <> inline const QofQuery*
scm_to_value<const QofQuery*>(SCM new_value)
{
    if (new_value == SCM_BOOL_F)
        return nullptr;
    void* ptr{};
    SWIG_ConvertPtr(new_value, &ptr, SWIGTYPE_p__QofQuery, 0);
    return static_cast<const QofQuery*>(ptr);
}

template <>inline GncOptionAccountList
scm_to_value<GncOptionAccountList>(SCM new_value)
{
    GncOptionAccountList retval{};
    if (scm_is_false(scm_list_p(new_value)) || scm_is_null(new_value))
        return retval;
    auto next{new_value};
    while (auto node{scm_car(next)})
    {
        void* account{};
        SWIG_ConvertPtr(node, &account, SWIGTYPE_p_Account, 0);
        if (account)
            retval.push_back(static_cast<Account*>(account));
        next = scm_cdr(next);
        if (scm_is_null(next))
            break;
    }
    return retval;
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
%ignore operator<<(std::ostream&, const RelativeDatePeriod);
%ignore operator>>(std::istream&, GncOption&);
%ignore GncOption::_get_option();


%rename(gnc_register_date_option_set)
    gnc_register_date_option(GncOptionDBPtr&, const char*, const char*,
                             const char*, const char*, RelativeDatePeriodVec&,
                             bool);

%typemap(typecheck, precedence=SWIG_TYPECHECK_INT64) time64 {
    $1 = scm_is_signed_integer($input, INT64_MAX, INT64_MIN);
}

%typemap(in) RelativeDatePeriodVec& (RelativeDatePeriodVec period_set)
{
    auto len = scm_is_true($input) ? scm_to_size_t(scm_length($input)) : 0;
    for (std::size_t i = 0; i < len; ++i)
    {
        SCM s_reldateperiod = scm_list_ref($input, scm_from_size_t(i));
        period_set.push_back(scm_relative_date_get_period(s_reldateperiod));
    }
    $1 = &period_set;
}

%typemap(in) GncMultichoiceOptionIndexVec (GncMultichoiceOptionIndexVec indexes)
{
    if (scm_is_true($input))
    {
        auto len{scm_to_size_t(scm_length($input))};
        for (std::size_t i = 0; i < len; ++i)
        {
            auto val{scm_list_ref($input, scm_from_size_t(i))};
            if (scm_is_unsigned_integer(val, 0, UINT_MAX))
                indexes.push_back(scm_to_unsigned_integer(val, 0, UINT_MAX));
        }
    }
    $1 = indexes;
}

%typemap(in) GncMultichoiceOptionChoices&& (GncMultichoiceOptionChoices choices)
{
    using KeyType = GncOptionMultichoiceKeyType;
    auto len = scm_is_true($input) ? scm_to_size_t(scm_length($input)) : 0;
    for (std::size_t i = 0; i < len; ++i)
    {
        SCM vec = scm_list_ref($input, scm_from_size_t(i));
        SCM keyval, v_ref_0 = SCM_SIMPLE_VECTOR_REF(vec, 0);
        GncOptionMultichoiceKeyType keytype;
        if (scm_is_symbol(v_ref_0))
        {
            keyval = scm_symbol_to_string(SCM_SIMPLE_VECTOR_REF(vec, 0));
            keytype = KeyType::SYMBOL;
        }
        else if (scm_is_string(v_ref_0))
        {
            keyval = SCM_SIMPLE_VECTOR_REF(vec, 0);
            keytype = KeyType::STRING;
        }
        else if (scm_is_integer(v_ref_0))
        {
            keyval = scm_number_to_string(v_ref_0, scm_from_uint(10u));
            keytype = KeyType::NUMBER;
        }
        else
            throw std::invalid_argument("Unsupported key type in multichoice option.");
        std::string key{scm_to_utf8_string(keyval)};
        std::string name{scm_to_utf8_string(SCM_SIMPLE_VECTOR_REF(vec, 1))};
        choices.push_back({std::move(key), std::move(name), keytype});
    }
    $1 = &choices;
 }


%typemap(in) GncOptionAccountList
{
    auto len = scm_is_true($input) ? scm_to_size_t(scm_length($input)) : 0;
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
    auto len = scm_is_true($input) ? scm_to_size_t(scm_length($input)) : 0;
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
    auto len = scm_is_true($input) ? scm_to_size_t(scm_length($input)) : 0;
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
    auto len = scm_is_true($input) ? scm_to_size_t(scm_length($input)) : 0;
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
    auto len = scm_is_true($input) ? scm_to_size_t(scm_length($input)) : 0;
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
#include <cassert>
#include <algorithm>
#include <array>
#include <string>
#include "gnc-option.hpp"
#include "gnc-option-ui.hpp"

    GncOptionVariant& swig_get_option(GncOption* option)
    {
        assert(option);
        return *option->m_option;
    }
%}

%ignore gnc_option_to_scheme;
%ignore gnc_option_from_scheme;
/* GncOptionDB::register_option comes in GncOption* and GncOption&&
 * overloads. The latter isn't useful to SWIG, ignore it.
 */
%ignore GncOptionDB::register_option(const char*, GncOption&&);

/* The following functions are overloaded in gnc-optiondb.hpp to provide both
 * GncOptionDB* and GncOptionDBPtr& versions. That confuses SWIG so ignore the
 * raw-ptr version.
 */
%ignore gnc_register_string_option(GncOptionDB*, const char* section, const char* name, const char* key, const char* doc_string, std::string value);
%ignore gnc_register_text_option(GncOptionDB*, const char*, const char*, const char*, const char*, std::string);
%ignore gnc_register_font_option(GncOptionDB*, const char*, const char*, const char*, const char*, std::string);
%ignore gnc_register_budget_option(GncOptionDB*, const char*, const char*, const char*, const char*, GncBudget*);
%ignore gnc_register_commodity_option(GncOptionDB*, const char*, const char*, const char*, const char*, gnc_commodity*);
%ignore gnc_register_simple_boolean_option(GncOptionDB*, const char* section, const char* name, const char* key, const char* doc_string, bool value);
%ignore gnc_register_complex_boolean_option(GncOptionDB*, const char* section, const char* name, const char* key, const char* doc_string, bool value);
%ignore gnc_register_pixmap_option(GncOptionDB*, const char*, const char*, const char*, const char*, std::string);
%ignore gnc_register_account_list_limited_option(GncOptionDB*, const char*, const char*, const char*, const char*, const GncOptionAccountList&, GncOptionAccountTypeList&&);
%ignore gnc_register_account_list_option(GncOptionDB*, const char*, const char*, const char*, const char*, const GncOptionAccountList&);
%ignore gnc_register_account_sel_limited_option(GncOptionDB*, const char*, const char*, const char*, const char*, const GncOptionAccountList&, GncOptionAccountTypeList&&);
%ignore gnc_register_multichoice_option(GncOptionDB*, const char*, const char*, const char*, const char*, const char*, GncMultichoiceOptionChoices&&);
%ignore gnc_register_list_option(GncOptionDB*, const char*, const char*, const char*, const char*, const char*, GncMultichoiceOptionChoices&&);
%ignore gnc_register_number_Plot_size_option(GncOptionDB*, const char*, const char*, const char*, const char*, int);
%ignore gnc_register_query_option(GncOptionDB*, const char*, const char*, const char*, const char*, QofQuery*);
%ignore gnc_register_color_option(GncOptionDB*, const char*, const char*, const char*, const char*, std::string);
%ignore gnc_register_internal_option(GncOptionDB*, const char*, const char*, const char*, const char*, std::string);
%ignore gnc_register_currency_option(GncOptionDB*, const char*, const char*, const char*, const char*, gnc_commodity*);
%ignore gnc_register_invoice_option(GncOptionDB*, const char*, const char*, const char*, const char*, GncInvoice*);
%ignore gnc_register_taxtable_option(GncOptionDB*, const char*, const char*, const char*, const char*, GncTaxTable*);
%ignore gnc_register_counter_option(GncOptionDB*, const char*, const char*, const char*, const char*, double);
%ignore gnc_register_counter_format_option(GncOptionDB*, const char*, const char*, const char*, const char*, std::string);
%ignore gnc_register_dateformat_option(GncOptionDB*, const char*, const char*, const char*, const char*, std::string);
%ignore gnc_register_date_option(GncOptionDB*, const char*, const char*, const char*, const char*, RelativeDatePeriod, RelativeDateUI);
%ignore gnc_register_date_option(GncOptionDB*, const char*, const char*, const char*, const char*, time64, RelativeDateUI);
%ignore gnc_register_date_option(GncOptionDB*, const char*, const char*, const char*, const char*, RelativeDatePeriodVec, bool);
%ignore gnc_register_start_date_option(GncOptionDB*, const char*, const char*, const char*, const char*, bool);
%ignore gnc_register_end_date_option(GncOptionDB*, const char*, const char*, const char*, const char*, bool);

%typemap(in) GncOption* "$1 = scm_is_true($input) ? static_cast<GncOption*>(scm_to_pointer($input)) : nullptr;"
%typemap(out) GncOption* "$result = ($1) ? scm_from_pointer($1, nullptr) : SCM_BOOL_F;"

%header %{

    inline static RelativeDatePeriod scm_relative_date_get_period(SCM date)
    {
    static SCM reldate_values = SCM_BOOL_F;

    if (scm_is_false(reldate_values))
        reldate_values = scm_c_eval_string(
            "'((absolute RelativeDatePeriod-ABSOLUTE)"
            "(today RelativeDatePeriod-TODAY)"
            "(one-week-ago RelativeDatePeriod-ONE-WEEK-AGO)"
            "(one-week-ahead RelativeDatePeriod-ONE-WEEK-AHEAD)"
            "(one-month-ago RelativeDatePeriod-ONE-MONTH-AGO)"
            "(one-month-ahead RelativeDatePeriod-ONE-MONTH-AHEAD)"
            "(three-months-ago RelativeDatePeriod-THREE-MONTHS-AGO)"
            "(three-months-ahead RelativeDatePeriod-THREE-MONTHS-AHEAD)"
            "(six-months-ago RelativeDatePeriod-SIX-MONTHS-AGO)"
            "(six-months-ahead RelativeDatePeriod-SIX-MONTHS-AHEAD)"
            "(one-year-ago RelativeDatePeriod-ONE-YEAR-AGO)"
            "(one-year-ahead RelativeDatePeriod-ONE-YEAR-AHEAD)"
            "(start-this-month RelativeDatePeriod-START-THIS-MONTH)"
            "(end-this-month RelativeDatePeriod-END-THIS-MONTH)"
            "(start-prev-month RelativeDatePeriod-START-PREV-MONTH)"
            "(end-prev-month RelativeDatePeriod-END-PREV-MONTH)"
            "(start-next-month RelativeDatePeriod-START-NEXT-MONTH)"
            "(end-next-month RelativeDatePeriod-END-NEXT-MONTH)"
            "(start-current-quarter RelativeDatePeriod-START-CURRENT-QUARTER)"
            "(end-current-quarter RelativeDatePeriod-END-CURRENT-QUARTER)"
            "(start-prev-quarter RelativeDatePeriod-START-PREV-QUARTER)"
            "(end-prev-quarter RelativeDatePeriod-END-PREV-QUARTER)"
            "(start-next-quarter RelativeDatePeriod-START-NEXT-QUARTER)"
            "(end-next-quarter RelativeDatePeriod-END-NEXT-QUARTER)"
            "(start-cal-year RelativeDatePeriod-START-CAL-YEAR)"
            "(end-cal-year RelativeDatePeriod-END-CAL-YEAR)"
            "(start-prev-year RelativeDatePeriod-START-PREV-YEAR)"
            "(end-prev-year RelativeDatePeriod-END-PREV-YEAR)"
            "(start-next-year RelativeDatePeriod-START-NEXT-YEAR)"
            "(end-next-year RelativeDatePeriod-END-NEXT-YEAR)"
            "(start-accounting-period RelativeDatePeriod-START-ACCOUNTING-PERIOD)"
            "(end-accounting-period RelativeDatePeriod-END-ACCOUNTING-PERIOD))");

    auto reldate_scm{scm_is_pair(date) ? scm_cdr(date) : date};
    auto reldate{scm_primitive_eval(scm_assq_ref(reldate_values,
                                                 reldate_scm))};
    return static_cast<RelativeDatePeriod>(scm_to_int(reldate));

    }

    inline static bool scm_date_absolute(SCM date)
    {
        if (scm_is_pair(date))
        {
            auto car{scm_to_utf8_string(scm_symbol_to_string(scm_car(date)))};
            if (strcmp(car, "relative") == 0)
                return false;
        }
        return true;
    }

    inline static time64 scm_absolute_date_to_time64(SCM date)
    {
        if (scm_date_absolute(date))
            return scm_to_int64(scm_is_pair(date) ? scm_cdr(date) : date);

        return gnc_relative_date_to_time64(scm_relative_date_get_period(date));
    }

    %}

%ignore GncOptionMultichoiceKeyType;

%inline %{
    inline GncMultichoiceOptionIndexVec
    scm_to_multichoices(const SCM new_value,
                        const GncOptionMultichoiceValue& option)
    {
        static const auto size_t_max = std::numeric_limits<std::size_t>::max();
        static const char* empty{""};
        auto scm_to_str = [](auto item)->const char* {
                if (scm_is_integer(item))
                    scm_number_to_string(item, scm_from_uint(10u));
                if (scm_is_symbol(item))
                    return scm_to_utf8_string(scm_symbol_to_string(item));
                else if (scm_is_string(item))
                    return scm_to_utf8_string(item);
                else return empty;
            };
        GncMultichoiceOptionIndexVec vec;
        auto choice_is_list{option.get_ui_type() == GncOptionUIType::LIST}; 
        if (scm_is_list(new_value))
        {
            if (!choice_is_list)
              throw std::invalid_argument{"Attempt to set multichoice with a list of values."};
            auto len{scm_to_size_t(scm_length(new_value))};
            for (std::size_t i = 0; i < len; ++i)
            {
                auto item{scm_list_ref(new_value, scm_from_size_t(i))};
                auto index{option.permissible_value_index(scm_to_str(item))};
                if (index < size_t_max)
                    vec.push_back(index);
            }
        }
        else
        {
            auto index{option.permissible_value_index(scm_to_str(new_value))};
            if (index < size_t_max)
                vec.push_back(index);
        }
        return vec;
    }

    inline SCM scm_from_multichoices(const GncMultichoiceOptionIndexVec& indexes,
                                     const GncOptionMultichoiceValue& option)
    {
        using KeyType = GncOptionMultichoiceKeyType;
        auto scm_value = [](const char* value, KeyType keytype) -> SCM {
            auto scm_str{scm_from_utf8_string(value)};
            switch (keytype)
            {
                case KeyType::SYMBOL:
                    return scm_string_to_symbol(scm_str);
                case KeyType::STRING:
                    return scm_str;
                case KeyType::NUMBER:
                    return scm_string_to_number(scm_str,
                                                scm_from_int(10));
            };
            return SCM_BOOL_F;
        };

        if (option.get_ui_type() == GncOptionUIType::MULTICHOICE)
            return scm_value(option.permissible_value(indexes[0]),
                             option.get_keytype(indexes[0]));
        auto values{SCM_BOOL_F};
        for(auto index : indexes)
        {
            auto val{scm_list_1(scm_value(option.permissible_value(index),
                                          option.get_keytype(index)))};
            if (scm_is_true(values))
                values = scm_append(scm_list_2(val, values));
            else
                values = val;
        }
        return scm_reverse(values);
    }

    SCM get_scm_value(const GncOptionMultichoiceValue& option)
    {

        auto indexes = option.get_multiple();
        if (indexes.empty())
            indexes = option.get_default_multiple();
        if (indexes.empty())
            return SCM_BOOL_F;
        return scm_from_multichoices(indexes, option);
     }

    SCM get_scm_default_value(const GncOptionMultichoiceValue& option)
    {

        auto indexes = option.get_default_multiple();
        if (indexes.empty())
            return SCM_BOOL_F;
        return scm_from_multichoices(indexes, option);
     }

    SCM get_scm_value(const GncOptionRangeValue<int>& option)
    {
        auto val{option.get_value()};
        auto desig{scm_c_eval_string(val > 100 ? "'pixels" : "'percent")};
        return scm_cons(desig, scm_from_int(val));
    }

    SCM get_scm_default_value(const GncOptionRangeValue<int>& option)
    {
        auto val{option.get_default_value()};
        auto desig{scm_c_eval_string(val > 100 ? "'pixels" : "'percent")};
        return scm_cons(desig, scm_from_int(val));
    }

    %}

%include "gnc-option-date.hpp"
%include "gnc-option.hpp"
%include "gnc-option-impl.hpp"
%include "gnc-optiondb.h"
%include "gnc-optiondb.hpp"
%include "gnc-optiondb-impl.hpp"
%include "gnc-option-uitype.hpp"

%template(gnc_make_string_option) gnc_make_option<std::string>;
%template(gnc_make_bool_option) gnc_make_option<bool>;
%template(gnc_make_int64_option) gnc_make_option<int64_t>;
%template(gnc_make_qofinstance_option) gnc_make_option<const QofInstance*>;
%template(gnc_make_query_option) gnc_make_option<const QofQuery*>;

%extend GncOption {
    SCM get_scm_value()
    {
        if (!$self)
            return SCM_BOOL_F;
        return std::visit([](const auto& option)->SCM {
                if constexpr (std::is_same_v<std::decay_t<decltype(option)>,
                              GncOptionMultichoiceValue> ||
                              std::is_same_v<std::decay_t<decltype(option)>,
                              GncOptionRangeValue<int>>)
                    return get_scm_value(option);
                auto value{option.get_value()};
                if constexpr (std::is_same_v<std::decay_t<decltype(value)>,
                              SCM>)
                    return value;
                return scm_from_value(static_cast<decltype(value)>(value));
            }, swig_get_option($self));
    }
    SCM get_scm_default_value()
    {
        if (!$self)
            return SCM_BOOL_F;
        return std::visit([](const auto& option)->SCM {
                if constexpr (std::is_same_v<std::decay_t<decltype(option)>,
                              GncOptionMultichoiceValue> ||
                              std::is_same_v<std::decay_t<decltype(option)>,
                              GncOptionRangeValue<int>>)
                    return get_scm_default_value(option);
                auto value{option.get_default_value()};
                if constexpr (std::is_same_v<std::decay_t<decltype(value)>,
                              SCM>)
                    return value;
                return scm_from_value(static_cast<decltype(value)>(value));
            }, swig_get_option($self));
    }
    void set_value_from_scm(SCM new_value)
    {
        if (!$self)
            return;
        std::visit([new_value](auto& option) {
                if constexpr (std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionDateValue>)
                {
                    if (scm_date_absolute(new_value))
                        option.set_value(scm_absolute_date_to_time64(new_value));
                    else
                        option.set_value(scm_relative_date_get_period(new_value));
                    return;
                }

                if constexpr (std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionMultichoiceValue>)
                {
                    option.set_multiple(scm_to_multichoices(new_value, option));
                    return;
                }

                if constexpr (std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionRangeValue<int>>)
                {
                    if (scm_is_pair(new_value))
                        option.set_value(scm_to_int(scm_cdr(new_value)));
                    else
                        option.set_value(scm_to_int(new_value));
                    return;
                }

                auto value{scm_to_value<std::decay_t<decltype(option.get_value())>>(new_value)};  //Can't inline, set_value takes arg by reference.
                option.set_value(value);
            }, swig_get_option($self));
    }

    void set_default_value_from_scm(SCM new_value)
    {
        if (!$self)
            return;
        std::visit([new_value](auto& option) {
                if constexpr (std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionDateValue>)
                {
                    if (scm_date_absolute(new_value))
                        option.set_default_value(scm_absolute_date_to_time64(new_value));
                    else
                        option.set_default_value(scm_relative_date_get_period(new_value));
                    return;
                }
                if constexpr (std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionMultichoiceValue>)
                {
                    option.set_default_multiple(scm_to_multichoices(new_value,
                                                                    option));
                    return;
                }
                if constexpr (std::is_same_v<std::decay_t<decltype(option)>,
                               GncOptionRangeValue<int>>)
                {
                    if (scm_is_pair(new_value))
                        option.set_default_value(scm_to_int(scm_cdr(new_value)));
                    else
                        option.set_default_value(scm_to_int(new_value));
                    return;
                }
                auto value{scm_to_value<std::decay_t<decltype(option.get_value())>>(new_value)};  //Can't inline, set_value takes arg by reference.
                option.set_default_value(value);
            }, swig_get_option($self));
    }

    SCM get_type()
    {
        if (!self)
            return SCM_BOOL_F;
        return std::visit([](const auto& option)->SCM {
                if constexpr (std::is_same_v<std::decay_t<decltype(option)>,
                    GncOptionMultichoiceValue>)
                    return scm_c_eval_string("'multichoice");
                else if constexpr (std::is_same_v<decltype(option.get_value()),
                    bool>)
                    return scm_c_eval_string("'boolean");
                else
                    return SCM_BOOL_F;
            }, swig_get_option($self));
    }
};

%extend GncOptionDB {
    %template(set_option_string) set_option<std::string>;
    %template(set_option_int) set_option<int>;
    %template(set_option_time64) set_option<time64>;
};

%template(gnc_register_number_range_option_double) gnc_register_number_range_option<double>;
%template(gnc_register_number_range_option_int) gnc_register_number_range_option<int>;

%inline %{
    GncOption* gnc_make_account_list_option(const char* section,
                                            const char* name, const char* key,
                                            const char* doc_string,
                                            const GncOptionAccountList& value)
    {
        try {
            return new GncOption{GncOptionAccountValue{section, name, key,
                        doc_string, GncOptionUIType::ACCOUNT_LIST, value}};
        }
        catch (const std::exception& err)
        {
            std::cerr << "Make account list option threw unexpected exception " << err.what() << ", option not created." << std::endl;
            return nullptr;
        }
    }

    GncOption* gnc_make_account_list_limited_option(const char* section,
                                                    const char* name,
                                                    const char* key,
                                                    const char* doc_string,
                                                    const GncOptionAccountList& value,
                                                    GncOptionAccountTypeList&& allowed)
    {
        try
        {
            return new GncOption{GncOptionAccountValue{section, name, key,
                        doc_string, GncOptionUIType::ACCOUNT_LIST, value,
                        std::move(allowed)}};
        }
        catch (const std::invalid_argument& err)
        {
            std::cerr << "Account List Limited Option, value failed validation, option not created.\n";
            return nullptr;
        }
    }

    GncOption* gnc_make_account_sel_limited_option(const char* section,
                                                   const char* name,
                                                   const char* key,
                                                   const char* doc_string,
                                                   const GncOptionAccountList& value,
                                                   GncOptionAccountTypeList&& allowed)
    {
        try
        {
            return new GncOption{GncOptionAccountValue{section, name, key,
                        doc_string, GncOptionUIType::ACCOUNT_SEL, value,
                        std::move(allowed)}};
        }
        catch (const std::invalid_argument& err)
        {
            std::cerr <<"Account Sel Limited Option, value failed validation, option not creted.\n";
            return nullptr;
        }
    }

    GncOption* gnc_make_date_option(const char* section,
                                    const char* name, const char* key,
                                    const char* doc_string,
                                    const SCM default_val,
                                    RelativeDatePeriodVec& period_set,
                                    bool both)
    {

        try {
            auto absolute{scm_date_absolute(default_val)};
            auto ui_type = both ? GncOptionUIType::DATE_BOTH : absolute ?
                GncOptionUIType::DATE_ABSOLUTE : GncOptionUIType::DATE_RELATIVE;
            if (!period_set.empty())
            {
                auto retval{new GncOption{GncOptionDateValue(section, name, key,
                                                             doc_string, ui_type,
                                                             period_set)}};
                if (absolute)
                    retval->set_default_value(scm_absolute_date_to_time64(default_val));
                else
                    retval->set_default_value(scm_relative_date_get_period(default_val));
                return retval;
            }

            if (absolute)
            {
                auto value{scm_absolute_date_to_time64(default_val)};
                auto retval{new GncOption{GncOptionDateValue(section, name, key,
                                                             doc_string, ui_type,
                                                             value)}};
                return retval;
            }
            auto value{scm_relative_date_get_period(default_val)};
            auto retval{new GncOption{GncOptionDateValue(section, name, key,
                                                         doc_string, ui_type,
                                                         period_set)}};
            return retval;
        }
        catch (const std::invalid_argument& err)
        {
            std::cerr <<"Date Option, value failed validation, option not creted.\n";
            return nullptr;
        }
    }

    GncOption* gnc_make_multichoice_option(const char* section,
                                           const char* name, const char* key,
                                           const char* doc_string,
                                           const char* default_val,
                                           GncMultichoiceOptionChoices&& choices)
    {
        try {
            std::string defval{default_val};
            auto found{std::find_if(choices.begin(), choices.end(),
                                    [&defval](auto& choice)->bool {
                                        return defval == std::get<0>(choice);
                                    })};
            if (found == choices.end())
                defval = (choices.empty() ? std::string{"None"} :
                          std::get<0>(choices.at(0)));
            return new GncOption{GncOptionMultichoiceValue{section, name, key,
                        doc_string, defval.c_str(), std::move(choices),
                        GncOptionUIType::MULTICHOICE}};
        }
        catch (const std::exception& err)
        {
            std::cerr << "Make multichoice option threw unexpected exception " << err.what() << ", option not created." << std::endl;
            return nullptr;
        }
    }

    GncOption* gnc_make_list_option(const char* section,
                                    const char* name, const char* key,
                                    const char* doc_string,
                                    GncMultichoiceOptionIndexVec indexes,
                                    GncMultichoiceOptionChoices&& list)
    {
        try {
            return new GncOption{GncOptionMultichoiceValue{section, name, key,
                        doc_string, std::move(indexes), std::move(list),
                        GncOptionUIType::LIST}};
        }
        catch (const std::exception& err)
        {
            std::cerr << "Make list option threw unexpected exception " << err.what() << ", option not created." << std::endl;
            return nullptr;
        }
    }

    GncOption* gnc_make_range_value_option(const char* section,
                                           const char* name, const char* key,
                                           const char* doc_string, double value,
                                           double min, double max, double step)
    {
        try
        {
            return new GncOption{GncOptionRangeValue<double>{section, name, key,
                        doc_string, value, min,
                        max, step}};
        }
        catch(const std::invalid_argument& err)
        {
            std::cerr <<"Number Range Option " << err.what() << ", option not created.\n";
            return nullptr;
        }
    }

    GncOption* gnc_make_plot_size_option(const char* section,
                                         const char* name, const char* key,
                                         const char* doc_string, int value,
                                         int min, int max, int step)
    {
        try
        {
            return new GncOption{GncOptionRangeValue<int>{section, name, key,
                        doc_string, value, min,
                        max, step}};
        }
        catch(const std::invalid_argument& err)
        {
            std::cerr <<"Plot Size Option " << err.what() << ", option not created.\n";
            return nullptr;
        }
    }

    GncOption* gnc_make_commodity_option(const char* section,
                                        const char* name, const char* key,
                                        const char* doc_string,
                                        gnc_commodity *value)
    {
        return new GncOption{GncOptionValue<const QofInstance*>{
                section, name, key, doc_string, (const QofInstance*)value}};
    }

    GncOption* gnc_make_commodity_option(const char* section,
                                        const char* name, const char* key,
                                        const char* doc_string,
                                        const char *value)
    {
        gnc_commodity* commodity{};
        const auto book{qof_session_get_book(gnc_get_current_session())};
        const auto commodity_table{gnc_commodity_table_get_table(book)};
        const auto namespaces{gnc_commodity_table_get_namespaces(commodity_table)};
        for (auto node = namespaces; node && commodity == nullptr; node = g_list_next(node))
            commodity = gnc_commodity_table_lookup(commodity_table,
                                                   (const char*)(node->data),
                                                   value);

        if (commodity)
            return gnc_make_commodity_option(section, name, key, doc_string,
                                            commodity);

        return nullptr;
    }

    GncOption* gnc_make_currency_option(const char* section,
                                        const char* name, const char* key,
                                        const char* doc_string,
                                        gnc_commodity *value)
    {
        try
        {
            return new GncOption{GncOptionValidatedValue<const QofInstance*>{
                    section, name, key, doc_string, (const QofInstance*)value,
                        [](const QofInstance* new_value) -> bool
                    {
                        return GNC_IS_COMMODITY (new_value) &&
                            gnc_commodity_is_currency(GNC_COMMODITY(new_value));
                    },
                        GncOptionUIType::CURRENCY
                            }
            };
        }
        catch (const std::exception& err)
        {
            std::cerr << "gnc_make_currency_option threw " << err.what() <<
                ", option not created." << std::endl;
            return nullptr;
        }
    }

    using GncOptionDBPtr = std::unique_ptr<GncOptionDB>;
/* Forward decls */
    GncOptionDBPtr new_gnc_optiondb();
    GncOption* gnc_lookup_option(const GncOptionDBPtr& optiondb,
                                 const char* section, const char* name);

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
    gnc_option_db_lookup_value(const GncOptionDB* optiondb, const char* section,
                               const char* name)
    {
        auto db_opt = optiondb->find_option(section, name);
        if (!db_opt)
            return SCM_BOOL_F;
        return GncOption_get_scm_value(const_cast<GncOption*>(db_opt));
    }

    static SCM
    gnc_option_default_value(const GncOptionDBPtr& optiondb,
                             const char* section, const char* name)
    {
        auto db_opt{optiondb->find_option(section, name)};
        if (!db_opt)
            return SCM_BOOL_F;
        return GncOption_get_scm_default_value(db_opt);
    }

    static void
    gnc_set_option(const GncOptionDBPtr& optiondb, const char* section,
                   const char* name, SCM new_value)
    {
        auto db_opt{optiondb->find_option(section, name)};
        if (!db_opt)
        {
            std::cerr <<"Attempt to write non-existent option " << section
                << "/" << name;
            return;
        }
        try
        {
            GncOption_set_value_from_scm(db_opt, new_value);
        }
        catch(const std::invalid_argument& err)
        {
            std::cerr << "Failed to set option " << section << "/" << name
                      << ": " << err.what() << "\n";
        }
    }

    GncOptionDBPtr
    new_gnc_optiondb()
    {
        auto db_ptr{std::make_unique<GncOptionDB>()};
        return db_ptr;
    }

    GncOption*
    gnc_lookup_option(const GncOptionDBPtr& optiondb, const char* section,
                      const char* name)
    {
        return optiondb->find_option(section, name);
    }

    void
    gnc_option_db_set_option_selectable_by_name(GncOptionDBPtr& odb,
                                                const char* section,
                                                const char* name,
                                                bool selectable)
    {
        auto option{odb->find_option(section, name)};
        option->set_ui_item_selectable(selectable);
    }

    void
    gnc_optiondb_foreach(GncOptionDBPtr& odb, SCM thunk)
    {
        odb->foreach_section(
            [&thunk](const GncOptionSectionPtr& section)
            {
                section->foreach_option(
                    [&thunk](auto& option)
                    {
                        auto optvoidptr{reinterpret_cast<void*>(
                                const_cast<GncOption*>(&option))};
                        auto scm_opt{scm_from_pointer(optvoidptr, nullptr)};
                        scm_call_1(thunk, scm_opt);
                    });
            });
    }
%}

#endif //SWIGGUILE
