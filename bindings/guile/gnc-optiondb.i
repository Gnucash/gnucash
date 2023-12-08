/*
 * gnc-optiondb.i -- Swig Guile interface for the options system.
 *
 * Copyright 2021 John Ralls <jralls@ceridwen.us>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/* unique_ptr SWIG wrapper from
 * https://stackoverflow.com/questions/27693812/how-to-handle-unique-ptrs-with-swig
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

%begin
%{
#include <gnc-optiondb.h>
#include <gnc-optiondb.hpp>
#include <gnc-optiondb-impl.hpp>
#include <gnc-option-date.hpp>
#include <array>
#include <sstream>
#include <iomanip>
#include <guile-mappings.h>
  %}
%{
static const QofLogModule log_module = "gnc.optiondb";

SCM scm_init_sw_gnc_optiondb_module(void);
/*Windows.h defines ERROR but SWIG needs it to, so undef it. */
#ifdef ERROR
#undef ERROR
#endif
/*Something somewhere in windows.h defines ABSOLUTE to something and
 *that contaminates using it in RelativeDateType.  Undef it.
 */
#ifdef ABSOLUTE
#undef ABSOLUTE
#endif

%}

%ignore gnc_get_current_session(void);

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
        SCM instance{$input};
        auto pos = std::find_if(types.begin(), types.end(),
                                [&instance, &ptr](auto type){
                                    SWIG_ConvertPtr(instance, &ptr, type, 0);
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
static inline QofBook*
get_current_book(void )
{
    return qof_session_get_book(gnc_get_current_session());
}

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
        type = SWIGTYPE_p_budget_s;
    else if (GNC_IS_INVOICE(value))
        type = SWIGTYPE_p__gncInvoice;
    else if (GNC_IS_TAXTABLE(value))
        type = SWIGTYPE_p__gncTaxTable;
    else if (GNC_IS_CUSTOMER(value))
        type = SWIGTYPE_p__gncCustomer;
    else if (GNC_IS_EMPLOYEE(value))
        type = SWIGTYPE_p__gncEmployee;
    else if (GNC_IS_JOB(value))
        type = SWIGTYPE_p__gncJob;
    else if (GNC_IS_VENDOR(value))
        type = SWIGTYPE_p__gncVendor;

    return SWIG_NewPointerObj(ptr, type, FALSE);
}

template <> inline SCM
scm_from_value<QofInstance*>(QofInstance* value)
{
    return scm_from_value<const QofInstance*>(value);
}

template <> inline SCM
scm_from_value<const Account*>(const Account* value)
{
    return scm_from_value<const QofInstance*>(QOF_INSTANCE(value));
}

template <> inline SCM
scm_from_value<gnc_commodity*>(gnc_commodity* value)
{
    if (!value)
        return SCM_BOOL_F;
    return scm_from_value<const QofInstance*>((const QofInstance*)value);
}

template <> inline SCM
scm_from_value<QofQuery*>(QofQuery* value)
{
    return gnc_query2scm(value);
}

template <> inline SCM
scm_from_value<const QofQuery*>(const QofQuery* value)
{
    return scm_from_value<QofQuery*>(const_cast<QofQuery*>(value));
}

template <> inline SCM
scm_from_value<const GncOwner*>(const GncOwner* value)
{
    auto ptr{static_cast<void*>(const_cast<GncOwner*>(value))};
    return SWIG_NewPointerObj(ptr, SWIGTYPE_p__gncOwner, FALSE);
}

template <> inline SCM
scm_from_value<GncOwner*>(GncOwner* value)
{
    return scm_from_value<const GncOwner*>(value);
}

template <>inline SCM
scm_from_value<GncOptionAccountList>(GncOptionAccountList value)
{
    SCM s_list = SCM_EOL;
    auto book{get_current_book()};
    for (auto guid : value)
    {
        auto acct{xaccAccountLookup(&guid, book)};
        s_list = scm_cons(SWIG_NewPointerObj(acct, SWIGTYPE_p_Account, 0),
                          s_list);
    }
    return scm_reverse(s_list);
}

template <>inline SCM
scm_from_value<GncOptionReportPlacementVec>(GncOptionReportPlacementVec value)
{
    SCM s_list = SCM_EOL;
    for (auto placement : value)
   {
        auto [id, wide, high] = placement;
        auto scm_id{scm_from_uint32(id)};
        auto scm_wide{scm_from_uint32(wide)};
        auto scm_high{scm_from_uint32(high)};
        /* The trailing SCM_BOOL_F is a placeholder for a never-used callback function,
         * present for backward compatibility so that older GnuCash versions can load a
         * saved multicolumn report.
         */
        s_list = scm_cons(scm_list_4(scm_id, scm_wide, scm_high, SCM_BOOL_F), s_list);
    }
    return scm_reverse(s_list);
}

template <> inline SCM
scm_from_value<GncOptionDateFormat>(GncOptionDateFormat value)
{
    return SCM_BOOL_F;
}

static std::string
scm_color_list_to_string(SCM list)
{
    std::ostringstream oss{};
    oss << std::hex << std::setfill('0');
    SCM cdr = list;
    while (scm_is_pair(cdr))
    {
        if (scm_is_rational(SCM_CAR(cdr)))
            oss << std::setw(2) <<
                static_cast<unsigned int>(scm_to_double(SCM_CAR(cdr)));
        cdr = SCM_CDR(cdr);
    }
    if (scm_is_rational(cdr))
        oss << std::setw(2) << static_cast<unsigned int>(scm_to_double(cdr));
    return oss.str();
}

template <typename ValueType> inline ValueType
scm_to_value(SCM new_value)
{
    if constexpr (is_same_decayed_v<ValueType, SCM>)
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
    if (scm_is_true(scm_list_p(new_value)))
        return scm_color_list_to_string(new_value);
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
        SWIGTYPE_p_budget_s, SWIGTYPE_p__gncInvoice,
        SWIGTYPE_p__gncTaxTable, SWIGTYPE_p_Account,
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

template <> inline gnc_commodity*
scm_to_value<gnc_commodity*>(SCM new_value)
{
    auto comm{scm_to_value<const QofInstance*>(new_value)};
    if (comm)
        return GNC_COMMODITY(comm);
    if (scm_is_list(new_value))
    {
        auto len{scm_to_uint(scm_length(new_value))};
        auto mnemonic{scm_to_utf8_string(scm_list_ref(new_value, scm_from_uint(0)))};
        auto name_space{(len > 1) ? scm_to_utf8_string(scm_list_ref(new_value,
                                                                    scm_from_uint(1)))
                : strdup ("CURRENCY")};
        auto book{get_current_book()};
        auto table = gnc_commodity_table_get_table(book);
        auto rv = gnc_commodity_table_lookup (table, name_space, mnemonic);
        free (name_space);
        free (mnemonic);
        return rv;
    }
    if (scm_is_string(new_value))
    {
        auto book{get_current_book()};
        auto table = gnc_commodity_table_get_table(book);
        auto mnemonic{scm_to_utf8_string(new_value)};
        auto rv = gnc_commodity_table_lookup(table, "CURRENCY", mnemonic);
        free (mnemonic);
        return rv;
    }
    return nullptr;
}

template <> inline const Account*
scm_to_value<const Account*>(SCM new_value)
{
    return GNC_ACCOUNT(scm_to_value<const QofInstance*>(new_value));
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

template <> inline const GncOwner*
scm_to_value<const GncOwner*>(SCM new_value)
{
    if (new_value == SCM_BOOL_F)
        return nullptr;
    void* ptr{};
    SWIG_ConvertPtr(new_value, &ptr, SWIGTYPE_p__gncOwner, 0);
    return static_cast<const GncOwner*>(ptr);
}

template <>inline GncOptionAccountList
scm_to_value<GncOptionAccountList>(SCM new_value)
{
    GncOptionAccountList retval{};
    if (scm_is_false(scm_list_p(new_value)) || scm_is_null(new_value))
        return retval;

    retval.reserve(scm_to_size_t(scm_length(new_value)));

    auto next{new_value};
    auto from_report{true};
    while (!scm_is_null(next) && scm_car(next))
    {
/* If the incoming scheme is from a report then it will contain an Account*, if
 * it's from restoring a saved report config it will be a guid.
 */
        if (scm_is_string(scm_car(next)))
        {
            auto guid_str{scm_to_utf8_string(scm_car(next))};
            from_report = false;
            GncGUID guid;
            string_to_guid(guid_str, &guid);
            retval.push_back(guid);
            free (guid_str);
        }
        else
        {
            void *account{};
            SWIG_ConvertPtr(scm_car(next), &account, SWIGTYPE_p_Account, 0);
            if (account)
            {
                auto guid{qof_entity_get_guid(static_cast<Account *>(account))};
                retval.push_back(*guid);
            }
        }
        next = scm_cdr(next);
    }

    if (!from_report)
        std::reverse(retval.begin(), retval.end());
    return retval;
}

template <>inline GncOptionReportPlacementVec
scm_to_value<GncOptionReportPlacementVec>(SCM new_value)
{
    GncOptionReportPlacementVec rp;
    GncOptionAccountList retval{};
    if (scm_is_false(scm_list_p(new_value)) || scm_is_null(new_value))
        return rp;
    auto next{new_value};
    while (auto node{scm_car(next)})
    {
        auto id{scm_to_uint32(scm_car(node))};
        auto wide{scm_to_uint32(scm_cadr(node))};
        auto high{scm_to_uint32(scm_caddr(node))};
        rp.emplace_back(id, wide, high);
        next = scm_cdr(next);
        if (scm_is_null(next))
            break;
    }
    return rp;
}

QofBook* gnc_option_test_book_new();
void gnc_option_test_book_destroy(QofBook*);

QofBook*
gnc_option_test_book_new()
{
    return get_current_book();
}

void
gnc_option_test_book_destroy(QofBook* book)
{
    gnc_clear_current_session();
}

%}

%ignore OptionClassifier;
%ignore OptionUIItem;
%nodefaultctor GncOption;
%ignore GncOptionMultichoiceValue(GncOptionMultichoiceValue&&);
%ignore GncOptionMultichoiceValue::operator=(const GncOptionMultichoiceValue&);
%ignore GncOptionMultichoiceValue::operator=(GncOptionMultichoiceValue&&);
%ignore GncOptionQofInstanceValue(GncOptionQofInstanceValue&&);
%ignore GncOptionQofInstanceValue::operator=(const GncOptionQofInstanceValue&);
%ignore GncOptionQofInstanceValue::operator=(GncOptionQofInstanceValue&&);
%ignore GncOptionCommodityValue(GncOptionCommodityValue&&);
%ignore GncOptionCommodityValue::operator=(const GncOptionCommodityValue&);
%ignore GncOptionCommodityValue::operator=(GncOptionCommodityValue&&);
%ignore GncOptionDateValue(GncOptionDateValue&&);
%ignore GncOptionDateValue::operator=(const GncOptionDateValue&);
%ignore GncOptionDateValue::operator=(GncOptionDateValue&&);
%ignore GncOptionDateValue::set_value(uint16_t); // Used only by dialog-options
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

%typemap(in) RelativeDatePeriod (RelativeDatePeriod rdp)
{
      if (scm_is_integer($input))
          rdp = (RelativeDatePeriod) scm_to_int($input);
      else if (scm_is_symbol($input))
          rdp = scm_relative_date_get_period($input);
      else
          rdp = RelativeDatePeriod::TODAY;

      $1 = rdp;
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
        auto key{scm_to_utf8_string(keyval)};
        auto name{scm_to_utf8_string(SCM_SIMPLE_VECTOR_REF(vec, 1))};
        choices.push_back({key, name, keytype});
        free (name);
        free (key);
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
        if (acct)
            $1.push_back(*qof_entity_get_guid(acct));
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

%typemap(in) GncOptionAccountList const & (GncOptionAccountList alist)
{
    auto len = scm_is_true($input) ? scm_to_size_t(scm_length($input)) : 0;
    for (std::size_t i = 0; i < len; ++i)
    {
        SCM s_account = scm_list_ref($input, scm_from_size_t(i));
        Account* acct = (Account*)SWIG_MustGetPtr(s_account,
                                                  SWIGTYPE_p_Account, 1, 0);
        if (acct)
            alist.push_back(*qof_entity_get_guid(acct));
    }
    $1 = &alist;
}

%typemap(in) GncOptionAccountList& (GncOptionAccountList acclist)
{
    auto len = scm_is_true($input) ? scm_to_size_t(scm_length($input)) : 0;
    acclist.reserve(len);
    for (std::size_t i = 0; i < len; ++i)
    {
        SCM s_account = scm_list_ref($input, scm_from_size_t(i));
        Account* acct = (Account*)SWIG_MustGetPtr(s_account,
                                                  SWIGTYPE_p_Account, 1, 0);
        acclist.push_back(*qof_entity_get_guid(acct));
    }
    $1 = &acclist;
}

%typemap (in) GncOptionReportPlacementVec& (GncOptionReportPlacementVec rp)
{
    rp = scm_to_value<GncOptionReportPlacementVec>($input);
    $1 = &rp;
}

%typemap(out) GncOptionAccountList
{
    $result = SCM_EOL;
    auto book{get_current_book()};
    for (auto guid : $1)
    {
        auto acct{xaccAccountLookup(&guid, book)};
        $result = scm_cons(SWIG_NewPointerObj(acct, SWIGTYPE_p_Account, 0),
                           $result);
    }
    $result = scm_reverse($result);
}

%typemap(out) const GncOptionAccountList&
{
    $result = SCM_EOL;
    auto book{get_current_book()};
    for (auto guid : *$1)
    {
        auto acct{xaccAccountLookup(&guid, book)};
        $result = scm_cons(SWIG_NewPointerObj(acct, SWIGTYPE_p_Account, 0),
                           $result);
    }
    $result = scm_reverse ($result)
}

%typemap(out) const GncOptionReportPlacementVec&
{
    $result = scm_from_value<GncOptionReportPlacementVec>($1);
}

wrap_unique_ptr(GncOptionDBPtr, GncOptionDB);

%ignore swig_get_option(GncOption&);
%ignore GncOwnerDeleter;
%ignore
    GncOptionGncOwnerValue::GncOptionGncOwnerValue(GncOptionGncOwnerValue &&);

%inline %{
#include <cassert>
#include <algorithm>
#include <array>
#include <string>
#include "gnc-option.hpp"
#include "gnc-option-impl.hpp"
#include "gnc-option-ui.hpp"

    GncOptionVariant& swig_get_option(GncOption* option)
    {
        assert(option);
        return *option->m_option;
    }
%}

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
%ignore gnc_register_commodity_option(GncOptionDB*, const char*, const char*, const char*, const char*, const char*);
%ignore gnc_register_simple_boolean_option(GncOptionDB*, const char* section, const char* name, const char* key, const char* doc_string, bool value);
%ignore gnc_register_complex_boolean_option(GncOptionDB*, const char* section, const char* name, const char* key, const char* doc_string, bool value);
%ignore gnc_register_pixmap_option(GncOptionDB*, const char*, const char*, const char*, const char*, std::string);
%ignore gnc_register_account_list_limited_option(GncOptionDB*, const char*, const char*, const char*, const char*, const GncOptionAccountList&, GncOptionAccountTypeList&&);
%ignore gnc_register_account_list_option(GncOptionDB*, const char*, const char*, const char*, const char*, const GncOptionAccountList&);
%ignore gnc_register_account_sel_limited_option(GncOptionDB*, const char*, const char*, const char*, const char*, const Account*, GncOptionAccountTypeList&&);
%ignore gnc_register_multichoice_option(GncOptionDB*, const char*, const char*, const char*, const char*, const char*, GncMultichoiceOptionChoices&&);
%ignore gnc_register_list_option(GncOptionDB*, const char*, const char*, const char*, const char*, const char*, GncMultichoiceOptionChoices&&);
%ignore gnc_register_number_Plot_size_option(GncOptionDB*, const char*, const char*, const char*, const char*, int);
%ignore gnc_register_query_option(GncOptionDB*, const char*, const char*, const QofQuery*);
%ignore gnc_register_color_option(GncOptionDB*, const char*, const char*, const char*, const char*, std::string);
%ignore gnc_register_currency_option(GncOptionDB*, const char*, const char*, const char*, const char*, gnc_commodity*);
%ignore gnc_register_currency_option(GncOptionDB*, const char*, const char*, const char*, const char*, const char*);
%ignore gnc_register_invoice_option(GncOptionDB*, const char*, const char*, const char*, const char*, GncInvoice*);
%ignore gnc_register_taxtable_option(GncOptionDB*, const char*, const char*, const char*, const char*, GncTaxTable*);
%ignore gnc_register_counter_option(GncOptionDB*, const char*, const char*, const char*, const char*, double);
%ignore gnc_register_counter_format_option(GncOptionDB*, const char*, const char*, const char*, const char*, std::string);
%ignore gnc_register_dateformat_option(GncOptionDB*, const char*, const char*, const char*, const char*, GncOptionDateFormat&&);
%ignore gnc_register_dateformat_option(GncOptionDBPtr&, const char*, const char*, const char*, const char*, GncOptionDateFormat&&);
%ignore gnc_register_date_option(GncOptionDB*, const char*, const char*, const char*, const char*, RelativeDatePeriod, RelativeDateUI);
%ignore gnc_register_date_option(GncOptionDB*, const char*, const char*, const char*, const char*, time64, RelativeDateUI);
%ignore gnc_register_date_option(GncOptionDB*, const char*, const char*, const char*, const char*, RelativeDatePeriodVec, bool);
%ignore gnc_register_start_date_option(GncOptionDB*, const char*, const char*, const char*, const char*, bool);
%ignore gnc_register_end_date_option(GncOptionDB*, const char*, const char*, const char*, const char*, bool);
%typemap(in) GncOption* "$1 = scm_is_true($input) ? static_cast<GncOption*>(scm_to_pointer($input)) : nullptr;"
%typemap(out) GncOption* "$result = ($1) ? scm_from_pointer($1, nullptr) : SCM_BOOL_F;"

%header %{

    static std::vector<SCM> reldate_values{};
    inline uint16_t index_of(RelativeDatePeriod per)
    {
        return static_cast<uint16_t>(per) + 1;
    }
    
    static void init_reldate_values()
    {
        if (!reldate_values.empty())
            return;
        std::vector<SCM> tmp (relative_date_periods, SCM_BOOL_F);
        using rdp = RelativeDatePeriod;
        tmp[index_of(rdp::ABSOLUTE)] =
            scm_from_utf8_symbol("absolute");
        tmp[index_of(rdp::TODAY)] =
            scm_from_utf8_symbol("today");
        tmp[index_of(rdp::ONE_WEEK_AGO)] =
            scm_from_utf8_symbol("one-week-ago");
        tmp[index_of(rdp::ONE_WEEK_AHEAD)] =
            scm_from_utf8_symbol("one-week-ahead");
        tmp[index_of(rdp::ONE_MONTH_AGO)] =
            scm_from_utf8_symbol("one-month-ago");
        tmp[index_of(rdp::ONE_MONTH_AHEAD)] =
            scm_from_utf8_symbol("one-month-ahead");
        tmp[index_of(rdp::THREE_MONTHS_AGO)] =
            scm_from_utf8_symbol("three-months-ago");
        tmp[index_of(rdp::THREE_MONTHS_AHEAD)] =
            scm_from_utf8_symbol("three-months-ahead");
        tmp[index_of(rdp::SIX_MONTHS_AGO)] =
            scm_from_utf8_symbol("six-months-ago");
        tmp[index_of(rdp::SIX_MONTHS_AHEAD)] =
            scm_from_utf8_symbol("six-months-ahead");
        tmp[index_of(rdp::ONE_YEAR_AGO)] =
            scm_from_utf8_symbol("one-year-ago");
        tmp[index_of(rdp::ONE_YEAR_AHEAD)] =
            scm_from_utf8_symbol("one-year-ahead");
        tmp[index_of(rdp::START_THIS_MONTH)] =
            scm_from_utf8_symbol("start-this-month");
        tmp[index_of(rdp::END_THIS_MONTH)] =
            scm_from_utf8_symbol("end-this-month");
        tmp[index_of(rdp::START_PREV_MONTH)] =
            scm_from_utf8_symbol("start-prev-month");
        tmp[index_of(rdp::END_PREV_MONTH)] =
            scm_from_utf8_symbol("end-prev-month");
        tmp[index_of(rdp::START_NEXT_MONTH)] =
            scm_from_utf8_symbol("start-next-month");
        tmp[index_of(rdp::END_NEXT_MONTH)] =
            scm_from_utf8_symbol("end-next-month");
        tmp[index_of(rdp::START_CURRENT_QUARTER)] =
            scm_from_utf8_symbol("start-current-quarter");
        tmp[index_of(rdp::END_CURRENT_QUARTER)] =
            scm_from_utf8_symbol("end-current-quarter");
        tmp[index_of(rdp::START_PREV_QUARTER)] =
            scm_from_utf8_symbol("start-prev-quarter");
        tmp[index_of(rdp::END_PREV_QUARTER)] =
            scm_from_utf8_symbol("end-prev-quarter");
        tmp[index_of(rdp::START_NEXT_QUARTER)] =
            scm_from_utf8_symbol("start-next-quarter");
        tmp[index_of(rdp::END_NEXT_QUARTER)] =
            scm_from_utf8_symbol("end-next-quarter");
        tmp[index_of(rdp::START_CAL_YEAR)] =
            scm_from_utf8_symbol("start-cal-year");
        tmp[index_of(rdp::END_CAL_YEAR)] =
            scm_from_utf8_symbol("end-cal-year");
        tmp[index_of(rdp::START_PREV_YEAR)] =
            scm_from_utf8_symbol("start-prev-year");
        tmp[index_of(rdp::END_PREV_YEAR)] =
            scm_from_utf8_symbol("end-prev-year");
        tmp[index_of(rdp::START_NEXT_YEAR)] =
            scm_from_utf8_symbol("start-next-year");
        tmp[index_of(rdp::END_NEXT_YEAR)] =
            scm_from_utf8_symbol("end-next-year");
        tmp[index_of(rdp::START_ACCOUNTING_PERIOD)] =
            scm_from_utf8_symbol("start-accounting-period");
        tmp[index_of(rdp::END_ACCOUNTING_PERIOD)] =
            scm_from_utf8_symbol("end-accounting-period");
        reldate_values = std::move(tmp);
    }

    inline static RelativeDatePeriod scm_relative_date_get_period(SCM date)
    {
        init_reldate_values();
        auto reldate_scm{scm_is_pair(date) ? scm_cdr(date) : date};
        SCM reldate_val{SCM_BOOL_F};
        if (scm_is_procedure(reldate_scm))
            reldate_val = scm_call_0(reldate_scm);
        if (scm_is_number(reldate_scm))
            reldate_val = reldate_scm;
        if (scm_is_number(reldate_val))
        {
            auto reldate_index = scm_to_int(reldate_val);
            assert(reldate_index >= static_cast<int>(RelativeDatePeriod::ABSOLUTE) && reldate_index < static_cast<int>(relative_date_periods - 1));
            return static_cast<RelativeDatePeriod>(reldate_index);
        }

        auto date_iter =
            std::find_if(reldate_values.begin(), reldate_values.end(),
                         [&reldate_scm](auto val)->bool {
                             return scm_is_eq(val, reldate_scm) == 1;
                         });
        if (date_iter == reldate_values.end())
            return RelativeDatePeriod::ABSOLUTE;
        return static_cast<RelativeDatePeriod>(date_iter - reldate_values.begin() - 1);
    }

    inline static SCM scm_relative_date_from_period(RelativeDatePeriod period)
    {
        init_reldate_values();
        return reldate_values[static_cast<uint16_t>(period) + 1];
    }

    inline static bool scm_date_absolute(SCM date)
    {
        if (scm_is_pair(date))
        {
            if (scm_is_symbol(scm_car(date)))
            {
                auto car{scm_to_utf8_string(scm_symbol_to_string(scm_car(date)))};
                bool rv = false;
                if (strcmp(car, "relative") == 0)
                    rv = false;
                else if (strcmp(car, "absolute") == 0)
                    rv = true;
                else
                    assert(false);
                free (car);
                return rv;
            }
            else
            {
                auto cdr{scm_cdr(date)};
                if (scm_is_symbol(cdr))
                    return false;
                if (scm_is_number(cdr))
                    return true;

                assert(false);
            }
        }
        return (!(scm_is_symbol(date) || scm_is_string(date)));
    }

    inline static time64 scm_absolute_date_to_time64(SCM date)
    {
        if (scm_date_absolute(date))
            return scm_to_int64(scm_is_pair(date) ? scm_cdr(date) : date);

        return gnc_relative_date_to_time64(scm_relative_date_get_period(date));
    }

    void gnc_register_complex_boolean_option(GncOptionDBPtr&, const char*,
                                             const char*, const char*,
                                             const char*, bool, SCM);

    void gnc_register_multichoice_callback_option(GncOptionDBPtr&, const char*,
                                                  const char*, const char*,
                                                  const char*, const char*,
                                                  GncMultichoiceOptionChoices&&,
                                                  SCM);


%} //%header

%ignore GncOptionMultichoiceKeyType;

%inline %{

    inline SCM
    is_gncoptiondb (const SCM ptr)
    {
        return SWIG_Guile_IsPointerOfType (ptr, SWIGTYPE_p_std__unique_ptrT_GncOptionDB_t) ? SCM_BOOL_T : SCM_BOOL_F;
    }

    inline GncMultichoiceOptionIndexVec
    scm_to_multichoices(const SCM new_value,
                        const GncOptionMultichoiceValue& option)
    {
        static const auto uint16_t_max = std::numeric_limits<uint16_t>::max();
        auto scm_to_str = [](auto item)->char* {
                static const char* empty{""};
                if (scm_is_integer(item))
                    item = scm_number_to_string(item, scm_from_uint(10u));
                else if (scm_is_symbol(item))
                    item = scm_symbol_to_string(item);
                if (scm_is_string(item))
                    return scm_to_utf8_string(item);
                return strdup(empty);
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
                auto item_str{scm_to_str(item)};
                auto index{option.permissible_value_index(item_str)};
                free (item_str);
                if (index < uint16_t_max)
                    vec.push_back(index);
            }
        }
        else
        {
            auto newval_str{scm_to_str(new_value)};
            auto index{option.permissible_value_index(newval_str)};
            free (newval_str);
            if (index < uint16_t_max)
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

    static SCM
    get_scm_value(const GncOptionMultichoiceValue& option)
    {

        auto indexes = option.get_multiple();
        if (indexes.empty())
            indexes = option.get_default_multiple();
        if (indexes.empty())
            return SCM_BOOL_F;
        return scm_from_multichoices(indexes, option);
     }

    static SCM
    get_scm_default_value(const GncOptionMultichoiceValue& option)
    {

        auto indexes = option.get_default_multiple();
        if (indexes.empty())
            return SCM_BOOL_F;
        return scm_from_multichoices(indexes, option);
     }

    static SCM
    get_scm_value(const GncOptionRangeValue<int>& option)
    {
        auto val{option.get_value()};
        auto desig{scm_c_eval_string(val > 100 ? "'pixels" : "'percent")};
        return scm_cons(desig, scm_from_int(val));
    }

    static SCM
    get_scm_default_value(const GncOptionRangeValue<int>& option)
    {
        auto val{option.get_default_value()};
        auto desig{scm_c_eval_string(val > 100 ? "'pixels" : "'percent")};
        return scm_cons(desig, scm_from_int(val));
    }

    static SCM
    get_scm_value(const GncOptionRangeValue<double>& option)
    {
        return scm_from_double(option.get_value());
    }

    static SCM
    get_scm_default_value(const GncOptionRangeValue<double>& option)
    {
        return scm_from_double(option.get_default_value());
    }

    static SCM
    get_scm_value(const GncOptionDateValue& option)
    {
        if (option.get_period() == RelativeDatePeriod::ABSOLUTE)
            return scm_cons(scm_from_utf8_symbol("absolute"),
                            scm_from_value(option.get_value()));
        else
            return scm_cons(scm_from_utf8_symbol("relative"),
                            scm_relative_date_from_period(option.get_period()));
    }

    static SCM
    get_scm_default_value(const GncOptionDateValue& option)
    {
        if (option.get_default_period() == RelativeDatePeriod::ABSOLUTE)
            return scm_cons(scm_from_utf8_symbol("absolute"),
                            scm_from_value(option.get_default_value()));
        else
            return scm_cons(scm_from_utf8_symbol("relative"),
                            scm_relative_date_from_period(option.get_default_period()));
    }

template <typename T>
struct is_MultichoiceDateOrRange
{
    static constexpr bool value =
        is_same_decayed_v<T, GncOptionMultichoiceValue> ||
        is_same_decayed_v<T, GncOptionRangeValue<int>> ||
        is_same_decayed_v<T, GncOptionDateValue>;
};

template <typename T>
inline constexpr bool is_MultichoiceDateOrRange_v = is_MultichoiceDateOrRange<T>::value;

template <typename ValueType>
inline SCM return_scm_value(ValueType value)
{
    if constexpr (is_same_decayed_v<ValueType, SCM>)
        return value;
    return scm_from_value(static_cast<ValueType>(value));
}

%}
%ignore GncOptionDBCallback;
%ignore operator<(const GncOption&, const GncOption&);
%ignore operator<(const GncOptionSectionPtr&, const GncOptionSectionPtr&);

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
%template(gnc_make_query_option) gnc_make_option<const QofQuery*>;

%rename (get_value) GncOption::get_scm_value;
%rename (get_default_value) GncOption::get_scm_default_value;
%rename (set_value) GncOption::set_value_from_scm;
%rename (set_default_value) GncOption::set_default_value_from_scm;
%extend GncOption {
    bool is_budget_option()
    {
        auto uitype{$self->get_ui_type()};
        return uitype == GncOptionUIType::BUDGET;
    }

    SCM get_scm_value()
    {
        if (!$self)
            return SCM_BOOL_F;
        return std::visit([](const auto& option)->SCM {
                if constexpr (is_MultichoiceDateOrRange_v<decltype(option)>)
                    return get_scm_value(option);
                auto value{option.get_value()};
                return return_scm_value(value);
            }, swig_get_option($self));
    }
    SCM get_scm_default_value()
    {
        if (!$self)
            return SCM_BOOL_F;
        return std::visit([](const auto& option)->SCM {
                if constexpr (is_MultichoiceDateOrRange_v<decltype(option)>)
                    return get_scm_default_value(option);
                auto value{option.get_default_value()};
                return return_scm_value(value);
            }, swig_get_option($self));
    }

    SCM save_scm_value()
    {
        [[maybe_unused]] static const SCM plain_format_str{scm_from_utf8_string("~s")};
        [[maybe_unused]] static const SCM ticked_format_str{scm_from_utf8_string("'~a")};
        [[maybe_unused]] static const SCM list_format_str{scm_from_utf8_string("'~s")};
//scm_simple_format needs a scheme list of arguments to match the format
//placeholders.
        return std::visit([$self] (auto &option) -> SCM {
                static const auto no_value{scm_from_utf8_string("")};
                if constexpr (is_same_decayed_v<decltype(option),
                              GncOptionAccountListValue>)
                {
                    auto guid_list{option.get_value()};
                    if (guid_list.empty())
                        return scm_simple_format(SCM_BOOL_F, list_format_str, scm_list_1(no_value));
                    SCM string_list{SCM_EOL};
                    char guid_str[GUID_ENCODING_LENGTH+1];
                    for(auto guid : guid_list)
                    {
                        guid_to_string_buff (&guid, guid_str);
                        auto guid_scm{scm_from_utf8_string(guid_str)};
                        string_list = scm_cons(guid_scm, string_list);
                    }
                    return scm_simple_format(SCM_BOOL_F, list_format_str, scm_list_1(string_list));

                }
                if constexpr (is_QofInstanceValue_v<decltype(option)>)
                {
                    auto serial{option.serialize()};
                    auto value{scm_list_1(scm_from_utf8_string(serial.empty() ? "" : serial.c_str()))};
                    return scm_simple_format(SCM_BOOL_F, plain_format_str, value);
                }
                if constexpr (is_same_decayed_v<decltype(option),
                              GncOptionCommodityValue>)
                {
                     auto comm{option.get_value()};
                     auto mnemonic{gnc_commodity_get_mnemonic(comm)};
                     if (gnc_commodity_is_currency(comm))
                     {
                         auto value{scm_list_1(scm_from_utf8_string(mnemonic))};
                         const SCM quoted_format_str{scm_from_utf8_string("~s")};
                         return scm_simple_format(SCM_BOOL_F, quoted_format_str, value);
                     }
                     else
                     {
                          const SCM commodity_fmt{scm_from_utf8_string("'(commodity-scm ~s ~s)")};
                          auto name_space{gnc_commodity_get_namespace(comm)};
                          auto commodity_val{scm_list_2(scm_from_utf8_string(name_space),
                                                        scm_from_utf8_string(mnemonic))};
                          return scm_simple_format(SCM_BOOL_F, commodity_fmt, commodity_val);
                     }
                }
                if constexpr (is_same_decayed_v<decltype(option),
                              GncOptionDateValue>)
                {
                    auto serial{option.serialize()};
                    auto value{scm_list_1(scm_from_utf8_string(serial.empty() ? "" :serial.c_str()))};
                    const SCM date_fmt{scm_from_utf8_string("'~a")};
                    return scm_simple_format(SCM_BOOL_F, date_fmt, value);
                }

                if constexpr (is_GncOwnerValue_v<decltype(option)>)
                {
                    auto value{option.get_value()};
                    auto guid{scm_from_utf8_string(qof_instance_to_string(qofOwnerGetOwner(value)).c_str())};
                    auto type{scm_from_long(gncOwnerGetType(value))};
                    return scm_simple_format(SCM_BOOL_F, list_format_str,
                                             scm_list_1(scm_cons(type, guid)));
                }
                if constexpr (is_QofQueryValue_v<decltype(option)>)
                {
                    QofQuery* value{const_cast<QofQuery*>(option.get_value())};
                    return scm_simple_format(SCM_BOOL_F, ticked_format_str,
                                             scm_list_1(gnc_query2scm(value)));
                }
                if constexpr (is_same_decayed_v<decltype(option),
                              GncOptionMultichoiceValue>)
                {
                  auto scm_val{get_scm_value(option)};
                  return scm_simple_format(SCM_BOOL_F, list_format_str,
                                       scm_list_1(scm_val));
                }
                if constexpr (is_same_decayed_v<decltype(option),
                              GncOptionRangeValue<int>>  ||
                              is_same_decayed_v<decltype(option),
                              GncOptionRangeValue<double>>)
                {
                    auto serial{get_scm_value(option)};
                    auto scm_str{serial == SCM_BOOL_F ? scm_list_1(no_value) : scm_list_1(serial)};
                    return scm_simple_format(SCM_BOOL_F, ticked_format_str, scm_str);
                }
                if constexpr (is_same_decayed_v<decltype(option),
                              GncOptionValue<bool>>)
                {
                    auto scm_val{scm_list_1(return_scm_value(option.get_value()))};
                    return scm_simple_format(SCM_BOOL_F, plain_format_str,
                                             scm_val);
                }
               if constexpr (is_same_decayed_v<decltype(option),
                              GncOptionValue<GncOptionReportPlacementVec>>)
                {
                    auto scm_val{scm_list_1(return_scm_value(option.get_value()))};
                    return scm_simple_format(SCM_BOOL_F, ticked_format_str,
                                             scm_val);
                }
                auto serial{option.serialize()};
                if (serial.empty())
                {
                    return scm_simple_format(SCM_BOOL_F, plain_format_str, scm_list_1(no_value));
                }
                else if ($self->get_ui_type() == GncOptionUIType::COLOR)
                {
                    auto red{static_cast<double>(std::stoi(serial.substr(0, 2),
                                                           nullptr, 16))};
                    auto blue{static_cast<double>(std::stoi(serial.substr(2, 2),
                                                            nullptr, 16))};
                    auto green{static_cast<double>(std::stoi(serial.substr(4, 2),
                                                             nullptr, 16))};
                    auto alpha{serial.length() > 7 ?
                            static_cast<double>(std::stoi(serial.substr(6, 2),
                                                          nullptr, 16)) :
                            255.0};
                    std::ostringstream outs;
                    outs << "(" << std::fixed << std::setprecision(1) << red <<
                        " " << blue << " " << green << " " << alpha << ")";
                    auto scm_out{scm_list_1(scm_from_utf8_string(outs.str().c_str()))};
                    return scm_simple_format(SCM_BOOL_F, ticked_format_str,
                                             scm_out);
                }
                else
                {
                    auto scm_str{scm_list_1(scm_from_utf8_string(serial.c_str()))};
                    return scm_simple_format(SCM_BOOL_F, plain_format_str, scm_str);
                }
            }, swig_get_option($self));
    }

    void set_value_from_scm(SCM new_value)
    {
        if (!$self)
            return;
        try {
            std::visit([new_value](auto& option) {
                    if constexpr (is_same_decayed_v<decltype(option),
                                  GncOptionDateValue>)
                    {
                        if (scm_date_absolute(new_value))
                            option.set_value(scm_absolute_date_to_time64(new_value));
                        else
                            option.set_value(scm_relative_date_get_period(new_value));
                        return;
                    }

                    if constexpr (is_same_decayed_v<decltype(option),
                                  GncOptionMultichoiceValue>)
                    {
                        option.set_multiple(scm_to_multichoices(new_value, option));
                        return;
                    }

                    if constexpr (is_same_decayed_v<decltype(option),
                                  GncOptionRangeValue<int>>)
                    {
                        auto value_num{scm_is_pair(new_value) ? scm_cdr(new_value) : new_value};
                        int value_int = scm_is_exact_integer(value_num) ?
                            scm_to_int(value_num) :
                            static_cast<int>(scm_to_double(value_num));
                        option.set_value(value_int);
                        return;
                    }
                    if constexpr (is_same_decayed_v<decltype(option),
                                  GncOptionCommodityValue>)
                    {
                        if (scm_list_p(new_value) == SCM_BOOL_F)
                        {
                            if (scm_is_string(new_value))
                            {
                                 auto strval{scm_to_utf8_string(new_value)};
                                 option.deserialize(strval);
                                 free (strval);
                                 return;
                            }
                            option.set_value(scm_to_value<gnc_commodity*>(new_value));
                            return;
                        }
                        auto len{scm_to_uint(scm_length(new_value))};
                        if (len > 1)
                        {
                            auto revlist{scm_reverse(new_value)};
                            auto name_space{scm_to_utf8_string(scm_cadr(revlist))};
                            auto mnemonic{scm_to_utf8_string(scm_car(revlist))};
                            option.deserialize(std::string (name_space) + ":" +
                                               std::string (mnemonic));
                            free (mnemonic);
                            free (name_space);
                        }
                        else
                        {
                            auto newval_str{scm_to_utf8_string(scm_car(new_value))};
                            option.deserialize(newval_str);
                            free (newval_str);
                        }
                        return;
                    }
                    if constexpr (is_QofInstanceValue_v<decltype(option)>)
                    {
                        if (scm_is_string(new_value))
                        {
                            auto strval{scm_to_utf8_string(new_value)};
                            auto val{qof_instance_from_string(strval, option.get_ui_type())};
                            option.set_value(val);
                            free (strval);
                        }
                        else
                        {
                            auto val{scm_to_value<const QofInstance*>(new_value)};
                            option.set_value(val);
                        }
                        return;
                    }
                    if constexpr (is_GncOwnerValue_v<decltype(option)>)
                    {
                        if (scm_is_pair(new_value))
                        {
                            GncOwner owner{};
                            owner.type = static_cast<GncOwnerType>(scm_to_int(scm_car(new_value)));
                            auto strval{scm_to_utf8_string(scm_cdr(new_value))};
                            owner.owner.undefined = qof_instance_from_string(strval, option.get_ui_type());
                            option.set_value(&owner);
                            free (strval);
                        }
                        else
                        {
                            auto val{scm_to_value<const GncOwner*>(new_value)};
                            option.set_value(val);
                        }
                        return;
                    }

                    if constexpr (is_QofQueryValue_v<decltype(option)>)
                    {
                        if (scm_is_pair(new_value))
                        {
                            auto val{gnc_scm2query(new_value)};
                            option.set_value(val);
                        }
                        else
                        {
                            auto val{scm_to_value<const QofQuery*>(new_value)};
                            option.set_value(val);
                        }
                        return;
                    }
                    if constexpr (is_same_decayed_v<decltype(option),
                                  GncOptionAccountSelValue>)
                    {
                        if (scm_is_string(new_value))
                        {
                            auto strval{scm_to_utf8_string(new_value)};
                            GncGUID guid{};
                            string_to_guid(strval, &guid);
                            auto book{get_current_book()};
                            option.set_value(xaccAccountLookup(&guid, book));
                            free (strval);
                        }
                        else
                        {
                            auto val{scm_to_value<const QofInstance*>(new_value)};
                            option.set_value(GNC_ACCOUNT(val));
                        }
                        return;
                    }
                    auto value{scm_to_value<std::decay_t<decltype(option.get_value())>>(new_value)};  //Can't inline, set_value takes arg by reference.
                    option.set_value(static_cast<decltype(option.get_value())>(value));
                }, swig_get_option($self));
        }
        catch(const std::invalid_argument& err)
        {
            PERR("Option %s:%s failed to set value: %s",
                 $self->get_section().c_str(), $self->get_name().c_str(),
                 err.what());
        }
    }

    void set_default_value_from_scm(SCM new_value)
    {
        if (!$self)
            return;
        try {
            std::visit([new_value](auto& option) {
                    if constexpr (is_same_decayed_v<decltype(option),
                                  GncOptionDateValue>)
                    {
                        if (scm_date_absolute(new_value))
                            option.set_default_value(scm_absolute_date_to_time64(new_value));
                        else
                            option.set_default_value(scm_relative_date_get_period(new_value));
                       return;
                    }
                    if constexpr (is_same_decayed_v<decltype(option),
                                  GncOptionMultichoiceValue>)
                    {
                        option.set_default_multiple(scm_to_multichoices(new_value,
                                                                        option));
                        return;
                    }
                    if constexpr (is_same_decayed_v<decltype(option),
                                  GncOptionRangeValue<int>>)
                    {
                        if (scm_is_pair(new_value))
                            option.set_default_value(scm_to_int(scm_cdr(new_value)));
                        else
                            option.set_default_value(scm_to_int(new_value));
                        return;
                    }
                    if constexpr (is_same_decayed_v<decltype(option),
                                  GncOptionCommodityValue>)
                    {
                       auto comm{scm_to_value<gnc_commodity*>(new_value)};
                       option.set_default_value(comm);
                    }
                    if constexpr (is_QofInstanceValue_v<decltype(option)>)
                    {
                        if (scm_is_string(new_value))
                        {
                            auto strval{scm_to_utf8_string(new_value)};
                            auto val{qof_instance_from_string(strval, option.get_ui_type())};
                            option.set_default_value(val);
                            free (strval);
                        }
                        else
                        {
                            auto val{scm_to_value<const QofInstance*>(new_value)};
                            option.set_default_value(val);
                        }
                        return;
                    }
                    if constexpr (is_GncOwnerValue_v<decltype(option)>)
                    {
                        if (scm_is_pair(new_value))
                        {
                            GncOwner owner{};
                            owner.type = static_cast<GncOwnerType>(scm_to_int(scm_car(new_value)));
                            auto strval{scm_to_utf8_string(scm_cdr(new_value))};
                            owner.owner.undefined = qof_instance_from_string(strval, option.get_ui_type());
                            option.set_default_value(&owner);
                            free (strval);
                        }
                        else
                        {
                            auto val{scm_to_value<const GncOwner*>(new_value)};
                            option.set_default_value(val);
                        }
                        return;
                    }
                    if constexpr (is_QofQueryValue_v<decltype(option)>)
                    {
                        if (scm_is_pair(new_value))
                        {
                            auto val{gnc_scm2query(new_value)};
                            option.set_default_value(val);
                        }
                        else
                        {
                            auto val{scm_to_value<const QofQuery*>(new_value)};
                            option.set_default_value(val);
                        }
                        return;
                    }
                    if constexpr (is_same_decayed_v<decltype(option),
                                  GncOptionAccountSelValue>)
                    {
                        if (scm_is_string(new_value))
                        {
                            auto strval{scm_to_utf8_string(new_value)};
                            GncGUID guid{};
                            string_to_guid(strval, &guid);
                            auto book{get_current_book()};
                            option.set_default_value(xaccAccountLookup(&guid, book));
                            free (strval);
                        }
                        else
                        {
                            auto val{scm_to_value<Account*>(new_value)};
                            option.set_default_value(val);
                        }
                        return;
                    }
                    auto value{scm_to_value<std::decay_t<decltype(option.get_value())>>(new_value)};  //Can't inline, set_value takes arg by reference.
                    option.set_default_value(value);
                }, swig_get_option($self));
        }
        catch(const std::invalid_argument& err)
        {
            PERR("Option %s:%s failed to set default value: %s",
                 $self->get_section().c_str(), $self->get_name().c_str(), err.what());
        }
    }

    SCM get_type()
    {
        if (!self)
            return SCM_BOOL_F;
        return std::visit([](const auto& option)->SCM {
                if constexpr (is_same_decayed_v<decltype(option),
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

%ignore SCMDeleter;
%ignore SCMCalbackWrapper;

%inline %{

    struct SCMDeleter
    {
        void operator()(SCM cb) {
            scm_gc_unprotect_object(cb);
        }
    };

    class SCMCallbackWrapper
    {
        std::unique_ptr<scm_unused_struct, SCMDeleter> m_callback;
    public:
    SCMCallbackWrapper(SCM cb) : m_callback{scm_gc_protect_object(cb)} {}
    SCMCallbackWrapper(const SCMCallbackWrapper& cbw) : m_callback{scm_gc_protect_object(cbw.get())} {}
        SCM get() const { return m_callback.get(); }
    };

/**
 * Create a new complex boolean option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option.
 * @param widget_changed_cb A Scheme callback to run from the UIItem's "changed" signal.
 */
void gnc_register_complex_boolean_option(GncOptionDBPtr& db,
                                         const char* section, const char* name,
                                         const char* key,
                                         const char* doc_string,
                                         bool value, SCM widget_changed_cb)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::BOOLEAN};
    option.set_widget_changed(std::make_any<SCMCallbackWrapper>(widget_changed_cb));
    db->register_option(section, std::move(option));
}

/**
 * Create a new multichoice option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The set of possible values for the option. Only one can be selected. Note that the value will be moved from the parameter and using the parameter after this call will result in undefined behavior.
 * @param widget_changed_cb A Scheme callback to run from the UIItem's "changed" signal.
 */
void
gnc_register_multichoice_callback_option(GncOptionDBPtr& db,
                                         const char* section,
                                         const char* name, const char* key,
                                         const char* doc_string,
                                         const char* default_val,
                                         GncMultichoiceOptionChoices&& choices,
                                         SCM widget_changed_cb)
{
    std::string defval{default_val};
    auto found{std::find_if(choices.begin(), choices.end(),
                            [&defval](auto& choice)->bool {
                                return defval == std::get<0>(choice);
                            })};
    if (found == choices.end())
        defval = (choices.empty() ? std::string{"None"} :
                  std::get<0>(choices.at(0)));
    GncOption option{GncOptionMultichoiceValue{section, name, key, doc_string,
                defval.c_str(), std::move(choices)}};
    option.set_widget_changed(std::make_any<SCMCallbackWrapper>(widget_changed_cb));
    db->register_option(section, std::move(option));
}

%}

%extend GncOptionDB {
    %template(set_option_string) set_option<std::string>;
    %template(set_option_int) set_option<int>;
    %template(set_option_time64) set_option<time64>;
};

%template(gnc_register_number_range_option) gnc_register_number_range_option<double>;

%inline %{
    /* qof_book_set_data isn't exported by sw-engine and we need it to set up a
     * commodity namespace table to test currencies.*/
    static void
    test_book_set_data(QofBook* book, const char* key, void* data)
    {
      qof_book_set_data(book, key, data);
    }

    static void
    test_book_clear_data(QofBook* book, const char* key)
    {
      qof_book_set_data(book, key, nullptr);
    }

    static void
    test_book_set_default_budget(QofBook* book, GncBudget* budget)
    {
        auto budget_guid{gnc_budget_get_guid(budget)};
        qof_book_begin_edit(book);
        qof_instance_set(QOF_INSTANCE(book), "default-budget",
                         budget_guid, nullptr);
        qof_book_commit_edit(book);
    }

    static GncOption*
    gnc_make_qofinstance_option(const char* section,
                                const char* name, const char* key,
                                const char* doc_string,
                                const QofInstance* value,
                                GncOptionUIType ui_type)
    {
        try {
            return new GncOption(GncOptionQofInstanceValue{section, name, key,
                                                           doc_string,
                                                           value, ui_type});
        }
        catch (const std::exception& err)
        {
            std::cerr << "Make QofInstance option threw unexpected exception"
            << err.what() << ", option not created." << std::endl;
            return nullptr;
        }
    }

    static GncOption*
    gnc_make_gncowner_option(const char* section,
                                const char* name, const char* key,
                                const char* doc_string,
                                const GncOwner* value,
                                GncOptionUIType ui_type)
    {
        try {
            return new GncOption(GncOptionGncOwnerValue{section, name, key,
                                                        doc_string,
                                                        value, ui_type});
        }
        catch (const std::exception& err)
        {
            std::cerr << "Make GncOwner option threw unexpected exception"
            << err.what() << ", option not created." << std::endl;
            return nullptr;
        }
    }

    static GncOption*
    gnc_make_account_list_option(const char* section,
                                 const char* name, const char* key,
                                 const char* doc_string,
                                 const GncOptionAccountList& value)
    {
        try {
            return new GncOption{GncOptionAccountListValue{section, name, key,
                        doc_string, GncOptionUIType::ACCOUNT_LIST, value}};
        }
        catch (const std::exception& err)
        {
            std::cerr << "Make account list option threw unexpected exception " << err.what() << ", option not created." << std::endl;
            return nullptr;
        }
    }

    static GncOption*
    gnc_make_account_list_limited_option(const char* section,
                                         const char* name,
                                         const char* key,
                                         const char* doc_string,
                                         const GncOptionAccountList& value,
                                         GncOptionAccountTypeList&& allowed)
    {
        try
        {
            return new GncOption{GncOptionAccountListValue{section, name, key,
                        doc_string, GncOptionUIType::ACCOUNT_LIST, value,
                        std::move(allowed)}};
        }
        catch (const std::invalid_argument& err)
        {
            std::cerr << "Account List Limited Option, value failed validation, option not created.\n";
            return nullptr;
        }
    }

    static GncOption*
    gnc_make_account_sel_limited_option(const char* section, const char* name,
                                        const char* key, const char* doc_string,
                                        const Account* value,
                                        GncOptionAccountTypeList&& allowed)
    {
        try
        {
            return new GncOption{GncOptionAccountSelValue{section, name, key,
                        doc_string, GncOptionUIType::ACCOUNT_SEL, value,
                        std::move(allowed)}};
        }
        catch (const std::invalid_argument& err)
        {
            std::cerr <<"Account Sel Limited Option, value failed validation, option not created.\n";
            return nullptr;
        }
    }

    static GncOption*
    gnc_make_date_option(const char* section, const char* name, const char* key,
                         const char* doc_string, const SCM default_val,
                         RelativeDatePeriodVec& period_set, bool both)
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
            auto retval{new GncOption{GncOptionDateValue(section, name, key,
                                                         doc_string, ui_type,
                                                         period_set)}};
            return retval;
        }
        catch (const std::invalid_argument& err)
        {
            std::cerr <<"Date Option, value failed validation, option not created.\n";
            return nullptr;
        }
    }

    static GncOption*
    gnc_make_multichoice_option(const char* section, const char* name,
                                const char* key, const char* doc_string,
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

    static GncOption*
    gnc_make_list_option(const char* section, const char* name, const char* key,
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

    static GncOption*
    gnc_make_range_value_option(const char* section, const char* name,
                                const char* key, const char* doc_string,
                                double value, double min, double max,
                                double step)
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

    static GncOption*
    gnc_make_plot_size_option(const char* section, const char* name,
                              const char* key, const char* doc_string,
                              int value, int min, int max, int step)
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

    static GncOption*
    gnc_make_commodity_option(const char* section, const char* name,
                              const char* key, const char* doc_string,
                              gnc_commodity *value)
    {
        return new GncOption{GncOptionCommodityValue{
                section, name, key, doc_string, value,
                    GncOptionUIType::COMMODITY}};
    }

    static GncOption*
    gnc_make_commodity_option(const char* section, const char* name,
                              const char* key, const char* doc_string,
                              const char *value)
    {
        gnc_commodity* commodity{};
        const auto book{qof_session_get_book(gnc_get_current_session())};
        const auto commodity_table{gnc_commodity_table_get_table(book)};
        const auto namespaces{gnc_commodity_table_get_namespaces(commodity_table)};
        for (auto node = namespaces; node && commodity == nullptr;
             node = g_list_next(node))
        {
            commodity = gnc_commodity_table_lookup(commodity_table,
                                                   (const char*)(node->data),
                                                   value);

            if (commodity)
                return gnc_make_commodity_option(section, name, key, doc_string,
                                                 commodity);
        }
        return nullptr;
    }

    static GncOption*
    gnc_make_currency_option(const char* section, const char* name,
                             const char* key, const char* doc_string,
                             gnc_commodity *value)
    {
        try
        {
            return new GncOption{GncOptionCommodityValue{
                    section, name, key, doc_string, value,
                    GncOptionUIType::CURRENCY}};
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
    GncOptionDBPtr gnc_new_optiondb();
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
    gnc_optiondb_lookup_value(const GncOptionDBPtr& optiondb,
                              const char* section,
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
    gnc_new_optiondb()
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

    static void
    gnc_optiondb_set_option_selectable_by_name(GncOptionDBPtr& odb,
                                                const char* section,
                                                const char* name,
                                                bool selectable)
    {
        auto option{odb->find_option(section, name)};
        option->set_ui_item_selectable(selectable);
    }

    static void
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

    /** Tailored for gnc:generate-restore-forms.
     * @param section_op A function to be called on each section name
     * @param option_op a function to be called on each option
     */
    static void
    gnc_optiondb_foreach2(GncOptionDBPtr& odb, SCM section_op,
                          SCM option_op)
    {
        odb->foreach_section(
            [&section_op, &option_op](const GncOptionSectionPtr& section)
            {
                auto scm_name{scm_from_utf8_string(section->get_name().c_str())};
                scm_call_1(section_op, scm_name);
                section->foreach_option(
                    [&option_op](auto& option)
                    {
                        auto optvoidptr{reinterpret_cast<void*>(
                                const_cast<GncOption*>(&option))};
                        auto scm_opt{scm_from_pointer(optvoidptr, nullptr)};
                        scm_call_1(option_op, scm_opt);
                    });
            });
    }
%}

#endif //SWIGGUILE
