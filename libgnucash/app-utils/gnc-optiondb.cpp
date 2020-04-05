/********************************************************************\
 * gnc-optiondb.cpp -- Collection of GncOption objects              *
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

#include <string>
#include <limits>
#include <sstream>
#include <kvp-value.hpp>
#include <qofbookslots.h>
#include "gnc-optiondb.h"
#include "gnc-optiondb.hpp"
#include "gnc-optiondb-impl.hpp"
#include "gnc-option-ui.hpp"

constexpr const char* log_module{G_LOG_DOMAIN};

constexpr auto stream_max = std::numeric_limits<std::streamsize>::max();

static bool
operator==(const std::string& str, const char* cstr)
{
    return strcmp(str.c_str(), cstr) == 0;
}

void
GncOptionSection::foreach_option(std::function<void(GncOption&)> func)
{
    std::for_each(m_options.begin(), m_options.end(), func);
}

void
GncOptionSection::foreach_option(std::function<void(const GncOption&)> func) const
{
    std::for_each(m_options.begin(), m_options.end(), func);
}

void
GncOptionSection::add_option(GncOption&& option)
{
    m_options.emplace_back(std::move(option));
}

void
GncOptionSection::remove_option(const char* name)
{
    m_options.erase(std::remove_if(m_options.begin(), m_options.end(),
                                   [name](const auto& option) -> bool
                                   {
                                       return option.get_name() == name;
                                   }));
}

const GncOption*
GncOptionSection::find_option(const char* name) const
{
    auto option = std::find_if(m_options.begin(), m_options.end(),
                               [name](auto& option) -> bool {
                                   return option.get_name() == name;
                               });
    return (option == m_options.end() ? nullptr : &*option);
}

GncOptionDB::GncOptionDB() : m_default_section{} {}

GncOptionDB::GncOptionDB(QofBook* book) : GncOptionDB() {}

void
GncOptionDB::register_option(const char* sectname, GncOption&& option)
{
    auto section = find_section(sectname);

    if (section)
    {
        section->add_option(std::move(option));
        return;
    }

    m_sections.emplace_back(std::make_shared<GncOptionSection>(sectname));
    m_sections.back()->add_option(std::move(option));
}

void
GncOptionDB::unregister_option(const char* sectname, const char* name)
{
    auto section = find_section(sectname);
    if (section)
        section->remove_option(name);
}

void
GncOptionDB::set_default_section(const char* sectname)
{
    m_default_section = find_section(sectname);
}

const GncOptionSection* const
GncOptionDB::get_default_section() const noexcept
{
        return m_default_section;
}

const GncOptionSection*
GncOptionDB::find_section(const std::string& section) const
{
    auto db_section = std::find_if(m_sections.begin(), m_sections.end(),
                                   [&section](auto& sect) -> bool
                                   {
                                       return section == sect->get_name();
                                   });
    return db_section == m_sections.end() ? nullptr : db_section->get();
}

const GncOption*
GncOptionDB::find_option(const std::string& section, const char* name) const
{
    auto db_section = const_cast<GncOptionDB*>(this)->find_section(section);
    if (!db_section)
        return nullptr;
    return db_section->find_option(name);
}

std::string
GncOptionDB::lookup_string_option(const char* section, const char* name)
{
    static const std::string empty_string{};

    auto db_opt = find_option(section, name);
    if (!db_opt)
        return empty_string;
    return db_opt->get_value<std::string>();
}

void
GncOptionDB::make_internal(const char* section, const char* name)
{

    auto db_opt = find_option(section, name);
    if (db_opt)
        db_opt->make_internal();
}

std::ostream&
GncOptionDB::save_option_scheme(std::ostream& oss,
                                const char* option_prolog,
                                const std::string& section,
                                const std::string& name) const noexcept
{
    auto db_opt = find_option(section, name.c_str());

    if (!db_opt || !db_opt->is_changed())
        return oss;
    oss << scheme_tags[0] << option_prolog << "\n";
    oss << scheme_tags[1] << '"' << section.substr(0, classifier_size_max) << "\"\n";
    oss << scheme_tags[1] << '"' << name.substr(0, classifier_size_max) << '"';
    oss  <<  scheme_tags[2] << "\n" << scheme_tags[3];
    db_opt->to_scheme(oss);
    oss << scheme_tags[4] << "\n\n";

    return oss;
}

static inline bool constexpr
is_eol(char c)
{
    return c == '\n';
}

static inline bool constexpr
is_whitespace(char c)
{
    return c == ' ' || c == '\n' || c == '\t';
}

static inline bool constexpr
is_begin_paren(char c)
{
    return c == '(';
}

static inline bool constexpr
is_end_paren(char c)
{
    return c == ')';
}

static inline bool constexpr
is_double_quote(char c)
{
    return c == '"';
}

static inline bool constexpr
is_single_quote(char c)
{
    return c == '\'';
}

static inline bool constexpr
is_semicolon(char c)
{
    return c == ';';
}

static inline bool constexpr
is_delim(char c)
{
    return is_begin_paren(c) || is_end_paren(c) || is_whitespace(c) ||
        is_single_quote(c) || is_double_quote(c) || is_semicolon(c);
}

static std::string
scan_scheme_symbol_from_streambuf(std::streambuf* sbuf)
{
    std::string retval;
    while(sbuf->in_avail() && !is_delim(sbuf->sgetc()))
        retval += sbuf->sbumpc();
    return retval;
}

#ifdef _LIBCPP_VERSION
static inline void constexpr
#else
static inline void
#endif
consume_scheme_comment(std::streambuf* sbuf)
{
    while (sbuf->in_avail() && !is_eol(sbuf->sgetc()))
           sbuf->sbumpc();
}

static inline std::string
scan_scheme_string_from_streambuf(std::streambuf* sbuf)
{
    std::string retval{static_cast<char>(sbuf->sbumpc())};
    while(sbuf->in_avail() && !is_double_quote(sbuf->sgetc()))
        retval += sbuf->sbumpc();
    retval += sbuf->sbumpc(); // Add the closing quote.
    return retval;
}

#ifdef _LIBCPP_VERSION
static inline void constexpr
#else
static inline void
#endif
consume_scheme_whitespace(std::streambuf* sbuf)
{
    while (sbuf->in_avail() && is_whitespace(sbuf->sgetc()))
           sbuf->sbumpc();
}

enum class IdentType
{
    NAME, //no introducing mark
    CONST, //introduced with single quote
    STRING, //delimited by double-quotes.
    LIST, //introduced ' and delimited by parentheses
    FORM //delimited by parentheses without ' introduction.
};

struct SchemeId
{
    IdentType m_type;
    std::string m_name;
    std::vector<SchemeId> m_ids;
};

/**
 * Scheme Parse Tree
 * An identifier is a string and a type (name, const, string, or form).
 */

static void scan_scheme_id_from_streambuf(std::streambuf* sbuf, SchemeId& id);

static void
scan_scheme_form_from_streambuf(std::streambuf* sbuf, SchemeId& id)
{
    sbuf->sbumpc();
    if (!sbuf->in_avail())
        return;
    char c = sbuf->sgetc();
    while (sbuf->in_avail() && !is_end_paren(c))
    {
        SchemeId next_id;
        scan_scheme_id_from_streambuf(sbuf, next_id);
        if (id.m_name.empty() && next_id.m_type == IdentType::NAME)
        {
            id.m_name = std::move(next_id.m_name);
            continue;
        }
        id.m_ids.emplace_back(std::move(next_id));
        if (!sbuf->in_avail())
        {
            std::string err{"End of streambuf before end of form "};
            err += id.m_name;
            throw std::runtime_error(err);
        }
        c = sbuf->sgetc();
    }
    sbuf->sbumpc();
}

static void
scan_scheme_list_from_streambuf(std::streambuf* sbuf, std::string& str)
{

    consume_scheme_whitespace(sbuf);
    if (!sbuf->in_avail())
        return;
    char c = sbuf->sgetc();
    while (sbuf->in_avail() && !is_end_paren(c))
    {
        str += static_cast<char>(sbuf->sbumpc());
        if (!sbuf->in_avail())
            return;
        c = sbuf->sgetc();
    }
    str += static_cast<char>(sbuf->sbumpc());
}

static void
scan_scheme_id_from_streambuf(std::streambuf* sbuf, SchemeId& id)
{
    consume_scheme_whitespace(sbuf);
    if (!sbuf->in_avail())
        return;
    auto c{sbuf->sgetc()};
    switch(c)
    {
        case ';':
            consume_scheme_comment(sbuf);
            break;
        case '"':
            id.m_type = IdentType::STRING;
            id.m_name = scan_scheme_string_from_streambuf(sbuf);
            break;
        case '\'':
        {
            std::string value{static_cast<char>(sbuf->sbumpc())};
            if (sbuf->sgetc() == '(')
            {
                id.m_type == IdentType::LIST;
                scan_scheme_list_from_streambuf(sbuf, value);
                if (value.back() != ')')
                    throw std::runtime_error("End of streambuf before end of form ");
            }
            else if (sbuf->sgetc() == '"')
                throw std::runtime_error("Malformed scheme particle starts '\"");
            else
            {
                id.m_type = IdentType::CONST;
                value += scan_scheme_symbol_from_streambuf(sbuf);
            }
            id.m_name = std::move(value);
            break;
        }
        case '(':
            id.m_type = IdentType::FORM;
            scan_scheme_form_from_streambuf(sbuf, id);
            break;
        default:
            id.m_type = IdentType::NAME;
            id.m_name = scan_scheme_symbol_from_streambuf(sbuf);
            break;
    }
    return;
}

static inline std::string
unquote_scheme_string(const std::string& str)
{
    if (str.front() == '"' && str.back() == '"')
       return str.substr(1, str.size() - 2);

    return str;
}

static std::optional<std::reference_wrapper<const SchemeId>>
find_form(const SchemeId& toplevel, IdentType type, const char* name)
{
    if (toplevel.m_type == type && toplevel.m_name == name)
        return std::ref(toplevel);
    for (const auto& id : toplevel.m_ids)
    {
        if (id.m_type == type && id.m_name == name)
            return std::ref(id);
        auto child{find_form(id, type, name)};
        if (child)
            return child;
    }
    return std::nullopt;
}

std::istream&
GncOptionDB::load_option_scheme(std::istream& iss)
{
    auto sbuf{iss.rdbuf()};
    SchemeId toplevel;
    std::optional<std::reference_wrapper<const SchemeId>> lookup_id;
    bool form_found = false;
    while (sbuf->in_avail()  && !lookup_id)
    {
        scan_scheme_id_from_streambuf(sbuf, toplevel);
        lookup_id = find_form(toplevel, IdentType::FORM, "gnc:lookup-option");
    }

    if (!lookup_id)
    {
        iss.setstate(std::ios_base::eofbit);
        return iss; // No options
    }
    const auto& classifier = lookup_id->get().m_ids;
    if (classifier.size() != 3)
        throw std::runtime_error("Malformed option classifier.");
    const auto& section = unquote_scheme_string(classifier[1].m_name);
    const auto& name = unquote_scheme_string(classifier[2].m_name);
    auto option = find_option(section, name.c_str());
    std::string option_str{section};
    option_str += ':';
    option_str += name;
    if (!option)
    {
        std::string err{"Option not found: "};
        err += option_str;
        throw std::runtime_error(err);
    }
    auto value_id = find_form(toplevel, IdentType::FORM, "gnc:option-set-value");
    if (!(value_id && value_id->get().m_ids.size() == 2))
    {
        std::string err{"Option "};
        err += option_str;
        throw std::runtime_error(err + " malformed value lambda form.");
    }
    std::istringstream value_iss{value_id->get().m_ids[1].m_name};
    option->from_scheme(value_iss);
    return iss;
}

std::ostream&
GncOptionDB::save_to_scheme(std::ostream& oss, const char* options_prolog) const noexcept
{
    foreach_section(
        [&oss, options_prolog](const GncOptionSectionPtr& section)
        {
            oss << "\n; Section: " << section->get_name() << "\n\n";
            section->foreach_option(
                [&oss, options_prolog, &section](auto& option)
                {
                    if (!option.is_changed())
                        return;
                    oss << scheme_tags[0] << options_prolog << "\n";
                    oss << scheme_tags[1] << '"' << section->get_name().substr(0, classifier_size_max) << "\"\n";
                    oss << scheme_tags[1] << '"' << option.get_name().substr(0, classifier_size_max) << '"';
                    oss  <<  scheme_tags[2] << "\n" << scheme_tags[3];
                    option.to_scheme(oss);
                    oss << scheme_tags[4] << "\n\n";
                });
        });
    return oss;
}

std::istream&
GncOptionDB::load_from_scheme(std::istream& iss) noexcept
{
    try {
        while (iss.good())
            load_option_scheme(iss);
        iss.clear(); //unset eofbit and maybe failbit
    }
    catch (const std::runtime_error& err)
    {
        std::cerr << "Load of options from Scheme failed: " <<
            err.what() << std::endl;
    }
    return iss;
}

std::ostream&
GncOptionDB::save_option_key_value(std::ostream& oss,
                                   const std::string& section,
                                   const std::string& name) const noexcept
{

    auto db_opt = find_option(section, name.c_str());
    if (!db_opt || !db_opt->is_changed())
        return oss;
    oss << section.substr(0, classifier_size_max) << ":" <<
        name.substr(0, classifier_size_max) << "=" << *db_opt << ";";
    return oss;
}

std::istream&
GncOptionDB::load_option_key_value(std::istream& iss)
{

    char section[classifier_size_max], name[classifier_size_max];
    iss.getline(section, classifier_size_max, ':');
    iss.getline(name, classifier_size_max, '=');
    if (!iss)
        throw std::invalid_argument("Section or name delimiter not found or values too long");
    auto option = find_option(section, name);
    if (!option)
        iss.ignore(stream_max, ';');
    else
    {
        std::string value;
        std::getline(iss, value, ';');
        std::istringstream item_iss{value};
        item_iss >> *option;
    }
    return iss;
}

std::ostream&
GncOptionDB::save_to_key_value(std::ostream& oss) const noexcept
{

    foreach_section(
        [&oss](const GncOptionSectionPtr& section)
        {
            oss << "[Options]\n";
            section->foreach_option(
                [&oss, &section](auto& option)
                {
                    if (option.is_changed())
                        oss << section->get_name().substr(0, classifier_size_max) <<
                            ':' << option.get_name().substr(0, classifier_size_max) <<
                            '=' << option << '\n';
                });
        });
    return oss;
}

std::istream&
GncOptionDB::load_from_key_value(std::istream& iss)
{
    if (iss.peek() == '[')
    {
        char buf[classifier_size_max];
        iss.getline(buf, classifier_size_max);
        if (strcmp(buf, "[Options]") != 0) // safe
            throw std::runtime_error("Wrong secion header for options.");
    }
    // Otherwise assume we were sent here correctly:
    while (iss.peek() != '[') //Indicates the start of the next file section
    {
        load_option_key_value(iss);
    }
    return iss;
}

bool
is_qofinstance_ui_type(GncOptionUIType type)
{
    switch (type)
    {
        case CURRENCY:
        case COMMODITY:
        case ACCOUNT_SEL:
        case BUDGET:
        case OWNER:
        case CUSTOMER:
        case VENDOR:
        case EMPLOYEE:
        case INVOICE:
        case TAX_TABLE:
        case QUERY:
            return true;
        default:
            return false;
    }
}

void
GncOptionDB::save_to_kvp(QofBook* book, bool clear_options) const noexcept
{
    if (clear_options)
        qof_book_options_delete(book, nullptr);
    const_cast<GncOptionDB*>(this)->foreach_section(
        [clear_options, book](GncOptionSectionPtr& section)
        {
            section->foreach_option(
                [clear_options, book, &section](auto& option) {
                    if (clear_options || option.is_changed())
                    {
                        // qof_book_set_option wants a GSList path. Let's avoid
                        // allocating and make one here.
                        GSList list_tail{(void*)option.get_name().c_str(), nullptr};
                        GSList list_head{(void*)section->get_name().c_str(), &list_tail};
                        auto type{option.get_ui_type()};
                        if (type == GncOptionUIType::BOOLEAN)
                        {
                            auto val{option.template get_value<bool>()};
                            // ~KvpValue will g_free the value.
                            auto kvp{new KvpValue(val ? g_strdup("t") :
                                                  g_strdup("f"))};
                            qof_book_set_option(book, kvp, &list_head);
                        }
                        else if (is_qofinstance_ui_type(type))
                        {
                            const QofInstance* inst{QOF_INSTANCE(option.template get_value<const QofInstance*>())};
                            auto guid = guid_copy(qof_instance_get_guid(inst));
                            auto kvp{new KvpValue(guid)};
                            qof_book_set_option(book, kvp, &list_head);
                        }
                        else if (type == GncOptionUIType::NUMBER_RANGE)
                        {
                            /* The Gtk control uses a double so that's what we
                             * have to store. */
                            auto kvp{new KvpValue(option.template get_value<double>())};
                            qof_book_set_option(book, kvp, &list_head);
                        }
                        else
                        {
                            auto kvp{new KvpValue{g_strdup(option.template get_value<std::string>().c_str())}};
                            qof_book_set_option(book, kvp, &list_head);
                        }
                    }
                });
        });
}

void
GncOptionDB::load_from_kvp(QofBook* book) noexcept
{
    foreach_section(
        [book](GncOptionSectionPtr& section)
        {
            section->foreach_option(
                [book, &section](GncOption& option)
                {
                    /* qof_book_set_option wants a GSList path. Let's avoid allocating
                     * and make one here.
                     */
                    GSList list_tail{(void*)option.get_name().c_str(), nullptr};
                    GSList list_head{(void*)section->get_name().c_str(), &list_tail};
                    auto kvp = qof_book_get_option(book, &list_head);
                    if (!kvp)
                        return;
                    switch (kvp->get_type())
                    {
                        case KvpValue::Type::DOUBLE:
                            option.set_value(kvp->get<double>());
                            break;
                        case KvpValue::Type::INT64:
                            option.set_value(kvp->get<int64_t>());
                            break;
                        case KvpValue::Type::STRING:
                        {
                            auto str{kvp->get<const char*>()};
                            if (option.get_ui_type() == GncOptionUIType::BOOLEAN)
                                option.set_value(*str == 't' ? true : false);
                            else
                                option.set_value(str);
                            break;
                        }
                        case KvpValue::Type::GUID:
                        {
                            auto guid{kvp->get<GncGUID*>()};
                            option.set_value((const QofInstance*)qof_instance_from_guid(guid, option.get_ui_type()));
                            break;
                        }
                        default:
                            return;
                            break;
                    }
                });
        });
}

void
gnc_register_string_option(GncOptionDB* db, const char* section,
                           const char* name, const char* key,
                           const char* doc_string, std::string value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::STRING};
    db->register_option(section, std::move(option));
}

void
gnc_register_text_option(GncOptionDB* db, const char* section, const char* name,
                         const char* key, const char* doc_string,
                         std::string value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::TEXT};
    db->register_option(section, std::move(option));

}

void
gnc_register_font_option(GncOptionDB* db, const char* section,
                         const char* name, const char* key,
                         const char* doc_string, std::string value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::FONT};
    db->register_option(section, std::move(option));
}

void
gnc_register_budget_option(GncOptionDB* db, const char* section,
                           const char* name, const char* key,
                           const char* doc_string, GncBudget *value)
{
    GncOption option{section, name, key, doc_string, (const QofInstance*)value,
            GncOptionUIType::BUDGET};
    db->register_option(section, std::move(option));
}

void
gnc_register_color_option(GncOptionDB* db, const char* section,
                         const char* name, const char* key,
                         const char* doc_string, std::string value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::COLOR};
    db->register_option(section, std::move(option));
}

void
gnc_register_commodity_option(GncOptionDB* db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string, gnc_commodity *value)
{
    GncOption option{section, name, key, doc_string, (const QofInstance*)value,
            GncOptionUIType::COMMODITY};
    db->register_option(section, std::move(option));
}

void
gnc_register_simple_boolean_option(GncOptionDB* db,
                                   const char* section, const char* name,
                                   const char* key, const char* doc_string,
                                   bool value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::BOOLEAN};
    db->register_option(section, std::move(option));
}

void
gnc_register_complex_boolean_option(GncOptionDB* db,
                                    const char* section, const char* name,
                                    const char* key, const char* doc_string,
                                    bool value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::BOOLEAN};
    db->register_option(section, std::move(option));
}

void
gnc_register_pixmap_option(GncOptionDB* db, const char* section,
                           const char* name, const char* key,
                           const char* doc_string, std::string value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::PIXMAP};
    db->register_option(section, std::move(option));
}

void
gnc_register_account_list_option(GncOptionDB* db, const char* section,
                                 const char* name, const char* key,
                                 const char* doc_string,
                                 const GncOptionAccountList& value)
{
    GncOption option{GncOptionAccountValue{section, name, key, doc_string,
                GncOptionUIType::ACCOUNT_LIST, value}};
    db->register_option(section, std::move(option));
}

void
gnc_register_account_list_limited_option(GncOptionDB* db,
                                         const char* section, const char* name,
                                         const char* key,
                                         const char* doc_string,
                                         const GncOptionAccountList& value,
                                         GncOptionAccountTypeList&& allowed)
{
    try
    {
        GncOption option{GncOptionAccountValue{section, name, key, doc_string,
                    GncOptionUIType::ACCOUNT_LIST, value, std::move(allowed)}};
        db->register_option(section, std::move(option));
    }
    catch (const std::invalid_argument& err)
    {
        std::cerr << "Account List Limited Option, value failed validation, option not registered.\n";
    }
}

using AccountPair = std::pair<GncOptionAccountList&,
                              const GncOptionAccountTypeList&>;
static void
find_children(Account* account, void* data)
{
    auto datapair =
        (AccountPair*)data;
    GncOptionAccountList& list = datapair->first;
    const GncOptionAccountTypeList& types = datapair->second;
    if (std::find(types.begin(), types.end(),
                  xaccAccountGetType(account)) != types.end())
        list.push_back(account);
}

GncOptionAccountList
gnc_account_list_from_types(QofBook *book,
                            const GncOptionAccountTypeList& types)
{
    GncOptionAccountList list;
    AccountPair funcdata{list, types};
    Account* base_acct = gnc_book_get_root_account(book);
    gnc_account_foreach_descendant(base_acct, (AccountCb)find_children,
                                   &funcdata);
    return list;
}


void
gnc_register_account_sel_limited_option(GncOptionDB* db,
                                        const char* section, const char* name,
                                        const char* key, const char* doc_string,
                                        const GncOptionAccountList& value,
                                        GncOptionAccountTypeList&& allowed)
{
    try
    {
        GncOption option{GncOptionAccountValue{section, name, key, doc_string,
                    GncOptionUIType::ACCOUNT_SEL, value, std::move(allowed)}};
    db->register_option(section, std::move(option));
    }
    catch (const std::invalid_argument& err)
    {
        std::cerr <<"Account Sel Limited Option, value failed validation, option not registerd.\n";
    }
}

void
gnc_register_multichoice_option(GncOptionDB* db, const char* section,
                                const char* name, const char* key,
                                const char* doc_string,
                                GncMultichoiceOptionChoices&& choices)
{
    GncOption option{GncOptionMultichoiceValue{section, name, key, doc_string,
                std::get<0>(choices.at(0)).c_str(), std::move(choices)}};
    db->register_option(section, std::move(option));
}

void
gnc_register_list_option(GncOptionDB* db, const char* section,
                         const char* name, const char* key,
                         const char* doc_string, const char* value,
                         GncMultichoiceOptionChoices&& list)
{
    GncOption option{GncOptionMultichoiceValue{section, name, key, doc_string,
                value,  std::move(list), GncOptionUIType::LIST}};
    db->register_option(section, std::move(option));
}

/* Only balance-forecast.scm, hello-world.scm, and net-charts.scm
 * use decimals and fractional steps and they can be worked around.
 */
template <typename ValueType> void
gnc_register_number_range_option(GncOptionDB* db, const char* section,
                                 const char* name, const char* key,
                                 const char* doc_string, ValueType value,
                                 ValueType min, ValueType max, ValueType step)
{
    GncOption option{GncOptionRangeValue<ValueType>{section, name, key,
                                                    doc_string, value, min,
                                                    max, step}};
    db->register_option(section, std::move(option));
}

void
gnc_register_number_plot_size_option(GncOptionDB* db,
                                     const char* section, const char* name,
                                     const char* key, const char* doc_string,
                                     int value)
{
    GncOption option{GncOptionRangeValue<int>{section, name, key, doc_string,
                value, 100, 20000, 5}};
    db->register_option(section, std::move(option));
}

void
gnc_register_query_option(GncOptionDB* db, const char* section,
                          const char* name, const char* key,
                          const char* doc_string, QofQuery* value)
{
    GncOption option{section, name, key, doc_string, (const QofInstance*)value,
            GncOptionUIType::INTERNAL};
    db->register_option(section, std::move(option));
}

void
gnc_register_internal_option(GncOptionDB* db, const char* section,
                             const char* name, const char* key,
                             const char* doc_string, std::string value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::INTERNAL};
    db->register_option(section, std::move(option));
}

static inline GncOptionUIType
owner_type_to_ui_type(GncOwnerType type)
{
    switch (type)
    {
        case GNC_OWNER_NONE:
        case GNC_OWNER_UNDEFINED:
        case GNC_OWNER_JOB:
            return GncOptionUIType::INTERNAL;
        case GNC_OWNER_CUSTOMER:
            return GncOptionUIType::CUSTOMER;
        case GNC_OWNER_VENDOR:
            return GncOptionUIType::VENDOR;
        case GNC_OWNER_EMPLOYEE:
            return GncOptionUIType::EMPLOYEE;
    }
}

void
gnc_register_owner_option(GncOptionDB* db, const char* section,
                          const char* name, const char* key,
                          const char* doc_string, GncOwner* value,
                          GncOwnerType type)
{
    GncOption option{section, name, key, doc_string,
                     (const QofInstance*)value->owner.undefined,
                     owner_type_to_ui_type(type)};
    db->register_option(section, std::move(option));
}

void
gnc_register_invoice_option(GncOptionDB* db, const char* section,
                            const char* name, const char* key,
                            const char* doc_string, GncInvoice* value)
{
    GncOption option{section, name, key, doc_string, (const QofInstance*)value,
            GncOptionUIType::INVOICE};
    db->register_option(section, std::move(option));
}

void
gnc_register_taxtable_option(GncOptionDB* db, const char* section,
                             const char* name, const char* key,
                             const char* doc_string, GncTaxTable* value)
{
    GncOption option{section, name, key, doc_string, (const QofInstance*)value,
            GncOptionUIType::TAX_TABLE};
    db->register_option(section, std::move(option));
}

void
gnc_register_counter_option(GncOptionDB* db, const char* section,
                            const char* name, const char* key,
                            const char* doc_string, double value)
{
    GncOption option{GncOptionRangeValue<double>{section, name, key, doc_string,
                value, 0.0, 999999999.0, 1.0}};
    db->register_option(section, std::move(option));
}

void
gnc_register_counter_format_option(GncOptionDB* db,
                                   const char* section, const char* name,
                                   const char* key, const char* doc_string,
                                   std::string value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::STRING};
    db->register_option(section, std::move(option));
}

void
gnc_register_dateformat_option(GncOptionDB* db, const char* section,
                               const char* name, const char* key,
                               const char* doc_string, std::string value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::DATE_FORMAT};
    db->register_option(section, std::move(option));
}

void
gnc_register_currency_option(GncOptionDB* db, const char* section,
                             const char* name, const char* key,
                             const char* doc_string, gnc_commodity *value)
{
    GncOption option{GncOptionValidatedValue<const QofInstance*>{
        section, name, key, doc_string, (const QofInstance*)value,
        [](const QofInstance* new_value) -> bool
            {
                return GNC_IS_COMMODITY (new_value) &&
                    gnc_commodity_is_currency(GNC_COMMODITY(new_value));
            },
            GncOptionUIType::CURRENCY
        }};
    db->register_option(section, std::move(option));
}

void
gnc_register_date_option(GncOptionDB* db, const char* section,
                         const char* name, const char* key,
                         const char* doc_string, time64 time,
                         RelativeDateUI ui)
{
    auto ui_type = ui == RelativeDateUI::BOTH ? GncOptionUIType::DATE_BOTH :
        ui == RelativeDateUI::RELATIVE ? GncOptionUIType::DATE_RELATIVE :
        GncOptionUIType::DATE_ABSOLUTE;
    GncOption option{GncOptionDateValue(section, name, key, doc_string,
                                        ui_type, time)};
    db->register_option(section, std::move(option));
}

void
gnc_register_date_option(GncOptionDB* db, const char* section,
                         const char* name, const char* key,
                         const char* doc_string, RelativeDatePeriod period,
                         RelativeDateUI ui)
{
    auto ui_type = ui == RelativeDateUI::BOTH ? GncOptionUIType::DATE_BOTH :
        ui == RelativeDateUI::RELATIVE ? GncOptionUIType::DATE_RELATIVE :
        GncOptionUIType::DATE_ABSOLUTE;
    GncOption option{GncOptionDateValue(section, name, key, doc_string,
                                        ui_type, period)};
    db->register_option(section, std::move(option));
}

void
gnc_register_date_option(GncOptionDB* db,
                                  const char* section, const char* name,
                                  const char* key, const char* doc_string,
                                  RelativeDatePeriodVec& period_set,
                                  bool both)
{
    auto ui_type = both ? GncOptionUIType::DATE_BOTH :
        GncOptionUIType::DATE_RELATIVE;
    GncOption option{GncOptionDateValue(section, name, key, doc_string,
                                        ui_type, period_set)};
    db->register_option(section, std::move(option));
}


static const RelativeDatePeriodVec begin_dates
{
    RelativeDatePeriod::TODAY,
    RelativeDatePeriod::START_THIS_MONTH,
    RelativeDatePeriod::START_PREV_MONTH,
    RelativeDatePeriod::START_CURRENT_QUARTER,
    RelativeDatePeriod::START_PREV_QUARTER,
    RelativeDatePeriod::START_CAL_YEAR,
    RelativeDatePeriod::START_PREV_YEAR,
    RelativeDatePeriod::START_ACCOUNTING_PERIOD
};

void
gnc_register_start_date_option(GncOptionDB* db, const char* section,
                               const char* name, const char* key,
                               const char* doc_string, bool both)
{
    auto ui_type = both ? GncOptionUIType::DATE_BOTH :
        GncOptionUIType::DATE_RELATIVE;
    GncOption option{GncOptionDateValue(section, name, key, doc_string,
                                        ui_type, begin_dates)};
    db->register_option(section, std::move(option));
}

static const RelativeDatePeriodVec end_dates
{
    RelativeDatePeriod::TODAY,
    RelativeDatePeriod::END_THIS_MONTH,
    RelativeDatePeriod::END_PREV_MONTH,
    RelativeDatePeriod::END_CURRENT_QUARTER,
    RelativeDatePeriod::END_PREV_QUARTER,
    RelativeDatePeriod::END_CAL_YEAR,
    RelativeDatePeriod::END_PREV_YEAR,
    RelativeDatePeriod::END_ACCOUNTING_PERIOD
};

void
gnc_register_end_date_option(GncOptionDB* db, const char* section,
                             const char* name, const char* key,
                             const char* doc_string, bool both)
{
    auto ui_type = both ? GncOptionUIType::DATE_BOTH :
        GncOptionUIType::DATE_RELATIVE;
    GncOption option{GncOptionDateValue(section, name, key, doc_string,
                                        ui_type, end_dates)};
    db->register_option(section, std::move(option));
}

GncOptionDB*
gnc_option_db_new(void)
{
    return new GncOptionDB;
}

GncOptionDB*
gnc_option_db_new_for_type(QofIdType type)
{
    if (strcmp(type, QOF_ID_BOOK))
        return nullptr;
    auto db = new GncOptionDB;
    gnc_option_db_book_options(db);
    return db;
}

void
gnc_option_db_destroy(GncOptionDB* odb)
{
    delete odb;
}

GList*
gnc_option_db_commit(GncOptionDB* odb)
{
    GList* errors{};
    odb->foreach_section(
        [&errors](GncOptionSectionPtr& section){
            section->foreach_option(
                [&errors](GncOption& option) {
                    try
                    {
                        option.set_option_from_ui_item();
                    }
                    catch (const std::invalid_argument& err)
                    {
                        PWARN("Option %s:%s failed to set its value %s",
                              option.get_section().c_str(),
                              option.get_name().c_str(), err.what());
                        errors = g_list_prepend(errors,
                                                (void*)option.get_name().c_str());
                    } });
        });
    return errors;
}

void
gnc_option_db_clean(GncOptionDB* odb)
{
        odb->foreach_section(
        [](GncOptionSectionPtr& section){
            section->foreach_option(
                [](GncOption& option) {
                    option.set_ui_item_from_option();
                });
        });
}

void gnc_option_db_load(GncOptionDB* odb, QofBook* book)
{
    odb->load_from_kvp(book);
}

void
gnc_option_db_save(GncOptionDB* odb, QofBook* book,
                        gboolean clear_options)
{
    odb->save_to_kvp(book, static_cast<bool>(clear_options));
}

void
gnc_option_db_book_options(GncOptionDB* odb)
{
    constexpr const char* business_section{N_("Business")};
    constexpr const char* counter_section{N_("Counters")};
    static const std::string empty_string{""};

//Accounts Tab

    gnc_register_number_range_option<double>(odb, OPTION_SECTION_ACCOUNTS,
                                     OPTION_NAME_AUTO_READONLY_DAYS, "a",
                                     N_("Choose the number of days after which transactions will be read-only and cannot be edited anymore. This threshold is marked by a red line in the account register windows. If zero, all transactions can be edited and none are read-only."),
                                     0.0, 0.0, 3650.0, 1.0);

    gnc_register_simple_boolean_option(odb, OPTION_SECTION_ACCOUNTS,
                                       OPTION_NAME_NUM_FIELD_SOURCE, "b",
                                       N_("Check to have split action field used in registers for 'Num' field in place of transaction number; transaction number shown as 'T-Num' on second line of register. Has corresponding effect on business features, reporting and imports/exports."),
                                       false);
    gnc_register_simple_boolean_option(odb, OPTION_SECTION_ACCOUNTS,
                                       OPTION_NAME_TRADING_ACCOUNTS, "a",
                                       N_("Check to have trading accounts used for transactions involving more than one currency or commodity."),
                                       false);

//Budgeting Tab

    gnc_register_budget_option(odb, OPTION_SECTION_BUDGETING,
                               OPTION_NAME_DEFAULT_BUDGET, "a",
                               N_("Budget to be used when none has been otherwise specified."),
                               nullptr);

//Counters Tab

    gnc_register_counter_option(odb, counter_section,
                                N_("Customer number"), "a",
                                N_("The previous customer number generated. This number will be incremented to generate the next customer number."),
                                0.0);
    gnc_register_counter_format_option(odb, counter_section,
                                       N_("Customer number format"), "b",
                                       N_("The format string to use for generating customer numbers. This is a printf-style format string."),
                                       empty_string);
    gnc_register_counter_option(odb, counter_section,
                                N_("Employee number"), "a",
                                N_("The previous employee number generated. This number will be incremented to generate the next employee number."),
                                0.0);
    gnc_register_counter_format_option(odb, counter_section,
                                       N_("Employee number format"), "b",
                                       N_("The format string to use for generating employee numbers. This is a printf-style format string."),
                                       empty_string);
    gnc_register_counter_option(odb, counter_section,
                                N_("Invoice number"), "a",
                                N_("The previous invoice number generated. This number will be incremented to generate the next invoice number."),
                                0.0);
    gnc_register_counter_format_option(odb, counter_section,
                                       N_("Invoice number format"), "b",
                                       N_("The format string to use for generating invoice numbers. This is a printf-style format string."),
                                       empty_string);
    gnc_register_counter_option(odb, counter_section,
                                N_("Bill number"), "a",
                                N_("The previous bill number generated. This number will be incremented to generate the next bill number."),
                                0.0);
    gnc_register_counter_format_option(odb, counter_section,
                                       N_("Bill number format"), "b",
                                       N_("The format string to use for generating bill numbers. This is a printf-style format string."),
                                       empty_string);
    gnc_register_counter_option(odb, counter_section,
                                N_("Expense voucher number"), "a",
                                N_("The previous expense voucher number generated. This number will be incremented to generate the next voucher number."),
                                0.0);
    gnc_register_counter_format_option(odb, counter_section,
                                       N_("Expense voucher number format"), "b",
                                       N_("The format string to use for generating expense voucher numbers. This is a printf-style format string."),
                                       empty_string);
    gnc_register_counter_option(odb, counter_section,
                                N_("Job number"), "a",
                                N_("The previous job number generated. This number will be incremented to generate the next job number."),
                                0.0);
    gnc_register_counter_format_option(odb, counter_section,
                                       N_("Job number format"), "b",
                                       N_("The format string to use for generating job numbers. This is a printf-style format string."),
                                       empty_string);
    gnc_register_counter_option(odb, counter_section,
                                N_("Order number"), "a",
                                N_("The previous order number generated. This number will be incremented to generate the next order number."),
                                0.0);
    gnc_register_counter_format_option(odb, counter_section,
                                       N_("Order number format"), "b",
                                       N_("The format string to use for generating order numbers. This is a printf-style format string."),
                                       empty_string);
    gnc_register_counter_option(odb, counter_section,
                                N_("Vendor number"), "a",
                                N_("The previous vendor number generated. This number will be incremented to generate the next vendor number."),
                                0.0);
    gnc_register_counter_format_option(odb, counter_section,
                                       N_("Vendor number format"), "b",
                                       N_("The format string to use for generating vendor numbers. This is a printf-style format string."),
                                       empty_string);

//Business Tab

    gnc_register_string_option(odb, business_section, N_("Company Name"), "a",
                               N_("The name of your business."),
                               empty_string);
    gnc_register_text_option(odb, business_section, N_("Company Address"), "b1",
                             N_("The address of your business."),
                             empty_string);
    gnc_register_string_option(odb, business_section,
                               N_("Company Contact Person"), "b2",
                               N_("The contact person to print on invoices."),
                               empty_string);
    gnc_register_string_option(odb, business_section,
                               N_("Company Phone Number"), "c1",
                               N_("The contact person to print on invoices."),
                               empty_string);
    gnc_register_string_option(odb, business_section,
                               N_("Company Fax Number"), "c2",
                               N_("The fax number of your business."),
                               empty_string);
    gnc_register_string_option(odb, business_section,
                               N_("Company Email Address"), "c3",
                               N_ ("The email address of your business."),
                               empty_string);
    gnc_register_string_option(odb, business_section,
                               N_("Company Website URL"), "c4",
                               N_("The URL address of your website."),
                               empty_string);
    gnc_register_string_option(odb, business_section, N_("Company ID"), "c5",
                               N_("The ID for your company (eg 'Tax-ID: 00-000000)."),
                               empty_string);

    gnc_register_taxtable_option(odb, business_section,
                                 N_("Default Customer TaxTable"), "e",
                                 N_("The default tax table to apply to customers."),
                                 nullptr);
    gnc_register_taxtable_option(odb, business_section,
                                 N_("Default Vendor TaxTable"), "f",
                                 N_("The default tax table to apply to vendors."),
                                 nullptr);
    gnc_register_dateformat_option(odb, business_section,
                                   N_("Fancy Date Format"), "g",
                                   N_("The default date format used for fancy printed dates."),
                                   empty_string);

//Tax Tab

    gnc_register_string_option(odb, N_("Tax"), N_("Tax Number"), "a",
                               N_("The electronic tax number of your business"),
                               empty_string);
}

const char*
gnc_option_db_lookup_string_value(GncOptionDB*, const char*, const char*)
{
    return nullptr;
}

void
gnc_option_db_set_string_value(GncOptionDB*, const char*,
                               const char*, const char*)
{
}

const QofInstance*
gnc_option_db_lookup_qofinstance_value(GncOptionDB*, const char*, const char*)
{
    return nullptr;
}

GList*
gnc_option_db_lookup_glist_value(GncOptionDB*, const char*, const char*)
{
    return nullptr;
}

void
gnc_option_db_set_glist_value(GncOptionDB*, const char*, const char*, GList*)
{
}

// Force creation of templates
template void gnc_register_number_range_option(GncOptionDB* db,
                                      const char* section, const char* name,
                                      const char* key, const char* doc_string,
                                      int value, int min, int max, int step);
template void gnc_register_number_range_option(GncOptionDB* db,
                                      const char* section, const char* name,
                                      const char* key, const char* doc_string,
                                      double value, double min,
                                      double max, double step);
