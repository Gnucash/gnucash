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

#include "gnc-optiondb.hpp"
#include <limits>
#include <sstream>
#include <kvp-value.hpp>

auto constexpr stream_max = std::numeric_limits<std::streamsize>::max();
GncOptionDB::GncOptionDB() : m_default_section{std::nullopt} {}

GncOptionDB::GncOptionDB(QofBook* book) : GncOptionDB() {}

void
GncOptionDB::save_to_book(QofBook* book, bool do_clear) const
{
}

void
GncOptionDB::register_option(const char* section, GncOption&& option)
{
    auto db_section = find_section(section);

    if (db_section)
    {
        db_section->get().second.emplace_back(std::move(option));
        return;
    }

    m_sections.emplace_back(std::make_pair(std::string{section},
                                               GncOptionVec{}));
    auto new_section = std::prev(m_sections.end());
    new_section->second.emplace_back(std::move(option));
}

void
GncOptionDB::unregister_option(const char* section, const char* name)
{
    auto db_section = find_section(section);
    if (db_section)
    {
        db_section->get().second.erase(
            std::remove_if(
                db_section->get().second.begin(), db_section->get().second.end(),
                [name](const GncOption& option) -> bool
                {
                    return option.get_name() == std::string{name};
                }));
    }
}

void
GncOptionDB::set_default_section(const char* section)
{
    m_default_section = find_section(section);
}

const GncOptionSection* const
GncOptionDB::get_default_section() const noexcept
{
    if (m_default_section)
        return &(m_default_section.value().get());
    return nullptr;
}

void
GncOptionDB::set_ui_item(const char* section, const char* name,
                         GncOptionUIItem* ui_item)
{
    auto option = find_option(section, name);
    if (!option) return;
    option->get().set_ui_item(ui_item);
}

GncOptionUIItem* const
GncOptionDB::get_ui_item(const char* section, const char* name)
{
    auto option = find_option(section, name);
    if (!option) return nullptr;
    return option->get().get_ui_item();
}

GncOptionUIType
GncOptionDB::get_ui_type(const char* section, const char* name)
{
    auto option = find_option(section, name);
    if (!option) return GncOptionUIType::INTERNAL;
    return option->get().get_ui_type();
}

void
GncOptionDB::set_ui_from_option(const char* section, const char* name,
                        std::function<void(GncOption&)> func)
{
    auto option = find_option(section, name);
    if (!option) return;
    func(option->get());
}

void
GncOptionDB::set_option_from_ui(const char* section, const char* name,
                        std::function<void(GncOption&)> func)
{
    auto option = find_option(section, name);
    if (!option) return;
    func(option->get());
}


std::optional<std::reference_wrapper<GncOptionSection>>
GncOptionDB::find_section(const std::string& section)
{
    auto db_section = std::find_if(
        m_sections.begin(), m_sections.end(),
        [&section](GncOptionSection sect) -> bool
        {
            return section.compare(0, classifier_size_max, sect.first) == 0;
        });
    if (db_section == m_sections.end())
        return std::nullopt;
    return *db_section;
}

std::optional<std::reference_wrapper<GncOption>>
GncOptionDB::find_option(const std::string& section, const std::string& name) const
{
    auto db_section = const_cast<GncOptionDB*>(this)->find_section(section);
    if (!db_section)
        return std::nullopt;
    auto db_opt = std::find_if(
        db_section->get().second.begin(), db_section->get().second.end(),
        [&name](GncOption& option) -> bool
        {
            return name.compare(0, classifier_size_max - 1,
                                  option.get_name()) == 0;
        });
    if (db_opt == db_section->get().second.end())
        return std::nullopt;
    return *db_opt;
}

std::string
GncOptionDB::lookup_string_option(const char* section, const char* name)
{
    static const std::string empty_string{};

    auto db_opt = find_option(section, name);
    if (!db_opt)
        return empty_string;
    return db_opt->get().get_value<std::string>();
}

void
GncOptionDB::make_internal(const char* section, const char* name)
{
    auto db_opt = find_option(section, name);
    if (db_opt)
        db_opt->get().make_internal();
}

std::ostream&
GncOptionDB::save_option_scheme(std::ostream& oss,
                                const char* option_prolog,
                                const std::string& section,
                                const std::string& name) const noexcept
{
    auto db_opt = find_option(section, name);

    if (!db_opt || !db_opt->get().is_changed())
        return oss;
    oss << scheme_tags[0] << option_prolog << "\n";
    oss << scheme_tags[1] << '"' << section.substr(0, classifier_size_max) << "\"\n";
    oss << scheme_tags[1] << '"' << name.substr(0, classifier_size_max) << '"';
    oss  <<  scheme_tags[2] << "\n" << scheme_tags[3];
    db_opt->get().to_scheme(oss);
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

static inline void constexpr
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

static inline void constexpr
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
 * An identifier is a string and a type (name, const, string, or form). A Form 
 * 
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

static inline bool
string_equal_charptr(const std::string& str, const char* chars)
{
    if (!chars || str.empty() || strlen(chars) != str.size())
        return false;
    return strcmp(str.c_str(), chars) == 0;
}

static std::optional<std::reference_wrapper<const SchemeId>>
find_form(const SchemeId& toplevel, IdentType type, const char* name)
{
    if (toplevel.m_type == type &&
        string_equal_charptr(toplevel.m_name.c_str(), name))
        return std::ref(toplevel);
    for (const auto& id : toplevel.m_ids)
    {
        if (id.m_type == type && string_equal_charptr(id.m_name.c_str(), name))
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
    auto option = find_option(section.c_str(), name.c_str());
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
    option->get().from_scheme(value_iss);
    return iss;
}

std::ostream&
GncOptionDB::save_to_scheme(std::ostream& oss, const char* options_prolog) const noexcept
{
    for (auto section : m_sections)
    {
        const auto& [s_name, s_vec] = section;
        oss << "\n; Section: " << s_name << "\n\n";
        for (auto option : s_vec)
        {
            if (!option.is_changed())
                continue;
            oss << scheme_tags[0] << options_prolog << "\n";
            oss << scheme_tags[1] << '"' << section.first.substr(0, classifier_size_max) << "\"\n";
            oss << scheme_tags[1] << '"' << option.get_name().substr(0, classifier_size_max) << '"';
            oss  <<  scheme_tags[2] << "\n" << scheme_tags[3];
            option.to_scheme(oss);
            oss << scheme_tags[4] << "\n\n";
        }
    }
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

    auto db_opt = find_option(section, name);
    if (!db_opt || !db_opt->get().is_changed())
        return oss;
    oss << section.substr(0, classifier_size_max) << ":" <<
        name.substr(0, classifier_size_max) << "=" << db_opt->get() << ";";
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
        item_iss >> option->get();
    }
    return iss;
}

std::ostream&
GncOptionDB::save_to_key_value(std::ostream& oss) const noexcept
{

    for (auto section : m_sections)
    {
        const auto& [s_name, s_vec] = section;
        oss << "[Options]\n";
        for (auto option : s_vec)
        {
            if (option.is_changed())
                oss << section.first.substr(0, classifier_size_max) <<
                    ':' << option.get_name().substr(0, classifier_size_max) <<
                    '=' << option << '\n';
       }
    }
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

void
GncOptionDB::save_to_kvp(QofBook* book, bool clear_options) const noexcept
{
    if (clear_options)
        qof_book_options_delete(book, nullptr);
    for (auto section : m_sections)
    {
        const auto& [s_name, s_vec] = section;
        for (auto option : s_vec)
            if (option.is_changed())
            {
                // qof_book_set_option wants a GSList path. Let's avoid allocating and make one here.
                GSList list_tail{(void*)option.get_name().c_str(), nullptr};
                GSList list_head{(void*)s_name.c_str(), &list_tail};
                auto type{option.get_ui_type()};
                if (type == GncOptionUIType::BOOLEAN)
                {
                    auto val{option.get_value<bool>()};
                    auto kvp{new KvpValue(val ? "t" : "f")};
                    qof_book_set_option(book, kvp, &list_head);
                }
                else if (type > GncOptionUIType::DATE_FORMAT)
                {
                    QofInstance* inst{QOF_INSTANCE(option.get_value<QofInstance*>())};
                    auto guid = guid_copy(qof_instance_get_guid(inst));
                    auto kvp{new KvpValue(guid)};
                    qof_book_set_option(book, kvp, &list_head);
                }
                else if (type == GncOptionUIType::NUMBER_RANGE)
                {
                    auto kvp{new KvpValue(option.get_value<int64_t>())};
                    qof_book_set_option(book, kvp, &list_head);
                }
                else
                {
                    auto kvp{new KvpValue{g_strdup(option.get_value<std::string>().c_str())}};
                    qof_book_set_option(book, kvp, &list_head);
                }
            }
    }
}

void
GncOptionDB::load_from_kvp(QofBook* book) noexcept
{
    for (auto section : m_sections)
    {
        const auto& [s_name, s_vec] = section;
        for (auto option : s_vec)
        {
            /* qof_book_set_option wants a GSList path. Let's avoid allocating
             * and make one here. */
            GSList list_tail{(void*)option.get_name().c_str(), nullptr};
            GSList list_head{(void*)s_name.c_str(), &list_tail};
            auto kvp = qof_book_get_option(book, &list_head);
            if (!kvp)
                continue;
            switch (kvp->get_type())
            {
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
                    option.set_value(qof_instance_from_guid(guid, option.get_ui_type()));
                    break;
                }
                default:
                    continue;
                    break;
            }
        }
    }
}

GncOptionDBPtr
gnc_option_db_new(void)
{
    return GncOptionDBPtr{new GncOptionDB};
}

void
gnc_register_string_option(const GncOptionDBPtr& db, const char* section,
                           const char* name, const char* key,
                           const char* doc_string, std::string value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::STRING};
    db->register_option(section, std::move(option));
}

void
gnc_register_text_option(const GncOptionDBPtr& db, const char* section, const char* name,
                         const char* key, const char* doc_string,
                         std::string value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::TEXT};
    db->register_option(section, std::move(option));

}

void
gnc_register_font_option(const GncOptionDBPtr& db, const char* section,
                         const char* name, const char* key,
                         const char* doc_string, std::string value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::FONT};
    db->register_option(section, std::move(option));
}

void
gnc_register_budget_option(const GncOptionDBPtr& db, const char* section,
                           const char* name, const char* key,
                           const char* doc_string, GncBudget *value)
{
    GncOption option{section, name, key, doc_string, QOF_INSTANCE(value),
            GncOptionUIType::BUDGET};
    db->register_option(section, std::move(option));
}

void
gnc_register_color_option(const GncOptionDBPtr& db, const char* section,
                         const char* name, const char* key,
                         const char* doc_string, std::string value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::COLOR};
    db->register_option(section, std::move(option));
}

void
gnc_register_commodity_option(const GncOptionDBPtr& db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string, gnc_commodity *value)
{
    GncOption option{section, name, key, doc_string, QOF_INSTANCE(value),
            GncOptionUIType::COMMODITY};
    db->register_option(section, std::move(option));
}

void
gnc_register_simple_boolean_option(const GncOptionDBPtr& db,
                                   const char* section, const char* name,
                                   const char* key, const char* doc_string,
                                   bool value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::INTERNAL};
    db->register_option(section, std::move(option));
}

void
gnc_register_complex_boolean_option(const GncOptionDBPtr& db,
                                    const char* section, const char* name,
                                    const char* key, const char* doc_string,
                                    bool value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::BOOLEAN};
    db->register_option(section, std::move(option));
}

void
gnc_register_pixmap_option(const GncOptionDBPtr& db, const char* section,
                           const char* name, const char* key,
                           const char* doc_string, std::string value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::PIXMAP};
    db->register_option(section, std::move(option));
}

void
gnc_register_account_list_option(const GncOptionDBPtr& db, const char* section,
                                 const char* name, const char* key,
                                 const char* doc_string,
                                 const GncOptionAccountList& value)
{
    GncOption option{GncOptionAccountValue{section, name, key, doc_string,
                GncOptionUIType::ACCOUNT_LIST, value}};
    db->register_option(section, std::move(option));
}

void
gnc_register_account_list_limited_option(const GncOptionDBPtr& db,
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
gnc_register_account_sel_limited_option(const GncOptionDBPtr& db,
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
gnc_register_multichoice_option(const GncOptionDBPtr& db, const char* section,
                                const char* name, const char* key,
                                const char* doc_string,
                                GncMultiChoiceOptionChoices&& choices)
{
    GncOption option{GncOptionMultichoiceValue{section, name, key, doc_string,
                std::get<0>(choices.at(0)).c_str(), std::move(choices)}};
    db->register_option(section, std::move(option));
}

void
gnc_register_list_option(const GncOptionDBPtr& db, const char* section,
                         const char* name, const char* key,
                         const char* doc_string, const char* value,
                         GncMultiChoiceOptionChoices&& list)
{
    GncOption option{GncOptionMultichoiceValue{section, name, key, doc_string,
                value,  std::move(list), GncOptionUIType::LIST}};
    db->register_option(section, std::move(option));
}

/* Only balance-forecast.scm, hello-world.scm, and net-charts.scm
 * use decimals and fractional steps and they can be worked around. */
void
gnc_register_number_range_option(const GncOptionDBPtr& db, const char* section,
                                 const char* name, const char* key,
                                 const char* doc_string, int value, int min,
                                 int max, int step)
{
    GncOption option{GncOptionRangeValue<int>{section, name, key, doc_string,
                value, min, max, step}};
    db->register_option(section, std::move(option));
}

void
gnc_register_number_plot_size_option(const GncOptionDBPtr& db,
                                     const char* section, const char* name,
                                     const char* key, const char* doc_string,
                                     int value)
{
    GncOption option{GncOptionRangeValue<int>{section, name, key, doc_string,
                value, 100, 20000, 5}};
    db->register_option(section, std::move(option));
}

void
gnc_register_query_option(const GncOptionDBPtr& db, const char* section,
                          const char* name, const char* key,
                          const char* doc_string, QofQuery* value)
{
    GncOption option{section, name, key, doc_string, QOF_INSTANCE(value),
            GncOptionUIType::INTERNAL};
    db->register_option(section, std::move(option));
}

void
gnc_register_internal_option(const GncOptionDBPtr& db, const char* section,
                             const char* name, const char* key,
                             const char* doc_string, std::string value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::INTERNAL};
    db->register_option(section, std::move(option));
}

void
gnc_register_invoice_option(const GncOptionDBPtr& db, const char* section,
                            const char* name, const char* key,
                            const char* doc_string, GncInvoice* value)
{
    GncOption option{section, name, key, doc_string, QOF_INSTANCE(value),
            GncOptionUIType::INVOICE};
    db->register_option(section, std::move(option));
}

void
gnc_register_owner_option(const GncOptionDBPtr& db, const char* section,
                          const char* name, const char* key,
                          const char* doc_string, GncOwner* value)
{
    GncOption option{section, name, key, doc_string, QOF_INSTANCE(value),
            GncOptionUIType::OWNER};
    db->register_option(section, std::move(option));
}

void
gnc_register_taxtable_option(const GncOptionDBPtr& db, const char* section,
                             const char* name, const char* key,
                             const char* doc_string, GncTaxTable* value)
{
    GncOption option{section, name, key, doc_string, QOF_INSTANCE(value),
            GncOptionUIType::TAX_TABLE};
    db->register_option(section, std::move(option));
}

void
gnc_register_counter_option(const GncOptionDBPtr& db, const char* section,
                            const char* name, const char* key,
                            const char* doc_string, int value)
{
    GncOption option{GncOptionRangeValue<int>{section, name, key, doc_string,
                value, 0, 999999999, 1}};
    db->register_option(section, std::move(option));
}

void
gnc_register_counter_format_option(const GncOptionDBPtr& db,
                                   const char* section, const char* name,
                                   const char* key, const char* doc_string,
                                   std::string value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::STRING};
    db->register_option(section, std::move(option));
}

void
gnc_register_dateformat_option(const GncOptionDBPtr& db, const char* section,
                               const char* name, const char* key,
                               const char* doc_string, std::string value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::DATE_FORMAT};
    db->register_option(section, std::move(option));
}

void
gnc_register_currency_option(const GncOptionDBPtr& db, const char* section,
                             const char* name, const char* key,
                             const char* doc_string, gnc_commodity *value)
{
    GncOption option{GncOptionValidatedValue<QofInstance*>{
        section, name, key, doc_string, QOF_INSTANCE(value),
        [](QofInstance* new_value) -> bool
            {
                return GNC_IS_COMMODITY (new_value) &&
                    gnc_commodity_is_currency(GNC_COMMODITY(new_value));
            },
            GncOptionUIType::CURRENCY
        }};
    db->register_option(section, std::move(option));
}

void
gnc_register_date_interval_option(const GncOptionDBPtr& db, const char* section,
                                  const char* name, const char* key,
                                  const char* doc_string,
                                  RelativeDatePeriod period)
{
    GncOption option{GncOptionDateValue(section, name, key, doc_string, period)};
    db->register_option(section, std::move(option));
}
