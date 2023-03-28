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

#include <cstdint>
#include <functional>
#include <string>
#include <limits>
#include <sstream>
#include "gnc-option-uitype.hpp"
#include "kvp-value.hpp"
#include "qofbookslots.h"
#include "guid.hpp"
#include "gnc-optiondb.h"
#include "gnc-optiondb.hpp"
#include "gnc-optiondb-impl.hpp"
#include "gnc-option-ui.hpp"

#include "gnc-session.h"
constexpr const char* log_module{G_LOG_DOMAIN};

constexpr auto stream_max = std::numeric_limits<std::streamsize>::max();
using AliasedOption = std::pair<const char*, const char*>;
using OptionAlias = std::pair<const char*, AliasedOption>;
using OptionAliases = std::vector<OptionAlias>;
class Aliases
{
    static const OptionAliases c_option_aliases;
public:
    static const AliasedOption* find_alias (const char* old_name)
    {
        if (!old_name) return nullptr;
        const auto alias =
            std::find_if(c_option_aliases.begin(), c_option_aliases.end(),
                         [old_name](auto alias){
                             return std::strcmp(old_name, alias.first) == 0;
                         });
        if (alias == c_option_aliases.end())
            return nullptr;

        return &alias->second;
    }
};

const OptionAliases Aliases::c_option_aliases
{
    {"Accounts to include", {nullptr, "Accounts"}},
    {"Exclude transactions between selected accounts?",
        {nullptr, "Exclude transactions between selected accounts"}},
    {"Filter Accounts", {nullptr, "Filter By…"}},
    {"Flatten list to depth limit?",
        {nullptr, "Flatten list to depth limit"}},
    {"From", {nullptr, "Start Date"}},
    {"Report Accounts", {nullptr, "Accounts"}},
    {"Report Currency", {nullptr, "Report's currency"}},
    {"Show Account Code?", {nullptr, "Show Account Code"}},
    {"Show Full Account Name?", {nullptr, "Show Full Account Name"}},
    {"Show Multi-currency Totals?",
        {nullptr, "Show Multi-currency Totals"}},
    {"Show zero balance items?", {nullptr, "Show zero balance items"}},
    {"Sign Reverses?", {nullptr, "Sign Reverses"}},
    {"To", {nullptr, "End Date"}},
    {"Charge Type", {nullptr, "Action"}}, // easy-invoice.scm, renamed June 2018
    // the following 4 options in income-gst-statement.scm renamed Dec 2018
    {"Individual income columns", {nullptr, "Individual sales columns"}},
    {"Individual expense columns",
        {nullptr, "Individual purchases columns"}},
    {"Remittance amount", {nullptr, "Gross Balance"}},
    {"Net Income", {nullptr, "Net Balance"}},
    // transaction.scm:
    {"Use Full Account Name?", {nullptr, "Use Full Account Name"}},
    {"Use Full Other Account Name?",
        {nullptr, "Use Full Other Account Name"}},
    {"Void Transactions?", {"Filter", "Void Transactions"}},
    {"Void Transactions", {"Filter", "Void Transactions"}},
    {"Account Substring", {"Filter", "Account Name Filter"}},
    {"Enable links", {nullptr, "Enable Links"}},
    // trep-engine: moved currency options to own tab
    {"Common Currency", {"Currency", "Common Currency"}},
    {"Show original currency amount",
        {"Currency", "Show original currency amount"}},
    {"Report's currency", {"Currency", "Report's currency"}},
    {"Reconcile Status", {nullptr, "Reconciled Status"}},
    // new-owner-report.scm, renamed Oct 2020 to differentiate with
    // Document Links:
    {"Links", {nullptr, "Transaction Links"}},
    // invoice.scm, renamed November 2018
    {"Individual Taxes", {nullptr, "Use Detailed Tax Summary"}},
    {"Show Accounts until level", {nullptr, "Levels of Subaccounts"}},
    {"Invoice number", {nullptr, "Invoice Number"}},
    {"Report title", {nullptr, "Report Title"}},
    {"Extra notes", {nullptr, "Extra Notes"}},
    // income-gst-statement.scm
    {"default format", {nullptr, "Default Format"}},
    {"Report format", {nullptr, "Report Format"}},
    // ... replaced to …, Dec 2022
    {"Filter By...", {nullptr, "Filter By…"}},
    {"Specify date to filter by...", {nullptr, "Specify date to filter by…"}},
};

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
    m_options.push_back(std::move(option));
    if (!std::is_sorted(m_options.begin(), m_options.end()))
        std::sort(m_options.begin(), m_options.end());
}

void
GncOptionSection::remove_option(const char* name)
{
    m_options.erase(std::remove_if(m_options.begin(), m_options.end(),
                                   [name](const auto& option) -> bool
                                   {
                                       return option.get_name() == name;
                                   }), m_options.end());
}

const GncOption*
GncOptionSection::find_option(const char* name) const
{
    auto option = std::find_if(m_options.begin(), m_options.end(),
                               [name](auto& option) -> bool {
                                   return option.get_name() == name;
                               });
    if (option != m_options.end())
        return &*option;

    auto alias = Aliases::find_alias(name);
    if (!alias || alias->first) // No alias or the alias
        return nullptr;         // is in a different section.
    return find_option(alias->second);
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

    m_sections.push_back(std::make_shared<GncOptionSection>(sectname));
    m_sections.back()->add_option(std::move(option));
    if (!std::is_sorted(m_sections.begin(), m_sections.end()))
        std::sort(m_sections.begin(), m_sections.end());
}

void
GncOptionDB::register_option(const char* sectname, GncOption* option)
{
    register_option(sectname, std::move(*option));
    delete option;
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
    const GncOption* option = nullptr;
    if (db_section)
        option = db_section->find_option(name);
    if (option)
        return option;
    auto alias = Aliases::find_alias(name);
     /* Only try again if alias.first isn't
     * nullptr. GncOptionSection::find_option already checked if the alias
     * should have been in the same section.
     */
    if (alias && alias->first && section != alias->first)
        return find_option(alias->first, alias->second);
    return nullptr;
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

size_t
GncOptionDB::register_callback(GncOptionDBChangeCallback cb, void* data)
{
    constexpr std::hash<GncOptionDBChangeCallback> cb_hash;
    auto id{cb_hash(cb)};
    if (std::find_if(m_callbacks.begin(), m_callbacks.end(),
                     [id](auto&cb)->bool{ return cb.m_id == id; }) == m_callbacks.end())
        m_callbacks.emplace_back(id, cb, data);
    return id;
}

void
GncOptionDB::unregister_callback(size_t id)
{
    m_callbacks.erase(std::remove_if(m_callbacks.begin(), m_callbacks.end(),
                                     [id](auto& cb)->bool { return cb.m_id == id; }),
                      m_callbacks.end());
}

void
GncOptionDB::run_callbacks()
{
    std::for_each(m_callbacks.begin(), m_callbacks.end(),
                  [](auto& cb)->void { cb.m_func(cb.m_data); });
}

static inline void
counter_option_path(const GncOption& option, GSList* list, std::string& name)
{
    constexpr const char* counters{"counters"};
    constexpr const char* formats{"counter_formats"};
    auto key = option.get_key();
    name = key.substr(0, key.size() - 1);
    list->next->data = (void*)name.c_str();
    if (option.get_name().rfind("format")
        != std::string::npos)
        list->data = (void*)formats;
    else
        list->data = (void*)counters;
}

static inline void
option_path(const GncOption& option, GSList* list)
{
    list->next->data = (void*)option.get_name().c_str();
    list->data = (void*)option.get_section().c_str();
}

static inline KvpValue*
kvp_value_from_bool_option(const GncOption& option)
{
    auto val{option.template get_value<bool>()};
    // ~KvpValue will g_free the value.
    return new KvpValue(val ? g_strdup("t") : g_strdup("f"));
}

static bool
is_qofinstance_ui_type(GncOptionUIType type)
{
    switch (type)
    {
        case GncOptionUIType::ACCOUNT_SEL:
        case GncOptionUIType::BUDGET:
        case GncOptionUIType::OWNER:
        case GncOptionUIType::CUSTOMER:
        case GncOptionUIType::VENDOR:
        case GncOptionUIType::EMPLOYEE:
        case GncOptionUIType::INVOICE:
        case GncOptionUIType::TAX_TABLE:
        case GncOptionUIType::QUERY:
            return true;
        default:
            return false;
    }
}

static inline KvpValue*
kvp_value_from_qof_instance_option(const GncOption& option)
{
    const QofInstance* inst{QOF_INSTANCE(option.template get_value<const QofInstance*>())};
    auto guid = guid_copy(qof_instance_get_guid(inst));
    return new KvpValue(guid);
}

void
GncOptionDB::save_to_kvp(QofBook* book, bool clear_options) const noexcept
{
    if (clear_options)
        qof_book_options_delete(book, nullptr);
    const_cast<GncOptionDB*>(this)->foreach_section(
        [book](GncOptionSectionPtr& section)
        {
            section->foreach_option(
                [book, &section](auto& option) {
                    if (option.is_changed())
                    {
                        /* We need the string name out here so that it stays in
                         * scope long enough to pass its c_str to
                         * gnc_book_set_option. */
                        std::string name;
                        /* qof_book_set_option wants a GSList path. Let's avoid
                         * allocating and make one here. */
                        GSList list_tail{}, list_head{nullptr, &list_tail};
                        if (strcmp(section->get_name().c_str(), "Counters") == 0)
                            counter_option_path(option, &list_head, name);
                        else
                            option_path(option, &list_head);
                        auto type{option.get_ui_type()};
                        KvpValue* kvp{};
                        if (type == GncOptionUIType::BOOLEAN)
                            kvp = kvp_value_from_bool_option(option);
                        else if (is_qofinstance_ui_type(type))
                            kvp = kvp_value_from_qof_instance_option(option);
                        else if (type == GncOptionUIType::NUMBER_RANGE)
                            /* The Gtk control uses a double so that's what we
                             * have to store. */
                            kvp = new KvpValue(option.template get_value<double>());
                        else
                        {
                            auto str{option.template get_value<std::string>()};
                            kvp = new KvpValue{g_strdup(str.c_str())};
                        }
                        qof_book_set_option(book, kvp, &list_head);
                    }
                });
        });
}

static inline void
fill_option_from_string_kvp(GncOption& option, KvpValue* kvp)
{
    auto str{kvp->get<const char*>()};
    if (option.get_ui_type() == GncOptionUIType::BOOLEAN)
        option.set_value(*str == 't' ? true : false);
    else
        option.set_value(std::string{str});
}

static inline void
fill_option_from_guid_kvp(GncOption& option, KvpValue* kvp)
{
    auto guid{kvp->get<GncGUID*>()};
    option.set_value(
        (const QofInstance*)qof_instance_from_guid(guid, option.get_ui_type()));
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
                    // Make path list as above.
                    std::string name;
                    /* qof_book_set_option wants a GSList path. Let's avoid
                     * allocating and make one here. */
                    GSList list_tail{}, list_head{nullptr, &list_tail};
                    if (strcmp(section->get_name().c_str(), "Counters") == 0)
                        counter_option_path(option, &list_head, name);
                    else
                        option_path(option, &list_head);
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
                            fill_option_from_string_kvp(option, kvp);
                            break;
                        case KvpValue::Type::GUID:
                            fill_option_from_guid_kvp(option, kvp);
                          break;
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
    GncOption option{GncOptionQofInstanceValue{section, name, key, doc_string,
                                               (const QofInstance*)value,
                                               GncOptionUIType::BUDGET}};
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
    GncOption option{GncOptionCommodityValue{section, name, key, doc_string,
                                               value,
                                               GncOptionUIType::COMMODITY}};
    db->register_option(section, std::move(option));
}

void
gnc_register_commodity_option(GncOptionDB* db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string, const char* value)
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
            break;
    }
    GncOption option{GncOptionCommodityValue{section, name, key, doc_string,
                commodity,
                GncOptionUIType::COMMODITY}};
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
    GncOption option{GncOptionAccountListValue{section, name, key, doc_string,
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
        GncOption option{GncOptionAccountListValue{section, name, key, doc_string,
                    GncOptionUIType::ACCOUNT_LIST, value, std::move(allowed)}};
        db->register_option(section, std::move(option));
    }
    catch (const std::invalid_argument& err)
    {
        PWARN("Account List Limited Option, value failed validation, option not registered.");
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
        list.push_back(*qof_entity_get_guid(account));
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
                                        const Account* value,
                                        GncOptionAccountTypeList&& allowed)
{
    try
    {
        GncOption option{GncOptionAccountSelValue{section, name, key, doc_string,
                    GncOptionUIType::ACCOUNT_SEL, value, std::move(allowed)}};
    db->register_option(section, std::move(option));
    }
    catch (const std::invalid_argument& err)
    {
        PWARN("Account Sel Limited Option, value failed validation, option not registerd.");
    }
}

void
gnc_register_multichoice_option(GncOptionDB* db, const char* section,
                                const char* name, const char* key,
                                const char* doc_string, const char* default_val,
                                GncMultichoiceOptionChoices&& choices)
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

/* Only balance-forecast.scm, sample-report.scm, and net-charts.scm
 * use decimals and fractional steps and they can be worked around.
 */
template <typename ValueType> void
gnc_register_number_range_option(GncOptionDB* db, const char* section,
                                 const char* name, const char* key,
                                 const char* doc_string, ValueType value,
                                 ValueType min, ValueType max, ValueType step)
{
    try
    {
        GncOption option{GncOptionRangeValue<ValueType>{section, name, key,
                                                        doc_string, value, min,
                                                        max, step}};
        db->register_option(section, std::move(option));
    }
    catch(const std::invalid_argument& err)
    {
        PWARN("Number Range Option %s, option not registerd.",
              err.what());
    }
}

void
gnc_register_number_plot_size_option(GncOptionDB* db,
                                     const char* section, const char* name,
                                     const char* key, const char* doc_string,
                                     int value)
{
//65K is 10x reasonable, but it's a convenient constant.
    GncOption option{GncOptionRangeValue<int>{section, name, key, doc_string,
            value, 10, UINT16_MAX, 1, GncOptionUIType::PLOT_SIZE}};
    db->register_option(section, std::move(option));
}

void
gnc_register_query_option(GncOptionDB* db, const char* section,
                          const char* name, const QofQuery* value)
{
    GncOption option{section, name, "", "", value,
            GncOptionUIType::INTERNAL};
    db->register_option(section, std::move(option));
}

void
gnc_register_owner_option(GncOptionDB* db, const char* section,
                          const char* name, const char* key,
                          const char* doc_string, const GncOwner* value,
                          GncOwnerType type)
{
    GncOptionUIType uitype;
    switch (type)
    {
    case GNC_OWNER_CUSTOMER:
        uitype = GncOptionUIType::CUSTOMER;
        break;
    case GNC_OWNER_EMPLOYEE:
        uitype = GncOptionUIType::EMPLOYEE;
        break;
    case GNC_OWNER_JOB:
        uitype = GncOptionUIType::JOB;
        break;
    case GNC_OWNER_VENDOR:
        uitype = GncOptionUIType::VENDOR;
        break;
    default:
        uitype = GncOptionUIType::INTERNAL;
    };
    GncOption option{GncOptionGncOwnerValue{section, name, key, doc_string,
                                            value, uitype}};
    db->register_option(section, std::move(option));
}

void
gnc_register_invoice_option(GncOptionDB* db, const char* section,
                            const char* name, const char* key,
                            const char* doc_string, GncInvoice* value)
{
    GncOption option{GncOptionQofInstanceValue{section, name, key, doc_string,
                                               (const QofInstance*)value,
                                               GncOptionUIType::INVOICE}};
    db->register_option(section, std::move(option));
}

void
gnc_register_taxtable_option(GncOptionDB* db, const char* section,
                             const char* name, const char* key,
                             const char* doc_string, GncTaxTable* value)
{
    GncOption option{GncOptionQofInstanceValue{section, name, key, doc_string,
                                               (const QofInstance*)value,
                                               GncOptionUIType::TAX_TABLE}};
    db->register_option(section, std::move(option));
}

void
gnc_register_invoice_print_report_option(GncOptionDB* db, const char* section,
                                         const char* name, const char* key,
                                         const char* doc_string, std::string value)
{
    GncOption option{section, name, key, doc_string,
                     value, GncOptionUIType::INV_REPORT};
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
    GncOption option{GncOptionCommodityValue{
        section, name, key, doc_string, value, GncOptionUIType::CURRENCY
        }};
    db->register_option(section, std::move(option));
}

void
gnc_register_currency_option(GncOptionDB* db, const char* section,
                             const char* name, const char* key,
                             const char* doc_string, const char* value)
{
    const auto book{qof_session_get_book(gnc_get_current_session())};
    const auto commodity_table{gnc_commodity_table_get_table(book)};
    const auto commodity = gnc_commodity_table_lookup(commodity_table,
                                                      "CURRENCY",
                                                      value);
    GncOption option{GncOptionCommodityValue{
        section, name, key, doc_string, commodity, GncOptionUIType::CURRENCY
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
    auto is_absolute = period_set.size() == 1 &&
                       period_set.front() == RelativeDatePeriod::ABSOLUTE;
    auto ui_type = both ? GncOptionUIType::DATE_BOTH :
        is_absolute ? GncOptionUIType::DATE_ABSOLUTE : GncOptionUIType::DATE_RELATIVE;
    GncOption option{GncOptionDateValue(section, name, key, doc_string,
                                        ui_type, period_set)};
    if (is_absolute)
        option.set_default_value(gnc_time(nullptr));
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

void
gnc_register_report_placement_option(GncOptionDBPtr& db,
                                     const char* section, const char* name)
{
    /* This is a special option with it's own UI file so we have fake values to pass
     * to the template creation function.
     */
    GncOptionReportPlacementVec value;
    GncOption option{GncOptionValue<GncOptionReportPlacementVec>{section, name,
                                                              "no_key", "nodoc_string",
                                                              value,GncOptionUIType::REPORT_PLACEMENT}};
    db->register_option(section, std::move(option));
}

void
gnc_register_internal_option(GncOptionDBPtr& db,
                             const char* section, const char* name,
                             const std::string& value)
{
    GncOption option{
        GncOptionValue<std::string>{section, name, "", "", value,
                                    GncOptionUIType::INTERNAL}};
    db->register_option(section, std::move(option));
}

void
gnc_register_internal_option(GncOptionDBPtr& db,
                             const char* section, const char* name,
                             bool value)
{
    GncOption option{
        GncOptionValue<bool>{section, name, "", "", value,
                             GncOptionUIType::INTERNAL}};
    db->register_option(section, std::move(option));
}

GncOptionDB*
gnc_option_db_new(void)
{
    return new GncOptionDB;
}

void
gnc_option_db_destroy(GncOptionDB* odb)
{
    PWARN("Direct Destroy called on GncOptionDB %" G_GUINT64_FORMAT, (uint64_t)odb);
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
    if (!errors)
        odb->run_callbacks();
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
                                N_("Customer number"), "gncCustomera",
                                N_("The previous customer number generated. This number will be incremented to generate the next customer number."),
                                0.0);
    gnc_register_counter_format_option(odb, counter_section,
                                       N_("Customer number format"),
                                       "gncCustomerb",
                                       N_("The format string to use for generating customer numbers. This is a printf-style format string."),
                                       empty_string);
    gnc_register_counter_option(odb, counter_section,
                                N_("Employee number"), "gncEmployeea",
                                N_("The previous employee number generated. This number will be incremented to generate the next employee number."),
                                0.0);
    gnc_register_counter_format_option(odb, counter_section,
                                       N_("Employee number format"),
                                       "gncEmployeeb",
                                       N_("The format string to use for generating employee numbers. This is a printf-style format string."),
                                       empty_string);
    gnc_register_counter_option(odb, counter_section,
                                N_("Invoice number"), "gncInvoicea",
                                N_("The previous invoice number generated. This number will be incremented to generate the next invoice number."),
                                0.0);
    gnc_register_counter_format_option(odb, counter_section,
                                       N_("Invoice number format"),
                                       "gncInvoiceb",
                                       N_("The format string to use for generating invoice numbers. This is a printf-style format string."),
                                       empty_string);
    gnc_register_counter_option(odb, counter_section,
                                N_("Bill number"), "gncBilla",
                                N_("The previous bill number generated. This number will be incremented to generate the next bill number."),
                                0.0);
    gnc_register_counter_format_option(odb, counter_section,
                                       N_("Bill number format"), "gncBillb",
                                       N_("The format string to use for generating bill numbers. This is a printf-style format string."),
                                       empty_string);
    gnc_register_counter_option(odb, counter_section,
                                N_("Expense voucher number"), "gncExpVouchera",
                                N_("The previous expense voucher number generated. This number will be incremented to generate the next voucher number."),
                                0.0);
    gnc_register_counter_format_option(odb, counter_section,
                                       N_("Expense voucher number format"),
                                       "gncExpVoucherb",
                                       N_("The format string to use for generating expense voucher numbers. This is a printf-style format string."),
                                       empty_string);
    gnc_register_counter_option(odb, counter_section,
                                N_("Job number"), "gncJoba",
                                N_("The previous job number generated. This number will be incremented to generate the next job number."),
                                0.0);
    gnc_register_counter_format_option(odb, counter_section,
                                       N_("Job number format"), "gncJobb",
                                       N_("The format string to use for generating job numbers. This is a printf-style format string."),
                                       empty_string);
    gnc_register_counter_option(odb, counter_section,
                                N_("Order number"), "gncOrdera",
                                N_("The previous order number generated. This number will be incremented to generate the next order number."),
                                0.0);
    gnc_register_counter_format_option(odb, counter_section,
                                       N_("Order number format"), "gncOrderb",
                                       N_("The format string to use for generating order numbers. This is a printf-style format string."),
                                       empty_string);
    gnc_register_counter_option(odb, counter_section,
                                N_("Vendor number"), "gncVendora",
                                N_("The previous vendor number generated. This number will be incremented to generate the next vendor number."),
                                0.0);
    gnc_register_counter_format_option(odb, counter_section,
                                       N_("Vendor number format"), "gncVendorb",
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
    gnc_register_invoice_print_report_option(odb, business_section,
                                 OPTION_NAME_DEFAULT_INVOICE_REPORT, "e1",
                                 N_("The invoice report to be used for printing."),
                                 empty_string);
    gnc_register_number_range_option<double>(odb, business_section,
                                     OPTION_NAME_DEFAULT_INVOICE_REPORT_TIMEOUT, "e2",
                                     N_("Length of time to change the used invoice report. A value of 0 means disabled."),
                                     0.0, 0.0, 20.0, 1.0);
    gnc_register_taxtable_option(odb, business_section,
                                 N_("Default Customer TaxTable"), "f1",
                                 N_("The default tax table to apply to customers."),
                                 nullptr);
    gnc_register_taxtable_option(odb, business_section,
                                 N_("Default Vendor TaxTable"), "f2",
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
const QofInstance*
gnc_option_db_lookup_qofinstance_value(GncOptionDB* odb, const char* section,
                                       const char* name)
{
    auto option{odb->find_option(section, name)};
    if (option)
        return option->get_value<const QofInstance*>();
    else
        return nullptr;
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
