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

GncOptionDB::GncOptionDB() : m_default_section{boost::none} {}

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
        db_section->second.emplace_back(std::move(option));
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
        db_section->second.erase(
            std::remove_if(
                db_section->second.begin(), db_section->second.end(),
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
        return &(m_default_section.get());
    return nullptr;
}

void
GncOptionDB::set_ui_item(const char* section, const char* name, void* ui_item)
{
    auto option = find_option(section, name);
    if (!option) return;
    option->set_ui_item(ui_item);
}

void* const
GncOptionDB::get_ui_item(const char* section, const char* name)
{
    auto option = find_option(section, name);
    if (!option) return nullptr;
    return option->get_ui_item();
}

GncOptionUIType
GncOptionDB::get_ui_type(const char* section, const char* name)
{
    auto option = find_option(section, name);
    if (!option) return GncOptionUIType::INTERNAL;
    return option->get_ui_type();
}

void
GncOptionDB::set_ui_from_option(const char* section, const char* name,
                        std::function<void(GncOption&)> func)
{
    auto option = find_option(section, name);
    if (!option) return;
    func(option.get());
}

void
GncOptionDB::set_option_from_ui(const char* section, const char* name,
                        std::function<void(GncOption&)> func)
{
    auto option = find_option(section, name);
    if (!option) return;
    func(option.get());
}


boost::optional<GncOptionSection&>
GncOptionDB::find_section(const char* section)
{
    auto db_section = std::find_if(
        m_sections.begin(), m_sections.end(),
        [section](GncOptionSection sect) -> bool
        {
            return sect.first == std::string{section};
        });
    if (db_section == m_sections.end())
        return boost::none;
    return *db_section;
}

boost::optional<GncOption&>
GncOptionDB::find_option(const char* section, const char* name)
{
    auto db_section = find_section(section);
    if (!db_section)
        return boost::none;
    auto db_opt = std::find_if(
        db_section->second.begin(), db_section->second.end(),
        [name](GncOption& option) -> bool
        {
            return option.get_name() == std::string{name};
        });
    if (db_opt == db_section->second.end())
        return boost::none;
    return *db_opt;
}

SCM
GncOptionDB::lookup_option(const char* section, const char* name)
{
    auto db_opt = find_option(section, name);
    if (!db_opt)
        return SCM_BOOL_F;
    return db_opt->get_scm_value();
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

bool
GncOptionDB::set_option(const char* section, const char* name, SCM value)
{
    return false;
}

void
GncOptionDB::set_selectable(const char* section, const char* name)
{
}

void
GncOptionDB::make_internal(const char* section, const char* name)
{
    auto db_opt = find_option(section, name);
    if (db_opt)
        db_opt->make_internal();
}

void
GncOptionDB::commit()
{
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
            GncOptionUIType::FONT};
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
gnc_register_account_liat_option(const GncOptionDBPtr& db, const char* section,
                                 const char* name, const char* key,
                                 const char* doc_string,
                                 std::vector<GncGUID> value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::ACCOUNT_LIST};
    db->register_option(section, std::move(option));
}

void
gnc_register_acount_list_limited_option(const GncOptionDBPtr& db,
                                        const char* section, const char* name,
                                        const char* key, const char* doc_string,
                                        std::vector<GncGUID> value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::ACCOUNT_LIST};
    db->register_option(section, std::move(option));
}

void
gnc_register_account_sel_limited_option(const GncOptionDBPtr& db,
                                        const char* section, const char* name,
                                        const char* key, const char* doc_string,
                                        std::vector<GncGUID> value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::ACCOUNT_SEL};
    db->register_option(section, std::move(option));
}

void
gnc_register_multichoice_option(const GncOptionDBPtr& db, const char* section,
                                const char* name, const char* key,
                                const char* doc_string,
                                GncMultiChoiceOptionChoices&& value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::MULTICHOICE};
    db->register_option(section, std::move(option));
}

void
gnc_register_list_option(const GncOptionDBPtr& db, const char* section,
                         const char* name, const char* key,
                         const char* doc_string,
                         GncMultiChoiceOptionChoices&& value)
{
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::LIST};
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
    GncOption option{section, name, key, doc_string, value,
            GncOptionUIType::QUERY};
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
