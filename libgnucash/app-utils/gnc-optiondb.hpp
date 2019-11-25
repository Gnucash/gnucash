/********************************************************************\
 * gnc-optiondb.hpp -- Collection of GncOption objects              *
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

#ifndef GNC_OPTIONDB_HPP_
#define GNC_OPTIONDB_HPP_

#include "gnc-option.hpp"
#include <functional>
#include <exception>
#include <optional>
#include <iostream>
extern "C"
{
#include <gncInvoice.h>
#include <gncOwner.h>
#include <gncTaxTable.h>
}

class GncOptionDB;

using GncOptionVec = std::vector<GncOption>;
using GncOptionSection = std::pair<std::string, GncOptionVec>;
class GncOptionDB
{
public:
    GncOptionDB();
    GncOptionDB(QofBook* book);
    ~GncOptionDB() = default;

    void save_to_book(QofBook* book, bool do_clear) const;
    int num_sections() const noexcept { return m_sections.size(); }
    bool get_changed() const noexcept { return m_dirty; }
    void register_option(const char* section, GncOption&& option);
    void unregister_option(const char* section, const char* name);
    void set_default_section(const char* section);
    const GncOptionSection* const get_default_section() const noexcept;
    void set_ui_item(const char* section, const char* name, GncOptionUIItem* ui_item);
    GncOptionUIItem* const get_ui_item(const char* section, const char* name);
    GncOptionUIType get_ui_type(const char* section, const char* name);
    void set_ui_from_option(const char* section, const char* name,
                            std::function<void(GncOption&)> func);
    void set_option_from_ui(const char* section, const char* name,
                            std::function<void(GncOption&)> func);
    std::string lookup_string_option(const char* section,
                                            const char* name);
    template <typename ValueType>
    bool set_option(const char* section, const char* name, ValueType value)
    {
        try
        {
            auto option{find_option(section, name)};
            if (!option)
                return false;
            option->get().set_value(value);
            return true;
        }
        catch(const std::invalid_argument& err)
        {
            printf("Set Failed: %s\n", err.what());
            return false;
        }
    }
//    void set_selectable(const char* section, const char* name);
    void make_internal(const char* section, const char* name);
    void commit();
    std::optional<std::reference_wrapper<GncOptionSection>> find_section(const char* section);
    std::optional<std::reference_wrapper<GncOption>> find_option(const char* section, const char* name) {
        return static_cast<const GncOptionDB&>(*this).find_option(section, name);
    }
    std::optional<std::reference_wrapper<GncOption>> find_option(const char* section, const char* name) const;
private:
    std::ostream& serialize_option_scheme(std::ostream& oss,
                                          const char* option_prolog,
                                          const char* section, const char* name) const noexcept;
    std::ostream& serialize_option_key_value(std::ostream& oss,
                                             const char* section,
                                             const char* name) const noexcept;
    void load_option_scheme(std::istream iss);
    void load_option_key_value(std::istream iss);
    std::optional<std::reference_wrapper<GncOptionSection>> m_default_section;
    std::vector<GncOptionSection> m_sections;
    bool m_dirty = false;

    std::function<GncOptionUIItem*()> m_get_ui_value;
    std::function<void(GncOptionUIItem*)> m_set_ui_value;
    static constexpr char const* const c_scheme_serialization_elements[]
    {
        "(let ((option (gnc:lookup-option ",
        "\n                                 ",
        ")))\n   ((lambda (o) (if o (gnc:option-set-value o",
        "))) option))\n\n"
        };

};

/**
 * Extract a list of accounts in the book having one of the GNCAccountTypes in
 * types.
 *
 * Note that in Scheme it's important to use this function and not to create a
 * list of accounts using gnc-get-descendants-sorted because the latter method
 * produces a SWIGTYPE for the accounts that's incompatible with the SWIGTYPE
 * used in this module.
 *
 * @param book The book whose accounts to search
 * @param types A std::vector of GNCAccountType containing the Account types to
 *             include in ther result
 * @return A std::vector<const Account*> of all accounts in the book having the
 *         Account types in the types parameter.
 */
GncOptionAccountList
gnc_account_list_from_types(QofBook *book,
                            const GncOptionAccountTypeList& types);


using GncOptionDBPtr = std::unique_ptr<GncOptionDB>;
/**
 * Create an empty option database.
 *
 * It would be nice to use a std::shared_ptr here but Swig doesn't implement
 * that for Guile.
 * @return A newly allocated GncOptionDB. Use delete to destroy it.
 */
GncOptionDBPtr gnc_option_db_new(void);

void gnc_register_string_option(const GncOptionDBPtr& db, const char* section,
                                const char* name, const char* key,
                                const char* doc_string, std::string value);

void gnc_register_text_option(const GncOptionDBPtr& db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string, std::string value);

void gnc_register_font_option(const GncOptionDBPtr& db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string, std::string value);

void gnc_register_budget_option(const GncOptionDBPtr& db, const char* section,
                                const char* name, const char* key,
                                const char* doc_string, GncBudget* value);

void gnc_register_commodity_option(const GncOptionDBPtr& db,
                                   const char* section, const char* name,
                                   const char* key, const char* doc_string,
                                   gnc_commodity* value);

void gnc_register_simple_boolean_option(const GncOptionDBPtr& db,
                                        const char* section, const char* name,
                                        const char* key, const char* doc_string,
                                        bool value);

void gnc_register_complex_boolean_option(const GncOptionDBPtr& db,
                                         const char* section, const char* name,
                                         const char* key,
                                         const char* doc_string,
                                         bool value);

void gnc_register_pixmap_option(const GncOptionDBPtr& db, const char* section,
                                const char* name, const char* key,
                                const char* doc_string, std::string value);

void gnc_register_account_list_limited_option(const GncOptionDBPtr& db,
                                             const char* section,
                                             const char* name, const char* key,
                                             const char* doc_string,
                                             const GncOptionAccountList& value,
                                             GncOptionAccountTypeList&& allowed);

void gnc_register_account_list_option(const GncOptionDBPtr& db,
                                      const char* section,
                                      const char* name, const char* key,
                                      const char* doc_string,
                                      const GncOptionAccountList& value);

void gnc_register_account_sel_limited_option(const GncOptionDBPtr& db,
                                             const char* section,
                                             const char* name, const char* key,
                                             const char* doc_string,
                                             const GncOptionAccountList& value,
                                             GncOptionAccountTypeList&& allowed);

void gnc_register_multichoice_option(const GncOptionDBPtr& db,
                                     const char* section, const char* name,
                                     const char* key, const char* doc_string,
                                     GncMultiChoiceOptionChoices&& value);

void gnc_register_list_option(const GncOptionDBPtr& db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string, const char* value,
                              GncMultiChoiceOptionChoices&& list);

void gnc_register_number_range_option(const GncOptionDBPtr& db,
                                      const char* section, const char* name,
                                      const char* key, const char* doc_string,
                                      int value, int min, int max, int step);

void gnc_register_number_plot_size_option(const GncOptionDBPtr& db,
                                          const char* section, const char* name,
                                          const char* key,
                                          const char* doc_string,
                                          int value);

void gnc_register_query_option(const GncOptionDBPtr& db, const char* section,
                               const char* name, const char* key,
                               const char* doc_string, QofQuery* value);

void gnc_register_color_option(const GncOptionDBPtr& db, const char* section,
                               const char* name, const char* key,
                               const char* doc_string, std::string value);

void gnc_register_internal_option(const GncOptionDBPtr& db, const char* section,
                                  const char* name, const char* key,
                                  const char* doc_string, std::string value);


void gnc_register_currency_option(const GncOptionDBPtr& db, const char* section,
                                  const char* name, const char* key,
                                  const char* doc_string, gnc_commodity* value);

void gnc_register_invoice_option(const GncOptionDBPtr& db, const char* section,
                                 const char* name, const char* key,
                                 const char* doc_string, GncInvoice* value);

void gnc_register_owner_option(const GncOptionDBPtr& db, const char* section,
                               const char* name, const char* key,
                               const char* doc_string, GncOwner* value);

void gnc_register_taxtable_option(const GncOptionDBPtr& db, const char* section,
                                  const char* name, const char* key,
                                  const char* doc_string, GncTaxTable* value);

void gnc_register_counter_option(const GncOptionDBPtr& db, const char* section,
                                 const char* name, const char* key,
                                 const char* doc_string, int value);

void gnc_register_counter_format_option(const GncOptionDBPtr& db,
                                        const char* section, const char* name,
                                        const char* key, const char* doc_string,
                                        std::string value);

void gnc_register_dateformat_option(const GncOptionDBPtr& db,
                                    const char* section, const char* name,
                                    const char* key, const char* doc_string,
                                    std::string value);

void gnc_register_date_interval_option(const GncOptionDBPtr& db,
                                       const char* section, const char* name,
                                       const char* key, const char* doc_string,
                                       RelativeDatePeriod period);
#endif //GNC_OPTIONDB_HPP_
