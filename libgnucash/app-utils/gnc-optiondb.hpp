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
#include <boost/optional.hpp>
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
    SCM lookup_option(const char* section, const char* name);
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
            option->set_value<ValueType>(value);
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
private:
    boost::optional<GncOptionSection&> find_section(const char* section);
    boost::optional<GncOption&> find_option(const char* section, const char* name);

    boost::optional<GncOptionSection&> m_default_section;
    std::vector<GncOptionSection> m_sections;
    bool m_dirty = false;

    std::function<GncOptionUIItem*()> m_get_ui_value;
    std::function<void(GncOptionUIItem*)> m_set_ui_value;
};

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

/* Complex boolean options are the same as simple boolean options with the
 * addition of two function arguments. (If both of them are #f, you have exactly
 * a simple-boolean-option.) Both functions should expect one boolean argument.
 * When the option's value is changed, the function option-widget-changed-cb
 * will be called with the new option value at the time that the GUI widget
 * representing the option is changed, and the function
 * setter-function-called-cb will be called when the option's setter is called
 * (that is, when the user selects "OK" or "Apply").

 * The option-widget-changed-cb is tested for procedurehood before it is called,
 * so it is not validated to be a procedure here. However, since there could be
 * an option-widget-changed-cb but not a setter-function-called-cb, the
 * procedurehood of the setter-function-called-cb is checked here.
 */
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

/* account-list options use the option-data as a pair; the car is a boolean
 * value, the cdr is a list of account-types. If the boolean is true, the gui
 * should allow the user to select multiple accounts. If the cdr is an empty
 * list, then all account types are shown. Internally, values are always a list
 * of guids. Externally, both guids and account pointers may be used to set the
 * value of the option. The option always returns a list of account pointers.
 */
void gnc_register_acount_list_limited_option(const GncOptionDBPtr& db,
                                             const char* section,
                                             const char* name, const char* key,
                                             const char* doc_string,
                                             std::vector<GncGUID> value);

/* Just like gnc:make-account-list-limited-option except it does not limit the
 * types of accounts that are available to the user.
 */
void gnc_register_account_liat_option(const GncOptionDBPtr& db,
                                      const char* section,
                                      const char* name, const char* key,
                                      const char* doc_string,
                                      std::vector<GncGUID> value);

/* account-sel options use the option-data as a pair; the car is ignored, the
 * cdr is a list of account-types. If the cdr is an empty list, then all account
 * types are shown.  Internally, the value is always a guid.  Externally, both
 * guids and account pointers may be used to set the value of the option. The
 * option always returns the "current" account pointer.
 */
void gnc_register_account_sel_limited_option(const GncOptionDBPtr& db,
                                             const char* section,
                                             const char* name, const char* key,
                                             const char* doc_string,
                                             std::vector<GncGUID> value);

/* Multichoice options use the option-data as a list of vectors. Each vector
 * contains a permissible value (scheme symbol), a name, and a description
 * string.
 *
 * The multichoice-option with callback function is the same as the usual
 * multichoice options (see above), with the addition of two function
 * arguments. (If both of them are #f, you have exactly a multichoice-option.)
 * Both functions should expect one argument. When the option's value is
 * changed, the function option-widget-changed-cb will be called with the new
 * option value at the time that the GUI widget representing the option is
 * changed, and the function setter-function-called-cb will be called when the
 * option's setter is called (that is, when the user selects "OK" or "Apply").
 */

void gnc_register_multichoice_option(const GncOptionDBPtr& db,
                                     const char* section, const char* name,
                                     const char* key, const char* doc_string,
                                     GncMultiChoiceOptionChoices&& value);

/* List options use the option-data in the same way as multichoice options. List
 * options allow the user to select more than one option.
 */
void gnc_register_list_option(const GncOptionDBPtr& db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string,
                              GncMultiChoiceOptionChoices&& value);

/* Number range options use the option-data as a list whose elements are:
 * (lower-bound upper-bound step-size). 
*/
void gnc_register_number_range_option(const GncOptionDBPtr& db,
                                      const char* section, const char* name,
                                      const char* key, const char* doc_string,
                                      int value, int min, int max, int step);

/* Number plot size options are a convenience wrapper on number range options
 * with fixed min, max, and step.
*/
void gnc_register_number_plot_size_option(const GncOptionDBPtr& db,
                                          const char* section, const char* name,
                                          const char* key,
                                          const char* doc_string,
                                          int value);

void gnc_register_query_option(const GncOptionDBPtr& db, const char* section,
                               const char* name, const char* key,
                               const char* doc_string, QofQuery* value);

/* Color options store rgba values in a list. The option-data is a list, whose
 * first element is the range of possible rgba values and whose second element
 * is a boolean indicating whether to use alpha transparency.
 */
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


#endif //GNC_OPTIONDB_HPP_
