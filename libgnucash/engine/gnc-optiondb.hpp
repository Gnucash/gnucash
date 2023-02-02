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
/** @addtogroup Engine
    @{ */
/** @addtogroup Options
    @{ */
/** @file gnc-optiondb.hpp
 *  @brief The primary C++ interface to options for books, reports, and
 *  stylesheets.
 *  @author Copyright 2019-2021 John Ralls <jralls@ceridwen.us>
*/

#ifndef GNC_OPTIONDB_HPP_
#define GNC_OPTIONDB_HPP_

#include <string>
#include <functional>
#include <exception>
#include <optional>
#include <iostream>

#include <config.h>
#include "Account.h"
#include "gnc-budget.h"
#include "gnc-commodity.h"
#include "gncInvoice.h"
#include "gncTaxTable.h"
#include "gnc-option.hpp"
#include "gnc-datetime.hpp"


class GncOptionDB;
using GncOptionDBPtr = std::unique_ptr<GncOptionDB>;
using GncOptionAccountList = std::vector<GncGUID>;

using GncOptionAccountTypeList = std::vector<GNCAccountType>;
using GncMultichoiceOptionEntry = std::tuple<const std::string,
                                             const std::string,
                                             GncOptionMultichoiceKeyType>;
using GncMultichoiceOptionChoices = std::vector<GncMultichoiceOptionEntry>;

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

/**
 * Create a new string option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option.
 */
void gnc_register_string_option(GncOptionDB* db, const char* section,
                                const char* name, const char* key,
                                const char* doc_string, std::string value);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_string_option(const GncOptionDBPtr& db,
                                       const char* section, const char* name,
                                       const char* key, const char* doc_string,
                                       std::string value)
{
    gnc_register_string_option(db.get(), section, name, key,
                               doc_string, value);
}

/**
 * Create a new text option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option.
 */
void gnc_register_text_option(GncOptionDB* db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string, std::string value);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_text_option(const GncOptionDBPtr& db,
                                     const char* section,
                                     const char* name, const char* key,
                                     const char* doc_string, std::string value)
{
    gnc_register_text_option(db.get(), section, name, key, doc_string, value);
}

/**
 * Create a new font option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option.
 */
void gnc_register_font_option(GncOptionDB* db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string, std::string value);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_font_option(const GncOptionDBPtr& db,
                                     const char* section, const char* name,
                                     const char* key, const char* doc_string,
                                     std::string value)
{
    gnc_register_font_option(db.get(), section, name, key, doc_string, value);
}

/**
 * Create a new budget option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option.
 */
void gnc_register_budget_option(GncOptionDB* db, const char* section,
                                const char* name, const char* key,
                                const char* doc_string, GncBudget* value);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_budget_option(const GncOptionDBPtr& db,
                                       const char* section, const char* name,
                                       const char* key, const char* doc_string,
                                       GncBudget* value)
{
    gnc_register_budget_option(db.get(), section, name, key, doc_string, value);
}

/**
 * Create a new commodity option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option.
 */
void gnc_register_commodity_option(GncOptionDB* db,
                                   const char* section, const char* name,
                                   const char* key, const char* doc_string,
                                   gnc_commodity* value);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_commodity_option(const GncOptionDBPtr& db,
                                          const char* section,
                                          const char* name, const char* key,
                                          const char* doc_string,
                                          gnc_commodity* value)
{
    gnc_register_commodity_option(db.get(), section, name, key,
                                  doc_string, value);
}
/**
 * As above but with a const char* value, which should be the symbol
 * for the commodity. All security editor namespaces will be searched
 * to retrieve it.
 */
void gnc_register_commodity_option(GncOptionDB* db,
                                   const char* section, const char* name,
                                   const char* key, const char* doc_string,
                                   const char* value);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_commodity_option(const GncOptionDBPtr& db,
                                          const char* section,
                                          const char* name, const char* key,
                                          const char* doc_string,
                                          const char* value)
{
    gnc_register_commodity_option(db.get(), section, name, key,
                                  doc_string, value);
}

/**
 * Create a new simple boolean option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option.
 */
void gnc_register_simple_boolean_option(GncOptionDB* db,
                                        const char* section, const char* name,
                                        const char* key, const char* doc_string,
                                        bool value);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_simple_boolean_option(const GncOptionDBPtr& db,
                                               const char* section,
                                               const char* name,
                                               const char* key,
                                               const char* doc_string,
                                               bool value)
{
    gnc_register_simple_boolean_option(db.get(), section, name, key,
                                       doc_string, value);
}

/**
 * Create a new pixmap option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option.
 */
void gnc_register_pixmap_option(GncOptionDB* db, const char* section,
                                const char* name, const char* key,
                                const char* doc_string, std::string value);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_pixmap_option(GncOptionDBPtr& db, const char* section,
                                       const char* name, const char* key,
                                       const char* doc_string,
                                       std::string value)
{
    gnc_register_pixmap_option(db.get(), section, name, key, doc_string, value);
}

/**
 * Create a new limited account list option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default values for the option.
 * @param allowed The accounts which are available for selection.
*/
void gnc_register_account_list_limited_option(GncOptionDB* db,
                                             const char* section,
                                             const char* name, const char* key,
                                             const char* doc_string,
                                             const GncOptionAccountList& value,
                                             GncOptionAccountTypeList&& allowed);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_account_list_limited_option(GncOptionDBPtr& db,
                                             const char* section,
                                             const char* name, const char* key,
                                             const char* doc_string,
                                             const GncOptionAccountList& value,
                                             GncOptionAccountTypeList&& allowed)
{
    gnc_register_account_list_limited_option(db.get(), section, name, key,
                                             doc_string, value,
                                             std::move(allowed));
}

/**
 * Create a new account list option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default values for the option.
 */
void gnc_register_account_list_option(GncOptionDB* db,
                                      const char* section,
                                      const char* name, const char* key,
                                      const char* doc_string,
                                      const GncOptionAccountList& value);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_account_list_option(GncOptionDBPtr& db,
                                      const char* section,
                                      const char* name, const char* key,
                                      const char* doc_string,
                                      const GncOptionAccountList& value)
{
    gnc_register_account_list_option(db.get(), section, name, key,
                                     doc_string, value);
}

/**
 * Create a limited account selection option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option.
 * @param allowed The accounts which are available for selection.
 */
void gnc_register_account_sel_limited_option(GncOptionDB* db,
                                             const char* section,
                                             const char* name, const char* key,
                                             const char* doc_string,
                                             const Account* value,
                                             GncOptionAccountTypeList&& allowed);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_account_sel_limited_option(GncOptionDBPtr& db,
                                             const char* section,
                                             const char* name, const char* key,
                                             const char* doc_string,
                                             const Account* value,
                                             GncOptionAccountTypeList&& allowed)
{
    gnc_register_account_sel_limited_option(db.get(), section, name, key,
                                            doc_string, value,
                                            std::move(allowed));
}

/**
 * Create a new multichoice option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The set of possible values for the option. Only one can be selected. Note that the value will be moved from the parameter and using the parameter after this call will result in undefined behavior.
 */
void gnc_register_multichoice_option(GncOptionDB* db,
                                     const char* section, const char* name,
                                     const char* key, const char* doc_string,
                                     const char* default_val,
                                     GncMultichoiceOptionChoices&& value);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_multichoice_option(GncOptionDBPtr& db,
                                        const char* section, const char* name,
                                        const char* key, const char* doc_string,
                                        const char* default_val,
                                        GncMultichoiceOptionChoices&& value)
{
    gnc_register_multichoice_option(db.get(), section, name,
                                    key, doc_string, default_val,
                                    std::move(value));
}

/**
 * Create a new list option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option.
 * @param list The values available for selection. Note that this parameter will be moved from so using it after this call will result in undefined behavior.
 */
void gnc_register_list_option(GncOptionDB* db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string, const char* value,
                              GncMultichoiceOptionChoices&& list);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_list_option(GncOptionDBPtr& db, const char* section,
                                     const char* name, const char* key,
                                     const char* doc_string, const char* value,
                                     GncMultichoiceOptionChoices&& list)
{
    gnc_register_list_option(db.get(), section, name, key, doc_string,
                             value, std::move(list));
}

/**
 * Create a new number range option and register it in the options database.
 *
 * These are normally associated with spin controls and ValueType is normally
 * double, but it's templated to permit other numeric types if needed.
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option.
 * @param min The minimum value for the spin control.
 * @param max The maximum value for the spin control.
 * @param step The step size (increment) of the spin control.
 */
template <typename ValueType>
void gnc_register_number_range_option(GncOptionDB* db,
                                      const char* section, const char* name,
                                      const char* key, const char* doc_string,
                                      ValueType value, ValueType min,
                                      ValueType max, ValueType step);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
template <typename ValueType>
void gnc_register_number_range_option(GncOptionDBPtr& db,
                                      const char* section, const char* name,
                                      const char* key, const char* doc_string,
                                      ValueType value, ValueType min,
                                      ValueType max, ValueType step)
{
    gnc_register_number_range_option<ValueType>(db.get(), section, name,
                                                key, doc_string, value,
                                                min, max, step);
}

/**
 * Create a new plot-size option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option.
 */
void gnc_register_number_plot_size_option(GncOptionDB* db,
                                          const char* section, const char* name,
                                          const char* key,
                                          const char* doc_string,
                                          int value);

inline void gnc_register_number_plot_size_option(GncOptionDB* db,
                                          const char* section, const char* name,
                                          const char* key,
                                          const char* doc_string,
                                          float value)
{
    gnc_register_number_plot_size_option(db, section, name, key,
                                         doc_string, static_cast<int>(value));
}


/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_number_plot_size_option(const GncOptionDBPtr& db,
                                                 const char* section,
                                                 const char* name,
                                                 const char* key,
                                                 const char* doc_string,
                                                 int value)
{
    gnc_register_number_plot_size_option(db.get(), section, name, key,
                                         doc_string, value);
}

inline void gnc_register_number_plot_size_option(const GncOptionDBPtr& db,
                                                 const char* section,
                                                 const char* name,
                                                 const char* key,
                                                 const char* doc_string,
                                                 float value)
{
    gnc_register_number_plot_size_option(db.get(), section, name, key,
                                         doc_string, static_cast<int>(value));
}

/**
 * Create a new QofQuery option and register it in the options database.
 *
 * Query options have no UI component so they don't get a key or a docstring.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param value The initial and default value for the option.
 */
void gnc_register_query_option(GncOptionDB* db, const char* section,
                               const char* name, const QofQuery* value);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_query_option(GncOptionDBPtr& db, const char* section,
                                      const char* name,
                                      const QofQuery* value)
{
    gnc_register_query_option(db.get(), section, name, value);
}

/**
 * Create a new GncOwner option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option.
 * @param type The type of owner, determines the UI type.
 */
void gnc_register_owner_option(GncOptionDB* db, const char* section,
                               const char* name, const char* key,
                               const char* doc_string, const GncOwner* value,
                               GncOwnerType type);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_owner_option(GncOptionDBPtr& db, const char* section,
                                      const char* name, const char* key,
                                      const char* doc_string,
                                      const GncOwner* value,
                                      GncOwnerType type)
{
    gnc_register_owner_option(db.get(), section, name, key, doc_string, value, type);
}

/**
 * Create a new color option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option.
 */
void gnc_register_color_option(GncOptionDB* db, const char* section,
                               const char* name, const char* key,
                               const char* doc_string, std::string value);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_color_option(GncOptionDBPtr& db, const char* section,
                                      const char* name, const char* key,
                                      const char* doc_string,
                                      std::string value)
{
    gnc_register_color_option(db.get(), section, name, key, doc_string, value);
}

void gnc_register_internal_option(GncOptionDBPtr& db,
                                  const char* section, const char* name,
                                  const std::string& value);

void gnc_register_internal_option(GncOptionDBPtr& db,
                                  const char* section, const char* name,
                                  bool value);

void gnc_register_report_placement_option(GncOptionDBPtr& db,
                                          const char* section, const char* name);

/**
 * Create a new currency option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option. It is checked with gnc_commodity_is_currency.
 */
void gnc_register_currency_option(GncOptionDB* db, const char* section,
                                  const char* name, const char* key,
                                  const char* doc_string, gnc_commodity* value);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_currency_option(const GncOptionDBPtr& db,
                                          const char* section,
                                          const char* name, const char* key,
                                          const char* doc_string,
                                          gnc_commodity* value)
{
    gnc_register_currency_option(db.get(), section, name, key,
                                 doc_string, value);
}

/**
 * As above but with a const char* value, which must be the ISO4217 three-letter
 * symbol for the currency.
 */
void gnc_register_currency_option(GncOptionDB* db,
                                  const char* section,
                                  const char* name, const char* key,
                                  const char* doc_string,
                                  const char* value);

inline void gnc_register_currency_option(const GncOptionDBPtr& db,
                                          const char* section,
                                          const char* name, const char* key,
                                          const char* doc_string,
                                          const char* value)
{
    gnc_register_currency_option(db.get(), section, name, key,
                                 doc_string, value);
}

/**
 * Create a new invoice option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option.
 */
void gnc_register_invoice_option(GncOptionDB* db, const char* section,
                                 const char* name, const char* key,
                                 const char* doc_string, GncInvoice* value);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_invoice_option(const GncOptionDBPtr& db,
                                        const char* section,
                                        const char* name, const char* key,
                                        const char* doc_string,
                                        GncInvoice* value)
{
    gnc_register_invoice_option(db.get(), section, name, key,
                                doc_string, value);
}

/**
 * Create a new taxtable option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option.
 */
void gnc_register_taxtable_option(GncOptionDB* db, const char* section,
                                  const char* name, const char* key,
                                  const char* doc_string, GncTaxTable* value);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_taxtable_option(const GncOptionDBPtr& db,
                                         const char* section, const char* name,
                                         const char* key,
                                         const char* doc_string,
                                         GncTaxTable* value)
{
    gnc_register_taxtable_option(db.get(), section, name, key,
                                 doc_string, value);
}

/**
 * Create a new print report option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option.
 */
void gnc_register_invoice_print_report_option(GncOptionDB* db, const char* section,
                                              const char* name, const char* key,
                                              const char* doc_string, std::string value);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_invoice_print_report_option(const GncOptionDBPtr& db,
                                                     const char* section, const char* name,
                                                     const char* key, const char* doc_string,
                                                     std::string value)
{
    gnc_register_invoice_print_report_option(db.get(), section, name, key,
                                             doc_string, value);
}

/**
 * Create a new counter option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option.
 */
void gnc_register_counter_option(GncOptionDB* db, const char* section,
                                 const char* name, const char* key,
                                 const char* doc_string, double value);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_counter_option(const GncOptionDBPtr& db,
                                        const char* section, const char* name,
                                        const char* key, const char* doc_string,
                                        double value)
{
    gnc_register_counter_option(db.get(), section, name, key,
                                  doc_string, value);
}

/**
 * Create a new counter format option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option.
 */
void gnc_register_counter_format_option(GncOptionDB* db,
                                        const char* section, const char* name,
                                        const char* key, const char* doc_string,
                                        std::string value);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_counter_format_option(GncOptionDBPtr& db,
                                               const char* section,
                                               const char* name,
                                               const char* key,
                                               const char* doc_string,
                                               std::string value)
{
    gnc_register_counter_format_option(db.get(), section, name, key,
                                       doc_string, value);
}

/**
 * Create a new date format option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param value The initial and default value for the option.
 */
void gnc_register_dateformat_option(GncOptionDB* db,
                                    const char* section, const char* name,
                                    const char* key, const char* doc_string,
                                    std::string value);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_dateformat_option(GncOptionDBPtr& db,
                                           const char* section,
                                           const char* name, const char* key,
                                           const char* doc_string,
                                           std::string value)
{
    gnc_register_dateformat_option(db.get(), section, name, key,
                                   doc_string, value);
}

enum RelativeDateUI : uint8_t
{
    ABSOLUTE,
    RELATIVE,
    BOTH
};

/**
 * Create a new date option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param period The default/starting relative date value for the option.
 * @param ui What UI to display, relative, absolute, or both.
*/
void gnc_register_date_option(GncOptionDB* db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string,
                              RelativeDatePeriod period =
                              RelativeDatePeriod::TODAY,
                              RelativeDateUI ui = RelativeDateUI::BOTH);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_date_option(GncOptionDBPtr& db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string,
                              RelativeDatePeriod period =
                              RelativeDatePeriod::TODAY,
                              RelativeDateUI ui = RelativeDateUI::BOTH)
{
    gnc_register_date_option(db.get(), section, name, key, doc_string,
                             period, ui);
}

/**
 * Create a new date option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param time The initial time to set in the option.
 * @param ui What UI to display, relative, absolute, or both.
 */
void gnc_register_date_option(GncOptionDB* db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string, time64 time,
                              RelativeDateUI ui = RelativeDateUI::BOTH);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_date_option(GncOptionDBPtr& db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string, time64 time,
                              RelativeDateUI ui = RelativeDateUI::BOTH)
{
    gnc_register_date_option(db.get(), section, name, key, doc_string,
                             time, ui);
}

/**
 * Create a new date option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param period_set A vector of relative date periods to display in the relative control.
 * @param both Whether to display both a relative and absolute control or a onla a relative control.
*/
void gnc_register_date_option(GncOptionDB* db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string,
                              RelativeDatePeriodVec& period_set,
                              bool both = true);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_date_option(GncOptionDBPtr& db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string,
                              RelativeDatePeriodVec& period_set,
                              bool both = true)
{
    gnc_register_date_option(db.get(), section, name, key, doc_string,
                             period_set, both);
}

/**
 * Create a new start-date option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param both Whether to display both a relative and absolute control or a onla a relative control.
 */
void gnc_register_start_date_option(GncOptionDB* db,
                                    const char* section,
                                    const char* name, const char* key,
                                    const char* doc_string, bool both = true);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_start_date_option(GncOptionDBPtr& db,
                                    const char* section,
                                    const char* name, const char* key,
                                    const char* doc_string, bool both = true)
{
    gnc_register_start_date_option(db.get(), section, name, key, doc_string,
                                   both);
}

/**
 * Create a new end-date option and register it in the options database.
 *
 * @param db A GncOptionDB* for calling from C. Caller retains ownership.
 * @param section The database section for the option.
 * @param name The option name.
 * @param key A short tag used to sort the controls in the dialog.
 * @param doc_string A description of the option. This will be used in tooltips and should be marked for translation.
 * @param both Whether to display both a relative and absolute control or a onla a relative control.
 */
void gnc_register_end_date_option(GncOptionDB* db, const char* section,
                                  const char* name, const char* key,
                                  const char* doc_string, bool both = true);

/**
 * As above but takes a const GncOptionDBPtr& (const std::unique_ptr<GncOptionDB>&) for calling from C++.
 */
inline void gnc_register_end_date_option(GncOptionDBPtr& db, const char* section,
                                  const char* name, const char* key,
                                  const char* doc_string, bool both = true)
{
    gnc_register_end_date_option(db.get(), section, name, key, doc_string,
                                 both);
}



#endif //GNC_OPTIONDB_HPP_
