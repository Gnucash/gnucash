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

#include <string>
#include <functional>
#include <exception>
#include <optional>
#include <iostream>
extern "C"
{
#include <config.h>
#include <Account.h>
#include <gnc-budget.h>
#include <gnc-commodity.h>
#include <gncInvoice.h>
#include <gncOwner.h>
#include <gncTaxTable.h>
}
#include "gnc-option.hpp"
#include <gnc-datetime.hpp>


class GncOptionDB;
using GncOptionAccountList = std::vector<const Account*>;

using GncOptionAccountTypeList = std::vector<GNCAccountType>;
using GncMultichoiceOptionEntry = std::tuple<const std::string,
                                             const std::string,
                                             const std::string>;
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


void gnc_register_string_option(GncOptionDB* db, const char* section,
                                const char* name, const char* key,
                                const char* doc_string, std::string value);

void gnc_register_text_option(GncOptionDB* db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string, std::string value);

void gnc_register_font_option(GncOptionDB* db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string, std::string value);

void gnc_register_budget_option(GncOptionDB* db, const char* section,
                                const char* name, const char* key,
                                const char* doc_string, GncBudget* value);

void gnc_register_commodity_option(GncOptionDB* db,
                                   const char* section, const char* name,
                                   const char* key, const char* doc_string,
                                   gnc_commodity* value);

void gnc_register_simple_boolean_option(GncOptionDB* db,
                                        const char* section, const char* name,
                                        const char* key, const char* doc_string,
                                        bool value);

void gnc_register_complex_boolean_option(GncOptionDB* db,
                                         const char* section, const char* name,
                                         const char* key,
                                         const char* doc_string,
                                         bool value);

void gnc_register_pixmap_option(GncOptionDB* db, const char* section,
                                const char* name, const char* key,
                                const char* doc_string, std::string value);

void gnc_register_account_list_limited_option(GncOptionDB* db,
                                             const char* section,
                                             const char* name, const char* key,
                                             const char* doc_string,
                                             const GncOptionAccountList& value,
                                             GncOptionAccountTypeList&& allowed);

void gnc_register_account_list_option(GncOptionDB* db,
                                      const char* section,
                                      const char* name, const char* key,
                                      const char* doc_string,
                                      const GncOptionAccountList& value);

void gnc_register_account_sel_limited_option(GncOptionDB* db,
                                             const char* section,
                                             const char* name, const char* key,
                                             const char* doc_string,
                                             const GncOptionAccountList& value,
                                             GncOptionAccountTypeList&& allowed);

void gnc_register_multichoice_option(GncOptionDB* db,
                                     const char* section, const char* name,
                                     const char* key, const char* doc_string,
                                     GncMultichoiceOptionChoices&& value);

void gnc_register_list_option(GncOptionDB* db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string, const char* value,
                              GncMultichoiceOptionChoices&& list);

void gnc_register_number_range_option(GncOptionDB* db,
                                      const char* section, const char* name,
                                      const char* key, const char* doc_string,
                                      int value, int min, int max, int step);

void gnc_register_number_plot_size_option(GncOptionDB* db,
                                          const char* section, const char* name,
                                          const char* key,
                                          const char* doc_string,
                                          int value);

void gnc_register_query_option(GncOptionDB* db, const char* section,
                               const char* name, const char* key,
                               const char* doc_string, QofQuery* value);

void gnc_register_color_option(GncOptionDB* db, const char* section,
                               const char* name, const char* key,
                               const char* doc_string, std::string value);

void gnc_register_internal_option(GncOptionDB* db, const char* section,
                                  const char* name, const char* key,
                                  const char* doc_string, std::string value);


void gnc_register_currency_option(GncOptionDB* db, const char* section,
                                  const char* name, const char* key,
                                  const char* doc_string, gnc_commodity* value);

void gnc_register_invoice_option(GncOptionDB* db, const char* section,
                                 const char* name, const char* key,
                                 const char* doc_string, GncInvoice* value);

void gnc_register_owner_option(GncOptionDB* db, const char* section,
                               const char* name, const char* key,
                               const char* doc_string, GncOwner* value);

void gnc_register_taxtable_option(GncOptionDB* db, const char* section,
                                  const char* name, const char* key,
                                  const char* doc_string, GncTaxTable* value);

void gnc_register_counter_option(GncOptionDB* db, const char* section,
                                 const char* name, const char* key,
                                 const char* doc_string, int value);

void gnc_register_counter_format_option(GncOptionDB* db,
                                        const char* section, const char* name,
                                        const char* key, const char* doc_string,
                                        std::string value);

void gnc_register_dateformat_option(GncOptionDB* db,
                                    const char* section, const char* name,
                                    const char* key, const char* doc_string,
                                    std::string value);

enum RelativeDateUI : uint8_t
{
    ABSOLUTE,
    RELATIVE,
    BOTH
};

void gnc_register_date_option(GncOptionDB* db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string,
                              RelativeDatePeriod period =
                              RelativeDatePeriod::TODAY,
                              RelativeDateUI ui = RelativeDateUI::BOTH);

void gnc_register_date_option(GncOptionDB* db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string, time64 time,
                              RelativeDateUI ui = RelativeDateUI::BOTH);

void gnc_register_date_option(GncOptionDB* db, const char* section,
                              const char* name, const char* key,
                              const char* doc_string,
                              RelativeDatePeriodVec& period_set,
                              bool both = true);

void gnc_register_start_date_option(GncOptionDB* db,
                                    const char* section,
                                    const char* name, const char* key,
                                    const char* doc_string, bool both = true);

void gnc_register_end_date_option(GncOptionDB* db, const char* section,
                                  const char* name, const char* key,
                                  const char* doc_string, bool both = true);


#endif //GNC_OPTIONDB_HPP_
