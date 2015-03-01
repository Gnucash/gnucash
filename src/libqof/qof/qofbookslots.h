/********************************************************************\
 * qofbookslots.h -- Defines the names of slots used in the book.   *
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

#ifndef SWIG             /* swig doesn't see N_() as a string constant */
#include <glib/gi18n.h>
#else
#define N_(string) string
#endif

/** @name Book parameter names

 * These define the names used for the slots used to store book level parameters.
 * They are defined here so swig will find them since they need to be available to
 * Scheme code too.
 @{
*/


/*
 * See also SET_ENUM() in src/engine/engine.i
 *
 * SOME_DEFINED_NAME gets mapped into SOME-DEFINED-NAME by SWIG
 * http://www.swig.org/Doc1.3/Guile.html#Guile_nn10
 */


/*
 * gnc:*kvp-option-path* is used to refer to the kvp frame
 * in which book-level options are stored.
 * It is tied from this C #define in
 *   src/app-utils/app-utils.scm
 * and is extensively used in
 *   src/app-utils/option-util.c
 *   src/gnome-utils/gnome-utils.scm
 *   various reports
 */

#define KVP_OPTION_PATH  "options"

/*
 * Various option sections and options within those sections
 * The untranslated string is used for the key in the KVP
 * The translated string appears as the tab name and as the
 * text associated with the option selector on the tab
 */

#define OPTION_SECTION_ACCOUNTS        N_("Accounts")
#define OPTION_NAME_TRADING_ACCOUNTS   N_("Use Trading Accounts")
#define OPTION_NAME_CURRENCY_ACCOUNTING   N_("Select Currency Accounting Method")
#define OPTION_NAME_BOOK_CURRENCY      N_("Select Book Currency")
#define OPTION_NAME_AUTO_READONLY_DAYS N_("Day Threshold for Read-Only Transactions (red line)")
#define OPTION_NAME_NUM_FIELD_SOURCE   N_("Use Split Action Field for Number")

#define OPTION_SECTION_BUDGETING       N_("Budgeting")
#define OPTION_NAME_DEFAULT_BUDGET     N_("Default Budget")

/** @} */

/* For the grep-happy:
 * KVP-OPTION-PATH
 * OPTION-SECTION-ACCOUNTS
 * OPTION-NAME-TRADING-ACCOUNTS
 * OPTION-NAME-CURRENCY-ACCOUNTING
 * OPTION-NAME-BOOK-CURRENCY
 * OPTION-NAME-AUTO-READONLY-DAYS
 * OPTION-NAME_NUM-FIELD-SOURCE
 * OPTION-SECTION-BUDGETING
 * OPTION-NAME-DEFAULT-BUDGET
 */
