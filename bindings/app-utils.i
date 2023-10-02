/********************************************************************\
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

%module sw_app_utils
%{
/* Includes the header in the wrapper code */
#include <glib.h>
#include <config.h>
#include <gnc-euro.h>
#include <gnc-ui-util.h>
#include <gnc-prefs-utils.h>
#if defined(SWIGGUILE)
#include <gnc-helpers.h>
#include "gnc-engine-guile.h"
#endif
#include <gnc-accounting-period.h>
#include <gnc-session.h>
%}

#if defined(SWIGGUILE) //Always C++
%{
#include "guile-mappings.h"

extern "C"
{
SCM scm_init_sw_app_utils_module (void);
}
%}

#endif

#if defined(SWIGPYTHON)
%{
/* avoid no previous prototype warning/error */
PyObject* SWIG_init (void);
%}
%import <gnucash_core.i>
#endif

%import "base-typemaps.i"

void gnc_prefs_init();
void gnc_prefs_remove_registered();

QofBook * gnc_get_current_book (void);
QofSession * gnc_get_current_session (void);
const gchar * gnc_get_current_book_tax_name (void);
const gchar * gnc_get_current_book_tax_type (void);
Account * gnc_get_current_root_account (void);


#if defined(SWIGGUILE)

gnc_commodity * gnc_default_currency (void);
gnc_commodity * gnc_default_report_currency (void);

GNCPrintAmountInfo gnc_default_print_info (gboolean use_symbol);
GNCPrintAmountInfo gnc_account_print_info (const Account *account,
        gboolean use_symbol);
GNCPrintAmountInfo gnc_commodity_print_info (const gnc_commodity *commodity,
        gboolean use_symbol);
GNCPrintAmountInfo gnc_price_print_info (const gnc_commodity *curr,
        gboolean use_symbol);
GNCPrintAmountInfo gnc_share_print_info_places (int decplaces);
const char * xaccPrintAmount (gnc_numeric val, GNCPrintAmountInfo info);

gchar *number_to_words(gdouble val, gint64 denom);
const gchar *printable_value (gdouble val, gint denom);

gboolean gnc_reverse_balance (const Account *account);

gboolean gnc_is_euro_currency(const gnc_commodity * currency);
gnc_numeric gnc_convert_to_euro(const gnc_commodity * currency,
        gnc_numeric value);
gnc_numeric gnc_convert_from_euro(const gnc_commodity * currency,
        gnc_numeric value);

time64 gnc_accounting_period_fiscal_start(void);
time64 gnc_accounting_period_fiscal_end(void);

#endif
