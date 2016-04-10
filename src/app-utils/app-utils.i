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
#include <config.h>
#include <option-util.h>
#include <gnc-euro.h>
#include <gnc-exp-parser.h>
#include <gnc-ui-util.h>
#include <gnc-gettext-util.h>
#include <gnc-prefs-utils.h>
#include <gnc-helpers.h>
#include <gnc-accounting-period.h>
#include <gnc-session.h>
#include <gnc-component-manager.h>
#include <guile-util.h>
#include <app-utils/gnc-sx-instance-model.h>

#include "engine-helpers-guile.h"
%}

#if defined(SWIGGUILE)
%{
#include "guile-mappings.h"

SCM scm_init_sw_app_utils_module (void);
%}
#endif

#if defined(SWIGPYTHON)
%{
/* avoid no previous prototype warning/error */
#if PY_VERSION_HEX >= 0x03000000
PyObject*
#else
void
#endif
SWIG_init (void);
%}
#endif

%import "base-typemaps.i"

typedef void (*GNCOptionChangeCallback) (gpointer user_data);
typedef int GNCOptionDBHandle;

void gnc_prefs_init();

QofBook * gnc_get_current_book (void);
const gchar * gnc_get_current_book_tax_name (void);
const gchar * gnc_get_current_book_tax_type (void);
Account * gnc_get_current_root_account (void);

%newobject gnc_gettext_helper;
char * gnc_gettext_helper(const char *string);

GNCOptionDB * gnc_option_db_new(SCM guile_options);
void gnc_option_db_destroy(GNCOptionDB *odb);

void gnc_option_db_set_option_selectable_by_name(SCM guile_option,
      const char *section, const char *name, gboolean selectable);

#if defined(SWIGGUILE)
%typemap(out) GncCommodityList * {
  SCM list = SCM_EOL;
  GList *node;

  for (node = $1; node; node = node->next)
    list = scm_cons(gnc_quoteinfo2scm(node->data), list);

  $result = scm_reverse(list);
}

%inline %{
typedef GList GncCommodityList;

GncCommodityList *
gnc_commodity_table_get_quotable_commodities(const gnc_commodity_table * table);
%}

gnc_commodity * gnc_default_currency (void);
gnc_commodity * gnc_default_report_currency (void);

void gncp_option_invoke_callback(GNCOptionChangeCallback callback, void *data);
void gnc_option_db_register_option(GNCOptionDBHandle handle,
        SCM guile_option);

GNCPrintAmountInfo gnc_default_print_info (gboolean use_symbol);
GNCPrintAmountInfo gnc_account_print_info (const Account *account,
        gboolean use_symbol);
GNCPrintAmountInfo gnc_commodity_print_info (const gnc_commodity *commodity,
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

void gnc_register_kvp_option_generator(QofIdType id_type, SCM generator);

%typemap(in) GList * {
  SCM path_scm = $input;
  GList *path = NULL;
  while (!scm_is_null (path_scm))
  {
    SCM key_scm = SCM_CAR (path_scm);
    char *key;
    gchar* gkey;
    if (!scm_is_string (key_scm))
      break;
    key = scm_to_locale_string (key_scm);
    gkey = g_strdup (key);
    free (key);
    path = g_list_prepend (path, gkey);
    path_scm = SCM_CDR (path_scm);
  }
  $1 = g_list_reverse (path);
}
Process *gnc_spawn_process_async(GList *argl, const gboolean search_path);
%clear GList *;

gint gnc_process_get_fd(const Process *proc, const guint std_fd);
void gnc_detach_process(Process *proc, const gboolean kill_it);

time64 gnc_parse_time_to_time64(const gchar *s, const gchar *format);

%typemap(out) GHashTable * {
  SCM table = scm_c_make_hash_table (g_hash_table_size($1) + 17);
  GHashTableIter iter;
  gpointer key, value;

  g_hash_table_iter_init (&iter, $1);
  while (g_hash_table_iter_next (&iter, &key, &value)) {
    const GncGUID* c_guid = (const GncGUID*) key;
    const gnc_numeric* c_numeric = (const gnc_numeric*) value;
    SCM scm_guid = gnc_guid2scm(*c_guid);
    SCM scm_numeric = gnc_numeric_to_scm(*c_numeric);

    scm_hash_set_x(table, scm_guid, scm_numeric);
  }
  g_hash_table_destroy($1);
  $result = table;
}
GHashTable* gnc_sx_all_instantiate_cashflow_all(GDate range_start, GDate range_end);
%clear GHashTable *;
#endif
