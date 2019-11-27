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

%module sw_core_utils
%{
#include <config.h>
#include <gnc-environment.h>
#include <gnc-glib-utils.h>
#include <gnc-prefs.h>
#include <gnc-path.h>
#include <gnc-filepath-utils.h>
#include <gnc-locale-utils.h>
#include <glib.h>
#include <gnc-version.h>
%}
#if defined(SWIGGUILE)
%{
#include "guile-mappings.h"

SCM scm_init_sw_core_utils_module (void);
%}
#endif
#if defined(SWIGPYTHON)
%{
PyObject* SWIG_init (void);
%}
#endif
%import "base-typemaps.i"

%include <gnc-environment.h>
%include <gnc-prefs.h>
%include <gnc-version.h>

%newobject gnc_path_get_bindir;
gchar * gnc_path_get_bindir(void);

%newobject gnc_path_get_scmdir;
gchar * gnc_path_get_scmdir(void);

%newobject gnc_path_get_reportsdir;
gchar * gnc_path_get_reportsdir(void);

%newobject gnc_path_get_stdreportsdir;
gchar * gnc_path_get_stdreportsdir(void);

%newobject gnc_path_find_localized_html_file;
gchar * gnc_path_find_localized_html_file(const gchar *);

%newobject gnc_build_userdata_path;
gchar * gnc_build_userdata_path(const gchar *);

%newobject gnc_file_path_absolute;
gchar *gnc_file_path_absolute (const gchar *, const gchar *);

gchar * gnc_build_scm_path(const gchar *);
gchar * gnc_build_report_path(const gchar *);
gchar * gnc_build_stdreports_path(const gchar *);
gchar * gnc_build_reports_path(const gchar *);

void gnc_scm_log_warn(const gchar *);
void gnc_scm_log_error(const gchar *);
void gnc_scm_log_msg(const gchar *);
void gnc_scm_log_debug(const gchar *);

%newobject gnc_utf8_strip_invalid_strdup;
gchar * gnc_utf8_strip_invalid_strdup(const gchar *);

%newobject gnc_locale_from_utf8;
gchar * gnc_locale_from_utf8(const gchar *);

%newobject gnc_locale_to_utf8;
gchar * gnc_locale_to_utf8(const gchar *);

const char * gnc_locale_default_iso_currency_code (void);

%newobject gnc_locale_name;
gchar *gnc_locale_name (void);

#if defined(SWIGGUILE)
%rename ("gnc-utf8?") wrap_gnc_utf8_validate;
%inline %{
  /* This helper function wraps gnc_utf8_validate() into a predicate. */
  gboolean wrap_gnc_utf8_validate(const gchar *);
  gboolean wrap_gnc_utf8_validate(const gchar * str)
  {
    return gnc_utf8_validate(str, -1, 0);
  }
%}
#elif defined(SWIGPYTHON)
gboolean gnc_utf8_validate(const gchar *, gssize, const gchar**);
#endif
