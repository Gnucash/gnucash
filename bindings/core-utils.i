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
#include <glib/gi18n.h>
#include <gnc-version.h>
#include <libintl.h>

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

%newobject gnc_path_get_localedir;
gchar * gnc_path_get_localedir(void);

/* Name of our gettext-domain (defined in config.h) */
%constant char* GETTEXT_PACKAGE = GETTEXT_PACKAGE;

%newobject gnc_path_get_stdreportsdir;
gchar * gnc_path_get_stdreportsdir(void);

%newobject gnc_build_userdata_path;
gchar * gnc_build_userdata_path(const gchar *);

%newobject gnc_file_path_absolute;
gchar *gnc_file_path_absolute (const gchar *, const gchar *);

%newobject gnc_resolve_file_path;
gchar *gnc_resolve_file_path (const gchar *);

%newobject gnc_build_scm_path;
gchar * gnc_build_scm_path(const gchar *);

%newobject gnc_build_report_path;
gchar * gnc_build_report_path(const gchar *);

%newobject gnc_build_stdreports_path;
gchar * gnc_build_stdreports_path(const gchar *);

%newobject gnc_build_reports_path;
gchar * gnc_build_reports_path(const gchar *);

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
%init {
    {
        char tmp[100];

#define SET_ENUM(e) snprintf(tmp, 100, "(set! %s (%s))", (e), (e));  \
        scm_c_eval_string(tmp);

        /* Enum conversions go here */
        SET_ENUM ("GNC-PREFS-GROUP-GENERAL");
        SET_ENUM ("GNC-PREFS-GROUP-GENERAL-REGISTER");
        SET_ENUM ("GNC-PREFS-GROUP-GENERAL-REPORT");
        SET_ENUM ("GNC-PREFS-GROUP-WARNINGS");
        SET_ENUM ("GNC-PREFS-GROUP-WARNINGS-TEMP");
        SET_ENUM ("GNC-PREFS-GROUP-WARNINGS-PERM");
        SET_ENUM ("GNC-PREFS-GROUP-ACCT-SUMMARY");

        SET_ENUM ("GNC-PREF-VERSION");
        SET_ENUM ("GNC-PREF-SAVE-GEOMETRY");
        SET_ENUM ("GNC-PREF-LAST-PATH");
        SET_ENUM ("GNC-PREF-USE-NEW");
        SET_ENUM ("GNC-PREF-ACCOUNTING-LABELS");
        SET_ENUM ("GNC-PREF-ACCOUNT-SEPARATOR");
        SET_ENUM ("GNC-PREF-NEGATIVE-IN-RED");
        SET_ENUM ("GNC-PREF-NUM-SOURCE");
        SET_ENUM ("GNC-PREF-DATE-FORMAT");
        SET_ENUM ("GNC-PREF-DATE-COMPL-THISYEAR");
        SET_ENUM ("GNC-PREF-DATE-COMPL-SLIDING");
        SET_ENUM ("GNC-PREF-DATE-BACKMONTHS");
        SET_ENUM ("GNC-PREF-SHOW-LEAF-ACCT-NAMES");
        SET_ENUM ("GNC-PREF-ENTER-MOVES-TO-END");
        SET_ENUM ("GNC-PREF-DRAW-HOR-LINES");
        SET_ENUM ("GNC-PREF-DRAW-VERT-LINES");
        SET_ENUM ("GNC-PREF-ALT-COLOR-BY-TRANS");
        SET_ENUM ("GNC-PREF-USE-THEME-COLORS");
        SET_ENUM ("GNC-PREF-USE-GNUCASH-COLOR-THEME");
        SET_ENUM ("GNC-PREF-TAB-TRANS-MEMORISED");
        SET_ENUM ("GNC-PREF-FUTURE-AFTER-BLANK");
        SET_ENUM ("GNC-PREF-START-CHOICE-ABS");
        SET_ENUM ("GNC-PREF-START-CHOICE-REL");
        SET_ENUM ("GNC-PREF-START-DATE");
        SET_ENUM ("GNC-PREF-START-PERIOD");
        SET_ENUM ("GNC-PREF-END-CHOICE-ABS");
        SET_ENUM ("GNC-PREF-END-CHOICE-REL");
        SET_ENUM ("GNC-PREF-END-DATE");
        SET_ENUM ("GNC-PREF-END-PERIOD");
        SET_ENUM ("GNC-PREF-CURRENCY-OTHER");
        SET_ENUM ("GNC-PREF-CURRENCY-CHOICE-LOCALE");
        SET_ENUM ("GNC-PREF-CURRENCY-CHOICE-OTHER");

#undef SET_ENUM
    }

}

%rename ("gnc:ngettext") ngettext;
extern const char* ngettext (const char *msgid1, const char *msgid2,
                             unsigned long int n);
%rename ("gnc:gettext") gettext;
extern const char* gettext(const char*);
%rename ("gnc:C_gettext") wrap_C_;
%inline %{
    /* This helper function wraps the C_() macro in a function. */
    const char* wrap_C_(const char* context, const char* msg);
    const char* wrap_C_(const char* context, const char* msg)
    {
        return g_dpgettext2 (NULL, context, msg);
    }
%}
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
