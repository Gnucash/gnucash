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
const gchar *gnc_version(void);
%}
#if defined(SWIGGUILE)
%{
#include "guile-mappings.h"

SCM scm_init_sw_core_utils_module (void);
%}
#endif
#if defined(SWIGPYTHON)
%{
#if PY_VERSION_HEX >= 0x03000000
PyObject*
#else
void
#endif
    SWIG_init (void);
%}
#endif
%import "base-typemaps.i"

%include <gnc-environment.h>
%include <gnc-prefs.h>
%inline %{
const gchar *gnc_version(void)
{ return VERSION; }
%}

%newobject gnc_path_get_bindir;
gchar * gnc_path_get_bindir(void);

%newobject gnc_path_get_stdreportsdir;
gchar * gnc_path_get_stdreportsdir(void);

%newobject gnc_path_find_localized_html_file;
gchar * gnc_path_find_localized_html_file(const gchar *);

%newobject gnc_build_dotgnucash_path;
gchar * gnc_build_dotgnucash_path(const gchar *);

gchar * gnc_build_report_path(const gchar *);
gchar * gnc_build_stdreports_path(const gchar *);

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
