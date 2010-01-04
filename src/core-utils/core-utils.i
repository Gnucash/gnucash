%module sw_core_utils
%{
#include <guile-mappings.h>
#include <gnc-glib-utils.h>
#include <gnc-main.h>
#include <glib.h>

SCM scm_init_sw_core_utils_module (void);
%}

%import "base-typemaps.i"

%newobject g_find_program_in_path;
gchar * g_find_program_in_path(const gchar *);

gboolean gnc_is_debugging(void);

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
%rename ("gnc-utf8?") wrap_gnc_utf8_validate;
%inline %{
  /* This helper function wraps gnc_utf8_validate() into a predicate. */
  gboolean wrap_gnc_utf8_validate(const gchar *);
  gboolean wrap_gnc_utf8_validate(const gchar * str)
  {
    return gnc_utf8_validate(str, -1, 0);
  }
%}
