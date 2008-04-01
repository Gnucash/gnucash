%module sw_core_utils
%{
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
