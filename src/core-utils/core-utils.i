%module sw_core_utils
%{
//#include <gnc-gconf-utils.h>
#include <gnc-glib-utils.h>
#include <gnc-main.h>
#include <glib.h>

SCM scm_init_sw_core_utils_module (void);
%}

typedef char gchar;
%typemap(newfree) gchar * "g_free($1);"
%typemap(in) gboolean " $1 = SCM_NFALSEP($input) ? TRUE : FALSE; "
%typemap(out) gboolean " $result = $1 ? SCM_BOOL_T : SCM_BOOL_F; "



%newobject g_find_program_in_path;
gchar * g_find_program_in_path(const gchar *);

gboolean gnc_is_debugging(void);

/* Special treatment because the string changes in place. */
%typemap(in) gchar * " $1 = SCM_STRING_CHARS($input); "
%typemap(freearg) gchar * ""
void gnc_utf8_strip_invalid (gchar *str);

