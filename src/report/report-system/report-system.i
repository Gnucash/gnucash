%module sw_report_system
%{
/* Includes the header in the wrapper code */
#include <config.h>
#include <gnc-report.h>

SCM scm_init_sw_report_system_module (void);
%}

%import "base-typemaps.i"

SCM gnc_report_find(gint id);
gint gnc_report_add(SCM report);

%newobject gnc_get_default_report_font_family;
gchar* gnc_get_default_report_font_family();
