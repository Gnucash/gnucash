%module sw_report_system
%{
/* Includes the header in the wrapper code */
#include <config.h>
#include <gnc-report.h>
%}
#if defined(SWIGGUILE)
%{
#include "guile-mappings.h"

SCM scm_init_sw_report_system_module (void);
%}
#endif

%import "base-typemaps.i"

SCM gnc_report_find(gint id);
gint gnc_report_add(SCM report);

%newobject gnc_get_default_report_font_family;
gchar* gnc_get_default_report_font_family();

void gnc_saved_reports_backup (void);
gboolean gnc_saved_reports_write_to_file (const gchar* report_def, gboolean overwrite);