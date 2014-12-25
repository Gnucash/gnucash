%module sw_report_gnome
%{
/* Includes the header in the wrapper code */
#include <config.h>
#include <gtk/gtk.h>
#include <gnc-plugin-page-report.h>
#include <window-report.h>
#include <dialog-custom-report.h>
%}
#if defined(SWIGGUILE)
%{
#include "guile-mappings.h"

SCM scm_init_sw_report_gnome_module (void);
%}
#endif

%import "base-typemaps.i"

void gnc_main_window_open_report(int report_id, GncMainWindow *window);
void gnc_ui_custom_report(GncMainWindow * window);
