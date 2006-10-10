%module sw_gnome
%{
/* Includes the header in the wrapper code */
#include <config.h>
#include <gtk/gtk.h>
#include <dialog-progress.h>

SCM scm_init_sw_gnome_module (void);
%}

// Temporary SWIG<->G-wrap converters for engine types
%typemap(in) gboolean "$1 = SCM_NFALSEP($input) ? TRUE : FALSE;"
%typemap(out) gboolean "$result = $1 ? SCM_BOOL_T : SCM_BOOL_F;"
// End of temporary typemaps.

/* Parse the header file to generate wrappers */
%include <dialog-progress.h>

