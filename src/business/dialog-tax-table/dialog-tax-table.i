%module sw_dialog_tax_table
%{
/* Includes the header in the wrapper code */
#include <config.h>
#include <dialog-tax-table.h>

SCM scm_init_sw_dialog_tax_table_module (void);
%}

%import "base-typemaps.i"

TaxTableWindow * gnc_ui_tax_table_window_new (QofBook *book);
