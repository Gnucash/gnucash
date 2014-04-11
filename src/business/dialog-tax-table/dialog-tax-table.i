%module sw_dialog_tax_table
%{
/* Includes the header in the wrapper code */
#include <config.h>
#include <dialog-tax-table.h>

SCM scm_init_sw_dialog_tax_table_module (void);
%}

TaxTableWindow * gnc_ui_tax_table_window_new (GNCBook *book);
