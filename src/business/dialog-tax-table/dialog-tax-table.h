/*
 * dialog-tax-table.h -- Dialog to create and edit tax-tables
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef _DIALOG_TAX_TABLE_H
#define _DIALOG_TAX_TABLE_H

typedef struct _taxtable_window TaxTableWindow;

#include "gnc-book.h"
#include "gncTaxTable.h"

/* Create a new tax-table by name */
GncTaxTable * gnc_ui_tax_table_new_from_name (GNCBook *book, const char *name);

/* Create a tax-table window */
TaxTableWindow * gnc_ui_tax_table_window_new (GNCBook *book);

/* Destroy a tax-table window */
void gnc_ui_tax_table_window_destroy (TaxTableWindow *ttw);

#endif /* _DIALOG_TAX-TABLE_H */
