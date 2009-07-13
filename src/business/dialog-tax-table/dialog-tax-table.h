/*
 * dialog-tax-table.h -- Dialog to create and edit tax-tables
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifndef _DIALOG_TAX_TABLE_H
#define _DIALOG_TAX_TABLE_H

typedef struct _taxtable_window TaxTableWindow;

#include "gnc-book.h"
#include "gncTaxTable.h"

/* Create a new tax-table by name */
GncTaxTable * gnc_ui_tax_table_new_from_name (QofBook *book, const char *name);

/* Create a tax-table window */
TaxTableWindow * gnc_ui_tax_table_window_new (QofBook *book);

/* Destroy a tax-table window */
void gnc_ui_tax_table_window_destroy (TaxTableWindow *ttw);

#endif /* _DIALOG_TAX-TABLE_H */
