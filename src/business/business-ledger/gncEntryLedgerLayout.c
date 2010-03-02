/*
 * gncEntryLedgerLayout.c -- Layout for GncEntry ledger
 * Copyright (C) 2001, 2002, 2003 Derek Atkins
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

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>

#include "gncEntryLedgerP.h"
#include "gncEntryLedgerLayout.h"

static void
gnc_register_add_cell (TableLayout *layout,
                       const char *cell_name,
                       const char *cell_type_name,
                       const char *sample_text,
                       CellAlignment alignment,
                       gboolean expandable,
                       gboolean span)
{
    BasicCell *cell;

    g_return_if_fail (layout != NULL);
    g_return_if_fail (cell_type_name != NULL);

    cell = gnc_register_make_cell (cell_type_name);

    gnc_basic_cell_set_name (cell, cell_name);
    gnc_basic_cell_set_sample_text (cell, sample_text);
    gnc_basic_cell_set_alignment (cell, alignment);
    gnc_basic_cell_set_expandable (cell, expandable);
    gnc_basic_cell_set_span (cell, span);

    gnc_table_layout_add_cell (layout, cell);
}

static void gnc_entry_ledger_layout_add_cells (GncEntryLedger *ledger,
        TableLayout *layout)
{
    struct cell_list
    {
        const char *cell_name;
        const char *cell_type_name;
        const char *sample_text;
        CellAlignment alignment;
        gboolean expandable;
        gboolean span;
    } cells[] =
    {
        /* Translators: The 'sample:' items are strings which are not
           displayed, but only used to estimate widths. Please only
           translate the portion after the ':' and leave the rest
           ("sample:") as is. */
        {
            ENTRY_INV_CELL, CHECKBOX_CELL_TYPE_NAME, N_("sample:X") + 7,
            CELL_ALIGN_LEFT, FALSE, FALSE
        },
        {
            ENTRY_DATE_CELL, DATE_CELL_TYPE_NAME, N_("sample:12/12/2000") + 7,
            CELL_ALIGN_RIGHT, FALSE, FALSE
        },
        {
            ENTRY_DESC_CELL, QUICKFILL_CELL_TYPE_NAME,
            N_("sample:Description of an Entry") + 7, CELL_ALIGN_LEFT, TRUE, FALSE
        },
        {
            ENTRY_ACTN_CELL, COMBO_CELL_TYPE_NAME,
            N_("sample:Action") + 7, CELL_ALIGN_RIGHT,
            FALSE, FALSE
        },
        {
            ENTRY_QTY_CELL, PRICE_CELL_TYPE_NAME, N_("sample:9,999.00") + 7,
            CELL_ALIGN_RIGHT, FALSE, FALSE
        },
        {
            ENTRY_PRIC_CELL, PRICE_CELL_TYPE_NAME, N_("sample:999,999.00") + 7,
            CELL_ALIGN_RIGHT, FALSE, FALSE
        },
        {
            ENTRY_DISC_CELL, PRICE_CELL_TYPE_NAME, N_("sample:9,999.00") + 7,
            CELL_ALIGN_RIGHT, FALSE, FALSE
        },
        /* xgettext:no-c-format */
        {
            ENTRY_DISTYPE_CELL, RECN_CELL_TYPE_NAME, N_("sample(DT):+%") + 11,
            CELL_ALIGN_LEFT, FALSE, FALSE
        },
        /* xgettext:no-c-format */
        {
            ENTRY_DISHOW_CELL, RECN_CELL_TYPE_NAME, N_("sample(DH):+%") + 11,
            CELL_ALIGN_LEFT, FALSE, FALSE
        },
        {
            ENTRY_IACCT_CELL, COMBO_CELL_TYPE_NAME,
            N_("sample:Expenses:Automobile:Gasoline") + 7,
            CELL_ALIGN_RIGHT, FALSE, FALSE
        },
        {
            ENTRY_BACCT_CELL, COMBO_CELL_TYPE_NAME,
            N_("sample:Expenses:Automobile:Gasoline") + 7,
            CELL_ALIGN_RIGHT, FALSE, FALSE
        },
        {
            ENTRY_TAXABLE_CELL, CHECKBOX_CELL_TYPE_NAME, N_("sample:T?") + 7,
            CELL_ALIGN_LEFT, FALSE, FALSE
        },
        {
            ENTRY_TAXINCLUDED_CELL, CHECKBOX_CELL_TYPE_NAME, N_("sample:TI") + 7,
            CELL_ALIGN_LEFT, FALSE, FALSE
        },
        {
            ENTRY_TAXTABLE_CELL, COMBO_CELL_TYPE_NAME, N_("sample:Tax Table 1") + 7,
            CELL_ALIGN_RIGHT, FALSE, FALSE
        },
        {
            ENTRY_VALUE_CELL, PRICE_CELL_TYPE_NAME, N_("sample:999,999.00") + 7,
            CELL_ALIGN_RIGHT, FALSE, FALSE
        },
        {
            ENTRY_TAXVAL_CELL, PRICE_CELL_TYPE_NAME, N_("sample:999.00") + 7,
            CELL_ALIGN_RIGHT, FALSE, FALSE
        },
        {
            ENTRY_BILLABLE_CELL, CHECKBOX_CELL_TYPE_NAME, N_("sample:BI") + 7,
            CELL_ALIGN_LEFT, FALSE, FALSE
        },
        {
            ENTRY_PAYMENT_CELL, COMBO_CELL_TYPE_NAME, N_("sample:Payment") + 7,
            CELL_ALIGN_LEFT, FALSE, FALSE
        }
    };
    unsigned int i;

    for (i = 0; i < (sizeof(cells) / sizeof(*cells)); i++)
        gnc_register_add_cell (layout, cells[i].cell_name, cells[i].cell_type_name,
                               cells[i].sample_text, cells[i].alignment,
                               cells[i].expandable, cells[i].span);
}

static void gnc_entry_ledger_layout_add_cursors (GncEntryLedger *ledger,
        TableLayout *layout)
{
    CellBlock *cursor;
    int num_cols;

    switch (ledger->type)
    {
    case GNCENTRY_ORDER_ENTRY:
    case GNCENTRY_ORDER_VIEWER:
    case GNCENTRY_INVOICE_ENTRY:
    case GNCENTRY_INVOICE_VIEWER:
        num_cols = 15;
        break;
    case GNCENTRY_BILL_ENTRY:
    case GNCENTRY_BILL_VIEWER:
        num_cols = 12;
        break;
    case GNCENTRY_EXPVOUCHER_ENTRY:
    case GNCENTRY_EXPVOUCHER_VIEWER:
        num_cols = 10;
        break;
    default:
        g_assert (FALSE);
        return;
    }

    cursor = gnc_cellblock_new (1, num_cols, CURSOR_HEADER);
    gnc_table_layout_add_cursor (layout, cursor);

    cursor = gnc_cellblock_new (1, num_cols, "cursor");
    gnc_table_layout_add_cursor (layout, cursor);
    gnc_table_layout_set_primary_cursor (layout, cursor);
}

static void gnc_entry_ledger_set_cells (GncEntryLedger *ledger,
                                        TableLayout *layout)
{
    CellBlock *curs;

    switch (ledger->type)
    {
    case GNCENTRY_ORDER_ENTRY:
    case GNCENTRY_ORDER_VIEWER:
    case GNCENTRY_INVOICE_ENTRY:
    case GNCENTRY_INVOICE_VIEWER:

        curs = gnc_table_layout_get_cursor (layout, "cursor");
        gnc_table_layout_set_cell (layout, curs, ENTRY_DATE_CELL, 0, 0);
        gnc_table_layout_set_cell (layout, curs, ENTRY_INV_CELL, 0, 1);
        gnc_table_layout_set_cell (layout, curs, ENTRY_DESC_CELL, 0, 2);
        gnc_table_layout_set_cell (layout, curs, ENTRY_ACTN_CELL, 0, 3);
        gnc_table_layout_set_cell (layout, curs, ENTRY_IACCT_CELL, 0, 4);
        gnc_table_layout_set_cell (layout, curs, ENTRY_QTY_CELL, 0, 5);
        gnc_table_layout_set_cell (layout, curs, ENTRY_PRIC_CELL, 0, 6);
        gnc_table_layout_set_cell (layout, curs, ENTRY_DISTYPE_CELL, 0, 7);
        gnc_table_layout_set_cell (layout, curs, ENTRY_DISHOW_CELL, 0, 8);
        gnc_table_layout_set_cell (layout, curs, ENTRY_DISC_CELL, 0, 9);
        gnc_table_layout_set_cell (layout, curs, ENTRY_TAXABLE_CELL, 0, 10);
        gnc_table_layout_set_cell (layout, curs, ENTRY_TAXINCLUDED_CELL, 0, 11);
        gnc_table_layout_set_cell (layout, curs, ENTRY_TAXTABLE_CELL, 0, 12);
        gnc_table_layout_set_cell (layout, curs, ENTRY_VALUE_CELL, 0, 13);
        gnc_table_layout_set_cell (layout, curs, ENTRY_TAXVAL_CELL, 0, 14);

        break;

    case GNCENTRY_BILL_ENTRY:
    case GNCENTRY_BILL_VIEWER:

        curs = gnc_table_layout_get_cursor (layout, "cursor");
        gnc_table_layout_set_cell (layout, curs, ENTRY_DATE_CELL, 0, 0);
        gnc_table_layout_set_cell (layout, curs, ENTRY_INV_CELL, 0, 1);
        gnc_table_layout_set_cell (layout, curs, ENTRY_DESC_CELL, 0, 2);
        gnc_table_layout_set_cell (layout, curs, ENTRY_ACTN_CELL, 0, 3);
        gnc_table_layout_set_cell (layout, curs, ENTRY_BACCT_CELL, 0, 4);
        gnc_table_layout_set_cell (layout, curs, ENTRY_QTY_CELL, 0, 5);
        gnc_table_layout_set_cell (layout, curs, ENTRY_PRIC_CELL, 0, 6);
        gnc_table_layout_set_cell (layout, curs, ENTRY_TAXABLE_CELL, 0, 7);
        gnc_table_layout_set_cell (layout, curs, ENTRY_TAXINCLUDED_CELL, 0, 8);
        gnc_table_layout_set_cell (layout, curs, ENTRY_TAXTABLE_CELL, 0, 9);
        gnc_table_layout_set_cell (layout, curs, ENTRY_VALUE_CELL, 0, 10);
        gnc_table_layout_set_cell (layout, curs, ENTRY_BILLABLE_CELL, 0, 11);

        break;

    case GNCENTRY_EXPVOUCHER_ENTRY:
    case GNCENTRY_EXPVOUCHER_VIEWER:

        curs = gnc_table_layout_get_cursor (layout, "cursor");
        gnc_table_layout_set_cell (layout, curs, ENTRY_DATE_CELL, 0, 0);
        gnc_table_layout_set_cell (layout, curs, ENTRY_INV_CELL, 0, 1);
        gnc_table_layout_set_cell (layout, curs, ENTRY_DESC_CELL, 0, 2);
        gnc_table_layout_set_cell (layout, curs, ENTRY_ACTN_CELL, 0, 3);
        gnc_table_layout_set_cell (layout, curs, ENTRY_BACCT_CELL, 0, 4);
        gnc_table_layout_set_cell (layout, curs, ENTRY_QTY_CELL, 0, 5);
        gnc_table_layout_set_cell (layout, curs, ENTRY_PRIC_CELL, 0, 6);
        gnc_table_layout_set_cell (layout, curs, ENTRY_VALUE_CELL, 0, 7);
        gnc_table_layout_set_cell (layout, curs, ENTRY_BILLABLE_CELL, 0, 8);
        gnc_table_layout_set_cell (layout, curs, ENTRY_PAYMENT_CELL, 0, 9);

        break;

    default:
        g_assert (FALSE);
        return;
    }
}

TableLayout * gnc_entry_ledger_layout_new (GncEntryLedger *ledger)
{
    TableLayout *layout;

    layout = gnc_table_layout_new ();
    gnc_entry_ledger_layout_add_cells (ledger, layout);
    gnc_entry_ledger_layout_add_cursors (ledger, layout);
    gnc_entry_ledger_set_cells (ledger, layout);

    return layout;
}


