/*
 * gncEntryLedgerLoad.c -- a Ledger widget for entering GncEntry objects
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
#include <libguile.h>

#include "Account.h"
#include "account-quickfill.h"
#include "combocell.h"
#include "gnc-component-manager.h"
#include "gnc-gconf-utils.h"
#include "gnc-ui-util.h"
#include "recncell.h"

#include "business-options.h"

#include "gncEntry.h"
#include "gncEntryLedger.h"
#include "gncEntryLedgerP.h"


/* XXX: This should go elsewhere */
const char * gnc_entry_ledger_type_string_getter (char flag)
{
    switch (flag)
    {
    case '1':
        return _("$");
    case '2':
        return _("%");
    default:
        return "?";
    };
}

const char * gnc_entry_ledger_how_string_getter (char flag)
{
    switch (flag)
    {
    case '1':
        return _("<");
    case '2':
        return _("=");
    case '3':
        return _(">");
    default:
        return "?";
    };
}

static void load_discount_type_cells (GncEntryLedger *ledger)
{
    RecnCell *cell;

    if (!ledger) return;

    cell = (RecnCell *)
           gnc_table_layout_get_cell (ledger->table->layout, ENTRY_DISTYPE_CELL);

    if (!cell) return;

    gnc_recn_cell_set_valid_flags (cell, "12", '2');
    gnc_recn_cell_set_flag_order (cell, "21");
    gnc_recn_cell_set_string_getter (cell, gnc_entry_ledger_type_string_getter);
}

static void load_discount_how_cells (GncEntryLedger *ledger)
{
    RecnCell *cell;

    if (!ledger) return;

    cell = (RecnCell *)
           gnc_table_layout_get_cell (ledger->table->layout, ENTRY_DISHOW_CELL);

    if (!cell) return;

    gnc_recn_cell_set_valid_flags (cell, "123", '1');
    gnc_recn_cell_set_flag_order (cell, "123");
    gnc_recn_cell_set_string_getter (cell, gnc_entry_ledger_how_string_getter);
}

static void load_payment_type_cells (GncEntryLedger *ledger)
{
    ComboCell *cell;
    GncOwner *owner;
    GncEmployee *employee;

    cell = (ComboCell *) gnc_table_layout_get_cell (ledger->table->layout,
            ENTRY_PAYMENT_CELL);
    if (!cell) return;

    if (!ledger->invoice) return;

    owner = gncOwnerGetEndOwner (gncInvoiceGetOwner (ledger->invoice));
    if (gncOwnerGetType (owner) != GNC_OWNER_EMPLOYEE)
        return;

    employee = gncOwnerGetEmployee (owner);
    g_return_if_fail (employee);

    gnc_combo_cell_clear_menu (cell);
    gnc_combo_cell_add_menu_item (cell, _("Cash"));

    if (gncEmployeeGetCCard (employee))
        gnc_combo_cell_add_menu_item (cell, _("Charge"));
}

/* ==================================================================== */
/* Return TRUE if we don't want to add this account to the xfer menu */

static gboolean
skip_expense_acct_cb (Account *account, gpointer user_data)
{
    GNCAccountType type;

    /* Don't add A/R, A/P, Bank, Cash, or Equity accounts */
    type = xaccAccountGetType (account);
    if (type == ACCT_TYPE_PAYABLE || type == ACCT_TYPE_RECEIVABLE ||
            type == ACCT_TYPE_CASH || type == ACCT_TYPE_BANK ||
            type == ACCT_TYPE_EQUITY || type == ACCT_TYPE_TRADING)
    {
        return TRUE;
    }

    /* If this is an ORDER or INVOICE, then leave out the expenses.  */
    if (type == ACCT_TYPE_EXPENSE) return TRUE;

    /* Don't add placeholder accounts */
    if (xaccAccountGetPlaceholder (account)) return TRUE;

    return FALSE;
}

static gboolean
skip_income_acct_cb (Account *account, gpointer user_data)
{
    GNCAccountType type;

    /* Don't add A/R, A/P, Bank, Cash, or Equity accounts */
    type = xaccAccountGetType (account);
    if (type == ACCT_TYPE_PAYABLE || type == ACCT_TYPE_RECEIVABLE ||
            type == ACCT_TYPE_CASH || type == ACCT_TYPE_BANK ||
            type == ACCT_TYPE_EQUITY || type == ACCT_TYPE_TRADING)
    {
        return TRUE;
    }

    /* If this is a BILL, then leave out the incomes */
    if (type == ACCT_TYPE_INCOME) return TRUE;

    /* Don't add placeholder accounts */
    if (xaccAccountGetPlaceholder (account)) return TRUE;

    return FALSE;
}

/* ===================================================================== */
/* Splat the account name into the transfer cell combobox menu */

#define EKEY "Expense Business entry quickfill"
#define IKEY "Income Business entry quickfill"

static void
load_xfer_type_cells (GncEntryLedger *ledger)
{
    Account *root;
    ComboCell *cell;
    QuickFill *qf = NULL;
    GtkListStore *store = NULL;

    root = gnc_book_get_root_account (ledger->book);
    if (root == NULL) return;

    /* Use a common, shared quickfill.  For the ORDER or INVOICE,
     * ledgers, we don't want expense-type accounts in the menu.
     * For BILL, etc. then leave out the income types.
     */
    switch (ledger->type)
    {
    case GNCENTRY_ORDER_ENTRY:
    case GNCENTRY_ORDER_VIEWER:
    case GNCENTRY_INVOICE_ENTRY:
    case GNCENTRY_INVOICE_VIEWER:
        qf = gnc_get_shared_account_name_quickfill (root, IKEY,
                skip_expense_acct_cb, NULL);
        store = gnc_get_shared_account_name_list_store (root, IKEY,
                skip_expense_acct_cb, NULL);
        break;

    case GNCENTRY_BILL_ENTRY:
    case GNCENTRY_BILL_VIEWER:
    case GNCENTRY_EXPVOUCHER_ENTRY:
    case GNCENTRY_EXPVOUCHER_VIEWER:
    case GNCENTRY_NUM_REGISTER_TYPES:
        qf = gnc_get_shared_account_name_quickfill (root, EKEY,
                skip_income_acct_cb, NULL);
        store = gnc_get_shared_account_name_list_store (root, EKEY,
                skip_income_acct_cb, NULL);
        break;
    }

    cell = (ComboCell *)
           gnc_table_layout_get_cell (ledger->table->layout, ENTRY_IACCT_CELL);
    gnc_combo_cell_use_quickfill_cache (cell, qf);
    gnc_combo_cell_use_list_store_cache (cell, store);

    cell = (ComboCell *)
           gnc_table_layout_get_cell (ledger->table->layout, ENTRY_BACCT_CELL);
    gnc_combo_cell_use_quickfill_cache (cell, qf);
    gnc_combo_cell_use_list_store_cache (cell, store);
}

/* ===================================================================== */

static void load_taxtable_type_cells (GncEntryLedger *ledger)
{
    GList *list;
    ComboCell *cell;

    cell = (ComboCell *)
           gnc_table_layout_get_cell (ledger->table->layout, ENTRY_TAXTABLE_CELL);
    gnc_combo_cell_clear_menu (cell);

    list = gncTaxTableGetTables (ledger->book);
    for ( ; list ; list = list->next )
    {
        GncTaxTable *table = list->data;
        const char *name = gncTaxTableGetName (table);
        if (name != NULL)
            gnc_combo_cell_add_menu_item (cell, (char*)name);
    }
}

static void
gnc_entry_ledger_show_entry (GncEntryLedger *ledger,
                             VirtualCellLocation start_loc)
{
    VirtualCellLocation end_loc;
    int v_row;

    end_loc = start_loc;
    v_row = end_loc.virt_row + 1;
    end_loc.virt_row = MIN (v_row, ledger->table->num_virt_rows - 1);

    gnc_table_show_range (ledger->table, start_loc, end_loc);
}

void gnc_entry_ledger_load_xfer_cells (GncEntryLedger *ledger)
{
    load_xfer_type_cells (ledger);
    load_taxtable_type_cells (ledger);
    load_payment_type_cells (ledger);
}

/* XXX (FIXME): This should be in a config file! */
/* Copy GncEntry information from the list to the rows of the Ledger. */
/* XXX This code is a cut-n-paste job from the SplitRegister code;
 * the split-regsiter should be generalized to the point where a cut-n-paste
 * like this isn't required, and this should be trashed.
 */
void gnc_entry_ledger_load (GncEntryLedger *ledger, GList *entry_list)
{
    GncEntry *blank_entry, *find_entry;
    CursorBuffer *cursor_buffer;
    Table *table;

    GList *node;
    CellBlock *cursor_header, *cursor;
    VirtualCellLocation vcell_loc;
    VirtualLocation save_loc;
    time_t present;
    gboolean start_primary_color = TRUE;

    int new_entry_row = -1;

    if (!ledger) return;

    /* Load up cells */
    load_discount_type_cells (ledger);
    load_discount_how_cells (ledger);
    gnc_entry_ledger_load_xfer_cells (ledger);

    blank_entry = gnc_entry_ledger_get_blank_entry (ledger);

    if (blank_entry == NULL && ledger->invoice == NULL && entry_list == NULL)
        return;

    if (blank_entry == NULL && ledger->invoice)
    {
        switch (ledger->type)
        {
        case GNCENTRY_ORDER_ENTRY:
        case GNCENTRY_INVOICE_ENTRY:
        case GNCENTRY_BILL_ENTRY:
        case GNCENTRY_EXPVOUCHER_ENTRY:

            gnc_suspend_gui_refresh ();

            blank_entry = gncEntryCreate (ledger->book);
            gncEntrySetDate (blank_entry, ledger->last_date_entered);
            ledger->blank_entry_guid = *gncEntryGetGUID (blank_entry);

            gnc_resume_gui_refresh ();

            /* The rest of this does not apply to expense vouchers */
            if (ledger->type != GNCENTRY_EXPVOUCHER_ENTRY)
            {
                GncOwner *owner = gncInvoiceGetOwner (ledger->invoice);
                GncTaxTable *table = NULL;
                GncTaxIncluded taxincluded_p = GNC_TAXINCLUDED_USEGLOBAL;
                gboolean taxincluded = FALSE;
                gnc_numeric discount = gnc_numeric_zero ();
                GNCOptionDB *odb;

                /* Determine the TaxIncluded and Discount values */
                owner = gncOwnerGetEndOwner (owner);
                switch (gncOwnerGetType (owner))
                {
                case GNC_OWNER_CUSTOMER:
                    taxincluded_p = gncCustomerGetTaxIncluded (owner->owner.customer);
                    discount = gncCustomerGetDiscount (owner->owner.customer);
                    break;
                case GNC_OWNER_VENDOR:
                    taxincluded_p = gncVendorGetTaxIncluded (owner->owner.vendor);
                    break;
                default:
                    break;
                }

                /* Compute the default taxincluded */
                switch (taxincluded_p)
                {
                case GNC_TAXINCLUDED_YES:
                    taxincluded = TRUE;
                    break;
                case GNC_TAXINCLUDED_NO:
                    taxincluded = FALSE;
                    break;
                case GNC_TAXINCLUDED_USEGLOBAL:
                    if (ledger->gconf_section)
                    {
                        taxincluded = gnc_gconf_get_bool(ledger->gconf_section, "tax_included", NULL);
                    }
                    else
                    {
                        taxincluded = FALSE;
                    }
                    break;
                }

                /* Compute the proper taxtable */
                odb = gnc_option_db_new_for_type (GNC_ID_BOOK);
                gnc_option_db_load_from_kvp (odb, gnc_book_get_slots (ledger->book));

                switch (gncOwnerGetType (owner))
                {
                case GNC_OWNER_CUSTOMER:
                    table = gnc_option_db_lookup_taxtable_option (odb,
                            "Business",
                            "Default Customer TaxTable",
                            NULL);

                    if (gncCustomerGetTaxTableOverride (owner->owner.customer))
                        table = gncCustomerGetTaxTable (owner->owner.customer);
                    break;

                case GNC_OWNER_VENDOR:
                    table = gnc_option_db_lookup_taxtable_option (odb,
                            "Business",
                            "Default Vendor TaxTable",
                            NULL);

                    if (gncVendorGetTaxTableOverride (owner->owner.vendor))
                        table = gncVendorGetTaxTable (owner->owner.vendor);
                    break;

                default:
                    break;
                }

                gnc_option_db_destroy (odb);

                if (ledger->is_invoice)
                {
                    gncEntrySetInvTaxTable (blank_entry, table);
                    gncEntrySetInvTaxIncluded (blank_entry, taxincluded);
                    gncEntrySetInvDiscount (blank_entry, discount);
                }
                else
                {
                    gncEntrySetBillTaxTable (blank_entry, table);
                    gncEntrySetBillTaxIncluded (blank_entry, taxincluded);
                }
            }

            break;
        default:
            ledger->blank_entry_guid = *xaccGUIDNULL ();
            break;
        }
        ledger->blank_entry_edited = FALSE;
    }

    table = ledger->table;

    gnc_table_leave_update (table, table->current_cursor_loc);
    save_loc = table->current_cursor_loc;

    /* Figure out where we are going to */
    if (ledger->traverse_to_new)
    {
        find_entry = blank_entry;
    }
    else if (ledger->hint_entry)
    {
        find_entry = ledger->hint_entry;
    }
    else
    {
        find_entry = gnc_entry_ledger_get_current_entry(ledger);
        /* XXX: get current entry (cursor_hint_xxx) */
    }

    /* If the current cursor has changed we save the values for later
     * possible restoration. */
    if (gnc_table_current_cursor_changed (table, TRUE) &&
            (find_entry == gnc_entry_ledger_get_current_entry (ledger)))
    {
        cursor_buffer = gnc_cursor_buffer_new ();
        gnc_table_save_current_cursor (table, cursor_buffer);
    }
    else
        cursor_buffer = NULL;

    /* disable move callback -- we don't want the cascade of
     * callbacks while we are fiddling with loading the register */
    gnc_table_control_allow_move (table->control, FALSE);

    /* invalidate the cursor */
    {
        VirtualLocation virt_loc;

        virt_loc.vcell_loc.virt_row = -1;
        virt_loc.vcell_loc.virt_col = -1;
        virt_loc.phys_row_offset = -1;
        virt_loc.phys_col_offset = -1;

        gnc_table_move_cursor_gui (table, virt_loc);
    }

    /* make sure that the header is loaded */
    vcell_loc.virt_row = 0;
    vcell_loc.virt_col = 0;
    cursor_header = gnc_table_layout_get_cursor (table->layout, CURSOR_HEADER);
    gnc_table_set_vcell (table, cursor_header, NULL, TRUE, TRUE, vcell_loc);
    vcell_loc.virt_row++;

    /* get the current time and reset the dividing row */
    present = gnc_timet_get_today_end ();
    table->model->dividing_row = -1;
    cursor = gnc_table_layout_get_cursor (table->layout, "cursor");

    /* Populate the table */
    for (node = entry_list; node; node = node->next)
    {
        GncEntry *entry = node->data;

        /* Don't load the blank entry */
        if (entry == blank_entry)
            continue;

        /* If this is the first load of the ledger, fill the quickfill cells */
        {
            /* XXX */
        }

        if (entry == find_entry)
            new_entry_row = vcell_loc.virt_row;

        gnc_table_set_vcell (table, cursor, gncEntryGetGUID (entry),
                             TRUE, start_primary_color, vcell_loc);
        vcell_loc.virt_row++;

        /* Flip color for the next guy */
        start_primary_color = !start_primary_color;
    }

    /* Add the blank entry at the end. */
    if (blank_entry)
    {
        gnc_table_set_vcell (table, cursor, gncEntryGetGUID (blank_entry),
                             TRUE, start_primary_color, vcell_loc);

        if (find_entry == blank_entry)
            new_entry_row = vcell_loc.virt_row;

        vcell_loc.virt_row++;
    }

    /* Resize the table */
    gnc_table_set_size (table, vcell_loc.virt_row, 1);

    /* Restore the cursor to its rightful position */
    if (new_entry_row > 0)
        save_loc.vcell_loc.virt_row = new_entry_row;

    if (gnc_table_find_close_valid_cell (table, &save_loc, FALSE))
    {
        gnc_table_move_cursor_gui (table, save_loc);

        if (find_entry == gnc_entry_ledger_get_current_entry (ledger))
            gnc_table_restore_current_cursor (table, cursor_buffer);
    }

    gnc_cursor_buffer_destroy (cursor_buffer);
    cursor_buffer = NULL;

    /* Reset the ledger */
    ledger->traverse_to_new = FALSE;
    ledger->hint_entry = NULL;

    /* Set the cell fractions */


    gnc_table_refresh_gui (table, TRUE);
    gnc_entry_ledger_show_entry (ledger, table->current_cursor_loc.vcell_loc);

    /* Set completion character */
    gnc_combo_cell_set_complete_char
    ((ComboCell *)
     gnc_table_layout_get_cell (table->layout, ENTRY_IACCT_CELL),
     gnc_get_account_separator ());

    gnc_combo_cell_set_complete_char
    ((ComboCell *)
     gnc_table_layout_get_cell (table->layout, ENTRY_BACCT_CELL),
     gnc_get_account_separator ());

    /* enable callback for cursor user-driven moves */
    gnc_table_control_allow_move (table->control, TRUE);
}

/* =========================== END OF FILE ========================== */
