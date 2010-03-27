/*
 * gncEntryLedger.c -- a Ledger widget for entering GncEntry objects
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

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "Account.h"
#include "dialog-account.h"
#include "dialog-utils.h"
#include "gnc-ui-util.h"
#include "combocell.h"
#include "pricecell.h"
#include "recncell.h"
#include "checkboxcell.h"

#include "gnc-component-manager.h"
#include "gnc-ui.h"

#include "gncEntry.h"
#include "gncEntryLedger.h"
#include "gncEntryLedgerP.h"
#include "gncEntryLedgerLayout.h"
#include "gncEntryLedgerModel.h"
#include "gncEntryLedgerControl.h"

/** Static Functions ***************************************************/

static void
gnc_entry_ledger_clear_blank_entry (GncEntryLedger *ledger)
{
    GncEntry *entry;

    if (!ledger) return;

    entry = gnc_entry_ledger_get_blank_entry (ledger);
    if (entry)
    {
        if (!gncEntryIsOpen (entry))
            gncEntryBeginEdit (entry);
        gncEntryDestroy (entry);
    }

    ledger->blank_entry_guid = *xaccGUIDNULL ();
    ledger->blank_entry_edited = FALSE;
}

/** Exported Functions ***************************************************/

GncEntry *
gnc_entry_ledger_get_blank_entry (GncEntryLedger *ledger)
{
    if (!ledger) return NULL;
    return gncEntryLookup (ledger->book, &(ledger->blank_entry_guid));
}

Account *
gnc_entry_ledger_get_account_by_name (GncEntryLedger *ledger, BasicCell * bcell,
                                      const char *name, gboolean *new)
{
    const char *placeholder = _("The account %s does not allow transactions.");
    const char *missing = _("The account %s does not exist. "
                            "Would you like to create it?");
    char *account_name;
    ComboCell *cell = (ComboCell *) bcell;
    Account *account;
    GList *account_types = NULL;

    /* Find the account */
    account = gnc_account_lookup_for_register (gnc_get_current_root_account (), name);

    if (!account)
    {
        /* Ask if they want to create a new one. */
        if (!gnc_verify_dialog (ledger->parent, TRUE, missing, name))
            return NULL;

        /* No changes, as yet. */
        *new = FALSE;

        /* User said yes, they want to create a new account. */
        account_types = g_list_prepend (account_types, (gpointer)ACCT_TYPE_CREDIT);
        account_types = g_list_prepend (account_types, (gpointer)ACCT_TYPE_ASSET);
        account_types = g_list_prepend (account_types, (gpointer)ACCT_TYPE_LIABILITY);
        if ( ledger->is_invoice )
            account_types = g_list_prepend (account_types, (gpointer)ACCT_TYPE_INCOME);
        else
            account_types = g_list_prepend (account_types, (gpointer)ACCT_TYPE_EXPENSE);

        account = gnc_ui_new_accounts_from_name_window_with_types (name, account_types);
        g_list_free ( account_types );
        if (!account)
            return NULL;
        *new = TRUE;

        /* Now have a new account. Update the cell with the name as created. */
        account_name = gnc_get_account_name_for_register (account);
        gnc_combo_cell_set_value (cell, account_name);
        gnc_basic_cell_set_changed (&cell->cell, TRUE);
        g_free (account_name);
    }

    /* See if the account (either old or new) is a placeholder. */
    if (xaccAccountGetPlaceholder (account))
    {
        gnc_error_dialog (ledger->parent, placeholder, name);
    }

    /* Be seeing you. */
    return account;
}

Account * gnc_entry_ledger_get_account (GncEntryLedger *ledger,
                                        const char * cell_name)
{
    BasicCell *cell;
    const char * name;
    gboolean dummy;

    cell = gnc_table_layout_get_cell (ledger->table->layout, cell_name);
    if (!cell)
        return NULL;
    name = gnc_basic_cell_get_value (cell);
    return gnc_entry_ledger_get_account_by_name (ledger, cell, name, &dummy);
}

GncTaxTable * gnc_entry_ledger_get_taxtable (GncEntryLedger *ledger,
        const char *cell_name)
{
    GncEntry *entry;
    const char * name;

    /* If the cursor has changed, then pull in the current table */
    if (gnc_table_layout_get_cell_changed (ledger->table->layout,
                                           cell_name, TRUE))
    {
        name = gnc_table_layout_get_cell_value (ledger->table->layout, cell_name);
        return gncTaxTableLookupByName (ledger->book, name);
    }

    /* If it has not changed, pull in the table from the entry */
    entry = gnc_entry_ledger_get_current_entry (ledger);
    if (ledger->is_invoice)
        return gncEntryGetInvTaxTable (entry);
    else
        return gncEntryGetBillTaxTable (entry);
}

gboolean gnc_entry_ledger_get_checkmark (GncEntryLedger *ledger,
        const char * cell_name)
{
    CheckboxCell *cell =
        (CheckboxCell *) gnc_table_layout_get_cell (ledger->table->layout, cell_name);

    if (!cell)
        return FALSE;

    return cell->flag;
}

gint gnc_entry_ledger_get_type (GncEntryLedger *ledger, const char * cell_name)
{
    RecnCell *cell =
        (RecnCell *) gnc_table_layout_get_cell (ledger->table->layout, cell_name);

    if (!cell)
        return -1;

    return (gnc_recn_cell_get_flag (cell) - '0');
}

/* Return TRUE if value is valid, return FALSE if invalid */
gboolean gnc_entry_ledger_get_numeric (GncEntryLedger *ledger,
                                       const char *cell_name,
                                       gnc_numeric *value)
{
    PriceCell *cell =
        (PriceCell *) gnc_table_layout_get_cell (ledger->table->layout, cell_name);

    if (!value || !cell)
        return FALSE;

    *value = gnc_price_cell_get_value (cell);
    return TRUE;
}


GncEntry * gnc_entry_ledger_get_entry (GncEntryLedger *ledger,
                                       VirtualCellLocation vcell_loc)
{
    GncGUID *guid;

    if (!ledger) return NULL;

    guid = gnc_table_get_vcell_data (ledger->table, vcell_loc);
    if (!guid) return NULL;

    return gncEntryLookup (ledger->book, guid);
}

/* Returns the Entry where the cursor is currently located. */
GncEntry * gnc_entry_ledger_get_current_entry (GncEntryLedger *ledger)
{
    if (!ledger) return NULL;

    return
        gnc_entry_ledger_get_entry (ledger,
                                    ledger->table->current_cursor_loc.vcell_loc);
}

static void gnc_entry_ledger_config_action (GncEntryLedger *ledger)
{
    ComboCell *cell;

    cell = (ComboCell *) gnc_table_layout_get_cell (ledger->table->layout,
            ENTRY_ACTN_CELL);
    if (!cell) return;

    /* XXX: change this based on the ledger type */

    gnc_combo_cell_add_menu_item (cell, _("Hours"));
    gnc_combo_cell_add_menu_item (cell, _("Project"));
    gnc_combo_cell_add_menu_item (cell, _("Material"));
}

static void
gnc_entry_ledger_config_cells (GncEntryLedger *ledger)
{
    /* the action cell */
    gnc_combo_cell_set_autosize
    ((ComboCell *)
     gnc_table_layout_get_cell (ledger->table->layout, ENTRY_ACTN_CELL), TRUE);

    /* The action cell should also accept strings not in the list */
    gnc_combo_cell_set_strict
    ((ComboCell *)
     gnc_table_layout_get_cell (ledger->table->layout, ENTRY_ACTN_CELL), FALSE);

    /* Use 6 decimal places for all prices and quantities */
    gnc_price_cell_set_fraction
    ((PriceCell *)
     gnc_table_layout_get_cell (ledger->table->layout, ENTRY_PRIC_CELL),
     1000000);

    gnc_price_cell_set_fraction
    ((PriceCell *)
     gnc_table_layout_get_cell (ledger->table->layout, ENTRY_DISC_CELL),
     1000000);

    gnc_price_cell_set_fraction
    ((PriceCell *) gnc_table_layout_get_cell (ledger->table->layout,
            ENTRY_QTY_CELL),
     1000000);

    /* add menu items for the action and payment cells */
    gnc_entry_ledger_config_action (ledger);
}

/* Create and return a new GncEntry Ledger */
GncEntryLedger * gnc_entry_ledger_new (QofBook *book, GncEntryLedgerType type)
{
    GncEntryLedger *ledger;

    if (!book) return NULL;
    if (type < 0 || type >= GNCENTRY_NUM_REGISTER_TYPES) return NULL;

    ledger = g_new0 (GncEntryLedger, 1);
    ledger->type = type;
    ledger->book = book;
    ledger->traverse_to_new = TRUE;
    ledger->gconf_section = NULL;

    /* Orders and Invoices are "invoices" for lookups */
    switch (type)
    {
    case GNCENTRY_ORDER_ENTRY:
    case GNCENTRY_ORDER_VIEWER:
    case GNCENTRY_INVOICE_ENTRY:
    case GNCENTRY_INVOICE_VIEWER:
        ledger->is_invoice = TRUE;
        break;
    case GNCENTRY_BILL_ENTRY:
    case GNCENTRY_BILL_VIEWER:
    case GNCENTRY_EXPVOUCHER_ENTRY:
    case GNCENTRY_EXPVOUCHER_VIEWER:
    case GNCENTRY_NUM_REGISTER_TYPES:
        ledger->is_invoice = FALSE;
        break;
    }

    ledger->blank_entry_guid = *xaccGUIDNULL();
    ledger->blank_entry_edited = FALSE;

    {
        Timespec ts = { 0, 0 };
        ts.tv_sec = time (NULL);
        ledger->last_date_entered = timespecCanonicalDayTime (ts);
    }

    {
        TableLayout *layout = gnc_entry_ledger_layout_new (ledger);
        TableModel *model = gnc_entry_ledger_model_new (type);
        TableControl *control = gnc_entry_ledger_control_new ();
        model->handler_user_data = ledger;
        control->user_data = ledger;

        ledger->table = gnc_table_new (layout, model, control);
    }

    gnc_entry_ledger_config_cells (ledger);

    /* set up header */
    {
        VirtualCellLocation vcell_loc = { 0, 0 };
        CellBlock *header;

        header = gnc_table_layout_get_cursor (ledger->table->layout, CURSOR_HEADER);

        gnc_table_set_vcell (ledger->table, header, NULL, TRUE, TRUE, vcell_loc);
    }

    /* set up first initial row */
    {
        VirtualLocation vloc;
        CellBlock *cursor;

        vloc.vcell_loc.virt_row = 1;
        vloc.vcell_loc.virt_col = 0;
        vloc.phys_row_offset = 0;
        vloc.phys_col_offset = 0;

        cursor = gnc_table_layout_get_cursor (ledger->table->layout, "cursor");

        gnc_table_set_vcell (ledger->table, cursor, NULL, TRUE, TRUE, vloc.vcell_loc);

        if (gnc_table_find_close_valid_cell (ledger->table, &vloc, FALSE))
            gnc_table_move_cursor (ledger->table, vloc);
        else
        {
            g_warning ("Can't find valid initial location");
        }
    }

    /* Initialize Display */
    gnc_entry_ledger_display_init (ledger);
    return ledger;
}

/* Destroy the GncEntry Ledger */
void gnc_entry_ledger_destroy (GncEntryLedger *ledger)
{
    if (!ledger) return;

    /* Destroy blank entry, etc. */
    gnc_entry_ledger_clear_blank_entry (ledger);
    gnc_entry_ledger_display_fini (ledger);
    gnc_table_destroy (ledger->table);
    gncQueryDestroy (ledger->query);
    g_free (ledger);
}

Table * gnc_entry_ledger_get_table (GncEntryLedger *ledger)
{
    if (!ledger) return NULL;
    return ledger->table;
}

void gnc_entry_ledger_set_default_order (GncEntryLedger *ledger,
        GncOrder *order)
{
    if (!ledger) return;
    ledger->order = order;

    if (!ledger->query && order)
    {
        ledger->query = gncQueryCreateFor (GNC_ENTRY_MODULE_NAME);
        gncQuerySetBook (ledger->query, gncOrderGetBook (order));
        gncQueryAddGUIDMatch (ledger->query,
                              g_slist_prepend (g_slist_prepend (NULL,
                                               QUERY_PARAM_GUID),
                                               ENTRY_ORDER),
                              gncOrderGetGUID (order), QUERY_AND);
    }
    gnc_entry_ledger_display_refresh (ledger);
}

static void create_invoice_query (GncEntryLedger *ledger)
{
    QueryNew *q, *q1;
    char * type = NULL;

    if (!ledger->invoice)
        return;

    if (ledger->query)
        gncQueryDestroy (ledger->query);

    /* Match:   (where I-TYPE == Invoice or Bill)
     *
     * 1. book AND
     * 2.   ( Entry->I-TYPE == ledger->invoice
     * #if I-TYPE == Invoice (entry only)
     *        OR
     * 3.     ( Entry->Invoice == NULL AND
     *          ( Entry->Billable == TRUE AND
     *            Entry->Bill->Is-Posted? == TRUE AND
     *            ( Entry->BillTo == Invoice->parent OR
     *              Entry->Bill->BillTo == Invoice->parent ) )
     *           OR
     *           ( Entry->Order->real-parent == Invoice->parent ) )
     * #endif
     *      )
     *
     * Note that term 3 is only for Editable invoices.
     */

    /* Term 1 */
    ledger->query = gncQueryCreateFor (GNC_ENTRY_MODULE_NAME);
    gncQuerySetBook (ledger->query, gncInvoiceGetBook (ledger->invoice));

    /* Term 2 */
    switch (ledger->type)
    {
    case GNCENTRY_INVOICE_ENTRY:
    case GNCENTRY_INVOICE_VIEWER:
        type = ENTRY_INVOICE;
        break;
    case GNCENTRY_BILL_ENTRY:
    case GNCENTRY_BILL_VIEWER:
    case GNCENTRY_EXPVOUCHER_ENTRY:
    case GNCENTRY_EXPVOUCHER_VIEWER:
        type = ENTRY_BILL;
        break;
    default:
        g_warning ("Invalid Ledger type");
        type = ENTRY_INVOICE;
        break;
    }

    q = gncQueryCreateFor (GNC_ENTRY_MODULE_NAME);
    gncQueryAddGUIDMatch (q, gncQueryBuildParamList (type, QUERY_PARAM_GUID, NULL),
                          gncInvoiceGetGUID (ledger->invoice), QUERY_OR);

    /* Term 3 */
    if (ledger->type == GNCENTRY_INVOICE_ENTRY &&
            gncOwnerGetEndGUID (gncInvoiceGetOwner (ledger->invoice)) != NULL)
    {

        const GncGUID *invoice_parent =
            gncOwnerGetGUID (gncInvoiceGetOwner (ledger->invoice));
        QueryNew *q2 = gncQueryCreateFor (GNC_ENTRY_MODULE_NAME);

        /*
         * Entry->BillTo == Invoice->parent OR
         * ( Entry->BillTo == NULL AND Entry->Bill->BillTo == Invoice->parent )
         */

        gncQueryAddGUIDMatch (q2, gncQueryBuildParamList (ENTRY_BILLTO,
                              QUERY_PARAM_GUID, NULL),
                              NULL, QUERY_AND);
        gncQueryAddGUIDMatch (q2, gncQueryBuildParamList (ENTRY_BILL, INVOICE_BILLTO,
                              QUERY_PARAM_GUID, NULL),
                              invoice_parent, QUERY_AND);
        gncQueryAddGUIDMatch (q2, gncQueryBuildParamList (ENTRY_BILLTO,
                              QUERY_PARAM_GUID, NULL),
                              invoice_parent, QUERY_OR);

        /* Entry->Billable == TRUE AND Entry->Bill->Is-Posted? == TRUE */
        gncQueryAddBooleanMatch (q2, gncQueryBuildParamList (ENTRY_BILLABLE, NULL),
                                 TRUE, QUERY_AND);
        gncQueryAddBooleanMatch (q2, gncQueryBuildParamList (ENTRY_BILL,
                                 INVOICE_IS_POSTED, NULL),
                                 TRUE, QUERY_AND);

        /* Entry->Order->real-parent == Invoice->parent */
        gncQueryAddGUIDMatch (q2, gncQueryBuildParamList (ENTRY_ORDER, ORDER_OWNER,
                              OWNER_PARENTG, NULL),
                              invoice_parent, QUERY_OR);

        /* Entry->Invoice == NULL */
        gncQueryAddGUIDMatch (q2, gncQueryBuildParamList (ENTRY_INVOICE,
                              QUERY_PARAM_GUID, NULL),
                              NULL, QUERY_AND);


        /* Combine terms 2 and 3 */
        q1 = gncQueryMerge (q, q2, QUERY_OR);
        gncQueryDestroy (q);
        gncQueryDestroy (q2);
        q = q1;
    }

    /* Combine terms 1 and 2 */
    q1 = gncQueryMerge (ledger->query, q, QUERY_AND);
    gncQueryDestroy (q);
    gncQueryDestroy (ledger->query);
    ledger->query = q1;
}

void gnc_entry_ledger_set_default_invoice (GncEntryLedger *ledger,
        GncInvoice *invoice)
{
    if (!ledger) return;
    ledger->invoice = invoice;

    if (!ledger->query && invoice)
        create_invoice_query (ledger);

    gnc_entry_ledger_display_refresh (ledger);
}

void gnc_entry_ledger_reset_query (GncEntryLedger *ledger)
{
    if (!ledger) return;
    if (!ledger->invoice) return;

    create_invoice_query (ledger);
    gnc_entry_ledger_display_refresh (ledger);
}

void gnc_entry_ledger_set_parent (GncEntryLedger *ledger, gncUIWidget parent)
{
    if (!ledger) return;
    ledger->parent = parent;
}

gboolean gnc_entry_ledger_find_entry (GncEntryLedger *ledger, GncEntry *entry,
                                      VirtualCellLocation *vcell_loc)
{
    Table *table = ledger->table;
    int v_row;
    GncEntry *e;

    for (v_row = 1; v_row < table->num_virt_rows; v_row++)
    {
        VirtualCellLocation vc_loc = { v_row, 0 };

        e = gnc_entry_ledger_get_entry (ledger, vc_loc);

        if (e == entry)
        {
            if (vcell_loc != NULL)
                *vcell_loc = vc_loc;
            return TRUE;
        }
    }
    return FALSE;
}

void gnc_entry_ledger_set_readonly (GncEntryLedger *ledger, gboolean readonly)
{
    if (!ledger) return;

    /* reset the ledger type appropriately */
    if (readonly)
    {
        switch (ledger->type)
        {
        case GNCENTRY_ORDER_ENTRY:
            ledger->type = GNCENTRY_ORDER_VIEWER;
            break;
        case GNCENTRY_INVOICE_ENTRY:
            ledger->type = GNCENTRY_INVOICE_VIEWER;
            create_invoice_query (ledger);
            break;
        case GNCENTRY_BILL_ENTRY:
            ledger->type = GNCENTRY_BILL_VIEWER;
            create_invoice_query (ledger);
            break;
        case GNCENTRY_EXPVOUCHER_ENTRY:
            ledger->type = GNCENTRY_EXPVOUCHER_VIEWER;
            create_invoice_query (ledger);
            break;
        default:
            return;			/* Nothing to do */
        }
    }
    else
    {
        switch (ledger->type)
        {
        case GNCENTRY_ORDER_VIEWER:
            ledger->type = GNCENTRY_ORDER_ENTRY;
            break;
        case GNCENTRY_INVOICE_VIEWER:
            ledger->type = GNCENTRY_INVOICE_ENTRY;
            create_invoice_query (ledger);
            break;
        case GNCENTRY_BILL_VIEWER:
            ledger->type = GNCENTRY_BILL_ENTRY;
            create_invoice_query (ledger);
            break;
        case GNCENTRY_EXPVOUCHER_VIEWER:
            ledger->type = GNCENTRY_EXPVOUCHER_ENTRY;
            create_invoice_query (ledger);
            break;
        default:
            return;			/* Nothing to do */
        }
    }

    /* reset the model */
    gnc_table_model_set_read_only (ledger->table->model, readonly);

    /* if readonly is TRUE, get rid of the blank entry. */
    if (readonly)
        gnc_entry_ledger_clear_blank_entry (ledger);

    /* and refresh the display */
    gnc_entry_ledger_display_refresh (ledger);
}

gboolean
gnc_entry_ledger_changed (GncEntryLedger *ledger)
{
    if (!ledger)
        return FALSE;

    if (gnc_table_current_cursor_changed (ledger->table, FALSE))
        return TRUE;

    return FALSE;
}

void
gnc_entry_ledger_compute_value (GncEntryLedger *ledger,
                                gnc_numeric *value, gnc_numeric *tax_value)
{
    gnc_numeric qty, price, discount;
    gint disc_type, disc_how;
    gboolean taxable, taxincluded;
    GncTaxTable *table;
    GList *taxes = NULL;
    int denom = 100;

    gnc_entry_ledger_get_numeric (ledger, ENTRY_QTY_CELL, &qty);
    gnc_entry_ledger_get_numeric (ledger, ENTRY_PRIC_CELL, &price);
    gnc_entry_ledger_get_numeric (ledger, ENTRY_DISC_CELL, &discount);

    disc_type = gnc_entry_ledger_get_type (ledger, ENTRY_DISTYPE_CELL);
    disc_how = gnc_entry_ledger_get_type (ledger, ENTRY_DISHOW_CELL);

    /* Bills and exp-vouchers dont have discounts */
    if (ledger->type == GNCENTRY_BILL_ENTRY ||
            ledger->type == GNCENTRY_BILL_VIEWER ||
            ledger->type == GNCENTRY_EXPVOUCHER_ENTRY ||
            ledger->type == GNCENTRY_EXPVOUCHER_VIEWER)
    {
        g_assert (gnc_numeric_zero_p (discount));
        disc_type = GNC_AMT_TYPE_VALUE;
        disc_how = GNC_DISC_PRETAX;
    }


    /* If we're so early in the process that we don't have info, stop now */
    if (disc_type < 0 || disc_how < 0)
    {
        if (value)
            *value = gnc_numeric_zero ();
        if (tax_value)
            *tax_value = gnc_numeric_zero ();
        return;
    }

    taxable = gnc_entry_ledger_get_checkmark (ledger, ENTRY_TAXABLE_CELL);
    taxincluded = gnc_entry_ledger_get_checkmark (ledger, ENTRY_TAXINCLUDED_CELL);
    table = gnc_entry_ledger_get_taxtable (ledger, ENTRY_TAXTABLE_CELL);

    /* Expense vouchers don't have taxable, taxincluded, or taxtable cells, either */
    if (ledger->type == GNCENTRY_EXPVOUCHER_ENTRY ||
            ledger->type == GNCENTRY_EXPVOUCHER_VIEWER)
    {
        taxable = FALSE;
        taxincluded = FALSE;
        table = NULL;
    }

    if (ledger->invoice)
    {
        gnc_commodity *currency = gncInvoiceGetCurrency(ledger->invoice);
        if (currency)
            denom = gnc_commodity_get_fraction(currency);
    }

    gncEntryComputeValue (qty, price, (taxable ? table : NULL), taxincluded,
                          discount, disc_type, disc_how, denom,
                          value, NULL, &taxes);

    /* return the tax value */
    if (tax_value)
        *tax_value = gncAccountValueTotal (taxes);
}

gboolean
gnc_entry_ledger_get_entry_virt_loc (GncEntryLedger *ledger, GncEntry *entry,
                                     VirtualCellLocation *vcell_loc)
{
    Table *table;
    int v_row;
    int v_col;

    if ((ledger == NULL) || (entry == NULL))
        return FALSE;

    table = ledger->table;

    /* go backwards because typically you search for entries at the end */

    for (v_row = table->num_virt_rows - 1; v_row > 0; v_row--)
        for (v_col = 0; v_col < table->num_virt_cols; v_col++)
        {
            VirtualCellLocation vc_loc = { v_row, v_col };
            VirtualCell *vcell;
            GncEntry *e;

            vcell = gnc_table_get_virtual_cell (table, vc_loc);
            if (vcell == NULL)
                continue;

            if (!vcell->visible)
                continue;

            e = gncEntryLookup (ledger->book, vcell->vcell_data);

            if (e == entry)
            {
                if (vcell_loc)
                    *vcell_loc = vc_loc;

                return TRUE;
            }
        }

    return FALSE;
}

void
gnc_entry_ledger_delete_current_entry (GncEntryLedger *ledger)
{
    GncEntry *entry;

    if (!ledger)
        return;

    /* If there is no entry, just return */
    entry = gnc_entry_ledger_get_current_entry (ledger);
    if (!entry)
        return;

    /* If this is the blank entry, just cancel the changes */
    if (entry == gnc_entry_ledger_get_blank_entry (ledger))
    {
        gnc_entry_ledger_cancel_cursor_changes (ledger);
        return;
    }

    /* Ok, let's delete this entry */
    gnc_suspend_gui_refresh ();
    if (!gncEntryIsOpen (entry))
        gncEntryBeginEdit (entry);

    {
        GncOrder *order;
        GncInvoice *invoice;

        order = gncEntryGetOrder (entry);
        if (order)
            gncOrderRemoveEntry (order, entry);

        invoice = gncEntryGetInvoice (entry);
        if (invoice)
            gncInvoiceRemoveEntry (invoice, entry);

        invoice = gncEntryGetBill (entry);
        if (invoice)
            gncBillRemoveEntry (invoice, entry);

        gncEntryDestroy (entry);
    }
    gnc_resume_gui_refresh ();
}

void
gnc_entry_ledger_duplicate_current_entry (GncEntryLedger *ledger)
{
    GncEntry *entry;
    gboolean changed;

    if (!ledger)
        return;

    /* Be paranoid */
    entry = gnc_entry_ledger_get_current_entry (ledger);
    if (!entry)
        return;

    changed = gnc_table_current_cursor_changed (ledger->table, FALSE);

    /* See if we're asked to duplicate an unchanged blank entry --
     * there is no point in doing that.
     */
    if (!changed && entry == gnc_entry_ledger_get_blank_entry (ledger))
        return;

    gnc_suspend_gui_refresh ();

    /* If the cursor has been edited, we are going to have to commit
     * it before we can duplicate. Make sure the user wants to do that. */
    if (changed)
    {
        const char *title = _("Save the current entry?");
        const char *message =
            _("The current transaction has been changed. Would you like to "
              "record the changes before duplicating this entry, or "
              "cancel the duplication?");
        GtkWidget *dialog;
        gint response;

        dialog = gtk_message_dialog_new(GTK_WINDOW(ledger->parent),
                                        GTK_DIALOG_DESTROY_WITH_PARENT,
                                        GTK_MESSAGE_QUESTION,
                                        GTK_BUTTONS_NONE,
                                        "%s", title);
        gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                "%s", message);
        gtk_dialog_add_buttons(GTK_DIALOG(dialog),
                               GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                               _("_Record"), GTK_RESPONSE_ACCEPT,
                               NULL);
        response = gnc_dialog_run(GTK_DIALOG(dialog), "invoice_entry_duplicated");
        gtk_widget_destroy(dialog);

        if (response != GTK_RESPONSE_ACCEPT)
        {
            gnc_resume_gui_refresh ();
            return;
        }

        if (!gnc_entry_ledger_commit_entry (ledger))
        {
            gnc_resume_gui_refresh ();
            return;
        }
    }

    /* Ok, we're ready to make the copy */
    {
        GncEntry * new_entry;

        new_entry = gncEntryCreate (ledger->book);
        gncEntryCopy (entry, new_entry);
        gncEntrySetDate (new_entry, ledger->last_date_entered);

        /* Set the hint for where to display on the refresh */
        ledger->hint_entry = new_entry;
    }

    gnc_resume_gui_refresh ();
    return;
}

QueryNew *
gnc_entry_ledger_get_query (GncEntryLedger *ledger)
{
    if (!ledger)
        return NULL;

    return ledger->query;
}

void
gnc_entry_ledger_set_gconf_section (GncEntryLedger *ledger, const gchar *string)
{
    if (!ledger)
        return;

    ledger->gconf_section = string;
}
