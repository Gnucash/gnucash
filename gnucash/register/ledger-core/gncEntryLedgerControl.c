/** \file gncEntryLedgerControl.c
 * \brief Control for GncEntry ledger
 *
 * Copyright (C) 2001, 2002, 2003 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 * Copyright (C) 2010 Christian Stimming <christian@cstimming.de> */
/*
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

#include <config.h>

#include <glib.h>
#include <glib/gi18n.h>

#include "Account.h"
#include "combocell.h"
#include "dialog-account.h"
#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-gui-query.h"
#include "gnc-warnings.h"
#include "table-allgui.h"
#include "pricecell.h"
#include "dialog-tax-table.h"
#include "checkboxcell.h"

#include "gncEntryLedgerP.h"
#include "gncEntryLedgerControl.h"
#include "gnc-session.h"


static gboolean
gnc_entry_ledger_save (GncEntryLedger *ledger, gboolean do_commit)
{
    GncEntry *blank_entry;
    GncEntry *entry;

    if (!ledger) return FALSE;

    blank_entry = gnc_entry_ledger_get_blank_entry (ledger);

    entry = gnc_entry_ledger_get_current_entry (ledger);
    if (entry == NULL) return FALSE;

    /* Try to avoid heavy-weight updates if nothing has changed */
    if (!gnc_table_current_cursor_changed (ledger->table, FALSE))
    {
        if (!do_commit) return FALSE;

        if (entry == blank_entry)
        {
            if (ledger->blank_entry_edited)
            {
                ledger->last_date_entered = gncEntryGetDateGDate (entry);
                ledger->blank_entry_guid = *guid_null ();
                ledger->blank_entry_edited = FALSE;
                blank_entry = NULL;
            }
            else
                return FALSE;
        }

        return TRUE;
    }

    gnc_suspend_gui_refresh ();

    if (!gncEntryIsOpen (entry))
        gncEntryBeginEdit (entry);

    gnc_table_save_cells (ledger->table, entry);

    if (entry == blank_entry)
    {
        time64 time = gnc_time (NULL);
        gncEntrySetDateEntered (blank_entry, time);

        switch (ledger->type)
        {
        case GNCENTRY_ORDER_ENTRY:
            gncOrderAddEntry (ledger->order, blank_entry);
            break;
        case GNCENTRY_INVOICE_ENTRY:
        case GNCENTRY_CUST_CREDIT_NOTE_ENTRY:
            /* Anything entered on an invoice entry must be part of the invoice! */
            gncInvoiceAddEntry (ledger->invoice, blank_entry);
            break;
        case GNCENTRY_BILL_ENTRY:
        case GNCENTRY_EXPVOUCHER_ENTRY:
        case GNCENTRY_VEND_CREDIT_NOTE_ENTRY:
        case GNCENTRY_EMPL_CREDIT_NOTE_ENTRY:
            /* Anything entered on an invoice entry must be part of the invoice! */
            gncBillAddEntry (ledger->invoice, blank_entry);
            break;
        default:
            /* Nothing to do for viewers */
            g_warning ("blank entry traversed in a viewer");
            break;
        }
    }

    if (entry == blank_entry)
    {
        if (do_commit)
        {
            ledger->blank_entry_guid = *guid_null ();
            blank_entry = NULL;
            ledger->last_date_entered = gncEntryGetDateGDate (entry);
        }
        else
            ledger->blank_entry_edited = TRUE;
    }

    if (do_commit)
        gncEntryCommitEdit (entry);

    gnc_table_clear_current_cursor_changes (ledger->table);

    gnc_resume_gui_refresh ();

    return TRUE;
}

static gboolean
gnc_entry_ledger_verify_acc_cell_ok (GncEntryLedger *ledger,
                                     const char *cell_name,
                                     const char *cell_msg)
{
    ComboCell *cell;
    const char *name;

    cell = (ComboCell *) gnc_table_layout_get_cell (ledger->table->layout,
            cell_name);
    g_return_val_if_fail (cell, TRUE);
    name = cell->cell.value;
    if (!name || *name == '\0')
    {
        const char *format = ("%s %s");
        const char *gen_msg = _("Invalid Entry: You need to supply an account in the right currency for this position.");

        gnc_error_dialog_async (GTK_WINDOW (ledger->parent), format, gen_msg, cell_msg);
        return FALSE;
    }
    return TRUE;
}

/** Verify whether we can save the entry, or warn the user when we can't
 * return TRUE if we can save, FALSE if there is a problem
 */
static gboolean
gnc_entry_ledger_verify_can_save (GncEntryLedger *ledger)
{
    gnc_numeric value;

    /* Compute the value and tax value of the current cursor */
    gnc_entry_ledger_compute_value (ledger, &value, NULL);

    /* If there is a value, make sure there is an account */
    if (! gnc_numeric_zero_p (value))
    {
        switch (ledger->type)
        {
        case GNCENTRY_INVOICE_ENTRY:
        case GNCENTRY_CUST_CREDIT_NOTE_ENTRY:
            if (!gnc_entry_ledger_verify_acc_cell_ok (ledger, ENTRY_IACCT_CELL,
                    _("This account should usually be of type income.")))
                return FALSE;
            break;
        case GNCENTRY_BILL_ENTRY:
        case GNCENTRY_EXPVOUCHER_ENTRY:
        case GNCENTRY_VEND_CREDIT_NOTE_ENTRY:
        case GNCENTRY_EMPL_CREDIT_NOTE_ENTRY:
            if (!gnc_entry_ledger_verify_acc_cell_ok (ledger, ENTRY_BACCT_CELL,
                    _("This account should usually be of type expense or asset.")))
                return FALSE;
            break;
        default:
            g_warning ("Unhandled ledger type");
            break;
        }
    }

    return TRUE;
}

static void gnc_entry_ledger_move_cursor (VirtualLocation *p_new_virt_loc,
        gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    VirtualLocation new_virt_loc = *p_new_virt_loc;
    GncEntry *new_entry;
    GncEntry *old_entry;
    gboolean saved;

    if (!ledger) return;

    old_entry = gnc_entry_ledger_get_current_entry (ledger);
    new_entry = gnc_entry_ledger_get_entry (ledger, new_virt_loc.vcell_loc);

    gnc_suspend_gui_refresh ();
    saved = gnc_entry_ledger_save (ledger, old_entry != new_entry);
    gnc_resume_gui_refresh ();

    /* redrawing can muck everything up */
    if (saved)
    {
        VirtualCellLocation vcell_loc;

        /* redraw */
        gnc_entry_ledger_display_refresh (ledger);

        if (ledger->traverse_to_new)
            new_entry = gnc_entry_ledger_get_blank_entry (ledger);

        /* if the entry we were going to is still in the register,
         * then it may have moved. Find out where it is now. */
        if (gnc_entry_ledger_find_entry (ledger, new_entry, &vcell_loc))
        {
            new_virt_loc.vcell_loc = vcell_loc;
        }
        else
            new_virt_loc.vcell_loc = ledger->table->current_cursor_loc.vcell_loc;
    }

    gnc_table_find_close_valid_cell (ledger->table, &new_virt_loc, FALSE);

    *p_new_virt_loc = new_virt_loc;
}

/** Creates a new query that searches for an GncEntry item with
 * description string equal to the given "desc" argument. The query
 * will find the single GncEntry with the latest (=newest)
 * DATE_ENTERED. */
static QofQuery *new_query_for_entry_desc(GncEntryLedger *reg, const char* desc, gboolean use_invoice)
{
    QofQuery *query = NULL;
    QofQueryPredData *predData = NULL;
    GSList *param_list = NULL;
    GSList *primary_sort_params = NULL;
    const char* should_be_null = (use_invoice ? ENTRY_BILL : ENTRY_INVOICE);

    g_assert(reg);
    g_assert(desc);

    /* The query itself and its book */
    query = qof_query_create_for (GNC_ID_ENTRY);
    qof_query_set_book (query, reg->book);

    /* Predicate data: We want to compare one string, namely the given
     * argument */
    predData =
        qof_query_string_predicate (QOF_COMPARE_EQUAL, desc,
                                    QOF_STRING_MATCH_CASEINSENSITIVE, FALSE);

    /* Search Parameter: We want to query on the ENTRY_DESC column */
    param_list = qof_query_build_param_list (ENTRY_DESC, NULL);

    /* Register this in the query */
    qof_query_add_term (query, param_list, predData, QOF_QUERY_FIRST_TERM);

    /* For invoice entries, Entry->Bill must be NULL, and vice versa */
    qof_query_add_guid_match (query,
                              qof_query_build_param_list (should_be_null,
                                      QOF_PARAM_GUID, NULL),
                              NULL, QOF_QUERY_AND);

    /* Set the sort order: By DATE_ENTERED, increasing, and returning
     * only one single resulting item. */
    primary_sort_params = qof_query_build_param_list(ENTRY_DATE_ENTERED, NULL);
    qof_query_set_sort_order (query, primary_sort_params, NULL, NULL);
    qof_query_set_sort_increasing (query, TRUE, TRUE, TRUE);

    qof_query_set_max_results(query, 1);

    return query;
}

/** Finds the GncEntry with the matching description string as given
 * in "desc", but searches this in the whole book. */
static GncEntry*
find_entry_in_book_by_desc(GncEntryLedger *reg, const char* desc)
{
    GncEntry *result = NULL;
    gboolean use_invoice;
    QofQuery *query;
    GList *entries = NULL;

    switch (reg->type)
    {
    case GNCENTRY_INVOICE_ENTRY:
    case GNCENTRY_INVOICE_VIEWER:
    case GNCENTRY_CUST_CREDIT_NOTE_ENTRY:
    case GNCENTRY_CUST_CREDIT_NOTE_VIEWER:
        use_invoice = TRUE;
        break;
    default:
        use_invoice = FALSE;
        break;
    };

    query = new_query_for_entry_desc(reg, desc, use_invoice);
    entries = qof_query_run(query);

    /* Do we have a non-empty result? */
    if (entries)
    {
        /* That's the result. */
        result = (GncEntry*) entries->data;
        /*g_warning("Found %d GncEntry items", g_list_length (entries));*/
    }

    qof_query_destroy(query);
    return result;
}

#if 0
/** Finds the GncEntry with the matching description string as given
 * in "desc", but searches this only in the given entry ledger
 * (i.e. the currently opened ledger window). */
static GncEntry*
gnc_find_entry_in_reg_by_desc(GncEntryLedger *reg, const char* desc)
{
    int virt_row, virt_col;
    int num_rows, num_cols;
    GncEntry *last_entry;

    g_assert(reg);
    g_assert(reg->table);
    if (!reg || !reg->table)
        return NULL;

    num_rows = reg->table->num_virt_rows;
    num_cols = reg->table->num_virt_cols;

    last_entry = NULL;

    for (virt_row = num_rows - 1; virt_row >= 0; virt_row--)
        for (virt_col = num_cols - 1; virt_col >= 0; virt_col--)
        {
            GncEntry *entry;
            VirtualCellLocation vcell_loc = { virt_row, virt_col };

            entry = gnc_entry_ledger_get_entry(reg, vcell_loc);

            if (entry == last_entry)
                continue;

            if (g_strcmp0 (desc, gncEntryGetDescription (entry)) == 0)
                return entry;

            last_entry = entry;
        }

    return NULL;
}
#endif

static void set_value_combo_cell(BasicCell *cell, const char *new_value)
{
    if (!cell || !new_value)
        return;
    if (g_strcmp0 (new_value, gnc_basic_cell_get_value (cell)) == 0)
        return;

    gnc_combo_cell_set_value ((ComboCell *) cell, new_value);
    gnc_basic_cell_set_changed (cell, TRUE);
}

static void set_value_price_cell(BasicCell *cell, gnc_numeric new_value)
{
    PriceCell *pcell = (PriceCell*) cell;
    if (!cell)
        return;
    if (gnc_numeric_equal (new_value, gnc_price_cell_get_value(pcell)))
        return;

    gnc_price_cell_set_value (pcell, new_value);
    gnc_basic_cell_set_changed (cell, TRUE);
}

static gboolean
gnc_entry_ledger_auto_completion (GncEntryLedger *ledger,
                                  gncTableTraversalDir dir,
                                  VirtualLocation *p_new_virt_loc)
{
    GncEntry *entry;
    GncEntry *blank_entry;
    GncEntry *auto_entry;
    const char* cell_name;
    const char *desc;
    BasicCell *cell = NULL;
    char *account_name = NULL;

    g_assert(ledger);
    g_assert(ledger->table);
    blank_entry = gnc_entry_ledger_get_blank_entry (ledger);

    /* auto-completion is only triggered by a tab out */
    if (dir != GNC_TABLE_TRAVERSE_RIGHT)
        return FALSE;

    entry = gnc_entry_ledger_get_current_entry (ledger);
    if (entry == NULL)
        return FALSE;

    cell_name = gnc_table_get_current_cell_name (ledger->table);

    /* Auto-completion is done only in an entry ledger */
    switch (ledger->type)
    {
    case GNCENTRY_ORDER_ENTRY:
    case GNCENTRY_INVOICE_ENTRY:
    case GNCENTRY_BILL_ENTRY:
    case GNCENTRY_EXPVOUCHER_ENTRY:
    case GNCENTRY_CUST_CREDIT_NOTE_ENTRY:
    case GNCENTRY_VEND_CREDIT_NOTE_ENTRY:
    case GNCENTRY_EMPL_CREDIT_NOTE_ENTRY:
        break;
    default:
        return FALSE;
    }

    /* Further conditions before we actually do auto-completion: */
    /* There must be a blank entry */
    if (blank_entry == NULL)
        return FALSE;

    /* we must be on the blank entry */
    if (entry != blank_entry)
        return FALSE;

    /* and leaving the description cell */
    if (!gnc_cell_name_equal (cell_name, ENTRY_DESC_CELL))
        return FALSE;

    /* nothing but the date and description should be changed */
    /* FIXME, this should be refactored. */
    if (gnc_table_layout_get_cell_changed (ledger->table->layout,
                                           ENTRY_ACTN_CELL, TRUE)
            || gnc_table_layout_get_cell_changed (ledger->table->layout,
                    ENTRY_QTY_CELL, TRUE)
            || gnc_table_layout_get_cell_changed (ledger->table->layout,
                    ENTRY_PRIC_CELL, TRUE)
            || gnc_table_layout_get_cell_changed (ledger->table->layout,
                    ENTRY_DISC_CELL, TRUE)
            || gnc_table_layout_get_cell_changed (ledger->table->layout,
                    ENTRY_DISTYPE_CELL, TRUE)
            || gnc_table_layout_get_cell_changed (ledger->table->layout,
                    ENTRY_DISHOW_CELL, TRUE)
            || gnc_table_layout_get_cell_changed (ledger->table->layout,
                    ENTRY_IACCT_CELL, TRUE)
            || gnc_table_layout_get_cell_changed (ledger->table->layout,
                    ENTRY_BACCT_CELL, TRUE)
            || gnc_table_layout_get_cell_changed (ledger->table->layout,
                    ENTRY_TAXABLE_CELL, TRUE)
            || gnc_table_layout_get_cell_changed (ledger->table->layout,
                    ENTRY_TAXINCLUDED_CELL, TRUE)
            || gnc_table_layout_get_cell_changed (ledger->table->layout,
                    ENTRY_TAXTABLE_CELL, TRUE)
            || gnc_table_layout_get_cell_changed (ledger->table->layout,
                    ENTRY_VALUE_CELL, TRUE)
            || gnc_table_layout_get_cell_changed (ledger->table->layout,
                    ENTRY_TAXVAL_CELL, TRUE)
            || gnc_table_layout_get_cell_changed (ledger->table->layout,
                    ENTRY_BILLABLE_CELL, TRUE)
            || gnc_table_layout_get_cell_changed (ledger->table->layout,
                    ENTRY_PAYMENT_CELL, TRUE))
        return FALSE;

    /* and the description should indeed be changed */
    if (!gnc_table_layout_get_cell_changed (ledger->table->layout,
                                            ENTRY_DESC_CELL, TRUE))
        return FALSE;

    /* to a non-empty value */
    desc = gnc_table_layout_get_cell_value (ledger->table->layout, ENTRY_DESC_CELL);
    if ((desc == NULL) || (*desc == '\0'))
        return FALSE;

    /* Ok, we are sure we want to trigger auto-completion. Now find an
     * entry to copy the values from. */
    auto_entry =
        /* Use this for book-wide auto-completion of the invoice entries */
        find_entry_in_book_by_desc(ledger, desc);

    if (auto_entry == NULL)
        return FALSE;

    /* now perform the completion */
    gnc_suspend_gui_refresh ();

    /* Auto-complete the action field */
    cell = gnc_table_layout_get_cell (ledger->table->layout, ENTRY_ACTN_CELL);
    set_value_combo_cell (cell, gncEntryGetAction (auto_entry));

    /* Auto-complete the account field */
    switch (ledger->type)
    {
    case GNCENTRY_INVOICE_ENTRY:
    case GNCENTRY_CUST_CREDIT_NOTE_ENTRY:
        cell = gnc_table_layout_get_cell (ledger->table->layout, ENTRY_IACCT_CELL);
        account_name = gnc_get_account_name_for_register (gncEntryGetInvAccount(auto_entry));
        break;
    case GNCENTRY_EXPVOUCHER_ENTRY:
    case GNCENTRY_BILL_ENTRY:
    case GNCENTRY_VEND_CREDIT_NOTE_ENTRY:
    case GNCENTRY_EMPL_CREDIT_NOTE_ENTRY:
        cell = gnc_table_layout_get_cell (ledger->table->layout, ENTRY_BACCT_CELL);
        account_name = gnc_get_account_name_for_register (gncEntryGetBillAccount(auto_entry));
        break;
    case GNCENTRY_ORDER_ENTRY:
    default:
        cell = NULL;
        account_name = NULL;
        break;
    }
    set_value_combo_cell (cell, account_name);
    g_free (account_name);

    /* Auto-complete quantity cell. Note that this requires some care because
     * credit notes store quantities with a reversed sign. So we need to figure
     * out if the original document from which we extract the autofill entry
     * was a credit note or not. */
    {
        gboolean orig_is_cn;
        switch (ledger->type)
        {
        case GNCENTRY_INVOICE_ENTRY:
        case GNCENTRY_CUST_CREDIT_NOTE_ENTRY:
            orig_is_cn = gncInvoiceGetIsCreditNote (gncEntryGetInvoice (auto_entry));
            break;
        default:
            orig_is_cn = gncInvoiceGetIsCreditNote (gncEntryGetBill (auto_entry));
            break;
        }
        cell = gnc_table_layout_get_cell (ledger->table->layout, ENTRY_QTY_CELL);
        set_value_price_cell (cell, gncEntryGetDocQuantity (auto_entry, orig_is_cn));
    }

    /* Auto-complete price cell */
    {
        gnc_numeric price;
        switch (ledger->type)
        {
        case GNCENTRY_INVOICE_ENTRY:
        case GNCENTRY_CUST_CREDIT_NOTE_ENTRY:
            price = gncEntryGetInvPrice (auto_entry);
            break;
        default:
            price = gncEntryGetBillPrice (auto_entry);
            break;
        }

        /* Auto-complete price cell */
        cell = gnc_table_layout_get_cell (ledger->table->layout, ENTRY_PRIC_CELL);
        set_value_price_cell (cell, price);
    }

    /* We intentionally skip the discount column */

    /* Taxable?, Tax-include?, Tax table */
    {
        gboolean taxable = FALSE, taxincluded = FALSE;
        GncTaxTable *taxtable = NULL;
        switch (ledger->type)
        {
        case GNCENTRY_INVOICE_ENTRY:
        case GNCENTRY_CUST_CREDIT_NOTE_ENTRY:
            taxable = gncEntryGetInvTaxable (auto_entry);
            taxincluded = gncEntryGetInvTaxIncluded (auto_entry);
            taxtable = gncEntryGetInvTaxTable (auto_entry);
            break;
        case GNCENTRY_BILL_ENTRY:
        case GNCENTRY_VEND_CREDIT_NOTE_ENTRY:
            taxable = gncEntryGetBillTaxable (auto_entry);
            taxincluded = gncEntryGetBillTaxIncluded (auto_entry);
            taxtable = gncEntryGetBillTaxTable (auto_entry);
            break;
        default:
            break;
        }

        switch (ledger->type)
        {
        case GNCENTRY_INVOICE_ENTRY:
        case GNCENTRY_CUST_CREDIT_NOTE_ENTRY:
        case GNCENTRY_BILL_ENTRY:
        case GNCENTRY_VEND_CREDIT_NOTE_ENTRY:
            /* Taxable? cell */
            cell = gnc_table_layout_get_cell (ledger->table->layout, ENTRY_TAXABLE_CELL);
            gnc_checkbox_cell_set_flag ((CheckboxCell *) cell, taxable);
            gnc_basic_cell_set_changed (cell, TRUE);

            /* taxincluded? cell */
            cell = gnc_table_layout_get_cell (ledger->table->layout, ENTRY_TAXINCLUDED_CELL);
            gnc_checkbox_cell_set_flag ((CheckboxCell *) cell, taxincluded);
            gnc_basic_cell_set_changed (cell, TRUE);

            /* Taxable? cell */
            cell = gnc_table_layout_get_cell (ledger->table->layout, ENTRY_TAXTABLE_CELL);
            set_value_combo_cell(cell, gncTaxTableGetName (taxtable));
            break;
        default:
            break;
        }
    }


    gnc_resume_gui_refresh ();

    /* now move to the non-empty amount column unless config setting says not */
    if ( !gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL_REGISTER,
                             GNC_PREF_TAB_TRANS_MEMORISED) )
    {
        VirtualLocation new_virt_loc;
        const char *cell_name = ENTRY_QTY_CELL;

        if (gnc_table_get_current_cell_location (ledger->table, cell_name,
                &new_virt_loc))
            *p_new_virt_loc = new_virt_loc;
    }

    return TRUE;
}

typedef struct
{
    GncEntryLedgerAsyncRequest base;
    VirtualLocation source_loc;
    VirtualLocation destination;
    gncTableTraversalDir direction;
    GncGUID source_entry_guid;
    GncGUID destination_entry_guid;
    char *cell_name;
    char *cell_value;
} EntryLedgerTraverseRequest;

static gboolean
entry_ledger_virtual_location_equal (VirtualLocation first, VirtualLocation second)
{
    return first.vcell_loc.virt_row == second.vcell_loc.virt_row &&
           first.vcell_loc.virt_col == second.vcell_loc.virt_col &&
           first.phys_row_offset == second.phys_row_offset &&
           first.phys_col_offset == second.phys_col_offset;
}

static GtkWindow *
entry_ledger_parent_window (GncEntryLedger *ledger)
{
    return ledger && GTK_IS_WINDOW (ledger->parent) ? GTK_WINDOW (ledger->parent) : NULL;
}

static EntryLedgerTraverseRequest *
entry_ledger_traverse_request_new (GncEntryLedger *ledger,
                                   VirtualLocation destination,
                                   gncTableTraversalDir direction)
{
    EntryLedgerTraverseRequest *request;
    GncEntry *source;
    GncEntry *target;

    request = g_new0 (EntryLedgerTraverseRequest, 1);
    request->source_loc = ledger->table->current_cursor_loc;
    request->destination = destination;
    request->direction = direction;
    source = gnc_entry_ledger_get_current_entry (ledger);
    target = gnc_entry_ledger_get_entry (ledger, destination.vcell_loc);
    request->source_entry_guid = source ? *gncEntryGetGUID (source) : *guid_null ();
    request->destination_entry_guid = target ? *gncEntryGetGUID (target) : *guid_null ();
    gnc_entry_ledger_async_request_track (ledger, &request->base);
    return request;
}

static void
entry_ledger_traverse_request_free (EntryLedgerTraverseRequest *request)
{
    if (!request)
        return;

    gnc_entry_ledger_async_request_untrack (&request->base);
    g_free (request->cell_name);
    g_free (request->cell_value);
    g_free (request);
}

static gboolean
entry_ledger_traverse_request_is_current (const EntryLedgerTraverseRequest *request)
{
    GncEntryLedger *ledger;
    GncEntry *entry;

    if (!request || !(ledger = request->base.ledger) ||
        qof_book_shutting_down (ledger->book) ||
        !entry_ledger_virtual_location_equal (ledger->table->current_cursor_loc,
                                              request->source_loc))
        return FALSE;

    entry = gnc_entry_ledger_get_current_entry (ledger);
    if (!entry || !guid_equal (gncEntryGetGUID (entry), &request->source_entry_guid))
        return FALSE;

    if (request->cell_name)
    {
        const char *value = gnc_table_layout_get_cell_value (ledger->table->layout,
                                                              request->cell_name);
        if (g_strcmp0 (value, request->cell_value) != 0)
            return FALSE;
    }

    return TRUE;
}

static gboolean
entry_ledger_traverse_request_destination (const EntryLedgerTraverseRequest *request,
                                           VirtualLocation *destination)
{
    GncEntryLedger *ledger;
    GncEntry *entry;
    VirtualCellLocation location;

    g_return_val_if_fail (request != NULL, FALSE);
    g_return_val_if_fail (destination != NULL, FALSE);

    ledger = request->base.ledger;
    if (!ledger)
        return FALSE;

    *destination = request->destination;
    if (!guid_equal (&request->destination_entry_guid, guid_null ()))
    {
        entry = gncEntryLookup (ledger->book, &request->destination_entry_guid);
        if (!entry || !gnc_entry_ledger_find_entry (ledger, entry, &location))
            return FALSE;
        destination->vcell_loc = location;
    }

    return gnc_table_find_close_valid_cell (ledger->table, destination, FALSE);
}

static void
entry_ledger_resume_traverse_request (EntryLedgerTraverseRequest *request)
{
    GncEntryLedger *ledger;
    VirtualLocation source;
    VirtualLocation destination;
    gboolean abort_move;
    gboolean skip_missing_tax_table_creation;
    gncTableTraversalDir direction;

    if (!entry_ledger_traverse_request_is_current (request))
    {
        entry_ledger_traverse_request_free (request);
        return;
    }

    ledger = request->base.ledger;
    source = request->source_loc;
    destination = request->destination;
    direction = request->direction;
    skip_missing_tax_table_creation = ledger->skip_missing_tax_table_creation;
    entry_ledger_traverse_request_free (request);

    abort_move = gnc_table_traverse_update (ledger->table, source,
                                            direction, &destination);
    if (skip_missing_tax_table_creation)
        ledger->skip_missing_tax_table_creation = FALSE;
    if (!abort_move)
        gnc_table_move_cursor_gui (ledger->table, destination);
}

static void
entry_ledger_account_created (Account *account,
                              gpointer user_data)
{
    EntryLedgerTraverseRequest *request = user_data;
    GncEntryLedger *ledger;
    BasicCell *cell;
    char *account_name;

    if (!account || !entry_ledger_traverse_request_is_current (request))
    {
        entry_ledger_traverse_request_free (request);
        return;
    }

    ledger = request->base.ledger;
    cell = gnc_table_layout_get_cell (ledger->table->layout, request->cell_name);
    if (!cell)
    {
        entry_ledger_traverse_request_free (request);
        return;
    }

    account_name = gnc_get_account_name_for_register (account);
    gnc_combo_cell_set_value ((ComboCell *)cell, account_name);
    gnc_basic_cell_set_changed (cell, TRUE);
    g_free (account_name);
    ledger->full_refresh = TRUE;
    g_clear_pointer (&request->cell_name, g_free);
    g_clear_pointer (&request->cell_value, g_free);

    entry_ledger_resume_traverse_request (request);
}

static void
entry_ledger_account_creation_confirmed (GtkWindow *parent [[maybe_unused]], gint response,
                                         gpointer user_data)
{
    EntryLedgerTraverseRequest *request = user_data;
    GncEntryLedger *ledger;
    GList *account_types = NULL;

    if (response != GTK_RESPONSE_YES || !entry_ledger_traverse_request_is_current (request))
    {
        entry_ledger_traverse_request_free (request);
        return;
    }

    ledger = request->base.ledger;
    account_types = g_list_prepend (account_types, GINT_TO_POINTER (ACCT_TYPE_CREDIT));
    account_types = g_list_prepend (account_types, GINT_TO_POINTER (ACCT_TYPE_ASSET));
    account_types = g_list_prepend (account_types, GINT_TO_POINTER (ACCT_TYPE_LIABILITY));
    account_types = g_list_prepend (account_types, GINT_TO_POINTER (
        ledger->is_cust_doc ? ACCT_TYPE_INCOME : ACCT_TYPE_EXPENSE));

    gnc_ui_new_accounts_from_name_with_defaults_async (
        entry_ledger_parent_window (ledger), request->cell_value, account_types,
        NULL, NULL, entry_ledger_account_created, request);
    g_list_free (account_types);
}

static void
entry_ledger_tax_table_created (GtkWindow *parent [[maybe_unused]],
                                GncTaxTable *table, gpointer user_data)
{
    EntryLedgerTraverseRequest *request = user_data;
    GncEntryLedger *ledger;
    BasicCell *cell;

    if (!entry_ledger_traverse_request_is_current (request))
    {
        entry_ledger_traverse_request_free (request);
        return;
    }

    ledger = request->base.ledger;
    if (!table)
    {
        ledger->skip_missing_tax_table_creation = TRUE;
        entry_ledger_resume_traverse_request (request);
        return;
    }

    cell = gnc_table_layout_get_cell (ledger->table->layout, request->cell_name);
    if (!cell)
    {
        entry_ledger_traverse_request_free (request);
        return;
    }

    gnc_combo_cell_set_value ((ComboCell *)cell, gncTaxTableGetName (table));
    gnc_basic_cell_set_changed (cell, TRUE);
    ledger->full_refresh = TRUE;
    g_clear_pointer (&request->cell_name, g_free);
    g_clear_pointer (&request->cell_value, g_free);
    entry_ledger_resume_traverse_request (request);
}

static void
entry_ledger_tax_table_creation_confirmed (GtkWindow *parent [[maybe_unused]], gint response,
                                           gpointer user_data)
{
    EntryLedgerTraverseRequest *request = user_data;
    GncEntryLedger *ledger;

    if (!entry_ledger_traverse_request_is_current (request))
    {
        entry_ledger_traverse_request_free (request);
        return;
    }

    ledger = request->base.ledger;
    if (response != GTK_RESPONSE_YES)
    {
        ledger->skip_missing_tax_table_creation = TRUE;
        entry_ledger_resume_traverse_request (request);
        return;
    }

    gnc_ui_tax_table_new_from_name_async (entry_ledger_parent_window (ledger),
                                          ledger->book, request->cell_value,
                                          entry_ledger_tax_table_created, request);
}

static gboolean
entry_ledger_begin_account_or_tax_table_request (GncEntryLedger *ledger,
                                                 VirtualLocation destination,
                                                 gncTableTraversalDir direction,
                                                 const char *current_cell_name)
{
    const char *account_cell_name = NULL;
    BasicCell *cell;
    const char *name;
    Account *account;
    GncTaxTable *tax_table;
    EntryLedgerTraverseRequest *request;

    switch (ledger->type)
    {
    case GNCENTRY_INVOICE_ENTRY:
    case GNCENTRY_INVOICE_VIEWER:
    case GNCENTRY_CUST_CREDIT_NOTE_ENTRY:
    case GNCENTRY_CUST_CREDIT_NOTE_VIEWER:
        account_cell_name = ENTRY_IACCT_CELL;
        break;
    case GNCENTRY_BILL_ENTRY:
    case GNCENTRY_BILL_VIEWER:
    case GNCENTRY_EXPVOUCHER_ENTRY:
    case GNCENTRY_EXPVOUCHER_VIEWER:
    case GNCENTRY_VEND_CREDIT_NOTE_ENTRY:
    case GNCENTRY_VEND_CREDIT_NOTE_VIEWER:
    case GNCENTRY_EMPL_CREDIT_NOTE_ENTRY:
    case GNCENTRY_EMPL_CREDIT_NOTE_VIEWER:
        account_cell_name = ENTRY_BACCT_CELL;
        break;
    default:
        break;
    }

    if (account_cell_name && gnc_cell_name_equal (current_cell_name, account_cell_name) &&
        gnc_table_layout_get_cell_changed (ledger->table->layout, account_cell_name, FALSE))
    {
        cell = gnc_table_layout_get_cell (ledger->table->layout, account_cell_name);
        name = cell ? gnc_basic_cell_get_value (cell) : NULL;
        if (name && *name)
        {
            account = gnc_entry_ledger_get_account_by_name (ledger, cell, name,
                                                             &ledger->full_refresh);
            if (!account)
            {
                request = entry_ledger_traverse_request_new (ledger, destination, direction);
                request->cell_name = g_strdup (account_cell_name);
                request->cell_value = g_strdup (name);
                gnc_verify_dialog_async (
                    entry_ledger_parent_window (ledger), TRUE,
                    entry_ledger_account_creation_confirmed, request,
                    _("The account %s does not exist. Would you like to create it?"), name);
                return TRUE;
            }
        }
    }

    if (!ledger->skip_missing_tax_table_creation &&
        gnc_cell_name_equal (current_cell_name, ENTRY_TAXTABLE_CELL) &&
        gnc_table_layout_get_cell_changed (ledger->table->layout, ENTRY_TAXTABLE_CELL, FALSE))
    {
        cell = gnc_table_layout_get_cell (ledger->table->layout, ENTRY_TAXTABLE_CELL);
        name = cell ? gnc_basic_cell_get_value (cell) : NULL;
        tax_table = name && *name ? gncTaxTableLookupByName (ledger->book, name) : NULL;
        if (name && *name && !tax_table)
        {
            request = entry_ledger_traverse_request_new (ledger, destination, direction);
            request->cell_name = g_strdup (ENTRY_TAXTABLE_CELL);
            request->cell_value = g_strdup (name);
            gnc_verify_dialog_async (
                entry_ledger_parent_window (ledger), TRUE,
                entry_ledger_tax_table_creation_confirmed, request,
                _("The tax table %s does not exist. Would you like to create it?"), name);
            return TRUE;
        }
    }

    return FALSE;
}

static void
entry_ledger_warning_choice_async (GtkWindow *parent, const gchar *pref_key,
                                   const gchar *title, const gchar *message,
                                   const gchar *first_label, gint first_response,
                                   const gchar *second_label, gint second_response,
                                   gboolean second_is_default,
                                   GncGuiQueryResponseCallback completed,
                                   gpointer user_data)
{
    GtkWidget *dialog = gtk_message_dialog_new (parent,
        GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE,
        "%s", title);
    gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog), "%s",
                                              message);
    gtk_dialog_add_buttons (GTK_DIALOG (dialog), first_label, first_response,
                            second_label, second_response, NULL);
    gtk_dialog_set_default_response (GTK_DIALOG (dialog),
                                     second_is_default ? second_response : first_response);
    gnc_dialog_run_async (GTK_DIALOG (dialog), pref_key, completed, user_data);
}

static void
entry_ledger_order_change_response (GtkWindow *parent, gint response, gpointer user_data)
{
    EntryLedgerTraverseRequest *request = user_data;
    GncEntryLedger *ledger;
    VirtualLocation destination;

    if (!entry_ledger_traverse_request_is_current (request))
    {
        entry_ledger_traverse_request_free (request);
        return;
    }

    ledger = request->base.ledger;
    if (response == GTK_RESPONSE_ACCEPT)
    {
        entry_ledger_resume_traverse_request (request);
        return;
    }

    if (response != GTK_RESPONSE_REJECT)
    {
        entry_ledger_traverse_request_free (request);
        return;
    }

    gnc_entry_ledger_cancel_cursor_changes (ledger);
    if (entry_ledger_traverse_request_destination (request, &destination))
        gnc_table_move_cursor_gui (ledger->table, destination);
    entry_ledger_traverse_request_free (request);
}

static gboolean
entry_ledger_begin_order_change_request (GncEntryLedger *ledger,
                                         VirtualLocation destination,
                                         gncTableTraversalDir direction)
{
    GncEntry *entry = gnc_entry_ledger_get_current_entry (ledger);
    EntryLedgerTraverseRequest *request;

    if ((ledger->type != GNCENTRY_INVOICE_ENTRY &&
         ledger->type != GNCENTRY_CUST_CREDIT_NOTE_ENTRY) ||
        !entry || !gncEntryGetOrder (entry))
        return FALSE;

    request = entry_ledger_traverse_request_new (ledger, destination, direction);
    entry_ledger_warning_choice_async (
        entry_ledger_parent_window (ledger), GNC_PREF_WARN_INV_ENTRY_MOD,
        _("Save the current entry?"),
        _("The current entry has been changed. However, this entry is "
          "part of an existing order. Would you like to record the change "
          "and effectively change your order?"),
        _("_Don't Record"), GTK_RESPONSE_REJECT, _("_Record"),
        GTK_RESPONSE_ACCEPT, TRUE, entry_ledger_order_change_response, request);
    return TRUE;
}

static gboolean
gnc_entry_ledger_traverse (VirtualLocation *p_new_virt_loc,
                           gncTableTraversalDir dir,
                           gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    GncEntry *entry;
    GncEntry *new_entry;
    VirtualLocation virt_loc;
    int changed;
    const char *cell_name;
    gboolean exact_traversal;

    if (!ledger)
        return FALSE;

    exact_traversal = dir == GNC_TABLE_TRAVERSE_POINTER;
    entry = gnc_entry_ledger_get_current_entry (ledger);
    if (!entry)
        return FALSE;

    changed = gnc_table_current_cursor_changed (ledger->table, FALSE);
    if (!changed)
        return FALSE;

    virt_loc = *p_new_virt_loc;
    cell_name = gnc_table_get_current_cell_name (ledger->table);
    if (entry_ledger_begin_account_or_tax_table_request (ledger, virt_loc, dir,
                                                         cell_name))
        return TRUE;

    if (dir == GNC_TABLE_TRAVERSE_RIGHT &&
        (changed || ledger->blank_entry_edited))
    {
        VirtualLocation end_loc = ledger->table->current_cursor_loc;

        if (!gnc_table_move_vertical_position (ledger->table, &end_loc, 1))
        {
            end_loc = ledger->table->current_cursor_loc;
            if (!gnc_table_move_tab (ledger->table, &end_loc, TRUE))
            {
                *p_new_virt_loc = ledger->table->current_cursor_loc;
                if (!gnc_entry_ledger_verify_can_save (ledger))
                    return TRUE;

                p_new_virt_loc->vcell_loc.virt_row++;
                p_new_virt_loc->phys_row_offset = 0;
                p_new_virt_loc->phys_col_offset = 0;
                ledger->traverse_to_new = TRUE;
                return FALSE;
            }
        }
    }

    if (!gnc_table_virtual_cell_out_of_bounds (ledger->table, virt_loc.vcell_loc) &&
        gnc_entry_ledger_auto_completion (ledger, dir, p_new_virt_loc))
        return FALSE;

    gnc_table_find_close_valid_cell (ledger->table, &virt_loc, exact_traversal);
    new_entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
    if (entry == new_entry)
    {
        *p_new_virt_loc = virt_loc;
        return FALSE;
    }

    if (!gnc_entry_ledger_verify_can_save (ledger))
    {
        *p_new_virt_loc = ledger->table->current_cursor_loc;
        return TRUE;
    }

    if (entry_ledger_begin_order_change_request (ledger, virt_loc, dir))
        return TRUE;

    return FALSE;
}
TableControl * gnc_entry_ledger_control_new (void)
{
    TableControl * control;

    control = gnc_table_control_new ();
    control->move_cursor = gnc_entry_ledger_move_cursor;
    control->traverse = gnc_entry_ledger_traverse;

    return control;
}


void gnc_entry_ledger_cancel_cursor_changes (GncEntryLedger *ledger)
{
    VirtualLocation virt_loc;

    if (ledger == NULL)
        return;

    virt_loc = ledger->table->current_cursor_loc;

    if (!gnc_table_current_cursor_changed (ledger->table, FALSE))
        return;

    /* When cancelling edits, reload the cursor from the entry. */
    gnc_table_clear_current_cursor_changes (ledger->table);

    if (gnc_table_find_close_valid_cell (ledger->table, &virt_loc, FALSE))
        gnc_table_move_cursor_gui (ledger->table, virt_loc);

    gnc_table_refresh_gui (ledger->table, TRUE);
}

typedef struct
{
    guint refs;
    GncEntryLedger *ledger;
    GWeakRef parent;
    GtkWidget *parent_signal_object;
    QofBook *book;
    QofSession *session;
    GncGUID entry_guid;
    VirtualLocation cursor;
    GncEntryLedgerCloseCallback callback;
    gpointer user_data;
    gboolean completed;
    gboolean save_confirmed;
    gboolean order_change_confirmed;
    gboolean order_confirmation_pending;
    gboolean tax_creation_decided;
    gchar *tax_decided_name;
    gchar *pending_account_name;
    gchar *pending_tax_name;
} GncEntryLedgerCloseRequest;

static void ledger_close_continue (GncEntryLedgerCloseRequest *request);

static GncEntryLedgerCloseRequest *
ledger_close_request_ref (GncEntryLedgerCloseRequest *request)
{
    ++request->refs;
    return request;
}

static void
ledger_close_request_unref (GncEntryLedgerCloseRequest *request)
{
    if (--request->refs)
        return;
    if (request->book)
        g_object_remove_weak_pointer (G_OBJECT (request->book),
                                      (gpointer *)&request->book);
    g_weak_ref_clear (&request->parent);
    g_free (request->tax_decided_name);
    g_free (request->pending_account_name);
    g_free (request->pending_tax_name);
    g_free (request);
}

static void
ledger_close_finish (GncEntryLedgerCloseRequest *request, gboolean accepted)
{
    if (request->completed)
        return;
    request->completed = TRUE;
    if (request->ledger)
    {
        request->ledger->async_close_requests =
            g_list_remove (request->ledger->async_close_requests, request);
        request->ledger = NULL;
        ledger_close_request_unref (request); /* ledger registration */
    }
    if (request->parent_signal_object)
    {
        g_signal_handlers_disconnect_by_data (request->parent_signal_object,
                                              request);
        request->parent_signal_object = NULL;
    }
    request->callback (accepted, request->user_data);
}

static gboolean
ledger_close_request_is_current (GncEntryLedgerCloseRequest *request)
{
    GtkWidget *parent = g_weak_ref_get (&request->parent);
    GncEntry *entry;
    gboolean current;
    if (!parent || gtk_widget_in_destruction (parent) || !request->ledger ||
        !request->book || !gnc_current_session_exist () ||
        gnc_get_current_session () != request->session ||
        qof_session_get_book (request->session) != request->book ||
        gnc_get_current_book () != request->book ||
        !qof_book_is_open (request->book) ||
        qof_book_shutting_down (request->book) ||
        qof_book_is_readonly (request->book) ||
        request->ledger->book != request->book ||
        request->ledger->parent != parent)
    {
        g_clear_object (&parent);
        return FALSE;
    }
    entry = gnc_entry_ledger_get_current_entry (request->ledger);
    current = entry && guid_equal (gncEntryGetGUID (entry),
                                   &request->entry_guid) &&
        request->ledger->table->current_cursor_loc.vcell_loc.virt_row ==
            request->cursor.vcell_loc.virt_row &&
        request->ledger->table->current_cursor_loc.vcell_loc.virt_col ==
            request->cursor.vcell_loc.virt_col &&
        request->ledger->table->current_cursor_loc.phys_row_offset ==
            request->cursor.phys_row_offset &&
        request->ledger->table->current_cursor_loc.phys_col_offset ==
            request->cursor.phys_col_offset;
    g_object_unref (parent);
    return current;
}

static void
ledger_close_parent_destroyed (GtkWidget *parent, gpointer user_data)
{
    GncEntryLedgerCloseRequest *request = user_data;
    request->parent_signal_object = NULL;
    ledger_close_finish (request, FALSE);
}

static void
ledger_close_response (GtkWindow *parent, gint response, gpointer user_data)
{
    GncEntryLedgerCloseRequest *request = user_data;
    GncEntryLedger *ledger = request->ledger;
    gboolean accepted = FALSE;
    if (!request->completed && response == GTK_RESPONSE_YES &&
        ledger_close_request_is_current (request) &&
        gnc_entry_ledger_verify_can_save (ledger))
    {
        request->save_confirmed = TRUE;
        if (request->order_confirmation_pending)
        {
            request->order_confirmation_pending = FALSE;
            request->order_change_confirmed = TRUE;
        }
        ledger_close_continue (request);
        ledger_close_request_unref (request); /* this response stage */
        return;
    }
    else if (!request->completed && response == GTK_RESPONSE_NO &&
             ledger_close_request_is_current (request))
    {
        gnc_entry_ledger_cancel_cursor_changes (ledger);
        accepted = TRUE;
    }
    ledger_close_finish (request, accepted);
    ledger_close_request_unref (request); /* the dialog response reference */
}

static gboolean
ledger_close_async_preflight (GncEntryLedger *ledger)
{
    return gnc_entry_ledger_verify_can_save (ledger);
}

static void ledger_close_account_created (Account *account, gpointer user_data);
static void ledger_close_tax_table_created (GtkWindow *parent,
                                            GncTaxTable *table,
                                            gpointer user_data);

static void
ledger_close_account_create_confirmed (GtkWindow *parent, gint response,
                                       gpointer user_data)
{
    GncEntryLedgerCloseRequest *request = user_data;
    GncEntryLedger *ledger = request->ledger;
    if (request->completed || response != GTK_RESPONSE_YES ||
        !ledger_close_request_is_current (request))
    {
        ledger_close_finish (request, FALSE);
        ledger_close_request_unref (request);
        return;
    }
    const gchar *cell_name =
        (ledger->type == GNCENTRY_INVOICE_ENTRY ||
         ledger->type == GNCENTRY_CUST_CREDIT_NOTE_ENTRY) ?
            ENTRY_IACCT_CELL : ENTRY_BACCT_CELL;
    ComboCell *cell = (ComboCell *)gnc_table_layout_get_cell (
        ledger->table->layout, cell_name);
    if (!cell)
    {
        ledger_close_finish (request, FALSE);
        ledger_close_request_unref (request);
        return;
    }
    const gchar *name = cell->cell.value ? cell->cell.value : "";
    if (g_strcmp0 (name, request->pending_account_name) != 0)
    {
        ledger_close_continue (request);
        ledger_close_request_unref (request);
        return;
    }
    GList *valid_types = NULL;
    valid_types = g_list_prepend (valid_types, GINT_TO_POINTER (ACCT_TYPE_CREDIT));
    valid_types = g_list_prepend (valid_types, GINT_TO_POINTER (ACCT_TYPE_ASSET));
    valid_types = g_list_prepend (valid_types, GINT_TO_POINTER (ACCT_TYPE_LIABILITY));
    valid_types = g_list_prepend (valid_types,
        GINT_TO_POINTER (ledger->is_cust_doc ? ACCT_TYPE_INCOME : ACCT_TYPE_EXPENSE));
    ledger_close_request_ref (request);
    gnc_ui_new_accounts_from_name_with_defaults_async (
        parent, name, valid_types, NULL, NULL,
        ledger_close_account_created, request);
    g_list_free (valid_types);
    ledger_close_request_unref (request); /* confirmation stage */
}

static void
ledger_close_tax_create_confirmed (GtkWindow *parent, gint response,
                                  gpointer user_data)
{
    GncEntryLedgerCloseRequest *request = user_data;
    GncEntryLedger *ledger = request->ledger;
    if (request->completed || !ledger_close_request_is_current (request))
    {
        ledger_close_finish (request, FALSE);
        ledger_close_request_unref (request);
        return;
    }
    ComboCell *cell = (ComboCell *)gnc_table_layout_get_cell (
        ledger->table->layout, ENTRY_TAXTABLE_CELL);
    if (g_strcmp0 (cell ? cell->cell.value : NULL,
                   request->tax_decided_name) != 0)
    {
        request->tax_creation_decided = FALSE;
        ledger_close_continue (request);
        ledger_close_request_unref (request);
        return;
    }
    request->tax_creation_decided = response != GTK_RESPONSE_YES;
    if (response != GTK_RESPONSE_YES)
    {
        /* Legacy traversal permits saving the entry without an unmatched
         * tax table when the user declines table creation. */
        ledger_close_continue (request);
        ledger_close_request_unref (request);
        return;
    }
    const gchar *name = cell ? cell->cell.value : NULL;
    if (!name || !*name)
    {
        ledger_close_continue (request);
        ledger_close_request_unref (request);
        return;
    }
    request->tax_creation_decided = FALSE;
    g_free (request->pending_tax_name);
    request->pending_tax_name = g_strdup (name);
    ledger_close_request_ref (request);
    gnc_ui_tax_table_new_from_name_async (
        parent, ledger->book, name, ledger_close_tax_table_created, request);
    ledger_close_request_unref (request);
}

static void
ledger_close_account_created (Account *account, gpointer user_data)
{
    GncEntryLedgerCloseRequest *request = user_data;
    GncEntryLedger *ledger = request->ledger;
    if (request->completed || !account || !ledger_close_request_is_current (request))
    {
        ledger_close_finish (request, FALSE);
        ledger_close_request_unref (request);
        return;
    }
    const gchar *cell_name =
        (ledger->type == GNCENTRY_INVOICE_ENTRY ||
         ledger->type == GNCENTRY_CUST_CREDIT_NOTE_ENTRY) ?
            ENTRY_IACCT_CELL : ENTRY_BACCT_CELL;
    ComboCell *cell = (ComboCell *)gnc_table_layout_get_cell (
        ledger->table->layout, cell_name);
    gchar *account_name = gnc_get_account_name_for_register (account);
    if (cell && g_strcmp0 (cell->cell.value, request->pending_account_name) == 0)
    {
        gnc_combo_cell_set_value (cell, account_name);
        gnc_basic_cell_set_changed (&cell->cell, TRUE);
    }
    g_clear_pointer (&request->pending_account_name, g_free);
    g_free (account_name);
    ledger_close_continue (request);
    ledger_close_request_unref (request); /* account creation stage */
}

static void
ledger_close_tax_table_created (GtkWindow *parent, GncTaxTable *table,
                                gpointer user_data)
{
    GncEntryLedgerCloseRequest *request = user_data;
    GncEntryLedger *ledger = request->ledger;
    if (request->completed || !ledger_close_request_is_current (request))
    {
        ledger_close_finish (request, FALSE);
        ledger_close_request_unref (request);
        return;
    }
    /* Cancellation historically leaves the unmatched text in the cell; the
     * model then saves a NULL table. An accepted editor canonicalizes it. */
    if (table)
    {
        request->tax_creation_decided = FALSE;
        ComboCell *cell = (ComboCell *)gnc_table_layout_get_cell (
            ledger->table->layout, ENTRY_TAXTABLE_CELL);
        if (cell && g_strcmp0 (cell->cell.value, request->pending_tax_name) == 0)
        {
            gnc_combo_cell_set_value (cell, gncTaxTableGetName (table));
            gnc_basic_cell_set_changed (&cell->cell, TRUE);
            g_free (request->tax_decided_name);
            request->tax_decided_name = g_strdup (cell->cell.value);
        }
    }
    else
    {
        request->tax_creation_decided = TRUE;
        g_free (request->tax_decided_name);
        request->tax_decided_name = g_strdup (request->pending_tax_name);
    }
    g_clear_pointer (&request->pending_tax_name, g_free);
    ledger_close_continue (request);
    ledger_close_request_unref (request); /* tax table creation stage */
}

static void
ledger_close_continue (GncEntryLedgerCloseRequest *request)
{
    GncEntryLedger *ledger = request->ledger;
    const gchar *account_cell_name = NULL;
    if (request->completed || !ledger_close_request_is_current (request))
    {
        ledger_close_finish (request, FALSE);
        return;
    }
    switch (ledger->type)
    {
    case GNCENTRY_INVOICE_ENTRY:
    case GNCENTRY_CUST_CREDIT_NOTE_ENTRY:
        account_cell_name = ENTRY_IACCT_CELL;
        break;
    case GNCENTRY_BILL_ENTRY:
    case GNCENTRY_EXPVOUCHER_ENTRY:
    case GNCENTRY_VEND_CREDIT_NOTE_ENTRY:
    case GNCENTRY_EMPL_CREDIT_NOTE_ENTRY:
        account_cell_name = ENTRY_BACCT_CELL;
        break;
    default:
        break;
    }
    if (account_cell_name && gnc_table_layout_get_cell_changed (
            ledger->table->layout, account_cell_name, FALSE))
    {
        ComboCell *cell = (ComboCell *)gnc_table_layout_get_cell (
            ledger->table->layout, account_cell_name);
        const gchar *name = cell && cell->cell.value ? cell->cell.value : "";
        Account *account = name && *name ? gnc_account_lookup_for_register (
            gnc_get_current_root_account (), name) : NULL;
        if (!account && name && *name)
            account = gnc_account_lookup_by_code (gnc_get_current_root_account (),
                                                   name);
        if (!account)
        {
            GtkWindow *parent = g_weak_ref_get (&request->parent);
            gchar *format = g_strdup_printf (_("The account %s does not exist. "
                                               "Would you like to create it?"), name);
            g_free (request->pending_account_name);
            request->pending_account_name = g_strdup (name ? name : "");
            ledger_close_request_ref (request);
            gnc_verify_dialog_async (parent, TRUE,
                                     ledger_close_account_create_confirmed,
                                     request, "%s", format);
            g_free (format);
            g_clear_object (&parent);
            return;
        }
    }
    if (gnc_table_layout_get_cell_changed (ledger->table->layout,
                                            ENTRY_TAXTABLE_CELL, FALSE))
    {
        ComboCell *cell = (ComboCell *)gnc_table_layout_get_cell (
            ledger->table->layout, ENTRY_TAXTABLE_CELL);
        const gchar *name = cell ? cell->cell.value : NULL;
        if ((!request->tax_creation_decided ||
             g_strcmp0 (request->tax_decided_name, name) != 0) && name && *name &&
            !gncTaxTableLookupByName (ledger->book, name))
        {
            GtkWindow *parent = g_weak_ref_get (&request->parent);
            gchar *format = g_strdup_printf (_("The tax table %s does not exist. "
                                               "Would you like to create it?"), name);
            g_free (request->tax_decided_name);
            request->tax_decided_name = g_strdup (name);
            request->tax_creation_decided = FALSE;
            ledger_close_request_ref (request);
            gnc_verify_dialog_async (parent, TRUE,
                                     ledger_close_tax_create_confirmed,
                                     request, "%s", format);
            g_free (format);
            g_clear_object (&parent);
            return;
        }
    }
    if (!ledger_close_async_preflight (ledger))
    {
        ledger_close_finish (request, FALSE);
        return;
    }
    GncEntry *entry = gnc_entry_ledger_get_current_entry (ledger);
    if ((ledger->type == GNCENTRY_INVOICE_ENTRY ||
         ledger->type == GNCENTRY_CUST_CREDIT_NOTE_ENTRY) && entry &&
        gncEntryGetOrder (entry) && !request->order_change_confirmed)
    {
        request->order_confirmation_pending = TRUE;
        ledger_close_request_ref (request);
        gnc_verify_dialog_async (GTK_WINDOW (ledger->parent), TRUE,
                                 ledger_close_response, request,
                                 "%s", _("The current entry belongs to an existing order. "
                                           "Do you want to record the change?"));
        return;
    }
    if (request->save_confirmed)
    {
        if (!ledger_close_request_is_current (request) ||
            !gnc_entry_ledger_verify_can_save (ledger))
        {
            ledger_close_finish (request, FALSE);
            return;
        }
        gboolean accepted;
        gnc_suspend_gui_refresh ();
        accepted = gnc_entry_ledger_save (ledger, TRUE);
        gnc_resume_gui_refresh ();
        ledger_close_finish (request, accepted);
        return;
    }
    ledger_close_request_ref (request);
    gnc_verify_dialog_async (GTK_WINDOW (ledger->parent), TRUE,
                             ledger_close_response, request, "%s",
                             _("The current entry has been changed. Would you like to save it?"));
}

void
gnc_entry_ledger_check_close_async (GtkWidget *parent,
                                    GncEntryLedger *ledger,
                                    GncEntryLedgerCloseCallback callback,
                                    gpointer user_data)
{
    GncEntryLedgerCloseRequest *request;
    GncEntry *entry;
    g_return_if_fail (GTK_IS_WIDGET (parent));
    g_return_if_fail (callback != NULL);
    if (!ledger)
    {
        callback (TRUE, user_data);
        return;
    }
    if (ledger->parent != parent || ledger->async_close_requests)
    {
        callback (FALSE, user_data);
        return;
    }
    if (!gnc_entry_ledger_changed (ledger))
    {
        callback (TRUE, user_data);
        return;
    }
    entry = gnc_entry_ledger_get_current_entry (ledger);
    if (!entry)
    {
        callback (FALSE, user_data);
        return;
    }
    request = g_new0 (GncEntryLedgerCloseRequest, 1);
    request->refs = 2; /* ledger registration and dialog response */
    request->ledger = ledger;
    request->book = ledger->book;
    request->session = gnc_get_current_session ();
    request->entry_guid = *gncEntryGetGUID (entry);
    request->cursor = ledger->table->current_cursor_loc;
    request->callback = callback;
    request->user_data = user_data;
    g_weak_ref_init (&request->parent, G_OBJECT (parent));
    g_object_add_weak_pointer (G_OBJECT (request->book),
                               (gpointer *)&request->book);
    request->parent_signal_object = parent;
    g_signal_connect (parent, "destroy",
                      G_CALLBACK (ledger_close_parent_destroyed), request);
    ledger->async_close_requests =
        g_list_prepend (ledger->async_close_requests, request);

    ledger_close_continue (request);
    ledger_close_request_unref (request); /* initial chain reference */
}

void
gnc_entry_ledger_commit_entry_async (GtkWidget *parent,
                                     GncEntryLedger *ledger,
                                     GncEntryLedgerCloseCallback callback,
                                     gpointer user_data)
{
    GncEntryLedgerCloseRequest *request;
    GncEntry *entry;
    g_return_if_fail (GTK_IS_WIDGET (parent));
    g_return_if_fail (callback != NULL);
    if (!ledger || !gnc_entry_ledger_changed (ledger))
    {
        callback (ledger != NULL, user_data);
        return;
    }
    if (ledger->parent != parent || ledger->async_close_requests ||
        !gnc_entry_ledger_verify_can_save (ledger))
    {
        callback (FALSE, user_data);
        return;
    }
    entry = gnc_entry_ledger_get_current_entry (ledger);
    if (!entry)
    {
        callback (FALSE, user_data);
        return;
    }
    request = g_new0 (GncEntryLedgerCloseRequest, 1);
    request->refs = 2;
    request->ledger = ledger;
    request->book = ledger->book;
    request->session = gnc_get_current_session ();
    request->entry_guid = *gncEntryGetGUID (entry);
    request->cursor = ledger->table->current_cursor_loc;
    request->callback = callback;
    request->user_data = user_data;
    request->save_confirmed = TRUE;
    g_weak_ref_init (&request->parent, G_OBJECT (parent));
    g_object_add_weak_pointer (G_OBJECT (request->book),
                               (gpointer *)&request->book);
    request->parent_signal_object = parent;
    g_signal_connect (parent, "destroy",
                      G_CALLBACK (ledger_close_parent_destroyed), request);
    ledger->async_close_requests = g_list_prepend (ledger->async_close_requests,
                                                    request);
    ledger_close_continue (request);
    ledger_close_request_unref (request);
}

void
gnc_entry_ledger_cancel_async_close_requests (GncEntryLedger *ledger)
{
    while (ledger && ledger->async_close_requests)
        ledger_close_finish (ledger->async_close_requests->data, FALSE);
}
