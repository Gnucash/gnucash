/*
 * gncEntryLedger.h -- a ledger widget for manipulating GncEntry's
 * Copyright (C) 2001, 2003 Derek Atkins
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

#ifndef GNC_ENTRY_LEDGER_H
#define GNC_ENTRY_LEDGER_H

#include "qof.h"
#include "gncEntry.h"
#include "gncOrder.h"
#include "table-allgui.h"
/** @ingroup Register
 * @addtogroup BusinessRegister Business Register
 * @brief A specialized register for Accounts Payable and Accounts Receivable.
 * @{
 */
/** @file gncEntryLedger.h
 */
typedef enum
{
    GNC_ENTRY_ORDER_NONE = -1, //Force clang to use int representation of enum.
    GNCENTRY_ORDER_ENTRY,
    GNCENTRY_ORDER_VIEWER,
    GNCENTRY_INVOICE_ENTRY,
    GNCENTRY_INVOICE_VIEWER,
    GNCENTRY_CUST_CREDIT_NOTE_ENTRY,
    GNCENTRY_CUST_CREDIT_NOTE_VIEWER,
    GNCENTRY_BILL_ENTRY,
    GNCENTRY_BILL_VIEWER,
    GNCENTRY_VEND_CREDIT_NOTE_ENTRY,
    GNCENTRY_VEND_CREDIT_NOTE_VIEWER,
    GNCENTRY_EXPVOUCHER_ENTRY,
    GNCENTRY_EXPVOUCHER_VIEWER,
    GNCENTRY_EMPL_CREDIT_NOTE_ENTRY,
    GNCENTRY_EMPL_CREDIT_NOTE_VIEWER,
    GNCENTRY_NUM_REGISTER_TYPES
} GncEntryLedgerType;

#define ENTRY_IACCT_CELL	"inv-account"
#define ENTRY_BACCT_CELL	"bill-account"
#define ENTRY_ACTN_CELL		"action"
#define ENTRY_DATE_CELL		"date"
#define ENTRY_DESC_CELL		"description"
#define ENTRY_DISC_CELL		"discount"
#define ENTRY_DISTYPE_CELL	"discount-type"
#define ENTRY_DISHOW_CELL	"discount-how"
#define ENTRY_PRIC_CELL		"price"
#define ENTRY_QTY_CELL		"quantity"
#define ENTRY_TAXABLE_CELL	"istaxable"
#define ENTRY_TAXTABLE_CELL	"taxtable"
#define ENTRY_TAXINCLUDED_CELL	"taxincluded"
#define ENTRY_BILLABLE_CELL	"isbillable"

#define ENTRY_INV_CELL		"isinvoiced"
#define ENTRY_VALUE_CELL	"line-value"
#define ENTRY_TAXVAL_CELL	"line-tax-val"

#define ENTRY_PAYMENT_CELL	"payment"

typedef struct GncEntryLedger_s GncEntryLedger;

/* Prototypes ***************************************************/

/** Create and return a new GncEntry Ledger */
GncEntryLedger * gnc_entry_ledger_new (QofBook *book, GncEntryLedgerType type);

/** Set the default order for this ledger */
void gnc_entry_ledger_set_default_order (GncEntryLedger *ledger,
        GncOrder *order);

/** Set the default invoice for this ledger */
void gnc_entry_ledger_set_default_invoice (GncEntryLedger *ledger,
        GncInvoice *invoice);

/** Destroy the GncEntry Ledger */
void gnc_entry_ledger_destroy (GncEntryLedger *ledger);

/** Returns the Entry where the cursor is currently located. */
GncEntry * gnc_entry_ledger_get_current_entry (GncEntryLedger *ledger);

/** Copy GncEntry information from the list to the rows of the Ledger. */
void gnc_entry_ledger_load (GncEntryLedger *ledger, GList *entry_list);

void gnc_entry_ledger_display_refresh (GncEntryLedger *ledger);

/** Get the Table */
Table * gnc_entry_ledger_get_table (GncEntryLedger *ledger);

void gnc_entry_ledger_set_parent (GncEntryLedger *ledger, GtkWidget *parent);

void gnc_entry_ledger_set_readonly (GncEntryLedger *ledger, gboolean readonly);

gboolean gnc_entry_ledger_changed (GncEntryLedger *ledger);

void gnc_entry_ledger_cancel_cursor_changes (GncEntryLedger *ledger);

/** This will act just like hitting 'return' to record an entry */
gboolean gnc_entry_ledger_commit_entry (GncEntryLedger *ledger);

/** This will ask the user if they really want to make a change */
gboolean gnc_entry_ledger_check_close (GtkWidget *parent, GncEntryLedger *ledger);

void gnc_entry_ledger_reset_query (GncEntryLedger *ledger);

/** Returns the GncEntry at the given location, or NULL if the
 * location is not valid. */
GncEntry * gnc_entry_ledger_get_entry (GncEntryLedger *ledger,
                                       VirtualCellLocation vcell_loc);

/** Returns the GncEntry that represents the blank new line at the
 * bottom of the ledger */
GncEntry * gnc_entry_ledger_get_blank_entry (GncEntryLedger *ledger);

/** Looks up the cell location of the given "entry" and writes the
 * location into the variable pointed to by vcell_loc (which must be
 * non-NULL). Returns TRUE if the entry was found, otherwise FALSE. */
gboolean gnc_entry_ledger_get_entry_virt_loc (GncEntryLedger *ledger,
        const GncEntry *entry,
        VirtualCellLocation *vcell_loc);

void gnc_entry_ledger_delete_current_entry (GncEntryLedger *ledger);
void gnc_entry_ledger_duplicate_current_entry (GncEntryLedger *ledger);

/** This implements the command of moving the current entry (where the
 * cursor is currently located) one row upwards or downwards,
 * effectively swapping this row and the other row. If the other row
 * is empty (or it is the blank entry), nothing will happen.
 *
 * \param move_up If TRUE, the current entry is moved upwards,
 * otherwise downwards. */
void gnc_entry_ledger_move_current_entry_updown (GncEntryLedger *ledger,
        gboolean move_up);

QofQuery * gnc_entry_ledger_get_query (GncEntryLedger *ledger);

void gnc_entry_ledger_set_prefs_group (GncEntryLedger *ledger, const gchar *string);
/** @} */
#endif /* GNC_ENTRY_LEDGER_H */
