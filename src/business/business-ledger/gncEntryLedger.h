/*
 * gncEntryLedger.h -- a ledger widget for manipulating GncEntry's
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_ENTRY_LEDGER_H
#define GNC_ENTRY_LEDGER_H

#include "QueryNew.h"
#include "gncEntry.h"
#include "gncOrder.h"
#include "gnc-book.h"
#include "table-allgui.h"

typedef enum {
  GNCENTRY_ORDER_ENTRY,
  GNCENTRY_ORDER_VIEWER,
  GNCENTRY_INVOICE_ENTRY,
  GNCENTRY_INVOICE_VIEWER,
  GNCENTRY_BILL_ENTRY,
  GNCENTRY_BILL_VIEWER,
  GNCENTRY_NUM_REGISTER_TYPES
} GncEntryLedgerType;

typedef struct entry_ledger_colors
{
  guint32 header_bg_color;

  guint32 primary_bg_color;
  guint32 secondary_bg_color;

  guint32 primary_active_bg_color;
  guint32 secondary_active_bg_color;
} GncEntryLedgerColors;

#define ENTRY_ACCT_CELL		"account"
#define ENTRY_ACTN_CELL		"action"
#define ENTRY_DATE_CELL		"date"
#define ENTRY_DESC_CELL		"description"
#define ENTRY_DISC_CELL		"discount"
#define ENTRY_DISTYPE_CELL	"discount-type"
#define ENTRY_DISHOW_CELL	"discount-how"
#define ENTRY_PRIC_CELL		"price"
#define ENTRY_QTY_CELL		"quantity"
#define ENTRY_TAXABLE_CELL	"taxable"
#define ENTRY_TAXTABLE_CELL	"taxtable"
#define ENTRY_TAXINCLUDED_CELL	"taxincluded"

#define ENTRY_INV_CELL		"invoiced-p"
#define ENTRY_VALUE_CELL	"line-value"
#define ENTRY_TAXVAL_CELL	"line-tax-val"

typedef struct GncEntryLedger_s GncEntryLedger;

/** Prototypes ***************************************************/

/* Create and return a new GncEntry Ledger */
GncEntryLedger * gnc_entry_ledger_new (GNCBook *book, GncEntryLedgerType type);

/* Set the default order for this ledger */
void gnc_entry_ledger_set_default_order (GncEntryLedger *ledger,
					 GncOrder *order);

/* Set the default invoice for this ledger */
void gnc_entry_ledger_set_default_invoice (GncEntryLedger *ledger,
					   GncInvoice *invoice);

/* Destroy the GncEntry Ledger */
void gnc_entry_ledger_destroy (GncEntryLedger *ledger);

/* Returns the Entry where the cursor is currently located. */
GncEntry * gnc_entry_ledger_get_current_entry (GncEntryLedger *ledger);

/* Copy GncEntry information from the list to the rows of the Ledger. */
void gnc_entry_ledger_load (GncEntryLedger *ledger, GList *entry_list);

void gnc_entry_ledger_display_refresh (GncEntryLedger *ledger);

/* Get the Table */
Table * gnc_entry_ledger_get_table (GncEntryLedger *ledger);

void gnc_entry_ledger_set_colors (GncEntryLedgerColors reg_colors_new);

void gnc_entry_ledger_set_parent (GncEntryLedger *ledger, gncUIWidget parent);

void gnc_entry_ledger_set_readonly (GncEntryLedger *ledger);

gboolean gnc_entry_ledger_changed (GncEntryLedger *ledger);

void gnc_entry_ledger_cancel_cursor_changes (GncEntryLedger *ledger);

/* This will act just like hitting 'return' to record an entry */
gboolean gnc_entry_ledger_commit_entry (GncEntryLedger *ledger);

/* This will ask the user if they really want to make a change */
gboolean gnc_entry_ledger_check_close (GtkWidget *parent, GncEntryLedger *ledger);

void gnc_entry_ledger_reset_query (GncEntryLedger *ledger);

GncEntry * gnc_entry_ledger_get_blank_entry (GncEntryLedger *ledger);

gboolean gnc_entry_ledger_get_entry_virt_loc (GncEntryLedger *ledger,
					      GncEntry *entry,
					      VirtualCellLocation *vcell_loc);

void gnc_entry_ledger_delete_current_entry (GncEntryLedger *ledger);
void gnc_entry_ledger_duplicate_current_entry (GncEntryLedger *ledger);

QueryNew * gnc_entry_ledger_get_query (GncEntryLedger *ledger);

#endif /* GNC_ENTRY_LEDGER_H */
