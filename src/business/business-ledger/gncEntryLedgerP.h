/*
 * gncEntryLedgerP.h -- a ledger widget for manipulating GncEntry's
 * Copyright (C) 2001, 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_ENTRY_LEDGERP_H
#define GNC_ENTRY_LEDGERP_H

#include "guid.h"
#include "gnc-book.h"
#include "table-allgui.h"
#include "gncEntryLedger.h"

struct GncEntryLedger_s {
  GUID		blank_entry_guid;
  gboolean	blank_entry_edited;
  gboolean      traverse_to_new;

  gboolean	loading;	/* To keep from recursing from events */
  gboolean	full_refresh;	/* Is a full refresh ok? */
  gint		component_id;	/* To register for events */

  Timespec	last_date_entered;

  GncEntry *	hint_entry;	/* A Hint for where to display */

  gncUIWidget	parent;
  GNCBook *	book;
  Table *	table;
  GncOrder *	order;
  GncInvoice *	invoice;
  QueryNew *	query;
  
  GncEntryLedgerType type;
};

GncEntry * gnc_entry_ledger_get_entry (GncEntryLedger *ledger,
				       VirtualCellLocation vcell_loc);
Account * gnc_entry_ledger_get_account (GncEntryLedger *ledger,
					const char * cell_name);
GncTaxTable * gnc_entry_ledger_get_taxtable (GncEntryLedger *ledger,
					     const char *cell_name);

gint gnc_entry_ledger_get_type (GncEntryLedger *ledger,
				const char * cell_name);
gboolean gnc_entry_ledger_get_checkmark (GncEntryLedger *ledger,
					 const char * cell_name);
char gnc_entry_ledger_get_inv (GncEntryLedger *ledger, const char * cell_name);
gboolean gnc_entry_ledger_get_numeric (GncEntryLedger *ledger,
				       const char *cell_name,
				       gnc_numeric *value);

const char * gnc_entry_ledger_type_string_getter (char flag);
const char * gnc_entry_ledger_how_string_getter (char flag);

gboolean gnc_entry_ledger_find_entry (GncEntryLedger *ledger, GncEntry *entry,
				      VirtualCellLocation *vcell_loc);

void gnc_entry_ledger_load_xfer_cells (GncEntryLedger *ledger);

void gnc_entry_ledger_display_init (GncEntryLedger *ledger);
void gnc_entry_ledger_display_fini (GncEntryLedger *ledger);

void gnc_entry_ledger_compute_value (GncEntryLedger *ledger,
				     gnc_numeric *value,
				     gnc_numeric *tax_value);

#endif /* GNC_ENTRY_LEDGERP_H */
