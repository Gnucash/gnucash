/*
 * gncEntryLedgerP.h -- a ledger widget for manipulating GncEntry's
 * Copyright (C) 2001 Derek Atkins
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

  Timespec	last_date_entered;

  gncUIWidget	parent;
  GNCBook *	book;
  Table *	table;
  GncOrder *	order;
  GncEntryLedgerType type;
};

GncEntry * gnc_entry_ledger_get_entry (GncEntryLedger *ledger,
				       VirtualCellLocation vcell_loc);
Account * gnc_entry_ledger_get_account (GncEntryLedger *ledger,
					const char * cell_name);
gint gnc_entry_ledger_get_type (GncEntryLedger *ledger, const char * cell_name);

const char * gnc_entry_ledger_type_string_getter (char flag);

gboolean gnc_entry_ledger_find_entry (GncEntryLedger *ledger, GncEntry *entry,
				      VirtualCellLocation *vcell_loc);

void gnc_entry_ledger_redraw (GncEntryLedger *ledger);

#endif /* GNC_ENTRY_LEDGERP_H */
