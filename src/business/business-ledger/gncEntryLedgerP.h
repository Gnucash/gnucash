/*
 * gncEntryLedgerP.h -- a ledger widget for manipulating GncEntry's
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_ENTRY_LEDGERP_H
#define GNC_ENTRY_LEDGERP_H

#include "gnc-book.h"
#include "table-allgui.h"
#include "gncEntryLedger.h"

struct GncEntryLedger_s {
  GNCBook *book;
  Table *table;
  GncEntryLedgerType type;
};

GncEntry * gnc_entry_ledger_get_entry (GncEntryLedger *ledger,
				       VirtualCellLocation vcell_loc);
Account * gnc_entry_ledger_get_account (GncEntryLedger *ledger,
					const char * cell_name);
gint gnc_entry_ledger_get_type (GncEntryLedger *ledger, const char * cell_name);

#endif /* GNC_ENTRY_LEDGERP_H */
