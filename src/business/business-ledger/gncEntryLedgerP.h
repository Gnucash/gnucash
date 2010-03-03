/*
 * gncEntryLedgerP.h -- a ledger widget for manipulating GncEntry's
 * Copyright (C) 2001, 2002 Derek Atkins
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

#ifndef GNC_ENTRY_LEDGERP_H
#define GNC_ENTRY_LEDGERP_H

#include "qof.h"
#include "table-allgui.h"
#include "gncEntryLedger.h"

struct GncEntryLedger_s
{
    GUID		blank_entry_guid;
    gboolean	blank_entry_edited;
    gboolean      traverse_to_new;

    gboolean	loading;	/* To keep from recursing from events */
    gboolean	full_refresh;	/* Is a full refresh ok? */
    gint		component_id;	/* To register for events */

    Timespec	last_date_entered;

    GncEntry *	hint_entry;	/* A Hint for where to display */

    gncUIWidget	parent;
    QofBook *	book;
    Table *	table;
    GncOrder *	order;
    GncInvoice *	invoice;
    QueryNew *	query;

    GncEntryLedgerType type;

    gboolean	is_invoice;	/* is this an invoice (or a bill)? */

    const gchar * gconf_section;
};

GncEntry * gnc_entry_ledger_get_entry (GncEntryLedger *ledger,
                                       VirtualCellLocation vcell_loc);
Account * gnc_entry_ledger_get_account_by_name (GncEntryLedger *ledger, BasicCell * bcell,
        const char *name, gboolean *new);
Account * gnc_entry_ledger_get_account (GncEntryLedger *ledger,
                                        const char * cell_name);
GncTaxTable * gnc_entry_ledger_get_taxtable (GncEntryLedger *ledger,
        const char *cell_name);

gint gnc_entry_ledger_get_type (GncEntryLedger *ledger,
                                const char * cell_name);
gboolean gnc_entry_ledger_get_checkmark (GncEntryLedger *ledger,
        const char * cell_name);
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
