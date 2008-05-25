/*
 * gncEntryLedgerDisplay.c -- handle the display management for an Entry Ledger
 * Copyright (C) 2002, 2003 Derek Atkins
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

#include "gnc-ui-util.h"
#include "gnc-component-manager.h"
#include "gnc-event.h"
#include "gnc-gconf-utils.h"

#include "gncEntry.h"
#include "gncEntryLedger.h"
#include "gncEntryLedgerP.h"

#define ENTRYLEDGER_CLASS	"entry-ledger-class"

/* Return the list of entries (NOTE: Should use a query here!) */
static GList *
gnc_entry_ledger_get_entries (GncEntryLedger *ledger)
{
  if (ledger->query)
    return gncQueryRun (ledger->query);

  //  g_warning ("No query to run?");
  return NULL;
}

static void
gnc_entry_ledger_refresh_internal (GncEntryLedger *ledger, GList *entries)
{
  if (!ledger || ledger->loading)
    return;

  /* If not full refresh ok, just load the xfer cells */
  if (!ledger->full_refresh) {
    gnc_entry_ledger_load_xfer_cells (ledger);
    return;
  }

  /* Viewers must always have at least one entry! */
  if ((ledger->type == GNCENTRY_ORDER_VIEWER ||
       ledger->type == GNCENTRY_INVOICE_VIEWER ||
       ledger->type == GNCENTRY_BILL_VIEWER ||
       ledger->type == GNCENTRY_EXPVOUCHER_VIEWER) && !entries)
    return;

  ledger->loading = TRUE;
  gnc_entry_ledger_load (ledger, entries);
  ledger->loading = FALSE;
}

static void
gnc_entry_ledger_gconf_changed (GConfEntry *entry, gpointer user_data)
{
  GncEntryLedger *ledger = user_data;

  g_return_if_fail (ledger && entry && entry->key);

  if (g_str_has_suffix (entry->key, KEY_ACCOUNT_SEPARATOR)) {
    gnc_entry_ledger_display_refresh (ledger);
  } else {
    g_warning ("gnc_entry_gconf_changed: Unknown gconf key %s", entry->key);
  }
}

static void
gnc_entry_ledger_set_watches (GncEntryLedger *ledger, GList *entries)
{
  GList *node;
  GNCIdType type = NULL;

  gnc_gui_component_clear_watches (ledger->component_id);

  switch (ledger->type) {
  case GNCENTRY_ORDER_ENTRY:
  case GNCENTRY_ORDER_VIEWER:
    type = GNC_ORDER_MODULE_NAME;
    break;

  case GNCENTRY_INVOICE_ENTRY:
    /* Watch the invoice owner to see when items get added via orders */
    gnc_gui_component_watch_entity (ledger->component_id,
				    gncOwnerGetGUID
				    (gncInvoiceGetOwner (ledger->invoice)),
				    QOF_EVENT_MODIFY);
  case GNCENTRY_INVOICE_VIEWER:
  case GNCENTRY_BILL_ENTRY:
  case GNCENTRY_BILL_VIEWER:
  case GNCENTRY_EXPVOUCHER_ENTRY:
  case GNCENTRY_EXPVOUCHER_VIEWER:
    type = GNC_INVOICE_MODULE_NAME;
    break;

  default:
    g_warning ("Invalid ledger type");
    break;
  }

  gnc_gui_component_watch_entity_type (ledger->component_id,
                                       type,
                                       QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

  /* To make sure the xfer cell is up to date */
  gnc_gui_component_watch_entity_type (ledger->component_id,
                                       GNC_ID_ACCOUNT,
                                       QOF_EVENT_MODIFY | QOF_EVENT_DESTROY
				       | GNC_EVENT_ITEM_CHANGED);

  /* To make sure the taxtable cell is up to date */
  gnc_gui_component_watch_entity_type (ledger->component_id,
                                       GNC_TAXTABLE_MODULE_NAME,
                                       QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

  /* For expense vouchers, watch the employee and refresh if it's changed */
  if (ledger->type == GNCENTRY_EXPVOUCHER_ENTRY) {
    GncOwner *owner = gncOwnerGetEndOwner (gncInvoiceGetOwner (ledger->invoice));
    GncEmployee *employee = gncOwnerGetEmployee (owner);

    if (employee)
      gnc_gui_component_watch_entity (ledger->component_id,
				      gncEmployeeGetGUID (employee),
				      QOF_EVENT_MODIFY);
  }

  for (node = entries; node; node = node->next)
  {
    GncEntry *entry = node->data;
    gnc_gui_component_watch_entity (ledger->component_id,
                                    gncEntryGetGUID (entry),
                                    QOF_EVENT_MODIFY);
  }
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
  GncEntryLedger *ledger = user_data;

  gnc_entry_ledger_display_refresh (ledger);
}

void
gnc_entry_ledger_display_init (GncEntryLedger *ledger)
{
  if (!ledger) return;

  ledger->full_refresh = TRUE;
  ledger->component_id = gnc_register_gui_component (ENTRYLEDGER_CLASS,
						     refresh_handler,
						     NULL, ledger);
  gnc_gconf_general_register_cb(KEY_ACCOUNT_SEPARATOR,
				gnc_entry_ledger_gconf_changed, ledger);

  gnc_entry_ledger_display_refresh (ledger);
}

void
gnc_entry_ledger_display_fini (GncEntryLedger *ledger)
{
  if (!ledger) return;

  gnc_unregister_gui_component (ledger->component_id);
  gnc_gconf_general_remove_cb(KEY_ACCOUNT_SEPARATOR,
			      gnc_entry_ledger_gconf_changed, ledger);
}

void
gnc_entry_ledger_display_refresh (GncEntryLedger *ledger)
{
  GList *entries;

  if (!ledger || ledger->loading) return;

  entries = gnc_entry_ledger_get_entries (ledger);

  gnc_entry_ledger_set_watches (ledger, entries);

  gnc_entry_ledger_refresh_internal (ledger, entries);
}
