/*
 * gncEntryLedgerControl.c -- Control for GncEntry ledger
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#define _GNU_SOURCE

#include "config.h"

#include <glib.h>

#include "gncEntryLedgerP.h"
#include "gncEntryLedgerControl.h"

static void gnc_entry_ledger_move_cursor (VirtualLocation *p_new_virt_loc,
					  gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  GncEntry *entry;

  if (!ledger) return;

}

static gboolean gnc_entry_ledger_traverse (VirtualLocation *p_new_virt_loc,
					   gncTableTraversalDir dir,
					   gpointer user_data)
{
  GncEntryLedger *ledger = user_data;

  if (!ledger) return FALSE;

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
