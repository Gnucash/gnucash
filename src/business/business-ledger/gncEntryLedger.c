/*
 * gncEntryLedger.c -- a Ledger widget for entering GncEntry objects
 * Copyright (C) 2001, 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#define _GNU_SOURCE

#include "config.h"

#include <glib.h>

#include "Account.h"
#include "gnc-ui-util.h"
#include "combocell.h"
#include "pricecell.h"
#include "recncell.h"
#include "messages.h"

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
    gncEntryDestroy (entry);

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
		 

Account * gnc_entry_ledger_get_account (GncEntryLedger *ledger,
					const char * cell_name)
{
  const char * name =
    gnc_table_layout_get_cell_value (ledger->table->layout, cell_name);

  return xaccGetAccountFromFullName (gnc_book_get_group (ledger->book),
				     name,
				     gnc_get_account_separator ());
}

char gnc_entry_ledger_get_inv (GncEntryLedger *ledger, const char * cell_name)
{
  const char *value =
    gnc_table_layout_get_cell_value (ledger->table->layout, cell_name);

  if (value)
    return *value;
  return '\0';
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
  GUID *guid;

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
    ((PriceCell *)
     gnc_table_layout_get_cell (ledger->table->layout, ENTRY_TAX_CELL),
     1000000);

  gnc_price_cell_set_fraction
    ((PriceCell *) gnc_table_layout_get_cell (ledger->table->layout,
					      ENTRY_QTY_CELL),
     1000000);

  /* add menu items for the action cell */
  gnc_entry_ledger_config_action (ledger);
}

/* Create and return a new GncEntry Ledger */
GncEntryLedger * gnc_entry_ledger_new (GNCBook *book, GncEntryLedgerType type)
{
  GncEntryLedger *ledger;

  if (!book) return NULL;
  if (type < 0 || type >= GNCENTRY_NUM_REGISTER_TYPES) return NULL;

  ledger = g_new0 (GncEntryLedger, 1);
  ledger->type = type;
  ledger->book = book;

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

  if (!ledger->query && order) {
    ledger->query = gncQueryCreate ();
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

  if (!ledger->invoice)
    return;

  if (ledger->query)
    gncQueryDestroy (ledger->query);

  /* Match:
   *
   * 1. book AND
   * 2.   ( Entry->Invoice == ledger->invoice OR
   * 3.      ( Entry->Invoice == NULL AND
   *           Entry->Order->real-parent == Invoice->parent ) )
   *
   * Note that term 3 is only for Editable invoices where the 'owner'
   * is valid.
   */
  
  /* Term 1 */
  ledger->query = gncQueryCreate ();
  gncQuerySetBook (ledger->query, gncInvoiceGetBook (ledger->invoice));

  /* Term 2 */
  q = gncQueryCreate ();
  gncQueryAddGUIDMatch (q,
			g_slist_prepend (g_slist_prepend (NULL,
							  QUERY_PARAM_GUID),
					 ENTRY_INVOICE),
			gncInvoiceGetGUID (ledger->invoice), QUERY_OR);

  /* Term 3 */
  if (ledger->type == GNCENTRY_INVOICE_ENTRY &&
      gncOwnerGetEndGUID (gncInvoiceGetOwner (ledger->invoice)) != NULL) {
    QueryNew *q2 = gncQueryCreate ();

    /* entry_invoice->invoice_guid == NULL */
    gncQueryAddGUIDMatch (q2,
			  g_slist_prepend (g_slist_prepend (NULL,
							    QUERY_PARAM_GUID),
					   ENTRY_INVOICE),
			  NULL, QUERY_OR);

    /* entry_order->owner->end_guid == invoice->owner->guid */
    gncQueryAddGUIDMatch (q2,
			  g_slist_prepend
			  (g_slist_prepend
			   (g_slist_prepend (NULL, OWNER_PARENTG),
			    ORDER_OWNER), ENTRY_ORDER),
			  gncOwnerGetGUID (gncInvoiceGetOwner
					   (ledger->invoice)),
			  QUERY_AND);
    
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

  for (v_row = 1; v_row < table->num_virt_rows; v_row++) {
    VirtualCellLocation vc_loc = { v_row, 0 };

    e = gnc_entry_ledger_get_entry (ledger, vc_loc);

    if (e == entry) {
      if (vcell_loc != NULL)
	*vcell_loc = vc_loc;
      return TRUE;
    }
  }
  return FALSE;
}

void gnc_entry_ledger_set_readonly (GncEntryLedger *ledger)
{
  if (!ledger) return;

  /* reset the ledger type to a viewer */
  switch (ledger->type) {
  case GNCENTRY_ORDER_ENTRY:
    ledger->type = GNCENTRY_ORDER_VIEWER;
    break;
  case GNCENTRY_INVOICE_ENTRY:
    ledger->type = GNCENTRY_INVOICE_VIEWER;
    create_invoice_query (ledger);
    break;
  default:
    return;			/* Nothing to do */
  }

  /* reset the model */
  gnc_table_model_set_read_only (ledger->table->model, TRUE);

  /* get rid of the blank entry, if it exists */
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
  gnc_numeric qty, price, discount, tax;
  gint disc_type, tax_type;

  gnc_entry_ledger_get_numeric (ledger, ENTRY_QTY_CELL, &qty);
  gnc_entry_ledger_get_numeric (ledger, ENTRY_PRIC_CELL, &price);
  gnc_entry_ledger_get_numeric (ledger, ENTRY_DISC_CELL, &discount);
  gnc_entry_ledger_get_numeric (ledger, ENTRY_TAX_CELL, &tax);

  disc_type = gnc_entry_ledger_get_type (ledger, ENTRY_DISTYPE_CELL);
  tax_type = gnc_entry_ledger_get_type (ledger, ENTRY_TAXTYPE_CELL);

  gncEntryComputeValue (qty, price, tax, tax_type, discount, disc_type,
			  value, tax_value);
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
