/*
 * gncEntryLedgerModel.c -- Model for GncEntry ledger
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#define _GNU_SOURCE

#include "config.h"

#include <glib.h>

#include "messages.h"
#include "Account.h"
#include "gnc-ui-util.h"

#include "datecell.h"
#include "pricecell.h"

#include "gncEntryLedgerP.h"
#include "gncEntryLedgerModel.h"

static GncEntryLedgerColors reg_colors =
{
  0xffffff, /* white */
  0xffffff,
  0xffffff,

  0xffffff,
  0xffffff
};

/** Private Interfaces ***********************************************/

/* GET_LABEL */

static const char * get_acct_label (VirtualLocation virt_loc, gpointer data)
{
  return _("Account");
}

static const char * get_actn_label (VirtualLocation virt_loc, gpointer data)
{
  return _("Action");
}

static const char * get_date_label (VirtualLocation virt_loc, gpointer data)
{
  return _("Date");
}

static const char * get_desc_label (VirtualLocation virt_loc, gpointer data)
{
  return _("Description");
}

static const char * get_disc_label (VirtualLocation virt_loc, gpointer data)
{
  return _("Discount");
}

static const char * get_distype_label (VirtualLocation virt_loc, gpointer data)
{
  return _("DT");
}

static const char * get_pric_label (VirtualLocation virt_loc, gpointer data)
{
  return _("Price");
}

static const char * get_qty_label (VirtualLocation virt_loc, gpointer data)
{
  return _("Quantity");
}

static const char * get_taxacc_label (VirtualLocation virt_loc, gpointer data)
{
  return _("Tax Account");
}

static const char * get_taxtype_label (VirtualLocation virt_loc, gpointer data)
{
  return _("TT");
}

static const char * get_tax_label (VirtualLocation virt_loc, gpointer data)
{
  return _("Tax");
}

/* GET_ENTRY */

static const char * get_acct_entry (VirtualLocation virt_loc,
				    gboolean translate,
				    gboolean *conditionally_changed,
				    gpointer user_data)
{
  static char *name = NULL;

  GncEntryLedger *ledger = user_data;
  GncEntry *entry;

  entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);

  g_free (name);
  name = xaccAccountGetFullName (gncEntryGetAccount (entry),
				 gnc_get_account_separator ());
  return name;
}

static const char * get_actn_entry (VirtualLocation virt_loc,
				    gboolean translate,
				    gboolean *conditionally_changed,
				    gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  GncEntry *entry;

  entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
  return gncEntryGetAction  (entry);
}

static const char * get_date_entry (VirtualLocation virt_loc,
				    gboolean translate,
				    gboolean *conditionally_changed,
				    gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  GncEntry *entry;
  Timespec ts;

  entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);

  ts = gncEntryGetDate (entry);
  return gnc_print_date (ts);
}

static const char * get_desc_entry (VirtualLocation virt_loc,
				    gboolean translate,
				    gboolean *conditionally_changed,
				    gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  GncEntry *entry;

  entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
  return gncEntryGetDescription (entry);
}

static const char * get_disc_entry (VirtualLocation virt_loc,
				    gboolean translate,
				    gboolean *conditionally_changed,
				    gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  GncEntry *entry;
  gnc_numeric discount;

  entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
  discount = gncEntryGetDiscount (entry);
  if (gnc_numeric_zero_p (discount))
    return NULL;

  return xaccPrintAmount (discount, gnc_default_price_print_info ());
}

static const char * get_distype_entry (VirtualLocation virt_loc,
				    gboolean translate,
				    gboolean *conditionally_changed,
				    gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  GncEntry *entry;

  entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
  return gnc_entry_ledger_type_string_getter ('0' +
					      gncEntryGetDiscountType (entry));
}

static const char * get_pric_entry (VirtualLocation virt_loc,
				    gboolean translate,
				    gboolean *conditionally_changed,
				    gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  GncEntry *entry;
  gnc_numeric price;

  entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
  price = gncEntryGetPrice (entry);

  if (gnc_numeric_zero_p (price))
    return NULL;

  return xaccPrintAmount (price, gnc_default_price_print_info ());
}

static const char * get_qty_entry (VirtualLocation virt_loc,
				    gboolean translate,
				    gboolean *conditionally_changed,
				    gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  GncEntry *entry;
  gnc_numeric qty;

  entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
  qty = gncEntryGetQuantity (entry);

  if (gnc_numeric_zero_p (qty))
    return NULL;

  return xaccPrintAmount (qty, gnc_default_price_print_info ());
}

static const char * get_taxacc_entry (VirtualLocation virt_loc,
				    gboolean translate,
				    gboolean *conditionally_changed,
				    gpointer user_data)
{
  char * name = NULL;

  GncEntryLedger *ledger = user_data;
  GncEntry *entry;

  entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);

  g_free (name);
  name = xaccAccountGetFullName (gncEntryGetTaxAccount (entry),
				 gnc_get_account_separator ());
  return name;
}

static const char * get_taxtype_entry (VirtualLocation virt_loc,
				    gboolean translate,
				    gboolean *conditionally_changed,
				    gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  GncEntry *entry;

  entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
  return gnc_entry_ledger_type_string_getter ('0' + 
					      gncEntryGetTaxType (entry));
}

static const char * get_tax_entry (VirtualLocation virt_loc,
				    gboolean translate,
				    gboolean *conditionally_changed,
				    gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  GncEntry *entry;
  gnc_numeric tax;

  entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
  tax = gncEntryGetTax (entry);

  if (gnc_numeric_zero_p (tax))
    return NULL;

  return xaccPrintAmount (tax, gnc_default_price_print_info ());
}

/* GET_HELP */

static char * get_acct_help (VirtualLocation virt_loc, gpointer user_data)
{
  const char *help;
  GncEntryLedger *ledger = user_data;

  help = gnc_table_get_entry (ledger->table, virt_loc);
  if (!help || *help == '\0')
    help = _("Enter the income/expense account for the Entry, "
	     "or choose one from the list");

  return g_strdup (help);
}

static char * get_actn_help (VirtualLocation virt_loc, gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  const char *help;

  help = gnc_table_get_entry (ledger->table, virt_loc);
  if (!help || *help == '\0')
    help = _("Enter the type of Entry");

  return g_strdup (help);
}

static char * get_date_help (VirtualLocation virt_loc, gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  BasicCell *cell;
  char string[1024];
  struct tm *tm;
  Timespec ts;
  time_t tt;

  cell = gnc_table_get_cell (ledger->table, virt_loc);
  if (!cell)
    return NULL;

  if (!cell->value || *cell->value == '\0')
    return NULL;

  gnc_date_cell_get_date ((DateCell *) cell, &ts);
  tt = ts.tv_sec;
  tm = localtime (&tt);
  strftime (string, sizeof(string), "%A %d %B %Y", tm);

  return g_strdup (string);
}

static char * get_desc_help (VirtualLocation virt_loc, gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  const char *help;

  help = gnc_table_get_entry (ledger->table, virt_loc);
  if (!help || *help == '\0')
    help = _("Enter the Entry Description");

  return g_strdup (help);
}

static char * get_disc_help (VirtualLocation virt_loc, gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  const char *help;
  GncEntry *entry;

  entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);

  if (GNC_ENTRY_INTERP_IS_VALUE (gncEntryGetDiscountType (entry)))
    help = _("Enter the Discount Amount");
  else
    help = _("Enter the Discount Percent");

  return g_strdup (help);
}

static char * get_distype_help (VirtualLocation virt_loc, gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  const char *help;

  help = gnc_table_get_entry (ledger->table, virt_loc);
  if (!help || *help == '\0')
    help = _("Select the Discount type");

  return g_strdup (help);
}

static char * get_pric_help (VirtualLocation virt_loc, gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  const char *help;

  help = gnc_table_get_entry (ledger->table, virt_loc);
  if (!help || *help == '\0')
    help = _("Enter the unit-Price for this Entry");

  return g_strdup (help);
}

static char * get_qty_help (VirtualLocation virt_loc, gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  const char *help;

  help = gnc_table_get_entry (ledger->table, virt_loc);
  if (!help || *help == '\0')
    help = _("Enter the Quantity of units for this Entry");

  return g_strdup (help);
}

static char * get_taxacc_help (VirtualLocation virt_loc, gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  const char *help;

  help = gnc_table_get_entry (ledger->table, virt_loc);
  if (!help || *help == '\0')
    help = _("Enter the Account for tax holdings or choose one from the list");

  return g_strdup (help);
}

static char * get_taxtype_help (VirtualLocation virt_loc, gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  const char *help;

  help = gnc_table_get_entry (ledger->table, virt_loc);
  if (!help || *help == '\0')
    help = _("Select the Tax Type");

  return g_strdup (help);
}

static char * get_tax_help (VirtualLocation virt_loc, gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  const char *help;
  GncEntry *entry;

  entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);

  if (GNC_ENTRY_INTERP_IS_VALUE (gncEntryGetDiscountType (entry)))
    help = _("Enter the Tax Amount");
  else
    help = _("Enter the Tax Percent");

  return g_strdup (help);
}

/* GET_IO_FLAGS */

static CellIOFlags get_standard_io_flags (VirtualLocation virt_loc,
					  gpointer user_data)
{
  return XACC_CELL_ALLOW_ALL;
}

/* GET BG_COLORS */

static guint32
gnc_entry_ledger_get_bg_color (VirtualLocation virt_loc,
			       gboolean *hatching, gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  VirtualCell *vcell;
  guint32 bg_color;
  gboolean is_current;

  if (hatching)
    *hatching = FALSE;

  bg_color = 0xffffff; /* white */

  if (!ledger) return bg_color;

  if (gnc_table_virtual_location_in_header (ledger->table, virt_loc))
    return reg_colors.header_bg_color;

  vcell = gnc_table_get_virtual_cell (ledger->table, virt_loc.vcell_loc);
  if (!vcell || !vcell->cellblock)
    return bg_color;

  if ((virt_loc.phys_col_offset < vcell->cellblock->start_col) ||
      (virt_loc.phys_col_offset > vcell->cellblock->stop_col))
    return bg_color;

  is_current = virt_cell_loc_equal
    (ledger->table->current_cursor_loc.vcell_loc, virt_loc.vcell_loc);

  if (is_current)
    return vcell->start_primary_color ?
      reg_colors.primary_active_bg_color :
    reg_colors.secondary_active_bg_color;

  return vcell->start_primary_color ?
    reg_colors.primary_bg_color : reg_colors.secondary_bg_color;
}

/* SAVE CELLS */

static void gnc_entry_ledger_save_cells (gpointer save_data,
					 gpointer user_data)
{
  GncEntryLedger *ledger = user_data;
  GncEntry *entry = save_data;

  g_return_if_fail (entry != NULL);

  /* copy the contents from the cursor to the split */

  if (gnc_table_layout_get_cell_changed (ledger->table->layout,
					 ENTRY_ACCT_CELL, TRUE)) {
    Account *acc;

    acc = gnc_entry_ledger_get_account (ledger, ENTRY_ACCT_CELL);

    if (acc != NULL)
      gncEntrySetAccount (entry, acc);
  }

  if (gnc_table_layout_get_cell_changed (ledger->table->layout,
					 ENTRY_ACTN_CELL, TRUE)) {
    const char *value;

    value = gnc_table_layout_get_cell_value (ledger->table->layout,
					     ENTRY_ACTN_CELL);
    gncEntrySetAction (entry, value);
  }

  if (gnc_table_layout_get_cell_changed (ledger->table->layout,
					 ENTRY_DATE_CELL, TRUE)) {
    BasicCell *cell;
    Timespec ts;

    cell = gnc_table_layout_get_cell (ledger->table->layout, ENTRY_DATE_CELL);

    /* commit any pending changes */
    gnc_date_cell_commit ((DateCell *) cell);

    gnc_date_cell_get_date ((DateCell *) cell, &ts);
    gncEntrySetDate (entry, &ts);
  }

  if (gnc_table_layout_get_cell_changed (ledger->table->layout,
					 ENTRY_DESC_CELL, TRUE)) {
    const char *value;

    value = gnc_table_layout_get_cell_value (ledger->table->layout,
					     ENTRY_DESC_CELL);
    gncEntrySetDescription (entry, value);
  }

  if (gnc_table_layout_get_cell_changed (ledger->table->layout,
					 ENTRY_DISC_CELL, TRUE)) {
    PriceCell *cell;
    gnc_numeric amount;

    cell = (PriceCell *) gnc_table_layout_get_cell (ledger->table->layout,
						    ENTRY_DISC_CELL);
    amount = gnc_price_cell_get_value (cell);
    gncEntrySetDiscount (entry, amount);
  }

  if (gnc_table_layout_get_cell_changed (ledger->table->layout,
					 ENTRY_DISTYPE_CELL, TRUE)) {
    gint type;

    type = gnc_entry_ledger_get_type (ledger, ENTRY_DISTYPE_CELL);

    if (type != -1)
      gncEntrySetDiscountType (entry, type);
  }

  if (gnc_table_layout_get_cell_changed (ledger->table->layout,
					  ENTRY_PRIC_CELL, TRUE)) {
    PriceCell *cell;
    gnc_numeric amount;

    cell = (PriceCell *) gnc_table_layout_get_cell (ledger->table->layout,
						    ENTRY_PRIC_CELL);
    amount = gnc_price_cell_get_value (cell);
    gncEntrySetPrice (entry, amount);
  }

  if (gnc_table_layout_get_cell_changed (ledger->table->layout,
					 ENTRY_QTY_CELL, TRUE)) {
    PriceCell *cell;
    gnc_numeric amount;

    cell = (PriceCell *) gnc_table_layout_get_cell (ledger->table->layout,
						    ENTRY_QTY_CELL);
    amount = gnc_price_cell_get_value (cell);
    gncEntrySetQuantity (entry, amount);
  }

  if (gnc_table_layout_get_cell_changed (ledger->table->layout,
					 ENTRY_TAXACC_CELL, TRUE)) {
    Account *acc;

    acc = gnc_entry_ledger_get_account (ledger, ENTRY_TAXACC_CELL);

    if (acc != NULL)
      gncEntrySetTaxAccount (entry, acc);
  }

  if (gnc_table_layout_get_cell_changed (ledger->table->layout,
					 ENTRY_TAXTYPE_CELL, TRUE)) {
    gint type;

    type = gnc_entry_ledger_get_type (ledger, ENTRY_TAXTYPE_CELL);

    if (type != -1)
      gncEntrySetTaxType (entry, type);
  }

  if (gnc_table_layout_get_cell_changed (ledger->table->layout,
					 ENTRY_TAX_CELL, TRUE)) {
    PriceCell *cell;
    gnc_numeric amount;

    cell = (PriceCell *) gnc_table_layout_get_cell (ledger->table->layout,
						    ENTRY_TAX_CELL);
    amount = gnc_price_cell_get_value (cell);
    gncEntrySetTax (entry, amount);
  }
}

/* Set Cell Handlers */

static void gnc_entry_ledger_model_new_handlers (TableModel *model)
{
  struct model_desc {
    const char * cell;
    gpointer entry_handler;
    gpointer label_handler;
    gpointer help_handler;
    gpointer io_flags_handler;
  } models[] = {
    { ENTRY_ACCT_CELL, get_acct_entry, get_acct_label, get_acct_help, get_standard_io_flags },
    { ENTRY_ACTN_CELL, get_actn_entry, get_actn_label, get_actn_help,  get_standard_io_flags },
    { ENTRY_DATE_CELL, get_date_entry, get_date_label, get_date_help,  get_standard_io_flags },
    { ENTRY_DESC_CELL, get_desc_entry, get_desc_label, get_desc_help,  get_standard_io_flags },
    { ENTRY_DISC_CELL, get_disc_entry, get_disc_label, get_disc_help,  get_standard_io_flags },
    { ENTRY_DISTYPE_CELL, get_distype_entry, get_distype_label, get_distype_help,  get_standard_io_flags },
    { ENTRY_PRIC_CELL, get_pric_entry, get_pric_label, get_pric_help,  get_standard_io_flags },
    { ENTRY_QTY_CELL, get_qty_entry, get_qty_label, get_qty_help,  get_standard_io_flags },
    { ENTRY_TAXACC_CELL, get_taxacc_entry, get_taxacc_label, get_taxacc_help,  get_standard_io_flags },
    { ENTRY_TAXTYPE_CELL, get_taxtype_entry, get_taxtype_label, get_taxtype_help,  get_standard_io_flags },
    { ENTRY_TAX_CELL, get_tax_entry, get_tax_label, get_tax_help,  get_standard_io_flags }
  };
  int i;

  gnc_table_model_set_default_bg_color_handler
    (model, gnc_entry_ledger_get_bg_color);


  for (i = 0; i < (sizeof(models)/sizeof(*models)); i++) {
    if (models[i].entry_handler)
      gnc_table_model_set_entry_handler (model, models[i].entry_handler,
					 models[i].cell);
    if (models[i].label_handler)
      gnc_table_model_set_label_handler (model, models[i].label_handler,
					 models[i].cell);
    if (models[i].help_handler)
      gnc_table_model_set_help_handler (model, models[i].help_handler,
					models[i].cell);
    if (models[i].io_flags_handler)
      gnc_table_model_set_io_flags_handler (model, models[i].io_flags_handler,
					    models[i].cell);
  } /* for */

  /*
  model->cell_data_allocator = ;
  model->cell_data_deallocator = ;
  model->cell_data_copy = ;
  */

  gnc_table_model_set_post_save_handler (model, gnc_entry_ledger_save_cells);
}

/** Public Interface ***********************************************/

TableModel * gnc_entry_ledger_model_new (void)
{
  TableModel * model;

  model = gnc_table_model_new ();
  gnc_entry_ledger_model_new_handlers (model);

  return model;
}

void gnc_entry_ledger_set_colors (GncEntryLedgerColors reg_colors_new)
{
  reg_colors = reg_colors_new;
}
