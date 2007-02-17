/*********************************************************************
 * gnc-register-gnome.c
 * initialization for Gnome parts of register
 *
 * Copyright (c) 2001 Linux Developers Group, Inc.
 *********************************************************************/

#include "config.h"

#include <gmodule.h>

#include "gnc-register-gnome.h"
#include "combocell.h"
#include "datecell.h"
#include "gnc-module-api.h"
#include "gnc-module.h"
#include "formulacell-gnome.h"
#include "pricecell-gnome.h"
#include "quickfillcell-gnome.h"
#include "register-common.h"
#include "table-gnome.h"

int
gnc_register_gnome_init(void) {
  if(!gnc_module_load("gnucash/gnome-utils", 0)) {
    return FALSE;
  }

  gnc_register_add_cell_type (COMBO_CELL_TYPE_NAME, gnc_combo_cell_new);

  gnc_register_add_cell_type (DATE_CELL_TYPE_NAME, gnc_date_cell_new);

  gnc_register_add_cell_type (PRICE_CELL_TYPE_NAME,
                              gnc_price_cell_gnome_new);

  gnc_register_add_cell_type (QUICKFILL_CELL_TYPE_NAME,
                              gnc_quickfill_cell_gnome_new);

  gnc_register_add_cell_type( FORMULA_CELL_TYPE_NAME,
                              gnc_formula_cell_gnome_new );

  gnc_table_gnome_init ();

  return TRUE;
}

