/********************************************************************
 * dialog-commodity.h -- "select" and "new" commodity windows       *
 *                       (GnuCash)                                  *
 * Copyright (C) 2000 Bill Gribble <grib@billgribble.com>           *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 ********************************************************************/

#ifndef GNC_DIALOG_COMMODITY_H
#define GNC_DIALOG_COMMODITY_H

#include <gnome.h>

#include "gnc-commodity.h"
#include "gnc-engine.h"

typedef struct select_commodity_window SelectCommodityWindow;
typedef struct commodity_window CommodityWindow;

typedef void (* gnc_commodity_callback)(const gnc_commodity *, void * data);
typedef void (* gnc_commodity_help_callback)(void);

void gnc_ui_commodity_set_help_callback (gnc_commodity_help_callback cb);

void gnc_ui_select_commodity_destroy(SelectCommodityWindow * w);

void gnc_ui_commodity_destroy(CommodityWindow * w);

gnc_commodity * 
gnc_ui_select_commodity_modal(gnc_commodity * orig_sel, 
                              GtkWidget * parent);

gnc_commodity * 
gnc_ui_new_commodity_modal(const char * default_namespace, 
                           GtkWidget * parent);

gboolean
gnc_ui_edit_commodity_modal(gnc_commodity *commodity,
                            GtkWidget * parent);

char * gnc_ui_update_namespace_picker(GtkWidget * combobox,
                                      const char * sel,
                                      gboolean include_iso,
                                      gboolean include_all);

const char * gnc_ui_namespace_picker_ns (GtkWidget *combobox);

void gnc_ui_update_commodity_picker(GtkWidget * combobox, 
                                    const char * namespace,
                                    const char * sel);

#endif
