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

#ifndef __DIALOG_COMMODITY_H_
#define __DIALOG_COMMODITY_H_

#include "glade-gnc-dialogs.h"
#include "glade-cb-gnc-dialogs.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"

typedef struct _selectcommoditywindow SelectCommodityWindow;
typedef struct _newcommoditywindow NewCommodityWindow;

typedef void (* gnc_commodity_callback)(const gnc_commodity *, void * data);
 
void gnc_ui_select_commodity_destroy(SelectCommodityWindow * w);

void gnc_ui_new_commodity_destroy(NewCommodityWindow * w);

gnc_commodity * 
gnc_ui_select_commodity_modal(gnc_commodity * orig_sel, 
                              GtkWidget * parent);

gnc_commodity * 
gnc_ui_new_commodity_modal(const char * default_namespace, 
                           GtkWidget * parent);

char * gnc_ui_update_namespace_picker(GtkWidget * combobox, const char * sel);
void gnc_ui_update_commodity_picker(GtkWidget * combobox, 
                                    const char * namespace,
                                    const char * sel);

#endif
