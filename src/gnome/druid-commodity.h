/********************************************************************
 * druid-commodity.h -- fancy importer for old Gnucash files        *
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

#ifndef __DRUID_COMMODITY_H__
#define __DRUID_COMMODITY_H__

#include <gnome.h>

#include "glade-gnc-dialogs.h"
#include "glade-cb-gnc-dialogs.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"

typedef struct _commoditydruid CommodityDruid;

/* create/destroy commodity import druid */
CommodityDruid  * gnc_ui_commodity_druid_create(const char * filename);
void            gnc_ui_commodity_druid_destroy(CommodityDruid * d);

/* invoke import druid modally */
void            gnc_import_legacy_commodities(const char * filename);

#endif

