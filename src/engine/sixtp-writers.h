/********************************************************************
 * sixtp-writers.h                                                  *
 * Copyright 2001 Gnumatic, Inc.                                    *
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
 *                                                                  *
 ********************************************************************/

#ifndef _SIXTP_WRITERS_H_
#define _SIXTP_WRITERS_H_

#include <config.h>
#include <glib.h>

#include "gnc-xml-helper.h"
#include "Query.h"
#include "gnc-pricedb.h"

gboolean xml_add_account_restorers(xmlNodePtr p, AccountGroup *g);

gboolean xml_add_commodity_restorers(xmlNodePtr p);

gboolean xml_add_commodity_ref(xmlNodePtr p, const char *tag,
                               const gnc_commodity *c);

gboolean xml_add_query_restorers(xmlNodePtr p, Query *q);

gboolean xml_add_txn_and_split_restorers(xmlNodePtr p, AccountGroup *g);

gboolean xml_add_gnc_price(xmlNodePtr p, const char *tag, GNCPrice *db);

gboolean xml_add_gnc_pricedb(xmlNodePtr p, const char *tag, GNCPriceDB *db);

#endif /* _SIXTP_WRITERS_H_ */
