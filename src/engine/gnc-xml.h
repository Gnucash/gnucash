/********************************************************************\
 * gnc-xml.h -- api for gnucash xml i/o                             *
 *                                                                  *
 * Copyright (C) 2001 James LewisMoss <dres@debian.org>             *
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
\********************************************************************/

#ifndef __GNC_XML_H__
#define __GNC_XML_H__

#include "config.h"

#include "gnc-xml-helper.h"

#include "Account.h"
#include "gnc-commodity.h"

#include "sixtp.h"
#include "gnc-pricedb.h"

xmlNodePtr gnc_account_dom_tree_create(Account *act);
sixtp* gnc_account_sixtp_parser_create(void);

xmlNodePtr gnc_commodity_dom_tree_create(const gnc_commodity *act);
sixtp* gnc_commodity_sixtp_parser_create(void);

xmlNodePtr gnc_transaction_dom_tree_create(Transaction *com);
sixtp* gnc_transaction_sixtp_parser_create(void);

xmlNodePtr split_to_dom_tree(const gchar *tag, Split *spl);
Split* dom_tree_to_split(xmlNodePtr node);

xmlNodePtr gnc_pricedb_dom_tree_create(GNCPriceDB *db);
sixtp* gnc_pricedb_sixtp_parser_create(void);


#endif /* __GNC_XML_H__ */
