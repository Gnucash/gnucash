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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef GNC_XML_H
#define GNC_XML_H

#include "SchedXaction.h"
#include "gnc-engine.h"
#include "gnc-pricedb.h"
#include "gnc-budget.h"
#include "gnc-xml-helper.h"
#include "sixtp.h"

xmlNodePtr gnc_account_dom_tree_create(Account *act, gboolean exporting,
				       gboolean allow_incompat);
sixtp* gnc_account_sixtp_parser_create(void);

xmlNodePtr gnc_book_dom_tree_create(QofBook *book);
sixtp* gnc_book_sixtp_parser_create(void);
sixtp* gnc_book_id_sixtp_parser_create(void);
sixtp* gnc_book_slots_sixtp_parser_create(void);

xmlNodePtr gnc_commodity_dom_tree_create(const gnc_commodity *com);
sixtp* gnc_commodity_sixtp_parser_create(void);

sixtp* gnc_freqSpec_sixtp_parser_create(void);

xmlNodePtr gnc_lot_dom_tree_create(GNCLot *);
sixtp* gnc_lot_sixtp_parser_create(void);

xmlNodePtr gnc_pricedb_dom_tree_create(GNCPriceDB *db);
sixtp* gnc_pricedb_sixtp_parser_create(void);

xmlNodePtr gnc_schedXaction_dom_tree_create( SchedXaction *sx );
sixtp* gnc_schedXaction_sixtp_parser_create(void);

xmlNodePtr gnc_budget_dom_tree_create( GncBudget *bgt );
sixtp* gnc_budget_sixtp_parser_create(void);

xmlNodePtr gnc_transaction_dom_tree_create(Transaction *txn);
sixtp* gnc_transaction_sixtp_parser_create(void);

sixtp* gnc_template_transaction_sixtp_parser_create(void);

#endif /* GNC_XML_H */
