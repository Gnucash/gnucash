
#ifndef __GNC_XML_H__
#define __GNC_XML_H__

#include "config.h"

#include "gnc-xml-helper.h"

#include "Account.h"
#include "gnc-commodity.h"

#include "sixtp.h"


xmlNodePtr gnc_account_dom_tree_create(Account *act);
sixtp* gnc_account_sixtp_parser_create(void);

xmlNodePtr gnc_commodity_dom_tree_create(const gnc_commodity *act);
sixtp* gnc_commodity_sixtp_parser_create(void);

xmlNodePtr gnc_transaction_dom_tree_create(const Transaction *com);
sixtp* gnc_transaction_sixtp_parser_create(void);


#endif /* __GNC_XML_H__ */
