
#ifndef _SIXTP_PARSERS_H_
#define _SIXTP_PARSERS_H_

#include "sixtp.h"

/* from Transaction-xml-parser-v1.c */
sixtp* gnc_transaction_parser_new(void);

/* from Account-xml-parser-v1.c */
sixtp* gnc_account_parser_new(void);

/* from Ledger-xml-parser-v1.c */
sixtp* ledger_data_parser_new(void);

/* from Commodity-xml-parser-v1.c */
sixtp* commodity_restore_parser_new(void);

/* from Commodity-xml-parser-v1.c */
sixtp* generic_gnc_commodity_lookup_parser_new(void);

/* from Query-xml-parser-v1.c */
sixtp* query_server_parser_new (void);

/* from sixtp-kvp-parser.c */
sixtp* kvp_frame_parser_new(void);

/* from sixtp-to-dom-parser.c */

/* Create a parser that will turn the entire sub-tree into a DOM tree
   an pass it in as (don't put anything into parent_data) 
   you must deal with the xml tree in *result.
*/
sixtp* sixtp_dom_parser_new(sixtp_end_handler ender,
                            sixtp_result_handler cleanup_result_by_default_func,
                            sixtp_result_handler cleanup_result_on_fail_func);

sixtp* gnc_pricedb_parser_new(void);

#endif /* _SIXTP_PARSERS_H_ */
