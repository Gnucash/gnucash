
#ifndef _SIXTP_PARSERS_H_
#define _SIXTP_PARSERS_H_

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



#endif /* _SIXTP_PARSERS_H_ */


    
