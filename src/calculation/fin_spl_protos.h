
/*==================================================*/
/* expression_parser.c
 */
/* Line Number:  344 */
parser_env_ptr init_parser(
                           var_store_ptr  predefined_vars,
                           unsigned char  radix_point,
                           unsigned char  group_char,
                           void          *trans_numeric(unsigned char  *digit_str,
                                                        unsigned char   radix_point,
                                                        unsigned char   group_char,
                                                        unsigned char **rstr),
                           void          *numeric_ops(unsigned char  op_sym,
                                                      void          *left_value,
                                                      void          *right_value),
                           void          *negate_numeric(void *value),
                           void           free_numeric(void *numeric_value));
