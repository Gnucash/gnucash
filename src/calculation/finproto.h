/***************************************************************************
 *              -------------------
 *    create   : Tue Jul 11 20:21:18 2000
 *    copyright: (C) 2000 by Terry D. Boldt
 *    email    : tboldt@attglobal.net
 *              -------------------
 ***************************************************************************/
/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
/***************************************************************************
 *  Global Function Prototypes
 *  Tue Jul 11 20:21:18 2000
 *
 ***************************************************************************/

#ifndef __FINPROTO_H__
#define __FINPROTO_H__

#include <stdio.h>

#include "finvar.h"

/*==================================================*/
/* fin.c
 */
/* Line Number: 1209 */
unsigned                 fi_calc_num_payments(
                           fi_ptr fi);
/* Line Number: 1221 */
double                   _fi_calc_num_payments(
                            double nint, /* nominal interest rate */
                            double pv, /* present value */
                            double pmt, /* periodic payment */
                            double fv, /* future value */
                            unsigned CF, /* compounding frequency */
                            unsigned PF, /* payment frequency */
                            unsigned disc, /* discrete/continuous compounding flag */
                            unsigned bep); /* beginning/end of period payment flag */
/* Line Number: 1240 */
double                   fi_calc_interest(
                           fi_ptr fi);
/* Line Number: 1257 */
double                   _fi_calc_interest(
                            unsigned per, /* number of periods */
                            double pv, /* present value */
                            double pmt, /* periodic payment */
                            double fv, /* future value */
                            unsigned CF, /* compounding frequency */
                            unsigned PF, /* payment frequency */
                            unsigned disc, /* discrete/continuous compounding flag */
                            unsigned bep); /* beginning/end of period payment flag */
/* Line Number: 1300 */
double                   fi_calc_present_value(
                            fi_ptr fi);
/* Line Number: 1312 */
double                   _fi_calc_present_value(
                             unsigned per, /* number of periods */
                             double nint, /* nominal interest rate */
                             double pmt, /* periodic payment */
                             double fv, /* future value */
                             unsigned CF, /* compounding frequency */
                             unsigned PF, /* payment frequency */
                             unsigned disc, /* discrete/continuous compounding flag */
                             unsigned bep); /* beginning/end of period payment flag */
/* Line Number: 1332 */
double                   fi_calc_payment(
                             fi_ptr fi);
/* Line Number: 1344 */
double                   _fi_calc_payment(
                              unsigned per, /* number of periods */
                              double nint, /* nominal interest rate */
                              double pv, /* present value */
                              double fv, /* future value */
                              unsigned CF, /* compounding frequency */
                              unsigned PF, /* payment frequency */
                              unsigned disc, /* discrete/continuous compounding flag */
                              unsigned bep); /* beginning/end of period payment flag */
/* Line Number: 1363 */
double                   fi_calc_future_value(
                            fi_ptr fi);
/* Line Number: 1375 */
double                   _fi_calc_future_value(
                             unsigned per, /* number of periods */
                             double nint, /* nominal interest rate */
                             double pv, /* present value */
                             double pmt, /* periodic payment */
                             unsigned CF, /* compounding frequency */
                             unsigned PF, /* payment frequency */
                             unsigned disc, /* discrete/continuous compounding flag */
                             unsigned bep); /* beginning/end of period payment flag */
/* Line Number: 1464 */
void                     set_default(
                                     fi_ptr fi);
/* Line Number: 1511 */
unsigned long            julian_day_number(
                             unsigned year,
                             unsigned month,
                             unsigned day);
/* Line Number: 1533 */
amort_sched_ptr          Amortization_init(
                                           amort_sched_ptr amortsched);
/* Line Number: 1664 */
amort_sched_ptr          Amortization_Schedule(
                                               amort_sched_ptr amortsched);
/* Line Number: 2336 */
void                     Amortization_free(
                                           amort_sched_ptr amortsched);

/*==================================================*/
/* expression_parser.c */

/* Line Number: 377 */
void                     exit_parser(parser_env_ptr pe);
/* Line Number: 400 */
ParseError               get_parse_error(parser_env_ptr pe);
/* Line Number: 408 */
var_store_ptr            get_vars(parser_env_ptr pe);
/* Line Number: 417 */
unsigned                 delete_var(unsigned char *var_name,
                                    parser_env_ptr pe);
/* Line Number: 451 */
unsigned char           *parse_string(var_store_ptr value,
                                      unsigned char *string,
                                      parser_env_ptr pe);

/*==================================================*/
/* numeric_ops.c */

/* Line Number: 85 */
void                    *trans_numeric(
                                       unsigned char *str, /* pointer to string to translate */
                                       unsigned char radix_point, /* radix character */
                                       unsigned char group_char, /* grouping character to left of radix */
                                       unsigned char **endstr); /* where to return pointer to first unrecognized character */
/* Line Number: 238 */
void                     free_numeric(
                                      void *numeric_value);
/* Line Number: 246 */
void                    *negate_numeric(
                                        void *value);
/* Line Number: 273 */
void                    *numeric_ops(
                                     unsigned char op_symbol,
                                     void *l_value,
                                     void *r_value);

/*==================================================*/
/* amort_opt.c
 */
/* Line Number: 32 */
amort_sched_ptr          amort_opt(
                                   amort_sched_ptr amortsched,
                                   void *parse_env);

/*==================================================*/
/* amort_prt.c
 */
/* Line Number: 34 */
void                     prt_amortization_schedule(
                                                   amort_sched_ptr amortsched, /* amortization schedule to print */
                                                   FILE *ofile); /* output file */

#endif
