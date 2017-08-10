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

#ifndef FINPROTO_H
#define FINPROTO_H

#include <stdio.h>

#include "finvar.h"

/*==================================================*/
/* fin.c */

unsigned fi_calc_num_payments (fi_ptr fi);

double
_fi_calc_num_payments (double nint,	/* nominal interest rate */
                       double pv,	/* present value */
                       double pmt,	/* periodic payment */
                       double fv,	/* future value */
                       unsigned CF,	/* compounding frequency */
                       unsigned PF,	/* payment frequency */
                       unsigned disc,   /* discrete/continuous compounding */
                       unsigned bep);   /* beginning/end of period payment */

double fi_calc_interest (fi_ptr fi);

double
_fi_calc_interest (unsigned per,  /* number of periods */
                   double pv,	  /* present value */
                   double pmt,	  /* periodic payment */
                   double fv,	  /* future value */
                   unsigned CF,	  /* compounding frequency */
                   unsigned PF,	  /* payment frequency */
                   unsigned disc, /* discrete/continuous compounding */
                   unsigned bep); /* beginning/end of period payment */

double fi_calc_present_value (fi_ptr fi);

double
_fi_calc_present_value (unsigned per,	/* number of periods */
                        double nint,	/* nominal interest rate */
                        double pmt,	/* periodic payment */
                        double fv,	/* future value */
                        unsigned CF,	/* compounding frequency */
                        unsigned PF,	/* payment frequency */
                        unsigned disc,	/* discrete/continuous compounding */
                        unsigned bep);	/* beginning/end of period payment */

double fi_calc_payment (fi_ptr fi);

double
_fi_calc_payment (unsigned per,	 /* number of periods */
                  double nint,	 /* nominal interest rate */
                  double pv,	 /* present value */
                  double fv,	 /* future value */
                  unsigned CF,	 /* compounding frequency */
                  unsigned PF,	 /* payment frequency */
                  unsigned disc, /* discrete/continuous compounding */
                  unsigned bep); /* beginning/end of period payment */

double fi_calc_future_value (fi_ptr fi);

double
_fi_calc_future_value (unsigned per,	/* number of periods */
                       double nint,	/* nominal interest rate */
                       double pv,	/* present value */
                       double pmt,	/* periodic payment */
                       unsigned CF,	/* compounding frequency */
                       unsigned PF,	/* payment frequency */
                       unsigned disc,	/* discrete/continuous compounding */
                       unsigned bep);	/* beginning/end of period payment */

void set_default (fi_ptr fi);

unsigned long julian_day_number (unsigned year, unsigned month, unsigned day);

amort_sched_ptr Amortization_init (amort_sched_ptr amortsched);

amort_sched_ptr Amortization_Schedule (amort_sched_ptr amortsched);

void Amortization_free (amort_sched_ptr amortsched);


/*==================================================*/
/* expression_parser.c */

void exit_parser (parser_env_ptr pe);

ParseError get_parse_error (parser_env_ptr pe);

var_store_ptr parser_get_vars (parser_env_ptr pe);

unsigned delete_var (char *var_name, parser_env_ptr pe);

char *parse_string (var_store_ptr value,
                    const char *string, parser_env_ptr pe);


/*==================================================*/
/* amort_opt.c */
amort_sched_ptr amort_opt (amort_sched_ptr amortsched, void *parse_env);


/*==================================================*/
/* amort_prt.c */
void prt_amortization_schedule (amort_sched_ptr amortsched, FILE * ofile);

#endif
