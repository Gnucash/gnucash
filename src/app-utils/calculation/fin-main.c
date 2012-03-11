/***************************************************************************
                          fin-main.c  -  description
                             -------------------
    begin                : Thursday June 15 2000
    email                : tboldt@attglobal.net
    Author               : Terry D. Boldt
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

/*
 *  Functions to call financial equations and output results
 *  6-15-2000
 *
 */

#include <stdio.h>

#include "finvar.h"
#include "finproto.h"

static void  prt_status(
    fi_ptr       fi,
    FILE        *ofile);

int             main(int argc, char **argv, char **env)
{
    financial_info  fininfo;
    amort_sched     amortsched;

    set_default(&fininfo);
    fininfo.prec = 2;

    fininfo.npp = 360;
    fininfo.ir = 8.25;
    fininfo.pv = 345725.0;

    (void)fi_calc_payment(&fininfo);
    printf("With npp == %u\n     ir == %.*f\n     pv == %.*f\n", fininfo.npp, fininfo.prec, fininfo.ir, fininfo.prec, fininfo.pv);
    printf("------------>Compute pmt: -2597.32\n");
    prt_status(&fininfo,
               stdout);

    fi_calc_interest(&fininfo);
    printf("\n------------>Compute ir\n");
    prt_status(&fininfo,
               stdout);

    fi_calc_num_payments(&fininfo);
    printf("\n------------>Compute npp\n");
    prt_status(&fininfo,
               stdout);

    fi_calc_future_value(&fininfo);
    printf("\n------------>Compute fv\n");
    prt_status(&fininfo,
               stdout);

    set_default(&fininfo);

    fininfo.npp = 360;
    fininfo.ir = 8.25;
    fininfo.pv = 345725.0;
    fi_calc_payment(&fininfo);

    printf("\n\n Reset financial variables and compute amortization schedules.\n");
    printf("First Schedule - ignore delay in first payment and\noutput annual summary\n");

    amortsched.n = fininfo.npp;
    amortsched.nint = fininfo.ir;
    amortsched.pv = fininfo.pv;
    amortsched.pmt = fininfo.pmt;
    amortsched.fv = fininfo.fv;
    amortsched.CF = fininfo.CF;
    amortsched.PF = fininfo.PF;
    amortsched.disc = fininfo.disc;
    amortsched.bep = fininfo.bep;
    amortsched.prec = fininfo.prec;
    amortsched.year_E = 1999;
    amortsched.month_E = 6;
    amortsched.day_E = 15;
    amortsched.year_I = 1999;
    amortsched.month_I = 8;
    amortsched.day_I = 1;
    amortsched.fixed_pmt = -400;

    (void)Amortization_init(&amortsched);

    amortsched.option = 3;
    amortsched.summary = 'y';
    (void)Amortization_Schedule(&amortsched);
    prt_amortization_schedule(&amortsched, stdout);

    printf("\n\nSecond Schedule - ignore delay in first payment and\noutput schedule for each payment\n");
    amortsched.summary = 'p';
    (void)Amortization_Schedule(&amortsched);
    prt_amortization_schedule(&amortsched, stdout);


    printf("\n\nThird Schedule - ignore delay in first payment and\noutput variable advanced prepayment schedule\n");
    amortsched.summary = 'a';
    (void)Amortization_Schedule(&amortsched);
    prt_amortization_schedule(&amortsched, stdout);

    printf("\n\nFourth Schedule - ignore delay in first payment and\noutput fixed prepayment schedule\n");
    amortsched.summary = 'f';
    (void)Amortization_Schedule(&amortsched);
    prt_amortization_schedule(&amortsched, stdout);

    printf("\n\nFifth Schedule - use new payments due to delay and\noutput annual summary\n");
    amortsched.option = 5;
    amortsched.summary = 'y';
    (void)Amortization_Schedule(&amortsched);
    prt_amortization_schedule(&amortsched, stdout);

    printf("\n\nSixth Schedule - use new payments due to delay and\noutput periodic payment schedule\n");
    amortsched.option = 5;
    amortsched.summary = 'p';
    (void)Amortization_Schedule(&amortsched);
    prt_amortization_schedule(&amortsched, stdout);

    printf("\n\nSeventh Schedule - use new payments due to delay and\noutput variable prepayment schedule\n");
    amortsched.option = 5;
    amortsched.summary = 'a';
    (void)Amortization_Schedule(&amortsched);
    prt_amortization_schedule(&amortsched, stdout);

    printf("\n\nEighth Schedule - use new payments due to delay and\noutput fixed prepayment schedule\n");
    amortsched.option = 5;
    amortsched.summary = 'f';
    (void)Amortization_Schedule(&amortsched);
    prt_amortization_schedule(&amortsched, stdout);
    Amortization_free(&amortsched);
} /* main */

static void  prt_status(
    fi_ptr       fi,
    FILE        *ofile)
{
    fprintf(ofile, "<================================>\nCurrent Financial Calculator Status:\n");
    fprintf(ofile, "Compounding Frequency: (CF) %u\n", fi->CF);
    fprintf(ofile, "Payment     Frequency: (PF) %u\n", fi->PF);
    fprintf(ofile, "Compounding: %s\n", fi->disc ? "Discrete (disc = TRUE)" : "Continuous (disc = FALSE)");
    fprintf(ofile, "Payments: %s\n", fi->bep ? "Beginning of Period (bep = TRUE)" : "End of Period (bep = FALSE)");
    if ( fi->npp > 12 ) fprintf(ofile, "Number of Payment Periods (n): %u\t\t(Years: %u)\n", fi->npp, fi->npp / fi->PF);
    else fprintf(ofile, "Number of Payment Periods (n): %u\n", fi->npp);
    if ((fi->CF == 1) && (fi->PF == 1) ) fprintf(ofile, "Nominal Interest per Payment Period (i): %f\t(Annualized: %.*f\n", fi->ir, fi->prec, fi->ir * 12);
    else fprintf(ofile, "Nominal Annual Interest Rate (i): %.*f\n", fi->prec, fi->ir);
    /*    fprintf(ofile, "  Effective Interest Rate Per Payment Period: %f\n",eff_int(nint/100.0,CF,PF));   */
    fprintf(ofile, "Present Value (pv): %.*f\n", fi->prec, fi->pv);
    fprintf(ofile, "Periodic Payment (pmt): %.*f\n", fi->prec, fi->pmt);
    fprintf(ofile, "Future Value (fv): %.*f\n", fi->prec, fi->fv);
}   /* prt_status */
