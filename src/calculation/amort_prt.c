/***************************************************************************
                          amort_prt.c  -  description
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
 *  Functions to print amortization schedules
 *  6-15-2000
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <mcheck.h>

#include "finvar.h"
#include "finproto.h"
#include "fin_spl_protos.h"


void             prt_amortization_schedule(
    amort_sched_ptr  amortsched,  /* amortization schedule to print           */
    FILE            *ofile)      /* output file                               */
{
    unsigned            j,
           jj,
           prec = amortsched->prec,
           option = amortsched->option,
           fv_case = amortsched->fv_case;
    unsigned char       datel[100],
             summary = amortsched->summary;
    struct tm          *times_E,
            *times_I;
    amort_sched_yr_ptr  amortyr,
                     prst_yr;
    sched_pmt_ptr       pmtsched = NULL;
    yearly_summary_ptr  annual_summary;

    times_E = (struct tm *)calloc(1, sizeof(struct tm));
    times_E->tm_mday = amortsched->day_E;
    times_E->tm_mon  = amortsched->month_E - 1;
    times_E->tm_year = amortsched->year_E - 1900;
    times_E->tm_wday = (amortsched->Eff_Date_jdn + 1) % 7;
    times_E->tm_yday = amortsched->yday_E;

    times_I = (struct tm *)calloc(1, sizeof(struct tm));
    times_I->tm_mday = amortsched->day_I;
    times_I->tm_mon  = amortsched->month_I - 1;
    times_I->tm_year = amortsched->year_I - 1900;
    times_I->tm_wday = (amortsched->Init_Date_jdn + 1) % 7;
    times_I->tm_yday = amortsched->yday_I;

    fprintf(ofile, "Amortization Table\n");
    qof_strftime(datel, (size_t)100, "%c", times_E);
    fprintf(ofile, "Effective       Date: %s\n", datel);
    qof_strftime(datel, (size_t)100, "%c", times_I);
    fprintf(ofile, "Initial Payment Date: %s\n", datel);
    fprintf(ofile, "Compounding Frequency per year: %u\n", amortsched->CF);
    fprintf(ofile, "Payment     Frequency per year: %u\n", amortsched->PF);
    fprintf(ofile, "Compounding: %s\n", (amortsched->disc ? "Discrete" : "Continuous"));
    fprintf(ofile, "Payments: %s\n", (amortsched->bep ? "Beginning of Period" : "End of Period"));
    fprintf(ofile, "Payments (%u): %.*f\n", amortsched->n - 1, (int)prec, (option < 3) ? amortsched->cpmt : (option == 5) ? amortsched->new_pmt : amortsched->pmt);
    fprintf(ofile, "Final payment (%u): %.*f\n", amortsched->n, (int)prec, amortsched->final_pmt);
    if ( (amortsched->CF == 1) && (amortsched->PF == 1) ) fprintf(ofile, "Nominal Interest per Payment Period: %g\t(Annualized: %g)\n", amortsched->nint, amortsched->nint * 12);
    else fprintf(ofile, "Nominal Annual Interest Rate: %g\n", amortsched->nint);
    fprintf(ofile, "  Effective Interest Rate Per Payment Period: %g\n", amortsched->eint);
    fprintf(ofile, "Present Value: %.*f\n", (int)prec, amortsched->pv);
    if ( (amortsched->option == 2) || (amortsched->option > 3) )
{
        fprintf(ofile, "Interest due to Delayed Intial Payment: %.*f\n", (int)prec, amortsched->delayed_int);
    } /* endif */

    free(times_E);
    free(times_I);

    if ( amortsched->option < 3 )
    {
        summary = (summary == 'y') ? 'x' : 'o';
    } /* endif */

    switch ( summary )
    {
    case 'a':
        /* variable prepayment schedule
         */
        fprintf(ofile, "Advanced Prepayment Amortization - Variable Prepayment\n");
        amortyr = amortsched->schedule.first_yr;
        for ( j = amortsched->total_periods , jj = 0 ; j && amortyr ; j-- )
        {
            if ( !jj )
            {
                fprintf(ofile, "Pmt *     Interest    Principal       Prepay    Total Pmt      Balance\n");
                pmtsched = amortyr->payments;
                jj = amortyr->num_periods;
            } /* endif */

            fprintf(ofile, "%4u  %12.*f %12.*f %12.*f %12.*f %12.*f\n",
                    pmtsched->period_num,
                    (int)prec, pmtsched->interest,
                    (int)prec, pmtsched->principal,
                    (int)prec, pmtsched->advanced_pmt,
                    (int)prec, pmtsched->total_pmt,
                    (int)prec, pmtsched->balance);

            if ( !--jj )
            {
                fprintf(ofile, "Summary for: %u:\n", amortyr->year);
                fprintf(ofile, "  Interest  Paid: %.*f\n", (int)prec, amortyr->interest_pd);
                fprintf(ofile, "  Principal Paid: %.*f\n", (int)prec, amortyr->principal_pd);
                fprintf(ofile, "  Year Ending Balance: %.*f\n", (int)prec, amortyr->yr_end_balance);
                fprintf(ofile, "  Sum of Interest Paid: %.*f\n", (int)prec, amortyr->total_interest_pd);
                prst_yr = amortyr;
                amortyr = amortyr->next_yr;
            }
            else
            {
                pmtsched++;
            } /* endif */
        } /* endfor */
        break;
    case 'f':
        /* fixed prepayment schedule
         */
        fprintf(ofile, "Advanced Prepayment Amortization - Fixed Prepayment: %.*f\n", (int)prec, amortsched->fixed_pmt);
        amortyr = amortsched->schedule.first_yr;
        for ( j = amortsched->total_periods , jj = 0 ; j && amortyr ; j-- )
        {
            if ( !jj )
            {
                fprintf(ofile, "Pmt *     Interest    Principal       Prepay    Total Pmt      Balance\n");
                pmtsched = amortyr->payments;
                jj = amortyr->num_periods;
            } /* endif */

            fprintf(ofile, "%4u  %12.*f %12.*f %12.*f %12.*f %12.*f\n",
                    pmtsched->period_num,
                    (int)prec, pmtsched->interest,
                    (int)prec, pmtsched->principal,
                    (int)prec, pmtsched->advanced_pmt,
                    (int)prec, pmtsched->total_pmt,
                    (int)prec, pmtsched->balance);

            if ( !--jj )
            {
                fprintf(ofile, "Summary for: %u:\n", amortyr->year);
                fprintf(ofile, "  Interest  Paid: %.*f\n", (int)prec, amortyr->interest_pd);
                fprintf(ofile, "  Principal Paid: %.*f\n", (int)prec, amortyr->principal_pd);
                fprintf(ofile, "  Year Ending Balance: %.*f\n", (int)prec, amortyr->yr_end_balance);
                fprintf(ofile, "  Sum of Interest Paid: %.*f\n", (int)prec, amortyr->total_interest_pd);
                prst_yr = amortyr;
                amortyr = amortyr->next_yr;
            }
            else
            {
                pmtsched++;
            } /* endif */
        } /* endfor */
        break;
    case 'o':
        /* constant payment to principal
         */
        fprintf(ofile, "Constant Payment to Principal: %.*f\n", (int)prec, amortsched->cpmt);
        amortyr = amortsched->schedule.first_yr;
        for ( j = amortsched->total_periods , jj = 0 ; j && amortyr ; j-- )
        {
            if ( !jj )
            {
                fprintf(ofile, "Pmt#       Interest  Total Payment        Balance\n");
                pmtsched = amortyr->payments;
                jj = amortyr->num_periods;
            } /* endif */

            fprintf(ofile, "%4u   %12.*f   %12.*f   %12.*f\n",
                    pmtsched->period_num,
                    (int)prec, pmtsched->interest,
                    (int)prec, pmtsched->total_pmt,
                    (int)prec, pmtsched->balance);

            if ( !--jj )
            {
                fprintf(ofile, "Summary for: %u:\n", amortyr->year);
                fprintf(ofile, "  Interest  Paid: %.*f\n", (int)prec, amortyr->interest_pd);
                fprintf(ofile, "  Principal Paid: %.*f\n", (int)prec, amortyr->principal_pd);
                fprintf(ofile, "  Year Ending Balance: %.*f\n", (int)prec, amortyr->yr_end_balance);
                fprintf(ofile, "  Sum of Interest Paid: %.*f\n", (int)prec, amortyr->total_interest_pd);
                prst_yr = amortyr;
                amortyr = amortyr->next_yr;
            }
            else
            {
                pmtsched++;
            } /* endif */
        } /* endfor */
        break;
    case 'p':
        /* normal payment schedule
         */
        fprintf(ofile, "Normal Amortization Schedule\n");
        amortyr = amortsched->schedule.first_yr;
        for ( j = amortsched->total_periods - 1 , jj = 0 ; j && amortyr ; j-- )
        {
            if ( !jj )
            {
                fprintf(ofile, amortsched->fv_case ? "Pmt *       Interest        Balance\n" : "Pmt *       Interest      Principal        Balance\n");
                pmtsched = amortyr->payments;
                jj = amortyr->num_periods;
            } /* endif */

            if ( fv_case )
            {
                fprintf(ofile, "%4u   %12.*f   %12.*f\n",
                        pmtsched->period_num,
                        (int)prec, pmtsched->interest,
                        (int)prec, pmtsched->balance);
            }
            else
            {
                fprintf(ofile, "%4u    %12.*f   %12.*f   %12.*f\n",
                        pmtsched->period_num,
                        (int)prec, pmtsched->interest,
                        (int)prec, pmtsched->principal,
                        (int)prec, pmtsched->balance);
            } /* endif */

            if ( !--jj )
            {
                fprintf(ofile, "Summary for: %u:\n", amortyr->year);
                fprintf(ofile, "  Interest  Paid: %.*f\n", (int)prec, amortyr->interest_pd);
                if ( !fv_case ) fprintf(ofile, "  Principal Paid: %.*f\n", (int)prec, amortyr->principal_pd);
                fprintf(ofile, "  Year Ending Balance: %.*f\n", (int)prec, amortyr->yr_end_balance);
                fprintf(ofile, "  Sum of Interest Paid: %.*f\n", (int)prec, amortyr->total_interest_pd);
                prst_yr = amortyr;
                amortyr = amortyr->next_yr;
            }
            else
            {
                pmtsched++;
            } /* endif */
        } /* endfor */

        if ( !jj )
        {
            fprintf(ofile, amortsched->fv_case ? "Pmt *       Interest        Balance\n" : "Pmt *       Interest      Principal        Balance\n");
            pmtsched = amortyr->payments;
        } /* endif */

        fprintf(ofile, "Final Payment: %.*f\n", (int)prec, amortyr->final_pmt);

        if ( fv_case )
        {
            fprintf(ofile, "%4u   %12.*f   %12.*f\n",
                    pmtsched->period_num,
                    (int)prec, pmtsched->interest,
                    (int)prec, pmtsched->balance);
        }
        else
        {
            fprintf(ofile, "%4u    %12.*f   %12.*f   %12.*f\n",
                    pmtsched->period_num,
                    (int)prec, pmtsched->interest,
                    (int)prec, pmtsched->principal,
                    (int)prec, pmtsched->balance);
        } /* endif */

        fprintf(ofile, "Summary for: %u:\n", amortyr->year);
        fprintf(ofile, "  Interest  Paid: %.*f\n", (int)prec, amortyr->interest_pd);
        if ( !fv_case ) fprintf(ofile, "  Principal Paid: %.*f\n", (int)prec, amortyr->principal_pd);
        fprintf(ofile, "  Year Ending Balance: %.*f\n", (int)prec, amortyr->yr_end_balance);
        fprintf(ofile, "  Sum of Interest Paid: %.*f\n", (int)prec, amortyr->total_interest_pd);
        break;
    case 'x':
        /* constant payment to principal - annual summary
         */
    case 'y':
        /* normal payment - annual summary
         */
        if ( summary == 'x' ) fprintf(ofile, "Annual Summary - Constant Payment to Principal: %.*f\n", (int)prec, amortsched->cpmt);
        else fprintf(ofile, "Annual Summary - Normal Amortization\n");
        fprintf(ofile, "Year      Interest   Ending Balance\n");
        annual_summary = amortsched->schedule.summary;
        for ( j = amortsched->total_periods , jj = 0 ; j ; j-- , jj++ )
        {
            fprintf(ofile, "%4u  %12.*f   %12.*f\n",
                    annual_summary[jj].year,
                    (int)prec, annual_summary[jj].interest,
                    (int)prec, annual_summary[jj].end_balance);
        } /* endfor */
        break;
    } /* endswitch */

    fprintf(ofile, "\nTotal Interest: %.*f\n", (int)prec, amortsched->total_interest);

} /* prt_amortization_schedule */
