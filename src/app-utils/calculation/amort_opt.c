/***************************************************************************
                          amort_opt.c  -  description
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
 *  Functions to determine amortizations options
 *  7-2-2000
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "finvar.h"
#include "finproto.h"
#include "fin_spl_protos.h"

amort_sched_ptr  amort_opt(
    amort_sched_ptr  amortsched,
    void            *parse_env)
{
    char            buffer[200], *errp;
    unsigned long   ii;
    unsigned        prec = amortsched->prec;
    var_store       value;
    numeric_ptr     nval;
    struct tm      *times_E,
            *times_I;

    /* print amortization options */
    times_E = (struct tm *)calloc(1, sizeof(struct tm));
    ii = amortsched->Eff_Date_jdn;
    times_E->tm_mday = amortsched->day_E;
    times_E->tm_mon  = amortsched->month_E - 1;
    times_E->tm_year = amortsched->year_E - 1900;
    times_E->tm_wday = (ii + 1) % 7;
    times_E->tm_yday = amortsched->yday_E;

    times_I = (struct tm *)calloc(1, sizeof(struct tm));
    ii = amortsched->Init_Date_jdn;
    times_I->tm_mday = amortsched->day_I;
    times_I->tm_mon  = amortsched->month_I - 1;
    times_I->tm_year = amortsched->year_I - 1900;
    times_I->tm_wday = (ii + 1) % 7;
    times_I->tm_yday = amortsched->yday_I;

    printf("\n******************************");
    qof_strftime(buffer, (size_t)50, "%c", times_E);
    printf("\nEffective       Date: %s\n", buffer);
    qof_strftime(buffer, (size_t)50, "%c", times_I);
    printf("Initial Payment Date: %s\n", buffer);
    free(times_E);
    free(times_I);
    printf("The Original Present Value (pv)        is: %.*f\n", (int)prec, amortsched->pv);
    printf("The Original Periodic Payment (pmt)    is: %.*f\n", (int)prec, amortsched->pmt);
    printf("The Original Future  Value (fv)        is: %.*f\n", (int)prec, amortsched->fv);

    printf("The Delayed Present Value (pve)        is:  %.*f\n", (int)prec, amortsched->pve);
    printf("The New Periodic Payment (pmt) for pve is:  %.*f\n\n", (int)prec, amortsched->new_pmt);

    printf("The amortization options are:\n");
    printf("1 -- Amortize with Original Amount and Constant Payment to Principal: %.*f\n", (int) prec, amortsched->cpmt1);
    printf("    and final payment: %.*f\n", (int)prec, amortsched->final_pmt_opt_1);
    printf("2 -- Amortize with Delayed Amount and Constant Payment to Principal: %.*f\n", (int)prec, amortsched->cpmt2);
    printf("    and final payment: %.*f\n", (int)prec, amortsched->final_pmt_opt_2);
    printf("3 -- Amortize with Original Transaction Values\n");
    printf("    and final payment: %.*f\n", (int)prec, amortsched->final_pmt_opt_3);
    printf("4 -- Amortize with Delayed Amount, Original Periodic Payment\n");
    printf("    and final payment: %.*f\n", (int)prec, amortsched->final_pmt_opt_4);
    printf("5 -- Amortize with Delayed Amount, New Periodic Payment\n");
    printf("    and final payment: %.*f\n", (int)prec, amortsched->final_pmt_opt_5);
    if ( amortsched->new_n )
{
        printf("6 -- Amortize with Original Amount, Original Periodic Payment,\n");
        printf("    new number of total payments (n): %u\n", amortsched->new_n);
        printf("    and final payment: %.*f\n", (int)prec, amortsched->final_pmt_opt_6);
    } /* endif */
    printf("Enter choice 1, 2, 3, 4, 5 or 6: ");
    fgets(buffer, 190, stdin);
    amortsched->option = buffer[0] - '0';

    printf("Amortization Schedule:\n");
    printf("y -- Yearly Summary\n");
    printf("p -- Periodic Payment\n");
    if ( amortsched->option < 3 )
    {
        printf("Enter Choice y or p: ");
    }
    else
    {
        printf("f -- Fixed Advanced Payment\n");
        printf("a -- Variable Advanced Payment\n");
        printf("Enter Choice y, p, f or a: ");
    } /* endif */
    fgets(buffer, 190, stdin);
    amortsched->summary = buffer[0];

    if ( amortsched->summary == 'f' )
    {
        if ( amortsched->fixed_pmt != 0.0 )
        {
            printf("Current Fixed Prepayment: %.*f\nChange Fixed Prepayment? (y/n): ", (int)prec, amortsched->fixed_pmt);
            fgets(buffer, 190, stdin);
        }
        else
        {
            buffer[0] = 'y';
        } /* endif */

        if ( buffer[0] == 'y' )
        {
            printf("Enter Fixed Prepayment Amount: ");
            fgets(buffer, 190, stdin);
            if ( (errp = parse_string(&value, buffer, parse_env)) == NULL )
            {
                nval = (numeric_ptr)(value.value);
                switch ( nval->type )
                {
                case INT_TYPE:
                    amortsched->fixed_pmt = (double)(nval->value.int_value);
                    break;
                case DBL_TYPE:
                    amortsched->fixed_pmt = nval->value.dbl_value;
                    break;
                } /* endswitch */
                if ( !value.variable_name ) free_numeric(value.value);
            }
            else
            {
                parse_error(get_parse_error(parse_env), buffer, errp);
            } /* endif */
        } /* endif */
    } /* endif */

    return amortsched;
} /* amort_opt */
