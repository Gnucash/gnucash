/***************************************************************************
                          fin-interactive.c  -  description
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
 *  Functions to interact with user and call financial equations
 *  6-22-2000
 *
 */

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <mcheck.h>

#include "finvar.h"
#include "finproto.h"
#include "fin_spl_protos.h"
#include "numeric_ops.h"

static void  prt_status(
    fi_ptr       fi,
    FILE        *ofile);

static void set_fin_vars(
    void);

static void unset_fin_vars(
    void);

#define PREDEFINED_FIN_VARS 9

/* define local financial variables
 */
static unsigned npp;
static double   ir;
static double   pv;
static double   pmt;
static double   fv;
static unsigned CF;
static unsigned PF;
static unsigned disc;
static unsigned bep;

/* define local variable for roundoff precesion
 * default here to value for US currency
 */
static unsigned prec = 2;

/* declare array of structures for local financial variables
 */
static var_store      predefined_fin_vars[PREDEFINED_FIN_VARS];

/* declare array of finacial varibale names used by user to access financial variables
 */
static char *fin_var_names[] =
{
    "n",
    "i",
    "pv",
    "pmt",
    "fv",
    "CF",
    "PF",
    "disc",
    "bep",
};

/* declare array of financial variables
 */
static void *fin_vars[] =
{
    (void *)&npp,
    (void *)&ir,
    (void *)&pv,
    (void *)&pmt,
    (void *)&fv,
    (void *)&CF,
    (void *)&PF,
    (void *)&disc,
    (void *)&bep,
};

/* declare array of financial variable basic numeric types
 */
static char fin_type[] =
{
    INT_TYPE,
    DBL_TYPE,
    DBL_TYPE,
    DBL_TYPE,
    DBL_TYPE,
    INT_TYPE,
    INT_TYPE,
    INT_TYPE,
    INT_TYPE,
};

static char sl_commands[] = "acdqsv";

/* function to set local financial variables into array for use by expression parser
 * as pre-defined variables
 */
static void set_fin_vars(
    void)
{
    unsigned    cntr;
    numeric_ptr value;

    for ( cntr = 0 ; cntr < PREDEFINED_FIN_VARS ; cntr++ )
    {
        predefined_fin_vars[cntr].variable_name = fin_var_names[cntr];
        predefined_fin_vars[cntr].assign_flag = EOS;
        predefined_fin_vars[cntr].value = value = (numeric_ptr)calloc(1, sizeof(numeric));
        predefined_fin_vars[cntr].next_var = &predefined_fin_vars[cntr + 1];
        switch ( value->type = fin_type[cntr] )
        {
        case INT_TYPE:
            value->value.int_value = *(unsigned *)(fin_vars[cntr]);
            break;
        case DBL_TYPE:
            value->value.dbl_value = *(double *)(fin_vars[cntr]);
            break;
        } /* endswitch */
    } /* endfor */
    predefined_fin_vars[PREDEFINED_FIN_VARS - 1].next_var = NULL;
} /* set_fin_vars */

/* free storage used by local financial variables
 */
static void unset_fin_vars(
    void)
{
    unsigned    cntr;
    numeric_ptr value;

    for ( cntr = 0 ; cntr < PREDEFINED_FIN_VARS ; cntr++ )
    {
        free(predefined_fin_vars[cntr].value);
    } /* endfor */
} /* unset_fin_vars */

/* check variable set by expression parser against local financial variables
 * and update local values as necessary. Also convert variables to proper type
 * to reflect the native type of the local variable
 */
void            chk_vars(
    var_store_ptr   predefined_vars,
    void          **var_array,
    char           *var_type,
    unsigned        var_cnt)
{
    unsigned    cntr;
    numeric_ptr value;

    for ( cntr = 0 ; cntr < var_cnt ; cntr++ )
    {
        if ( predefined_vars[cntr].assign_flag == ASSIGNED_TO )
        {
            predefined_vars[cntr].assign_flag = EOS;
            value = (numeric_ptr)(predefined_vars[cntr].value);
            switch ( var_type[cntr] )
            {
            case INT_TYPE:
                switch ( value->type )
                {
                case INT_TYPE:
                    *(int *)(var_array[cntr]) = value->value.int_value;
                    break;
                case DBL_TYPE:
                    value->value.int_value      =
                        *(int *)(var_array[cntr]) = (unsigned)(value->value.dbl_value);
                    value->type = INT_TYPE;
                    break;
                } /* endswitch */
                break;
            case DBL_TYPE:
                switch ( value->type )
                {
                case INT_TYPE:
                    value->value.dbl_value         =
                        *(double *)(var_array[cntr]) = (double)(value->value.int_value);
                    value->type = DBL_TYPE;
                    break;
                case DBL_TYPE:
                    *(double *)(var_array[cntr]) = value->value.dbl_value;
                    break;
                } /* endswitch */
                break;
            } /* endswitch */
        } /* endif */
    } /* endfor */
} /* chk_fin_vars */

/* error encountered by expression parser - output error message
 * and offending string
 */
void           parse_error(unsigned error_code,
                           char *buf_start,
                           char *buf_err)
{
    char *err_str;
    unsigned       bc = (unsigned)(buf_err - buf_start);

    switch ( error_code )
    {
    case UNBALANCED_PARENS:
        err_str = "Unbalanced Parenthesis\n";
        break;
    case STACK_OVERFLOW:
        err_str = "Stack Overflow\n";
        break;
    case STACK_UNDERFLOW:
        err_str = "Stack Underflow\n";
        break;
    case UNDEFINED_CHARACTER:
        err_str = "Unrecognized Character\n";
        break;
    case NOT_A_VARIABLE:
        err_str = "Need a Variable on Left side of assignment operator, '='\n";
        break;
    case NOT_A_FUNC:
        err_str = "Need a valid Function name.\n";
        break;

    } /* endswitch */
    printf(err_str);
    printf("%s\n", buf_start);
    if ( bc ) for ( bc - 1 ; bc ; bc-- ) printf(" ");
    printf("^");
    /*	printf("%s\n",buf_err + 1);	*/
    printf("\n");
} /* parse_error */

int             main(int argc, char **argv, char **env)
{
    char   buffer[200], *errp;
    size_t          sbuf;
    size_t          retcnt;
    var_store       value;
    var_store_ptr   value_list;
    numeric_ptr     nval;
    unsigned        compute,
    jj,
    yrE,
    monthE,
    dayE,
    yrI,
    monthI,
    dayI;
    struct tm      *times_E,
                *times_I;
    void           *parse_env;
    amort_sched     amortsched;
    financial_info  fininfo;

    /* check dynamic storage allocation
     */
    /*  	mtrace();	*/
    set_default(&fininfo);
    set_fin_vars();
    parse_env = init_parser(predefined_fin_vars,
                            '.',
                            ',',
                            trans_numeric,
                            numeric_ops,
                            negate_numeric,
                            free_numeric);

    npp  = fininfo.npp;
    ir   = fininfo.ir;
    pv   = fininfo.pv;
    pmt  = fininfo.pmt;
    fv   = fininfo.fv;
    CF   = fininfo.CF;
    PF   = fininfo.PF;
    disc = fininfo.disc;
    bep  = fininfo.bep;

    fininfo.prec = prec;

    printf("Single Letter Commands:\na -- amortization schedule\nc -- compute financial variable\nd -- delete variable\ns -- output financial variable status\nq -- quit\nv -- list defined variables\n");
    for (;;)
{
        printf("<>");
        retcnt = strlen(fgets(buffer, 190, stdin));
        if ( (retcnt == 2) && (strchr(sl_commands, buffer[0]) != NULL) )
        {
            if ( buffer[0] == 'q' ) break;
            amortsched.prec = fininfo.prec;
            switch ( buffer[0] )
            {
            case 'a':
                if ( amortsched.Eff_Date_jdn && amortsched.Init_Date_jdn )
                {
                    printf("Current Effective  year: %u\nCurrent Effective month: %u\nCurrent Effective   day: %u\nCurrent Initial    year: %u\nCurrent Initial   month: %u\nCurrent Initial     day %u\n",
                           amortsched.year_E,
                           amortsched.month_E,
                           amortsched.day_E,
                           amortsched.year_I,
                           amortsched.month_I,
                           amortsched.day_I);
                    printf("Change dates ? (y/n) ");
                    fgets(buffer, 190, stdin);
                }
                else
                {
                    buffer[0] = 'y';
                } /* endif */
                if ( buffer[0] == 'y' )
                {
                    printf("Enter Effective Date - year: ");
                    fgets(buffer, 190, stdin);
                    if ( (errp = parse_string(&value, buffer, parse_env)) == NULL )
                    {
                        nval = (numeric_ptr)(value.value);
                        switch ( nval->type )
                        {
                        case INT_TYPE:
                            amortsched.year_E = nval->value.int_value;
                            break;
                        case DBL_TYPE:
                            amortsched.year_E = (unsigned)(nval->value.dbl_value);
                            break;
                        } /* endswitch */
                        if ( !value.variable_name ) free_numeric(value.value);
                    }
                    else
                    {
                        parse_error(get_parse_error(parse_env), buffer, errp);
                    } /* endif */
                    printf("Enter Effective Date - month: ");
                    fgets(buffer, 190, stdin);
                    if ( (errp = parse_string(&value, buffer, parse_env)) == NULL )
                    {
                        nval = (numeric_ptr)(value.value);
                        switch ( nval->type )
                        {
                        case INT_TYPE:
                            amortsched.month_E = nval->value.int_value;
                            break;
                        case DBL_TYPE:
                            amortsched.month_E = (unsigned)(nval->value.dbl_value);
                            break;
                        } /* endswitch */
                        if ( !value.variable_name ) free_numeric(value.value);
                    }
                    else
                    {
                        parse_error(get_parse_error(parse_env), buffer, errp);
                    } /* endif */
                    printf("Enter Effective Date - day: ");
                    fgets(buffer, 190, stdin);
                    if ( (errp = parse_string(&value, buffer, parse_env)) == NULL )
                    {
                        nval = (numeric_ptr)(value.value);
                        switch ( nval->type )
                        {
                        case INT_TYPE:
                            amortsched.day_E = nval->value.int_value;
                            break;
                        case DBL_TYPE:
                            amortsched.day_E = (unsigned)(nval->value.dbl_value);
                            break;
                        } /* endswitch */
                        if ( !value.variable_name ) free_numeric(value.value);
                    }
                    else
                    {
                        parse_error(get_parse_error(parse_env), buffer, errp);
                    } /* endif */
                    printf("Enter Initial Payment Date - year: ");
                    fgets(buffer, 190, stdin);
                    if ( (errp = parse_string(&value, buffer, parse_env)) == NULL )
                    {
                        nval = (numeric_ptr)(value.value);
                        switch ( nval->type )
                        {
                        case INT_TYPE:
                            amortsched.year_I = nval->value.int_value;
                            break;
                        case DBL_TYPE:
                            amortsched.year_I = (unsigned)(nval->value.dbl_value);
                            break;
                        } /* endswitch */
                        if ( !value.variable_name ) free_numeric(value.value);
                    }
                    else
                    {
                        parse_error(get_parse_error(parse_env), buffer, errp);
                    } /* endif */
                    printf("Enter Initial Payment Date - month: ");
                    fgets(buffer, 190, stdin);
                    if ( (errp = parse_string(&value, buffer, parse_env)) == NULL )
                    {
                        nval = (numeric_ptr)(value.value);
                        switch ( nval->type )
                        {
                        case INT_TYPE:
                            amortsched.month_I = nval->value.int_value;
                            break;
                        case DBL_TYPE:
                            amortsched.month_I = (unsigned)(nval->value.dbl_value);
                            break;
                        } /* endswitch */
                        if ( !value.variable_name ) free_numeric(value.value);
                    }
                    else
                    {
                        parse_error(get_parse_error(parse_env), buffer, errp);
                    } /* endif */
                    printf("Enter Initial Payment Date - day: ");
                    fgets(buffer, 190, stdin);
                    if ( (errp = parse_string(&value, buffer, parse_env)) == NULL )
                    {
                        nval = (numeric_ptr)(value.value);
                        switch ( nval->type )
                        {
                        case INT_TYPE:
                            amortsched.day_I = nval->value.int_value;
                            break;
                        case DBL_TYPE:
                            amortsched.day_I = (unsigned)(nval->value.dbl_value);
                            break;
                        } /* endswitch */
                        if ( !value.variable_name ) free_numeric(value.value);
                    }
                    else
                    {
                        parse_error(get_parse_error(parse_env), buffer, errp);
                    } /* endif */
                } /* endif */

                amortsched.n     = npp;
                amortsched.nint  = ir;
                amortsched.pv    = pv;
                amortsched.pmt   = pmt;
                amortsched.fv    = fv;
                amortsched.CF    = CF;
                amortsched.PF    = PF;
                amortsched.disc  = disc;
                amortsched.bep   = bep;

                Amortization_init(&amortsched);
                amort_opt(&amortsched, parse_env);

                (void)Amortization_Schedule(&amortsched);
                prt_amortization_schedule(&amortsched, stdout);
                Amortization_free(&amortsched);
                break;
            case 'c':

                printf("Compute:\nn   - 1\ni   - 2\npv  - 3\npmt - 4\nfv  - 5\n1, 2, 3, 4 or 5: ");
                retcnt = strlen(fgets(buffer, 190, stdin));
                compute = buffer[0] - '0';

                switch ( compute-- )
                {
                case 0: /* all values specified nothing to compute */
                    break;
                case 1: /* compute number of periods, npp */
                    printf("Computing numbor of periods\n");
                    npp = fi_calc_num_payments(&fininfo);
                    printf("Number of Periods: %u\n", npp);
                    nval = (numeric_ptr)(predefined_fin_vars[compute].value);
                    nval->value.int_value = npp;
                    break;
                case 2: /* compute interest, ir */
                    printf("Computing interest rate\n");
                    ir = fi_calc_interest(&fininfo);
                    printf("Nominal Interest Rate: %.*f\n", prec, ir);
                    nval = (numeric_ptr)(predefined_fin_vars[compute].value);
                    nval->value.dbl_value = ir;
                    break;
                case 3: /* compute present value, pv */
                    printf("Computing Present Value\n");
                    pv = fi_calc_present_value(&fininfo);
                    printf("Present Value: %.*f\n", prec, pv);
                    nval = (numeric_ptr)(predefined_fin_vars[compute].value);
                    nval->value.dbl_value = pv;
                    break;
                case 4: /* compute periodic payment, pmt */
                    printf("Computing periodic payment\n");
                    pmt = fi_calc_payment(&fininfo);
                    printf("Periodic Payment: %.*f\n", prec, pmt);
                    nval = (numeric_ptr)(predefined_fin_vars[compute].value);
                    nval->value.dbl_value = pmt;
                    break;
                case 5: /* compute future value, fv */
                    printf("Computing Future Value\n");
                    fv = fi_calc_future_value(&fininfo);
                    printf("Future Value: %.*f\n", prec, fv);
                    nval = (numeric_ptr)(predefined_fin_vars[compute].value);
                    nval->value.dbl_value = fv;
                    break;
                default:    /* whoops */
                    break;
                } /* endswitch */
                break;
            case 'd':
                printf("Enter name of variable to delete: ");
                retcnt = strlen(fgets(buffer, 190, stdin));
                buffer[retcnt - 1] = EOS;
                if ( !delete_var(buffer, parse_env) )
                {
                    printf("Unable to delete specified variable\n");
                } /* endif */
                break;
            case 's':
                prt_status(&fininfo,
                           stdout);
                break;
            case 'v':
                for ( value_list = parser_get_vars(parse_env) ; value_list ; value_list = value_list->next_var )
                {
                    printf("%s: ", value_list->variable_name);
                    nval = (numeric_ptr)(value_list->value);
                    switch ( nval->type )
                    {
                    case INT_TYPE:
                        printf("%i\n", nval->value.int_value);
                        break;
                    case DBL_TYPE:
                        printf("%.*f\n", prec, nval->value.dbl_value);
                        break;
                    } /* endswitch */
                } /* endfor */
                break;
            } /* endswitch */
        }
        else if ( retcnt > 1 )
        {
            buffer[retcnt - 1] = EOS;

            if ( (errp = parse_string(&value, buffer, parse_env)) == NULL )
            {
                if ( value.variable_name ) printf("Variable: %s\n", value.variable_name);
                nval = (numeric_ptr)(value.value);
                switch ( nval->type )
                {
                case INT_TYPE:
                    printf("Evaluated Value: %i\n", nval->value.int_value);
                    break;
                case DBL_TYPE:
                    printf("Evaluated Value: %.*f\n", prec, nval->value.dbl_value);
                    break;
                } /* endswitch */
                if ( !value.variable_name ) free_numeric(value.value);
                chk_vars(predefined_fin_vars, fin_vars, fin_type, PREDEFINED_FIN_VARS);
                fininfo.npp = npp;
                fininfo.ir = ir;
                fininfo.pv = pv;
                fininfo.pmt = pmt;
                fininfo.fv = fv;
                fininfo.CF = CF;
                fininfo.PF = PF;
                fininfo.disc = disc;
                fininfo.bep = bep;
            }
            else
            {
                parse_error(get_parse_error(parse_env), buffer, errp);
            } /* endif */
        } /* endif */
    } /* endfor */
    exit_parser(parse_env);
    unset_fin_vars();
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
    fprintf(ofile, "Future Value (fv): %.*f\n<================================>\n", fi->prec, fi->fv);
}   /* prt_status */
