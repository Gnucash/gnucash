/***************************************************************************
 *              -------------------
 *    create   : Sat Jun 17 20:14:13 2000
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
 *  Global Financial Variables
 *  Sat Jun 17 20:14:13 2000
 *
 ***************************************************************************/

#ifndef FINVAR_H
#define FINVAR_H

#if !defined( EOS )
#define EOS '\x000'
#endif

#if !defined( TRUE )
#define TRUE (1)
#endif

#if !defined( FALSE )
#define FALSE (0)
#endif

#define INT_TYPE    '\x001'
#define DBL_TYPE    '\x002'

typedef enum
{
    PARSER_NO_ERROR = 0,
    UNBALANCED_PARENS,
    STACK_OVERFLOW,
    STACK_UNDERFLOW,
    UNDEFINED_CHARACTER,
    NOT_A_VARIABLE,
    NOT_A_FUNC,
    PARSER_OUT_OF_MEMORY,
    NUMERIC_ERROR,
    EXPRESSION_ERROR,
    PARSER_NUM_ERRORS
}
ParseError;

#define UNUSED_VAR  '\x000'
#define USED_VAR    '\x001'
#define ASSIGNED_TO '\x002'

#define ADD_OP  '+'
#define SUB_OP  '-'
#define DIV_OP  '/'
#define MUL_OP  '*'
#define ASN_OP  '='

/* The following structure is used by the expression parser to store
 * named and temporary variables.  */

/* structure used for storing variables - used by expression parser/evaluator
 */
typedef struct var_store *var_store_ptr;

/* the type of entity contained in the var_store */
typedef enum
{
    VST_NUMERIC = 0,
    VST_STRING
} VarStoreType;

typedef struct var_store
{
    char *variable_name;	  /* variable name if variable, NULL otherwise       */
    char use_flag;	  /* flag if variable has been assigned to           */
    char assign_flag;	  /* flag if variable is used                        */
    VarStoreType type;
    void *value;		  /* pointer to implementation defined numeric value */
    var_store_ptr next_var; /* pointer to next variable in linked list         */
}
var_store;


/* The following structure is used for the numeric operations
 * involving double float and integer arithmetic */

/* structure used for storing numeric values - used by routines which
 * evaluate arithmetic operators '+', '-', '/', '*' */
typedef struct numeric *numeric_ptr;
typedef struct numeric
{
    char type;			/* designates type of value */
    union
    {
        long int int_value;		/* long integer value   */
        double dbl_value;		/* double value         */
    }
    value;
}
numeric;

/* The following structures are used by the amortization functions for
 * storing amortization schedule information */

/* structure used by amortization routines for storing annual summary
 information */
typedef struct yearly_summary *yearly_summary_ptr;
typedef struct yearly_summary
{
    unsigned year;
    double interest;
    double end_balance;
}
yearly_summary;

/* structure used by amortization routines for storing information on
 a single payment */
typedef struct sched_pmt *sched_pmt_ptr;
typedef struct sched_pmt
{
    unsigned period_num;
    double interest;
    double principal;
    double advanced_pmt;
    double total_pmt;
    double balance;
}
sched_pmt;

/* structure used by amortization routines for storing information on
 * payments for a single year */
typedef struct amort_sched_yr *amort_sched_yr_ptr;
typedef struct amort_sched_yr
{
    unsigned year;
    unsigned num_periods;
    sched_pmt_ptr payments;
    double interest_pd;
    double principal_pd;
    double yr_end_balance;
    double total_interest_pd;
    double final_pmt;
    amort_sched_yr_ptr next_yr;
}
amort_sched_yr;

/* structure used by amortization routines for passing and storing
 * infomation on a particular amortization transaction */
typedef struct amort_sched *amort_sched_ptr;
typedef struct amort_sched
{
    /* following information set by function calling amortization
       functions */
    unsigned n;			/* number of periods                        */
    double nint;			/* nominal interest rate                    */
    double pv;			/* present value                            */
    double pmt;			/* periodic payment                         */
    double fv;			/* future value                             */
    unsigned CF;			/* compounding frequency                    */
    unsigned PF;			/* payment frequency                        */
    unsigned disc;		/* discrete/continuous compounding flag     */
    unsigned bep;			/* beginning/end of period payment flag     */
    unsigned prec;		/* roundoff precision                       */
    unsigned year_E;		/* Effective date - year                    */
    unsigned month_E;		/* Effective date - month                   */
    unsigned day_E;		/* Effective date - day of month            */
    unsigned year_I;		/* Initial payment date - year              */
    unsigned month_I;		/* Initial payment date - month             */
    unsigned day_I;		/* Initial payment date - day of month      */

    /* following information set by calling function to indicate which
     * schedule to compute and which type of schedule */
    unsigned option;		/* option flag from 1 to 6 inclusive        */
    char summary;			/* summary flag == 'y', 'p', 'a' or 'f'     */

    /* following information set by amortization functions */
    double eint;			/* effective interest rate                  */
    double bp;			/* float value of bep                       */
    double total_interest;	/* total interest paid                  */
    unsigned total_periods;	/* total numer of periods in schedule   */
    unsigned long yr_pmt;		/* number of payments in first year         */
    double final_pmt_opt_1;	/* final payment option 1 */
    double final_pmt_opt_2;	/* final payment option 2 */
    double final_pmt_opt_3;	/* final payment option 3 */
    double final_pmt_opt_4;	/* final payment option 4 */
    double final_pmt_opt_5;	/* final payment option 5 */
    double final_pmt_opt_6;	/* final payment option 6 */
    double final_pmt;		/* final payment          */
    double pve;			/* pv adjusted for delayed initial payment  */
    double new_pmt;		/* pmt adjusted for delayed initial payment */
    double cpmt;			/* constant payment to principal            */
    double cpmt1;			/* constant payment to principal, 1st case  */
    double cpmt2;			/* constant payment to principal, 2cd case  */
    double delayed_int;		/* interest due to delayed initial payment  */
    double fixed_pmt;		/* fixed prepayment amount for amortization */
    unsigned new_n;		/* new number of periods to amortize due to
                                   delayed intial payment */
    unsigned fv_case;		/* fv case flag */
    unsigned long Eff_Date_jdn;
    unsigned yday_E;
    unsigned long Init_Date_jdn;
    unsigned yday_I;
    union
    {
        amort_sched_yr_ptr first_yr;
        yearly_summary_ptr summary;
    }
    schedule;
}
amort_sched;

/* The following structure is used to hold all of the financial
 * variables used by the financial calculator */

/* structure used by financial computation routines to store financial
   variables */
typedef struct financial_info *fi_ptr;
typedef struct financial_info
{
    double ir;			/* interest rate            */
    double pv;			/* present value            */
    double pmt;			/* periodic payment         */
    double fv;			/* future value             */

    unsigned npp;			/* number of payment periods            */
    unsigned CF;			/* Compounding frequency                */
    unsigned PF;			/* payment frequency                    */
    unsigned bep;			/* beginning/end of period payment flag */
    /* TRUE  == beginning of period         */
    /* FALSE == end of period               */
    unsigned disc;		/* discrete/continuous compounding flag */
    /* TRUE  == discrete compounding        */
    /* FALSE == continuous compounding      */

    /* precision of roundoff for pv, pmt and fv.
     * i, Interest not rounded
     * n, number of periods rounded to integer value, implicit value of zero, 0
     *
     * 2 for US Dollars
     */
    unsigned prec;
}
financial_info;

typedef struct parser_env *parser_env_ptr;

#endif
