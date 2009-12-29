/***************************************************************************
                          numeric_ops.c  -  description
                             -------------------
    begin                : Wednesday June 21 2000
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
 *  Functions to execute arthmetic operators on integer and double operands
 *  6-23-2000
 *
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <float.h>
#include <math.h>

#define NUMERIC_OPS_STATICS
#include "finvar.h"

static double neg_table[] =
{
    1e-256,
    1e-128,
    1e-64,
    1e-32,
    1e-16,
    1e-8,
    1e-4,
    1e-2,
    1e-1,
    1.0
};

static double pos_table[] =
{
    1e+256,
    1e+128,
    1e+64,
    1e+32,
    1e+16,
    1e+8,
    1e+4,
    1e+2,
    1e+1
};

#define MAX_SCALE ((LONG_MAX - 10) / 10)

/* function to translate ASCII string to numeric format.
 *
 * Recognizes either integer numerics or floating point numerics.
 * Recognizes integers in format:
 *  (sign)digit_sequence
 *  digit_sequence may contain a grouping character, the grouping character is ignored
 *  optional sign == '+' or '-'
 *
 *  Recognizes floating point in formats:
 *  (sign)digit_sequence.digits(exp)
 *  (sign)digit_sequence.(exp)
 *  (sign)digit_sequence(exp)
 *  (sign).digits(exp)
 *  '.' represents the radix point passed, digit_sequence may contain a grouping character
 *      the grouping character is ignored
 *  optional sign == '+' or '-'
 *  optional exp == ('e' or 'E')(sign)digits
 *
 * Terminates on first unrecognized character.
 *
 */
void           *trans_numeric(
    const char *str,              /* pointer to string to translate */
    char   radix_point,      /* radix character                */
    char   group_char,       /* grouping character to left of radix  */
    char **endstr)           /* where to return pointer to first unrecognized character */
{
    double         dblval = 0.0;
    int            exp = 0,
                         dchr,
                         err = 0,
                               base = 10;
    long int       inum = 0;
    unsigned long  msdec = 0,
                           lsdec = 0,
                                   msscale = 1;
    unsigned       radix = 0,
                           sign = 0,
                                  digit_cnt = 0;
    const char    *strinit = str;
    numeric_ptr    rslt = NULL;

    while ( isspace(*str) ) str++;

    switch (*str)
    {
    case '-':
        sign++;
    case '+':
        str++;
    default:
        break;
    } /* endswitch */

    while ( *str )
    {

        while ( (*str >= '0') && (*str <= '9') )
        {
            digit_cnt++;

            if ( msdec < MAX_SCALE ) msdec = msdec * 10 + (*str - '0');
            else if ( msscale < MAX_SCALE )
            {
                lsdec = lsdec * 10 + (*str - '0');
                msscale *= 10;
            }
            else exp++;

            if ( radix ) exp--;
            else
            {
                dchr = *str - '0';
                if ( ((LONG_MIN + dchr) / base) > inum ) err = 1;
                inum = inum * base + dchr;
            } /* endif */
            str++;
        } /* endwhile */

        if ( !radix )
        {
            if ( *str == radix_point ) radix++;
            else if ( *str != group_char ) break;
        }
        else
        {
            break;
        } /* endif */

        str++;
    } /* endwhile */

    if ( digit_cnt )
    {
        unsigned      exp_dcnt = 0;

        if ( (*str == 'e') || (*str == 'E') )
        {
            char exp_sign = EOS;
            int  ex_exp = 0;

            switch (*++str)
            {
            case '-':
                exp_sign++;
            case '+':
                str++;
            default:
                break;
            } /* endswitch */

            while ( (*str >= '0') && (*str <= '9') )
            {
                if (ex_exp < (DBL_MAX_EXP * 2) ) ex_exp = ex_exp * 10 + (*str - '0');
                str++;
                exp_dcnt++;
            } /* endwhile */

            exp += exp_sign ? -ex_exp : ex_exp;
        } /* endif */

        if ( radix || exp )
        {
            int pow = 256;

            dblval = msdec;
            if ( msscale != 1 )	dblval = dblval * msscale + lsdec;

            if ( dblval && exp )
            {
                unsigned u = 0;

                pow = 256;
                while ( exp > 0 )
                {
                    while ( exp >= pow )
                    {
                        dblval *= pos_table[u];
                        exp -= pow;
                    } /* endwhile */
                    pow >>= 1;
                    u++;
                } /* endwhile */

                while ( exp < 0 )
                {
                    while ( exp <= -pow )
                    {
                        dblval *= neg_table[u];
                        if ( dblval == 0.0 )
                        {
                            errno = ERANGE;
                            err = 1;
                        } /* endif */
                        exp += pow;
                    } /* endwhile */
                    pow >>= 1;
                    u++;
                } /* endwhile */

                /* if overflow occurred		*/
                if ( dblval == HUGE_VAL )
                {
                    errno = ERANGE;
                    err = 1;
                } /* endif */
            } /* endif */

            if ( !err )
            {
                rslt = (numeric_ptr)calloc(1, sizeof(numeric));
                rslt->type = DBL_TYPE;
                rslt->value.dbl_value = dblval;
            } /* endif */
        }
        else
        {
            if ( (!sign && (inum == LONG_MIN)) || err )
            {
                inum = LONG_MIN + sign;
                errno = ERANGE;
            }
            else
            {
                rslt = (numeric_ptr)calloc(1, sizeof(numeric));
                rslt->type = INT_TYPE;
                rslt->value.int_value = inum;
            } /* endif */
        } /* endif */
    } /* endif */

    if ( endstr )
    {
        if ( !digit_cnt ) *endstr = (char *) strinit;
        else *endstr = (char *) str;
    } /* endif */

    return (void *)rslt;
} /* strtod_flt */

/* function to free memory used by numeric structure
 */
void  free_numeric(
    void *numeric_value)
{
    if ( numeric_value ) free(numeric_value);
} /* free_numeric */

/* function to perform unary '-' operation
 */
void *negate_numeric(
    void *value)
{
    numeric_ptr rslt = (numeric_ptr)value;

    switch ( rslt->type )
    {
    case INT_TYPE:
        rslt->value.int_value = -rslt->value.int_value;
        break;
    case DBL_TYPE:
        rslt->value.dbl_value = -rslt->value.dbl_value;
        break;
    } /* endswitch */

    return (void *)rslt;
} /* negate_numeric */

/* function to perform binary operators
 *	op_symbol - operation to perform
 *		ADD_OP	== perform '+'
 *		SUB_OP  == perform '-'
 *		DIV_OP	== perform '/'
 *		MUL_OP	== perform '*'
 *		ASN_OP	== perform '='
 *  l_value - pointer to left hand value
 *  r_value - pointer to right hand value
 */
void          *numeric_ops(
    char  op_symbol,
    void *l_value,
    void *r_value)
{
    numeric_ptr lval = (numeric_ptr)l_value,
                       rval = (numeric_ptr)r_value,
                              rslt = (op_symbol == ASN_OP) ? lval : (numeric_ptr)calloc(1, sizeof(numeric));

    switch ( op_symbol )
    {
    case ADD_OP:
        if ( lval->type == rval->type )
        {
            rslt->type = lval->type;
            switch ( lval->type )
            {
            case INT_TYPE:
                rslt->value.int_value = lval->value.int_value + rval->value.int_value;
                break;
            case DBL_TYPE:
                rslt->value.dbl_value = lval->value.dbl_value + rval->value.dbl_value;
                break;
            } /* endswitch */
        }
        else
        {
            rslt->type = DBL_TYPE;
            switch ( lval->type )
            {
            case INT_TYPE:
                rslt->value.dbl_value = (double)(lval->value.int_value) + rval->value.dbl_value;
                break;
            case DBL_TYPE:
                rslt->value.dbl_value = lval->value.dbl_value + (double)(rval->value.int_value);
                break;

            } /* endswitch */
        } /* endif */
        break;
    case SUB_OP:
        if ( lval->type == rval->type )
        {
            rslt->type = lval->type;
            switch ( lval->type )
            {
            case INT_TYPE:
                rslt->value.int_value = lval->value.int_value - rval->value.int_value;
                break;
            case DBL_TYPE:
                rslt->value.dbl_value = lval->value.dbl_value - rval->value.dbl_value;
                break;
            } /* endswitch */
        }
        else
        {
            rslt->type = DBL_TYPE;
            switch ( lval->type )
            {
            case INT_TYPE:
                rslt->value.dbl_value = (double)(lval->value.int_value) - rval->value.dbl_value;
                break;
            case DBL_TYPE:
                rslt->value.dbl_value = lval->value.dbl_value - (double)(rval->value.int_value);
                break;

            } /* endswitch */
        } /* endif */
        break;
    case DIV_OP:
        rslt->type = DBL_TYPE;
        if ( lval->type == rval->type )
        {
            switch ( lval->type )
            {
            case INT_TYPE:
                rslt->value.dbl_value = (double)(lval->value.int_value) / (double)(rval->value.int_value);
                break;
            case DBL_TYPE:
                rslt->value.dbl_value = lval->value.dbl_value / rval->value.dbl_value;
                break;
            } /* endswitch */
        }
        else
        {
            switch ( lval->type )
            {
            case INT_TYPE:
                rslt->value.dbl_value = (double)(lval->value.int_value) / rval->value.dbl_value;
                break;
            case DBL_TYPE:
                rslt->value.dbl_value = lval->value.dbl_value / (double)(rval->value.int_value);
                break;

            } /* endswitch */
        } /* endif */
        break;
    case MUL_OP:
        if ( lval->type == rval->type )
        {
            rslt->type = lval->type;
            switch ( lval->type )
            {
            case INT_TYPE:
                rslt->value.int_value = lval->value.int_value * rval->value.int_value;
                break;
            case DBL_TYPE:
                rslt->value.dbl_value = lval->value.dbl_value * rval->value.dbl_value;
                break;
            } /* endswitch */
        }
        else
        {
            rslt->type = DBL_TYPE;
            switch ( lval->type )
            {
            case INT_TYPE:
                rslt->value.dbl_value = (double)(lval->value.int_value) * rval->value.dbl_value;
                break;
            case DBL_TYPE:
                rslt->value.dbl_value = lval->value.dbl_value * (double)(rval->value.int_value);
                break;

            } /* endswitch */
        } /* endif */
        break;
    case ASN_OP:
        if ( !lval ) lval = (numeric_ptr)calloc(1, sizeof(numeric));
        lval->type = rval->type;
        lval->value = rval->value;
        rslt = lval;
        break;
    } /* endswitch */


    return (void *)rslt;
} /* numeric_ops */
