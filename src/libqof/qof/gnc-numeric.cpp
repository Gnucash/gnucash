/********************************************************************
 * gnc-numeric.c -- an exact-number library for accounting use      *
 * Copyright (C) 2000 Bill Gribble                                  *
 * Copyright (C) 2004 Linas Vepstas <linas@linas.org>               *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
 *******************************************************************/
#ifdef __cplusplus
extern "C"
{
#endif

#include "config.h"

#include <glib.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef __cplusplus
}
#endif

#include "gnc-numeric.h"
#include "gnc-rational.hpp"

using GncNumeric = GncRational;

static const gint64 pten[] = { 1, 10, 100, 1000, 10000, 100000, 1000000,
			       10000000, 100000000, 1000000000, 10000000000,
			       100000000000, 1000000000000, 10000000000000,
			       100000000000000, 10000000000000000,
			       100000000000000000, 1000000000000000000};
static const int POWTEN_OVERFLOW {-5};

static inline gint64
powten (int exp)
{
    if (exp > 18 || exp < -18)
	return POWTEN_OVERFLOW;
    return exp < 0 ? -pten[-exp] : pten[exp];
}


/* =============================================================== */
/* This function is small, simple, and used everywhere below,
 * lets try to inline it.
 */
GNCNumericErrorCode
gnc_numeric_check(gnc_numeric in)
{
    if (G_LIKELY(in.denom != 0))
    {
        return GNC_ERROR_OK;
    }
    else if (in.num)
    {
        if ((0 < in.num) || (-4 > in.num))
        {
            in.num = (gint64) GNC_ERROR_OVERFLOW;
        }
        return (GNCNumericErrorCode) in.num;
    }
    else
    {
        return GNC_ERROR_ARG;
    }
}


/* *******************************************************************
 *  gnc_numeric_zero_p
 ********************************************************************/

gboolean
gnc_numeric_zero_p(gnc_numeric a)
{
    if (gnc_numeric_check(a))
    {
        return 0;
    }
    else
    {
        if ((a.num == 0) && (a.denom != 0))
        {
            return 1;
        }
        else
        {
            return 0;
        }
    }
}

/* *******************************************************************
 *  gnc_numeric_negative_p
 ********************************************************************/

gboolean
gnc_numeric_negative_p(gnc_numeric a)
{
    if (gnc_numeric_check(a))
    {
        return 0;
    }
    else
    {
        if ((a.num < 0) && (a.denom != 0))
        {
            return 1;
        }
        else
        {
            return 0;
        }
    }
}

/* *******************************************************************
 *  gnc_numeric_positive_p
 ********************************************************************/

gboolean
gnc_numeric_positive_p(gnc_numeric a)
{
    if (gnc_numeric_check(a))
    {
        return 0;
    }
    else
    {
        if ((a.num > 0) && (a.denom != 0))
        {
            return 1;
        }
        else
        {
            return 0;
        }
    }
}


/* *******************************************************************
 *  gnc_numeric_compare
 *  returns 1 if a>b, -1 if b>a, 0 if a == b
 ********************************************************************/

int
gnc_numeric_compare(gnc_numeric a, gnc_numeric b)
{
    gint64 aa, bb;

    if (gnc_numeric_check(a) || gnc_numeric_check(b))
    {
        return 0;
    }

    if (a.denom == b.denom)
    {
        if (a.num == b.num) return 0;
        if (a.num > b.num) return 1;
        return -1;
    }

    GncNumeric an (a), bn (b);

    return (an.m_num * bn.m_den).cmp(bn.m_num * an.m_den);
}


/* *******************************************************************
 *  gnc_numeric_eq
 ********************************************************************/

gboolean
gnc_numeric_eq(gnc_numeric a, gnc_numeric b)
{
    return ((a.num == b.num) && (a.denom == b.denom));
}


/* *******************************************************************
 *  gnc_numeric_equal
 ********************************************************************/

gboolean
gnc_numeric_equal(gnc_numeric a, gnc_numeric b)
{
    return gnc_numeric_compare (a, b) == 0;
}


/* *******************************************************************
 *  gnc_numeric_same
 *  would a and b be equal() if they were both converted to the same
 *  denominator?
 ********************************************************************/

int
gnc_numeric_same(gnc_numeric a, gnc_numeric b, gint64 denom,
                 gint how)
{
    gnc_numeric aconv, bconv;

    aconv = gnc_numeric_convert(a, denom, how);
    bconv = gnc_numeric_convert(b, denom, how);

    return(gnc_numeric_equal(aconv, bconv));
}


/* *******************************************************************
 *  gnc_numeric_add
 ********************************************************************/

gnc_numeric
gnc_numeric_add(gnc_numeric a, gnc_numeric b,
                gint64 denom, gint how)
{
    if (gnc_numeric_check(a) || gnc_numeric_check(b))
    {
        return gnc_numeric_error(GNC_ERROR_ARG);
    }

    GncNumeric an (a), bn (b);
    GncDenom new_denom (an, bn, denom, how);
    if (new_denom.m_error)
        return gnc_numeric_error (new_denom.m_error);


    return static_cast<gnc_numeric>(an.add(bn, new_denom));
}

/* *******************************************************************
 *  gnc_numeric_sub
 ********************************************************************/

gnc_numeric
gnc_numeric_sub(gnc_numeric a, gnc_numeric b,
                gint64 denom, gint how)
{
    gnc_numeric nb;
    if (gnc_numeric_check(a) || gnc_numeric_check(b))
    {
        return gnc_numeric_error(GNC_ERROR_ARG);
    }

    nb = b;
    nb.num = -nb.num;
    return gnc_numeric_add (a, nb, denom, how);
}

/* *******************************************************************
 *  gnc_numeric_mul
 ********************************************************************/

gnc_numeric
gnc_numeric_mul(gnc_numeric a, gnc_numeric b,
                gint64 denom, gint how)
{
    if (gnc_numeric_check(a) || gnc_numeric_check(b))
    {
        return gnc_numeric_error(GNC_ERROR_ARG);
    }

    GncNumeric an (a), bn (b);
    GncDenom new_denom (an, bn, denom, how);
    if (new_denom.m_error)
        return gnc_numeric_error (new_denom.m_error);

    return static_cast<gnc_numeric>(an.mul(bn, new_denom));
}


/* *******************************************************************
 *  gnc_numeric_div
 ********************************************************************/

gnc_numeric
gnc_numeric_div(gnc_numeric a, gnc_numeric b,
                gint64 denom, gint how)
{
    if (gnc_numeric_check(a) || gnc_numeric_check(b))
    {
        return gnc_numeric_error(GNC_ERROR_ARG);
    }

    GncNumeric an (a), bn (b);
    GncDenom new_denom (an, bn, denom, how);
    if (new_denom.m_error)
        return gnc_numeric_error (new_denom.m_error);

    return static_cast<gnc_numeric>(an.div(bn, new_denom));
}

/* *******************************************************************
 *  gnc_numeric_neg
 *  negate the argument
 ********************************************************************/

gnc_numeric
gnc_numeric_neg(gnc_numeric a)
{
    if (gnc_numeric_check(a))
    {
        return gnc_numeric_error(GNC_ERROR_ARG);
    }
    return gnc_numeric_create(- a.num, a.denom);
}

/* *******************************************************************
 *  gnc_numeric_abs
 *  return the absolute value of the argument
 ********************************************************************/

gnc_numeric
gnc_numeric_abs(gnc_numeric a)
{
    if (gnc_numeric_check(a))
    {
        return gnc_numeric_error(GNC_ERROR_ARG);
    }
    return gnc_numeric_create(ABS(a.num), a.denom);
}


/* *******************************************************************
 *  gnc_numeric_convert
 ********************************************************************/

gnc_numeric
gnc_numeric_convert(gnc_numeric in, int64_t denom, int how)
{
    GncNumeric a (in), b (gnc_numeric_zero());
    GncDenom d (a, b, denom, how);
    a.round (d);
    return static_cast<gnc_numeric>(a);
}


/* *******************************************************************
 *  reduce a fraction by GCF elimination.  This is NOT done as a
 *  part of the arithmetic API unless GNC_HOW_DENOM_REDUCE is specified
 *  as the output denominator.
 ********************************************************************/

gnc_numeric
gnc_numeric_reduce(gnc_numeric in)
{
    if (gnc_numeric_check(in))
    {
        return gnc_numeric_error(GNC_ERROR_ARG);
    }

    if (in.denom < 0) /* Negative denoms multiply num, can't be reduced. */
        return in;
    GncNumeric a (in), b (gnc_numeric_zero());
    GncDenom d (a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE | GNC_HOW_RND_ROUND);
    a.round (d);
    return static_cast<gnc_numeric>(a);
}


/* *******************************************************************
 * gnc_numeric_to_decimal
 *
 * Attempt to convert the denominator to an exact power of ten without
 * rounding. TRUE is returned if 'a' has been converted or was already
 * decimal. Otherwise, FALSE is returned and 'a' remains unchanged.
 * The 'max_decimal_places' parameter may be NULL.
 ********************************************************************/

gboolean
gnc_numeric_to_decimal(gnc_numeric *a, guint8 *max_decimal_places)
{
    guint8 decimal_places = 0;
    gnc_numeric converted_val;
    gint64 fraction;

    g_return_val_if_fail(a, FALSE);

    if (gnc_numeric_check(*a) != GNC_ERROR_OK)
        return FALSE;

    converted_val = *a;
    if (converted_val.denom <= 0)
    {
        converted_val = gnc_numeric_convert(converted_val, 1, GNC_HOW_DENOM_EXACT);
        if (gnc_numeric_check(converted_val) != GNC_ERROR_OK)
            return FALSE;
        *a = converted_val;
        if (max_decimal_places)
            *max_decimal_places = decimal_places;
        return TRUE;
    }

    /* Zero is easily converted. */
    if (converted_val.num == 0)
        converted_val.denom = 1;

    fraction = converted_val.denom;
    while (fraction != 1)
    {
        switch (fraction % 10)
        {
        case 0:
            fraction = fraction / 10;
            break;

        case 5:
            converted_val = gnc_numeric_mul(converted_val,
                                            gnc_numeric_create(2, 2),
                                            GNC_DENOM_AUTO,
                                            GNC_HOW_DENOM_EXACT |
                                            GNC_HOW_RND_NEVER);
            if (gnc_numeric_check(converted_val) != GNC_ERROR_OK)
                return FALSE;
            fraction = fraction / 5;
            break;

        case 2:
        case 4:
        case 6:
        case 8:
            converted_val = gnc_numeric_mul(converted_val,
                                            gnc_numeric_create(5, 5),
                                            GNC_DENOM_AUTO,
                                            GNC_HOW_DENOM_EXACT |
                                            GNC_HOW_RND_NEVER);
            if (gnc_numeric_check(converted_val) != GNC_ERROR_OK)
                return FALSE;
            fraction = fraction / 2;
            break;

        default:
            return FALSE;
        }

        decimal_places += 1;
    }

    if (max_decimal_places)
        *max_decimal_places = decimal_places;

    *a = converted_val;

    return TRUE;
}


/* *******************************************************************
 *  double_to_gnc_numeric
 ********************************************************************/

#ifdef _MSC_VER
# define rint /* */
#endif
gnc_numeric
double_to_gnc_numeric(double in, gint64 denom, gint how)
{
    gnc_numeric out;
    gint64 int_part = 0;
    double frac_part;
    gint64 frac_int = 0;
    double logval;
    double sigfigs;

    if (isnan (in) || fabs (in) > 1e18)
	return gnc_numeric_error (GNC_ERROR_OVERFLOW);

    if ((denom == GNC_DENOM_AUTO) && (how & GNC_HOW_DENOM_SIGFIG))
    {
        if (fabs(in) < 10e-20)
        {
            logval = 0;
        }
        else
        {
            logval   = log10(fabs(in));
            logval   = ((logval > 0.0) ?
                        (floor(logval) + 1.0) : (ceil(logval)));
        }
        sigfigs  = GNC_HOW_GET_SIGFIGS(how);
	if ((denom = powten (sigfigs - logval)) == POWTEN_OVERFLOW)
            return gnc_numeric_error(GNC_ERROR_OVERFLOW);

        how =  how & ~GNC_HOW_DENOM_SIGFIG & ~GNC_NUMERIC_SIGFIGS_MASK;
    }

    int_part  = (gint64)(floor(fabs(in)));
    frac_part = in - (double)int_part;

    int_part = int_part * denom;
    frac_part = frac_part * (double)denom;

    switch (how & GNC_NUMERIC_RND_MASK)
    {
    case GNC_HOW_RND_FLOOR:
        frac_int = (gint64)floor(frac_part);
        break;

    case GNC_HOW_RND_CEIL:
        frac_int = (gint64)ceil(frac_part);
        break;

    case GNC_HOW_RND_TRUNC:
        frac_int = (gint64)frac_part;
        break;

    case GNC_HOW_RND_ROUND:
    case GNC_HOW_RND_ROUND_HALF_UP:
        frac_int = (gint64)rint(frac_part);
        break;

    case GNC_HOW_RND_NEVER:
        frac_int = (gint64)floor(frac_part);
        if (frac_part != (double) frac_int)
        {
            /* signal an error */
        }
        break;
    }

    out.num   = int_part + frac_int;
    out.denom = denom;
    return out;
}

/* *******************************************************************
 *  gnc_numeric_to_double
 ********************************************************************/

double
gnc_numeric_to_double(gnc_numeric in)
{
    if (in.denom > 0)
    {
        return (double)in.num / (double)in.denom;
    }
    else
    {
        return (double)(in.num * -in.denom);
    }
}

/* *******************************************************************
 *  gnc_numeric_error
 ********************************************************************/

gnc_numeric
gnc_numeric_error(GNCNumericErrorCode error_code)
{
    return gnc_numeric_create(error_code, 0LL);
}


/* *******************************************************************
 *  gnc_numeric_add_with_error
 ********************************************************************/

gnc_numeric
gnc_numeric_add_with_error(gnc_numeric a, gnc_numeric b,
                           gint64 denom, gint how,
                           gnc_numeric * error)
{

    gnc_numeric sum   = gnc_numeric_add(a, b, denom, how);
    gnc_numeric exact = gnc_numeric_add(a, b, GNC_DENOM_AUTO,
                                        GNC_HOW_DENOM_REDUCE);
    gnc_numeric err   = gnc_numeric_sub(sum, exact, GNC_DENOM_AUTO,
                                        GNC_HOW_DENOM_REDUCE);

    if (error)
    {
        *error = err;
    }
    return sum;
}

/* *******************************************************************
 *  gnc_numeric_sub_with_error
 ********************************************************************/

gnc_numeric
gnc_numeric_sub_with_error(gnc_numeric a, gnc_numeric b,
                           gint64 denom, gint how,
                           gnc_numeric * error)
{
    gnc_numeric diff  = gnc_numeric_sub(a, b, denom, how);
    gnc_numeric exact = gnc_numeric_sub(a, b, GNC_DENOM_AUTO,
                                        GNC_HOW_DENOM_REDUCE);
    gnc_numeric err   = gnc_numeric_sub(diff, exact, GNC_DENOM_AUTO,
                                        GNC_HOW_DENOM_REDUCE);
    if (error)
    {
        *error = err;
    }
    return diff;
}


/* *******************************************************************
 *  gnc_numeric_mul_with_error
 ********************************************************************/

gnc_numeric
gnc_numeric_mul_with_error(gnc_numeric a, gnc_numeric b,
                           gint64 denom, gint how,
                           gnc_numeric * error)
{
    gnc_numeric prod  = gnc_numeric_mul(a, b, denom, how);
    gnc_numeric exact = gnc_numeric_mul(a, b, GNC_DENOM_AUTO,
                                        GNC_HOW_DENOM_REDUCE);
    gnc_numeric err   = gnc_numeric_sub(prod, exact, GNC_DENOM_AUTO,
                                        GNC_HOW_DENOM_REDUCE);
    if (error)
    {
        *error = err;
    }
    return prod;
}


/* *******************************************************************
 *  gnc_numeric_div_with_error
 ********************************************************************/

gnc_numeric
gnc_numeric_div_with_error(gnc_numeric a, gnc_numeric b,
                           gint64 denom, gint how,
                           gnc_numeric * error)
{
    gnc_numeric quot  = gnc_numeric_div(a, b, denom, how);
    gnc_numeric exact = gnc_numeric_div(a, b, GNC_DENOM_AUTO,
                                        GNC_HOW_DENOM_REDUCE);
    gnc_numeric err   = gnc_numeric_sub(quot, exact,
                                        GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE);
    if (error)
    {
        *error = err;
    }
    return quot;
}

/* *******************************************************************
 *  gnc_numeric text IO
 ********************************************************************/

gchar *
gnc_numeric_to_string(gnc_numeric n)
{
    gchar *result;
    gint64 tmpnum = n.num;
    gint64 tmpdenom = n.denom;

    result = g_strdup_printf("%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT, tmpnum, tmpdenom);

    return result;
}

gchar *
gnc_num_dbg_to_string(gnc_numeric n)
{
    static char buff[1000];
    static char *p = buff;
    gint64 tmpnum = n.num;
    gint64 tmpdenom = n.denom;

    p += 100;
    if (p - buff >= 1000) p = buff;

    sprintf(p, "%" G_GINT64_FORMAT "/%" G_GINT64_FORMAT, tmpnum, tmpdenom);

    return p;
}

gboolean
string_to_gnc_numeric(const gchar* str, gnc_numeric *n)
{
    gint64 tmpnum;
    gint64 tmpdenom;

    if (!str) return FALSE;

    tmpnum = g_ascii_strtoll (str, NULL, 0);
    str = strchr (str, '/');
    if (!str) return FALSE;
    str ++;
    tmpdenom = g_ascii_strtoll (str, NULL, 0);

    n->num = tmpnum;
    n->denom = tmpdenom;
    return TRUE;
}

/* *******************************************************************
 *  GValue handling
 ********************************************************************/
static gpointer
gnc_numeric_boxed_copy_func( gpointer in_gnc_numeric )
{
    gnc_numeric* newvalue;

    newvalue = static_cast<gnc_numeric*>(g_malloc (sizeof (gnc_numeric)));
    memcpy( newvalue, in_gnc_numeric, sizeof( gnc_numeric ) );

    return newvalue;
}

static void
gnc_numeric_boxed_free_func( gpointer in_gnc_numeric )
{
    g_free( in_gnc_numeric );
}

GType
gnc_numeric_get_type( void )
{
    static GType type = 0;

    if ( type == 0 )
    {
        type = g_boxed_type_register_static( "gnc_numeric",
                                             gnc_numeric_boxed_copy_func,
                                             gnc_numeric_boxed_free_func );
    }

    return type;
}

/* *******************************************************************
 *  gnc_numeric misc testing
 ********************************************************************/
#ifdef _GNC_NUMERIC_TEST

static char *
gnc_numeric_print(gnc_numeric in)
{
    char * retval;
    if (gnc_numeric_check(in))
    {
        retval = g_strdup_printf("<ERROR> [%" G_GINT64_FORMAT " / %" G_GINT64_FORMAT "]",
                                 in.num,
                                 in.denom);
    }
    else
    {
        retval = g_strdup_printf("[%" G_GINT64_FORMAT " / %" G_GINT64_FORMAT "]",
                                 in.num,
                                 in.denom);
    }
    return retval;
}

int
main(int argc, char ** argv)
{
    gnc_numeric a = gnc_numeric_create(1, 3);
    gnc_numeric b = gnc_numeric_create(1, 4);
    gnc_numeric c;

    gnc_numeric err;

    c = gnc_numeric_add_with_error(a, b, 100, GNC_HOW_RND_ROUND, &err);
    printf("add 100ths/error : %s + %s = %s + (error) %s\n\n",
           gnc_numeric_print(a), gnc_numeric_print(b),
           gnc_numeric_print(c),
           gnc_numeric_print(err));

    c = gnc_numeric_sub_with_error(a, b, 100, GNC_HOW_RND_FLOOR, &err);
    printf("sub 100ths/error : %s - %s = %s + (error) %s\n\n",
           gnc_numeric_print(a), gnc_numeric_print(b),
           gnc_numeric_print(c),
           gnc_numeric_print(err));

    c = gnc_numeric_mul_with_error(a, b, 100, GNC_HOW_RND_ROUND, &err);
    printf("mul 100ths/error : %s * %s = %s + (error) %s\n\n",
           gnc_numeric_print(a), gnc_numeric_print(b),
           gnc_numeric_print(c),
           gnc_numeric_print(err));

    c = gnc_numeric_div_with_error(a, b, 100, GNC_HOW_RND_ROUND, &err);
    printf("div 100ths/error : %s / %s = %s + (error) %s\n\n",
           gnc_numeric_print(a), gnc_numeric_print(b),
           gnc_numeric_print(c),
           gnc_numeric_print(err));

    printf("multiply (EXACT): %s * %s = %s\n",
           gnc_numeric_print(a), gnc_numeric_print(b),
           gnc_numeric_print(gnc_numeric_mul(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_EXACT)));

    printf("multiply (REDUCE): %s * %s = %s\n",
           gnc_numeric_print(a), gnc_numeric_print(b),
           gnc_numeric_print(gnc_numeric_mul(a, b, GNC_DENOM_AUTO, GNC_HOW_DENOM_REDUCE)));


    return 0;
}
#endif

const char* gnc_numeric_errorCode_to_string(GNCNumericErrorCode error_code)
{
    switch (error_code)
    {
    case GNC_ERROR_OK:
        return "GNC_ERROR_OK";
    case GNC_ERROR_ARG:
        return "GNC_ERROR_ARG";
    case GNC_ERROR_OVERFLOW:
        return "GNC_ERROR_OVERFLOW";
    case GNC_ERROR_DENOM_DIFF:
        return "GNC_ERROR_DENOM_DIFF";
    case GNC_ERROR_REMAINDER:
        return "GNC_ERROR_REMAINDER";
    default:
        return "<unknown>";
    }
}

/* ======================== END OF FILE =================== */
