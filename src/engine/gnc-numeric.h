/********************************************************************
 * gnc-numeric.h -- an exact-number library for gnucash.            *
 * Copyright (C) 2000 Bill Gribble                                  *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
 *******************************************************************/

#ifndef GNC_NUMERIC_H
#define GNC_NUMERIC_H

#include <glib.h>

struct _gnc_numeric {
  gint64  num;
  gint64  denom;
};

typedef struct _gnc_numeric gnc_numeric;

/* bitmasks for HOW flags */
#define GNC_NUMERIC_RND_MASK     0x0000000f
#define GNC_NUMERIC_DENOM_MASK   0x000000f0
#define GNC_NUMERIC_SIGFIGS_MASK 0x0000ff00

/* rounding/truncation modes for operations */
enum { 
  GNC_RND_FLOOR            = 0x01, 
  GNC_RND_CEIL             = 0x02,  
  GNC_RND_TRUNC            = 0x03,
  GNC_RND_PROMOTE          = 0x04,
  GNC_RND_ROUND_HALF_DOWN  = 0x05, 
  GNC_RND_ROUND_HALF_UP    = 0x06, 
  GNC_RND_ROUND            = 0x07, 
  GNC_RND_NEVER            = 0x08
};

/* auto-denominator types */
enum { 
  GNC_DENOM_EXACT  = 0x10, 
  GNC_DENOM_REDUCE = 0x20,
  GNC_DENOM_LCD    = 0x30,
  GNC_DENOM_FIXED  = 0x40,
  GNC_DENOM_SIGFIG = 0x50
};

/* bits 8-15 of 'how' are reserved for the number of significant
 * digits to use in the output with GNC_DENOM_SIGFIG */ 

/* errors */
enum {
  GNC_ERROR_OK         =  0,
  GNC_ERROR_ARG        = -1,
  GNC_ERROR_OVERFLOW   = -2,
  GNC_ERROR_DENOM_DIFF = -3,
  GNC_ERROR_REMAINDER  = -4
};

#define GNC_DENOM_AUTO 0

#define GNC_DENOM_RECIPROCAL( a ) (- ( a ))
#define GNC_DENOM_SIGFIGS( a ) ( ((( a ) & 0xff) << 8) | GNC_DENOM_SIGFIG)
#define GNC_NUMERIC_GET_SIGFIGS( a ) ( (( a ) & 0xff00 ) >> 8)

/* make a gnc_numeric from numerator and denominator */
gnc_numeric gnc_numeric_create(gint64 num, gint64 denom);

/* create a zero-value gnc_numeric */
gnc_numeric gnc_numeric_zero(void);

/* make a special error-signalling gnc_numeric */
gnc_numeric gnc_numeric_error(int error_code);

/* check for error signal in value */ 
int         gnc_numeric_check(gnc_numeric a);

/* get parts */
gint64 gnc_numeric_num(gnc_numeric a);
gint64 gnc_numeric_denom(gnc_numeric a);

/* tests */
int gnc_numeric_zero_p(gnc_numeric a);                /* 1 if 0, 0 else */
int gnc_numeric_compare(gnc_numeric a, gnc_numeric b);
int gnc_numeric_negative_p(gnc_numeric a);
int gnc_numeric_positive_p(gnc_numeric a);

/* equivalence predicates : 
 * eq    : a and b are exactly the same (same numerator and denominator)
 * equal : a and b represent exactly the same number (ratio of numerator
 *         to denominator is exactly equal)
 * same  : after both are converted to DENOM using method HOW, a and b 
 *         are equal().
 */ 
int gnc_numeric_eq(gnc_numeric a, gnc_numeric b);     
int gnc_numeric_equal(gnc_numeric a, gnc_numeric b);  
int gnc_numeric_same(gnc_numeric a, gnc_numeric b,   
                     gint64 denom, gint how);

/* arithmetic operations */
gnc_numeric gnc_numeric_add(gnc_numeric a, gnc_numeric b, 
                            gint64 denom, gint how);
gnc_numeric gnc_numeric_sub(gnc_numeric a, gnc_numeric b, 
                            gint64 denom, gint how);
gnc_numeric gnc_numeric_mul(gnc_numeric a, gnc_numeric b, 
                            gint64 denom, gint how);
gnc_numeric gnc_numeric_div(gnc_numeric a, gnc_numeric b, 
                            gint64 denom, gint how);
gnc_numeric gnc_numeric_neg(gnc_numeric a);
gnc_numeric gnc_numeric_abs(gnc_numeric a);

/* some shortcuts for common operations */
gnc_numeric gnc_numeric_add_fixed(gnc_numeric a, gnc_numeric b);
gnc_numeric gnc_numeric_sub_fixed(gnc_numeric a, gnc_numeric b);

/* arithmetic functions with exact error returns */
gnc_numeric gnc_numeric_add_with_error(gnc_numeric a, gnc_numeric b,
                                       gint64 denom, gint how,
                                       gnc_numeric * error);
gnc_numeric gnc_numeric_sub_with_error(gnc_numeric a, gnc_numeric b,
                                       gint64 denom, gint how,
                                       gnc_numeric * error);
gnc_numeric gnc_numeric_mul_with_error(gnc_numeric a, gnc_numeric b,
                                       gint64 denom, gint how,
                                       gnc_numeric * error);
gnc_numeric gnc_numeric_div_with_error(gnc_numeric a, gnc_numeric b,
                                       gint64 denom, gint how,
                                       gnc_numeric * error);

/* change the denominator of a gnc_numeric value */
gnc_numeric gnc_numeric_convert(gnc_numeric in, gint64 denom, 
                                gint how);

gnc_numeric gnc_numeric_convert_with_error(gnc_numeric in, gint64 denom, 
                                           gint how,
                                           gnc_numeric * error);

/* reduce by GCF elimination */
gnc_numeric gnc_numeric_reduce(gnc_numeric in);

/* convert to and from floating-point values */
gnc_numeric double_to_gnc_numeric(double in, gint64 denom,  
                                  gint how);
double      gnc_numeric_to_double(gnc_numeric in);

gchar *gnc_numeric_to_string(gnc_numeric n);

/* Read a gnc_numeric from str, skipping any leading whitespace, and
   returning a pointer to just past the last byte read.  Return NULL
   on error. */
const gchar *string_to_gnc_numeric(const gchar* str, gnc_numeric *n);

#endif
