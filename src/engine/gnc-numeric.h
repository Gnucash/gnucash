/********************************************************************
 * gnc-numeric.h - An exact-number library for gnucash              *
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

/** @addtogroup Numeric
    The 'Numeric' functions provide a way of working with rational
    numbers while maintaining strict control over rounding errors
    when adding rationals with different denominators.  The Numeric
    class is primarily used for working with monetary amounts, 
    where the denominator typically represents the smallest fraction
    of the currency (e.g. pennies, centimes).  The numeric class
    can handle any fraction (e.g. twelfth's) and is not limited
    to fractions that are powers of ten.

    Please refer to the GnuCash texinfo documentation for details 
    on the numeric functions.  (The texinfo will someday be
    brought inline, here, into this file).

    @{ */
/** @file gnc-numeric.h
    @brief An exact-rational-number library for gnucash.
    @author Copyright (C) 2000 Bill Gribble
*/

#ifndef GNC_NUMERIC_H
#define GNC_NUMERIC_H

#include <glib.h>

struct _gnc_numeric {
  gint64  num;
  gint64  denom;
};

/** @brief An exact-number type
 *
 * This is a rational number, defined by nominator and denominator. */
typedef struct _gnc_numeric gnc_numeric;

/** bitmasks for HOW flags.
 * bits 8-15 of 'how' are reserved for the number of significant
 * digits to use in the output with GNC_DENOM_SIGFIG */ 

#define GNC_NUMERIC_RND_MASK     0x0000000f
#define GNC_NUMERIC_DENOM_MASK   0x000000f0
#define GNC_NUMERIC_SIGFIGS_MASK 0x0000ff00

/** rounding/truncation modes for operations */
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

/** auto-denominator types */
enum { 
  GNC_DENOM_EXACT  = 0x10, 
  GNC_DENOM_REDUCE = 0x20,
  GNC_DENOM_LCD    = 0x30,
  GNC_DENOM_FIXED  = 0x40,
  GNC_DENOM_SIGFIG = 0x50
};

/** Error codes */
typedef enum {
  GNC_ERROR_OK         =  0,   /**< No error */
  GNC_ERROR_ARG        = -1,   /**< Argument is not a valid number */
  GNC_ERROR_OVERFLOW   = -2,   /**< Intermediate result overflow */
  GNC_ERROR_DENOM_DIFF = -3,   /**< Argument denoms differ in GNC_DENOM_FIXED operation */
  GNC_ERROR_REMAINDER  = -4    /**< Remainder part in GNC_RND_NEVER operation */
} GNCNumericErrorCode;

#define GNC_DENOM_AUTO 0

#define GNC_DENOM_RECIPROCAL( a ) (- ( a ))
#define GNC_DENOM_SIGFIGS( a ) ( ((( a ) & 0xff) << 8) | GNC_DENOM_SIGFIG)
#define GNC_NUMERIC_GET_SIGFIGS( a ) ( (( a ) & 0xff00 ) >> 8)

/** @name Constructors */
/*@{*/
/** make a gnc_numeric from numerator and denominator */
gnc_numeric gnc_numeric_create(gint64 num, gint64 denom);

/** create a zero-value gnc_numeric */
gnc_numeric gnc_numeric_zero(void);

/** convert from floating-point values */
gnc_numeric double_to_gnc_numeric(double in, gint64 denom,  
                                  gint how);

/** Read a gnc_numeric from str, skipping any leading whitespace, and
   returning a pointer to just past the last byte read.  Return NULL
   on error. */
const gchar *string_to_gnc_numeric(const gchar* str, gnc_numeric *n);

/** make a special error-signalling gnc_numeric */
gnc_numeric gnc_numeric_error(GNCNumericErrorCode error_code);
/*@}*/

/** @name Value accessors */
/*@{*/
/** Get parts */
gint64 gnc_numeric_num(gnc_numeric a);
/** Get parts */
gint64 gnc_numeric_denom(gnc_numeric a);

/** Convert to floating-point values */
double      gnc_numeric_to_double(gnc_numeric in);

/** Convert to string. The returned buffer is to be g_free'd by the
 * caller (it was allocated through g_strdup) */
gchar *gnc_numeric_to_string(gnc_numeric n);
/*@}*/

/** @name Tests */
/*@{*/
/** Check for error signal in value. Returns GNC_ERROR_OK (==0) if
 *  the number appears to be valid, otherwise it returns the
 *  type of error.
 */ 
GNCNumericErrorCode  gnc_numeric_check(gnc_numeric a);

/** Returns 1 if the given gnc_numeric is 0 (zeros), else returns 0. */
int gnc_numeric_zero_p(gnc_numeric a);                
/** Returns 1 if a>b, -1 if b>a, 0 if a == b  */
int gnc_numeric_compare(gnc_numeric a, gnc_numeric b);
int gnc_numeric_negative_p(gnc_numeric a);
int gnc_numeric_positive_p(gnc_numeric a);

/** Equivalence predicate: Returns TRUE (1) if a and b are exactly the
 * same (same numerator and denominator)
 */ 
gboolean gnc_numeric_eq(gnc_numeric a, gnc_numeric b);     

/** Equivalence predicate: Returns TRUE (1) if a and b represent
 *  exactly the same number (ratio of numerator to denominator is
 *  exactly equal)
 */ 
gboolean gnc_numeric_equal(gnc_numeric a, gnc_numeric b);  

/** Equivalence predicate: Returns TRUE (1) if after both are
 * converted to DENOM using method HOW, a and b are
 * gnc_numeric_equal().
 */ 
int gnc_numeric_same(gnc_numeric a, gnc_numeric b,   
                     gint64 denom, gint how);
/*@}*/

/** @name Arithmetic operations */
/*@{*/
gnc_numeric gnc_numeric_add(gnc_numeric a, gnc_numeric b, 
                            gint64 denom, gint how);
gnc_numeric gnc_numeric_sub(gnc_numeric a, gnc_numeric b, 
                            gint64 denom, gint how);
gnc_numeric gnc_numeric_mul(gnc_numeric a, gnc_numeric b, 
                            gint64 denom, gint how);

/** Division.  Note that division can overflow, in the following 
 *  sense: if we write x=a/b and y=c/d  then x/y = (a*d)/(b*c)  
 *  If, after eliminating all common factors between the numerator 
 *  (a*d) and the denominator (b*c),  then if either the numerator 
 *  and/or the denominator are *still* greater than 2^63, then 
 *  the division has overflowed.
 */
gnc_numeric gnc_numeric_div(gnc_numeric x, gnc_numeric y, 
                            gint64 denom, gint how);
/** Negate the argument  */
gnc_numeric gnc_numeric_neg(gnc_numeric a);
/** Return the absolute value of the argument */
gnc_numeric gnc_numeric_abs(gnc_numeric a);

/** 
 * Shortcut for most common case: gnc_numeric_add(a, b, GNC_DENOM_AUTO,
 *                        GNC_DENOM_FIXED | GNC_RND_NEVER);
 */
gnc_numeric gnc_numeric_add_fixed(gnc_numeric a, gnc_numeric b);
/** 
 * Shortcut for most common case: gnc_numeric_sub(a, b, GNC_DENOM_AUTO,
 *                        GNC_DENOM_FIXED | GNC_RND_NEVER);
 */
gnc_numeric gnc_numeric_sub_fixed(gnc_numeric a, gnc_numeric b);
/*@}*/

/** @name Arithmetic functions with exact error returns */
/*@{*/
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
/*@}*/

/** @name Change denominator */
/*@{*/
/** change the denominator of a gnc_numeric value */
gnc_numeric gnc_numeric_convert(gnc_numeric in, gint64 denom, 
                                gint how);

/** change the denominator of a gnc_numeric value */
gnc_numeric gnc_numeric_convert_with_error(gnc_numeric in, gint64 denom, 
                                           gint how,
                                           gnc_numeric * error);

/** reduce by GCF elimination */
gnc_numeric gnc_numeric_reduce(gnc_numeric in);
/*@}*/


#endif

/*@}*/
