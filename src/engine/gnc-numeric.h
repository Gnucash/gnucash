/********************************************************************
 * gnc-numeric.h - A rational number library                        *
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

/** @addtogroup Rational
    The 'Numeric' functions provide a way of working with rational
    numbers while maintaining strict control over rounding errors
    when adding rationals with different denominators.  The Numeric
    class is primarily used for working with monetary amounts, 
    where the denominator typically represents the smallest fraction
    of the currency (e.g. pennies, centimes).  The numeric class
    can handle any fraction (e.g. twelfth's) and is not limited
    to fractions that are powers of ten.

    A 'Numeric' value represents a number in rational form, with a
    64-bit integer as numerator and denominator.  Rationals are
    ideal for many uses, such as performing exact, roundoff-error-free
    addition and multiplication, but 64-bit rationals do not have 
    the dynamic range of floating point numbers.  

    @{ */
/** @file gnc-numeric.h
    @brief An exact-rational-number library for gnucash.
    @author Copyright (C) 2000 Bill Gribble
    @author Copyright (C) 2004 Linas Vepstas <linas@linas.org>
*/

#if JUNK

/*******************
@menu
* Standard Numeric Arguments::  
* Creating Numeric Objects::    
* Basic Arithmetic Operations::  
* Numeric Comparisons and Predicates::  
* Numeric Denominator Conversion::  
* Numeric Floating Point Conversion::  
* Numeric String Conversion::   
* Numeric Error Handling ::     
* Numeric Example::             
@end menu

@node Standard Numeric Arguments, Creating Numeric Objects, Numeric Library, Numeric Library
@subsection Standard Numeric Arguments
@cindex Standard Numeric Arguments

It is useful to specify a denominator in cases where it is known that
the output value is of constrained precision. For example, monetary
transactions must be executed in an integer number of the "smallest
currency unit" of the transaction. In US Dollars, the smallest currency
unit is the cent, and all monetary transactions must be done in units of
cents. Therefore, any fractional cents in a computed price must be
rounded away.

Most of the @code{gnc_numeric} arithmetic functions take two arguments
in addition to their numeric args: @var{denom}, which is the denominator
to use in the output @code{gnc_numeric object}, and @var{how}, which
describes how the arithmetic result is to be converted to that
denominator. This combination of output denominator and rounding policy
allows the results of financial and other exact computations to be
properly rounded to the appropriate units.

Valid values for @var{denom} are:

Valid values for @var{how} are bitwise combinations of zero or one
"rounding instructions" with zero or one "denominator types". 

The denominator type specifies how to compute a denominator if
@code{GNC_DENOM_AUTO} is specified as the @var{denom}. Valid denominator
types are:



To use traditional rational-number operational semantics (all results
are exact and are reduced to relatively-prime fractions) pass the
argument @code{GNC_DENOM_AUTO} as @var{denom} and @code{GNC_DENOM_REDUCE
| GNC_RND_NEVER} as @var{how}.

To enforce strict financial semantics (such that all operands must have
the same denominator as each other and as the result), use
@var{GNC_DENOM_AUTO} as @var{denom} and @code{GNC_DENOM_FIXED |
GNC_RND_NEVER} as @var{how}.


@node Creating Numeric Objects, Basic Arithmetic Operations, Standard Numeric Arguments, Numeric Library
@subsection Creating Numeric Objects
@cindex Creating Numeric Objects

@deftypefun gnc_numeric gnc_numeric_create (int @var{num}, int @var{denom})
Create a @code{gnc_numeric} object with a value of "@var{num} / @var{denom}".
@end deftypefun

@deftypefun gnc_numeric gnc_numeric_zero ()
Create a @code{gnc_numeric} object with a value of 0. 
@end deftypefun


@node Basic Arithmetic Operations, Numeric Comparisons and Predicates, Creating Numeric Objects, Numeric Library
@subsection Basic Arithmetic Operations
@cindex Basic Arithmetic Operations

See @ref{Standard Numeric Arguments} for a description of the @var{denom}
and @var{how} arguments to each arithmetic function.

@deftypefun gnc_numeric gnc_numeric_add (gnc_numeric @var{a}, gnc_numeric @var{b}, gint64 @var{denom}, gint @var{how})
Return the sum of @var{a} and @var{b}.
@end deftypefun

@deftypefun gnc_numeric gnc_numeric_sub (gnc_numeric @var{a}, gnc_numeric @var{b}, gint64 @var{denom}, gint @var{how})
Return "@var{a} - @var{b}".
@end deftypefun

@deftypefun gnc_numeric gnc_numeric_div (gnc_numeric @var{a}, gnc_numeric @var{b}, gint64 @var{denom}, gint @var{how})
Return "@var{a} / @var{b}".
@end deftypefun

@deftypefun gnc_numeric gnc_numeric_add_with_error (gnc_numeric @var{a}, gnc_numeric @var{b}, gint64 @var{denom}, gint @var{how}, {gnc_numeric *} @var{error})
The same as @code{gnc_numeric_add}, but uses @var{error} for accumulating
conversion roundoff error.
@end deftypefun

@deftypefun gnc_numeric gnc_numeric_sub_with_error (gnc_numeric @var{a}, gnc_numeric @var{b}, gint64 @var{denom}, gint @var{how}, {gnc_numeric *} @var{error})
The same as @code{gnc_numeric_sub}, but uses @var{error} for accumulating
conversion roundoff error.
@end deftypefun

@deftypefun gnc_numeric gnc_numeric_div_with_error (gnc_numeric @var{a}, gnc_numeric @var{b}, gint64 @var{denom}, gint @var{how}, {gnc_numeric *} @var{error})
The same as @code{gnc_numeric_div}, but uses @var{error} for accumulating
conversion roundoff error.
@end deftypefun


@node Numeric Comparisons and Predicates, Numeric Denominator Conversion, Basic Arithmetic Operations, Numeric Library
@subsection Numeric Comparisons and Predicates
@cindex Numeric Comparisons and Predicates

@deftypefun int gnc_numeric_compare (gnc_numeric @var{a}, gnc_numeric @var{b})
Returns +1 if @code{@var{a} > @var{b}}, -1 if @code{@var{b} > @var{a}}, 0 if @code{@var{a} == @var{b}}.
@end deftypefun

@deftypefun int gnc_numeric_eq (gnc_numeric @var{a}, gnc_numeric @var{b})
Returns 1 if @code{numerator(@var{a}) == numerator(@var{b})} and
@code{denominator(@var{a}) == denominator(@var{b})}, otherwise returns 0.
@end deftypefun

@deftypefun int gnc_numeric_equal (gnc_numeric @var{a}, gnc_numeric @var{b})
Returns 1 if the fraction represented by @var{a} is equal to the fraction
represented by @var{b}, otherwise returns 0.
@end deftypefun

@deftypefun int gnc_numeric_same (gnc_numeric @var{a}, gnc_numeric @var{b}, gint64 @var{denom}, gint @var{how})
Convert both @var{a} and @var{b} to @var{denom} (@pxref{Standard Numeric
Arguments} and compare numerators of the result.

@example
  For example, if @code{@var{a} == 7/16} and @code{@var{b} == 3/4},
  @code{gnc_numeric_same(@var{a}, @var{b}, 2, GNC_RND_TRUNC) == 1}
  because both 7/16 and 3/4 round to 1/2 under truncation. However,
  @code{gnc_numeric_same(@var{a}, @var{b}, 2, GNC_RND_ROUND) == 0}
  because 7/16 rounds to 1/2 under unbiased rounding but 3/4 rounds
  to 2/2.
@end example
@end deftypefun


@node Numeric Denominator Conversion, Numeric Floating Point Conversion, Numeric Comparisons and Predicates, Numeric Library
@subsection Numeric Denominator Conversion
@cindex Numeric Denominator Conversion

@deftypefun gnc_numeric gnc_numeric_convert (gnc_numeric @var{in}, gint64 @var{denom}, gint @var{how})
Convert @var{in} to the specified denominator under standard arguments
@var{denom} and @var{how}. @xref{Standard Numeric Arguments}.
@end deftypefun

@deftypefun gnc_numeric gnc_numeric_convert_with_error (gnc_numeric @var{in}, gint64 @var{denom}, gint @var{how}, {gnc_numeric *} @var{error})
Same as @code{gnc_numeric_convert}, but return a remainder value for
accumulating conversion error.
@end deftypefun

@node Numeric Floating Point Conversion, Numeric String Conversion, Numeric Denominator Conversion, Numeric Library
@subsection Numeric Floating Point Conversion
@cindex Numeric Floating Point Conversion

@deftypefun gnc_numeric double_to_gnc_numeric (double @var{arg}, gint64 @var{denom}, gint @var{how})
Convert a floating-point number to a @code{gnc_numeric}. Both @var{denom}
and @var{how} are used as in arithmetic, but @code{GNC_DENOM_AUTO} is 
not recognized.
@end deftypefun

@deftypefun double gnc_numeric_to_double (gnc_numeric @var{arg})
Convert @var{arg} to a @code{double} value.
@end deftypefun


@node Numeric String Conversion, Numeric Error Handling , Numeric Floating Point Conversion, Numeric Library
@subsection Numeric String Conversion
@cindex Numeric String Conversion

@deftypefun {gchar *} gnc_numeric_to_string (gnc_numeric @var{n})
Return a string representation of @var{n}. The string must be
freed with @code{g_free}.
@end deftypefun

@deftypefun {const gchar *} string_to_gnc_numeric (const {gchar *} @var{str}, {gnc_numeric *} @var{n})
Read a @code{gnc_numeric} from @var{str}, skipping any leading
whitespace, and returning a pointer to just past the last byte
read. Return NULL on error.
@end deftypefun


@node Numeric Error Handling , Numeric Example, Numeric String Conversion, Numeric Library
@subsection Numeric Error Handling 
@cindex Numeric Error Handling 


@deftypefun gnc_numeric gnc_numeric_error (int error_code)
Create a @code{gnc_numeric} object that signals the error condition
noted by @var{error_code} rather than a number.
@end deftypefun


@node Numeric Example,  , Numeric Error Handling , Numeric Library
@subsection Numeric Example
@cindex Numeric Example

The following program finds the best @code{gnc_numeric} approximation to
the @file{math.h} constant @code{M_PI} given a maximum denominator. For
large denominators, the @code{gnc_numeric} approximation is accurate to
more decimal places than will generally be needed, but in some cases
this may not be good enough. For example,

@example
    M_PI                   = 3.14159265358979323846
    245850922 / 78256779   = 3.14159265358979311599  (16 sig figs)
    3126535 / 995207       = 3.14159265358865047446  (12 sig figs)
    355 / 113              = 3.14159292035398252096  (7 sig figs)
@end example

@example
#include <glib.h>
#include "gnc-numeric.h"
#include <math.h>

int
main(int argc, char ** argv)
@{
  gnc_numeric approx, best;
  double err, best_err=1.0;
  double m_pi = M_PI;
  gint64 denom;
  gint64 max;

  sscanf(argv[1], "%Ld", &max);
  
  for (denom = 1; denom < max; denom++)
  @{
    approx = double_to_gnc_numeric (m_pi, denom, GNC_RND_ROUND);
    err    = m_pi - gnc_numeric_to_double (approx);
    if (fabs (err) < fabs (best_err))
    @{
      best = approx;
      best_err = err;
      printf ("%Ld / %Ld = %.30f\n", gnc_numeric_num (best),
              gnc_numeric_denom (best), gnc_numeric_to_double (best));
    @}
  @}
@}
@end example

**********************/
#endif


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

/** Rounding/Truncation modes for operations.
 *  Rounding instructions control how fractional parts in the specified
 *  denominator affect the result. For example, if a computed result is
 *  "3/4" but the specified denominator for the return value is 2, should
 *  the return value be "1/2" or "2/2"?  
 *
 * Possible rounding instructions are:
 */
enum { 
  /** Round toward -infinity */
  GNC_RND_FLOOR            = 0x01, 

  /** Round toward +infinity */
  GNC_RND_CEIL             = 0x02,  

  /** Truncate fractions (round toward zero) */
  GNC_RND_TRUNC            = 0x03,

  /** Promote fractions (round away from zero) */
  GNC_RND_PROMOTE          = 0x04,

  /** Round to the nearest integer, rounding toward zero 
   *  when there are two equidistant nearest integers.
   */
  GNC_RND_ROUND_HALF_DOWN  = 0x05, 

  /** Round to the nearest integer, rounding away from zero 
   *  when there are two equidistant nearest integers.
   */
  GNC_RND_ROUND_HALF_UP    = 0x06, 

  /** Use unbiased ("banker's") rounding. This rounds to the 
   *  nearest integer, and to the nearest even integer when there 
   *  are two equidistant nearest integers. This is generally the 
   *  one you should use for financial quantities.
   */
  GNC_RND_ROUND            = 0x07, 

  /** Never round at all, and signal an error if there is a 
   *  fractional result in a computation.
   */
  GNC_RND_NEVER            = 0x08
};

/** How to compute a denominator, or'ed into the "how" field. */
enum { 
  /** Use any denominator which gives an exactly correct ratio of 
   *  numerator to denominator. Use EXACT when you do not wish to 
   *  lose any information in the result but also do not want to 
   *  spend any time finding the "best" denominator.
   */
  GNC_DENOM_EXACT  = 0x10, 

  /** Reduce the result value by common factor elimination, 
   *  using the smallest possible value for the denominator that 
   *  keeps the correct ratio. The numerator and denominator of 
   *  the result are relatively prime. 
   */
  GNC_DENOM_REDUCE = 0x20,

  /** Find the least common multiple of the arguments' denominators 
   *  and use that as the denominator of the result.
   */
  GNC_DENOM_LCD    = 0x30,

  /** All arguments are required to have the same denominator,
   *  that denominator is to be used in the output, and an error 
   *  is to be signaled if any argument has a different denominator.
   */
  GNC_DENOM_FIXED  = 0x40,

  /** Round to the number of significant figures given in the rounding
   *  instructions by the GNC_DENOM_SIGFIGS () macro.
   */
  GNC_DENOM_SIGFIG = 0x50
};

/** Error codes */
typedef enum {
  GNC_ERROR_OK         =  0,   /**< No error */
  GNC_ERROR_ARG        = -1,   /**< Argument is not a valid number */
  GNC_ERROR_OVERFLOW   = -2,   /**< Intermediate result overflow */

  /** GNC_DENOM_FIXED was specified, but argument denominators differed.  */
  GNC_ERROR_DENOM_DIFF = -3,   

  /** GNC_RND_NEVER  was specified, but the result could not be
   *  converted to the desired denominator without a remainder. */
  GNC_ERROR_REMAINDER  = -4   
} GNCNumericErrorCode;


/** Values that can be passed as the 'denom' argument.  
 *  The include a positive number n to be used as the 
 *  denominator of teh output value.  Other possibilities 
 *  include the list below:
 */

/** Compute an appropriate denominator automatically. Flags in 
 *  the @var{how} argument will specify how to compute the denominator.
 */
#define GNC_DENOM_AUTO 0

/** Use the value @code{1/n} as the denominator of the output value. */
#define GNC_DENOM_RECIPROCAL( a ) (- ( a ))

/** Use a value for the denominator that will keep at least n
 *  significant figures in the result.
 */
#define GNC_DENOM_SIGFIGS( n ) ( ((( n ) & 0xff) << 8) | GNC_DENOM_SIGFIG)
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
 *  type of error.  Error values always have a denominator of zero.
 */ 
GNCNumericErrorCode  gnc_numeric_check(gnc_numeric a);

/** Returns 1 if a>b, -1 if b>a, 0 if a == b  */
int gnc_numeric_compare(gnc_numeric a, gnc_numeric b);

/** Returns 1 if the given gnc_numeric is 0 (zero), else returns 0. */
gboolean gnc_numeric_zero_p(gnc_numeric a);

/** Returns 1 if @var{a} < 0, otherwise returns 0. */
gboolean gnc_numeric_negative_p(gnc_numeric a);

/** Returns 1 if @var{a} > 0, otherwise returns 0. */
gboolean gnc_numeric_positive_p(gnc_numeric a);

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

/** Multiply a times b, returning the product.  An overflow
 *  may occur if the result of the multiplication can't
 *  be represented as a ratio of 64-bit int's after removing
 *  common factors.
 */
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
 * Shortcut for common case: gnc_numeric_add(a, b, GNC_DENOM_AUTO,
 *                        GNC_DENOM_FIXED | GNC_RND_NEVER);
 */
static inline
gnc_numeric gnc_numeric_add_fixed(gnc_numeric a, gnc_numeric b) {
   return gnc_numeric_add(a, b, GNC_DENOM_AUTO,
                         GNC_DENOM_FIXED | GNC_RND_NEVER);
}

/** 
 * Shortcut for most common case: gnc_numeric_sub(a, b, GNC_DENOM_AUTO,
 *                        GNC_DENOM_FIXED | GNC_RND_NEVER);
 */
static inline 
gnc_numeric gnc_numeric_sub_fixed(gnc_numeric a, gnc_numeric b) {
  return gnc_numeric_sub(a, b, GNC_DENOM_AUTO,
                         GNC_DENOM_FIXED | GNC_RND_NEVER);
}
/*@}*/

/** @name Arithmetic functions with exact error returns */
/*@{*/
gnc_numeric gnc_numeric_add_with_error(gnc_numeric a, gnc_numeric b,
                                       gint64 denom, gint how,
                                       gnc_numeric * error);
gnc_numeric gnc_numeric_sub_with_error(gnc_numeric a, gnc_numeric b,
                                       gint64 denom, gint how,
                                       gnc_numeric * error);

/** The same as @code{gnc_numeric_mul}, but uses @var{error} for 
 *  accumulating conversion roundoff error.
 */
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

/** Return input after reducing it by Greated Common Factor (GCF) 
 *  elimination */
gnc_numeric gnc_numeric_reduce(gnc_numeric in);
/*@}*/


#endif

/*@}*/
