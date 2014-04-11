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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
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

    A 'Numeric' value represents a number in rational form, with a
    64-bit integer as numerator and denominator.  Rationals are
    ideal for many uses, such as performing exact, roundoff-error-free
    addition and multiplication, but 64-bit rationals do not have 
    the dynamic range of floating point numbers.  

See \ref gncnumericexample

@{ */
/** @file gnc-numeric.h
    @brief An exact-rational-number library for gnucash.
	(to be renamed qofnumeric.h in libqof2)
    @author Copyright (C) 2000 Bill Gribble
    @author Copyright (C) 2004 Linas Vepstas <linas@linas.org>
*/


#ifndef GNC_NUMERIC_H
#define GNC_NUMERIC_H

#include <glib-object.h>

struct _gnc_numeric 
{
  gint64  num;
  gint64  denom;
};

/** @brief An rational-number type
 *
 * This is a rational number, defined by numerator and denominator. */
typedef struct _gnc_numeric gnc_numeric;

/** @name Arguments Standard Arguments to most functions

    Most of the gnc_numeric arithmetic functions take two arguments
    in addition to their numeric args: 'denom', which is the denominator
    to use in the output gnc_numeric object, and 'how'. which
    describes how the arithmetic result is to be converted to that
    denominator. This combination of output denominator and rounding policy
    allows the results of financial and other rational computations to be
    properly rounded to the appropriate units.

    Valid values for denom are:
    GNC_DENOM_AUTO  -- compute denominator exactly
    integer n       -- Force the denominator of the result to be this integer
    GNC_DENOM_RECIPROCAL -- Use 1/n as the denominator (???huh???)

    Valid values for 'how' are bitwise combinations of zero or one
    "rounding instructions" with zero or one "denominator types".
    Valid rounding instructions are:
        GNC_HOW_RND_FLOOR
        GNC_HOW_RND_CEIL 
        GNC_HOW_RND_TRUNC
        GNC_HOW_RND_PROMOTE 
        GNC_HOW_RND_ROUND_HALF_DOWN
        GNC_HOW_RND_ROUND_HALF_UP 
        GNC_HOW_RND_ROUND
        GNC_HOW_RND_NEVER

    The denominator type specifies how to compute a denominator if
    GNC_DENOM_AUTO is specified as the 'denom'. Valid 
    denominator types are:
        GNC_HOW_DENOM_EXACT  
        GNC_HOW_DENOM_REDUCE 
        GNC_HOW_DENOM_LCD   
        GNC_HOW_DENOM_FIXED 
        GNC_HOW_DENOM_SIGFIGS(N)

   To use traditional rational-number operational semantics (all results
   are exact and are reduced to relatively-prime fractions) pass the
   argument GNC_DENOM_AUTO as 'denom' and 
   GNC_HOW_DENOM_REDUCE| GNC_HOW_RND_NEVER as 'how'.

   To enforce strict financial semantics (such that all operands must have
   the same denominator as each other and as the result), use
   GNC_DENOM_AUTO as 'denom' and 
   GNC_HOW_DENOM_FIXED | GNC_HOW_RND_NEVER as 'how'.
@{
*/

/** \brief bitmasks for HOW flags.

 * bits 8-15 of 'how' are reserved for the number of significant
 * digits to use in the output with GNC_HOW_DENOM_SIGFIG 
 */ 
#define GNC_NUMERIC_RND_MASK     0x0000000f
#define GNC_NUMERIC_DENOM_MASK   0x000000f0
#define GNC_NUMERIC_SIGFIGS_MASK 0x0000ff00

/** \brief Rounding/Truncation modes for operations.

 *  Rounding instructions control how fractional parts in the specified
 *  denominator affect the result. For example, if a computed result is
 *  "3/4" but the specified denominator for the return value is 2, should
 *  the return value be "1/2" or "2/2"?  
 *
 * Possible rounding instructions are:
 */
enum { 
  /** Round toward -infinity */
  GNC_HOW_RND_FLOOR            = 0x01, 

  /** Round toward +infinity */
  GNC_HOW_RND_CEIL             = 0x02,  

  /** Truncate fractions (round toward zero) */
  GNC_HOW_RND_TRUNC            = 0x03,

  /** Promote fractions (round away from zero) */
  GNC_HOW_RND_PROMOTE          = 0x04,

  /** Round to the nearest integer, rounding toward zero 
   *  when there are two equidistant nearest integers.
   */
  GNC_HOW_RND_ROUND_HALF_DOWN  = 0x05, 

  /** Round to the nearest integer, rounding away from zero 
   *  when there are two equidistant nearest integers.
   */
  GNC_HOW_RND_ROUND_HALF_UP    = 0x06, 

  /** Use unbiased ("banker's") rounding. This rounds to the 
   *  nearest integer, and to the nearest even integer when there 
   *  are two equidistant nearest integers. This is generally the 
   *  one you should use for financial quantities.
   */
  GNC_HOW_RND_ROUND            = 0x07, 

  /** Never round at all, and signal an error if there is a 
   *  fractional result in a computation.
   */
  GNC_HOW_RND_NEVER            = 0x08
};

/** How to compute a denominator, or'ed into the "how" field. */
enum { 
  /** Use any denominator which gives an exactly correct ratio of 
   *  numerator to denominator. Use EXACT when you do not wish to 
   *  lose any information in the result but also do not want to 
   *  spend any time finding the "best" denominator.
   */
  GNC_HOW_DENOM_EXACT  = 0x10, 

  /** Reduce the result value by common factor elimination, 
   *  using the smallest possible value for the denominator that 
   *  keeps the correct ratio. The numerator and denominator of 
   *  the result are relatively prime. 
   */
  GNC_HOW_DENOM_REDUCE = 0x20,

  /** Find the least common multiple of the arguments' denominators 
   *  and use that as the denominator of the result.
   */
  GNC_HOW_DENOM_LCD    = 0x30,

  /** All arguments are required to have the same denominator,
   *  that denominator is to be used in the output, and an error 
   *  is to be signaled if any argument has a different denominator.
   */
  GNC_HOW_DENOM_FIXED  = 0x40,

  /** Round to the number of significant figures given in the rounding
   *  instructions by the GNC_HOW_DENOM_SIGFIGS () macro.
   */
  GNC_HOW_DENOM_SIGFIG = 0x50
};

/** Build a 'how' value that will generate a denominator that will 
 *  keep at least n significant figures in the result.
 */
#define GNC_HOW_DENOM_SIGFIGS( n ) ( ((( n ) & 0xff) << 8) | GNC_HOW_DENOM_SIGFIG)
#define GNC_HOW_GET_SIGFIGS( a ) ( (( a ) & 0xff00 ) >> 8)

/** Error codes */
typedef enum {
  GNC_ERROR_OK         =  0,   /**< No error */
  GNC_ERROR_ARG        = -1,   /**< Argument is not a valid number */
  GNC_ERROR_OVERFLOW   = -2,   /**< Intermediate result overflow */

  /** GNC_HOW_DENOM_FIXED was specified, but argument denominators differed.  */
  GNC_ERROR_DENOM_DIFF = -3,   

  /** GNC_HOW_RND_NEVER  was specified, but the result could not be
   *  converted to the desired denominator without a remainder. */
  GNC_ERROR_REMAINDER  = -4   
} GNCNumericErrorCode;


/** Values that can be passed as the 'denom' argument.  
 *  The include a positive number n to be used as the 
 *  denominator of the output value.  Other possibilities 
 *  include the list below:
 */

/** Compute an appropriate denominator automatically. Flags in 
 *  the 'how' argument will specify how to compute the denominator.
 */
#define GNC_DENOM_AUTO 0

/** Use the value 1/n as the denominator of the output value. */
#define GNC_DENOM_RECIPROCAL( a ) (- ( a ))

/**  @} */

/** @name Constructors
@{
*/
/** Make a gnc_numeric from numerator and denominator */
static inline 
gnc_numeric gnc_numeric_create(gint64 num, gint64 denom) {
  gnc_numeric out;
  out.num = num;
  out.denom = denom;
  return out;
}

/** create a zero-value gnc_numeric */
static inline
gnc_numeric gnc_numeric_zero(void) { return gnc_numeric_create(0, 1); }

/** Convert a floating-point number to a gnc_numeric. 
 *  Both 'denom' and 'how' are used as in arithmetic, 
 *  but GNC_DENOM_AUTO is not recognized; a denominator
 *  must be specified either explicitctly or through sigfigs.
 */
gnc_numeric double_to_gnc_numeric(double in, gint64 denom,  
                                  gint how);

/** Read a gnc_numeric from str, skipping any leading whitespace.
 *  Return TRUE on success and store the resulting value in "n".
 *  Return NULL on error. */
gboolean string_to_gnc_numeric(const gchar* str, gnc_numeric *n);

/** Create a gnc_numeric object that signals the error condition
 *  noted by error_code, rather than a number. 
 */
gnc_numeric gnc_numeric_error(GNCNumericErrorCode error_code);
/** @} */

/** @name Value Accessors
 @{
*/
/** Return numerator */
static inline 
gint64 gnc_numeric_num(gnc_numeric a) { return a.num; }
/** Return denominator */
static inline 
gint64 gnc_numeric_denom(gnc_numeric a) { return a.denom; }

/** Convert numeric to floating-point value. */
gdouble      gnc_numeric_to_double(gnc_numeric in);

/** Convert to string. The returned buffer is to be g_free'd by the
 *  caller (it was allocated through g_strdup) */
gchar *gnc_numeric_to_string(gnc_numeric n);

/** Convert to string. Uses a static, non-thread-safe buffer.
 *  For internal use only. */
gchar * gnc_num_dbg_to_string(gnc_numeric n);
/** @}*/

/** @name Comparisons and Predicates 
 @{
*/
/** Check for error signal in value. Returns GNC_ERROR_OK (==0) if
 *  the number appears to be valid, otherwise it returns the
 *  type of error.  Error values always have a denominator of zero.
 */ 
GNCNumericErrorCode  gnc_numeric_check(gnc_numeric a);

/** Returns 1 if a>b, -1 if b>a, 0 if a == b  */
gint gnc_numeric_compare(gnc_numeric a, gnc_numeric b);

/** Returns 1 if the given gnc_numeric is 0 (zero), else returns 0. */
gboolean gnc_numeric_zero_p(gnc_numeric a);

/** Returns 1 if a < 0, otherwise returns 0. */
gboolean gnc_numeric_negative_p(gnc_numeric a);

/** Returns 1 if a > 0, otherwise returns 0. */
gboolean gnc_numeric_positive_p(gnc_numeric a);

/** Equivalence predicate: Returns TRUE (1) if a and b are 
 *  exactly the same (have the same numerator and denominator)
 */ 
gboolean gnc_numeric_eq(gnc_numeric a, gnc_numeric b);     

/** Equivalence predicate: Returns TRUE (1) if a and b represent
 *  the same number.  That is, return TRUE if the ratios, when 
 *  reduced by eliminating common factors, are identical.
 */ 
gboolean gnc_numeric_equal(gnc_numeric a, gnc_numeric b);  

/** Equivalence predicate: 
 *  Convert both a and b to denom using the 
 *  specified DENOM and method HOW, and compare numerators 
 *  the results using gnc_numeric_equal.
 *
  For example, if a == 7/16 and b == 3/4,
  gnc_numeric_same(a, b, 2, GNC_HOW_RND_TRUNC) == 1
  because both 7/16 and 3/4 round to 1/2 under truncation. However,
  gnc_numeric_same(a, b, 2, GNC_HOW_RND_ROUND) == 0
  because 7/16 rounds to 1/2 under unbiased rounding but 3/4 rounds
  to 2/2.
 */ 
gint gnc_numeric_same(gnc_numeric a, gnc_numeric b,   
                     gint64 denom, gint how);
/** @} */

/** @name Arithmetic Operations 
 @{ 
*/
/** Return a+b. */
gnc_numeric gnc_numeric_add(gnc_numeric a, gnc_numeric b, 
                            gint64 denom, gint how);

/** Return a-b. */
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
 *                        GNC_HOW_DENOM_FIXED | GNC_HOW_RND_NEVER);
 */
static inline
gnc_numeric gnc_numeric_add_fixed(gnc_numeric a, gnc_numeric b) {
   return gnc_numeric_add(a, b, GNC_DENOM_AUTO,
                         GNC_HOW_DENOM_FIXED | GNC_HOW_RND_NEVER);
}

/** 
 * Shortcut for most common case: gnc_numeric_sub(a, b, GNC_DENOM_AUTO,
 *                        GNC_HOW_DENOM_FIXED | GNC_HOW_RND_NEVER);
 */
static inline 
gnc_numeric gnc_numeric_sub_fixed(gnc_numeric a, gnc_numeric b) {
  return gnc_numeric_sub(a, b, GNC_DENOM_AUTO,
                         GNC_HOW_DENOM_FIXED | GNC_HOW_RND_NEVER);
}
/** @} */

/** @name Arithmetic Functions with Exact Error Returns 
 @{
*/
/** The same as gnc_numeric_add, but uses 'error' for accumulating
 *  conversion roundoff error. */
gnc_numeric gnc_numeric_add_with_error(gnc_numeric a, gnc_numeric b,
                                       gint64 denom, gint how,
                                       gnc_numeric * error);

/** The same as gnc_numeric_sub, but uses error for accumulating
 *  conversion roundoff error. */
gnc_numeric gnc_numeric_sub_with_error(gnc_numeric a, gnc_numeric b,
                                       gint64 denom, gint how,
                                       gnc_numeric * error);

/** The same as gnc_numeric_mul, but uses error for 
 *  accumulating conversion roundoff error.
 */
gnc_numeric gnc_numeric_mul_with_error(gnc_numeric a, gnc_numeric b,
                                       gint64 denom, gint how,
                                       gnc_numeric * error);

/** The same as gnc_numeric_div, but uses error for 
 *  accumulating conversion roundoff error.
 */
gnc_numeric gnc_numeric_div_with_error(gnc_numeric a, gnc_numeric b,
                                       gint64 denom, gint how,
                                       gnc_numeric * error);
/** @} */

/** @name Change Denominator 
 @{
*/
/** Change the denominator of a gnc_numeric value to the 
 *  specified denominator under standard arguments 
 *  'denom' and 'how'. 
 */
gnc_numeric gnc_numeric_convert(gnc_numeric in, gint64 denom, 
                                gint how);

/** Same as gnc_numeric_convert, but return a remainder 
 *  value for accumulating conversion error. 
*/
gnc_numeric gnc_numeric_convert_with_error(gnc_numeric in, gint64 denom, 
                                           gint how,
                                           gnc_numeric * error);

/** Return input after reducing it by Greated Common Factor (GCF) 
 *  elimination */
gnc_numeric gnc_numeric_reduce(gnc_numeric in);

/** Attempt to convert the denominator to an exact power of ten without
 *  rounding.
 *
 *  @param a the ::gnc_numeric value to convert
 *
 *  @param max_decimal_places the number of decimal places of the
 *  converted value. This parameter may be @c NULL.
 *
 *  @return @c TRUE if @a a has been converted or was already decimal.
 *  Otherwise, @c FALSE is returned and @a a and @a max_decimal_places
 *  remain unchanged.
 ********************************************************************/
gboolean gnc_numeric_to_decimal(gnc_numeric * a,
                                guint8 * max_decimal_places);
/** @} */

/** @name GValue 
  @{
*/
GType gnc_numeric_get_type( void );
#define GNC_TYPE_NUMERIC (gnc_numeric_get_type ())

/** @} */

/** @name Deprecated, backwards-compatible definitions 
  @{
*/
#define GNC_RND_FLOOR	GNC_HOW_RND_FLOOR
#define GNC_RND_CEIL 	GNC_HOW_RND_CEIL 
#define GNC_RND_TRUNC	GNC_HOW_RND_TRUNC
#define GNC_RND_PROMOTE 	GNC_HOW_RND_PROMOTE 
#define GNC_RND_ROUND_HALF_DOWN	GNC_HOW_RND_ROUND_HALF_DOWN
#define GNC_RND_ROUND_HALF_UP 	GNC_HOW_RND_ROUND_HALF_UP 
#define GNC_RND_ROUND	GNC_HOW_RND_ROUND
#define GNC_RND_NEVER	GNC_HOW_RND_NEVER

#define GNC_DENOM_EXACT  	GNC_HOW_DENOM_EXACT  
#define GNC_DENOM_REDUCE 	GNC_HOW_DENOM_REDUCE 
#define GNC_DENOM_LCD   	GNC_HOW_DENOM_LCD   
#define GNC_DENOM_FIXED 	GNC_HOW_DENOM_FIXED 
#define GNC_DENOM_SIGFIG 	GNC_HOW_DENOM_SIGFIG 

#define GNC_DENOM_SIGFIGS(X)  GNC_HOW_DENOM_SIGFIGS(X)
#define GNC_NUMERIC_GET_SIGFIGS(X) GNC_HOW_GET_SIGFIGS(X)
/** @} */
/** @} */
#endif
